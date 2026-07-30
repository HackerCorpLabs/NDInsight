# PIOC-OS (ENCOS Ethernet II) vs the ACCP octobus firmware

**Date**: 2026-07-27
**Question asked**: is the ACCP's firmware "very similar" to the PIOC-OS that runs the
Ethernet controller?

**Short answer: same compiler and same house style, but NOT the same operating system.**
PIOC-OS is a priority-preemptive multitasking kernel with a trap-based system-call API.
The ACCP firmware has no kernel at all - it is a single-threaded monitor program with
interrupt handlers. The reuse between them is the PLANC-MC runtime, not an RTOS.

Sources:
- PIOC-OS: `..\..\Installation\Communication\Ethernet\x\stripped\docs\PIOC-OS\` (10 documents)
- ACCP: `ACCP-324716-FIRMWARE-RE-2026-07-27.md` and the live Ghidra database `octo.bin`
- Both images are open in Ghidra, so every claim below was checked against both, not
  recalled.

---

## 1. What PIOC-OS is

PIOC-OS runs on the ENCOS Ethernet II controller card (`encos-ser-all-banks-68k.bin`,
MC68000, PLANC-MC, ND 1986). It is a real RTOS:

| Property | Value |
|---|---|
| Structure | **nine linked modules** with a circular directory of 32-byte records at 0x05C8, each carrying an 8-char name and an ASCII build date |
| Scheduling | **priority-preemptive**, 16 ready queues at 0x0B06, highest priority wins |
| Processes | **three**: `FREE` (priority 5, idle), `PRO1` (priority 1, the application), `RTC ` (priority 14, the tick) |
| Objects | 30-slot object table at 0x0A8A, heap-allocated descriptors |
| System calls | **`trap #2`, 27 services**, dispatch table at 0x0C6A, D0 = function code, A0 = argument block |
| Context switch | deferred preemption by **return-address hijack** - the event poster rewrites the interrupt frame's saved PC to the scheduler address so `rte` lands in the scheduler |
| Time | MFP timer at 2457600 Hz, ISR on vector 69, 32 timer elements |
| IPC | three layers - per-process event bit-sets, named ports (`PORT*`/`PONA*`), and sub-process work queues |
| Memory | general heap, a never-reclaimed PLANC frame arena, and about nine fixed pools |
| Host link | ND-100 doorbell on vector 78 in, SCIP at 0x00EF0080 out, monitor SUBFN dispatch at 0x0512 |

The single mechanism to understand is the deferred preemption: nothing switches inside the
code that makes a process runnable; it sets a flag and rewrites the exception frame, guarded
by a **double latch** (0x0660 and 0x0662) so a second event post cannot destroy the saved PC.

---

## 2. What the ACCP firmware is

`octo.bin`, the Samson ACCP (ND-324716), December 5, 1988. Checked in Ghidra today:

- **No `trap #2` anywhere.** Searching the whole 128 KB for `4E 42` returns 11 hits and every
  one of them falls inside the string region or at an odd address. There is no kernel entry
  point and no service dispatch table.
- **The one distinguished TRAP is #10, and it is a fault path, not an API.** The handler at
  0x08A4 does `addi.l #-2,(2,SP)` to back the saved PC onto the trapping instruction, stores
  fault code 0x2A at 0x00113112, and falls straight into `FaultRecordAndPanic` (0x08C4) -
  the same place every processor fault stub goes. Its job is to print
  `"6 8 0 0 0   T R A P : "` and the register dump, not to serve a request.
- **No module directory.** ENCOS has nine records with build dates. ACCP has exactly one
  version record - a numeric date string `"88.12. 5"` followed by an array descriptor
  `{origo 0x13BFC, lower 0, upper 0x0F}` over `"December 5, 1988"`, at 0x13BF4. One module,
  one link.
- **No process table, no ready queues, no object handles.** Instead the strings name a
  **single main loop**: `"Error exit from idle loop"`. Work arrives by interrupt (autovectors
  IRQ1-7 at 0x4BA/0x4C6/0x510/0x694/0x796/0x7A8/0x826) and by console command.
- **Its "API" is a human command line** - the 43-entry console command table at 0x130FE
  (LOAD-CONTROL-STORE, START-MICROPROGRAM, SEND-OCTOBUS, RESET-CPU, READ-ACCP-STATUS,
  RUN-SHORT/LONG-SELFTEST, ...), reached over the SCN2681 console at 0x00DD0000.

---

## 3. What the two genuinely share

This is where the "very similar" intuition is right, and it is worth being precise about it,
because it is the part that transfers.

| Shared | Detail |
|---|---|
| **Compiler** | PLANC-MC on MC68000, both ND-built |
| **Skip return** | normal return goes to RETLINK+2; the 2 bytes after every call are the error slot |
| **`jmp (A5)` error unwind** | A5 permanently holds the runtime error vector. ENCOS: `#XRET` 0x135A8 / `#ERET` 0x13596. **ACCP: 0x115AE**, loaded at 0x0900 |
| **A6 bump-allocated frames** | frames live in an arena and are never popped; `move.l A2,(A6)` publishes the next-free cursor. ACCP's TRAP handler does this inline at 0x08EC-0x08FA with arena base 0x00112800 |
| **Array descriptors** | `{origo, lower, upper}` passed by copying three longwords into the callee frame |
| **`$` = newline** | 0x24 terminates/breaks every string in both images |
| **Fault-record-and-panic** | both funnel every processor fault through one recorder that saves SR/PC/SP/A6 and the register file, then prints |
| **Leaf runtime** | the same `#IMU`/`#IDV`/`#APPD`/`#REMV` style helpers, register-argument and plain `rts` |

So the **`ghidra-planc` skill and its five scripts apply to both images.** That is the real
carry-over.

---

## 4. Where they differ in ways that will bite

Do not copy PIOC-OS offsets into ACCP work.

| | PIOC-OS (ENCOS, 1986) | ACCP (1988) |
|---|---|---|
| First "further" parameter | **+0x12** | **+0x14** |
| ERRCODE | 16-bit at +0x10 | (consistent with +0x14 parameter start) |
| Array descriptor | **8 bytes** `{long origo, word lower, word upper}` | **12 bytes** `{long origo, long lower, long upper}` |
| Argument staging | through `(0x4,A6)`, the outgoing-frame pointer | through **`(A6)`** directly |
| Symbol table | **ND linker symbols present**, 241 names at file offset 0x663E0 | **none** - candidates all fall inside the microcode blob |
| Kernel | trap #2, 27 services | none |

The descriptor-width difference is the PLANC-MC **version-F word-size boundary** (word went
from 2 bytes to 4). ACCP is on the later side of it, ENCOS on the earlier. Two years apart,
and it silently doubles every descriptor.

---

## 5. What this means practically

1. **Do not go looking for a PIOC-OS in `octo.bin`.** It is not there. Time spent hunting an
   object table or a trap dispatcher is time wasted; the carving targets are the octobus and
   MF-bus drivers and the command table.
2. **Do reuse the PLANC tooling and reading discipline** - skip returns, error slots, the
   arena, descriptor-chasing to resolve strings. That technique is exactly how the MF-bus
   routine at 0x70CC was identified (its timeout descriptor resolves to
   `"$MF-bus memory timeout$"`).
3. **The ACCP is the simpler machine to emulate.** No scheduler, no descriptor size that is
   only computable at boot, no host-supplied board-config record gating the heap. A single
   main loop plus interrupt handlers plus the DUART is the whole model - which is what the
   RetroCore handoff assumes.

---

## 6. UNVERIFIED / open

- Whether the ACCP firmware has **any** multitasking at all, cooperative or otherwise, has
  been established only negatively (no trap kernel, no process table, an idle-loop string).
  The main loop itself has not yet been transcribed. That is not the same as proving it is
  strictly single-threaded.
- Whether ND ever shipped a PIOC-OS-based ACCP in an earlier revision (ND-324702) is unknown.
  Only the 324716 image has been read.

## Provenance

Section 1 is a condensation of the ten PIOC-OS documents. Section 2's negative findings were
run against `octo.bin` in Ghidra on 2026-07-27 - the `4E 42` byte search, the TRAP #10
handler disassembly, the string enumeration (154 strings) and the version record hexdump.
Section 3 and 4's shared/divergent items come from both databases.
