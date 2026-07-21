# NDIX kernel + vendor-manual evidence for the ND-100 <-> ND-500 interface (2026-07-20)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\NDIX-KERNEL-INTERFACE-EVIDENCE-2026-07-20.md`

**What this is:** a session's findings from three sources that are INDEPENDENT of the SINTRAN
carves and the B30 microcode image:

1. **NDIX (Norsk Data Unix) Release 3 kernel source**, 1987-88 — a second, non-SINTRAN guest OS
   that runs on the 500-side CPU and talks to a SINTRAN ND-100 front end. Located at
   `E:\Dev\Ronny\NDIX-C\kernel\MASTER\` (C + ND-500 assembly, with SCCS history).
2. **ND-05.012.01 ND-500 Micro Program Guide** (vendor), OCR at
   `E:\Dev\Ronny\ND500UC\manuals\ND-05.012.01 ND-500 Micro Program Guide.md` — section 13
   is the vendor statement of the message protocol.
3. **NEC-01 ND-500 course** (vendor training), OCR at
   `E:\Dev\Ronny\ND500UC\manuals\NEC-01 - ND-500 course.md` — documents the 5015 (CONTROL II)
   as an internal CPU module and lists its registers as microcode unit-select codes.

**Grades:** `[SRC]` = read directly from NDIX source text; `[MANUAL]` = read directly from a
vendor manual OCR; `[D]` = derived/inferred; `[?]` = unknown. Nothing here is from carved bytes.

**Master reference this extends:**
[`ND500-BUS-INTERFACE-REFERENCE.md`](ND500-BUS-INTERFACE-REFERENCE.md). Nothing below
contradicts it; sections 3-5 corroborate its §5.4/§6/§7 from the other side of the wall.

---

## 1. What NDIX is, and why it matters as evidence

- `E:\Dev\Ronny\NDIX-C\kernel\MASTER\machine\if.h:10`: "definitions for the interface on
  **ND-5000**" `[SRC]`. Machine-level headers consistently say ND-500 for the CPU architecture
  (`cxb.h` "ND-500 context block", `pte.h`, `param.h`), while the interface layer and memory
  comments say ND-5000 (`machdep.c:196-198` "the 100 has loaded the kernel into the start of
  the 5000 memory"; `machdep.c:224` "map all of physical (ND5000) memory") `[SRC]`.
- `machdep.c:99`: `extern naddr_t scont; /* First mpm addr used by Ndix */` — the shared area
  is in **MPM** `[SRC]`.
- File dates 1986-88 (`baseline/bin/as/l.README`: releases 86/07-87/12) `[SRC]`.
- NDIX is a **domain-level client of the same SINTRAN-side machinery the emulator must
  implement**: everything NDIX needs from the interface is visible in its source, so it defines
  a second compatibility contract, independent of what SINTRAN's own 500-monitor uses.

Related analysis docs (earlier sessions, same tree):
`E:\Dev\Ronny\NDIX-C\FECALL-INTERFACE.md` (1900-line protocol write-up),
`E:\Dev\Ronny\NDIX-C\NDIX_KERNEL_SEGMENT31_INDIRECT_CALL.md`,
`E:\Dev\Ronny\NDIX-C\ND500_PC_OMC_OTHER_MACHINE_MECHANISM.md`,
`E:\Dev\Ronny\NDIX-C\NDIX_0xF8_SYMBOLIC_ADDRESSES.md`.

---

## 2. Outbound: `fecall` = one `callg` through a PC_OMC segment-31 capability

`E:\Dev\Ronny\NDIX-C\kernel\MASTER\GENERIC\locore.s:610-630` (same text in
`machine\locore.c:219-239`) `[SRC]`:

```
_feinit_fecall:  ents $36 ; go fe2          # FE_INIT: NO address conversion
_feexit_fecall:  ents $36 ; go fe1          # FE_EXIT: converts only b.32
_fecall:         ents $36
    w1 phyladr ind(b.28)                    # response pkt -> ND-500 physical
    w add2 r1, _private
    w div3 r1, $2, b.28                     # -> ND-100 WORD address
fe1:
    w2 phyladr ind(b.32)                    # command pkt, same conversion
    w add2 r2, _private
    w div3 r2, $2, b.32
fe2:
    callg $0xf8000180,$4,b.20,b.24,b.28,b.32
    ret
```

- Segment = `0xF8000180 >> 27` = **31**; offset = **0x180 = 600 octal** `[SRC]`.
- Kernel PCB capability flags (`machine\pcb.h`): `PC_IND 0x8000`, `PC_OMC 0x4000` ("Other
  Machine") `[SRC]`. The kernel never initializes segment 31 itself (`kpcbinit()` in
  `vm_machdep.c` sets only segment 1); user processes get `pcb_pc[31] = PC_IND|1` (plain
  domain call to Domain 0 Segment 1) in `pcbfork()` `[SRC]`. The kernel's PC_OMC capability is
  therefore installed by SINTRAN before start `[D]`.
- **Cross-tie to the carve track:** the microcode's trap/OMC path must turn this `callg` into a
  mailbox message whose call number is the segment offset — i.e. **MCNO = 600B** for every NDIX
  I/O request. The four `callg` arguments are: word 1 = `s3_dev` (gen device<<16 | subdevice),
  word 2 = `s3_req` (FE function | QF flags), words 3/4 = response/command packet **ND-100 word
  addresses** `[SRC]`; that they land in the message parameter slots (`0o100+2k`) is `[D]` —
  worth one microcode/carve check.

### 2.1 Address arithmetic — `private` is the window constant

- `param.h:51-53`: `htob(x) = x<<1`, `btoh(x) = x>>1` — "ND-100 word address to ND-500 byte
  address and v.v." `[SRC]`.
- Outbound conversion: ND-100 word addr = (ND-500 physical byte + `private`) / 2 `[SRC]`.
  `private` is **returned by SINTRAN in the FE_INIT response** (below), so the ND-100-window
  placement of 500-memory (the ADRZERO / DEFINE-MEMORY-CONFIGURATION business) is entirely
  SINTRAN's choice and communicated at boot, not compiled in `[SRC]`.
- FE_INIT itself runs **before** `private` is known — `_feinit_fecall` skips conversion and
  passes raw kernel VAs; SINTRAN must resolve those itself (it loaded the kernel, so it knows
  the layout) `[SRC]+[D]`.

### 2.2 FE function codes and devices (`machine\if.h:17-49`) `[SRC]`

| Code | Function | | Device # | Class |
|---|---|---|---|---|
| 0x1 | FE_INIT | | 0 | FAULT |
| 0x2 | FE_IDEV (init generic device) | | 1 | DISK |
| 0x3 | FE_OPEN | | 2 | TAPE |
| 0x4 | FE_CLOS | | 3 | TERM_IN |
| 0x5 | FE_READ | | 4 | TERM_OUT |
| 0x6 | FE_RCON (sync console read) | | 5 | CLOCK |
| 0x7 | FE_WRIT | | 6 | GTRAP |
| 0x8 | FE_WCON (sync console write) | | 7 | XMSG |
| 0x9 | FE_DCTL | | 8 | SIINTR |
| 0xb | FE_EXIT | | | |
| 0xe | FE_ERRM (error code to ND-100) | | | |

Qualifiers: `QF_ASYNC 0`, `QF_SYNC 1`, `QF_RE 2`. A QF_SYNC `fecall` does not return until the
ND-100 has completed; QF_ASYNC returns immediately and completion arrives as an interrupt
(section 4) `[SRC]`.

---

## 3. The FE_INIT boot handshake (`machine\machdep.c:797-836`) `[SRC]`

What NDIX sends (struct `init_pkt.init_cpk`, `if.h:57-65`):

| Field | Content |
|---|---|
| `ux_vers` | protocol version |
| `cxb0` | address of `x_cxbtab` — **7 spare context blocks** (`locore.s:682-684`) |
| `trcxb`, `trdata` | fatal-trap context block + error record (`errcxb`, `errdata`) |
| `intvec` | **entry point the ND-100 forces the CPU into for I/O interrupts** |
| `trapvec` | entry point for fatal traps (`errvec`) |

What SINTRAN answers (`init_rpk`, `if.h:66-80`): `completion` (0 = ok), `howto`, `rootdev`,
`condev`, then the memory map as ND-100 word addresses (converted with `htob`): `stext`,
`sdata`, `sstack`, `sfree`, `sphys`, `scont` (first MPM addr), `spst`, plus `private`,
`sharedseg` (NDIX computes `shseg = htob(sharedseg) + NBPG`), `cputype`, `s3_vers`,
`contigno/pageno`, and `booted[256]` (boot file name, default `(SYSTEM)VMUNIX`).

**Emulator consequence:** any hope of ever booting NDIX under the emulator requires an ND-100
side that implements MON 600B with at least FE_INIT semantics and this response block `[D]`.

---

## 4. Inbound: interrupt descriptors in shared memory + forced entry at `intvec`

- Shared segment fixed layout, KVA `_sharebase = 0x30000000` (`GENERIC\locore.s:500-512`)
  `[SRC]`:

| KVA | Symbol |
|---|---|
| 0x30000000 | `xmsg_cmd_buf` |
| 0x30000800 | `xmsg_resp_buf` |
| 0x30001000 | `iplrec` = {`ip_next`, `ip_current`, `ip_mask`, `ip_lock`} |
| 0x30001040 | `clockrec` (tick counter) |
| 0x30001044 / 48 | `cin` / `cout` (console words) |
| 0x3000104c | `cons_pkt` |
| 0x30001080 | `sub_dev_descrip` |

  Bulk data at `0x38000000`: `rawbuf`, `xdata`, `xpara`, `tape_pkt`, `mx_bin/bout`,
  `disk_pkt`, `sint_pkt`, `clock_pkt`, `tty_in/out_pkt`, `xmsg_pkt`, `fe_exit_pkt`, `et_info`
  (`locore.s:520-544`) `[SRC]`.
- The ND-100 queues `struct int_descr` records (`machine\icb.h`: `id_next` chain / `id_ipl` /
  SINTRAN device info `id_s3add,id_s3dev` / `id_gen_dev,id_sub_dev` / `id_s3func` /
  `id_resp_pkt`) and links them on `iplrec.ip_next` `[SRC]`. Same link-chain idiom as the
  SINTRAN message queue, but this is NDIX's own queue in its own shared segment.
- `ip_next` is stored as an **ND-100 word address**; conversion at interrupt entry:
  `byte = (ip_next << 1) - shseg + 0x30000000` (`locore.c:791-796`, same in `trap.c:696`)
  `[SRC]`.
- Entry `_intvec` (`locore.c:728-757` comment block) `[SRC]`: single vector for all ND-100 I/O
  interrupts; "The interrupt is entered with the IPL record **locked, in solo mode**"; the
  `tutti` and the unlock happen inside C `dispatch()` (`machine\trap.c:602-717`), which runs a
  **software IPL system** (`ip_mask`/`ip_current` shared with the FE) and dispatches
  `drvtab[gen-1].fr_intr(sub)`. Return through `lcntxt` reloading the saved context block.
- Separate `_errvec` + `errcxb` for "fatal traps" vectored by the ND-100 (`locore.c:839`);
  `vm_machdep.c:125` "All kernel traps go to the ND-100" `[SRC]`.
- Clock: "The ND-100 interrupts every 40ms" (`machine\clock.c:63-67`); FEIDEV on the CLOCK
  device returns the ND-100's absolute date `[SRC]`.
- **How the ND-100 physically forces the CPU into `intvec` is not visible in NDIX** — that is
  the activate/context-load machinery on the SINTRAN/microcode side; the `intvec`/`cxb0`
  addresses handed over at FE_INIT are evidently its parameters `[D]`.

---

## 5. Vendor anchor: ND-05.012.01 §13 (Micro Program Guide) `[MANUAL]`

`E:\Dev\Ronny\ND500UC\manuals\ND-05.012.01 ND-500 Micro Program Guide.md:1090-1400`:

- Line 1094: init sequence ends in the IDLE loop; "**Nothing but an activate or a terminate
  from the ND-100 can cause the micro program to leave the IDLE loop.**" (Already quoted in
  the master reference §5.4 via the dossier — here is the primary text location.)
- §13.1 message block: **6-word ND-100 header** (next-link 2 words, status, sender = "address
  of RT description for sender", receiver = "ND-500 process number", size) + data part
  (function value + parameters). Status values: **0 free, 1 message to ND-500, 2 in process
  (set by microcode at start), 3 answer (set when finished), 4 error return** — the N5STA
  lifecycle, vendor-stated.
- §13 function-value table (octal): 1 read µ-version; 6/7 physical data memory
  examine/deposit; 10/11 logical data memory read/write; 12 set cache mode; 13/14 physical
  data memory read/write; 16/17 register examine/deposit; 20/21 register read/write;
  **23 start / monitor call / trap; 24 restart after monitor call; 25 restart after trap**;
  30/31 physical segment read/write; 34/35 logical instruction memory read/write; 42
  programmed trap; 44 histogram read. §13.2-13.x give the per-function parameter layouts
  (`link.06` = function, `link.07+` = parameters, with direction arrows).
- Cross-check against the symbol-table MICFU values in the master reference §6.4: 1/23/24/25/44
  match (3RMICV/3START/3MONCO=restart-after-moncall/3TRACO/3RPREG). The reference's
  26=3WMONCO and 27=3FITRNSF are not in the vendor table (later additions `[D]`); the vendor
  debug codes (6-21, 30-35) are not in the symbol list (TMP/monitor-only traffic `[D]`).

---

## 6. CPU <-> 5015: the access mechanism (NEC-01 course + TMP) `[MANUAL]`

- `NEC-01 - ND-500 course.md:1406-1418` (page 57): **CONTROL II 5015** contents: transceivers
  for control-store bits 63-32, XD-bus transceivers, and the "NORD-100 communication logic":
  DATA-OUT, DATA-IN, TAG-IN, TAG-OUT, control-store write-address (WA), micro-address BREAK,
  CSCNT registers, plus cable drivers/receivers.
- `NEC-01:958`: control-store groups CS3/CS2 (bits 63-32) are routed "directly to the control
  store via the internal bus on the NORD-100/500 communication module 5015".
- `NEC-01:1098-1103`: the course's **microcode unit-code tables** list 5015 registers with
  unit-select codes like every other CPU register (`IDATIN` "TO DATA IN REGISTER", 32-bit,
  unit 0; `CSAR` control store address reg). **The microcode addresses the 5015's registers as
  ordinary internal-bus source/destination units — there is no I/O instruction and no special
  channel.**
- ND-30.013.02 §3.13 (`E:\Dev\Ronny\ND500UC\manuals\ND-30.013.02 Test Micro Program
  Descriptions for ND-500.md:923-940`): the MOST bit "determines which part of the register to
  use **when micro-programmed**" — direct vendor confirmation that microcode manipulates
  DATA-IN/DATA-OUT/TAG-OUT itself.
<!-- CORRECTION 2026-07-20: the original said "RIOM/WIOM". There is no WIOM. Manual ND-05.009.4
     section 16.23 = "Read I/O processor memory" (RIOM only); 16.24 = "Clear translation speedup
     buffer". No "Write I/O processor memory" section exists. The WIOM name propagated from a
     hallucinated dangling See-Also in the generated riom.md (now removed). RIOM has no write
     companion. Text below corrected to RIOM. -->
- Consequence for the ISA level: `RIOM` (Reference Manual section 16.23; per-instruction doc
  `E:\Dev\Ronny\ND500-DOCS\instructions\asm\riom.md` — supervisor-only halfword DMA against
  ND-100 private memory "through the ND-500 interface", "does not interrupt ND-100 program
  execution") are implemented by the microcode driving **TAG-OUT codes 6/7 with the 3022 MAR**
  `[D]` — consistent with master reference §10.2. This is a MANUAL-route partial answer to
  open question **C7 / Q-OTH-05** (classic-500 physical path for RIOM): the path is
  5015 TAG-OUT/DATA-OUT -> DBU cable -> 3022 MAR/DATA -> ND-100 memory, bounded by the 3022
  DMA limit registers; only the exact microword sequence remains unknown `[?]`.

---

## 7. What remains open (unchanged or sharpened)

| Item | Status |
|---|---|
| Exact microword operation that sets STATUS "finished" / raises level 12 | `[?]` — needs a classic-500 (5200/5400) control-store listing; B30 is octobus-generation |
| Whether the 4 `callg` args land verbatim in message param slots `0o100+2k` | `[D]` — checkable against B30 OMC path or a SINTRAN-side carve of a live NDIX message |
| How SINTRAN forces the 500 into NDIX's `intvec` (which MICFU / context-load) | `[D]` — candidate: 3START with a context block built from the FE_INIT `cxb0` handover |
| NDIX `private` value vs the ADRZERO window constant | `[D]` — compute from a DEFINE-MEMORY-CONFIGURATION example |

---

## 8. Debunk cleanup done in the same session (2026-07-20)

The fabricated "high-level TAG protocol" (TAG-IN 8 = MonitorCallRequest, TAG-OUT 16 =
OperationComplete, process number in TAG bits) survived in four documents outside this folder.
All four are now corrected or deprecation-bannered:

- `E:\Dev\Ronny\NDInsight\Developer\MON\calls\60B_N500M_Hardware_Mapping.md` — corrected in
  place (banner + rewritten sections 2, 4, 5.3, 6, 7.1, 8, 9.2, glossary)
- `E:\Dev\Ronny\NDInsight\Operations\SINTRAN\ND500-MONITOR-CALL-ARCHITECTURE.md` — body fixed
  (sequence diagrams, invented handler disassembly, invented message layout, TAG-value + IOX
  tables, C# pseudo-code); banner was already present
- `E:\Dev\Ronny\NDInsight\SINTRAN\Emulator\DETAILED-TAG-MECHANISM-EXPLANATION.md` — deprecated
  wholesale (banner; kept as poisoned-prior record)
- `E:\Dev\Ronny\NDInsight\SINTRAN\Emulator\ND500-QUICK-REFERENCE.md` — deprecated (banner)

Authority for all corrections: [`ND500-BUS-INTERFACE-REFERENCE.md`](ND500-BUS-INTERFACE-REFERENCE.md)
§3.2/§4/§5/§7/§10, ND-30.013.02 §3.12-3.15, ND-05.012.01 §13.

---

## 9. Octobus-code impact assessment (D4 plan task 0.4, 2026-07-20)

**Question asked:** does this NDIX evidence require changes to the octobus transport logic
(`OctobusND5000Station.cs`, `NDBusOctobus.cs`), or is it doc/TODO-only?

**Verdict: NO octobus code change. TODO/doc-only.** Rationale, per finding:

1. **fecall / segment-31 PC_OMC / MON 600B (section 2)** is the **3022-style mailbox doorbell**, not
   octobus frame transport. It belongs to the 3022/swapper bridge track (`CpuND500.ND100Bridge.cs`
   segment-31 handling), not the octobus station. No octobus change.
2. **The `(phys + private)/2` address arithmetic (section 2.1)** is IDENTICAL to the emulator's
   existing `MapND100ToPhysical` / `MapPhysicalToND100` (`_private + word*2`). NDIX independently
   **corroborates the current 3022 bridge**; it also confirms `private` is handed over by SINTRAN at
   boot (FE_INIT), which supports the C4 note now in `CpuND500.ND100Bridge.cs`. No change - evidence
   FOR existing code.
3. **The §13 message protocol + status lifecycle 0/1/2/3/4 (section 5)** corroborates the existing
   servicer/MICFU handling; the function table (23 start / 24 restart-moncall / 25 restart-trap)
   matches the symbol-table values already modelled. No octobus change.
4. **The 5015 unit-select / RIOM-via-TAG-OUT-6/7 finding (section 6)** is a MANUAL partial answer to
   C7 / Q-OTH-05 (classic-500 physical path), already recorded in the register. Classic 3022/5015
   generation, not octobus. Doc-only.

**TODO items surfaced (not octobus, not blocking D4):**
- **[TODO-NDIX-1]** If the emulator ever targets NDIX as a guest OS, the ND-100 side must implement
  **MON 600B with FE_INIT semantics** and the `init_rpk` response block (section 3). New feature on the
  3022/mailbox side; nothing exists today. Not on the SINTRAN-L D4 critical path.
- **[TODO-BRIDGE-1]** `IsND100SharedMemory` keys on `ND100_SHARED_SEGMENT = 6`, but NDIX's OMC
  doorbell is **segment 31** (`0xF8000180 >> 27`) with the `PC_OMC 0x4000` capability flag. These are
  different roles (segment 6 = shared data window; segment 31 = OMC call gate), so this is likely fine,
  but the segment-31 OMC path is worth an explicit check when the bridge track resumes. 3022 bridge
  item, not octobus.

Both TODOs are 3022/bridge-track, not octobus. The octobus open questions (Q-OCT-*, Q-CSL-*, Q-ACT-*)
are unaffected by this evidence.
