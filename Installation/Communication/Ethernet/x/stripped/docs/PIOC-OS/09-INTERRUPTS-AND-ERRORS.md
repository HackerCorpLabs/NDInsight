# PIOC-OS - Interrupts, faults and error handling

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: interrupt map essentially complete. Error subsystem PARTIAL.

---

## 1. The 68000 vector table (0x000-0x3FF)

**Fully populated in the image** - it is a real static table, not built at run time. (PIOC-OS trap
0x0F can also patch it at run time, so a live card may differ.)

Reading it first is the fastest orientation available on an unfamiliar ND firmware image: it gives
you the entry point and every interrupt handler before you disassemble anything. The
`M68kVectorTable.java` script types all 256 entries as pointers, which creates the references that
make otherwise-unreferenced handlers visible.

### The handlers that matter

| Vector | Offset | Value | What |
|---|---|---|---|
| 0 | 0x000 | 0x05C8 | initial SSP (a value, not a handler) |
| **1** | 0x004 | **0x1CFE** | **RESET PC - the firmware entry point** |
| 25-31 | 0x064-0x07C | | autovectors; **IRQ6 = 0x1B00**, **IRQ7 = 0x1DD8** are real handlers |
| 32 | 0x080 | 0x3498 | TRAP #0 - same handler as TRAP #2 |
| **34** | 0x088 | **0x3498** | **TRAP #2 - the PIOC-OS kernel entry** |
| 69 | 0x114 | 0x3A68 | **RTC / MFP timer ISR** |
| 78 | 0x138 | 0x250E | **ND-100 host doorbell** |

### Vector 78 is re-hooked at run time

`POMNPROCES` saves the static handler to **0x199E8**, reads a new pointer from 0x199EC and writes
it to 0x138. The installed handler at **0x7726** is hand-written, not PLANC:

```
movem.l {D0-D5/A0-A4},-(SP)
... walks an 8-slot ring anchored at (0x18A38).l, issuing trap 0x09 per armed slot ...
movem.l (SP)+,{...}
move.l (0x199E8).l,-(SP)
rts                              <- chains to the PREVIOUS handler
```

The `push saved vector ; rts` idiom is a tail-chain, not a return. An emulator must preserve the
chaining or the original 0x250E handler never runs.

### All processor faults funnel into TRAP #1

The 2-byte stubs at 0x1F24+ that vectors 4-11 point at are each **`4E 41` = `trap #1`**. So the
TRAP #1 handler (vector 33) is the fault reporter, and **the stub ADDRESS is what identifies which
fault occurred** - the stubs are not distinct routines and should not be named individually.

Most other vectors point into dense stub runs at 0x1F24-0x1F7A, 0x1F9C-0x211A and 0x4074-0x409A.
The ones at 0x4074+ are `rte` - do-nothing handlers.

---

## 2. Installing a vector at run time - trap 0x0F

`PosTrap0FInstallExceptionVector` @0x3396:

```
vec = arg[0]                                 word
if (vec < 8 || vec > 255) return error       validated 8..255
*(long *)(0 + vec*4) = arg.handler
return 1
```

Vectors 0-7 cannot be installed this way. This is the mechanism by which vector 78 gets hooked.

---

## 3. The host doorbell, both directions

| Direction | Mechanism |
|---|---|
| Card -> ND-100 | write **1** to **0x00EF0080** (SCIP). `post_and_signal_nd100_scip` @0x1A5C does this after bumping the postbox counters at 0x040A and 0x0410. Trap 0x19 also uses it |
| ND-100 -> card | vector 78, handler chain above |

The monitor request path (`04-SCHEDULER.md` section 8) dispatches on **SUBFN at 0x0408**, bounds
0..5, replying at **0x040C** - 1 on success, **-2** for an out-of-range SUBFN, **4** after a warm
restart.

---

## 4. Error handling

| Routine | Address |
|---|---|
| `POMNERHAND` | 0x704A |
| `SENDERROR` | 0x70CC |
| `GETALLERRO` | 0x72CA |
| `ANALYZEEVE` | 0x73A0 |
| `CONNECTERR` | 0x7412 |
| `INITERRORS` | 0x7528 |
| `FATALERROR` | 0x4C26 |
| `LnmaReportPosiErrorAndFatal` | 0x4D1A |
| `HARDWAREERROR` | 0x58E0 |
| `WATCHDOGDE` | 0x5AC6 |

`SENDERROR`, `GETALLERRO` and `INITERRORS` all reference the bank-3 message tables via
`lea (0x6xxxx).l` - the error TEXT lives in bank 3's data segment even though bank 3's code is
unreachable (`01-MODULE-INVENTORY.md` section 4).

### Hardware error codes

`LNMAHWERRO` carries a negative code; three are distinguished by dedicated counters:

| Code | Counter | Behaviour |
|---|---|---|
| -1 | 0x188BA | |
| -9 | 0x188C0 | escalates to `FATALERROR` |
| -10 | 0x188C2 | does **not** restart the MA |
| -5 (watchdog) | 0x188C4 | |

**The watchdog is a two-variable no-progress detector** - ring position and TX wrap count,
snapshotted at 0x18948/0x1894A. So `LNMAHWERRO = -5` means specifically "the transmitter made no
progress", not a generic hardware fault.

### The PLANC error path

Distinct from all of the above, and easy to confuse with it: `jmp (A5)` is the **PLANC error
unwind**, where A5 holds `#XRET` (0x135A8) or `#ERET` (0x13596). There are 851 of them in the image
and none is a dispatch. `#PRERR` raises a runtime error with a message table. See the
`ghidra-planc` skill.

---

## 5. Three dispatch defects worth knowing

| Where | Defect |
|---|---|
| `tbl_xroutSubfunctionDispatch` @0x1D190 | 4-bit index (0..15), only 9 entries. Indices 9-15 read the next table's maxindex word and jump to **0x07000000**. Peer-supplied, unchecked |
| `tbl_pocsInboundStateDispatch` @0x2D31C | 3-bit index (0..7), only 5 entries. Indices 5-7 read the ASCII `"POCS"` that follows and jump to **0x504F4353**. Peer-supplied, unchecked |
| `tbl_lnPrintMasNetTypeDispatch` @0x65FE0 | 8 entries, index correctly masked - but entries **5-7 are NULL** and reachable, because the guard constrains bits 5..12 while the index uses bits 13..15. Result: `jmp` to **0x00000000** |

The third is a different failure mode from the first two: **a bounds mask does not make a dispatch
safe if the table itself has holes.**

The host command dispatch (`g_hostCommandJumpTable` @0x18982) and the monitor SUBFN dispatch
(0x0512) are both properly checked and are **not** in this list.

---

## 6. Open

- The TRAP #1 fault reporter body - what it does with the stub address
- The `POMNERHAND` / `ANALYZEEVE` logic and the full error code list
- The 8-slot ring at (0x18A38).l walked by the vector-78 handler
- What IRQ6 (0x1B00) and IRQ7 (0x1DD8) actually service

---

## Provenance

The vector map, the trap #1 funnel and the three dispatch defects were established by reading the
image. The error-routine inventory is from the vendor symbol table; their behaviour beyond the
hardware error codes is not yet traced.
