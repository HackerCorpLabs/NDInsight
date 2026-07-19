# ND-500 SYSTEM MONITOR carve (S3SM5) - the "more than MON 60" code

This is the code the MON 60B / N500M worker hands off to via the `5NOPAR` common path
(`FPT2ENTRY`). It is the ND-100-resident **ND-500 system monitor**: it takes the packaged moncall,
builds the 5MPM message, and drives the ND-500. Carving it is the convergent next target of both the
worker-side (`../mon-analysis/60B-N500M/`) and caller-side (`nd-500-mon:prog`) efforts, and the core
of the emulator Phase 1 (5MPM handshake) / Phase 3 (bus interface, bring-up).

**Status:** **COMPLETE (2026-07-15)** - entry + base validated, dispatch table, IOX driver + register
map, control-store gate, 5MPM message + activation, level-12 return path, AND all ~60 FUNCS operation
bodies carved and byte-verified. The worker side is [`../mon-analysis/60B-N500M/`](../mon-analysis/60B-N500M/README.md).

## Location + entry chain (byte-verified)
- Segment **`030-S3SM5`** ("ND-500 System Monitor"), load base **`040000B`** (16384 dec), 48 pages
  (`040000B`-`177777B`). Bytes: `../../segments/030-S3SM5.bin`.
- `MON 60B N500M 5NOPAR` -> **`FPT2ENTRY = FP2ENT = FPT2E = 040003B`** (a `125001 JMP I 1` trampoline)
  -> **`5FP2E = 142231B`** (the system-monitor entry body).
- The `FPT2` name = "enter page-table-2 (PIT2)" context - the system monitor runs in PT2 per the
  architecture doc (`SINTRAN\ND500\MON\...` / `60B_N500M_Functions.md`).

## Base/overlay validation (why this is the right overlay - unlike the N500M scare)
1. `030-S3SM5`'s own symbol file is **N500-SYMBOLS**, and it **contains both** `FP2ENT=040003B` and
   `5FP2E=142231B` (self-consistent - the meta symbol_file matches the actual symbols here).
2. The code at `142231B` is **coherent** (frame build + `SAT 15/16` function dispatch via `JPL I`).
3. The known 5xx level-12 handlers sit in the same `142xxx` region (`A5XMS=142253B`, `SWMC=142153B`,
   `5MTRA=143445B`, ...) - so this region is genuinely the ND-500 system monitor + its handlers.
4. Release-doc PIT layout maps "ND-500 system monitor" at 5PIT page 20 = `40000B` (the S3SM5 base).

See [`ND500-SYSMON-ENTRY.ASM`](ND500-SYSMON-ENTRY.ASM) for the trampoline + entry disassembly.

## THE FUNCS dispatch table (the ND-500 operations) - see [`FUNCS-dispatch-table.md`](FUNCS-dispatch-table.md)
`FUNCS = 142031B` (128 entries, indexed by subfunction code) is the server-side twin of the worker's
`5IFUNC`: `5IFUNC[N]` marshals params, **`FUNCS[N]` performs the ND-500 operation**. Every entry lands
on a named N500-SYMBOLS routine (`REGRE`, `PMWRI`, `CSLOA`, `RSTAT`, ...). This maps the ENTIRE MON 60B
path end to end with named routines at every hop. Byte-verified from `030-S3SM5.bin`.

## Entry body structure (5FP2E @ 142231B - from bytes)
Loads a set of fields into its frame (`LDA 111; STA ,B -57; ...`), reads the moncall descriptor
(indexed loads `LDX ,X 22; LDA ,X 7`), computes an index (`AND 65; ADD 65`), then dispatches on a
function selector via `SAT 15 / SAT 16 / SAT -3` compares and `JPL I` calls. This is the system
monitor's own function dispatch on the moncall that N500M packaged onto the ND-500 data segment.

## THE 3022 IOX BUS INTERFACE - see [`ND500-3022-IOX-INTERFACE.md`](ND500-3022-IOX-INTERFACE.md)
The low-level ND-100 <-> ND-500 hardware access lives at `051023B`-`052070B` in this segment: routines
`WADR`/`WRDAT`/`RDATL`/`REDAT`/`WRTAG`/`GETMA` that hit the **3022 interface device via `IOXT`**
(`LDX ,B -56; LDT ,X -3; AAT <off>; IOXT`). **Confirms Q6 from bytes** (IOX is in resident SINTRAN,
not the caller). `RSTAT`'s status read is `IOXT dev+2` - the prime candidate for the "control store
loaded?" gate. This is the emulator's Phase 3 bus interface. Register-offset map is VERIFIED; per-
register meanings are the next tracing step (cross-check vs `SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md`).

## CARVE STATUS (2026-07-15)
Deliverables in this folder (byte-verified against `030-S3SM5` unless noted):
- [`FUNCS-dispatch-table.md`](FUNCS-dispatch-table.md) - the 128-entry ND-500 operation table (twin of `5IFUNC`).
- [`ND500-3022-IOX-INTERFACE.md`](ND500-3022-IOX-INTERFACE.md) - the 3022 IOX driver + register map, byte-validated both ways.
- [`ND500-CONTROL-STORE-GATE.md`](ND500-CONTROL-STORE-GATE.md) - **the emulator fix** (RSTA5 bit 9 5CLOST clear).
- [`ND500-5MPM-MESSAGE-AND-ACTIVATION.md`](ND500-5MPM-MESSAGE-AND-ACTIVATION.md) - message layout + ACT50 activation.
- [`ND500-SYSMON-ENTRY.ASM`](ND500-SYSMON-ENTRY.ASM), [`ND500-3022-IOX-DRIVER.ASM`](ND500-3022-IOX-DRIVER.ASM) - disassemblies.
- [`FUNCS-BODIES/`](FUNCS-BODIES/README.md) - **ALL ~60 FUNCS operation routine bodies**, grouped into
  7 annotated `.ASM` files (register/memory, control-store/micro, segload primitives, file/process,
  memconfig/reserve, name-segment/process, domain/trace/CPU), byte-verified vs `030-S3SM5`.

**The ND-100 <-> ND-500 interface is now mapped end to end and byte-verified.** The emulator has
concrete answers: the FUNCS/5IFUNC dispatch, the IOX register map, the control-store gate fix, the
5MPM message + activation sequence, and the level-12 return path.
- [`ND500-LEVEL12-RETURN-PATH.ASM`](ND500-LEVEL12-RETURN-PATH.ASM) - the ISR chain
  `5STDR=135010 -> CHN5S=135205 -> DECOM=135361 -> MCHAN=137206`, in **`026-S3IMPIT`** (the RESIDENT
  interrupt PIT, base `32000B` - NOT `030-S3SM5`; overlay verified by code coherence). `DECOM` reads
  the message via `LDATX` and dispatches on the MICFU code (`SAT 24`=`3MONC` monitor-call,
  `SAT 25`=`3TRAC` trace) - byte-verified.

## HONEST COMPLETION ASSESSMENT (2026-07-15)
**The ND-100 <-> ND-500 interface MECHANISM is fully carved and byte-verified, both directions:**
1. command path: caller thunk -> gateway -> MON 60 -> N500M `5IFUNC` (param prep) -> `5NOPAR` ->
   `FPT2ENTRY` -> `5FP2E` -> `FUNCS[code]` -> ND-500 operation;
2. hardware: the 3022 IOX register map (validated vs the manual);
3. the control-store gate (`RSTA5` bit 9 `5CLOST`) with the exact emulator fix;
4. activation: mailbox message + `ACT50` (MAR + CONTROL);
5. answer path: level-12 ISR `5STDR -> CHN5S -> DECOM -> MCHAN`, dispatch on MICFU.

**What remains is mechanical body-listing, not new mechanism:** the full per-instruction disassembly
of each individual `FUNCS` operation routine (`REGRE`, `PMWRI`, `CSLOA`, ...) and the full ISR-chain
bodies. That is completeness/depth work; the emulator has everything it needs to implement the
interface from the docs in this folder.

## What was carved (all four originally-planned next steps are DONE 2026-07-15)
1. `5FP2E`'s dispatch targets traced - see `ND500-SYSMON-ENTRY.ASM` + [`FUNCS-dispatch-table.md`](FUNCS-dispatch-table.md).
2. **The 5MPM message build** (how the monitor writes `MCNO`/`STOPR`/`NUMPA`/params into the message
   block and signals the ND-500) - [`ND500-5MPM-MESSAGE-AND-ACTIVATION.md`](ND500-5MPM-MESSAGE-AND-ACTIVATION.md).
3. **The level-12 return path** (`5STDR`/`CHN5S`/`DECOM`/`MCHAN`) - [`ND500-LEVEL12-RETURN-PATH.ASM`](ND500-LEVEL12-RETURN-PATH.ASM).
4. The FUNCS operation routine bodies (incl. the symbol-pinned `A5XMS` region) - [`FUNCS-BODIES/`](FUNCS-BODIES/README.md).

Remaining is depth/completeness only (full per-instruction ISR-chain bodies), not new mechanism.

Prior caution (now addressed): a previous pass called `030-S3SM5` "cannot enumerate with confidence"
(~53% opcode-0). That was whole-segment linear disassembly; carving from validated symbol entry points
(`5FP2E` and the N500-SYMBOLS addresses) gives coherent code, as shown.
