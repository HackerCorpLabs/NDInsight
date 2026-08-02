# ND-500/5000 Monitor J04 (MON-DEBUG:PROG) - Reverse-Engineering

Analysis of `nd-500-mon-j04.prog`, the ND-100 program "ND-500/5000 MONITOR Version J04
88. 6.16 / 88. 8.17" (embedded name `MON-DEBUG:PROG`). It runs on the ND-100 and is the
operator's front end to the ND-500 (`@nd-500` -> `N500:` prompt) - the program that loads
code into the ND-500 and drives it across the bus interface.

All repo paths below are relative to the repo root `E:\Dev\Ronny\NDInsight`. The ORIGINAL
source package (disk image + the `.prog` original) remains outside the repo at
`/mnt/d/ND/500/ND-500(0) System Package for SINTRAN IIIVSX L/`.

---

## Start here

`SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.md` - the full analysis: container/memory
layout, entry point, the single `MON 60` gateway, the subfunction map, and the end-to-end
bus path.

## The one big finding

The monitor **never touches the ND-500 bus hardware itself** (zero `IOX`/`IOXT`). It drives
the ND-500 through **exactly one `MON 60` (N500M) instruction at address `146256B`**, fed by
a fan-in of *159 call sites -> 123 thunks -> 1 gateway -> 1 MON 60*. Below the `MON 60`, the
3022 IOX registers, the mailbox, the TAG handshake, and the level-12 interrupt all live in
the privileged SINTRAN driver. On the ND-100 side the front door is `MON 60`/N500M ->
**FPT2ENTRY ("ENTER ND-500 SYSTEM MONITOR")** -> the 5MPM shared-memory message -> the ND-500.

---

## Files

| File (repo-relative) | What it is |
|----------------------|------------|
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog` | Original SINTRAN III two-bank `:PROG` binary |
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm` | ND-100 disassembly (byte-swapped to big-endian, base 0, ~65460 lines) |
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.md` | Full analysis (start here): MON 60 gateway, subfunction map, diagrams |
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04-bank1.bin` | Big-endian program bank image - load into a big-endian ND-100 Ghidra at address 0 (entry at address 9) |
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04-bank2.bin` | Big-endian data bank image (alternate page table) |
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04-symtab1.sym` | Recovered symbol-table RESIDUE #1 (block-0 tail) |
| `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04-symtab2.sym` | Recovered symbol-table RESIDUE #2 (inter-bank padding) |
| `SINTRAN/ND500/nd-500-mon/nd-500-control-store-debug-handoff.md` | RetroCore emulator control-store / TAG-OUT DMA "Unmapped memory" crash analysis |
| `SINTRAN/ND500/nd-500-mon/ND500-BRINGUP-BUS-INTERFACE-FEEDBACK.md` | Bootstrap + bus-interface synthesis for the CPU-connect goal |
| `SINTRAN/ND500/nd-500-mon/COMMAND-DISPATCH-TABLE-CARVED-2026-08-02.md` | **The outer command dispatch table** `[V]` - how an operator command name reaches its handler, plus the complete 151-command name -> handler map. Closes open question 9 |
| `SINTRAN/ND500/nd-500-mon/RECOVER-DOMAIN-WORKER-AND-SEGMENT-LOAD-CARVED-2026-08-02.md` | **The PLACE call graph** `[V]` - RECOVER-DOMAIN -> handler -> worker -> START-PLACE / ISEGLOAD x2 / END-PLACE, all 159 MON 60 thunk call sites mapped, and the proof that the segment-load defect is NOT monitor-side |
| `SINTRAN/ND500/nd-500-mon/mon60-callers/` | The MON 60 carve (see below) |

Note: a `:PROG` file carries **no** symbol table; the two `.sym` files are a leaked
fragment of the MON-DEBUG build's own symbol table found in padding, kept because it names
the ND-500 command routines.

## mon60-callers/ - the MON 60 carve

- `SINTRAN/ND500/nd-500-mon/mon60-callers/INDEX.md` - the mechanism + operator-command -> subfunction mapping
- `SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md` - every subfunction `000B`-`177B` with its
  authoritative purpose and server handler (from the SINTRAN worker source)
- **101 per-subfunction folders**, each with `<name>.asm` (annotated caller listing), `<name>.pseudo.c`, and
  `README.md`. Eight are named by operator command (`LOAD-CONTROL-STORE/`, `LOAD-SWAPPER/`, `START-SWAPPER/`,
  `STATUS/`, `START-STANDARD-DOMAIN/`, `LIST-STANDARD-DOMAINS/`, `LIST-SYSTEM-PARAMETERS/`,
  `LIST-EXECUTION-QUEUE/`); the rest are `<CODE>B-<MNEMONIC>/`.

---

## SINTRAN worker side (the ND-100 resident that services `MON 60`)

The counterpart carves of the resident SINTRAN worker are under the segment-carver tree:

- `tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/60B-N500M/` - the MON 60 worker handler bodies
- `tools/sintran-segment-carver/versions/L-VSX-500/re/MON-CALL-INDEX.md` - master MON-call index
- `tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-HANDLERS-OVERLAY.md` - the level-12 ND-500 handlers overlay
- `tools/sintran-segment-carver/versions/L-VSX-500/re/030-S3SM5-routine-map.md` - the ND-500 System Monitor routine map

The `SUBFUNCTION-TABLE.md` purposes come from the worker source
`SINTRAN/NPL-SOURCE/NPL/5P-P2-MON60.NPL` (its `5IFUNC` dispatch array).

## Related

- `SINTRAN/ND500/swapper/` - the ND-500-side swapper domain the monitor places and starts
- `SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md` - the authoritative 3022/5015 bus-interface reference
- `SINTRAN/ND500/ND500-STATUS-AND-INDEX.md` - the ND-500 subsystem status hub

**Parent:** `SINTRAN/ND500/README.md`
