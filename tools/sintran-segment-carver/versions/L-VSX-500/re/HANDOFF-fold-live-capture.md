# Handoff prompt - capture the SINTRAN password fold live via DAP (Linux / nd100x)

> ## DONE / SOLVED (2026-07-10) - live capture is NOT needed
> The password fold was fully reverse-engineered **statically** from the corrected
> S3CP LOGIN disassembly and verified against real stored values:
> `acc = ROL16(acc,3) + toupper(char)` (a 16-bit fold). See **PASSWORD-ALGORITHM.md**
> in this folder for the algorithm, the disassembly, the tools (`sintran-passcrack.c`,
> `sintran-passdb.c`), and the security assessment. This DAP handoff is retained only
> as a record of the approach that was NOT ultimately required.

Paste everything below the line into an LLM running on **Linux** with nd100x + the
DAP/MCP debugger. The Windows/Ghidra side cannot reach ND-100 over DAP (the DAP
bridge only serves SDL2-CLI nuget runners and ND-100 is not ported there), so the
resident-memory capture has to happen on the Linux side.

All ND addresses are **octal** (hex given where useful). Radix: SINTRAN symbol
tables and docs are octal; the emulator/DAP may want hex.

---

## Goal
Reverse-engineer the SINTRAN III **password fold**: the routine that turns a typed
password string into the single 16-bit word stored per user. You have a running
system, so you can both breakpoint the code and generate known input/output pairs.

## VERIFIED facts (do not re-derive - established by static RE of the carved segments)
- Stored password = one **16-bit word** at user-entry **word 9** (byte offset 18),
  big-endian. Read live during login at S3CP octal **060571** (`LDA ,X 11`).
- **Known I/O pair**: password `FORD` -> stored word **octal 41620** (0x4390 =
  17296). Passwords are **case-insensitive**.
- The fold is **NOT a simple arithmetic per-character fold**. A broad brute force
  (ROL/ROR/SHL/SHR/byte-swap/multiply/pack; add and xor; both char orders; many
  seeds; preprocessings raw/&37B/&177B/&337B/&77B; space padding 0..8) cannot turn
  `FORD` into octal 41620 (harness self-checked). So it is most likely
  **table-driven** (substitution/CRC-style) or folds a padded/longer buffer.
- Login orchestration lives in **S3ISYS `PWLOG`** (octal **145734**; that segment
  loads at 144000). PWLOG stashes the typed password (`LDA ,B -200`), sets char
  count `SAA 17` (=15 max) at 145747, then calls the per-character routine through a
  **runtime linkage pointer table `PWLOG_linktab`** (octal **146031-146037**) via
  `JPL I 61` at octal **145750**. The linkage words are filled at load time, so the
  fold's real address is only knowable at runtime.
- The fold routine is **resident common code** - it is NOT in any carved SEGFIL0
  segment. (This was originally justified by a now-retired "segments are half
  length / `segle*512`" bug. The corrected model carves full-length `SEGLE*1024`
  words, so the segment windows now span up to 177777 - but the resident common
  data page 177600-177777 is baked as **zeros** into every segment image (verified:
  `OPWCH`/`PASST` read 000000 and the top page is identical across S3CP and S3FS).
  The resident common code/data is mapped at runtime via the PITs from separate
  files, not stored per-segment - see `../../../EXTRACTING-RESIDENT-CODE.md`.) So
  the fold's runtime values and its load-time-resolved linkage are observable only
  live, which is why this must be captured from live resident memory.

## Resident-memory pointers (the useful part)
The top resident page **177600-177777 is the resident DATA area** (its symbols are
variables: device regs `2TREG/2DREG/2XREG`, file-system state `WCOUN 177651`,
`FSTA1 177652`, `FDRIV 177657`, `FMEMD 177646`...). Two password-holding data cells
sit inside it:
- **`OPWCH` = octal 177650** ("Old PassWord CHange" - the working cell that holds a
  password word during login / change).
- **`PASST` = octal 177606** ("PASsword Temp").

Resident login/user **code** candidates (disassemble these once mapped):
- `USERD` = octal 176254   (user-directory access)
- `LOGSE` = octal 176146
- `BCLOG` = octal 176112
- `D8LOG` = octal 170271, `8LOGL` = octal 170272

## Where the resident code/data lives on disk
The resident common code (`SINTRAN:DATA`) and resident data are extracted by raw
block range (they are the `madr==0` region the segment carver skips) - the full
procedure, the section-8.1 disk-layout table, and the extracted artifacts are in
`../../../EXTRACTING-RESIDENT-CODE.md`. Disassemble the resident common-code image
around the resident login symbols (`USERD`/`LOGSE`/`BCLOG`) and the routine that
`PWLOG_linktab` points to. Because the linkage is resolved at load time and the
folded value lives in the runtime-only resident data page, the DAP capture below is
the reliable path.

## DAP plan - two independent ways to find the fold code
**A. Data-watchpoint (most direct).** Boot the L-VSX-500 image. Set a **write
watchpoint on `OPWCH` (177650)** and on `PASST` (177606). Perform a login or
`@SET-PASSWORD` with a known password. The instruction that writes the folded 16-bit
value into `OPWCH`/`PASST` **is inside (or one return from) the fold routine** -
disassemble outward from where the watchpoint fires. Dump any constant/substitution
**table** the loop indexes.

**B. Linkage-resolution.** Set an execution breakpoint at **`PWLOG` = 145734**.
Trigger a login. Single-step to the `JPL I 61` at **145750** and read the resolved
target of `PWLOG_linktab` (live word at 146031) - that is the fold routine's mapped
address. Disassemble it and the resident login candidates above.

Either way, **verify**: the recovered algorithm must map `FORD` -> octal 41620 and
be case-insensitive.

## Fastest path if disassembly is slow - behavioural solve
Set these passwords, read back each stored octal word (same method that gave
FORD->41620), and send the pairs back; the fold can then be solved/falsified
purely from behaviour, no resident code needed:

| Password | Reveals |
|----------|---------|
| `A`      | seed + single-char contribution |
| `AA`, `AAA` | the repeated step (shift/rotate amount, or table growth) |
| `B`, `AB`, `BA` | per-char weight and character order |
| `ford`   | must equal `FORD` (octal 41620) -> confirms case-insensitivity |
| `FOR`, `FORDS` | length / padding effect |

## Report back
Append findings to
`tools/sintran-segment-carver/versions/L-VSX-500/re/README-password-login.md`
under a new "Fold algorithm (VERIFIED live)" section: the algorithm + any table, the
resident address of the fold routine, and the check that it yields octal 41620 from
`FORD`. Or just return the input/output pairs.

## Reference files (Windows repo; same paths under /mnt/e on WSL)
- `re/README-password-login.md` - full RE notes + login/conversion mermaid diagrams
- `re/045-S3ISYS.dis` - PWLOG / CPSW disassembly (base octal 144000)
- `re/003-S3CP.dis` - login path incl. the 060571 stored-word read (base octal 030000)
- `segments/manifest.json` - segment load addresses and sizes
- SYMBOL tables: `SINTRAN/NPL-SOURCE/SYMBOLS/L07/*.SYMB.TXT`
