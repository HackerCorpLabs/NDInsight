# MON 312B (MOINF) and MON 317B (UECOM) - Behavioral Oracle for NC

Purpose: give another team's emulator the REAL SINTRAN III VSX/500 **L07** behavior of
two monitor calls that NC issues. Every claim below is tagged **VERIFIED** (grounded in
carved L07 bytes and/or the matching official manual) or **INFERRED** (reasoned, not
byte-proven). No guessing.

Carved L07 sources used:
- Monitor-call table: `MCTAB/9MCTA @ 005620B` in segment `044-S3IDPIT` (the real
  per-call entry addresses).  Level-14 table: `MGOTA/GOTAB @ 071233B` in the monitor-PIT
  segments `017-S3SMPIT` / `026-S3IMPIT`.  (NOT in `SINTRAN-DATA_commoncode` - see the
  correction notice below.)
- ND-500 System Monitor segment 030-S3SM5 (`re/segments-ref/030-S3SM5/`),
  segment 026-S3IMPIT (`re/segments-ref/026-S3IMPIT/`).
- Official contract: `Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md`
  and `Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md`.

---

## Dispatch-table facts (both calls confirmed present in L07)

> **CORRECTED 2026-07-13.** An earlier revision of this document reported
> `MON 312B entry = 112277B` and `MON 317B entry = 112242B`, read from the
> `SINTRAN-DATA_commoncode` carve at `071233B`. **Those numbers were wrong** and must not
> be used. Commoncode's `071233B` is *not* the GOTAB (its slot 0 is `000000`, not the
> illegal-call handler; its slot 1 is `120303B`, not `M1 = 071633B`). The `112xxx` values
> were unrelated bytes sitting at that address in a different overlay. The corrected,
> byte-verified numbers are below. Full derivation:
> `re/mon-analysis/317B-ExecuteCommand/README.md`.

SINTRAN dispatches a monitor call through **two** tables, not one:

1. **`MGOTA` / `GOTAB` @ `071233B`** - the *level-14* table, 256 words, indexed by MON#.
   It lives in the monitor-PIT segments (`017-S3SMPIT` / `026-S3IMPIT`), which is also
   where the level-14 entry `ENT14 @ 072167B` actually lives. Only **32 of its 256 slots**
   are resident fast handlers (MON 1B read, 2B write, 21B-24B, 63B, 163B, 200B XMSG, 310B,
   346B-377B). The other **224 slots hold `MFELL = 072114B`**, which is *not* an error
   path: it hands the call to the monitor program level (it writes `CALLP = 032201B` into
   that level's P register and activates it).
2. **`MCTAB` / `9MCTA` @ `005620B`** - the *monitor-level* Monitor-Call TABle, 256 words,
   indexed by MON#, in segment `044-S3IDPIT`. **This is the table that holds the real
   monitor-call entry addresses**, and it is the datum MON 312B reports. 216 of its 256
   slots are populated and every populated slot lands exactly on a named L07 symbol.

| MON#   | GOTAB slot (`071233B`+N) | GOTAB word | MCTAB slot (`005620B`+N) | MCTAB word = **entry** | Meaning |
|--------|--------------------------|------------|--------------------------|------------------------|---------|
| 312B   | `071545B`                | `072114B` (MFELL) | `006132B`         | **`032600B` = `MOINF`** | MON 312B IS present |
| 317B   | `071552B`                | `072114B` (MFELL) | `006137B`         | **`050701B` = `UECOM`** | MON 317B IS present |

**VERIFIED** (bytes, in `tools/sintran-segment-carver/versions/L-VSX-500/segments/`):
```
dd if=044-S3IDPIT.bin bs=1 skip=2228 count=2 | od -An -tx1   ->  35 80   (= 032600B = MOINF)
dd if=044-S3IDPIT.bin bs=1 skip=2238 count=2 | od -An -tx1   ->  51 c1   (= 050701B = UECOM)
```

**VERIFIED**: both MON 312B and MON 317B are implemented in this L07 build. Their monitor-call
entry addresses are **`032600B` (MOINF)** and **`050701B` (UECOM)**.

Correction to the earlier note about `MOINF @ 32600B`: that symbol is **not** stale. It is
the genuine MON 312B entry - it is exactly the value carved in `MCTAB[312B]`. The earlier
note dismissed it only because it was checked against the wrong image (commoncode) rather
than against `MCTAB`.

---

## CALL 1 - MON 312B = MOINF = CheckMonCall

### What it does (VERIFIED - manual p.108-109 + carved MCTAB)
A **capability query**: "is monitor call N present in this SINTRAN III system, and if so
what is its dispatch entry?" Optional monitor calls are included/omitted when SINTRAN is
generated; MOINF lets a program probe for one before using it. This is exactly why NC
calls it 6 times - it is probing 6 optional MON calls it wants to use.

The returned datum is the monitor call's **entry address** - i.e. the value stored in
**`MCTAB[N]`** (`005620B + N`), shown above. `0` = not implemented. (It is *not* the
`GOTAB[N]` word: for 224 of the 256 calls `GOTAB[N]` is just `MFELL`, the same value for
all of them, so it could not distinguish one call from another.)

**This is now byte-proven, not inferred.** The MOINF handler is `032600B` in segment
`026-S3IMPIT` (it is dispatch machinery, next to `CALLP=032201B`; do NOT read `032600B` in
the `003-S3CP` overlay - it is ASCII data there). Its disassembly:
```
032600  IRR 10 DA          ; A := caller's MON number N
032601  LDT 35             ; T := MEM[032636B] = 000400B = 256  (bound)
032602  SKP IF DA MLST ST  ; N < 256 ?
032604  RADD CLD SA DX     ; X := N
032605  LDA I ,X 32        ; A := MEM[ MEM[032637B] + N ] = MEM[005620B + N] = MCTAB[N]
032610  IRW 10 DA          ; hand the entry (or 0) back to the caller's A
032611  JAZ 4              ; entry == 0  -> normal return (not implemented)
032613  AAA 1 ; IRW 10 DP  ; entry != 0  -> caller P += 1 = SKIP return (implemented)
```
`MEM[032637B] = 005620B` = the MCTAB base (dd-verified). So MON 312B literally indexes
`MCTAB` by the MON number, returns that word, and skip-returns iff it is non-zero.

### Register / ABI contract (VERIFIED - ND-100 MAC form, manual p.109)
```
    LDA  MONNO      ; A := monitor-call number to test    (INPUT: A)
    MON  312        ; CheckMonCall
    ...             ; NORMAL return  => NOT implemented (A/entry = 0)
    STA  ENTRY      ; SKIP  return  => implemented; A = monitor-call entry (non-zero)
```
- INPUT: **A register** = monitor call number.
- OUTPUT uses the ND-100 **skip-return** convention:
  - **no-skip (normal) return  => call NOT present**; entry value = `0`.
  - **skip return (PC+1)       => call present**; **A = entry address** (non-zero,
    the `MCTAB[N]` word).
- PLANC view (`ND-60.117.5`, "MON312 - MOINF (ND-100 only)"):
  `ROUTINE VOID,BOOLEAN (INTEGER) : MN312 (num)` - argument = call number, result =
  BOOLEAN present/not. The two-parameter form `CheckMonCall(MonCallNumber, MonCallEntry)`
  writes the entry address into the second parameter (`0` = not implemented).

### Real return value for the inputs NC passes
For each of the 6 numbers NC probes:
- If NC's target build has the call: **skip-return, A = the non-zero MCTAB entry**
  (e.g. for 312B itself A=`032600B`; for 317B A=`050701B`).
- If not: **normal return, A = 0**.

### Why the other team's fakes are WRONG
- Returning a fixed `0x4C` (=114B) for every number: wrong - that is neither a
  per-call entry address nor 0, and it never toggles the skip/no-skip that signals
  presence. NC would mis-detect capabilities.
- Returning `0xF8000000 + num`: wrong - MON 312B returns a **16-bit** entry word (or 0),
  never a 32-bit sentinel, and again ignores the skip-return boolean.
- Correct minimal emulation: for a MON number the emulator actually implements, take the
  **skip return** and put a **non-zero** value in A (any non-zero stand-in for the entry
  works if NC only tests presence; use the true MCTAB entry if NC dereferences it). For an
  unimplemented number, take the **normal return** with **A = 0**.

---

## CALL 2 - MON 317B = UECOM = ExecuteCommand

### What it does (VERIFIED - manual p.180-181)
Executes a **SINTRAN III command line as if typed at the terminal** (without the leading
`@`). The command name and its parameters are passed as one text string. Key guaranteed
behaviour:
- **Errors do NOT terminate the calling program** - an error message is printed and
  control returns. (Contrast MON 70B COMND / CallCommand, which terminates on error;
  the manual explicitly recommends UECOM over COMND for this reason.)
- Runs **synchronously**: control returns after the command completes. (Manual advises
  inserting a SuspendProgram between two dependent ExecuteCommands, e.g. CreateFile then
  OpenFile - confirming each call is a discrete synchronous action.)
- Command **output goes to the terminal** (e.g. `@LIST-FILES`).
- **Missing parameters are prompted for** on the terminal.
- Availability: "ND-100 and ND-500, All users, Background programs".

### Register / ABI contract (VERIFIED - manual p.180-181)
ND-100 MAC form:
```
    LDA  (CMND      ; A := address of the command string   (INPUT: A = string pointer)
    MON  317        ; ExecuteCommand
    ...
CMND, 'CLOSE-FILE 102'   ; the SINTRAN command text
```
- INPUT: **A register = address of the command string** (ND-100). High-level bindings pass
  a fixed-length text field: PASCAL `PACKED ARRAY[0..34] OF CHAR`, FORTRAN `CHARACTER*35`,
  PLANC `BYTES : Command(0:35)`.
- ND-500 form (manual): `ExecuteCommand : EQU 37B9 + 317B` /
  `CALLG ExecuteCommand, 1, Command` with `Command : STRING 35` - i.e. one string
  descriptor argument.
- The string is parsed by the standard SINTRAN command decoder (same one the terminal
  command processor uses): shortest-unique-prefix command matching, then dispatch.

### ND-500-side handler (VERIFIED symbols; body not disassembled here)
`UECOM` is present as an **N500-SYMBOLS** entry at **125752B** in both segment
030-S3SM5 and 026-S3IMPIT. The carved SINTRAN NPL notes (`SINTRAN/ND500/CC-P2-N500.md`)
document `5RETUECOM` - "Change PITs before returning after MON UECOM (317)" / "handle
return from user escape command" - i.e. the ND-500 monitor swaps page-index tables around
the call and returns normally to the ND-500 caller. This confirms MON 317B is a real,
returning (non-terminating) call on the ND-500 side that NC uses.

### The command NC runs: "DEFINE-CAT-COPY" (define ... cat ... copy)
**INFERRED / partially unverifiable.** There is no single SINTRAN command literally named
`DEFINE-CAT-COPY`. The carve does not contain the exact string NC passes, so the precise
command cannot be byte-confirmed here. What IS verifiable:
- SINTRAN has real commands `@CREATE-FILE <name> <pages>`, `@COPY-FILE <dest> <src>`, and
  a family of `@DEFINE-...` commands (e.g. define-synonym / define segment symbol), all of
  which produce **persistent side effects** (a new/catalogued file, a copied file, a
  defined name). Files are created and catalogued in the user's directory; `@COPY-FILE`
  can create the destination if it is quoted.
- Therefore, if NC's string resolves to a define/create-and-copy sequence, the SIDE EFFECT
  NC depends on is a **real filesystem mutation** (a file gets defined/created and its
  contents copied), performed synchronously before UECOM returns.

**What the handler guarantees regardless of the exact command** (VERIFIED): UECOM parses
and runs the given command line synchronously through the SINTRAN command processor;
on error it prints a message and **returns without terminating NC**; any output is emitted
to the terminal device. For a faithful oracle, the emulator must actually perform the
command's side effect (e.g. create/copy the file) if downstream NC logic reads it back -
returning "OK" without doing the filesystem work will diverge from real L07.

---

## Summary of real return behaviour
- **MON 312B / MOINF**: input A = MON#; **skip-return + non-zero entry** if the call
  exists (entry = `MCTAB[MON#]`, carved: **312B -> 032600B, 317B -> 050701B**),
  **normal-return + A=0** if not. It is a presence/capability probe, not a constant.
- **MON 317B / UECOM**: input A = pointer to a command string; runs that SINTRAN command
  line synchronously, prints errors without terminating NC, and its side effect is whatever
  the command does (file create/copy/define = persistent filesystem change).
