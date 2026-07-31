# BM-FILERE-B02 - Backup-Manager file-restore (form-driven)

## Overview

BM-FILERE-B02 is a Norsk Data ND-500 full-screen, form-driven application. The
README index calls it a "binary file reader/editor"; however, the symbol and
string content of the DOM strongly indicates it is the FILE-RESTORE component of
the ND Backup Manager (BM) suite. [from disasm] Embedded names include
`BACKUP-MANAGER`, `BM-FILERESTORE`, `FILE-RESTORE`, `BM-SCHEDULER`,
`BM-OPERATOR`, `BM-DEFINITION`, plus device/tape and name-server references
(`MAG-TAPE-`, `READ-DEVICES`, `DP-NAME-SERVER`, `DMA-SERVER`). [from disasm] So
`BM-FILERE` most likely expands to "Backup Manager FILE REstore". This is
version B02. [verified] (folder name) The "binary file reader/editor" label is
the README's provisional description and is not confirmed by the binary.
[UNVERIFIED]

The program runs a "command/form" system (it prints `New command/form system
established.`) and works through named forms with typed fields. [from disasm]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `BM-FILERE-B02.DOM` - the runnable ND-500 domain (~1 MB). [verified]
  Self-contained (no PSEG/DSEG/HELP/INIT ships). [verified]

There is no `analysis/*.asm` disassembly for this program; the notes here come
from strings inside the DOM plus one verified load run. [verified] (analysis/ is
empty)

## Requirements

- The `.DOM` file to load and initialise its UI. [verified]
- XMSG must be available: the program issues `MON 513B` (B5XMSG) shortly after
  starting, which is not yet implemented in nd500x - so it stops there.
  [verified] See "Known issues" below.
- Its inputs are backup/dump files: prompts ask for a `.DATA file for dump`, a
  `.DSEG file for dump`, and a `.DATA file for input`. [from disasm]
- Install: copy `files/BM-FILERE-B02.DOM` into the sintran-root. See
  [../README.md](../README.md).

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
BM-FILERE-B02
```

Scripted drive from `~/repos/nd500x` (will only get as far as the XMSG stop, see
below):

```
printf 'LOGIN GUEST\nBM-FILERE-B02\n' | ./build/bin/nd500x --monitor \
    --user GUEST --sintran-root ~/ND500USERS
```

## Commands and options

This is a form-driven, full-screen tool, not a command-line one. Because it
stops at the unimplemented XMSG call before reaching normal operation, its
command/form interaction has NOT been observed. [verified]

Prompt strings embedded in the DOM (order and full flow UNVERIFIED): [from disasm]

- `:DATA file for input:` - the input dump/data file.
- `.DATA file for dump (must exist):` - a reply of `HOME` means no DATA file
  required.
- `.DSEG file for dump (must exist):` - a reply of `HOME` means no DSEG file
  required.

It also carries a crash/stack handler (`Hit CR(continue),P(prev. stack),N(Next
stack),L(Local data),E(exit):`) and form-management diagnostics
(`This form has already been defined`, `Attempt to define a too big form`).
[from disasm]

UNVERIFIED: the actual restore commands, the form names a user selects, and the
full field layout.

## Verified behaviour in nd500x

Verified 2026-07-31 in the `nd500x` C emulator: [verified]

- Loads, initialises its full-screen UI (clears the screen).
- Then issues `MON 513B` (B5XMSG, an XMSG call), which is not implemented, so
  execution stops at that point.
- Earlier it was also blocked on `MON 416B` (WSEGN); that call is now
  implemented and is no longer the blocker.

## Known issues / status

- BLOCKED: needs XMSG (`MON 512B`/`MON 513B`), which is deferred in nd500x. The
  program cannot proceed past its first XMSG call. [verified]
- Identity ("Backup Manager File Restore" vs the README's "binary file
  reader/editor") is inferred from strings, not confirmed. [UNVERIFIED]
- No disassembly present; interaction flow not observed. [verified]

## References

- Shared conventions: [../README.md](../README.md)
- XMSG background: [../../XMSG/](../../XMSG/)
- Runnable domain: [files/BM-FILERE-B02.DOM](files/BM-FILERE-B02.DOM)
