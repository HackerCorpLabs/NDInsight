# ND-210260 — NORTEXT-100 Book Pagination

> Status: IN-PROGRESS — real floppy set decoded, real installer present (not decoded — compiled)

| Field | Value |
|-------|-------|
| ND article number | `ND-210260` |
| Product name | NORTEXT-100 Book Pagination |
| Functional category | Office — NORTEXT (typesetting/publishing) |
| CPU target | ND-100 |

## What is known — real floppy set, decoded

Three floppies, `210260K02-XX-01D`, `210260K02-NO-02D`, `210260K01-EN-02D` (downloaded via
NDwiki, imaged by Torfinn "Tingo" Ingolfsen). Real file inventory:

```
INSTALL-NTXPG-K0:XCOM          installer command file
NTXPG-INSTALL2-K:XCOM          second-stage installer command file
(RELEASE-NTX)PG-PAGINATE-K66:PROG / :ENTR    the pagination program + its entry-point table
(RELEASE-NTX)PG-EDITOR-K66:PROG / :ENTR      an editor program + entry-point table
(RELEASE-NTX)PG-MAIN-NO-K66:PROG / :ENTR     Norwegian-language main program
(RELEASE-NTX)PG-NO-J:INFO                    Norwegian version-info file
```

Note the non-default user `RELEASE-NTX` these files ship under — a real, confirmed convention for
NORTEXT-100 release media. `210260K01-EN-02D` mounted with a page-alignment warning (trailing
partial page) but was still readable.

## Installation — real, decoded `:XCOM` scripts (a genuine ND install scripting language)

Source: byte-for-byte decode (`byte & 0x7F`), both files in full. [decoded from real files] This
is the first real, complete example in this catalog of ND's `XCOM` command-file language — a
proper scripting language with variables (`^NAME:= value`), loops (`^LOOP:`/`^GOTO`), conditionals
(`^IF ... THEN ^GOTO`), user-defined subroutines (`^CALL name`/`^RETURN` with positional
parameters `<P0>`), and interactive prompts (`^ASK VAR/'default' text:`) — distinct from `:MODE`
(SINTRAN command replay), `:BATC` (batch queue jobs), and `@DMAC`/`@RT-LOADER` (kernel-level
scripting) seen elsewhere in this catalog.

**Bootstrap, `INSTALL-NTXPG-K0:XCOM`:**
```
^ND_DIR:= '210260'
^INSTALL_JOB:== 'NTXPG-INSTALL2'
^ENTER 'SYSTEM'
^IF '<_USER>' .EQS. 'SYSTEM' THEN ^GOTO USER_OK
   ^TYPE ERROR: You must be logged in as user SYSTEM to use this program!
   ^EXIT
^USER_OK:
@BACKUP-SYSTEM
COPY-USERS-FILES D , , D <ND_DIR> FLOPPY-USER <INSTALL_JOB> L
EXIT
^EXECUTE_MODE
^IFDEF INSTALL_ALL THEN ^EXIT
^CALLFILE <INSTALL_JOB>
^ABORT
```
Reading it: checks the calling user is `SYSTEM`, then uses SINTRAN's `@BACKUP-SYSTEM`
`COPY-USERS-FILES` command to copy the real second-stage script off the floppy, then
`^CALLFILE`s it. `^IFDEF INSTALL_ALL` is a hook for a larger combined-product installer that
`^CALL`s this file as a step — this script also works standalone.

**Second stage, `NTXPG-INSTALL2-K:XCOM` (abridged — full structure, PART 2's file list trimmed for
space):**
```
^ND_DIR:= '210260'
^PART1:= '01'
^PART2:= '02'

^PART1_FILE1:= 'PG-PAGINATE:PROG'
^PART1_FILE2:= 'PG-PAGINATE:ENTR'
... (PG-EDITOR, PG-UTILITY, PG-JULIE :PROG/:ENTR pairs, DUMP-PG:XCOM)
^MAIN_FILE1:= 'DESCRIBE-PG:PROG'
^MAIN_FILE2:= 'REGISTER-PG:PROG'

^CALL NEW 'RELEASE-NTX'
^I=1
^LOOP1:
^IFNOTDEF PART1_FILE<I> THEN ^GOTO ENDLOOP1
   ^@DELETE-USERS-FILE <PART1_FILE<I>> N
   ^I=I+1
   ^GOTO LOOP1
^ENDLOOP1:
@BACKUP-SYSTEM
COPY-USERS-FILES D , , D <ND_DIR>--<PART1> , , L
EXIT
^EXECUTE_MODE

^CALL NEW 'MAINTENANCE-NTX'
... (same delete-old-then-copy-new pattern for MAIN_FILE1/2)
^@RELEASE-DIRECTORY <ND_DIR>--<PART1>

^TYPE The PART-01 floppy is released and may now be removed.
^ASK DUMMY Insert the PART-02 floppy, and then press RETURN:
^@ENTER-DIRECTORY <ND_DIR>--<PART2> F-D-1 0
^IF <_COMPLETIONCODE> .NEQ. 0 THEN ^@ENTER-DIRECTORY <ND_DIR>--<PART2> F-D-1 1
^IF <_COMPLETIONCODE> .NEQ. 0 THEN ^ABORT

% Find the 2 language letters from one file on the floppy.
^OPENR FILE_ID0 '(<ND_DIR>:RELEASE-NTX)PG-MAIN:PROG'
^TEST_NAME:= '<_FILENAME(FILE_ID0)>'
^CLOSE FILE_ID0
^LAND:== TEST_NAME(9:10)

^PART2_FILE1:= 'PG-MAIN-<LAND>:PROG'
... (PG-MAIN-<LAND>:ENTR, PG-<LAND>:INFO/:HELP, PG-BATCH-1/2/3:PROG/:ENTR, APPEND-BATCH-PG:XCOM)

^CALL NEW 'RELEASE-NTX'
... (same delete-old-then-copy-new pattern for PART2 files)

^CALL NEW 'WORKFILES-NTX'
^OPENR FILE_ID2 'DESCRIBE-PG:FORM'
^IF <_FILEERROR> .NEQ. 0 THEN ^GOTO CONT2
   % file already exists — ask before overwriting
   ^ASK NEW_FORMFILE/'Y' and the new one installed? (Y/N) (Default = Y):
   ^IF '<NEW_FORMFILE>' .EQS. 'N' THEN ^GOTO SKIP_FORMFILE
   ^CALL DEL_FI '<FORM_FILE>'
^CONT2:
@BACKUP-SYSTEM
COPY-USERS-FILES D , , D <ND_DIR> , , L
EXIT
^EXECUTE_MODE
^SKIP_FORMFILE:

... (same exists-check-then-ask-then-copy pattern for user PARAMETERS-NTX's PGBATCH-PARAM:NTXP)

^CALL NEW 'SYSTEM'
... (same pattern for APPEND-BATCH-PG:XCOM, but via ^@COPY-FI instead of the BACKUP-SYSTEM path)

^@RELEASE-DIRECTORY <ND_DIR>--<PART2>
^TYPE Finished, and the floppy is released.
^IFDEF INSTALL_ALL THEN ^EXIT
^ABORT

^DEL_FI:
^@SET-FILE-ACCESS <P0> R,R,D
^@DELETE-FILE <P0>
^RETURN

^NEW:
^ENTER '<P0>'
^TYPE You are now entered as user <P0>
^RETURN
```

**Reading it**: the script auto-detects the language variant from a filename substring
(`TEST_NAME(9:10)` — a real XCOM string-slice expression) rather than asking the operator, then
for every existing target file, asks the operator before overwriting (default Yes) — a real,
careful, idempotent-reinstall pattern, and a genuinely reusable template for understanding any
other NORTEXT-100 `:XCOM` installer in this catalog.

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: three real floppy images, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x` for listing/extraction, `byte & 0x7F` for both `:XCOM` scripts).

---
**Parent:** [../README.md](../README.md) (Software catalog)
