# CONVERT-DOM-A03 - convert old-format domains to the new :DOM format

## Overview

CONVERT-DOM-A03 (Convert-Domain, version A03) converts Norsk Data ND-500 domains
from the OLD domain format (a description file plus per-segment `:PSEG`, `:DSEG`
and `:LINK` files) to the NEW domain format (a single `:DOM` file with the
bookkeeping information stored in a header, plus `:SEG` files for shared/free
segments). [from HELP]

The main difference between the two formats: the old format has a separate
description file; the new format does not - each domain is one `:DOM` file that
can be copied with a plain `@COPY-FILE`. [from HELP]

It uses the ND-SHELL as its command processor, so the interface is very similar
to ND's LINKER. [from HELP]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `CONVERT-DOM-A03.DOM` - the runnable ND-500 domain. [verified]
- `CONVERT-DOM-A03.HELP` - the vendor help text (topics and command syntax).
  [from HELP]
- `CONVERT-DOM-A03.INIT` - the startup command script run on entry. It contains
  a `LIST` command and two comment (`%`) lines describing the tool. [from HELP]

## Requirements

- The `.DOM` file to run. [verified]
- Install: copy `files/*` into the sintran-root. See
  [../README.md](../README.md).
- To convert a domain you need the source domain in the OLD format (its
  description file plus its `:PSEG`/`:DSEG`/`:LINK` files) available under a
  user directory. [from HELP]

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
CONVERT-DOM-A03
```

With no command-line parameters the ND-SHELL is used and you get an interactive
command prompt (press the HELP key for help). If you instead write the
parameters on the command line, the shell is NOT used, for example
`ND CONVERT-DOM DEST-DOM SOURCE-DOM`. [from HELP]

Scripted (non-interactive) drive, from `~/repos/nd500x`:

```
printf 'LOGIN GUEST\nCONVERT-DOM-A03\nCONVERT-DOMAIN NEW-DOM OLD-DOM\nEXIT\n' | \
    ./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

## Commands and options

Commands (from the shipped HELP): [from HELP]

- `CONVERT-DOMAIN <Destination domain> <Source domain> <Include linked
  segments (Yes/No)> <Display progress information (Yes/No)> <Force free segment
  number(s)>...`
  - `<Destination>` (mandatory) - name of the new-format `:DOM` file to create.
    Accepts an empty string (just CR), which is equivalent to a single `$`.
    A `$` in the name is substituted with the source domain name. Enclose the
    name in double quotes to prevent overwriting an existing `:DOM` file.
  - `<Source>` (mandatory) - name of the source domain, which must be in the old
    format. No default.
  - `<Include linked segments>` (optional, default NO) - YES copies all `:SEG`
    files needed by the destination to the destination's user (useful for
    putting everything on one floppy); NO links to `:SEG` files that may live in
    other user areas.
  - `<Display progress information>` (optional, default YES) - YES prints
    progress messages such as `>> Converting debug part for segment 3 <<`.
  - `<Force free segment numbers>...` (optional, repeated) - force listed
    segment numbers onto `:SEG` files even when the tool would otherwise put
    them on the `:DOM`. Ranges accepted, for example `0:31`, `0-31` or `0..31`.
    Example: `CONVERT-DOMAIN $ NOTIS-WP,,,0:31`.
- `EXIT` - leave the Convert-Domain command processor. [from HELP]
- `HELP` - built-in help; accepts SINTRAN matching and the wildcards `-`, `+`
  (any single character) and `*` (any string). SHIFT+HELP lists all matching
  commands. [from HELP]
- `%` - shell comment line. [from HELP]
- `@<command>` - run a SINTRAN III command from inside the tool
  (for example `@DELETE-FILE destination:DOM`). [from HELP]

Help topics defined: COMMENT, CONVERT-DOMAIN, EXIT, HELP, LIMITATIONS,
NEW-DOMAIN-FORMAT, OLD-DOMAIN-FORMAT, SHELL, SIBAS. [from HELP]

Limitations (do NOT convert): Sibas version F or older; Notis-DS version D or
older; Notis-ID version B or older; ND-500 Basic version B or older; and the
ND-500/5000 Swapper and Symbolic Debugger (they have no description file).
[from HELP]

## Verified behaviour in nd500x

Verified 2026-07-31 in the `nd500x` C emulator: the program loads and runs.
[verified]

**Full end-to-end conversion VERIFIED 2026-08-10** - the first real conversion run
recorded for this tool. Source: `LINKAGE-LOAD-H02` (the NLL H02 installer floppy's own
domain, old format: `DESCRIPTION-FILE:DESC` + `:PSEG`/`:DSEG`/`:LINK`, staged as SINTRAN
user `FLOPPY-USER` under `~/ND500USERS/FLOPPY-USER/`). Driven non-interactively:

```
printf 'LOGIN FLOPPY-USER\nCONVERT-DOM-A03\nCONVERT-DOMAIN "LINKAGE-LOAD-H02" LINKAGE-LOAD-H02\nEXIT\n' | \
    ./build/bin/nd500x --monitor --user FLOPPY-USER --sintran-root ~/ND500USERS
```

Output (verbatim, ANSI codes stripped):

```
- Convert Domain, Version A03            January 24,  1989
- CONV entered:
CONVERT-DOM:INIT
% This program converts domains and segments from :PSEG/:DSEG/:LINK
% format to :DOM/:SEG format. If you need help, press the help key.
CONV: CONVERTDOMAIN "LINKAGE-LOAD-H02" LINKAGE-LOAD-H02
 >> Converting debug part for segment 22
 >> Converting link part for segment 22
 >> Converting program segment 22
 >> Converting data segment 22
 >> Finished
CONV: EXIT
-- program exited (316156 instructions) --
```

Produced `LINKAGE-LOAD-H02.DOM` (2,316,049 bytes - roughly
4096-byte header + 123,989-byte PSEG + 2,184,977-byte DSEG + debug/link overhead,
consistent with the source sizes). Header bytes independently confirm
`../../File-Formats/DOM-FILE-FORMAT.md`'s FLAGS byte layout: offset 0x06 = `0xF8` =
bits 3/4/5/6/7 all set = TRAPBLOCK_VALID + IS_DOMAIN_FILE + IS_ROOT_DOMAIN +
IS_SINTRAN_III + IS_ND500, exactly as that spec's bit table predicts.

Notes: the tool reported the domain's logical segment as **22**, not 0 or 1 - a real
data point toward pinning `DESCRIPTION-FILE:DESC`'s still-unverified PLOG/DLOG bitfield
(see `../../File-Formats/DESCRIPTION-FILE-FORMAT.md` section 5).

**The converted `.DOM` DOES run, confirmed 2026-08-10.** Ran directly (no floppy, no old
`:PSEG`/`:DSEG`/`RECOVER-DOMAIN` path - just `@LINKAGE-LOAD-H02` against the file
CONVERT-DOM-A03 produced):

```
printf 'LOGIN FLOPPY-USER\nLINKAGE-LOAD-H02\nEXIT\n' | \
    ./build/bin/nd500x --monitor --user FLOPPY-USER --sintran-root ~/ND500USERS
```

```
@LINKAGE-LOAD-H02
-- LINKAGE-LOAD-H02 placed (domain 1, start 0xB0000DD1) --
  [SINTRAN ERROR 132B]
Nll: EXIT
[STOP] Unimplemented MON 405B (USTRK) with 2 args
-- program exited (15066 instructions) --
```

The domain **placed at the correct start address and reached its own live `Nll:` command
prompt** - strong evidence the conversion is structurally and functionally correct (entry
point, segment placement, and enough of the loaded code to run its own startup and print
its prompt). `SINTRAN ERROR 132B`'s meaning is not yet decoded here - it appeared but did
not stop execution, so treat it as non-fatal until checked against the SINTRAN error-code
list. The eventual stop is an **`nd500x` emulator gap** (MON call `405B`/USTRK not
implemented), not a defect in the converted domain - a materially different, and better,
result than the OLD-format run path in
`../../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md` (which hits a
5SWAP protect-violation before ever reaching a prompt).

## Known issues / status

- Loads and runs; ships both HELP and INIT files. [verified]
- End-to-end conversion output VERIFIED 2026-08-10 - see above. [verified]
- The converted `.DOM` runs and reaches its own `Nll:` prompt, VERIFIED 2026-08-10 - see
  above. **MON 405B (USTRK) is now implemented in `nd500x`** (fixed same day - the
  handler existed but was registered with `MON_STATUS_NOT_IMPLEMENTED` instead of
  `MON_STATUS_IN_PROGRESS` in `external/ndmonlib/src/core/mon_registry.c`, which forced a
  STOP regardless of the real handler code; one-line fix). Re-tested after rebuild: the
  call now returns SUCCESS (confirmed via `ND500X_MONLOG=1` trace) and `Nll:` commands run
  to completion without stopping - `WRITE-DOMAIN-STATUS LINKAGE-LOAD-H02` and `EXIT` both
  now finish cleanly at `MON 0B LEAVE` instead of halting.
- `WRITE-DOMAIN-STATUS` produces no visible console text - traced to `MON 120B WFILE`
  writing 2048 bytes back into `DESCRIPTION-FILE:DESC` itself (file 101, block 0, the same
  block holding the Domain Entries) rather than printing to the terminal.

  **UPDATE, same session, further tracing:** the manual (ND-60.136.04A section 6.1.6)
  explicitly says WRITE-DOMAIN-STATUS "Prints all the available information about the
  domain" and 6.1.5 says LIST-DOMAIN "Writes ... on the output device" - so both SHOULD
  print, contradicting the "persist-only" read above. Re-traced with `ND500X_MONLOG=1`
  and found a real secondary bug: mid-command, NLL tries (twice, access codes 2 and 3)
  to open `(SYSTEM)DESCRIPTION-FILE:DESC`, which didn't exist (`error -46`,
  `Cannot open host file '.../SYSTEM/DESCRIPTION-FILE.DESC'`) - a genuine emulator-adjacent
  finding: NLL appears to unconditionally consult SYSTEM's own description file as part of
  status reporting, not just the current user's. Creating a `SYSTEM/DESCRIPTION-FILE.DESC`
  (copied from FLOPPY-USER's) cleared that specific error - **but status text still never
  printed**, so it was a real bug, just not THE blocker. `OUTST` call count stayed at the
  same 5 calls (all short prompt/banner writes, never a real status listing) before and
  after the fix.

  **Not yet resolved**: the actual status-print short-circuit is somewhere past this
  point in NLL's own code, not identified from MON-call tracing alone - narrowing it
  further needs single-instruction tracing (`--trace-file`, or the DAP debugger) from the
  last confirmed-good `OUTST` call forward to find exactly where the status-formatting
  routine diverges or returns early. Command variants tried without success: bare
  `WRITE-DOMAIN-STATUS`, with domain name space-separated, with domain name comma-separated
  (`WRITE-DOMAIN-STATUS,LINKAGE-LOAD-H02`). `LIST-DOMAIN` inside the `Nll:` shell also
  produced no visible text with either no argument (bare CR to accept the documented "all
  domains" default) or the domain name on the following line (its actual `Domain-name:`
  prompt syntax, confirmed from ND-60.136.04A section 6.1.5) - unlike the system-wide
  `LIST-DOMAIN` at the top-level `ND-5000:`/monitor prompt seen working in
  `../../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md`, which is a
  different prompt context (`nd500x` has no separate `ND-500`/monitor shell - only
  whatever the placed domain itself, here NLL, provides).

## References

- Shared conventions: [../README.md](../README.md)
- Vendor help text: [files/CONVERT-DOM-A03.HELP](files/CONVERT-DOM-A03.HELP)
- Startup script: [files/CONVERT-DOM-A03.INIT](files/CONVERT-DOM-A03.INIT)
- Disassembly: [analysis/convert-dom-a03.asm](analysis/convert-dom-a03.asm)
- Runnable domain: [files/CONVERT-DOM-A03.DOM](files/CONVERT-DOM-A03.DOM)
