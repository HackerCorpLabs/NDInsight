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

Full end-to-end conversion was NOT driven here. The command and option details
above are taken from the shipped `CONVERT-DOM-A03.HELP`, not from a live
conversion run. [from HELP] [UNVERIFIED end-to-end]

## Known issues / status

- Loads and runs; ships both HELP and INIT files. [verified]
- End-to-end conversion output not verified in nd500x. [UNVERIFIED]

## References

- Shared conventions: [../README.md](../README.md)
- Vendor help text: [files/CONVERT-DOM-A03.HELP](files/CONVERT-DOM-A03.HELP)
- Startup script: [files/CONVERT-DOM-A03.INIT](files/CONVERT-DOM-A03.INIT)
- Disassembly: [analysis/convert-dom-a03.asm](analysis/convert-dom-a03.asm)
- Runnable domain: [files/CONVERT-DOM-A03.DOM](files/CONVERT-DOM-A03.DOM)
