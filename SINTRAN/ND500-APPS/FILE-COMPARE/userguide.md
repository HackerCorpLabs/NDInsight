# FILE-COMPARE - text/file difference (diff) tool

## Overview

FILE-COMPARE is a Norsk Data ND-500 text/file comparison (diff) tool
(ND FILE-COMPARE, version ND-10603B, 1985) [from disasm]. It reads two files,
an "old" file and a "new" file, and prints the lines that differ between them,
then reports where the compare ended. [verified]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `FILE-COMPARE.DOM` - the runnable ND-500 domain (self-contained: one segment,
  entry point 0x0800072F). [from disasm]

No PSEG/DSEG/HELP/INIT files ship with this program - the DOM is all that is
needed to run it. [verified]

## Requirements

- Just the `.DOM` file to run - no external libraries or segments. [verified]
- Install: copy `files/FILE-COMPARE.DOM` into the sintran-root
  (`SYSTEM/` for system-wide, or a user directory). See
  [../README.md](../README.md) "Installing a program into the emulator".

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
FILE-COMPARE
```

The tool starts and shows its own `FCOM:` prompt. [verified]

Scripted (non-interactive) drive, from `~/repos/nd500x`:

```
printf 'LOGIN GUEST\nFILE-COMPARE\nCOMPARE A:SYMB B:SYMB\n\nEXIT\n' | \
    ./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

The blank line after the `COMPARE` line answers the `logfile:` prompt with an
empty response, sending diff output to the terminal. `EXIT` leaves the tool.
[verified]

## Commands and options

At the `FCOM:` prompt: [verified]

- `COMPARE <file-1> <file-2>` - compare `<file-1>` (the "old" file) against
  `<file-2>` (the "new" file). After the two filenames the tool prompts:
  - `logfile:` - press Enter (blank) to send output to the terminal, or give a
    filename to write the diff to a file. [verified]
- `EXIT` - leave the tool and return to the SINTRAN `@` prompt. [verified]

The two filenames may also be supplied as arguments; a bare run with no
arguments waits at the `FCOM:` prompt for a `COMPARE` command. [verified]

## Verified behaviour in nd500x

Verified 2026-07-31 in the `nd500x` C emulator: [verified]

- `FILE-COMPARE` starts and presents the `FCOM:` prompt.
- `COMPARE A:SYMB B:SYMB` with a blank `logfile:` response printed:
  - `Reading old file...`
  - `Reading new file...`
  - the old and new differing lines
  - `-- End of compare --`
- `EXIT` returned cleanly to the SINTRAN `@` prompt.

## Known issues / status

- No known issues. Runs and produces real diff output. [verified]
- The exact set of any additional `FCOM:` commands beyond `COMPARE` and `EXIT`
  was not exhaustively enumerated. [UNVERIFIED]

## References

- Shared conventions: [../README.md](../README.md)
- Disassembly: [analysis/file-compare.asm](analysis/file-compare.asm)
- Runnable domain: [files/FILE-COMPARE.DOM](files/FILE-COMPARE.DOM)
