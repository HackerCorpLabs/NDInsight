# AUTOMAKE-500-C00 - make-like build / dependency driver

## Overview

AUTOMAKE-500 is a Norsk Data ND-500 build automation tool, the ND equivalent of
a "make": it reads an automake file (a rules/dependency file), works out which
targets are out of date relative to their sources, and executes the commands
needed to bring them up to date. [from disasm] The embedded version string is
`April 27, 1987`. [from disasm] This is version C00. [verified] (folder name)

The tool reaches an interactive command prompt and waits for input. [verified]
(load-sweep 2026-07-31 in nd500x)

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `AUTOMAKE-500-C00.DOM` - the runnable ND-500 domain. One segment, entry point
  0x08001305, linker v97.251. [from disasm] Self-contained (no PSEG/DSEG/HELP/
  INIT ships). [verified]

## Requirements

- Just the `.DOM` file to run. [verified]
- Install: copy `files/AUTOMAKE-500-C00.DOM` into the sintran-root. See
  [../README.md](../README.md) "Installing a program into the emulator".
- To actually build a target, AUTOMAKE runs whatever commands the automake file
  names (typically a compiler + LINKER-B01). Those tools and their runtime
  libraries must be installed too. [from disasm] The FORTRAN library gap
  described in [../README.md](../README.md) applies to any FORTRAN target.

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
AUTOMAKE-500-C00
```

Scripted (non-interactive) drive, from `~/repos/nd500x`:

```
printf 'LOGIN GUEST\nAUTOMAKE-500-C00\nHELP\nEXIT\n' | ./build/bin/nd500x \
    --monitor --user GUEST --sintran-root ~/ND500USERS
```

The command set is NOT run-verified in nd500x - only that the program loads and
reaches its prompt. [verified] The commands below are read from the strings and
disassembly of the DOM, not from a live session.

## Commands and options

The following command names and their argument syntax are embedded verbatim in
the DOM. [from disasm]

- `MAKE <Automake file> <Target name> <Output file>` - build a target.
- `GENERATE-AUTOMAKE-FILE <Input file> <Automake file> <Own username> <Expand>`
  - produce an automake file from an input file.
- `COPY-REQUIRED <Automake file> <Target name> <Destination>` - copy the files a
  target needs to a destination.
- `LIST-REQUIRED <Automake file> <Target name> <Output file>` - list the files a
  target needs.
- `EXECUTION-MODE <Execute/Unconditional/Touch/List/Off>` - how MAKE acts
  (actually run / force / just timestamp / just list / disabled).
- `STOP-BATCH-ON-FAULT <Ignore/Error/Warning>` - batch abort policy.
- `SET-VALUE <Name> <Value>` - define a macro/value.
- `CHANGE-VALUE <Name> <Value>` - change a macro/value.
- `LIST-VALUES` - list defined macros/values.
- `SEARCH-ORDER <User name 1> ...` - directory/user search order for files.
- `CROSS-REFERENCE <On/Off>` - cross-reference listing on/off.
- `DEBUG-MODE <On/Off>` - debug output on/off.
- `SEPARATE-DATA <On/Off>` - separate-data option on/off.
- `HELP` - list commands. [from disasm]
- `EXIT` - leave the tool. [from disasm]

Prompts seen in the DOM: `Automake file:`, `Destination:`, `Input file:`,
`Identifier:`, `Add own user name (<On/Off>):`, `Expand file names (<On/Off>):`,
and a target-CPU selector `100/500/68000:`. [from disasm]

The automake (rules) file has its own mini-language: conditional statements
(`if ... then`/`elsif`/`else`/`endif`), `head`/`tail` statements, `include`,
`search-order`, and macro assignment with `in` scoping. Error texts such as
`Dependency statements are not allowed in the rulesfile.` and
`` `Else` must be preceded by `if ... then`. `` are embedded. [from disasm]
The exact rules-file grammar is NOT verified here - see the disassembly.

UNVERIFIED: which command is the default when a bare target name is typed, and
the precise semantics of each `<On/Off>` toggle.

## Verified behaviour in nd500x

- Loads and reaches its command prompt; waits for input. [verified]
  (load-sweep 2026-07-31)
- No end-to-end build has been run through it in nd500x. [verified]

## Known issues / status

- Command set documented from the binary only; not exercised live. [from disasm]
- Building a FORTRAN target is blocked by the missing FORTRAN-LIB / EXCEPT-LIB
  (see [../README.md](../README.md)). C targets can use the working C toolchain.
  [from disasm]

## References

- Shared conventions: [../README.md](../README.md)
- Disassembly: [analysis/automake-500-c00.asm](analysis/automake-500-c00.asm)
- Runnable domain: [files/AUTOMAKE-500-C00.DOM](files/AUTOMAKE-500-C00.DOM)
