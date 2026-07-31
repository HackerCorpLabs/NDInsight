# LED-FORTRAN-A01 - LED editor with integrated FORTRAN compiler

## Overview

LED-FORTRAN is the Norsk Data LED full-screen editor bound to the ND FORTRAN
compiler: the same LED screen editor as [../LED-NEW/userguide.md](../LED-NEW/userguide.md),
plus a FORTRAN "language mode" that syntax-checks and compiles the edited source
in place. [from disasm] Embedded strings confirm both halves: the full LED
`WINDOW KEY COMMANDS` table (windows, marked areas, `Func # EXIT`, `Func ? HELP`,
`Flc M Syntax check area. LANGUAGE MODE`), and FORTRAN-specific text such as
`Syntax check completed.`, `Fortranwork`, and a large list of FORTRAN compiler
options (see below). [from disasm] This is version A01. [verified] (folder name)

The compiler domain loads. [verified] (load-sweep 2026-07-31 in nd500x)

IMPORTANT BLOCKER: taking FORTRAN source through the full compile -> link -> run
chain is currently BLOCKED AT LINK, because `FORTRAN-LIB` and `EXCEPT-LIB` are
missing from all available media, so the linker cannot resolve the FORTRAN
runtime and exception handlers. See [../README.md](../README.md) "Requirements
model" (the C toolchain works around this with a self-contained auto-job; there
is no such workaround for FORTRAN here). [from README]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `LED-FORTRAN-A01.DOM` - the runnable ND-500 domain (editor engine + FORTRAN
  compiler in one, ~1.3 MB). One segment, entry point 0x08000004, linker v97.2.
  [from disasm] Self-contained (no separate PSEG/DSEG/HELP/INIT ships alongside).
  [verified]

## Requirements

- To RUN the editor/compiler: just the `.DOM` file. [verified]
- To LINK and RUN compiled FORTRAN output: `FORTRAN-LIB` + `EXCEPT-LIB` - which
  are NOT present on any available media. FORTRAN link is therefore blocked.
  [from README]
- A full-screen terminal to use the editor interactively. [from disasm]
- Install: copy `files/LED-FORTRAN-A01.DOM` into the sintran-root. See
  [../README.md](../README.md).

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
LED-FORTRAN-A01
```

A source file name can normally follow the name. [UNVERIFIED - exact argument
syntax not confirmed.]

Scripted drive can only launch it (it is full-screen; you cannot edit over a
pipe):

```
printf 'LOGIN GUEST\nLED-FORTRAN-A01\n' | ./build/bin/nd500x --monitor \
    --user GUEST --sintran-root ~/ND500USERS
```

Real use needs an interactive terminal. [from disasm]

## Commands and options

Two layers, both read from the DOM strings and NOT run-verified. [from disasm]

Editor keys (the LED `WINDOW KEY COMMANDS` table, embedded verbatim): [from disasm]

- `Func #` (EXIT) - exit from editor.
- `Func ?` (HELP) - give help.
- `Flc M` (Execute key) - syntax check area, LANGUAGE MODE.
- `Flc ^` (Shift Execute) - continue syntax check, LANGUAGE MODE.
- `Func C` copy, `Func D` delete, `Func M` move, `Func F` mark field,
  `Func Z` mark contiguous area, `Func ]` window mode, plus many cursor/scroll
  and `Ctrl`-key line-editing commands. See the disassembly for the full table.

FORTRAN compiler options (option names embedded in the DOM; set in language mode
/ compiler directives): [from disasm]

`ARRAY-INDEX-CHECK`, `CHECK-NUMBER-OF-PARAMETERS`, `COBOL-INTERFACE`,
`CONDITIONAL-COMPILING`, `CROSS-REFERENCE`, `DEBUG-MODE`, `HEADING-TEXT`,
`INLINE-EXPANSION`, `LINK-SEGMENT`, `LOCAL-STACK-SIZE`, `MAIN-STACK-SIZE`,
`MON-CALL-NAMES`, `MOVE-COMMON-VARIABLES`, and several non-standard-language
flags. [from disasm]

UNVERIFIED: the exact directive syntax for setting these options, and how the
compile is invoked from inside the editor versus as a batch compile.

## Verified behaviour in nd500x

- The domain loads. [verified] (load-sweep 2026-07-31)
- No compile/edit session has been driven in nd500x; the FORTRAN link step is
  blocked (missing libraries), so a compiled program cannot currently be linked
  or run. [verified] / [from README]

## Known issues / status

- BLOCKER: FORTRAN compile -> link -> run is blocked at link (`FORTRAN-LIB` and
  `EXCEPT-LIB` missing from all media). [from README]
- Full-screen interactive tool: needs a real terminal; not drivable by pipe.
  [from disasm]
- Command/key map and compiler-option syntax are [UNVERIFIED] for this binary.

## References

- Shared conventions: [../README.md](../README.md)
- Plain LED editor (same key map): [../LED-NEW/userguide.md](../LED-NEW/userguide.md)
- Disassembly: [analysis/led-fortran-a01.asm](analysis/led-fortran-a01.asm)
- Runnable domain: [files/LED-FORTRAN-A01.DOM](files/LED-FORTRAN-A01.DOM)
