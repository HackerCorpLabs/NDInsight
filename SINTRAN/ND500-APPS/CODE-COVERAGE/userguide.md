# CODE-COVERAGE - code-coverage analyser / reporter

## Overview

CODE-COVERAGE is a Norsk Data code-coverage reporting tool. It combines a
DEBUGGER dump-log file with a compiler source listing and produces a listing in
which the non-executed statements are highlighted, then prints a coverage
figure. [from disasm] The embedded welcome text is
`Welcome to the code-coverage analyzer, version of ...` and the version date in
the DOM is `DECEMBER 3, 1986`; it credits `OJH, M4`. [from disasm] It works for
both ND-100 and ND-500 programs (`a for ND-100` / `a for ND-500`). [from disasm]

The tool loads and reaches its input prompts. [verified] (load-sweep
2026-07-31 in nd500x)

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `CODE-COVERAGE.DOM` - the runnable ND-500 domain. One segment, entry point
  0x08001CA9, linker v97.2. [from disasm] Self-contained (no PSEG/DSEG/HELP/
  INIT ships). [verified]

## Requirements

- Just the `.DOM` file to run. [verified]
- To produce useful output it needs, as inputs, an existing DEBUGGER dump-log
  file and the compiler list file for the same program. [from disasm]
- Install: copy `files/CODE-COVERAGE.DOM` into the sintran-root. See
  [../README.md](../README.md).

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
CODE-COVERAGE
```

It then asks a fixed series of questions (see below). Scripted drive from
`~/repos/nd500x`, answering the prompts in order:

```
printf 'LOGIN GUEST\nCODE-COVERAGE\nMYPROG:DUMP\nMYPROG:LIST\nN\nOUT:SYMB\nEXIT\n' \
    | ./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

WARNING: the exact prompt ORDER and how many answers are needed is NOT
run-verified in nd500x - only that the program loads. [verified] The printf
above is a plausible ordering derived from the embedded prompts, not a proven
transcript. Adjust the answer lines to match what the program actually asks.

## Commands and options

CODE-COVERAGE is question-driven, not command-driven: there is no command
prompt, no `HELP` and no `EXIT` token in the DOM. [from disasm] It asks a set of
questions and then produces the report. The prompt strings embedded in the DOM
are: [from disasm]

- `Compiler List file:` - the source listing produced by the compiler.
- `DUMP-LOG file:` - the DEBUGGER dump-log to analyse.
- `New input file:` - used when the debug file contains include statements and a
  new input file must be built.
- `Print the source (Y/N):` - whether to emit the annotated source listing.
- `Output file:` - where the report is written.
- `Program language:` - source language selector (rejects with `Illegal keyword`
  / `Unknown language` / `Ambiguous language`).

Report text it prints includes:
`Total number of active lines (included declarations) in the program are`,
`Number of lines not executed are`,
`The following routines have non executed source lines:`, and
`The code coverage figure is <n> percent`. [from disasm]

UNVERIFIED: the precise order of the questions, and which are conditional.

## Verified behaviour in nd500x

- Loads and reaches its input prompt. [verified] (load-sweep 2026-07-31)
- A full analysis run has NOT been driven end to end in nd500x. [verified]

## Known issues / status

- Prompt sequence documented from the binary only; not exercised live.
  [from disasm]
- Needs valid DEBUGGER dump-log + compiler list-file inputs to do anything
  useful; producing those is a separate step not covered here. [from disasm]

## References

- Shared conventions: [../README.md](../README.md)
- Disassembly: [analysis/code-coverage.asm](analysis/code-coverage.asm)
- Runnable domain: [files/CODE-COVERAGE.DOM](files/CODE-COVERAGE.DOM)
