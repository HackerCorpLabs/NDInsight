# PLANC-500-G00 - PLANC compiler for the ND-500 (version G00)

## Overview

PLANC is Norsk Data's systems-programming language; SINTRAN III itself and most
ND system tools are written in it. PLANC-500-G00 is the ND-500 PLANC compiler.
It runs as an interactive command shell that reads compiler directives
(COMPILE, INCLUDE, OPTION, LIST, DEBUG-MODE, ...), compiles a PLANC source into
a relocatable `:NRF` object, and can produce a listing and cross-reference. The
`:NRF` is then linked into a runnable `.DOM` with LINKER-B01.

Banner (embedded in the DOM) [from disasm]: `PLANC COMPILER - VERSION G`.

See [../README.md](../README.md) for install, sintran-root layout, and the
requirements model.

## Files (in files/)

| File | Purpose |
|---|---|
| `PLANC-500-G00.DOM` | The PLANC compiler domain (the program you run). ND-500 root domain, linker v97.2, entry `0x0800065C`. |

No `.HELP` or `.INIT` ships in this folder; the command help is built into the
DOM (a `HELP` command lists it).

## Requirements

- To RUN the compiler: only `PLANC-500-G00.DOM` [verified: the DOM loads and
  reaches its prompt].
- To COMPILE + LINK a PLANC program end to end you also need, in SYSTEM
  ([../_shared/files/](../_shared/files/)):
  - `PLANC-LIB.NRF` (the PLANC runtime library)
  - `LINKER-AUTO-PLNC.JOB` (the PLANC linker auto-job)
- Known media gap: the FORTRAN/exception libraries (`FORTRAN-LIB`,
  `EXCEPT-LIB`) are absent from all media; this affects the linker's default
  FORTRAN trap auto-job, not PLANC's own library. See
  [../README.md](../README.md) [verified].

## How to run

Start the SINTRAN shell as described in [../README.md](../README.md), then type
`PLANC-500-G00` at the `@` prompt (the `@` is the prompt, do NOT type it). The
compiler starts and waits for directives at its prompt [verified].

Scripted (non-interactive) invocation feeding stdin. This drives the compiler
to its prompt and issues a COMPILE; a full compile has NOT been run to a
finished `:NRF` here, so treat the compile result as not yet verified:

```sh
printf 'LOGIN GUEST\nPLANC-500-G00\nCOMPILE MYPROG MYPROG MYPROG\nEXIT\n' \
  | ./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

COMPILE takes three file arguments: `<source file> <list file> <object file>`
[from disasm].

## Commands and options

Command set read from the compiler's built-in HELP table embedded in the DOM
[from disasm]. Angle-bracket tokens are the argument the compiler expects.
These are extracted from the binary, not yet each run-verified.

| Command | Arguments |
|---|---|
| `COMPILE` | `<source file> <list file> <object file>` |
| `INCLUDE` | `<source file>` |
| `LIST` | `<ON/OFF>` |
| `NOLIST` | (list off) |
| `OBLIST` | `<ON/OFF>` (object/generated-code listing) |
| `OPTION` | `<options>` |
| `CONSTANT` | `<identifier>=<constant value>,...` |
| `KILL` | `<constant identifier>,...` |
| `CROSS-REFERENCE` | `<work file-name>` |
| `XREF` | `<auxiliary file>` |
| `LINKAGE-REFERENCE` | `<work file name>` |
| `DEBUG-MODE` | `<ON/OFF>` |
| `LIBRARY-MODE` | `<ON/OFF>` |
| `MODULE-LIBRARY-MODE` | `<ON/OFF>` |
| `SEPARATE-DATA` | `<ON/OFF>` |
| `CALL-HIERARCHY` | `<ON/OFF>` |
| `SQUEEZE` | `<ON/OFF>` (code-size optimisation; see Known issues) |
| `ARRAY-INDEX-CHECK` | `<ON/OFF>` |
| `BOOLEAN2-ENUMERATION2` | `<ON/OFF>` |
| `REAL-PRECISION` | `<no-of-digits>` |
| `LINE-BIAS` | `<line-number>` |
| `CPU-EXTENSION` | `<number>` |
| `TARGET-MACHINE` | (target selection) |
| `MESSAGE-TO-TERMINAL` | `<message>` |
| `MACRO` / `ENDMACRO` | Define a macro block. |
| `IF` / `THEN` / `ELSIF` / `ELSE` / `ENDIF` | Conditional compilation. |
| `EJECT` | Page eject in the listing. |
| `DATE` | Emit/print the date. |
| `EOF` | End of input. |
| `HELP` | List commands. |
| `EXIT` | Leave the compiler. |

Compiler diagnostics are also embedded (e.g. `SYNTAX ERROR IN GENERAL
OPERAND`, `RANGE EXCEEDED`, `ILLEGAL DATA TYPE`, `CODE BUFFER FULL`) [from
disasm].

## Verified behaviour in nd500x

- The DOM loads and reaches its prompt, waiting for input [verified].
- A full source-to-`:NRF` compile has NOT been driven end to end here; the
  command syntax above is extracted from the binary and is not yet
  run-verified [verified statement of status].

## Known issues / status

- Status: loads and reaches prompt only [verified]. End-to-end compile is
  unproven in nd500x - the command/option details are [from disasm] and should
  be confirmed by an actual compile.
- The DOM embeds the warning `SQUEEZE OPTION GENERATES INCORRECT CODE FOR THIS
  ROUTINE`, so the `SQUEEZE` optimisation can be unsafe on some routines [from
  disasm].
- Linking a compiled PLANC program needs `PLANC-LIB` + `LINKER-AUTO-PLNC.JOB`
  (present in `_shared/files/`); this link has not been verified here.

## References

- [analysis/planc-500-g00.asm](analysis/planc-500-g00.asm) - ND-500 disassembly (command strings extracted from the DOM)
- [../README.md](../README.md) - shared install/run conventions and requirements model
- [../LINKER-B01/](../LINKER-B01/) - linking the resulting `:NRF` into a `.DOM`
- [../_shared/files/PLANC-LIB.NRF](../_shared/files/PLANC-LIB.NRF) and [../_shared/files/LINKER-AUTO-PLNC.JOB](../_shared/files/LINKER-AUTO-PLNC.JOB) - PLANC runtime + linker auto-job
- ND-500 monitor-call analysis: [../../ND500/](../../ND500/) and [../../../Developer/MON/calls/](../../../Developer/MON/calls/)
