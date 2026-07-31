# LINKER-B01 - ND Linkage editor / domain linker (version B01)

## Overview

LINKER-B01 (the "ND LINKER") is Norsk Data's ND-500 linkage editor. It takes
relocatable object files (`:NRF`) produced by a compiler (NC C, PLANC,
FORTRAN, ...) and links them into a runnable domain (`.DOM`). It runs an
ND-SHELL user interface with the prompt `NDL:`, supports domains and segments,
libraries, entry/reference management, trap-condition setup, and a service
sub-program (LINKER-SERVICE-PROGRAM) for permanent settings.

A key convenience: on CLOSE, if the domain still has undefined references or
trap blocks, the linker automatically runs `LINKER-AUTO-<language>:JOB` from
SYSTEM, choosing the language from the program's NRF language marker. That is
how the runtime library and trap handlers get pulled in without the user
listing them by hand.

See [../README.md](../README.md) for install, sintran-root layout, and the
requirements model.

## Files (in files/)

| File | Purpose |
|---|---|
| `LINKER-B01.DOM` | The linker domain (the program you run). ND-500 root domain, linker v97.2. |
| `LINKER-B01.HELP` | Full built-in help text (191 KB) - every command with explanation, parameters, and the old Linkage-Loader command it replaces. |
| `LINKER-B01.INIT` | Startup script the linker runs on entry: `LIST` then `SET-ADVANCED-MODE` [verified from file]. |

## Requirements

- To RUN the linker: only `LINKER-B01.DOM` (plus the optional `.HELP`/`.INIT`
  alongside it) [verified].
- To LINK a C program end to end you also need, in SYSTEM
  ([../_shared/files/](../_shared/files/)) [verified]:
  - `USLIB3.NRF`, `NC-LIB.NRF`, `CAT-LIB.NRF`
  - `LINKER-AUTO-C.JOB`
- To LINK a PLANC program: `PLANC-LIB.NRF` + `LINKER-AUTO-PLNC.JOB`.
- The `_shared/files/` folder also carries the other language auto-jobs
  (`LINKER-AUTO-FORT.JOB`, `-PASC`, `-COB`, `-ADA`, `-5ASM`, ... and the
  generic `LINKER-AUTO.JOB`).
- Known media gap [verified]: `FORTRAN-LIB` and `EXCEPT-LIB` are absent from
  all media. See Known issues below and [../README.md](../README.md).

## How to run

Start the SINTRAN shell as described in [../README.md](../README.md), then type
`LINKER-B01` at the `@` prompt (the `@` is the prompt, do NOT type it). The
linker runs its `.INIT`, then presents `NDL:`.

A minimal manual link of one NRF into a new domain:

```
NDL: OPEN-DOMAIN "MY-PROG"
NDL: LOAD MY-PROG
NDL: CLOSE
NDL: EXIT
```

Scripted (non-interactive) link of a C program, feeding stdin. This mirrors the
run-verified C link path (the C runtime is pulled in as libraries) [verified]:

```sh
printf 'LOGIN GUEST\nLINKER-B01\nOPEN-DOMAIN "HELLO"\nLOAD HELLO\nSPECIAL-LOAD USLIB3,LIBRARY\nSPECIAL-LOAD NC-LIB,LIBRARY\nSPECIAL-LOAD CAT-LIB,LIBRARY\nCLOSE\nEXIT\n' \
  | ./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

In normal use CLOSE alone triggers the correct `LINKER-AUTO-C.JOB` (which
itself issues the SPECIAL-LOAD / DEFINE-ENTRY / REFER for the C runtime), so
the explicit SPECIAL-LOAD lines are the manual equivalent of what the auto-job
does [verified].

## Commands and options

Full command set, read from `LINKER-B01.HELP` [from HELP]. Press SHIFT+HELP in
the real ND-SHELL to list them; type a command name then HELP for its full
parameter description.

| Command | Purpose |
|---|---|
| `OPEN-DOMAIN <name> [privileges]` | Open (or create, name in `"..."`) a domain as current; default type `DOM`; auto-CLOSEs the previous one. |
| `OPEN-SEGMENT` | Open a free segment as current. |
| `APPEND-DOMAIN` / `APPEND-SEGMENT` | Append to an existing domain/segment. |
| `LOAD <file>` | Load a relocatable (NRF) file into the current domain/segment. |
| `SPECIAL-LOAD <file>,<Library/Total/Select/Omit>[,<entries>]` | Load with control: LIBRARY (only referred entries), TOTAL (all), SELECT (referred + named), OMIT (all but named). |
| `RELOAD` / `GET-MODULES` / `REPLACE-MODULES` / `DELETE-MODULES` | Reload / fetch / replace / remove modules. |
| `LINK` | Link the loaded modules. |
| `SPECIAL-LINK` | Link with options. |
| `LINK-RT-PROGRAMS` | Link real-time programs. |
| `CLOSE` | Close current domain/segment; runs `LINKER-AUTO-<lang>:JOB` if undefined refs/trap blocks remain. |
| `DEFINE-ENTRY` | Define an entry point (name -> address). |
| `DEFINE-FORTRAN-COMMON` | Define a FORTRAN COMMON block. |
| `REFER-ENTRY` | Create a reference to an entry. |
| `DELETE-ENTRIES` / `SAVE-ENTRIES` | Remove / save entry definitions. |
| `CREATE-ROUTINE-VECTOR` / `INCLUDE-IN-ROUTINE-VECTOR` | Build / add to a routine vector. |
| `LIST-DOMAINS` / `LIST-SEGMENTS` / `LIST-MODULES` / `LIST-ENTRIES` / `LIST-NRF` / `LIST-STATUS` | Listings. |
| `MATCH-RT-SEGMENT` | Match a real-time segment. |
| `FIX-SEGMENT` | Fix a segment in place. |
| `CHANGE-FILE-REFERENCES` / `CHANGE-LINK-LOCK` | Change file references / link lock. |
| `COMPRESS` | Compress the domain. |
| `SET-TRAP-CONDITION` | Set trap-block conditions. |
| `SET-START-ADDRESS` / `SET-LOAD-ADDRESS` / `SET-HIGH-ADDRESS` | Address control. |
| `SET-SEGMENT-LIMITS` / `SET-SEGMENT-NUMBER` / `SET-SEGMENT-SIZE` | Segment control. |
| `SET-AREA-SIZE` / `SET-HEAP-SIZE` / `SET-IO-BUFFERS` | Sizing. |
| `SET-COMPUTER` / `SET-CASE-SIGNIFICANCE` / `SET-FORMAT` / `SET-LIST-MODE` | Modes/format. |
| `SET-ADVANCED-MODE` | Enable the advanced command set (run by `.INIT`). |
| `SET-LIBRARY` / `FORCE-LIBRARY` / `PREPARE-LIBRARY` / `SAVE-LIBRARY` | Library handling. |
| `NRF-LIBRARY-HANDLER` / `LINKER-SERVICE-PROGRAM` | Sub-programs (NRF library handler; service program for permanent settings, e.g. SET-FORMAT). |
| `DELETE-DEBUG-INFORMATION` / `IGNORE-DEBUG-INFORMATION` | Debug-info control. |
| `INSERT-MESSAGE` | Insert a message into the domain. |
| `SPECIAL-DEFINE` | Special entry definition. |
| `RESET-LINKER` | Reset the linker state. |
| `ABORT-BATCH-ON-ERROR` | Abort a batch/job on error. |
| `% <text>` | Comment (batch/mode jobs). |
| `@<command>` | Pass a command to SINTRAN III. |
| `EXIT` | Leave the linker. |

Number format [from HELP]: addresses default to octal, other numbers to
decimal; a `B`/`D`/`H` suffix overrides per number. The default can be made
permanent with `SET-FORMAT` in the service program.

## Verified behaviour in nd500x

- Runs its `.INIT` (`LIST`, `SET-ADVANCED-MODE`) and reaches `NDL:`
  [verified].
- Links a C program end to end via the C auto-job (SPECIAL-LOAD of USLIB3,
  NC-LIB, CAT-LIB) into a runnable `.DOM` [verified].

## Known issues / status

- Known issue [verified]: for a C program the linker's NRF language marker
  reads FORTRAN, so on CLOSE it picks `LINKER-AUTO-FORT:JOB`, which needs the
  absent `FORTRAN-LIB` / `EXCEPT-LIB`. Workaround used here: the C auto-job
  content (`LINKER-AUTO-C.JOB`) is placed in the `LINKER-AUTO-FORT.JOB` slot
  the linker opens, so the C runtime is linked instead. See
  [../README.md](../README.md) and the memory note
  `nc-link-fortran-autojob-missing-libs`.
- FORTRAN linking proper cannot complete until `FORTRAN-LIB` / `EXCEPT-LIB`
  are recovered from media [verified].

## References

- [files/LINKER-B01.HELP](files/LINKER-B01.HELP) - full built-in command help (authoritative command reference)
- [analysis/linker-b01.asm](analysis/linker-b01.asm) - ND-500 disassembly
- [../_shared/files/LINKER-AUTO-C.JOB](../_shared/files/LINKER-AUTO-C.JOB) - the C runtime auto-job
- [../README.md](../README.md) - shared install/run conventions and requirements model
- [../NC-A06/](../NC-A06/) - the C compiler that produces the `:NRF` inputs
- ND-500 monitor-call analysis: [../../ND500/](../../ND500/) and [../../../Developer/MON/calls/](../../../Developer/MON/calls/)
