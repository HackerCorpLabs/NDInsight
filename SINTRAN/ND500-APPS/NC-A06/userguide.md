# NC-A06 - Norsk Data C compiler (version A06, 1989-01-10)

## Overview

NC is Norsk Data's C compiler for the ND-500. It is an interactive command
shell (prompt `NC:`) that drives the C build phases: preprocess, syntax check,
code generation, a combined compile, and a link. The real vendor compile flow
does not do everything in one step; it runs CHECK to produce a `:CAT`
intermediate, then GENERATE-CODE to turn that into a relocatable `:NRF` object
via the CAT_COMPILER back end (the CAT-CAT5 domain). The resulting `:NRF` is
then linked into a runnable `.DOM` with LINKER-B01.

Banner printed at start [verified]:
`Norsk Data C - Version: A06 - 1989-01-10`

See the shared conventions in [../README.md](../README.md) for install, the
sintran-root layout, and the requirements model.

## Files (in files/)

| File | Purpose |
|---|---|
| `NC-A06.DOM` | The compiler domain (the program you run). ND-500 root domain, entry `0x08000004`, linker v97.251. |

NC does not ship a `.HELP` or `.INIT` in this folder. It can, at run time,
read/write a per-user init file `NC-A:INIT` (host `NC-A.INIT`) holding the
default option string (see Commands and options). That file is optional; when
absent NC just uses built-in defaults [verified].

## Requirements

- To RUN NC: only `NC-A06.DOM` [verified].
- The CHECK -> GENERATE-CODE flow needs the CAT-CAT5 back end
  (CAT_COMPILER) present in SYSTEM. GENERATE-CODE hands off to it, and the
  console prints `programCAT_COMPILER terminated` on success [verified]. See
  `../CAT-CAT5-B06/`.
- To COMPILE + LINK a C program end to end you also need the C runtime
  libraries and the C linker auto-job (all in
  [../_shared/files/](../_shared/files/)) [verified]:
  - `NC-LIB.NRF`, `CAT-LIB.NRF`, `USLIB3.NRF`
  - `LINKER-AUTO-C.JOB`
- Known media gap: `FORTRAN-LIB` and `EXCEPT-LIB` are missing from all media,
  so the linker's default FORTRAN auto-job cannot resolve its exception
  handlers. The C path works around this with the self-contained
  `LINKER-AUTO-C.JOB`. See [../README.md](../README.md) and the memory note
  `nc-link-fortran-autojob-missing-libs` [verified].

## How to run

Start the SINTRAN shell as described in [../README.md](../README.md), then type
the bare name `NC-A06` at the `@` prompt (the `@` is the prompt, do NOT type
it).

Interaction model [verified]: NC prints its banner, then reads input one
character at a time starting from device 0 (the SINTRAN command buffer, i.e.
the initial argument line). The `NC:` prompt does not appear until the first
carriage return; on that first CR NC switches its input to the terminal
(device 1). So an interactive session needs a leading CR before the first
typed command.

The reliable, run-verified way to compile is a MODE file that runs CHECK then
GENERATE-CODE. Scripted example that produces a real `HELLO.NRF` [verified]:

```sh
# create MODE file COMPILE-HELLO.MODE in the user area with these lines:
#   CREATE-FILE HELLO:CAT
#   CREATE-FILE HELLO:LIST
#   CREATE-FILE HELLO:NRF
#   NC-A06
#   CHECK HELLO,HELLO,HELLO
#   GENERATE-CODE HELLO,HELLO
# then drive the emulator non-interactively:
printf 'LOGIN GUEST\nMODE COMPILE-HELLO\n' | ./build/bin/nd500x --monitor \
    --user GUEST --sintran-root ~/ND500USERS
```

That run writes a real `HELLO.NRF` and prints `programCAT_COMPILER terminated`
cleanly [verified].

Interactive alternative: type `NC-A06`, press Enter once to get `NC:`, then
`CHECK HELLO,HELLO,HELLO`, then `GENERATE-CODE HELLO,HELLO`, then `EXIT`.

## Commands and options

Command set from NC's own built-in `help` [from HELP]. Notation: `<x: >` is a
value NC prompts for, `[...]` optional, `...` repeatable.

| Command | Arguments / prompts |
|---|---|
| `compile` | `<source file>,<list file>,<object file>` |
| `preprocess` | `<source file>,[<list file>],[<output file>]` |
| `check` | `<source file>,[<list file>],[<CAT file>]` |
| `generate-code` | `<CAT file>,<object file>` |
| `link` | `<source file>,<program>` |
| `cross` | `<source file>,<cross reference file>,<lines per page>` |
| `format` | `<source file>,<new source file>` |
| `define` | `[<macro identifier [(identifier,...)]>],[<value>]` |
| `undef` | `[<macro identifier>]` |
| `directory` | `[<include directory/user>]` |
| `options` | `<option>...` (repeatable) |
| `library` | `<library file>...` (repeatable) |
| `value` | `<definitions / options / libraries>` |
| `page-length` | `[<lines>]` |
| `initialize-compile-parameters` | `[<initialization file>]` (reads `NC-A:INIT`) |
| `save-compile-parameters` | `[<initialization file>]` (writes `NC-A:INIT`) |
| `clear` | reset compile parameters |
| `cc` | silent, returns to `NC:` |
| `help` | `<command>` (blank lists all) |
| `exit` | leave NC |
| `@<cmd>` | pass a command to SINTRAN |

File-name trap [verified]: NC treats a dot as part of the name and appends the
default type itself. Type the bare SINTRAN name (`HELLO`), not `HELLO.C`.
Default types NC appends: source `:C`, listing `:LIST`, object `:NRF`,
preprocessed `:PP`, intermediate `:CAT`.

Option string (default, written by `save-compile-parameters` into
`NC-A:INIT`) [verified]:

```
options m2  a4  f-  r4  l+  d+  n+  s-  p-  i-  o-  pr- ic+ lm+ t-  a-  lo+
```

Each token is `<flag><+|-|digit>`. Exact per-flag meanings are not confirmed
against an NC manual - treat individual flag semantics as [UNVERIFIED].

## Verified behaviour in nd500x

- Banner prints; command-buffer -> terminal input switch on first CR
  [verified].
- CHECK writes a `:CAT`, GENERATE-CODE drives the CAT_COMPILER back end and
  writes a real `:NRF`; console prints `programCAT_COMPILER terminated`
  [verified, 2026-07-31].
- The historical "no rewrite / terminated" symptom (the compiler only
  preprocessing and never generating code) was an emulator GETB-heap bug, since
  fixed; it was NOT a compiler-driver problem [verified]. The older analysis
  documents below describe that pre-fix behaviour.

## Known issues / status

- Runs end to end (CHECK + GENERATE-CODE) in nd500x [verified].
- Single-step "all in one" `compile` was the phase that surfaced the old
  GETB-heap bug; the run-verified path of record is the two-step
  CHECK -> GENERATE-CODE MODE flow above.
- Individual option-flag meanings in `NC-A:INIT` are not manual-confirmed
  [UNVERIFIED].

## References

- [analysis/nc-a06-usage-and-mon-contract.md](analysis/nc-a06-usage-and-mon-contract.md) - command table, terminal model, and the 34-MON-call contract
- [analysis/NC-INTERFACE.md](analysis/NC-INTERFACE.md) - terminal/command interface probe results
- [analysis/nc-a06_analysis.md](analysis/nc-a06_analysis.md) - deep binary/disassembly analysis
- [analysis/nc-a06.asm](analysis/nc-a06.asm) - ND-500 disassembly
- [../README.md](../README.md) - shared install/run conventions and requirements model
- [../CAT-CAT5-B06/](../CAT-CAT5-B06/) - the CAT compiler back end NC hands off to
- [../LINKER-B01/](../LINKER-B01/) - linking the resulting `:NRF` into a `.DOM`
- ND-500 monitor-call analysis: [../../ND500/](../../ND500/) and [../../../Developer/MON/calls/](../../../Developer/MON/calls/)
