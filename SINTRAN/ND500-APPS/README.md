# ND-500 Applications

Real Norsk Data ND-500 programs reserved with every
file needed to run them in the `nd500x` emulator, plus a user guide per program.

Each program lives in its own folder:

```
ND500-APPS/
  README.md              <- this index + shared install/run/usage conventions
  _shared/               <- runtime libraries + linker auto-jobs used by several apps
    files/               <- NC-LIB, CAT-LIB, USLIB3, PLANC-LIB, LINKER-AUTO-*.JOB, ...
  <PROGRAM>/
    userguide.md         <- what it does, requirements, invocation, options, status
    files/               <- the actual runtime files (DOM, PSEG/DSEG, HELP, INIT, ...)
    analysis/            <- disassembly (.asm) and RE notes (reference, not needed to run)
```

Full absolute path of this folder:
`E:\Dev\Ronny\NDInsight\SINTRAN\ND500-APPS\`

---

## Program index

| Program | Folder | What it is | Run status in nd500x |
|---|---|---|---|
| NC-A06 | `NC-A06/` | Norsk Data C compiler (A06, 1989) | Compiles end-to-end (CHECK+GENERATE-CODE) |
| LINKER-B01 | `LINKER-B01/` | ND Linkage editor / domain linker | Links C DOMs (see FORTRAN-auto-job note) |
| FILE-COMPARE | `FILE-COMPARE/` | Text/file diff tool (ND-10603B) | Verified: real compare + diff output |
| CPU-STAT | `CPU-STAT/` | Prints CPU type / microcode / system info | Verified: runs clean to exit |
| CONVERT-DOM-A03 | `CONVERT-DOM-A03/` | Domain-file format converter | Loads, has HELP + INIT |
| PLANC-500-G00 | `PLANC-500-G00/` | PLANC compiler (G00) | Loads, reaches prompt |
| LED-FORTRAN-A01 | `LED-FORTRAN-A01/` | FORTRAN compiler (LED-based) | Loads |
| LED-NEW | `LED-NEW/` | LED screen editor | Loads |
| AUTOMAKE-500-C00 | `AUTOMAKE-500-C00/` | Build/dependency driver (make-like) | Loads, reaches prompt |
| CODE-COVERAGE | `CODE-COVERAGE/` | Code-coverage instrumentation tool | Loads, reaches prompt |
| TEST-REAL | `TEST-REAL/` | Interactive real-number parse/echo loop | Loads |
| CAT-CAT5-B06 | `CAT-CAT5-B06/` | CAT compiler back-end (used by NC) | Runs (driven by NC) |
| BM-FILERE-B02 | `BM-FILERE-B02/` | Backup Manager file-restore component | Runs to its UI; then needs XMSG (513B) |

Status reflects the `nd500x` C emulator (WSL `~/repos/nd500x`) as of 2026-07-31.

---

## Installing a program into the emulator

The emulator's SINTRAN file system is a host directory tree (the "sintran-root"),
by convention `~/ND500USERS/` with `SYSTEM/`, `GUEST/`, `SCRATCH/` beneath it.
A domain is just a `NAME.DOM` file placed under a user directory.

1. Copy the program's `files/*` into the sintran-root:
   - system-wide programs -> `~/ND500USERS/SYSTEM/`
   - a user's own programs -> `~/ND500USERS/<USER>/`
   - the shared libraries + linker auto-jobs (`_shared/files/*`) -> `~/ND500USERS/SYSTEM/`
2. File names are UPPER-CASE with a `.DOM` extension on the host
   (e.g. `NC-A06.DOM`). PSEG/DSEG/HELP/INIT/JOB files keep their upper-case names.

That is the whole install: no registration step, no catalogue. The shell finds a
domain by looking for `<sintran-root>/<user>/<NAME>.DOM`, then `SYSTEM/<NAME>.DOM`.

## Running a program

Start the SINTRAN shell and type the program's NAME:

```
cd ~/repos/nd500x
./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

At the `@` prompt type the bare domain name (the `@` IS the prompt - do NOT type
it yourself):

```
@NC-A06            <- you type: NC-A06
```

- Arguments after the name go to the program's command buffer (e.g. `NC TEST`).
- Batch: `MODE <file>` runs a `.MODE` script (lines are `@`-prefixed inside the file).
- A program ends with `MON 0B LEAVE` ("program<NAME> terminated").

Scripted (non-interactive) drive, feeding stdin:

```
printf 'LOGIN GUEST\nCPU-STAT\nEXIT\n' | ./build/bin/nd500x --monitor \
    --user GUEST --sintran-root ~/ND500USERS
```

## Requirements model (what a program needs to run / link)

- **To RUN a DOM**: just the `.DOM` file. Some carry data in separate PSEG/DSEG
  files (e.g. LED); keep them alongside the DOM.
- **To COMPILE + LINK your own program**: the language's runtime libraries and
  the linker's language auto-job:
  - C: `NC-LIB`, `CAT-LIB`, `USLIB3` + `LINKER-AUTO-C.JOB`
  - PLANC: `PLANC-LIB` + `LINKER-AUTO-PLNC.JOB`
  - all in `_shared/files/`.
- **Known gap**: `FORTRAN-LIB` and `EXCEPT-LIB` are not present on any available
  media, so FORTRAN linking (and the linker's default FORTRAN trap auto-job)
  cannot resolve its exception handlers. The C toolchain works around this by
  using the self-contained `LINKER-AUTO-C.JOB`. See the memory note
  `nc-link-fortran-autojob-missing-libs` and each tool's userguide.

## How to discover a program's own commands/options

1. **Built-in HELP** - most tools accept a `HELP` (or `help`) command at their
   prompt; some read a `NAME.HELP` file (LINKER-B01, CONVERT-DOM ship one - see
   the program's `files/`).
2. **The `.HELP` / `.INIT` files** in `files/` - the vendor help text and the
   startup command script the program runs on entry.
3. **The disassembly** in `analysis/*.asm` and the RE notes (`*.md`) - the
   authoritative source when the manual is missing. Mark anything derived from
   disassembly as such; do not present a guess as fact.

---

## Related

- Emulator: `~/repos/nd500x` (WSL) - `--monitor` SINTRAN shell.
- ND-500 monitor-call analysis: `../ND500/` and `../../Developer/MON/calls/`.
- Skill: `nd500-apps` (install + usage-discovery workflow for this folder).
