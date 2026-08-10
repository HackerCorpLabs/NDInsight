# ND-210336F01 — ND-100 Symbolic Debugger, version F (rev 01)

> Status: IN-PROGRESS — install command and a real worked ND-100 debugging session both sourced from the actual manual; not yet run live   ·   Install source: [manual] + [OBS]

| Field | Value |
|-------|-------|
| Part number | `210336F01` |
| Base product | [`ND-210336`](../README.md) |
| Version | F, revision 01 |
| Release date | file dated 1986-04-29 |
| CPU target | ND-100 |
| OS requirement | unknown |

## Description
Single-file floppy — the debugger shipped as a raw `:BPUN`, the standard ND-100 reentrant-dump
shape (unlike the ND-500 debugger, which ships as `:NRF` and gets loaded per-program via the
Linkage-Loader — see [ND-10335B](../../ND-10335/ND-10335B/README.md) for that different
mechanism).

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210336F01-XX-01D` | `DEBUGGER-F01:BPUN` (87 pages), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `6e073738400aa362f06fcbf827814808`) and reading with
`ndtool -t`.

## Installation procedure

**The exact reentrant-dump command is real, found verbatim in the actual System Supervisor
manual's own worked example of defining standard reentrant subsystems** (the same source already
used for [ND-10311A Assembler-500](../../ND-10311/ND-10311A/README.md)'s install command):

```
@DUMP-REENTRANT DEBUGGER-F 0 1 (BPUN-FILES)DEBUGGER-F:BPUN DEBUG
```
— start address `0`, restart address `1`, source file `(BPUN-FILES)DEBUGGER-F:BPUN`, reentrant
segment named `DEBUG`.

Before this works, copy the floppy's file to `(BPUN-FILES)`:
```
@ENTER-DIRECTORY 210336F01-XX-01D,FLOPPY-DISC-1,0,
@COPY-FILE "DEBUGGER-F01:BPUN",(BPUN-FILES)DEBUGGER-F:BPUN
```
(the manual's command references the file as `DEBUGGER-F:BPUN`, without the `01` revision suffix
present in this floppy's own file name `DEBUGGER-F01:BPUN` — rename during copy, or adjust the
`DUMP-REENTRANT` file argument to match whichever name you actually use; not independently
resolved here.)

## Usage — real, worked ND-100 example from the manual

Source: `ND-60.158.5 EN` Symbolic Debugger User Guide, "Using the Symbolic Debugger" — an actual
transcript explicitly labeled `ND-100 SYMBOLIC DEBUGGER. VERSION 7.`, verbatim: [manual]

```
@DEBUG
```
(entered without any `:PROG-file` specification — be on user `SYSTEM` or the program's own user)
```
ND-100 SYMBOLIC DEBUGGER. VERSION 7.
*ATTACH-REENTRANT-SEGMENT (own-user)-cobol,acobol
```
Links a `:PROG` file (built by the BRF-Linker — see
[LINKING-GUIDE.md](../../../../Developer/Workflow/LINKING-GUIDE.md)) to the segment its main
program resides on (`<segment>` may be a name or a number):
```
COBOL PROGRAM. (Segment 156B) A-COBOL.1
*REENTRANT-PLACE person
```
Initiates the reentrant program system for the debugger, on the reentrant subsystem
`person` — the debugger reports the program type, its segment number, and confirms it's
positioned at the first line (internal name `A-COBOL`).

**For a multi-segment program, attach every segment involved** — here a subroutine on a second
segment:
```
*ATTACH-REENTRANT-SEGMENT (own-user)b-cobol,bcobol
```

**Then the actual debugging loop — `BREAK`, `RUN`, `DISPLAY`, `SET`:**
```
*BREAK b-cobol.18
*RUN
Break at (Segment 157B) B-COBOL.18
*DISPLAY age
AGE=28
*SET age=45
*BREAK a-cobol.18
*RUN
```
`BREAK <routine>.<line>` sets a breakpoint at a source line in a named routine; `RUN` executes
until the next breakpoint (reporting which segment it broke in); `DISPLAY <item>` prints a
variable's current value; `SET <item>=<value>` changes it. This matches the general command shape
already seen on the ND-500 debugger ([ND-10335B](../../ND-10335/ND-10335B/README.md)'s
`DEBUG-PLACE`/`RUN`/`SCOPE`) — same underlying tool family, `REENTRANT-PLACE`/
`ATTACH-REENTRANT-SEGMENT` being the ND-100-specific commands for connecting to a reentrant
subsystem instead of a raw domain.

Other real command names found in the same manual's command index (exact syntax not transcribed
here — see the manual directly): `PLACE <file-name> (W)` (non-reentrant, single-segment programs
— the `W` option presumably grants write access, matching `ND-210721C` BRF-Linker's own
`DEBUG-PLACE`-adjacent note that write access needs a special command or `DEBUGGER commands
respond with ATTEMPT TO MODIFY READ-ONLY SEGMENT`), `RT-PLACE <program-name>` (RT/background
programs), `FORMATS-DISPLAY <A,D,F,H,O>` (choose numeric display format), `DUMP`.

## Configuration / post-install
Add the `DUMP-REENTRANT` line to the site's `(UTILITY)DUMP-REENTRANT:MODE` (called from
`HENT-MODE`) for cold-start persistence — same generic pattern already documented in
[../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md §12](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md).

## Documentation
- PD-sheet: not located
- PI-sheet: not located under this article number
- Manual(s): `ND-60.158` Debugger User's Manual — [../../../../Reference-Manuals/ND-60158-5-EN Symbolic Debugger - User Guide.md](../../../../Reference-Manuals/ND-60158-5-EN%20Symbolic%20Debugger%20-%20User%20Guide.md)

## Provenance & open items
- Source: floppy directory listing via `ndtool`; install command quoted verbatim from
  *ND-30.003.7 EN SINTRAN III System Supervisor*; usage session quoted verbatim from
  *ND-60.158.5 EN Symbolic Debugger User Guide*.
- **TODO:** resolve the `DEBUGGER-F` vs `DEBUGGER-F01` file-name mismatch between the manual's
  install example and this floppy's actual file name.
- **TODO:** transcribe `PLACE`/`RT-PLACE`/`FORMATS-DISPLAY`/`DUMP` exact syntax from the manual
  if a non-reentrant or RT-program debugging workflow is needed.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210336` product overview)
