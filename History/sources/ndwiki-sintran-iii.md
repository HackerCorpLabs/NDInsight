# Source: ndwiki article "SINTRAN III"

- **Live page**: <https://www.ndwiki.org/wiki/SINTRAN_III>
- **Copy used here**: Wayback Machine snapshot of 17 December 2024
  http://web.archive.org/web/20241217193317/https://www.ndwiki.org/wiki/SINTRAN_III
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY, and largely redundant with material this repo already holds
- except at both ends.** It closes with a long list of real ND document numbers,
so its detail is traceable even though the body is not footnoted.

**Read this next to `SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md`,
which is better.** That file covers versions **J through N** in depth, compiled
from the primary release manuals we hold, with monitor calls, commands, file
system and hardware support per version. Anything this wiki page says about J to N
should defer to it.

## What the wiki page adds that our file does not

- **Versions A through I.** Our release history starts at J. The wiki sketches the
  earlier ones - A possibly 1974, F corresponding to ND-60.125.02 of October 1981,
  **H the last version available for the NORD-10**, and I as the first with the
  four VSE/VSX product numbers (ND-10174 VSE, ND-10575 VSX, ND-10175 VSE-500,
  ND-10576 VSX-500). Thin, and openly unsure - "Released which year? 1974?"
- **The predecessors.** SINTRAN III dates from 1974 and, "unlike its predecessors
  **SINTRAN I and II**, it was entirely written by Norsk Data". **SINTRAN I** is a
  name in nothing else we hold. Before SINTRAN III you had to choose between
  **SINTRAN II for real-time and NORD-TSS for timesharing**; III did both.
- **What it was written in**: the SINTRAN III core and file system in **NORD PL**.
- **How it felt to use** - the part no manual captures well. Command and filename
  abbreviation between hyphens, so `LIST-ALL-FILES` becomes `L-A-F` and `PED H-W`
  opens `HELLO-WORLD:SYMB` if that is the only match; trailing commas to skip
  prompts, `LI-FI,,,`; double quotes to force a new file rather than match an
  existing one. The page notes the hyphen sits where a US keyboard has the slash,
  on Norwegian keyboards. The **USER-ENVIRONMENT** shell was the popular way to
  constrain users.
- **How timesharing actually worked**: a real-time background program per
  terminal, priorities adjusted continuously by the RT monitor rather than fixed,
  with dedicated RT programs able to outrank any timesliced terminal program.
  Release **J** made the priority sequence configurable; release **K** let any RT
  program be timesliced.
- **VSE and VSX spelled out**: VSE is Virtual Storage Extended, VSX is the same for
  CX CPUs. **K was the last version with both**; L onward is VSX only and needs an
  ND-100/CX or newer.

## Two things to check against our own file

- **When the NORD-10 was dropped.** The wiki says **H** was the last version
  available for the NORD-10. Our release history has "NORD-10 dropped" under
  **J**. If H was the last, the drop happened at I, not J. One of the two is off.
- **The K-version date.** The wiki cites release notes ND-60.230.4 from **May
  1987**; our file lists K as ND-60230.5 (v5), **May 1988**. These may simply be
  two editions of the same release information rather than a contradiction - but
  note the wiki also mislabels that document's title as "J-version".

## Incidentally

The photograph at the top of the article is captioned "A booting SINTRAN III
VSX/500 K on a **ND-110 Satellite**" - another machine variant name, and further
evidence that the Satellite range was real and shipped.

The reference list independently repeats **ND-60.081 NORDNET SYSTEM
DOCUMENTATION**, which the primary SINTRAN III Communication Guide also carries.

**Note on scope**: per `CLAUDE.md`, operating-system detail belongs under
`SINTRAN/`, not in the machine history. This source is kept here because it
arrived with the history research; the substance belongs in the SINTRAN tree.

---

## Verbatim extract

A booting SINTRAN III VSX/500 K on a ND-110 Satellite. Photo of the terminal connected to the console.

SINTRAN III is a real-time, multitasking, multi-user operating system used with Norsk Data computers from 1974. Unlike its predecessors SINTRAN I and II, it was entirely written by Norsk Data.

Sintran III core and file system was written in NORD PL, intermediate language for Norsk Data computers.[1]

SINTRAN was primarily a command line based operating system though there were several shells which could be installed to control the user environment more strictly, by far the most popular of which was USER-ENVIRONMENT. 

One of the clever features was to be able to abbreviate commands and filenames between hyphens, for example LIST-FILES when typed would ask you for several prompts including print, paging etc, you could override this using the following LI-FI,,, which would abbreviate the LIST-FILES command prompt and bypass any of the prompts. One could also refer to files in this way, say, with PED H-W which would refer to HELLO-WORLD:SYMB if this was the only file having H, any number of characters, a hyphen, a W, any number of characters, and any file ending. 

In case the user wanted to actually edit a new file called H-W:SYMB (with no automatic expansion to match an existing file), the file name would have to be enclosed in double quotes, i.e. PED "H-W:SYMB".
The double quotes automatically created a new file. The other common way to create a file was to use the SINTRAN command CREATE-FILE.

The abbreviation concept saved quite a lot of keystrokes and would allow users a very nice learning curve, from complete and self-explanatory commands like LIST-ALL-FILES to L-A-F for the advanced user. (The hyphen key on Norwegian keyboards resides where the slash key does on U.S. ones.)

SINTRAN III implements timeslicing, needed for timesharing, by dedicating a real-time background program to each terminal. The priority of these RT background programs are not fixed, but are regularly adjusted by the RT monitor. Unless in I/O wait the program with the highest priority runs, and the RT monitor lowers the priority of the active program if its allocated time has been consumed. Dedicated real-time programs may still have a higher priority than the highest possible priority of the time-sliced terminal RT background programs (i.e. an RT program not in I/O wait, or otherwise suspended, will have priority).
Initially the sequence of priorities the background program went through was fixed, but SINTRAN III Release J made this configurable. From SINTRAN III Release K it was also possible to run any RT program as a timesliced program. Prior to SINTRAN III one would have to decide between SINTRAN II for real-time use, or NORD-TSS for timesharing. SINTRAN III serves both purposes.

### Contents

- 1 See also

- 2 Releases

- 3 VSE/VSX

- 4 References

- 5 External links

### See also

- SINTRAN III Real Time system

- SINTRAN III Disk Layout

- SINTRAN III Commands

- File Formats

- Programming Languages

- NORD PL

- PLANC, Program LAnguage for Norsk Data Computers

### Releases

This section lists the releases of SINTRAN III.

A-version
Released which year? 1974?
B-version

C-version

D-version

E-version

F-version
1981? ND-60.125.02 SINTRAN III INTRODUCTION from October 1981 says "Corresponds to the F version of SINTRAN III".
G-version

H-version
Last version available for NORD-10.
I-version

SINTRAN III VSE, version I ND-10174
SINTRAN III VSX, version I ND-10575
SINTRAN III VSE-500, version I ND-10175
SINTRAN III VSX-500, version I ND-10576

J-version
Release notes (ND-60.230.01 SINTRAN III J-version Release Information ) from January 1985.

Introduced time-slice classes and a new time slicer program, and commands to define time-slice classes and specify time-slice class for programs.
Modified monitor calls: BRKM (MON 4), FIXC5 (MON 61), ABSTR (MON 131), LAMU (MON 315). New monitor calls: FSMTY (MON 327), TERST (MON 330), TREPP (MON 332), UDMA (MON 333), GETXM (MON 334), IOMTY (MON 336), SPCHG (MON 337)

K-version
Release notes (ND-60.230.4 SINTRAN III J-version Release Information ) from May 1987.

Any RT-program can now be time sliced. New commands: DEFINE-MASS-STORAGE-UNIT, DELETE-MASS-STORAGE-UNIT, GIVE-OBJECT-BLOCKS, LIST-MASS-STORAGE-UNITS, SET-INITIAL-FILE-ACCESS, SET-INITIAL-FRIEND-ACCESS, TAKE-OBJECT-BLOCKS, UNLOCK-DIRECTORY. Commands removed: COMMUNICATIONS-LINE-STATUS, COMMUNICATIONS-STATUS, LOCAL, REMOTE, REMOTE-LOAD, REMOTE-PASSWORD, START-COMMUNICATION, STOP-COMMUNICATION. New monitor calls: RSREC (MON 340), SGMTY (MON 341), ADP (MON 342), CONFG (MON 343), PERFO (MON 344), MTAD (MON 345), 5MTRANS (MON 515).

SINTRAN III/VSX ND-210575K
SINTRAN III/VSX-500 ND-210576K
SINTRAN III/VSX-500 (standard system D) ND-220046K03
SINTRAN III/VSX-500 (standard system C) ND-220047K03
SINTRAN III/VSX-500 (standard system B) ND-220048K03

L-version
this version has Y2K patches[2].

Release notes (ND-860230.6 SINTRAN III L-VERSION RELEASE INFORMATION ) from September 1988. A ND-100/CX cpu is the minimum required hardware to run this version. New commands: AUTOMATIC-ND5000-ERROR-MESSAGES, FILE-SYSTEM-ERROR-MESSAGES, LIST-ALL-OPEN-FILES, LIST-SERVERS, SET-DIRECTORY-AVAILABLE, SET-DIRECTORY-UNAVAILABLE, START-SERVERS. Monitor calls (ND-100) modified: TERMO (MON 52), ABSTR (MON 131), WSEG (MON 164), CPUST (MON 262), MLAMU (MON 315), FSMTY (MON 327), ADP (MON 342), CONFG (MON 343). New monitor calls: NUCL (MON 347), RWSEG (MON 350). 

M-version
this version has Y2K patches.

New command: EXPAND-DIRECTORY.
Removed command: COPY-DIRECTORY.
New monitor calls: IOPEN (MON 351), EVENT (MON 352), PLACE (MON 441) (ND-500/5000 only).

N-version
this version has Y2K patches.

Introduces a lot of performance enhancements (e.g. multi-threaded ND-5000 swapper).

### VSE/VSX

VSE is short for Virtual Storage Extended, VSX is short for Virtual Storage Extended for CX CPU:s. Version K was the last version which had both VSE and VSX variants. Version L and later were VSX only (and work only on ND100/CX and newer systems)

### References

Norsk Data Document ND-30.003.06A SINTRAN III System Supervisor printed 1985

Norsk Data Document ND-30.049.1 EN SINTRAN III TUNING GUIDE printed May 1985

Norsk Data Document ND-60.050.03 SINTRAN III USERS GUIDE printed January 1975

Norsk Data Document ND-60.051.03 SINTRAN III REAL TIME LOADER printed February 1976

Norsk Data Document ND-60.062 SINTRAN III SYSTEM DOCUMENTATION 

Norsk Data Document ND-60.072 SINTRAN III REAL TIME LOADER SYSTEM DOCUMENTATION 

Norsk Data Document ND-60.081 NORDNET SYSTEM DOCUMENTATION 

Norsk Data Document ND-60.112 SINTRAN III SYSTEM DOCUMENTATION APPENDIX A-DATA FIELDS 

Norsk Data Document ND-60.125.02 SINTRAN III INTRODUCTION printed October 1981

Norsk Data Document ND-60.128.03 SINTRAN III REFERENCE MANUAL printed February 1983

Norsk Data Document ND-60.132.01 SINTRAN III TIMESHARING/BATCH GUIDE printed June 1980

Norsk Data Document ND-60.133 SINTRAN III REAL TIME GUIDE 

Norsk Data Document ND-60.134.02 SINTRAN III COMMUNICATION GUIDE printed November 1981

Norsk Data Document ND-60.136 ND-500 LOADER/MONITOR 

Norsk Data Document ND-60.151.02A SINTRAN III UTILITIES MANUAL printed February 1985

Norsk Data Document ND-60.163.4 EN COSMOS USER GUIDE printed May 1986

Norsk Data Document ND-60.164 COSMOS PROGRAMMER GUIDE 

- ^ Norsk Data Document ND-60.047.03 NORD PL USER'S GUIDE Page 3

- ^ Sintran Y2K patch kit description

- This article was originally a copy of the English Wikipedia article SINTRAN III in 1 November 2008.

### External links

- NODAF's guide to use of SINTRAN (Norwegian)

- NODAF's library of SINTRAN documentation (Wiki is Norwegian, docs aren't)
