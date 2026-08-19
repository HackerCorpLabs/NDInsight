# Building Screen-Oriented User Interfaces with VTM

**VTM (Virtual Terminal Manager) is SINTRAN III's terminal-independence layer — how a
screen-oriented program (COBOL screen handling, FOCUS, NOTIS, ...) works identically across dozens
of physical terminal models without knowing any of their individual escape-sequence dialects.**

Every claim here is sourced from a real manual already in this repo, cited inline. Where no
source exists, it says so plainly instead of inventing one.

---

## 1. What VTM actually is, and the one honest limit on this document

Per the internal library-and-resource list in *ND-Specific Programming & Advanced PLANC*
(`ND-20034-1-EN`), VTM is listed with **manual number "Internal"** — the same status as a
handful of other Norsk Data internal-only libraries (Buffer Mgmt, MLE, S-LIB, SLE). **There is no
externally-published VTM programming manual in this repo, and none has been found in the wider
archive searched.** This means:

- The low-level VTM call interface (the actual routine names/parameters a PLANC or FORTRAN
  program would call directly) is **not documented anywhere in this repo**, and this document
  does not invent it.
- What *is* well documented, and is the actual substance of this chapter, is: (a) how VTM is
  **configured** at the system level (terminal types, the `DDBTABLES:VTM` file), and (b) how a
  **language's own screen-handling extensions** (COBOL's, concretely) sit on top of VTM — which
  is almost certainly what "programming a user interface using VTM terminal tables" means in
  practice, since that is how every real, sourced example in this repo actually uses VTM.

## 2. Terminal types — the configuration layer

Source: *ND-30.003.7 EN SINTRAN III System Supervisor* (already in this repo, `Operations/SINTRAN/`).

Every terminal model "speaks" a different escape-sequence dialect. SINTRAN abstracts this with a
**terminal type** number, and a system-wide file that maps each type to its actual codes:

> "The file DDBTABLES:VTM contains terminal dependent codes and is used by screen-oriented
> applications." [manual, §6.4.1]

Set your own terminal's type (any user may do this for their own terminal; `SYSTEM` may set any
terminal's type):
```
@QSET-TERMINAL-TYPE (<terminal number>) (<terminal type>)
```
Read it back:
```
@QGET-TERMINAL-TYPE (<terminal number>)
```
Type `0` is a dummy/unset value — "the application programs using VTM will prompt the user for
terminal type" if left at 0. **Nothing else needs to be set manually** — "It is not necessary to
set anything but the terminal type before running an application using VTM. The remaining
attributes will then [be] modified automatically." [manual, CTYTP section]

### The standard terminal-type table (excerpt — Table 18 of the manual)

| Type | Model | Type | Model |
|---|---|---|---|
| 0 | dummy (not set) | 48 | Hazeltine-Executive-80 |
| 2 | Teletype ASR-33 | 50 | Volker-Craig-VC404Q (ADM-3A) |
| 3 | Tandberg TDV2115-Standard | 53 | Tandberg TDV2200/9-ND-NOTIS |
| 6 | DEC-VT100 (80 column) | 57 | Facit 4420-ND-NOTIS |
| 9 | ND-NCT (Nord Colour Terminal) | 79 | IBM Personal Computer |
| 11 | DEC-LA36 (Decwriter-II) | 91/92 | Facit Twist (24-line / 72-line mode) |
| 29 | DEC-VT52 | 96 | RACAL NORSK Bit-mapped screen |

(Full table has ~70 entries — see the manual for the complete list. Types 2 and 11 are
line-oriented, not screen-oriented — VTM programs can still run on them, degraded.)

### CTYTP — how the terminal type is actually stored

The system variable `CTYTP` packs the terminal type into bits 0–7; higher bits (`ND`, `VDU`,
`BS`, `FF`, `CPOS`, `ESC`) are auto-derived from `DDBTABLES:VTM` once the type is set — you do not
set these yourself. [manual, "CTYTP - Terminal type"]

## 3. Building and extending terminal tables — the real `VTM-COMPOUND` procedure

Source: the real PD sheet for `ND-211464` "VTM terminal tables (Type 128/129) DEC VT200" — see
[Installation/Software/ND-211464](../../Installation/Software/ND-211464/README.md) for the full
install writeup this section summarizes. This is the concrete, sourced answer to "how do you
actually work with VTM terminal tables":

- **Version A** of VTM used one file per terminal type, `DDBnnn-A:VTM`, with `DDB999-A:VTM`
  holding the standard set.
- **Version B onward** consolidates into one composite file: `DDBARRAYS-B:VTM` (version B) or
  `DDBTABLES-n:VTM` (version C onward) — both hold equivalent data, and can be renamed into each
  other if needed (`@RENAME-FILE DDBTABLES-C:VTM DDBARRAYS-B:VTM`).
- A dedicated tool, **`VTM-COMPOUND`**, edits these composite files interactively. Two real,
  verbatim menu-driven procedures from the PD sheet:

**Add a new (non-standard) terminal type, version B onward:**
```
@VTM-COMPOUND-E09
2    (ADD TERMINAL TYPES)
2    (DDBTABLES-n:VTM)
E    (E-Version)
47   (the new terminal type number)
777  (no more DDB-files to add)
9    (EXIT)
```

**Generate a loadable BRF/NRF file from the compounded table** (so an application can load its
own terminal tables alongside the program itself, instead of relying on the system-wide file):
```
@VTM-COMPOUND-E09
4    (GENERATE A FILE WITH BRF/NRF FORMAT)
2    (DDBTABLES-n:VTM)
E    (E-Version)
1    (ND-100, 1-bank)   / 2 (ND-100, 2-bank)   / 3 (ND-500)
4    (RETURN)
9    (EXIT)
```
The resulting file (`VTM-ARRAY-D:NRF` on ND-500, or `VTM-(128/129)-ARRAY-D:BRF` on ND-100) is
loaded together with the program system via NRL/the ND-500 loader — see
[LINKING-GUIDE.md](LINKING-GUIDE.md).

**Version A procedure** (older, single-file-per-type scheme) instead edits `DDB999:VTM` directly:
```
@VTM-COMPOUND-E09
8    (EDIT THE CONTENT IN DDB999:VTM)
2    (ADD TERMINAL TYPE DESCRIPTIONS)
47   (new terminal type)
777  (no more terminal types)
9    (EXIT)
```

## 4. How an application actually presents UI through VTM — the COBOL example

No repo source documents raw VTM calls, but **COBOL's screen-handling extensions are a fully
documented, real example of a language built on top of VTM** (see the COBOL PI sheet, already
cited in [ND-10176](../../Installation/Software/ND-10176/README.md)):

```cobol
BLANK     LINE IND
DISPLAY   (IND 1) 'Give password:' WITH INVERSE-VIDEO
ACCEPT    (IND 16) PASSWORD WITH BEEP INVISIBLE

  UP        ENTER-USER
  LEFT      ENTER-USER
  HOME      EXIT-PROGRAM
            CONTROL CHECK-PASSWORD
```
Statement shapes:
```
ACCEPT   [position spec.] identifier with options
DISPLAY  [position spec.] identifier or literal with options
BLANK    SCREEN
BLANK    LINE i [TO j] COLUMN n [TO m]
```
where **position spec.** is a line/column pair on the screen (identifier or literal). Real
options table (from the PI sheet): `BEEP`, `BLINK`, `INVERSE-VIDEO`, `LOW-INTENSITY`,
`UNDERLINE`, `NORMAL`, `SPACE-FILL`, `MUST`, `UPPER-CASE`, `AUTO-SKIP`, `INVISIBLE`,
`LENGTH-CHECK`, `PROMPT`, `UPDATE`, `LISTEN`, `AUTO-ERASE`, `BLANK-WHEN-ZERO`,
`JUSTIFIED-RIGHT`, plus per-key navigation labels (`DOWN`/`EXIT`/`HOME`/`LEFT`/`RIGHT`/`UP`/
`CONTROL`/`HELP`/`RE-DISPLAY`/`TIME-OUT`/`F1`-`F8`/`CANCEL` — each takes a label to branch to).

**The wiring between COBOL and VTM is not abstract** — this repo has a real, decoded example: the
`VTM-BRIDGE-1-H00:MODE`/`-2-H00:MODE` scripts on the
[ND-10176H00](../../Installation/Software/ND-10176/ND-10176H00/README.md) floppy are literal
BRF-editor patches that strip specific low-level video/terminal-control units out of the COBOL
runtime library and splice in a VTM bridge module in their place. That is the actual mechanism —
not a documented call convention, but a binary-level integration performed once at install time.

**The alternative, per the COBOL PI sheet:** FOCUS screen handling (a separate library, manual
`ND-60.137`, not in this repo) can be used instead of COBOL's built-in VTM-backed screen handling
— "The programmer may choose to use either the screen handling system (incorporated in COBOL) or
... FOCUS."

## 5. Related products

- **[PLANC-SCREEN-H](../../Installation/Software/ND-PLANC-SCREEN-H/README.md)** — a real,
  decoded PLANC screen-handling library built directly on top of VTM ("Vtm... must be loaded
  together with this program", per its own demo program's header comment). Ten real callable
  routines (`bytdis`/`bytacc`, `intdis`/`intacc`, `realdis`/`realacc` for field display/edit,
  `frame`/`fullbar`/`sparsebar` for boxes and bars, `blankscreen`/`blankarea`/`resetscreen`), plus
  a real `.PICT` screen-picture file format (`%HEADING`/`%CONTROL`/`%DEFINITIONS`/`%ATTRIBUTES`
  sections, `@position`/`@size`/`@field-defaults` directives) — a fourth documented UI-definition
  syntax in this catalog, alongside VTM's raw API, NSHS's "pictures", and UNIQUE's `start-form`.
- [`ND-10013`](../../Installation/Software/ND-10013/README.md) NSHS (NORD Screen Handling
  System) — real BRF runtime files decoded (1-bank/2-bank/reentrant); its own "picture" file
  format looks conceptually close to PLANC-SCREEN-H's `.PICT` format above, but no NSHS manual has
  been found to confirm whether they're the same format or two independent ones.
- [`ND-211464`](../../Installation/Software/ND-211464/README.md) — VTM terminal tables for DEC
  VT200 (Type 128/129), fully documented from a real PD sheet, see §3 above.
- [`ND-210455`](../../Installation/Software/ND-210455/README.md) "VTM terminal tables
  (Standard)" — the base set of ~60 standard terminal-type descriptors plus a second installer
  (`INSTALL-TABLES:PROG`) not seen on any other VTM product; floppy contents confirmed, install
  procedure not yet established (no PD sheet, and the installer is a compiled program, not a
  `:MODE` script).
- `ND-211024` SINTRAN III Configuration Program — a real, unrelated-but-adjacent example of a
  VTM-dependent screen-oriented tool: "The configuration program is a screen-oriented program...
  requires that the file DDBTABLES:VTM containing terminal-dependent definitions is present on
  user SYSTEM." [System Supervisor manual, §on SINTRAN III Configuration Program]

---

## See Also

- **[LINKING-GUIDE.md](LINKING-GUIDE.md)** — how compiled VTM-array files get loaded alongside a
  program.
- **[COBOL-DEVELOPER-GUIDE.md](../Languages/Application/COBOL-DEVELOPER-GUIDE.md)** — the
  concrete language example this document leans on.
- *ND-30.003.7 EN SINTRAN III System Supervisor* — the source for §2
  ([Operations/SINTRAN/](../../Operations/SINTRAN/)).
