# ND-10076J — Pascal for ND-100, version J

> Status: VERIFIED (transcribed from PD sheet + separate installation guide; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `10076J` (source article: `10157J`) |
| Base product | [`ND-10076`](../README.md) |
| Version | J |
| Release date | 83.12.07 (PD sheet) / installation guide dated 84.01.18 / diskette dated 84.02.07 |
| CPU target | NORD-10 / ND-100, 48-bit floating-point |
| OS requirement | SINTRAN III VS |

## Description
Compile and execute Pascal programs. A two-bank compiler — any terminal running it, or
two-bank Pascal programs, needs 128K-word user segments.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10076J` | `PASCAL-COD-J:BRF` (78 pages, the compiler) · `PASCAL-LIB-J:BRF` (14 pages, one-bank runtime) · `PASCAL-ZLIB-J:BRF` (14 pages — almost certainly `PASCAL-2LIB-J`, two-bank runtime; the installation guide consistently calls it `PASCAL-2LIB` while only the diskette-listing page prints `ZLIB` — the same kind of single-character OCR slip as this catalog's other `:IFUN`/`:BPUN` cases, "2"→"Z") · `PASCAL-ERR-J:SYMB` (5 pages, compile-time error messages), user `FLOPPY-USER` |

## Installation procedure

Source: the separate "Installation of ND-100 Pascal, Version J" document, verbatim — this is the
exact text already quoted as the worked example in
[../../INSTALL-METHODOLOGY.md §5](../../INSTALL-METHODOLOGY.md). [PD]

1. Copy `PASCAL-LIB` to a system file `PASCAL-LIB:BRF`. Copy `PASCAL-2LIB` (see the `ZLIB`/`2LIB`
   naming note above) to a system file `PASCAL-2LIB:BRF`.
2. Copy `PASCAL-ERR` to a system file `PASCAL-ERR:SYMB`.
3. Build and dump as a reentrant subsystem — **branches by SINTRAN version**:

   **SINTRAN version H:**
   ```
   $NRL
   *IMAGE-FILE 100
   *SIZE 1500
   (o)*DEFINE LINPP n
   (o)*DEFINE NOBUF 4
   *LOAD PASCAL-COD PASCAL-2LIB
   *VALUE PASCAL      → xxxxxx
   *VALUE CONTINU     → yyyyyy
   *DUMP "PASCAL:PROG",xxxxxx,yyyyyy
   *EXIT
   $DITAP "PASCAL" PASCAL
   $DUMP-REENTRANT PASCAL,xxxxxx,yyyyyy,PASCAL
   ```

   **SINTRAN version I or later:**
   ```
   $NRL
   *IMAGE-FILE 100
   *SIZE 1500
   (o)*DEFINE LINPP n
   (o)*DEFINE NOBUF 4
   *LOAD PASCAL-COD PASCAL-2LIB
   *VALUE PASCAL      → xxxxxx
   *VALUE CONTINU     → yyyyyy
   *DUMP "PASCAL:PROG",xxxxxx,yyyyyy
   *EXIT
   $DUMP-PROGRAM-REENTRANT PASCAL,PASCAL
   ```
   `(o)` = optional (`*DEFINE LINPP n` = lines per page, octal; `*DEFINE NOBUF 4` = files
   buffered, improves `$INCLUDE`-heavy compiles).

"The installation is now complete, and the system can be used according to the user manual." [PD]

## Configuration / post-install
None beyond the reentrant dump above. The compiler needs a 128K-word terminal background segment
(see [ND-210721C](../../ND-210721/ND-210721C/README.md)'s `@CHANGE-BACKGROUND-SEGMENT-SIZE`
command for the equivalent BRF-Linker case — not independently confirmed the exact command form
is stated for Pascal, but the requirement itself is explicit on the PD sheet).

## Documentation
- PD-sheet: [../../../../Reference-Manuals/19831207_ND-10076J_PASCAL_for_ND-100_NORD_Software_Library_Program_Description.md](../../../../Reference-Manuals/19831207_ND-10076J_PASCAL_for_ND-100_NORD_Software_Library_Program_Description.md)
- Installation guide: [../../../../Reference-Manuals/19840118_ND-100_Pascal_version_J_Installation.md](../../../../Reference-Manuals/19840118_ND-100_Pascal_version_J_Installation.md)
- Diskette listing: [../../../../Reference-Manuals/19840207_ND-10076J_PASCAL_for_ND-100_NORD_Software_Library_Diskette.md](../../../../Reference-Manuals/19840207_ND-10076J_PASCAL_for_ND-100_NORD_Software_Library_Diskette.md)
- Manual(s): `ND-60.124.5` ND-Pascal Reference Manual

## Provenance & open items
- Source: three OCR'd documents (PD sheet, installation guide, diskette listing), already
  cross-used as this repo's generic methodology worked example.
- **TODO:** confirm `PASCAL-ZLIB-J` vs `PASCAL-2LIB-J` against the mounted floppy.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10076` product overview)
