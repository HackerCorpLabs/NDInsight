# ND-10176 — COBOL for ND-100/NORD-10

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10176` |
| Product name | COBOL for ND-100 (companion: `ND-10177` COBOL for ND-500 — same PI sheet, see [../ND-10177/README.md](../ND-10177/README.md)) |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown per-version — no install PD sheet located, only the marketing PI sheet |
| Related products | `ND-10177`/`ND-210177` COBOL for ND-500 · `ND-10189` COBOL runtime System (its one located floppy is byte-identical to this product's own ISAM bundle — see [../ND-10189/README.md](../ND-10189/README.md)) · `ND-10536` COB-GEN code generator · `ND-210073` ISAM (bundled onto these floppies, see [../ND-210073/README.md](../ND-210073/README.md)) |

## Description
ANSI-74 COBOL (Level One, many Level Two selections) for ND-100/NORD-10, with ND extensions
(screen handling, multi-user Indexed/Relative file locking, `DO`/`END-DO`, `IF-THEN-ELSE-END-IF`,
`EXPORT`/`IMPORT` inter-program common areas). Programs written on ND-100 run unmodified on
ND-500. Full feature description:
[../../Product-Info/ND-10176-B1-EN.md](../../Product-Info/ND-10176-B1-EN.md). [PI]

**Two-bank mode is the default** for this compiler ("Programs are by default compiled in a
two-bank mode making it possible to run programs with 128 KBytes of code and 128 KBytes of data.
It is possible to compile in the 1-bank mode by giving the command `1-BANK` prior to the compile
command.") — the *opposite* default from FORTRAN/PLANC, which default to 1-bank and need
`SEPARATE-DATA ON` to get 2-bank (see
[../../../Developer/Workflow/TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md)). [PI]

**No install PD sheet located** — everything version-specific below is read from the actual
mounted floppies.

## Versions — pick the "latest complete" set

Three generations exist in the floppy library; they are **not equally complete**:

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A | [ND-10176A](ND-10176A/README.md) | verified from floppy | 1981, oldest, 3-part set — compiler only, no ISAM/VTM bundle |
| H00 | [ND-10176H00](ND-10176H00/README.md) | **verified from floppy — this is the latest COMPLETE set** | 1985, single floppy bundles compiler (both 1-bank and 2-bank), ISAM, and VTM screen-handling bridge together |
| H03PRE | not catalogued as its own version | **pre-release, incomplete — do not use as your primary source** | Dec 1985, newer date than H00 but this floppy carries only the compiler+banks, no ISAM/VTM bundle. The `PRE` in its own volume label (`COBOL-100-H03PRE`) marks it as a pre-release build; it may be a partial/beta distribution rather than a finished replacement for H00. |

**Recommendation: use H00**, not H03PRE — despite being dated later, H03PRE's disk (the only H03
floppy located) is missing ISAM and VTM, which the H00-D disk has. If a *complete* H03 (or later)
release turns up with all three bundled, prefer that instead.

A fourth floppy, `210176J01-XX-01D`, was found labeled but **mounts with zero files** — either
corrupt or blank; it lives in the source library's own `bad/` folder alongside several other
confirmed-corrupted COBOL images (garbled volume labels). Do not trust it without re-acquiring a
clean dump.

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-10176-A2-EN.md](../../Product-Info/ND-10176-A2-EN.md), [../../Product-Info/ND-10176-A3-EN.md](../../Product-Info/ND-10176-A3-EN.md), [../../Product-Info/ND-10176-B1-EN.md](../../Product-Info/ND-10176-B1-EN.md)
- Manual(s): `ND-60.144` COBOL Reference Manual — [../../../Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md](../../../Reference-Manuals/ND-60.144.3%20EN%20COBOL%20Reference%20Manual.md)
  · `ND-60.066` ND Relocating Loader · `ND-60.158` Symbolic Debugger User's Guide · `ND-60.171` COB-GEN Reference Manual (none of the latter three located in this repo)
- NDWIKI: not checked yet

## Provenance
All version facts from mounting the real floppy images (MD5s in each version's own doc) and
reading them with `ndtool`; the two-bank-default fact and feature list from the PI sheet.

---
**Parent:** [../README.md](../README.md) (Software catalog)
