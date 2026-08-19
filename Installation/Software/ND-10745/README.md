# ND-10745 — ND-Ada (subset compiler)

> Status: VERIFIED — real floppy images decoded, including two working `:MODE` scripts (build + compile/link/run)

| Field | Value |
|-------|-------|
| ND article number | `ND-10745` |
| Product name | ND-Ada |
| Functional category | Programming Languages & Compilers |
| CPU target | ND-500 |
| OS requirement | SINTRAN III version J or later; minimum 4 MB memory recommended |
| Related products | `ND-211114` ADA for ND-500/5000 — see [../ND-211114/README.md](../ND-211114/README.md). **Confirmed a different product**, not just an older article number for the same one: this PI sheet explicitly says "ND-Ada is a **subset** of the Ada programming language" (four domains `ADA:`/`ADA.LIB:`/`ADA.LINK:`), where `ND-211114`'s PD sheet claims full validation against Ada Compiler Validation Suite v1.9 with a different four-domain shape (`ADA-INTERFAC`/`ADA-COMPILER`/`ADA-LIBRARIA`/`ADA-PREPARSE`). Earlier notes on `ND-211114`'s page flagged the relationship as unconfirmed — this PI sheet resolves it: they are two distinct compilers, not two revisions of one product. |

## What is known
A subset of Ada — "has all the important features of Ada and includes all the facilities crucial
to a proper understanding of how Ada should be used" — full separate compilation, generic
packages/subprograms, constrained/unconstrained arrays, records, access types, full exception
handling, TEXT-IO, interfacing to non-Ada (PLANC) programs via interface pragma, automatic
load-and-go, and integration with the ND-500 Symbolic Debugger. [PI]

**Real floppy images found and decoded** (via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) — see
the version doc below for the full file inventory and two real, byte-for-byte decoded `:MODE`
scripts: building the shared `ADA-ENVIRONMENT` library, and compiling/linking/running a demo
program (Tower of Hanoi) end to end.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A (rev 00) | [ND-10745A00](ND-10745A00/README.md) | VERIFIED — real `:MODE` scripts decoded | floppies `10745A00-XX-01D`/`-02D` |

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10745-A1-EN.md](../../Product-Info/ND-10745-A1-EN.md)
  (a multi-product bundled sheet — also covers `ND-10755`/`ND-210755` ND-500 BASIC on page 2, already documented)
- Manual(s): `ND-60.198` ND-Ada User Manual · `ND-60.158` Symbolic Debugger User Manual ·
  `ND-60.136` ND-500 Loader/Monitor — none located in this repo

## Provenance
PI sheet plus two real floppy images (downloaded via NDwiki, not the `floppies.json` reference
library) decoded in this session. Installer program internals (`INSTALL-ADA-A00:PROG`) remain
unread (compiled binary).

---
**Parent:** [../README.md](../README.md) (Software catalog)
