# ND-210336 — ND-100 Symbolic Debugger

> Status: IN-PROGRESS — install command and a real worked debugging session both sourced from real manuals; not yet run live in the emulator

| Field | Value |
|-------|-------|
| ND article number | `ND-210336` (also referenced as `ND-10336` — the same base/`21`-prefixed-revision pattern seen throughout this catalog; the actual floppy in hand is labeled `210336F01`) |
| Product name | Symbolic Debugger for ND-100 |
| Functional category | Language Tools — Linkers / Loaders / Debuggers / Assemblers / Monitors |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 |
| OS requirement | unknown — no PD/PI sheet located |
| Related products | `ND-10335`/`ND-210166`... — no: `ND-10335` **ND-500** Symbolic Debugger (companion product, same PI sheet family, see [../ND-10335/README.md](../ND-10335/README.md)) — same debugger for FORTRAN/COBOL/PLANC(/Pascal) programs, described together on [`ND-10335-C1-EN.md`](../../Product-Info/ND-10335-C1-EN.md) |

## Description
**No PD or PI sheet naming this product specifically has been located** — everything here is
either from the mounted floppy or from the real System Supervisor manual's own worked example of
installing it. The companion ND-500 product's PI sheet
([`ND-10335-C1-EN.md`](../../Product-Info/ND-10335-C1-EN.md)) describes the *shared* feature set
both debuggers offer (breakpoints by line/routine/address/conditional, call-hierarchy inspection,
`DISPLAY`/`SET` for variables, `LOOK-AT` commands with a built-in assembler/disassembler,
symbolic references for modules compiled in `DEBUG-MODE`) — but does not describe the ND-100
version's own command syntax, which may differ from the ND-500 version documented in
[ND-10335B](../ND-10335/ND-10335B/README.md).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| F (rev 01) | [ND-210336F01](ND-210336F01/README.md) | IN-PROGRESS — install command real/manual-sourced, usage not confirmed | floppy `210336F01-XX-01D` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): not located under this article number — see the ND-500
  companion's sheet, [`ND-10335-C1-EN.md`](../../Product-Info/ND-10335-C1-EN.md), for the shared
  feature description
- Manual(s): `ND-60.158` Debugger User's Manual — [../../../Reference-Manuals/ND-60158-5-EN Symbolic Debugger - User Guide.md](../../../Reference-Manuals/ND-60158-5-EN%20Symbolic%20Debugger%20-%20User%20Guide.md)
  (this manual likely covers both the ND-100 and ND-500 debuggers — not yet cross-checked for
  ND-100-specific command differences)

## Provenance
Floppy contents confirmed by downloading the image (MD5 `6e073738400aa362f06fcbf827814808`, from
a floppy your friend imaged — logged in
[research/FLOPPY-BACKLOG-2026-08.md](../research/FLOPPY-BACKLOG-2026-08.md)) and reading with
`ndtool`. Install command found verbatim in the real *ND-30.003.7 EN SINTRAN III System
Supervisor* manual's own worked example of setting up standard reentrant subsystems.

---
**Parent:** [../README.md](../README.md) (Software catalog)
