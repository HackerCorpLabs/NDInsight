# ND-10340 — SIBAS-II Data Base System for ND-500

> Status: IN-PROGRESS — real floppy set (6 parts) decoded, full file inventory recovered; no install script found

| Field | Value |
|-------|-------|
| ND article number | `ND-10340` (also seen as `ND-210340` on later floppy labels) |
| Product name | SIBAS-II Data Base System for ND-500 |
| Functional category | Databases & File Access |
| CPU target | ND-500 |
| OS requirement | SINTRAN III/VS |
| Related products | `ND-10166`/`ND-210166` SIBAS-II for ND-100 (already documented, install VERIFIED — see [../ND-210166/README.md](../ND-210166/README.md)); same on-disk database format as the ND-100 product, so applications/databases move between the two CPUs. `ND-211049` SQL for ND-500 (see [../ND-211049/README.md](../ND-211049/README.md)) requires `SIB2-DRL` on `SYSTEM` before installing — **`SIB2-DRL-D:PROG` is confirmed present on this floppy set** (part 4). Programming concepts (realms, CALC/SERIAL, DDL/DML calls) are covered generically in [SIBAS-DATABASE-PROGRAMMING.md](../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md). |

## What is known — real floppy set, decoded

Six floppies, `ND-10340D-PART1` through `PART6` (downloaded via NDwiki, imaged by Torfinn "Tingo"
Ingolfsen). All six mount cleanly. Real file inventory:

| Part | Files |
|---|---|
| PART1 | `DESCRIPTION-FILE:DESC` (binary domain-metadata, not readable text — same format as other Linkage-Loader-domain floppies in this catalog) · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholder) · `SIBAS-MAIN-C2:LINK`/`:DSEG`, `SIBAS-MAIN-FIX:LINK`/`:DSEG`, `SIBAS-MAIN-C1:LINK`/`:DSEG` (three Linkage-Loader domains) · `SIBAS-MAIN-SEG:LINK`/`:DSEG`/`:PSEG` (a fourth, complete domain) |
| PART2 | `SIBAS-MESS-ARE:NRF`, `SIBAS-MESS-USE:NRF` (message-area/message-use modules) · `SIBAS-LIBRARY:NRF` (the main call library) · `SERVER-MAIN:NRF`, `SERVER-UTILITY:NRF`, `SERVER-DEVCOM:NRF` (the SIBAS Service Program's modules) · `SI5-DML-SIB2:NRF`, `SI5-DML-BLOC:NRF` (Data Manipulation Language modules) · `ND500-SIB2A:BRF` through `ND500-SIB2E:BRF` (5 runtime BRF files) · `ND500-SERV2A:BRF`/`B:BRF`/`C:BRF` (3 more runtime BRF files) · `DUMMY-SEXMC:SYMB` |
| PART3 | `SIBAS-SERV-MH-D:BPUN` (the SIBAS Service Program, compiled) · `SIBINTER-MH:BPUN` (SIBINTER — ad-hoc database access, compiled) · `SIB2-SCHEMAS-D:BPUN`, `SIB2-LOOKLOG-D:BPUN` (further compiled utilities) |
| PART4 | `SIB2-DRL-D:PROG` — **the SIBAS Data Retrieval Language interactive program.** This is the exact prerequisite `ND-211049` (SQL for ND-500) names as needing to already be on `SYSTEM` before installing SQL — confirming this floppy set is a genuine source for that dependency. |
| PART5 | `FORTRAN-LIB-E:NRF`, `PLANC-LIB-B:NRF` (language interface libraries) · `PLANC-1BANK-C:BRF` (1-bank PLANC runtime — see [TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md)) |
| PART6 | `SIB2-DBM-D:PROG` — the SIBAS Data Base Maintenance program (passwords, print, load/unload, regenerate/verify — matches the PI sheet's "Maintenance Module" description) · `SIB2-DML-B-MH-D:BRF`, `SIB2-DML-R-MH-D:BRF` (Data Manipulation Language runtime, two variants) · `SIBLIB-1N-MH-D:BRF`, `SIBLIB-1R-MH-D:BRF`, `SIBLIB-2N-MH-D:BRF` (call-library runtime, N/R variants across bank splits) |

**No `:MODE`/`:BATC` install script and no installer `:PROG` were found** — this looks like the raw
product file set (Linkage-Loader domains + NRF/BRF runtime modules + two standalone interactive
`:PROG`s), meant to be installed the same manual way already documented for
[ND-210177 COBOL-85 for ND-500](../ND-210177/README.md) and [ND-211037 PLANC](../ND-211037/README.md):
`COPY-DOMAIN` for the `SIBAS-MAIN-*` domains, `COPY-FILE` for the runtime BRF/NRF files, and
`DEFINE-STANDARD-DOMAIN` to register the result — inferred by strong analogy to those already-
verified installs, **not evidenced on this floppy set itself**.

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10340-A1-EN.md](../../Product-Info/ND-10340-A1-EN.md)
- Manual(s): `ND-60.127` SIBAS-II User's Manual (see [../../../Reference-Manuals/](../../../Reference-Manuals/README.md) for the ND-100-side copy, not confirmed identical for ND-500)

## Provenance & open items
- Source: six real floppy images, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x`).
- **TODO:** the exact install command sequence is inferred by analogy to other ND-500 products in
  this catalog, not evidenced on the floppy itself — no `:MODE`/`:BATC` script exists to confirm it.
- A seventh floppy, `ND-210340F` (a later-labelled revision), was reported in the backlog but
  failed to image ("errors") and is not covered here.

---
**Parent:** [../README.md](../README.md) (Software catalog)
