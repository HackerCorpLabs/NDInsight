# ND-210913 — SINTRAN III Monitor Call Package

> Status: VERIFIED install procedure — real floppy confirmed AND the primary PD-sheet is now
> in hand, OCR'd to [210913A_SINTRAN_III_MONITOR_CALL_Package.md](210913A_SINTRAN_III_MONITOR_CALL_Package.md).
> The NDWiki transcript below is confirmed against it.

| Field | Value |
|-------|-------|
| ND article number | `ND-210913`, version A (`210913A`) |
| Product name | SINTRAN III Monitor Call Package |
| Functional category | Language Tools — Linkers/Loaders/Debuggers/Assemblers/Monitors |
| CPU target | ND-10, ND-100, or ND-500 |
| OS requirement | SINTRAN III |
| Release date | 1986-08-01 |
| Manual | `ND-60.228.1 EN` SINTRAN III Monitor Calls manual |
| Related products | The real, separately-installed source of `mon-call-lib` — one of the three libraries (`Vtm, mon-call-lib, planc-lib`) that PLANC-SCREEN-H's demo program requires, see [PLANC-UI-VTM-GUIDE.md §4](../../../Developer/Languages/Application/PLANC-UI-VTM-GUIDE.md#4-building-and-linking-a-screen-program) |

## Description

Per NDWiki (ND article page, itself marked a stub): "ND100 and ND500 monitor call interface for
FORTRAN, COBOL and PLANC."

## What is known — real floppy, confirmed

Floppy `210913A00-XX-01D` mounts cleanly:

```
MON-CALL-1B-A00:BRF     mon-call-lib, ND-100 1-bank runtime      3 pages, 6120 bytes
MON-CALL-2B-A00:BRF     mon-call-lib, ND-100 2-bank runtime      4 pages, 6159 bytes
MON-CALL-LIB-A00:NRF    mon-call-lib, ND-500 (Linkage-Loader)    4 pages, 7923 bytes
MON-CALL-NAMES-A:DATA   symbolic monitor-call-name data table    4 pages, 7379 bytes
```

**Correction to an earlier note in this file**: the archive holds two floppy-image entries for
this product (`nd-210913-a00-d1-23804c5c`, `nd-210913-a00-d1-dff64661`) with different checksums.
A dedup pass in the archive's own reports
(`reports/nd110-emulator-images.md`) shows these are **not two independent reads of two physical
disks** — they are the same physical disk, one image simply truncated at 616 pages of a 640-page
read, byte-identical over the full overlap. There is really only one surviving copy of this
floppy in the archive.

## Installation procedure — VERIFIED, sourced from NDWiki

**Source note**: this was transcribed from the NDWiki article page for `ND-210913A`, which
itself cites a `Program Description ND-210913A.pdf` as the primary source. That PD sheet has now
been obtained and OCR'd (see
[210913A_SINTRAN_III_MONITOR_CALL_Package.md](210913A_SINTRAN_III_MONITOR_CALL_Package.md),
pages 3-6), and the procedure below matches it — including the `MON-CALL-NAMES-A:DATA`
no-revision-suffix asymmetry noted underneath. Cross-check done; this is no longer
secondary-source-only.

The PD sheet also carries a detail the wiki page does not: the per-file ND article numbers, and
the minimum mass-storage requirement (any user: 11 pages over 3 files; `SYSTEM`: 4 pages over
1 file).

| ND file no. | File name | Type | Contains |
|---|---|---|---|
| `208287A` | `MON-CALL-1B-A<rev>` | BRF | Monitor Call ND-100, 1 bank |
| `208288A` | `MON-CALL-2B-A<rev>` | BRF | Monitor Call ND-100, 2 bank |
| `208289A` | `MON-CALL-LIB-A<rev>` | NRF | Monitor Call ND-500 |
| `208290A` | `MON-CALL-NAMES-A` | DATA | Monitor call names |

The file `MON-CALL-NAMES-A<rev>:DATA` must be copied to user `SYSTEM`. The other three files may
also go to `SYSTEM` but don't have to — any user works, as long as the files have public read
access. Verbatim procedure (`<rev>` = the revision letter/number, `A00` on the floppy actually
decoded here; `<drive no.>` / `<floppy unit>` are site-specific):

```
@ENTER-DIRECTORY 210913A-XX-01,FLOPPY-DISC-<drive no.>,<floppy unit>
@CREATE-FILE MON-CALL-1B-A<rev>:BRF 0
@CREATE-FILE MON-CALL-2B-A<rev>:BRF 0
@CREATE-FILE MON-CALL-LIB-A<rev>:NRF 0
@CREATE-FILE MON-CALL-NAMES-A:DATA 0
@SET-FILE-ACCESS MON-CALL-1B-A<rev>:BRF,R,R,RWACD
@SET-FILE-ACCESS MON-CALL-2B-A<rev>:BRF,R,R,RWACD
@SET-FILE-ACCESS MON-CALL-LIB-A<rev>:NRF,R,R,RWACD
@SET-FILE-ACCESS MON-CALL-NAMES-A:DATA,R,R,RWACD
@COPY-FILE MON-CALL-1B-A<rev>:BRF (210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-1B-A<rev>:BRF
@COPY-FILE MON-CALL-2B-A<rev>:BRF (210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-2B-A<rev>:BRF
@COPY-FILE MON-CALL-LIB-A<rev>:NRF (210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-LIB-A<rev>:NRF
@COPY-FILE MON-CALL-NAMES-A:DATA (210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-NAMES-A:DATA
@RELEASE-DIRECTORY 210913A-XX-01
```

Note the asymmetry the source itself shows: three files are created under their `<rev>`-suffixed
name (`MON-CALL-1B-A<rev>:BRF` etc.) but `MON-CALL-NAMES-A:DATA` is created and copied **without**
a `<rev>` suffix on the destination side, even though the source-side copy path still names
`MON-CALL-NAMES-A:DATA` unchanged — i.e. this one file's name is not revision-tagged at all,
matching what the real floppy directory listing shows (`MON-CALL-NAMES-A:DATA`, no `-A00` in the
name, unlike the other three). This is a verbatim read of the source procedure, not a
transcription error.

Once copied, `mon-call-lib` is used exactly like any other link-time library: `MON-CALL-1B-A<rev>:BRF`
or `MON-CALL-2B-A<rev>:BRF` for an ND-100 program (pick per bank model, see
[TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md)), or
`MON-CALL-LIB-A<rev>:NRF` for ND-500 via `LOAD-SEGMENT`.

## Documentation
- Program Description (PD-sheet): **obtained and OCR'd** —
  [210913A_SINTRAN_III_MONITOR_CALL_Package.md](210913A_SINTRAN_III_MONITOR_CALL_Package.md).
  8 pages: two diskette label sheets, the PD sheet itself (twice, two scans of the same 2-page
  form), and the Source Description.
- Manual: `ND-60.228.1 EN` SINTRAN III Monitor Calls manual. (The PD sheet writes the number as
  `60.228.1 EN`; the Source Description page writes `60.288.1 EN` — one of the two is a typo or
  an OCR slip, not resolved here.)

## Source Description — ND-250104A (from the same PD-sheet scan, pages 7-8)

The PD sheet names `250104A` as the source product. Its Source Description sheet is included in
the scan and gives what the binaries were built from and how:

- Sources on the same floppy: `MON-CALL-100-A00:SYMB` (ND-100), `MON-CALL-500-A00:SYMB` (ND-500),
  `MON-CALL-NAMES-A:DATA`.
- Built on an ND-500 under SINTRAN III VSX version K.
- Build tools, by ND number: `210309F` PLANC for ND-100, `210310F` PLANC for ND-500,
  `210721B` BRF-Linker for ND-100, `210319F` Linkage Loader for ND-500.
- The compile recipe compiles the ND-100 source **four** times — 1-bank and 2-bank, each once
  with `ree_fort=FALSE` and once with `ree_fort=TRUE` — and appends the `ree_fort=TRUE` result
  onto the main BRF with `BRF-LINKER APPEND-BRF`. So the FORTRAN re-entrant entry points are a
  separate compilation grafted onto the same library file.

The OCR of those two pages is rough (`Θ`/`θ`/`ø` for the `@` prompt, `$mon_f` vs `5mon_f`,
`MON-CALL-TB` for `MON-CALL-1B`). Read the exact command text off the scan, not off the
markdown, before trying to reproduce the build.

## Provenance & open items
- Floppy contents: real image in the archive, confirmed by catalog listing and by a real
  `ndfs -t` directory dump (both match, verbatim: 3/4/4/4 pages, matching byte counts, all dated
  1986-07-17, user `FLOPPY-USER`).
- Install procedure: transcribed verbatim from the NDWiki article for `ND-210913A`, and now
  **cross-checked against the PD-sheet scan itself** (2026-08-25) — they agree.
- Page counts disagree between sources and are NOT resolved: the real `ndfs -t` dump gives
  3/4/4/4 pages, while the OCR'd diskette label sheets give 4/5/5/5 (page 1 of the scan even
  shows a `$` where a page count should be — plainly an OCR failure). The `ndfs` dump is the
  measurement; the label OCR is not. Do not "correct" the dump to match the label.
- The scan's two label sheets name two directories, `210913A00-XX-01S` and `210913A00-XX-01D`,
  with 148 and 610 reserved pages. Not investigated — the archive's real image is the `...01D`
  one.
- Directory size on the real floppy: 611 pages, bit-file 1 page, matching the wiki's own
  `ndfs -t` transcript exactly.

---
**Parent:** [../README.md](../README.md) (Software catalog)
