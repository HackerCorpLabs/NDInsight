# ND-210913 — SINTRAN III Monitor Call Package

> Status: VERIFIED install procedure (sourced from NDWiki) — real floppy confirmed; the
> primary PI-sheet PDF is not yet in hand (access pending)

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

**Source note**: this is transcribed from the NDWiki article page for `ND-210913A`, which itself
cites a `Program Description ND-210913A.pdf` as the primary source. That PDF has not yet been
obtained for this repo (access pending) — treat this section as a verified secondary-source
transcript, not yet cross-checked against the primary PD sheet.

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
- Program Description (PD-sheet): `Program Description ND-210913A.pdf` — cited by NDWiki, **not
  yet obtained for this repo**.
- Manual: `ND-60.228.1 EN` SINTRAN III Monitor Calls manual.

## Provenance & open items
- Floppy contents: real image in the archive, confirmed by catalog listing and by a real
  `ndfs -t` directory dump (both match, verbatim: 3/4/4/4 pages, matching byte counts, all dated
  1986-07-17, user `FLOPPY-USER`).
- Install procedure: transcribed verbatim from the NDWiki article for `ND-210913A`, itself
  sourced from the PD-sheet PDF — **not yet cross-checked against that PDF directly**. Update this
  file once the PDF is in hand.
- Directory size on the real floppy: 611 pages, bit-file 1 page, matching the wiki's own
  `ndfs -t` transcript exactly.

---
**Parent:** [../README.md](../README.md) (Software catalog)
