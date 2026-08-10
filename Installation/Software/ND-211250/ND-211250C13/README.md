# ND-211250C13 — UNIQUE DOCUMENTATION for ND-500, version C13

> Status: IN-PROGRESS — real decoded install scripts, dependencies not independently verified   ·   Install source: [MODE]

| Field | Value |
|-------|-------|
| Part number | `211250C13` |
| Base product | [`ND-211250`](../README.md) |
| Version | C, revision 13 |
| Release date | unknown (not stated in the floppy directory listing read) |
| CPU target | ND-500, against SIBAS-500 |
| OS requirement | unknown |

## Description
An ND-500 domain built from 8 NRF modules (`DOC-DATA`, `DOC-MAIN`, `DOC-SYS`, `DOC-XT-DATA`,
`DOC-SIBAS`, `DOC-ONLINE`, `DOC-XTRA`, `DOC-LIB`) plus a small dummy module, linked against the
SIBAS-500 runtime library, the User Environment library (`UE-PLIB`), a Norwegian NOTIS-WP module,
and a `UQ-KEY` product-key file — the same license-key mechanism seen on the `UNIQUE` floppy
found while researching this product family (a bare `UQ-KEY-19204:NRF` file, 113 bytes).

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `211250C13-XX-01D` | `DOC-MAIN-C13:NRF` (4 pages) · `DOC-DATA-C13:NRF` (2 pages) · `DOC-SIBAS-C13:NRF` (54 pages) · `DOC-DUMMY-C13:NRF` (2 pages) · `DOC-SYS-C13:NRF` (3 pages) · `DOC-LIB-C13:NRF` (103 pages) · `DOC-ONLINE-B:NRF` (17 pages) · `DOC-ONLINE-C13:NRF` (22 pages) · `DOC-XTRA-B:NRF` (16 pages) · `DOC-XT-DATA-C13:NRF` (2 pages) · `DOC-WP-N-C13:NRF` (1 page) · `DOC-IN-S5-C13:PROG` (77 pages) · `INSTALL-UNIQ:PROG` (56 pages) · `DOC-LOAD-S5-C13:MODE`, `DOC-DUMP-S5-C13:MODE`, `DOC-LINKER-C13:MODE` (all **decoded below**), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `9a4808530a4c66021778517f6787f18b`) and reading/extracting
with `ndtool`.

## Installation procedure — real, decoded scripts

Three `:MODE` files were extracted and decoded (`byte & 0x7F`, clean output — same technique
verified on the CC-100/COBOL/ND-Linker scripts elsewhere in this catalog). [MODE]

### `DOC-LOAD-S5-C13:MODE` — build the domain via the older ND-500 Linkage-Loader
```
@ND-500-MONITOR
LINKAGE-LOADER
ABORT-BATCH-ON-ERROR OFF
RELEASE-DOMAIN       UNIQUE-DOC-S5-C
DELETE-DOMAIN        UNIQUE-DOC-S5-C
ABORT-BATCH-ON-ERROR ON
SET-DOMAIN          "UNIQUE-DOC-S5-C"
SET-SEGMENT-NUMBER   2
OPEN-SEGMENT        "UNIQUE-DOC-S5-C",,
PROGRAM-REF          UQRLSTBYTE,,P
LOAD                (DIALOG-SYS)DOC-DATA-C13
LOAD                (DIALOG-SYS)DOC-MAIN-C13
LOAD                (DIALOG-SYS)DOC-SYS-C13
LOAD                (DIALOG-SYS)DOC-XT-DATA-C13
LOAD                (DIALOG-SYS)DOC-SIBAS-C13
LOAD                (DIALOG-SYS)DOC-ONLINE-C
LOAD                (DIALOG-SYS)DOC-XTRA-B
LOAD                (DIALOG-SYS)DOC-LIB-C13
LOAD-SEGMENT  (USER-ENVIRONMENT)UE-PLIB-P5-C03:NRF
LOAD-SEGMENT  (USER-ENVIRONMENT)UE-PLIB-D5-C03:NRF
FORCE-SEGMENT-LINK    (SIB2-500)SIBR-LIBRARY-A05
LINK-SEGMENT          (SIB2-500)SIBR-MESS-TS-A05
LINK-SEGMENT          (SIB2-500)SIBR-MESS-TS-A05
LOAD                (DIALOG-SYS)UQ-KEY
LINK-SEGMENT       (DOMAIN-USER)NOTIS-WP-NO-M
LOAD                (DIALOG-SYS)DOC-DUMMY-C13
KILL BCCVTR8,BCCVFR8
EXIT

CC   PATCH: system name and default language
LOOK-AT-DATA 2'7620 ()UNIQUE-DOC-S5-C
PERMIT
'                    '
2'7620/
'UNIQUE-DOC-S5-C'
2'7504/
'NO'
EXIT
EXIT
@SET-FILE-ACCESS UNIQUE-DOC-S5-C:LINK R,R,,
@SET-FILE-ACCESS UNIQUE-DOC-S5-C:PSEG R,R,,
@SET-FILE-ACCESS UNIQUE-DOC-S5-C:DSEG R,R,,
```

### `DOC-LINKER-C13:MODE` — the alternative, newer-format build via ND LINKER (`:DOM`)
```
@DEL-FI UNIQUE-DOC-S5-C:DOM
@LINKER
ABORT-BATCH-ON-ERROR YES
OPEN-DOMAIN          "UNIQUE-DOC-S5-C"
SET-SEG-NUMBER  4
IGNORE-DEBUG-INFO YES
REF-ENTRY                UQRLSTBYTE,,P,P
LOAD          (DIALOG-SYS)DOC-DATA-C
LOAD          (DIALOG-SYS)DOC-MAIN-C
LOAD          (DIALOG-SYS)DOC-SYS-C
LOAD          (DIALOG-SYS)DOC-XT-DATA-C
LOAD          (DIALOG-SYS)DOC-SIBAS-C
LOAD          (DIALOG-SYS)DOC-ONLINE-C
LOAD          (DIALOG-SYS)DOC-XTRA-B
LOAD          (DIALOG-SYS)DOC-LIB-C
LINK             (SIB2-500)SIBR-LIBRARY-A
SPECIAL-LINK  (DOMAIN-USER)NOTIS-WP-NO-M    LIBRARY
LOAD           (DIALOG-SYS)UQ-KEY
LOAD     (USER-ENVIRONMENT)UE-PLIB-P5-C
LOAD     (USER-ENVIRONMENT)UE-PLIB-D5-C
LOAD          (DIALOG-SYS)DOC-DUMMY-C
DELETE-ENTR BCCVTR8,BCCVFR8,/,ALL,PD
EXIT

@ND-500-MONITOR
CC   PATCH: system name and default language
LOOK-AT-DATA 4'10262B ()UNIQUE-DOC-S5-C
PERMIT
4'7600/
'                    '
4'7600/
'UNIQUE-DOC-S5-C'
4'7504/
'NO'
EXIT
EXIT
@SET-FILE-ACCESS UNIQUE-DOC-S5-C:DOM R,R,,,
```

### `DOC-DUMP-S5-C13:MODE` — register as a standard domain (cold-start persistence)
```
@ND-500
DELETE-STANDARD-DOMAIN UNIQUE-DOC-S5-C
DEFINE-STANDARD-DOMAIN UNIQUE-DOC-S5-C,(DIALOG-SYS)UNIQUE-DOC-S5-C
EXIT
```

### Reading the scripts

1. **Prerequisites (real, from the scripts themselves):** all 8 NRF modules plus `UQ-KEY` and
   `DOC-DUMMY-C13` must already be copied to `DIALOG-SYS`; the SIBAS-500 runtime
   (`(SIB2-500)SIBR-LIBRARY-A05`/`SIBR-MESS-TS-A05`) must already be installed and accessible to
   user `SIB2-500`; the User Environment library
   (`(USER-ENVIRONMENT)UE-PLIB-P5-C03`/`UE-PLIB-D5-C03`) must already exist; a Norwegian
   NOTIS-WP module (`(DOMAIN-USER)NOTIS-WP-NO-M`) must be installed (this floppy is the Norwegian
   `-NO-` release, per the `PATCH: default language` step setting `'NO'`).
2. **Two independent build paths are shipped** — the older Linkage-Loader path
   (`DOC-LOAD-S5-C13:MODE`, produces the old `:LINK`/`:PSEG`/`:DSEG` + `DESCRIPTION-FILE` shape)
   and the newer ND LINKER path (`DOC-LINKER-C13:MODE`, produces a single `:DOM` file) — pick
   one, not both. This is the first real, decoded confirmation in this catalog of the "both
   domain formats can be built from the same source modules" claim made on
   [ND-211224's PI sheet](../../ND-211224/README.md).
3. **A "PATCH: system name and default language" step follows the build in both paths** — it
   patches two in-memory string fields (system name, blanked and reset to `'UNIQUE-DOC-S5-C'`;
   default language, set to `'NO'`) directly via `LOOK-AT-DATA`/octal offsets, not through any
   documented configuration command.
4. **Persist across cold start:** run `DOC-DUMP-S5-C13:MODE` to register `UNIQUE-DOC-S5-C` as a
   standard domain — add this to the site's `ND500-HENT` cold-start chain (not independently
   confirmed for this product, follow the generic pattern already documented for the ND-500
   Linkage-Loader).
5. Both scripts finish by tightening file access on the built domain to read-only
   (`@SET-FILE-ACCESS ... R,R,,`).

## Configuration / post-install
See step 3-4 above — the system-name/language patch and the standard-domain registration are the
only configuration steps found.

## Documentation
- PD-sheet: not located
- PI-sheet: not located under this article number — see
  [`ND-211202-A1-EN.md`](../../../Product-Info/ND-211202-A1-EN.md)

## Provenance & open items
- Source: `ndtool -t`/`-x` on the downloaded image; all three `:MODE` scripts decoded with
  `byte & 0x7F`, clean grammatical output confirming the technique worked correctly.
- **TODO:** locate a PD/PI sheet giving this product's own article-number confirmation
  independent of the domain-name inference (very likely correct, but not from an official
  product-description document).
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-211250` product overview)
