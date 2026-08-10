# ND-211226B02 — Backup Manager for ND-500/5000, version B (rev 02)

> Status: IN-PROGRESS — real decoded MODE scripts, installer identified by analogy to the verified NLL installer, not yet run live   ·   Install source: [OBS] + [MODE] + [INF, installer flow by analogy]

| Field | Value |
|-------|-------|
| Part number | `211226B02` |
| Base product | [`ND-211226`](../README.md) |
| Version | B, revision 02 |
| Release date | files dated 1988-06 through 1989-07-10 |
| CPU target | ND-500 / ND-5000 |
| OS requirement | unknown |

## Description
Four floppies: the installer + core files/MODE scripts (disk 1), and three large ND-500 `:DOM`
domains — `BM-FILERE-B02:DOM` (disk 2, 508 pages), `BM-OPERATOR-B02:DOM` (disk 3, 604 pages),
`BM-DEFINI-B02:DOM` + `BM-SCHED-B02:DOM` + a second `BM-OPERATOR` domain part (disk 4).

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `211226B02-XX-01D` | `IN-BCKMAN-XX-B02:PROG`/`:INIT`/`:XCOM` (installer, 80 pages) · `BM-JOBDB-B02:DATA` (32 pages, job database) · `BM-PARAMETER-B02:DATA` (2 pages) · `BM-SIBDUM-B02:NRF` (29 pages, SIBAS dummy/stub) · `BM-OSIBAS-B02:NRF` (86 pages, real SIBAS interface) · `BM-DRSIB-B02:MODE`, `BM-DNOSIB-B02:MODE`, `BM-SERVER-B02:MODE`, `BM-DUMP-B02:MODE`, `BM-START-B02:MODE`, `BM-DMAPTC:MODE` (all **decoded below**) · `BM-SERVER-B02:PROG` (104 pages, the scheduler server binary) |
| `211226B02-XX-02D` | `BM-FILERE-B02:DOM` (508 pages) |
| `211226B02-XX-03D` | `BM-OPERATOR-B02:DOM` (604 pages) |
| `211226B02-XX-04D` | `BM-DEFINI-B02:DOM` (335 pages) · `BM-SCHED-B02:DOM` (168 pages) · `BM-OPERATOR-B02:DO02` (52 pages, a second segment/part of the operator domain) |

An English-language variant also exists (`211226B02-EN-01D`/`-02D`), not mounted for this entry.

Confirmed by downloading all four `-XX-` images and reading/extracting with `ndtool`.

## Installation procedure

`IN-BCKMAN-XX-B02:PROG`/`:INIT`/`:XCOM` is the same three-file installer shape already seen on
the ND-500 Linkage-Loader and ND LINKER installers in this catalog — by that analogy, expect the
same module-driven flow (get start info → delete old version → check environment → copy product
files → exit). **Not run live, not confirmed for this specific installer.**

```
@(211226B02-XX-01D:FLOPPY-USER)IN-BCKMAN-XX-B02:PROG
```

## Choosing SIBAS integration — real, decoded scripts

Two mutually exclusive `:MODE` scripts append either the real SIBAS interface or a no-op stub to
the `BM-OPERATOR-B02` domain, both decoded (`byte & 0x7F`, clean output): [MODE]

**With SIBAS/R:**
```
@LINKER
APPEND-DOMAIN     BM-OPERATOR-B02
IGNORE-DEBUG-INFO NO
% Please ignore warning: "Address within already loaded program area"
SET-LOAD-ADDRESS  3000000B,P
LOAD              ()BM-OSIBAS-B02:NRF
CLOSE             NO NO

LINKER-SERVICE
CHANGE-LINK-LOCK  BM-OPERATOR-B02 07108
EXIT

APPEND-DOMAIN     BM-OPERATOR-B02
SPECIAL-LINK      (SIBR-B)SIBR-LIBRARY LIBRARY
SPECIAL-LINK      (SIBR-B)SIBR-FAST    SELECT fvFastVeri
LIST-ENTRIES      UNDEF,,ALL,,,
Save-Entries      INLOG,RELSI,RESIB,SABOR,SACT,SCLDB,SDBEC,SEMSG,SETDV
Save-Entries      SFINI,SICON,SISTA,SOPDB,SPASS,SPAUS,SRECO
Save-Entries      SREPR,SRUN,START,STGET,STOPS,SYNCP
Save-Entries      fvFastVeri
CLOSE             NO NO

LINKER-SERVICE
CHANGE-LINK-LOCK  BM-OPERATOR-B02 UNIVERSAL
EXIT
```
(this is `SIBR` — SIBAS/**R**, the relational-flavored SIBAS variant, a different library user
`SIBR-B` than the CODASYL SIBAS-II covered elsewhere in this catalog — not the same product.
`fvFastVeri` and the `Save-Entries` list of SIBAS DML call names — `SOPDB`, `SCLDB`, `SACT`,
`SREPR`, etc. — largely match the real DML vocabulary already documented in
[SIBAS-DATABASE-PROGRAMMING.md](../../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md),
a useful cross-check that SIBR and SIBAS-II share a call convention even if they're different
products.)

**Without SIBAS (the dummy stub, no `LINKER-SERVICE`/lock-changing steps needed):**
```
@LINKER
APPEND-DOMAIN     BM-OPERATOR-B02
IGNORE-DEBUG-INFO YES
% Please ignore warning: "Address within already loaded program area"
SET-LOAD-ADDRESS  3000000B,P
LOAD              ()BM-SIBDUM-B02:NRF
CLOSE             NO NO
```

## Loading the scheduler server — real, decoded `BM-SERVER-B02:MODE`

```
@ABORT          BMSERV
@RT-LOADER
CLEAR-SEGMENT   BMSERV
Y
DELETE-SEGMENT  BMSERV
NEW-SEGMENT     BMSERV,,,,,,,
READ-PROGFILE   (BACKUP-MANAGER)BM-SERVER-B:PROG,BMSERV,,
END-LOAD
DECLARE-PROGRAM BMSERV,,,
CHANGE-RT-DESCR BMSERV,32,BMSERV,,0,2,,,
EXIT
```
Loads `BM-SERVER-B02:PROG` as an RT (real-time) program named `BMSERV` on a fresh segment —
priority 32 (per `CHANGE-RT-DESCR`'s parameters). Assumes a user named **`BACKUP-MANAGER`**
already holds the copied product files.

## Cold-start / warm-start persistence — real, decoded scripts (their own header comments say exactly where to put them)

`BM-DUMP-B02:MODE` — **"should be appended to your standard HENT-MODE file"** (cold start):
```
@ND
Delete-St-Domain BM-OPERATOR
Delete-St-Domain BM-DEFINITION
Delete-St-Domain BM-SCHEDULER
Delete-St-Domain BM-FILERESTORE
Define-St-Domain BM-OPERATOR         (BACKUP-MANAGER)BM-OPERAT-B02
Define-St-Domain BM-DEFINITION       (BACKUP-MANAGER)BM-DEFINI-B02
Define-St-Domain BM-SCHEDULER        (BACKUP-MANAGER)BM-SCHED-B02
Define-St-Domain BM-FILERESTORE      (BACKUP-MANAGER)BM-FILERE-B02
Exit
```
(the standard-domain names are shorter than the actual `:DOM` file names on disk — e.g.
`BM-OPERAT-B02`, not `BM-OPERATOR-B02` as the floppy listing shows; not resolved here, follow the
script's own spelling.)

`BM-START-B02:MODE` — **"File to start BM-SCHEDULER Server program after a warm start... should
be appended to your standard LOAD-MODE file"** (warm start):
```
@RT BMSERV
```

This is a real, self-documenting example of the cold-start-loads/warm-start-runs split already
covered generically in
[SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md) —
`BM-DUMP-B02:MODE` (registers standard domains, one-time work at cold start) goes in `HENT-MODE`;
`BM-START-B02:MODE` (starts the RT server) goes in `LOAD-MODE`, run on every warm start.

## Configuration / post-install
See "Cold-start / warm-start persistence" above.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-211226-A1-EN.md](../../../Product-Info/ND-211226-A1-EN.md)

## Provenance & open items
- Source: `ndtool -t`/`-x` on the four downloaded `-XX-` images; all decoded `:MODE` files clean,
  grammatical output confirming the technique worked correctly.
- **TODO:** run the installer live to confirm/refute the NLL-installer-analogy flow.
- **TODO:** resolve the `BM-OPERAT-B02` vs `BM-OPERATOR-B02` naming mismatch between
  `BM-DUMP-B02:MODE` and the actual floppy file names.
- **TODO:** `BM-DMAPTC:MODE` (a small, dated-separately file on disk 1) was not decoded for this
  entry.

---
**Parent:** [../README.md](../README.md) (`ND-211226` product overview)
