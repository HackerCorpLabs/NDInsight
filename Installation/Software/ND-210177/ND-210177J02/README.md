# ND-210177J02 — COBOL-85 for ND-500/5000, version J02

> Status: VERIFIED (real install `:MODE` files decoded from the floppy; not yet run live)   ·   Install source: [MODE]

| Field | Value |
|-------|-------|
| Part number | `210177J02` |
| Base product | [`ND-210177`](../README.md) |
| Version | J02 |
| Release date | files dated 1987-04 through 1987-12-21 |
| CPU target | ND-500 / ND-5000 |
| OS requirement | unknown |

## Description
Bundles the COBOL-85 compiler domain, its runtime library, and the "IS-MULTI" ISAM multiuser
supervisor process onto one floppy — all with real, decoded `Linkage-Loader` mode files.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210177J02-XX-01D` | `DESCRIPTION-FILE:DESC` (8 pages) · `COBOL-500-J02:LINK`/`:DSEG`/`:PSEG` (8/49/92 pages) — the compiler domain · `COBOL-LIB-J02:NRF` (139 pages, runtime library) · `COBOL-LIB-J02:MODE` (**decoded below**) · `IS-MULTI-K00:NRF` (1 page) + `IS-MULTI-K00:MODE` (**decoded below**) · `ISAM-SERVICE-K01:PROG` (81 pages), `ISAM-INTER-K01:PROG` (58 pages) — ISAM add-on programs (product `ND-10343`) · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `7ac060f5f683d8504ef735abd8c56479`) and reading with
`ndtool -t`/`-x`.

## Installation procedure

Two real `:MODE` files were extracted and decoded (same technique as the CC-100 install scripts:
SINTRAN packs these command files with the high bit set on every byte; `byte & 0x7F` recovers
plain text — confirmed clean, no artifacts). Both are Linkage-Loader batch scripts. [MODE]

### `COBOL-LIB-J02:MODE` — decoded verbatim
```
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ COBOL-LIB-J02:MODE                                              @@
@@ Example mode file to define the COBOL library as a LINK library @@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

@Linkage-Loader
Abort-Batch-On-Error  Off
Release-Domain        COBOL-LIB-J02
Delete-Domain         COBOL-LIB-J02
Abort-Batch-On-Error  On
Set-Domain            "COBOL-LIB-J02"
Common-Segment-Number 20d                   cc
Common-Segment-Open   "MULTI-IS-K00",wcd    cc
Define-Common         IsPool,40000,4        cc This sequence may
Define-Common         IsParam,10,40004      cc be omitted if the
Define-Common         IsHeadFil,4,40014     cc multiuser facility
Define-Common         IsHeadRec,4,40020     cc is not to be used.
Define-Common         IsRecArr,2000,40024   cc
Common-Segment-Close                        cc
Set-Segment-Number    21d  cc Or some other segment > 20d
Open-Segment          "COBOL-LIB-J02",wcp
Total-Segment-Load    COBOL-LIB-J02
Total-Segment-Load    EXCEPT-LIB
Close-Segment         21d
End-Domain
Exit
```
(`cc` is the Linkage-Loader's trailing-comment marker, matching the `%`/`'` comment conventions
already documented for XMSG `.INCL` files elsewhere in this repo — a different tool, same idea.)

### `IS-MULTI-K00:MODE` — decoded verbatim
```
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ IS-MULTI-K:MODE                                  @@
@@ Mode file to create a shared link data segment,  @@
@@ to supervice an IS multiuser process.            @@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

@Linkage-Loader
Abort-Batch-On-Error Off
Release-Domain       IS-MULTI-K00
Clear-Domain         IS-MULTI-K00
Delete-Domain        IS-MULTI-K00
Abort-Batch-On-Error On
Set-Domain          "IS-MULTI-K00"
Set-Segment-Number   20d
Open-Segment        "IS-MULTI-K00",wcd
Total-Segment-load   IS-MULTI-K00
Close-Segment        20d
End-Domain
Exit
```

### Reading of the scripts

1. **`IS-MULTI-K00:MODE` runs first** — it creates a shared common data segment (segment 20,
   named `IS-MULTI-K00`) that supervises the ISAM multiuser process. This must exist before
   `COBOL-LIB-J02:MODE` runs, because that script's `Common-Segment-Open "MULTI-IS-K00",wcd`
   attaches to it by name.
2. **`COBOL-LIB-J02:MODE` runs second** — it (re)creates the `COBOL-LIB-J02` domain, opens the
   shared common segment from step 1 and defines five named commons inside it (`IsPool`,
   `IsParam`, `IsHeadFil`, `IsHeadRec`, `IsRecArr` — explicitly optional "if the multiuser
   facility is not to be used"), then opens its own segment 21 and does a
   `Total-Segment-Load` of both `COBOL-LIB-J02` (the library itself) and `EXCEPT-LIB` (a
   COBOL exception-handling library, present by name only — not a file on this floppy, so it must
   already exist elsewhere on the target system).
3. Both scripts are **self-cleaning**: each starts with `Release-Domain`/`Clear-Domain`/
   `Delete-Domain` on its own target (with `Abort-Batch-On-Error Off` around that cleanup, since
   the domain won't exist on a first-time install — the same "expected NO SUCH FILE/DOMAIN on
   first run" pattern already documented for the Backup-System and NLL installers), then
   `Abort-Batch-On-Error On` before the real work.
4. Run them from the ND-500 monitor, via the Linkage-Loader's own mode-file runner, in the order
   above:
   ```
   @ENTER-DIRECTORY 210177J02-XX-01D,FLOPPY-DISC-1,0,
   @ND-500-MONITOR
   N500: LINKAGE-LOADER
   NLL: MODE (210177J02-XX-01D:FLOPPY-USER)IS-MULTI-K00:MODE
   NLL: MODE (210177J02-XX-01D:FLOPPY-USER)COBOL-LIB-J02:MODE
   ```
   (both scripts already `@Linkage-Loader`/`End-Domain`/`Exit` internally — the exact top-level
   invocation syntax for feeding a `:MODE` file to the Linkage-Loader is carried over from the
   generic `NLL: MODE` pattern documented for the Linkage-Loader itself; not independently
   re-verified here.)
5. The **compiler domain** (`COBOL-500-J02:LINK`/`:DSEG`/`:PSEG`) and the **ISAM programs**
   (`ISAM-SERVICE-K01:PROG`, `ISAM-INTER-K01:PROG`) are plain files/domain segments with no
   accompanying `:MODE` script — copy them following the same recovery-path pattern already
   verified for the ND-500 Linkage-Loader itself
   ([../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md §4a-VERIFIED](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)).

## Configuration / post-install
Presumably a `DEFINE-STANDARD-DOMAIN`-style step for cold-start persistence, by analogy with the
NLL installer — not confirmed for this product.

## Documentation
- PD-sheet: not located
- PI-sheet: not located for this article number
- Manual(s): `ND-60.144.3` COBOL Reference Manual

## Provenance & open items
- Source: `ndtool -t`/`-x` on the downloaded image; `:MODE` files decoded with the same `byte &
  0x7F` technique verified clean on the CC-100 install scripts.
- **TODO:** confirm the exact `NLL: MODE <file>` invocation syntax against the Linkage-Loader's
  own manual (`ND-60.136.04A`, already in this repo) rather than carrying it over from memory of
  the generic pattern.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210177` product overview)
