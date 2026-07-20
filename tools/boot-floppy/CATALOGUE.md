# ND floppy catalogue — SINTRAN distribution sets

Produced by sweeping all of `D:\ND` (15,431 files) with `ndtool`: 1,473
size-candidates, **242 readable NDFS images**, 1,231 not NDFS.

Volume labels read `<part-no><VERSION><rev>-<lang>-<NN>D`, e.g.
`250305L07-XX-01D` = SINTRAN **L**, revision **07**, diskette **1**.

Flags: **S** contains `SINTRAN*:DATA` · **M** contains `MACM*` ·
**A** contains an assembler.

---

## Complete SINTRAN distribution sets

### VSX/500 L rev 07 — `250305L07` — 3/3 ✅ *(carver: `L-VSX-500`)*
| | image | flags |
|---|---|---|
| `-01D` | `D:\ND\S\VSXL1.IMG` | **S M** MACM-1718L + SINTRAN-L-1:DATA |
| `-02D` | `D:\ND\S\VSXL2.IMG` | **S** SINTRAN-L-2:DATA |
| `-03D` | `D:\ND\S\VSXL3.IMG` | **A** DMAC-1915G, F32-FMAC-1920C, F48-FMAC-1408D |

### VSX/500 M rev 06 — `250306M06` — 3/3 ✅ *(carver: `M-VSX-500`)*
| | image | flags |
|---|---|---|
| `-01D` | `D:\ND\S\250306M06-XX-01D.image` | **S M** (disk 100 % full) |
| `-02D` | `D:\ND\S\250306M06-XX-02D.image` | **S** SINTRAN-M-2:DATA |
| `-03D` | `D:\ND\S\250306M06-XX-03D.image` | **A** + N5000-SYMBOLS, ND500-MONITOR:BPUN |

Companion: `250360M04-XX-01D` (M patch / ER-S3WD server set).

### K rev 03 — `N-220046K03` — 2/2 ✅ *(carver: `K-VSX-500`)*, 1987-09-08
`D:\ND\S\VSXK1.img` (**S M**) · `D:\ND\S\VSXK2.img` (**A**, DMAC-1915F)

### K rev 05 — `N-250306K05` — 2/2 ✅ **not in the carver**, 1988-06-08
`D:\ND\S\N-250306K05--01D.img` (**S M**, SINTRAN:DATA 730,551 B) ·
`--02D.img` (**A**) · patch `D:\ND\S\N-250306K05-patch.img`

### J — `N-900-188` — 4/4 ✅ **not in the carver**, three dated releases
86-08-04, 86-11-26, 86-12-09 — `-I` (**S M**) · `-II` (**S**) · `-III`
(**A**) · `-IV` (symbol lists). Plus 4 × `ND-PATCH-SIN-J`.

### H (COSMOS / Satellite-9) — `N-900-000` — 3/3 ✅ **not in the carver**
1983-01-07, `D:\ND\Frode\Standard Satellite-9 83.01.06 ver H\` — uses the
older **MACM-1718K** and DMAC-1915E.

### `N-10-102` — 3/3 ✅ **not in the carver**
Heavily duplicated (15 copies of `-I`). Canonical: `D:\ND\S\DISK3.img`,
`DISK4.img`, `DISK5.img`. **Caution:** the `-I` copies are *not*
byte-identical — `SINTRAN-I:DATA` ranges 187,629–194,371 B across dates
1984-11-06 and 1985-12-22, so at least two generations share one label.

---

## Incomplete / special

- **H 85-04-17 — `N-10-203` — 2/3**, diskette **II is missing from this
  machine**. `-I` and `-III` present under
  `D:\ND\Frode\Sintran III Version H 85-04-17\`.
- **`N-102-2921-I`** — a self-contained **single-diskette** SINTRAN
  (1984-12-17), `D:\ND\S\N-102-292-I.img`: MACM-1718L, SINTRAN:DATA
  (421,083 B), assemblers and symbol tables all on one floppy.
- **`FLOPPY-SINTRAN`** — `D:\ND\S\FLOPPY-SINTRAN.img`, not a distribution
  set but a **pre-generated running floppy SINTRAN**: `SEGFIL0:DATA`,
  `SEGMENT-03…41:SEGM`, `SYSTEM-SEGMENT`, `DMAC-SEGMENT:SEGM`,
  `START-UP:MODE`. Directly comparable to a carve input.

## MACM versions in the wild

| build | on |
|---|---|
| `MACM-1718K` | `N-10-203-I`, `N-900-000-I`, `N-10-102-I` |
| `MACM-1718L` | `N-900-188-I`, `N-220046K03--01D`, `N-250306K05--01D`, `N-102-2921-I`, `250305L07-XX-01D` (40,039 B), `250306M06-XX-01D` (39,497 B) |

Note the L and M copies differ in size — they are **not** the same build.
The standalone `D:\ND\BPUN\MACM-1718L.BPUN` is a third variant again
(base 0o77120 / 19,273 words, versus the L floppy's 0o76203 / 19,738 words;
both checksums verify).

## Assemblers

| assembler | found on |
|---|---|
| `F32-FMAC-1920C` + `F48-FMAC-1408D` | every `-III` / `-02D` diskette from H onward |
| `DMAC-1915E / F / G` | H · J,K03,K05 · L,M respectively |
| **`FMAC-1408C`, `MAC-1415C`, `F32-FMAC-1920B`** | **`ND-10400A`** only — `D:\ND\Frode\Subsystem Packages\ND-10400A.img`, the sole image carrying plain `MAC` and `FMAC` |
| `MACREL-B:BRF` | `ND-10142B-PART4` |

## Anomalies

- Readable but damaged: `D:\ND\S\tempfile` (0 users/0 files),
  `D:\ND\moved\IMG\disk1.img` (0 users), `D:\ND\S\ND0113.BIN` (0 files),
  `D:\ND\S\TEST.img` (4 users on a 1-user volume).
- Four `COS-CONN-TO-D00.PROG` files are exactly 315,392 bytes but are ND
  `:PROG` executables, not floppy images.
- Genuinely unparseable at floppy sizes: `D:\ND\moved\IMG\ND-10634A.image`,
  `NDDISK11.img`, `NDDISK12.img`, `D:\ND\S\disk22.img`.
