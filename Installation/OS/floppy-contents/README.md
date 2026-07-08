# SINTRAN OS Distribution Floppies — Volume & Content Patterns

> **Validation:** all volume names, page counts, boot-format flags, and file listings below
> are **observed** via `ndtool` (the NDFS disk-image tool, `-i` / `-t`, read-only) on the
> floppy images in the SINTRAN distribution archives (`SINTRAN-*/FLOPPY/`). Interpretations
> of what name *fields mean* are marked **[inf]** where not confirmed by a manual.

---

## 1. Two separate things: volume NAME vs floppy CONTENT

- The **volume-name scheme** is a **general Norsk Data product-floppy convention** — it is
  **NOT unique to SINTRAN OS**. ENCOS, BRF-LINKER, S3-CONFIG, the assembler subsystem, etc.
  all use the same `NNNNNN<L><VV>-XX-NND` form (verified: `210580B01-XX-01D` = ENCOS,
  `210721C01-XX-01D` = BRF-LINKER, `210400B00-XX-01D` = MAC/FMAC assemblers).
- What **uniquely identifies a SINTRAN OS floppy is its CONTENT signature** (§3), not its
  name. The user's assumption is correct: the OS file set is distinctive.

---

## 2. Volume-name pattern (general ND convention)

```
[N-] NNNNNN <L> <VV> [-XX] - <DD> D
      │      │    │     │      │   └ format/density indicator (always "D" here) [inf]
      │      │    │     │      └──── disk number within the set (01, 02, 03)      [obs]
      │      │    │     └─────────── option/sub-product field ("XX" = standard)   [inf]
      │      │    └───────────────── release revision (e.g. 03, 05, 06, 07)       [inf]
      │      └────────────────────── SINTRAN release letter (K, L, M)             [obs]
      └───────────────────────────── ND article/product number (6 digits)        [obs]
```

Observed OS-floppy volume names:

| Version | Disk 1 | Disk 2 | Disk 3 | Pages | Floppy |
|---------|--------|--------|--------|-------|--------|
| **H** (old scheme) | `N-10-102-I` | `N-10-102-II` | `N-10-102-III` | 154 | 360 KB |
| **K** | `N-220046K03--01D` | `N-220046K03--02D` | — | 616 | 1.2 MB |
| **K05** | `N-250306K05--01D` | `N-250306K05--02D` | — | 640 | 1.25 MB |
| **L** | `250305L07-XX-01D` | `250305L07-XX-02D` | `250305L07-XX-03D` | 616 | 1.2 MB |
| **M** | `250306M06-XX-01D` | `250306M06-XX-02D` | `250306M06-XX-03D` | 616 | 1.2 MB |

Notes:
- **H uses an older naming scheme** (`N-<part>-<roman numeral>`), not the `NNNNNN<L><VV>` form.
- K/K05 use a double dash `--` where the `-XX` option field would be; L/M use `-XX-`. [obs]
- The release letter embedded in the name (`K03`, `K05`, `L07`, `M06`) = SINTRAN release +
  internal revision. This is the most reliable *name-based* version cue for K/L/M. [obs]

---

## 3. CONTENT signature — what makes a floppy a SINTRAN OS floppy

Every OS floppy set has exactly **one USER: SYSTEM** and a fixed role per disk:

### Disk 1 = the BOOT floppy (the strongest signature)
- **`Boot format: FLOMON`** (verified on every version's disk 1: H-I, VSXK1, K05-01D, VSXL1, VSXM1)
- Contains exactly **2 files**: `MACM-1718x:BPUN` + the first SINTRAN image part:

| Version | MACM loader | SINTRAN image (part 1) |
|---------|-------------|------------------------|
| H | `MACM-1718K:BPUN` | `SINTRAN-I:DATA` |
| K | `MACM-1718L:BPUN` | `SINTRAN:DATA` |
| K05 | `MACM-1718L:BPUN` | `SINTRAN:DATA` |
| L | `MACM-1718L:BPUN` | `SINTRAN-L-1:DATA` |
| M | `MACM-1718L:BPUN` | `SINTRAN-M-1:DATA` |

> **`MACM-1718*:BPUN` together with a `SINTRAN*:DATA` file on a FLOMON-boot floppy is the
> definitive SINTRAN-OS-boot-floppy fingerprint.** No other ND product floppy has this pair.

### Disk 2 = continuation of the SINTRAN image (mostly)
- H: `SINTRAN-II:DATA`; L: `SINTRAN-L-2:DATA`; M: `SINTRAN-M-2:DATA` (single file, the rest of the OS image).
- K/K05 differ: disk 2 is the **tools/symbols disk** (see disk 3 content) rather than image part 2 — K's full OS image fits on one 574-page file.

### Last disk = tools + symbol lists (the post-load setup kit)
Common files across K/K05/L/M last disks (verified):
- `NEW-SYSTEM:PROG` — the guided post-install program (run after entering main directory)
- `DMAC-191x:BPUN`, `F32-FMAC-1920C:PROG`, `F48-FMAC-1408D:PROG`
- `COS-TADADM:BPUN`
- Symbol lists: `SYMBOL-1-LIST:SYMB`, `SYMBOL-2-LIST:SYMB`, `FILSYS-SYMBOLS:SYMB`,
  `RTLO-SYMBOLS:SYMB`, `LIBRARY-MARKS:SYMB`, `N500-SYMBOLS:SYMB`
- **L/M add XMSG**: `XMSG-COMMAND:PROG`, `XMSG-STARTEX:MODE`, `XMSG-STARTEX:BATC`, `XMSG-SYMBOL-LIST:SYMB`
- **M adds**: `N5000-SYMBOLS:SYMB`, `ND500-MONITOR:BPUN`, `ER-S3WD-*` (error-logging) — matching the richer M feature set.

H's disk III is the equivalent kit (NEW-SYSTEM, symbol lists, DMAC-1915E, F32/F48-FMAC).

---

## 4. Recognition algorithm (how to detect a SINTRAN OS floppy)

Given a floppy image, classify with `ndtool -i` + `ndtool -t`:

1. **Single user `SYSTEM`** and small volume (154 / 616 / 640 pages) → candidate.
2. **Boot floppy?** `Boot format: FLOMON` **AND** files = {`MACM-1718*:BPUN`, `SINTRAN*:DATA`}
   → **SINTRAN OS boot floppy.** Read the MACM version (`MACM-1718K` = older/H, `-1718L` = K+).
3. **Continuation floppy?** single `SINTRAN*-2:DATA` (or `SINTRAN-II:DATA`) file → OS image part 2.
4. **Tools/symbols floppy?** contains `NEW-SYSTEM:PROG` + the `*-SYMBOLS`/`SYMBOL-*-LIST:SYMB`
   set + `DMAC`/`FMAC` → OS distribution kit disk.
5. **Version cue:** the volume name's embedded release token (`K03/K05/L07/M06`); confirm with
   the `SINTRAN-<x>-*:DATA` filename (L/M encode the letter in the file name) and the MACM build.
6. **Patch floppy** (related, not OS): own product number, `Boot format: Binary`, contains
   `PATCH-FILE:MODE` + `PATCHES:PATC` + `NEW-SYSTEM:PROG` (verified: `ND-PATCH-SIN-M.img` =
   vol `250360M04-XX-01D`).

> Counter-examples confirming the rule: `210400B00-XX-01D` is `Boot format: FLOMON` too, but
> contains only assembler BPUNs (FMAC/MAC/QED/NPL) — **no MACM-1718, no SINTRAN:DATA** → not
> an OS floppy. So FLOMON-boot alone is insufficient; the **MACM+SINTRAN pair** is decisive.

---

## 5. Per-floppy verified inventory (summary)

See §3 tables. Full file listings were captured via `ndtool -t` on the floppy images in
each distribution archive's `FLOPPY/` folder:
- SINTRAN-H: `N-10-102-{I,II,III}.img`
- SINTRAN-K: `VSXK{1,2}.img`
- SINTRAN-K05: `N-250306K05--0{1,2}D.img`
- SINTRAN-L: `VSXL{1,2,3}.IMG`
- SINTRAN-M: `VSXM{1,2,3}.IMG`

Re-run `ndtool -i <img>` / `ndtool -t <img>` to recover details.

**Parent:** [../README.md](../README.md)
