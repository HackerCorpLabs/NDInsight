# SINTRAN III PATCH media — inventory

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\patches\inventory.md`

Every patch floppy image found on this machine, extracted with

```
E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build-win\ndtool.exe -t <image>
E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build-win\ndtool.exe -x -p -o <dir> <image>
```

All listings and dates below are **VERIFIED** — copied from `ndtool -t` output.
`33CPV` values are **VERIFIED** — read from each floppy's `SYMBOLS:FADM`.
Record counts / report ranges are **VERIFIED** — produced by
`tools/parse_patch.py --summary`.

---

## Summary table

| Key | Image | Volume | Date | `33CPV` (patch level) | payload | records | report range |
|---|---|---|---|---|---|---|---|
| H-17 | `D:\ND\Frode\Sintran III H Patch 17\Patchfile-17.img` | `PATCH-SINTRAN` | 1983-07-04 (+ 1984-01-06) | `000017` | `PATCH-FILE:PATC` 96 910 B | 94 | SIN-H 1–144 |
| H-223 | `D:\ND\Frode\Sintran III H Patch 223\SINTRAN III H Patchfile 223 (backup).img` | `PATCH-SINTRAN` | 1984-03-25 (+ 1984-11-09) | `000223` | `PATCH-FILE:PATC` 144 608 B | 124 | SIN-H 1–184 |
| H-2204 | `D:\ND\S\nddisk5.img` | `PATCH-SINTRAN` | 1984-03-25 | `002204` | `PATCH-FILE:PATC` 147 005 B | 126 | SIN-H 1–189 |
| J-10300 | `D:\ND\Frode\Sintran III J Patch 10300\PATCH-SIN-J-10300, 86.05.14.img` | `ND-PATCH-SIN-J` | 1986-05-14 | `010300` | `PATCHES:PATC` 214 193 B | 236 | SIN-J 1–309 |
| J-11100 | `D:\ND\Frode\Sintran III J Patch 11100\PATCH-SIN-J-11100, 86.10.23.HKE.img` (and `… (2).img`) | `ND-PATCH-SIN-J` | 1986-10-23 | `011100` | `PATCHES:PATC` 232 179 B | 258 | SIN-J 1–337 |
| J-11110 | `D:\ND\Frode\Sintran III J Patch 11110\PATCH-SIN-J-11110, 87.08.13.img` | `ND-PATCH-SIN-J` | 1986-10-23 (file dates), floppy label 87.08.13 | `011110` | `PATCHES:PATC` 232 179 B | 258 | SIN-J 1–337 |
| K-10200 | `D:\ND\S\N-250306K05-patch.img` | `ND-PATCH-SIN-K` | 1988-02-26 … 1988-07-27 | `010200` | `PATCHES-10200:PATC` 241 139 B | 284 | SIN-K 7–417 |
| K-011411 | `D:\ND\Frode\Sintran III K Patch 011411 (211291K12, 5.25 inch)\211291K12-XX-01D.img` | `ND-PATCH-SIN-K` | 1988-08-23 … 1988-09-01 | `011411` | `PATCHES:PATC` 258 222 B | 297 | SIN-K 7–451 |

Note on naming: the task brief called `D:\ND\S\nddisk5.img` "vol PATCH-FILE-G".
Its **volume label is actually `PATCH-SINTRAN`**; `PATCH-FILE-G:MODE` is a *file*
on it. Its patch level is `33CPV=002204`.

The two images in `Sintran III J Patch 11100\` are byte-identical in every
extracted file (MD5-verified).

---

## Per-floppy file listings

### H-17 — `PATCH-SINTRAN`, `33CPV=000017`

```
1983-07-04 16:11:43  (SYSTEM)FILE-TEST:PROG;1              2048 bytes   12 pages
1983-07-04 16:11:58  (SYSTEM)FILE-TEST-32:PROG;1          23552 bytes   12 pages
1983-07-04 16:12:15  (SYSTEM)FILE-TEST-48:PROG;1          23552 bytes   12 pages
1983-07-04 16:12:31  (SYSTEM)PATCH-FILE-F:MODE;1           4639 bytes    3 pages
1983-07-04 16:12:39  (SYSTEM)PATCH-FILE:PATC;1            96910 bytes   48 pages
1983-07-04 16:13:44  (SYSTEM)REFERENCE:FADM;1              1955 bytes    1 pages
1983-07-04 16:13:51  (SYSTEM)SYMBOLS:FADM;1                  17 bytes    1 pages
1984-01-06 16:43:51  (SYSTEM)PATCH-FILE:OUT;1             36903 bytes   19 pages
```

`PATCH-FILE:OUT` is a **log of an actual patch run on 1984-01-06** — six months
after the floppy was written. It is the single best evidence document on the
media: it shows the FMAC `IMAGE-FILE :` prompt, the `33CPV:000017` banner, the
`DEFINE-SEGMENT-FILE`, and the `READ-BINARY DMAC-1915E 7` RT-loader step.

### H-223 — `PATCH-SINTRAN`, `33CPV=000223`

```
1984-03-25 16:05:32  (SYSTEM)FILE-TEST-H:PROG;1           18432 bytes    9 pages
1984-03-25 16:05:45  (SYSTEM)CPU-TYPE:PROG;1             133120 bytes    2 pages
1984-03-25 16:05:50  (SYSTEM)PATCH-FILE-G:MODE;1           5080 bytes    3 pages
1984-03-25 16:05:55  (SYSTEM)PATCH-FILE:PATC;1           144608 bytes   72 pages
1984-03-25 16:07:16  (SYSTEM)REFERENCE:FADM;1              2886 bytes    2 pages
1984-03-25 16:07:22  (SYSTEM)SYMBOLS:FADM;1                  17 bytes    1 pages
1984-11-09 15:13:52  (SYSTEM)PATCH-FILE:OUT;1              9689 bytes    5 pages
```

### H-2204 — `PATCH-SINTRAN`, `33CPV=002204` (`D:\ND\S\nddisk5.img`)

```
1984-03-25 16:05:32  (SYSTEM)FILE-TEST-H:PROG;1           18432 bytes    9 pages
1984-03-25 16:05:45  (SYSTEM)CPU-TYPE:PROG;1             133120 bytes    2 pages
1984-03-25 16:05:50  (SYSTEM)PATCH-FILE-G:MODE;1           5080 bytes    3 pages
1984-03-25 16:05:55  (SYSTEM)PATCH-FILE:PATC;1           147005 bytes   72 pages
1984-03-25 16:07:16  (SYSTEM)REFERENCE:FADM;1              2960 bytes    2 pages
1984-03-25 16:07:22  (SYSTEM)SYMBOLS:FADM;1                  17 bytes    1 pages
(not set)            (SYSTEM)PATCH-FILE:OUT;1              2038 bytes    1 pages
```

`FILE-TEST-H:PROG`, `CPU-TYPE:PROG` and `PATCH-FILE-G:MODE` are **byte-identical**
to H-223's copies (MD5). Only `PATCH-FILE:PATC`, `REFERENCE:FADM` and
`SYMBOLS:FADM` differ — this is H-223 rolled forward by three reports.

### J-10300 — `ND-PATCH-SIN-J`, `33CPV=010300`

```
1986-05-14 20:51:29  (SYSTEM)MULTI-FUNCTION:PROG;1        36864 bytes   18 pages
1986-05-14 20:52:00  (SYSTEM)START-PATCH-FILE:MODE;1       4193 bytes    3 pages
1986-05-14 20:52:16  (SYSTEM)PATCH-FILE:MODE;1             3677 bytes    2 pages
1986-05-14 20:52:23  (SYSTEM)PATCHES:PATC;1              214193 bytes  105 pages
1986-05-14 20:55:05  (SYSTEM)REFERENCE:FADM;1              4998 bytes    3 pages
1986-05-14 20:55:17  (SYSTEM)SYMBOLS:FADM;1                  33 bytes    1 pages
```

`SYMBOLS:FADM` here carries **two** definitions — `33WMR=000001` as well as
`33CPV=010300`. J-11100/11110 dropped `33WMR`.

### J-11100 / J-11110 — `ND-PATCH-SIN-J`

Both floppies carry an identical directory (all file dates 1986-10-23):

```
1986-10-23 10:55:22  (SYSTEM)MULTI-FUNCTION:PROG;1        36864 bytes   18 pages
1986-10-23 10:58:13  (SYSTEM)START-PATCH-FILE:MODE;1       4193 bytes    3 pages
1986-10-23 10:52:14  (SYSTEM)PATCH-FILE:MODE;1             3677 bytes    2 pages
1986-10-23 10:23:10  (SYSTEM)PATCHES:PATC;1              232179 bytes  115 pages
1986-10-23 10:26:18  (SYSTEM)REFERENCE:FADM;1              5357 bytes    3 pages
1986-10-23 10:26:46  (SYSTEM)SYMBOLS:FADM;1                  23 bytes    1 pages
```

MD5 comparison shows only `PATCHES:PATC` and `SYMBOLS:FADM` differ between the
two levels. The `PATCHES:PATC` difference is **four lines** (see README §5) —
report SIN-J 315 revised C → D. The file dates were **not** updated for the
11110 release; the floppy label date (87.08.13) is the only date evidence.

### K-10200 — `ND-PATCH-SIN-K`, `33CPV=010200` (`D:\ND\S\N-250306K05-patch.img`)

```
1988-07-27 14:54:47  (SYSTEM)PATCHES-10200:PATC;1        241139 bytes  118 pages
1988-02-26 14:52:53  (SYSTEM)REFERENCE-10200:FADM;1        8174 bytes    4 pages
1988-02-26 14:52:36  (SYSTEM)SYMBOLS-10200:FADM;1            17 bytes    1 pages
1988-06-07 16:07:52  (SYSTEM)NEW-SYSTEM:PROG;1            36864 bytes   18 pages
1988-06-07 16:08:15  (SYSTEM)START-PATCH-FILE:MODE;1       1785 bytes    1 pages
1988-06-07 16:08:28  (SYSTEM)PATCH-FILE:MODE;1             3144 bytes    2 pages
```

**This is the patch set that matches the carved `K-VSX-500` system**
(`REVLE = 010200`). See README §6.

### K-011411 — `ND-PATCH-SIN-K`, `33CPV=011411`

```
1988-08-23 10:19:21  (SYSTEM)NEW-SYSTEM:PROG;1            36864 bytes   18 pages
1988-08-23 10:19:25  (SYSTEM)START-PATCH-FILE:MODE;1       1808 bytes    1 pages
1988-08-23 10:19:26  (SYSTEM)PATCH-FILE:MODE;1             3138 bytes    2 pages
1988-09-01 10:11:38  (SYSTEM)SYMBOLS:FADM;1                  17 bytes    1 pages
1988-09-01 10:12:22  (SYSTEM)PATCHES:PATC;1              258222 bytes  127 pages
1988-08-23 10:20:50  (SYSTEM)REFERENCE:FADM;1              8991 bytes    5 pages
```

Product number on the label: **211291K12**.

---

## Patch identifiers carried

**VERIFIED.** Each patch record carries `SIN-<family> <report-number>
[<revision-letter>]`. There is **no date** on an individual patch record in any
of the nine files; the only dates are the file timestamps above and the floppy
labels.

Reports that changed revision letter between the sets we hold (useful as
fingerprints):

| Step | report → revision change |
|---|---|
| H-17 → H-223 | 3 C→D, 6 C→D, 74 –→B, 83 –→B, 84 B→C, 105 C→E, 127 B→C, 130 –→B, 140 –→B, 142 B→G |
| H-223 → H-2204 | none |
| J-10300 → J-11100 | 34 D→E, 180 B→C, 208 B→D, 271 –→B, 273 –→B, 279 D→E |
| J-11100 → J-11110 | **315 C→D** (the entire release) |
| K-10200 → K-011411 | 61 B→C, 160 D→G, 162 I→J, 164 A→C, 254 B→C, 271 –→B, 318 –→B, 328 –→B |

Reports **withdrawn** (present in the earlier set, absent from the later):
H-17→H-223 removes 19, 94, 111; H-223→H-2204 removes 14. The J and K steps
remove nothing.

---

## Carved installed systems in this repo

`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\`

| System | `REVLE` (word oct 4057 of S3?DPIT) | `SYSNO` | matching patch media held? |
|---|---|---|---|
| `K-VSX-500` | `010200` | `000144` | **YES** — `D:\ND\S\N-250306K05-patch.img` |
| `L-VSX-500` | `000000` | `000146` | n/a — no level recorded |
| `M-VSX-500` | `003200` | `000144` | no M-series patch floppy found |

`S3PATCH` (segment 43, load 174000, 2 pages) residue:

| System | `S3PATCH` byte sum | first non-zero word |
|---|---|---|
| K-VSX-500 | 175 577 | `174000 = 004057` (= address of `REVLE`) |
| L-VSX-500 | **0** | — |
| M-VSX-500 | 34 811 | `176000 = 004057` |

Machine-readable: `samples/carved-revle.json`.
