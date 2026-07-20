# Disc support in SINTRAN III — answered from CARVED SYSTEM DATA

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\CARVED-DISC-SUPPORT.md`

**Scope rule for this document:** every statement is derived from *carved system
data* — carved segment binaries, the resident common-code image, the SINTRAN
image symbol tables, and the recovered NPL source listings. **No OCR'd manual
was consulted, and none is cited.** Where the NPL source is used it is used for
*naming and comments only* and is always cross-checked against carved bytes
(the NPL tree is a different revision from the carved images).

Every claim is tagged **[VERIFIED]** (exact file + address/offset + the bytes or
words quoted) or **[INFERRED]** (reasoning shown). Where nothing was found the
document says **NOT FOUND**.

Companion tool written for this analysis:
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\find_disc_layout_table.py`

---

## 0. Executive summary

| Question | Answer |
|---|---|
| Is there a disc-type / geometry TABLE inside the kernel? | **YES — two of them, and they are linked.** (1) `DISPE` + the `DTxxx` geometry records; (2) the **device-name table** `S3DNAM`, which names every supported disc unit, gives its **size in pages**, and points at a `DTxxx` record. Both byte-verified in all three carved systems. |
| Where are the valid disc SIZES? | In the **device-name table** (§2A). Every named large-disc unit carries a hard-coded 32-bit page count. Every **SCSI** and **optical** entry carries **size = 0** — those sizes are interrogated from the drive. |
| How many disc types does it hold? | **24 records** in L-VSX-500 and M-VSX-500; **18 records** in K-VSX-500 (the ST-506/Winchester group is absent from that generated system). Index range 0..47B (`MAXDI=47`). |
| Is SCSI in the table? | **YES**, as disc-type index **36B**, record `DTSSS`, with **all geometry fields zero** — SCSI geometry is *not* tabulated, it is interrogated from the drive. |
| Are SCSI disc SIZES validated against a table? | **NO.** Capacity is taken from the device (`READ CAPACITY`) and from the disc's own control record; it is a 32-bit value. The only kernel-enforced size limit on the SCSI path is on the **block/record size**, not on the capacity. |
| `MSTYP` numeric value | **NOT FOUND** in any carved symbol table (K03/L07/M06) or NPL listing. The open question is *not* resolved; see §7. |

---

## 1. THE disc-type table — `DISPE` and the `DTxxx` records  **[VERIFIED]**

### 1.1 Record layout

The layout is documented in the recovered source listing
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb`, listing address
`040752` (lines 19568-19582), quoted literally:

```
%  DISK LAYOUT:
%      SECWO, SECTR, SECSY, MAXCY, POLSY, REFOR, RESCY, DISPN
%          SECWO = WORDS   / SECTOR.
%          SECTR = SECTORS / TRACK.
%          SECSY = SECTORS / CYLINDER.
%          MAXCY = VALUE OF MAX CYLINDER.
%          POLSY = VALUE OF FIRST CYLINDER IN POOL.
%          REFOR = FORMAT TYPE:
%                     0 = TRACK OR NO REALLOCATION.
%                    10 = TRACK OR NO REALLOCATION + TEST SECTOR ADDRESS
%                    20 = SECTOR REALLOCATION.
%          RESCY = VALUE OF FIRST RESERVED CYLINDER.
%          ALTFO = ALTERNATIVE FORMAT (ADDRESS OF DTxxx OR 0)
%          DISPN = INDEX FOR THIS ENTRY(FORMAT).
```
and `DILEZ=11` (octal) — **9 words per record** (the comment lists 8 names but
`ALTFO` sits between `RESCY` and `DISPN`, giving 9). **[VERIFIED]**

### 1.2 The table AS CARVED

Carved from the raw big-endian binaries, not from any listing:

| Version | file | load base | table start | `DISPE` | records |
|---|---|---|---|---|---|
| K-VSX-500 | `versions\K-VSX-500\segments\044-S3IDPIT.bin` (twin `053-S3SDPIT.bin`) | `4000B` | `012337B` | `012267B` | **18** |
| L-VSX-500 | `versions\L-VSX-500\segments\044-S3IDPIT.bin` (twin `053-S3SDPIT.bin`) | `4000B` | `031041B` | **`031371B`** | **24** |
| M-VSX-500 | `versions\M-VSX-500\segments\044-S3IDPIT.bin` (twin `053-S3SDPIT.bin`) | `4000B` | `035756B` | **`036306B`** | **24** |

**Independent confirmation of the L and M addresses:** the L07 symbol table
`SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-2-LIST.SYMB.TXT:337` says `DISPE=031371`
and `:338 MAXDI=000047`; M06 `SYMBOL-2-LIST.SYMB.TXT:461` says `DISPE=036306`.
The carved table of 24 records is 24 × 9 = 216 words = `330B` long and starts at
`031041B`; `031041B + 330B = 031371B` — **exactly `DISPE`**. Same arithmetic
holds for M (`035756B + 330B = 036306B`). **[VERIFIED]**

The carved K image is a **K05** system while the only K symbol table available
is **K03**; `K03 DISPE=012221` does *not* match the carved image (that address
is zero-filled). The K figure `012267B` above was derived structurally, from
the carved pointer array itself (see §1.4). **[VERIFIED by structure,
INFERRED as to the name `DISPE`]**

### 1.3 The carved records — L-VSX-500 (L07), addresses octal

Produced by `tools\find_disc_layout_table.py 044-S3IDPIT.bin 4000`
(add `4000B` to the addresses it prints; it prints file-relative addresses
when the meta base is not supplied). Values octal.

| addr | SECWO | SECTR | SECSY | MAXCY | POLSY | REFOR | RESCY | ALTFO | DISPN | name (from listing) |
|---|---|---|---|---|---|---|---|---|---|---|
| 031041 | 1000 | 14 | 44 | 577 | 1124 | 0 | 0 | 0 | **10** | DT014 — DISC-14MB |
| 031052 | 1000 | 14 | 44 | 1134 | 1124 | 0 | 0 | 0 | **11** | DT021 — DISC-21MB |
| 031063 | 1000 | 11 | 33 | 1475 | 1462 | 0 | 0 | 0 | **12** | DT023 — DISC-23MB |
| 031074 | 1000 | 11 | 66 | 1475 | 1462 | 0 | 0 | 0 | **13** | DT045 — DISC-45MB |
| 031105 | 1000 | 11 | 33 | 1775 | 1763 | 0 | 1776 | 0 | **14** | DT028 — DISC-28MB |
| 031116 | 1000 | 11 | 110 | 1775 | 1763 | 0 | 1776 | 0 | **15** | DT074 — DISC-74MB |
| 031127 | 1000 | 22 | 22 | 1466 | 1465 | 10 | 0 | 0 | **20** | DT030 — DISC-30/60/90MB |
| 031140 | 1000 | 22 | 132 | 632 | 0 | 10 | 0 | 0 | **21** | DT037 — DISC-38MB |
| 031151 | 1000 | 22 | 132 | 1466 | 1434 | 10 | 1465 | 0 | **22** | DT070 — DISC-70MB |
| 031162 | 1000 | 22 | 132 | 1466 | 1465 | 10 | 0 | 0 | **23** | DT075 — DISC-75MB |
| 031173 | 1000 | 22 | 264 | 1466 | 1441 | 10 | 1465 | 031204 | **24** | DT140 — DISC-140MB-F |
| 031204 | 1000 | 21 | 252 | 1466 | 1462 | 20 | 1465 | 031173 | **25** | DT135 — DISC-140MB-S |
| 031215 | 1000 | 22 | 264 | 1466 | 1465 | 10 | 0 | 0 | **26** | DT160 — DISC-2-75MB |
| 031226 | 1000 | 22 | 526 | 1466 | 1465 | 10 | 0 | 0 | **27** | DT288 — DISC-288MB-R |
| 031237 | 1000 | 21 | 630 | 1306 | 1303 | 20 | 1305 | 031250 | **30** | DT285 — DISC-288MB-S |
| 031250 | 1000 | 22 | 660 | 1306 | 1260 | 10 | 1305 | 031237 | **31** | DT300 — DISC-288MB-F |
| 031261 | 1000 | 32 | 1160 | 1306 | 1303 | 20 | 1305 | 031272 | **32** | DT450 — DISC-450MB-S |
| 031272 | 1000 | 33 | 1210 | 1306 | 1260 | 10 | 1305 | 031261 | **33** | DT460 — DISC-450MB-F |
| 031303 | 1000 | 32 | 404 | 2300 | 2267 | 20 | 2277 | 0 | **17** | DT310 — DISC-288MB-E |
| 031314 | 1000 | 37 | 1115 | 1367 | 1363 | 20 | 1366 | 0 | **34** | DT470 — DISC-450MB-N |
| 031325 | 1000 | 43 | 536 | 1466 | 1460 | 20 | 1465 | 0 | **35** | DT290 — DISC-288MB-N |
| 031336 | 1000 | **0** | **0** | **0** | **0** | 0 | 0 | 0 | **36** | **DTSSS — SCSI DISK** |
| 031347 | 1000 | 36 | 0 | 0 | 0 | 0 | 0 | 0 | **40** | DTOD1 — OPTICAL DISK (RS=1024) |
| 031360 | **2000** | 36 | 0 | 0 | 0 | 0 | 0 | 0 | **41** | DTOD2 — OPTICAL DISK (RS=2048) |

The 24 records are **[VERIFIED]** from the carved bytes. The `DTxxx` names and
the `% DISC-nnnMB-x` comments are **[VERIFIED]** from the recovered source
listing `s3vs-4.symb` lines 19583-19613, where the numeric rows match the
carved rows word for word.

M-VSX-500 carries the identical 24 records at `035756B..036305B` (spot check:
`036253B = 1000 0 0 0 0 0 0 0 36` = `DTSSS`). **[VERIFIED]**

K-VSX-500 carries only **18** records at `012337B..012600B`: DISPN
`20,21,22,23,24,25,26,27,30,31,32,33,17,34,35,36,40,41`. The six ST-506 /
Winchester records (DISPN 10-15) are **absent** from the carved K05 system.
**[VERIFIED]**

### 1.4 `DISPE` — the disc-type pointer array **[VERIFIED]**

L07, carved bytes at `031371B` (`044-S3IDPIT.bin`, base `4000B`):

```
 031371: 000000 000000 000000 000000 000000 000000 000000 000000   % types 00-07
 031401: 031041 031052 031063 031074 031105 031116 000000 031303   % types 10-17
 031411: 031127 031140 031151 031162 031173 031204 031215 031226   % types 20-27
 031421: 031237 031250 031261 031272 031314 031325 031336 000000   % types 30-37
 031431: 031347 031360 000000 000000 000000 000000 000000 000000   % types 40-47
```

M06 at `036306B` is structurally identical (pointers `035756`…`036275`).

K05 (carved), pointer array at `012267B`:

```
 012267: 000000 x8                                                 % types 00-07
 012277: 000000 x7 012513                                          % types 10-17  (10-15 EMPTY, 17=DT310)
 012307: 012337 012350 012361 012372 012403 012414 012425 012436   % types 20-27
 012317: 012447 012460 012471 012502 012524 012535 012546 000000   % types 30-37
 012327: 012557 012570 000000 x6                                   % types 40-47
```

**Reading:** a disc type is *supported by this generated system* iff its
`DISPE` entry is non-zero. `MAXDI=47B` is the maximum index. **[VERIFIED]**

The bound is enforced in code: `IP-P2-DISK-START.NPL:52` (listing address
`054024`) reads

```
054024   IF 40>T AND DISPE(T)><0 THEN   % 37 IS MAX FORMAT NUMBER
```

for the SMD driver's *fn 36 = READ ELEMENT IN "DISC-LAYOUT-TABLE"* function,
and `IP-P2-SCSI-DISK.NPL:328` (`057052`) / `IP-P2-SCSI-OPDI.NPL:196` (`066064`)
use `IF X<<="MAXDI" AND DISPE(X)><0`. **[VERIFIED from the recovered source;
the table those lines index is byte-verified above.]**

### 1.5 Correction to an earlier finding

A parallel pass over `006-S3FS` concluded "no repeating disc-geometry table
exists in SINTRAN L07". That conclusion is **wrong**: the table exists, but it
lives in the **DPIT** segment (`044-S3IDPIT` / `053-S3SDPIT`, load base
`4000B`), not in the file-system segment. The symbols `SECSY`, `SECTR`,
etc. found in `FILSYS-SYMBOLS` are the *field offsets into a record of this
table* (`SECTR=1`, `SECSY=2`, `DISPN=10B` — and indeed word 1 = SECTR, word 2 =
SECSY, word 8 = DISPN in the carved records, exactly matching). **[VERIFIED]**

---

## 2A. The DEVICE-NAME table `S3DNAM` — the list of supported disc units and their sizes **[VERIFIED]**

This is the single most direct answer to "what are the valid disc sizes and
supported disc units".

### 2A.1 Where it lives **[VERIFIED]**

| version | file | load base | first entry |
|---|---|---|---|
| K-VSX-500 | inside `segments\006-S3FS.bin` (twin `012-S3FSSV.bin`) | `26000B` | `DISC-38MB-1` at `114651B` |
| L-VSX-500 | **own segment** `segments\101-S3DNAM.bin` (twin `100-S3SDNAM.bin`) | `164000B` | `DISC-38MB-1` at `164000B` |
| M-VSX-500 | **own segment** `segments\101-S3DNAM.bin` (twin `100-S3SDNAM.bin`) | `164000B` | `DISC-38MB-1` at `164000B` |

`100-S3SDNAM.meta.json` states `"description": "Save of device name table"`.
`versions\L-VSX-500\inputs\list-segment.txt` lists `S3SDNAM 100` / `S3DNAM 101`;
K's `list-segment.txt` has no such segments. **The move of this table out of the
file-system segment into its own segment pair is a K→L architectural change.**
**[VERIFIED]**

### 2A.2 Entry format — 17 words per entry, identical in K, L, M **[VERIFIED]**

Raw first entry, `101-S3DNAM.bin` byte 0, address `164000B`:

```
042111 051503 026463 034115 041055 030447 000000 000000
000000 044066 001000 002001 030346 000515 001100 000000 031140
```

| word | value here | content |
|---|---|---|
| 0-7 | `042111…030447` | packed ASCII name `DISC-38MB-1` + `'` (047) terminator — **[VERIFIED]** |
| 8:9 | `000000 044066` | **32-bit device size in PAGES** = 18486 — **[VERIFIED value, INFERRED unit]** |
| 10 | `001000` | `1000B` disc, `400B` floppy, `1B` mag tape — **[VERIFIED]** |
| 11 | `002001` | unit / target selector (SCSI targets 1..8 = `2021`…`2027`,`2020`) — **[VERIFIED]** |
| 12 | `030346` | = symbol `BABST=030346` (`SYMBOLS\L07\FILSYS-SYMBOLS.SYMB.TXT`) — **[VERIFIED]** |
| 13 | `000515` | constant, disc class — **[VERIFIED]** |
| 14 | `001100` | **LOGICAL DEVICE NUMBER** — **[VERIFIED, see 2A.4]** |
| 16 | `031140` | **pointer to the `DTxxx` geometry record** — `031140B` = `DT037` in §1.3 — **[VERIFIED]** |

**Word 16 is the link to §1.** Every value observed in the L07 table is one of
the 24 record addresses byte-verified in §1.3 — `031041`(DT014) … `031360`
(DTOD2), and **`031336` (`DTSSS`) for every one of the 112 SCSI entries**.
Two tables carved independently, and the pointers agree exactly.
**[VERIFIED]**

### 2A.3 The valid disc sizes, decoded from the carved bytes **[VERIFIED]**

268 records parse as named entries in L07 (266 real + 2 false hits on
misaligned data). Sizes are the 32-bit word 8:9, decimal pages; a SINTRAN page
is 1024 words = 2048 bytes.

| named family | pages | ≈ bytes | `DTxxx` record(s) pointed at |
|---|---|---|---|
| `DISC-450MB-n-F` / `-N` | **220584** | 451.8 MB | `031261` DT450, `031314` DT470 |
| `DISC-288MB-n-R/F/E/N` | **140391** | 287.5 MB | `031226` DT288, `031250` DT300, `031303` DT310, `031325` DT290 |
| `DISC-225MB-n-R/F/E/N` | **110292** | 225.9 MB | DT288 / DT300 / DT450 / DT310 / DT470 / DT290 |
| `DISC-140MB-n-F` | **69530** | 142.4 MB | `031204` DT135 |
| `DISC-75MB-n` | **36945** | 75.7 MB | `031162` DT075, `031226` DT288, `031215` DT160 |
| `DISC-74MB-n` | **36396** | 74.5 MB | `031116` DT074 |
| `DISC-70MB-n(-R/F/E/N)` | **34765** | 71.2 MB | `031151` DT070 + the -R/F/E/N variants |
| `DISC-45MB-n` | **22032** | 45.1 MB | `031074` DT045 |
| `DISC-38MB-n` | **18486** | 37.9 MB | `031140` DT037 |
| `DISC-28MB-n` | **13648** | 28.0 MB | `031105` DT028 |
| `DISC-23MB-n` | **11016** | 22.6 MB | `031063` DT023 |
| `DISC-21MB-n` | **10728** | 22.0 MB | `031052` DT021 |
| `DISC-16MB-n` | **8000** | 16.4 MB | `031063` DT023 |
| `DISC-30MB-n` / `-60MB-n` / `-90MB-n` | **7389** | 15.1 MB | `031127` DT030 |
| `DISC-14MB-n` | **6912** | 14.2 MB | `031041` DT014 |
| `FLOPPY-DISC-1/2` | **154** | 0.3 MB | 0 |
| **`DISC-[n-]SCSI-1…14`** (112 entries) | **0** | — | **`031336` DTSSS** |
| **`DISC-[n-]OPTICAL-1…4`** (32 entries) | **0** | — | `031347` DTOD1 |
| `MAG-TAPE-1…4`, `STREAMER-1/2` | 0 | — | 0 |

**[INFERRED]** `DISC-30MB` / `-60MB` / `-90MB` all sharing 7389 pages and
`DT030` means the size field there is *per surface/pack member*, not the whole
drive; the record `DT030` is literally commented `% DISC-30/60/90MB` in the
source listing.

**THE DECISIVE FACT: every SCSI and every optical entry has size 0.**
That is the byte-level statement that SINTRAN does **not** hold a table of
valid SCSI disc sizes — the size is obtained from the drive and from the disc's
control record (§4.3, §4.4). **[VERIFIED]**

### 2A.4 Word 14 is the logical device number **[VERIFIED]**

The values observed in word 14 are exactly the logical device numbers held in
the `MDISCS` attribute arrays of §2:

| word-14 value | device family | matching `MDISCS` entry |
|---|---|---|
| `001100` | all SMD / large-disc names | `BBDIS = (BIGDI, **1100**, …, 1540)` — SMD |
| `002210`…`002225` | `DISC-*-SCSI-1` … `SCSI-14` (controller 1..14) | `SCDIS = (SCDI1, **2210**, …, 144300)` — SCSI, +1 per controller |
| `002232`…`002235` | `DISC-*-OPTICAL-1` … `-4` | continues the SCSI logical-device block |
| `002206`/`002207` | `STREAMER-1/2` | — |
| `001145`/`001156` | `FLOPPY-DISC-1/2` | — |
| `000560`, `001111`, `001231`, `001224` | `MAG-TAPE-*` | `1224` also appears as `WWDIS`'s ST-506 logical device no. |

This resolves what word 14 is: it is **not** the IOX hardware device number
(`500`/`1540`/`144300`) but the SINTRAN **logical device number**, and its base
values come from `MDISCS`. **[VERIFIED — the 1100 / 2210 coincidence with the
byte-verified `MDISCS` arrays is exact.]**

### 2A.5 SCSI addressing capacity of the name table **[VERIFIED]**

`101-S3DNAM.bin` names **14 SCSI controllers × 8 targets = 112 SCSI disc
units** (`167770B DISC-SCSI-1` … `173527B DISC-8-SCSI-14`) and
**4 optical controllers × 8 targets = 32 units** (`173550B` … `174547B`).
K's copy inside `006-S3FS` has the same SCSI block starting at `165556B`.
Unused slots after `174652B` carry the literal name `0000000000'`.
**[VERIFIED]**

Note the distinction: 112 *names* exist, but the number of SCSI unit
**datafields** actually generated into a given system is 9 (K), 10 (L), 4 (M) —
see §4.5.

---

## 2. The main-swap-disc attribute table `MDISCS` **[VERIFIED]**

Carved from the **resident common code**, not a segment:
`versions\L-VSX-500\resident\SINTRAN-DATA_commoncode.bin` (load base `0`).

```
 041445: 000000 000000 000000 041510 041510 041510 041510 041510
 041455: 041510 000000 041515 041515 041515 041515 041515 041515
 041465: 041515 041515 041515 041515 041515 041515 041515 041515
 041475: 041515 041522 000000 000000 000000 000000 000000 000000
 041505: 000000 000000 000000 032770 001224 101160 076732 000500
 041515: 031550 001100 077603 074436 001540 036442 002210 102020
 041525: 102020 144300 000000 000000 000000 000000 000000 000000
```

Decoded (array base `MDISCS = 041440B`, one word per disc-type index):

| index range | pointer | attribute array (5 words) | meaning |
|---|---|---|---|
| 00-07 | 0 | — | no swap device |
| **10-15** | `041510` | `032770, 001224, 101160, 076732, 000500` | **ST-506 / Winchester** |
| 16 | 0 | — | — |
| **17-35** | `041515` | `031550, 001100, 077603, 074436, 001540` | **SMD** |
| **36** | `041522` | `036442, 002210, 102020, 102020, 144300` | **SCSI** |
| 37-47 | 0 | — | — |

**Cross-verification that this really is `MDISCS`:** word 0 of each attribute
array is a datafield address, and all three match the L07 symbol table exactly:

| word | value | L07 `SYMBOL-2-LIST.SYMB.TXT` |
|---|---|---|
| `041510` | `032770` | `WIGDI=032770` (line 362) |
| `041515` | `031550` | `BIGDI=031550` (line 342) |
| `041522` | `036442` | `SCDI1=036442` (line 375) |

**[VERIFIED — three independent hits.]**

Element meaning, from the source header at `s3vs-4.symb`/`PH-P2-OPPSTART.NPL`
listing address `041352` (quoted literally):

```
% MAIN-SWAPPING-DISC DATA  (XXDIS)
% EACH ENTRY CONSIST OF THE FOLLOWING 4 LOCATIONS:
%       0: ADDRESS OF DATAFIELD
%       1: LOGICAL DEVICE NUMBER
%       2: ENTRY POINT OF DRIVER USED BY BOOTS-STRAP, ("SWAP DRIVER")
%       3: ENTRY POINT OF START-UP DRIVER
%       4: HARDW. DEV NO FOR SWAPPE CONT.
INTEGER ARRAY WWDIS:=(WIGDI,1224,ZWDIS,WIDIS,   500);  % ST-506
INTEGER ARRAY BBDIS:=(BIGDI,1100,ZBDIS,BDISK,  1540);  % SMD
INTEGER ARRAY SCDIS:=(SCDI1,2210,SCSWD,SCSWD,144300);  % SCSI
```

So, **byte-verified in the carved image**:

| class | logical device no. | hardware device no. (IOX) |
|---|---|---|
| ST-506 / Winchester | **1224B** | **500B** |
| SMD (large disc) | **1100B** | **1540B** |
| SCSI | **2210B** | **144300B** |

This confirms the previously-verified `)9BYTT` DEVNO mapping (500 / 1540 /
144300) **from the inside of the running system**, and adds the logical device
numbers, which the generation streams do not carry. **[VERIFIED]**

M-VSX-500 has the same three arrays at `041367B / 041375B / 041403B` but with
**6-word** entries (one extra field):
`037175,1224,101434,744,77132,500` / `036465,1100,100007,1425,74636,1540` /
`043060,2210,102400,1111,102400,144300`. The datafield words again match the
M06 symbol table (`WIGDI=037175`, `BIGDI=036465`, `SCDI1=043060`).
**[VERIFIED]**

**K-VSX-500: NOT FOUND.** No 5- or 6-word array of this shape exists in the
carved K resident common code or its segments (searched for word `2210B`
followed within 6 words by `144300B`, and for `1100B` followed by `1540B` — no
hits). The K resident carve may not cover this address range. To settle it,
`SINTRAN-DATA_commoncode.bin` would have to be re-carved for the K05 image over
the full resident extent.

### 2.1 The valid swap-device-type range **[VERIFIED from source, table byte-verified]**

`PH-P2-OPPSTART.NPL:722` (listing `044525`):

```
044525   IF SWTYP<<7 OR>>36 THEN CALL ERRFATAL FI  % ILLEGAL MAIN-SWAP-DEVICE TYPE
044535   IF A=7 THEN                               % MAIN SWAP-DEVICE IS FIXED PHOENIX DISK
044540      20=:SWTYP; 40000=:DSKTYPE; 100000=:XXSWTYPE
```

**A system disc type outside 7..36B is a fatal error at cold start.** Type 7 is
remapped to type 20B with the "fixed" bit `100000B` — this is the carved-system
counterpart of the generation stream's `FR=100000` fixed-disc bit. **[VERIFIED
as source text; the `MDISCS` table it indexes is byte-verified above.]**

---

## 3. How SINTRAN is installed / generated onto a LARGE DISC

Two distinct mechanisms exist, and they must not be conflated.

### 3.1 Off-line: the `)9BYTT` MACM generation stream

Already verified in this project and documented in
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\device-geometry.md`. Not repeated here.

### 3.2 On-line: what the *carved kernel itself* does — `SINTR` cold start

`PH-P2-OPPSTART.NPL` (`SINTR`), listing addresses `044525`-`045101`, is the
routine that lays a SINTRAN system out on the system disc at cold start. The
sequence, quoted:

```
044525  FILL1:  IF SWTYP<<7 OR>>36 THEN CALL ERRFATAL FI  % find swap device
044552          X:= MDISCS(SWTYPE)=:LDTA                  % ADDR OF DISC-ATTRIBUTES
044565          IF A=0 OR A >< T THEN CALL ERRFATAL FI    % ILLEGAL LOGICAL DEV.NO.
044601          LDTA.SWDDRIVER=:"CTRNSF"                  % DISC-DRIVER ADDR
044606          A=:SWPDRIVER                              % ADDR OF SWAP-DRIVER
044612          A=:MASSNO(0)                              % LOGICAL DEV.NO. OF MAIN-SWAP-DEVICE
044614          X:=DISPE(SWTYP); T:=X.S0; A:=2000=:D:=0; *RDIV ST   % SECTORS/TRACK
044623          IF D><0 THEN CALL ERRFATAL FI
044626          A=:ABLPAGE(0)=:CDABLPAGE
044631          A*200+XXSWTYPE=:BLST(1)                   % SET START OF SEGFIL0 IN SECTORS.
044637          DISPE(SWTYP)=:HTABL(0)                    % INITIAL DISC-LAYOUT TABLE
044663          IF "CTRNSF"="BDISK" OR ="SCSWD" THEN
044672             T:=42; X:=1; A:=0=:D                   % READ FORMAT NECESSARY
044676  DIBUS:    CALL CTRNSF; CALL ERRFATAL; GO DIBUS
044701          FI
044701          X:=HTABL(0); ...  =:CDIFELEMENT           % DISC LAYOUT ELEMENT ADDRESS
...
044757  % COPY FROM SAVE TO IMAGE
044764             X:="LSECO"; CALL FAR CRWDISC           % COPY EXT. COMMON
044773             X:="LSDPT"; CALL FAR CRWDISC           % COPY DPIT
045002             X:="LSRPT"; CALL FAR CRWDISC           % COPY RPIT
045011             X:="LSMPT"; CALL FAR CRWDISC           % COPY MPIT
045020             X:="LSIPT"; CALL FAR CRWDISC           % COPY IPIT
045027             X:="LSSGT"; CALL FAR CRWDISC           % COPY SEGMENT TABLE
045036             X:="LFILS"; CALL FAR CRWDISC           % COPY FILE SYSTEM
045044             X:=20;      CALL FAR CRWDISC           % COPY RT-LOADER PROGRAM SEGMENT
```

**Reading [VERIFIED as source text; every table it consults is byte-verified in
§1 and §2]:**

1. The generated constant `SWTYP` selects both the **driver set** (`MDISCS`)
   and the **geometry** (`DISPE`). This is the single point where a system's
   disc type enters the running kernel.
2. `CDABLPAGE = SECWO / 2000B` — sectors per SINTRAN page — is computed from
   the table's `SECWO` field. `RDIV` must divide exactly (`IF D><0 THEN
   ERRFATAL`), so **`SECWO` must be an exact divisor of `2000B` (1024 words =
   one page)**. Every tabulated entry has `SECWO = 1000B` (2 sectors/page)
   except `DTOD2`, which has `2000B` (1 sector/page). **[VERIFIED against the
   carved records in §1.3]**
3. `BLST(1) = CDABLPAGE*200B + XXSWTYPE` — the start of `SEGFIL0` in sectors;
   `XXSWTYPE` is the fixed-disc bit `100000B`. This is the run-time twin of the
   generation stream's `<page>@<shift> FR` address construction.
4. **For SMD (`BDISK`) and SCSI (`SCSWD`) the layout is not taken on faith:**
   the kernel issues driver function **42 = READ FORMAT** against the actual
   drive and installs the answer as `HTABL(0)`, the live disc-layout element.
5. Installation proper is a straight page-range copy from the disc's SAVE area
   to the IMAGE/segment area, one `CRWDISC` call per system component
   (common, DPIT, RPIT, MPIT, IPIT, segment table, file system, RT loader).
   Addresses are `page * CDABLPAGE + XXSWTYPE`, i.e. **derived entirely from
   the disc-layout table entry** — see `CRWDISC` at listing `044427`.

**Where the format number comes from on an SMD disc** — `IP-P2-DISK-START.NPL`
listing `054620`:

```
054620  FIN:   0=:DIFTCOUNT;
054621         IF CTRG/\77=42 THEN                 % READ FORMAT
054626            CTRG SHZ -6/\7=:X                % X=UNIT NUMBER
054632            X:=HTABL(X); T:=X.DISPN; AD:=PARDF.MEMAD; *DEPO
```

i.e. fn 42 returns `DISPN` — the *index into `DISPE`* — for that unit; and fn 36
(`054014`-`054052`) copies the whole `DILEZ`-word layout record out to the
caller. **[VERIFIED as source text]**

---

## 4. How SINTRAN is installed onto SCSI (K onward)

### 4.1 SCSI is disc type 36B and carries NO tabulated geometry **[VERIFIED]**

The carved `DTSSS` record (L07 `031336B`, M06 `036253B`, K05 `012546B`) is

```
 1000  0  0  0  0  0  0  0  36
```

— `SECWO = 1000B` (512 words = 1024 bytes per sector) and **every other
geometry field is zero**. There is no cylinder/head/sector description for SCSI
anywhere in the kernel. **[VERIFIED]**

**[INFERRED]** Consequence: the *only* disc-size information SINTRAN has for a
SCSI disc is what it reads off the device at run time. §4.2 and §4.3 show
exactly what it reads and what it rejects.

### 4.2 The SCSI init sequence, carved

Segment `065-S3SIPIT.bin` (twin `066-S3IIPIT.bin`), load base `32000B`;
whole-segment disassembly
`versions\L-VSX-500\re\segments-ref\065-S3SIPIT\065-S3SIPIT.asm`.
`INQUI` at `062613B` (L07 `SYMBOL-2-LIST`, +376B revision offset, already
established in `re\kernel-carving\SCSI-DISKLAYER-COMPLETE\README.md`).

**INQUIRY, 8-byte allocation length [VERIFIED]:**

| addr | word | meaning |
|---|---|---|
| `062621` | `044073` | `LDA 73` → P-rel `062714` |
| `062714` | `011000` | the CDB word = **`0x1200` = opcode 0x12 INQUIRY** |
| `062624` | `170404` | `SAA 4` |
| `062625` | `156411` | `SHA ZIN 11` → A = 4<<9 = `04000B` = **allocation length 8** |

**Device-type dispatch [VERIFIED bytes, SCSI meanings INFERRED]:**

| addr | word | test | outcome |
|---|---|---|---|
| `062656` | `156570` | `A := SUTYP >> 8` (INQUIRY byte 0) | — |
| `062657` | `131007` | `JAZ` type == 0 | → `062666` |
| `062660-062662` | `171003 142065 124004` | type == 3 | → `062666` |
| `062663-062665` | `171004 140065 124006` | type == 4 | → `062666` else `062673` |
| `062666-062670` | `044423 174265 004423` | set `SUTYP` bit 6 (`5SCDA`, "direct access") | — |
| `062671` | `044033` → `062724` = `022400` | CDB word `0x2500` = **READ CAPACITY(10)** | |
| `062673-062675` | `171001 140065 124003` | type == 1 | → `062676` |
| `062676` | `044027` → `062725` = `002400` | CDB word `0x0500` = **READ BLOCK LIMITS** | |
| `062700-062704` | `171177 140065 124004 171007` | type == `177B` (`0x7F`) | `T := 7` error |

Named by the recovered NPL (`IP-P2-SCSI-DISK.NPL:1240-1256`, listing `062260`):

```
062260   IF A SHZ -10=0 OR =3 OR =4 THEN
062270      SUTYP BONE 5SCDA=:SUTYP           % DIRECT ACCESS DEVICE
062273      22400                             % READ CAPACITY
062275   ELSE IF A=1 THEN
062300      2400                              % READ BLOCK SIZE
062302   ELSE IF A=177 THEN
062305      T:=NOLUN; GO FAR ERREX            % NO SUCH LUN
```

**So the SCSI disc layer accepts INQUIRY peripheral device types `0`, `3` and
`4` as direct-access, type `1` as sequential (tape), and treats `177B` as
"no such LUN".** Anything else falls through to the shift-instruction default.
Separately, `SCSDISK`'s device-type gate at `057552-057556B` (`LDA ,B 23` /
`SHA ZIN SHR 10` / `JAZ` / `SAT 1` / `JMP I 54`) rejects **any nonzero device
type** with error `TYPER = 1` on the *disk* path. **[VERIFIED]**

### 4.3 THE hard SCSI limit: block/record size, not capacity **[VERIFIED]**

Carved (`065-S3SIPIT.asm`, addresses octal):

| addr | word | meaning |
|---|---|---|
| `062757` | `173470` | `AAX 70` — point at the DMA response buffer |
| `062760` | `143322` | `LDDTX 20` — load the **second** double of the READ CAPACITY data = **BLOCK LENGTH (bytes 4-7)** |
| `062761` | `140005` | `SKP IF DA EQL 0` — high 16 bits must be zero |
| `062762` | `125110` → `063072`=`063120` | **error exit** |
| `062763-062764` | `146115 004441` | `SURSZ (,B 41) := low 16 bits` |
| `063023` | `171412` | `SAX 12` — X := 10 decimal (2^10 = 1024) |
| `063036-063041` | `044441 171001 143056 125031` | `SURSZ <= 1` → **error exit** |
| `063042-063046` | `175005 124004 173777 156577 124374` | `while (A & 1)==0 { X--; A >>= 1 }` |
| `063047-063051` | `171001 140065 125021` | `A != 1` after shifting → **error exit** (not a power of two) |
| `063052` | `146671` | `D := D - X` = log2(record size) |
| `063053-063055` | `143007 124002 146107` | if `X < 0` then `X := 0` |
| `063120-063121` | `171002 125054` | the common error exit: `T := 2`, jump away |

Named by the recovered NPL (`IP-P2-SCSI-DISK.NPL`, listing `062354`-`062522`):

```
062354   IF SUTYP BIT 5SCDA THEN
062357      T:=SMBP1; X:=SMBP2+SINBS; *LDDTX 20
062363      IF A><0 GO FAR RSZER              % TO BIG
062365      A:=D=:SURSZ                       % DIRECT ACCESS DEVICE SIZE
...
062422   IF SUTYP BIT 5SCDA THEN
062425      X:=12                             % RECORD SIZE 1024 BYTES
...
062434   IF X=:D>1 THEN
062440      IF SURSZ<=1 GO FAR RSZER
062444      DO WHILE A NBIT 0 ; X-1; A SHZ -1 ; OD
062451      IF A><1 GO FAR RSZER              % NOT POWER OF 2
062454   FI
062454   D-X; IF X<0 THEN X:=0 FI             % BYTE AND RECORD SHIFT
...
062522   RSZER: T:=ILRCS; GO FAR ERREX        % ILLEGAL RECORD SIZE
```

**Answer to "any hardcoded size or addressing limit":**

* **Block/record size — YES, a hard limit.** The `READ CAPACITY` block length
  must (a) fit in 16 bits (`IF A><0 GO FAR RSZER  % TO BIG`), (b) be `> 1`, and
  (c) be an exact **power of two**. Otherwise the unit is rejected with
  `ILRCS` ("illegal record size"). SINTRAN's own record is `2^12B = 2^10 =
  1024` bytes and the shift `12B - log2(size)` converts between them; a size
  larger than 1024 clamps the shift to 0 at `063053`. **[VERIFIED]**
* **Capacity — NO limit, and no table.** The capacity is a **32-bit double**
  (`UHLIM`, `FILSYS-SYMBOLS L07: UHLIM=177776` = field `,B -2`; stored with a
  *double* store `057742 020776 STD ,B -2`). The bounds check in `SCSDISK`
  (`057352-057362`) is a 32-bit `COMPD` of `ABPA2 + ABP32` against `UHLIM`, with
  `BADPA (T=5)` on overflow. **[VERIFIED]**
* **LBA addressing — 32-bit, with automatic 6→10-byte CDB promotion.**
  Carved in `CACOB`: `064102 171037 SAT 37` / `064103 143456 SKP IF DT MLST SA`
  / `064105 174354 BSET ONE 150 DL`, and the buffer-store fork at
  `064110 175354 BSKP ONE 150 DL` → `064112 143316 STDTX` (4-byte LBA) versus
  `064117 145454 RORA SA DL` (packed 6-byte CDB). Named by NPL `063504`
  `IF A>>37 THEN L BONE 15 FI % 10 BYTE FORMAT NECESSARY` and `063426`
  `IF A><0 OR D>>377 OR D=0 THEN L BONE 15 % 10 BYTES FORMAT NECESSARY`.
  **[VERIFIED]**

### 4.4 Where the disc size actually comes from **[VERIFIED]**

`READ CAPACITY` gives the *last block address*, which the disk layer uses to
locate the **control record on the last block**. `FINEX` (`057655B`) then:

| addr | word | meaning |
|---|---|---|
| `057673` | `143300` | `LDATX` — control-record header |
| `057674-057675` | `156570 004775` | `NPART := header >> 8`, stored to `,B -3` (`NPART=177775`) |
| `057702-057706` | `143300 145051 173401 040347 124374` | XOR checksum loop over the record |
| `057707-057710` | `140001 124007` | checksum `!= 0` → `NOCRC` |
| `057711-057713` | `171002 143046 124004` | `NPART <= 2` → `NOCRC` |
| `057714-057716` | `171012 143046 124003` | `NPART > 012B` (= `NCOPA=000012`) → `NOCRC` |
| `057732` | `143110` | `MOVEW` — copy the partition table to the caller |
| `057741-057742` | `143316 020776` | `STDTX` then **`STD ,B -2` → `UHLIM` (32-bit)** |
| `057743-057744` | `170436 143304` | status `36B` to the caller |
| `057747` | `146106` | `T := 0` (success) |

**So a SCSI disc's size and partitioning are self-describing, held in a control
record on the disc's own last block, protected by an XOR checksum, and the only
structural constraint is `2 < NPART <= 10` partitions** (`NCOPA = 000012` in
`FILSYS-SYMBOLS`). There is **no kernel table of legal SCSI disc sizes**.
**[VERIFIED]**

### 4.5 How many SCSI units a generated system supports **[VERIFIED counts, INFERRED meaning]**

Scanning the carved DPIT segments for the SCSI hardware device number
`144300B` yields regularly-spaced datafield records:

| version | file | unit-datafield addresses | stride | count |
|---|---|---|---|---|
| K-VSX-500 | `044-S3IDPIT.bin` | `013242, 013470, 013716, 014144, 014372, 014620, 015046, 015274, 015522` | `226B` | **9** |
| L-VSX-500 | `044-S3IDPIT.bin` | `036437, 036642, 037045, 037250, 037453, 037656, 040061, 040264, 040467, 040672` | `203B` | **10** |
| M-VSX-500 | `044-S3IDPIT.bin` | `043055, 043260, 043463, 043666` | `203B` | **4** |

Plus, in each, one further record whose neighbour word is the SCSI driver's
interrupt entry (L: `…144300 56621 67247` — `67247B` is `SCINT`, the carved SCSI
interrupt handler), i.e. the **controller** datafield as opposed to a **unit**
datafield. **[VERIFIED addresses/words; INFERRED that these are per-unit
datafields and therefore that the number of configured SCSI units differs per
generated system: 9 / 10 / 4.]** This is a *generation* parameter, not a
kernel-imposed maximum; no maximum-unit constant was found.

---

## 5. `@CREATE-DIRECTORY`, `ALBIT`, and the bit file

Carved from `versions\L-VSX-500\segments\006-S3FS.bin` (load base `26000B`);
disassembly `re\segments-ref\006-S3FS\006-S3FS.asm`.
`ALBIT = 137500B`, `CRDIR = 136741B` (`SYMBOLS\L07\FILSYS-SYMBOLS.SYMB.TXT:1314`).

* **Bit-file default placement [VERIFIED]** — `137523-137532`:
  `LDT ,B 1` / `JPL I 63`→`GSIZE (037101B)` / `156777 SAD ZIN SHR 1` /
  `171011 SAT 11` / `141660 RDIV ST` / `141265 RMPY ST DA` / `020410 STD ,B 10`
  ⇒ `bit_start = 9 * floor(floor(pages/2)/9)`. The **9 is a hard literal**
  (`SAT 11`), identical for every device — it is *not* read from the
  disc-layout table.
* **Bit-file span [VERIFIED words, INFERRED arithmetic]** — `137535-137544`:
  `GSIZE` then `156602 SAD ZIN 2` (32-bit shift left 2) then `172777 AAA -1`
  ⇒ `ceil(pages / 16384)` bitmap pages, i.e. **16384 bits per page = 1 bit per
  page over a 1024-word page**.
* **"Place at top of disc"** — a supplied 32-bit bit address of `-1`
  (`137545-137562`) makes `ALBIT` compute `pages - bitmap_pages`. **[VERIFIED
  words, INFERRED meaning]**
* **The only size check [VERIFIED]** — `137563-137575`: 32-bit compare of
  `bit_start` against the declared page count; on failure `170500 SAA 100`
  (error code `100B`) and `124123 JMP` to the error return.
* **NO MAXIMUM DISC SIZE IS ENFORCED.** An exhaustive read of `ALBIT`
  (`137500B..137730B`) and `CRDIR` (`136741B..137477B`) finds no comparison of
  the page count against any ceiling constant. **[VERIFIED by exhaustive read]**
  The only implicit ceiling is structural: the bitmap-page count is kept in the
  16-bit `A` register (`137544 004421 STA ,B 21`) after a 32-bit `<<2`, which
  overflows at `pages >= 2^30`. **[INFERRED]**
* **`@CREATE-DIRECTORY` does NOT write a disc boot area [VERIFIED for the call,
  INFERRED for the byte range]** — `CRDIR` reserves the device with
  `137046 153124 MON 124`, calls `ALBIT` (`137145`, pointer word
  `137234 = 137500`), `GSIZE` (`137153` → `037101`), `WBFBU` (`137167` →
  `050565`), `ALPAG` (`137324` → `050627`), then writes the directory label /
  master block through **`WDIRS` at `137403` (pointer `137440 = 040221`)** and
  releases with `137417 153125 MON 125`. There is no bootstrap-area write.

**Correction to existing repo docs:** `SINTRAN\Filesystem\create-directory-placement.md`
(≈ lines 49, 104, 182) and `create-directory.md` §4 read `137710B MPY 20` as
"multiply by 16 = 16 bits/word". That is wrong: `MPY` (`120000B`) is a
memory-reference instruction with no immediate form; `20` is a P-relative
displacement and **both** `MPY` sites (`137701B MPY 27` and `137710B MPY 20`)
fetch the same constant word `000011B = 9` at `137730B`. They belong to the
bad-page **relocation** (± one 9-page track), not to bitmap sizing. The
16384-bits-per-page conclusion is still correct but comes from
`137540 156602 SAD ZIN 2`. **[VERIFIED]**

---

## 6. Installation artefacts that survive inside the running system

### 6.1 The command-name table of the command processor **[VERIFIED]**

`versions\L-VSX-500\segments\003-S3CP.bin`, load base `30000B`. The
`'`-terminated ASCII name table runs **`054131B` … `060230B`**, immediately
followed by a parameter-descriptor pointer table at `060231B`. Confirmed against
`re\segments-ref\003-S3CP\003-S3CP.asm:11217`:

```
054131  025102  	LDD I 102        ; 0x2A42 = '*','B'  -> "*BACKUP-DIRECTORY"
054132  040503                     ; 0x4143 = 'A','C'
```

Disc / directory / mass-storage commands, octal addresses in L:

| addr | commands |
|---|---|
| `054131` | `*BACKUP-DIRECTORY` `*COPY-USERS-FILES` `*CREATE-VOLUME` |
| `054163` / `054172` | `*LIST-VOLUME` / `*RETRIEVE-DIRECTORY` |
| `054352` | `CHANGE-BIT-FILE` `CHANGE-DIRECTORY-ENTRY` |
| `054476` / `054543` | `CLEAR-MAIN-DIRECTORY` / **`COLD-START`** |
| `054573` / `054611` | `COPY-DEVICE` `COPY-DIRECTORY` / `COPY-FILE` **`CREATE-DIRECTORY`** |
| **`054777`** | **`DEFINE-MASS-STORAGE-UNIT`** |
| `055135` | `DELETE-FILE` … **`DELETE-MASS-STORAGE-UNIT`** |
| `055250` | **`DEVICE-FUNCTION`** `DIRECTORY-STATISTICS` |
| `055331` / `055337` | `DSCNT` **`DUMP`** / `DUMP-BIT-FILE` `DUMP-DIRECTORY-ENTRY` |
| `055447` | … **`ENTER-DIRECTORY`** `EXECUTE-IOX` `EXPAND-FILE` |
| `056103` | … **`LIST-MASS-STORAGE-UNITS`** … |
| `056332` / `056373` | **`LOAD-BINARY`** … / `MEMORY-LIMITS` **`MODE`** |
| `056533` | `PRSRV` `RECOVER` **`REGENERATE-DIRECTORY`** |
| `056653` | … **`RESERVE-DEVICE-UNIT`** `RESERVE-DIRECTORY` |
| `057055` / `057376` | **`SAVE-DIRECTORY`** / **`SET-MAIN-DIRECTORY`** |
| **`057410`** | **`SET-MASS-STORAGE-SIZE`** `SET-MEMORY-CONTENTS` |
| `060046` / `060136` / `060152` | **`TEST-DIRECTORY`** / **`UNLOCK-DIRECTORY`** / **`UNRESERVE-DIRECTORY`** |

M's table is the same, sorted, at `061770B`…`066022B` (`DEFINE-MASS-STORAGE-UNIT`
`062636B`, `LIST-MASS-STORAGE-UNITS` `063753B`, `SET-MASS-STORAGE-SIZE`
`065260B`). **K's table is in definition order, not sorted**, observed extent
`047753B`…`053723B`; K's exact start address is **NOT FOUND**.
**[VERIFIED except where marked]**

The `S3CP` table entry that dispatches `@CREATE-DIRECTORY` to `CRDIR=136741B`
(the pointer, as opposed to the name string at `054611B`) was **NOT FOUND**.

### 6.2 The `DEVICE-FUNCTION` subcommand table **[VERIFIED]**

L `003-S3CP.bin`, `116734B`…`117406B`. Disc-relevant subcommands:
`CLEAR-DEVICE`, `READ-STATUS`, `READ-FORMAT`, `CLEAR-SELECTED-UNIT`,
`RESERVE-DEVICE`, `RELEASE-DEVICE`, `SET-FLOPPY-FORMAT`, `FORMAT-FLOPPY`,
**`GET-CURRENT-DISC-ADDRESS`**, **`DUMP-BOOTSTRAP`**,
**`SET-CURRENT-DISC-ADDRESS`**.

Version deltas: K has **no** `RESERVE-DEVICE`/`RELEASE-DEVICE` here;
**M adds `FORMAT-TRACK`** at `125622B`. **[VERIFIED]**

`DUMP-BOOTSTRAP` — the subcommand that writes the boot block to the disc —
is at K `153367B`, L `117361B`, M `125575B`. **[VERIFIED]** This is the
mechanism that writes the disc boot area; `@CREATE-DIRECTORY` does not (§5).

### 6.3 The canned cold-start `ENTER-DIRECTORY` line **[VERIFIED]**

Present verbatim in all three versions, twice each:

| version | file | octal addr | text |
|---|---|---|---|
| K | `003-S3COM.bin` | `067171` | `ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0'` |
| K | `005-S3ERRS.bin` | `130740` | same |
| L | `003-S3CP.bin` | `074123` | same |
| L | `005-S3ERRS.bin` | `144744` | same |
| M | `003-S3CP.bin` | `102325` | same |
| M | `005-S3ERRS.bin` | `144744` | same |

Followed in `005-S3ERRS` by `SC-75MB-1'`, `SYMB'`, `DATA'`, `MODE'`,
`(SCRATCH)SCRATCH01'`, `SYSTEM'`. **[VERIFIED bytes; INFERRED that this is the
built-in default main-directory entry command plus default file types.]**
Note `DISC-75MB-1` is exactly a name in the device-name table of §2A
(`164021B`, 36945 pages).

### 6.4 The `@CREATE-DIRECTORY` / `@DEFINE-MASS-STORAGE-UNIT` dialogue **[VERIFIED]**

`006-S3FS.bin` (base `26000B`), prompt table from `043220B`:

```
043220  SA DIRECTORY NAME: '
043233  SA OLD DIRECTORY NAME: '
043247  SA NEW DIRECTORY NAME: '
043263  SA DEVICE NAME: ' ID DEVICE UNIT: ' ID DEVICE SUB-UNIT: ' SA FIXED(F) OR REMOVABLE(R): '
043334  SU USER NAME: ' ID NUMBER OF PAGES: ' ...
043535  IB DEVICE NUMBER (OCT): '
046135  IB OCTAL BIT FILE ADDRESS (-1 END OF DISC): ' SA MANUAL CHECK? '
046174  SA VOLUME NAME: '
153331  MASTER BLOCK'   BIT FILE'
```

`SA DEVICE NAME:` is the prompt that is matched against the §2A device-name
table; `ID NUMBER OF PAGES:` is the operator-supplied size that reaches `GSIZE`
and thence `ALBIT` (§5); `IB OCTAL BIT FILE ADDRESS (-1 END OF DISC)` is
exactly the `-1` case carved at `ALBIT 137545B`. **[VERIFIED strings; the
linkage to `ALBIT` is VERIFIED at the `-1` site, INFERRED for the others.]**

### 6.5 Cold-start MODE-file machinery **[VERIFIED]**

`070-S3SSM.bin` / `071-S3SM.bin` (L): `SET-COLDSTART-MODE-FILE'`
`RESET-COLDSTART-MODE-FILE'` at word offset `014063B`, and at `064273B`:
`PARAMETERS TO THE ENTER-DIRECTORY COMMAND'`
`$WHEN ENTERING MAIN DIRECTORY: '` `S COLDSTART INPUT FILE: '`
`S COLDSTART OUTPUT FILE: '`. In K these live inside the merged `003-S3COM` /
`013-S3OPCSV`.

### 6.6 Library marks — the system-generation record of each build **[VERIFIED]**

`SYMBOLS\{K03,L07,M06}\LIBRARY-MARKS.SYMB.TXT` are the conditional-assembly
switches of each build (K 687, L 732, M 914 marks). Example raw line
(`L07`, line 539): `176000/^8SCSI   %000004`.

| mark | K03 | L07 | M06 | [INFERRED] meaning |
|---|---|---|---|---|
| `8SCSI`, `8SCS1`, `8ZSCS` | Y | Y | Y | SCSI support / controller 1 |
| `8SCS2` | — | **Y** | — | SCSI controller 2 |
| `8SCOD`, `8SCMT` | — | **Y** | **Y** | SCSI optical disc / SCSI mag tape |
| `8BDIS`, `8ZBDI` | Y | Y | Y | big disc (SMD) driver |
| `8BDIO`, `8WDIS`, `8ZWDI` | — | **Y** | **Y** | BDIO pool / Winchester disc |
| `7D1U0`…`7D8U0` | only `7D1U0` | all 8 | `7D1U0`,`7D2U0` | SCSI disc unit marks |
| `7O1U0`,`7O2U0` | — | Y | Y | optical units |
| `WM300`, `WM310` | **Y** | — | — | K only |

`8SCS2` existing only in L matches symbol `SCSI2` existing only in L07 —
**this particular L machine was generated with two SCSI controllers; the K and
M machines with one.** **[VERIFIED]**

### 6.7 Version deltas that matter for disc support **[VERIFIED]**

1. Device-name table moved out of `006-S3FS` into its own segment pair
   `100/101-S3*DNAM` at K→L.
2. **SCSI is already fully present in K**: `DISC-SCSI-*` names at `165556B` in
   K's `006-S3FS`, marks `8SCSI`/`8SCS1`/`8ZSCS`, symbols `SCSI1`/`SCSID`,
   `DTSSS` disc-layout record at `012546B`, `DISPE[36B]` non-zero.
3. K's carved `DISPE` has **no** ST-506/Winchester types (indices 10-15 zero);
   L and M have all six. Marks `8WDIS`/`8ZWDI` are absent from K03 accordingly.
4. `DTOD1`/`DTOD2` (optical) are absent from K03, present in L07/M06 — matching
   the carved `DISPE[40B]`/`DISPE[41B]` being zero in K and non-zero in L/M.
5. `RESERVE-DEVICE`/`RELEASE-DEVICE` added to `DEVICE-FUNCTION` in L;
   `FORMAT-TRACK` added in M.
6. Command-name table becomes alphabetically sorted from L onward.

### 6.8 Things explicitly NOT FOUND in the strings sweep

* No error text about a disc being too large. The nearest are
  `DIRECTORY INDEX TOO LARGE` / `OBJECT INDEX TOO LARGE`
  (L `014-S3ERRP.bin`, word offset `012423B`).
* No `WINCHESTER` string anywhere in any of the three systems.
* No standalone `SCSI` / `SMD` / `ND-3201` device-type word — the only SCSI
  text in the images is inside the `DISC-n-SCSI-m` device names.
* `MODE` is an ordinary command (L `056373B`), not an install-only `:MODE`
  construct.
* M-VSX-500 has no `list-segment.txt` / `list-rt-programs.txt` in `inputs\`.
* No RT program with a disc/SCSI name in either K's or L's
  `list-rt-programs.txt`; the mass-storage-adjacent ones are `1SWAP`, `5SWAP`,
  `RTDIL`, `RTRFA`, `DIMWD` (L only).

---

## 7. `MSTYP` — the open question is NOT resolved

The cross-reference asked whether `MSTYP`'s value could be recovered from the
carved symbol tables.

**NOT FOUND.** Exhaustive grep of

* `SINTRAN\NPL-SOURCE\SYMBOLS\K03\*.SYMB.TXT`
* `SINTRAN\NPL-SOURCE\SYMBOLS\L07\*.SYMB.TXT`
* `SINTRAN\NPL-SOURCE\SYMBOLS\M06\*.SYMB.TXT`

for `MSTYP`, and for every symbol beginning `MST`, returns only
`MSTAT, MSTEN, MSTMP, MSTOR, MSTPN, MSTRM, MSTRN, MSTS, MSTTY, MSTUS` —
**no `MSTYP`**. The only near-hit anywhere is `CMSTY` (L07
`SYMBOL-2-LIST.SYMB.TXT:2960 CMSTY=120462`, M06 `:2862 CMSTY=121200`), which the
NPL listing (`s3vs-4.symb:38028 SUBR CSTYP,CMSTYP`) shows is an unrelated
routine. **[VERIFIED negative]**

Since MAC symbol significance is the last 5 characters and `MSTYP` is exactly 5,
truncation cannot be hiding it. **[INFERRED]**

The documented `Drum=0 / NCR=1 / CDC=2 / Large disc=3` numbering is therefore
**neither verified nor refuted** by the carve. What the carve *does* show is
that the running kernel does not use a value of that shape at all: it uses
**`SWTYP` in the range 7..36B** indexing `MDISCS` and `DISPE` (§2, §2.1). Those
two numbering schemes are not the same and should not be equated without
further evidence. **[INFERRED]**

---

## 8. What is still uncarved

| Gap | What would have to be carved |
|---|---|
| K-VSX-500 `MDISCS` / `WWDIS` / `BBDIS` / `SCDIS` | Re-carve the resident common code for the K05 image over the full resident extent (`SINTRAN:DATA` pages 1..63 of the K05 SMD image) into `versions\K-VSX-500\resident\SINTRAN-DATA_commoncode.bin`; the present K carve does not contain the array. |
| `SINTR` cold-start code as carved bytes | `PH-P2-OPPSTART` lives on the **start segment**; segment `0002 S3IMAGE` (load base 0, `madr` page 0, 63 pages) is carved but has no `segments-ref` disassembly. Produce `segments-ref\002-S3IMAGE` to byte-verify §3.2 rather than relying on the NPL listing. |
| `@CREATE-DIRECTORY` command-table **dispatch pointer** | `003-S3CP` (load base `30000B`). The *name* table is now located (`054131B`-`060230B`, §6.1) and the parameter-descriptor pointer table starts at `060231B`; the routine-address table that maps `CREATE-DIRECTORY` → `CRDIR 136741B` still has to be walked out from `060231B`. |
| K-VSX-500 command-name table start | K's `003-S3COM` table is unsorted and the run found begins mid-word at `047753B`; a few words earlier is the true start. Disassemble `K-VSX-500\segments\003-S3COM.bin` around `047740B` to pin it. |
| Meaning of device-name-table words 12/13 (`BABST=030346`, `000515`) | Not decoded. They are constant per device class. |

No source image was modified; every read in this analysis was read-only.

---

## 9. Provenance

* Carved binaries: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\{K,L,M}-VSX-500\segments\*.bin`, `…\resident\*.bin`
* Disassemblies: `…\versions\L-VSX-500\re\segments-ref\065-S3SIPIT\065-S3SIPIT.asm`, `…\006-S3FS\006-S3FS.asm`
* Prior byte-verified carves relied on: `…\versions\L-VSX-500\re\kernel-carving\SCSI-DISKLAYER-COMPLETE\README.md`, `…\SCSI-DRIVER-COMPLETE\`, `…\SMD-DRIVER-BASELINE\README.md`
* Symbol tables: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\{K03,L07,M06}\*.SYMB.TXT`
* Recovered source listings (naming/comments only, always cross-checked): `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb`, `…\NPL\PH-P2-OPPSTART.NPL`, `…\NPL\IP-P2-DISK-START.NPL`, `…\NPL\IP-P2-SCSI-DISK.NPL`
* Device-name table: `…\versions\L-VSX-500\segments\101-S3DNAM.bin` (+ `100-S3SDNAM.bin`), `…\M-VSX-500\segments\101-S3DNAM.bin`, `…\K-VSX-500\segments\006-S3FS.bin`
* Command tables / strings: `…\segments\003-S3CP.bin` (K: `003-S3COM.bin`), `005-S3ERRS.bin`, `006-S3FS.bin`, `014-S3ERRP.bin`, `070-S3SSM.bin` / `071-S3SM.bin`, `037-S3RTD.bin`
* Generation switches: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\{K03,L07,M06}\LIBRARY-MARKS.SYMB.TXT`
* Live listings: `…\versions\{K,L}-VSX-500\inputs\list-segment.txt`, `list-rt-programs.txt`
* Tool: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\find_disc_layout_table.py`

No file under `D:\ND\`, no `.img`/`.image`, and no carved binary was modified.
Nothing was written outside `E:\Dev\Ronny\NDInsight\tools\boot-floppy\` and the
session scratchpad. `INSTALL-PROCEDURE.md` was not touched.
