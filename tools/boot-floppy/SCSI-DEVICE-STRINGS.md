# SCSI VENDOR / PRODUCT identification strings in Norsk Data software

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\SCSI-DEVICE-STRINGS.md`

Companion tool written for this analysis (handles 7-bit / parity-set / byte-swapped
encodings, re-runnable on new media):
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\find_nd_strings.py`

Every statement below is tagged **[VERIFIED]** (exact file, byte offset, quoted
bytes) or **[INFERRED]** (reasoning shown). Where nothing was found the document
says **NOT FOUND**. The ndwiki "SCSI-TV" article was treated as a *hypothesis*,
not as evidence; the tables below are decoded from the binaries.

---

## 0. Executive summary

| Question | Answer |
|---|---|
| Do hard-coded SCSI VENDOR / PRODUCT strings exist in ND software? | **YES** — but **not** in SINTRAN III and **not** in MACM. |
| Where? | In the ND **stand-alone diagnostic programs** `SCSI-TV` ("SCSI Test and Verify") and `DISK-MM` ("DISK Media Maintenance"), which ship on the ND-210523 test diskettes and run under `TPE-MON`. Also as an installed file-system copy on one hard-disk image, `D:\ND\HDD\nd-test_ny.img` (§5.5). **[VERIFIED]** |
| Is it a table? | **YES — two linked tables**: a VENDOR table (8-char ASCII + code + pointer + count) and a PRODUCT table (16-char ASCII + code). The vendor record points into the product table and says how many products that vendor has. Layout byte-verified, §2. |
| Is it a whitelist or cosmetic? | **A whitelist.** Both programs carry distinct operator-facing errors for a failed lookup: `(CS) Disk drive vendor unknown to the program` / `(CS) Disk drive is unknown to the program` (DISK-MM) and `Unknown vendor` / `Unknown product` / `This test is not applicable for the selected drive` / `Unknown drive type in test` (SCSI-TV). **[VERIFIED as strings]** Whether the program hard-aborts or only skips a test was **not** determined — no disassembly was done (§4). |
| Does the SINTRAN III kernel contain them? | **NO — NOT FOUND** in any carved K/L/M segment, resident common code, symbol list or NPL source, in any of the four encodings searched. The kernel's SCSI `INQUIRY` requests only **8 bytes**, so it never even receives the vendor field (bytes 8-15) or product field (bytes 16-31). §5. |
| Does MACM contain them? | **NO — NOT FOUND** in `D:\ND\BPUN\MACM-1718L.BPUN`, `MACM-1718K.BPUN`, or the MACM copies on the K/L/M generation diskettes. §5. |
| Hardware in the binaries but **NOT** on the wiki list | **EXABYTE EXB-8200 / EXB-8500**, **HP 88780**, **CDC 94161-156 / 94171-9 / 94171-7 / 94171-5 / 94181-15 / 94181-13**, **EMD 97201 (000)** and **(1.2)**, extra ARCHIVE VIPER serial variants **99999 / 25066 / 21835**, ` TDC 3600 EXPR.` and `RG TDC 3600`, plus the vendor aliases `MICROP`, `CDC`, `NDARCHIV`, `ND ARCHV`, `NDTANDBE`, `NDEXABYT`, `NDEXABY`, `NDHP`. §3. |

---

## 1. Where the tables live **[VERIFIED]**

The strings occur in exactly two ND program families, on the ND-210523
("SCSI test") diskettes. Everything is plain 7-bit ASCII stored big-endian —
**not** parity-set, **not** byte-swapped.

| diskette image | program file | version banner (byte offset) |
|---|---|---|
| `D:\ND\S\210523G02-XX-02D.image` | `SCSI-TV-B00.TEST` | `SCSI Test and Verify - Version : B00 - 1988-06-15` @ 531 |
| `D:\ND\S\210523G02-XX-02D.image` | `DISK-MM-B00.TEST` | `DISK Media Maintenance - Version : B00 - 1988-06-09` @ 541 |
| `D:\ND\S\210523H00-XX-02D.image` | `SCSI-TV-C00.TEST` | `SCSI Test and Verify - Version: C00 - 1988-12-08` @ 531 |
| `D:\ND\S\210523H00-XX-02D.image` | `DISK-MM-B01.TEST` | `DISK Media Maintenance - Version : B01 - 1988-11-14` @ 541 |
| `D:\ND\S\Nd-210523I01-XX-02D.img` | `SCSI-TV-C04.TEST` | `SCSI Test and Verify - Version: C04 - 1990-06-11` @ 2991 |
| `D:\ND\S\Nd-210523I01-XX-02D.img` | `DISK-MM-C03.TEST` | `DISK Media Maintenance - Version: C03 - 1990-06-11` @ 31237 |
| `D:\ND\S\210523G02-XX-02D.image-org` | identical copy of the G02 diskette | — |

The B00 program's overlays `SCSI-TV-OVL2/OVL3/OVL4-B00.NEXT` each contain a
**second, identical copy** of both tables (`SCSI-TV-OVL2-B00.NEXT` @ 26232
etc.), because a TPE overlay is a relinked image of the whole program.
**[VERIFIED]**

Files were extracted read-only with
`ndtool -x -o <dir> <image>` (no `-p`). Raw-image offsets of the tables inside
`210523G02-XX-02D.image` are 681032, 719372, 761266, 873602, 885700 etc.
(one per copy of the program on the diskette) — the per-file offsets below are
the authoritative ones. **[VERIFIED]**

### Not on the wiki list: `ND-DMM_1988-09-30.image`

`D:\ND\S\ND-DMM_1988-09-30.image` offset 1185333 (0o4413065) carries the
free-text help line **[VERIFIED]**:

```
    Micropolis 1375 : 1024
    CDC EMD 97201   :  512
    CDC WREN IV     :  512
```

`CDC WREN IV` appears nowhere else and is **not** in the wiki list.
**[INFERRED]** these are the drives' native block sizes in bytes.

---

## 2. Record layout, byte-verified

Anchor example: `SCSI-TV-B00.TEST` (from `210523G02-XX-02D.image`).

### 2.1 PRODUCT table — 18 bytes per record: 16 ASCII + 2-byte big-endian code

Starts at file byte **17290** (0o41612). Raw bytes of the first two records:

```
4c 44 20 31 32 30 30 20 53 43 53 49 20 20 20 20   "LD 1200 SCSI    "
00 02                                             code = 2
31 33 37 35 20 20 20 20 20 20 20 20 20 20 20 20   "1375            "
00 00                                             code = 0
```

**[VERIFIED]**

### 2.2 VENDOR table — 16 bytes per record: 8 ASCII + code(2) + word-pointer(2) + (count-1)(4)

Starts at file byte **17488** (0o42120), 10 records, ends at 17648.
Raw bytes of the first two records:

```
4e 44 4d 49 43 52 4f 50  00 00  8c ce  00 00 00 00   "NDMICROP" code 0 ptr 0x8cce n-1=0
4d 49 43 52 4f 50 20 20  00 00  8c ce  00 00 00 00   "MICROP  " code 0 ptr 0x8cce n-1=0
```

**[VERIFIED]**

### 2.3 The pointer is a **word** address; the mapping is exact **[VERIFIED]**

`byte_offset = 2 × word_address − 54784`, i.e. **file byte 0 loads at word
`0o65400` (27392 = 0x6B00)**. All ten vendor pointers land exactly on a product
record start under this one constant — that is the proof, not an assumption:

| vendor | ptr | 2·ptr−54784 | product record found there |
|---|---|---|---|
| `OSI     ` | 0x8cc5 | 17290 | `LD 1200 SCSI    ` |
| `NDMICROP` | 0x8cce | 17308 | `1375            ` |
| `CDC     ` | 0x8cd7 | 17326 | `94161-156       ` |
| `TANDBERG` | 0x8cf2 | 17380 | ` TDC 3600       ` |
| `ARCHIVE ` | 0x8d0d | 17434 | `VIPER 150  21247` |
| `NDSTK   ` | 0x8d16 | 17452 | `2925            ` |
| `EXABYTE ` | 0x8d1f | 17470 | `EXB-8200        ` |

The 4-byte trailing field is **(number of products − 1)**: `CDC` and `TANDBERG`
carry 2, and each of them owns exactly 3 consecutive 18-byte product records;
every other vendor carries 0 and owns 1. **[VERIFIED by the arithmetic above —
the pointer deltas 9 / 27 words are exactly 1 / 3 records.]**

### 2.4 The two code fields

* The vendor `code` is a small **vendor index** (0…7).
* The product `code` is a small **model index**.

They are *different* enumerations — `OSI` has vendor code 4 while its product
`LD 1200 SCSI` has product code 2 — and the product enumeration is **not stable
across versions** (`94171-9` is code 7 in `SCSI-TV-C00` but code 8 in
`SCSI-TV-C04`; `88780` is 8 in C00 and 10 in C04). **[VERIFIED by comparison of
the dumps in §3.]** What each index selects (test parameters? defect-list
format?) was **not** determined — **NOT FOUND** in the strings, and no
disassembly was done.

**No device-type byte and no Direct/Sequential/Write-Once field exists in these
records.** The 16-byte vendor record and 18-byte product record are fully
accounted for by the fields above. The wiki's "Device type" column has **no
counterpart in the table** — the programs get the peripheral device type from
the live `INQUIRY` reply instead (SCSI-TV prints it in its bus listing, §4.1).
**[VERIFIED by exhaustive field accounting]**

---

## 3. The tables, decoded, per version **[VERIFIED]**

### 3.1 `SCSI-TV-B00.TEST` (1988-06-15) — 10 vendor records, 11 product records

| vendor (byte) | vcode | product (byte) | pcode |
|---|---|---|---|
| `NDMICROP` 17488 | 0 | `1375            ` 17308 | 0 |
| `MICROP  ` 17504 | 0 | *(same)* | |
| `CDC     ` 17520 | 1 | `94161-156       ` 17326<br>`EMD 97201 (736) ` 17344<br>`EMD 97201 (368) ` 17362 | 7<br>1<br>1 |
| `NDCDC   ` 17536 | 1 | *(same three)* | |
| `ARCHIVE ` 17552 | 3 | `VIPER 150  21247` 17434 | 3 |
| `TANDBERG` 17568 | 2 | ` TDC 3600       ` 17380<br>` TDC 3600 EXPR. ` 17398<br>`RG TDC 3600     ` 17416 | 4<br>4<br>4 |
| `NDTANDBE` 17584 | 2 | *(same three)* | |
| `OSI     ` 17600 | 4 | `LD 1200 SCSI    ` 17290 | 2 |
| `NDSTK   ` 17616 | 5 | `2925            ` 17452 | 5 |
| `EXABYTE ` 17632 | 6 | `EXB-8200        ` 17470 | 6 |

### 3.2 `SCSI-TV-C00.TEST` (1988-12-08) — 16 vendor records, 13 product records

Vendor table 17516…17772, product table 17264…  (base 54784, same mapping).

| vendor | vcode | products |
|---|---|---|
| `NDMICROP`, `MICROP  ` | 0 | `1375            ` (0) |
| `CDC     `, `NDCDC   ` | 1 | `94171-9         ` (7), `EMD 97201 (736) ` (1), `EMD 97201 (368) ` (1) |
| `TANDBERG`, `NDTANDBE` | 2 | ` TDC 3600       ` (4), `RG TDC 3600     ` (4), ` TDC 3600 EXPR. ` (4) |
| `ARCHIVE `, `NDARCHIV`, `ND ARCHV` | 3 | `VIPER 150  99999` (3), `VIPER 150  21835` (3), `VIPER 150  21247` (3) |
| `OSI     ` | 4 | `LD 1200 SCSI    ` (2) |
| `NDSTK   ` | 5 | `2925            ` (5) |
| `NDEXABYT`, `NDEXABY `, `EXABYTE ` | 6 | `EXB-8200        ` (6) |
| `HP      `, `NDHP    ` | 7 | `88780           ` (8) |

**Deltas from B00:** `94161-156` replaced by `94171-9`; **HP 88780 added**;
two more ARCHIVE VIPER serial variants added; six vendor aliases added.
**[VERIFIED]**

### 3.3 `SCSI-TV-C04.TEST` (1990-06-11) — 17 product records; **vendor record shrinks to 14 bytes**

Product table at file byte **54334**, vendor table at **54640**. In C04 the
vendor record has **no vcode field**: it is `8 ASCII + wordptr(2) + count-1(4)`
= 14 bytes. Raw: `4e 44 4d 49 43 52 4f 50 d5 28 00 00 00 00`. The word→byte
base is still **54784** and every pointer again lands on a product record.
**[VERIFIED]**

| vendor | products (pcode) |
|---|---|
| `NDMICROP`, `MICROP  ` | `1375            ` (0) |
| `CDC     `, `NDCDC   ` | `94171-9         ` (8), `EMD 97201 (736) ` (1), `EMD 97201 (368) ` (1), `94181-15        ` (9) |
| `ARCHIVE `, `NDARCHIV`, `ND ARCHV` | `VIPER 150  99999` (3), `VIPER 150  25066` (3), `VIPER 150  21835` (3), `VIPER 150  21247` (3) |
| `TANDBERG`, `NDTANDBE` | ` TDC 3600       ` (4), `RG TDC 3600     ` (4), ` TDC 3600 EXPR. ` (4) |
| `OSI     ` | `LD 1200 SCSI    ` (2) |
| `NDSTK   ` | `2925            ` (5) |
| `NDEXABYT`, `NDEXABY `, `EXABYTE ` | `EXB-8200        ` (6), `EXB-8500        ` (6) |
| `HP      `, `NDHP    ` | `88780           ` (10) |

**Deltas from C00:** `EXB-8500` and `94181-15` added, `VIPER 150  25066` added.
**[VERIFIED]**

### 3.4 `DISK-MM` — a **disk-only** table, different enumeration

`DISK-MM-B00.TEST` (1988-06-09): vendor table @ 34660 (16-byte records),
product table @ 34570.

| vendor | vcode | products (pcode) |
|---|---|---|
| `NDMICROP` 34660 | 0 | `1375            ` (0) |
| `NDCDC   ` 34676 | 1 | `EMD 97201 (000) ` (1), `EMD 97201 (368) ` (1), `EMD 97201 (736) ` (1), `94171-9         ` (2) |

`DISK-MM-B01.TEST` (1988-11-14) is identical in content (tables at 34664 /
34574). **[VERIFIED]**

`DISK-MM-C03.TEST` (1990-06-11): 14-byte vendor records @ 45586, product table
@ 45370, **12 products**:

```
1375              (0)
EMD 97201 (000)   (1)   EMD 97201 (368)  (1)   EMD 97201 (736)  (1)   EMD 97201 (1.2)  (1)
94171-9           (2)   94171-7          (2)   94171-5          (2)   94171-           (2)
94181-15          (3)   94181-13         (3)   94181-           (3)
```
`NDMICROP` ptr 0xc39d (1 product), `NDCDC` ptr 0xc3a6 (11 products) — both
verified against base 54784. **[VERIFIED]**

**[INFERRED]** the trailing-blank entries `94171-` and `94181-` are
wildcard/family catch-alls; the record is a fixed-width prefix so a shorter
string matches a whole family. Not proven — no comparison code was read.

---

## 4. What the code does with the table

### 4.1 The bus listing prints the RAW `INQUIRY` reply, not the table **[VERIFIED]**

`SCSI-TV-B00.TEST` offset 5848 / 5878 / 5898:

```
DEVICES CONNECTED TO ADAPTOR
                                     Block        Media
 Scsi  Device    Vendor            Product          size         size
  id    type       id        id              rev    (dec.)       (dec.)
Dir Seq Pri Pro WORMRdO ---
```

So the device-type classes (`Dir` / `Seq` / `Pri` / `Pro` / `WORM` / `RdO`) and
the vendor/product/rev fields shown to the operator come **from the live
INQUIRY data**, not from the table.

### 4.2 The table is a whitelist — failed lookup has its own errors **[VERIFIED as strings]**

`DISK-MM-B00.TEST`, error-message block, byte 45072-45264:

```
45072  (DF) Reservation conflict ( already reserved by another user )(DF) Disk table full(CS) No space in heap
45176  (CS) Disk drive vendor unknown to the program
45222  (CS) Disk drive is unknown to the program
45264  (CS) Error in consistency of partition block (not INITIALIZED ?)(CS) Table index out of range
```
and at 44870: `(VD) Unknown disk type`.
Identical strings at 44874 / 45180 / 45226 in `DISK-MM-B01.TEST`. **[VERIFIED]**

`SCSI-TV-B00.TEST` byte 63708-63902, the `STV-A00` module's error enumeration:

```
No device selected / No space in free area / Unknown vendor / Unknown product /
No memory available / Memory already Used / Element is allocated outside area /
Wrong ident code / No ident code / Driver error return /
Parameters outside range / No information available
```

`SCSI-TV-OVL2-B00.NEXT` byte 2706 (also OVL3 @ 2774, OVL4 @ 2844, and
`SCSI-TV-D0..D3-C00.NEXT`):

```
    Selected SCSI id number is not present
    Unknown product
    Unknown vendor
    This test is not applicable for the selected drive
    Unknown drive type in test
    Errorous data read back from drive
```
**[VERIFIED]**

**[INFERRED]** Two distinct errors — one for the vendor lookup, one for the
product lookup — plus a per-vendor product sub-list is the shape of a
two-level whitelist: match the 8-byte INQUIRY vendor field against the vendor
table, then match the 16-byte product field only against that vendor's product
records. `This test is not applicable for the selected drive` /
`Unknown drive type in test` show the consequence is at least a **test being
refused**.

**NOT DETERMINED:** whether an unmatched drive aborts the program or only
disables some tests. That requires disassembling the ND-100/PLANC code, which
was **not** done here. Structural evidence that a lookup loop exists: in
`SCSI-TV-B00.TEST` the constant `0x8d28` (= word address of the vendor table
start) occurs at byte 17726, and `0x8d78` (= word address of the vendor table
**end**) at 17770 and 17892 — a base/limit pair. **[VERIFIED bytes, INFERRED
meaning.]**

### 4.3 Other operator-facing messages found **[VERIFIED]**

`SCSI-TV-B00.TEST`: `* Unable to detect device type (device not present ?) *`
(byte 1478), `Illegal SCSI type` (15480), `Illegal device record size` (15498),
`Unsupported function` (28354, in the SCSI additional-sense text table),
`Drive not ready` (13970), `Reservation conflict` (15844),
`Unexpected or illegal SCSI status` (in DISK-MM at 41110).

---

## 5. Where the strings are **NOT** — the negative results

All of the following were searched with `find_nd_strings.py` in **four**
encodings — plain 7-bit ASCII, parity-set (`byte | 0x80`), byte-swapped word
pairs, and byte-swapped at odd phase — case-insensitively, for the needles
`NDMICROP`, `MICROP`, `TANDBERG`, `NDTANDBE`, `ARCHIVE`, `VIPER`, `VIPER 150`,
`NDCDC`, `NDSTK`, `EXABYTE`, `97201`, `21247`, `TDC 3600`, `LD 1200 SCSI`,
`94161`, `94171`, `2925`, `1375`.

### 5.1 SINTRAN III (K / L / M) — **NOT FOUND** **[VERIFIED]**

Scanned: every `*.bin` under
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\` —
`K-VSX-500`, `L-VSX-500`, `M-VSX-500`, all carved segments plus
`resident\SINTRAN-DATA_commoncode.bin` (228 `.bin` files, 200 MB).

A second, exhaustive pass over the same 228 `.bin` files with the full needle
set `NDMICROP NDCDC NDSTK NDTANDBE EXABYTE EXB-8 "VIPER 150" "TDC 3600"
"LD 1200" 97201 94171 94161 94181 88780 "ARCHIVE "` returned literally
**`--- 0 hit(s)`**. **[VERIFIED]**

Result: **zero** hits for any SCSI vendor or product string. The only
`TANDBERG` hits in the whole carved corpus are in
`L-VSX-500\segments\134-SNA3270.bin` at bytes 198696…199213, which is the
**terminal-type** list (`3:TANDBERG-TDV2115`, `36:TANDBERG-TDV2215-EXTENDED`,
…) — unrelated to SCSI. **[VERIFIED]**

The carved SCSI driver/disk-layer segment `065-S3SIPIT.bin` (L and M,
104 448 bytes) contains **no ASCII strings at all** — every printable run ≥6
chars is code bytes that happen to fall in the printable range. **[VERIFIED by
exhaustive strings extraction]**

### 5.2 SINTRAN NPL source and symbol lists — **NOT FOUND** **[VERIFIED]**

`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\` (92 MB, including `SYMBOLS\L07`,
`K03`, `M06`, the `IP-P2-SCSI-*.NPL` drivers and `s3vs-4.symb`) contains no
SCSI vendor/product string. The only `TANDBERG` matches are comments about the
**Tandberg mag-tape controller** and a Tandberg **terminal**
(`s3vs-4.symb:4981 % TANDBERG MAG.TAPE DATAFIELD`, `:8055 % A TANDBERG
TERMINAL`, `IP-P2-1.NPL:402`). **[VERIFIED]**

### 5.3 The kernel structurally *cannot* hold them **[VERIFIED]**

`SINTRAN\NPL-SOURCE\NPL\IP-P2-SCSI-DISK.NPL:1216-1256` (listing `062214`-`062310`),
the `INQUI` routine, quoted:

```
062215   INQUI: SUTYP/\SMASK\/77400=:SUTYP           % ZERO OLD STATUS
062221          T:=SMBP1; X:=SMBP2                   % PHYSICAL ADDRESS OF COMMAND BUFFER
062223          11000; *STATX 00; STZTX 10           % COMMAND      (0x1200 = INQUIRY)
062226          "SINBL" SHZ 11; *STATX 20            % ALLOCATION LENGTH
...
062251          T:=SMBP1; X:=SMBP2+SINBS; *LDATX 00
062255          A\/377/\SUTYP=:SUTYP                 % UPDATE DEVICE TYPE INFORMATION
062260          IF A SHZ -10=0 OR =3 OR =4 THEN
062270             SUTYP BONE 5SCDA=:SUTYP           % DIRECT ACCESS DEVICE
```

Only **byte 0** of the INQUIRY reply (the peripheral device type) is ever read.
The allocation length is **8** (already byte-verified in
`CARVED-DISC-SUPPORT.md` §4.2 from the carved words at `062624`/`062625`), so
the vendor field (INQUIRY bytes 8-15) and product field (bytes 16-31) are never
even transferred. **[VERIFIED — source text plus the previously byte-verified
allocation length.]**

### 5.4 MACM — **NOT FOUND** **[VERIFIED]**

Scanned `D:\ND\BPUN\` entire directory, including `MACM-1718L.BPUN`,
`MACM-1718K.BPUN`, `MACM-1718K:BPUN`, `MAC.BPUN`, `SINTRAN-I:DATA`, and the
MACM copies on `D:\ND\S\N-102-292-I.img`, `VSXK2.img`,
`N-250306K05--02D.img`. Zero hits. The only `TDC` match in that tree is
`TCD= TEST CDC MATRIX PRINTER` in `ND-disk-00501.img` (a byte-swapped false
positive). **[VERIFIED]**

### 5.5 Real installed hard-disk images — the tables appear on exactly ONE ND disk, as an installed copy of the diagnostics **[VERIFIED]**

Every image in `D:\ND\HDD\` was scanned (≈7 GB, 34 files produced hits out of
the whole directory; needles `NDMICROP NDCDC NDSTK NDTANDBE EXABYTE VIPER
"LD 1200 SCSI" "TDC 3600" 97201 21247 ARCHIVE TANDBERG`, all four encodings).

**One ND image contains the tables: `D:\ND\HDD\nd-test_ny.img`.** **[VERIFIED]**

| what | byte offset | contents |
|---|---|---|
| SCSI-TV product table | **713790** (0o2562076) | `LD 1200 SCSI    ` … `88780           ` — 17 records, identical to `SCSI-TV-C04.TEST` §3.3 |
| SCSI-TV vendor table | **714096** (0o2562560) | `NDMICROP` … `NDHP    ` — 17 records, 14-byte format |
| SCSI-TV version banner | **764848** (0o2725660) | `SCSI Test and Verify - Version: C04 - 1990-06-11` |
| DISK-MM product table | **932154** (0o3434472) | `1375` … `94181-          ` — 12 records, identical to `DISK-MM-C03.TEST` §3.4 |
| DISK-MM vendor table | **932370** (0o3435022) | `NDMICROP`, `NDCDC   ` |
| DISK-MM version banner | **946694** (0o3471006) | `DISK Media Maintenance - Version: C03 - 1990-06-11` |

**[INFERRED]** This is simply the `SCSI-TV`/`DISK-MM` diagnostic pair copied
onto a SINTRAN file system, not a kernel or driver structure — the byte content
is identical to the diskette files of §3.3/§3.4, banners included.

**All other ND disc images are NOT FOUND**, including the SCSI-attached ones:
`BIGDISK0-SCSI.IMG`, `SCSI-K.image`, `scsi-1.img`, `scsi-k.img`,
`sintran_iii_m05_st31200n.image`, `BIGDISK0-K/K2/L/M.IMG`, `1325.img`,
`WD0-L/M.img`, the `c3*` family, `HD0.IMG`, `COPYTEST.IMG`, `tor-disk.img`.
Their hits are all noise: `TANDBERG` from the terminal-type list, `ARCHIVE`
from the `@COPY-USERS-FILES` `COPY-MODE` help text ("*Set a special mode for
copying, e.g. COPY, ARCHIVE, etc.*"), and `VIPER`/`21247`/`97201` from
byte-swapped false positives inside Norwegian error text and octal patch
listings. **[VERIFIED — each inspected.]**

**Not ND software:** the SunOS/BSD images in the same directory
(`micropolis1355-sun2-sunos1.1/2.0.img`, `my-sun2-s3.2-disk.img`,
`sun2-sunos-3.2.img`, `SUN2_BSD.img`, `MacDisk.img`) carry *their own*
unrelated SCSI drive tables — e.g. `SUN2_BSD.img` @1243514
`EXABYTE .EXB-8200        .` … `TANDBERG. TDC 3600       .ARCHIVE .VIPER 150
21247.VIPER 1500 21247.`, and `my-sun2-s3.2-disk.img` @11215812
`HP.8124.LMS.ARCHIVE.WANGTEK.ADAPT.>..EXABYTE.st%d:  warning, unknown tape
drive found...`. These are Sun's `st`/`probe_fbs` driver tables and have
nothing to do with Norsk Data. **[VERIFIED]**

**[INFERRED]** A running SINTRAN system does not store the SCSI identity of its
drives anywhere on disc, consistent with §5.3 — the only occurrences on disc
are file-system copies of the diagnostic programs.

### 5.6 Rest of `D:\ND\` — only modern emulator artefacts **[VERIFIED]**

A sweep of `D:\ND\` excluding the already-covered `S\` and `HDD\` trees (needles
`NDMICROP NDCDC NDSTK EXABYTE NDTANDBE "LD 1200 SCSI" "TDC 3600" 97201
"VIPER 150"`) produced hits in only 13 files, all of them modern. It found
`NDMICROP` / `1375` only in **present-day SCSI-emulator artefacts** created for
this project:
`D:\ND\110.2\SCSI-EMU\bluescsi.ini` (offsets 107, 294, 1403),
`zuluscsi.ini`, `sd-card-prepped\*.ini`, `zululog.txt` (7200),
`dump-log.txt` (82690). These are BlueSCSI/ZuluSCSI configuration and logs, not
ND software. `D:\ND\S\Test-microprogram\*.exe` (`RetroCore.exe`,
`RetroCommander.exe`, `TDV.exe`) and
`D:\ND\S\string-scsi.txt` / `scsi-string-7.txt` / `tempfile` are likewise
present-day project artefacts (the latter three are earlier `strings` dumps of
the `210523H00` diskette). **[VERIFIED]**

The remaining hits in that sweep were `97201` inside hex/trace dumps
(`D:\ND\5000\RetroCommander.exe`, `D:\ND\500\csharp-window2-1095k-1100k.txt`,
`nd500x-window2-1095k-1100k.txt`, `FraTor\nc\*.txt`) — decimal/hex digit runs,
not device names. **[VERIFIED — each inspected.]**

---

## 6. Mapping the wiki list onto the binaries

| wiki row | present in a binary? | where |
|---|---|---|
| `NDMICROP` / `1375` / Direct | **YES** | SCSI-TV B00, C00, C04; DISK-MM B00, B01, C03 |
| `TANDBERG` / `TDC 3600` / Sequential | **YES**, as ` TDC 3600       ` (leading blank) | SCSI-TV all versions |
| `OSI` / `LD 1200 SCSI` / Write Once | **YES** | SCSI-TV all versions |
| `NDCDC` / `EMD 97201 (736)` / Direct | **YES** | SCSI-TV all versions; DISK-MM all versions |
| `NDCDC` / `EMD 97201 (368)` / Direct | **YES** | SCSI-TV all versions; DISK-MM all versions |
| `NDSTK` / `2925` / Sequential | **YES** | SCSI-TV all versions |
| `ARCHIVE` / `VIPER 150 21247` / Sequential | **YES**, as `VIPER 150  21247` (**two** blanks) | SCSI-TV all versions |

All seven wiki rows are real strings. **But the wiki list is incomplete** — it
omits everything in §0's last row — **and the "Device type" column is not a
field in the table** (§2.4); it must have come from a manual, not from the
binary. **[VERIFIED / INFERRED as marked.]**

---

## 7. Reproducing this

```powershell
# vendor/product string hunt, all encodings
python E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\find_nd_strings.py `
    -n NDMICROP -n NDCDC -n NDSTK -n EXABYTE -n "LD 1200 SCSI" -n "TDC 3600" `
    -n "VIPER 150" -n 97201 <file-or-directory>

# extract a diskette read-only (never use -p on binaries)
wsl -d Ubuntu -- bash -lc "ndtool -x -o /tmp/out /mnt/d/ND/S/210523G02-XX-02D.image"
```
