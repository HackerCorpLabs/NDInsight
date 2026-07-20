# The SINTRAN III disc bootstrap — what page 0 of a hard disc is, and who writes it

**Question answered:** where does the *hard-disc* boot sector come from? Not the
floppy BPUN/FLOMON loader (already documented in
[`../../SINTRAN/Filesystem/boot-creation.md`](../../SINTRAN/Filesystem/boot-creation.md)
§4) — the raw ND-100 program that lives in page 0 of an SMD / Winchester / SCSI
system pack.

**Short answer (VERIFIED):** it is **not** shipped as a ready-made boot image, and
it is **not** assembled by MACM during system generation. **SINTRAN writes it
itself**, every `@COLD-START` and every `@RESTART-SYSTEM`, from code that lives
inside the running SINTRAN image: a fixed 192-word *LOAD PROGRAM* (`RELOA..LDEND`)
plus the *swap driver* for whichever disc type the system disc is, with about a
dozen parameter words patched in place. That closes the item previously marked
**OPEN** in `boot-creation.md` §10 ("the utility/MODE file that originally
*authored* the SMD page-0 program").

Evidence classes used below: **[VERIFIED]** = read directly from a named file
(source, manual, or disc bytes). **[INFERRED]** = deduced, stated as such.
**[NOT FOUND]** = looked for, not present in the artifacts on this machine.

---

## 1. The consumer contract — what the hardware does

**[VERIFIED]** `Reference-Manuals/ND-06.015.02 ND-100 Functional Description.md`
§7.2.5.2 *Mass Storage Load* (p. 229):

> "When loading from mass storage, 1 K words will be read from mass storage
> address 0 into main memory, starting in address 0. After a successful load, the
> CPU is started in main memory address 0. The mass storage device must conform
> with either drum or disk programming specifications."

**[VERIFIED]** §7.2.5.3 *Automatic Load Descriptor*: the ALD thumbwheel supplies a
16-bit value `0 0 M 0 <address 0-10>`; `M` (bit 13) = 1 selects mass-storage load
from the device whose lowest address is in bits 0-10, 0 selects binary (BPUN) load.
`$` / `&` / the LOAD button use it. The bootstrap loaders live **in the ND-100
microprogram**, not in any ROM on the disc controller (§2.3.1, §7.2.1).

So page 0 of a system disc is a **raw ND-100 program**: word 0 is the first
instruction executed, there is no header and no checksum, and only 1 K words are
loaded.

---

## 2. Who writes it — SINTRAN, at COLD-START / RESTART-SYSTEM

**[VERIFIED]** `SINTRAN/NPL-SOURCE/NPL/PH-P2-OPPSTART.NPL`, routine `FILL2`,
section `*PL011=*` (lines 845–876, ND-100 addresses `045464`–`045626`):

```
045464   % SET UP DISC LAYOUT TABLE FOR THE "SWAP-DRIVER"
045504   %INITIALIZE THE DATAFIELD FOR THE "SWAP-DRIVER"
045504          CDABLPAGE*77; *1BANK; STA I (NOBLK; 2BANK
045511          CSWPDF.HDEV; *1BANK; STA I (KLHDE; 2BANK
045516          A+"IOX+4"; *1BANK; STA I (KLIOX; 2BANK      <-- builds the IOX instruction
045522          DBLST(0); *1BANK; STD I (DYBLS; 2BANK
045527          DSKTYP; *1BANK; STA I (XSWTP; 2BANK
045533          A:=-4; *1BANK; STA I (KLRC1; 2BANK
045537          SWPDRIVER; *1BANK; STA I (LDRAD; 2BANK
045543          A+SWDSIZE; *1BANK; STA I (ADR2B; 2BANK
045547          IF SWPDRIVER="ZBDIS" THEN A:=1               % SYSTEM DISC IS A "BIG-DISC"
045554          ELSE IF A="ZWDIS" THEN A:=2                  % SYSTEM DISC IS A "WINCHESTER" DISC
045561          ELSE IF A="SCSWD" THEN
045565             CSWPDF.BLSZ; *1BANK; STA I (KBLSZ
045571             *STZ I (KLIOX; 2BANK                      <-- SCSI: no literal IOX
045573             A:=3
045574          ELSE A:=0
045576          FI; FI; FI; *1BANK; STA I (YSWTY; 2BANK

045601   % READ PAGE #0 AND INSERT BOOTS-STRAP AND "SWAP-DRIVER", AND
045601   % THEN WRITE THE PAGE BACK
045601          A:=0; X:=1; T:=0; CALL FAR CRDISC            % read page 0
045605   % MOVE BOOTS-STRAP INTO "PAGE #0"
045605          A:="LDEND"-"RELOA"=:L; A:="RELOA"=:D
045612          X:=1CDDMADR; T:=2CDDMADR; *MOVNP
045615   % MOVE SWAP-DRIVER TO "PAGE #0"
045615          SWDSIZE=:L; A:=SWPDRIVER=:D; *MOVNP
045622   % WRITE PAGE #0 BACK TO DISC
045622          A:=0; X:=1; T:=1; CALL FAR CRDISC            % write page 0
```

**[VERIFIED]** The same thing in prose, in
`Reference-Manuals/ND-820023-1-EN SINTRAN III-VSX System Documentation.md`
(§13, *Start Routines*, p. 185–192):

> "SINTR … IF COLD-start: Copy segments from save to image. **Write bootstrap to
> disk.**"
> "PL011 … **Read page #0 from disk and insert bootstrap and 'swap driver', and
> then write the page back**, use subroutine CRDISC during read/write to disk."
> "Read page 0 from the system disk. This contains the start bootstrap
> (LOAD-PROGRAM). Place it into memory from address 0. Clear CACHE. Start the
> bootstrap in address 0."

So the page-0 image is **generated at install/boot time by SINTRAN itself**, from
two pieces of the running system image. `@CREATE-DIRECTORY` has nothing to do with
it (it writes bytes 2000..2047, the extended-info block and label —
see `create-directory.md`).

---

## 3. The layout of page 0

**[VERIFIED]** sizes, from the shipped symbol tables in
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/`:

| symbol | value | file |
|---|---|---|
| `RELOA` | `062417B` | `SYMBOL-2-LIST.SYMB.TXT` |
| `LDEND` | `062717B` | `SYMBOL-2-LIST.SYMB.TXT` |
| `SWDSI` (`SWDSIZE`) | `001350B` = 744 words | `SYMBOL-1-LIST.SYMB.TXT`, `FILSYS-SYMBOLS`, `RTLO-SYMBOLS`, `N500-SYMBOLS` |

```
word 0000B .. 0177B   LOAD PROGRAM   (LDEND-RELOA = 0300B = 192 words)   fixed
word 0200B .. 1747B   swap driver    (SWDSI       = 1350B = 744 words)   per disc type
                      192 + 744 = 936 words = 1872 bytes
byte 2000  .. 2015    extended-info block  (filesystem, not boot)
byte 2016  .. 2047    master block / directory label
```

**[VERIFIED]** measured last-non-zero word of 18 real boot pages: 927, 943, 953 or
999 — consistent with 936 words of program plus a little version-to-version drift
and residual data. It confirms the RetroFS convention of treating **bytes 0..1999**
(1000 words) as the boot region, and refutes any 1024-byte cut.

---

## 4. The LOAD PROGRAM (`RELOA`) — source and shipped bytes

**[VERIFIED]** source: `PH-P2-OPPSTART.NPL` lines 3720–3790 (`% "LOAD" PROGRAM`):

```
062331  RELOA, PIOF; TRA STS; BSKP ONE 140 DA; JMP *+5; SAA 0
062336         TRR 11; SAA 77; JMP *+2; LDA (37400; TRR 12; TRR 10
062344  % MOVE RELOAD PROGRAM TO ADDRESS LKONS-WORD2
062344         LDX ADR3; LDA I ,X ADR1; STA I ,X ADR2; JNC *-2
062350         LDA ADR1; ADD (SWDSI; STA ADR1
062353  % MOVE DRIVER TO THE CORRECT ADDRESS
062353         LDX (-SWDSI; LDA I ,X ADR1; STA I ,X ADR2B; JNC *-2
062357         JMP I *+1; LKONS-WORD2
...
062422  NOBLK, 0
062423  ADR2B, 0
062424  DYBLS, 0;0
062426  LDRAD, 0
062427  XSWTP, 0
062430  YSWTY, 0         % 1=BDIS; 2=WDIS
062431  NALOA, LDX NOBLK; LDT LOUNI,B; SHT 6
062434         LDA XSWTP; RADD SA DT; LDD DYBLS
062437  NALO4, JPL I LDRAD; JMP NALOA; JMP *+2; JMP NALO3   % CALL DRIVER
062443         STA NALO5
062444  NALOY, MCL PID; MCL PID; MCL PID; MCL PID
062450  KLIOX, IOX 4; BSKP ZRO 20 DA; JMP NALOY; LDA NALO5; JMP NALO4
062455  NALO2, IDENT PL11; JMP I (SINTR
062457  NALO5, 0
```

(The addresses in the listing are the assembly addresses of the NPL source we hold,
which is one revision off the L distribution: in the L build `RELOA` sits at
`062417`, i.e. 8 words earlier. The instruction sequence is identical.)

**[VERIFIED]** the shipped bytes. Extracted from the **VSX L distribution floppy**,
BPUN record #2 (`)9READ` at file offset 29151, load address `026000B`,
`144001B` words, **checksum verified**), words `062417B..062716B`:

```
062417  150405   PIOF                     <- word 0 of page 0; the raw-bootstrap signature
062420  150001   TRA STS
062421  170412   SAA 12
062422  150103   TRR PCR
...
062510  000000   NOBLK   \
062511  000000   ADR2B    |
062512  000000   DYBLS    |  the parameter block — ALL ZERO as shipped
062513  000000   DYBLS+1  |
062514  000000   LDRAD    |
062515  000000   XSWTP    |
062516  000000   YSWTY   /
...
062532  150206   MCL PID
062533  150206   MCL PID
062534  150206   MCL PID
062535  150206   MCL PID
062536  164004   IOX 4    <- KLIOX, still the literal source value
062537  175025   BSKP ZRO 20 DA
062540  124372   JMP NALOY
062541  044004   LDA NALO5
062542  124363   JMP NALO4
062543  143611   IDENT PL11
062544  125003   JMP I (SINTR
062545  000000   NALO5
```

Saved as
**`E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\loadprogram-VSX-L-RELOA.bin`**
(+ `.md`).

### 4.1 The clinching cross-check

**[VERIFIED]** word-diff of that shipped, unpatched LOAD PROGRAM against the first
192 words of a **real installed L-version SMD system disc**
(`D:\ND\HDD\BIGDISK0-L.IMG`, page-0 sha8 `ec962fc2`):

**176 of 192 words identical.** The 16 that differ are exactly the words PL011
patches:

| word | shipped | on the real disc | symbol / meaning |
|---|---|---|---|
| `0071B` | `000000` | `000176` | `NOBLK` = `CDABLPAGE*77` |
| `0072B` | `000000` | `101163` | `ADR2B` = `SWPDRIVER + SWDSIZE` |
| `0074B` | `000000` | `000400` | `DYBLS` = `DBLST(0)`, first block of SEGFILE 0 |
| `0075B` | `000000` | `077603` | `LDRAD` = address of the swap driver |
| `0077B` | `000000` | `000001` | **`YSWTY` = 1 → "big disc" (SMD)** |
| `0117B` | `164004` | `165544` | **`KLIOX` = `IOX 4` → `IOX 1544` (= HDEV 1540 + 4)** |
| `0132B`–`0142B` | `000000` | `001000,000022,000132,001466,001465,000010,000023` | the swap driver's data field (`DFELT`) |
| `0174B` | `000000` | `177774` | `KLRC1` = -4 (`ERRC1` retry count) |
| `0236B` | `000000` | `001540` | **`KLHDE` = `HDEV` = SMD controller base** |
| `0271B` | `000000` | `062551` | (driver data-field entry) |
| all others | — | identical | |

This is the whole mechanism, byte-proven.

---

## 5. How it varies per media type

**[VERIFIED]** the disc-type table, `PH-P2-OPPSTART.NPL` lines 29–31:

```
INTEGER ARRAY WWDIS:=(WIGDI,1224,ZWDIS,WIDIS,   500);  % ST-506  (Winchester)
INTEGER ARRAY BBDIS:=(BIGDI,1100,ZBDIS,BDISK,  1540);  % SMD
INTEGER ARRAY SCDIS:=(SCDI1,2210,SCSWD,SCSWD,144300);  % SCSI
% entry 0 = datafield addr, 1 = logical device no, 2 = swap driver ("boots-strap driver"),
% 3 = start-up driver, 4 = hardware device number of the swapper controller
```

Those hardware device numbers — `1540`, `500`, `144300` — are **exactly the DEVNO
values that the `)9BYTT` generation stream sets** for `BD288/BDFIX`, `W8INC /
REMOV / FIXED` and `SCASI` (see `device-geometry.md`). Same numbers, two different
consumers: `)9BYTT` fills SINTRAN's tables at generation time, `PL011` then bakes
the number into the boot page at COLD-START.

Three things change, and only three:

1. **`SWPDRIVER`** → a *different 744-word swap driver* is copied into words
   `0200B..1747B`: `ZBDIS` (SMD), `ZWDIS` (Winchester), `SCSWD` (SCSI).
   **[VERIFIED]** by diffing two real boot pages: SMD vs SCSI differ in **790 of the
   808 words** after word 192, but in only **20 of the first 192**.
2. **`KLIOX`** → `IOX (HDEV+4)` for SMD/Winchester (a *literal* IOX built by
   addition at run time); **zeroed** for SCSI, because the SCSI driver addresses its
   controller with `IOXT` (device number in `T`) instead.
3. **`YSWTY`** → `1` = SMD, `2` = Winchester, `3` = SCSI, `0` = other; the boot
   program branches on it (`NBDI`/`LOWDI`/`SCDI`) to plant the right spare-track
   address (`NWLBB` for SMD, `WNLBA` for Winchester) or, for SCSI, to issue an
   extra `SAT 42; JPL I LDRAD` driver call first. SCSI additionally gets `KBLSZ`
   (block size) patched.

**[VERIFIED]** in real bytes:

| specimen | word 0 | `KLIOX` | `YSWTY` | `IOX` window used | `IOXT` count |
|---|---|---|---|---|---|
| `disc-smd-ec962fc2` (BIGDISK0-L) | `PIOF` | `165544` = `IOX 1544` | 1 | 1540–1547 | 0 |
| `disc-winchester-05afdb0f` (WD0) | `PIOF` | `IOX 504` | 2 | 500–507 | 17 |
| `disc-scsi-d90b55c5` (scsi-1) | `PIOF` | `170400` (no IOX) | 3 | none | 42 |

The `IDENT PL11` (`143611`) that ends the LOAD PROGRAM is present in **every**
bootable specimen — that is `NALO2, IDENT PL11; JMP I (SINTR`.

---

## 6. What is in the distribution stream, and what the octal `!` commands mean

**[VERIFIED] Extraction gotcha:** `ndtool -x -p` **destroys** the `:DATA` stream.
`-p` is "even parity: strip on extract", which clears bit 7 of every byte. The
`)9READ` payloads are 8-bit binary; with `-p` no BPUN checksum verifies and no
ND-100 opcode can be found. Extract with **`ndtool -x -o <dir> <image>`** (no `-p`).

**[VERIFIED]** `SINTRAN-L-1:DATA` (from `D:\ND\S\VSXL1.IMG`, 1 095 538 bytes)
contains **22 `)9READ` commands**. Each is followed by ~131 bytes of NUL leader,
then a MAC `)BPUN` ASCII preamble, then a binary record
`'!' | u16 load-address | u16 word-count | words | u16 checksum`, big-endian,
checksum = plain 16-bit sum of the code words. **All 22 checksums verify.**

| # | file offset of `!` | load addr | words | end |
|---|---|---|---|---|
| 0 | 7887 | `000000` | `024131` (10329) | `024130` |
| 1 | 29151 | `026000` | `144001` (51201) | `172000` |
| 2 | 132159 | `004000` | `130114` (45132) | `134113` |
| 3 | 223029 | `030000` | `031024` (12820) | `061023` |
| 4 | 249275 | `144000` | `003515` (1869) | `147514` |
| 5 | 253619 | `030000` | `060515` (24909) | `110514` |
| 6 | 304043 | `164000` | `001123` (595) | `165122` |
| 7 | 305839 | `026000` | `002527` (1367) | `030526` |
| 8 | 309179 | `032000` | `107266` (36534) | `141265` |
| 9 | 382853 | `032000` | `122733` (42459) | `154732` |
| 10 | 468377 | `032000` | `045724` (19412) | `077723` |
| 11 | 507807 | `000000` | `032621` (13713) | `032620` |
| 12 | 535839 | `026000` | `145566` (52086) | `173565` |
| 13 | 640617 | `026000` | `075253` (31403) | `123252` |
| 14 | 704029 | `030000` | `065050` (27176) | `115047` |
| 15 | 758987 | `026000` | `011476` (4926) | `037475` |
| 16 | 769444 | `120000` | `056000` (23552) | `175777` |
| 17 | 817153 | `000000` | `116073` (39995) | `116072` |
| 18 | 897749 | `164000` | `013700` (6080) | `177677` |
| 19 | 910388 | `000000` | `115123` (39507) | `115122` |
| 20 | 989986 | `040000` | `136000` (48128) | `175777` |
| 21 | 1086848 | `002000` | `010000` (4096) | `011777` |

They **overlap heavily** — the same memory addresses are re-used for successive
SINTRAN segments, which MACM writes out to different disc areas between reads. A
flat overlay of all 22 records is therefore meaningless; records must be examined
individually. (The LOAD PROGRAM lives in record #1.)

**The octal numbers before `!`:**

* **`22!`** — this is not part of a BPUN record at all. It is a **MOPC operator
  command**: **[VERIFIED]** `ND-06.015.02` §7.2.1, "Characters only legal in STOP:
  `!` = Start program in main memory command", with the address typed in front of
  it. `22!` = *start execution at address `22B`*. **[VERIFIED]**
  `ND-820023-1-EN` §13.1 flow-chart shows exactly `22!` as the step that ends the
  MACM generation dialogue, and §13.2.3 says the load program "Start[s] in address
  22". Likewise `10,0$` / `10,1$` / `1,0$` in the stream are MACM commands, not
  MOPC.
* **`160616!` and `115123!`** — these terminate the **ASCII preamble of each BPUN
  record**. The preamble is the standard MAC `)BPUN` header: a location-set
  `160616/` followed by 34 octal words and then the same number again terminated by
  `!`. **[VERIFIED]** `ND-06.015.02` §7.2.5.1 defines fields *B* (octal number
  terminated by CR) and *C* (octal number terminated by `!`) of the binary-load
  format, and the action code *I* decides whether the program is started at that
  address. **[VERIFIED by inspection]** the 34 octal words are a small
  character-device read loop — they contain `164403`/`164402`/`164400` (`IOX 403 /
  402 / 400`, the tape-reader window), `175235` (BSKP), `124376` (JMP *-2) — i.e.
  the classic "octal-coded bootstrap" that ND-60.066.04 describes for the
  Relocating Loader. **[INFERRED]** `160616B` / `115123B` are that mini-loader's own
  entry address; MACM's `)9READ` ignores the ASCII and consumes only the `!`
  record. One record (#19) uses `115123`, the rest use `160616`.

### Is a per-media boot image present verbatim in the stream?

**No — and it is not assembled by MACM either.** **[VERIFIED]**

* The **LOAD PROGRAM is present verbatim** in the stream, but in its *unpatched*
  form and *not* at disc address 0 — it is simply part of the SINTRAN system image
  at `062417B` (record #1, §4).
* The **swap drivers** are likewise ordinary SINTRAN driver code inside the image.
* There is **no `)9BYTT`, no MACM conditional, and no `"BD288`/`"W8INC`/`"SCASI`
  guard that emits a boot page.** The disc-variant conditionals in the header
  select *layout parameters* (MSTYP/DEVNO/CORAD/LONG/CLM/BLST/DRES/CRMAX/MACAD/DASA),
  which end up in SINTRAN's tables; the boot page is built later, at run time, by
  PL011.
* Searching the raw stream for a page-0-shaped image (`PIOF` at a page boundary,
  or `MCL PID`×4 + patched `IOX 154x`/`IOX 50x`) finds **nothing** — the only
  `PIOF` sites are inside the system image, and their `KLIOX` is the unpatched
  `IOX 4`.

---

## 7. Real specimens extracted

`scan_disc_boot.py` was run **read-only** over 54 hard-disc images under `D:\ND\`.
Result: **18 distinct bootable page-0 images**, plus several zero / space-filled
(non-bootable) and one non-standard. All 18 are saved with a provenance `.md` in

**`E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\`**

| file | YSWTY | source images (examples) |
|---|---|---|
| `disc-smd-059ac510.bin` | 1 SMD | `D:\ND\HDD\BIGDISK0.IMG` |
| `disc-smd-21382ad9.bin` | 1 SMD | `D:\ND\HDD\a.IMG` |
| `disc-smd-296ed770.bin` | 1 SMD | `D:\ND\HDD\BIGDISK0-K.IMG`, `BIGDISK0-K2.IMG` |
| `disc-smd-80ba323f.bin` | 1 SMD | `D:\ND\HDD\c3-k-bd-clean.img` |
| `disc-smd-86e648bd.bin` | 1 SMD | `D:\ND\HDD\BIGDISK0-M.IMG` |
| `disc-smd-ec962fc2.bin` | 1 SMD | `D:\ND\HDD\BIGDISK0-L.IMG`, `BIGDISK0-SCSI.IMG`, `c3-k-bd.img` |
| `disc-winchester-05afdb0f.bin` | 2 W | `D:\ND\HDD\WD0.img`, `WD0-L.img` |
| `disc-winchester-0ab983b4.bin` | 2 W | `D:\ND\HDD\1325.img`, `c3-recovered.img`, `disk-dump-1k.img`, `c3_2024_1.img` |
| `disc-winchester-c6f30aba.bin` | 2 W | `D:\ND\HDD\disk-dump.img` |
| `disc-winchester-d9df77b6.bin` | 2 W | `D:\ND\HDD\tingo_raw_debug.img` |
| `disc-winchester-e2da6491.bin` | 2 W | `D:\ND\HDD\WD0-M.IMG`, `HD0.IMG`, `COPYTEST.IMG`, `sintran_m.img` |
| `disc-scsi-47f5dc0d.bin` | 3 SCSI | `D:\ND\HDD\HD00_imaged.img` |
| `disc-scsi-70c3d994.bin` | 3 SCSI | `D:\ND\HDD\scsi-k.img`, `SCSI-K.image`, `tor-disk.img` |
| `disc-scsi-9411182f.bin` | 3 SCSI | `D:\ND\SI1.img` |
| `disc-scsi-a47ce5c4.bin` | 3 SCSI | `D:\ND\HDD\sintran_iii_m05_st31200n.image` |
| `disc-scsi-d90b55c5.bin` | 3 SCSI | `D:\ND\HDD\scsi-1.img`, `test.IMG`, `disk.image` |
| `disc-unknown-1da707c1.bin` | 0 | `D:\ND\HDD\BIGDISK0-H.IMG`, `WD.IMG`, `BDH.IMG` — H-version SMD page that *also* drives the console (`IOX 300..306`) |
| `disc-unknown-8329440e.bin` | 0 | `D:\ND\HDD\disk-dump-2k.img` — Winchester window but `YSWTY` not at the usual offset |
| `loadprogram-VSX-L-RELOA.bin` | — | the unpatched 192-word LOAD PROGRAM from the L distribution floppy |

Non-bootable (page 0 all zero): `BIGDISK1.IMG`, `BIGDISK0-EMPTY.IMG`,
`BIGDISK0-L-TEST.IMG`, `RAND.IMG`, `c3-k.img`, `image.img`.

### Cross-validation against `RetroFS.NDFS.Creation.NdfsBootBlobs`

**[VERIFIED]** the three hard-disc base64 blobs in
`E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Creation\NdfsBootBlobs.cs` decode to 2000
bytes each and are **byte-identical** to real page-0 regions found here:

| blob | sha8 | identical to |
|---|---|---|
| `SmdB64` | `296ed770` | `D:\ND\HDD\BIGDISK0-K.IMG` page 0 |
| `WinchesterB64` | `0ab983b4` | `D:\ND\HDD\1325.img` (Micropolis 1325) page 0 |
| `ScsiB64` | `d90b55c5` | `D:\ND\HDD\scsi-1.img` page 0 |
| `FloppyB64` | `f3fe2d5a` | a FLOMON BPUN stream (not a mass-storage page) |

Their claimed provenance is confirmed, and the structural claims in
`NdfsBootSectors.cs` (2000-byte region, live code past byte 1024, PIOF + literal
IOX for SMD/Winchester, PIOF + `IOXT` for SCSI) all hold.

Two corrections to `SINTRAN/Filesystem/boot-creation.md`:

* §5 "Winchester — **DERIVED**, no real Winchester boot image" and §6 "SCSI —
  **DERIVED**" are **out of date**: five real SCSI and five real Winchester boot
  pages exist on this machine and are now extracted.
* §7.2 / §10 "the utility that originally *authored* the SMD page-0 program —
  **OPEN**" is now **closed**: `PH-P2-OPPSTART.NPL` PL011, run by SINTRAN itself.
* §3.3's annotation of word 2 as `SAA 12` is right for the *Winchester* blob but
  wrong for the SMD/SCSI ones, where word 2 is `175345` = `BSKP ONE 140 DA`, per
  the `RELOA` source. The two blob families are different SINTRAN revisions.

---

## 8. Tools written (all read-only w.r.t. images)

All under **`E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\`**:

| file | purpose |
|---|---|
| `scan_disc_boot.py` | survey page 0 of any number of images; classify, list IOX/IOXT/IDENT, dedupe, `--extract` unique regions + `provenance.json` |
| `compare_disc_boot.py` | word-diff two boot pages, split at the LOAD-PROGRAM/swap-driver boundary |
| `decode_9read.py` | parse the `)9READ` BPUN records of a `SINTRAN*:DATA` stream, verify checksums, `--region` to pull an address range out of one record, `--dump` for a flat overlay |
| `export_disc_specimens.py` | name/save/document boot pages as `disc-<type>-<sha8>.bin` + `.md` |

---

## 9. What remains unknown

* **[NOT FOUND]** the *source* of the three swap drivers `ZBDIS` / `ZWDIS` /
  `SCSWD`. They are referenced by `PH-P2-OPPSTART.NPL` but defined in a module not
  present in `SINTRAN/NPL-SOURCE/NPL/`, and the symbol is not in the `K03`/`L07`/`M06`
  symbol lists either. Only their compiled bytes (words `0200B..1747B` of the real
  boot pages) are available. To close this you need the NPL/MAC source of the disc
  and SCSI *swap* drivers, or a symbol list covering the segment they live in.
* **[NOT FOUND]** how page 0 gets onto a **brand-new, never-booted** pack. PL011
  requires a running SINTRAN that already booted from *somewhere*. The plausible
  paths are (a) floppy-booted stand-alone copy program (`COP-VERIFY`, ND-10022S),
  (b) floppy-boot SINTRAN once, then `@COLD-START` writes page 0 of the hard disc.
  Both are consistent with everything above; neither is documented in the manuals
  on hand. **[INFERRED]** — (b) is what the ND-820023 flow-chart depicts
  (`Floppy-Load → MACM → ... → 22! → SINTR → "Write bootstrap to disk"`), so (b) is
  the likelier normal path, but I have not found it stated.
* **[VERIFIED bytes, INFERRED intent]** the SCSI specimens carry `KLIOX = 170400`
  (`SAA 0`) rather than the `0` that `*STZ I (KLIOX` in the L source would write.
  Either those packs were built by a different SINTRAN revision, or `KLIOX` sits at
  a different offset in them. Not resolved.
* **[NOT FOUND]** an `@DEVICE-FUNCTION`-style operator command that writes a
  *hard-disc* bootstrap. `DUMP-BOOTSTRAP` is documented as floppy-only
  (ND-60.128.5 p.97) and that remains the case.

---

## References

* `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-OPPSTART.NPL` — MDISCS table
  (l. 29–31), PL011 boot-page write (l. 845–876), LOAD PROGRAM source (l. 3720–3790).
* `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-2-LIST.SYMB.TXT`
  (`RELOA=062417`, `LDEND=062717`), `SYMBOL-1-LIST.SYMB.TXT` (`SWDSI=001350`).
* `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-06.015.02 ND-100 Functional Description.md`
  §7.2.1, §7.2.5.1–3 (MOPC, binary load, mass-storage load, ALD).
* `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-820023-1-EN SINTRAN III-VSX System Documentation.md`
  §13 Start Routines (p. 185–192), PL010/PL011.
* `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\boot-creation.md` — the floppy half and
  the page-0 map (this document supersedes its §5, §6 and §7.2 "OPEN" items).
* `E:\Dev\Ronny\NDInsight\tools\boot-floppy\device-geometry.md` — the `)9BYTT`
  parameter set and the `"BD288`/`"W8INC`/`"SCASI` conditionals.
* `E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Creation\NdfsBootBlobs.cs` — the four
  embedded blobs, all cross-validated here.
* Distribution floppy: `D:\ND\S\VSXL1.IMG` → `SINTRAN-L-1:DATA` (opened read-only).
