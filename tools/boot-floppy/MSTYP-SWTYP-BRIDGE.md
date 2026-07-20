# The MSTYP ↔ SWTYP bridge — how MACM's disc-type choice reaches the SINTRAN kernel

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\MSTYP-SWTYP-BRIDGE.md`

**Question answered:** MACM (system-generation program) numbers disc types with
`MSTYP`; the running SINTRAN kernel numbers them with `SWTYP`. The two schemes
were each decoded separately. This document is the *bridge* between them.

**Scope / method.** Everything here is derived from binaries and carved system
data: the MACM binary `D:\ND\BPUN\MACM-1718L.BPUN` (in Ghidra), the carved
kernel segments and resident data under
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\{K,L,M}-VSX-500\`,
the recovered NPL source and symbol tables under
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\`, and the distribution generation
stream `…\versions\L-VSX-500-07\inputs\distribution-layout-params.txt`. It
builds on two prior byte-verified analyses:
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\MACM-DIALOGUE.md` (MACM side) and
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\CARVED-DISC-SUPPORT.md` (kernel side).

Marking convention: **[VERIFIED]** = quoted bytes / disassembly / source line at
a named address. **[DERIVED]** = arithmetic/join shown over two verified facts.
**[INFERRED]** = reasoning shown. **NOT FOUND / COULD NOT DETERMINE** = stated
plainly, never invented.

---

## 0. The answer in one paragraph

MACM stores **two** numbers when the operator picks a disc, not one.
`MSTYP` (`ram:8342`) is a small MACM-internal index (0..23B) that selects the
**device number** and the **library mark** — it is *not* the kernel's disc-type
number and does not survive into the running kernel (`MSTYP` is not a kernel
symbol). The **second** number MACM computes — the "disc-type code" at
`ram:833b` — **is numerically the kernel's `SWTYP`**: it equals the geometry
index `DISPN` that indexes both `DISPE` (geometry) and `MDISCS` (driver) in the
cold-start code. The bridge is therefore: **`MSTYP` and `SWTYP` are two
different axes; the value that crosses from MACM into the kernel as `SWTYP` is
MACM's `ram:833b` disc-type code, not `MSTYP`.** The join is proven two ways —
a 19-for-19 value identity, and a matched encode/decode pair on the magic value
`7` (fixed Phoenix disc). Device number and library mark are the *other* two
join keys, and they agree on both sides.

---

## 1. MACM keeps two numbers, not one  **[VERIFIED]**

`disktype_menu_prompt_and_parse` @ `ram:94ad` reads the operator's octal menu
answer, doubles it, and loads a **two-word** entry from the table at `ram:9483`
(`MACM-DIALOGUE.md` §2.2, disassembly `ram:94b7`–`94ba`):

```
ram:94b7: LDD ,X I *0x94c9    ; A,D := table[0x9483 + 2*answer]   (LDD: A=word0, D=word1)
ram:94b8: STA -0x79,B         ; M[0x8342] := A   -> MSTYP
ram:94ba: STA -0x80,B         ; M[0x833b] := D   -> "disc-type code"
```

I re-dumped the table directly from the binary to confirm the two-word layout
and the SCSI row **[VERIFIED — Ghidra `MACM-1718L.BPUN`, hexdump `ram:9483`]**:

```
ram:9483: 00 08 00 08  00 08 00 09  00 0a 00 0a  00 0c 00 0c   ...
          (menu0: word0=0008 word1=0008)  ...
ram:94a2: 00 13 00 1e                                          (menu 24B / SCSI)
```

The last pair `0013 001e` = **word0 = 0x13 = 23B (MSTYP for SCSI)**,
**word1 = 0x1e = 30 decimal (disc-type code for SCSI)**. Every pair matches
`MACM-DIALOGUE.md` §6.2. So MACM's two outputs per disc are:

* **word0 → `MSTYP`** (`ram:8342`) — index into MACM's own record table
  `mstyp_record_ptr_table` @ `ram:9715`, which yields the **device number**
  (record word 1) and the **library-mark pointer** (record word 10/11).
* **word1 → disc-type code** (`ram:833b`) — carried separately.

---

## 2. THE BRIDGE — the disc-type code (`ram:833b`) IS the kernel's `SWTYP`

### 2.1 What `SWTYP` is, kernel-side  **[VERIFIED]**

`SWTYP` is a datafield word (`SWTYP=000072` in L07/M06, `=004655` in K03 —
`SYMBOLS\{L07,M06,K03}\SYMBOL-1-LIST.SYMB.TXT`). At cold start it selects **both**
the driver and the geometry (`PH-P2-OPPSTART.NPL`, quoted from
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-OPPSTART.NPL`):

```
:722 044525   IF SWTYP<<7 OR>>36 THEN CALL ERRFATAL FI   % legal type = 7..36B
:728 044552   X:= MDISCS(SWTYPE)=:LDTA                    % driver attributes (HDEV, logical no.)
:743 044614   X:=DISPE(SWTYP); T:=X.S0; A:=2000; *RDIV ST % geometry: SECWO -> sectors/page
:748 044637   DISPE(SWTYP)=:HTABL(0)                      % initial disc-layout table
```

`DISPE[i]` points at the `DTxxx` geometry record whose `DISPN` field equals `i`
(`CARVED-DISC-SUPPORT.md` §1.4). So **`SWTYP` == `DISPN` == the DISPE index.**
**[VERIFIED]**

### 2.2 Proof #1 — the value identity  **[DERIVED, over two VERIFIED tables]**

MACM's disc-type code (`ram:833b`, a decimal value) equals `DISPN` (an octal
index) for every disc type the L07 kernel supports. `DISPN` values from the
carved `DISPE` records (`CARVED-DISC-SUPPORT.md` §1.3); disc-type codes from the
`ram:9483` table (`MACM-DIALOGUE.md` §6.2, re-confirmed against the hexdump above):

| disc | MACM disc-type code (`833b`, dec) | kernel `DTxxx` | `DISPN` (oct) | `DISPN` (dec) | equal? |
|---|---|---|---|---|---|
| DISC-14MB | 8 | DT014 | 10 | 8 | ✓ |
| DISC-21MB | 9 | DT021 | 11 | 9 | ✓ |
| DISC-23MB | 10 | DT023 | 12 | 10 | ✓ |
| DISC-45MB | 11 | DT045 | 13 | 11 | ✓ |
| DISC-28MB | 12 | DT028 | 14 | 12 | ✓ |
| DISC-74MB | 13 | DT074 | 15 | 13 | ✓ |
| DISC-288MB-E | 15 | DT310 | 17 | 15 | ✓ |
| DISC-30/60/90MB | 16 | DT030 | 20 | 16 | ✓ |
| DISC-38MB | 17 | DT037 | 21 | 17 | ✓ |
| DISC-70MB | 18 | DT070 | 22 | 18 | ✓ |
| DISC-75MB | 19 | DT075 | 23 | 19 | ✓ |
| DISC-140MB-F | 20 | DT140 | 24 | 20 | ✓ |
| DISC-2-75MB | 22 | DT160 | 26 | 22 | ✓ |
| DISC-288MB-R | 23 | DT288 | 27 | 23 | ✓ |
| DISC-288MB-F | 25 | DT300 | 31 | 25 | ✓ |
| DISC-450MB-F | 26 | DT450/DT460* | 32 | 26 | ✓ |
| DISC-450MB-N | 28 | DT470 | 34 | 28 | ✓ |
| DISC-288MB-N | 29 | DT290 | 35 | 29 | ✓ |
| **SCSI** | **30** | **DTSSS** | **36** | **30** | ✓ |

The relation is exactly: **`SWTYP`(octal) printed = disc-type-code(decimal
value)** — i.e. the disc-type code is `DISPN` written as a decimal integer.
19 rows, 19 agreements, across two tables consumed by unrelated code paths.

\* DISC-450MB-F resolves to the `-S`/`-F` pair `DT450`/`DT460`, which point at
each other via the `ALTFO` alternate-format field (`CARVED-DISC-SUPPORT.md` §1.3,
records `031261`↔`031272`); the kernel picks the actual variant at run time from
the format read off the drive (`fn 42`), so either record is a valid landing.
**[INFERRED from the ALTFO cross-pointers]**

### 2.3 Proof #2 — the matched `7 ↔ 20B` encode/decode  **[VERIFIED both sides]**

The disc type `DISC-30/60/90MB` (record `DT030`, `DISPN 20B`) has a special
"fixed Phoenix" encoding using the magic value **7**. Both programs implement
the *same* remap in opposite directions:

**MACM encodes 20B → 7** when the operator answers FIXED
**[VERIFIED — Ghidra `MACM-1718L.BPUN`, disassembly `ram:9524`–`9528`]**:

```
ram:9524: LDA -0x80,B    ; A := M[0x833b]   (disc-type code)
ram:9525: AAA -0x10      ; A := A - 020B    (16 decimal)
ram:9526: JAF *0x9529    ; if A != 0 (code != 020B) skip
ram:9527: SAA 0x7        ; A := 7
ram:9528: STA -0x80,B    ; M[0x833b] := 7
```

**The kernel decodes 7 → 20B** at cold start
**[VERIFIED — `PH-P2-OPPSTART.NPL:723-724`]**:

```
044535   IF A=7 THEN                              % MAIN SWAP-DEVICE IS FIXED PHOENIX DISK
044540      20=:SWTYP; 40000=:DSKTYPE; 100000=:XXSWTYPE
```

A shared magic value (`7`) standing for the *same* physical disc (fixed
30/60/90 MB Phoenix), written by MACM and read back by the kernel, on the *same*
variable, is not reconcilable as coincidence. It is a designed encode/decode
pair — decisive that `ram:833b` and `SWTYP` are the same field. **[DERIVED]**

### 2.4 The two *other* join keys agree too  **[VERIFIED]**

* **Device number.** MACM record word 1 = `500`/`1540`/`144300`
  (`MACM-DIALOGUE.md` §6.6). Kernel `MDISCS` hardware device numbers =
  `500` (ST-506/Winchester) / `1540` (SMD) / `144300` (SCSI)
  (`CARVED-DISC-SUPPORT.md` §2, resident `SINTRAN-DATA_commoncode.bin`
  `041505`–`041525`). Identical set. **[VERIFIED]**
* **Library mark → device number.** The generation stream selects the device
  number `G` *from MACM's marks* (see §4). Same three values. **[VERIFIED]**

---

## 3. Master correspondence table — the deliverable

One row per MACM `DISK TYPE` menu answer (the 21 the operator can type). Columns
joined on device number + mark (MACM side) and on `SWTYP`/`DISPN` (kernel side).
`SWTYP` shown octal; it equals the decimal disc-type code by §2.2.

| menu (oct) | disc name | MSTYP (oct) | mark | dev no (oct) | `SWTYP` (oct) | `DTxxx` geometry | in K05? | in L07/M06? |
|---|---|---|---|---|---|---|---|---|
| 0 | DISC-14MB | 10 | W8INC | 500 | 10 | DT014 | **no** | yes |
| 1 | DISC-21MB | 10 | W8INC | 500 | 11 | DT021 | **no** | yes |
| 2 | DISC-23MB | 12 | W8INC | 500 | 12 | DT023 | **no** | yes |
| 3 | DISC-28MB | 14 | W8INC | 500 | 14 | DT028 | **no** | yes |
| 4 | DISC-30MB | 6 | BD288/BDFIX | 1540 | 20 | DT030 | yes | yes |
| 5 | DISC-33MB | 3 | BD288 | 1540 | **2** | **none** | **no** | **no** |
| 6 | DISC-38MB | 4 | BD288 | 1540 | 21 | DT037 | yes | yes |
| 7 | DISC-45MB | 11 | W8INC | 500 | 13 | DT045 | **no** | yes |
| 10 | DISC-66MB | 3 | BD288 | 1540 | **3** | **none** | **no** | **no** |
| 11 | DISC-70MB | 4 | BD288 | 1540 | 22 | DT070 | yes | yes |
| 12 | DISC-74MB | 13 | W8INC | 500 | 15 | DT074 | **no** | yes |
| 13 | DISC-75MB | 4 | BD288 | 1540 | 23 | DT075 | yes | yes |
| 14 | DISC-140MB | 15 | BD288 | 1540 | 24 | DT140 | yes | yes |
| 15 | DISC-2-75MB | 7 | BD288 | 1540 | 26 | DT160 | yes | yes |
| 16 | DISC-288MB-R | 5 | BD288 | 1540 | 27 | DT288 | yes | yes |
| 17 | DISC-288MB-F | 16 | BD288 | 1540 | 31 | DT300 | yes | yes |
| 20 | DISC-450MB-F | 20 | BD288 | 1540 | 32 | DT450/DT460 | yes | yes |
| 21 | DISC-288MB-E | 17 | BD288 | 1540 | 17 | DT310 | yes | yes |
| 22 | DISC-450MB-N | 21 | BD288 | 1540 | 34 | DT470 | yes | yes |
| 23 | DISC-288MB-N | 22 | BD288 | 1540 | 35 | DT290 | yes | yes |
| 24 | SCSI | 23 | SCASI | 144300 | 36 | DTSSS | yes | yes |

SINTRAN device names (`0 DRUM`, `2 DISC-10MB-1`, … `23 SCSI`) are keyed by MSTYP
in MACM's static string table and are listed in full in `MACM-DIALOGUE.md` §6.6.
The DRUM row (MSTYP 0, dev `540`, mark `DRUM`) has no `SWTYP`/`DISPE` entry — it
is the NORD-10 drum, not a disc-layout-table device. **[VERIFIED]**

---

## 4. Where the library marks gate code — the mechanism  **[VERIFIED]**

The marks MACM installs (`DRUM/REMOV/FIXED/BD288/BDFIX/W8INC/SCASI`,
`MACM-DIALOGUE.md` §6.4) do their work in the **`)9BYTT` distribution generation
stream**, not in the recovered kernel NPL. `distribution-layout-params.txt`
(lines 128-147) reads, literally **[VERIFIED]**:

```
128  F=MSTYP      % MASS STORAGE TYPE
131  % DEVICE NUMBER
132  "BD288+BDFIX -MADEF
133  G=1540
134  "W8INC+REMOV+FIXED -MADEF
135  G=500
136  "SCASI -MADEF
137  G=144300
139  % BIT 17 IS SET IF FIXED DISK
140  "BD288+W8INC+REMOV+SCASI -MADEF
141  FR=0
142  "BDFIX+FIXED
143  FR=100000
```

`"MARK` is the MAC library-mark conditional (true when the mark is set — CLAUDE.md
corpus note: "a library mark is true when the symbol is referenced but
undefined"; MACM makes it true by defining it with value −1, `MACM-DIALOGUE.md`
§6.4). So, **byte-verified end to end**: the single mark MACM installs selects
the assembled **device number `G`** (`1540`/`500`/`144300`) and the **fixed bit
`FR`** (`0`/`100000`). This is the concrete gate that ties MACM's operator choice
to what is baked into the generated system. **[VERIFIED]**

Note the fixed bit here (`FR=100000`) is the generation-stream twin of the
kernel's `XXSWTYPE=100000B` set by the `SWTYP=7` path (§2.3), and of the
per-word bit-15 that MACM sets on the geometry words for a FIXED answer
(`MACM-DIALOGUE.md` §6.4, Ghidra `ram:951c`–`9523`, which I re-confirmed:
`LDD -0x74,B / BSET 0xf,DA / BSET 0xf,DD / STD -0x74,B` and the same for
`-0x71,B`). **[VERIFIED]**

**Important negative — the MACM marks are NOT the kernel's conditional-assembly
marks.** Grep of the entire recovered NPL and all K03/L07/M06 symbol tables for
`BD288`, `BDFIX`, `W8INC`, `SCASI` returns **no matches**
(`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\`). The kernel's own driver-inclusion
switches are differently named — `8BDIS`/`8ZBDI` (SMD), `8WDIS`/`8ZWDI`
(Winchester), `8SCSI`/`8SCS1`/`8ZSCS` (SCSI) —
(`SYMBOLS\{K03,L07,M06}\LIBRARY-MARKS.SYMB.TXT`, `CARVED-DISC-SUPPORT.md` §6.6).
So the task premise "the marks MACM sets are exactly what the assembled kernel is
conditionally built around" is **only half right**: MACM's marks gate the
**generation stream** (device number, fixed bit); a *separate* set of `8xxx`
marks, fixed when the whole system was assembled, gate the **kernel driver
inclusion**. They describe the same three device classes but are different
symbols at different build stages. **[VERIFIED negative + INFERRED role split]**

---

## 5. Does the kernel read MACM's value, or are they independent? (Task 2)

**Established [VERIFIED / DERIVED]:**
* `SWTYP` is a **runtime variable** (a datafield word), not an assembly constant
  — its only in-code write is the `7→20B` remap (§2.3). Its steady value must be
  planted at generation time. **[VERIFIED — no other `=:SWTYP` exists in the
  NPL; only `044540 20=:SWTYP`.]**
* The value that logically becomes `SWTYP` is MACM's **disc-type code**
  (`ram:833b`), proven by the value identity and the `7↔20B` pair (§2). They are
  **the same axis**, not two independent encodings that merely agree on device
  numbers.
* `MSTYP` itself is **not** the kernel's disc-type number: it is absent from all
  carved symbol tables (`CARVED-DISC-SUPPORT.md` §7), and it is a different,
  smaller axis (§6). What `MSTYP` feeds is the device number and the mark.

**COULD NOT DETERMINE — the exact word that carries `833b` into `SWTYP`'s
datafield.** The `)9BYTT` command planted by the stream passes symbol `F=MSTYP`
as parameter 1 (`distribution-layout-params.txt:128,182`), i.e. it carries
**`MSTYP`**, and `device-geometry.md` verifies "`MSTYP` is NOT set by the stream
… resolved by MACM from the symbol table of the loaded SINTRAN image." I did not
find, in the recovered material, the code that converts the planted disc
selection into the `SWTYP` datafield value:

* the recovered NPL is a **different revision** and contains neither `MSTYP` nor
  any initializer/`DATA` for `SWTYP` (grep: no `SWTYP,` data word, no
  `=:SWTYP` other than the remap);
* the 11-word block MACM copies to the symbol table (`MACM-DIALOGUE.md` §6.5)
  lands in slots whose **names did not decode** (`ram:872d`, that doc's open
  question #2), so the destination symbol of `833b` is not named.

So: *what* crosses the bridge is settled (the disc-type code = `SWTYP`); the
*plumbing* that copies it into the datafield is **NOT FOUND** in the available
binaries/source and would need either the SINTRAN-image symbol table that
defines `MSTYP`, or the `002-S3IMAGE` start-segment carve, to close.

---

## 6. Range reconciliation (Task 3)  **[DERIVED]**

The two axes have **different origins and different spans**; they are not one
axis offset by a constant.

* **`MSTYP` (0..23B, `1` excluded)** is an index into MACM's 20-entry record
  table (`ram:9715`) that groups disc *models* by **controller/driver** — every
  SMD model shares `MSTYP 3..7,15..22` → mark `BD288`, dev `1540`; every
  Winchester model shares `MSTYP 10..14` → mark `W8INC`, dev `500`. Many distinct
  discs collapse onto one `MSTYP` (e.g. `MSTYP 4` = DISC-38/70/75MB). It is a
  *device-class* index. **[VERIFIED from §6.6 groupings.]**
* **`SWTYP`/`DISPN` (kernel-valid 7..36B)** is a **per-geometry** index — one
  value per `DTxxx` record. It is finer: it separates the models `MSTYP` groups
  together (DISC-38MB=21B, DISC-70MB=22B, DISC-75MB=23B all under `MSTYP 4`).

So `SWTYP` distinguishes *more* geometries than `MSTYP` distinguishes *classes*.
The disc-type code (`833b`) is the value that lives on the `SWTYP` axis; MACM
computes both a class index (`MSTYP`) and a geometry index (`833b`) from the one
menu answer.

**The floor `SWTYP >= 7` is a real cut-off, and it explains the "2..24B vs
7..36B" apparent mismatch:** the two oldest CDC discs still in MACM's menu —
`DISC-33MB` (code `2`) and `DISC-66MB` (code `3`) — carry disc-type codes **below
7**. They have **no `DTxxx` record** in any carved kernel and would hit
`ERRFATAL` at `044525` (`IF SWTYP<<7 … ERRFATAL`). MACM still offers them (and
maps them onto the SMD driver via `MSTYP 3`), but an L07/M06/K05 kernel will not
run on them. So `MSTYP`'s axis reaches *older/more* physical types than the
current kernel's `SWTYP` window; the low end (codes 2,3) is legacy dead wood.
**[DERIVED — codes from §2.2, floor from `PH-P2-OPPSTART.NPL:722`.]**

**K05 (18 records) vs L07/M06 (24 records):** K05's `DISPE` omits `DISPN 10..15B`
= disc-type codes 8..13 = **the entire Winchester/ST-506 group**
(`CARVED-DISC-SUPPORT.md` §1.4, §6.7). Those are exactly MACM menu answers
`0,1,2,3,7,12` (DISC-14/21/23/28/45/74MB, all mark `W8INC`, dev `500`). A K05
system therefore cannot be generated for any `W8INC` disc, whereas L07/M06 can —
consistent with K03 lacking the `8WDIS`/`8ZWDI` marks. The SMD group
(codes 15..30, i.e. `SWTYP 17..36B`) and SCSI (`SWTYP 36B`) exist in all three.
**[VERIFIED via the carved `DISPE` pointer arrays.]**

---

## 7. A second, coarser kernel disc-type number: `YSWTY` (1/2/3)  **[VERIFIED]**

There is a *third* number worth recording, because it is easy to confuse with
`SWTYP`. When the kernel authors page 0 at cold start it writes a **coarse
driver-family selector `YSWTY`** into the boot loader
(`PH-P2-OPPSTART.NPL:3757`: `YSWTY, 0  % 1=BDIS; 2=WDIS`; boot-sector fact
`nd-disc-boot` skill: `1=SMD, 2=Winchester, 3=SCSI`). It is **derived from
`SWTYP`** at cold start, not planted — `PH-P2-OPPSTART.NPL:845-865`
**[VERIFIED]**:

```
045547   IF SWPDRIVER="ZBDIS" THEN A:=1         % big-disc (SMD)
045554   ELSE IF A="ZWDIS"    THEN A:=2         % Winchester
045561   ELSE IF A="SCSWD"    THEN ... A:=3      % SCSI
045574   ELSE A:=0
045576   FI;FI;FI; ... STA I (YSWTY            % plant into page 0
```

where `SWPDRIVER` came from `MDISCS(SWTYP)` (§2.1). So the chain is
**`SWTYP` → `MDISCS(SWTYP).SWDDRIVER` → `YSWTY` (1/2/3) → page 0**. `YSWTY` is a
per-family digest of `SWTYP`; it is *not* the MACM `MSTYP` and not the disc-type
code. (Its 1/2/3 = SMD/Winchester/SCSI is the closest thing in the whole system
to the mythical "1/2/3" numbering — see §8 — but it omits DRUM and does not
match the claimed labels.) **[VERIFIED]**

---

## 8. The "Drum=0 / NCR=1 / CDC=2 / Large=3" claim (Task 5) — REFUTED as stated

This numbering appears in `device-geometry.json:16-17` and `device-geometry.md`
(≈line 231) and is repeated with a caveat in `README.md:358`. **In every source
that states it, it is explicitly flagged as *unresolved / not recoverable*, never
verified** — `device-geometry.md`: *"the numeric mass-storage type
(Drum=0 / NCR=1 / CDC=2 / Large=3) cannot be recovered from the generation stream
text, and this tool reports it as unresolved rather than guessing."* **[VERIFIED
that the claim is presented as a guess, not a finding.]**

Testing it against the two real axes:

* **MACM `MSTYP`:** `MSTYP 0 = DRUM` matches "Drum=0". But `MSTYP 1` is the
  **rejected** placeholder (name `?`, record all zero — `MACM-DIALOGUE.md` §2.3),
  not "NCR"; `MSTYP 2 = DISC-10MB (Winchester, REMOV/FIXED)`, not "CDC";
  `MSTYP 3 = SMD`. So beyond the DRUM=0 coincidence the labels do **not** match
  `MSTYP`. **[DERIVED — refutes.]**
* **Kernel `SWTYP`:** range is 7..36B; it has no values 0/1/2/3 that mean
  drum/NCR/CDC/large (codes 2,3 are legacy DISC-33/66MB, §6). **[DERIVED —
  refutes.]**
* **`YSWTY`:** 1/2/3 = SMD/Winchester/SCSI (§7) — a real 1/2/3 axis, but the
  labels are wrong (no NCR/CDC, no drum) and it starts at 1, not 0.

**Conclusion:** the `Drum=0/NCR=1/CDC=2/Large=3` scheme is **not** MACM's
`MSTYP`, **not** the kernel's `SWTYP`, and **not** `YSWTY`. It matches nothing in
these binaries except the isolated fact that DRUM is MSTYP 0. Its origin is a
much older ND mass-storage convention (NCR and CDC name 1970s disc *vendors*, not
anything in these VSX-500 systems); **where it actually comes from is NOT FOUND**
in any binary examined here, and it should be treated as folklore for this
generation. **[VERIFIED negative + INFERRED provenance.]**

---

## 9. Open questions / NOT FOUND

1. **The exact word that copies MACM's disc-type code into the `SWTYP`
   datafield.** §5. Needs the SINTRAN-image symbol table that defines `MSTYP`, or
   the `002-S3IMAGE` start-segment carve. The bridge *value* is proven; the
   *plumbing* is not.
2. **The destination symbol name for `833b`** in MACM's `)9BYTT` symbol-slot
   copy (`ram:872d` names did not decode — `MACM-DIALOGUE.md` open q#2).
3. **The `-1` offset on DISC-450MB-F** (code 26 → `DT450`=DISC-450MB-**S** rather
   than `DT460`=-F). Absorbed by the `ALTFO` runtime format resolution
   [INFERRED]; not separately confirmed.
4. **K05 `MDISCS`** was NOT FOUND in the carved K resident data
   (`CARVED-DISC-SUPPORT.md` §2) — the K-side device-number join rests on the
   `DISPE`/name-table evidence, not on a K `MDISCS`.

---

## 10. Provenance

* MACM binary + Ghidra facts (this session, re-verified):
  `D:\ND\BPUN\MACM-1718L.BPUN` — table `ram:9483` (hexdump), remap `ram:9524-9528`,
  FIXED bits `ram:951c-9523` (all quoted above, read-only; nothing modified).
* MACM side, prior byte-verified analysis:
  `E:\Dev\Ronny\NDInsight\tools\boot-floppy\MACM-DIALOGUE.md` §2.2, §6.
* Kernel side, prior byte-verified carve:
  `E:\Dev\Ronny\NDInsight\tools\boot-floppy\CARVED-DISC-SUPPORT.md` §1, §2, §6, §7.
* Kernel source (naming/logic, cross-checked to carved bytes):
  `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-OPPSTART.NPL`
  (`:722` SWTYP range, `:724` 7→20B, `:728/743/748` MDISCS/DISPE, `:3757` YSWTY,
  `:845-865` YSWTY derivation).
* Symbol tables: `…\SINTRAN\NPL-SOURCE\SYMBOLS\{K03,L07,M06}\SYMBOL-1-LIST.SYMB.TXT`
  (`SWTYP`), `…\LIBRARY-MARKS.SYMB.TXT` (`8BDIS/8WDIS/8SCSI`).
* Generation stream:
  `…\tools\boot-floppy\versions\L-VSX-500-07\inputs\distribution-layout-params.txt`
  (`:128` F=MSTYP, `:132-143` mark-gated G/FR, `:182` )9BYTT).
* "Drum/NCR/CDC/Large" claim sources: `…\tools\boot-floppy\device-geometry.md`,
  `device-geometry.json`, `README.md`.
* Join/derivation tool written for this analysis:
  `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\mstyp_swtyp_join.py`.

Nothing under `D:\ND\` or any carved binary was modified. In Ghidra only reads
were performed this session. This document was the only file written.
