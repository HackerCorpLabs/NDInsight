# SINTRAN III Output-Spooling Internals - Carved (byte-verified) Findings

**Version under analysis:** L-VSX-500 (L07). K-VSX-500 / M-VSX-500 used for segment-presence comparison.
**Carve tree:** [`../../tools/sintran-segment-carver/versions/L-VSX-500/segments/`](../../tools/sintran-segment-carver/versions/L-VSX-500/segments/)
**Symbols:** [`../NPL-SOURCE/SYMBOLS/L07/`](../NPL-SOURCE/SYMBOLS/L07/) (SYMBOL-1-LIST, SYMBOL-2-LIST, l07-kallsyms.txt)
**Method:** static RE per the `sintran-carving` skill. Carved big-endian bytes = ground truth; `nd100-dis`
after byte-swap. Every address below reproduced from bytes with `dd`/python before publishing.

Conventions in this document: **octal** for all SINTRAN addresses; **hex** for any Ghidra base;
memory in **words** (bytes in parens); ASCII only. "VERIFIED" = read from carved bytes myself.
"DOCUMENTED" = from an official manual, not (yet) proven in these bytes. Anything not established is
marked **NOT RESOLVED**.

---

## 0. Summary table (what each piece is)

| Piece | Segment (load base) | Role | Status |
|---|---|---|---|
| **Queue manager** (add/dequeue file, page-pool wait/wake) | **006-S3FS** (base 26000B) | MON-call workers APSPF/SPCLO/RSPQE + the queue list handlers + the page-pool reserve/release | **VERIFIED** (MCTAB slots, entry disassembly, wait/wake sequence) |
| **Spooler control-block table** | `SPTAB=122562B .. ENDSP=123034B`, stride `SPLEN=12B` (10 words), ~17 slots | one control block per spooler (per device): `SPROG=+1`, `SPERI=+2` peripheral LDN, `SPINX=+10B` spool index | **VERIFIED** (symbols + NPL stride) |
| **Per-file spooling queue** | dedicated queue segment, VA `150000B-177777B`; list head `LSPOQ=162122B` | linked records keyed by file/job/user, carry page count `SPAGE=+31B` | **VERIFIED** (offsets + RSPQE walk) |
| **Spooling datafield SAVE area** | **031-S3SSPD** (base 164000B; K/L/M) | saved copy of each `SPPRx` (SPRTx) spooling-program datafield | **VERIFIED** (QSEMAPHORE/QIOSEMAPHORE anchor) |
| **Physical line-printer driver** | per-device IOX cells in **044-S3IDPIT** / **053-S3SDPIT** (base 4000B) + device-dependent `HEAPR`/`TRAPR`/`SPRIN` | executes IOX 430B..433B against the printer | **VERIFIED** (IOX cells) |
| **Remote (COSMOS) spooling** | **137-COSPOOL** (L only) holds `COSPO` RT program | network/remote printing via XMSG; NOT local output spooling | role DOCUMENTED, byte-consistent |
| **Background spooling program body** (the `SPOOL` routine SPRTx runs) | NOT located in bytes | 120-S3SPRMA is an L/M spooling segment but its content is not overlay-proven | **NOT RESOLVED** |

Key correction to the framing of the request: the **spooling QUEUE MANAGER is not in 031-S3SSPD nor
137-COSPOOL - it is in the file-system segment 006-S3FS** (the MON-call workers + queue handlers +
page-pool semaphore). 031-S3SSPD is a datafield *save* area (data, not code). 137-COSPOOL is COSMOS
*remote* spooling, a separate subsystem.

**POISONED PRIOR corrected in this document (see section 1.2):** `SPOOL=147510B` is the buffer/page
**pool gate**, NOT the spooling program body. An earlier draft here decoded 147510B as code in the
120-S3SPRMA overlay and called it the program - that was the overlay trap (symbol in a resident data
region). The spooling-program body location is now marked NOT RESOLVED.

---

## 1. Segment roles: 031-S3SSPD, 137-COSPOOL, 120-S3SPRMA

### 1.1 031-S3SSPD - "Save of spooling data fields" (K/L/M) - VERIFIED as DATA

- Meta: [`031-S3SSPD.meta.json`](../../tools/sintran-segment-carver/versions/L-VSX-500/segments/031-S3SSPD.meta.json)
  says *"Save of spooling data fields"*, 1 page = 1024 words (2KB), load base **164000B** (0xE800).
- Present in **K, L and M** (verified by directory listing).
- Content is **data, not code**: word[0]=164000B (self/load-address marker), word[1]=177777B,
  word[2]=165123B, then a long run of zeros, then a small pointer/value table from 164047B on.
- **It holds saved `SPPRx` spooling-program datafields.** Byte-anchored against the documented
  `SPPRx` layout (ND-60.112.01 Data Fields, Data Field Layout "SLPx / SPPRx"):
  - 164047B = **1136B** = `QSEMAPHORE` (SPPRx rel +4)  -- VERIFIED
  - 164050B = **1137B** = `QIOSEMAPHORE` (SPPRx rel +5)  -- VERIFIED
  - 164054B/164055B/164056B = 172330B / 172646B / 171675B = saved device-dependent routine addresses
    (`TRAILER`/`PRINTBUFFER`/`FILENUMBER` region).
  This two-value semaphore anchor (1136B,1137B adjacent) matches the manual's rel+4/rel+5 exactly,
  which is what pins 031-S3SSPD to the SPPRx datafield save.
- This is the `<save-area?>` operand referenced by `*SET-SPOOLING-DEVICE-NUMBER` (see section 3).

### 1.2 SPOOL=147510B is the buffer/PAGE-POOL gate (VERIFIED) - not the program body

- `SPOOL=147510B`, `RPOOL=147511B`, `NPOOL=147512B`, `BPOOL=147715B` are a group of adjacent
  **buffer/page-pool cells** (from SYMBOL-1-LIST / FILSYS-SYMBOLS). `SPOOL` is the free-buffer gate:
  NPL `CC-P2-COMMON.NPL:431` reads `IF SPOOL=0 THEN % NO BUFFERS FREE` (verified in that source).
  They are grouped as pool cells in the 006-S3FS reference table at 114745B-114752B
  (BPOOL,NPOOL,-,BPOOL,SPOOL,RPOOL), and the DPIT relocation table places `{RPOOL,SPOOL}` directly
  beside the wait/reserve/release primitives `{WAITF,RESRV,RELES}` (section 4.3). Resident pointer
  `SSPOO=004274B` holds **147510B** = a pointer to the SPOOL pool cell (VERIFIED), i.e. it points at
  the pool head, not at a program.
- **Correction / poisoned prior:** an earlier draft of this file read 147510B as executable code in
  the 120-S3SPRMA overlay and called it "the spooling program body". That was the classic overlay
  trap (skill traps 4 and 8): a symbol that names a resident *data* cell also decodes as unrelated
  code in a segment that happens to span the same virtual address. `SPOOL=147510B` is the pool gate.
- **120-S3SPRMA** is present in **L and M** and **absent in K** (verified by listing) - so it is a
  spooling segment introduced at the L/M generation - but its exact contents are **not** byte-proven
  here, and the manual's `SPPRx` rel-0 label "SPOLPROGRAM = SPOOL" is a datafield-layout label, not
  proof that L07 symbol `SPOOL` is the program entry.
- **NOT RESOLVED:** the byte location of the common spooling-program routine that the `SPRTx` RT
  programs execute.

### 1.3 137-COSPOOL - COSMOS REMOTE spooling (L only) - role DOCUMENTED, byte-consistent

- Present in **L only** (verified). Meta has **no load address** and no description; 18 pages
  (18432 words); flag 000100B.
- **Contains no local device I/O:** scanning all 18432 words found **zero IOX (164xxx) and zero MON
  (1534xx) instruction words** - VERIFIED. So it is neither the physical printer driver nor a
  MON-issuing background program in the local sense.
- Disassembled (base 0) it is real code that manipulates the paging control register
  (`TRR PCR`, `IOF`/`ION`, `BSET ... SSPTM`) - i.e. it installs/uses its own page table.
- **Role (DOCUMENTED):** segment `COSPOOL` holds `COSPO`, the **COSMOS SPOOLING** RT program =
  *remote* spooling (printing to printers on other systems over XMSG). Sources:
  [`../../Installation/Communication/COSMOS Basic/COSMOS-Basic-Install-Guide.md`](../../Installation/Communication/COSMOS%20Basic/COSMOS-Basic-Install-Guide.md)
  lines 197-198 ("chain `COS-COSP-VSX-E02:MODE` (loads COSPO into segment COSPOOL, `SET-PAGE-TABLE 1`)")
  and 215 ("`RTON COSPO`; `START-SPOOLING COSMOS-SPOOLING`"). The `SET-PAGE-TABLE 1` step matches the
  byte-observed paging-register code above.
- **This is a separate subsystem from local output spooling** and does not implement the local
  print-queue manager, the local printer driver, or the SPRTx local scheduler.

---

## 2. MON calls APSPF (240B) and SPCLO (40B) - dispatch + workers + register conventions

### 2.1 Dispatch (via the MCTAB model) - VERIFIED

Per the byte-verified MON dispatch model (`MON N -> ENT14 -> GOTAB[N]=MFELL -> CALLP -> MCTAB[N]`),
the worker address is `MCTAB[N]`, MCTAB=`9MCTA`=**005620B** in segment **044-S3IDPIT** (base 4000B).
MCTAB validated against known slots before use (skill trap 2): `MCTAB[5B]=102021B` (RDISK),
`MCTAB[144B]=026354B` (MAGTP) - both correct.

| MON | Name | MCTAB slot (addr) | Worker | Worker seg | Status |
|---|---|---|---|---|---|
| **240B** | AppendSpooling `APSPF` | MCTAB[240B] @ **006060B** = **106307B** | `APSPF=106307B` | 006-S3FS | VERIFIED |
| **40B** | CloseSpoolingFile `SPCLO` | MCTAB[40B] @ **005660B** = **067572B** | `SPCLO=067572B` | 006-S3FS | VERIFIED |
| **55B** | GetSpoolingEntry (`RSPQE`) | MCTAB[55B] @ **005675B** = **106212B** | (no L07 symbol) | 006-S3FS | VERIFIED (worker word) |

`APSPF=106307B` and `SPCLO=067572B` also match the symbol tables (l07-kallsyms: `APSPF=0x8CC7`=106307B,
`SPCLO=0x6F7A`=067572B). All three workers sit in the file-system segment **006-S3FS** (base 26000B,
which spans 26000B..177777B); GetSpoolingEntry's worker (106212B) is right below APSPF (106307B) -
one clustered spooling-queue-manager region.

### 2.2 Worker entry points - VERIFIED from bytes

- **SPCLO @ 067572B** begins `021131 STD I 131` (save link), then (067577B..067617B) sets a
  print/condition flag in `,B 42`, then at 067620B..067630B does
  `SAA -1 / SKP IF DT UEQ SA` and `SAA -2 / SKP IF DT UEQ SA` and `SKP IF DT UEQ 0` -
  i.e. it tests the **T register** (`DT`) against -1/-2/0. This byte-confirms **T = file number**
  (the SPCLO "File Number" parameter). VERIFIED.
- **APSPF @ 106307B** begins `021046 STD I 46` (save the incoming **D** register = the UserText
  pointer), then calls helper routines (`JPL I 43 -> 106356`, `JPL I 40 -> 106360`, ...),
  and at 106363B issues `IOT 3233`. Entry + D-save VERIFIED.

### 2.3 Register / parameter conventions - cross-checked vs the Monitor Calls manual

Source: [`../../Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md`](../../Reference-Manuals/ND-860228-2-EN%20SINTRAN%20III%20Monitor%20Calls.md).

**AppendSpooling (MON 240B), MAC calling sequence (manual p.79, lines 3196-3213):**
- **X** = address of file name (to be printed; name may be abbreviated)
- **T** = number of copies in bits 0:14; **bit 15 = 1** means "also print the UserText message"
- **A** = address of UserText message, then `COPY SA DO` so **D** = message address
- **A** = address of the spooling-device (printer) peripheral-file name (loaded last, live at MON)
- Return: **A** = standard error code (0 = OK).
- Byte cross-check: worker saves **D** first (`STD I 46`) = the message pointer per the convention.

**CloseSpoolingFile (MON 40B), MAC calling sequence (manual p.118-119, lines 5013-5026):**
- **T** = file number (from the earlier open)
- **A** = number of copies, then `COPY SA D0` so **D** = number of copies
- **A** = condition/print flag (0 = only print text if @DEFINE-SPOOLING-CONDITIONS requires it;
  non-zero = print unconditionally)
- **X** = address of the error-device text
- Return: **A** = standard error code.
- Semantics: if the file is not a spooling file, a normal close is performed (manual line 4921).
- Byte cross-check: worker tests **T** (`DT`) = the file-number parameter. VERIFIED.

---

## 3. The *SET-SPOOLING-DEVICE-NUMBER "spooling index" and the peripheral-file "versions" mechanism

These are **two different structures** that meet at a device number.

### 3.1 The spooling index -> the spooling-programs table (DOCUMENTED)

- `*SET-SPOOLING-DEVICE-NUMBER <spooling index> <logical device number> (<memory?>) (<image?>) (<save-area?>)`
  is a **SINTRAN Service command, not a monitor call** (so it is absent from the Monitor Calls manual).
  Source: [`../../Operations/SINTRAN/ND-30.003.007 EN SINTRAN III System Supervisor.md`](../../Operations/SINTRAN/ND-30.003.007%20EN%20SINTRAN%20III%20System%20Supervisor.md)
  lines 9926-9962, 9984.
- **The spooling index is the identity of a spooling PROGRAM.** SINTRAN is generated with N
  system-included RT-programs called **`SPRTx`**, where x = the spooling index. `SPRT1` = index 1,
  `SPRT2` = index 2, ... indices run from 1 upward. Source:
  [`../../Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md`](../../Reference-Manuals/ND-60.128.5%20EN%20SINTRAN%20III%20Reference%20Manual.md)
  line 10837.
- So the index **indexes into the parallel spooling tables**: the "spooling device numbers" table
  (index -> the printer's logical device number that this SPRTx serves) and the "define printer type"
  table (same index -> printer-type). The command writes the LDN (and the three segment numbers
  `<memory?>` = the in-memory datafield, `<image?>` = the datafield image on the segment file,
  `<save-area?>` = the save-area segment = **031-S3SSPD**) into entry [spooling index].
  Source lines 9956 ("The index in this table now corresponds to the spooling index") and 9941.
- The `SPRTx` runtime datafield (`SPPRx`, see section 1.1 / 4) carries the effective binding:
  rel +2 `DEVNO` = peripheral logical device number, rel +3 `QSEGMENTS` = the queue segment number,
  rel +4/+5 = the queue / queue-I/O semaphores.

**Byte status of the "spooling device numbers" table itself: NOT RESOLVED.** I did not byte-locate the
resident table that `*SET-SPOOLING-DEVICE-NUMBER` writes. Candidate resident data cells exist
(`SPQSS=005117B`, `PAGPN=005223B`, `SSPOO=004274B`) but the table's own address/stride is not proven
here. (The byte-verified spooler tables that DO exist are `SPTAB=122562B` with `SPINX=+10B`, and the
per-file queue off `LSPOQ=162122B` - see section 4.1.) What is verified: `SSPOO=004274B` = 147510B
(= `SPOOL`, the buffer-pool gate, section 1.2), and the save-area segment is 031-S3SSPD.

### 3.2 The peripheral-file "versions" mechanism (DOCUMENTED)

Source: [`../../Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md`](../../Reference-Manuals/ND-60.050.06%20SINTRAN%20III%20Users%20Guide.md)
lines 3822-3844.

- "A peripheral file may be created in more versions than the existing number of corresponding
  peripherals. **All versions of the file not connected to a device number will be treated as
  spooling files.**"
- Worked example (manual):
  - `@CREATE-FILE LINE-PRINTER;10,0` -> ten versions of file LINE-PRINTER.
  - `@SET-PERIPHERAL-FILE LINE-PRINTER, 5` -> **version 1** is bound to **device number 5** (the real
    printer). Versions 2..10 have **no device number** -> they are **spooling files**.
  - `@START-SPOOLING LINE-PRINTER` starts output spooling if a spooling program was generated for
    device 5.
- The connection between the two structures: the peripheral file's **device number** (5 in the
  example) is the same LDN that a `SPRTx`/spooling-index entry serves (its `DEVNO`). When a user opens
  the peripheral-file *name*, an unbound (device-less) version is used as the spooling file; on
  close/append (APSPF/SPCLO) it is queued to the spooling program serving that device.
- The device number lives in the file-system **object entry**: LDN in bits 0-11, unit number in bits
  12-15 (Monitor Calls manual line 17962). A version with no device number = spooling file.

---

## 4. The spooling QUEUE data structure + "currently printing not in queue" + page-pool wait/wake

### 4.1 Two distinct records - VERIFIED (symbols + NPL stride + disassembly)

There are **two** structures, not one:

**(a) Per-spooler control block** - array `SPTAB=122562B .. ENDSP=123034B`, stride
`SPLEN=12B` (10 words). `(ENDSP-SPTAB)/SPLEN = 252B / 12B = ~17` spooler slots. One block per
*device* (spooler), NOT per queued file. Field offsets (FILSYS-SYMBOLS, VERIFIED):
`SPROG=+1` (the spooler RT-program ref), `SPERI=+2` (peripheral logical device number; set to -1 to
disable a spooler), `SPMOD=+30B`, `SPAGE=+31B`, `SPFNA=+35B`, `SPINX=+10B` (spool index). The stride
and iteration `X := X + SPLEN` are corroborated in NPL (`PH-P2-OPPSTART.NPL`, `RP-P2-CONFG.NPL`;
logic only).

**(b) Per-file spooling-queue ENTRY** - a linked record in a **dedicated spooling-queue segment**
whose VA window is `150000B-177777B` (`PH-P2-OPPSTART.NPL:1374` "ADDR 150000-177777 IS AVAILABLE FOR
SPOOL.QUEUE"; size = config param `SPQSSIZE`). List head / anchor `LSPOQ=162122B` (referenced by
pointer from 003-S3CP and the user segments 110/111/112/113). Field offsets in the entry (VERIFIED
symbols): `SPMOD=+30B` mode/flags, `SPAGE=+31B` page count of the queued file, `SPFNA=+35B` file
name, `SPUME=+71B` user, 3-word job name `SPJN1/2/3=+151B/152B/153B`, `SPMES=+161B` message text.
The record is `>= 114B` words - far larger than an SPTAB element, confirming these are different
tables. `RSPQE` (release spool-queue entry, worker of MON 55B `GetSpoolingEntry`) at
**006-S3FS 106212B** walks the chain via `LDA ,X 17 / LDX ,X 31` (offset 31B = `SPAGE`). Other
resident queue handlers: `GSPQB=034267B`/`RSPQB=034305B` (get/return queue block), `MSPQE=165567B`.
So a queue entry is a dedicated linked record in a separate spool-queue segment, keyed by
file/job/user and carrying its page count - it is **NOT** stored in the normal open-file object
tables.

**API view of the same entry:** at the MON boundary GetSpoolingEntry (MON 55B) presents a
**272-byte record** (Monitor Calls manual lines 12278-12291): bytes 0:1 copies, 2:3 apostrophe flag,
4:97 spooling-file name, 98:255 error-device message. This is the caller-facing projection of the
in-segment entry above.

### 4.2 "The file currently printing is NOT in the queue"

- API/DOCUMENTED: GetSpoolingEntry (MON 55B) "gets the next spooling queue entry ... **The entry is
  removed from the spooling queue**" (manual line 12280). The spooler pulls the head into its private
  SPTAB control block and prints from there, so it is no longer on the `LSPOQ` list.
- Building blocks VERIFIED: the queue is a linked list off `LSPOQ=162122B`; the resident has generic
  list insert/remove primitives (`XINSP`/`XREMP`/`YINSP`/`YREMP`, 006-S3FS ~034410B-034451B) and the
  release worker `RSPQE=106212B`.
- **NOT RESOLVED (byte level):** the exact instruction sequence that unlinks the current entry from
  `LSPOQ` into the spooler's control block was not traced. The removal is manual-documented and
  structurally consistent, but not byte-proven here.

### 4.3 Page-pool wait/wake (default 500 pages) - VERIFIED

The wait/wake is a **SINTRAN semaphore reserve/release**, byte-verified in **006-S3FS at
171001B-171013B** (all words re-read from bytes in this session):

```
171001  044011  LDA 11           ; A := word@171012 = SPSEM pointer
171002  153122  MON 122          ; 2RESR = RESERVE semaphore -> BLOCKS caller (page-pool WAIT)
171003  130002  JAP 2
171004  135007  JPL I 7   -> 171013   ; error path
171005  044005  LDA 5            ; A := SPSEM pointer
171006  153123  MON 123          ; 2RELE = RELEASE semaphore -> WAKES first waiter
171007  130002  JAP 2
171010  135003  JPL I 3   -> 171013
171011  153000  MON 0            ; LEAVE
171012  164607  <data>           ; = SPSEM pointer (164607B)
```

- `2RESR=122B` and `2RELE=123B` are VERIFIED symbols (FILSYS-SYMBOLS / SYMBOL-1-LIST), and the words
  `153122B`/`153123B`/`153000B`/`164607B` at 171002B/171006B/171011B/171012B were verified directly
  from the carved bytes. `MON 122B` reserving a held semaphore puts the caller in the semaphore's
  waiting queue; `MON 123B` (RELES) removes the first waiter and inserts it into the execution queue
  (semantics: [`../OS/21-SEMAPHORES-EXPLAINED.md`](../OS/21-SEMAPHORES-EXPLAINED.md)). That is
  exactly "writer blocks when the pool is empty / spooler wakes it when pages are freed".
- The semaphore is the spool-semaphore cluster `FSPSM=164601B`, `SPSEM=164607B`, `USPSM=164612B` in
  the spool-queue segment.
- **Pool counter cells:** `SPOOL=147510B` (free-buffer gate; NPL `CC-P2-COMMON.NPL:431`
  `IF SPOOL=0 THEN % NO BUFFERS FREE`), `RPOOL=147511B`, `NPOOL=147512B`, `BPOOL=147715B`.
  Independent corroboration that the pool accounting sits next to the wait primitives: the
  044-S3IDPIT / 053-S3SDPIT relocation table bundles `RPOOL(147511B)` / `SPOOL(147510B)` immediately
  beside `WAITF=043717B` / `RESRV=037103B` / `RELES=037156B`.
- `PAGPN=005223B` reads **0** in the static 044-S3IDPIT image (VERIFIED) - consistent with the
  500-page default being a *generation/warm-start* configuration value, not baked into the carve.
- **NOT RESOLVED (byte level):** the precise `SPOOL` decrement-on-allocate -> fall-into-`RESRV`/block,
  and the spooler's increment -> `RELES` ordering was not traced end to end. The wait/wake *primitive*
  (171001B-171013B) and the pool *cells* are byte-verified; the exact decrement/increment sequence
  around them is inferred from the co-located pointer table + `CC-P2-COMMON` logic.

---

## 5. Line-printer physical driver - IOX sequence for device 430B (CDC 9380)

### 5.1 Manual register map (CDC 9380) - DOCUMENTED

Source: [`../../Reference-Manuals/ND-06.016.01_NORD-100_Input_Output_System.md`](../../Reference-Manuals/ND-06.016.01_NORD-100_Input_Output_System.md)
Appendix B (section B.3). Standard device numbers **0430B-0433B**, interrupt level 10 (dec), ident 3.

| IOX offset | Dir | Function |
|---|---|---|
| DEV+0 = IOX 430 | Read | Read Data Word (read back buffer; test mode) |
| DEV+1 = IOX 431 | Write | Write Data Word (character into buffer) |
| DEV+2 = IOX 432 | Read | Read Status Word |
| DEV+3 = IOX 433 | Write | Write Control Word |

Write Control (+3) bits: b0 int-enable-on-ready, b1 int-enable-on-error, b2 activate (print buffered
char), b3 test, b4 device+interface clear. Read Status (+2) bits: b3 ready-for-transfer, b4 error,
b5 not-ready, b6 out-of-paper, etc. (**Note:** the task hint "data register = 1" matches DEV+1 =
Write Data; but per the manual DEV+0 is Read Data and DEV+2 is Read Status, not "+0 = status".)

### 5.2 Carved driver IOX cells - VERIFIED

The device number 430B is baked into IOX-instruction cells inside the line-printer **device datafield**
(DPIT), found identically in both DPIT copies (base 4000B):
[`044-S3IDPIT.bin`](../../tools/sintran-segment-carver/versions/L-VSX-500/segments/044-S3IDPIT.bin)
("Image of DPIT") and
[`053-S3SDPIT.bin`](../../tools/sintran-segment-carver/versions/L-VSX-500/segments/053-S3SDPIT.bin)
("Save of DPIT"). Raw big-endian words verified directly:

**LP1 (device 430B):**

| VA (oct) | Word (oct) | Instruction | Manual meaning |
|---|---|---|---|
| 106770 | 164433 | IOX 433 | LP1 Write Control |
| 106771 | 164432 | IOX 432 | LP1 Read Status |
| 106772 | 164431 | IOX 431 | LP1 Write Data |
| 107003 | 164430 | IOX 430 | LP1 Read Data |

**LP2 (device 434B), parallel block +346B words later:**

| VA (oct) | Word (oct) | Instruction | Manual meaning |
|---|---|---|---|
| 107336 | 164437 | IOX 437 | LP2 Write Control |
| 107337 | 164436 | IOX 436 | LP2 Read Status |
| 107340 | 164435 | IOX 435 | LP2 Write Data |
| 107351 | 164434 | IOX 434 | LP2 Read Data |

Structural coherence: LP2 is an exact copy of the LP1 template shifted +346B words; both have a
contiguous Control/Status/WriteData triple then a Read-Data cell +13B words later
(107003-106770 = 107351-107336 = 13B). This is the standard SINTRAN pattern: the common line-printer
driver code executes these per-device IOX cells in place, so the device number lives in the datafield,
not in shared code. (All eight words re-verified against raw bytes in this session.)

**NOT RESOLVED:** the generic (shared) line-printer driver *code body* that dispatches/executes these
IOX cells was not located as a distinct L07 symbol (the DPIT cells are the byte-anchored artifact).

---

## 6. Resolved vs. open

**Resolved (byte-verified):**
- MON 240B `APSPF` and MON 40B `SPCLO` dispatch via MCTAB (240B@006060B=106307B, 40B@005660B=067572B),
  plus MON 55B `GetSpoolingEntry` worker @005675B=106212B; all workers in 006-S3FS; entries
  disassembled; SPCLO T=file-number and APSPF D=message-ptr confirmed in code.
- 031-S3SSPD = SPPRx spooling-datafield SAVE area (QSEMAPHORE=1136B/QIOSEMAPHORE=1137B anchor);
  present K/L/M.
- 137-COSPOOL = no IOX / no MON, paging-register code = COSMOS remote-spooling `COSPO` (L only,
  documented + byte-consistent).
- Queue data structures: per-spooler control block `SPTAB=122562B..ENDSP=123034B` stride `SPLEN=12B`
  (~17 slots, fields SPROG/SPERI/SPINX/SPAGE); per-file queue = linked records in the queue segment
  VA 150000B-177777B off head `LSPOQ=162122B`, fields SPMOD/SPAGE/SPFNA/SPUME/SPJNx/SPMES; walked by
  `RSPQE=106212B`.
- Page-pool wait/wake = semaphore reserve/release at 006-S3FS 171001B-171013B (MON 122B `2RESR` block
  / MON 123B `2RELE` wake on `SPSEM=164607B`); pool gate `SPOOL=147510B` (=0 => no buffers free).
- Line-printer IOX cells for device 430B (and 434B) in DPIT (044/053), mapped to the CDC 9380 manual
  register set.

**Open / NOT RESOLVED:**
- The resident "spooling device numbers" table that `*SET-SPOOLING-DEVICE-NUMBER` writes (distinct
  from the byte-verified `SPTAB`; the command's exact target/stride is unconfirmed) - section 3.1.
- The byte location of the common spooling-program routine (`SPOOL` label is the pool gate, NOT the
  program; 120-S3SPRMA content not overlay-proven) - sections 1.2, 0.
- The exact `SPOOL` decrement->block (writer) and increment->wake (spooler) instruction ordering
  around the verified reserve/release primitive - section 4.3.
- The byte-level unlink of the currently-printing entry from `LSPOQ` - section 4.2.
- The generic line-printer driver code body that runs the DPIT IOX cells - section 5.2.

---

*Cross-checked manuals:* Monitor Calls (ND-860228-2), NORD-100 I/O System (ND-06.016.01) App B,
System Supervisor (ND-30.003.007), Users Guide (ND-60.050.06), Reference Manual (ND-60.128.5),
Data Fields (ND-60.112.01), COSMOS Basic install guide.
