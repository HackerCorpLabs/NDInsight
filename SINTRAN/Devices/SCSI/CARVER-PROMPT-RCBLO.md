# Carver prompt: carve RCBLO and close the ENTER-DIRECTORY -> SCSI gap

**Full path:** `SINTRAN/Devices/SCSI/CARVER-PROMPT-RCBLO.md`

Hand the text below (everything from "MISSION" down) to the carving session. It is self-contained.

---

## MISSION (one line)

Carve and analyse **`RCBLO`** and the resident disk-driver overlay that turns a filesystem
"read page 0" request into a call to the SCSI driver entry `SCLLD` - and find why, during
`@ENTER-DIRECTORY,,DISC-SCSI-1,0`, **that call is never made**.

Use the `sintran-carving` skill. Running system = **L-VSX-500**, symbol set = **L07**.

---

## 1. GROUND TRUTH - VERIFIED THIS SESSION. DO NOT RE-LITIGATE ANY OF IT.

Every layer below the gap has been checked and is CORRECT. The bug is NOT in any of them.

### 1.1 What happens on the wire (from the device trace)
`@ENTER-DIRECTORY,,DISC-SCSI-1,0` produces exactly three CDBs, then silence:
1. `INQUIRY` (0x12) - completes.
2. `READ CAPACITY` (0x25) - completes, returns **blockSize=1024, lastLBA=129311**.
3. `READ_6` (0x08) of **LBA 129311** - completes, 1024 bytes DMA'd to ND-100 memory.
4. Target disconnects. Driver idles correctly.
5. **NOTHING FURTHER. Page 0 / block 0 is NEVER read.** Console prints
   `APPROACHING END OF ACCOUNTING FILE` (a known non-fatal, coincidental warning) and returns to `@`.

**Critical nuance: this is not a wrong-LBA bug. No read is issued AT ALL.** Whatever aborts, aborts
*before* any further transfer is requested.

### 1.2 The SCSI controller emulation is CORRECT (verified)
- The final bare `WCONT=0x0005` is `SCINT`'s unconditional exit re-arm
  (`IP-P2-SCSI-DRIV.NPL` line 187: `5\/SCCCW; T:=HDEV+WCONT; *IOXT` then `GO SCWTI`, with
  `SCCCW=0`). It is not a request for work. Silence after it is correct behaviour.
- Do NOT "fix" `ExecuteGo()` to raise an interrupt. That would be a bug.

### 1.3 The SCSI driver (`IP-P2-SCSI-DRIV.NPL`) is EXONERATED (verified)
- On the disconnect it takes `A=4` -> `DCTHR` -> `TEROP` (sets **T=0 = success**) -> `GO FAR BUSFP`
  -> `SELEC`. `SELEC` finds the arbitration queue `SCWAQ` **empty**, writes `BUSFL=0` and a single
  `WCONT=5`, returns. Control falls into `EXDRI` -> `A+2=:P` = the **normal/success** return.
- Trace signatures confirming this: exactly ONE `WCONT=5` (the `GO FAR` skips line 187), and NO
  `WDESI`/`WNCOM`/transfer-counter writes (a non-empty queue would have emitted them).
- **The driver never initiates work.** Work enters ONLY via `SCLLD` -> `INITO` -> `SCWAQ`.
  All `SCLLD` callers are in `IP-P2-SCSI-DISK.NPL` lines 116, 445, 1140, 1191, 1364, 1383, 1412, 1413.
- The driver never tests BSY/REQ/ACK (`SCREQ` is defined at line 32 and used ZERO times), never reads
  RSTAU bits 7/15 (transceiver type), never tests bit 6. Observed RSTAU values (0x0208/0x3208/0x5208)
  all have bit 4 (Error) and bit 5 (Bus reset) CLEAR, so the error pre-filter at lines 124-131 was
  skipped entirely. **"The bus was in a bad state" is RULED OUT.**

### 1.4 The DMA is byte-exact (verified from the trace's own logging)
```
totalWritten=1  MAR=0x04C600  first16=08     <- byte 0 -> HIGH byte, no MAR increment
totalWritten=2  MAR=0x04C601  first16=00     <- byte 1 -> LOW byte, MAR increments
totalWritten=3  MAR=0x04C601  first16=54
totalWritten=4  MAR=0x04C602  first16=D9
```
ND-100 memory at `0x04C600` = `0x0800`, `0x04C601` = `0x54D9`, `0x04C602` = `0x8000` - identical to
the disk image. Big-endian, correct. `totalWritten` reaching exactly 1024 proves the counter was
reset before the data phase. **Byte-order bugs are RULED OUT.**

### 1.5 The disk is GOOD (verified by reading the image directly)
Image: `E:\Dev\Ronny\RetroFS\demo\test-images\ndfs\tor-disk.img` (132,415,488 bytes = 129312 x 1024).
LBA 129311 first 32 bytes: `08 00 54 D9 80 00 00 00 00 00 00 00 00 01 DC D8 C0 00 00 00 00 01 F9 1F ...`
- `word0 = 0x0800` -> **NPART = 8**
- XOR of all 512 words = **0**
- `w10:w11 = 0x0001F91F` = **129311** (the record states the disk's own last block - self-consistent,
  and proves the image stores words BIG-ENDIAN)

### 1.6 The disk layer's function 42 SUCCEEDS (verified by walking the NPL with real values)
Source: `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`
- The last-block read is **NOT a geometry probe**. Line 364 says `% ADDRESS OF CONTROLL RECORD`.
  SINTRAN reads the LAST 1024-byte page because that is where the **control record** lives.
- Lines 366-367 (`X.MEMAD; T:=1000; D+T; A:=A+C` / `AD=:X.MEMAD`) advance `MEMA1:MEMA2` by 0o1000
  (512) words, so the validation at line 391 reads exactly where the DMA landed (`0x04C600`). Correct.
- Line 397 `IF D><0 OR L<=2 OR L>"NCOPA" THEN T:=NOCRC; GO FAR RETEX` - with `D=0`, `L=8`,
  `NCOPA=0o12=10`: `0><0` false, `8<=2` false, `8>10` false. **PASSES.**
- Lines 400-405 then copy `NPART*6` = 48 words of partition table to `CMADR` and set
  **`UHLIM = 0x0001DCD8 = 121,560`** (data-area size, from word 4 of the partition table).
  Line 408: `T:=0` = success.
- The `INQUI` arithmetic (lines 1222-1290) is clean for blockSize=1024: line 1253
  `IF A><0 GO FAR RSZER % TO BIG` does not fire (A=0, only fires if blockSize >= 65536); the
  power-of-2 loop exits with A=1 so line 1280 does not fire. **All three `RSZER` exits are passed.**

### 1.7 MYTHS - do not repeat these, they are FALSE
- **`ECAPD` does not exist in any SCSI source (zero occurrences).** It is a TERMINAL driver routine
  (`MP-P2-TERM-DRIV.NPL:144`: `ECAPD: A=:D; T:=HDEV+DST; *IOXT % READ STATUS`). Several docs claim an
  "ECAPD capacity check" rejected a capacity value. There is no such check. Ignore it entirely.
- **Reporting a different capacity cannot fix this.** The real capacity (`UHLIM`) comes from INSIDE
  the on-disk control record (line 402), never from READ CAPACITY. Capacity only selects WHICH block
  is read as the control record. `122071` and raw `129311` both failed - as this model predicts.
- **The ND-3201's Z80 firmware (`45900E.bin`) is NOT in the SCSI path.** The board is two independent
  halves: Z80 + AM9517 + FD1797 = FLOPPY; the NCR 5386 is hardware-decoded onto the ND-100 IOX bus and
  driven by SINTRAN. Verified by an exhaustive I/O port sweep: the Z80 never touches the NCR. Do not
  reverse-engineer that ROM for this bug. Details: `SINTRAN/Devices/SCSI/nd-scsi-3201.md`.

---

## 2. THE GAP - THIS IS WHAT YOU ARE CARVING

Existing carve: `SINTRAN/Filesystem/code-logic/enter-directory.md` (501 lines)
covers ENTER-DIRECTORY end-to-end: command dispatch -> unit reserve + directory datafield -> the
page-0 read (`RXDIR` -> `RCBLO`) -> master-block parse/validation (`CHDSI`) -> success/error exits.

**But it declares its own boundary OPEN.** Quoting that document:
- line 268: `RCBLO` - the "Octobus / SCSI **driver transfer primitive**" - is a
  **"resident overlay, not part of [the carved] segment"**.
- line 290: `| Driver function code / DMA count / SCSI opcode+LBA | set inside the resident driver |
  **OPEN - uncarved boundary** |`

So: we have the filesystem down to the moment it calls the driver, and we have the SCSI driver from
`SCLLD` onward. **The code that joins them is uncarved. That is where the missing page-0 request dies.**

### Known anchors (from `enter-directory.md`, verify them - do not trust blindly)
- `RCBLO` at **35766B** (octal).
- The device-transfer dispatch sites: **`036135`, `036142`, `036167`, `036174`** - all
  `JPL I ,B 10` (`036142` is annotated "alternate read/format entry").
- `RXDIR` - reads via the page cache (`R3BUF`), section 4.1 of that document.
- Cross-reference document (NOT yet read - read it first):
  `SINTRAN/Filesystem/DEBUG-scsi-enter-directory.md`

---

## 3. THE LEADING HYPOTHESIS (UNVERIFIED - test it, do not assume it)

**A page/block unit mismatch of exactly 2.**

- `enter-directory.md` (lines 286, 292): a SINTRAN filesystem **page = 1024 WORDS = 2048 BYTES**.
- The SCSI logical block is **1024 BYTES**.
- Therefore **page 0 = TWO SCSI blocks (LBA 0 and LBA 1)**, not one.
- But `INQUI` computes its page<->block shift instructions against a **1024-BYTE** reference:
  line 1267 `X:=12  % RECORD SIZE 1024 BYTES` (0o12 = 10 = log2(1024 bytes)). With blockSize=1024 the
  loop yields shift = 10 - 10 = **0**, so `SUSI1`/`SUSI2`/`SUSI3` come out as **`SAD 0` = NO SHIFT**.
- If some layer expects those shift instructions to convert filesystem pages (2048 bytes) to device
  blocks (1024 bytes), a no-shift converter is **wrong by a factor of 2**.

Whether that mismatch is benign (a layer above simply issues 2 blocks per page) or fatal is
**exactly what the carve must determine**. Do not assert it either way without evidence.

**Other candidates worth keeping open** (do not fixate on the one above):
- Something between function 42's success return and `RXDIR`/`RCBLO` aborts on the data function 42
  produced (`UHLIM=121560`, `NPART=8`, the 48-word partition table at `CMADR`, `SUTYP` flags).
- `5SCIN` ("initialization finished") handling - function 42 forces a re-INQUIRY by clearing it
  (`IP-P2-SCSI-DISK.NPL` lines 337-340); check the state machine does not deadlock or re-enter.
- `RXDIR`'s page cache (`R3BUF`) deciding it already has page 0, or failing to allocate a buffer.

---

## 4. DELIVERABLES

Produce a golden-path analysis folder in the carver tree, matching the existing `mon-analysis`
convention (`<Name>.ASM`, `<Name>.pseudo.c`, `README.md`):

`tools/sintran-segment-carver/versions/L-VSX-500/re/<...>\RCBLO\`

Content required:

1. **`RCBLO.ASM`** - the carved, commented disassembly of `RCBLO` and the dispatch sites
   `036135/036142/036167/036174`, plus whatever resident overlay they land in. Byte-verified.
2. **`RCBLO.pseudo.c`** - readable pseudo-code of the logic.
3. **`README.md`** covering:
   - **The call chain**, closed end to end: `ENTER-DIRECTORY -> RXDIR -> RCBLO -> ??? -> SCLLD`.
     Name every routine and address. This is the whole point - the two existing analyses must MEET.
   - **What `RCBLO` does with**: the `ABFUN` function code, the DMA word count, the memory address,
     the block/page number, and the `SUSI1/SUSI2/SUSI3` shift instructions. Show the actual
     conversion arithmetic from "filesystem page N" to "SCSI LBA".
   - **THE ANSWER**: the exact instruction/decision that prevents `SCLLD` being called for page 0
     after function 42 returns success. If you cannot find it, say so explicitly and state what
     would settle it.
   - **Verification of the factor-2 hypothesis** in section 3: confirmed, refuted, or undetermined.
   - A **VERIFIED / INFERRED / OPEN** summary table (follow `enter-directory.md` section 8's style).
4. **Update** `SINTRAN/Filesystem/code-logic/enter-directory.md`: close or
   narrow the OPEN boundary at line 290 and cross-link the new folder.

---

## 5. CARVING TRAPS (learned the hard way - heed these)

- The MON dispatch table is **`MCTAB@005620B`**, NOT `GOTAB`. Byte-verified.
- **Validate any table against known slots** before trusting it.
- **Pick the overlay by sibling coherence**, not by a single entry test.
- **`STD-I` is NOT a universal entry test.** Do not use it as one.
- 56 segments have never been disassembled - the routine may live in one of them.
- `RCBLO` is a **resident overlay**. Resolve which overlay is actually mapped at the time
  ENTER-DIRECTORY runs; do not assume the first plausible one.
- The **NPL sources are a DIFFERENT REVISION** from the running L binary. Treat NPL **logic** as
  reliable and NPL **addresses** as indicative only. The carved binary is ground truth.

---

## 6. CONSTRAINTS

- **NEVER ASSUME. If you do not know, say "I don't know."** Label every claim **VERIFIED** (you read
  the bytes) vs **INFERRED** (you reasoned). Four confident-but-wrong conclusions have already died
  this session - each one was killed by looking at actual data. Prefer "undetermined + here is the
  one check that settles it" over a confident narrative.
- Cite addresses (octal, as SINTRAN writes them) and quote the actual instructions.
- Give **full absolute paths** for every file mentioned or created.
- **ASCII only** in `.ASM` / `.md` / anything fed to period tools. No Unicode, no em dashes, no
  arrows - use `-` and `->`.
- Never mention any AI assistant or tool in files, comments, or commits.
- Keep as many comments as possible in the carved output; never delete an existing correct comment.

---

## 7. KEY FILE PATHS

| What | Path |
|---|---|
| Carver tree (L-VSX-500) | `tools/sintran-segment-carver/versions/L-VSX-500/` |
| ENTER-DIRECTORY carve (has the OPEN boundary) | `SINTRAN/Filesystem/code-logic/enter-directory.md` |
| SCSI-side cross-ref (read first) | `SINTRAN/Filesystem/DEBUG-scsi-enter-directory.md` |
| SCSI disk layer NPL (function 42, control record) | `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL` |
| SCSI driver NPL (SCLLD/SCINT/SELEC) | `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DRIV.NPL` |
| Board reference (corrected) | `SINTRAN/Devices/SCSI/nd-scsi-3201.md` |
| Investigation handoff | `SINTRAN/Devices/SCSI/SCSI-CONTROLLER-PWSH-HANDOFF.md` |
| Device trace | `C:\Users\ronny\AppData\Local\trace\file-trace.txt` |
| Disk image under test | `E:\Dev\Ronny\RetroFS\demo\test-images\ndfs\tor-disk.img` |
| L07 symbols | `SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT` |
| Corrected mount doc (last-block = control-record connect; bug = block 0 not read) | `SINTRAN/Filesystem/code-logic/scsi-mount-geometry.md` |
