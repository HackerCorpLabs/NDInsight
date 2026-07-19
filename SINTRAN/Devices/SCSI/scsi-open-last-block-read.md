# SCSI Disk Open — the "Last Block" Read, and What SINTRAN Does With It

**Full path:** `SINTRAN/Devices/SCSI/scsi-open-last-block-read.md`

**Status:** Live-trace reverse engineering. Every claim below is tagged **[VERIFIED]** (read
directly from the execution trace bytes/registers) or **[INFERRED]** (reasoned, not yet proven
from bytes). Assume nothing that is not tagged VERIFIED.

**Evidence source:** full ND-100 opcode+register execution trace of
`@ENTER-DIRECTORY,,DISC-SCSI-1,0` on **SINTRAN III VSX/500 version K**, captured to
`C:\Users\ronny\AppData\Local\trace\file-trace.txt` (438,631 lines; `Opcodes;` lines carry
`X B A D T L` registers in octal, interleaved with `Device;` `[SCSI-TRACE]` lines). Version
confirmed K by byte-matching the running code at `035766` (`135106`) to the carved
`versions/K-VSX-500/segments/006-S3FS.bin`.

**Disk under test:** raw dump from real ND hardware, `D:\ND\HDD\SCSI-K.image`, 129,312 SCSI
blocks (LBA 0..129311), 1024-byte sectors (2 sectors = 1 ND page of 1024 words / 2048 bytes),
directory **PACK-ONE**.

---

## 1. Executive answer

During SCSI disk open, SINTRAN issues **READ CAPACITY**, then reads **exactly one** block — the
disk's **last physical block** (LBA = the lastLBA that READ CAPACITY reported) — and consumes
those 1024 bytes as a **checksum-validated data structure**: a **SINTRAN disk area/layout table**.
It is **never executed as code** and **never copied wholesale**. After parsing that table into the
device datafield, SINTRAN issues **no directory (block-0) read**, so the directory does not enter.

The `APPROACHING END OF ACCOUNTING FILE` console message is a **[INFERRED] coincident** background
warning from the accounting subsystem, not part of this disc-open path.

> Correction to earlier notes: the last LBA is an ordinary host data block. Real SCSI drives keep
> defect data internal (obtained via the `READ DEFECT DATA` command), never in the data area. The
> `08 00 54 D9 ...` content at LBA 129311 is therefore **SINTRAN/ND filesystem data**, not a SCSI
> defect/reallocation table. The old "defect table" label — and the READ-CAPACITY-as-usable hack
> built on it — were a misreading and have been dropped.

---

## 2. Which block, and why that block — [VERIFIED, proven]

- Line 281496: `[SCSI-TRACE] READ CAPACITY -> blockSize=1024 lastLBA=129311`. The controller
  builds the 8-byte reply `00 01 F9 1F 00 00 04 00` (lastLBA = `0x0001F91F` = 129311, block size
  `0x400` = 1024) and **DMAs it into ND memory** at MAR `0x8C4E..0x8C52` (the `DMA->ND xfer` lines
  are the controller writing memory, not the CPU).
- The value `0xF91F` (`174437`&#8323;) appears in **no CPU register** and is **never stored by any
  CPU instruction** before the read is built (both trace searches return empty). It reaches memory
  **only via the READ CAPACITY DMA**.
- At PC `114504`–`114530` the resident SCSI driver **loads that value back out of the response
  buffer** (low word `D=174437` = `0xF91F`, high word `A=000001`) and packs the double-word disc
  address `0x0001F91F` = 129311 into the READ_6 CDB (`STDTX`/`STATX`), with length `002000`&#8323;
  = 1024.
- Line 283792: `CDB op=0x08 (SC_READ_6) lba=129311 blocks=1 len=1024`. The CDB LBA bytes
  `01 F9 1F` are **byte-identical** to the READ CAPACITY reply's lastLBA field.

**Conclusion:** the read address is **not computed** and is **not** a filesystem block-0
translation. SINTRAN deliberately reads **the last block reported by READ CAPACITY**. This settles
the long-standing ambiguity: it is a driver-level last-block read keyed off READ CAPACITY, not a
capacity-relative address translation.

---

## 3. The block is a SINTRAN disk area/layout structure — [VERIFIED decode]

The 1024 data bytes DMA to physical MAR `0x03FE00..0x040200` (WHMAR=`0x0003`, WLMAR=`0xFE00`,
WTC2=`0x04` WTCL=`0x00` = 1024), completing at line 288122. Content (first 32 bytes) and decode
as big-endian 16-bit words:

```
08 00 54 D9 80 00 00 00 00 00 00 00 00 01 DC D8 C0 00 00 00 00 01 F9 1F 00 00 00 01 C0 00 00 00
```

| word | octal    | value  | meaning (interpretation) |
|------|----------|--------|--------------------------|
| w0   | 004000   | 2048   | bytes per ND page (page size) |
| w1   | 052331   | 0x54D9 | magic / identifier (unconfirmed) |
| w2   | 100000   | 0x8000 | area-descriptor #0 flag (bit15) |
| w6:w7| —        | 122072 | area #0 size = usable blocks (61036 pages × 2) |
| w8   | 140000   | 0xC000 | area-descriptor #1 flag (bit15+bit14) |
| w10:w11| —      | 129311 | area #1 size = physical last LBA |
| w14  | 140000   | 0xC000 | area-descriptor #2 flag |

**The structure is a table of area/partition descriptors** laid out at stride 6 words
(`{flag, ..., size-doubleword}`), preceded by a header (page size + magic).

---

## 4. What SINTRAN does with it — [VERIFIED]

### 4a. Whole-block XOR checksum, tested against zero (validation)

Loop at PC `111051`–`111055`, running **512 iterations** (the full 512-word block, buffer virtual
base `0177000` — confirmed because the first `LDATX` returns `004000`&#8323; = `0x0800` = block
word 0):

```
111051  LDATX 0        A := block[X]           (alt page table, T=3)
111052  REXO SA DD     D := D XOR A            (accumulate)
111053  AAX 1          X := X + 1
111054  MIN *-27       decrement counter, skip when zero
111055  JMP *-4        loop
```

Exit test at PC `111056`: `SKP IF DD EQL 0` with **`D=000000`** → the running XOR is zero → the
**checksum PASSES**. (The block carries its own trailing checksum word so the 512 words XOR to 0.)

### 4b. Parse of the area descriptors into the device datafield

Loop at PC `111072`–`111140`. For each descriptor it computes an entry pointer
(`MPY *110`, `LDX 16,B`, `RADD SA DX`), reads the flag word and two double-words from the buffer
(`LDATX 0`, `LDDTX 2`, `LDDTX 4`), and stores them into per-unit datafield slots
(`STA 23,B,X`, `STD 35,B,X`, `STD 61,B,X`). Observed values threaded through the registers match
the decoded table (flag `0x8000` then size `122072`; flag `0xC000` then size `129311`; …).

The loop exits at `111140` (`STDTX 1`) and calls (`111151 JPL I *35`) a validation/dispatch
routine at `110132`+ (line 292018) that does further field extraction and equality tests
(`REXO SL DT`, `SKP IF DT EQL 0`, `BSKP …`).

### 4c. It is a data STRUCTURE, not CODE — [VERIFIED]

Across all instructions executed after the read (lines 288122–300600), **no PC ever falls in the
buffer's virtual range `0177000–0177777`**, and **no `JMP`/`JPL` targets that range**. The block
is read and parsed as data; it is never jumped into or executed. (Contrast: only the boot block is
executed in ND systems — this is the ordinary data path.)

---

### 4d. The parsed values, and what is written to the device datafield — [VERIFIED]

The block is a **SINTRAN disk area / spare-block map**: descriptors of the form
`{flag (1 word), rangeStart (doubleword), rangeEnd (doubleword)}`. Decoded live from the parse
loop stores (device datafield base `B = 031707`):

| # | flag | rangeStart | rangeEnd | meaning |
|---|------|-----------|----------|---------|
| 0 | `0x8000` | 0 | **122072** | main **USABLE** area `[0 … 122072)` = 61036 pages (= PACK-ONE's size) |
| 1 | `0xC000` | 129311 | 129312 | reserved block |
| 2 | `0xC000` | 129310 | 129311 | reserved block |
| 3 | `0xE000` | 129309 | 129310 | reserved block |
| 4 | `0xE000` | 129289 | 129309 | reserved region (20 blocks) |
| 5 | `0xE000` | 129269 | 129289 | reserved region (20 blocks) |
| 6 | `0` | 0 | 0 | empty slot |
| 7 | `0xE000` | 129098 | 129269 | reserved region (171 blocks) |
| 8–11 | `0` | 0 | 0 | empty |

So the last block records one **usable area** `[0, 122072)` plus a set of **reserved/spare regions
at the top of the disk** (`129098 … 129312`). This is SINTRAN's own in-filesystem area/spare map —
**not** the SCSI drive's internal defect list (which is obtained via `READ DEFECT DATA`, never from
the data area).

**Writes to the device datafield** (`B = 031707`), per descriptor index `X`:

| datafield field | absolute address (octal) | store instruction | contents |
|-----------------|--------------------------|-------------------|----------|
| flag array | `031732 + X` | `STA 23,B,X` | `0x8000, 0xC000, 0xC000, 0xE000, …` |
| rangeStart array (doubleword) | `031744 + X` | `STD 35,B,X` | `0, 129311, 129310, 129309, …` |
| rangeEnd array (doubleword) | `031770 + X` | `STD 61,B,X` | `122072, 129312, 129311, 129310, …` |

i.e. the disc-open **copies the area map out of the block buffer into the device datafield** at
`031732` (flags), `031744` (starts), `031770` (ends), then finalizes (`STDTX 1`) and returns. The
derived usable size (`122072` blocks = `61036` pages) is **correct** and matches the directory.

### 4e. Data-integrity validation: disk = memory, nothing corrupts it — [VERIFIED]

Verified the whole transfer path against the on-disk image `D:\ND\HDD\SCSI-K.image`
(132,415,488 bytes = 129,312 blocks; last LBA = 129311):

- The bytes at LBA 129311 on disk are **byte-identical** to the bytes the trace shows DMA'd into ND
  memory (`08 00 54 D9 80 00 00 00 …`).
- The **XOR of all 512 words of the on-disk block = 0** — the identical value SINTRAN's checksum
  loop produced in memory (`D=0`, PC 111056). A single dropped/shifted/byte-swapped/truncated word
  would make this nonzero and SINTRAN would reject the block; it does not.
- The block is a valid sparse structure (29 of 512 words nonzero, valid trailing checksum).

**Conclusion:** the block SINTRAN reads is genuine, valid, and correctly delivered SINTRAN
filesystem data. The mount failure is therefore **not** data corruption, **not** a checksum
failure, and **not** a DMA/transfer fault — it is a **logical decision in the FS-object code acting
on correct data**.

## 5. Outcome — the mount ends by *silent completion*, not by a value-comparison abort — [VERIFIED]

There is **no single "reject" branch**. The disc-open reaches "done" and the directory read simply
never becomes a request:

1. **Checksum gate (the one hard requirement) — PASSES.** PC `111056` `SKP IF DD EQL 0`, `D=0`
   (line 291672). The 512-word XOR includes word 0 (`004000`) and the magic word 1 (`052331` =
   `0x54D9`) — the magic is folded into the checksum but **never compared** against a constant.
2. **No further validation.** Searching the whole post-read window, the magic `0x54D9`, the sizes
   (`122072`/`129311`), and the flags (`0x8000`/`0xC000`) are **never tested** against a constant
   or against the SCSI-reported capacity. The parsed geometry is accepted as-is.
3. **Disc-open returns, queue empty.** PC `111153`–`111201`: phase field `[B+5]` steps `3→1`, the
   request-queue head `[B+1]` loads `X=0` (empty), `JXZ`→return; the process reaches `WAIT 0`
   (PC 027540, line 292084) and idles. The resident disc-open code `110xxx`/`111xxx` never runs
   again.
4. **The FS requester is woken but enqueues nothing.** Completion wakes the higher file-system
   process (datafield `B=066642`) around lines 298425–299482 (`IRW 130 …`, then resumes at PC
   `033701`/`032510`). It traverses FS objects (`LDATX`/`LDXTX`) and **returns without issuing the
   directory-master (block-0) read**.
5. **Result:** the last SCSI CDB in the entire trace is the READ_6 at line 283792; block 0 is never
   read; control returns to command level and prints `@` (line 326768). The directory does **not**
   enter.

**[INFERRED — the remaining gap]** The branch that decides *not* to enqueue the directory read
lives in the FS-object / enter-directory layer (segments `032xxx`/`033xxx`, process `B=066642`,
entry PC `033701`/`032510`). That code is **not in the current K carve**, so the exact test —
almost certainly the derived directory-master address vs the parsed area geometry — is not yet
proven. See §7.

## 5b. The whole command is a disc IDENTIFY, not a directory read — [VERIFIED]

The **entire** SCSI conversation for `@ENTER-DIRECTORY,,DISC-SCSI-1,0` is three CDBs, then silence:

| CDB | line | purpose |
|-----|------|---------|
| `INQUIRY` (0x12) | 279508 | device identify |
| `READ CAPACITY` (0x25) | 281496 | size → lastLBA 129311 |
| `READ_6 lba=129311` (0x08) | 283792 | read the last block (area map) |

**No `READ_6 lba=0` (directory master) is ever issued** — 283792 is the last CDB in the trace. So
the directory read is **not attempted**, not merely failed.

The proximate branch (resident driver, `B=031707`):

```
111154  LDA 5,B      ; phase = 3
111155  AAA -2       ; -> 1
111156  STA 5,B      ; phase 3 -> 1 (idle)
111157  LDX 1,B      ; X <- [031707+1] = request-queue head
111160  JXZ 21       ; queue head == 0  -> branch to 111201 (start no I/O)
...
111201  JMP I *7     ; return -> 027540 WAIT 0
```

**[VERIFIED]** At line 292083 `111160 JXZ` executes with `X=000000` → **branch taken** → `WAIT`.
The disc datafield request-queue head `[031707+1]` is **0 (empty)**: no block-0 request was ever
placed in the queue, so the driver starts nothing and idles. The woken FS process (`B=066642`,
resumes line 299482, PC 033701) runs **doubly-linked-list unlink cleanup** (carved routines
`032510`/`032461` in `016-S3SRPIT`) that returns the identify read's buffer to a free pool — it
contains **no code that computes or enqueues a block-0 directory read**.

**[INFERRED — the real locus]** The choice "do identify only, never the directory" was made in the
**enter-directory / disc-connect sequencer that ran *before* the identify read** (the command-parse
→ first-disc-request window, before line 283792). That sequencer is the next thing to trace; the
post-completion code (both resident driver and carved FS handler) behaves correctly given an empty
queue.

## 5c. How to mount — what a mountable SCSI disk must present [VERIFIED where noted]

- **[VERIFIED]** The **last physical block** (LBA = the drive's READ-CAPACITY lastLBA) must be a
  genuine SINTRAN area/layout table whose **512-word XOR checksum is 0** (PC 111056). *The current
  disk already satisfies this* (§4d, §4e) — geometry is valid, so the block content is **not** the
  blocker.
- **[VERIFIED]** For the directory to be read, a `READ_6 lba=0` request must be **enqueued** in the
  disc datafield (`[031707+1]` non-zero, phase driven active) at/after identify completion — in this
  run it never is.
- **[INFERRED]** The trigger that should build that request lives in the enter-directory sequencer
  before line 283792. Whether it is suppressed by an INQUIRY/device-type check, a command-parameter
  decision, or a missing "directory phase" is the open question (§7). Since the geometry is valid,
  the fix is almost certainly **not** a disk-byte change but a condition in that sequencer / the way
  the emulator presents the unit to it.

---

## 5d. DEFINITIVE: the directory-mount worker is never dispatched — [VERIFIED]

Direct trace checks (not inference):

- **`ENDIR` (140176B) executes 0 times.** The mount worker chain
  (`ENDIR → ForceReserve → CHDSI → RXDIR → RCBLO`) and the high-level SCSI builders
  (`SCSID/SCSI1/SCSI2/SCDTS`) run **zero times**. No `READ_6 lba=0` is ever issued.
- The 3-CDB **identify/connect fully succeeds**: all CDBs `SS_GOOD`, device datafield `031707`
  `[+5]=1` (valid), geometry stored — **no error/status word is set**. INQUIRY returned
  `00 00 05 01 34 00 00 00` (device-type 0x00 = direct-access disk), not a rejection trigger.
- After the successful connect, the FS object is **torn down** (list-unlink `032510`/`032461`) and
  control returns; the directory read is never built.

**Conclusion (VERIFIED behavior; INFERRED cause):** SINTRAN abandons the mount at the **resident
enter-directory command dispatch (uncarved 140xxx)** — it takes a **skip arm** and never calls
`ENDIR`. Because the connect fully succeeds, **no emulator-side disk value can fix this**
(INQUIRY, READ CAPACITY size, completion interrupt, and block content are all already correct —
this also retires the older "lost RSTAU interrupt" and "wrong device size" theories in
`SCSI-MOUNT-FIX-PLAN.md`). The lever is **SINTRAN-side**: a **device-kind / configuration gate**
that does not treat `DISC-SCSI-1` as a mountable directory device. The exact guard PC is in
uncarved resident code and, being the never-taken arm, leaves **no trace** — so the specific gate
is not yet proven.

**To prove the guard:** capture an ENTER-DIRECTORY trace of a disk that *does* mount (e.g. the
Winchester `DISC-75MB-1`/PACK-UNO) and diff the resident dispatch — the working one takes the arm
that calls `ENDIR`; the divergence word is the device-kind/config gate `DISC-SCSI-1` fails. (Or
carve the resident 140xxx dispatch.)

## 6. The accounting message — [VERIFIED: tied to command completion, not the mount skip]

- Reconstructed from the per-character `CONOUT` logs, the console prints exactly
  `CR LF "APPROACHING END OF ACCOUNTING FILE" CR LF "@"` (first char at line 300511) via the shared
  terminal-output primitive at PC `057505 LBYT` → `057512 CONOUT`, terminal DCB `B=122200`,
  at PIL 10.
- **[INFERRED]** It is driven from a pre-queued string by background accounting activity at level 3
  (PID `0E`) around lines 297405–300039 — the accounting subsystem warning that its log file is
  near full — with **no data dependency** found from the LBA-129311 block bytes. Treat it as a
  **coincident** warning, temporally adjacent to (not caused by) the failed mount.

---

## 7. Open questions / next steps

1. **The abort branch (highest priority).** Isolate, in the live trace from line ~292008 onward
   (routine `110132`+), the exact compare+branch that decides not to read the directory, the field
   it tests, and the value/condition that would make SINTRAN **proceed** — that condition is the
   direct "how to mount" answer. This code is largely resident (`110xxx`/`111xxx`) and not present
   in the current K carve, so it must be read from the live trace + registers. *(In progress.)*
2. **What creates this structure (task #29).** The area/layout table at the last block is
   SINTRAN-written. Find the routine in the carved filesystem code (`006-S3FS` + the create-
   directory logic under `SINTRAN/Filesystem/`) that **builds and writes
   it** on create-directory/format for SCSI drives: its exact field layout, the checksum algorithm,
   and the trigger. That gives the authoritative structure needed to reproduce a mountable SCSI
   image (retrofs) and to know precisely what a mountable disk must present.
3. **Magic/flags.** Confirm whether `w1 = 0x54D9` and the descriptor flags `0x8000`/`0xC000` are
   tested against expected constants during dispatch.

---

## 8. Evidence index (trace line numbers)

| line     | event |
|----------|-------|
| 281496   | READ CAPACITY reply lastLBA=129311, DMA'd to memory `0x8C4E` |
| 114504–114530 (PCs) | driver loads lastLBA from response buffer, packs READ_6 CDB |
| 283792   | READ_6 lba=129311 CDB emitted |
| 288122   | 1024 data bytes DMA complete at physical `0x03FE00` |
| 289167–291672 | XOR-checksum loop (512 words), PC 111051–111055 |
| 291672   | checksum test PC 111056, `D=0` → PASS |
| 291673–292008 | area-descriptor parse loop, PC 111072–111140 |
| 292018+  | dispatch/validation routine at PC 110132+ |
| 300511   | first char of "APPROACHING END OF ACCOUNTING FILE" |
| 326768   | `@` prompt (command returned; directory not entered) |

---

**Related:**
`SINTRAN/Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md`,
`SINTRAN/Devices/SCSI/scsi-disk-format.md`,
`SINTRAN/Filesystem/` (create-directory + on-disk format).
