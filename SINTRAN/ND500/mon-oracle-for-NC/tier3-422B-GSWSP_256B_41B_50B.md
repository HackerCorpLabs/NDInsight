# MON-oracle for NC - Tier 3: 422B GSWSP, 256B FullFileName, 41B ROBJE, 50B OPEN

Return-contract reference for the ND Linker (NC) allocator/object monitor calls, confirmed
against the carved SINTRAN III VSX/500 **L07** image. Every claim is tagged **VERIFIED**
(bytes-on-disk or manual text is ground truth) or **INFERRED** (reasoned from carved symbols +
manual, not statically decoded from the handler body).

Evidence layers used:

- Carved segments: `030-S3SM5` (ND-500 System Monitor, load base `40000B`, 32-bit byte image),
  `006-S3FS` (file system, load base `26000B`), resident `SINTRAN-DATA_commoncode` (base 0,
  holds `GOTAB` at `71233B`). Disassembly + symbols under `re/segments-ref/<seg>/`.
- Golden path already done: `re/mon-emulation/050B-OPEN/`.
- Manuals (API contract ground truth): ND-860228-2 *SINTRAN III Monitor Calls*,
  ND-60.136.04A *ND-500 Loader Monitor*, ND-05.009.4 *ND-500 Reference Manual*,
  ND-30.003.007 *SINTRAN III System Supervisor* (App. F.6 Object Entry).

Honesty note carried from the segment layer: the ND-100 MON dispatch is 3 layers
(`GOTAB` head -> uncarved `CALLPROC`/`MFELL` bridge -> worker body). The worker *symbol* is
byte-anchored; the `MON# -> worker` link crosses the uncarved bridge and is not statically
provable for ND-100 calls. ND-500 handler bodies in `030-S3SM5` decode only partially under
`nd500-dis`, so ND-500 bodies are trusted only at the symbol-anchor level.

---

## Q1 - 422B GSWSP (GetScratchSegment) [priority]

**Call:** `CALLG GSWSP, 3, <SizeInBytes>, <LogSegNo>, <RetLogSegNo>` (ND-500 CALLG, 3 params).
Short name in the loader monitor manual: **GSWSP**; SINTRAN name: **GetScratchSegment**.

### Parameter / return register contract

| # | Param | Type | Dir | Meaning |
|---|-------|------|-----|---------|
| 1 | SizeInBytes | INTEGER (32-bit W) | I | Requested data-segment size in **bytes** |
| 2 | LogSegNo | INTEGER (32-bit W) | I | Logical segment number to use; **0 = let system pick** |
| 3 | RetLogSegNo | INTEGER (32-bit W) | O | Logical segment number actually assigned |

- **Success/error signalling (VERIFIED, manual):** ND-500 CALLG convention - on error the
  **K-register** is set and the standard error code is in **W1**; `IF K GO ERROR`. On success
  the segment is connected to the caller's current domain and reserved on the swap file.
- **Side effect (VERIFIED, manual):** the new segment is a data segment (DSEG), initially
  empty, given the default name `SCRATCH-SEGMENT:DSEG`.

### Auto-allocation (LogSegNo = 0): what number is assigned

- **VERIFIED (manual, ND-60.136.04A p.189 / ND-860228-2 422B):** with `LogSegNo = 0`,
  `RetLogSegNo` receives *"the number of the first free segment number, and this number will
  be used."* It is a **logical segment index within the caller's current domain**, not a
  physical segment id.
- **VERIFIED (ND-05.009.4 ND-500 Ref. Manual, line 1237):** *"Each domain is divided into 32
  logical segments."* So the assignable index space is **0..37B (0..31 decimal)**.
- **VERIFIED (ND-60.136.04A loader errors, p.~200):** *"No more than 32 scratch segments are
  allowed in SCRATCH-DOMAIN"* / *"A domain may contain no more than 32 segments."* Auto
  allocation fails (error) once all 32 slots are used.
- **INFERRED (carved-symbol anchored, `030-S3SM5`):** the "first free segment number" scan is
  the routine **`GSGNO` = 126253B** (N500-SYMBOLS, "Get SeGment NO"); the reserve/free pair is
  **`FSWSP` = 66335B** (free the scratch working space, inverse of GSWSP) and
  **`SFREE` = 66746B / `FREES` = 74141B**; the capability/segment slot table anchors at
  **`FSCAP` = 40000B**, with **`WNSEG` = 64133B**, **`TDSEG` = 43615B**. These symbols place the
  whole scratch-segment machinery in `030-S3SM5`; the exact instruction path is not cleanly
  decodable (ND-500 body limitation), hence INFERRED.

**Answer - auto segment number:** the **lowest-numbered currently-free logical segment slot in
the caller's domain**, in the range **0..31 (0..37B)**; returned in param 3 `RetLogSegNo`.
Granularity of the *segment number* is **1 (whole logical-segment slots)**.

### Size granularity (rounding of param 1)

- **VERIFIED (ND-05.009.4, lines 1237 & 1610):** ND-500 physical segments are *"divided into
  blocks of 2k bytes called pages ... may have any size ... **in units of 2k bytes (1 page)**"*;
  *"2048-byte pages ... 2048 = 2**11."*
- **VERIFIED (cross-check, ND-60.124.05 ND-PASCAL User's Guide):** the default STACK-HEAP the
  Pascal runtime asks GSWSP for is *"400000 octal (= 131,072 decimal) bytes"* = **64 pages**;
  131072 / 64 = **2048 bytes/page**. (The ND-60.136.04A "64 pages ... byte address 40000B" is
  an OCR truncation of 400000B - it agrees once the dropped zero is restored.)

**Answer - size granularity:** `SizeInBytes` is **rounded up to a whole number of 2048-byte
(2 KB = 1 KW) ND-500 pages**. A segment therefore always occupies a multiple of 2048 bytes of
swap space; requesting fewer bytes still consumes one full page.

### Dispatch (what is / is not provable)

- **VERIFIED (bytes, `030-S3SM5.bin`):** the ND-500 "0x60" fix-family vector table that routes
  410B/412B/416B (`slot = 0x60 + 2*MON#`) has slot **0x0284 (= 0x60 + 2*0o422) = 0x0000
  (empty)**, and 0x0286 (423B) = 0x0000. So **GSWSP is NOT dispatched through that vector
  table** - it reaches its worker by a different path in the System Monitor. (Neighbouring
  live slots for contrast: 410B@0x0270=0xBAE1, 412B@0x0274=0x98DD, 416B@0x027C=0xBD70.)
- Consequence: the 422B `MON# -> worker` entry is **not statically pinned** from L07 carved
  bytes alone; the return contract above stands on the manual + the symbol-anchored machinery,
  not on a decoded jump.

---

## Q2a - 256B FullFileName (DEABF)

**Call (ND-500, what NC uses):** `CALLG FullFileName, 2, <AbrevName>, <FullName>`
(loader-monitor form: `256B DEABF <abbreviated file name> <full file name>`).

### Return contract

- **VERIFIED (manual, ND-860228-2 255B/256B + worked ASSEMBLY-500 example):**
  - Input param 1 `AbrevName`: abbreviated file name string (may include a file type).
  - Output param 2 `FullName`: the **complete file name written into the caller's buffer**,
    containing **directory : user : file name : file type ; version**, and **terminated by an
    apostrophe `'`**. The example declares it `STRING 100` (buffer up to ~100 bytes; caller
    must size it).
  - Param 3 (default file type) exists on the ND-100 form only; **ignored by the ND-500**, so
    NC's 2-arg CALLG is correct.
  - Preconditions: caller must have **read access**; the abbreviation must be **unambiguous**.
  - Error signalling: ND-500 CALLG - **K-register set on error, code in W1**;
    **error 46 = NO SUCH FILE NAME** (the value NC/loader tests to decide "create it").
- **INFERRED (carved-symbol anchored, `030-S3SM5`):** the handler is **`DEABF` = 123655B**
  (N500-SYMBOLS) - present and byte-resident in the carved System Monitor, confirming the call
  lives in `030-S3SM5`; the body is not cleanly decoded (ND-500 limitation), so field-by-field
  string assembly is taken from the manual, not the disassembly.

**What NC depends on:** a NUL/blank abbreviated name yields no expansion; on success NC reads the
returned string up to the `'` terminator; on error 46 it proceeds to create the file.

---

## Q2b - 41B ROBJE (ReadObjectEntry)

**Call (ND-100 & ND-500):** MAC form `LDT <fileno>; LDA (<buf>; MON 41`.
ND-500 form: `CALLG ReadObjectEntry, 2, <FileNumber>, <Buff>`.

### Register / buffer contract (VERIFIED, manual ND-860228-2 41B + ND-30.003.007 App. F.6)

- **Input:** `T` = file number (from a prior OPEN); `A` = address of a **64-byte** buffer
  (`H BLOCK 40B` = 32 words).
- **Output:** the buffer is filled with the file's **64-byte Object Entry**. On error, the
  **error return** is taken and **A = standard error code** (MAC: `MON 41` / `JMP ERROR`;
  ND-500: K set, code in W1). Common code: **46 = NO SUCH FILE NAME** family / access errors.
- **Object Entry layout (word offsets, App. F.6), fields NC cares about:**

  | Word (octal) | Field |
  |--------------|-------|
  | 0 | INFO ABOUT CURRENT USE (flags: U=used, V=open-for-write, R=reserved) |
  | 1B-10B | FILE NAME (16 chars / 8 words) |
  | 11B-12B | FILE TYPE (4 chars / 2 words) |
  | 12B/14B | NEXT / PREVIOUS FILE-VERSION pointers |
  | 16B | FILE ACCESS bits |
  | 17B | OBJECT BLOCK + LOG. FILE TYPE (OBJL) |
  | 20B | DEVICE NO. (peripheral file): **logical dev = bits 11-0, unit = bits 15-12** |
  | 26B/30B | DATE CREATED / LAST DATE OPENED (read/write) |
  | **32B (2 words)** | **NO. OF PAGES IN FILE** (the file-size field NC reads) |
  | 34B (2 words) | MAX. VERSION POINTER |
  | 36B (2 words) | FILE POINTER (disk page of this version's data) |

### Dispatch / worker (carved)

- **VERIFIED (bytes, resident `SINTRAN-DATA_commoncode`):** `GOTAB[41B]` at virtual `71274B`
  (= `71233B + 41B`) = **121023B** (non-zero) - i.e. ROBJE takes a **direct GOTAB branch**
  (matches the Quick-Reference "br" flag), unlike OPEN (`GOTAB[50B]=0`, fall-through).
- **VERIFIED (symbol + bytes, `006-S3FS`):** worker **`ROBJE` = 55566B** (FILSYS-SYMBOLS),
  byte-anchored; the handler at `55566B..` reads the file-number param, walks the open-file
  table and copies the 64-byte object entry (siblings `FOBJB = 55563B`). The `MON 41B ->
  55566B` link crosses the uncarved dispatch bridge (3-layer caveat), so it is byte-anchored at
  the worker, dispatch-verified at the `GOTAB` head, but not end-to-end statically proven.

---

## Q2c - 50B OPEN (OpenFile) - reuse of existing golden path

Fully analysed in `re/mon-emulation/050B-OPEN/`. Return contract relevant to NC:

- **VERIFIED (golden path + manual):** MAC form `SAT <access>; LDX (<name>; LDA (<type>;
  MON 50`. **On success the file number is returned in the `A` register**; that number is what
  NC passes to subsequent ReadFromFile / WriteToFile / ROBJE / CloseFile.
- **VERIFIED (byte-anchored):** dispatch `GOTAB[50B] = 000000` (fall-through -> `MFELL` ->
  `CALLPROC`); worker **`OPENF = 123525B`** in `006-S3FS`, shared open-file-table allocator
  **`FOPEN = 067432B`**.
- **VERIFIED (behaviour):** OPEN has **no** empty-name/default fallback (that is a *different*
  call - ScratchOpen 235B `OPENS`, DirectOpen 220B `DOPEN`). An empty / all-zero name is
  refused with a **non-zero error in A** (most consistently **056B "No such file name"**), so
  NC must never rely on OPEN to invent a name.

---

## One-line register summary (for the emulator)

| MON | Name | On success | On error |
|-----|------|-----------|----------|
| 50B | OPEN | file number in **A** | error code in **A**, error return |
| 41B | ROBJE | 64-byte object entry in caller buffer (T=fileno, A=buf) | code in **A** / K+W1 (ND-500) |
| 256B | FullFileName/DEABF | full name string (`'`-terminated) in caller buffer | K set, **W1** = code (46 = no such file) |
| 422B | GSWSP | segment reserved; **first-free log.seg (0..31)** in param 3 | K set, **W1** = code (e.g. >32 scratch segs) |

---

*Provenance: carved SINTRAN III VSX/500 L07 (`030-S3SM5`, `006-S3FS`, resident commoncode) +
ND official manuals. VERIFIED = bytes-on-disk or manual text; INFERRED = symbol-anchored
reasoning where the ND-500 body does not cleanly decode.*
