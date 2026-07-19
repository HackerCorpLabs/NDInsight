# 07 - Internals and Monitor Calls

This document is the programmer / kernel view of spooling: the monitor calls a
program uses to drive it, and the SINTRAN segments that implement the spooling
machinery.

**The byte-verified carve is done.** The detailed, address-level results now
live in [07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md).
This document is the readable summary; 07a is the ground truth (every address
re-read from carved bytes). Where the two differ, 07a wins.

Sources: `../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md` and
`../../Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md` (MON-call
mappings + register conventions); the L-VSX-500 carve (see 07a).

---

## 1. Monitor calls behind spooling

Three monitor calls drive spooling. All three dispatch through `MCTAB` (the
byte-verified dispatch model) and all three workers live in the **file-system
segment `006-S3FS`** - which is where the real spooling **queue manager** is
(NOT in segment 031 or 137; see section 2). Byte-verified in 07a:

| Monitor call | Number (octal) | MCTAB slot -> worker | Meaning |
|--------------|----------------|----------------------|---------|
| `APSPF` | MON 240 | 006060B -> 106307B | Append a file to the spooling queue (behind `@APPEND-SPOOLING-FILE`). |
| `SPCLO` | MON 40 | 005660B -> 067572B | Close a spooling file (link it to the queue). |
| `GetSpoolingEntry` (`RSPQE`) | MON 55 | 005675B -> 106212B | Pull and remove the next queue entry (the spooler uses this). |

### Register / parameter conventions (verified vs the Monitor Calls manual)

**`APSPF` (MON 240) - AppendSpooling:**
- `X` = address of the file name to print (may be abbreviated).
- `T` = number of copies in bits 0:14; **bit 15 = 1** also prints the UserText.
- `A` = address of the UserText message (then copied so `D` = message address).
- `A` = address of the spooling-device peripheral-file name (loaded last).
- Return: `A` = error code (0 = OK). (Worker saves `D` first = the message
  pointer, byte-confirmed.)

**`SPCLO` (MON 40) - CloseSpoolingFile:**
- `T` = file number (from the earlier open).
- `A` = number of copies (then copied so `D` = number of copies).
- `A` = condition/print flag (0 = print text only if the spooling conditions
  require; non-zero = print unconditionally).
- `X` = address of the error-device text.
- Return: `A` = error code. If the file is not a spooling file, a normal close
  is done. (Worker tests `T`, byte-confirmed = the file-number parameter.)

So a program can spool without the `@` commands: write to the spooling file,
`SPCLO` to close/queue it, or `APSPF` to append an existing file.

---

## 2. Which segment does what (byte-verified)

The carve corrected the naive assumption that the spooling segments (`031`,
`137`) hold the queue logic. They do not. The real layout (07a):

| Piece | Segment | Role |
|-------|---------|------|
| **Queue manager** (APSPF/SPCLO/RSPQE workers, queue list handlers, page-pool wait/wake) | **`006-S3FS`** (base 26000B) | The actual local spooling engine. |
| **Physical line-printer driver** (IOX 430B-433B cells) | **`044-S3IDPIT`** / **`053-S3SDPIT`** (DPIT image/save) | Per-device IOX cells; the device number lives in the datafield. |
| **Spooling datafield SAVE area** | **`031-S3SSPD`** (K/L/M) | Saved copy of each `SPPRx`/`SPRTx` spooling-program datafield - **data, not code**. This is the `<save-area?>` operand of `*SET-SPOOLING-DEVICE-NUMBER`. |
| **Remote (COSMOS) spooling** | **`137-COSPOOL`** (L only) | Holds `COSPO`, the COSMOS *remote* spooling RT program (prints to other systems over XMSG). Contains **zero** IOX/MON words - a separate subsystem, not the local engine. |
| **Local spooling-program body** (the routine `SPRTx` runs) | **NOT located** | `120-S3SPRMA` (L/M only) is a candidate but its content is not overlay-proven. See 07a. |

### Key data structures (verified)

- **Per-spooler control block:** array `SPTAB=122562B..ENDSP=123034B`, stride
  `SPLEN=12B` (~17 slots), one per device. Fields include `SPERI=+2` (peripheral
  logical device number; `-1` disables), `SPINX=+10B` (spool index),
  `SPAGE=+31B`.
- **Per-file queue entry:** linked records in a dedicated spool-queue segment
  (VA 150000B-177777B), list head `LSPOQ=162122B`; walked by `RSPQE=106212B`.
  A queued file is **not** stored in the normal open-file object tables.
- **Page-pool wait/wake:** a SINTRAN semaphore reserve/release at `006-S3FS`
  171001B-171013B - `MON 122B` (`2RESR`) blocks the writer when the pool is
  empty, `MON 123B` (`2RELE`) wakes it, on `SPSEM=164607B`. Pool free-buffer gate
  = `SPOOL=147510B` (=0 means no buffers free).

---

## 3. What is resolved, and what is still open

Most of the original open questions are now byte-verified in
[07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md):

- **Resolved:** `APSPF`/`SPCLO`/`RSPQE` dispatch and workers (all in `006-S3FS`)
  and their register conventions; the two queue structures (`SPTAB` per-spooler
  block; `LSPOQ` per-file queue in the spool-queue segment); the page-pool
  wait/wake semaphore; segment roles (031 = datafield save, 137 = COSMOS remote,
  006 = the engine); and the line-printer IOX cells for device 430B/434B in DPIT.

Still **NOT RESOLVED** (open for a future carving pass, all listed in 07a
section 6):

1. The resident "spooling device numbers" table that `*SET-SPOOLING-DEVICE-NUMBER`
   writes (distinct from the verified `SPTAB`).
2. The byte location of the common **spooling-program body** the `SPRTx` RT
   programs run (`120-S3SPRMA` is a candidate but not overlay-proven; the `SPOOL`
   symbol is the pool gate, not the program).
3. The exact decrement->block / increment->wake instruction ordering around the
   verified reserve/release primitive.
4. The byte-level unlink of the currently-printing entry from `LSPOQ`.
5. The generic line-printer driver **code body** that executes the DPIT IOX cells.

---

## 4. Resolving the remainder (carving)

The remaining items live in the SINTRAN III image, reachable through the
`sintran-carving` skill and the segment carver under
`../../tools/sintran-segment-carver/`. Record any new byte-verified result in
[07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md) and update
this summary accordingly.
