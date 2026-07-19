# SINTRAN III L07 - SCSI Disk Driver + Disk Layer (carved)

> **CORRECTION (superseded):** This folder's **function-42 / disk-layer** section
> was disassembled at the **wrong (no-offset)** NPL addresses. The `IP-P2-SCSI-DISK`
> unit is at **NPL label + 376B** in the L07 image (e.g. SCSID = NPL 061621 + 376
> = `062217B`), so those function-42 bytes decoded to garbage. That analysis is
> **superseded by** [`../FUNCTION-42-RETURN/`](../FUNCTION-42-RETURN/) (correct
> +376B carve) and [`../SCSDISK-TRANSFER/`](../SCSDISK-TRANSFER/) (fn-0 dispatch).
> The **driver** half (`SCLLD`/`INITO`/`SELEC`, aligned to its NPL labels) remains correct.

Static reverse-engineering of the SCSI protocol driver (`IP-P2-SCSI-DRIV`) and
the SCSI disk layer (`IP-P2-SCSI-DISK`) as they exist in the running L07 system
(L-VSX-500). Purpose: show exactly what the SCSI driver does with a
"read block N" request, and determine why a SCSI `ENTER-DIRECTORY` fails when
SMD / Winchester / floppy succeed.

Carved bytes are ground truth. NPL is a different revision (its addresses run
~+376B off L07) and is used only to name the logic.

---

## 1. Which overlay holds the driver + disk layer

**Segment `065-S3SIPIT` (load base `32000B`)** holds the coherent SCSI driver
body. Its twin **`066-S3IIPIT`** is byte-for-byte identical (`cmp -l` reports 0
differences over the whole segment), so either maps the same content.

The overlay was resolved by **sibling coherence**, not a single entry test
(drivers are reached by dispatch and have no `STD I` prologue). Six candidate
"SM/PIT" overlays that map `067160B` were disassembled at the driver symbols;
only `065/066` decode to code that matches the NPL at *every* sibling anchor:

| Symbol | Addr | 065-S3SIPIT decodes to | NPL match |
|--------|------|------------------------|-----------|
| SCLLD  | 067160B | `D:=A` `D>>=8` `T:=3` `SKP D<T` then `A=:X.SUCON`(STA ,X 30) | `IF D:=A SHZ -10<3 ... A=:X.SUCON` exact |
| SCINT  | 067247B | `T:=HDEV` `+4`(RSTAU) `IOXT` `X:=64/\A` | `T:=HDEV+RSTAU; *IOXT; IF X:=64/\A` exact |
| SELEC  | 070165B | save TAD/X, `-1=:SCEIM`, `X:=SCWAQ`, `JXZ` bus-free | `SELEC: TAD=:SVTAD... -1=:SCEIM; IF X:=SCWAQ` exact |
| INITO  | 070261B | `A:=X.SUCON` `D:=A` `A>>=8` classify, splice SCWAQ | `INITO: IF X.SUCON=:D SHZ -10=0 ...` exact |
| SCSDI  | 057215B | datafield code, `LDA ,X 14`(=ABFUN) | disk-layer datafield, ABFUN=14 anchor |

In the other overlays (`070-S3SSM`, `071-S3SM`, `030-S3SM5`, `062-S3SSM5`,
`017-S3SMPIT`, `026-S3IMPIT`) `067160B` decodes to unrelated loop bytes
(`SKP IF DT GRE SX`, `LDA ,X 1 / SAT 0`, `SAA 1`), none matching SCLLD.

### Byte spot-check (reproduced with `dd`)
`SCLLD @067160B` = byte offset `(067160-032000)=035160B = 14960 words = 29920 bytes`:

```
dd if=065-S3SIPIT.bin bs=1 skip=29920 count=10 | od -An -tx1
 cc 69  dc f8  f2 03  c6 31  a8 0e
```
= `146151 156370 171003 143061 124016` (big-endian) =
`RADD CLD SA DD` / `SHD ZIN SHR 10` / `SAT 3` / `SKP IF DD LST ST` / `JMP 16`.

Load-bearing CALL targets are embedded pointer words right after SCLLD and were
verified: `word@067241 = 070261` (INITO), `word@067242 = 070165` (SELEC). So
`067175 JPL I 44` = `CALL INITO` and `067200 JPL I 42` = `CALL SELEC`.

---

## 2. Driver state machine (the enqueue path)

Work enters the driver through **exactly one door**: `SCLLD`. In pseudo-C terms:

```
SCLLD(A=function, X=unit):
    class = A >> 8
    if class < 3:                 # data transfer (READ/WRITE)
        X.SUCON=A; X.SULRG=L; X.SUTRG=0
        if NCROK<0: error
        INITO(X)                  # <-- link X onto SCWAQ (the enqueue)
        if BUSFL==0: SELEC()      # <-- if bus idle, begin arbitration
    elif class==3: ENTIM(X)       # timed control op
    elif class==4: SCRST          # bus reset
    else: T=ILDCO                 # illegal
    goto SCWTI

INITO(X):  ... ; walk SCWAQ to tail ; tail.SULINK = X      # THE enqueue

SELEC():   X = SCWAQ
           if X != 0: program NCR select, arm timeout
           else:      BUSFL = 0            # SCWAQ empty -> bus free -> return
```

The driver never links anything onto SCWAQ except from `INITO`, and `INITO` is
only reached from `SCLLD`. Therefore: **if `SCLLD` is not called for a block,
that block is never enqueued and never read.** This is consistent with the live
trace: on the disconnect the handler runs `A=4 -> DCTHR -> TEROP(T=0) -> BUSFP
-> SELEC`, `SELEC` finds `SCWAQ` empty, sets `BUSFL:=0`, returns success. One
`WCONT=5`, no transfer writes.

---

## 3. The connect / function-42 flow (disk layer)

`ENTER-DIRECTORY` first asks the disk for its **format/layout** via function
`42` (READ FORMAT). For SCSI this is handled by the disk layer (`SCSID`), which:

1. Forces re-inquiry (`5SCIN` cleared, `057106`).
2. Runs **INQUIRY** then **READ CAPACITY** once (`INQUI`, sets `5SCIN`); for a
   `42` op it finishes at `RCAFI` returning the record/block size.
3. Reads **one** block: the **CONTROL RECORD** at the address in the unit's
   `MEMA1/MEMA2` (the last LBA), `X.ABP32=1` = 1 block (`057170..057210`). This
   is the single `READ(6)` the trace shows against LBA 129311.
4. In `FINEX` it checksum-validates the control record, extracts the partition
   table, and returns **`UHLIM`** (data-area size / geometry) plus a format
   status word `36` to the caller, then terminates the operation
   (`RETEX -> RETOP`).

That is the **entire** function-42 transaction: INQUIRY, READ CAPACITY, one
control-record READ(6), return geometry. It matches the ground-truth trace
byte-for-byte in shape.

For SMD/Winchester the same function `42` is a near-trivial "return the format
number from a static table" (`IP-P2-DISK-START.NPL` line 201-203); it does no
control-record disk read at all, and for non-`BDISK` controllers line 60 makes
it a silent no-op. So function 42 is handled by a **completely different**
routine per device class.

---

## 4. THE ANSWER: why is SCLLD never called for page 0 after function-42 success?

**Because function 42 does not include a page-0/block-0 data read, and the SCSI
disk layer is not designed to chain one.** Function 42 = "learn the disk
layout." It reads and validates the *control record* (partition table at the
last LBA), returns `UHLIM` + partitions, and terminates. The only SCSID/SCLLD
calls in the whole function-42 flow are INQUIRY, READ CAPACITY, and the single
control-record `READ(6)`. There is no fourth call, by design.

The page-0 / block-0 directory read is a **separate function-0 request** that
the *device-agnostic* `ENTER-DIRECTORY` / mount path must issue **after**
consuming the function-42 result. That second request would re-enter the disk
layer with `func==0`, fall through to the ordinary-transfer `SCSID(X)` call, and
only *then* reach `SCLLD -> INITO -> SCWAQ` for block 0. The live trace shows
that second request never arrives (total silence after the control-record read),
so the fault is **upstream of the SCSI driver and disk layer**, in the
device-agnostic mount hand-off (the companion carve's territory) - exactly as
the verified ground truth states.

Static SCSI bytes **cannot** decide *why* the upstream path skips the block-0
read, because that decision and its address arithmetic live in the
device-agnostic module, not here. The most likely upstream mechanisms (for the
companion agent) are: (a) `ENTER-DIRECTORY` treats the function-42 return as
"done" for SCSI and never issues the follow-up read; or (b) it computes the
block-0 address from the SCSI `UHLIM`/partition result and produces an
out-of-range or partition-relative address that is rejected before `SCSID` is
re-entered.

### The one settling runtime check
Trace/breakpoint **`SCSID` entry (057xxx)** and **`SCLLD` (067160B)**. After the
function-42 completion, watch for a **second** `SCSID` entry with
`(ABFUN & 077) == 0` (a READ) and a block-0 disk address:

- `SCSID` **never** re-entered  ->  the mount path never requested block 0 ->
  fault is entirely upstream in device-agnostic `ENTER-DIRECTORY` (it did not
  issue the read). The trace's silence already points here.
- `SCSID` re-entered but returns before `SCLLD` (e.g. `BADPA`, early `RETEX`) ->
  fault is the block-0 address translation of the SCSI request (UHLIM /
  partition base mis-scaled).

---

## 5. VERIFIED / INFERRED / OPEN

| # | Claim | Status |
|---|-------|--------|
| 1 | SCSI driver core lives in `065-S3SIPIT` (= `066-S3IIPIT`, 0 byte diffs), base `32000B` | VERIFIED (bytes + `cmp`) |
| 2 | `SCLLD@067160B` = `D:=A SHZ -10 <3` classify; `A=:X.SUCON`(STA ,X 30) | VERIFIED (dd `cc69 dcf8 f203 c631 a80e`) |
| 3 | `SCLLD` calls `INITO`(070261B) then, if `BUSFL==0`, `SELEC`(070165B) | VERIFIED (embedded ptr words @067241/067242) |
| 4 | `INITO` splices the unit datafield onto the `SCWAQ` tail (the enqueue) | VERIFIED (070310-070317 `LDA 60`/`RADD SB`/`STT ,X 27`) |
| 5 | `SELEC` with empty `SCWAQ` sets `BUSFL:=0` (bus free) and returns | VERIFIED (070171 `LDX SCWAQ` / 070172 `JXZ` / 070240 `STZ BUSFL`) |
| 6 | `SCINT@067247B` reads status via `HDEV+RSTAU`, `IOXT`, dispatches phase | VERIFIED (067247-067254) |
| 7 | Work enters the driver ONLY via `SCLLD -> INITO -> SCWAQ` | VERIFIED (no other writer of SCWAQ in the carved body) |
| 8 | Function 42 = INQUIRY + READ CAPACITY + one control-record `READ(6)`, return `UHLIM`, terminate | VERIFIED (NPL logic + coherent carve at 057074-057351, matches trace) |
| 9 | Function 42 issues NO block-0/page-0 read and does not chain one | VERIFIED (no 4th SCSID/SCLLD call on the func-42 path) |
| 10 | SMD/Winchester handle function 42 in a different routine (return format number / no-op) | VERIFIED (IP-P2-DISK-START.NPL 60, 201-203) |
| 11 | The block-0 read must be a separate function-0 request from the device-agnostic mount path | INFERRED (from disk-layer structure; the caller is out of scope) |
| 12 | The fault is upstream (device-agnostic ENTER-DIRECTORY never issues, or mis-addresses, the block-0 read) | INFERRED (consistent with trace silence + ground truth) |
| 13 | Whether upstream never-issues vs. mis-addresses the read | OPEN - settled by the runtime check in section 4 |

---

## Files

- `SCSI-DRIVER.ASM` - annotated, byte-verified disassembly of the driver core
  and disk-layer function-42/INQUI path.
- `SCSI-DRIVER.pseudo.c` - readable pseudo-C; the enqueue decision is the marked
  function `does_scsi_layer_enqueue_block0_after_func42()` (returns 0).
- `_driver.dis` / `_disklayer.dis` / `_inqui.dis` - complete raw disassembly
  ranges (regenerate with `../../_scsi_dump.sh`).

Source segment: `../../../segments/065-S3SIPIT.bin`
(sha in `065-S3SIPIT.meta.json`), disassembled with
`nd100-dis -a -o -b 13312` on the byte-swapped (little-endian) image.
