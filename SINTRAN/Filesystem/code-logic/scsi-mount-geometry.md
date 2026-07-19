# SCSI mount - the control-record connect, and the missing block-0 read

**THE framing (do not reintroduce the old one).** On `@ENTER-DIRECTORY,,DISC-SCSI-1,0`
the SCSI activity is: **INQUIRY -> READ CAPACITY -> one READ(6) of the last block
(lba 129311)**. That last-block read is the SCSI **control record** read, performed by
the driver's **function-42 connect/init** to learn the disk geometry (partition table,
`UHLIM`). This is **correct and expected** device behaviour - it is NOT a "geometry
probe", NOT a "capacity leak", and NOT SINTRAN "expecting the directory master at the
last block". That idea is wrong and dead; do not resurrect it.

**THE BUG (priority #1):** the directory master at **block 0 is never read**, so the disk
is never mounted. The whole investigation is about *why block 0 is not read* - not about
the last-block read, which is legitimate.

> Verified end to end from the live instruction+register trace
> `C:\Users\ronny\AppData\Local\trace\file-trace.txt`: the mount issues exactly one data
> read, of lba 129311, and no read of block 0. TYPER/device-type is ruled out (the gate
> passes with device type = 0). The SCSI disk layer and driver would read block 0
> correctly if asked (`fn 0 -> CACOB -> EXCOM -> SCLLD`); the fault is upstream, in the
> filesystem's directory-read issue. Full carve set:
> `tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/` (ENTER-DIRECTORY,
> CHDSI-COMPLETE, RXDIR-CACHE-COMPLETE, SCSI-DISKLAYER-COMPLETE, SCSI-DRIVER-COMPLETE, ...).

This note reconciles the trace with the **carved SINTRAN L bytes** of segment `006-S3FS`
(load base **26000B**) and the RetroCore C# SCSI target. The byte-level facts in the
sections below (LBA arithmetic, checksum, `SS_GOOD`) remain valid; the last-block read is
the control-record connect, and the open question is the missing block-0 read.

**Evidence grades**

- **VERIFIED** - proven from the carved `006-S3FS` bytes (disassembly shown), the
  RetroCore C# source (line-cited), or arithmetic that is fully determined.
- **INFERRED** - strong reasoning from the bytes + architecture, not one decisive
  instruction.
- **OPEN** - crosses into an uncarved resident overlay; the boundary is stated and
  what a live trace would pin is named.

ND addresses are **octal**; SCSI LBAs and block counts are **hex/decimal** as
marked. Disassembly is the byte-identity-checked whole-segment listing
[`006-S3FS.asm`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm);
opcodes grounded in
[`ND100-INSTRUCTION-SEMANTICS.md`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md).

---

## 0. TL;DR

1. **The last-block read is NOT the directory read.** The carved enter-directory
   filesystem path (`ENDIR` 140176B -> `CHDSI` 37763B -> `RXDIR` 37643B) reads
   **block 0** (`RADD CLD 0 DD` feeds `RCBLO` with block number 0). The SCSI
   driver init (INQUIRY + READ CAPACITY, function `42`) does **not** read the last
   block either. So the `READ(6)` of `LBA 0x1F91F` is a **separate operation** that
   runs *before* `CHDSI` ever reads block 0. **VERIFIED (carved + NPL).**

2. **The LBA is intentional, not a capacity leak.** `READ CAPACITY` returns
   `lastLBA = 0x1F91F = 129311` (RetroCore `DiskSizeInBlocks = cylinders*heads*
   sectors - 1 = 898*8*18 - 1`). The `READ(6)` targets **exactly that last LBA** -
   the highest addressable block. Total blocks = `129312` = **64656 ND pages of
   2048 bytes, exactly** (`129312 * 1024 / 2048 = 64656`, integer). The read is
   in-bounds, returns the real last block, `SS_GOOD`. **CORRECTED framing:** this
   is a **function-42 READ FORMAT read of the CONTROL RECORD** (the control record
   is stored in the last block - `IP-P2-SCSI-DISK.NPL` 057170 `% ADDRESS OF
   CONTROLL RECORD`), i.e. disk initialization, not a "size/presence probe" and
   not a capacity leak. LBA = last LBA because that is where the control record
   lives. **VERIFIED (arithmetic + NPL control-record logic).**

3. **Why it stops before block 0 is an uncarved-overlay question.** The probe
   read is issued by the **resident mass-storage connect/verify layer**, which is
   *not* part of carved `006-S3FS`. The carved segment proves what the probe is
   *not* (not `CHDSI`, not the driver init) but the compare/branch that consumes
   its result lives in the resident overlay. The two live candidates are
   (a) a **size/parameter reject** or (b) the **lost NCR completion interrupt**
   (`RSTAU`-clear bug) stranding the probe read so `CHDSI` is never entered.
   **OPEN** - a live trace pins it (Section 6).

4. **What the emulated disk must provide** (Section 5): a page-aligned capacity
   (even total-blocks so it is a whole number of 2048-byte ND pages - already
   satisfied at 129312/64656), a readable last block at `LBA = lastLBA` (already
   satisfied), and - the actual blocker - the controller must deliver the
   **completion interrupt** for that read to the ND-100 so the mount advances to
   the block-0 read.

---

## 1. The carved enter-directory path reads BLOCK 0, not the last block

VERIFIED from `006-S3FS`. `CHDSI` (37763B) is the enter/validate worker; its first
disk access is `RXDIR` (37643B), reached through the literal pointer at 040143:

```
037777  146037   RADD SB DX
040000  135143   JPL I 143      ; -> [040143]=037643  RXDIR   (read page 0)
040001  124134   JMP 134        ; RXDIR error exit
040002  170401   SAA 1          ; begin 8-word checksum loop over block-0 ext-info
...
040022  050401   LDT ,B 1
040023  135121   JPL I 121      ; -> [040144]=037101  GSIZE   (get configured size)
...
040027  026006   LDD ,X 6       ; DD = stored capacity (block-0 words 6-7)
040030  140065   SKP IF DA EQL ST   ; compare vs GSIZE-derived geometry (hi word)
040032  142041   SKP IF DD UEQ SL   ; (lo word)
```

`RXDIR` sets the block number to **0** and calls the page-cache reader `RCBLO`:

```
037650  146105   RADD CLD 0 DA
037651  146101   RADD CLD 0 DD  ; 32-bit block number = 0
037652  135023   JPL I 23       ; -> [037675]=035766  RCBLO  (reserve/read block 0)
```

So the mount's directory read is **block 0 -> LBA 0**, and the block address that
would reach the SCSI CDB builder for it is **0**, never `0x1F91F`. The capacity
compare at 040027-040032 uses `GSIZE` (37101B), which reads an **in-core**
disk-parameter table (helpers at 50124B/50220B/50223B/50226B are parameter
getters with a bounds check to error `174`, not disk transfers) - no device read.

**Conclusion (VERIFIED):** nothing on the carved `ENDIR/CHDSI/RXDIR/GSIZE` path
forms a read of the last block. The `LBA 0x1F91F` read is upstream of `CHDSI`.

---

## 2. The SCSI driver init does not read the last block either

The carved SCSI driver command-builder is in this same segment (`SCSID` 62217B,
`SCDTS` 62107B, `SCSI1` 46530B, `SCSI2` 46661B; datafield offsets `SUSI1..3`=24/25/26B,
`SURSZ`=41B, `SUTYP`=23B, `SMBP1/2`=42/43B). The device-init function
(`ABFUN = 42`, "INQUIRY AND READ CAPACITY") is INQUIRY then, for a direct-access
device, READ CAPACITY - and then it returns. Cross-referenced in
`SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL` (INQUI, 062215+):

```
062273   22400            % READ CAPACITY
062357   *LDDTX 20        % read bytes 4-7 (block length) ...
062365   A:=D=:SURSZ      % ... -> SURSZ (block size); the last-LBA at offset 0
                          %     is NOT stored, and NO read of it is issued
062460   ... =:SUSI1 ... =:SUSI3   % build shift instructions, then
062506   IF 77/\ABFUN=42 GO RCAFI  % finish init and return
```

**VERIFIED (NPL) / INFERRED (carved region is PLANC data-before-code and not
linearly clean here):** init keeps only the **block size** and the shift
instructions; it does not read, and does not even retain, the capacity. So the
`READ(6)` of the last block is **not** part of `INQUIRY+READ CAPACITY`. (NPL is a
different revision than L; treated as INFERRED where not byte-proven.)

The CDB address field is built by `CACOB` from the caller's amount/address via the
`SUSI1` shift (`IP-P2-SCSI-DISK.NPL` 063477 `ABPA2; T:=SUSI1; *EXR ST`). With
`blockSize = 1024` the driver's record base is also 1024 bytes
(`062425 X:=12` = log2 1024), so the address shift is **0** and the SCSI LBA equals
the ND block address the caller passed. Therefore `LBA 0x1F91F` means **the caller
passed block number `0x1F91F`** - the last block - deliberately.

---

## 3. Why the last block, and why it is the capacity value (not a bug)

**VERIFIED (RetroCore C#).** `SCSIHDD.cs`:

- `DiskSizeInBlocks => cylinders*heads*sectors - 1` (line 851-856). Default
  geometry `898*8*18 = 129312`, minus 1 = **129311 = 0x1F91F**.
- `CommandReadCapacity()` returns `DiskSizeInBlocks` in bytes 0-3 and `sectorbytes`
  in bytes 4-7 (lines 481-482). Because `DiskSizeInBlocks` already subtracts one,
  the reported value **is the true last LBA** - `READ CAPACITY` is spec-correct
  here (last LBA = total - 1), *not* off-by-one.
- `SC_READ_6` (lines 254-284): `lba = get_u24be(1) & 0x1fffff`; bounds test
  `if (lba > DiskSizeInBlocks)` -> `129311 > 129311` is false, so it reads
  `readBlock(129311)` at byte offset `129311*1024`, one full 1024-byte block ending
  exactly at the image end (`129312*1024`), returns `SS_GOOD` with real data.

**Arithmetic that makes this a legitimate probe (VERIFIED):** total 129312 blocks x
1024 B = 132,415,488 B = **64656 ND pages** of 2048 B, exact. The medium is a whole
number of ND filesystem pages, the top block is real and readable.

**CORRECTED (control record, not a probe):** reading `LBA = lastLBA` is the
**function-42 READ FORMAT** read of the **control record**, which SINTRAN stores in
the last block of the volume. Function 42 is disk initialization - it reads the
control record to obtain the defect/reallocation table and disk-parameter block and
to set `UHLIM` (`IP-P2-SCSI-DISK.NPL` FINEX 057261+). The LBA equals the capacity
value **because the control record is located at the last block**, not because the
capacity leaked into an unrelated transfer, and not because SINTRAN is "probing" the
top block. This is a self-contained init operation that completes successfully;
block 0 is simply never reached afterwards (Section 4 and the RCBLO carve).

**INFERRED (which layer):** this probe is issued by the SINTRAN **mass-storage
connect / device-verify** code that runs when the unit is first touched -
`ENDIR` reserves the unit with `MON 124` (140252B, `ForceReserve`) before calling
`CHDSI`, and the first physical transfer through the resident mass-storage driver
triggers device init (INQUIRY+READ CAPACITY) and the size-verify. That connect/
verify primitive is a **resident overlay and is not carved in `006-S3FS`** - the
same OPEN boundary already documented for the transfer primitive in
[`enter-directory.md`](enter-directory.md) Section 4.2.

---

## 4. What SINTRAN validates on that block, and why it rejects before block 0

The probe read returns `SS_GOOD`, so **readability is not the failure**. The stop
therefore comes from what the connect layer does *with* the result, which is in the
uncarved overlay. Two candidates, both consistent with "reads last block, then
stops, mount fails":

### 4a. A size/parameter reject (INFERRED)

The carved `CHDSI` proves SINTRAN *does* reconcile disk size: 040023 calls `GSIZE`
and 040027-040032 compares a stored capacity against the geometry figure. If the
connect-layer verify compares the SCSI-discovered geometry (129312 blocks / 64656
pages) against the **configured disk-type** for `DISC-SCSI-1` and they differ, it
aborts. Candidate operator codes (reference manual
[`ND-60.128.5`](../../../Reference-Manuals/ND-60.128.5%20EN%20SINTRAN%20III%20Reference%20Manual.md)):

| Octal | Meaning | Note |
|-------|---------|------|
| 224B | Incompatible device sizes | manual scopes it to COPY-DEVICE, so **unlikely** the enter-directory code |
| 252B | Not a multiple of hardware block size | geometry/block-size reconcile failure |
| 232B | Device error (SCSI default via `SCDTS`) | catch-all if the verify's status handling fails |

The exact code is **OPEN** - it is decided in the resident overlay, and the carved
`SCDTS` region (62107B) is PLANC data-before-code and not yet hand-decoded, so its
internal->user code table is only readable from NPL (a different revision).

### 4b. Lost completion interrupt (INFERRED, currently the stronger fit)

"Reads the last block with `SS_GOOD` and then **STOPS**" (no further CDB) matches a
**hang**, not a clean reject. Per
[`SCSI-MOUNT-FIX-PLAN.md`](../../Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md) root cause #1,
`NDBusDiscControllerSCSI.cs:905` clears `InterruptFromNCR5386` on **every RSTAU
status read**. The size-verify probe is the **first interrupt-driven** SCSI
transaction of the mount; if its NCR completion interrupt is cleared by a status
poll before `StepGoState` delivers the ND-100 level-11 IRQ, SINTRAN never sees the
read finish, never issues the block-0 read, and eventually times out. A timeout
surfaces as **232B device error**, which the existing analysis shows is then
**mis-rendered** as the 243B "approaching end of accounting file" string (see
[`../DEBUG-scsi-enter-directory.md`](../DEBUG-scsi-enter-directory.md) Section 1-2).
This reconciles the console string, the "always the same," and the single trailing
`SS_GOOD` read.

**Either way, block 0 is never reached** because the mount is stranded/aborted at
the size-verify probe.

---

## 5. What the emulated disk (and controller) must provide to reach block 0

Concrete, testable contract for the `DISC-SCSI-1` unit:

| # | Requirement | Status on the traced run | Basis |
|---|-------------|--------------------------|-------|
| 1 | `READ CAPACITY` reports `lastLBA = total_blocks - 1`, `blockSize` a power of two (1024) | **Met** - `129311`, `1024` | VERIFIED (C# lines 481, 851) |
| 2 | `total_blocks` even, so the medium is a whole number of 2048-byte ND pages | **Met** - `129312 = 64656` pages | VERIFIED (arithmetic) |
| 3 | Block at `LBA = lastLBA` (`0x1F91F`) is readable and returns real data | **Met** - `SS_GOOD` | VERIFIED (C# lines 266-278) |
| 4 | The configured geometry (`898*8*18`) matches the actual image file length | **Verify this** - default `hdinfo` geometry vs the mounted image size | OPEN (image-dependent) |
| 5 | Controller delivers the **NCR completion interrupt** to the ND-100 (level-11) for the size-verify read, acknowledged only on `RITRG`, not cleared on `RSTAU` | **Prime suspect - likely NOT met** | VERIFIED seam (C# line 905) / INFERRED |

**So the disk image itself is almost certainly fine** (requirements 1-3 pass, and
the last-block read succeeds with real content). The remaining blocker is
**requirement 5** - the controller interrupt hand-off. Apply
[`SCSI-MOUNT-FIX-PLAN.md`](../../Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md) FIX 1
(acknowledge on `RITRG`, do not clear on `RSTAU`) first; then confirm requirement 4
by checking that the configured `cylinders*heads*sectors` matches the mounted
image's block count (if the image is not `898*8*18` blocks, the size-verify may
legitimately reject on a geometry mismatch - requirement 4).

There is **no on-disk "label" required at the last block** for the read to be
accepted - the probe checks presence/readability of the top block, and (candidate
4a) reconciles *size*, not a signature. So the fix is capacity/geometry + interrupt
delivery, not authoring special bytes at `LBA 0x1F91F`.

---

## 6. What a live trace must pin (the OPEN items)

The reject/stop condition cannot be closed statically because the size-verify
primitive is in the resident mass-storage overlay, not carved `006-S3FS`. A live
nd100x run resolves it:

- **Break at `CHDSI` = 37763B** (segment `006-S3FS`, load 26000B; absolute address
  = mapped base + offset). **If `CHDSI` is never hit**, the mount aborts/hangs in
  the connect/verify overlay *before* the directory read - confirming the
  last-block probe is the stop point (Section 3-4), and you are in case 4a/4b.
- **Watch the SCSI CDB stream** (`[SCSI-TRACE]` in `NDBusDiscControllerSCSI.cs`):
  confirm order INQUIRY -> READ CAPACITY -> `READ(6) LBA 0x1F91F`, and that **no**
  `READ` of `LBA 0` follows.
- **Watch the interrupt flag** on the size-verify read: the fingerprint of case 4b
  is `INT set by NCR` immediately followed by `INT cleared by RSTAU read` with **no**
  `INT processed ... level11` in between (fix-plan Section 4 step 5).
- **Read the T/A error at the command-interpreter return**: `232B` (device error,
  likely from a lost-interrupt timeout) vs a size code (`252B`) distinguishes 4b
  from 4a. Do **not** trust the console string (`243B` is a rendering artifact).

---

## 7. VERIFIED / INFERRED / OPEN summary

| Claim | Verdict |
|-------|---------|
| Carved `ENDIR/CHDSI/RXDIR` reads **block 0** (block number 0), never `0x1F91F` | VERIFIED (carved) |
| `GSIZE` + helpers read an in-core parameter table, not a disk block | VERIFIED (carved) |
| SCSI init (`ABFUN 42`) = INQUIRY+READ CAPACITY, keeps only block size, no last-block read | VERIFIED (NPL) / INFERRED (carved SCSID region is data-before-code) |
| `READ CAPACITY` returns true last LBA `129311` (`DiskSizeInBlocks = C*H*S-1`); spec-correct, not off-by-one | VERIFIED (C#) |
| Total 129312 blocks = 64656 ND pages exactly; last block in-bounds, `SS_GOOD` | VERIFIED (arithmetic + C#) |
| The `READ(6) LBA 0x1F91F` is a **function-42 READ FORMAT read of the CONTROL RECORD** (stored in the last block); LBA = last block by design, not a leak bug and not a size/presence probe | VERIFIED (NPL control-record logic) |
| Function 42 is a self-contained init op (reads control record, sets `UHLIM`) that completes before `CHDSI` reads page 0 | VERIFIED (NPL) |
| The page-0 read is never enqueued (SCWAQ empty); mount aborts with no further I/O. Exact fault instruction UNDETERMINED (connect/init overlay vs early RCBLO exit); factor-2 REFUTED as cause | OPEN (live trace) - see [`RCBLO/README.md`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/RCBLO/README.md) |
| Exact reject code (`232B`/`252B`/...) | OPEN (resident overlay; `SCDTS` 62107B not hand-decoded) |
| Disk image + capacity are adequate; blocker is interrupt delivery + geometry-match, not last-block content | INFERRED (from 5.1-5.3 passing) |

**Provenance:** carved `006-S3FS` SINTRAN L bytes (`ENDIR` 140176B, `CHDSI` 37763B,
`RXDIR` 37643B, `GSIZE` 37101B, `RCBLO` 35766B, `SCSID` 62217B, `SCDTS` 62107B,
`SCSI1` 46530B), load base 26000B, disassembly
[`006-S3FS.asm`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm);
NPL cross-ref `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL` (INQUI/CACOB, different
revision); RetroCore `Emulated.HW/Common/SCSI/SCSIHDD.cs` (lines 254-284, 456-488,
851-856); reference manual
[`ND-60.128.5`](../../../Reference-Manuals/ND-60.128.5%20EN%20SINTRAN%20III%20Reference%20Manual.md)
error table.

## See also

- [`enter-directory.md`](enter-directory.md) - the carved block-0 mount trace
  (`ENDIR`/`CHDSI`/`RXDIR`/`RCBLO`), which this note extends with the SCSI-specific
  last-block step that runs first.
- [`../DEBUG-scsi-enter-directory.md`](../DEBUG-scsi-enter-directory.md) - the 243B
  "accounting file" red-herring diagnosis and `SCDTS` code translation.
- [`RCBLO/README.md`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/RCBLO/README.md)
  - the carved `RCBLO` disk-cache manager, the control-record correction, and the
  closed `RCBLO -> ,X 14 -> SCSDISK -> SCLLD` chain.
- [`../../Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md`](../../Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md)
  - ranked controller fixes (FIX 1 = the `RSTAU`/`RITRG` interrupt-ack bug).
