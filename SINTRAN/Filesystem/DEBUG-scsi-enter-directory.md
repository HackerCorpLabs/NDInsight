# DEBUG: `ENTER-DIRECTORY ,,DISC-SCSI-1,0` fails with "APPROACHING END OF ACCOUNTING FILE"

**Symptom:** the SCSI disk *boots*, but `@ENTER-DIRECTORY ,,DISC-SCSI-1,0`
always fails, and the console shows **`APPROACHING END OF ACCOUNTING FILE`**.

**Bottom line (VERIFIED from SINTRAN L source + carved kernel):** that message is
a **red herring**. It is SINTRAN error **243B** (163 decimal), a benign
accounting warning that is emitted from **one place only** - the RT-accounting
collector at logout - and has **no code path** from `ENTER-DIRECTORY`, `CHDSI`,
the mass-storage layer, or the SCSI driver. The real failure is a SCSI page-0
read/geometry problem whose genuine error code is almost certainly **232B "Device
error"** (or 141B "Transfer error"), being **mis-rendered** to the accounting
string in the emulator's error-message lookup, *or* the accounting warning is a
pre-existing, coincidental console state. Instrument the SCSI READ of page 0
first.

All numbers octal unless marked. Grading: **VERIFIED** = kernel/source/disk bytes
prove it; **INFERRED** = strong reasoning, not byte-proven; **OPEN** = unsettled.

> **See also - concrete fix plan:**
> [`SINTRAN/Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md`](../Devices/SCSI/SCSI-MOUNT-FIX-PLAN.md)
> ranks the root causes against the actual RetroCore C# controller lines and gives
> ordered, manually-testable fixes plus a `[SCSI-TRACE]` instrumentation plan. It
> also **re-grounds the SCSI-side claims below in the carved SINTRAN L bytes**:
> `SCDTS` (62107B) and `SCSID` (62217B) are confirmed present in the carved
> `006-S3FS`, but the exact internal-code -> user-code mapping (232B/141B/252B/224B)
> is currently only readable from NPL (a *different revision* than L) - treat that
> table as INFERRED until the 62107B region is hand-decoded.

---

## 1. What "APPROACHING END OF ACCOUNTING FILE" actually is

**VERIFIED.** It is SINTRAN error code **243B = 163 decimal**.

Error-message table (`Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference
Manual.md`, and `Developer/MON/Monitor Calls.md`):

```
| Octal | Decimal | Message                            |
| 242   | 162     | Segment not fixed                  |
| 243   | 163     | Approaching end of accounting file |   <-- our message
| 244   | 164     | Accounting file full               |
| 245   | 165     | No more unused spooling files ...  |
```

Where 243B is set - the **only** place in the entire source tree:

`SINTRAN/NPL-SOURCE/NPL/RP-P2-ACCRT.NPL` (the RT-accounting collector,
`RP-P2-ACCRT` = *Read/Process accounting RT-program*):

```
123125  %   CHECK THAT THERE IS ROOM IN THE ACCOUNTS:DATA FILE ...
123125          IF NMBR > RMAX THEN            % ACCOUNTS:DATA FILE FULL
123131             244=:ERFL; GO FAR SUIT6     % 244B = "accounting file full"
123142          ELSE
123143             IF A > DMAX THEN            % passed the WARNING threshold
123146                243=:ERFL                % 243B = "approaching end ..."
123150             FI
123150          FI
...
123341          *MON 2RTEX                     % RT EXIT  (this is an RT program)
```

Facts that make it a red herring (all VERIFIED):

- It is set **only** inside `RP-P2-ACCRT`, an **RT program** that runs at
  **logout** to append a record to `ACCOUNTS:DATA`. It is gated on
  `A > DMAX` - the number of accounting records already in the file exceeding the
  warning limit (default 1000, per `Reference-Manuals/ND-10315B SINTRAN III
  ACCOUNTING SYSTEM.md` and `Operations/SINTRAN/ND-30.003.007 EN SINTRAN III
  System Supervisor.md`).
- The manuals state the message is printed **"on the terminal at every log out
  whilst accounting is running"** (ND-10315B line 511; ND-60.128.5 line 5310) -
  **not** at mount time, and only if the ACCOUNTING system was started
  (`@INIT-ACCOUNTING` / `@START-ACCOUNTING`).
- `ENTER-DIRECTORY` never enters the accounting collector, never opens
  `ACCOUNTS:DATA`, and never touches `ERFL`. There is **no** call edge from the
  mount path to `RP-P2-ACCRT`.

**Conclusion (Hypothesis #4 - the "honest accounting" angle): RULED OUT.**
Mounting a directory cannot genuinely raise 243B. If the accounting message is
truly being generated, it is an *independent, coincidental* logout event on a
system where accounting happens to be running and the file happens to be near
full - unrelated to the SCSI mount.

---

## 2. Hypothesis #1 - the string is wrong / mis-indexed

**INFERRED (strong).** The genuine failure code from a SCSI `ENTER-DIRECTORY`
read error is **not** 243B - it is one of the mass-storage codes below. If the
emulator's error-to-string table is offset or truncated, the real code renders as
the wrong string, and 243B "approaching end of accounting file" is what the user
sees.

### What codes the SCSI / mass-storage read path can actually return

The SCSI driver's **internal** error codes are tiny T-register values (they are
**not** the 2xxB user codes):

`SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT`:

```
ILRCS=000002   NOLUN=000007   BADPA=000005   ILNOD=000006
```

They are translated to the **user-facing** SINTRAN error numbers by routine
**`SCDTS`** ("determine status", `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`
line 1010 / `s3vs-4.symb` 061511):

```
061514   X:=17/\A; X:=SCSTA(X)        % low 4 status bits -> code via SCSTA table
061526   IF D=TYPER THEN X:=240       % Illegal device type
061533   IF D=ILAOP THEN X:=201       % Illegal function code
061540   IF D=BADPA THEN X:=174       % Illegal parameter
061545   IF D=ILNOD OR D=NOLUN THEN X:=33    % No such logical unit
061555   IF D=COPNP THEN X:=3206      % Illegal request
061562   IF D=TRANE THEN X:=141       % Transfer error
061567   IF D=SBUSY THEN X:=3207      % Device busy
061574   IF D=RCONF THEN X:=3210      % Reservation conflict
061601   IF D=NESER THEN X:=3211      % Device do not answer
061606   ELSE          X:=232        % DEVICE ERROR  <-- default catch-all
```

**None of these is 243B.** A hard SCSI read failure of page 0 surfaces as
**232B "Device error"** (the default), or 141B "Transfer error", 33B "No such
logical unit", 240B "Illegal device type", 174B "Illegal parameter", or the 32xxB
SCSI-status range. Two other geometry-specific codes from the mass-storage layer
are relevant: **252B "Not a multiple of hardware block size"** and **224B
"Incompatible device sizes."**

So: the code the mount path really returns is one of
`{232, 141, 33, 252, 224, 240, 174}`B. If the user's console prints 243B, either
the emulator mis-maps one of those into 243B's slot, or (per section 1) the
accounting message is unrelated. **Test:** capture the *numeric* error the mount
returns (see checklist), do not trust the rendered string.

---

## 3. The `ENTER-DIRECTORY` -> page-0 read path (`CHDSI`)

Segment `006-S3FS`, load address **26000B**
(`tools/sintran-segment-carver/versions/L-VSX-500/segments/006-S3FS.bin`;
annotated dis at `.../re/006-S3FS.annotated.dis`). Anchors (VERIFIED, carved
SINTRAN L bytes):

- **`CHDSI` = 37763B** - enter/validate a directory.
- **`RXDIR` = 37643B** - read the 8-word extended-info block from page 0.
- **`WXDIR` = 37702B** - recompute checksum + write the block back.
- **`REENB` = 40162B** - clear the "entered" flag on release.

`CHDSI` reads page-0 word **1750B** (byte **2000** / `0x07D0`) - the 8-word
extended-info block - then the 32-byte master block at word **1760B** (byte
**2016** / `0x07E0`). Page 0 = **1024 words = 2048 bytes**, big-endian
(`SINTRAN/Filesystem/on-disk-format/extended-info-block.md`,
`.../directory-label.md`).

**Critical VERIFIED behaviour (from `SINTRAN/Filesystem/NDFS-VALIDATION.md`
Q1/Q7):** on a **bad or zero checksum, `CHDSI` does NOT reject the mount** - it
**zeroes and REBUILDS** the extended-info block (writes the geometry-derived
capacity into words 1756-1757B, stamps the owner system number + "entered" flag
bit 15, recomputes the checksum via `WXDIR`, and writes it back). Consequences:

- A page-0 read that returns **garbage** does **not** produce a checksum-mount
  error. It triggers a **write-back**. So the surfaced failure is either
  (a) the *write* of the rebuilt block failing -> **232B device error**, or
  (b) a silent "success" onto corrupt geometry that fails again on the next
  enter - matching the user's *"always the same"* symptom.
- `CHDSI` also compares the stored page capacity (words 1756-1757B) against a
  **device-geometry figure** it derives from the unit datafield (which is
  populated from SCSI **READ CAPACITY**). A wrong READ CAPACITY value feeds a
  wrong geometry into this compare/rebuild. (Capacity-compare is VERIFIED in
  NDFS-VALIDATION Q3; the exact error code on a capacity mismatch is **OPEN** -
  see the live-trace note in section 6.)

The read that `CHDSI`/`RXDIR` issues goes through the mass-storage layer to the
SCSI driver as an ordinary page read (transfer unit = one filesystem page = 1024
words = 2048 bytes at page 0 / LBA-equivalent 0). Any driver-level failure of
that read returns through `SCDTS` (section 2).

For the full carved-bytes trace of how the mount reaches this read - the
`ENDIR` 140176B command worker, the `RXDIR`/`RCBLO` page-cache lookup that
dispatches the transfer through the device datafield pointer, and the exact
disk-controller read contract - see
[`code-logic/enter-directory.md`](code-logic/enter-directory.md).

---

## 4. SCSI page <-> sector geometry the driver requires

**VERIFIED from `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`.** The driver is
**self-configuring from READ CAPACITY**, but it is built around a **1024-byte
logical block**:

- **MODE SELECT sets the drive to 1024-byte blocks** (line 556):
  ```
  057574   2000; *STATX 50        % BLOCK SIZE 1024 BYTES   (2000B = 1024 dec)
  ```
- The direct-access "record size" base is **1024 bytes** (lines 1267/1270,
  `X:=12` octal = **10** decimal = log2(1024)):
  ```
  062425   X:=12                  % RECORD SIZE 1024 BYTES  (direct-access disk)
  ```
- It issues **READ CAPACITY** (opcode `22400`B) to learn the actual block size
  (lines 1234-1237, `INQUI`/geometry init), stores it in `SURSZ`, and builds
  three **shift instructions** `SUSI1/SUSI2/SUSI3` (`*EXR SA`/`*EXR ST`) that
  scale between ND page/word/byte quantities and SCSI **LBA + block count**:
  ```
  062434   IF X=:D>1 THEN         % X starts = 10 (log2 of the 1024-byte base)
  062444   DO WHILE A NBIT 0      % shift the reported block size right ...
  062446      X-1; A SHZ -1       % ... decrementing X per bit
  062450   OD
  062451   IF A><1 GO FAR RSZER   % block size NOT a power of 2 -> ILLEGAL RECORD SIZE
  062454   D-X; IF X<0 THEN X:=0 FI   % byte-and-record shift
  062460   SUSI3; X\/A=:SUSI1; A\/D=:SUSI3   % build the shift instructions
  ```
- The READ command builder (lines 1480-1533) uses those shifts to turn the ND
  amount/address into the SCSI byte count / LBA, and chooses **READ(6)** vs
  **READ(10)** ("10 byte format necessary" when the LBA or count overflows the
  6-byte fields, lines 1504/1533).

**Required geometry the emulated drive must present, therefore:**

1. **READ CAPACITY must return a power-of-two logical block size** (the driver
   was configured for **1024** bytes; 512 also works arithmetically because the
   shift adapts, but the block size **must be a power of two** or the driver aborts
   with **ILRCS = 2 -> ILLEGAL RECORD SIZE**, line 062522 `RSZER: T:=ILRCS`).
2. **READ CAPACITY's block size and the READ data payload must agree.** If READ
   CAPACITY says 1024 but the READ actually delivers 512-byte sectors (or vice
   versa), the shift-derived LBA/count is wrong and page 0 reads the wrong bytes.
3. **READ CAPACITY's max-LBA (capacity) must be right.** It feeds `CHDSI`'s
   capacity compare/rebuild (section 3) and the pages-available field.
4. One ND filesystem page = **2048 bytes = 2 x 1024-byte SCSI blocks = 4 x
   512-byte sectors.** Any off-by-one or wrong sectors-per-page makes page 0
   (master block + extended info at byte 2000/2016) read from the wrong LBA.

`RSZER`->`ILRCS(2)` is translated by the caller; note `SCDTS` does **not** list
`ILRCS` explicitly, so it falls to the **232B default** at the user level.

---

## 5. Why boot works but ENTER-DIRECTORY fails (INFERRED)

Boot uses the ROM/`MASB` bootstrap path with fixed, hard-wired transfer
parameters (a raw load of the boot area, low LBAs, small fixed count) and does
**not** depend on READ CAPACITY, the `SUSI` shift instructions, the master/
extended-info block, or `CHDSI`. `ENTER-DIRECTORY` is the **first** operation that
(a) runs the full READ-CAPACITY-driven geometry, (b) reads page-0 *metadata* at
byte 2000/2016, and (c) validates/rebuilds it. So a geometry or status-byte bug
that boot tolerates will first bite at mount. This is consistent with "boots but
won't mount, always the same."

---

## 6. Ranked checklist - what to instrument in the SCSI emulator

Ranked by likelihood given the evidence above.

1. **Capture the real numeric error, not the string.** Break in the emulator
   where the mount returns its error and read the code. If it is **232B / 141B /
   33B / 252B / 224B**, the "accounting" string is a **rendering bug** in the
   error-message table - fix the table mapping and chase the real code below. If
   the code genuinely is **243B**, accounting is running and the message is an
   unrelated logout event (section 1) - separate problem.

2. **READ CAPACITY correctness (highest-value data bug).** Verify the emulated
   drive returns: a **power-of-two block size** (ideally **1024**), the **correct
   last-LBA/capacity**, in the exact byte layout the driver's `LDDTX` expects
   (`IP-P2-SCSI-DISK.NPL` 062357-062365, big-endian 32-bit). A non-power-of-two
   size aborts with ILRCS; a wrong capacity corrupts `CHDSI`'s capacity compare.

3. **Block size vs actual READ payload agreement.** Confirm the READ data for
   LBA 0..n delivers exactly the sector size READ CAPACITY advertised. A
   1024-vs-512 mismatch (READ CAPACITY says one, transfer uses the other) makes
   the `SUSI1/2/3` shift math read page 0 from the wrong LBA -> garbage master/
   extended-info block. **This is the prime suspect** the task flagged.

4. **LBA / block-count mapping and READ(6) vs READ(10).** Instrument the exact
   opcode + LBA + transfer length the driver issues for the first page-0 read.
   Expect a read of the **first 2048 bytes** (= 2 blocks @1024 / 4 sectors @512)
   at LBA 0. Watch for off-by-one LBA and wrong block count (the 6-byte vs
   10-byte format switch at `IP-P2-SCSI-DISK.NPL` 1504/1533).

5. **Byte order of the returned page.** Page 0 is **big-endian words**; the
   extended-info checksum is a 16-bit additive sum of words 1751-1757B, master
   block at 1760B. If the emulator byte-swaps the payload, the checksum mismatches
   -> `CHDSI` rebuilds and writes back (section 3); if the write-back path also
   mis-orders bytes it never converges -> persistent failure.

6. **SCSI status / phase bytes.** The low 4 bits of the status word index
   `SCSTA` (`IP-P2-SCSI-DISK.NPL` 1010) and feed `SCDTS`. A bogus non-zero status
   (CHECK CONDITION not cleared, wrong message/phase sequence, missing
   REQUEST SENSE handling) forces the `232B`/`141B` device-error path even when
   the data would have been fine. Verify status = 0 (GOOD) on a clean read and
   that sense data is well-formed.

7. **Write-back path.** Because `CHDSI` **rebuilds and writes** page 0 on any
   checksum miss, verify WRITE(6)/WRITE(10) to LBA 0 works with the same geometry
   as READ. A read that is merely garbled but a write that fails yields a device
   error at exactly mount time.

### If you can run a live nd100x DAP trace

Break at **`CHDSI` = 37763B** (segment `006-S3FS`, loaded at **26000B**; absolute
break address depends on where the resident FS segment is mapped in your run) and:

- Single-step into `RXDIR = 37643B` and dump the **2048 bytes** it read for page
  0. Compare against the on-disk `SMD0.IMG`-equivalent bytes at `0x07D0`
  (extended info) and `0x07E0` (master block). If they differ, it is a
  read/geometry bug (items 2-5); if they match but the enter still fails, it is
  the capacity compare or the write-back (items 2, 7).
- Watch whether execution reaches the **rebuild** branch (zero-fill loop + `WXDIR`
  = 37702B). Reaching rebuild every time = the read data never validates =
  geometry/byte-order bug.
- At the point the mount returns to the command interpreter, read the T/A error
  code and confirm whether it is 232B (real) vs whether the console still prints
  243B (rendering bug).

---

## 7. Evidence / provenance

- Error table: `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md`
  (243B/244B), `Developer/MON/Monitor Calls.md`.
- Accounting-only source of 243B: `SINTRAN/NPL-SOURCE/NPL/RP-P2-ACCRT.NPL`
  (123146 `243=:ERFL`), gating + "at logout" semantics in
  `Reference-Manuals/ND-10315B SINTRAN III ACCOUNTING SYSTEM.md` and
  `Operations/SINTRAN/ND-30.003.007 EN SINTRAN III System Supervisor.md`.
- SCSI error translation `SCDTS`: `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`
  (~line 1010) / `SINTRAN/NPL-SOURCE/s3vs-4.symb` 061511-061607; internal codes in
  `SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT`.
- SCSI geometry / block size / READ CAPACITY / shift instructions / READ(6/10):
  `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL` lines 556, 1234-1345, 1480-1533.
- `CHDSI`/`RXDIR`/`WXDIR` behaviour + rebuild-on-bad-checksum + capacity compare:
  `SINTRAN/Filesystem/NDFS-VALIDATION.md`,
  `SINTRAN/Filesystem/on-disk-format/extended-info-block.md`,
  `SINTRAN/Filesystem/on-disk-format/directory-label.md`; carved segment
  `tools/sintran-segment-carver/versions/L-VSX-500/segments/006-S3FS.bin`
  (load 26000B) and `.../re/006-S3FS.annotated.dis`.
