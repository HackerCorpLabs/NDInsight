# SCSI Mount Fix Plan - `ENTER-DIRECTORY ,,DISC-SCSI-1,0`

**Goal:** get `@ENTER-DIRECTORY ,,DISC-SCSI-1,0` to mount a SCSI disk in the
RetroCore ND-100 emulator. The disk *boots* but the mount always fails and the
console shows the red-herring **`APPROACHING END OF ACCOUNTING FILE`**.

This plan correlates the **carved SINTRAN L** mount path (ground truth, not NPL)
with the actual **C# SCSI controller** code and the already-known firmware bugs,
then gives a **ranked, manually-testable** fix list and a **tracing plan**.

**Grading:** **VERIFIED** = proven by carved SINTRAN L bytes or by reading the C#
source; **INFERRED** = strong reasoning, not byte-proven; **OPEN** = unsettled.

All ND-100 addresses are **octal**; C# values are **hex/decimal**.

**Carved provenance (SINTRAN side):** segment `006-S3FS`, load address **26000B**
(11264 dec) - `tools/sintran-segment-carver/versions/L-VSX-500/segments/006-S3FS.bin`,
annotated disassembly `.../L-VSX-500/re/006-S3FS.annotated.dis`. Opcodes grounded
in `tools/sintran-segment-carver/versions/L-VSX-500/re/instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md`.

**C# provenance (RetroCore, outside this repo):**
- Controller: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`
- NCR chip: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\NCR\SCSI\NCR5386\NCR5386SCSI.cs` (+ `.CommandHandling.cs`, `.StateHandling.cs`)
- Disk target: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\Common\SCSI\SCSIHDDMicropolis.cs`, `...\SCSIHDD.cs`

---

## 0. Architecture reality (VERIFIED - read from C# + carved symbols)

The C# emulator **does not emulate the ND-3201 Z80**. It exposes the NCR 5386
registers directly on the ND-100 IOX map (offsets 0x20-0x3D) and passes ND-100
IOX reads/writes straight through to the `NCR5386SCSI` class. The SINTRAN driver
(`SCSID` = **62217B**, carved) therefore **drives the NCR 5386 itself**: it sets
Own ID / Destination ID / transfer counter, sends the CDB, and DMAs the data. The
emulated **disk target** (`SCSIHDDMicropolis` -> `SCSIHDD`) answers INQUIRY, READ
CAPACITY, MODE SELECT, READ(6/10), etc.

Consequence for triage: the mount read is an ordinary interrupt-driven,
multi-phase NCR transaction. Completion is signalled to the ND-100 by
`Ncr5386_OnInterrupt` -> `regs.InterruptFromNCR5386` -> `SetInterruptBit(true)` ->
ND-100 **level 11** IRQ. **This interrupt hand-off is where the mount breaks.**
`ExecuteGo()` being an empty stub (bug #5) is *not* fatal precisely because
SINTRAN programs the NCR directly - it matches the TODO's own LOW ranking.

---

## 1. The carved mount path (SINTRAN L, VERIFIED)

`@ENTER-DIRECTORY` -> `CHDSI` -> `RXDIR` (read page 0) -> on bad checksum,
`WXDIR` (rebuild + write page 0 back). Carved anchors in `006-S3FS`:

| Symbol | Octal | Carved evidence (`006-S3FS.annotated.dis`) |
|--------|-------|--------------------------------------------|
| `RXDIR` | 37643B | line 5030 `037643 RXDIR: JPC -115` - reads the page-0 extended-info block |
| `WXDIR` | 37702B | line 5061 `037702 WXDIR: SAT 30` then `037703 RDIV ST` - divide = **checksum** recompute, writes block back |
| `CHDSI` | 37763B | line 5110 `037763 CHDSI: STZ 2` - enter/validate directory |
| `REENB` | 40162B | line 5237 `040162 REENB: STZ 1` - clear "entered" flag on release |

**Page-0 block-address geometry is done inside CHDSI (VERIFIED, carved).** At
`040034-040044` CHDSI multiplies the directory page number by fixed geometry
factors to form the device block address:

```
040034  LDA ,B 20
040035  MPY 130        ; *STATX geometry multiply (130B = 88 dec)
040036  ADD 130
040037  STA ,X 3
...
040042  LDA ,B 20
040043  MPY 124        ; second geometry multiply (124B = 84 dec)
040044  ADD 124
```

This is the on-ND-side scaling of "filesystem page" -> "mass-storage block". It
is *fed* the device geometry that ultimately comes from SCSI **READ CAPACITY**
(block size + capacity). The page-0 read therefore requires: READ CAPACITY to
return a sane power-of-two block size and correct capacity, and the READ payload
to deliver the first **2048 bytes** (one ND page = 1024 words) at LBA 0,
big-endian words. Extended-info checksum sits at page byte **2000** (word 1750B),
master block at byte **2016** (word 1760B).

**VERIFIED critical behaviour (from `SINTRAN/Filesystem/NDFS-VALIDATION.md` +
carved CHDSI/WXDIR):** on a bad/zero checksum, `CHDSI` does **not** reject - it
**rebuilds** the extended-info block and **writes it back** via `WXDIR`
(`037703 RDIV ST` = checksum). So a garbled page-0 read does not surface as a
clean "checksum error"; it surfaces as a *device error at write-back time* or a
silent-but-wrong rebuild that fails again identically on the next enter. **That is
exactly the user's "always the same failure" symptom.**

### 1a. Correcting the earlier doc's SCSI-side citations (re-grounded)

`SINTRAN/Filesystem/DEBUG-scsi-enter-directory.md` cited **NPL** for the SCSI
error translator. Re-grounding against carved L:

- **VERIFIED (carved):** the SCSI error translator **`SCDTS` exists at 62107B**
  (`006-S3FS.annotated.dis` line 14410 `062107 SCDTS: STT ,B 16`) and the SCSI
  driver **`SCSID` at 62217B** (line 14482), with entry points **`SCSI1` 46530B**
  (line 8539) and **`SCSI2` 46661B** (line 8628). The routines are real and on
  the mount path.
- **OPEN (carved disassembly is data-before-code garbled here):** the fine-grained
  *internal-code -> user-code* table (232B device-error default, 141B transfer,
  252B block-size, 224B incompatible sizes, 33B no-LUN) is **not cleanly readable**
  from the current carved disassembly of the 62107B region (the linear sweep
  produces PLANC jump-table/data noise: `ROP NOOP`, `STZ I,B,X ...`). Until that
  region is hand-decoded, treat the exact code table as **INFERRED from NPL**
  (`IP-P2-SCSI-DISK.NPL` ~line 1010), with the caveat that **NPL is a different
  revision than L** so the numeric mapping must not be quoted as carved-proven.
  What **is** carved-proven: `SCDTS`/`SCSID` are the routines, and a device-level
  read failure of page 0 returns through them to the command interpreter.
- **243B is not on this path (VERIFIED, unchanged):** `243=:ERFL` is set only in
  `RP-P2-ACCRT` (RT accounting, at logout). Mounting cannot legitimately raise it.
  So the console string is either a **rendering/offset bug in the emulator's
  error-string table** masking the real numeric code, or an unrelated coincidental
  logout event. **Capture the numeric code, never the string.**

---

## 2. Root-cause hypothesis, RANKED

### #1 (most likely) - Lost NCR completion interrupt: flag cleared on the wrong IOX read

**VERIFIED (C#) + INFERRED (correlation).**

- **C# evidence:** `NDBusDiscControllerSCSI.cs:905` clears the pending NCR
  interrupt on **every RSTAU (status) read**:
  ```
  case Register.RSTAU:
      ...
      regs.InterruptFromNCR5386 = false;  // Clear flag   <-- line 905
  ```
  The NCR interrupt is *set* by the completion callback
  `Ncr5386_OnInterrupt` -> `regs.InterruptFromNCR5386 = true` (line 755) and is
  supposed to be **processed** by `StepGoState()` (line 1225: `if
  (regs.InterruptFromNCR5386){ active=false; readyForTransfer=true;
  SetInterruptBit(true);}`) and **acknowledged** only by reading **RITRG**
  (line 978-979: `case Register.RITRG: rval = ncr5386.Read(InterruptRegister);`).
  On real hardware RSTAU is a *status poll* and does **not** acknowledge the NCR
  interrupt - only RITRG does (TODO bug #2).
- **The race (INFERRED):** while a command is `active`, SINTRAN polls **RSTAU**.
  If the NCR raises its interrupt between two `Clock()` ticks and SINTRAN reads
  RSTAU (line 905 clears the flag) before `StepGoState()` runs, the completion is
  **lost**: `active` never clears, `SetInterruptBit` never fires, the level-11
  IRQ never reaches SINTRAN. SINTRAN waits, times out, retries, and eventually
  reports a device error - **deterministically the same every time**, and can
  loop into a **BUS RESET**.
- **Why boot works but mount fails (INFERRED):** the bootstrap/`MASB` path is
  **polled** (fixed low-LBA raw read, no ND-100 interrupt), so the lost-interrupt
  bug never bites it. `ENTER-DIRECTORY` runs under SINTRAN multiprogramming and is
  the **first interrupt-driven** SCSI transaction - exactly what the RSTAU-clear
  bug breaks. This is the cleanest single explanation for "boots but mount
  hangs/always the same."

### #2 - Wrong NCR Own-ID register (selection/arbitration)

**VERIFIED (C#).** `SetSCSIIdNumber` (line 792) and `Reset` (line 804) write the
Own ID to the **read-only Source-ID** register instead of the Own-ID register:
```
792:  ncr5386.Write((byte)SCSIRegisters.SourceID, (byte)regs.TW1);   // WRONG
804:  ncr5386.Write((byte)SCSIRegisters.SourceID, (byte)regs.TW1);   // WRONG
```
Compare the correct path used by IOX `WOIDN` (line 1184):
`ncr5386.Write(SCSIRegisters.IDRegister, ...)`. If the NCR Own ID is never set,
arbitration/(re)selection can fail so the target is never selected - the read
never even starts. Ranked below #1 because a simplified emulator may let selection
succeed anyway, and SINTRAN itself may re-write Own ID via `WOIDN`; but it is a
confirmed HIGH bug that must be eliminated before trusting any trace.

### #3 - READ CAPACITY / block-size vs image geometry mismatch

**VERIFIED (C#) that the seam exists; INFERRED whether it is active.**

- READ CAPACITY returns `m_hdinfo.sectorbytes` for **both** the reported block
  size and the actual read sizing (`SCSIHDD.cs:434-435` big-endian; `readBlock`
  uses the same `sectorbytes` at line 103) - so block size and payload are
  **internally consistent** (no 512-vs-512 vs 1024 self-contradiction).
- **BUT MODE SELECT is a no-op (VERIFIED):** `SCSIHDD.cs:383-385` only "accepts"
  the MODE SELECT parameter data and **never changes `sectorbytes`**. SINTRAN's
  driver issues MODE SELECT to force **1024-byte** blocks; the emulated disk
  silently keeps whatever the **preset** chose. `SCSIHDDMicropolis` presets:
  `MICORPOL_1375` = **512**, `MICORPOL_1375_ND` = **1024**, `MICORPOL_1355` =
  **512** (`SCSIHDDMicropolis.cs:132/140/149`).
- **Risk (INFERRED):** if the disk **image** was authored as 1024-byte logical
  blocks (ND native) but the configured preset reports **512**, then
  `DiskSizeInBlocks` and the derived page count are wrong, which feeds CHDSI's
  capacity compare/rebuild (section 1) - garbage master block -> rebuild ->
  device error. The *byte-linear* first-2048-bytes read still lands page 0
  regardless of block size, so this only bites via the **capacity/page-count**
  path, not the raw offset. Hence ranked below the interrupt bugs.

### #4 (lower) - `readyForTransfer`/interrupt on Clear-Device and Bus-Reset

**VERIFIED (C#).** Clear Device sets `regs.readyForTransfer = true` immediately
(line 1126) and Reset-SCSI-bus likewise (line 1135) with no modelled Z80 reboot
delay, and the bus-reset path does not itself assert the ND interrupt
(TODO bug #4). Mostly a timing nicety; can produce spurious "ready" but is not the
primary "always same error." Fix after #1-#3.

---

## 3. Ranked fix list - each MANUALLY TESTABLE

Apply **one at a time**, re-run `@ENTER-DIRECTORY ,,DISC-SCSI-1,0`, observe the
stated change. Enable `[SCSI-TRACE]` (section 4) so the observable is visible.

### FIX 1 (HIGH, do first) - Acknowledge the NCR interrupt on RITRG, not RSTAU

- **File/line:** `NDBusDiscControllerSCSI.cs:905` (remove) and `:978-979` (add).
- **Change:** delete `regs.InterruptFromNCR5386 = false;` from the `RSTAU` case
  (line 905). Move the clear into the `RITRG` case so it clears only when SINTRAN
  actually acknowledges the NCR:
  ```csharp
  case Register.RITRG: // Read Interrupt Register (acknowledge)
      rval = ncr5386.Read((byte)SCSIRegisters.InterruptRegister);
      regs.InterruptFromNCR5386 = false;   // <-- acknowledge HERE, not on RSTAU
      break;
  ```
- **Why (carved tie-in):** SINTRAN polls RSTAU while waiting; the completion IRQ
  that lets `CHDSI`/`RXDIR` (37763B/37643B) finish the page-0 read must survive
  those polls and clear only on the RITRG acknowledge. Losing it strands the read
  -> device error -> the "always the same" mount failure (section 2 #1).
- **Observable:** the mount **proceeds past the page-0 read** - `[SCSI-TRACE]`
  shows the level-11 interrupt asserted after the READ status, `active` returns to
  false, and the failure either disappears or the error **code changes** away from
  the timeout/device-error path. No more BUS RESET loop.

### FIX 2 (HIGH) - Program the NCR **Own ID** register, not Source ID

- **File/line:** `NDBusDiscControllerSCSI.cs:792` and `:804`.
- **Change:** both writes to `SCSIRegisters.SourceID` become
  `SCSIRegisters.IDRegister`:
  ```csharp
  ncr5386.Write((byte)SCSIRegisters.IDRegister, (byte)regs.TW1);
  ```
- **Why:** the ND-100 is the SCSI **initiator**; a correct Own ID is needed for
  arbitration/(re)selection. Writing the read-only Source-ID leaves Own ID unset.
- **Observable:** `[SCSI-TRACE]` shows **SELECTION succeeds** (target ID 0
  selected, BSY asserted by target) on the first mount attempt instead of a
  selection timeout / retry.

### FIX 3 (MEDIUM) - Confirm the disk preset/block-size matches the image

- **File/line:** disk instantiation (which `MicropolisType` is passed) +
  `SCSIHDDMicropolis.cs:130-150`; MODE SELECT no-op at `SCSIHDD.cs:383-385`.
- **Change:** select the preset whose `sectorbytes` matches how the mounted image
  was authored - for an ND-native 1024-byte-block image use
  **`MICORPOL_1375_ND` (1024)**. If images are 512-byte-block, keep a 512 preset
  but ensure the **capacity** (`DiskSizeInBlocks`) is correct. Optionally make
  MODE SELECT actually honour the block-size descriptor so a driver that forces
  1024 is respected.
- **Why (carved tie-in):** READ CAPACITY block size + capacity feed CHDSI's
  geometry multiply (`040035 MPY 130`, `040043 MPY 124`) and its capacity
  compare/rebuild. Wrong capacity => wrong page count => garbage master block =>
  `WXDIR` rebuild loop.
- **Observable:** `[SCSI-TRACE]` READ CAPACITY logs **block size = 1024** (or the
  intended size) and a **capacity consistent with the image size**; the page-0
  dump (byte 2016) shows a **printable directory name** and sane block pointers
  instead of garbage. Error code changes from 232B toward success.

### FIX 4 (LOW) - Don't fake instant ready on Clear-Device / Bus-Reset; assert IRQ on bus reset

- **File/line:** `NDBusDiscControllerSCSI.cs:1126`, `:1130-1136`.
- **Change:** gate the immediate `readyForTransfer = true`, and after
  `InitiateResetSCSIBus()` assert the ND interrupt when enabled
  (`if (regs.interruptEnabled) SetInterruptBit(true);`), matching the real board
  setting RSTAU[5]+IRQ on bus reset.
- **Observable:** after a Clear Device the controller reports busy briefly then
  ready (not instantaneously), and a deliberate bus reset raises a level-11 IRQ;
  removes spurious early-ready races. Apply only after 1-3.

---

## 4. Tracing plan - instrumentation to ADD (if static fixes don't resolve it)

Add a **`[SCSI-TRACE]`** log category that writes to disk, keyed to the exact
methods below in `NDBusDiscControllerSCSI.cs` (and the disk target). Log fields
are chosen so the user can see the whole transaction and diff good vs bad.

1. **Every IOX register read/write** - in `Read(int address)` (line ~832) and
   `Write(...)` (the `WCONT`/`W*` cases, line ~1027+): log
   `[SCSI-TRACE] IOX {R|W} reg={Register name}(off=0xNN) val=0xXXXX active={regs.active} intFlag={regs.InterruptFromNCR5386}`.
   This immediately reveals whether an **RSTAU read is clearing the interrupt**
   (watch `intFlag` flip 1->0 on an RSTAU read - the FIX 1 smoking gun).

2. **Every SCSI command (CDB)** - in the disk target command dispatch
   (`SCSIHDD.cs` command switch, and `SCSIConfigurableHDD.cs:192/200/207`): log
   `[SCSI-TRACE] CDB opcode=0xNN lba=0xXXXX blocks=N cdb=[..10 bytes..]`. Confirms
   READ(6) vs READ(10) and the exact LBA/count for the **page-0 read** (expect LBA
   0, 2048 bytes).

3. **READ CAPACITY response** - in `CommandReadCapacity()` (`SCSIHDD.cs:409`,
   `SCSIConfigurableHDD.cs:620`): log
   `[SCSI-TRACE] READ CAPACITY -> lastLBA={DiskSizeInBlocks-1} blockSize={sectorbytes}`.
   Verify **power-of-two block size** and a capacity consistent with the image
   (FIX 3).

4. **Page-0 data returned** - in `readBlock()` (`SCSIHDD.cs:101`) and the DMA
   writer `WriteNextByteDMA` (line 1302): when `lba==0`, dump the **first 32
   bytes** and the region around byte **2000/2016** (`0x07D0`/`0x07E0`):
   `[SCSI-TRACE] PAGE0 bytes[0..31]=... bytes[2000..2047]=...`. This is the master
   block + extended-info checksum SINTRAN actually receives.

5. **Interrupt assert/clear events with the triggering register** - in
   `Ncr5386_OnInterrupt` (line 750), `StepGoState` (line 1225), `SetInterruptBit`,
   and both the RSTAU (905) and RITRG (979) cases:
   `[SCSI-TRACE] INT set by NCR` / `INT processed in StepGoState -> level11` /
   `INT cleared by {RSTAU|RITRG} read`. The bug fingerprint is
   `INT set` immediately followed by `INT cleared by RSTAU read` with **no**
   `INT processed ... level11` in between.

6. **Completion / RFT signalling** - log `readyForTransfer` transitions and every
   `OUT`-equivalent completion (the `active=false; readyForTransfer=true` in
   `StepGoState`): `[SCSI-TRACE] RFT set active->false` so a missing completion is
   obvious.

### nd100x-side cross-check (SINTRAN side of the same transaction)

Using the nd100x DAP / breakpoint tooling, break at **`CHDSI` = 37763B** and
step into **`RXDIR` = 37643B** (absolute address = 26000B segment load + offset,
adjusted for where the resident FS segment is mapped in your run):

- Dump the **2048 bytes** RXDIR read for page 0; compare byte **0x07D0**
  (extended-info/checksum) and **0x07E0** (master block - name should be
  printable) against what `[SCSI-TRACE]` step 4 shows the controller **sent**. If
  they differ, it is a read/geometry/byte-order bug; if they match but the enter
  still fails, it is the capacity compare or the write-back.
- Watch whether execution reaches the **rebuild** branch (`WXDIR = 37702B`,
  `037703 RDIV ST`). Reaching `WXDIR` every attempt = the read never validated =
  geometry/byte-order bug.
- At the point the mount returns to the command interpreter, read the T/A error
  code and confirm it is **232B** (real device error) rather than the console's
  243B (rendering bug).

---

## 5. How to correlate - run this, capture that, good vs bad

1. **Baseline:** enable `[SCSI-TRACE]` to a file, run `@ENTER-DIRECTORY
   ,,DISC-SCSI-1,0`. Save the log.
   - **Bad (current):** you will see the READ(6/10) for LBA 0, an `INT set by NCR`,
     then `INT cleared by RSTAU read` with no `level11`, then repeated
     RSTAU polls / retries / a BUS RESET, then a device-error return.
2. **Apply FIX 1**, re-run.
   - **Good:** `INT set` -> `INT processed in StepGoState -> level11` ->
     `INT cleared by RITRG read`. The mount proceeds past the page-0 read.
3. **If selection never happens** (no target BSY in the trace before FIX 1 can
   even matter), apply **FIX 2**, re-run; expect `SELECTION ok, target 0`.
4. **If the read completes but the enter still fails**, look at `[SCSI-TRACE]`
   READ CAPACITY (step 3) and PAGE0 (step 4), and the nd100x dump at `RXDIR`:
   - **Good:** block size = 1024 (or intended), capacity matches image, byte 2016
     shows a printable directory name; nd100x page-0 dump == controller-sent
     bytes; `WXDIR` is **not** reached.
   - **Bad:** wrong block size/capacity, garbage at 2016, `WXDIR` reached every
     time -> apply **FIX 3**.
5. Confirm the **numeric** error code at the command-interpreter return (nd100x
   T/A register). If it is 232B/141B/252B/224B, also fix the emulator's
   error-string table (the 243B "accounting" string is a rendering artifact, not
   the real code).

---

## 6. Cross-references

- Red-herring analysis + carved CHDSI/RXDIR/WXDIR detail:
  [DEBUG-scsi-enter-directory.md](../../Filesystem/DEBUG-scsi-enter-directory.md)
- ND-3201 firmware analysis, IOX map, NCR 5386, error codes:
  [nd-scsi-3201.md](nd-scsi-3201.md)
- Prioritised controller bug list (source of bugs #1-#7): the RetroCore TODO
  `E:\Dev\Ronny\TODO\Norsk-Data\nd-scsi-3201-controller-fixes.md`
- Error-code table: `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md`
- Filesystem on-disk format: `SINTRAN/Filesystem/on-disk-format/extended-info-block.md`,
  `.../directory-label.md`; `SINTRAN/Filesystem/NDFS-VALIDATION.md`

---

*SINTRAN side grounded in carved SINTRAN L bytes (segment `006-S3FS`, load 26000B).
C# side grounded by reading the RetroCore source. Items marked INFERRED/OPEN are
not byte-proven; the SCDTS/SCSID internal error-code table needs a hand-decode of
the 62107B region before it can be quoted as carved-proven.*
