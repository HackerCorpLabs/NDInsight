# ND-500 Emulator Discrepancy Audit - NDBusND500IF.cs vs the verified spec

**Target:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusND500IF.cs`
(1851 lines as of 2026-07-08) plus `SimulatedND500.cs`, `NDSharedMemory.cs`,
`IND500Cpu.cs` in the same repo.
**Spec:** [ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md)
("REF" below); evidence in
[ND500-EVIDENCE-AND-CONTRADICTIONS.md](ND500-EVIDENCE-AND-CONTRADICTIONS.md)
("dossier").

**Rule for this document:** NO code is changed by the documentation task that
produced it. This is the burn-down list for a separate RetroCore fix task. C# line
numbers reference the 2026-07-08 state of the file and will drift as fixes land -
anchor on member names.

**Severity:** CRITICAL = SINTRAN cannot work correctly against this behavior /
protocol is fabricated. HIGH = wrong values or decode visible to the driver.
MEDIUM = wrong in corner cases or against hardware (not exercised by SINTRAN's
normal path). LOW = documentation/cleanup.

## Recommended fix order

1. D01/D02 - delete the fabricated TAG message protocol; introduce the message
   engine (REF 5, 6, 7).
2. D03 - interrupt gating (CONTROL bit 0).
3. D04-D08 - four-mode IOX decode corrections (REF 3.2).
4. D09/D10 - DMA word stride + shared-memory endianness unification.
5. D11-D14 - stop/clear semantics (RETG5, MCLR5, TERM5, LSTA5 masking).
6. D15+ - cleanups.

Each fix should quote its D-number in the commit and flip the Status field here.

---

## CRITICAL

### D01 - The TAG-IN/TAG-OUT message protocol is fabricated

- **Where:** `OnND500WritesTag` (:1167-1200), `ProcessTagIn` (:1495-1552),
  `TagInHighLevelCodes`/`TagOutHighLevelCodes` (:719-755), `TagRegisterFormat`
  (:760-779); consumed by `SimulatedND500` via the `TagWritten` event.
- **Current:** ND-500 -> ND-100 signalling is modeled as TAG-IN codes
  8 = MonitorCallRequest, 9 = PageFaultRequest, 16 = OperationComplete (with a
  process number in bits 8-11), which set ND500Finished and raise the interrupt.
- **Correct:** No such codes exist. TAG-IN codes are 4-bit register strobes
  (8 = DIEN, 9 = DUEN; 16 unrepresentable) - REF 10.1, dossier C6. ND-500 -> ND-100
  signalling is: microcode writes the answer into the MESSAGE (N5STA status word,
  STOPR stop reason), sets STATUS "finished"/stop bits, and raises level 12; the
  driver walks the message queue (REF 7). Monitor calls arrive as messages with
  MICFU in {3MONCO,3TRACO,3START,3WMONCO} and STOPR in {MOCALL,5FMOCALL} (REF 7.4).
- **Fix:** remove the high-level TAG code path entirely; replace with the message
  engine (D02). Keep the TAG registers only as the hardware strobe channel
  (REF 10.1-10.2) if microcode-level tests/loader support is wanted.
- **Status:** OPEN

### D02 - No message engine; ExecuteND500Operation reports instant completion

- **Where:** `ExecuteND500Operation` (:1824-1848), TODO at :1826.
- **Current:** on CONTROL activate, extracts bits 8-14, does nothing with the
  attached CPU, immediately clears Busy, sets Finished and interrupts. The CPU is
  never stepped; the MAR-addressed message is never fetched.
- **Correct:** activate (CONTROL bit 2) locks the interface (STATUS bit 5) and
  starts the microcode; the operation = fetch the message at MAR from ND-100
  memory, execute MICFU, write answer status/fields back, THEN finish + interrupt
  (REF 5.4, 14 item 5). Completion must also clear the lock or respond to TERM5
  (D12) or SINTRAN's timeout logic will master-clear the interface (REF 14 item 7).
- **Fix:** implement the message lifecycle against the attached IND500Cpu (or the
  simulated one); message layout in REF 6.2; status-word semantics REF 6.3.
- **Status:** OPEN

### D03 - Interrupt gate reads the wrong enable bit

- **Where:** `CheckTriggerInterrupt` (:1206-1220).
- **Current:** raises level 12 iff STATUS bit0 (InterruptEnabled) AND STATUS bit3
  (ND500Finished). Nothing ever copies CONTROL bit0 into STATUS bit0, so enabling
  interrupts via the CONTROL register (the only thing SINTRAN does) never arms the
  gate.
- **Correct:** the software enable is CONTROL bit 0 "Enable interrupt from ND-500"
  (REF 4.1); STATUS bit 0 "Interrupt enabled" REFLECTS the enable state and is not
  settable via LSTA5 (TST26, REF 4.2). Gate on CONTROL bit 0; mirror it into STATUS
  bit 0 on read.
- **Fix:** gate on controlRegister bit0; derive statusRegister bit0 from it.
- **Status:** OPEN

---

## HIGH - four-mode IOX decode (REF 3.2 is the normative table)

### D04 - RMAR5/LMAR5 unguarded

- **Where:** `ReadMarX2` (:1564-1579), `LoadMarX2` (:1664-1678).
- **Current:** MAR reads/writes work in every mode.
- **Correct:** MAR access exists only while UNLOCKED (both test and not-test);
  locked modes expose no MAR (REF 3.2; TMP table 1 lists no RMAR/LMAR).
- **Status:** OPEN

### D05 - Read CONTROL guard wrong

- **Where:** `ReadControlRegister` (:1592-1595).
- **Current:** returns CONTROL when `!isLocked || isTestMode` - i.e. also in
  unlocked+not-test.
- **Correct:** RCON decodes ONLY in test mode (both unlocked+test and locked+test);
  it is absent from both not-test columns (REF 3.2).
- **Status:** OPEN

### D06 - Master Clear and Terminate gating wrong; unlocked master clear is a no-op

- **Where:** `Write` cases MasterClear (:1721-1737) and Terminate (:1739-1754);
  `Reset` (:1643).
- **Current:** Master Clear and Terminate only act when `isLocked && !isTestMode`;
  issued while unlocked+not-test they do nothing.
- **Correct:** MCLR (offset 6) and TERM (offset 7) are available in BOTH not-test
  columns - locked and unlocked (REF 3.2). MCLR is a strobe that restarts the
  microprogram at control-store address 0 (REF 3.2 notes, dossier 4.2); it should
  work regardless of lock state when not in test mode. In unlocked+test both
  offsets become "Load DATA register" (already partially modeled).
- **Status:** OPEN

### D07 - Offsets 14 and 16 modeled as "Read LOCKED" - they are lock COMMANDS

- **Where:** `Register.ReadLockedMaybe`/`Register.ReadLocked` and the shared read
  case (:1625-1629); enum docs :337, :352-355.
- **Current:** offsets 14 (074) and 16 (076) READ back `isLocked ? 1 : 0` in every
  mode; no write behavior.
- **Correct:** 074 = SLOC "Set locked" (write/command - SINTRAN's activate sequence
  ends with it, REF 5.3); 076 = UNLC "Release locked" (write/command - first step
  of 5MCST, REF 9.1). Both exist in both not-test columns; neither is documented as
  a read of the lock state (REF 3.2). The "ReadLockedMaybe/ReadLocked" model came
  from the retired old/ND-500-INTERFACE.md guesswork.
- **Fix:** implement SLOC5/UNLC5 as lock set/clear on WRITE (and IOX-read variants
  should not return lock state unless new evidence appears).
- **Status:** OPEN

### D08 - Offset 13 and offset 15 decode

- **Where:** `WriteDataX` case (:1769-1780), `WriteData` case (:1782-1790).
- **Current:** offset 13 writes DATAX only when locked+not-test (misses
  unlocked+not-test); offset 15 ("WriteData") writes DATA-low whenever locked
  (including locked+test, where TMP exposes nothing).
- **Correct:** offset 13 (WDAT/LLOW5) = Write DATAX in BOTH not-test columns; load
  lower limit in unlocked+test. Offset 15 (CLKD5) = "Clock DATA" only in
  locked+not-test (REF 3.2).
- **Status:** OPEN

### D09 - 32-bit ND-100 DMA uses a +2 WORD stride (skips a word)

- **Where:** `ReadND100Memory` (:1395-1405), `WriteND100Memory` (:1410-1415);
  `DMARead/DMAWrite` in `NDBusDeviceBase.cs` (:515-548) treat the argument as a
  word address and shift left by 1.
- **Current:** the second half of a 32-bit value is accessed at `addr+2` (word
  address), i.e. 4 bytes above the first half, leaving the word at addr+1
  untouched.
- **Correct:** messages are contiguous 16-bit words in ND-100 memory (REF 6.2);
  consecutive words are at addr and addr+1. The +2 stride corrupts every multi-word
  transfer.
- **Status:** OPEN

### D10 - Two shared-memory models with conflicting 32-bit endianness

- **Where:** `NDSharedMemory.ReadWord32/WriteWord32` (NDSharedMemory.cs:110-131,
  little-endian, commented "little-endian for ND-500") vs the MPM Port B path
  `ReadMpmWord32/WriteMpmWord32` and `SimulatedND500` local memory
  (SimulatedND500.cs:257-529, big-endian, commented "ND-500 is BIG-ENDIAN").
- **Current:** the same CPU model uses both conventions on different memories.
- **Correct:** ND-500 is big-endian (project memory + REF sources); one shared
  memory model. Note also the emulator-invented constants: MPM at 0x00420000 with
  8MB and the bit-31 "MPM address" convention are EMULATOR conventions, not
  hardware (the hardware uses per-port BASE/window translation, REF 8.4) - keep
  them if convenient but document them as emulator-internal.
- **Status:** OPEN

---

## MEDIUM

### D11 - RETG5 semantics incomplete

- **Where:** `ReturnGate` case (:1791-1815).
- **Current:** bit1 sets ND500MicroClockStopped, clears Busy, and UNLOCKS the
  interface.
- **Correct:** RETG5 bit1 = stop bit (micro stop -> STATUS bit 9 5CLOST), bit0 =
  reverse tag bus (REF 3.2 notes, dossier 4.5). No evidence that RETG5 clears the
  lock - SINTRAN's 5MCST issues UNLC5 separately BEFORE RETG5 (REF 9.1). Remove the
  unlock side effect; model bit0 if TAG-level fidelity is wanted.
- **Status:** OPEN

### D12 - TERM5 completion semantics

- **Where:** Terminate case (:1739-1754).
- **Current:** immediately unlocks, clears Busy, sets Finished, interrupts.
- **Correct:** TERM5 REQUESTS a stop; the microcode acknowledges by releasing the
  lock, and SINTRAN polls RSTA5 until 5ILOCK clears (bounded spin), falling back to
  5MCST on timeout (REF 9.2). Instant-unlock is acceptable only as a simplification
  if the attached CPU model is synchronous - but Finished/interrupt on TERM5 is not
  evidenced; the driver expects the LOCK BIT to clear, not an interrupt.
- **Status:** OPEN

### D13 - LSTA5 must not set bits 0, 5, 9, 15

- **Where:** `LoadStatusRegister` case (:1680-1686).
- **Current:** `statusRegister = value` wholesale (guard `!isLocked && isTestMode`
  is correct).
- **Correct:** bits 0 (int enabled), 5 (locked), 9 (clock stopped) and 15 (CONTROL
  bit 15 mirror) are not settable via LSTA5 (TST26; REF 4.2). Mask them.
- **Status:** OPEN

### D14 - CONTROL bit 4 side effect missing

- **Where:** `LoadControlRegister` (:1688-1719) - `ND500ProgrammedClear` triggers
  only `nd500Cpu.Reset()`.
- **Current:** no effect on STATUS.
- **Correct:** programmed clear (bit 4 = 1) also CLEARS the DMA-error status bit 6
  (TST25; REF 4.1).
- **Status:** OPEN

### D15 - ProcessTagOut is a real hardware feature attached to the wrong port

- **Where:** `ProcessTagOut` (:1434-1488) - invoked when the ND-100 writes offset
  071 (WriteTagOut).
- **Current:** the ND-100's TAG-OUT write executes the TAG-OUT command table
  (read/write MAR/STATUS/CONTROL/DATA vs ND-100 memory) on the spot.
- **Correct:** offset 071 writes the TAG-IN register on the 5015 (strobe codes,
  REF 10.1). The 8-code table the method implements is the TAG-OUT register that
  the ND-500 MICROCODE drives toward the 3022 (REF 10.2) - i.e. it belongs on the
  CPU/microcode side, not on the IOX write path. (The code values 0-7 themselves
  match TMP section 3.13 and can be reused on the correct side.)
- **Status:** OPEN

---

## LOW / cleanups

### D16 - Enum XML octal typo

`Register.ReadLocked` doc says "IOX +16 (070)"; offset 14 decimal is octal 076
(:352-355). Overtaken by D07 anyway.

### D17 - Dead field

`ioxRegister` (:850) declared, never used, commented `//32 bit IOX register ??`.

### D18 - Direction wording

`ProcessTagOut` comment says "Process TAG-OUT command from ND-500" while the
method runs on ND-100 writes (:1431); contradicts the file's own TAG naming
diagram. Overtaken by D15.

### D19 - Header decode table incomplete

The four-column table in the file header (:60-79) omits RETG5 at 077 and contains
the "Read LOCKED??" guesses; replace it with the REF 3.2 table (cite TMP section
3.14).

### D20 - IDENT does not clear a device interrupt-enable

`NDBusND500IF` does not override `IDENT` (base note at NDBusDeviceBase.cs:217-219).
Verify against the ND-100 interrupt model used by other devices in the repo; align
with however the 3022 should behave after ident (no direct evidence either way -
UNVERIFIED, flag for hardware-doc follow-up).

---

## What the current implementation gets RIGHT (keep)

- **IOX bases and ident codes per thumbwheel** (:876-901) match ND-06.015.02
  exactly: 60/1060/660/760/560 with idents 16/116/36/114/76 (REF 3.1).
- **Interrupt level 12** (:905) matches the SINTRAN driver (REF 7.1).
- **ControlWordBits enum** (:375-426) matches TMP section 3.1 bit-for-bit
  (bit0 enable, bit2 activate, bit3 test, bit4 programmed clear, bit5 disable
  TAG-IN decode, bit6 DMA error, bit7 chaining, bits8-14 operation).
- **StatusRegisterBits enum** (:429-493) matches TMP section 3.2 including bits
  10-14 stop reason and bit15 CONTROL-15 mirror.
- **LoadStatusRegister mode guard** (`!isLocked && isTestMode`) matches the
  four-mode table (needs only the D13 bit mask).
- **MAR two-step access flip-flops** (:1564-1579, :1664-1678) - the half-order
  matches TMP section 3.3 (load MS first, read LS first); only the mode gating
  (D04) is wrong.
- **Big-endian MPM word access on Port A/B helpers** - consistent with ND-500
  byte order (keep as the survivor of D10).

---

**Version history**

| Date | Change |
|---|---|
| 2026-07-08 | Initial audit from the Phase 1-3 exploration + evidence dossier |
