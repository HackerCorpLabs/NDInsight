# SINTRAN III SCSI Optical Disk Commands - Addendum

**Document Created:** 2025-10-13
**Source File:** IP-P2-SCSI-OPDI.NPL
**Purpose:** Document optical disk-specific SCSI commands and write recovery mechanism

---

## Overview

The optical disk driver (IP-P2-SCSI-OPDI.NPL) extends the standard SCSI disk driver with special handling for write-once and rewritable optical media (WORM and MO disks). The key difference is the **CCRWO (Compare-Compare-Rewrite-Optical)** recovery mechanism.

---

## Optical-Specific SCSI Commands

### 1. VERIFY(10) with BYTCHK - Used as COMPARE

**Command:** 0x2F with BYTCHK bit set
**Source:** Line 303-304

**Command Block (10-byte CDB):**
```
Byte 0: 0x2F                         % VERIFY(10) opcode
Byte 1: 0x02                         % BYTCHK bit (bit 1) set = compare mode
Bytes 2-5: Logical Block Address     % Start block (big-endian)
Bytes 6-7: Reserved
Bytes 8-9: Verification Length       % Number of blocks
```

**Purpose:**
- Compares data on optical media with data in host memory
- Used in CCRWO write recovery to detect blank or corrupted blocks

**Data Phase:**
- **OUT** (Host → Device)
- Device reads media and compares byte-by-byte with host data

**Expected Status:**
- **GOOD (0x00)**: Data matches perfectly
- **CHECK CONDITION**: Mismatch detected

**CHECK CONDITION Sense Keys:**
- **0x05 (ILLEGAL REQUEST)**: Block out of range
- **0x03 (MEDIUM ERROR)**: Media defect prevents comparison
- **0x08 (BLANK CHECK)**: Block contains blank/unwritten data ← **CRITICAL for optical**

**SINTRAN Processing (Lines 315-327):**
```npl
IF A=100010 THEN                     % BLANK CHECK (0x08 << 13 = 0100010 octal)
   % Block is blank - rewrite it
   % Switch from VERIFY to WRITE command
FI
```

**Decoding the Status Check:**
- `A=100010` means sense key = 8 (BLANK CHECK)
- Bit 15 is set (extended sense indicator)
- This is the optical disk's way of saying "this block was never written"

---

### 2. WRITE(10) - Extended Write Command

**Command:** 0x2A
**Source:** Line 317

**Command Block (10-byte CDB):**
```
Byte 0: 0x2A                         % WRITE(10) opcode
Byte 1: Flags
        Bit 3: FUA (Force Unit Access) - bypass cache
        Bit 4: DPO (Disable Page Out) - don't cache
Bytes 2-5: Logical Block Address     % Start block (big-endian)
Byte 6: Group Number (reserved)
Bytes 7-8: Transfer Length           % Number of blocks (big-endian)
Byte 9: Control
```

**Why WRITE(10) instead of WRITE(6)?**

WRITE(10) is used for optical disks because:
1. **Larger address space** - Optical disks can be > 2GB (21-bit limit of WRITE(6))
2. **Better error detection** - More status bits available
3. **FUA/DPO control** - Important for write-once media

**SINTRAN Usage (Lines 317-326):**
```npl
25000; *STATX 00; LDATX 20        % WRITE(10) command, load next word
A=:L; ABPA3; *STDTX 10; LDDTX 30  % Store failing address
AD SHZ -10; ABP32-L=:L:=:D-D      % Calculate new amount (partial write)
```

**Partial Write Support:**
If COMPARE finds a blank block in the middle of a write:
- Calculates remaining blocks after the blank block
- Adjusts memory address to skip already-written blocks
- Issues WRITE(10) for remaining data only

---

## CCRWO: Compare-Compare-Rewrite-Optical Recovery

### Purpose

The CCRWO routine (lines 299-329) implements write recovery for optical disks. It handles the case where a write operation fails due to media defects or partial writes.

### When CCRWO is Triggered

**Trigger Conditions (Lines 248-252):**
```npl
IF PFAIL=T OR SBRST=T OR LIRST=T THEN
   D:=X; 77/\X.SLINK.ABFUN; D=:X:=A
   IF 1=D OR 61=D THEN                    % Function 1 or 61 = WRITE
      X.OPSTA BONE 4SRWO=:X.OPSTA         % Set write recovery flag
   FI
FI
```

**Trigger Reasons:**
- **PFAIL**: Power failure detected during write
- **SBRST**: SCSI bus reset occurred
- **LIRST**: LUN reset occurred
- **Functions 1 or 61**: Only for WRITE operations

### CCRWO Algorithm Flow

**Step 1: Compare Written Data (Lines 301-314)**

```
┌─────────────────────────────────────────────┐
│ Build VERIFY(10) with BYTCHK command        │
│ - Opcode: 0x2F                              │
│ - BYTCHK bit set (0x02 in byte 1)          │
│ - Address: Original write start address     │
│ - Length: Original write length             │
│ - Data: Original write data buffer          │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│ Send VERIFY command to device               │
│ Device compares media with host data        │
└─────────────────────────────────────────────┘
```

**Step 2: Analyze VERIFY Result (Line 315)**

```
         ┌─────────────────────────┐
         │ VERIFY Status?          │
         └────────────┬────────────┘
                      │
        ┌─────────────┴──────────────┐
        │                            │
        ▼                            ▼
   ┌─────────┐                 ┌──────────┐
   │  GOOD   │                 │ CHECK    │
   │ Status  │                 │CONDITION │
   └────┬────┘                 └─────┬────┘
        │                            │
        ▼                            ▼
  All data      ┌──────────────────────────────┐
  verified      │ Check Sense Key              │
  correctly     └────────────┬─────────────────┘
                             │
                ┌────────────┴────────────┐
                │                         │
                ▼                         ▼
        ┌──────────────┐          ┌──────────────┐
        │ Sense Key 8  │          │  Other       │
        │ BLANK CHECK  │          │  Error       │
        └──────┬───────┘          └──────┬───────┘
               │                         │
               ▼                         ▼
        Rewrite blank           Report error
        blocks (Step 3)         and exit
```

**Step 3: Rewrite Blank Blocks (Lines 316-327)**

If BLANK CHECK detected:

```npl
% Get failing block address from sense data
A=:L; ABPA3; *STDTX 10; LDDTX 30      % Load address of first blank block

% Calculate how many blocks remain after blank block
ABP32-L=:L:=:D-D                       % Remaining blocks

% Adjust memory buffer pointer to skip good blocks
A:=0; D:=L; *EXR ST
AD SHZ -1; A:=:D+X.MEMA2=:X.MEMA2     % New memory address
A:=D+C+X.MEMA1=:X.MEMA1

% Issue WRITE(10) command for remaining blocks
25000; *STATX 00                       % WRITE(10) opcode
% ... (setup CDB with new address and length)
CALL SCSID                             % Execute write
```

**Why This Works:**

For optical media with defects:
1. Initial WRITE command partially succeeds, writes good blocks
2. Encounters bad/blank block, returns CHECK CONDITION
3. CCRWO runs VERIFY with BYTCHK to check what was written
4. Device compares media with buffer, finds first mismatch
5. Returns BLANK CHECK with exact address of failed block
6. SINTRAN rewrites only the failed and subsequent blocks
7. Good blocks that were already written are skipped

---

## Optical Disk Device Types

### INQUIRY Device Type Values

**Device Type 0x04: Write-Once (WORM)**

```c
// INQUIRY response byte 0
device_type = 0x04;
```

**Characteristics:**
- Can write to each block only once
- Cannot erase or rewrite blocks
- Used for archival storage
- Example: OSI LD 1200 SCSI

**SINTRAN Behavior (Lines 214-216):**
```npl
IF SUTYP SHZ -10><3 AND ><4 THEN
   T:=TYPER; GO FAR ERREX              % ILLEGAL DEVICE TYPE
FI
```

**Accepted Types:**
- Type 3: Processor device (rare)
- Type 4: Write-once device

**Device Type 0x07: Optical Memory (Rewritable MO)**

Although not explicitly shown in the code, SCSI-2 defines:
```c
device_type = 0x07;  // Optical memory (Magneto-Optical)
```

**Characteristics:**
- Can rewrite blocks multiple times
- Uses magnetic field + laser for writing
- More reliable than magnetic disks
- Example: Sony, Fujitsu MO drives

**SINTRAN may accept type 7 through type 4 handling.**

---

## Write Recovery Example Scenario

### Scenario: Write to Defective Optical Disk

**Initial Write Request:**
```
Function:  1 (WRITE)
Address:   Block 1000
Length:    100 blocks (1000-1099)
Data:      Buffer at memory address 0x10000
```

**What Happens:**

**Attempt 1: Normal WRITE**

```
1. SINTRAN issues WRITE(10) command
   - Start: Block 1000
   - Length: 100 blocks

2. Optical drive writes:
   - Blocks 1000-1019 ✓ (20 blocks succeed)
   - Block 1020 ✗ (media defect)
   - Write aborts with CHECK CONDITION

3. REQUEST SENSE returns:
   - Sense Key: 0x03 (MEDIUM ERROR)
   - Information: 0x000003EC (1020 in decimal)

4. SINTRAN detects write failure on Function 1
   - Sets OPSTA bit 4SRWO (write recovery needed)
```

**Attempt 2: CCRWO Recovery**

```
5. CCRWO routine activates (line 226)

6. Issues VERIFY(10) with BYTCHK:
   - Start: Block 1000
   - Length: 100 blocks
   - Data: Original buffer at 0x10000

7. Optical drive compares:
   - Blocks 1000-1019: Match ✓
   - Block 1020: BLANK (not written) ✗
   - Returns CHECK CONDITION

8. REQUEST SENSE returns:
   - Sense Key: 0x08 (BLANK CHECK)
   - Information: 0x000003EC (block 1020)
   - Valid bit set, Information field contains first blank block
```

**Attempt 3: Rewrite Remaining Blocks**

```
9. CCRWO calculates:
   - Blocks written successfully: 20 (1000-1019)
   - First failed block: 1020
   - Remaining blocks: 100 - 20 = 80 blocks
   - New memory address: 0x10000 + (20 × 2048) = 0x1A000

10. Issues new WRITE(10):
    - Start: Block 1020
    - Length: 80 blocks (1020-1099)
    - Data: Buffer at 0x1A000

11. Optical drive writes:
    - Blocks 1020-1099 ✓ (80 blocks succeed)
    - Returns GOOD status

12. Operation completes successfully
    - Total blocks written: 20 + 80 = 100 ✓
```

**Final Result:**
- All 100 blocks written despite media defect at block 1020
- SINTRAN automatically recovered without user intervention
- Blocks 1000-1019 not rewritten (already good)

---

## BLANK CHECK Sense Key (0x08)

### Special Meaning for Optical Disks

**Standard SCSI-2 Definition:**
```
Sense Key 0x08: BLANK CHECK
- Indicates attempt to read blank medium
- Used for sequential access devices (tape)
- Optical: Indicates unwritten or erased block
```

**SINTRAN Interpretation (Line 315):**
```npl
IF A=100010 THEN                     % Sense key 8, bit 15 set
   % This block is blank - needs to be written
```

**When BLANK CHECK Occurs:**

1. **Reading unwritten blocks on WORM**
   - Application reads block that was never written
   - Drive returns BLANK CHECK
   - Data returned is zeros or undefined

2. **VERIFY after partial write**
   - Write operation was interrupted
   - VERIFY with BYTCHK finds first unwritten block
   - Drive returns BLANK CHECK with block address

3. **Format verification**
   - Checking if disk is formatted
   - Unformatted areas return BLANK CHECK

**ASC/ASCQ Values:**
```
ASC 0x00, ASCQ 0x05: End of data detected
ASC 0x14, ASCQ 0x05: Record not found (blank)
```

### Handling in C# Emulator

```csharp
public class OpticalDiskEmulator
{
    // Track which blocks have been written
    private BitArray writtenBlocks;

    public ScsiStatus HandleVerifyCommand(byte[] cdb, byte[] dataBuffer)
    {
        uint startBlock = GetLBA(cdb);
        uint numBlocks = GetTransferLength(cdb);
        bool byteCheck = (cdb[1] & 0x02) != 0;

        if (byteCheck)
        {
            // Compare mode - check each block
            for (uint i = 0; i < numBlocks; i++)
            {
                uint blockNum = startBlock + i;

                // Check if block was ever written
                if (!writtenBlocks[blockNum])
                {
                    // Generate BLANK CHECK sense
                    SetSense(
                        senseKey: 0x08,        // BLANK CHECK
                        asc: 0x14,
                        ascq: 0x05,
                        information: blockNum  // Which block is blank
                    );
                    return ScsiStatus.CheckCondition;
                }

                // Compare block data with buffer
                if (!CompareBlockData(blockNum, dataBuffer, i * blockSize))
                {
                    // Data mismatch
                    SetSense(
                        senseKey: 0x03,        // MEDIUM ERROR
                        asc: 0x11,
                        ascq: 0x00,
                        information: blockNum
                    );
                    return ScsiStatus.CheckCondition;
                }
            }
        }

        return ScsiStatus.Good;
    }

    public ScsiStatus HandleWriteCommand(byte[] cdb, byte[] dataBuffer)
    {
        uint startBlock = GetLBA(cdb);
        uint numBlocks = GetTransferLength(cdb);

        for (uint i = 0; i < numBlocks; i++)
        {
            uint blockNum = startBlock + i;

            // For WORM media, check if already written
            if (isWORM && writtenBlocks[blockNum])
            {
                // Cannot overwrite on WORM
                SetSense(
                    senseKey: 0x05,        // ILLEGAL REQUEST
                    asc: 0x21,
                    ascq: 0x00,
                    information: blockNum
                );
                return ScsiStatus.CheckCondition;
            }

            // Write the block
            WriteBlock(blockNum, dataBuffer, i * blockSize);
            writtenBlocks[blockNum] = true;
        }

        return ScsiStatus.Good;
    }
}
```

---

## MODE SELECT for Optical Disks

### Optical-Specific Mode Parameters

**Source:** Lines 111-124

**Command Flow:**
```npl
MDSEL: IF SUTYP SHZ -10=3 THEN      % Device type 3 (processor)
          1                          % Mode page 1
       ELSE
          "0"                        % Mode page 0
       FI
       X=:MSXRG; T:=X.CMAD1; X:=X.CMAD2
       A SHZ 10; *STZTX 00; STATX 10     % Sense data header
```

**Mode Parameter Header:**
```
Byte 0-1: Reserved (0x0000)
Byte 2-3: Block descriptor length = 4
```

**Block Descriptor:**
```
Byte 0: Density code (for optical: media type)
        - 0x01: CD-ROM
        - 0x02: DVD-ROM
        - 0x10: Rewritable MO
        - 0x11: Write-once
Bytes 1-3: Reserved
```

**Why This Matters:**

Optical drives need MODE SELECT to:
1. Set write power level
2. Configure error correction mode
3. Set defect management on/off
4. Configure write verification

---

## Optical Disk Functions

### Supported Functions (Lines 10-28)

| Function | Octal | Description | Notes |
|:--------:|:-----:|:------------|:------|
| 0 | 00 | READ | Same as magnetic disk |
| 1 | 01 | WRITE | **With CCRWO recovery** |
| 2 | 02 | READ PARITY | Verify read |
| 3 | 03 | COMPARE | **Not implemented** (line 85) |
| 4 | 04 | SEEK | Pre-positioning |
| 34 | 42 | RESERVE DEVICE | Multi-host support |
| 35 | 43 | RELEASE DEVICE | Multi-host support |
| 37 | 45 | READ EXTENDED STATUS | Get sense data |
| 42 | 52 | READ FORMAT | Get disk geometry |
| 60 | 74 | READ (Double address) | Large disks > 4GB |
| 61 | 75 | WRITE (Double address) | **With CCRWO recovery** |
| 62 | 76 | READ PARITY (Double) | Large disk verify |
| 63 | 77 | COMPARE (Double) | **Not implemented** |
| 73 | 111 | TEST UNIT READY | Status check |
| 74 | 112 | USER SPECIFIED CDB | Custom commands |
| 75 | 113 | INQUIRY | Device identification |

### COMPARE Not Implemented

**Line 84-86:**
```npl
IF A/\17=5 AND 77/\X.ABFUN=3 OR=63 THEN
   0=:X.HSTAT                        % COMPARE NOT IMPLEMENTED
FI
```

**Reason:**
- Sense key 5 = ILLEGAL REQUEST
- Functions 3 and 63 are COMPARE functions
- SINTRAN clears error status (pretends success)
- COMPARE would use VERIFY with BYTCHK, but SINTRAN doesn't fully implement it for explicit COMPARE function calls
- CCRWO uses VERIFY internally, but user applications cannot call COMPARE directly

---

## Implementation Checklist for C# Optical Emulator

### Required Features

- [ ] **VERIFY(10) with BYTCHK support**
  - [ ] Compare media data with host buffer
  - [ ] Return GOOD if all blocks match
  - [ ] Return BLANK CHECK (0x08) for unwritten blocks
  - [ ] Set Information field to first mismatched block address

- [ ] **WRITE(10) support**
  - [ ] Accept 10-byte CDB format
  - [ ] Support large LBAs (32-bit addresses)
  - [ ] Track which blocks have been written

- [ ] **BLANK CHECK sense generation**
  - [ ] Sense Key = 0x08
  - [ ] Information field = block address
  - [ ] ASC/ASCQ appropriate values

- [ ] **WORM write protection**
  - [ ] For WORM media (type 4), prevent overwriting written blocks
  - [ ] Return ILLEGAL REQUEST if attempting to overwrite

- [ ] **MODE SELECT parsing**
  - [ ] Accept mode page 0 or 1
  - [ ] Parse block descriptor
  - [ ] Store density/media type

- [ ] **CCRWO compatibility**
  - [ ] Support VERIFY → BLANK CHECK → WRITE sequence
  - [ ] Return accurate block numbers in sense data
  - [ ] Handle partial writes correctly

### Testing Scenarios

#### Test 1: Normal Write

```
1. WRITE(10) blocks 1000-1099
2. All blocks succeed
3. Return GOOD status
4. writtenBlocks[1000-1099] all set
```

#### Test 2: Write with Defect

```
1. WRITE(10) blocks 1000-1099
2. Blocks 1000-1019 succeed
3. Block 1020 fails (simulated defect)
4. Return CHECK CONDITION, sense key 0x03, info=1020
5. writtenBlocks[1000-1019] set, [1020+] clear
```

#### Test 3: CCRWO Recovery

```
1. After Test 2 failure
2. Receive VERIFY(10) with BYTCHK, blocks 1000-1099
3. Compare blocks 1000-1019: Match ✓
4. Check block 1020: Not written (blank) ✗
5. Return CHECK CONDITION, sense key 0x08, info=1020
6. Receive WRITE(10), blocks 1020-1099
7. Write all blocks successfully
8. Return GOOD status
9. writtenBlocks[1000-1099] all set
```

#### Test 4: WORM Protection

```
1. WRITE(10) block 1000 (first time)
2. Return GOOD, writtenBlocks[1000] set
3. WRITE(10) block 1000 (second time)
4. Return CHECK CONDITION, sense key 0x05 (ILLEGAL REQUEST)
5. ASC = 0x21 (Logical block address out of range or write protected)
```

#### Test 5: INQUIRY

```
1. Receive INQUIRY command
2. Return device type 0x04 (Write-once)
3. Vendor: "OSI     " (space padded)
4. Product: "LD 1200 SCSI    "
5. Followed by READ CAPACITY
```

---

## Summary

### Key Differences: Optical vs Magnetic

| Aspect | Magnetic Disk | Optical Disk |
|--------|---------------|--------------|
| **Write Command** | WRITE(6) 0x0A | WRITE(10) 0x2A |
| **Addressing** | 21-bit (2MB limit) | 32-bit (2TB limit) |
| **Error Recovery** | Retry on error | CCRWO (Compare-Rewrite) |
| **Overwrite** | Unlimited | WORM: once only; MO: limited |
| **BLANK CHECK** | Rare | Critical for recovery |
| **VERIFY** | Optional | Required for CCRWO |
| **Device Type** | 0x00 | 0x04 (WORM), 0x07 (MO) |

### CCRWO Advantages

1. **Automatic Recovery** - No user intervention needed
2. **Selective Rewrite** - Only rewrites failed blocks
3. **Media Defect Tolerance** - Works around bad sectors
4. **Data Integrity** - Verifies all data written correctly

### C# Implementation Key Points

1. **Track written blocks** with BitArray or similar
2. **VERIFY with BYTCHK** must compare and return BLANK CHECK
3. **Information field** in sense data must contain exact block number
4. **Partial writes** supported - can write subset of requested blocks
5. **WORM enforcement** - prevent overwriting once-written blocks

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\SCSI-Optical-Commands-Addendum.md`

**Related Documents:**
- SCSI-Commands-Analysis.md - General SCSI commands
- IP-P2-SCSI-OPDI.md - Full optical driver documentation
- SCSI-C#-Implementation-Guide.md - C# emulator implementation guide
