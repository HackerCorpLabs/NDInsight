# SINTRAN III SCSI Commands Analysis

**Document Created:** 2025-10-13
**Source Files:** IP-P2-SCSI-DISK.NPL, IP-P2-SCSI-DRIV.NPL
**Purpose:** Document all SCSI commands used, their response requirements, and custom command support

---

## Table of Contents

1. [SCSI Commands Used by SINTRAN](#scsi-commands-used-by-sintran)
2. [Command Details and Response Requirements](#command-details-and-response-requirements)
3. [Custom SCSI Command Support](#custom-scsi-command-support)
4. [INQUIRY Command Support](#inquiry-command-support)
5. [Implementation Requirements](#implementation-requirements)

---

## SCSI Commands Used by SINTRAN

### Overview

SINTRAN III uses two command mapping arrays (`SCSF1` and `SCSF2`) that map SINTRAN function codes (0-77 octal) to SCSI opcodes. The choice of array depends on the SCSI device type:

- **SCSF1**: Used for Direct Access Devices (Type 0, 4, 5)
- **SCSF2**: Used for Sequential Access Devices (Type 1, 2, 3 - Tape devices)

### Command Mapping Tables

#### SCSF1 - Direct Access Devices (Disks)

| Function | Octal | SCSI Opcode | Command Name | Notes |
|:--------:|:-----:|:-----------:|:-----------|:------|
| 0 | 00 | 0x08 | READ(6) | Read sectors |
| 1 | 01 | 0x0A | WRITE(6) | Write sectors |
| 2 | 02 | 0x2F | VERIFY(6) | Verify data |
| 3 | 03 | 0x2F | VERIFY(6) | Verify with compare |
| 4 | 04 | 0x0B | SEEK(6) | Seek to address |
| 16 | 20 | 0x00 | TEST UNIT READY | Check device status |
| 19 | 23 | 0x15 | MODE SELECT(6) | Set device parameters |
| 21 | 25 | 0x1A | MODE SENSE(6) | Query device parameters |
| 28 | 34 | 0x16 | RESERVE DEVICE | Reserve for exclusive use |
| 29 | 35 | 0x17 | RELEASE DEVICE | Release reservation |
| 33 | 41 | 0x04 | FORMAT UNIT | Format disk |
| 34 | 42 | Special | READ CAPACITY(10) | Get disk size (auto-inquiry) |
| 36 | 44 | 0x08 | READ(6) | Read control area |
| 37 | 45 | 0x0A | WRITE(6) | Write control area |
| 48 | 60 | 0x08 | READ(6) | Read (double address) |
| 49 | 61 | 0x0A | WRITE(6) | Write (double address) |
| 50 | 62 | 0x2F | VERIFY(6) | Verify (double address) |
| 51 | 63 | 0x2F | VERIFY(6) | Verify compare (double addr) |
| 54 | 66 | 0x08 | READ(6) | Read with special handling |
| 57 | 71 | 0x2E | WRITE AND VERIFY | Write with verify |
| 59 | 73 | 0x00 | TEST UNIT READY | Device ready check |
| 60 | 74 | User | USER COMMAND | Execute user-specified CDB |
| 61 | 75 | 0x12 | INQUIRY | Device identification |

#### SCSF2 - Sequential Access Devices (Tapes/Streamers)

| Function | Octal | SCSI Opcode | Command Name | Notes |
|:--------:|:-----:|:-----------:|:-----------|:------|
| 0 | 00 | 0x08 | READ(6) | Read tape blocks |
| 1 | 01 | 0x0A | WRITE(6) | Write tape blocks |
| 2 | 02 | 0x13 | VERIFY(6) | Verify tape data |
| 3 | 03 | 0x13 | VERIFY(6) | Verify with compare |
| 7 | 07 | 0x19 | ERASE | Erase tape |
| 8 | 10 | 0x11 | SPACE BLOCKS | Space forward/backward |
| 9 | 11 | 0x11 | SPACE FILEMARKS | Space over filemarks |
| 10 | 12 | 0x10 | WRITE FILEMARKS | Write end-of-file marks |
| 11 | 13 | 0x01 | REWIND | Rewind to beginning |
| 12 | 14 | 0x19 | ERASE | Erase (gap) |
| 13 | 15 | 0x11 | SPACE BLOCKS | Space reverse |
| 14 | 16 | 0x11 | SPACE BLOCKS | Space forward |
| 15 | 17 | 0x1B | START/STOP UNIT | Unload tape |
| 16 | 20 | 0x00 | TEST UNIT READY | Device status |
| 17 | 21 | 0x01 | REWIND | Rewind |
| 18 | 22 | 0x01 | REWIND | Rewind |
| 19 | 23 | 0x15 | MODE SELECT(6) | Set tape parameters |
| 21 | 25 | 0x1A | MODE SENSE(6) | Query tape parameters |
| 22 | 26 | 0x08 | READ(6) | Read variable blocks |
| 23 | 27 | 0x0A | WRITE(6) | Write variable blocks |
| 24 | 30 | 0x1B | START/STOP UNIT | Load tape |
| 28 | 34 | 0x16 | RESERVE DEVICE | Reserve device |
| 29 | 35 | 0x17 | RELEASE DEVICE | Release device |
| 34 | 42 | 0x05 | READ BLOCK LIMITS | Query block size limits |
| 48 | 60 | 0x08 | READ(6) | Read (special mode) |
| 49 | 61 | 0x0A | WRITE(6) | Write (special mode) |
| 50 | 62 | 0x13 | VERIFY(6) | Verify (special mode) |
| 51 | 63 | 0x13 | VERIFY(6) | Verify compare |
| 54 | 66 | 0x08 | READ(6) | Read with handling |
| 56 | 70 | 0x1B | START/STOP UNIT | Retension tape |
| 59 | 73 | 0x00 | TEST UNIT READY | Device ready |
| 60 | 74 | User | USER COMMAND | Execute user-specified CDB |
| 61 | 75 | 0x12 | INQUIRY | Device identification |
| 62 | 76 | 0x11 | SPACE BLOCKS | Space to end of data |

### Automatic Commands

These commands are issued automatically by the driver:

| SCSI Opcode | Command | When Issued | Source Line |
|:-----------:|:--------|:-----------|:------------|
| 0x03 | REQUEST SENSE | After CHECK CONDITION status | 1359-1364 |
| 0x12 | INQUIRY | Device initialization | 1223-1227 |
| 0x25 | READ CAPACITY(10) | After INQUIRY for disks | 1234 |
| 0x05 | READ BLOCK LIMITS | After INQUIRY for tapes | 1237 |
| 0x1A | MODE SENSE(6) | Tape initialization | Line 544 |
| 0x15 | MODE SELECT(6) | After MODE SENSE for tapes | Line 558 |

---

## Command Details and Response Requirements

### 1. TEST UNIT READY (0x00)

**Function Codes:** 16 (20 octal), 59 (73 octal)

**Command Block:**
```
Byte 0: 0x00 | (LUN << 5)
Byte 1-4: 0x00
Byte 5: Control byte
```

**Expected Response:**
- **GOOD status (0x00)**: Device is ready
- **CHECK CONDITION**: Device not ready, REQUEST SENSE will provide details

**SINTRAN Behavior:**
- No data phase
- Used to check if device is operational
- Retries with wait on BUSY status

---

### 2. REQUEST SENSE (0x03)

**Automatically issued after CHECK CONDITION status**

**Command Block:**
```
Byte 0: 0x03 | (LUN << 5)
Byte 1-2: 0x0000
Byte 3: Reserved
Byte 4: Allocation length (sense buffer size)
Byte 5: Control byte
```

**Expected Response:**
Must return extended sense data (minimum 8 bytes):

```
Byte 0: Error code (0x70 = current error, 0x71 = deferred error)
Byte 1: Reserved
Byte 2: Sense Key (bits 0-3)
        - 0x00: NO SENSE
        - 0x01: RECOVERED ERROR
        - 0x02: NOT READY
        - 0x03: MEDIUM ERROR
        - 0x04: HARDWARE ERROR
        - 0x05: ILLEGAL REQUEST
        - 0x06: UNIT ATTENTION
        - 0x0B: ABORTED COMMAND
        - 0x0C: COPY ABORTED (for COPY command)
Byte 3-6: Information bytes
Byte 7: Additional sense length
Byte 8+: Additional sense data
```

**Critical Bits (Line 1200-1209):**
```
Bit 15: Must be 1 for extended sense
Bits 5-7: ILI (Illegal Length), EOM (End of Media), EOF flags
Bits 0-3: Sense key
```

**SINTRAN Special Handling:**
- **Sense Key = 6 (UNIT ATTENTION)**: Forces re-initialization (INQUIRY) at line 1210-1212
- **Sense Key = 1 or 3**: Logged as corrected/medium error
- **Sense Key = 12**: Interpreted as COPY ABORTED
- **Sense Key = 13**: ABORTED COMMAND - triggers retry

**Source:** Lines 1186-1214

---

### 3. INQUIRY (0x12)

**Function Code:** 61 (75 octal)
**Also automatically issued during initialization**

**Command Block (Line 1224-1227):**
```
Byte 0: 0x12 | (LUN << 5)
Byte 1: 0x00
Byte 2: 0x00 (Page code)
Byte 3: Reserved
Byte 4: Allocation length (usually 36+ bytes)
Byte 5: Control
```

**Expected Response (Standard INQUIRY Data):**

```
Byte 0: Peripheral Device Type
        - 0x00: Direct Access (disk)
        - 0x01: Sequential Access (tape)
        - 0x02: Printer
        - 0x03: Processor
        - 0x04: Write-once
        - 0x05: CD-ROM
        - 0x7F: Logical unit not present
Byte 1: RMB (bit 7 = removable)
Byte 2: Version
Byte 3: Response Data Format (must be 2)
Byte 4: Additional length (n-4)
Byte 5-6: Reserved
Byte 7: Flags
Byte 8-15: Vendor ID (8 bytes ASCII)
Byte 16-31: Product ID (16 bytes ASCII)
Byte 32-35: Product Revision (4 bytes ASCII)
```

**SINTRAN Processing (Lines 1230-1265):**

1. **Device Type Check (Line 1232-1242):**
   - Type 0, 3, 4 → Direct Access, issues READ CAPACITY(10) next
   - Type 1 → Sequential Access, issues READ BLOCK LIMITS next
   - Type 0x7F → Returns "NO SUCH LUN" error

2. **For Disks - READ CAPACITY(10) follows (Line 1234):**

**READ CAPACITY Command:**
```
Byte 0: 0x25
Byte 1-8: Reserved
Byte 9: Control
```

**READ CAPACITY Response:**
```
Bytes 0-3: Last Logical Block Address (big-endian)
Bytes 4-7: Block Length in bytes (big-endian)
```

**Stored at:** Line 1252-1254 → SURSZ (device record size)

3. **For Tapes - READ BLOCK LIMITS follows (Line 1237):**

**READ BLOCK LIMITS Command:**
```
Byte 0: 0x05
Byte 1-4: Reserved
Byte 5: Control
```

**READ BLOCK LIMITS Response:**
```
Byte 0: Reserved
Bytes 1-3: Maximum block length
Bytes 4-5: Minimum block length
```

**Processing (Lines 1257-1264):**
- Fixed block mode if min = max
- Variable block mode if different
- Block size stored in SURSZ

**Required Flags Set:**
- Bit 5SCIN: Initialization complete
- Bit 5SCDA: Direct access device (if applicable)
- Bit 5SFBM: Fixed block mode (tapes)
- Bit 5SVBS: Variable block size (tapes)

**Source:** Lines 1216-1314

---

### 4. READ(6) (0x08)

**Function Codes:** 0, 36, 48, 54, 60, 66

**Command Block (Lines 1533-1539):**

**6-byte format:**
```
Byte 0: 0x08 | (LUN << 5) | (MSB 3 bits of address)
Byte 1: Middle byte of address
Byte 2: LSB of address
Byte 3: Transfer length (0 = 256 blocks)
Byte 4: Control
```

**10-byte format (for large addresses):**
```
Byte 0: 0x28 (READ(10))
Byte 1: Flags
Bytes 2-5: Logical Block Address (big-endian)
Bytes 6-7: Reserved
Bytes 7-8: Transfer Length
Byte 9: Control
```

**SINTRAN uses 10-byte format when:**
- Address > 0x1FFFFF (21 bits) - Line 1531

**Data Phase:**
- IN (Device → Host)
- Transfer size = Block size × Transfer length
- Block size obtained from INQUIRY/READ CAPACITY

**Expected Status:**
- **GOOD (0x00)**: Data transferred successfully
- **CHECK CONDITION**: Error occurred, REQUEST SENSE provides details

**Error Handling:**
- Retries on UNIT ATTENTION (sense key 6)
- Retries on ABORTED COMMAND (sense key 13)
- Fails on MEDIUM ERROR (sense key 3)

**Source:** Lines 1476-1545, CACOB routine

---

### 5. WRITE(6) (0x0A)

**Function Codes:** 1, 37, 49, 55, 61

**Command Block:** Same format as READ(6)

**Data Phase:**
- OUT (Host → Device)
- Transfer size = Block size × Transfer length

**Expected Status:** Same as READ

**Optical Disk Special Handling:**
- Function 71 (WRITE AND VERIFY) uses opcode 0x2E
- See IP-P2-SCSI-OPDI.NPL for write recovery mechanism

**Source:** Same as READ

---

### 6. MODE SENSE(6) (0x1A)

**Function Code:** 21 (25 octal)
**Also automatic for tape initialization**

**Command Block (Line 544):**
```
Byte 0: 0x1A | (LUN << 5)
Byte 1: DBD flag (Disable Block Descriptors)
Byte 2: Page Control (bits 6-7) | Page Code (bits 0-5)
Byte 3: Reserved
Byte 4: Allocation Length
Byte 5: Control
```

**Expected Response:**

**Mode Parameter Header:**
```
Byte 0: Mode Data Length
Byte 1: Medium Type
Byte 2: Device-Specific Parameter
        - Bit 7: Write Protect (1 = protected)
Byte 3: Block Descriptor Length
```

**Block Descriptor (if present):**
```
Bytes 0-3: Number of blocks (or density code in byte 0)
Byte 4: Reserved
Bytes 5-7: Block length
```

**SINTRAN Processing (Lines 992-1004):**
- Extracts write-protect flag from byte 2, bit 7
- Extracts density code from byte 4 of block descriptor
- Stores in CFORM field

**Used For:**
- Determining tape density
- Checking write protection
- Querying block size

**Source:** Lines 542-564, 989-1004

---

### 7. MODE SELECT(6) (0x15)

**Function Code:** 19 (23 octal)
**Also automatic after MODE SENSE for tapes**

**Command Block (Line 558):**
```
Byte 0: 0x15 | (LUN << 5)
Byte 1: PF (Page Format, bit 4) | SP (Save Pages, bit 0)
Byte 2: Reserved
Byte 3: Reserved
Byte 4: Parameter List Length
Byte 5: Control
```

**Data Phase OUT - Mode Parameter List:**

**Header:**
```
Byte 0: Reserved
Byte 1: Reserved
Byte 2: Reserved
Byte 3: Block Descriptor Length
```

**Block Descriptor (Lines 553-557):**
```
Byte 0: Density Code
Bytes 1-3: Reserved
Byte 4: Reserved
Bytes 5-7: Block Length (0 for variable blocks)
```

**SINTRAN Sets:**
- Density code from previous MODE SENSE or user parameter
- Block size: 1024 (0x400) for fixed block mode, 0 for variable

**Used For:**
- Setting tape density
- Setting block size mode (fixed vs variable)

**Source:** Lines 546-577, MODE SELECT logic

---

### 8. VERIFY(6) (0x2F)

**Function Codes:** 2, 3, 50, 51, 62, 63

**Command Block:**
```
Byte 0: 0x2F | (LUN << 5) | (Address MSB)
Byte 1: Address middle
Byte 2: Address LSB
Byte 3: Verification Length
Byte 4: Control
        - Bit 1: BYTCHK (Byte Check) - compare with host data
```

**Two Modes:**

1. **BYTCHK = 0:** Verify media only (no data transfer)
2. **BYTCHK = 1:** Compare with host data (Data OUT phase)

**Expected Status:**
- **GOOD**: Verification passed
- **CHECK CONDITION**: Verification failed or media error

**SINTRAN Note:**
Line 198: "COMPARE NOT IMPLEMENTED" - SINTRAN may not fully support BYTCHK=1 mode

**Source:** SCSF1/SCSF2 arrays, lines 1452-1469

---

### 9. WRITE AND VERIFY (0x2E)

**Function Code:** 57 (71 octal)

**Command Block:**
```
Byte 0: 0x2E | (LUN << 5)
Byte 1: Flags
Bytes 2-5: Logical Block Address
Bytes 6-7: Reserved
Bytes 7-8: Transfer Length
Byte 9: Control
```

**Behavior:**
- Writes data to medium
- Immediately verifies written data
- Single operation from host perspective

**Used By:**
- Optical disk driver for critical writes
- See CCRWO (Compare-Compare-Rewrite-Optical) in IP-P2-SCSI-OPDI.NPL

**Source:** SCSF1 array index 69

---

### 10. SEEK(6) (0x0B)

**Function Code:** 4

**Command Block:**
```
Byte 0: 0x0B | (LUN << 5) | (Address MSB)
Byte 1: Address middle
Byte 2: Address LSB
Byte 3: Reserved
Byte 4: Control
```

**Purpose:**
- Pre-position disk head to reduce latency on next READ/WRITE
- Used by elevator algorithm (DSORT) for optimization

**Expected Status:**
- **GOOD**: Seek complete
- **CHECK CONDITION**: Seek error

**Source:** SCSF1 array index 4

---

### 11. FORMAT UNIT (0x04)

**Function Code:** 33 (41 octal)

**Command Block:**
```
Byte 0: 0x04 | (LUN << 5)
Byte 1: Format Options
Byte 2: Vendor Specific
Byte 3: Interleave (MSB)
Byte 4: Interleave (LSB)
Byte 5: Control
```

**Optional Data Phase:**
- OUT phase if defect list provided
- No data phase for simple format

**Expected Status:**
- **GOOD**: Format complete
- **CHECK CONDITION**: Format failed

**SINTRAN Note:**
- Used for disk formatting
- May take significant time (several minutes to hours)

**Source:** SCSF1 array index 34

---

### 12. RESERVE/RELEASE DEVICE (0x16/0x17)

**Function Codes:** 28 (34 octal), 29 (35 octal)

**RESERVE Command:**
```
Byte 0: 0x16 | (LUN << 5)
Byte 1-4: Reserved
Byte 5: Control
```

**RELEASE Command:**
```
Byte 0: 0x17 | (LUN << 5)
Byte 1-4: Reserved
Byte 5: Control
```

**Purpose:**
- RESERVE: Lock device for exclusive use by this initiator
- RELEASE: Unlock device

**Expected Status:**
- **GOOD**: Reservation accepted/released
- **RESERVATION CONFLICT (0x18)**: Device already reserved by another initiator

**SINTRAN Handling (Lines 649-651, 1144-1146):**
- Tracks reservation state in OPSTA bit 4SRES
- Returns error code 3210 (RESERVATION CONFLICT) to application

**Source:** SCSF1/SCSF2 arrays index 28-29

---

### 13. Tape-Specific Commands

#### REWIND (0x01)

**Function Codes:** 11 (13 octal), 17, 21

**Command Block:**
```
Byte 0: 0x01 | (LUN << 5)
Byte 1: Immediate bit (bit 0)
Byte 2-4: Reserved
Byte 5: Control
```

**Immediate Mode:**
- Bit 0 = 1: Command returns immediately, rewind continues in background
- Bit 0 = 0: Command waits for rewind to complete

#### WRITE FILEMARKS (0x10)

**Function Code:** 10 (12 octal)

**Command Block:**
```
Byte 0: 0x10 | (LUN << 5)
Byte 1: Reserved
Bytes 2-4: Number of filemarks (3 bytes, big-endian)
Byte 5: Control
```

#### SPACE (0x11)

**Function Codes:** 8-10, 13-14, 62

**Command Block:**
```
Byte 0: 0x11 | (LUN << 5)
Byte 1: Code (bits 0-1)
        - 0x00: Space blocks
        - 0x01: Space filemarks
        - 0x03: Space to end of data
Bytes 2-4: Count (signed 3-byte value, can be negative for reverse)
Byte 5: Control
```

#### ERASE (0x19)

**Function Code:** 7

**Command Block:**
```
Byte 0: 0x19 | (LUN << 5)
Byte 1: Long bit (bit 0)
Byte 2-4: Reserved
Byte 5: Control
```

#### START/STOP UNIT (0x1B)

**Function Codes:** 15 (17 octal - unload), 24 (30 octal - load), 56 (70 octal - retension)

**Command Block:**
```
Byte 0: 0x1B | (LUN << 5)
Byte 1: Immediate bit (bit 0)
Byte 2-3: Reserved
Byte 4: Power/LoEj/Start
        - Bit 0: Start (1 = start, 0 = stop)
        - Bit 1: LoEj (Load/Eject)
Byte 5: Control
```

**Operations:**
- Load: LoEj=1, Start=1
- Unload: LoEj=1, Start=0
- Retension: Special vendor-specific mode

**Source:** SCSF2 array, tape functions

---

## Custom SCSI Command Support

### Function 74: Execute User-Specified SCSI Command Block

**✅ YES - Applications CAN send custom SCSI commands including INQUIRY**

**Function Code:** 60 (74 octal)

**API Entry Point (Line 1100-1127):**
```npl
SCSID: A=:X.HSTAT:=L=:X."FINISH":=B=:X.SCUDF
       T=:X.SCTRG:=:X; 77/\X.ABFUN; X:=T
       ...
       IF A=74 THEN
          CALL GUSCB     % USER SPECIFIED COMMAND
       ...
```

**GUSCB Routine (Lines 1385-1392):**
```npl
GUSCB: ABPA3=:SUIBC; A:=L=:SAVA:=6=:L       % NUMBER OF WORDS (6 = 12 bytes)
       ABPA2; X:=SMBP1; T:=SMBP2; *MOVPP    % MOVE COMMAND BLOCK FROM USER MEMORY
       IF ABFUN SHZ -14=0 THEN 377/\SUICO FI
       T:=SAVA=:P
```

### How It Works

1. **User builds SCSI Command Descriptor Block (CDB)** in memory
2. **Calls SCSID with:**
   - **ABFUN** = 74 (octal) with LUN in bits 6-8
   - **MEMAD** = Data buffer address (for data-in/data-out phases)
   - **ABPA2** = Pointer to CDB (Command Descriptor Block)
   - **ABPA3** = Byte count for data transfer
   - **A register** = Driver control flags

3. **Driver copies 12-byte CDB** from user memory (line 1389)
4. **Driver executes SCSI command** through normal protocol (EXCOM routine)
5. **Results returned** in user's data buffer

### Parameters

**ABSTR Parameter Block:**

```c
struct ABSTR {
    uint16_t ABFUN;      // Function code = 74 (octal)
                        // Bits 0-5: Function (74)
                        // Bits 6-8: LUN (0-7)

    uint16_t MEMAD_HI;   // Data buffer address (high word)
    uint16_t MEMAD_LO;   // Data buffer address (low word)

    uint16_t ABPA2_HI;   // CDB address (high word)
    uint16_t ABPA2_LO;   // CDB address (low word)
                        // Points to 12-byte command block

    uint16_t ABPA3_HI;   // Byte count (high word)
    uint16_t ABPA3_LO;   // Byte count (low word)
                        // Data transfer length in bytes
};
```

**A Register Control Flags:**

```
Bit 0-3: Log₂ of timeout (seconds = 2^n)
Bit 4:   Use SELECT without ATN
Bit 5:   Don't use disconnect/reconnect
Bit 6:   Return on command accepted (before data transfer complete)
```

### Example: Sending INQUIRY Command

**Step 1: Build INQUIRY CDB in memory:**

```c
uint8_t inquiry_cdb[12] = {
    0x12,           // INQUIRY opcode
    0x00,           // LUN=0, no EVPD
    0x00,           // Page code
    0x00,           // Reserved
    0x24,           // Allocation length = 36 bytes
    0x00,           // Control
    0x00, 0x00,     // Reserved
    0x00, 0x00,     // Reserved
    0x00, 0x00      // Reserved
};
```

**Step 2: Allocate data buffer:**

```c
uint8_t inquiry_data[36];  // Buffer for INQUIRY response
```

**Step 3: Set up ABSTR parameters:**

```c
struct ABSTR params;
params.ABFUN = 074;                    // Function 74 (octal), LUN 0
params.MEMAD = (uint32_t)inquiry_data; // Response buffer address
params.ABPA2 = (uint32_t)inquiry_cdb;  // CDB address
params.ABPA3 = 36;                     // Expect 36 bytes response
```

**Step 4: Call SCSID:**

```assembly
; ND-100 Assembly example
        T = &params
        X = device_datafield
        B = lun_datafield
        A = 4              ; Timeout = 2^4 = 16 seconds
        JPL I (SCSID)
        JMP ERROR_EXIT
        JMP BUSY_EXIT
        JMP SUCCESS_EXIT

SUCCESS_EXIT:
        ; inquiry_data now contains device information
        ; Parse bytes 0-35 for device type, vendor, product, etc.
```

**Step 5: Parse results:**

```c
// After successful return:
uint8_t device_type = inquiry_data[0] & 0x1F;
bool removable = (inquiry_data[1] & 0x80) != 0;
char vendor[9];
memcpy(vendor, &inquiry_data[8], 8);
vendor[8] = '\0';

char product[17];
memcpy(product, &inquiry_data[16], 16);
product[16] = '\0';

char revision[5];
memcpy(revision, &inquiry_data[32], 4);
revision[4] = '\0';
```

---

## INQUIRY Command Support

### Built-in INQUIRY Support

**Function Code:** 61 (75 octal)

Applications can also use the built-in INQUIRY support instead of Function 74:

**Automatic Processing:**
- Sends INQUIRY command
- For disks: Automatically follows with READ CAPACITY(10)
- For tapes: Automatically follows with READ BLOCK LIMITS
- Parses device type
- Stores block size in device data structure
- Sets initialization flags

**Usage:**

```c
struct ABSTR params;
params.ABFUN = 075;          // Function 75 (octal) = INQUIRY
params.MEMAD = buffer_addr;   // Buffer for extended information
params.ABPA2 = 0;            // Not used
params.ABPA3 = 100;          // Buffer size for format info

// Call SCSID
// After return, device is fully initialized and ready
```

**What's Returned:**

For **Function 42** (Inquiry + Read Capacity), returned in MEMAD buffer:

```c
struct FormatInfo {
    uint32_t disk_size;      // Total blocks on disk
    uint32_t block_size;     // Bytes per block
    // ... other fields depend on device type
};
```

**Advantages of Function 75 vs Function 74:**

| Aspect | Function 75 (Built-in) | Function 74 (Custom) |
|--------|----------------------|---------------------|
| **Setup** | Simple - just call with buffer | Must build CDB manually |
| **Parsing** | Automatic | Must parse response manually |
| **Follow-up** | Auto READ CAPACITY/BLOCK LIMITS | Must send separately if needed |
| **Initialization** | Sets device flags automatically | Must track device state manually |
| **Use Case** | Device initialization | Advanced diagnostics, custom commands |

---

## Implementation Requirements

### For C# SCSI Emulator

To properly support SINTRAN's SCSI expectations, the C# emulator must:

#### 1. Implement All Commands

Support all commands in SCSF1/SCSF2 tables, particularly:

- ✅ **Must Have:**
  - TEST UNIT READY
  - REQUEST SENSE (with proper extended sense format)
  - INQUIRY (with proper device type response)
  - READ CAPACITY(10) or READ BLOCK LIMITS
  - READ(6) and WRITE(6)
  - MODE SENSE(6) and MODE SELECT(6)

- ⚠️ **Should Have:**
  - SEEK(6) for performance
  - VERIFY(6) for data integrity
  - RESERVE/RELEASE for multi-host
  - START/STOP UNIT for removable media

- 🔧 **Optional:**
  - FORMAT UNIT
  - WRITE AND VERIFY
  - Tape commands (if emulating tape)

#### 2. Status Handling

**Return proper SCSI status:**

```csharp
public enum ScsiStatus : byte
{
    Good = 0x00,
    CheckCondition = 0x02,
    ConditionMet = 0x04,
    Busy = 0x08,
    Intermediate = 0x10,
    IntermediateConditionMet = 0x14,
    ReservationConflict = 0x18,
    CommandTerminated = 0x22,
    QueueFull = 0x28
}
```

**For CHECK CONDITION, prepare valid extended sense data.**

#### 3. REQUEST SENSE Response

Must return valid extended sense data:

```csharp
public class ExtendedSenseData
{
    public byte ErrorCode = 0x70;        // Current error
    public byte SenseKey;                // 0x00-0x0F
    public byte[] Information = new byte[4];
    public byte AdditionalSenseLength = 10;
    public byte[] CommandSpecificInfo = new byte[4];
    public byte AdditionalSenseCode;     // ASC
    public byte AdditionalSenseCodeQualifier; // ASCQ
    public byte FieldReplaceableUnitCode;
    public byte[] SenseKeySpecific = new byte[3];
}
```

**Critical Sense Keys for SINTRAN:**

- **0x00 (NO SENSE)**: Operation succeeded
- **0x01 (RECOVERED ERROR)**: Error but data is valid
- **0x02 (NOT READY)**: Device not ready
- **0x03 (MEDIUM ERROR)**: Unrecoverable read/write error
- **0x05 (ILLEGAL REQUEST)**: Invalid command or parameter
- **0x06 (UNIT ATTENTION)**: Device reset or media changed - **triggers re-initialization**
- **0x0B (ABORTED COMMAND)**: Command aborted - triggers retry
- **0x0C (COPY ABORTED)**: COPY command failed

#### 4. INQUIRY Response

Must return proper INQUIRY data:

```csharp
public class InquiryData
{
    public byte PeripheralDeviceType;   // 0=disk, 1=tape, 5=CD-ROM
    public bool Removable;
    public byte Version = 0x02;         // SCSI-2 compliance
    public byte ResponseDataFormat = 0x02;
    public byte AdditionalLength = 31;  // n-4 where n=36
    public byte[] Vendor = new byte[8]; // ASCII, space-padded
    public byte[] Product = new byte[16]; // ASCII, space-padded
    public byte[] Revision = new byte[4]; // ASCII

    // Followed by READ CAPACITY or READ BLOCK LIMITS
}
```

#### 5. Function 74 Support

**Must support user-specified CDB execution:**

```csharp
public void ExecuteUserCommand(byte[] cdb, byte[] dataBuffer,
                               int dataLength, bool dataIn)
{
    // 1. Validate CDB (6, 10, or 12 bytes)
    // 2. Parse opcode from cdb[0]
    // 3. Execute command
    // 4. Transfer data if dataLength > 0
    // 5. Return status
}
```

**Security Considerations:**
- Consider limiting dangerous commands (FORMAT UNIT, WRITE BUFFER, etc.)
- Or require special permissions for raw SCSI access
- Log all Function 74 commands for audit

#### 6. Interrupt Handling

**As documented in SCSI-C#-Implementation-Guide.md:**

- Generate interrupts on phase changes
- Set RSTAU bit 11 (NCR interrupt)
- Trigger Level 11 CPU interrupt
- Wait for WCONT=5 (resume) before continuing

**Reference:** See previous analysis in SCSI-C#-Implementation-Guide.md

#### 7. Response Timing

SINTRAN expects responses within timeout period:
- Default: 2^4 = 16 seconds
- Can be specified in A register bits 0-3
- Return BUSY status if operation will take longer
- After BUSY, SINTRAN will retry with delays

#### 8. Block Size Handling

**Critical for proper operation:**

```csharp
// After INQUIRY, device must report:
// - For disks: Use READ CAPACITY(10) to return:
//   * Last block address (total blocks - 1)
//   * Block size in bytes (typically 512 or 1024)
//
// - For tapes: Use READ BLOCK LIMITS to return:
//   * Maximum block length
//   * Minimum block length
//   * If max == min: Fixed block mode
//   * If different: Variable block mode

public class DeviceGeometry
{
    public uint TotalBlocks;
    public uint BlockSize;
    public bool FixedBlockMode;
}
```

SINTRAN uses block size for:
- Calculating DMA transfer lengths
- Converting between logical records and physical blocks
- Validating transfer requests

---

## Testing Checklist

### Basic Commands

- [ ] TEST UNIT READY returns GOOD status
- [ ] INQUIRY returns valid device type
- [ ] READ CAPACITY returns correct disk size
- [ ] REQUEST SENSE returns extended sense data
- [ ] READ(6) transfers data correctly
- [ ] WRITE(6) accepts data correctly

### Status Handling

- [ ] GOOD status allows operation to complete
- [ ] CHECK CONDITION triggers REQUEST SENSE
- [ ] BUSY status causes retry with delay
- [ ] RESERVATION CONFLICT returns proper error

### Sense Keys

- [ ] NO SENSE (0x00) = success
- [ ] RECOVERED ERROR (0x01) = warning logged
- [ ] NOT READY (0x02) = device not ready
- [ ] MEDIUM ERROR (0x03) = data error
- [ ] ILLEGAL REQUEST (0x05) = bad parameter
- [ ] UNIT ATTENTION (0x06) = triggers re-INQUIRY
- [ ] ABORTED COMMAND (0x0B) = triggers retry

### Function 74 (Custom Commands)

- [ ] Can send INQUIRY via Function 74
- [ ] Can send TEST UNIT READY via Function 74
- [ ] Can read INQUIRY response data
- [ ] Invalid CDB returns ILLEGAL REQUEST
- [ ] Works for both data-in and data-out commands

### Interrupt Mode

- [ ] Interrupts generated on phase changes
- [ ] RSTAU bit 11 set on NCR interrupt
- [ ] Level 11 interrupt triggered
- [ ] Operation pauses on WCONT=0
- [ ] Operation resumes on WCONT=5

### Advanced

- [ ] MODE SENSE returns device parameters
- [ ] MODE SELECT changes device parameters
- [ ] VERIFY command works
- [ ] SEEK improves performance (optional)
- [ ] RESERVE/RELEASE work for multi-host (optional)

---

## Conclusion

SINTRAN III has comprehensive SCSI support including:

1. ✅ **All essential SCSI-2 commands** for disks and tapes
2. ✅ **Automatic command sequences** (INQUIRY→READ CAPACITY)
3. ✅ **Full error handling** with proper sense key interpretation
4. ✅ **User-accessible custom commands** via Function 74
5. ✅ **INQUIRY command support** both built-in (Function 75) and custom (Function 74)

**Applications CAN:**
- Send INQUIRY commands using Function 74 (custom CDB)
- Send INQUIRY commands using Function 75 (automatic parsing)
- Send ANY valid SCSI command via Function 74
- Read complete response data
- Parse device information

**For C# Emulator:**
- Implement all commands in SCSF1/SCSF2 arrays
- Return proper extended sense data on CHECK CONDITION
- Support Function 74 for custom CDB execution
- Handle UNIT ATTENTION (sense key 6) correctly - triggers re-initialization
- Generate interrupts on all SCSI phase changes (see previous guide)

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\SCSI-Commands-Analysis.md`
