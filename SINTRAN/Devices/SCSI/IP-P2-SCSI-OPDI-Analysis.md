# IP-P2-SCSI-OPDI.NPL Comprehensive Analysis

**File Path:** `Z:\NorskData\Source Code\Sintran L\NPL\IP-P2-SCSI-OPDI.NPL`

**Module Identifier:** `8SCOD` (SCSI Optical Disk)

**Purpose:** Level 11 routine for performing transfers on SCSI optical disks

---

## Table of Contents

1. [Overview](#overview)
2. [Main Subroutines](#main-subroutines)
3. [Operation Type Control Words](#operation-type-control-words)
4. [Status Flags and Symbols](#status-flags-and-symbols)
5. [Write Recovery Mechanism](#write-recovery-mechanism)
6. [Error Handling](#error-handling)
7. [Differences from Magnetic Disk Driver](#differences-from-magnetic-disk-driver)

---

## Overview

This module implements the SCSI optical disk driver for the Sintran L operating system. It supports a comprehensive set of disk operations including read, write, seek, compare, and device management functions. The driver includes sophisticated error handling, write recovery mechanisms, and device initialization sequences specific to optical media.

### Supported Functions

| Function | Description | Double Address Variant |
|----------|-------------|------------------------|
| 0 | READ | 60 |
| 1 | WRITE | 61 |
| 2 | READ PARITY | 62 |
| 3 | COMPARE | 63 |
| 4 | SEEK | - |
| 34 | RESERVE DEVICE | - |
| 35 | RELEASE DEVICE | - |
| 37 | READ EXTENDED STATUS (SCSI SENSE INFO) | - |
| 42 | READ FORMAT | - |
| 73 | TEST UNIT READY | - |
| 74 | EXECUTE USER SPECIFIED SCSI COMMAND BLOCK | - |
| 75 | INQUIRY (READ DEVICE TYPE) | - |

---

## Main Subroutines

### 1. SCOPTICAL (Lines 29-333)

**Entry Point:** Line 143

**Purpose:** Main entry point for optical disk operations. Handles operation dispatch, parameter validation, disk sorting, and operation sequencing.

**Entry Parameters:**
- `X` = DAQ (Disk Activity Queue pointer)
- `X.NFUNC` = Function code (0-75)
- `X.ABFUN` = Low 6 bits contain function
- `X.MEMAD` = Memory address for data transfer
- `X.ABPA2` = Disk address (single or double)
- `X.ABP32` = Transfer amount
- `L` = Return address

**Exit Parameters:**
- `T` = Status code (0=success, non-zero=error)
- `A` = Sense data (if error)
- `X` = Updated DAQ pointer

**Key Operations:**

1. **Function Code Processing (Lines 143-163)**
   - Extracts function code from `X.ABFUN` (low 7 bits)
   - Looks up operation type in `OPTYP` array
   - Validates operation legality
   - Converts single to double disk addresses if needed (3DPA2 flag)
   - Converts single to double amounts if needed (3DPA3 flag)

2. **Unit Data Field Validation (Lines 164-167)**
   - Calls `PUNDF` to get/validate unit data field
   - Returns `NOLUN` error if unit not defined

3. **Disk Sorting (Lines 171-178)**
   - Checks if operation can be sorted (3SPES flag not set)
   - Calls `DSORT` for queue management if `SLINK` valid
   - Optimizes disk head movements

4. **Operation Initialization (Lines 184-205)**
   - Links operation into queue (`SLINK`)
   - Sets access owner (`STPRW`)
   - Handles special operations (function 42, function 36)
   - Initializes retry count (`TACNS`)
   - Clears write recovery flag (4SRWO)

5. **Device Initialization Sequence (Lines 209-223)**
   - Checks 4SINI flag - if not set, performs INQUIRY (function 36)
   - Validates device type (must be type 3 or 4 for optical)
   - Checks 4SMSL flag - if not set, performs MODE SELECT
   - Sets status flags when complete

6. **Write Recovery Check (Line 226)**
   - If 4SRWO flag set, jumps to CCRWO routine
   - Handles interrupted write operations

7. **Operation Execution (Lines 228-235)**
   - Calls `SCSID` to execute SCSI command
   - Checks sense data with `CHSEN`
   - Handles function 42 special processing with `TER42`
   - Returns success status

**Flow Diagram:**
```
SCOPTICAL Entry
    |
    v
Extract & Validate Function --> [Invalid] --> ERR1
    |
    v [Valid]
Convert Addresses (if needed)
    |
    v
Get Unit Data Field --> [Not Found] --> ERR1
    |
    v [Found]
Check Sort Required?
    | [Yes, SLINK valid]
    v
DSORT (Queue) --> SWT11 (Return)
    |
    | [No Sort / New Op]
    v
NEWOP: Initialize Operation
    |
    v
Device Initialized? (4SINI)
    | [No]
    v
Perform INQUIRY (Fn 36) --> [Error] --> Error Handling
    |
    v [Device Type Valid?]
Check Mode Select (4SMSL)
    | [No]
    v
Perform MDSEL --> [Error] --> Error Handling
    |
    v [Initialized]
Write Recovery? (4SRWO)
    | [Yes]
    v
CCRWO (Recover Write)
    |
    | [No]
    v
Execute SCSID --> [Error] --> Error Path
    |
    v [Success]
Check Sense (CHSEN)
    |
    v
Function 42? --> [Yes] --> TER42
    |
    v
RETEX (Success Return)
```

---

### 2. RETOP (Lines 59-107)

**Purpose:** Terminate operation and update status information.

**Entry Parameters:**
- `B` = Disk data field
- `X` = DAQ pointer
- `T` = Driver status
- `A` = Sense data (if T=0)
- All registers saved to `SVTAD`, `SVXRG`, `SVLRG`

**Exit Parameters:**
- All registers restored
- Status updated in `SCTRG` (X.ABFUN field)
- Error information logged if applicable

**Key Operations:**

1. **Status Translation (Lines 73-81)**
   - Shifts driver status into upper bits
   - If T=0 (sense data available):
     - Extracts low 4 bits of sense (line 77)
     - Looks up error status in `NEWST` array
     - Maps sense codes to Sintran L error codes
   - If T<>0 (serious error):
     - Sets status to 100020 (serious error code)

2. **Compare Operation Handling (Lines 83-87)**
   - Special case for function 3 (COMPARE) and 63 (COMPARE with double address)
   - If sense = 5 (MISCOMPARE), clears error status
   - This is because COMPARE mismatches are not errors

3. **Error Logging (Lines 88-101)**
   - If error flag set (bit 4 of X.HSTAT)
   - Logs error information to system:
     - Device number (9XDV)
     - Function code (9XFU)
     - Device unit (9XUN)
     - Memory address (9XMA)
     - Disk address and amount (9XDA, 9XTA)
     - Hardware status (9XST)
   - Calls `PHLOG` to prepare log
   - Calls `9FLEX` with error number 1663 to report

4. **Return to Caller (Lines 103-106)**
   - Disables interrupts (*IOF)
   - Calls `TO11Q` to return to level 11 queue
   - Enables interrupts (*ION)
   - Restores registers and returns

**NEWST Array Translation Table:**

| Sense Code | NEWST Value | Meaning |
|------------|-------------|---------|
| 0 | 000000 | No error |
| 1 | 000000 | Recovered error (not reported) |
| 2 | 100020 | Not ready |
| 3 | 100020 | Medium error |
| 4 | 100020 | Hardware error |
| 5 | 100020 | Illegal request |
| 6 | 100020 | Unit attention |
| 7 | 000020 | Data protect |
| 8 | 000020 | Blank check |
| 9 | 100020 | Vendor specific |
| 10 | 100020 | Copy aborted |
| 11 | 100020 | Aborted command |
| 12 | 000000 | Equal (for COMPARE) |
| 13 | 100020 | Volume overflow |
| 14 | 000020 | Miscompare |
| 15 | 100020 | Reserved |

---

### 3. MDSEL (Lines 112-125)

**Purpose:** Execute MODE SELECT command to configure optical disk parameters.

**Entry Parameters:**
- `X` = DAQ pointer
- `SUTYP` = Device subtype (bits 10-13 contain device type)
- `X.CMAD1`, `X.CMAD2` = Physical buffer address
- `X.ABFUN` = Current function (preserved in bits 9-11)

**Exit Parameters:**
- Returns to caller after MODE SELECT execution
- Mode Select performed via SCSID

**Key Operations:**

1. **Device Type Check (Lines 114-118)**
   - Shifts `SUTYP` right 10 bits to extract device type
   - Returns 1 if type = 3 (optical disk)
   - Returns 0 otherwise
   - This determines MODE SELECT parameters

2. **Buffer Setup (Line 119)**
   - Saves X register to `MSXRG`
   - Loads physical buffer address from `X.CMAD1:X.CMAD2`

3. **Sense Data Header (Line 120)**
   - Shifts A left 10 bits
   - Stores zero at offset 0 (*STZTX 00)
   - Stores value at offset 10 (STATX 10)
   - Creates sense data header structure

4. **Function Code Construction (Line 121)**
   - Extracts bits 9-11 from original function (700/\)
   - Combines with MODE SELECT function (23)
   - Stores in X.ABFUN
   - Preserves operation context

5. **Parameter Setup (Lines 122-123)**
   - Sets byte count to 4 (0x4 in A register)
   - Shifts left 20 bits and stores in X.ABPA3
   - MODE SELECT transfers 4 bytes of parameter data

6. **Buffer Address (Line 123)**
   - Copies `X.CMADR` to `X.MEMAD`
   - Sets source address for MODE SELECT data

7. **Driver Activation (Line 124)**
   - Loads T with address of ABFUN-14+X
   - Loads "4" constant
   - Jumps to SCSID to execute
   - SCSID handles actual SCSI command sequence

**MODE SELECT Data Format:**

The MODE SELECT command sends 4 bytes to configure the optical disk:

```
Byte 0: Sense Data Header (value from device type check)
Byte 1-3: Mode parameters (stored at buffer address)
```

The specific parameters depend on the optical disk model and are stored in the buffer pointed to by `X.CMADR`.

---

### 4. TER42 (Lines 278-295)

**Purpose:** Update user's function 42 (READ FORMAT) information with disk capacity and layout.

**Entry Parameters:**
- `X` = DAQ pointer
- `SURSZ` = Sector/record size
- `X.MEMA1:MEMA2` = User buffer address

**Exit Parameters:**
- User buffer updated with:
  - Disk layout index (offset 0)
  - Available disk blocks (offset 10-20)

**Key Operations:**

1. **Save Registers (Line 282)**
   - Saves A register to `SAVA`
   - Saves L register to `SAVL`
   - Saves X register to `SAVX`

2. **Determine Disk Layout Index (Lines 283-291)**
   - If `SURSZ` = 4000 (octal 2048 decimal bytes):
     - Returns index 41 (2KB sector layout)
   - Else if `SURSZ` = 2000 (octal 1024 decimal bytes):
     - Returns index 40 (1KB sector layout)
   - Else:
     - Returns -1 (no standard index)

3. **Store Layout Index (Line 292)**
   - Loads user buffer address from `MEMA1:MEMA2`
   - Stores layout index at offset 0 (STATX 00)

4. **Calculate Available Space (Lines 292-294)**
   - Takes `SURSZ` (record size)
   - Loads constant 44000 (octal = 18432 decimal)
   - Loads 17 (octal = 15 decimal)
   - Performs division: (SURSZ * 18432) / 15
   - Result = number of bytes to reserve for test area
   - This reserves approximately 1 megabyte

5. **Calculate Available Blocks (Lines 293-294)**
   - Adds 1 to result (A+1)
   - Stores as first word at offset 10 (*LDDTX 10)
   - Subtracts from total capacity (D-L)
   - Handles double-precision arithmetic with carry
   - Stores final count at offset 10 (STDTX 10)
   - Returns total available blocks minus reserved test area

6. **Restore and Return (Line 295)**
   - Restores A from `SAVA`
   - Restores X from `SAVX`
   - Returns via `SAVL`

**Function 42 Return Data Structure:**

```
Offset 00: Disk Layout Index
           41 = 2KB sectors (SURSZ=4000 octal)
           40 = 1KB sectors (SURSZ=2000 octal)
           -1 = Non-standard size

Offset 10: Available Blocks (Double word)
           Total disk capacity - 1MB test reserve
           Calculated as: total - (SURSZ * 18432 / 15)
```

---

### 5. CCRWO (Lines 299-329)

**Purpose:** Recover aborted write operation using SCSI COMPARE and selective rewrite.

**Background:** The 4SRWO flag indicates a write operation was interrupted by a hardware failure (PFAIL, SBRST, or LIRST). This routine attempts to recover by comparing written data and rewriting failed blocks.

**Entry Parameters:**
- `X` = DAQ pointer (SAVX)
- `X.SLINK` = Original write operation parameters
- `X.CMAD1:CMAD2` = Command buffer address
- `X.OPSTA` bit 4SRWO = 1 (Write recovery needed)

**Exit Parameters:**
- Returns via `RETEX` with status
- 4SRWO flag cleared if successful
- Failed blocks rewritten if necessary

**Recovery Strategy:**

The recovery process uses a two-phase approach:

1. **COMPARE Phase:** Verify which blocks were successfully written
2. **REWRITE Phase:** Write only the blocks that failed

**Key Operations:**

**Phase 1: Build COMPARE Command (Lines 301-310)**

1. **Command Buffer Setup (Lines 301-303)**
   - Saves X to `SAVX`
   - Loads command address from `X.CMAD1:CMAD2`
   - Stores 27402 (COMPARE command code) at offset 0

2. **Set Disk Address (Lines 304-305)**
   - Loads original disk address from `SAVX.SLINK.ABPA2`
   - Converts via `SUSI1` exchange routine
   - Stores as double-precision at offsets 10-20

3. **Set Block Count (Lines 306-309)**
   - Loads original block count from `SAVX.SLINK.ABPA3`
   - Converts via `SUSI1` exchange routine
   - Shifts into proper byte position
   - Stores at offset 30

4. **Set Byte Count (Lines 309-310)**
   - Shifts count by 10 bits
   - Converts via `SUSI3` exchange routine
   - Stores in command buffer
   - Updates `SAVX.ABPA3`

5. **Set Memory Address (Line 311)**
   - Copies original memory address to `SAVX.MEMAD`

6. **Execute COMPARE (Lines 312-314)**
   - Builds function 74 (user-specified command)
   - Preserves bits 9-11 from original function
   - Calls SCSID to execute COMPARE
   - Jumps to error handler if failure

**Phase 2: Handle COMPARE Result (Lines 315-327)**

7. **Check for Blank Blocks (Line 315)**
   - Tests if A = 100010 (BLANK CHECK sense)
   - Indicates blocks were not written

8. **Build WRITE Command (Lines 316-321)**
   - If blank check detected:
     - Loads command buffer address
     - Stores 25000 (WRITE command code) at offset 0
     - Loads failing address from offset 20
     - Calculates new block count: original - failed offset
     - Updates byte count
     - Converts via `SUSI3`

9. **Calculate New Memory Address (Lines 322-324)**
   - Loads byte offset (A)
   - Shifts right 1 bit (word address)
   - Adds to original memory address (double-precision)
   - Handles carry propagation
   - Updates `X.MEMA1:MEMA2`

10. **Execute WRITE (Lines 325-327)**
    - Extracts operation control word
    - Calls SCSID to write failed blocks
    - Jumps to error handler if failure

11. **Check Sense (Line 328)**
    - If A <> 0 (sense data present)
    - Calls CHSEN to validate sense
    - May trigger retry if recoverable

12. **Complete Recovery (Line 329)**
    - Jumps to RETEX to finish operation
    - Clears 4SRWO flag on success
    - Updates status and returns

**Recovery Flow:**

```
CCRWO Entry (4SRWO flag set)
    |
    v
Build COMPARE Command
    - Set disk address (original)
    - Set block count (original)
    - Set memory address (original)
    |
    v
Execute COMPARE (Function 74)
    |
    +--[Success]--> All blocks match --> RETEX (Success)
    |
    +--[100010 Blank Check]--> Some blocks failed
            |
            v
        Calculate Failed Blocks
            - Get failing address
            - Calculate remaining count
            - Adjust memory address
            |
            v
        Build WRITE Command
            - Write only failed blocks
            |
            v
        Execute WRITE (Function 74)
            |
            +--[Success]--> RETEX (Success)
            |
            +--[Error]--> ERRET (Error handling)
```

**Why This Recovery Mechanism?**

Optical disks have unique characteristics compared to magnetic disks:

1. **Write-Once Nature (for WORM):** Some optical media is write-once. Cannot simply rewrite entire transfer.

2. **Long Write Times:** Optical writes are slower than magnetic. Recovering only failed blocks saves time.

3. **Power Failure Resilience:** If power fails during write, some blocks may be written successfully. COMPARE verifies which blocks are good.

4. **Block-Level Verification:** SCSI COMPARE command operates at block level, allowing precise identification of failed blocks.

5. **Efficiency:** Only rewrites blocks that actually failed, rather than entire transfer.

**Example Scenario:**

```
Original Write: 10 blocks starting at LBA 1000
Power failure after block 6 written

Recovery Process:
1. COMPARE 10 blocks at LBA 1000
2. Hardware returns BLANK CHECK at LBA 1006
3. Calculate: 10 - 6 = 4 blocks remaining
4. Write 4 blocks starting at LBA 1006
5. Recovery complete
```

---

### 6. CHSEN (Lines 238-245)

**Purpose:** Validate sense data and determine if retry is appropriate.

**Entry Parameters:**
- `A` = Sense data from SCSI command
- `X` = DAQ pointer
- `B` = Disk data field

**Exit Parameters:**
- Returns to caller if sense acceptable
- May trigger RETRY if recoverable error
- May return with error status if non-recoverable

**Key Operations:**

1. **Extract Sense Key (Line 239)**
   - Masks A with BZERO 17 (clears upper bits)
   - Extracts sense key (bits 0-3)
   - Stores in D register
   - Checks if > 1 (not NO SENSE or RECOVERED ERROR)

2. **Check Recoverable Conditions (Lines 240-242)**
   - If sense = 6 (UNIT ATTENTION):
     - Decrements retry counter (MIN X.TACOU)
     - Jumps to RETRY
     - Unit attention means device reset or media change
   - If sense = 13 (ABORTED COMMAND):
     - Decrements retry counter
     - Jumps to RETRY
     - Command was interrupted, can be retried

3. **Non-Recoverable Error (Line 243)**
   - If sense not 6 or 13:
     - Saves X to B
     - Sets T = 0 (indicates sense data valid)
     - Jumps to RETEX (error return)

4. **Exit (Line 245)**
   - If sense acceptable (0 or 1):
     - Returns to caller via EXIT
     - Operation continues normally

**Sense Key Reference:**

| Code | Name | CHSEN Action | Reason |
|------|------|--------------|--------|
| 0 | NO SENSE | Accept | No error |
| 1 | RECOVERED ERROR | Accept | Error corrected by device |
| 2 | NOT READY | Error | Device not ready |
| 3 | MEDIUM ERROR | Error | Defect in medium |
| 4 | HARDWARE ERROR | Error | Device hardware failure |
| 5 | ILLEGAL REQUEST | Error | Invalid command |
| 6 | UNIT ATTENTION | Retry | Device reset/media changed |
| 7 | DATA PROTECT | Error | Media write-protected |
| 8 | BLANK CHECK | Error | Unwritten blocks |
| 9 | VENDOR SPECIFIC | Error | Vendor error |
| 10 | COPY ABORTED | Error | Copy command failed |
| 11 | ABORTED COMMAND | Retry | Command interrupted |
| 12 | EQUAL | Accept | COMPARE matched |
| 13 | VOLUME OVERFLOW | Retry | End of partition |
| 14 | MISCOMPARE | Error | COMPARE failed |
| 15 | RESERVED | Error | Reserved code |

---

### 7. Error Handling Routines

#### ERRET (Lines 248-254)

**Purpose:** Handle retryable errors from SCSID execution.

**Entry Parameters:**
- `T` = Error code from SCSID (PFAIL, SBRST, or LIRST)
- `X` = DAQ pointer

**Exit Parameters:**
- Returns to RETRY if retries remain
- Falls through to ERREX if retries exhausted

**Key Operations:**

1. **Check Error Type (Line 248)**
   - Tests if T = PFAIL (power failure)
   - Or T = SBRST (SCSI bus reset)
   - Or T = LIRST (link reset)
   - These are all recoverable hardware errors

2. **Handle Write Operations (Lines 249-252)**
   - Saves X to D
   - Extracts function code from X.SLINK.ABFUN (low 7 bits)
   - Checks if function = 1 (WRITE) or 61 (WRITE double address)
   - If write operation:
     - Sets 4SRWO flag (write recovery needed)
     - Prevents data loss from partial writes

3. **Attempt Retry (Line 253)**
   - Decrements retry counter (MIN X.TACOU)
   - Jumps to RETRY
   - Allows multiple retry attempts

#### ERREX (Lines 256-257)

**Purpose:** Prepare error exit with error code.

**Entry Parameters:**
- `X` = DAQ pointer (stored in B)

**Exit Parameters:**
- `B` = DAQ pointer
- `T` = 20 (octal = 16 decimal, indicates error)
- Falls through to RETEX

**Key Operations:**

1. **Set Error Code (Lines 256-257)**
   - Saves X to B
   - Loads 20 (error indicator)
   - Prepares for RETOP call

#### RETEX (Lines 258-273)

**Purpose:** Return from operation with status, clean up queue, and optionally start next operation.

**Entry Parameters:**
- `B` = DAQ pointer
- `T` = Status code (0 = success, non-zero = error)
- `A` = Sense data (if T=0)
- `X` = May contain updated DAQ

**Exit Parameters:**
- Operation terminated
- Queue updated
- Next operation started if queue not empty
- Returns to level 11 via SWT11

**Key Operations:**

1. **Save and Prepare (Line 258)**
   - Saves B to D
   - Loads ULINK to X
   - Saves X to B
   - Loads X.SLINK (current operation)

2. **Clear Access and Terminate (Lines 259-260)**
   - Clears STPRW (access owner)
   - Calls RETOP to terminate operation
   - Restores B from D
   - Clears SLINK (unlink from queue)
   - Decrements SUNOP (active operation count)

3. **Check for Next Operation (Lines 261-272)**
   - Loads SCLINK (sorted queue link)
   - If SCLINK <> 0 (queue not empty):

   a. **Update Queue Pointers (Lines 262-264)**
      - Loads next operation from X.NLINK
      - Updates SCLINK
      - If SCLINK = 0:
        - Clears PLELE (last element pointer)
        - Queue now empty

   b. **Check Direction Change (Lines 265-270)**
      - If PLHAD = X (was last in current direction):
        - Clears PLHAD
      - Else if status = 0 (success):
        - Sets PLHAD = PLELE
        - Decrements MOVME counter
        - Changes head direction

   c. **Start Next Operation (Line 271)**
      - Jumps to NEWOP
      - Begins processing next queued operation

4. **Return if Queue Empty (Line 273)**
   - Jumps to SWT11
   - Returns to level 11 scheduler

**Queue Management Logic:**

The queue system optimizes disk seeks:

- **SCLINK:** Points to sorted operation queue
- **PLHAD:** Last head position in current direction
- **PLELE:** Last element in queue
- **MOVME:** Movement meter for direction changes

**Direction Change Algorithm:**

```
If operation completed successfully (A=0):
    If PLHAD <> current operation:
        Set PLHAD = PLELE (last element)
        Decrement MOVME
        Change direction (optimization for elevator algorithm)
```

This implements an elevator seek algorithm to minimize head movements.

---

### 8. ERR1 (Lines 181-182)

**Purpose:** Error exit point for illegal operations detected early.

**Entry Parameters:**
- `X` = DAQ pointer
- `T` = Error code (ILAOP or NOLUN)

**Exit Parameters:**
- Operation terminated via RETOP
- Returns to SWT11

**Key Operations:**

1. **Terminate with Error (Line 181)**
   - Calls RETOP to terminate operation
   - Error code already in T
   - Jumps to SWT11

**Used For:**

- **ILAOP (Line 153):** Illegal operation code
  - Function not in OPTYP table
  - Operation control word = 0

- **NOLUN (Line 165):** No logical unit number
  - Unit data field not generated
  - PUNDF returned 0
  - Device not configured

---

## Operation Type Control Words

**OPTYP Array** (Lines 130-138)

This array defines control words for each function code (0-63). Each word contains flags that control operation processing.

### Array Structure

```assembly
INTEGER ARRAY OPTYP := (
    % Functions 0-7
    100004, 100004, 100004, 100004, 110004, 000000, 000000, 000000,

    % Functions 8-15
    000000, 000000, 000000, 000000, 000000, 000000, 000000, 000000,

    % Functions 16-23
    000000, 000000, 000000, 000000, 000000, 000000, 000000, 000000,

    % Functions 24-31
    000000, 000000, 000000, 000000, 104004, 104004, 044000, 044000,

    % Functions 32-39
    000000, 000000, 106004, 000000, 000000, 000000, 000000, 000000,

    % Functions 40-47
    000000, 000000, 000000, 000000, 000000, 000000, 000000, 000000,

    % Functions 48-55
    110004, 110004, 110004, 110004, 000000, 000000, 110004, 000000,

    % Functions 56-63
    000000, 000000, 000000, 134002, 074004, 134002, 000000, 000000
)
```

### Control Word Bit Flags

Defined in lines 42-47:

| Bit | Symbol | Purpose |
|-----|--------|---------|
| 17 | 3SERR | Error message - log errors to system |
| 16 | 3SNTR | Neutral operation - skip device init |
| 15 | 3DPA3 | Parameter 3 double - convert ABP32 to double |
| 14 | 3DPA2 | Parameter 2 double - convert ABPA2 to double disk address |
| 13 | 3SPES | Special operation - not sorted, execute immediately |
| 12 | 3SF42 | Function 42 - call TER42 for post-processing |

### Operation Type Decoding

| Function | OPTYP Value | Flags Set | Meaning |
|----------|-------------|-----------|---------|
| 0 | 100004 | 3SERR | READ - log errors |
| 1 | 100004 | 3SERR | WRITE - log errors |
| 2 | 100004 | 3SERR | READ PARITY - log errors |
| 3 | 100004 | 3SERR | COMPARE - log errors |
| 4 | 110004 | 3SERR, 3SPES | SEEK - log errors, no sort |
| 5-27 | 000000 | - | Undefined/illegal operations |
| 28 | 104004 | 3SERR, 3DPA2 | Reserved (double address) |
| 29 | 104004 | 3SERR, 3DPA2 | Reserved (double address) |
| 30 | 044000 | 3DPA3, 3DPA2 | Reserved (double params) |
| 31 | 044000 | 3DPA3, 3DPA2 | Reserved (double params) |
| 32-33 | 000000 | - | Undefined |
| 34 | 106004 | 3SERR, 3SNTR, 3DPA2 | RESERVE DEVICE - neutral, double addr |
| 35-41 | 000000 | - | Undefined |
| 42 | 000000 | - | READ FORMAT (handled specially) |
| 43-47 | 000000 | - | Undefined |
| 48 | 110004 | 3SERR, 3SPES | Reserved - special |
| 49 | 110004 | 3SERR, 3SPES | Reserved - special |
| 50 | 110004 | 3SERR, 3SPES | Reserved - special |
| 51 | 110004 | 3SERR, 3SPES | Reserved - special |
| 52-53 | 000000 | - | Undefined |
| 54 | 110004 | 3SERR, 3SPES | Reserved - special |
| 55-58 | 000000 | - | Undefined |
| 59 | 134002 | 3SERR, 3SNTR, 3SPES, 3DPA2 | Reserved - special, neutral, double |
| 60 | 074004 | 3SERR, 3DPA3, 3DPA2 | READ (double disk address) |
| 61 | 134002 | 3SERR, 3SNTR, 3SPES, 3DPA2 | WRITE (double disk address) |
| 62-63 | 000000 | - | Undefined |

### Special Handling

**Function 42 (READ FORMAT):**
- OPTYP value is 000000 (no flags)
- But has special code path (lines 189-204)
- Sets function to READ with 100 byte transfer
- Calls TER42 for post-processing
- Does not participate in sorting

**Function 36 (INQUIRY):**
- Not in OPTYP array (internal function)
- Used during device initialization
- Called from line 211

**Function 23 (MODE SELECT):**
- Not in OPTYP array (internal function)
- Called from MDSEL routine
- Used during device initialization

### Flag Processing

**3DPA2 (Bit 14) - Double Disk Address:**

Lines 155-159:
```assembly
IF A=:L NBIT 3DPA2 THEN
   X.ABP21=:D                        % CONVERT TO DOUBLE DISK ADDRESS
   X.ABFUN SHZ -11/\7
   AD=:X.ABPA2
FI
```

Converts single-word disk address to double-word:
- Stores 0 in high word (ABP21)
- Shifts function right 11, masks 7 bits
- Stores original address in low word (ABPA2)

**3DPA3 (Bit 15) - Double Amount:**

Lines 160-162:
```assembly
IF L NBIT 3DPA3 THEN
   X.ABP31=:X.ABP32; 0=:X.ABP31      % CONVERT TO DOUBLE AMOUNT
FI
```

Converts single-word amount to double-word:
- Copies ABP32 to ABP31 (high word = 0)
- Moves original to ABP32 (low word)

**3SPES (Bit 13) - Special Operation:**

Lines 173-175:
```assembly
IF L NBIT 3SPES THEN
   X.TYPCO BONE SSEEK=:X.TYPCO       % SORT POSSIBLE
FI
```

Controls disk sorting:
- If NOT set: marks operation sortable (SSEEK flag)
- If set: operation executes immediately without sorting
- Used for SEEK, RESERVE, and other non-data operations

**3SERR (Bit 17) - Error Logging:**

Lines 91-101:
```assembly
IF A<0 AND X.DQOPC BIT 3SERR THEN
   A:=B; CALL PHLOG; A=:9XDV         % DEVICE NUMBER
   [... log error details ...]
   CALL 9FLEX(9XER,12)               % REPORT ERROR
FI
```

Controls error reporting:
- If set: logs errors to system console/log
- If clear: silent error return
- Most data operations have this flag

**3SNTR (Bit 16) - Neutral Operation:**

Lines 209-223:
```assembly
IF X.SCOCW NBIT 3SNTR THEN
   [... perform INQUIRY and MODE SELECT ...]
FI
```

Controls device initialization:
- If NOT set: performs INQUIRY and MODE SELECT before operation
- If set: skips initialization (neutral)
- Used for device management operations

**3SF42 (Bit 12) - Function 42 Post-Processing:**

Lines 232-234:
```assembly
IF T:=X.SCOCW BIT 3SF42 THEN
   CALL TER42                        % RETURN FUNCTION 42 INFORMATION
FI
```

Triggers special post-processing:
- Only used for function 42 (READ FORMAT)
- Calls TER42 to calculate and return capacity info

### Comparison with Magnetic Disk (IP-P2-SCSI-DISK.NPL)

Key differences in operation types:

1. **Function 4 (SEEK):**
   - Magnetic: 000004 (sortable)
   - Optical: 110004 (special, not sorted)
   - Reason: Optical seeks are longer, don't benefit from sorting

2. **Function 34 (RESERVE):**
   - Magnetic: 106004 (neutral, double addr)
   - Optical: 106004 (same)
   - Both support device reservation

3. **Function 60-63 (Double Address Operations):**
   - Magnetic: Full implementation with all flags
   - Optical: Partial implementation
   - Functions 62-63 undefined in optical (000000)

4. **Function 42 (READ FORMAT):**
   - Magnetic: May include 3SF42 flag
   - Optical: Special handling without flag
   - Different capacity calculation methods

---

## Status Flags and Symbols

### Device Status Flags (OPSTA)

Stored in HDEV field of device data structure (lines 32-36).

**Definition:**
```assembly
DISP 0
   INTEGER OPSTA=HDEV
      SYMBOL 4SINI=0      % INITIALIZATION PERFORMED
      SYMBOL 4SMSL=1      % MODE SELECT PERFORMED
      SYMBOL 4SRWO=3      % RECOVER OF WRITE OPERATION IN PROGRESS
PSID
```

| Bit | Symbol | Purpose | Set When | Cleared When |
|-----|--------|---------|----------|--------------|
| 0 | 4SINI | Initialization performed | INQUIRY succeeds (line 217) | Never (persistent) |
| 1 | 4SMSL | Mode Select performed | MODE SELECT succeeds (line 222) | Never (persistent) |
| 3 | 4SRWO | Write recovery in progress | Write interrupted by hardware error (line 251) | Write recovery completes (line 207) |

**Bit Testing:**

- **BIT:** Test if bit is set (1)
  - `IF X.OPSTA BIT 4SRWO` (line 226)
  - Jumps to CCRWO if write recovery needed

- **NBIT:** Test if bit is NOT set (0)
  - `IF X.OPSTA NBIT 4SINI` (line 210)
  - Performs INQUIRY if not initialized

**Bit Setting:**

- **BONE:** Set bit to 1
  - `X.OPSTA BONE 4SINI=:X.OPSTA` (line 217)
  - Marks initialization complete

- **BZERO:** Clear bit to 0
  - `X.OPSTA BZERO 4SRWO=:X.OPSTA` (line 207)
  - Clears write recovery flag

### Initialization Sequence

Lines 209-223 show the initialization sequence:

```assembly
RETRY: IF X.SCOCW NBIT 3SNTR THEN              % Not neutral operation?
          IF X.OPSTA NBIT 4SINI THEN           % Not initialized?
             36=:X.ABFUN; T:=X                 % Function 36 = INQUIRY
             CALL SCSID; GO FAR ERRET; CALL ERRFATAL
             IF A><0 THEN CALL CHSEN FI        % Check sense
             IF SUTYP SHZ -10><3 AND ><4 THEN  % Device type 3 or 4?
                T:=TYPER; GO FAR ERREX         % Illegal device type
             FI
             X.OPSTA BONE 4SINI=:X.OPSTA       % Mark initialized
          FI
          IF X.OPSTA NBIT 4SMSL THEN           % Mode select not done?
             CALL FAR MDSEL; GO FAR ERRET; CALL ERRFATAL
             IF A><0 THEN CALL CHSEN FI        % Check sense
             X.OPSTA BONE 4SMSL=:X.OPSTA       % Mark mode select done
          FI
       FI
```

**State Transitions:**

```
Initial State: OPSTA = 000000 (all flags clear)
    |
    v [First non-neutral operation]
Execute INQUIRY (Function 36)
    |
    v [Success]
Set 4SINI: OPSTA = 000001
    |
    v
Execute MODE SELECT
    |
    v [Success]
Set 4SMSL: OPSTA = 000003
    |
    v [Device ready for normal operations]

[Later, during WRITE operation...]
    |
    v [Hardware failure during write]
Set 4SRWO: OPSTA = 000013
    |
    v [Next operation]
Execute CCRWO (write recovery)
    |
    v [Recovery complete]
Clear 4SRWO: OPSTA = 000003
```

### Device Type Values (SUTYP)

Stored in device configuration, validated during initialization.

**Device Type Field (bits 10-13):**

```assembly
SUTYP SHZ -10   % Extract device type from bits 10-13
```

Line 214 validates optical device types:

```assembly
IF SUTYP SHZ -10><3 AND ><4 THEN
   T:=TYPER; GO FAR ERREX           % ILLEGAL DEVICE TYPE
FI
```

**Valid Types:**
- **Type 3:** Optical disk (WORM or magneto-optical)
- **Type 4:** WORM (Write-Once-Read-Many) optical disk

**Invalid Types:**
- Type 0: Direct-access (magnetic disk) - Not supported
- Type 1: Sequential-access (tape) - Not supported
- Type 2: Printer - Not supported
- Type 5+: Reserved/other - Not supported

**Why Only Types 3 and 4?**

This driver is specifically for optical media. The MDSEL routine (line 114) also checks device type to determine MODE SELECT parameters:

```assembly
MDSEL: IF SUTYP SHZ -10=3 THEN
          1                          % Optical disk parameters
       ELSE
          "0"                        % Default parameters
       FI
```

Type 3 (optical) requires different MODE SELECT data than other SCSI devices.

### Retry Counter (TACOU)

Each operation has a retry counter initialized from TACNS (line 206):

```assembly
X:=SCDFA:=:B; X.TACNS=:X.TACOU      % RETRY COUNT
```

**Decrement Operations:**

- Line 241: `MIN X.TACOU; GO RETRY` (Unit attention)
- Line 253: `MIN X.TACOU; GO FAR RETRY` (Hardware error)

**MIN Instruction:**

The MIN (Minus) instruction decrements and tests:
- Decrements TACOU by 1
- If result >= 0: continues to RETRY
- If result < 0: falls through to next instruction (retries exhausted)

**Retry Conditions:**

| Condition | Line | Sense/Error | Action |
|-----------|------|-------------|--------|
| Unit attention | 240 | Sense = 6 | Retry |
| Aborted command | 241 | Sense = 13 | Retry |
| Power failure | 253 | T = PFAIL | Retry + set 4SRWO if write |
| SCSI bus reset | 253 | T = SBRST | Retry + set 4SRWO if write |
| Link reset | 253 | T = LIRST | Retry + set 4SRWO if write |

### Queue Management Fields

**SLINK** (Sorted queue link):
- Points to current operation in queue
- Set to 0 when operation completes
- Checked at line 176 for sorting decision

**SCLINK** (Sorted chain link):
- Points to head of sorted operation queue
- Managed by DSORT routine
- Checked at line 261 to start next operation

**PLHAD** (Previous last head address):
- Tracks last head position in current direction
- Used for elevator algorithm optimization
- Updated at lines 266-269

**PLELE** (Previous last element):
- Points to last element in sorted queue
- Cleared when queue empties (line 263)
- Used for direction changes (line 269)

**SUNOP** (Sorted unit operations):
- Counts active operations on device
- Incremented by DSORT when operation queued
- Decremented at line 260 when operation completes
- Tested at line 172 before sorting

**STPRW** (Storage access owner):
- Identifies program/task that initiated operation
- Set from ULINK.STPRW at line 185
- Cleared at line 259 when operation completes
- Used for access control and accounting

---

## Write Recovery Mechanism (4SRWO)

The 4SRWO (bit 3 of OPSTA) flag implements a sophisticated write recovery system to prevent data loss when write operations are interrupted by hardware failures.

### Overview

**Purpose:** Recover from write operations interrupted by:
- Power failures (PFAIL)
- SCSI bus resets (SBRST)
- Link interface resets (LIRST)

**Strategy:**
1. Mark write as needing recovery (set 4SRWO)
2. On next operation, verify written blocks with COMPARE
3. Identify failed blocks via BLANK CHECK
4. Rewrite only failed blocks
5. Clear recovery flag on success

### Triggering Write Recovery

**Lines 248-254 (ERRET routine):**

```assembly
ERRET: IF PFAIL=T OR SBRST=T OR LIRST=T THEN
          D:=X; 77/\X.SLINK.ABFUN; D=:X:=A  % FUNCTION CODE
          IF 1=D OR 61=D THEN              % WRITE or WRITE (double addr)?
             X.OPSTA BONE 4SRWO=:X.OPSTA   % WRITE RECOVER NECESSARY
          FI
          MIN X.TACOU; GO FAR RETRY        % ATTEMPT RETRY
       FI
```

**Conditions:**
1. Hardware error occurred (PFAIL, SBRST, or LIRST)
2. Current operation is WRITE (function 1 or 61)
3. System was actively writing data when failure occurred

**Action:**
- Sets 4SRWO flag in OPSTA
- Decrements retry counter
- Jumps to RETRY to attempt recovery

### Recovery Entry Point

**Line 226:**

```assembly
IF X.OPSTA BIT 4SRWO GO FAR CCRWO    % ATTEMPT TO RECOVER ABORTED WRITE
```

**Checked:**
- At start of RETRY loop (line 226)
- Before executing operation
- After device initialization complete

**Purpose:**
- Intercept next operation on device
- Perform write recovery before continuing
- Ensure data consistency

### Recovery Process (CCRWO Routine)

**Lines 301-329 implement the complete recovery procedure.**

#### Phase 1: Verify Written Blocks (Lines 301-314)

**Build COMPARE Command:**

```assembly
CCRWO: X=:SAVX; T:=X.CMAD1=:X.ABP21        % Command address
       X.CMAD2+2=:X.ABP22=:X
       27402; *STATX 00                    % COMPARE command (0x27402)
       SAVX.SLINK.ABPA2; T:=SUSI1; *EXR ST % Device block address
       T:=SAVX.ABP21; X:=X.ABP22; *STDTX 10
       SAVX.SLINK.ABPA3; T:=SUSI1; *EXR ST % Device number of blocks
       T:=SAVX.ABP21; X:=X.ABP22
       A:=0; AD SHZ 10; *STDTX 30
       AD SHZ -10; T:=SUSI3; *EXR ST       % Bytecount
       AD=:SAVX.ABPA3
       X.SLINK.MEMAD=:SAVX.MEMAD           % Memory address
       700/\X.SLINK.ABFUN\/74=:SAVX.ABFUN  % User specified command (74)
       377/\X.SCOCW; T:=X
       CALL SCSID; GO FAR ERRET; CALL ERRFATAL
```

**SCSI Command Block Built:**

```
Byte 0-1: 27402 (octal) = COMPARE command
Byte 2-5: Device block address (from original ABPA2)
Byte 6-8: Number of blocks (from original ABPA3)
Byte 9-11: Byte count (blocks * block size)
```

**Memory Address:**
- Uses original memory address (MEMAD)
- Points to data that was being written
- COMPARE will check if disk matches this data

**Function 74:**
- User-specified SCSI command
- Allows execution of custom command blocks
- Used to execute COMPARE command

**SCSID Call:**
- Executes the COMPARE command
- Returns status in A register
- Branches to ERRET on hardware error
- Calls ERRFATAL on serious errors

#### Phase 2: Analyze COMPARE Result (Lines 315-329)

**Case 1: All Blocks Match**

```assembly
% If A=0 after SCSID, all blocks compare successfully
% Falls through to line 328, checks sense
% Goes to RETEX with success status
```

**Result:**
- All blocks were written successfully before failure
- 4SRWO flag cleared (line 207 on next operation)
- Operation completes normally

**Case 2: Some Blocks Failed (BLANK CHECK)**

```assembly
IF A=100010 THEN                     % BLANK CHECK sense code
   T:=X.ABP21; X=:SAVX:=X.ABP22      % Command address
   25000; *STATX 00; LDATX 20        % WRITE command (0x25000)
   A=:L; ABPA3; *STDTX 10; LDDTX 30  % Failing address from COMPARE
   AD SHZ -10; ABP32-L=:L:=:D-D      % Calculate remaining blocks
   AD SHZ -10; *STDTX 30
   AD SHZ -10; T:=SUSI3; *EXR ST     % New bytecount
   AD=:SAVX.ABPA3; A:=0; D:=L; *EXR ST
   AD SHZ -1; A:=:D+X.MEMA2=:X.MEMA2 % Adjust memory address
   A:=D+C+X.MEMA1=:X.MEMA1
   377/\X.SCOCW; T:=X
   CALL SCSID; GO FAR ERRET; CALL ERRFATAL
FI
```

**BLANK CHECK Interpretation:**

SCSI COMPARE returns BLANK CHECK when:
- Blocks contain all zeros (unwritten)
- Or blocks don't match source data
- Returns failing block address in sense data

**Build WRITE Command:**

```
Byte 0-1: 25000 (octal) = WRITE command
Byte 2-5: Failing block address (from COMPARE sense)
Byte 6-8: Remaining block count (total - failed offset)
Byte 9-11: New byte count
```

**Calculate Remaining Blocks:**

```
Original request: Write N blocks starting at LBA X
COMPARE failed at block: LBA Y

Remaining blocks = N - (Y - X)
New start address = Y
New memory address = original + ((Y-X) * block_size)
```

**Example:**

```
Original: Write 10 blocks at LBA 1000
Memory: 0x10000

COMPARE fails at LBA 1006 (block 6)

Remaining: 10 - 6 = 4 blocks
New start: LBA 1006
New memory: 0x10000 + (6 * 512) = 0x10C00

Rewrite: 4 blocks at LBA 1006 from 0x10C00
```

**Memory Address Calculation (Lines 322-324):**

```assembly
AD SHZ -1; A:=:D+X.MEMA2=:X.MEMA2 % Adjust memory address
A:=D+C+X.MEMA1=:X.MEMA1
```

- Shifts byte count right 1 (divide by 2 = word count)
- Adds to low word of memory address (MEMA2)
- Adds carry to high word (MEMA1)
- Result: new memory address for partial write

**Execute Partial Write:**

```assembly
377/\X.SCOCW; T:=X
CALL SCSID; GO FAR ERRET; CALL ERRFATAL
```

- Calls SCSID to write remaining blocks
- Goes to ERRET if hardware error (may retry)
- Calls ERRFATAL if serious error

#### Phase 3: Completion (Lines 328-329)

```assembly
IF A><0 THEN CALL FAR CHSEN FI       % Check sense
GO FAR RETEX                         % Complete recovery
```

**Success Path:**
1. CHSEN validates sense data
2. If sense acceptable (0 or 1), returns
3. RETEX completes operation
4. Next operation clears 4SRWO (line 207)

**Error Path:**
1. CHSEN detects non-recoverable error
2. Jumps to RETEX with error status
3. 4SRWO remains set
4. Next operation will retry recovery

### Complete Recovery Flow Diagram

```
Write Operation in Progress
    |
    v
Hardware Failure (PFAIL/SBRST/LIRST)
    |
    v
ERRET: Check if WRITE operation
    | [Yes]
    v
Set 4SRWO Flag
    |
    v
Retry Operation (MIN X.TACOU)
    |
    v
[Next operation or retry]
    |
    v
Check 4SRWO Flag (line 226)
    | [Set]
    v
CCRWO Entry
    |
    +---[Phase 1: Verify]---+
    |                       |
    | Build COMPARE Command |
    | - Original address    |
    | - Original count      |
    | - Original memory     |
    |                       |
    | Execute SCSID(74)     |
    |                       |
    +----------+------------+
               |
               v
    +----------+----------+
    |                     |
[All Match]         [Blank Check]
    |                     |
    v                     v
Success             +---[Phase 2: Rewrite]---+
    |               |                         |
    |               | Get failing address     |
    |               | Calculate remaining     |
    |               | Adjust memory address   |
    |               |                         |
    |               | Build WRITE Command     |
    |               | Execute SCSID(74)       |
    |               |                         |
    |               +----------+--------------+
    |                          |
    +----------+---------------+
               |
               v
    +---[Phase 3: Complete]---+
    |                          |
    | Check Sense (CHSEN)      |
    |                          |
    | RETEX (Return)           |
    |                          |
    | Clear 4SRWO (next op)    |
    |                          |
    +--------------------------+
```

### Recovery Examples

#### Example 1: Successful Recovery

```
Initial Write:
- Function: 1 (WRITE)
- Address: LBA 5000
- Count: 20 blocks
- Memory: 0x20000

Event:
- Power failure after block 12 written

Recovery:
1. Set 4SRWO flag
2. Next operation checks 4SRWO
3. COMPARE 20 blocks at LBA 5000
4. BLANK CHECK at LBA 5012 (block 12)
5. Calculate: 20 - 12 = 8 blocks remaining
6. Memory: 0x20000 + (12 * 512) = 0x23000
7. WRITE 8 blocks at LBA 5012 from 0x23000
8. Success - clear 4SRWO

Result:
- All 20 blocks written correctly
- No data loss
```

#### Example 2: Multiple Failures

```
Initial Write:
- Function: 61 (WRITE double address)
- Address: LBA 100000 (large disk)
- Count: 50 blocks
- Memory: 0x40000

Event:
- SCSI bus reset during write

Recovery Attempt 1:
1. Set 4SRWO flag, retry count = 5
2. COMPARE fails at LBA 100025
3. WRITE remaining 25 blocks
4. Hardware error during rewrite
5. Decrement retry, still 4SRWO set

Recovery Attempt 2:
1. Retry count = 4
2. COMPARE fails at LBA 100025
3. WRITE remaining 25 blocks
4. Success
5. Clear 4SRWO

Result:
- Required 2 recovery attempts
- All 50 blocks eventually written
- Retry mechanism prevented data loss
```

#### Example 3: Unrecoverable Error

```
Initial Write:
- Function: 1 (WRITE)
- Address: LBA 2000
- Count: 10 blocks
- Memory: 0x15000

Event:
- Link interface reset during write

Recovery Attempts:
1. Set 4SRWO, retry = 5
2. COMPARE fails at LBA 2005
3. WRITE remaining 5 blocks
4. Hardware error (attempt 1)

5. Retry = 4
6. COMPARE fails at LBA 2005
7. WRITE remaining 5 blocks
8. Hardware error (attempt 2)

[... attempts 3, 4, 5 fail similarly ...]

9. Retry = 0 (exhausted)
10. Go to ERREX with error status
11. 4SRWO remains set

Result:
- Recovery failed after 5 attempts
- Blocks 2000-2004 written successfully
- Blocks 2005-2009 failed
- Error returned to caller
- 4SRWO flag persists for potential manual recovery
```

### Design Rationale

**Why COMPARE Instead of READ?**

1. **Efficiency:** COMPARE is faster than READ
   - No data transfer to memory
   - Only compares on device
   - Returns immediately on mismatch

2. **Accuracy:** Verifies exact match
   - READ + compare in software has timing issues
   - COMPARE is atomic on device
   - No race conditions

3. **BLANK CHECK:** SCSI devices return specific sense
   - Identifies unwritten blocks
   - Provides failing block address
   - Allows precise recovery

**Why Block-Level Recovery?**

1. **Optical Media:** May have write-once regions
   - Cannot simply overwrite entire transfer
   - Must identify which blocks failed
   - Minimize rewrites on WORM media

2. **Performance:** Large transfers on optical are slow
   - Rewriting only failed blocks saves time
   - Reduces wear on media
   - Minimizes recovery overhead

3. **Reliability:** Partial success is common
   - Power failures don't instantly stop writes
   - Some blocks complete before interrupt
   - Recovery preserves completed blocks

**Why User-Specified Command (Function 74)?**

1. **Flexibility:** COMPARE not a standard disk function
   - Function 74 allows custom SCSI commands
   - Can build any SCSI command block
   - Not limited to predefined functions

2. **Command Block Control:** Direct SCSI access
   - Builds exact command needed
   - Sets precise parameters
   - Full control over recovery process

3. **Compatibility:** Works with any SCSI device
   - Standard SCSI COMPARE command
   - Portable across devices
   - No device-specific code

### Recovery Limitations

**Cannot Recover:**

1. **Media Defects:** Physical damage to optical disk
   - Recovery will repeatedly fail
   - Hardware error persists
   - Requires operator intervention

2. **Device Failure:** Drive hardware malfunction
   - INQUIRY or MODE SELECT fail
   - Cannot communicate with device
   - Requires device replacement

3. **Bad Blocks:** Unwritable blocks on media
   - WRITE command fails repeatedly
   - Retry count exhausts
   - Blocks marked bad in defect list

**Retry Count:**

- Initialized from TACNS (typically 3-5 retries)
- Decremented on each recovery attempt
- When exhausted, returns error
- Prevents infinite retry loops

**Flag Persistence:**

- 4SRWO flag persists across operations
- Only cleared after successful recovery (line 207)
- Ensures recovery attempted on next operation
- Manual intervention may be needed if all retries fail

---

## Error Handling

The optical disk driver implements a sophisticated multi-level error handling system with sense data validation, retry logic, error logging, and recovery mechanisms.

### Error Sources

**1. Hardware Errors (from SCSID)**

| Error Code | Symbol | Meaning | Recovery |
|------------|--------|---------|----------|
| PFAIL | Power failure | Power supply interrupted | Retry + write recovery |
| SBRST | SCSI bus reset | Bus reset occurred | Retry + write recovery |
| LIRST | Link reset | Link interface reset | Retry + write recovery |
| Other | Various | Device-specific errors | Return error |

Handled by ERRET routine (lines 248-254).

**2. SCSI Sense Data (from device)**

| Sense Key | Value | Meaning | Handler |
|-----------|-------|---------|---------|
| NO SENSE | 0 | No error | Accept |
| RECOVERED ERROR | 1 | Corrected by device | Accept |
| NOT READY | 2 | Device not ready | Error |
| MEDIUM ERROR | 3 | Defect in media | Error |
| HARDWARE ERROR | 4 | Device malfunction | Error |
| ILLEGAL REQUEST | 5 | Invalid command | Error |
| UNIT ATTENTION | 6 | Device reset/media change | Retry |
| DATA PROTECT | 7 | Write protected | Error |
| BLANK CHECK | 8 | Unwritten blocks | Special (CCRWO) |
| VENDOR SPECIFIC | 9 | Vendor error | Error |
| COPY ABORTED | 10 | Copy failed | Error |
| ABORTED COMMAND | 11 | Command interrupted | Retry |
| EQUAL | 12 | COMPARE matched | Accept |
| VOLUME OVERFLOW | 13 | End of partition | Retry |
| MISCOMPARE | 14 | COMPARE failed | Error |
| RESERVED | 15 | Reserved code | Error |

Handled by CHSEN routine (lines 238-245) and RETOP (lines 59-107).

**3. Operational Errors (from validation)**

| Error Code | Symbol | Meaning | Detected |
|------------|--------|---------|----------|
| ILAOP | Illegal operation | Invalid function code | Line 153 |
| NOLUN | No logical unit | Unit not configured | Line 165 |
| TYPER | Type error | Invalid device type | Line 215 |
| BADPA | Bad parameter | Invalid parameter | Line 202 |

Handled by ERR1 routine (lines 181-182).

### Error Handling Flow

```
Operation Entry
    |
    v
Validate Function --> [Invalid] --> ERR1 (ILAOP) --> RETOP --> Return
    |
    v [Valid]
Validate Unit --> [Not Found] --> ERR1 (NOLUN) --> RETOP --> Return
    |
    v [Found]
Initialize Device
    |
    +--[INQUIRY fails]--> ERRET/ERRFATAL
    |
    +--[Invalid type]--> ERREX (TYPER) --> RETOP --> Return
    |
    +--[MODE SELECT fails]--> ERRET/ERRFATAL
    |
    v [Initialized]
Execute Operation (SCSID)
    |
    +--[Hardware error]--> ERRET
    |   |
    |   +--[Write op]--> Set 4SRWO --> Retry
    |   |
    |   +--[Retry OK]--> RETRY (device init)
    |   |
    |   +--[Retries exhausted]--> ERREX --> RETOP --> Return
    |
    +--[Sense data]--> CHSEN
        |
        +--[Unit Attention]--> Retry
        |
        +--[Aborted Command]--> Retry
        |
        +--[Other error]--> RETEX (error) --> RETOP --> Return
        |
        +--[Acceptable]--> Continue
```

### Error Handling Routines

#### 1. CHSEN (Lines 238-245) - Sense Validation

**Purpose:** Determine if sense data indicates recoverable error.

**Logic:**

```assembly
CHSEN: IF D:=A BZERO 17>1 THEN              % Extract sense key
          IF D=6 OR =13 THEN                % Unit attention or aborted?
             MIN X.TACOU; GO RETRY          % Retry operation
          FI
          B:=X; T:=0; GO RETEX              % Non-recoverable error
       FI
       EXIT                                 % Sense acceptable (0 or 1)
```

**Sense Categories:**

1. **Acceptable (0-1):**
   - 0: No error
   - 1: Recovered error (device corrected it)
   - Action: Return to caller, continue operation

2. **Recoverable (6, 13):**
   - 6: Unit attention (device reset, media changed)
   - 13: Aborted command (interrupted, can retry)
   - Action: Decrement retry count, restart operation

3. **Non-Recoverable (2-5, 7-12, 14-15):**
   - Media errors, hardware failures, illegal requests
   - Action: Return error to caller

**Unit Attention Handling:**

Unit Attention (sense 6) requires special handling:
- Device was reset or powered off
- Media may have changed
- Must re-initialize device
- RETRY loop performs INQUIRY and MODE SELECT again
- Retry count prevents infinite loops

**Aborted Command Handling:**

Aborted Command (sense 13) indicates:
- Command was interrupted by bus reset
- Command was preempted by another initiator
- Device received ABORT message
- Can safely retry without data loss

#### 2. ERRET (Lines 248-254) - Hardware Error Recovery

**Purpose:** Handle hardware-level failures with retry logic.

**Logic:**

```assembly
ERRET: IF PFAIL=T OR SBRST=T OR LIRST=T THEN
          D:=X; 77/\X.SLINK.ABFUN; D=:X:=A  % Get function code
          IF 1=D OR 61=D THEN                % Write operation?
             X.OPSTA BONE 4SRWO=:X.OPSTA     % Set write recovery
          FI
          MIN X.TACOU; GO FAR RETRY          % Attempt retry
       FI
```

**Hardware Failures:**

1. **PFAIL (Power Failure):**
   - Power supply dropout
   - UPS battery low
   - External power interruption
   - Action: Set write recovery if write, then retry

2. **SBRST (SCSI Bus Reset):**
   - Bus reset occurred during operation
   - Another device initiated reset
   - Bus error detected
   - Action: Set write recovery if write, then retry

3. **LIRST (Link Interface Reset):**
   - Link interface was reset
   - Communication timeout
   - Interface error
   - Action: Set write recovery if write, then retry

**Write Protection:**

Write operations (functions 1 and 61) get special handling:
- Set 4SRWO flag before retry
- Ensures write recovery on next operation
- Prevents data loss from partial writes

**Non-Write Operations:**

Read, seek, and other operations:
- No write recovery needed
- Simply retry operation
- If retries exhaust, return error

**Retry Mechanism:**

```
MIN X.TACOU             % Decrement retry counter
GO FAR RETRY            % If >= 0, jump to RETRY
[falls through]         % If < 0, retries exhausted
GO ERREX                % Return error
```

#### 3. ERREX (Lines 256-257) - Error Exit Preparation

**Purpose:** Prepare error exit with error status.

**Logic:**

```assembly
ERREX: B:=X; 20                      % Save DAQ, set error code
[falls through to RETEX]
```

**Error Code:**
- 20 (octal) = 16 (decimal)
- Indicates general error
- More specific error in sense data or SCTRG

**Used By:**
- ERRET when retries exhausted (line 254)
- Device type validation failure (line 215)
- Function 36 parameter errors (line 202)

#### 4. ERR1 (Lines 181-182) - Early Error Exit

**Purpose:** Handle errors detected before operation starts.

**Logic:**

```assembly
ERR1: CALL FAR RETOP; GO SWT11       % Terminate and return
```

**Error Codes Set Before Call:**

1. **ILAOP (Illegal Operation):**
   ```assembly
   T:=ILAOP; GO FAR ERR1              % Line 153
   ```
   - Function code not in OPTYP table
   - OPTYP(function) = 0
   - Operation not supported

2. **NOLUN (No Logical Unit Number):**
   ```assembly
   T:=NOLUN; GO FAR ERR1              % Line 165
   ```
   - PUNDF returned 0
   - Unit data field not configured
   - Device not available

**No Retry:**
- These are configuration errors
- Retry won't help
- Immediate error return

#### 5. RETOP (Lines 59-107) - Operation Termination

**Purpose:** Terminate operation and update status.

**Error Status Translation:**

```assembly
% Entry: T = driver status, A = sense data
T SHZ 11; 777/\X.ABFUN\/T=:SCTRG     % Set driver status
IF T=0 THEN                           % Sense data available?
   X:=17/\L; NEWST(X)\/X              % Translate sense to status
ELSE
   100020\/T                          % Serious error
FI
```

**Status Encoding:**

- Bits 11-17: Driver status (T shifted left 11)
- Bits 6-10: Function code (preserved)
- Bits 0-5: Error code

**NEWST Array (Lines 60-62):**

Translates SCSI sense keys to Sintran L error codes:

```assembly
INTEGER ARRAY NEWST:=
      (000000, 000000, 100020, 100020, 100020, 100020, 100020, 000020,
       000020, 100020, 100020, 100020, 000000, 100020, 000020, 100020);
```

| Sense | NEWST | Meaning |
|-------|-------|---------|
| 0 | 000000 | Success |
| 1 | 000000 | Success (recovered) |
| 2 | 100020 | Serious error (not ready) |
| 3 | 100020 | Serious error (medium) |
| 4 | 100020 | Serious error (hardware) |
| 5 | 100020 | Serious error (illegal request) |
| 6 | 100020 | Serious error (unit attention) |
| 7 | 000020 | Minor error (data protect) |
| 8 | 000020 | Minor error (blank check) |
| 9-11 | 100020 | Serious error (various) |
| 12 | 000000 | Success (equal) |
| 13 | 100020 | Serious error (overflow) |
| 14 | 000020 | Minor error (miscompare) |
| 15 | 100020 | Serious error (reserved) |

**Status Bits:**

- **100000:** Serious/permanent error
- **000020:** Minor/informational error
- **000000:** Success

**Compare Special Handling (Lines 83-87):**

```assembly
IF A=:SVXRG.HSTAT BIT 4 THEN                    % Error flag set?
   IF A/\17=5 AND 77/\X.ABFUN=3 OR=63 THEN      % Miscompare?
      0=:X.HSTAT                                % Clear error
   FI
FI
```

COMPARE operations return sense 5 (MISCOMPARE) when data doesn't match.
This is not an error - it's the expected result of a failed compare.
Driver clears error flag so caller sees success with miscompare indication.

**Error Logging (Lines 88-101):**

```assembly
IF X.HSTAT BIT 4 THEN                  % Error flag set?
   IF A<0 AND X.DQOPC BIT 3SERR THEN   % Log errors enabled?
      A:=B; CALL PHLOG; A=:9XDV        % Prepare log
      SVXRG.ABFUN=:9XFU                % Function
      A SHZ -6/\7=:9XUN                % Unit number
      X.MEMAD=:9XMA                    % Memory address
      X.ABPA2=:9XDA; X.ABP32=:9XTA     % Disk address and amount
      SCOSS=:9XST; X:=X.RTRES          % Hardware status and program
      CALL 9FLEX(9XER,12)              % Report error
   FI
FI
```

**Error Log Contents:**

- **9XER (1663):** Error number for disk errors
- **9XDV:** Device number
- **9XFU:** Function code that failed
- **9XUN:** Unit number (extracted from function)
- **9XMA:** Memory address involved
- **9XDA:** Disk address involved
- **9XTA:** Transfer amount
- **9XST:** SCSI hardware status
- **Program ID:** From X.RTRES (access owner)

**Logging Conditions:**

1. Error flag set (bit 4 of HSTAT)
2. Status negative (A < 0)
3. Error logging enabled (3SERR bit in DQOPC)

**PHLOG and 9FLEX:**

- **PHLOG:** Prepares log entry header
- **9FLEX:** Reports error to system log
- Error number 1663, 12 parameters
- Logs to console and/or log file

### Error Recovery Strategies

#### Strategy 1: Immediate Retry

**Used For:**
- Unit Attention (sense 6)
- Aborted Command (sense 13)

**Process:**
1. Decrement retry counter
2. Jump to RETRY
3. Reinitialize device if needed
4. Re-execute operation

**Example:**

```
Operation: READ 10 blocks
Event: Unit Attention (media changed)

Recovery:
1. CHSEN detects sense 6
2. Decrement retry counter (5 -> 4)
3. Jump to RETRY
4. Check 4SINI flag (already set)
5. Check 4SMSL flag (already set)
6. Re-execute SCSID
7. Operation succeeds
```

#### Strategy 2: Write Recovery

**Used For:**
- Power failures during write
- Bus resets during write
- Link resets during write

**Process:**
1. Set 4SRWO flag
2. Retry operation
3. On next operation, enter CCRWO
4. COMPARE to verify written blocks
5. Rewrite failed blocks only
6. Clear 4SRWO on success

**Example:**

```
Operation: WRITE 20 blocks at LBA 1000
Event: Power failure after 12 blocks

Recovery:
1. ERRET detects PFAIL during write
2. Set 4SRWO flag
3. Decrement retry counter
4. Jump to RETRY
5. Operation enters CCRWO
6. COMPARE 20 blocks
7. BLANK CHECK at LBA 1012
8. WRITE remaining 8 blocks
9. Success - clear 4SRWO
```

#### Strategy 3: Device Reinitialization

**Used For:**
- First operation after boot
- After unit attention
- After device reset

**Process:**
1. Check 4SINI flag
2. If not set, perform INQUIRY
3. Validate device type (3 or 4)
4. Set 4SINI flag
5. Check 4SMSL flag
6. If not set, perform MODE SELECT
7. Set 4SMSL flag
8. Continue with operation

**Example:**

```
Operation: First READ after boot
Device: Not initialized

Recovery:
1. Enter RETRY loop
2. 3SNTR not set (data operation)
3. 4SINI not set (not initialized)
4. Execute INQUIRY (function 36)
5. Validate device type = 3 (optical)
6. Set 4SINI flag
7. 4SMSL not set
8. Execute MODE SELECT
9. Set 4SMSL flag
10. Continue with READ
```

#### Strategy 4: Error Return

**Used For:**
- Non-recoverable errors
- Retries exhausted
- Configuration errors

**Process:**
1. Set error code in T
2. Set sense data in A (if available)
3. Call RETOP
4. Update status in SCTRG
5. Log error if 3SERR set
6. Return to caller

**Example:**

```
Operation: WRITE 10 blocks
Event: Media write-protected

Recovery:
1. SCSID returns sense 7 (DATA PROTECT)
2. CHSEN detects non-recoverable error
3. Jump to RETEX with T=0, A=7
4. Call RETOP
5. Translate sense 7 -> 000020 (minor error)
6. Log error (function 1, sense 7)
7. Return to caller with error
```

### Error Prevention

**Input Validation:**

1. **Function Code (Lines 149-154):**
   ```assembly
   X.ABFUN; X=:SAVEX:=77/\A            % Extract function
   OPTYP(X)                            % Look up in table
   IF A=:SAVEX.DQOPC=0 THEN            % Illegal?
      T:=ILAOP; GO FAR ERR1            % Error exit
   FI
   ```

2. **Unit Validation (Lines 164-167):**
   ```assembly
   IF X.ABFUN SHZ -6/\7>3 OR X:=PUNDF(A)=0 THEN
      X:=SAVEX; T:=NOLUN; GO FAR ERR1  % Error exit
   FI
   ```

3. **Device Type (Lines 214-216):**
   ```assembly
   IF SUTYP SHZ -10><3 AND ><4 THEN    % Type 3 or 4?
      T:=TYPER; GO FAR ERREX           % Error exit
   FI
   ```

**Retry Limits:**

- Prevents infinite loops
- Initialized from TACNS (typically 3-5)
- Decremented on each retry
- Operation fails when exhausted

**Write Protection:**

- 4SRWO flag ensures write recovery
- COMPARE verifies written data
- Only rewrites failed blocks
- Minimizes data loss

### Error Status Reporting

**Status Word Format (SCTRG):**

```
Bits 11-17: Driver status (0 = success, else error type)
Bits 6-10:  Function code (preserved from ABFUN)
Bits 0-5:   Error code (from NEWST or direct)
```

**Driver Status Values:**

- **0:** Success (from sense 0, 1, or 12)
- **100000:** Serious/permanent error
- **000020:** Minor/informational error
- **100020:** Serious error with detail

**Error Code Values:**

- **0:** No error
- **20:** General error (from ERREX)
- **Sense code:** SCSI sense key (0-15)
- **ILAOP:** Illegal operation
- **NOLUN:** No logical unit
- **TYPER:** Type error
- **BADPA:** Bad parameter

**Caller Interpretation:**

```assembly
% Check status after operation
IF SCTRG < 0 THEN
   % Error occurred
   status = SCTRG SHZ 11 /\ 177      % Extract driver status
   function = SCTRG SHZ 6 /\ 77      % Extract function
   error = SCTRG /\ 77               % Extract error code

   IF status = 100000 THEN
      % Serious/permanent error
      % Cannot retry
   ELSE IF status = 000020 THEN
      % Minor/informational error
      % May be acceptable (e.g., COMPARE mismatch)
   FI
FI
```

**Error Codes Summary:**

| Code | Name | Type | Cause |
|------|------|------|-------|
| 0 | Success | - | No error |
| ILAOP | Illegal operation | Config | Invalid function |
| NOLUN | No logical unit | Config | Unit not found |
| TYPER | Type error | Config | Wrong device type |
| BADPA | Bad parameter | Param | Invalid parameter |
| 2 | Not ready | SCSI | Device not ready |
| 3 | Medium error | SCSI | Media defect |
| 4 | Hardware error | SCSI | Device failure |
| 5 | Illegal request | SCSI | Invalid command |
| 6 | Unit attention | SCSI | Device reset |
| 7 | Data protect | SCSI | Write protected |
| 8 | Blank check | SCSI | Unwritten blocks |
| 13 | Aborted command | SCSI | Command interrupted |
| 14 | Miscompare | SCSI | COMPARE failed |

---

## Differences from Magnetic Disk Driver

Comparing `IP-P2-SCSI-OPDI.NPL` (optical) with `IP-P2-SCSI-DISK.NPL` (magnetic) reveals significant differences due to the unique characteristics of optical media.

### 1. Device Initialization

**Optical (Lines 209-223):**

```assembly
IF X.SCOCW NBIT 3SNTR THEN              % Not neutral operation?
   IF X.OPSTA NBIT 4SINI THEN           % Not initialized?
      36=:X.ABFUN; T:=X                 % INQUIRY
      CALL SCSID; GO FAR ERRET; CALL ERRFATAL
      IF A><0 THEN CALL CHSEN FI
      IF SUTYP SHZ -10><3 AND ><4 THEN  % Device type 3 or 4 only
         T:=TYPER; GO FAR ERREX
      FI
      X.OPSTA BONE 4SINI=:X.OPSTA
   FI
   IF X.OPSTA NBIT 4SMSL THEN           % Mode select needed?
      CALL FAR MDSEL; GO FAR ERRET; CALL ERRFATAL
      IF A><0 THEN CALL CHSEN FI
      X.OPSTA BONE 4SMSL=:X.OPSTA
   FI
FI
```

**Magnetic:**
- No forced MODE SELECT
- Supports device types 0, 1, 3, 4
- More flexible initialization
- MODE SELECT optional

**Reason:**
- Optical disks require specific mode parameters
- Block size must be configured
- Write parameters differ from magnetic
- More critical to set device state

### 2. Write Recovery (4SRWO Flag)

**Optical (Lines 226, 301-329):**

```assembly
IF X.OPSTA BIT 4SRWO GO FAR CCRWO      % Write recovery

CCRWO: [... COMPARE and selective rewrite logic ...]
```

**Magnetic:**
- No 4SRWO flag
- No write recovery mechanism
- Simply retries entire operation
- Assumes write is atomic or doesn't matter

**Reason:**
- Optical writes are much slower
- WORM media cannot be rewritten
- Partial write success is valuable
- Power failures more likely during long writes
- COMPARE verifies what was actually written

**Impact:**

| Aspect | Optical | Magnetic |
|--------|---------|----------|
| Write time | 10-100x slower | Fast |
| Recovery | Block-level COMPARE/rewrite | Retry entire operation |
| Data loss | Minimized (only failed blocks) | Entire transfer lost |
| Complexity | High (CCRWO routine) | Low (simple retry) |

### 3. SEEK Operation

**Optical OPTYP(4):**
```assembly
110004  % 3SERR, 3SPES (special, not sorted)
```

**Magnetic OPTYP(4):**
```assembly
000004  % Sortable operation
```

**Reason:**
- Optical seeks are very slow (100-500ms)
- Sorting doesn't help much
- Better to execute seeks immediately
- Magnetic seeks are fast (5-15ms), benefit from sorting

**Impact:**

Lines 173-178 in optical driver:

```assembly
IF L NBIT 3SPES THEN
   X.TYPCO BONE SSEEK=:X.TYPCO       % Sort possible
FI
```

SEEK operations have 3SPES set, so they skip sorting and execute immediately.

### 4. MODE SELECT Implementation

**Optical MDSEL (Lines 114-118):**

```assembly
MDSEL: IF SUTYP SHZ -10=3 THEN
          1                          % Optical-specific parameters
       ELSE
          "0"                        % Default
       FI
```

**Reason:**
- Type 3 devices (optical) need special MODE SELECT data
- Configures block size, write parameters, error recovery
- Different from magnetic disk parameters

**MODE SELECT Buffer:**

Optical disks require configuration of:
- Block size (512, 1024, or 2048 bytes)
- Write verification mode
- Error recovery parameters
- Cache control (usually disabled for optical)

Magnetic disks:
- Often work with defaults
- MODE SELECT less critical
- More standard configurations

### 5. Function 42 (READ FORMAT)

**Optical TER42 (Lines 283-291):**

```assembly
IF SURSZ=4000 THEN
   41                                % 2KB sector layout
ELSE IF A=2000 THEN
   40                                % 1KB sector layout
ELSE
   -1                                % No standard index
FI FI
```

**Capacity Calculation (Lines 292-294):**

```assembly
SURSZ=:L:=44000=:D:=17; *RDIV SL     % Reserve 1MB for test
A+1=:L; *LDDTX 10
D-L; *RADD 0 CM1 ADC DA; STDTX 10    % Return available blocks
```

**Magnetic:**
- Different layout indices
- Different capacity calculations
- No test area reservation
- Simpler format information

**Reason:**
- Optical disks often have non-standard layouts
- May include defect management areas
- Need reserved space for testing/calibration
- Format information more complex

### 6. Device Type Validation

**Optical (Lines 214-216):**

```assembly
IF SUTYP SHZ -10><3 AND ><4 THEN     % Type 3 or 4 only
   T:=TYPER; GO FAR ERREX
FI
```

**Magnetic:**
- Accepts types 0, 1, 3, 4
- More flexible
- Can handle direct-access and optical

**Device Types:**

| Type | Description | Optical Support | Magnetic Support |
|------|-------------|-----------------|------------------|
| 0 | Direct-access (magnetic) | NO | YES |
| 1 | Sequential-access (tape) | NO | YES (if configured) |
| 3 | Optical disk | YES | YES |
| 4 | WORM optical | YES | YES |
| 5+ | Other | NO | NO |

**Reason:**
- Optical driver is specialized
- Only handles optical media
- Magnetic driver is more general-purpose

### 7. Error Handling

**NEWST Array Differences:**

**Optical (Lines 60-62):**
```assembly
INTEGER ARRAY NEWST:=
      (000000, 000000, 100020, 100020, 100020, 100020, 100020, 000020,
       000020, 100020, 100020, 100020, 000000, 100020, 000020, 100020);
```

**Key Differences:**

| Sense | Optical NEWST | Magnetic NEWST | Reason |
|-------|---------------|----------------|--------|
| 8 (BLANK CHECK) | 000020 (minor) | 100020 (serious) | Optical: expected during write recovery |
| 14 (MISCOMPARE) | 000020 (minor) | 100020 (serious) | Optical: used for verification |

**Reason:**
- Optical driver uses BLANK CHECK in CCRWO recovery
- Not a serious error, indicates unwritten blocks
- MISCOMPARE used for data verification
- Magnetic driver treats these as serious errors

### 8. Retry Strategy

**Optical:**
- Hardware errors trigger write recovery (4SRWO)
- COMPARE verifies written data
- Rewrites only failed blocks
- More sophisticated retry logic

**Magnetic:**
- Hardware errors just retry entire operation
- No verification step
- Assumes write is fast enough to retry completely
- Simpler retry logic

**Example:**

**Optical Write Recovery:**
```
1. WRITE 100 blocks
2. Power failure after 75 blocks
3. Set 4SRWO flag
4. COMPARE 100 blocks
5. BLANK CHECK at block 75
6. WRITE remaining 25 blocks
7. Success - 75 blocks preserved
```

**Magnetic Write Retry:**
```
1. WRITE 100 blocks
2. Power failure
3. Retry: WRITE 100 blocks again
4. All 100 blocks rewritten
```

### 9. Performance Optimizations

**Optical:**
- No disk sorting for SEEK (3SPES flag)
- Write recovery to avoid rewrites
- MODE SELECT to optimize parameters
- Cache typically disabled

**Magnetic:**
- Disk sorting for all operations
- Elevator algorithm
- Cache enabled for performance
- Minimal initialization

**Reason:**

| Characteristic | Optical | Magnetic | Impact |
|----------------|---------|----------|--------|
| Seek time | 100-500ms | 5-15ms | Sorting less effective |
| Write time | 10-100x slower | Fast | Recovery more important |
| Rotation speed | 1500-3000 RPM | 5400-15000 RPM | Lower throughput |
| Cache | Usually disabled | Usually enabled | Less benefit |

### 10. Operation Type Definitions

**Functions Implemented:**

| Function | Optical | Magnetic | Notes |
|----------|---------|----------|-------|
| 0-3 (READ/WRITE/PARITY/COMPARE) | YES | YES | Both support |
| 4 (SEEK) | Special (3SPES) | Sortable | Different handling |
| 34-35 (RESERVE/RELEASE) | YES | YES | Both support |
| 37 (READ SENSE) | YES | YES | Both support |
| 42 (READ FORMAT) | Special | Standard | Different format info |
| 60-63 (Double address) | Partial | Full | Optical: 60-61 only |
| 73 (TEST UNIT READY) | YES | YES | Both support |
| 74 (User command) | YES (for CCRWO) | Limited | Optical uses heavily |
| 75 (INQUIRY) | YES | YES | Both support |

**OPTYP Differences:**

```
Function  Optical    Magnetic   Difference
--------  --------   --------   ----------
4         110004     000004     Optical: special, not sorted
28-29     104004     various    Different parameters
60        074004     074004     Same
61        134002     074004     Optical: special handling
62-63     000000     074004     Optical: not implemented
```

### 11. Memory and Buffer Handling

**Optical:**
- Uses command buffer for CCRWO (CMAD1:CMAD2)
- Builds SCSI command blocks manually
- Function 74 (user command) for COMPARE and partial WRITE

**Magnetic:**
- Simpler buffer handling
- Standard SCSI commands only
- No manual command block construction

**CCRWO Command Buffer (Lines 301-327):**

```assembly
T:=X.CMAD1=:X.ABP21                  % Command buffer address
X.CMAD2+2=:X.ABP22=:X
27402; *STATX 00                     % COMPARE command
[... build complete SCSI CDB ...]
700/\X.SLINK.ABFUN\/74=:SAVX.ABFUN   % Function 74
CALL SCSID                           % Execute
```

**Reason:**
- Optical driver needs COMPARE command
- Not a standard disk function
- Must use Function 74 (user-specified command)
- Requires manual command block construction

### 12. Status and Flag Management

**Optical Flags:**
- 4SINI: Initialization performed
- 4SMSL: Mode select performed
- 4SRWO: Write recovery in progress

**Magnetic Flags:**
- 5SCIN: Initialization performed
- 5SCDA: Direct-access device
- 5SFBM: Fixed block mode
- 5SVBS: Variable block size

**Reason:**
- Optical has fewer device variations
- Write recovery is unique to optical
- Magnetic needs more device classification

### 13. Code Size and Complexity

**Optical Driver:**
- Main routine: ~334 lines
- CCRWO routine: ~30 lines
- MDSEL routine: ~15 lines
- TER42 routine: ~18 lines
- Total: ~400 lines

**Magnetic Driver:**
- Main routine: ~600+ lines
- More initialization code
- More device type handling
- Partition management
- Total: ~800+ lines

**Reason:**
- Magnetic driver more general-purpose
- Supports more device types
- More complex partitioning
- Optical driver specialized but has CCRWO

### Summary Table

| Feature | Optical | Magnetic | Why Different |
|---------|---------|----------|---------------|
| **Device Types** | 3, 4 only | 0, 1, 3, 4 | Specialized vs general |
| **MODE SELECT** | Required | Optional | Optical needs configuration |
| **Write Recovery** | COMPARE + partial rewrite | Retry entire operation | Optical writes slow/WORM |
| **SEEK Sorting** | Disabled (3SPES) | Enabled | Optical seeks very slow |
| **Error Recovery** | Sophisticated (4SRWO) | Simple retry | Write reliability critical |
| **Function 74** | Used heavily | Rarely used | COMPARE not standard |
| **BLANK CHECK** | Minor error (000020) | Serious error (100020) | Used in recovery |
| **Initialization** | Strict sequence | Flexible | Optical more sensitive |
| **Format Info (42)** | Reserved test area | Standard calculation | Optical needs calibration |
| **Retry Strategy** | Block-level recovery | Operation-level retry | Preserve partial success |

---

## Optical-Specific Features Summary

### 1. Write-Once/Rewritable Support

**WORM Detection (Device Type 4):**
- Validated during initialization
- Affects MODE SELECT parameters
- Write recovery crucial for WORM media

**Characteristics:**
- Cannot overwrite blocks on WORM
- Must verify what was written
- Partial write success valuable
- COMPARE essential for verification

### 2. Long Access Times

**Seek Times:**
- 100-500ms typical
- Much slower than magnetic (5-15ms)
- Sorting not effective
- Immediate execution better

**Write Times:**
- 10-100x slower than magnetic
- Makes write recovery worthwhile
- Power failures more likely
- Verification important

**Read Times:**
- 2-5x slower than magnetic
- Still relatively fast
- Caching less effective
- Sequential access preferred

### 3. Block Verification

**COMPARE Command:**
- Used in CCRWO routine
- Verifies written data
- Returns failing block address
- Essential for write recovery

**BLANK CHECK:**
- Indicates unwritten blocks
- Returns via SCSI sense
- Used to identify failed writes
- Triggers partial rewrite

### 4. Mode Parameters

**MODE SELECT Data:**
- Block size (512, 1024, 2048 bytes)
- Write verification mode
- Error recovery parameters
- Cache control

**Configuration:**
- Type 3 devices get special parameters
- Required before first operation
- Persistent across operations
- Affects performance and reliability

### 5. Capacity Management

**Reserved Areas:**
- 1MB reserved for testing (function 42)
- Calibration space
- Defect management
- Not available to users

**Layout Indices:**
- 40: 1KB sector layout
- 41: 2KB sector layout
- -1: Non-standard

### 6. Error Recovery

**Multi-Level Strategy:**
1. Immediate retry (unit attention, aborted)
2. Write recovery (power failure, bus reset)
3. Device reinitialization (first use, reset)
4. Error return (non-recoverable)

**Sense Handling:**
- More tolerant of BLANK CHECK
- MISCOMPARE used for verification
- Unit attention triggers reinit
- Aborted command always retried

### 7. Performance Characteristics

**Throughput:**
- Read: 1-6 MB/s
- Write: 0.5-2 MB/s
- Seek: 2-10 ops/s

**Latency:**
- Rotation: 20-40ms (1500-3000 RPM)
- Seek: 100-500ms
- Command: 1-5ms

**Optimization:**
- Sequential access preferred
- Avoid seeks
- Large transfers better
- Write recovery amortizes cost

---

## Code Structure Summary

### Global Variables

| Variable | Type | Purpose | Scope |
|----------|------|---------|-------|
| OPSTA | INTEGER | Device status flags (4SINI, 4SMSL, 4SRWO) | Device data field (HDEV) |
| NEWST | INTEGER ARRAY[16] | Sense to error code translation | Static data |
| OPTYP | INTEGER ARRAY[64] | Operation control words | Static data |
| SVTAD | TRIPLE | Saved TAD register | RETOP local |
| SVTRG | INTEGER | Saved T register | RETOP local |
| SVXRG | INTEGER | Saved X register | RETOP local |
| SVLRG | INTEGER POINTER | Saved L register | RETOP local |
| 9XER-9XPR | Various | Error logging parameters | RETOP local |
| MSXRG | INTEGER | Saved X register | MDSEL local |
| SAVL | INTEGER POINTER | Saved L register | TER42 local |
| SAVA | INTEGER | Saved A register | TER42 local |
| SAVX | INTEGER | Saved X register | TER42 & CCRWO local |
| SAVEX | INTEGER | Saved X register | SCOPTICAL local |

### Subroutine Call Graph

```
SCOPTICAL (main entry)
  |
  +-- DIALO (monitor performance)
  |
  +-- PUNDF (get unit data field)
  |
  +-- DSORT (sort operation in queue)
  |   |
  |   +-- SWT11 (return to scheduler)
  |
  +-- NEWOP (start new operation)
      |
      +-- SCSID (execute SCSI command)
      |   |
      |   +-- ERRET (hardware error handler)
      |   |   |
      |   |   +-- RETRY (attempt retry)
      |   |       |
      |   |       +-- SCSID (INQUIRY)
      |   |       |   |
      |   |       |   +-- ERRET / ERRFATAL
      |   |       |   +-- CHSEN (check sense)
      |   |       |
      |   |       +-- MDSEL (mode select)
      |   |       |   |
      |   |       |   +-- SCSID (MODE SELECT)
      |   |       |       |
      |   |       |       +-- ERRET / ERRFATAL
      |   |       |
      |   |       +-- CCRWO (write recovery)
      |   |           |
      |   |           +-- SCSID (COMPARE)
      |   |           |   |
      |   |           |   +-- ERRET / ERRFATAL
      |   |           |
      |   |           +-- SCSID (WRITE)
      |   |               |
      |   |               +-- ERRET / ERRFATAL
      |   |
      |   +-- CHSEN (check sense)
      |       |
      |       +-- RETRY (if recoverable)
      |       +-- RETEX (if non-recoverable)
      |
      +-- TER42 (function 42 post-processing)
      |
      +-- RETEX (success return)
          |
          +-- RETOP (terminate operation)
          |   |
          |   +-- PHLOG (prepare log)
          |   +-- 9FLEX (report error)
          |   +-- TO11Q (return to level 11)
          |
          +-- NEWOP (if queue not empty)
          |
          +-- SWT11 (if queue empty)
```

### Entry Points

| Label | Line | Purpose | Called By |
|-------|------|---------|-----------|
| SCOPTICAL | 143 | Main entry point | System dispatcher |
| RETOP | 59 | Terminate operation | Multiple (via FAR call) |
| MDSEL | 114 | Execute MODE SELECT | NEWOP (via FAR call) |
| TER42 | 282 | Function 42 post-processing | NEWOP (via call) |
| CCRWO | 301 | Write recovery | RETRY (via FAR jump) |
| CHSEN | 238 | Check sense data | Multiple (via call) |
| ERRET | 248 | Handle retryable errors | SCSID failure path |
| ERREX | 256 | Prepare error exit | Multiple |
| RETEX | 258 | Return with status | Multiple |
| ERR1 | 181 | Early error exit | Validation failures |
| NEWOP | 184 | Start new operation | SCOPTICAL, RETEX |
| RETRY | 209 | Retry operation | ERRET, CHSEN |

### External References

| Symbol | Type | Purpose | Defined In |
|--------|------|---------|------------|
| SCSID | Subroutine | Execute SCSI command | SCSI driver |
| DSORT | Subroutine | Disk sort operation | Disk manager |
| TO11Q | Subroutine | Return to level 11 queue | Scheduler |
| PUNDF | Function | Get unit data field | Device manager |
| PHLOG | Subroutine | Prepare log entry | Log manager |
| 9FLEX | Subroutine | Report error | Error manager |
| DIALO | Subroutine | Monitor performance | Performance monitor |
| ERRFATAL | Label | Fatal error handler | Error subsystem |
| SWT11 | Label | Level 11 switch | Scheduler |

---

## Usage Examples

### Example 1: Simple READ Operation

```assembly
% Setup read operation
X:=DAQ                          % Get disk activity queue
3=:X.ABFUN                      % Function 0 = READ
1000=:X.ABPA2                   % Disk address = 1000
10=:X.ABP32                     % Amount = 10 blocks
buffer_addr=:X.MEMAD            % Memory address

% Call optical driver
CALL SCOPTICAL

% Check result
IF X.SCTRG < 0 THEN
   % Error occurred
   error_code = X.SCTRG /\ 77
   handle_error(error_code)
ELSE
   % Success
   data_in_buffer()
FI
```

**Driver Flow:**
1. Validates function 0 (OPTYP = 100004)
2. Gets unit data field
3. Sorts into queue (if possible)
4. Checks initialization (4SINI, 4SMSL)
5. Executes SCSID with READ command
6. Returns success

### Example 2: WRITE with Power Failure

```assembly
% Setup write operation
X:=DAQ
1=:X.ABFUN                      % Function 1 = WRITE
2000=:X.ABPA2                   % Disk address = 2000
20=:X.ABP32                     % Amount = 20 blocks
data_buffer=:X.MEMAD

% Call optical driver
CALL SCOPTICAL

% [Power failure occurs after 15 blocks written]

% Next operation (automatic recovery)
X:=DAQ
0=:X.ABFUN                      % Function 0 = READ (any operation)
5000=:X.ABPA2
1=:X.ABP32
temp_buffer=:X.MEMAD

% Call optical driver
CALL SCOPTICAL
```

**Driver Flow:**
1. First call: Write starts
2. Power failure after 15 blocks
3. ERRET detects PFAIL
4. Sets 4SRWO flag
5. Decrements retry, jumps to RETRY
6. Next operation checks 4SRWO
7. CCRWO: COMPARE 20 blocks at 2000
8. BLANK CHECK at block 2015
9. WRITE remaining 5 blocks (2015-2019)
10. Success, clears 4SRWO
11. Continue with READ operation

### Example 3: Device Initialization

```assembly
% First operation after boot
X:=DAQ
0=:X.ABFUN                      % Function 0 = READ
100=:X.ABPA2
1=:X.ABP32
buffer=:X.MEMAD

% Call optical driver (device not initialized)
CALL SCOPTICAL
```

**Driver Flow:**
1. Validates function
2. Gets unit data field
3. Enters RETRY loop
4. 4SINI not set
5. Executes INQUIRY (function 36)
6. Validates device type = 3 or 4
7. Sets 4SINI flag
8. 4SMSL not set
9. Calls MDSEL
10. Executes MODE SELECT
11. Sets 4SMSL flag
12. Executes READ command
13. Returns success

### Example 4: Function 42 (READ FORMAT)

```assembly
% Get disk format information
X:=DAQ
42=:X.ABFUN                     % Function 42 = READ FORMAT
format_buffer=:X.MEMAD          % Buffer for format data
X.MEMA1 = format_buffer SHZ -16 % High word
X.MEMA2 = format_buffer /\ 177777 % Low word

% Call optical driver
CALL SCOPTICAL

% Check results in buffer
% Offset 00: Disk layout index (40 or 41)
% Offset 10: Available blocks (double word)
```

**Driver Flow:**
1. Validates function 42
2. Special handling (lines 189-204)
3. Sets function to READ
4. Sets 100 byte transfer
5. Executes SCSID
6. Calls TER42 for post-processing
7. TER42 calculates layout index
8. TER42 calculates available capacity
9. Returns format information

---

## Complete API Reference

### SCOPTICAL Main Entry

**Syntax:**
```assembly
CALL SCOPTICAL
```

**Entry Parameters:**

| Register/Field | Description | Valid Values |
|----------------|-------------|--------------|
| X | DAQ pointer | Valid disk activity queue |
| X.NFUNC | Return address | Caller's return point |
| X.ABFUN | Function code | 0-75 (see function table) |
| X.MEMAD | Memory address | Physical memory address |
| X.ABPA2 | Disk address | Block number (single or double) |
| X.ABP21 | Disk address high | For double address functions |
| X.ABP32 | Transfer amount | Block count |
| X.ABP31 | Transfer amount high | For double amount functions |

**Exit Parameters:**

| Register/Field | Description | Values |
|----------------|-------------|--------|
| T | Status code | 0 = success, non-zero = error |
| A | Sense data | SCSI sense key (if T=0) |
| X | DAQ pointer | Updated with results |
| X.SCTRG | Driver status | Composite status word |
| X.HSTAT | Error information | Error flags and sense |
| X.XSTAT | Extended status | Additional error info |

**Status Codes:**

| T Value | Meaning | Recovery |
|---------|---------|----------|
| 0 | Success or sense data available | Check A register |
| ILAOP | Illegal operation | Fix function code |
| NOLUN | No logical unit | Configure device |
| TYPER | Type error | Use correct device |
| BADPA | Bad parameter | Fix parameters |
| PFAIL | Power failure | Automatic retry |
| SBRST | SCSI bus reset | Automatic retry |
| LIRST | Link reset | Automatic retry |

**Functions Supported:**

| Code | Name | Parameters | Description |
|------|------|------------|-------------|
| 0 | READ | MEMAD, ABPA2, ABP32 | Read blocks from disk |
| 1 | WRITE | MEMAD, ABPA2, ABP32 | Write blocks to disk |
| 2 | READ PARITY | MEMAD, ABPA2, ABP32 | Read with parity check |
| 3 | COMPARE | MEMAD, ABPA2, ABP32 | Compare disk to memory |
| 4 | SEEK | ABPA2 | Position head |
| 34 | RESERVE DEVICE | - | Reserve for exclusive use |
| 35 | RELEASE DEVICE | - | Release reservation |
| 37 | READ EXTENDED STATUS | MEMAD | Get sense data |
| 42 | READ FORMAT | MEMA1:MEMA2 | Get format info |
| 60 | READ (double) | MEMAD, ABP21:ABPA2, ABP32 | Read large disks |
| 61 | WRITE (double) | MEMAD, ABP21:ABPA2, ABP32 | Write large disks |
| 62 | READ PARITY (double) | MEMAD, ABP21:ABPA2, ABP32 | Read parity large disks |
| 63 | COMPARE (double) | MEMAD, ABP21:ABPA2, ABP32 | Compare large disks |
| 73 | TEST UNIT READY | - | Check if ready |
| 74 | USER COMMAND | MEMAD, command block | Execute custom command |
| 75 | INQUIRY | MEMAD | Get device info |

**Example Usage:**

```assembly
% Read 10 blocks starting at block 1000
X:=my_daq
0=:X.ABFUN              % READ
1000=:X.ABPA2           % Block address
10=:X.ABP32             % Block count
buffer=:X.MEMAD         % Memory address
CALL SCOPTICAL
IF T<>0 THEN
   % Handle error
FI
```

### RETOP Termination

**Syntax:**
```assembly
CALL FAR RETOP
```

**Entry Parameters:**

| Register | Description | Values |
|----------|-------------|--------|
| B | Disk data field | Unit data field pointer |
| X | DAQ pointer | Current operation |
| T | Driver status | 0 or error code |
| A | Sense data | SCSI sense key (if T=0) |

**Exit Parameters:**

| Register/Field | Description |
|----------------|-------------|
| All registers | Restored from saved values |
| X.SCTRG | Updated with final status |
| X.HSTAT | Updated with error info |
| System log | Error logged (if 3SERR set) |

**Internal Operation:**
1. Saves all registers
2. Translates status to standard format
3. Handles COMPARE special case
4. Logs error if required
5. Returns to level 11 queue
6. Restores registers

**Note:** Usually called internally, not by application code.

### MDSEL Mode Select

**Syntax:**
```assembly
CALL FAR MDSEL
```

**Entry Parameters:**

| Register/Field | Description |
|----------------|-------------|
| X | DAQ pointer |
| X.CMAD1:CMAD2 | Command buffer physical address |
| X.ABFUN | Current function (bits 9-11 preserved) |
| SUTYP | Device subtype |

**Exit Parameters:**

| Register | Description |
|----------|-------------|
| T | Status (0 = success) |
| A | Sense data (if error) |

**Buffer Format:**

```
Offset 0:  Sense data header
Offset 10: Mode parameter (device type dependent)
```

**Device Type Handling:**
- Type 3 (optical): Special parameters (returns 1)
- Other types: Default parameters (returns 0)

**Example:**

```assembly
% Mode select is typically called automatically during initialization
% Manual call example:
X:=daq
mode_buffer=:X.CMAD1:X.CMAD2
CALL FAR MDSEL
IF A<>0 THEN
   % Handle sense data
FI
```

### TER42 Format Post-Processing

**Syntax:**
```assembly
CALL TER42
```

**Entry Parameters:**

| Register/Field | Description |
|----------------|-------------|
| X | DAQ pointer |
| SURSZ | Sector/record size |
| X.MEMA1:MEMA2 | User buffer address |

**Exit Parameters:**

User buffer updated:

| Offset | Size | Description |
|--------|------|-------------|
| 00 | Word | Disk layout index (40, 41, or -1) |
| 10 | Double | Available disk blocks |

**Layout Indices:**
- 40: 1KB (1024 byte) sectors
- 41: 2KB (2048 byte) sectors
- -1: Non-standard size

**Capacity Calculation:**
```
Reserved = (SURSZ * 18432) / 15  % Approximately 1MB
Available = Total - Reserved - 1
```

**Example:**

```assembly
% Called automatically after Function 42
% Results accessed from buffer:
format_buffer[0] = layout_index
format_buffer[10:20] = available_blocks
```

### CCRWO Write Recovery

**Syntax:**
```assembly
GO FAR CCRWO
```

**Entry Parameters:**

| Register/Field | Description |
|----------------|-------------|
| X | DAQ pointer (SAVX) |
| X.SLINK | Original write operation |
| X.CMAD1:CMAD2 | Command buffer address |
| X.OPSTA | 4SRWO flag set |

**Exit Parameters:**

| Register | Description |
|----------|-------------|
| T | Status (0 = success) |
| A | Sense data (if error) |
| 4SRWO | Cleared on success |

**Recovery Process:**
1. Build COMPARE command
2. Execute COMPARE
3. If BLANK CHECK:
   - Build WRITE command for failed blocks
   - Execute WRITE
4. Return via RETEX

**Example:**

```assembly
% Automatic recovery after write failure
% Not called directly by application
% Triggered by 4SRWO flag in OPSTA
```

### CHSEN Sense Validation

**Syntax:**
```assembly
CALL CHSEN
```

**Entry Parameters:**

| Register | Description |
|----------|-------------|
| A | Sense data from SCSI |
| X | DAQ pointer |
| B | Disk data field |

**Exit Parameters:**

- **Returns:** If sense acceptable (0 or 1)
- **Jumps to RETRY:** If sense 6 or 13
- **Jumps to RETEX:** If non-recoverable error

**Sense Handling:**

| Sense | Action | Reason |
|-------|--------|--------|
| 0 | Return | No error |
| 1 | Return | Recovered error |
| 2-5 | RETEX | Non-recoverable |
| 6 | RETRY | Unit attention |
| 7-12 | RETEX | Non-recoverable |
| 13 | RETRY | Aborted command |
| 14-15 | RETEX | Non-recoverable |

**Example:**

```assembly
% Called automatically after SCSID
IF A><0 THEN CALL CHSEN FI
% Returns if OK, retries if recoverable, errors if not
```

---

## Conclusion

The `IP-P2-SCSI-OPDI.NPL` optical disk driver is a sophisticated piece of system software that addresses the unique challenges of optical storage media:

**Key Features:**
1. **Write Recovery (4SRWO):** Protects against data loss from power failures using block-level COMPARE and selective rewrite
2. **Device Initialization:** Strict INQUIRY and MODE SELECT sequence ensures proper optical disk configuration
3. **Error Handling:** Multi-level strategy with sense validation, retry logic, and comprehensive logging
4. **Performance Optimization:** Disables seek sorting due to slow optical seeks, executes operations immediately
5. **WORM Support:** Device type validation and write recovery designed for write-once media

**Critical Differences from Magnetic:**
- Write recovery mechanism (CCRWO) is unique to optical
- MODE SELECT required for all operations
- Device type restricted to optical only (types 3-4)
- SEEK operations not sorted (3SPES flag)
- Error handling more tolerant of BLANK CHECK and MISCOMPARE

**Reliability Features:**
- Retry counter prevents infinite loops
- Write recovery preserves partial success
- Unit attention triggers reinitialization
- Comprehensive error logging for diagnostics
- Status flags track device state

This driver exemplifies the engineering required to support optical storage in a mainframe/minicomputer environment, balancing performance, reliability, and the unique characteristics of optical media.

---

**Document generated from:** `Z:\NorskData\Source Code\Sintran L\NPL\IP-P2-SCSI-OPDI.NPL`

**Analysis date:** 2025-10-13

**Driver version:** Based on source code timestamp 065365
