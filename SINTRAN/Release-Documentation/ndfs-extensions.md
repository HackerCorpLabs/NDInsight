# NDFS (Norsk Data File System) Extensions - Release History

## Overview

This document details all NDFS file system changes introduced across SINTRAN III versions J through N, as documented in the official release information manuals. It is organized by topic/structure rather than by version to serve as a reference for filesystem implementation.

**Sources:**
- ND-60.230.01 - SINTRAN III J-version Release Information (January 1985)
- ND-60.230.5 EN - SINTRAN III Release Information, K-version
- ND-860230.6 EN - SINTRAN III Release Information, L-Version
- ND-860230.7A EN - SINTRAN III Release Information, M-Version
- ND-860230.8 EN - SINTRAN III Release Information, N-version (February 1993)

---

## 1. Directory Entry On Disk

### 1.1 Extension from 20₈ to 30₈ Words (K-version)

The directory entry stored on disk page 0 was extended from 20₈ (16 decimal) to 30₈ (24 decimal) words. The new extended part occupies words 1750₈-1757₈ on page 0 of the directory.

**Source:** K-version, Chapter 7.10, Page 136

#### Layout (displacements within page 0, octal):

| Offset | Content |
|--------|---------|
| 1750₈ | Checksum of words 1751₈-1757₈ |
| 1751₈ | Reserved for future use |
| 1752₈ | Reserved for future use |
| 1753₈ | Reserved for future use |
| 1754₈ | Flag word (bit 17₈ set = entered) |
| 1755₈ | System number last entering |
| 1756₈ | Number of pages available (most significant part) |
| 1757₈ | Number of pages available (least significant part) |
| 1760₈ | Directory name (16 characters = 8 words) |
| 1770₈ | Object file index pointer (double word) |
| 1772₈ | User file index pointer (double word) |
| 1774₈ | Bit file index pointer (double word) |
| 1776₈ | Number of pages not reserved (double word) |

The old directory entry only used the 20₈ words starting at 1760₈. If the checksum in word 1750₈ is incorrect, the directory entry is assumed to be of the old format.

**Source:** K-version, Page 136, Lines 7326-7349

### 1.2 @ENTER-DIRECTORY Stores System Number (K-version)

The @ENTER-DIRECTORY command now stores the system number of the system entering a directory in the directory entry (on disk). This feature is used in FTX systems but may cause problems if a directory on a removable disk is moved to a new system without being released by @RELEASE-DIRECTORY. The command @UNLOCK-DIRECTORY may be used in such cases.

**Source:** K-version, Lines 1584-1586

### 1.3 Extended Part Access via Negative Displacements (N-version)

The @CHANGE-DIRECTORY-ENTRY command was extended to access the extended part (first 10₈ words at addresses 1750₈-1757₈) using negative displacements:

| Displacement | Content | Master Block Word |
|-------------|---------|-------------------|
| -1 | Number of pages available, least significant part | 1757₈ |
| -2 | Number of pages available, most significant part | 1756₈ |
| -3 | System number last entering | 1755₈ |
| -4 | Flag word | 1754₈ |
| -5 | Reserved | 1753₈ |
| -6 | Reserved | 1752₈ |
| -7 | Reserved | 1751₈ |
| -10₈ | Checksum | 1750₈ |

**Source:** N-version, Page 20, Lines 873-883

---

## 2. Object File System - Extended File Capacity (K-version)

### 2.1 Overview

The file system was extended to support more than 256 files per user. Each user can now have up to 4096 files through a sub-indexed object file structure.

**Source:** K-version, Chapter 7, Pages 130-136

### 2.2 Object File Sub-Index Structure

All files belonging to a user are divided into blocks of 256 objects. When creating file number 257, 513, etc., a new index block is allocated for that user. The maximum number of index blocks per user is 16 (16 × 256 = 4096 files max).

The file system automatically establishes a sub-indexed structure when:
- User 64 is created, OR
- The first object with index exceeding 255 is created by any user

**Source:** K-version, Page 131, Lines 7186-7192

#### Sub-Index Block Layout:

| Block | Contents |
|-------|----------|
| Object file subindex block | Points to index blocks 0-63 |
| Index block 0 | Object entries 0-31 for User 0 |
| Index block 1 | Object entries 224-255 for User 63 |
| Index block 2 | Object entries 0-31 for User 64 |
| Index block 3 | Object entries 224-255 for User 255 |
| Index block 4 | Object entries 256-287 for User 0 |
| ... | ... |
| Index block 7 | Object entries 480-511 for User 255 |
| ... | ... |
| Index block 63 | Object entries 4064-4095 for Users 192-255 |

**Source:** K-version, Section 7.4, Page 132, Lines 7204-7217

### 2.3 User File Entry - MXOBL and ACOBL

The user entry at displacement 27₈ now contains two 4-bit fields:

| Offset | Bits | Field | Description |
|--------|------|-------|-------------|
| 27₈ | 7-4 | MXOBL | Maximum number of extra object blocks allowed |
| 27₈ | 3-0 | ACOBL | Actual number of extra object blocks in use |

Full user entry layout:

| Offset | Content |
|--------|---------|
| 0 | U flag, Enter count |
| 1 | User name (8 words) |
| 11₈ | Password |
| 12₈ | Date created (double word) |
| 14₈ | Last date entered (double word) |
| 16₈ | No of pages reserved (double word) |
| 20₈ | No of pages used (double word) |
| 22₈ | User index |
| 23₈ | (unused) |
| 24₈ | Default file access |
| 25₈ | Previous user entry |
| 26₈ | Next user entry |
| 27₈ | ₇MXOBL₄ ₃ACOBL₀ (UXOBL - **New in K-version**) |
| 30₈ | Friend table |

If MXOBL (and ACOBL) is zero, object file extension is not allowed and the file system works exactly as in previous versions.

**Source:** K-version, Section 7.5, Page 133, Lines 7231-7252

### 2.4 Object File Entry - OBJBL Field

The object file entry must comprise the object block numbers of the current, next, and previous versions of the file. Vacant bits in OFTYP (word 16₈) are used for the object block number. The same object block number is used for both the current, next, and previous versions.

| Offset | Content |
|--------|---------|
| 0 | U W R M flags, Terminal no. reserving |
| 1 | File name (8 words) |
| 11₈ | File type (2 words) |
| 13₈ | Next version |
| 14₈ | Previous version |
| 15₈ | Public acc. / Friend acc. / Own acc. |
| 16₈ | ¹⁵OBJBL₁₄ / TM L M A C I S P T OFTYP (**OBJBL new in K-version**) |
| 17₈ | Device number for peripheral file |
| 20₈ | Dir. index of r. / User index of res |
| 21₈ | Object index of this object entry |
| 22₈ | Current open count |
| 23₈ | Total open count |
| 24₈ | Date created (double word) |
| 26₈ | Last date opened for read (double word) |
| 30₈ | Last date opened for write (double word) |
| 32₈ | No. of pages in file (double word) |
| 34₈ | Maximum byte pointer (double word) |
| 36₈ | S J flags, File pointer |

**Source:** K-version, Section 7.6, Page 134, Lines 7260-7284

### 2.5 Object File Buffer Header

The object file buffer header was increased by one word to hold the block number of the object in the buffer:

| Offset | Content |
|--------|---------|
| 0 | Lock number of object buffer |
| 1 | Directory index |
| 2 | Current object block number (**New in K-version**) |
| 3 | Current object index (8 bits) |
| 4 | (empty) |
| 5 | First index in index buffer |
| 6 | (empty) |
| 7 | Index buffer |

**Source:** K-version, Section 7.7, Page 135, Lines 7293-7306

### 2.6 Open File Table Entry - OFFTP

The 4 most significant bits in OFFTP (displacement 6 in the open file table entry) were previously free and are now used to hold the object block number of the file.

**Source:** K-version, Section 7.8, Page 135, Line 7310

### 2.7 Restrictions and Compatibility

- All versions of a file must have object entries in the same object block (to keep compatible with earlier versions).
- **WARNING**: If moving a directory with files on object index > 255 from version K back to version J:
  - All files with object index > 255 will be invisible (they reappear when moved back to K)
  - @REGENERATE-DIRECTORY or @TEST-DIRECTORY in J-version will corrupt such directories and files with index > 255 will be lost
  - The FILE-SYSTEM-INVESTIGATOR for version J will report errors on such files

**Source:** K-version, Sections 7.3 and 7.9, Pages 131 and 135, Lines 7194-7320

### 2.8 Related Commands

| Command | Description | Version |
|---------|-------------|---------|
| @GIVE-OBJECT-BLOCKS | Allow user to create more than 256 files. Each block = 256 files, max 16 blocks = 4096 files | K |
| @TAKE-OBJECT-BLOCKS | Restrict number of files for a user. Object blocks to be taken must be free | K |
| @USER-STATISTICS | Reports number of files allowed for a user | K |

**Source:** K-version, Sections 2.3.3 and 2.3.8, Lines 1805-1882

---

## 3. FSMTY - MON 327 (File System Multifunction Monitor Call)

FSMTY was introduced in the J-version and extended in each subsequent version. It uses the T-register for function selection.

### 3.1 Function 1: WRBIX - Write Back Index Blocks (J-version)

Forces writing to disk of modifications to the datafield index blocks which may not yet have been written back (where changes have only been made in the index blocks in the open file entry). This leaves the file consistent in the event of an uncontrolled system stop with the file still open. Particularly useful for SIBAS and ISAM applications.

**Monitor call format (ND-100):**
```
LDT FUNC       % T = function (1)
LDA FILNO      % A = open file number
MON 327
JMP ERROR      % error return
               % normal return
```

| Register | Content |
|----------|---------|
| T (input) | Function = 1 |
| A (input) | Open file number |
| A (error) | Error code |

**Source:** J-version, Section 6.2.1, Lines 1395-1413

### 3.2 Function 2: Return Block Size (K-version)

Returns the block size of an open file.

| Register | Content |
|----------|---------|
| T (input) | Function = 2 |
| A (input) | Open file number |
| A (output, skip) | Block size in words |
| A (error) | Error code |

**Source:** K-version, Section 3.1.13, Lines 2175-2187

### 3.3 Function 3: Get File Name (K-version)

Gets the full file name of an open file. The file name can be a remote file in the format: `system.{directory:user}file:type`

| Register | Content |
|----------|---------|
| T (input) | Function = 3 |
| A (input) | Open file number |
| X (input) | Address of buffer to receive file name |
| Buffer (output) | Full file name |
| A (error) | Error code |

**Source:** K-version, Section 3.1.13, Lines 2198-2220

### 3.4 Function 4: Get File/Device Information (K-version)

Gets information about an open file identified by open file number or device number.

| Register | Content |
|----------|---------|
| T (input) | Function = 4 |
| A (input) | Open file number or device number |
| A (output, skip) | The opposite of input (device number if file number was given, and vice versa) |
| X (output) | TYPRING bits (TYPRING word from data field of device) |
| D (output) | Status: bit 0 = 1 if file open for write, bit 1 = 1 if spooling file or terminal/TAD |
| A (error) | Error code |

**Source:** K-version, Section 3.1.13, Lines 2221-2243

### 3.5 Function 5: Internal Use (L-version)

For internal use by ND only.

**Source:** L-version, Section 3.1.6, Line 1081

### 3.6 Function 6: Get Next Open File on Directory (L-version)

Gets the next open file (for a logged-in user) on a directory. Used for iterating through all open files.

**Monitor call format (ND-100):**
```
LDT FUNC       % T = function (6)
LDA FILNO
COPY SA DD     % D = open file number
LDA DIRIN      % A = directory index
LDX TERNO      % X = terminal number
MON 327
JMP ERROR      % error return
               % normal return

FUNC, 6
FILNO, -1      % -1 means from start
DIRIN, 1
TERNO, 17
```

| Register | Direction | Content |
|----------|-----------|---------|
| T | Input | Function = 6 |
| A | Input | Directory index |
| D | Input | Open file number (-1 = from start) |
| X | Input | Terminal number (-1 = from start) |
| A | Output (skip) | User index of open file |
| T | Output (skip) | Object index of open file |
| D | Output (skip) | Open file number |
| X | Output (skip) | Terminal number |
| A | Error | Error code (-1 = no more open files in directory) |

**Source:** L-version, Section 3.1.6, Lines 1097-1131

### 3.7 Function 7₈: Set Directory Available (L-version)

Sets a directory available for general use (after it was set unavailable).

| Register | Content |
|----------|---------|
| T (input) | Function = 7 |
| A (input) | Directory index |
| A (error) | Error code |

**Source:** L-version, Section 3.1.6, Lines 1133-1156

### 3.8 Function 10₈: Set Directory Unavailable (L-version)

Sets a directory unavailable for general use. This means:
- No more users may enter it (log in with this directory as main or default directory)
- No more files may be opened on the directory

Users already entered or files already open are not affected.

| Register | Content |
|----------|---------|
| T (input) | Function = 10₈ |
| A (input) | Directory index |
| A (error) | Error code |

**Source:** L-version, Section 3.1.6, Lines 1167-1196

### 3.9 Function 11₈: Get Next File Matching String (L-version)

Gets the next file matching a specified string. Used for file name pattern matching.

**Monitor call format (ND-100):**
```
LDT FUNC       % T = function (11₈)
LDA FILNO
COPY SA DD     % D = object index of file to check
LDA (FNAME     % A = address of buffer to receive file name
LDX (MATCH     % X = address of buffer containing match string
MON 327
JMP ERROR      % error return
               % normal return

FUNC, 11
FILNO, 0
MATCH, 'TEST'
FNAME, 0; *+47/
```

| Register | Direction | Content |
|----------|-----------|---------|
| T | Input | Function = 11₈ |
| A | Input | Address of buffer to receive full file name |
| D | Input | Object index of first file to check |
| X | Input | Address of buffer containing match string |
| D | Output (skip) | Object index of matching file |
| A | Error | Error code |

**Source:** L-version, Section 3.1.6, Lines 1200-1233

### 3.10 Function 12₈: Get EXSECURITY Value (L-version)

Returns the current value of the SINTRAN III system variable EXSECURITY.

| Register | Content |
|----------|---------|
| T (input) | Function = 12₈ |
| A (output, skip) | Value of EXSECURITY |
| A (error) | Error code |

**Source:** L-version, Section 3.1.6, Lines 1247-1267

### 3.11 Function 13₈: Reset File-Modified Bit (M-version)

Resets the "file-modified" bit in the object entry.

**Monitor call format (ND-100):**
```
LDT FUNC       % T = function (13₈)
LDA DUIDX      % A = directory- and user indexes
LDX OINDX      % X = file object index
MON 327
JMP ERROR      % error return
               % normal return

FUNC, 13
DUIDX, 1       % MSB = directory index, LSB = user index
OINDX, 2       % file object index
```

| Register | Direction | Content |
|----------|-----------|---------|
| T | Input | Function = 13₈ |
| A | Input | Directory and user indexes: MSB = directory index, LSB = user index. If bit 17₈ is set, the D-register contains a remote system identification |
| X | Input | File object index |
| D | Input | Address of buffer containing remote system identification (only if bit 17₈ of A is set) |
| A | Error | Error code |

**ND-500/5000 format:**
```
CALLG 37000000327B,4 or 5,<function>,<directory index>,<user index>,<file object index>[,<remote system specification>]
```

If bit number 7 is set in `<directory index>`, the file is on a remote system and the 5th parameter contains remote system specification.

**Source:** M-version, Section 3.1.6, Lines 1095-1138 and Section 5.6.5, Lines 1838-1861

### 3.12 Function 14₈: Get Next Page in File (M-version)

Gets the page number of the next existing page in a file. Especially useful for files containing holes: if the input page number is inside a "hole", the first page number after the hole is returned.

**Monitor call format (ND-100):**
```
LDT FUNC       % T = function (14₈)
LDX OPNFN      % X = open file number
LDD SPGNO      % AD = logical page number to start from
MON 327
JMP ERROR      % error return
               % normal return

FUNC, 14
OPNFN, 101
SPGNO, 0;50    % double word: start page number
```

| Register | Direction | Content |
|----------|-----------|---------|
| T | Input | Function = 14₈ |
| X | Input | Open file number |
| AD | Input | Logical page number to start from |
| AD | Output (skip) | Logical page number of next existing page (may be same as input) |
| A | Error | Error code |

**ND-500/5000 format:**
```
CALLG 37000000327B,4,<function>,<file number>,<start logical page number>,<next existing logical page number>
```

**Source:** M-version, Section 3.1.6, Lines 1139-1180 and Section 5.6.5, Lines 1863-1886

### 3.13 FSMTY Function Summary

| Function | Name | Version | Description |
|----------|------|---------|-------------|
| 1 | WRBIX | J | Write back index blocks for open file to disk |
| 2 | (block size) | K | Return block size of open file |
| 3 | (get name) | K | Get full file name of open file |
| 4 | (get info) | K | Get file/device information |
| 5 | (internal) | L | Internal use by ND only |
| 6 | (next open) | L | Get next open file on a directory |
| 7₈ | (set available) | L | Set directory available |
| 10₈ | (set unavailable) | L | Set directory unavailable |
| 11₈ | (match file) | L | Get next file matching string |
| 12₈ | (get EXSECURITY) | L | Get value of EXSECURITY |
| 13₈ | (reset modified) | M | Reset file-modified bit in object entry |
| 14₈ | (next page) | M | Get next page in file (handles holes) |

---

## 4. IOPEN - MON 351 (M-version)

Open a file using specific directory, user, and file object indexes, or return indexes for a file opened by name.

### 4.1 ND-100 Format

**Monitor call format:**
```
LDA (INDEX
COPY SA DD      % D = address of index list
LDX (FNAME      % X = address of file name buffer
LDA (FTYPE      % A = address of default file type buffer
LDT ACCES       % T = access mode

MON 351         % IOPEN
JMP ERROR       % error return
                % normal return

INDEX,  1       % MSB = directory index, LSB = user index
         2      % second word = file object index
FNAME, 'EXAMPL:SYMB'  % file name (with or without type)
FTYPE, 'DATA'          % default file type
ACCES, 3               % file access mode
```

### 4.2 Input Parameters (ND-100)

| Register | Content |
|----------|---------|
| D | Address of double word buffer: Word 1 MSB = directory index, Word 1 LSB = user index, Word 2 = file object index. If both words = -1, file name/type parameters apply and indexes will be returned |
| X | Address of buffer containing file name and/or type (not used if D points to valid indexes) |
| A | Address of buffer containing default file type (must not contain leading colon, not used if D points to valid indexes) |
| T | File access mode (see table below) |

### 4.3 Access Mode Ranges

MON IOPEN covers three access mode ranges corresponding to existing monitor calls:

| MON Call | MON Number | Access Mode Range |
|----------|-----------|-------------------|
| MON OPEN | MON 50 | 0 - 11₈ |
| MON SCROP | MON 235 | 40₈ - 51₈ |
| MON DOPEN | MON 220 | 100₈ - 111₈ |

### 4.4 Access Mode Details (Range 0-11₈, MON OPEN equivalent)

| Code | Description |
|------|-------------|
| 0 | Sequential write |
| 1 | Sequential read |
| 2 | Random read or write |
| 3 | Random read only |
| 4 | Sequential read or write |
| 5 | Sequential write append |
| 6 | Random read or write common on contiguous files |
| 7 | Random read common on contiguous files |
| 10₈ | Random read or write on contiguous files |
| 11₈ | Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 114) in RT-programs |

**Source:** M-version, Section 3.2.1, Lines 1266-1348

### 4.5 ND-500/5000 Format

```
CALLG 37000000351B,5,<file number>,<access code>,<file name>,<file type>,<directory/user/object index>
```

| Parameter | Description |
|-----------|-------------|
| `<file number>` | ND-500/5000 connect file number. If 0, first free number used and returned |
| `<access code>` | File access mode (same ranges as ND-100) |
| `<file name>` | File name and/or type (not used if indexes provided) |
| `<file type>` | Default file type (not used if indexes provided) |
| `<dir/user/object index>` | 32-bit word: 1st byte = directory index, 2nd byte = user index, last 2 bytes = object index. Value -1 = use file name/type instead, indexes returned |

**Source:** M-version, Section 5.7.1, Lines 1947-1975

---

## 5. File System Cache and Disk I/O

### 5.1 Device Buffers Increased: 64 to 128 (M-version)

The maximum number of device buffers was increased from 64 to 128. This effectively doubled the maximum disk cache size. However, installing M-version matching the L-version configuration retains the same cache size. To increase, use the SINTRAN III Configuration Program to change the number of device buffers. A cold start is required.

**Source:** M-version, Lines 439, 2389, 2396

### 5.2 BDIO Pools Increased to 64 (M-version, Generation 6)

The maximum number of BDIO pools supported by SINTRAN III was increased to 64 in generation 6.

**Source:** M-version, Lines 443, 2390

### 5.3 Indexed File Page Allocation 3x Faster (M-version)

The function for allocating a new page for indexed files was changed to perform approximately three times faster. This means common functions like reading a file into NOTIS-WP (which writes a copy to a scratch file) run significantly faster.

**Source:** M-version, Line 2394

### 5.4 Dirty Cache / Delayed Write (N-version)

The file system cache was changed to introduce delayed write operations ("dirty cache") which is flushed to disk when necessary.

#### Flush Conditions

Flushing is always done when:

1. **LRU reuse** - The least recently used buffer in the file system cache is about to be used again
2. **60-second timeout** - The buffer has not been accessed for 60 seconds
3. **System commands** - One of @RESTART-SYSTEM, @STOP-SYSTEM, or @COLD-START is given (flushes complete cache)
4. **@RELEASE-DIRECTORY** - Flushes all buffers for a specific directory
5. **User management** - One of @RENAME-USER, @CREATE-USER, @DELETE-USER is given (flushes complete cache)

#### DELWR Variable

The delayed write option may be disabled using the SINTRAN Service Program command `*CHANGE-VARIABLE` to set the variable `DELWR` to 0 (setting it to 1 means enable the option).

| Variable | Value | Meaning |
|----------|-------|---------|
| DELWR | 0 | Disable delayed write |
| DELWR | 1 | Enable delayed write |

**IMPORTANT NOTE from M-version:** Operating the system with a large disk cache implies that the contents of the disk cache may not be written to disk immediately. To force writing the disk cache to disk on a controlled stop of the system, ensure that all files are closed properly. For example, use the command @RELEASE-DIRECTORY on all entered directories.

**Source:** N-version, Section 8.2, Lines 1587-1599; M-version, Line 2398

---

## 6. Disk Access Log (J-version)

The disk access log facility is an option that must be ordered at system generation time.

### 6.1 Service Program Commands

The SINTRAN-SERVICE-PROGRAM command `*DISC-ACCESS-LOG` has the following subcommands:

| Subcommand | Description |
|------------|-------------|
| DEFINE-DISC-ACCESS-LOG | Define log file, record size, and filter criteria |
| START-DISC-ACCESS-LOG | Start the log procedure |
| STOP-DISC-ACCESS-LOG | Stop the log procedure |
| START-DISC-ACCESS-COUNTER | Start and define disk access counter |
| STOP-DISC-ACCESS-COUNTER | Stop disk access counter |
| CLEAR-DISC-ACCESS-COUNTER | Reset counters to 0 |
| DISC-ACCESS-COUNTER | Display present counter values |
| DISC-DRIVER-ERROR-INFORMATION | List error variables from disk driver |
| DISC-ERROR-STATUS | List error information from disk datafield |
| LOG-DISC-ACCESS-COUNTER | Log counter values at intervals (default 60s, terminate with Escape) |
| EXIT | Exit subcommand mode |
| HELP | List available subcommands |

**Source:** J-version, Section 10.5.2, Lines 2528-2544

### 6.2 Log File Requirements

The log file must be contiguous. The area occupied by the log file will be used to log disk accesses but will not be accessed through the file system (the byte pointer of the file remains zero).

### 6.3 Filter Options

The DEFINE-DISC-ACCESS-LOG function can specify:
- Record size: Small (4 words) or Big (8 words)
- All disk accesses or only accesses to a specified controller or drive
- Read-only or write-only access logging
- Accesses to a limited part of a disk (first/last disk address)

### 6.4 Log Record Layouts

#### Small Record (4 words):

| Word | Symbol | Content |
|------|--------|---------|
| 0 | CTREG | Function |
| 1 | CAREG | Disk address |
| 2 | CDREG | Disk address |
| 3 | CLUNIT | Logical unit no. |

#### Big Record (8 words):

| Word | Symbol | Content |
|------|--------|---------|
| 0 | CTREG | Function |
| 1 | CAREG | Disk address |
| 2 | CDREG | Disk address |
| 3 | CXREG | No. of sectors to transfer |
| 4 | MEMA1 | Memory address |
| 5 | MEMA2 | Memory address |
| 6 | CLUNIT | Logical unit no. |
| 7 | RTREF | Current RT-program |

**Source:** J-version, Section 10.5, Lines 2623-2646

### 6.5 Internal Operation

Information about the disk log is in datafield DF0IL. If defined and started, CTRDISK checks the disk log datafield for each transfer. When started, a buffer page is allocated and divided into two buffers. When one buffer is full, RT-program RTDIL is activated to dump the buffer to the log file. Subsequent transfers are logged to the other buffer.

**Source:** J-version, Section 10.5.3, Lines 2647-2652

### 6.6 Disk Access Log Datafield (DF0IL) Layout

| Disp | Symbol | Description |
|------|--------|-------------|
| -2 to 11₈ | ... | Standard mass storage locations |
| 12₈ | DILBPNT | Buffer pointer for disk log |
| 13₈ | DILBANK | Memory bank for disk log buffer |
| 14₈ | DILADDR | 16 lower phys. memory addr bits of disk log buffer |
| | | DOUBLE D0IBADDR=DILBANK |
| 15₈ | DIL1DADDR | Start address of disk log file (disk addr, word 1) |
| 16₈ | DIL2DADDR | Start address of disk log file (disk addr, word 2) |
| 17₈ | DILNSEC | Number of sectors per page on disk log file disk |
| 20₈ | DIL1LDADD | Last legal disk address on disk log (word 1) |
| 21₈ | DIL2LDADDR | Last legal disk address on disk log (word 2) |
| 22₈ | DILGFLAG | Disk log flags (see below) |
| 23₈ | DILFLOG | Logical device number of disk log file disk |
| 24₈ | DILFUNIT | Drive number for disk log file disk drive |
| | | DOUBLE D0ILFLOG=DILFLOG |
| 25₈ | DILLOGV | Logical unit number of disk to log |
| 26₈ | DILDLIVE | Drive number of disk to log |
| 27₈ | DILALOGV | Disk controller to count disk accesses on |
| 30₈ | DILAUNIT | Disk unit number to count disk accesses on |
| 31₈ | DIL1FADDR | First disk address to log (word 1) |
| 32₈ | DIL2FADDR | First disk address to log (word 2) |
| 33₈ | DIL1LADDR | Last disk address to log (word 1) |
| 34₈ | DIL2LADDR | Last disk address to log (word 2) |
| 35₈ | 1XNDACCESS | Disk access counter (word 1) |
| 36₈ | 2XNDACCESS | Disk access counter (word 2) |
| 37₈ | 1XNWACCESS | Write disk access counter (word 1) |
| 40₈ | 2XNWACCESS | Write disk access counter (word 2) |
| 41₈ | DALFUNC | Function code in MON ABSTR for disk access log RT-program |
| 42₈ | DALCMADDR | Memory address in MON ABSTR |
| 43₈ | DALCDADDR | Disk address in MON ABSTR |

**Source:** J-version, Lines 2662-2702

### 6.7 DILGFLAG Bits

| Bit | Symbol | Description |
|-----|--------|-------------|
| 0 | DILCONTROLLER | Log for one controller only |
| 1 | DIL1UNIT | Log for one unit (drive) number only |
| 2 | DILLIMIT | Log a part of the disk only |
| 3 | DILRACCESS | Log read accesses only |
| 4 | DILWACCESS | Log write accesses only |
| 5 | DILSMALL | Write small record (4 words) on disk log file |
| 6 | DAC1CONTROLLER | Disk access counter for one controller only |
| 7 | DAC1UNIT | Disk access counter for one unit number only |
| 10₈ | DILSTART | Disk log started |
| 11₈ | DILDEFINED | Disk log file defined |
| 12₈ | ID1TLBFULL | Disk log file buffer #1 is full |
| 13₈ | 2DILBFULL | Disk log file buffer #2 is full |
| 14₈ | DILCOUNT | Count disk accesses |
| 15₈ | DILBOK | Disk log buffer is fixed |

**Source:** J-version, Lines 2703-2733

### 6.8 MON ABSTR (MON 131) Functions for Disk Log

Uses logical device number 2200₈.

```
LDT LDN
LDA (PARLI

PARLI, FUNC
       DESAD

FUNC, 3          ! Function
DESAD, DESA1     ! Physical address of
       DESA2     ! working area

LDN, 2200        ! Logical device number for MON ABSTR against disk log
```

#### Working Area Layout for Function 3 (Define):

| Offset | Symbol | Content |
|--------|--------|---------|
| 0 | ZFSTART | Start address of disk log file |
| 1 | | (sector address) |
| 2 | ZFNBLCK | Sectors per page on log file disk |
| 3 | ZFEND | Last legal address on disk log file |
| 4 | | |
| 5 | ZFLOGU | Logical device number of log file disk |
| 6 | ZFUNIT | Drive number of log file disk |
| 7 | ZDILGFLAG | Flag (see DILGFLAG bits) |
| 10₈ | ZILOGU | Logical device number of disk to be logged |
| 11₈ | ZLUNIT | Drive number of disk to be logged |
| 12₈ | ZFLGADDR | First sector address of specified area |
| 13₈ | | |
| 14₈ | ZLLGADDR | Last sector address of specified area |
| 15₈ | | |

#### All MON ABSTR Functions:

| Function | Description |
|----------|-------------|
| 1 | Write disk log record (4 or 8 words depending on record size) |
| 2 | As function 1, then fill rest of buffer with zeros and write to log file |
| 3 | Define disk log record (write working area to DF0IL) |
| 4 | Start disk log (working area not used) |
| 5 | Stop disk log (working area not used) |
| 6 | Start disk access counter (first word of working area = flag word) |
| 7 | Stop disk access counter |
| 10₈ | Clear disk access counter |
| 11₈ | Read disk access counter (4 words returned: words 0-1 = reads, words 2-3 = writes) |
| 12₈ | Read last disk error information (variables from disk driver to working area) |
| 13₈ | Read disk datafield (variables from disk datafield to working area) |

MON ABSTR activates the "driver" OVDIL on level 11 which performs the necessary updating of the disk access log datafield DFDIL.

**Source:** J-version, Section 10.5.4, Lines 2734-2824

---

## 7. File System Event-Log Utility (M-version)

### 7.1 Overview

The file system event-log utility reports occurrences of selected file system events (login, logout, open file, etc.). It is intended for security-violation tracing and debugging purposes.

All reports are routed through the ERS/SINTRAN III Watchdog. It may be necessary to increase the size of the log file `ER-S3WD-LOG:DATA` on user area SYSTEM.

**Source:** M-version, Section 6.3, Lines 2400-2412

### 7.2 EXSECURITY Bit 6

Bit 6 in the system variable EXSECURITY indicates whether the file system event-log utility is activated.

**Source:** M-version, Section 10.1, Lines 2544-2558

### 7.3 Loggable Operations (M-version)

| Operation | Description |
|-----------|-------------|
| change password | @CHANGE-PASSWORD or @CLEAR-PASSWORD |
| change user area | UE-FUNCTION CHANGE-USER-AREA or MON SUSCN or MON RUSCN |
| change user entry | @CHANGE-USER-ENTRY |
| create file | @CREATE-FILE, @ALLOCATE-FILE, @CREATE-NEW-VERSION, @ALLOCATE-NEW-VERSION, MON CRALF, MON CRALN |
| create user area | @CREATE-USER |
| delete file | @DELETE-FILE, @DELETE-USERS-FILES, MON MDLFI |
| delete user area | @DELETE-USER |
| login | @LOGIN or MON MLOGI |
| logout | @LOGOUT |
| open file | @OPEN-FILE, @SCRATCH-OPEN, @CONNECT-FILE, @RTOPEN-FILE, @RTCONNECT-FILE, MON OPEN, MON DOPEN, MON SCROP, MON IOPEN |
| read user entry | @DUMP-USER-ENTRY or MON RUSER |
| rename file | @RENAME-FILE or MON MRNFI |
| rename user area | @RENAME-USER |
| set file access | @SET-FILE-ACCESS or MON SFACC |

**Source:** M-version, Section 6.3.2, Lines 2418-2437

### 7.4 Additional Loggable Operations (N-version)

| Operation | Description |
|-----------|-------------|
| CREATE-FRIEND | @CREATE-FRIEND |
| DELETE-FRIEND | @DELETE-FRIEND |
| SET-FRIEND-ACCESS | @SET-FRIEND-ACCESS |

The following events are always reported (cannot be disabled):

| Event | Description |
|-------|-------------|
| DISABLE-ERROR | *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-ERROR |
| DISABLE-EVENT | *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-EVENT |
| DISABLE-LOG | *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-LOG |
| ENABLE-ERROR | *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-ERROR |
| ENABLE-EVENT | *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-EVENT |
| ENABLE-LOG | *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-LOG |

**Source:** N-version, Section 8.3, Lines 1601-1620

### 7.5 *FILE-SYSTEM-EVENT-LOG Subcommands (M-version)

| Subcommand | Description |
|------------|-------------|
| HELP | List all subcommands |
| ENABLE-LOG `<memory?>,<image?>,<save area?>` | Enable the event-log utility (sets EXSECURITY bit 6) |
| DISABLE-LOG `<memory?>,<image?>,<save area?>` | Disable the event-log utility |
| ENABLE-EVENT | Select file system operations to log |
| DISABLE-EVENT | Deselect file system operations |
| ENABLE-ERROR | Select error codes to report |
| DISABLE-ERROR | Deselect error codes |

Error code specification: error codes in ranges 1:255 and 1664:1698 indicate errors. Code 0 means all situations including successful attempts.

**Source:** M-version, Section 4.2.2, Lines 1667-1690 and Section 6.3.3, Lines 2439-2447

---

## 8. EXSECURITY Variable

The EXSECURITY variable controls various security features. It was extended across versions:

| Bit | Description | Version |
|-----|-------------|---------|
| 0 | No listing of command lines in @TERMINAL-STATUS except for own user. User SYSTEM sees all background programs. | J |
| 1 | Background segment (program and data bank) set to zero when logging out. Delays logout (seconds). | J |
| 2 | Scratch file pages written to in last session set to zero when logging out. Slows logout. | J |
| 3 | Zeroing of pages released from a file (normally in @DELETE-FILE). | J |
| 4 | Not allowed to log in if user has no password. Only one login without password after @CREATE-USER. Also blocks remote file access to users without password. | L |
| 5 | @HELP and @LIST-REENTRANT only list commands/subsystems available to the user. Unprivileged users don't "see" SYSTEM/RT-only commands. | L |
| 6 | File system event-log utility is activated. | M |

**Default value:** 7 (bits 0, 1, and 2 are set). Can be changed via `*CHANGE-VARIABLE`.

**Source:** L-version, Lines 1269-1283; M-version, Lines 2544-2558

---

## 9. Default File Access (J-version)

### 9.1 Default Access for New Users

When a new user is created, the default file access is set to:

| Access Type | Default |
|-------------|---------|
| Public access | NONE |
| Friend access | RWACO |
| Own access | RWACD |

Files with public access equal to NONE may be appended to a batch process and to a spooling queue.

### 9.2 Default Friend Access

When a friend is created, the default access is R (read).

### 9.3 Scratch File Access

Scratch files may now have no public nor friend access.

**Source:** J-version, Sections 16-17, Lines 3376-3409

---

## 10. File System Commands

### 10.1 New Commands by Version

#### J-version
(No new file system commands documented)

#### K-version

| Command | Description |
|---------|-------------|
| @DEFINE-MASS-STORAGE-UNIT | Define a mass storage device in directory table (reserves directory index without entering) |
| @DELETE-MASS-STORAGE-UNIT | Delete mass storage device definition from directory table |
| @GIVE-OBJECT-BLOCKS | Allow user to create >256 files (max 16 blocks × 256 = 4096) |
| @LIST-MASS-STORAGE-UNITS | List all mass storage units and corresponding directory index |
| @SET-MASS-STORAGE-SIZE | Set or change directory size for SCSI controllers |
| @TAKE-OBJECT-BLOCKS | Restrict number of files for a user (blocks must be free) |
| @UNLOCK-DIRECTORY | Unlock directory entered but not released on another system |

**Source:** K-version, Sections 2.3.1-2.3.9, Lines 1775-1898

#### L-version

| Command | Description |
|---------|-------------|
| @FILE-SYSTEM-ERROR-MESSAGES | Enable detailed error messages from file system monitor calls (on terminal or error device) |
| @LIST-ALL-OPEN-FILES | List all files open on a directory (file name and which terminal) |
| @SET-DIRECTORY-AVAILABLE | Set directory available for general use |
| @SET-DIRECTORY-UNAVAILABLE | Set directory unavailable (no more logins, no more file opens) |

**Source:** L-version, Sections 2.3.2-2.3.6, Lines 883-950

#### M-version

| Command | Description |
|---------|-------------|
| @EXPAND-DIRECTORY | Expand directory on SCSI/DOMINO disk, or reposition bit-file |
| *FILE-SYSTEM-EVENT-LOG | Security event logging utility (in SINTRAN Service Program) |

**Removed command:**

| Command | Replacement |
|---------|-------------|
| @COPY-DIRECTORY | Use MULTI-USER-COPY function in the Backup System instead |

**Source:** M-version, Sections 2.1.1 and 2.3.1, Lines 840-842 and 1041-1063

### 10.2 @COPY Modification (L-version)

The @COPY command now opens the source file prior to opening the destination file. This means that if the source file could not be opened successfully, the destination file is not affected.

**Source:** L-version, Section 2.2.1, Lines 804-806

### 10.3 @RTCLOSE-FILE Changes (N-version)

The command @RTCLOSE-FILE now reports the closing of an RT-open file to be logged by the ERS/SINTRAN III Watchdog. This applies unless the file was opened by the command @RTOPEN-FILE from the same process.

**Source:** N-version, Section 3.1.6, Lines 901-904

### 10.4 MON 43 CLOSE Changes (N-version)

MON CLOSE (MON 43) used from an RT-program to close a file not opened by the program itself now reports the closing of the RT-open file to be logged by the ERS/SINTRAN III Watchdog.

**Source:** N-version, Section 4.1.1, Lines 960-962

---

## 11. Spooling System Reorganization (J-version)

### 11.1 Overview

The spooling system was reorganized to save space in resident memory and reduce the number of segments used by each spooling process. The maximum number of spooling processes increased by 15 to 30 (plus COSMOS spooling).

**Source:** J-version, Section 18, Lines 3413-3418

### 11.2 Resident Spooling Datafield (10 words)

The memory-resident spooling datafield was reduced to 10 words:

| Symbol | Description |
|--------|-------------|
| (JPL I ++1) | |
| SPORT | Start address to spooling process |
| (label) | Label on spooling program segment |
| SPROG | Spooling prog RT-desc. address |
| SPERI | Peripheral device number |
| SQUEU | Spooling queue segment number |
| SQSEM | Spooling queue semaphore no |
| SQIOS | Spooling queue I/O semaphore no |
| SSTOP | Stop command flag |
| SABOR | Abort command flag |
| SPINX | Spooling index (1-30) |

**Symbols:** SPLEN=12₈ (length of table entry in resident), SSPLEN=340₈ (length on segment)

**Source:** J-version, Section 18.1, Lines 3420-3526

### 11.3 Spooling Program Segment Layout

The spooling program was moved from segment 25 (File System Reentrant segment no. 2) to its own segment. Save area on segment 42 (Initial Spooling Program Segment).

| Address | Content |
|---------|---------|
| 110000₈ | 31 spooling datafields (30 processes + COSMOS) |
| 125443₈ | 31 Form datafields |
| 132677₈ | SPORT: Start of spooling program |
| 137632₈ | End of spooling program |

**Source:** J-version, Section 18.2, Lines 3537-3550

### 11.4 Spooling Queue Segment Layout

Each spooling process has one spooling queue segment:

| Address | Content |
|---------|---------|
| 140000₈ | Subroutines operating on the queue (copied from segment 25) |
| 140345₈ | Number of elements in the queue |
| 140353₈ | Maximum number of elements in the queue |
| 140356₈ | Start of queue elements |
| 147777₈ | End of segment |

Standard queue segment = 4KW (4 pages) = 28 queue elements.
Maximum queue segment = 14KW = 103 queue elements.

**Source:** J-version, Section 18.3, Lines 3551-3565

### 11.5 Queue Size Reduction (L-version)

Maximum size of a spooling queue reduced to 6 pages. Default unchanged at 4 pages.

Queue length calculation:
- First 2 pages: 10 queue entries
- Each additional page: ~7 additional entries

| Pages | Max Queue Entries |
|-------|-------------------|
| 2 | 10 |
| 3 | ~17 |
| 4 | ~24 |
| 5 | ~31 |
| 6 | ~38 |

**Source:** L-version, Chapter 7, Lines 2334-2340

### 11.6 Additional Spooling Changes (J-version)

- If the machine stops while printing, the interrupted file is restarted
- The command DEFINE-SPOOLING-FILE-MESSAGE is reinstalled
- Files with no public access may now be appended to a spooling queue

**Source:** J-version, Section 18.4, Lines 3576-3585

---

## 12. File System Error Codes

### 12.1 New Error Codes for IOPEN (M-version)

| Octal | Decimal | Message |
|-------|---------|---------|
| 3231₈ | 1688 | Illegal directory index |
| 3232₈ | 1689 | Illegal user index |
| 3233₈ | 1690 | Illegal object index |
| 3234₈ | 1691 | Directory index does not match directory name |
| 3235₈ | 1692 | User index does not match user name |
| 3236₈ | 1693 | Object index does not match object name |
| 3237₈ | 1694 | Illegal file type |
| 3240₈ | 1695 | Illegal version number |
| 3241₈ | 1696 | Not so much space available |
| 3242₈ | 1697 | The specified bit file pages are not free |

#### Detailed Descriptions:

- **3231₈ - Illegal directory index**: Directory index out of range or no directory with the index specified
- **3232₈ - Illegal user index**: User index out of range or no user with the index specified
- **3233₈ - Illegal object index**: Object (file) index out of range or no file with the index specified
- **3234₈ - Directory index does not match directory name**: The directory name in a file specification does not match the name of the directory with the directory index specified
- **3235₈ - User index does not match user name**: The user name does not match the name of the user with the user index specified
- **3236₈ - Object index does not match object name**: The file name does not match the name of the file with the object index specified
- **3237₈ - Illegal file type**: The file type does not match the file type of the file with the object index specified
- **3240₈ - Illegal version number**: The version number does not match the version number of the file with the object index specified
- **3241₈ - Not so much space available**: Space not available to expand the directory with the number of pages specified
- **3242₈ - The specified bit file pages are not free**: Attempt to reposition the bit-file to an area which is not free

**Source:** M-version, Section 12.2-12.3, Lines 2591-2648

### 12.2 BDIO Error Codes (M-version)

| Octal | Message | Parameters |
|-------|---------|------------|
| 1671₈ | BDIO pool reconnected | BDIO pool name (device name) |
| 1672₈ | Unsuccessful attempt to reconnect to BDIO pool | BDIO pool name (device name) |

**Source:** M-version, Lines 2586-2587

### 12.3 Event-Log Error Code Ranges (M-version)

For the file system event-log, error codes in the ranges 1:255 and 1664:1698 indicate errors. Code 0 means all situations including successful attempts.

**Source:** M-version, Line 2447

---

## 13. GETXM - MON 334 (J-version)

Get error-message text for file system error codes.

**Monitor call format:**
```
LDA ECODE       % A = error code
LDX (BUFFR      % X = address of buffer for text
MON GETXM
JMP ERROR       % Error return
```

| Register | Content |
|----------|---------|
| A (input) | Error code (normal file system error codes used in MON 64 and MON 65) |
| X (input) | Address of buffer to receive error-message text (minimum 100 words) |

**Source:** J-version, Section 6.2.5, Lines 1564-1586

---

## 14. SCSI and Optical Disk File System Support (K-version)

### 14.1 SCSI Device Limits

| Resource | Max per System |
|----------|----------------|
| SCSI adaptors | 4 |
| SCSI magnetic disk drives | 14 |
| SCSI optical disk drives | 4 |
| SCSI streamer tape drives | 2 |
| SCSI magnetic tape drives | 4 |

### 14.2 SCSI Device Names

| Device Type | Naming Pattern |
|-------------|----------------|
| Magnetic disk | DISC-SCSI-1 through DISC-SCSI-14 |
| Optical disk | DISC-OPTICAL-1 through DISC-OPTICAL-4 |
| Streamer tape | STREAMER-1, STREAMER-2 |
| Magnetic tape | MAG-TAPE-1 through MAG-TAPE-4 |

Subdivided disks: e.g., DISC-2-SCSI-3 = disk 3 divided into 2 subunits.

### 14.3 Supported Magnetic Disk Sizes

- 60 MB (5.25" fixed)
- 125 MB (5.25" fixed)
- 150 MB (5.25" fixed)
- 310 MB (8" fixed)
- 630 MB (8" fixed)

All SCSI magnetic disk units may be divided into 2, 3, 4, 5, 6, or 8 subunits.

### 14.4 Optical Disk (1 GB Laserdrive 1200)

- May be divided into 2-8 subunits
- **Read-only directory support**: The file system ignores all errors (saying "sector already written") when trying to update directory/user/object entries, but returns an error if modification of file data is attempted
- Creation of new files or versions and any modification of entries (rename, set access, etc.) returns an error
- The whole directory structure must be written to the optical disk in one operation via DEVICE-COPY
- Single file copy or MULTI-USER-COPY is not possible
- Maximum 4 optical disk drives = 4 GB online capacity
- For best performance, files on optical disk should be contiguous
- Directory size per subunit should not be less than 125 MB to utilize total capacity

### 14.5 Defining Directory Size on SCSI Disks

SCSI disk directory sizes are computed from total disk capacity divided by number of subunits, resulting in various sizes. Use `@SET-MASS-STORAGE-SIZE` to match directory sizes (e.g., to match SMD 450 MB directory):

```
@SET-MASS-STORAGE-SIZE DISC-2-OPTICAL-1,,,220584
```

Cannot change directory size if a directory is already entered on the device.

**Source:** K-version, Sections 22.1-22.9, Lines 13045-13633

### 14.6 L-version SCSI Updates

SCSI adaptor can connect to a DOMINO controller, making the ND-100 less of a bottleneck in high-volume disk I/O operations.

**L-version limits:**

| Resource | Limit |
|----------|-------|
| SCSI host adaptors | 3 (reduced from 4 in K) |
| SCSI disk units | 8 (reduced from 14 in K) |
| SCSI streamer units | 2 |
| SCSI magnetic tape units | 3 |
| SCSI optical disk units | 2 (reduced from 4 in K) |

**Source:** L-version, Lines 374-397

---

## 15. File System Segments (J-version)

### 15.1 Segment Layout

| Seg. No. | Name | Address Range | PT | Description |
|----------|------|--------------|-----|-------------|
| 6 | S3FSCOM | 110000₈ | 0 | File system common segment |
| 12 | S3FS2SV | 140000₈ | 0 | Initial reentrant filesys seg. no.1 |
| 22 | S3RFUS1 | 110000₈ | 0 | Reentrant file user segment no.1 |
| 24 | S3FSRS1 | 140000₈ | 0 | File system reentrant segment no.1 |
| 25 | S3FSRS2 | 140000₈ | 0 | File system reentrant segment no.2 |
| 26 | S3RFUS2 | 110000₈ | 0 | Reentrant file user segment no.2 |
| 40 | S3FUFRT | 164000₈ | 0 | File user data segment for RT-prog. |

### 15.2 MACM-AREA:DATA Layout (disk addresses in pages, octal, relative to directory start)

| Start | End | Content |
|-------|-----|---------|
| 100₈ | 132₈ | File system, segments 6 and 24 |
| 132₈ | 137₈ | Error program |
| 137₈ | 145₈ | ND-NET file copy |
| 145₈ | 17₈ | Command segment (segment 3) |

Displacements when patching:
- Command segment: -110000₈
- File system seg. 6 + 24: 2000₈

**Source:** J-version, Lines 549-628

### 15.3 Disk Mirroring Segments (L-version)

| Seg. No. | Name | Address Range | PT | Description |
|----------|------|--------------|-----|-------------|
| 73 | S3SDMWD | 2000₈:11777₈ | 1 | Save of disk mirroring WD segment |
| 74 | S3IDMWD | 2000₈:11777₈ | 1 | Image of disk mirroring WD segment |

**Source:** L-version, Lines 2569-2570

---

## 16. File System Related RT-Programs

| Program | Purpose | Version |
|---------|---------|---------|
| FIXRT | Monitor call/command FIXC execution | J |
| FSART | Administration of the file servers (COSMOS remote file access) | J |
| RTD1L | Buffer transfer program for DISC-ACCESS-LOG | J |
| RTRFA | Does remote file access for RT-programs (COSMOS) | J |
| RWR1T | Block data transfer (activated from RFILE, WFILE, RPAGE, WPAGE) | J |
| RWR2T | Open file from RT-programs | J |
| DIMMD | Disk mirroring facility (FTX) | L |
| REVIVE | Fault Tolerant eXtension | L |
| RTREC | Reconnect SINTRAN file system directory to DOMINO controller | L |
| FLUSH | Write (flush) file-system cache to disk | N |

**Source:** J-version, Lines 656-694; L-version, Lines 2631-2687; N-version, Line 2249

---

## 17. Directory Semaphore Changes (M-version)

### 17.1 Newly Released Semaphores (No Longer Used)

| Semaphore | Previously Used For |
|-----------|-------------------|
| 505 | User-file-buffer semaphore |
| 506 | Object-file-buffer semaphore |
| 545, 547, 551, 553 | ECC disk controller 3, units 0-3, bit-file semaphore |
| 1102 | ECC disk controller 1, unit 0, bit-file semaphore |
| 1114 | ECC disk controller 4, unit 0, bit-file semaphore |
| 2560-2577 | Directory entries 24-31, directory and bit-file semaphores |

### 17.2 New Directory Semaphore Range

| Semaphore Range | Description |
|----------------|-------------|
| 2501-2551₈ | Directory entry numbers 1-41, directory semaphore |

**Source:** M-version, Lines 2681-2790

---

## 18. Backward Compatibility and Product Requirements

### 18.1 Object Index > 255 Support Requirements (L-version)

| Product | Required Version | Notes |
|---------|-----------------|-------|
| Backup System | Version H (ND-210337) | Handle files with file index > 255 |
| Backup System | Version I | Handle SCSI streamer tape drives |
| Backup System + DMA server | Revision I05 | Handle SCSI optical disks and magnetic tape |
| File Manager | Version C (ND-211075) | Handle files with index > 255 (versions A/B do not) |
| File System Investigator | Version 0 (ND-210628) | Handle files with index > 255 |
| Disk Mirroring | Version E (ND-210855) | Required for L-version |

**Source:** L-version, Lines 3572-3593

### 18.2 File System Verification (N-version)

Version B of the File System Verification (ND-211073) is recommended as it includes a significantly improved user interface and enhanced functions.

**Source:** N-version, Line 2370

---

## 19. @EXPAND-DIRECTORY (M-version)

Expand a directory on a SCSI or DOMINO controller disk, or reposition the bit-file to another free area within the directory (applies to all hard disk types).

### Restrictions

1. Available only to user SYSTEM
2. Expansion only for SCSI or DOMINO controller disks
3. Bit-file repositioning available for all hard disk types (when `<number of pages>` = 0)
4. Not available for floppy disks
5. On DOMINO controller: must remove definition (@RELEASE-DIRECTORY, @DELETE-MASS-STORAGE-UNIT), use BDIO service program to change directory size on disk, then @ENTER-DIRECTORY and @EXPAND-DIRECTORY

**Source:** M-version, Section 2.3.1, Lines 1041-1063

---

## Appendix A: Version Cross-Reference

### File System Feature Introduction Timeline

| Feature | J | K | L | M | N |
|---------|---|---|---|---|---|
| FSMTY MON 327 | Func 1 | Func 2-4 | Func 5-12₈ | Func 13₈-14₈ | - |
| IOPEN MON 351 | - | - | - | Introduced | - |
| Object files >256 | - | Introduced | - | - | - |
| Directory entry 30₈ words | - | Introduced | - | - | Neg. displacements |
| EXSECURITY bits | 0-3 | - | 4-5 | 6 | - |
| Disk access log | Introduced | - | - | - | - |
| Event-log utility | - | - | - | Introduced | Extended |
| Default file access | Changed | - | - | - | - |
| Dirty cache / DELWR | - | - | - | - | Introduced |
| Device buffers max | - | - | 64 | 128 | - |
| BDIO pools max | - | - | - | 64 (gen 6) | - |
| @EXPAND-DIRECTORY | - | - | - | Introduced | - |
| @COPY-DIRECTORY | Available | Available | Available | **Removed** | - |
| Spooling own segment | Moved | - | Max 6 pages | - | - |
| SCSI/optical disk | - | Introduced | DOMINO support | - | - |
| File System Verification | - | - | - | - | Version B rec. |

---

**Last Updated**: 2026-02-09
**Sources**: Official Norsk Data SINTRAN III Release Information documents (J through N versions)
