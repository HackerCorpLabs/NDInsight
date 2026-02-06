# MPM5 Memory Detection and Identification

**How SINTRAN Identifies Which Memory is 5MPM for ND-500 Shared Memory Communication**

**Version:** 1.0  
**Date:** 2025-01-XX  
**Status:** Complete  
**Source:** Analysis of SINTRAN III source code (`PH-P2-OPPSTART.NPL`)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Detection Process](#2-detection-process)
3. [MEMARRAY Structure](#3-memarray-structure)
4. [MPM5 Identification Algorithm](#4-mpm5-identification-algorithm)
5. [ADRZERO Calculation](#5-adrzero-calculation)
6. [Concrete Identification Method](#6-concrete-identification-method)
7. [Emulator Implementation Guide](#7-emulator-implementation-guide)

---

## 1. Overview

### 1.1 The Problem

When SINTRAN boots with an ND-500 coprocessor, it must identify which physical memory pages are MPM5 (5MPM) - the shared multiport memory used for ND-100/ND-500 communication.

**Key Questions:**
- How does SINTRAN know which memory is MPM5?
- How is ADRZERO (5MPM base address) determined?
- What is the concrete algorithm for identifying MPM5 pages?

### 1.2 The Answer

**SINTRAN uses a two-stage process:**

1. **Initial Assignment:** All detected memory is initially marked as MPM5 (`KMPM5 = 4₈`)
2. **Refinement:** Other memory types (MPM3, MPM4, Local, PIOC) are detected and overwrite MPM5 markings
3. **Final Identification:** Remaining pages still marked as MPM5 are the actual 5MPM memory

**ADRZERO is set to:** The first page found that is marked as MPM3, MPM4, or MPM5 in MEMARRAY.

---

## 2. Detection Process

### 2.1 Boot Sequence

```npl
% From PH-P2-OPPSTART.NPL boot sequence

% Step 1: Physical memory scan (builds TMMAP bitmap)
CURRPAGE:=1000₈
DO WHILE CURRPAGE << ENDPAGE
    CALL PHYSPTEST    % Test if page exists
    IF page_exists THEN
        SETBIT TMMAP(page_number)
    FI
    CURRPAGE+100=:CURRPAGE
OD

% Step 2: Initial MPM5 assignment (lines 2396-2406)
RETU: FOR X:=0 TO 17 DO     % ALL FOUND MEMORY IS INITIALLY SET TO MPM5 MEMORY
    IF TMMAP(X)><0 THEN
        X=:CSAVX; A=:XA:=X SH 12=:CURRPAGE
        FOR X:=-20 DO
            IF XA BIT "0" THEN               % MEMORY BANK EXISTS
                T:=KMPM5; A:=CURRPAGE; CALL SMEMTYPE  % Mark as MPM5
            FI; XA SHZ -1=:XA
            CURRPAGE+100=:CURRPAGE
        OD; X:=CSAVX
    FI
OD

% Step 3: Controller-level detection (refines memory types)
% - BUS EXPANDER test (IOX 100000)
% - MPM3 controller test (IOX 750)
% - ECCR test (IOX 100115) → marks as Local (KMECCR)
% - BUSC scan (IOX 100200+) → marks as MPM4 (KMPM4)
% - PIOC configuration → marks as PIOC (KMPIOC)

% Step 4: Page-level mapping
% - MPM3MAP: Tests pages with IOX 751 → marks as MPM3 (KMPM3)
% - MPM4MAP: Tests pages with ECCR → marks as Local (KMECCR)

% Step 5: MPM5 refinement (lines 2510-2519)
FMPM5: X:=MEMARRAY; A:=X+200=:D
    DO WHILE X<<D
        T:=MBMEMARRAY; *LDATX
        IF A SHZ -10=KMPM5 GO SMPM5    % Check upper byte
        T:=MBMEMARRAY; *LDATX
        IF A/\377=KMPM5 GO SMPM5       % Check lower byte
        X+1
    OD; GO MEMFINE
SMPM5: MEMTYPE BONE BMPM5=:MEMTYPE
    GO MEMFINE

% Step 6: Find first MPM page for ADRZERO (lines 2492-2498)
FN5MEM:
    A:=0; CALL FPMPMPAGE; A:=-1; A=:FPIMPM    % Find first MPM page
    IF X:=PN500D><0 AND X.ADRZERO=-1 THEN
        CURRPAGE; CALL FPMPMPAGE; A=:CURRPAGE
        IF PN500D.ADRZERO=-1 THEN CURRPAGE=:X.ADRZERO FI  % Set ADRZERO
    FI
```

### 2.2 Memory Type Detection Order

| Step | Process | Memory Types Identified | Code Reference |
|------|---------|------------------------|-----------------|
| 1 | Initial Assignment | **All memory → MPM5** | Lines 2396-2406 |
| 2 | BUS EXPANDER Test | MPM4 indicator | Line 2410 |
| 3 | MPM3 Controller Test | MPM3 controller present | Line 2413 |
| 4 | ECCR Test | Local/OnCpu memory | Line 2415 |
| 5 | BUSC Scan | MPM4 memory ranges | Lines 2418-2433 |
| 6 | MPM3MAP | MPM3 pages | Lines 3857-3860 |
| 7 | MPM4MAP | Local pages | Lines 3851-3855 |
| 8 | PIOC Configuration | PIOC memory | Lines 2450-2461 |
| 9 | MPM5 Refinement | **Remaining MPM5 pages** | Lines 2510-2519 |

**Key Insight:** MPM5 is the **default** memory type. Other types overwrite it during detection.

---

## 3. MEMARRAY Structure

### 3.1 Purpose

**MEMARRAY** stores the memory type code for each physical page in the ND-100's 24-bit address space.

### 3.2 Format

**Structure:** Array of 16-bit words  
**Indexing:** One entry per 128 pages (100₈ pages)  
**Encoding:** Packed format - two pages per word

**Bit Layout:**

```
MEMARRAY Entry (16-bit word):
┌─────────────────────────────────────┐
│ Bits 15-8: Memory type code          │
│ Bits 7-0:  Memory type code          │
└─────────────────────────────────────┘
```

**Index Calculation:**
```npl
MEMARRAY_index = (page_number >> 7) + MEMARRAY
```

**Byte Selection (from FPMPMPAGE line 2483):**
- **Bit 6 of page number = 1:** Extract lower byte (`A/\377`) - bits 7-0
- **Bit 6 of page number = 0:** Extract upper byte (`A SHZ -10`) - bits 15-8

**Storage (from SMEMTYPE lines 3884-3887):**
- **Bit 6 of page number = 1:** Store in upper byte (`A/\177400\/TR`) - bits 15-8
- **Bit 6 of page number = 0:** Store in lower byte (`A/\377; T:=TR SH 10; A\/T`) - bits 7-0

**Note:** The encoding is determined by bit 6 of the page number:
- Pages 64-127 (bit 6=1): Stored in upper byte, read from lower byte
- Pages 0-63, 128-191, etc. (bit 6=0): Stored in lower byte, read from upper byte

### 3.3 Memory Type Codes

| Symbol | Value (Octal) | Value (Dec/Hex) | Memory Type |
|--------|--------------|-----------------|-------------|
| `KMPM3` | 000001₈ | 1 / 0x01 | MPM3 memory |
| `KMPM4` | 000002₈ | 2 / 0x02 | MPM4 memory |
| `KMPM5` | 000004₈ | 4 / 0x04 | **MPM5 memory** |
| `KMECCR` | 000010₈ | 8 / 0x08 | Local/OnCpu memory |
| `KMPIOC` | 000020₈ | 16 / 0x10 | PIOC memory |

### 3.4 SMEMTYPE Routine

**Purpose:** Store memory type code for a page in MEMARRAY

**Parameters:**
- **A:** Physical page number
- **T:** Memory type code (KMPM3, KMPM4, KMPM5, KMECCR, KMPIOC)

**Implementation:**
```npl
% From PH-P2-OPPSTART.NPL, lines 3880-3891
SUBR SMEMTYPE
SMEMTYPE: TAD=:TRARDR; X=:XR
    A=:D SHZ -7+MEMARRAY=:X; T:=MBMEMARRAY; *LDATX    % Load MEMARRAY entry
    IF D BIT 6 THEN
        A/\177400\/TR    % Update upper byte (bits 15-8)
    ELSE
        A/\377; T:=TR SH 10; A\/T; T:=MBMEMARRAY    % Update lower byte (bits 7-0)
    FI; *STATX    % Store updated entry
    X:=XR; TAD:=TRARDR
    EXIT
```

**Process (from lines 3883-3887):**
1. Calculate MEMARRAY index: `(page >> 7) + MEMARRAY`
2. Load current MEMARRAY entry
3. Check bit 6 of page number:
   - **Bit 6 = 1:** Update upper byte (`A/\177400\/TR`) - bits 15-8
   - **Bit 6 = 0:** Update lower byte (`A/\377; T:=TR SH 10; A\/T`) - bits 7-0
4. Store updated entry back to MEMARRAY

---

## 4. MPM5 Identification Algorithm

### 4.1 FPMPMPAGE Routine

**Purpose:** Find the first page in MPM memory (MPM3, MPM4, or MPM5)

**Entry Points:**
- **FPMPMPAGE:** Find first page **IN** MPM memory (K=0)
- **LPMPMPAGE:** Find first page **NOT IN** MPM memory (K=1)

**Implementation:**
```npl
% From PH-P2-OPPSTART.NPL, lines 2477-2489
FPMPMPAGE: K:="0"; GO FMPMFELLS    % FIND FIRST PAGE IN MPM MEMORY
LPMPMPAGE: K:=1                     % FIND FIRST PAGE NOT IN MPM MEMORY

FMPMFELLS:
    A=:D                            % D = starting page number
    DO WHILE D<<ENDPAGE
        A:=D SHZ -7+MEMARRAY=:X; T:=MBMEMARRAY; *LDATX    % Load MEMARRAY entry
        IF D BIT 6 THEN A/\377 ELSE A SHZ -10 FI    % Extract memory type code
        IF A=KMPM3 OR A=KMPM4 OR A=KMPM5 THEN
            IF K NBIT THEN A:=D; EXITA FI    % FIRST PAGE IN MPM PART
        ELSE
            IF K THEN A:=D; EXITA FI         % FIRST PAGE IN NOT-MPM PART
        FI; A:=100; D+A                       % Next page (increment by 128)
    OD; A:=D; EXIT
```

**Algorithm:**
1. Start at page number D (passed in A register)
2. Calculate MEMARRAY index: `(D >> 7) + MEMARRAY`
3. Load MEMARRAY entry
4. Extract memory type code (from line 2483):
   - **If bit 6 of D = 1:** Extract lower byte (`A/\377`) - bits 7-0
   - **If bit 6 of D = 0:** Extract upper byte (`A SHZ -10`) - bits 15-8
5. Check if memory type is MPM (KMPM3, KMPM4, or KMPM5):
   - **If K=0 (FPMPMPAGE):** Return first page that IS MPM
   - **If K=1 (LPMPMPAGE):** Return first page that is NOT MPM
6. Increment D by 128 (100₈) and continue

**Note:** The increment by 128 is because each MEMARRAY entry covers 128 pages. The routine scans in 128-page chunks.

### 4.2 MPM5 Refinement (FMPM5)

**Purpose:** Confirm that MPM5 memory exists (set MEMTYPE |= BMPM5)

**Implementation:**
```npl
% From PH-P2-OPPSTART.NPL, lines 2510-2519
FMPM5: X:=MEMARRAY; A:=X+200=:D    % Scan first 200₈ (128 decimal) entries
    DO WHILE X<<D
        T:=MBMEMARRAY; *LDATX      % Load MEMARRAY entry
        IF A SHZ -10=KMPM5 GO SMPM5    % Check upper byte (even pages)
        T:=MBMEMARRAY; *LDATX      % Reload entry
        IF A/\377=KMPM5 GO SMPM5       % Check lower byte (odd pages)
        X+1                         % Next entry
    OD; GO MEMFINE
SMPM5: MEMTYPE BONE BMPM5=:MEMTYPE    % Set MPM5 flag
    GO MEMFINE
```

**Algorithm:**
1. Scan MEMARRAY entries from 0 to 200₈ (128 decimal entries = 16,384 pages)
2. For each entry:
   - Check upper byte (`A >> 10`) for KMPM5 (4₈)
   - Check lower byte (`A & 377₈`) for KMPM5 (4₈)
3. If KMPM5 found, set `MEMTYPE |= BMPM5`
4. Continue to MEMFINE

**Purpose:** This confirms that MPM5 memory exists in the system (used for ND-500 communication).

---

## 5. ADRZERO Calculation

### 5.1 ADRZERO Purpose

**ADRZERO** is the **page number** of the first MPM page (MPM3, MPM4, or MPM5) used for ND-500 communication.

**Important:** ADRZERO is a **PAGE NUMBER**, not a byte address!

### 5.2 ADRZERO Setting

**Two Mechanisms:**

**Mechanism 1: MEMDEF from ND-500**
```npl
% From 5P-P2-MON60.NPL:587 (CHMEMDEF routine)
5D12=:ADRZERO    % ND-500 sends its view of shared memory
```
During Memory Definition operation, ND-500 sends ADRZERO via message parameter 5D12.

**Mechanism 2: Startup Scan (if not set)**
```npl
% From PH-P2-OPPSTART.NPL, lines 2492-2498
FN5MEM:
    A:=0; CALL FPMPMPAGE; A:=-1; A=:FPIMPM    % Find first MPM page
    IF X:=PN500D><0 AND X.ADRZERO=-1 THEN
        CURRPAGE; CALL FPMPMPAGE; A=:CURRPAGE  % First MPM page in memory part
        IF PN500D.ADRZERO=-1 THEN CURRPAGE=:X.ADRZERO FI  % Set ADRZERO
    FI
```

**Process:**
1. Call `FPMPMPAGE` starting at page 0
2. Find first page marked as MPM3, MPM4, or MPM5
3. If ADRZERO = -1 (not configured), set ADRZERO to this page number

**ADRZERO = -1 means:** "5MPM not yet configured"

### 5.3 ADRZERO to Address Conversion

**ND-100 Context (word addressing):**
```
Physical Word Address = ADRZERO × 1024
```

**ND-500 Context (byte addressing):**
```
Physical Byte Address = ADRZERO × 4096
```

**Example:**
```
If ADRZERO = 2048 (page number):
  ND-100: 2048 × 1024 = 2,097,152 words = word address 20000000₈
  ND-500: 2048 × 4096 = 8,388,608 bytes = 0x800000
```

**NPL Code:**
```npl
% From MP-P2-N500.NPL:170
A:="N500DF".ADRZERO=:D:=0; AD SH 12    % Load ADRZERO, shift left 12
% Shift left 12 = multiply by 4096 = convert page number to ND-500 byte address
```

### 5.4 5MBBANK Calculation

**5MBBANK** is the bank number containing 5MPM.

**Calculation:**
```npl
5MBBANK = ADRZERO / 256    % 256 pages per bank
```

**Usage:** Used for bank register selection when accessing 5MPM.

---

## 6. Concrete Identification Method

### 6.1 Step-by-Step Algorithm

**For Emulator Implementation:**

```python
def identify_mpm5_memory(memarray, endpage):
    """
    Identify which memory pages are MPM5 (5MPM) for ND-500 communication.
    
    Args:
        memarray: Array of 16-bit words storing memory type codes
        endpage: Maximum page number to scan
    
    Returns:
        List of page numbers that are MPM5
    """
    KMPM5 = 4  # MPM5 memory type code
    
    mpm5_pages = []
    
    # Scan MEMARRAY entries
    for memarray_index in range(len(memarray)):
        entry = memarray[memarray_index]
        
        # Extract upper and lower bytes
        upper_byte = (entry >> 8) & 0xFF  # Bits 15-8
        lower_byte = entry & 0xFF         # Bits 7-0
        
        # Calculate page range for this entry
        base_page = memarray_index * 128  # 128 pages per entry
        
        # Check each page in this entry
        for page_offset in range(128):
            page_number = base_page + page_offset
            
            if page_number >= endpage:
                break
            
            # Determine which byte to use based on bit 6 (from FPMPMPAGE line 2483)
            if (page_number >> 6) & 1 == 1:
                # Bit 6 = 1: read from lower byte
                memory_type = lower_byte
            else:
                # Bit 6 = 0: read from upper byte
                memory_type = upper_byte
            
            # Check if this page is MPM5
            if memory_type == KMPM5:
                mpm5_pages.append(page_number)
    
    return mpm5_pages


def find_adrzero(memarray, start_page=0, endpage=16384):
    """
    Find ADRZERO - the first MPM page (MPM3, MPM4, or MPM5).
    
    Args:
        memarray: Array of 16-bit words storing memory type codes
        start_page: Starting page number to scan
        endpage: Maximum page number to scan
    
    Returns:
        Page number of first MPM page, or None if not found
    """
    KMPM3 = 1
    KMPM4 = 2
    KMPM5 = 4
    
    # Scan pages in 128-page chunks (MEMARRAY entry size)
    page = start_page
    while page < endpage:
        memarray_index = page >> 7  # Divide by 128
        page_offset = page & 0x7F   # Page within entry (0-127)
        
        if memarray_index >= len(memarray):
            break
        
        entry = memarray[memarray_index]
        
        # Extract memory type code
        if page_offset % 2 == 0:
            # Even page: use upper byte
            memory_type = (entry >> 8) & 0xFF
        else:
            # Odd page: use lower byte
            memory_type = entry & 0xFF
        
        # Check if this is MPM memory
        if memory_type in (KMPM3, KMPM4, KMPM5):
            return page
        
        # Increment by 128 (next MEMARRAY entry)
        page = ((page >> 7) + 1) << 7
    
    return None
```

### 6.2 Complete Detection Flow

**For Emulator:**

```python
class MPM5Detector:
    """Detects and identifies MPM5 memory for ND-500 communication."""
    
    # Memory type codes
    KMPM3 = 1    # MPM3 memory
    KMPM4 = 2    # MPM4 memory
    KMPM5 = 4    # MPM5 memory (5MPM)
    KMECCR = 8   # Local/OnCpu memory
    KMPIOC = 16  # PIOC memory
    
    def __init__(self, memarray, endpage=16384):
        """
        Initialize detector.
        
        Args:
            memarray: Array of 16-bit words (MEMARRAY structure)
            endpage: Maximum page number to scan
        """
        self.memarray = memarray
        self.endpage = endpage
    
    def get_memory_type(self, page_number):
        """
        Get memory type code for a specific page.
        
        Args:
            page_number: Physical page number (0-16383)
        
        Returns:
            Memory type code (KMPM3, KMPM4, KMPM5, KMECCR, KMPIOC)
        """
        if page_number >= self.endpage:
            return None
        
        memarray_index = page_number >> 7  # Divide by 128
        page_offset = page_number & 0x7F   # Page within entry
        
        if memarray_index >= len(self.memarray):
            return None
        
        entry = self.memarray[memarray_index]
        
        # Extract memory type code (from FPMPMPAGE line 2483)
        if (page_number >> 6) & 1 == 1:
            # Bit 6 = 1: read from lower byte (bits 7-0)
            memory_type = entry & 0xFF
        else:
            # Bit 6 = 0: read from upper byte (bits 15-8)
            memory_type = (entry >> 8) & 0xFF
        
        return memory_type
    
    def find_first_mpm_page(self, start_page=0):
        """
        Find first page marked as MPM3, MPM4, or MPM5.
        
        This is used to determine ADRZERO.
        
        Args:
            start_page: Starting page number to scan
        
        Returns:
            Page number of first MPM page, or None if not found
        """
        # Scan in 128-page chunks (MEMARRAY entry size)
        page = start_page
        while page < self.endpage:
            memory_type = self.get_memory_type(page)
            
            if memory_type in (self.KMPM3, self.KMPM4, self.KMPM5):
                return page
            
            # Increment to next MEMARRAY entry boundary
            page = ((page >> 7) + 1) << 7
        
        return None
    
    def get_all_mpm5_pages(self):
        """
        Get all pages marked as MPM5 (5MPM).
        
        Returns:
            List of page numbers that are MPM5
        """
        mpm5_pages = []
        
        for page in range(self.endpage):
            memory_type = self.get_memory_type(page)
            if memory_type == self.KMPM5:
                mpm5_pages.append(page)
        
        return mpm5_pages
    
    def calculate_adrzero(self):
        """
        Calculate ADRZERO (first MPM page for ND-500).
        
        Returns:
            ADRZERO page number, or None if no MPM memory found
        """
        return self.find_first_mpm_page(start_page=0)
    
    def calculate_5mbbank(self, adrzero):
        """
        Calculate 5MBBANK (bank number containing 5MPM).
        
        Args:
            adrzero: ADRZERO page number
        
        Returns:
            Bank number (0-63)
        """
        if adrzero is None:
            return None
        return adrzero >> 8  # Divide by 256 (256 pages per bank)
```

### 6.3 Usage Example

```python
# Example: Identify MPM5 memory

# Assume MEMARRAY has been populated during boot
memarray = [0x0404, 0x0404, 0x0404, ...]  # Example: pages 0-127 all MPM5
endpage = 16384  # 16MW scan limit

detector = MPM5Detector(memarray, endpage)

# Find ADRZERO (first MPM page)
adrzero = detector.calculate_adrzero()
print(f"ADRZERO = page {adrzero}")

# Calculate 5MBBANK
mbbank = detector.calculate_5mbbank(adrzero)
print(f"5MBBANK = {mbbank}")

# Get all MPM5 pages
mpm5_pages = detector.get_all_mpm5_pages()
print(f"MPM5 pages: {mpm5_pages}")

# Convert ADRZERO to addresses
if adrzero is not None:
    nd100_word_addr = adrzero * 1024
    nd500_byte_addr = adrzero * 4096
    print(f"ND-100 word address: {nd100_word_addr:o}₈ ({nd100_word_addr} words)")
    print(f"ND-500 byte address: 0x{nd500_byte_addr:X} ({nd500_byte_addr} bytes)")
```

---

## 7. Emulator Implementation Guide

### 7.1 Key Points for Emulation

1. **MEMARRAY Structure:**
   - One 16-bit word per 128 pages
   - Upper byte: Even pages (0, 2, 4, ...)
   - Lower byte: Odd pages (1, 3, 5, ...)

2. **Memory Type Codes:**
   - `KMPM5 = 4₈` (4 decimal, 0x04 hex)
   - Pages still marked as KMPM5 after refinement are 5MPM

3. **ADRZERO:**
   - Page number (not byte address)
   - First page marked as MPM3, MPM4, or MPM5
   - Can be set via MEMDEF from ND-500 or found via FPMPMPAGE

4. **5MBBANK:**
   - Bank number = ADRZERO / 256
   - Used for bank register selection

### 7.2 Detection Sequence

**During SINTRAN Boot:**

1. **Physical Memory Scan:** Build TMMAP bitmap
2. **Initial Assignment:** Mark all memory as MPM5 (`KMPM5 = 4₈`)
3. **Controller Detection:** Detect MPM3, MPM4, Local, PIOC
4. **Page-Level Mapping:** Refine memory types
5. **MPM5 Refinement:** Confirm remaining MPM5 pages
6. **ADRZERO Calculation:** Find first MPM page

**For Emulator:**

1. **Initialize MEMARRAY:** Mark all detected memory as MPM5
2. **Detect Controllers:** Update MEMARRAY based on hardware detection
3. **Identify MPM5:** Pages still marked as KMPM5 are 5MPM
4. **Set ADRZERO:** First MPM page found

### 7.3 Validation

**To verify MPM5 identification:**

1. **Check MEMARRAY:** Pages marked as `KMPM5 = 4₈` are 5MPM
2. **Check ADRZERO:** Should point to first MPM page
3. **Check 5MBBANK:** Should be ADRZERO / 256
4. **Check ND-500 Interface:** 3022 interface should be configured with ADRZERO

---

## 8. Summary

### 8.1 How SINTRAN Identifies MPM5 Memory

1. **All memory initially marked as MPM5** (`KMPM5 = 4₈`)
2. **Other memory types overwrite MPM5** during detection
3. **Remaining MPM5 pages are 5MPM** for ND-500 communication
4. **ADRZERO = first MPM page** (MPM3, MPM4, or MPM5)

### 8.2 Concrete Identification

**For a given page number:**

```python
memarray_index = page_number >> 7
page_offset = page_number & 0x7F
entry = memarray[memarray_index]

if page_offset % 2 == 0:
    memory_type = (entry >> 8) & 0xFF  # Upper byte
else:
    memory_type = entry & 0xFF         # Lower byte

is_mpm5 = (memory_type == 4)  # KMPM5 = 4₈
```

**To find ADRZERO:**

```python
adrzero = find_first_mpm_page(memarray, start_page=0)
```

**To get all MPM5 pages:**

```python
mpm5_pages = [p for p in range(endpage) 
              if get_memory_type(memarray, p) == 4]
```

---

**End of Document**
