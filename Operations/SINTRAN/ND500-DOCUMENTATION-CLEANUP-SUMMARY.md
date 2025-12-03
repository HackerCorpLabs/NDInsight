# ND-500 Documentation Cleanup Summary

**Date:** 2025-12-03
**Reason:** Remove AI-generated assumptions and replace with ACTUAL SINTRAN source code analysis

---

## What Was Wrong

All previous ND-500 documentation was based on **assumptions**, **emulator code**, and **logical inferences** rather than actual SINTRAN III NPL source code.

### The Critical Error: TAG Registers

**CLAIMED:** TAG registers (RTAG5/LTAG5) are used for ND-500 ↔ ND-100 communication with a protocol:
- 16-bit values
- High byte = operation code (0x01 = MON_CALL, 0x02 = COMPLETE, etc.)
- Low byte = process number
- Hardware interrupt signaling

**REALITY (from actual NPL source code):**
- TAG registers **ARE NEVER USED** in SINTRAN III
- Searched entire NPL source: **ZERO occurrences** of RTAG5 or LTAG5
- Registers are **defined** in symbol table but **unused** in code
- Communication uses **shared memory polling**, not TAG interrupts

---

## Files Deleted

### 1. ND500-TAG-REGISTER-COMPLETE-EXPLANATION.md (39KB)
- **Reason:** Entirely about TAG register protocol that doesn't exist
- **Content:** Detailed explanation of TAG codes 0x00-0x0F, timing, hardware signals
- **Problem:** 100% fiction - none of this exists in SINTRAN source code

### 2. ND500-MON-CALL-EXECUTION-DETAILED.md (36KB)
- **Reason:** 88 TAG references claiming TAG-based protocol
- **Content:** Detailed flow showing TAG signal propagation, TAG codes, IOX TAG reads
- **Problem:** Claims ND-500 writes TAG → ND-100 reads TAG → processes request (FALSE)

### 3. ND500-PROTOCOL-BYTE-LEVEL-DETAIL.md (40KB)
- **Reason:** 87 TAG references with byte-level TAG protocol
- **Content:** Step-by-step TAG write operations, TAG signal timing (35 nanoseconds)
- **Problem:** Invented protocol with specific timing that doesn't match source code

### 4. ND500-MONITOR-CALL-ARCHITECTURE.md (25KB)
- **Reason:** 27 TAG references, architectural diagrams showing TAG flow
- **Content:** Mermaid diagrams with TAG signal paths, TAG register operations
- **Problem:** Architecture is fundamentally wrong - no TAG protocol exists

### 5. ND500-INITIALIZATION-AND-SCHEDULING.md (73KB)
- **Reason:** 50 TAG references in initialization sequences
- **Content:** Claims "ND-100 polls TAG registers" and "TAG registers operational"
- **Problem:** SINTRAN doesn't poll TAG registers - polls shared memory queue

### 6. ND500-SEGMENT31-INSTRUCTION-DECODE.md (30KB)
- **Reason:** 71 TAG references with IOX code examples
- **Content:** Example code showing IOX_Read(RTAG5), IOX_Write(LTAG5)
- **Problem:** These IOX operations never occur in actual SINTRAN code

---

## Files Kept (Correct)

### 1. ND500-REAL-SOURCE-CODE-ANALYSIS.md (11KB) ✅
- **Source:** Actual SINTRAN NPL source code (CC-P2-N500.NPL, MP-P2-N500.NPL, etc.)
- **Content:** What ACTUALLY happens based on NPL code line-by-line analysis
- **Key Findings:**
  - NO TAG register usage
  - Shared memory (5MPM) for message passing
  - ND-100 self-interrupts on level 12 to activate driver
  - Driver polls execution queue (MAILINK)
  - IOX registers used: RSTA5, LCON5, LMAR5, UNLC5, TERM5, SLOC5
  - IOX registers unused: RTAG5, LTAG5

### 2. ND500-SEGMENT31-DETECTION-SOURCE-ANALYSIS.md (12KB) ✅
- **Source:** ND-500 Reference Manual + emulator code analysis
- **Content:** Source transparency about O-bit in capability registers
- **Key Findings:**
  - O-bit (bit 14) in program capability register is REAL (from manual)
  - O-bit indicates "other machine" segment
  - CALL/CALLG check O-bit during instruction decode
  - Clear distinction between FACTS (from manual) vs INFERENCES
- **Note:** Does NOT claim TAG registers are used

---

## What We Now Know (From Real Source Code)

### Actual ND-500 Communication Mechanism:

1. **Message Passing via Shared Memory (5MPM)**
   - Each process has message buffer in 5MPM (multiport memory)
   - Message buffers linked in execution queue (MAILINK head pointer)
   - Messages contain: flags, function code (MICFU), parameters

2. **ND-100 Driver Activation**
   ```npl
   XLOWACT500:
       A:=B; *IRW LV12B DB           % Write CPU datafield to Level 12 B
       "5STDRIV"; *IRW LV12B DP      % Write driver address to Level 12 P
       LV12; *MST PID; EXIT          % Trigger interrupt level 12
   ```
   - ND-100 **triggers its own interrupt**
   - Not triggered by ND-500 hardware
   - Driver (5STDRIV) runs on interrupt level 12

3. **Driver Polls Execution Queue**
   ```npl
   5STDRIV:
       X:=MAILINK                     % Scan EX-QUEUE from "MAR"
       DO
       WHILE X><-1
          T:=5MBBANK; *LINK@3 LDDTX   % Follow linked list
       WHILE D><-1
          X:=D=:N5MESSAGE             % Process each message
          CALL CHN5STATUS; GO N500ERR
       OD
   ```
   - Polling-based, not interrupt-driven from ND-500
   - Scans doubly-linked list in shared memory

4. **IOX Registers Actually Used**
   - **RSTA5/LSTA5** (offset 2/3): Read/Write status register
   - **RCON5/LCON5** (offset 4/5): Read/Write control register
   - **RMAR5/LMAR5** (offset 0/1): Read/Write memory address register
   - **TERM5** (offset 7): Terminate ND-500 process
   - **UNLC5** (offset 16₈): Unlock interface
   - **SLOC5** (offset 14₈): Status lock
   - **RTAG5/LTAG5** (offset 10₈/11₈): **DEFINED BUT NEVER USED**

---

## Why This Happened

### Sources of Misinformation:

1. **Emulator Code (Interface3022-5015.cs)**
   - Community-written C# emulator invented TAG protocol
   - Seemed logical: "How else would they communicate?"
   - **But:** Emulator != Real Hardware

2. **Logical Inference**
   - TAG registers exist in symbol table
   - Must be used for something, right?
   - **But:** Defined doesn't mean used

3. **Reference Manual Ambiguity**
   - Manual mentions "other machine" bit (O-bit)
   - Manual doesn't explain complete communication protocol
   - Easy to fill gaps with assumptions

4. **AI Generation**
   - Previous documents generated by AI (me)
   - AI made logical inferences without checking source code
   - Created detailed "documentation" of non-existent protocol

---

## Lessons Learned

### ✅ ALWAYS Check Source Code
- Don't trust emulators
- Don't trust reference manuals alone
- Don't trust logical inferences
- **Read the actual implementation**

### ✅ Distinguish Facts from Inferences
- **FACT:** Found in source code or official documentation
- **INFERENCE:** Logical conclusion based on evidence
- **ASSUMPTION:** Gap filled without evidence

### ✅ Be Honest About Confidence
- If you can't prove it from source, say so
- Mark speculation clearly
- Don't present assumptions as facts

---

## Current State

### Remaining Documentation:

1. **ND500-REAL-SOURCE-CODE-ANALYSIS.md**
   - Based ONLY on SINTRAN NPL source code
   - All claims have source file + line number references
   - Clear marking: ✅ Facts vs ⚠️ Cannot Verify

2. **ND500-SEGMENT31-DETECTION-SOURCE-ANALYSIS.md**
   - Source transparency document
   - Shows exact quotes from reference manual
   - Distinguishes facts from inferences
   - No TAG register claims

### What's Correct Now:

✅ IOX register offsets (from N500-SYMBOLS.SYMB.TXT)
✅ Interrupt level 12 activation (LOWACT500 routine)
✅ Execution queue structure (ITO500XQ, IFM500XQ routines)
✅ Driver polling mechanism (5STDRIV routine)
✅ Shared memory (5MPM) usage
✅ Message buffer structure (LINK, 5MSFL, MICFU, etc.)

### What We Cannot Prove:

⚠️ TAG register protocol (not used in SINTRAN)
⚠️ ND-500 trap handler details (not in NPL source - would be in ND-500 ROM)
⚠️ Exact hardware signaling (no hardware schematics)
⚠️ Message buffer size (symbol used but value not found)

---

## Conclusion

**The previous documentation was based on dreams, not source code.**

**The new documentation is based on ACTUAL SINTRAN III NPL source code.**

All deleted files claimed TAG register protocols that **do not exist** in the real SINTRAN operating system.

The real communication mechanism is **simpler**: shared memory polling, not hardware TAG signaling.

---

## Verification

To verify these claims yourself:

```bash
# Search for TAG register usage in SINTRAN NPL source
cd /home/user/NDInsight/SINTRAN/NPL-SOURCE/NPL
grep -r "RTAG5" *.NPL    # Result: ZERO occurrences
grep -r "LTAG5" *.NPL    # Result: ZERO occurrences

# Check symbol table
grep "RTAG5\|LTAG5" ../SYMBOLS/N500-SYMBOLS.SYMB.TXT
# Result: Defined at offsets 10₈ and 11₈ - but never used

# Check actual driver activation
grep -A5 "LOWACT500:" CC-P2-N500.NPL
# Result: ND-100 self-interrupt, no TAG write

# Check driver message processing
grep -A10 "5STDRIV:" MP-P2-N500.NPL
# Result: Polls MAILINK queue, no TAG read
```

**The source code doesn't lie.**
