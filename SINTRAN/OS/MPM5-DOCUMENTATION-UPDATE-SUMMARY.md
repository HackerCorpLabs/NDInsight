# MPM5 Documentation Update Summary

**Date:** October 17, 2025  
**Based on:** Official MPM 5 Technical Description Manual (ND-10.004.01)  
**Status:** ✅ Complete

---

## 🎯 Key Discovery

**The ND-500 is a BYTE-ORIENTED CPU, not a 32-bit word-oriented CPU!**

The "32-bit" terminology in MPM5 documentation refers to **memory bus width** (bandwidth optimization), NOT CPU architecture.

---

## 📋 Documents Updated

### 1. **MPM5-KEY-FINDINGS.md** ✅
   - **Created:** New comprehensive reference document
   - **Content:** All technical findings from MPM5 manual
   - **Key sections:**
     - Hardware architecture (Twin 16-Bit Port, Dynamic RAM, Line Driver)
     - Address windows and BASE register operation
     - ND-500 byte-oriented architecture clarification
     - Memory bus width vs CPU architecture
     - Cache coherency mechanisms
     - Interleaving for bandwidth optimization

### 2. **06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md** ✅
   - **Updated:** Version 1.0 → 2.0
   - **Changes:**
     - Added warning section with 6 key corrections
     - Inserted section 1.2: "CRITICAL: Understanding ND-500 Architecture"
     - Clarified memory bus width terminology
     - Updated table of contents
     - Renumbered sections 1.3-1.5

### 3. **NPL-DEVELOPER-GUIDE.md** (Section 13.6) ✅
   - **Context:** Real-world patterns document
   - **Section affected:** Multi-CPU Communication Patterns
   - **Note:** May need review to ensure consistent terminology

---

## 🔑 Critical Clarifications

### CPU Architecture vs Memory Bus Width

| Aspect | ND-100 | ND-500 |
|--------|--------|--------|
| **CPU Type** | 16-bit word machine | **Byte-oriented machine** |
| **Addressability** | Word addresses | **Byte addresses** |
| **Word Size** | 16-bit words | **8-bit bytes** (but has 16/32-bit ops) |
| **Memory Bus to MPM5** | 16-bit wide channel | 32-bit wide channel |
| **Bus Width Purpose** | Fetch 2 bytes/access | Fetch 4 bytes/access (bandwidth!) |

### The "32-bit" Confusion

**WRONG interpretation:**
- "ND-500 is a 32-bit CPU with 32-bit words"

**CORRECT interpretation:**
- "ND-500 is a byte-oriented CPU that uses a 32-bit wide memory bus for bandwidth optimization"

**Analogy:**
```
x86-64:
  ✓ Byte-addressable
  ✓ 64-byte cache lines (512 bits!)
  ✗ Not a "512-bit CPU"

ND-500:
  ✓ Byte-addressable
  ✓ 32-bit memory bus (4 bytes)
  ✗ Not a "32-bit CPU"
```

---

## 🛠️ Technical Corrections

### 1. Terminology

| Old Term | New Term | Notes |
|----------|----------|-------|
| "5MPM" | "MPM5" | '5' = Generation 5, not memory type |
| "32-bit channel (ND-500)" | "32-bit **wide** channel (ND-500)" | Emphasis on bus width |
| "32-bit words" | "4-byte fetch" or "32-bit bus width" | Avoid "word" for ND-500 |
| "ND-500 accesses as 32-bit" | "ND-500 fetches 4 bytes at once" | Clearer mechanism |

### 2. Cache Coherency

**Old understanding:**
- MPM5 hardware provides cache coherency
- S flag in segment capability signals MPM5

**New understanding:**
- Cache coherency is **CPU-level** (ND-500 internal)
- S flag tells **ND-500 CPU** to bypass its cache
- MPM5 hardware has **no knowledge** of S flag
- MPM5 only provides arbitration logic

### 3. Address Translation

**Old understanding:**
- Both CPUs see same channel address
- Direct 1:1 mapping

**New understanding:**
- Each port has BASE register for address translation
- ND-100 channel address ≠ ND-500 channel address
- Both map to same **physical RAM** location
- Address windows control access range per port

### 4. Port Configuration

**From Port Control Register bit 6:**
- `Bit 6 = 0`: 32-bit **wide** data channel (ND-500) → fetches 4 bytes
- `Bit 6 = 1`: 16-bit **wide** data channel (ND-100) → fetches 2 bytes

**This is about bandwidth, not CPU word size!**

---

## 📚 New Sections Added

### In MPM5-KEY-FINDINGS.md:

1. **Section 6:** ND-500 Byte-Oriented Architecture vs Memory Bus Width
   - Detailed explanation with analogies
   - Comparison table
   - x86-64 analogy for modern developers

2. **Corrections section:** Four major misconceptions corrected
   - With ❌ INCORRECT and ✅ CORRECT format
   - Clear before/after comparison

3. **Summary section:** 8 specific updates needed
   - Actionable list for documentation review
   - Emphasis points for developers

### In 06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md:

1. **Version 2.0 Warning Banner**
   - 6 key corrections highlighted
   - Reference to official manual

2. **Section 1.2:** CRITICAL: Understanding ND-500 Architecture
   - Comparison table (ND-100 vs ND-500)
   - x86-64 analogy
   - "Why this matters" explanation

---

## ✅ Verification Checklist

- [x] MPM5-KEY-FINDINGS.md created with all official manual details
- [x] Section on ND-500 byte-oriented architecture added
- [x] 06-MULTIPORT-MEMORY document updated to v2.0
- [x] Warning banner added with key corrections
- [x] New section 1.2 inserted with architecture clarification
- [x] Table of contents updated
- [x] Subsection numbers corrected (1.3→1.4, 1.4→1.5)
- [x] Consistent terminology throughout

---

## 🎓 Key Takeaways for Developers

1. **ND-500 is byte-oriented** - Always think in terms of bytes, not 32-bit words
2. **"32-bit channel" = bus width** - Bandwidth optimization, not CPU architecture
3. **MPM5 = standard RAM + special ports** - Not exotic memory hardware
4. **Cache coherency = CPU-level** - S flag is for ND-500 CPU, not MPM5
5. **Address translation happens** - ND-100 and ND-500 see different channel addresses
6. **Both CPUs share same physical RAM** - Just accessed through different ports with different bus widths

---

## 📖 References

### Official Documentation:
- **ND-10.004.01**: MPM 5 Technical Description (June 1984)
  - Section 1.2: Twin 16-Bit Port
  - Section 1.3: Data Part
  - Section 1.4: Address Part
  - Section 1.5: Control and Test Registers
  
### Updated Documents:
- `KERNEL/MPM5-KEY-FINDINGS.md` (NEW)
- `KERNEL/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md` (v2.0)
- `KERNEL/MPM5-DOCUMENTATION-UPDATE-SUMMARY.md` (this document)

---

## 🔄 Next Steps

### Recommended Reviews:

1. **12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md**
   - Check for "32-bit" terminology
   - Verify byte-oriented references

2. **05-ND500-DMA-KERNEL.md**
   - Update DMA address calculations
   - Clarify byte addressing

3. **ND500-EMULATION-COMPLETE.cs**
   - Review C# structs for byte alignment
   - Verify address calculations

4. **NPL-DEVELOPER-GUIDE.md Section 13.6**
   - Multi-CPU Communication Patterns
   - Ensure consistent terminology

### Search Terms for Global Review:
- "32-bit CPU"
- "32-bit word"
- "ND-500 is 32-bit"
- "word-oriented ND-500"

---

**✅ This update provides accurate, verified information from official Norsk Data documentation.**

---

*Last updated: October 17, 2025*


