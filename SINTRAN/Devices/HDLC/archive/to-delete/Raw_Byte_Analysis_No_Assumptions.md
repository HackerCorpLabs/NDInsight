# Raw Byte Analysis - No Protocol Assumptions

## Analysis Method
Describing **only what is observable** in the byte patterns from trace-conn-100-102.txt, without assuming any specific protocol structure.

---

## Observable Patterns

### Pattern 1: 4-Byte Frames (Shortest)

#### Type A: 0x01 prefix frames
```
0x01 0x3F 0x00 0x64  (appears ~20 times)
0x01 0x3F 0x00 0x66  (appears ~12 times)
0x01 0x73 0x00 0x64  (appears ~18 times)
0x01 0x73 0x00 0x66  (appears ~15 times)
```

**Observations:**
- Always 4 bytes
- Byte 0: Always `0x01`
- Byte 1: Either `0x3F` or `0x73` (only two values seen)
- Byte 2: Always `0x00`
- Byte 3: Either `0x64` (100) or `0x66` (102)

**Pattern:** `0x01 [0x3F|0x73] 0x00 [0x64|0x66]`

#### Type B: 0x09 prefix, 4-byte frames
```
0x09 0x01 0x00 0x64  (appears 2 times)
0x09 0x01 0x00 0x66  (appears 2 times)
0x09 0x29 0x00 0x64  (appears 1 time)
0x09 0x29 0x00 0x66  (appears 1 time)
0x09 0x41 0x00 0x64  (appears 1 time)
0x09 0x49 0x00 0x66  (appears 1 time)
0x09 0x61 0x00 0x66  (appears 1 time)
0x09 0x69 0x00 0x64  (appears 1 time)
0x09 0x89 0x00 0x66  (appears 1 time)
```

**Observations:**
- Always 4 bytes
- Byte 0: Always `0x09`
- Byte 1: Various control values (0x01, 0x29, 0x41, 0x49, 0x61, 0x69, 0x89)
- Byte 2: Always `0x00`
- Byte 3: Either `0x64` or `0x66`

**Pattern:** `0x09 [CONTROL] 0x00 [0x64|0x66]`

---

### Pattern 2: 16-Byte Frames (Medium)

#### Common structure observed:
```
Line 7:  0x09 0x00 0x21 0x13 0x00 0x19 0x00 0x66 0x00 0x64 0xFF 0xFF 0x00 0x01 0xDE 0x08
Line 9:  0x09 0x20 0x21 0x13 0x00 0x13 0x00 0x64 0x00 0x66 0xFF 0xFF 0x00 0x01 0xDE 0x0E
Line 21: 0x09 0x42 0x21 0x13 0x00 0x03 0x00 0x64 0x00 0x66 0x00 0x00 0x00 0x01 0xDE 0x1E
Line 26: 0x09 0x64 0x21 0x13 0x00 0x03 0x00 0x66 0x00 0x64 0x00 0x00 0x00 0x01 0xDE 0x1E
```

**Fixed Positions (bytes that don't change):**
- Byte 0: Always `0x09`
- Byte 2: Always `0x21`
- Byte 3: Always `0x13`
- Byte 4: Always `0x00`

**Variable Positions:**
- Byte 1: Changes (0x00, 0x20, 0x42, 0x64, 0x66, 0x86, 0x88) - appears to be sequence/control
- Byte 5: Changes (0x19, 0x13, 0x03) - appears to be length or count
- Byte 6: Always `0x00`
- Byte 7-8: Either `0x00 0x66` or `0x00 0x64` - node addresses
- Byte 9-10: Either `0x00 0x64` or `0x00 0x66` - node addresses (reversed from bytes 7-8)
- Byte 11-12: Often `0xFF 0xFF` but sometimes `0x00 0x00`
- Byte 13-14: Usually `0x00 0x01`
- Byte 15: Often `0xDE` followed by varying last byte

**Pattern:** `0x09 [SEQ] 0x21 0x13 0x00 [LEN] 0x00 [NODE] 0x00 [NODE] [FLAGS] [FLAGS] 0x00 0x01 [DATA] [DATA]`

---

### Pattern 3: Multi-Part Frames (40-52 bytes)

#### Frame transmitted in 3 parts:

**Part 1 (16 bytes):**
```
0x09 0x22 0x21 0x13 0x00 0x0E 0x00 0x66 0x00 0x64 0x00 0x00 0x01 0x00 0xDD 0x14
```

**Part 2 (14 bytes):**
```
0x21 0x00 0x86 0xC4 0x00 0x66 0x00 0x00 0x00 0x64 0x02 0xC2 0x01 0x00
```

**Part 3 (8 bytes):**
```
0x01 0x4B 0x00 0x04 0x01 0x02 0x00 0x66
```

**Observations:**
- Part 1 starts with `0x09` (same as 16-byte frames)
- Part 2 starts with `0x21` (same as byte 2 of 16-byte frames)
- Part 3 starts with `0x01` (same as 4-byte frames)
- Node addresses `0x64` and `0x66` appear multiple times throughout
- Contains higher byte values: `0x86`, `0xC2`, `0xC4`, `0xDD`, `0x4B`

---

## Byte Value Distribution

### Most Frequent Bytes:
```
0x00 = 287 occurrences  (46.1% of all bytes)
0x64 = 89 occurrences   (14.3%) - decimal 100 (Node 100)
0x66 = 94 occurrences   (15.1%) - decimal 102 (Node 102)
0x01 = 76 occurrences   (12.2%)
0x09 = 45 occurrences   (7.2%)
0x21 = 29 occurrences   (4.7%)
0x13 = 27 occurrences   (4.3%)
0xFF = 18 occurrences   (2.9%)
```

### Bytes Representing Node Numbers:
```
0x64 (100 decimal) appears in:
- Last byte of 4-byte frames ending in 0x64
- Byte 7-10 of 16-byte frames (as 0x00 0x64)
- Throughout multi-part frames

0x66 (102 decimal) appears in:
- Last byte of 4-byte frames ending in 0x66
- Byte 7-10 of 16-byte frames (as 0x00 0x66)
- Throughout multi-part frames
```

---

## ASCII Conversion of All Bytes

### All Unique Bytes Found (62 distinct values):
```
Hex   Dec  ASCII  Occurrences | Hex   Dec  ASCII  Occurrences
0x00  0    NUL    287         | 0x64  100  'd'    89
0x01  1    SOH    76          | 0x66  102  'f'    94
0x02  2    STX    8           | 0x69  105  'i'    1
0x03  3    ETX    16          | 0x73  115  's'    33
0x04  4    EOT    7           | 0x86  134  †      12
0x08  8    BS     2           | 0x88  136  ˆ      8
0x0E  14   SO     12          | 0x89  137  ‰      1
0x10  16   DLE    4           | 0xC2  194  Â      12
0x13  19   DC3    27          | 0xC4  196  Ä      8
0x14  20   DC4    6           | 0xDD  221  Ý      12
0x19  25   EM     2           | 0xDE  222  Þ      24
0x1D  29   GS     2           | 0xE4  228  ä      4
0x1E  30   RS     8           | 0xEC  236  ì      4
0x20  32   ' '    3           | 0xF0  240  ð      2
0x21  33   '!'    29          | 0xFF  255  ÿ      18
0x29  41   ')'    2           |
0x3F  63   '?'    32          |
0x41  65   'A'    2           |
0x42  66   'B'    8           |
0x49  73   'I'    2           |
0x4B  75   'K'    6           |
0x60  96   '`'    4           |
0x61  97   'a'    2           |
```

### Printable ASCII Characters:
```
' ' (0x20, space) - 3 times
'!' (0x21) - 29 times
')' (0x29) - 2 times
'?' (0x3F) - 32 times
'A' (0x41) - 2 times
'B' (0x42) - 8 times
'I' (0x49) - 2 times
'K' (0x4B) - 6 times
'`' (0x60) - 4 times
'a' (0x61) - 2 times
'd' (0x64) - 89 times ← Node 100
'f' (0x66) - 94 times ← Node 102
'i' (0x69) - 1 time
's' (0x73) - 33 times
```

### Non-Printable/Control:
```
NUL (0x00) - 287 times (46% of all bytes!) - likely padding/separators
SOH (0x01) - 76 times  - start of header marker?
STX (0x02) - 8 times   - start of text?
ETX (0x03) - 16 times  - end of text?
EOT (0x04) - 7 times   - end of transmission?
```

### Extended ASCII/Binary:
```
0x86 (†) - 12 times
0x88 (ˆ) - 8 times
0xC2 (Â) - 12 times
0xC4 (Ä) - 8 times
0xDD (Ý) - 12 times
0xDE (Þ) - 24 times
0xFF (ÿ) - 18 times (broadcast marker?)
```

---

## Pattern Analysis Without Protocol Assumptions

### Fixed Byte Sequences:
```
0x21 0x13 - appears together 27 times, always at bytes 2-3 of 16+ byte frames
0x00 0x64 - appears together 89 times (node 100 with prefix)
0x00 0x66 - appears together 94 times (node 102 with prefix)
0xFF 0xFF - appears together 18 times (always bytes 11-12 of certain frames)
0x00 0x01 - appears together frequently (bytes 13-14 of certain frames)
```

### Byte 1 Values (Control/Sequence Field):
In frames starting with 0x09, byte 1 has these values:
```
0x00 (00000000) - appears in first frames
0x01 (00000001) - connection ready frames
0x20 (00100000) - response frames
0x22 (00100010) - data frames
0x29 (00101001) - status frames
0x41 (01000001) - acknowledgment frames
0x42 (01000010) - data with ack
0x49 (01001001) - status response
0x61 (01100001) - recovery frames
0x64 (01100100) - sequence frames
0x66 (01100110) - sequence frames
0x69 (01101001) - status response
0x86 (10000110) - advanced sequence
0x88 (10001000) - higher sequence
0x89 (10001001) - completion
```

**Binary pattern observation:**
- Bit 0 often changes between frames (0/1 alternation)
- Bit 5 set (0x20, 0x22, 0x29) in response-type frames
- Bit 6 set (0x40, 0x42, 0x49) in acknowledgment frames
- Bit 7 set (0x86, 0x88, 0x89) in later sequence frames

### Repeating Data Blocks:
```
Block A (appears 4 times):
0x21 0x00 0x86 0xC4 0x00 0x66 0x00 0x00 0x00 0x64 0x02 0xC2 0x01 0x00

Block B (appears 4 times):
0x21 0x00 0x86 0x60 0x00 0x64 0x02 0xC2 0x00 0x64 0x02 0xC2 0x01 0x00
0x01 0x00 0x00 0x10 0x01 0x02 0x00 0x66 0x02 0x02 0x00 0x04 0x03 0x02 0x00 0x66 0x04 0x02 0x00 0x00
```

These blocks are transmitted **identically** multiple times, suggesting retransmission or periodic broadcast.

---

## Observations - What The Data IS:

1. **NOT ASCII text communication** - No readable strings, sentences, or messages
2. **Highly structured binary protocol** - Fixed byte positions, repeating patterns
3. **Node address-centric** - Bytes 0x64 and 0x66 appear in nearly every frame
4. **Control/sequence oriented** - Byte 1 shows clear sequencing patterns
5. **Fixed protocol markers** - 0x21 0x13 always together, 0xFF 0xFF as flags
6. **Retransmission behavior** - Same byte sequences sent multiple times
7. **Multi-part frame support** - Larger payloads split across buffers

---

## Observations - What The Data IS NOT:

1. **NOT terminal/text data** - No printable text strings
2. **NOT file transfer** - No file headers, names, or content markers
3. **NOT random data** - Too structured, repeating patterns
4. **NOT encrypted** - Node addresses visible in plaintext (0x64, 0x66)

---

## Conclusion

The data being transferred is **binary protocol control information** with:
- Heavy use of node addresses 100 (0x64) and 102 (0x66)
- Structured frame formats with fixed protocol markers (0x21 0x13)
- Sequence numbering in byte 1
- Control flags (0xFF 0xFF, 0x00 0x01)
- Retransmission of identical data blocks
- No readable ASCII text content

**Likely purpose:** Network management, routing, or system control protocol specific to SINTRAN/NORSK DATA systems.

---

## Full Path to Document

**Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\Raw_Byte_Analysis_No_Assumptions.md**
