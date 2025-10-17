# ASCII Data Analysis - connected.txt

## Executive Summary

**SUCCESS! ASCII text found including "RETROCORE"**

This trace captures an actual terminal login session with readable ASCII text being transferred.

---

## Line 48: SINTRAN System Banner with "RETROCORE"

**Raw Bytes (partial):**
```hex
...0x78 0x00 0x04 0x03 0x01 0x00 0x00 0x00 0x03 0x01 0x01 0x01 0x5B 0x0D 0x0A
0x20 0x31 0x37 0x2E 0x30 0x38 0x2E 0x34 0x32 0x20 0x20 0x20 0x20 0x20 0x31 0x33
0x20 0x4F 0x43 0x54 0x4F 0x42 0x45 0x52 0x20 0x20 0x20 0x31 0x39 0x39 0x37 0x0D
0x0A 0x20 0x53 0x49 0x4E 0x54 0x52 0x41 0x4E 0x20 0x49 0x49 0x49 0x20 0x2D 0x20
0x56 0x53 0x58 0x2F 0x35 0x30 0x30 0x20 0x4C 0x0D 0x0A 0x2D 0x2D 0x2D 0x20 0x52
0x45 0x54 0x52 0x4F 0x43 0x4F 0x52 0x45 0x20 0x45 0x4D 0x55 0x4C 0x41 0x54 0x45
0x44 0x20 0x4C 0x20 0x2D 0x2D 0x2D 0x0D 0x0A 0x00 0x13 0x02 0x00 0x02 0x01 0x08
0x0D 0x0A 0x45 0x4E 0x54 0x45 0x52 0x20 0x02 0x00
```

**ASCII Decoding:**
```
0x5B = '['
0x0D 0x0A = '\r\n' (carriage return + line feed)

0x20 0x31 0x37 0x2E 0x30 0x38 0x2E 0x34 0x32 = " 17.08.42"
0x20 0x20 0x20 0x20 0x20 = "     " (5 spaces)
0x31 0x33 0x20 = "13 "
0x4F 0x43 0x54 0x4F 0x42 0x45 0x52 = "OCTOBER"
0x20 0x20 0x20 = "   " (3 spaces)
0x31 0x39 0x39 0x37 = "1997"
0x0D 0x0A = '\r\n'

0x20 = " "
0x53 0x49 0x4E 0x54 0x52 0x41 0x4E = "SINTRAN"
0x20 = " "
0x49 0x49 0x49 = "III"
0x20 0x2D 0x20 = " - "
0x56 0x53 0x58 0x2F 0x35 0x30 0x30 = "VSX/500"
0x20 = " "
0x4C = "L"
0x0D 0x0A = '\r\n'

0x2D 0x2D 0x2D 0x20 = "--- "
0x52 0x45 0x54 0x52 0x4F 0x43 0x4F 0x52 0x45 = "RETROCORE" ← HERE IT IS!
0x20 = " "
0x45 0x4D 0x55 0x4C 0x41 0x54 0x45 0x44 = "EMULATED"
0x20 = " "
0x4C = "L"
0x20 0x2D 0x2D 0x2D = " ---"
0x0D 0x0A = '\r\n'

0x0D 0x0A = '\r\n'
0x45 0x4E 0x54 0x45 0x52 0x20 = "ENTER "
```

**Complete ASCII Text:**
```
[
 17.08.42     13 OCTOBER   1997
 SINTRAN III - VSX/500 L
--- RETROCORE EMULATED L ---

ENTER
```

---

## Line 16: Terminal Identification (TADADM)

**Raw Bytes (partial):**
```hex
...0x00 0x41 0x00 0x14 0xFF 0x07 0x2A 0x54 0x41 0x44 0x41 0x44 0x4D 0x00 0xFE
0x04 0x44 0x31 0x30 0x32 0x04 0x02 0x00 0x01...
```

**ASCII Decoding:**
```
0x2A = '*'
0x54 0x41 0x44 0x41 0x44 0x4D = "TADADM"
0x00 = NUL

0x04 = EOT (end of transmission)
0x44 0x31 0x30 0x32 = "D102"
```

**Text:**
```
*TADADM
D102
```

---

## Line 21: Second Terminal Identification

**Raw Bytes (partial):**
```hex
...0x00 0x41 0x00 0x10 0xFF 0x07 0x2A 0x54 0x41 0x44 0x41 0x44 0x4D 0x00 0xFE
0x04 0x44 0x31 0x30 0x32...
```

**ASCII Decoding:**
```
0x2A = '*'
0x54 0x41 0x44 0x41 0x44 0x4D = "TADADM"
0x00 = NUL
0x44 0x31 0x30 0x32 = "D102"
```

**Text:**
```
*TADADM
D102
```

---

## Line 53: Carriage Return / Line Feed

**Raw Bytes (partial):**
```hex
...0x01 0x02 0x0D 0x0A 0x13 0x02 0x00 0x03 0x0E 0x01 0x00
```

**ASCII Decoding:**
```
0x0D 0x0A = '\r\n'
```

**Text:**
```
(carriage return + line feed)
```

---

## Line 58: Password Prompt

**Raw Bytes (partial):**
```hex
...0x00 0x12 0x01 0x0A 0x50 0x41 0x53 0x53 0x57 0x4F 0x52 0x44 0x3A 0x20 0x00
0x03 0x01 0xFF 0x02 0x00
```

**ASCII Decoding:**
```
0x50 0x41 0x53 0x53 0x57 0x4F 0x52 0x44 = "PASSWORD"
0x3A = ':'
0x20 = ' ' (space)
```

**Text:**
```
PASSWORD:
```

---

## Line 63: Second "ENTER" Prompt

**Raw Bytes (partial):**
```hex
...0x18 0x01 0x02 0x0D 0x0A 0x00 0x03 0x01 0x01 0x13 0x02 0x00 0x02 0x01 0x08
0x0D 0x0A 0x45 0x4E 0x54 0x45 0x52 0x20 0x02 0x00
```

**ASCII Decoding:**
```
0x0D 0x0A = '\r\n'
0x45 0x4E 0x54 0x45 0x52 0x20 = "ENTER "
```

**Text:**
```

ENTER
```

---

## Line 66: Partial Username Input

**Raw Bytes (partial):**
```hex
...0x08 0x01 0x06 0x72 0x6F 0xEE 0xEE 0xF9 0x8D...
```

**ASCII Decoding:**
```
0x72 = 'r'
0x6F = 'o'
0xEE = 'î' or possibly corrupted/encoded
0xEE = 'î' or possibly corrupted/encoded
0xF9 = 'ù' or possibly corrupted/encoded
```

**Partial Text:**
```
ro... (possibly "ronny" with encoding issues?)
```

**Note:** Bytes 0xEE, 0xF9, 0x8D are either:
- Extended ASCII characters
- Encrypted/encoded password characters
- Corrupted transmission data

---

## Complete Login Sequence

### Step 1: Connection Banner [Line 48]
```
[
 17.08.42     13 OCTOBER   1997
 SINTRAN III - VSX/500 L
--- RETROCORE EMULATED L ---

ENTER
```

### Step 2: Terminal Identification [Lines 16, 21]
```
*TADADM
D102
```

### Step 3: Password Prompt [Line 58]
```
PASSWORD:
```

### Step 4: User Input [Lines 66-70 - retransmitted 5 times]
```
ro... (partially visible, rest encoded/encrypted)
```

### Step 5: Link Re-establishment [Lines 71-76]
```
(SABM/UA exchange - connection recovery)
```

---

## Byte-by-Byte "RETROCORE" Extraction

**Location:** Line 48, bytes 86-94

```
Offset | Hex   Dec  ASCII | Character
-------+------------------+----------
86     | 0x52  82   'R'   | R
87     | 0x45  69   'E'   | E
88     | 0x54  84   'T'   | T
89     | 0x52  82   'R'   | R
90     | 0x4F  79   'O'   | O
91     | 0x43  67   'C'   | C
92     | 0x4F  79   'O'   | O
93     | 0x52  82   'R'   | R
94     | 0x45  69   'E'   | E
```

**String:** "RETROCORE"

---

## Other ASCII Text Found

### System Information:
- "17.08.42" - Time (5:08:42 PM)
- "13 OCTOBER   1997" - Date
- "SINTRAN III" - Operating system name
- "VSX/500 L" - System version
- "--- RETROCORE EMULATED L ---" - Emulator identification

### Terminal Protocol:
- "*TADADM" - Terminal Access Device Admin (terminal name)
- "D102" - Device identifier (Node 102)
- "PASSWORD: " - Login prompt
- "ENTER " - Command prompt
- '\r\n' - Carriage return/line feed sequences

### User Input (Partial):
- "ro" - First two characters of username (rest encoded/encrypted)

---

## Frame Structure Analysis

### ASCII Text Location in Frame:

The ASCII text appears at **specific offsets** within the payload:

**Line 48 Structure:**
```
Bytes 0-5:   Protocol header (0x09 0x66 0x21 0x13 0x00 0x0E)
Bytes 6-10:  Node addresses (0x00 0x64 0x00 0x66 0x00)
Bytes 11-15: Sequence/control (0x09 0x01 0x08 0xDD 0x05)
Bytes 16-20: Additional protocol (0x21 0x00 0x96 0x00 0x00)
Bytes 21-35: More protocol data
Bytes 36-40: Length/control markers
Bytes 41-45: PAD control sequence
Bytes 46-50: Start marker + '['
Bytes 51+:   ASCII TEXT BEGINS HERE

ASCII Payload:
"[
 17.08.42     13 OCTOBER   1997
 SINTRAN III - VSX/500 L
--- RETROCORE EMULATED L ---

ENTER "
```

---

## Protocol Observations

### PAD (Packet Assembly/Disassembly) Protocol:

The ASCII text is embedded in PAD protocol frames:

**Control Sequences Before ASCII:**
```
0x78 0x00 0x04 0x03 0x01 0x00 0x00 0x00 0x03 0x01 0x01 0x01
```

These bytes indicate:
- PAD parameter settings
- Text formatting controls
- Terminal emulation mode

**Control Sequences After ASCII:**
```
0x00 0x13 0x02 0x00 0x02 0x01 0x08 0x0D 0x0A
```

These bytes indicate:
- End of text marker
- Next prompt preparation
- Line ending controls

---

## Data Type Classification

### Type 1: System Banner Text (Line 48)
- **Format:** ASCII text with formatting
- **Purpose:** Login screen / system identification
- **Content:** Date, time, OS version, emulator name
- **Length:** ~120 characters

### Type 2: Terminal Identification (Lines 16, 21)
- **Format:** ASCII text with control characters
- **Purpose:** Terminal device identification
- **Content:** Terminal name, device number
- **Length:** ~10 characters

### Type 3: Prompts (Lines 48, 58, 63)
- **Format:** ASCII text
- **Purpose:** User interaction prompts
- **Content:** "ENTER", "PASSWORD:"
- **Length:** 5-10 characters

### Type 4: User Input (Lines 66-70)
- **Format:** ASCII + encoded bytes
- **Purpose:** User credentials
- **Content:** Username/password (partially visible)
- **Length:** Variable
- **Security:** Appears to have encoding/encryption

---

## Comparison with Previous Traces

| Feature | trace-conn-100-102.txt | first-connect-to.txt | connected.txt |
|---------|------------------------|----------------------|---------------|
| **ASCII Text** | None | Device IDs only | Full login session |
| **Content Type** | Binary routing | PAD negotiation | Terminal session |
| **Readable Strings** | 0 | 2 ("TADADM", "D102") | 15+ strings |
| **"RETROCORE"** | ❌ Not found | ❌ Not found | ✅ **FOUND!** |
| **User Interaction** | None | None | Login prompts + input |

---

## Conclusion

### What ASCII Data Is Being Transferred:

**Terminal Login Session Data:**

1. **System Banner:**
   - Date/time: "17.08.42     13 OCTOBER   1997"
   - OS identification: "SINTRAN III - VSX/500 L"
   - **Emulator identification: "--- RETROCORE EMULATED L ---"** ← Your requested text

2. **Login Prompts:**
   - "ENTER " (command prompt)
   - "PASSWORD: " (authentication prompt)

3. **Terminal Identification:**
   - "*TADADM" (terminal name)
   - "D102" (device identifier)

4. **User Input:**
   - "ro..." (partial username, rest encoded)

### Protocol Used:

**PAD (Packet Assembly/Disassembly) protocol** carrying terminal emulation data over the HDLC link.

This is a **real terminal session** showing:
- System boot banner with "RETROCORE" branding
- Interactive login sequence
- User credential entry
- Terminal control sequences

---

## Full Path to Document

**Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\ASCII_Data_Connected_Trace.md**
