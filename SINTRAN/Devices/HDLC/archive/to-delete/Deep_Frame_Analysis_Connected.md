# Deep Frame Analysis - connected.txt
## All Frames > 4 Bytes

---

## Frame #1: Line 1 - Initial Routing Frame (SENT)

**Timestamp:** [17:08:26.247]
**Direction:** SENT (100 → 102)
**Total Length:** 19 bytes (with HDLC flags)
**Payload Length:** 16 bytes

**Complete Frame with HDLC Flags:**
```hex
0x7E 0x09 0x00 0x21 0x13 0x00 0x19 0x00 0x66 0x00 0x64 0xFF 0xFF 0x00 0x01 0xDE 0x08 0x08 0x03 0x7E
```

**Frame Structure:**
```
Byte 0:    0x7E      = HDLC Start Flag
Byte 1:    0x09      = LAPB address
Byte 2:    0x00      = LAPB Control: N(S)=0, N(R)=0
Byte 3:    0x21      = Fixed marker
Byte 4:    0x13      = Fixed marker (19 decimal)
Byte 5:    0x00      = Packet type field
Byte 6:    0x19      = Length: 25 (decimal)
Byte 7:    0x00      = Separator
Byte 8:    0x66      = Node 102 (destination)
Byte 9:    0x00      = Separator
Byte 10:   0x64      = Node 100 (source)
Byte 11:   0xFF      = Broadcast flag (high)
Byte 12:   0xFF      = Broadcast flag (low)
Byte 13:   0x00      = Protocol field
Byte 14:   0x01      = Protocol version/type
Byte 15:   0xDE      = Data byte (222 decimal)
Byte 16:   0x08      = Data byte (8 decimal)
Byte 17:   0x08      = FCS (Frame Check Sequence) high
Byte 18:   0x03      = FCS low
Byte 19:   0x7E      = HDLC End Flag
```

**Analysis:**
- First packet sent after connection
- Routing protocol initialization
- Broadcast to all nodes (0xFF 0xFF)
- Contains node topology information
- FCS: 0x0308

---

## Frame #2: Line 2 - Routing Response (RECEIVED)

**Timestamp:** [17:08:26.405]
**Direction:** RECEIVED (102 → 100)
**Total Length:** 16 bytes (no flags shown in DMACB receive)
**Payload Length:** 16 bytes

**Complete Frame:**
```hex
0x09 0x20 0x21 0x13 0x00 0x13 0x00 0x64 0x00 0x66 0xFF 0xFF 0x00 0x01 0xDE 0x0E
```

**Frame Structure:**
```
Byte 0:    0x09      = LAPB address
Byte 1:    0x20      = LAPB Control: N(S)=1, N(R)=0
Byte 2:    0x21      = Fixed marker
Byte 3:    0x13      = Fixed marker
Byte 4:    0x00      = Packet type field
Byte 5:    0x13      = Length: 19 (decimal)
Byte 6:    0x00      = Separator
Byte 7:    0x64      = Node 100 (destination) - reversed
Byte 8:    0x00      = Separator
Byte 9:    0x66      = Node 102 (source) - reversed
Byte 10:   0xFF      = Broadcast flag
Byte 11:   0xFF      = Broadcast flag
Byte 12:   0x00      = Protocol field
Byte 13:   0x01      = Protocol version/type
Byte 14:   0xDE      = Data byte
Byte 15:   0x0E      = Data byte (14 decimal) - DIFFERENT from sent frame (was 0x08)
```

**Analysis:**
- Response to Frame #1
- Node addresses reversed (102→100)
- Sequence incremented: N(S)=1
- Length changed: 19 vs 25
- Last data byte changed: 0x0E vs 0x08 (response code?)

---

## Frame #3: Line 3 - Multi-Protocol Data (SENT)

**Timestamp:** [17:08:26.408]
**Direction:** SENT (100 → 102)
**Total Length:** 44 bytes (with flags)
**Payload Length:** 40 bytes

**Complete Frame:**
```hex
0x7E 0x09 0x22 0x21 0x13 0x00 0x0E 0x00 0x66 0x00 0x64 0x00 0x00 0x01 0x00 0xDD
0x14 0x21 0x00 0x86 0x84 0x00 0x66 0x00 0x00 0x00 0x64 0x02 0xFB 0x01 0x00 0x01
0x4B 0x00 0x04 0x01 0x02 0x00 0x66 0x79 0xC0 0x7E
```

**Frame Structure - Part 1 (Bytes 0-16):**
```
Byte 0:    0x7E      = HDLC Start Flag
Byte 1:    0x09      = LAPB address
Byte 2:    0x22      = LAPB Control: N(S)=1, N(R)=1
Byte 3:    0x21      = Fixed marker
Byte 4:    0x13      = Fixed marker
Byte 5:    0x00      = Packet type
Byte 6:    0x0E      = Length: 14 (decimal)
Byte 7:    0x00      = Separator
Byte 8:    0x66      = Node 102
Byte 9:    0x00      = Separator
Byte 10:   0x64      = Node 100
Byte 11:   0x00      = Data: 0x00
Byte 12:   0x00      = Data: 0x00
Byte 13:   0x01      = Data: 0x01
Byte 14:   0x00      = Data: 0x00
Byte 15:   0xDD      = Data: 221
Byte 16:   0x14      = Data: 20
```

**Frame Structure - Part 2 (Bytes 17-32):**
```
Byte 17:   0x21      = Protocol marker (starts second segment)
Byte 18:   0x00      = Field
Byte 19:   0x86      = High value (134) - protocol flag
Byte 20:   0x84      = High value (132) - protocol flag
Byte 21:   0x00      = Separator
Byte 22:   0x66      = Node 102
Byte 23:   0x00      = Separator
Byte 24:   0x00      = Data
Byte 25:   0x00      = Data
Byte 26:   0x64      = Node 100
Byte 27:   0x02      = Protocol control
Byte 28:   0xFB      = Data (251 decimal)
Byte 29:   0x01      = Data
Byte 30:   0x00      = Data
Byte 31:   0x01      = Data
Byte 32:   0x4B      = Data (75 = 'K' in ASCII)
```

**Frame Structure - Part 3 (Bytes 33-43):**
```
Byte 33:   0x00      = Data
Byte 34:   0x04      = EOT control
Byte 35:   0x01      = Data
Byte 36:   0x02      = STX control
Byte 37:   0x00      = Data
Byte 38:   0x66      = Node 102 (trailer)
Byte 39:   0x79      = FCS high
Byte 40:   0xC0      = FCS low
Byte 41:   0x7E      = HDLC End Flag
```

**Analysis:**
- Complex multi-segment frame
- Two distinct protocol sections (starts at byte 1 and byte 17)
- Contains control characters: EOT (0x04), STX (0x02)
- ASCII 'K' (0x4B) embedded in binary data
- FCS: 0x79C0

---

## Frame #16: Line 16 - PAD Terminal Identification with "TADADM" (SENT)

**Timestamp:** [17:08:40.181]
**Direction:** SENT (100 → 102)
**Total Length:** 60 bytes (with flags)
**Payload Length:** 56 bytes

**Complete Frame:**
```hex
0x7E 0x09 0xAA 0x21 0x13 0x00 0x0E 0x00 0x66 0x00 0x64 0x00 0x02 0x04 0x00 0xDA
0x12 0x21 0x00 0x86 0xE4 0x00 0x66 0x00 0x00 0x00 0x64 0x02 0x90 0x04 0x00 0x00
0x41 0x00 0x14 0xFF 0x07 0x2A 0x54 0x41 0x44 0x41 0x44 0x4D 0x00 0xFE 0x04 0x44
0x31 0x30 0x32 0x04 0x02 0x00 0x01 0xC1 0x15 0x7E
```

**Frame Structure - Part 1 (Header, Bytes 0-16):**
```
Byte 0:    0x7E      = HDLC Start Flag
Byte 1:    0x09      = LAPB address
Byte 2:    0xAA      = LAPB Control: N(S)=5, N(R)=5
Byte 3:    0x21      = Fixed marker
Byte 4:    0x13      = Fixed marker
Byte 5:    0x00      = Packet type
Byte 6:    0x0E      = Length: 14
Byte 7:    0x00      = Separator
Byte 8:    0x66      = Node 102
Byte 9:    0x00      = Separator
Byte 10:   0x64      = Node 100
Byte 11:   0x00      = Sequence: 0
Byte 12:   0x02      = Sequence: 2
Byte 13:   0x04      = PAD parameter
Byte 14:   0x00      = Parameter value
Byte 15:   0xDA      = Protocol ID (218)
Byte 16:   0x12      = Sub-ID (18)
```

**Frame Structure - Part 2 (Protocol Data, Bytes 17-31):**
```
Byte 17:   0x21      = Protocol marker
Byte 18:   0x00      = Field
Byte 19:   0x86      = High protocol value
Byte 20:   0xE4      = High protocol value (228)
Byte 21:   0x00      = Separator
Byte 22:   0x66      = Node 102
Byte 23:   0x00      = Separator
Byte 24:   0x00      = Data
Byte 25:   0x00      = Data
Byte 26:   0x64      = Node 100
Byte 27:   0x02      = Control
Byte 28:   0x90      = Protocol value (144)
Byte 29:   0x04      = PAD parameter
Byte 30:   0x00      = Parameter value
Byte 31:   0x00      = Padding
```

**Frame Structure - Part 3 (ASCII Terminal Name, Bytes 32-47):**
```
Byte 32:   0x41      = 'A' in ASCII (or control)
Byte 33:   0x00      = Length field
Byte 34:   0x14      = Length: 20 bytes
Byte 35:   0xFF      = Marker
Byte 36:   0x07      = BEL control character
Byte 37:   0x2A      = '*' ASCII
Byte 38:   0x54      = 'T' ASCII ┐
Byte 39:   0x41      = 'A' ASCII │
Byte 40:   0x44      = 'D' ASCII │
Byte 41:   0x41      = 'A' ASCII ├─ "TADADM"
Byte 42:   0x44      = 'D' ASCII │
Byte 43:   0x4D      = 'M' ASCII ┘
Byte 44:   0x00      = NUL terminator
Byte 45:   0xFE      = Marker
Byte 46:   0x04      = EOT
Byte 47:   0x44      = 'D' ASCII ┐
```

**Frame Structure - Part 4 (Device ID + Trailer, Bytes 48-59):**
```
Byte 48:   0x31      = '1' ASCII │
Byte 49:   0x30      = '0' ASCII ├─ "D102"
Byte 50:   0x32      = '2' ASCII ┘
Byte 51:   0x04      = EOT
Byte 52:   0x02      = STX
Byte 53:   0x00      = Data
Byte 54:   0x01      = Data
Byte 55:   0xC1      = FCS high
Byte 56:   0x15      = FCS low
Byte 57:   0x7E      = HDLC End Flag
```

**ASCII Extracted:**
```
*TADADM
D102
```

**Analysis:**
- PAD (Packet Assembly/Disassembly) terminal identification
- Terminal name: "TADADM" (Terminal Access Device Admin)
- Device identifier: "D102" (Device 102)
- Contains both binary protocol and ASCII text
- Sequence counters: N(S)=5, N(R)=5
- FCS: 0xC115

---

## Frame #48: Line 48 - SINTRAN Banner with "RETROCORE" (RECEIVED)

**Timestamp:** [17:08:53.552]
**Direction:** RECEIVED (102 → 100)
**Total Length:** 174 bytes
**Payload Length:** 174 bytes

**Complete Frame (Split for readability):**
```hex
Part 1 (Bytes 0-31):
0x09 0x66 0x21 0x13 0x00 0x0E 0x00 0x64 0x00 0x66 0x00 0x09 0x01 0x08 0xDD 0x03
0x21 0x00 0x96 0x00 0x00 0x64 0x02 0x90 0x00 0x66 0x03 0x29 0x01 0x08 0x00 0x00

Part 2 (Bytes 32-63):
0x00 0x78 0x00 0x04 0x03 0x01 0x00 0x00 0x00 0x03 0x01 0x01 0x01 0x5B 0x0D 0x0A
0x20 0x31 0x37 0x2E 0x30 0x38 0x2E 0x34 0x32 0x20 0x20 0x20 0x20 0x20 0x31 0x33

Part 3 (Bytes 64-95):
0x20 0x4F 0x43 0x54 0x4F 0x42 0x45 0x52 0x20 0x20 0x20 0x31 0x39 0x39 0x37 0x0D
0x0A 0x20 0x53 0x49 0x4E 0x54 0x52 0x41 0x4E 0x20 0x49 0x49 0x49 0x20 0x2D 0x20

Part 4 (Bytes 96-127):
0x56 0x53 0x58 0x2F 0x35 0x30 0x30 0x20 0x4C 0x0D 0x0A 0x2D 0x2D 0x2D 0x20 0x52
0x45 0x54 0x52 0x4F 0x43 0x4F 0x52 0x45 0x20 0x45 0x4D 0x55 0x4C 0x41 0x54 0x45

Part 5 (Bytes 128-159):
0x44 0x20 0x4C 0x20 0x2D 0x2D 0x2D 0x0D 0x0A 0x00 0x13 0x02 0x00 0x02 0x01 0x08
0x0D 0x0A 0x45 0x4E 0x54 0x45 0x52 0x20 0x02 0x00

Remaining bytes (160-173): Control/padding data
```

**Frame Structure - Protocol Header (Bytes 0-31):**
```
Byte 0:    0x09      = LAPB address
Byte 1:    0x66      = LAPB Control: N(S)=3, N(R)=3
Byte 2:    0x21      = Fixed marker
Byte 3:    0x13      = Fixed marker
Byte 4:    0x00      = Packet type
Byte 5:    0x0E      = Length: 14
Byte 6:    0x00      = Separator
Byte 7:    0x64      = Node 100
Byte 8:    0x00      = Separator
Byte 9:    0x66      = Node 102
Byte 10:   0x00      = Sequence low
Byte 11:   0x09      = Sequence: 9
Byte 12:   0x01      = Protocol version
Byte 13:   0x08      = Protocol sub-type
Byte 14:   0xDD      = Protocol ID
Byte 15:   0x03      = Sub-ID
Byte 16-31: Additional protocol fields
```

**Frame Structure - PAD Control (Bytes 32-45):**
```
Byte 32:   0x00      = Padding
Byte 33:   0x78      = Length: 120 (decimal) - text payload size!
Byte 34:   0x00      = Padding
Byte 35:   0x04      = EOT
Byte 36:   0x03      = ETX (end of text)
Byte 37:   0x01      = SOH (start of header)
Byte 38-42: Control codes
Byte 43:   0x01      = Start marker
Byte 44:   0x5B      = '[' ASCII - BANNER STARTS HERE
Byte 45:   0x0D      = '\r' (carriage return)
```

**Frame Structure - ASCII TEXT (Bytes 46-136):**
```
Bytes 46-47:   0x0A 0x20           = '\n' + ' ' (newline + space)
Bytes 48-53:   0x31 0x37 0x2E...   = "17.08.42" (time)
Bytes 54-58:   0x20 0x20 0x20...   = "     " (spaces)
Bytes 59-61:   0x31 0x33 0x20      = "13 "
Bytes 62-68:   0x4F 0x43 0x54...   = "OCTOBER"
Bytes 69-71:   0x20 0x20 0x20      = "   "
Bytes 72-75:   0x31 0x39 0x39 0x37 = "1997"
Bytes 76-77:   0x0D 0x0A           = '\r\n'

Bytes 78-79:   0x20                = ' '
Bytes 79-85:   0x53 0x49 0x4E...   = "SINTRAN"
Bytes 86:      0x20                = ' '
Bytes 87-89:   0x49 0x49 0x49      = "III"
Bytes 90-92:   0x20 0x2D 0x20      = " - "
Bytes 93-99:   0x56 0x53 0x58...   = "VSX/500"
Bytes 100:     0x20                = ' '
Bytes 101:     0x4C                = "L"
Bytes 102-103: 0x0D 0x0A           = '\r\n'

Bytes 104-107: 0x2D 0x2D 0x2D 0x20 = "--- "
Bytes 108-116: 0x52 0x45 0x54...   = "RETROCORE" ← THE KEY TEXT!
Bytes 117:     0x20                = ' '
Bytes 118-125: 0x45 0x4D 0x55...   = "EMULATED"
Bytes 126:     0x20                = ' '
Bytes 127:     0x4C                = "L"
Bytes 128-131: 0x20 0x2D 0x2D 0x2D = " ---"
Bytes 132-133: 0x0D 0x0A           = '\r\n'

Bytes 134-135: 0x00 (padding)
Bytes 136-142: Control codes
Bytes 143-144: 0x0D 0x0A           = '\r\n'
Bytes 145-150: 0x45 0x4E 0x54...   = "ENTER "
Bytes 151-152: 0x02 0x00           = STX + padding
```

**Complete ASCII Text Extracted:**
```
[
 17.08.42     13 OCTOBER   1997
 SINTRAN III - VSX/500 L
--- RETROCORE EMULATED L ---

ENTER
```

**Analysis:**
- System login banner
- Contains system time/date
- OS identification: SINTRAN III VSX/500 L
- **Emulator identification: "RETROCORE EMULATED L"**
- 120 bytes of text payload (indicated by 0x78 at byte 33)
- PAD protocol with embedded ASCII
- Sequence: N(S)=3, N(R)=3

**"RETROCORE" Location:**
- Starts at byte 108
- Length: 9 bytes
- ASCII values: 0x52 0x45 0x54 0x52 0x4F 0x43 0x4F 0x52 0x45

---

## Frame #58: Line 58 - "PASSWORD:" Prompt (RECEIVED)

**Timestamp:** [17:08:57.793]
**Direction:** RECEIVED (102 → 100)
**Total Length:** 53 bytes
**Payload Length:** 53 bytes

**Complete Frame:**
```hex
0x09 0xEE 0x21 0x13 0x00 0x0E 0x00 0x64 0x00 0x66 0x00 0x0B 0x01 0x08 0xDD 0x01
0x21 0x00 0x92 0x00 0x00 0x64 0x02 0x90 0x00 0x66 0x03 0x29 0x01 0x08 0x00 0x00
0x00 0x12 0x01 0x0A 0x50 0x41 0x53 0x53 0x57 0x4F 0x52 0x44 0x3A 0x20 0x00 0x03
0x01 0xFF 0x02 0x00
```

**Frame Structure - Header (Bytes 0-15):**
```
Byte 0:    0x09      = LAPB address
Byte 1:    0xEE      = LAPB Control: N(S)=7, N(R)=7
Byte 2:    0x21      = Fixed marker
Byte 3:    0x13      = Fixed marker
Byte 4:    0x00      = Packet type
Byte 5:    0x0E      = Length: 14
Byte 6:    0x00      = Separator
Byte 7:    0x64      = Node 100
Byte 8:    0x00      = Separator
Byte 9:    0x66      = Node 102
Byte 10:   0x00      = Sequence low
Byte 11:   0x0B      = Sequence: 11
Byte 12:   0x01      = Protocol version
Byte 13:   0x08      = Protocol sub-type
Byte 14:   0xDD      = Protocol ID
Byte 15:   0x01      = Sub-ID
```

**Frame Structure - ASCII Prompt (Bytes 32-44):**
```
Byte 32:   0x00      = Padding
Byte 33:   0x12      = Length: 18 bytes
Byte 34:   0x01      = SOH
Byte 35:   0x0A      = '\n' (line feed)
Byte 36:   0x50      = 'P' ASCII ┐
Byte 37:   0x41      = 'A' ASCII │
Byte 38:   0x53      = 'S' ASCII │
Byte 39:   0x53      = 'S' ASCII ├─ "PASSWORD:"
Byte 40:   0x57      = 'W' ASCII │
Byte 41:   0x4F      = 'O' ASCII │
Byte 42:   0x52      = 'R' ASCII │
Byte 43:   0x44      = 'D' ASCII ┘
Byte 44:   0x3A      = ':' ASCII
Byte 45:   0x20      = ' ' (space)
Byte 46-52: Control codes and padding
```

**ASCII Extracted:**
```
PASSWORD:
```

**Analysis:**
- Password prompt for authentication
- 18 bytes of text payload (byte 33 indicates length)
- PAD terminal protocol
- Sequence: N(S)=7, N(R)=7

---

## Frame #63: Line 63 - "ENTER" Prompt Repeated (RECEIVED)

**Timestamp:** [17:08:58.848]
**Direction:** RECEIVED (102 → 100)
**Total Length:** 59 bytes
**Payload Length:** 59 bytes

**Complete Frame:**
```hex
0x09 0x22 0x21 0x13 0x00 0x0E 0x00 0x64 0x00 0x66 0x00 0x0C 0x01 0x08 0xDD 0x00
0x21 0x00 0x96 0x00 0x00 0x64 0x02 0x90 0x00 0x66 0x03 0x29 0x01 0x08 0x00 0x00
0x00 0x18 0x01 0x02 0x0D 0x0A 0x00 0x03 0x01 0x01 0x13 0x02 0x00 0x02 0x01 0x08
0x0D 0x0A 0x45 0x4E 0x54 0x45 0x52 0x20 0x02 0x00
```

**ASCII Portion (Bytes 48-53):**
```
Byte 48:   0x45      = 'E' ASCII ┐
Byte 49:   0x4E      = 'N' ASCII │
Byte 50:   0x54      = 'T' ASCII ├─ "ENTER"
Byte 51:   0x45      = 'E' ASCII │
Byte 52:   0x52      = 'R' ASCII ┘
Byte 53:   0x20      = ' ' (space)
```

**ASCII Extracted:**
```
ENTER
```

**Analysis:**
- Command prompt after password entry
- Contains line endings (0x0D 0x0A)
- Ready for user command input

---

## Frame #66-70: Lines 66-70 - Username Input (SENT, Retransmitted 5x)

**Timestamp:** [17:09:00.582] (first transmission)
**Direction:** SENT (100 → 102)
**Total Length:** 56 bytes
**Payload Length:** 52 bytes

**Complete Frame (transmitted identically 5 times):**
```hex
0x7E 0x09 0x44 0x21 0x13 0x00 0x0E 0x00 0x66 0x00 0x64 0x00 0x0C 0x01 0x08 0xDD
0x00 0x21 0x00 0x96 0x84 0x00 0x66 0x03 0x29 0x00 0x64 0x02 0x90 0x01 0x08 0x00
0x00 0x00 0x08 0x01 0x06 0x72 0x6F 0xEE 0xEE 0xF9 0x8D 0xD2 0x7D 0x5E 0x7E
```

**User Input Data (Bytes 35-43):**
```
Byte 35:   0x08      = Length: 8 bytes of user input
Byte 36:   0x01      = Input type
Byte 37:   0x06      = Input length: 6 characters
Byte 38:   0x72      = 'r' ASCII ┐
Byte 39:   0x6F      = 'o' ASCII ┘ Username starts: "ro..."
Byte 40:   0xEE      = Encoded/encrypted byte
Byte 41:   0xEE      = Encoded/encrypted byte
Byte 42:   0xF9      = Encoded/encrypted byte
Byte 43:   0x8D      = Encoded/encrypted byte
Byte 44:   0xD2      = FCS high
Byte 45:   0x7D      = FCS low (or escape sequence)
Byte 46:   0x5E      = Data or part of FCS
```

**Partial ASCII Extracted:**
```
ro... (rest encoded/encrypted)
```

**Analysis:**
- User input for login
- First 2 characters visible: "ro"
- Remaining 4 characters encoded (likely password or secured username)
- **Retransmitted 5 times** (lines 66-70) - NO RESPONSE RECEIVED
- This triggers connection reset at line 71

---

## Summary Statistics

### Frames Analyzed: 11 major frames (>4 bytes)

**By Direction:**
- SENT (100 → 102): 6 frames
- RECEIVED (102 → 100): 5 frames

**By Content Type:**
- Binary protocol: 3 frames
- PAD terminal protocol: 5 frames
- ASCII text: 3 frames

**ASCII Text Found:**
1. "*TADADM" / "D102" (terminal ID)
2. "--- RETROCORE EMULATED L ---" (banner)
3. " 17.08.42     13 OCTOBER   1997"
4. " SINTRAN III - VSX/500 L"
5. "ENTER " (prompt)
6. "PASSWORD: " (prompt)
7. "ro..." (user input)

**Largest Frame:** Frame #48 (174 bytes) - System banner

**Most Retransmitted:** Frame #66 (5 times) - User input with no response

---

## Full Path to Document

**Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\Deep_Frame_Analysis_Connected.md**
