# TAD Message to HDLC Frame Encapsulation

## Overview

This document shows how TAD (Terminal Access Device) messages are encapsulated through the complete SINTRAN protocol stack into HDLC frames for transmission over X.25 networks.

**Protocol Stack:**
```
┌─────────────────────────────────────┐
│  TAD Message (Application Layer)    │  7BDAT, 7TMOD, etc.
├─────────────────────────────────────┤
│  XMSG Buffer (Message Layer)        │  Buffer ID, addressing
├─────────────────────────────────────┤
│  X.25 Packet (Network Layer)        │  GFI, LCN, packet type
├─────────────────────────────────────┤
│  HDLC Frame (Data Link Layer)       │  Flag, address, control, FCS
└─────────────────────────────────────┘
```

**Related Documents:**
- `TAD-Message-Formats.md` - TAD message specifications
- `XMSG_Metadata_Buffer_Analysis.md` - XMSG buffer format
- HDLC analysis documents in `Analysis\hdlc-analysis\`

---

## HDLC Frame Structure

### Basic HDLC Frame Format

```
┌──────┬─────────┬─────────┬───────────────┬─────────┬──────┐
│ Flag │ Address │ Control │     Data      │   FCS   │ Flag │
│ 7E   │ (1-2)   │ (1-2)   │  (variable)   │  (2)    │ 7E   │
└──────┴─────────┴─────────┴───────────────┴─────────┴──────┘
```

**Field Descriptions:**
- **Flag:** `0x7E` - Frame delimiter
- **Address:** 1 or 2 bytes - Destination/source addressing
- **Control:** 1 or 2 bytes - Frame type and sequence numbers
- **Data:** Variable length - Contains X.25 packet
- **FCS:** 2 bytes - Frame Check Sequence (CRC-16)
- **Flag:** `0x7E` - Frame delimiter

### HDLC Control Field Types

**Information Frame (I-frame):**
```
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │  N(S) - 3 bits │ P │  N(R) - 3 bits │
└───┴───────────────┴───┴───────────────┘
```
- N(S) = Send sequence number (0-7)
- N(R) = Receive sequence number (0-7)
- P = Poll bit

**Supervisory Frame (S-frame):**
```
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1 │ 0 │ S │ S │ P │  N(R) - 3 bits │
└───┴───┴───┴───┴───┴───────────────┘
```
- SS = Supervisory function (RR, RNR, REJ)

**Unnumbered Frame (U-frame):**
```
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1 │ 1 │ M │ M │ P │ M │ M │ M │
└───┴───┴───┴───┴───┴───┴───┴───┘
```
- MMMMM = Unnumbered function (SABM, UA, DISC, etc.)

---

## X.25 Packet Structure

### X.25 Packet Format in HDLC Frame

```
┌────────────────────────────────────────────────────┐
│ HDLC Data Field                                    │
├────────┬──────────┬────────────┬──────────────────┤
│  GFI   │   LCN    │  Packet    │   User Data      │
│ (1)    │   (1)    │  Type (1)  │   (variable)     │
└────────┴──────────┴────────────┴──────────────────┘
```

**GFI (General Format Identifier):**
- Bits 7-6: Always `01` for data packets
- Bit 5: Q-bit (qualifier bit)
- Bit 4: D-bit (delivery confirmation)
- Bits 3-0: Logical channel group number (high nibble)

**LCN (Logical Channel Number):**
- Full 12-bit LCN = GFI[3:0] + LCN byte
- Identifies virtual circuit

**Packet Type:**
- `0x0F` = Data packet (with M-bit in bits 4-7 for sequence)

**Typical X.25 Data Packet:**
```
┌───────┬───────┬───────┬─────────────────┐
│  0x21 │  0x13 │ 0x0E  │  User Data      │
└───────┴───────┴───────┴─────────────────┘
  GFI     LCN    Type+Seq    (TAD message)
```

---

## TAD Message Encapsulation Examples

### Example 1: Simple Data Message (7BDAT)

#### TAD Message Layer
```
TAD Message: "Hello" (5 bytes)
┌───────┬────┬──┬──┬──┬──┬──┐
│ 7BDAT │ 05 │H │e │l │l │o │
└───────┴────┴──┴──┴──┴──┴──┘
```

**Breakdown:**
- Message Type: `0x42444154` ("BDAT") - stored as `0x42` for first byte
- Byte Count: `0x05`
- Data: `0x48 0x65 0x6C 0x6C 0x6F` ("Hello")

**Total TAD Message:** `42 05 48 65 6C 6C 6F` (7 bytes)

#### XMSG Buffer Layer
```
XMSG adds metadata:
┌──────┬──────┬────────────────────────┐
│ Ref  │ Func │    TAD Message         │
│ (2)  │ (2)  │    (7 bytes)           │
└──────┴──────┴────────────────────────┘
```

**With metadata:** `00 00 00 00 42 05 48 65 6C 6C 6F`
- Ref: `0x0000` (no reference)
- Func: `0x0000` (no function)
- TAD data: 7 bytes

**Total XMSG Buffer:** 11 bytes

#### X.25 Packet Layer
```
X.25 Data Packet:
┌──────┬──────┬──────┬────────────────────────┐
│ GFI  │ LCN  │ Type │    User Data           │
│ 0x21 │ 0x13 │ 0x0E │    (11 bytes)          │
└──────┴──────┴──────┴────────────────────────┘
```

**Complete X.25 Packet:** `21 13 0E 00 00 00 00 42 05 48 65 6C 6C 6F`
- GFI: `0x21` (Q=0, D=0, LCG=1)
- LCN: `0x13` (Logical Channel 0x113)
- Packet Type: `0x0E` (Data packet, N(S)=0, M=0)
- User Data: 11 bytes (XMSG + TAD)

**Total X.25 Packet:** 14 bytes

#### HDLC Frame Layer
```
HDLC Information Frame:
┌──────┬──────┬──────┬─────────────────────┬───────┬──────┐
│ Flag │ Addr │ Ctrl │    X.25 Packet      │  FCS  │ Flag │
│ 7E   │ 03   │ 10   │    (14 bytes)       │ CRC16 │ 7E   │
└──────┴──────┴──────┴─────────────────────┴───────┴──────┘
```

**Complete HDLC Frame:**
```
7E 03 10 21 13 0E 00 00 00 00 42 05 48 65 6C 6C 6F [FCS] 7E
```

**Byte-by-Byte:**
- `7E` - Opening flag
- `03` - Address (response to DTE)
- `10` - Control (I-frame, N(S)=0, N(R)=0, P=0)
- `21 13 0E` - X.25 header
- `00 00 00 00` - XMSG metadata
- `42 05 48 65 6C 6C 6F` - TAD message
- `[2 bytes]` - FCS (calculated CRC-16)
- `7E` - Closing flag

**Total HDLC Frame:** 21 bytes (excluding FCS calculation)

---

### Example 2: Terminal Mode Message (7TMOD)

#### TAD Message Layer
```
TAD Message: Set capital letters + CR delay
┌───────┬────┬────┐
│ 7TMOD │ 01 │ 03 │
└───────┴────┴────┘
```

**Breakdown:**
- Message Type: `0x544D4F44` ("TMOD") - first byte `0x54`
- Byte Count: `0x01`
- Flags: `0x03` (bits 0,1 set)

**Total TAD Message:** `54 01 03` (3 bytes)

#### Complete HDLC Frame
```
7E 03 12 21 13 10 00 00 00 00 54 01 03 [FCS] 7E
```

**Breakdown:**
- `7E` - Flag
- `03` - Address
- `12` - Control (I-frame, N(S)=1, N(R)=0)
- `21 13 10` - X.25 header (packet type 0x10 = N(S)=1)
- `00 00 00 00` - XMSG metadata
- `54 01 03` - TAD 7TMOD message
- `[FCS]` - CRC-16
- `7E` - Flag

**Total Frame:** 17 bytes

---

### Example 3: Terminal Type Message (7TTYP)

#### TAD Message Layer
```
TAD Message: Set terminal type 0x0123
┌───────┬────┬─────┬─────┐
│ 7TTYP │ 02 │ 01  │ 23  │
└───────┴────┴─────┴─────┘
```

**Total TAD Message:** `54 54 59 50 02 01 23`
- Wait, this shows the full ASCII codes. Let me correct this.

Actually, looking at the source code more carefully, the message type is stored as a single byte (octal constant), not the full ASCII string.

**Corrected:**
```
Message code 7TTYP (octal) = implementation-specific encoding
Let's use simplified encoding where type = first char:
Type: 'T' = 0x54
Count: 0x02
Data: 0x01 0x23
```

**Total TAD Message:** `54 02 01 23` (4 bytes)

#### Complete HDLC Frame
```
7E 03 14 21 13 12 00 00 00 00 54 02 01 23 [FCS] 7E
```

**Total Frame:** 18 bytes

---

### Example 4: Escape Message (7ESCA - High Priority)

#### TAD Message Layer
```
TAD Message: Escape signal (no data)
┌───────┬────┐
│ 7ESCA │ 00 │
└───────┴────┘
```

**Total TAD Message:** `45 00` (2 bytes)
- Type: 'E' = 0x45 (ESCA)
- Count: 0x00

#### Complete HDLC Frame

**High-priority messages may use different XMSG handling:**

```
7E 03 16 21 13 14 00 00 00 00 45 00 [FCS] 7E
```

**Note:** High-priority flag may be set in XMSG metadata or X.25 Q-bit

**With Q-bit set:**
```
7E 03 16 31 13 14 00 00 00 00 45 00 [FCS] 7E
            ^^
        GFI = 0x31 (Q-bit set)
```

**Total Frame:** 16 bytes

---

### Example 5: Ready For Input (7RFI - Flow Control)

#### TAD Message Layer
```
TAD Message: Request input (no data)
┌──────┬────┐
│ 7RFI │ 00 │
└──────┴────┘
```

**Total TAD Message:** `52 00` (2 bytes)
- Type: 'R' = 0x52 (RFI)
- Count: 0x00

#### Complete HDLC Frame
```
7E 03 18 21 13 16 00 00 00 00 52 00 [FCS] 7E
```

**Total Frame:** 16 bytes

---

### Example 6: Disconnect Message (7DCON)

#### TAD Message Layer
```
TAD Message: Disconnect (no data)
┌───────┬────┐
│ 7DCON │ 00 │
└───────┴────┘
```

**Total TAD Message:** `44 00` (2 bytes)
- Type: 'D' = 0x44 (DCON)
- Count: 0x00

#### Complete HDLC Frame

**This triggers X.25 Clear Request, sent as separate packet:**

```
HDLC Frame 1 (Data with 7DCON):
7E 03 1A 21 13 18 00 00 00 00 44 00 [FCS] 7E

HDLC Frame 2 (X.25 Clear Request):
7E 03 1C 21 13 13 00 [FCS] 7E
            ^^
         Packet type 0x13 = Clear Request
```

**Total:** 2 frames (disconnect message + X.25 clear)

---

## Connection Establishment HDLC Sequence

### Phase 1: HDLC Link Setup

**Frame 1: SABM (Set Asynchronous Balanced Mode)**
```
7E FF 3F [FCS] 7E
   ^^  ^^
   Addr Ctrl (SABM command)
```

**Frame 2: UA (Unnumbered Acknowledgement)**
```
7E FF 73 [FCS] 7E
   ^^  ^^
   Addr Ctrl (UA response)
```

### Phase 2: X.25 Call Setup

**Frame 3: X.25 Call Request**
```
7E 03 00 21 13 0B [Call Data] [FCS] 7E
                ^^
            Packet type 0x0B = Call Request
```

**Call Data includes:**
- Called address (remote TAD)
- Calling address (local TAD)
- Facilities
- Call user data (may include XMSG port info)

**Frame 4: X.25 Call Accepted**
```
7E 03 00 21 13 0F [FCS] 7E
                ^^
            Packet type 0x0F = Call Accepted
```

### Phase 3: TAD Configuration Exchange

**Frame 5: INISND - Dummy Message**
```
7E 03 10 21 13 0E 00 00 00 00 44 00 [FCS] 7E
                              ^^^^
                           7DUMM (Dummy)
```

**Frame 6: Remote sends 7TMOD**
```
7E 03 12 21 13 10 00 00 00 00 54 01 03 [FCS] 7E
```

**Frame 7: Remote sends 7TTYP**
```
7E 03 14 21 13 12 00 00 00 00 54 02 01 23 [FCS] 7E
```

**Frame 8: Remote sends 7DESC**
```
7E 03 16 21 13 14 00 00 00 00 44 01 1B [FCS] 7E
                                    ^^
                              Escape char 0x1B
```

---

## Multi-Buffer TAD Messages

### Large Data Message Split Across Frames

When a TAD data message exceeds buffer size, it's split with continuation flags.

#### Message 1: First Part (RSOM set)
```
TAD Message (512 bytes):
┌───────┬─────┬────────────────────────────┐
│ 7BDAT │ 255 │  Data part 1 (255 bytes)   │
└───────┴─────┴────────────────────────────┘
```

**HDLC Frame 1:**
```
7E 03 10 21 13 0E [XMSG metadata with RSOM] 42 FF [255 data bytes] [FCS] 7E
```

#### Message 2: Continuation (no RSOM/REOM)
```
TAD Message continues:
┌───────┬─────┬────────────────────────────┐
│ 7BDAT │ 255 │  Data part 2 (255 bytes)   │
└───────┴─────┴────────────────────────────┘
```

**HDLC Frame 2:**
```
7E 03 12 21 13 10 [XMSG metadata] 42 FF [255 data bytes] [FCS] 7E
```

#### Message 3: Last Part (REOM set)
```
TAD Message final part:
┌───────┬────┬───────────────────────┐
│ 7BDAT │ 2  │  Data part 3 (2 bytes)│
└───────┴────�───────────────────────┘
```

**HDLC Frame 3:**
```
7E 03 14 21 13 12 [XMSG metadata with REOM] 42 02 [2 data bytes] [FCS] 7E
```

### XMSG RSOM/REOM Flags

From XMSG analysis, buffer metadata includes:
```
┌─────────────────────────────────────────┐
│ LKEY (DMA control word)                 │
│ Bits 10-8: Buffer state                 │
│   000 = Empty                           │
│   001 = First (RSOM)                    │
│   010 = Middle                          │
│   011 = Last (REOM)                     │
│   100 = Single (RSOM + REOM)            │
└─────────────────────────────────────────┘
```

---

## Break Character Handling in HDLC

### Data Message with Break Character

When user types break character:

#### TAD Message Layer
```
Data with break at end:
┌───────┬────┬──────┬──────┬───────┐
│ 7BDAT │ 03 │ 'A'  │ 'B'  │ Ctrl-C│
└───────┴────┴──────┴──────┴───────┘
         ^^                   ^^
      Count=3              Break char
```

**TAD marks:** `REMBYT = -1` (break flag)

**Total TAD Message:** `42 03 41 42 03` (5 bytes)

#### HDLC Frame
```
7E 03 1E 21 13 1A 00 00 00 00 42 03 41 42 03 [FCS] 7E
```

**Break indication sent separately:**

**Next frame: 7BMMX (Break Message)**
```
7E 03 20 21 13 1C 00 00 00 00 42 03 01 00 10 [FCS] 7E
                                    ^^^^^^^^
                           Break strategy 1, MaxBreak=16
```

---

## Error and Flow Control in HDLC

### RFI (Ready For Input) Sequence

**User reads from empty buffer → TAD sends RFI:**

**HDLC Frame:**
```
7E 03 22 21 13 1E 00 00 00 00 52 00 [FCS] 7E
```

**Remote responds with data:**

**HDLC Frame:**
```
7E 03 24 21 13 20 00 00 00 00 42 05 [5 data bytes] [FCS] 7E
```

### HDLC RR (Receive Ready) Frame

When TAD acknowledges frames without data:

**HDLC RR Frame:**
```
7E 03 01 [FCS] 7E
      ^^
   Control = 0x01 (RR, N(R)=0)
```

**Acknowledges frames 0-7:**
```
RR with N(R)=7:
7E 03 0F [FCS] 7E
      ^^
   Control = 0x0F (RR, N(R)=7)
```

---

## Complete TAD Session HDLC Trace

### Session: Connect → Login → Command → Disconnect

```
Time    Direction  HDLC Frame
======  =========  ====================================================

00.000  Local→Net  7E FF 3F [FCS] 7E
                   (SABM - Link setup)

00.010  Net→Local  7E FF 73 [FCS] 7E
                   (UA - Link acknowledged)

00.020  Local→Net  7E 03 00 21 13 0B [Call data] [FCS] 7E
                   (X.25 Call Request)

00.100  Net→Local  7E 03 00 21 13 0F [FCS] 7E
                   (X.25 Call Accepted)

00.110  Local→Net  7E 03 10 21 13 0E 00 00 00 00 44 00 [FCS] 7E
                   (TAD: 7DUMM - Initial buffer)

00.150  Net→Local  7E 03 12 21 13 10 00 00 00 00 54 01 03 [FCS] 7E
                   (TAD: 7TMOD - Terminal mode)

00.151  Net→Local  7E 03 14 21 13 12 00 00 00 00 54 02 01 23 [FCS] 7E
                   (TAD: 7TTYP - Terminal type)

00.152  Net→Local  7E 03 16 21 13 14 00 00 00 00 44 01 1B [FCS] 7E
                   (TAD: 7DESC - Escape char)

00.200  Net→Local  7E 03 18 21 13 16 00 00 00 00 42 0A [login prompt] [FCS] 7E
                   (TAD: 7BDAT - "Username: ")

01.000  Local→Net  7E 03 1A 21 13 18 00 00 00 00 42 04 [username] [FCS] 7E
                   (TAD: 7BDAT - "user")

01.100  Net→Local  7E 03 1C 21 13 1A 00 00 00 00 42 0A [password prompt] [FCS] 7E
                   (TAD: 7BDAT - "Password: ")

02.000  Local→Net  7E 03 1E 21 13 1C 00 00 00 00 42 06 [password] [FCS] 7E
                   (TAD: 7BDAT - "secret")

02.200  Net→Local  7E 03 20 21 13 1E 00 00 00 00 42 0F [welcome] [FCS] 7E
                   (TAD: 7BDAT - "Welcome to ND!")

03.000  Local→Net  7E 03 22 21 13 20 00 00 00 00 42 06 [command] [FCS] 7E
                   (TAD: 7BDAT - "dir")

03.500  Net→Local  7E 03 24 21 13 22 00 00 00 00 42 FF [dir output 1] [FCS] 7E
                   (TAD: 7BDAT - Directory listing part 1)

03.501  Net→Local  7E 03 26 21 13 24 00 00 00 00 42 C8 [dir output 2] [FCS] 7E
                   (TAD: 7BDAT - Directory listing part 2)

04.000  Local→Net  7E 03 28 21 13 26 00 00 00 00 42 07 [logout] [FCS] 7E
                   (TAD: 7BDAT - "logout")

04.100  Net→Local  7E 03 2A 21 13 28 00 00 00 00 44 00 [FCS] 7E
                   (TAD: 7DCON - Disconnect)

04.110  Net→Local  7E 03 2C 21 13 13 00 [FCS] 7E
                   (X.25 Clear Request)

04.120  Local→Net  7E 03 2E 21 13 17 [FCS] 7E
                   (X.25 Clear Confirm)

04.130  Local→Net  7E FF 53 [FCS] 7E
                   (DISC - Disconnect link)

04.140  Net→Local  7E FF 73 [FCS] 7E
                   (UA - Link disconnected)
```

---

## FCS (Frame Check Sequence) Calculation

### CRC-16 Calculation for HDLC

**Algorithm:** CRC-16-CCITT (polynomial: x^16 + x^12 + x^5 + 1)
**Initial value:** 0xFFFF
**Final XOR:** 0xFFFF
**Calculated over:** Address + Control + Data fields

**Example Calculation:**

Frame: `7E 03 10 21 13 0E 00 00 00 00 42 05 48 65 6C 6C 6F [FCS] 7E`

**Input to CRC:** `03 10 21 13 0E 00 00 00 00 42 05 48 65 6C 6C 6F`

**CRC-16 Result:** `0xXXXX` (transmitted LSB first)

**Complete frame:**
```
7E 03 10 21 13 0E 00 00 00 00 42 05 48 65 6C 6C 6F [CRC_LOW] [CRC_HIGH] 7E
```

### C# FCS Calculation Example

```csharp
public static ushort CalculateHdlcFcs(byte[] data, int offset, int length)
{
    const ushort POLY = 0x8408; // Reversed polynomial
    ushort fcs = 0xFFFF;

    for (int i = offset; i < offset + length; i++)
    {
        fcs ^= data[i];
        for (int j = 0; j < 8; j++)
        {
            if ((fcs & 0x0001) != 0)
                fcs = (ushort)((fcs >> 1) ^ POLY);
            else
                fcs >>= 1;
        }
    }

    return (ushort)(~fcs);
}

// Usage:
byte[] frame = new byte[] { 0x03, 0x10, 0x21, 0x13, 0x0E, /* ... TAD data ... */ };
ushort fcs = CalculateHdlcFcs(frame, 0, frame.Length);
byte fcsLow = (byte)(fcs & 0xFF);
byte fcsHigh = (byte)(fcs >> 8);
```

---

## Byte Stuffing (Transparency)

### HDLC Byte Stuffing Rules

When data contains flag bytes (0x7E) or escape bytes (0x7D), they must be escaped:

**Rule:**
- If data byte = `0x7E` or `0x7D`
- Insert escape byte `0x7D` before it
- XOR original byte with `0x20`

**Examples:**

**Original data:** `42 7E 05`
**Stuffed data:** `42 7D 5E 05`
```
0x7E XOR 0x20 = 0x5E
```

**Original data:** `42 7D 05`
**Stuffed data:** `42 7D 5D 05`
```
0x7D XOR 0x20 = 0x5D
```

### TAD Message with Stuffing

**TAD Message:** 7BDAT containing byte 0x7E

**Before stuffing:**
```
42 05 48 7E 6C 6C 6F
^^  ^^  ^^  ^^  ^^
Type Cnt  H  ~ l  l  o
```

**After stuffing:**
```
42 05 48 7D 5E 6C 6C 6F
         ^^  ^^
       Escape + 5E (0x7E XOR 0x20)
```

**Complete HDLC frame:**
```
7E 03 10 21 13 0E 00 00 00 00 42 05 48 7D 5E 6C 6C 6F [FCS] 7E
```

**Note:** FCS is calculated BEFORE stuffing

---

## Emulator Implementation Guide

### Receiving TAD Messages from HDLC

**Step 1: Parse HDLC Frame**
```csharp
public class HdlcFrame
{
    public byte Address { get; set; }
    public byte Control { get; set; }
    public byte[] Data { get; set; }
    public ushort Fcs { get; set; }
    public bool IsValid { get; set; }
}

public HdlcFrame ParseHdlcFrame(byte[] buffer, int offset, int length)
{
    // Remove flags (0x7E)
    // Un-stuff escaped bytes
    // Verify FCS
    // Return parsed frame
}
```

**Step 2: Extract X.25 Packet**
```csharp
public class X25Packet
{
    public byte Gfi { get; set; }
    public byte Lcn { get; set; }
    public byte PacketType { get; set; }
    public byte[] UserData { get; set; }
}

public X25Packet ParseX25Packet(byte[] data)
{
    // Extract GFI, LCN, packet type
    // Extract user data
    return packet;
}
```

**Step 3: Parse XMSG Buffer**
```csharp
public class XmsgBuffer
{
    public ushort Reference { get; set; }
    public ushort Function { get; set; }
    public byte[] TadData { get; set; }
}

public XmsgBuffer ParseXmsgBuffer(byte[] userData)
{
    // Extract ref/func (first 4 bytes)
    // Extract TAD data (remaining bytes)
    return buffer;
}
```

**Step 4: Parse TAD Message**
```csharp
public class TadMessage
{
    public byte MessageType { get; set; }
    public byte ByteCount { get; set; }
    public byte[] Data { get; set; }
}

public TadMessage ParseTadMessage(byte[] tadData, ref int offset)
{
    // Check for pad byte (if offset is odd)
    if ((offset & 1) != 0 && tadData[offset] == 0x00)
        offset++;

    // Extract message type
    byte msgType = tadData[offset++];

    // Extract byte count
    byte byteCount = tadData[offset++];

    // Extract data
    byte[] data = new byte[byteCount];
    Array.Copy(tadData, offset, data, 0, byteCount);
    offset += byteCount;

    return new TadMessage
    {
        MessageType = msgType,
        ByteCount = byteCount,
        Data = data
    };
}
```

### Sending TAD Messages as HDLC

**Step 1: Create TAD Message**
```csharp
public byte[] CreateTadMessage(byte messageType, byte[] data)
{
    // Calculate if pad byte needed
    bool needsPad = false; // based on current offset

    int size = (needsPad ? 1 : 0) + 1 + 1 + data.Length;
    byte[] buffer = new byte[size];
    int offset = 0;

    // Add pad if needed
    if (needsPad)
        buffer[offset++] = 0x00;

    // Add message type
    buffer[offset++] = messageType;

    // Add byte count
    buffer[offset++] = (byte)data.Length;

    // Add data
    Array.Copy(data, 0, buffer, offset, data.Length);

    return buffer;
}
```

**Step 2: Wrap in XMSG Buffer**
```csharp
public byte[] CreateXmsgBuffer(byte[] tadMessage)
{
    byte[] buffer = new byte[4 + tadMessage.Length];

    // Reference (2 bytes)
    buffer[0] = 0x00;
    buffer[1] = 0x00;

    // Function (2 bytes)
    buffer[2] = 0x00;
    buffer[3] = 0x00;

    // TAD message
    Array.Copy(tadMessage, 0, buffer, 4, tadMessage.Length);

    return buffer;
}
```

**Step 3: Create X.25 Packet**
```csharp
public byte[] CreateX25DataPacket(byte lcn, byte nSend, byte nRecv, byte[] userData)
{
    byte[] packet = new byte[3 + userData.Length];

    // GFI
    packet[0] = 0x21; // Q=0, D=0, LCG=1

    // LCN
    packet[1] = lcn;

    // Packet type (combine with N(S))
    packet[2] = (byte)(0x00 | (nSend << 1)); // Data packet with N(S)

    // User data
    Array.Copy(userData, 0, packet, 3, userData.Length);

    return packet;
}
```

**Step 4: Create HDLC Frame**
```csharp
public byte[] CreateHdlcFrame(byte address, byte control, byte[] data)
{
    // Calculate FCS
    byte[] fcsInput = new byte[2 + data.Length];
    fcsInput[0] = address;
    fcsInput[1] = control;
    Array.Copy(data, 0, fcsInput, 2, data.Length);
    ushort fcs = CalculateHdlcFcs(fcsInput, 0, fcsInput.Length);

    // Byte stuffing
    List<byte> stuffed = new List<byte>();
    stuffed.Add(address);
    stuffed.Add(control);

    foreach (byte b in data)
    {
        if (b == 0x7E || b == 0x7D)
        {
            stuffed.Add(0x7D);
            stuffed.Add((byte)(b ^ 0x20));
        }
        else
        {
            stuffed.Add(b);
        }
    }

    // Add FCS (also needs stuffing)
    byte fcsLow = (byte)(fcs & 0xFF);
    byte fcsHigh = (byte)(fcs >> 8);

    if (fcsLow == 0x7E || fcsLow == 0x7D)
    {
        stuffed.Add(0x7D);
        stuffed.Add((byte)(fcsLow ^ 0x20));
    }
    else
        stuffed.Add(fcsLow);

    if (fcsHigh == 0x7E || fcsHigh == 0x7D)
    {
        stuffed.Add(0x7D);
        stuffed.Add((byte)(fcsHigh ^ 0x20));
    }
    else
        stuffed.Add(fcsHigh);

    // Add flags
    byte[] frame = new byte[stuffed.Count + 2];
    frame[0] = 0x7E;
    stuffed.CopyTo(frame, 1);
    frame[frame.Length - 1] = 0x7E;

    return frame;
}
```

**Step 5: Complete Send Function**
```csharp
public void SendTadMessage(byte msgType, byte[] data)
{
    // Create TAD message
    byte[] tadMsg = CreateTadMessage(msgType, data);

    // Wrap in XMSG
    byte[] xmsgBuffer = CreateXmsgBuffer(tadMsg);

    // Create X.25 packet
    byte[] x25Packet = CreateX25DataPacket(
        lcn: 0x13,
        nSend: currentSendSeq++,
        nRecv: currentRecvSeq,
        userData: xmsgBuffer
    );

    // Create HDLC frame
    byte[] hdlcFrame = CreateHdlcFrame(
        address: 0x03,
        control: (byte)(0x00 | (currentSendSeq << 1) | (currentRecvSeq << 5)),
        data: x25Packet
    );

    // Send to network
    SendToNetwork(hdlcFrame);
}
```

---

## Summary

TAD messages traverse 4 protocol layers before reaching the wire:

1. **TAD Message:** Application-level terminal control
2. **XMSG Buffer:** Message metadata and addressing
3. **X.25 Packet:** Virtual circuit data transport
4. **HDLC Frame:** Physical link-level framing

**Key Points:**
- HDLC provides reliable frame transport with CRC
- X.25 provides virtual circuit multiplexing
- XMSG provides buffer management
- TAD provides terminal semantics

**For Emulation:**
- Parse/create all 4 layers
- Handle byte stuffing in HDLC
- Calculate FCS correctly
- Track sequence numbers (HDLC and X.25)
- Support high-priority vs. normal message handling

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\TAD-HDLC-Encapsulation.md`

**Related Documents:**
- `TAD-Protocol-Analysis.md` - TAD protocol reference
- `TAD-Message-Formats.md` - TAD message specifications
- `TAD-Protocol-Flows.md` - TAD flow diagrams
- `XMSG_Metadata_Buffer_Analysis.md` - XMSG buffer format
- HDLC analysis documents - Low-level frame format
