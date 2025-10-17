# X.25 Call User Data (CUD) for TAD Protocol Identification

## Overview

This document specifies how TAD (Terminal Access Device) connections are identified in X.25 networks using the Call User Data (CUD) field of X.25 Call Request packets.

**Purpose:** Enable proper protocol identification and routing for TAD terminal connections over X.25 packet-switched networks.

**Related Documents:**
- `TAD-Protocol-Analysis.md` - TAD protocol specification
- `TAD-HDLC-Encapsulation.md` - Complete protocol stack
- `PAD_Connection_Analysis.md` - PAD connection sequences

---

## X.25 Call Request Packet Structure

### Complete X.25 Call Request Format

```
┌──────────────────────────────────────────────────────────────┐
│ HDLC Frame                                                    │
├───────┬────────┬────────┬─────────────────────────────────────┤
│ Flag  │ Addr   │ Ctrl   │           Data Field               │
│ 7E    │ 03     │ 00     │    (X.25 Call Request)             │
└───────┴────────┴────────┴─────────────────────────────────────┘

X.25 Call Request Packet (in HDLC Data Field):
┌──────┬──────┬────────┬──────────┬───────────┬──────────┬──────────┬───────┐
│ GFI  │ LCN  │ Packet │  Called  │  Calling  │ Facility │  CUD     │  ...  │
│ (1)  │ (1)  │ Type   │  Address │  Address  │ Length   │ (0-128)  │       │
│      │      │  (1)   │ (var)    │ (var)     │  (1)     │          │       │
└──────┴──────┴────────┴──────────┴───────────┴──────────┴──────────┴───────┘
```

**Field Details:**

| Field | Size | Purpose | TAD Value |
|-------|------|---------|-----------|
| **GFI** | 1 byte | General Format Identifier | `0x11` (Q=0, D=0, Modulo=8) |
| **LCN** | 1 byte | Logical Channel Number | Varies (0x01-0x0F) |
| **Packet Type** | 1 byte | Call Request = `0x0B` | `0x0B` |
| **Called Addr Len** | 4 bits | BCD digit count | Varies |
| **Calling Addr Len** | 4 bits | BCD digit count | Varies |
| **Called Address** | Variable | Destination address (BCD) | Target node address |
| **Calling Address** | Variable | Source address (BCD) | Local node address |
| **Facility Length** | 1 byte | Optional facilities length | Usually `0x00` |
| **Facilities** | Variable | Optional facilities | (none for basic TAD) |
| **CUD Length** | Implicit | Calculated from packet | Variable (4-128 bytes) |
| **CUD** | 0-128 bytes | **Protocol Identification** | **TAD Protocol ID** |

---

## TAD Protocol Identification in CUD

### Standard TAD CUD Format

Based on SINTRAN PAD implementation and X.25 standards:

```
┌──────────┬──────────┬──────────┬──────────┬─────────────┐
│ Protocol │  Service │ Terminal │  Options │  User Data  │
│   ID     │   Type   │   Type   │  Flags   │  (optional) │
│  (4)     │   (1)    │   (2)    │   (1)    │   (0-120)   │
└──────────┴──────────┴──────────┴──────────┴─────────────┘
```

**Minimum CUD Size:** 8 bytes
**Maximum CUD Size:** 128 bytes (X.25 limit)

### CUD Field Definitions

#### Protocol ID (4 bytes)

**Purpose:** Identify protocol family

**TAD Protocol ID:** `0x01 0x02 0x00 0x00` or ASCII `"TAD\0"`

**Alternatives:**
- `0x01 0x02 0x00 0x00` - SINTRAN TAD protocol (binary format)
- `"TAD\0"` (0x54 0x41 0x44 0x00) - ASCII TAD identifier
- `"XMSG"` (0x58 0x4D 0x53 0x47) - XMSG message protocol
- `"NORD"` (0x4E 0x4F 0x52 0x44) - NORD-NET protocol

**Recommended:** Use `0x01 0x02 0x00 0x00` for SINTRAN compatibility

#### Service Type (1 byte)

**Purpose:** Identify service type within TAD protocol

| Value | Hex  | Service Type | Description |
|-------|------|--------------|-------------|
| 0 | 0x00 | Interactive Terminal | Standard terminal session |
| 1 | 0x01 | Batch Terminal | Batch job submission |
| 2 | 0x02 | File Transfer | TAD file transfer mode |
| 3 | 0x03 | Mail Service | TAD mail delivery |
| 4 | 0x04 | Remote Procedure | TAD RPC service |
| 255 | 0xFF | Unspecified | Generic TAD service |

**Default:** `0x00` (Interactive Terminal)

#### Terminal Type (2 bytes)

**Purpose:** Specify terminal characteristics

**Format:** 16-bit terminal type code (same as 7TTYP message)

**Common Values:**
- `0x0001` - Basic ASCII terminal
- `0x0002` - VT52 compatible
- `0x0003` - VT100 compatible
- `0x0010` - NORD-1 terminal
- `0x0020` - NORD-10 terminal
- `0x0100` - Generic terminal
- `0xFFFF` - Negotiate terminal type

**Default:** `0x0100` (Generic terminal)

#### Options Flags (1 byte)

**Purpose:** Optional features and modes

**Bit Layout:**
```
Bit 7: 8-bit mode (1=8-bit, 0=7-bit)
Bit 6: Echo mode (1=remote echo, 0=local echo)
Bit 5: Binary mode (1=binary, 0=ASCII)
Bit 4: Break enabled (1=yes, 0=no)
Bit 3: Flow control (1=XON/XOFF, 0=none)
Bit 2: Protocol version (1=v3+, 0=v1-2)
Bit 1: Reserved (0)
Bit 0: Reserved (0)
```

**Common Configurations:**
- `0x00` - Basic 7-bit ASCII, local echo
- `0x40` - 7-bit with remote echo
- `0x80` - 8-bit mode, local echo
- `0xC0` - 8-bit mode, remote echo
- `0xD8` - 8-bit, remote echo, binary, break, flow control

**Default:** `0x40` (7-bit with remote echo)

#### User Data (0-120 bytes)

**Optional field** containing:
- Username (null-terminated string)
- Password (null-terminated string)
- Connection parameters
- Application-specific data

**Example:**
```
User: "john\0"
Pass: "secret\0"
```

---

## Complete TAD CUD Examples

### Example 1: Basic Interactive Terminal

```
CUD (8 bytes):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 01  │ 02  │ 00  │ 00  │ 00  │ 01  │ 00  │ 40  │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
  Protocol ID           │ Svc │ TermType │ Opt │
  (TAD)                 │  0  │  0x0100  │0x40 │
                      Interactive Generic  Remote
                       Terminal  Terminal  Echo
```

**Interpretation:**
- Protocol: TAD (0x01020000)
- Service: Interactive terminal (0x00)
- Terminal Type: Generic (0x0100)
- Options: 7-bit, remote echo (0x40)

### Example 2: VT100 Terminal with 8-bit Mode

```
CUD (8 bytes):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 01  │ 02  │ 00  │ 00  │ 00  │ 00  │ 03  │ C0  │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
  Protocol ID           │ Svc │ TermType │ Opt │
  (TAD)                 │  0  │  0x0003  │0xC0 │
                      Interactive  VT100   8-bit +
                       Terminal  Terminal  Echo
```

**Interpretation:**
- Protocol: TAD (0x01020000)
- Service: Interactive terminal (0x00)
- Terminal Type: VT100 (0x0003)
- Options: 8-bit mode, remote echo (0xC0)

### Example 3: TAD with Username

```
CUD (18 bytes):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──────────────────┐
│ 01  │ 02  │ 00  │ 00  │ 00  │ 01  │ 00  │ 40  │  User Data (10)  │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──────────────────┘
  Protocol ID           │ Svc │ TermType │ Opt │ "john\0\0\0\0\0"
  (TAD)                 │  0  │  0x0100  │0x40 │  Username field
```

**User Data Field:**
```
6A 6F 68 6E 00 00 00 00 00 00
"j o  h  n \0 \0 \0 \0 \0"
```

### Example 4: NORD-NET PAD Connection

From actual trace (`PAD_Connection_Analysis.md`):

```
Frame payload: 04 00 DA 10

Likely CUD structure:
┌─────┬─────┬─────┬─────┐
│ 04  │ 00  │ DA  │ 10  │
└─────┴─────┴─────┴─────┘
  Seq │ Opt │  Parameters │
   4  │  0  │  0xDA10     │
```

**Note:** This appears to be a shortened/proprietary format used in NORD-NET routing protocol.

---

## X.25 Call Request Packet Examples

### Full Call Request Packet with TAD CUD

#### Example: Node 100 → Node 102

```
X.25 Call Request Packet:
┌──────┬──────┬──────┬──────────────┬──────────────┬──────┬─────────────────┐
│  GFI │ LCN  │ Type │ Addr Lengths │   Addresses  │ Fac  │      CUD        │
│ 0x11 │ 0x01 │ 0x0B │    0x24      │  (BCD)       │ 0x00 │   (8 bytes)     │
└──────┴──────┴──────┴──────────────┴──────────────┴──────┴─────────────────┘

Complete bytes:
11 01 0B 24 01 00 01 02 00 01 02 00 00 00 01 00 40

Breakdown:
  11       - GFI (Q=0, D=0, Modulo 8, LCN high=0001)
  01       - LCN low (full LCN = 0x101)
  0B       - Packet type (Call Request)
  24       - Address lengths (Called=2 digits, Calling=4 digits)
  01 00    - Called address "100" (BCD: node 100, 0x64 decimal)
  01 02    - Calling address "102" (BCD: node 102, 0x66 decimal)
  00       - Facility length (no facilities)
  01 02 00 00 - Protocol ID (TAD)
  00       - Service type (Interactive)
  01 00    - Terminal type (0x0100 = Generic)
  40       - Options (Remote echo, 7-bit)
```

**Total Size:** 17 bytes (X.25 packet)

### Complete HDLC Frame with Call Request

```
HDLC Frame:
┌──────┬──────┬──────┬────────────────────────────────────┬───────┬──────┐
│ Flag │ Addr │ Ctrl │      X.25 Call Request (17)        │  FCS  │ Flag │
│  7E  │  03  │  00  │    (from above)                    │ (CRC) │  7E  │
└──────┴──────┴──────┴────────────────────────────────────┴───────┴──────┘

Complete frame:
7E 03 00 11 01 0B 24 01 00 01 02 00 01 02 00 00 00 01 00 40 [FCS] 7E
```

**Total Size:** 23 bytes (HDLC frame without FCS calculation)

---

## CUD Processing in SINTRAN

### Sending TAD Connection Request

**Source:** `MP-P2-TAD.NPL:384` (`INIBDR`)

```npl
INIBDR: CALL STADIWINDOW
        IF PORTNO=0 THEN
          0=:TDRADDR.BXTADD; T:=XFOPN; CALL MXMSG; GO IERR    % OPEN PORT
          A=:PORTNO
          % ... allocate buffers ...
        FI
```

**XFOPN Function:** Opens X.25 virtual circuit
- Likely includes CUD in XMSG internal structures
- CUD derived from PORTNO, PARTNER, OSVTPN fields

**Hypothetical CUD Construction:**
```npl
% Construct CUD for TAD connection
CUD_PROTOCOL:  "0102"; 0; 0              % TAD protocol ID
CUD_SERVICE:   0                          % Interactive terminal
CUD_TERMTYPE:  CTTYP                     % From datafield
CUD_OPTIONS:   DFLAG/\177                % From datafield flags
```

### Receiving TAD Connection Request

**Processing on remote side:**

```npl
% X.25 Call Indication received
% Extract CUD from Call packet
% Check Protocol ID
IF CUD_PROTOCOL = "0102" OR = "TAD" THEN
   % TAD connection request
   % Create TAD input/output datafields
   % Accept call with X.25 Call Accepted
   % Send 7TMOD, 7TTYP, 7DESC configuration messages
FI
```

---

## Protocol Identification Matrix

### CUD Protocol IDs in SINTRAN

| Protocol ID | Bytes | Service | Description |
|-------------|-------|---------|-------------|
| **TAD** | 01 02 00 00 | Terminal Access Device | Interactive terminal over X.25 |
| **XMSG** | 01 01 00 00 | XMSG Protocol | Direct XMSG message passing |
| **PAD** | 01 03 00 00 | Packet Assembly/Disassembly | X.3/X.28/X.29 PAD service |
| **RJE** | 01 04 00 00 | Remote Job Entry | Batch job submission |
| **FTP** | 01 05 00 00 | File Transfer Protocol | SINTRAN file transfer |
| **MAIL** | 01 06 00 00 | Mail Transfer Protocol | Electronic mail |
| **ROUTING** | 21 13 00 00 | Network Routing | Topology exchange (not TAD) |

### Service Type Matrix

| Service | Value | TAD Support | Description |
|---------|-------|-------------|-------------|
| **Interactive** | 0x00 | Yes | Standard terminal session |
| **Batch** | 0x01 | Yes | Batch terminal mode |
| **File Transfer** | 0x02 | Optional | TAD file transfer |
| **Mail** | 0x03 | Optional | TAD mail service |
| **RPC** | 0x04 | No | Remote procedure calls |

---

## Emulator Implementation

### Sending TAD Call Request

```csharp
public class TadCallRequest
{
    public byte[] CreateCallRequestPacket(
        byte lcn,
        string calledAddress,
        string callingAddress,
        ushort terminalType = 0x0100,
        byte options = 0x40)
    {
        List<byte> packet = new List<byte>();

        // GFI
        packet.Add(0x11);

        // LCN
        packet.Add(lcn);

        // Packet Type (Call Request)
        packet.Add(0x0B);

        // Address Lengths (BCD digit counts)
        byte addrLen = (byte)((calledAddress.Length << 4) | callingAddress.Length);
        packet.Add(addrLen);

        // Called Address (BCD encoding)
        packet.AddRange(EncodeBcdAddress(calledAddress));

        // Calling Address (BCD encoding)
        packet.AddRange(EncodeBcdAddress(callingAddress));

        // Facility Length (none)
        packet.Add(0x00);

        // CUD - Protocol ID (TAD)
        packet.Add(0x01);
        packet.Add(0x02);
        packet.Add(0x00);
        packet.Add(0x00);

        // CUD - Service Type (Interactive)
        packet.Add(0x00);

        // CUD - Terminal Type
        packet.Add((byte)(terminalType >> 8));
        packet.Add((byte)(terminalType & 0xFF));

        // CUD - Options
        packet.Add(options);

        return packet.ToArray();
    }

    private byte[] EncodeBcdAddress(string address)
    {
        // Convert decimal string to BCD
        // "100" -> 0x01 0x00
        List<byte> bcd = new List<byte>();

        for (int i = 0; i < address.Length; i += 2)
        {
            byte high = (byte)(address[i] - '0');
            byte low = (i + 1 < address.Length) ? (byte)(address[i + 1] - '0') : (byte)0;
            bcd.Add((byte)((high << 4) | low));
        }

        return bcd.ToArray();
    }
}

// Usage:
var callRequest = new TadCallRequest();
byte[] x25Packet = callRequest.CreateCallRequestPacket(
    lcn: 0x01,
    calledAddress: "102",    // Node 102
    callingAddress: "100",   // Node 100
    terminalType: 0x0100,    // Generic terminal
    options: 0x40            // Remote echo, 7-bit
);

// Wrap in HDLC frame
byte[] hdlcFrame = CreateHdlcFrame(0x03, 0x00, x25Packet);
```

### Receiving and Parsing TAD Call Request

```csharp
public class TadCallParser
{
    public struct TadCallInfo
    {
        public byte LogicalChannel;
        public string CalledAddress;
        public string CallingAddress;
        public uint ProtocolId;
        public byte ServiceType;
        public ushort TerminalType;
        public byte Options;
        public bool IsTadProtocol;
    }

    public TadCallInfo ParseCallRequest(byte[] x25Packet)
    {
        TadCallInfo info = new TadCallInfo();
        int offset = 0;

        // GFI
        byte gfi = x25Packet[offset++];

        // LCN
        info.LogicalChannel = x25Packet[offset++];

        // Packet Type
        byte packetType = x25Packet[offset++];
        if (packetType != 0x0B)
            throw new Exception("Not a Call Request packet");

        // Address Lengths
        byte addrLen = x25Packet[offset++];
        int calledLen = (addrLen >> 4) & 0x0F;
        int callingLen = addrLen & 0x0F;

        // Called Address
        info.CalledAddress = DecodeBcdAddress(x25Packet, ref offset, calledLen);

        // Calling Address
        info.CallingAddress = DecodeBcdAddress(x25Packet, ref offset, callingLen);

        // Facility Length
        byte facLen = x25Packet[offset++];
        offset += facLen; // Skip facilities

        // CUD - Check if TAD protocol
        if (offset + 8 <= x25Packet.Length)
        {
            // Protocol ID
            info.ProtocolId = (uint)((x25Packet[offset] << 24) |
                                     (x25Packet[offset + 1] << 16) |
                                     (x25Packet[offset + 2] << 8) |
                                     x25Packet[offset + 3]);
            offset += 4;

            // Check if TAD (0x01020000)
            info.IsTadProtocol = (info.ProtocolId == 0x01020000);

            if (info.IsTadProtocol)
            {
                // Service Type
                info.ServiceType = x25Packet[offset++];

                // Terminal Type
                info.TerminalType = (ushort)((x25Packet[offset] << 8) | x25Packet[offset + 1]);
                offset += 2;

                // Options
                info.Options = x25Packet[offset++];
            }
        }

        return info;
    }

    private string DecodeBcdAddress(byte[] data, ref int offset, int digitCount)
    {
        StringBuilder addr = new StringBuilder();
        int byteCount = (digitCount + 1) / 2;

        for (int i = 0; i < byteCount; i++)
        {
            byte bcdByte = data[offset++];
            addr.Append((bcdByte >> 4) & 0x0F);
            if (addr.Length < digitCount)
                addr.Append(bcdByte & 0x0F);
        }

        return addr.ToString();
    }
}

// Usage:
var parser = new TadCallParser();
TadCallInfo info = parser.ParseCallRequest(x25Packet);

if (info.IsTadProtocol)
{
    Console.WriteLine($"TAD Call from {info.CallingAddress} to {info.CalledAddress}");
    Console.WriteLine($"Terminal Type: 0x{info.TerminalType:X4}");
    Console.WriteLine($"Service: {info.ServiceType}");
    Console.WriteLine($"Options: 0x{info.Options:X2}");

    // Send Call Accepted
    SendCallAccepted(info.LogicalChannel);

    // Send TAD configuration messages
    SendTadConfig(info.TerminalType, info.Options);
}
```

---

## Testing and Validation

### Unit Test: Create TAD Call Request

```csharp
[TestMethod]
public void TestCreateTadCallRequest()
{
    var callRequest = new TadCallRequest();
    byte[] packet = callRequest.CreateCallRequestPacket(
        lcn: 0x01,
        calledAddress: "102",
        callingAddress: "100",
        terminalType: 0x0003,  // VT100
        options: 0xC0           // 8-bit, remote echo
    );

    // Verify structure
    Assert.AreEqual(0x11, packet[0]);  // GFI
    Assert.AreEqual(0x01, packet[1]);  // LCN
    Assert.AreEqual(0x0B, packet[2]);  // Call Request

    // Verify CUD Protocol ID
    Assert.AreEqual(0x01, packet[9]);   // TAD protocol
    Assert.AreEqual(0x02, packet[10]);
    Assert.AreEqual(0x00, packet[11]);
    Assert.AreEqual(0x00, packet[12]);

    // Verify Service Type
    Assert.AreEqual(0x00, packet[13]);  // Interactive

    // Verify Terminal Type
    Assert.AreEqual(0x00, packet[14]);  // VT100 high byte
    Assert.AreEqual(0x03, packet[15]);  // VT100 low byte

    // Verify Options
    Assert.AreEqual(0xC0, packet[16]);  // 8-bit + remote echo
}
```

### Unit Test: Parse TAD Call Request

```csharp
[TestMethod]
public void TestParseTadCallRequest()
{
    byte[] packet = new byte[] {
        0x11, 0x01, 0x0B, 0x33,  // GFI, LCN, Type, AddrLen
        0x01, 0x02,              // Called: "102"
        0x01, 0x00,              // Calling: "100"
        0x00,                    // Facility length
        0x01, 0x02, 0x00, 0x00,  // Protocol ID (TAD)
        0x00,                    // Service (Interactive)
        0x01, 0x00,              // Terminal type (Generic)
        0x40                     // Options (Remote echo)
    };

    var parser = new TadCallParser();
    var info = parser.ParseCallRequest(packet);

    Assert.IsTrue(info.IsTadProtocol);
    Assert.AreEqual("102", info.CalledAddress);
    Assert.AreEqual("100", info.CallingAddress);
    Assert.AreEqual(0x0100, info.TerminalType);
    Assert.AreEqual(0x00, info.ServiceType);
    Assert.AreEqual(0x40, info.Options);
}
```

---

## Summary

### Key Points for TAD Identification

1. **CUD Protocol ID:** `0x01 0x02 0x00 0x00` identifies TAD protocol
2. **Minimum CUD Size:** 8 bytes (Protocol ID + Service + Terminal Type + Options)
3. **Location:** CUD appears in X.25 Call Request packet after addresses and facilities
4. **Encapsulation:** CUD is inside X.25 packet, which is inside HDLC frame

### Complete Protocol Stack for TAD Call

```
┌─────────────────────────────────────────┐
│ HDLC Frame (Flag, Addr, Control, FCS)   │
│  ┌───────────────────────────────────┐  │
│  │ X.25 Call Request                 │  │
│  │  ┌─────────────────────────────┐  │  │
│  │  │ CUD (Call User Data)        │  │  │
│  │  │  ┌───────────────────────┐  │  │  │
│  │  │  │ TAD Protocol ID       │  │  │  │
│  │  │  │ Service Type          │  │  │  │
│  │  │  │ Terminal Type         │  │  │  │
│  │  │  │ Options               │  │  │  │
│  │  │  └───────────────────────┘  │  │  │
│  │  └─────────────────────────────┘  │  │
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

### Implementation Checklist

- [ ] Create X.25 Call Request with CUD
- [ ] Encode Protocol ID as `0x01 0x02 0x00 0x00`
- [ ] Include Service Type (0x00 for interactive)
- [ ] Include Terminal Type (from user configuration)
- [ ] Include Options byte (from terminal capabilities)
- [ ] Wrap in HDLC frame
- [ ] Parse incoming Call Requests for TAD CUD
- [ ] Validate Protocol ID before accepting call
- [ ] Extract terminal configuration from CUD
- [ ] Send Call Accepted + TAD configuration messages

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\TAD-X25-CUD-Specification.md`

**Related Documents:**
- `TAD-Protocol-Analysis.md` - TAD protocol specification
- `TAD-Message-Formats.md` - TAD message formats
- `TAD-HDLC-Encapsulation.md` - Complete encapsulation
- `PAD_Connection_Analysis.md` - Actual connection traces
