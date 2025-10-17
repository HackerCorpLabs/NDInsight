# First Connect Trace Analysis: C:\Users\ronny\AppData\Local\trace\SIN-X-TRACE\first-connect-to.txt

## Executive Summary

**CRITICAL FINDING:** This trace file also does **NOT** contain X.25 Call Request (0x0B) or Call Accepted (0x0F) packets.

The trace captures:
1. **PAD terminal connection** over pre-existing X.25 Virtual Circuit 1.19
2. **Application-level authentication** with device names and login credentials
3. **LAPB link re-establishment** (happens AFTER data exchange - unusual!)

**All X.25 packets are DATA packets (type 0x00)** - the virtual circuit was already established.

---

## Key Differences from trace-conn-100-102.txt

| Feature | trace-conn-100-102.txt | first-connect-to.txt |
|---------|------------------------|----------------------|
| **LAPB Address** | 0x09 | 0x07 (then switches to 0x09) |
| **First Packet** | SABM (link setup) | DATA with payload |
| **LAPB Establishment** | Lines 1-6 (start) | Lines 86+ (after data!) |
| **Payload Content** | Binary routing data | ASCII terminal data |
| **Connection Type** | Routing protocol | PAD terminal session |

---

## Detailed Packet Analysis

### **First Packet: PAD Terminal Identification (Lines 1-3)**

**Multi-buffer frame (48 bytes total):**

```
Buffer 1 [Line 1] (16 bytes):
0x07 0x00 0x21 0x13 0x00 0x0E 0x00 0x66 0x00 0x64 0x00 0x02 0x04 0x00 0xDA 0x12

Buffer 2 [Line 2] (14 bytes):
0x21 0x00 0x86 0xE4 0x00 0x66 0x00 0x00 0x00 0x64 0x02 0xFD 0x04 0x00

Buffer 3 [Line 3] (20 bytes):
0x00 0x41 0x00 0x10 0xFF 0x07 0x2A 0x54 0x41 0x44 0x41 0x44 0x4D 0x00 0xFE 0x04 0x44 0x31 0x30 0x32
                                    |  T     A     D     A     D     M        |     D     1     0     2
```

**HDLC/LAPB Layer (Buffer 1 header):**
```
0x07      = LAPB Address (non-standard - different from 0x09)
0x00      = LAPB Control: I-frame, N(S)=0, N(R)=0
```

**X.25 Layer:**
```
0x21 0x13 = X.25 Logical Channel Identifier:
            0x21 = GFI (Q=0, D=0, Modulo-8) + LCG=1
            0x13 = Logical Channel Number 19 (decimal)
            Combined: Virtual Circuit 1.19

0x00      = X.25 Packet Type: DATA (0x00)
            *** NOT CALL REQUEST (0x0B) ***
```

**Payload (Application Layer):**
```
ASCII Text Decoded:
- "TADADM" = Terminal Access Device Admin (terminal name)
- "D102"   = Destination Device 102 (target node)
```

**Interpretation:** This is a **PAD terminal connection request** over an already-established X.25 virtual circuit, not an X.25 Call Request.

---

### **Response Packet (Line 7):**

```
0x09 0x20 0x21 0x13 0x00 0x03 0x00 0x64 0x00 0x66 0x00 0x02 0x00 0x01 0xDE 0x1C

Breakdown:
0x09      = LAPB Address (switches back to 0x09!)
0x20      = LAPB Control: I-frame, N(S)=1, N(R)=0
0x21 0x13 = X.25 VC 1.19
0x00      = X.25 DATA packet
...       = Response data from Node 102
```

**LAPB Address Change:** Traffic switches from 0x07 to 0x09 after the first exchange.

---

### **Login Authentication Packet (Line 66):**

```
Multi-buffer containing:
0x44 0x31 0x30 0x30 = "D100" (Source device identifier)
0x53 0x59 0x53 0x54 0x45 0x4D = "SYSTEM" (Login username)

Full Buffer 66 (67 bytes):
0x00 0x00 0x00 0x3F 0x28 0x00 0x0A 0x21 0x44 0x31 0x30 0x30 0x27...
0x0B 0x02 0x00 0x01 0x10 0x11 0x53 0x59 0x53 0x54 0x45 0x4D 0x27...
      ^
      Note: 0x0B appears here but NOT as X.25 packet type!
            It's part of application payload data.
```

**Important:** The byte 0x0B appears in the payload, but **NOT** in the X.25 packet type position (which would be byte 4). This is application-level data, not an X.25 Call Request.

---

### **Late LAPB Link Establishment (Lines 86-125):**

**Unusual Pattern:** SABM/UA exchange happens **AFTER** extensive data exchange

```
Line 86:  [01:20:40.907] RECEIVED: 0x01 0x3F 0x00 0x66  - SABM from 102
Line 87:  [01:20:55.269] SENT:     0x01 0x3F 0x00 0x64  - SABM from 100
...
Line 90:  [01:20:55.730] SENT:     0x01 0x73 0x00 0x64  - UA from 100
...
Line 124: [01:20:58.971] RECEIVED: 0x01 0x73 0x00 0x66  - UA from 102
Line 125: [01:20:59.051] SENT:     0x07 0x01 0x00 0x64  - Connection ready
Line 126: [01:20:59.127] RECEIVED: 0x09 0x01 0x00 0x66  - Connection confirm
```

**Analysis:**
- Data exchange occurs for **~35 seconds** (01:20:04 → 01:20:40)
- Then LAPB link is **re-established** with SABM/UA
- Suggests the trace captures a **reconnection** or **recovery** scenario
- X.25 layer **remains active** during LAPB re-establishment

---

## Protocol Stack Analysis

### **Complete Protocol Layers:**

```
┌─────────────────────────────────────────────────────┐
│  Application Layer: PAD Terminal Protocol           │
│  - Device identification: "TADADM", "D102", "D100"  │
│  - Login credentials: "SYSTEM"                      │
│  - Terminal emulation commands                      │
├─────────────────────────────────────────────────────┤
│  Layer 3: X.25 Packet Layer                         │
│  - Virtual Circuit: 1.19 (ALREADY ESTABLISHED)     │
│  - Packet Type: DATA (0x00) only                    │
│  - NO Call Request (0x0B)                           │
│  - NO Call Accepted (0x0F)                          │
├─────────────────────────────────────────────────────┤
│  Layer 2: LAPB (Link Access Procedure Balanced)     │
│  - Address: 0x07 (initial) → 0x09 (later)          │
│  - Control: I-frames with N(S), N(R) sequencing    │
│  - Late re-establishment with SABM/UA               │
├─────────────────────────────────────────────────────┤
│  Layer 1: HDLC Physical                              │
│  - DMA transfers over serial line                   │
│  - Multi-buffer frame assembly (RSOM/REOM flags)    │
└─────────────────────────────────────────────────────┘
```

---

## X.25 Packet Type Search Results

### **Systematic Search for Call Request (0x0B):**

Checked **ALL** packets for X.25 packet type field (byte position 4 after LAPB headers):

| Line | X.25 Packet Type | Result |
|------|------------------|--------|
| 1    | 0x00 (DATA)      | ❌ Not Call Request |
| 7    | 0x00 (DATA)      | ❌ Not Call Request |
| 12   | 0x00 (DATA)      | ❌ Not Call Request |
| 14-85 | 0x00 (DATA)     | ❌ Not Call Request |

**Instances of 0x0B byte:** Found in line 66 at **byte position 43** (application payload), NOT at byte position 4 (X.25 packet type).

**Conclusion:** NO X.25 Call Request packets present in this trace.

---

## Why "first-connect" Has No Call Request?

### **Three Possible Explanations:**

#### 1. **Permanent Virtual Circuit (PVC)**
```
X.25 supports two VC types:
- SVC (Switched Virtual Circuit): Requires Call Request/Accepted
- PVC (Permanent Virtual Circuit): Pre-configured, always active

If VC 1.19 is a PVC, no call setup is needed.
```

#### 2. **Call Request Happened Earlier**
```
Timeline:
[Before trace] → X.25 Call Request/Accepted → [Trace starts] → PAD terminal data

The trace may begin AFTER the X.25 call was already established.
```

#### 3. **Proprietary Setup Mechanism**
```
SINTRAN may use a custom protocol for VC establishment that
doesn't follow standard X.25 Call Request procedures.
```

---

## PAD Protocol Analysis

### **PAD Terminal Connection Sequence:**

```
Step 1: [01:20:04.942] PAD Identification
        SENT: "TADADM" + "D102"
        (Terminal identifies itself to remote PAD)

Step 2: [01:20:07.248] PAD Response
        RECEIVED: Acknowledgment from Node 102

Step 3: [01:20:07.984] PAD Negotiation
        RECEIVED: Multi-buffer with PAD parameters

Step 4: [Multiple] Data Exchange
        Bidirectional PAD traffic for ~35 seconds

Step 5: [01:20:40.907] Link Recovery
        LAPB SABM/UA exchange (re-establish Layer 2)

Step 6: [01:20:59.127] Connection Confirmed
        Resume PAD session after link recovery
```

### **PAD Parameters Observed:**

From line 12 payload:
```
0x00 0x41 0x00 0x08 0x01 0x02 0x00 0x00 0x02 0x02 0x00 0x0A

Possible PAD parameters (ITU-T X.3):
- Parameter 1: 0x01 (Echo)
- Parameter 2: 0x02 (Forwarding)
- Line terminator: 0x0A (LF)
```

---

## Packet Statistics

### **Total Packets: 127**

**By LAPB Address:**
- 0x07 address: 8 packets (6.3%) - early phase
- 0x09 address: 74 packets (58.3%) - main phase
- 0x01 address (SABM/UA): 45 packets (35.4%) - link recovery

**By X.25 Packet Type:**
- 0x00 (DATA): 82 packets (100% of X.25 packets)
- 0x0B (Call Request): 0 packets ❌
- 0x0F (Call Accepted): 0 packets ❌

**By Frame Type:**
- Single-buffer: 89 packets (70.1%)
- Multi-buffer (2-3 buffers): 38 packets (29.9%)

**ASCII Payload Content:**
- Device IDs: "D100", "D102", "TADADM"
- Credentials: "SYSTEM"
- Various PAD control sequences

---

## Critical Timeline Events

```
[01:20:04.942] → First packet sent (PAD identification with 0x07 address)
[01:20:07.248] → First response received (switches to 0x09 address)
[01:20:38.405] → 33-second gap begins (processing delay?)
[01:20:40.907] → SABM received (link recovery initiated)
[01:20:55.269] → SABM sent (15-second delay!)
[01:20:58.971] → UA received (link restored)
[01:20:59.127] → Connection confirmed (PAD session resumes)
```

**Major Gap:** 33-second pause (line 62→63) suggests:
- Processing delay
- User interaction
- Application-level timeout
- Buffer/queue management

---

## Comparison with X.25 Standard Call Setup

### **Standard X.25 SVC Establishment (NOT in this trace):**

```
Step 1: DTE → DCE: CALL REQUEST (0x0B)
        ┌─────────────────────────────────────┐
        │ GFI + LCN                            │
        │ Packet Type: 0x0B                    │
        │ Calling Address Length               │
        │ Called Address Length                │
        │ Calling DTE Address                  │
        │ Called DTE Address                   │
        │ Facility Length                      │
        │ Facilities (window size, packet size)│
        │ Call User Data (up to 16 bytes)     │
        └─────────────────────────────────────┘

Step 2: DCE → DTE: CALL ACCEPTED (0x0F)
        ┌─────────────────────────────────────┐
        │ GFI + LCN                            │
        │ Packet Type: 0x0F                    │
        │ Responding Address Lengths           │
        │ Responding DTE Address               │
        │ Facility Length                      │
        │ Negotiated Facilities                │
        │ Accept User Data (up to 16 bytes)   │
        └─────────────────────────────────────┘

Step 3: Both: DATA TRANSFER (0x00)
        Normal data exchange on established VC
```

### **What This Trace Shows (Actual):**

```
Pre-trace: [X.25 VC 1.19 already active - setup NOT captured]

Step 1: PAD terminal identification over existing VC
Step 2: PAD parameter negotiation
Step 3: User authentication
Step 4: Terminal session data
```

---

## Conclusion

### **Definitive Findings:**

1. ✅ **PAD terminal connection** captured successfully
2. ✅ **Application-level protocol** fully documented
3. ❌ **X.25 Call Request (0x0B)** NOT present
4. ❌ **X.25 Call Accepted (0x0F)** NOT present
5. ✅ **X.25 Virtual Circuit 1.19** pre-existing
6. ✅ **LAPB re-establishment** captured (unusual timing)

### **The X.25 Virtual Circuit Was Already Established**

**Evidence:**
- First packet is DATA (0x00), not Call Request (0x0B)
- Logical Channel 1.19 already allocated
- No facility negotiation observed
- Both nodes immediately use VC 1.19
- PAD protocol operates over existing connection

### **To Capture an X.25 Call Request Packet:**

You would need a trace that captures:
1. **Initial system boot** - before any connections exist
2. **First-ever connection** to a new remote node
3. **SVC (Switched Virtual Circuit) establishment** - not PVC
4. **Network-level connection** - not just PAD/terminal session

### **Alternative Hypothesis:**

The SINTRAN system may be using **Permanent Virtual Circuits (PVCs)** configured at system initialization, which never require Call Request/Accepted packets during normal operation.

---

## Full Path to Document

**Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\First_Connect_Analysis.md**

## Related Documents

- **Complete_Packet_Type_Analysis.md** - Analysis of trace-conn-100-102.txt
- **PAD_Connection_Analysis.md** - PAD protocol documentation
- **LAPB_vs_X25_Protocol_Handling.md** - Protocol layer differences
