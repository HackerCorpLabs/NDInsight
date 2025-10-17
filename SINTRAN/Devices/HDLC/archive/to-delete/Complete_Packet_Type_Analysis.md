# Complete Packet Type Analysis: trace-conn-100-102.txt

## Executive Summary

**CRITICAL FINDING:** The trace file `trace-conn-100-102.txt` does **NOT** contain X.25 Call Request (0x0B) or Call Accepted (0x0F) packets. The X.25 virtual circuit was **already established** before this trace began.

All captured packets are:
1. LAPB link re-establishment (SABM/UA)
2. X.25 DATA packets on pre-existing Virtual Circuit 1.19
3. SINTRAN proprietary routing protocol frames

---

## Packet Type Categories

### 1. LAPB Control Frames (Layer 2)

#### SABM (Set Asynchronous Balanced Mode)
```
Format: 0x01 0x3F 0x00 [NODE]
Example: 0x01 0x3F 0x00 0x64

Occurrences:
- Line 1:  [23:25:57.844] Node 100 → 102 (initial)
- Line 57-87: Multiple retransmissions after CRC error
```

#### UA (Unnumbered Acknowledgment)
```
Format: 0x01 0x73 0x00 [NODE]
Example: 0x01 0x73 0x00 0x64

Occurrences:
- Line 3-4:  [23:25:58.623] Initial acknowledgment
- Line 89-123: Multiple exchanges after link recovery
```

#### Connection Ready/Confirm
```
Format: 0x09 0x01 0x00 [NODE]
Example: 0x09 0x01 0x00 0x64

Occurrences:
- Line 5:  [23:25:58.952] Node 100 ready
- Line 6:  [23:25:59.196] Node 102 confirm
- Line 104: [23:26:55.346] Re-establishment
- Line 124: [23:27:03.940] Final confirm
```

---

### 2. X.25 Data Packets (Layer 3) - Virtual Circuit 1.19

**ALL X.25 packets in this trace are DATA packets (type 0x00) on an already-established Virtual Circuit 1.19**

#### Format Structure:
```
Byte 0:    0x09           - LAPB address (non-standard)
Byte 1:    [CONTROL]      - LAPB I-frame control (N(S), N(R))
Byte 2-3:  0x21 0x13      - X.25 GFI + Logical Channel
                            0x21 = GFI (Q=0, D=0, Modulo-8)
                            0x13 = Channel 19 (0x13 = 19 decimal)
                            Combined: Logical Channel Identifier 1.19
Byte 4:    0x00           - X.25 Packet Type: DATA
                            (P(S), M, P(R) sequence numbers)
Byte 5+:   [PAYLOAD]      - User data
```

#### Example X.25 DATA Packet:
```
Line 7: [23:26:22.787] 16 bytes
0x09 0x00 0x21 0x13 0x00 0x19 0x00 0x66 0x00 0x64 0xFF 0xFF 0x00 0x01 0xDE 0x08

Breakdown:
0x09      = LAPB address
0x00      = LAPB Control: I-frame N(S)=0, N(R)=0
0x21      = X.25 GFI: 0010 0001
            - Q bit = 0 (user data)
            - D bit = 0 (no delivery confirmation)
            - Mod = 10 (modulo-8 sequencing)
            - LCG = 1 (Logical Channel Group 1)
0x13      = LCN = 19 (Logical Channel Number)
0x00      = X.25 DATA Packet: P(S)=0, M=0, P(R)=0
0x19      = Length field: 25 bytes
0x00 0x66 = Destination node: 102
0x00 0x64 = Source node: 100
0xFF 0xFF = Broadcast/routing flag
0x00 0x01 0xDE 0x08 = Routing protocol payload
```

#### X.25 DATA Packet Occurrences:

**Single-buffer frames (16 bytes):**
- Line 7, 8:   N(S)=0, N(R)=0 (retransmission)
- Line 9, 14, 18: N(S)=1, N(R)=0 (responses from 102)
- Line 21, 27, 37, 44: Control byte 0x42 variants
- Line 26, 33, 39, 45, 50: Control byte 0x64 variants

**Multi-buffer frames (40-52 bytes total):**
```
Lines 10, 12, 13: First multi-buffer transmission (40 bytes)
- Buffer 1 (16 bytes): LAPB + X.25 header + data
- Buffer 2 (14 bytes): Continuation data
- Buffer 3 (8 bytes):  Final data + node trailer

Transmitted: [23:26:24.581] → [23:26:25.039]
Received:    [23:26:28.624] (4 seconds later)
```

**Retransmissions:** Same multi-buffer frame sent 3+ times due to ACK timeouts

---

### 3. Status/Control Frames (4 bytes)

#### Status Poll
```
Format: 0x09 0x29 0x00 [NODE]
Example: 0x09 0x29 0x00 0x64

Occurrences:
- Line 11: [23:26:24.634] From Node 102
- Line 19: [23:26:26.316] From Node 100
```

#### Response/Acknowledgment Frames
```
0x09 0x41 0x00 [NODE] - Response frame
0x09 0x42 0x21 0x13... - Data with ACK required (16 bytes)
0x09 0x49 0x00 [NODE] - Final status
0x09 0x61 0x00 [NODE] - Recovery status
0x09 0x64 0x21 0x13... - Data continuation
0x09 0x66 0x21 0x13... - Sequence advancement
0x09 0x69 0x00 [NODE] - Status response
0x09 0x86 0x21 0x13... - Advanced sequence
0x09 0x88 0x21 0x13... - Higher sequence (52 bytes)
0x09 0x89 0x00 [NODE] - Completion status
```

---

## Missing X.25 Packet Types

### NOT PRESENT in this trace:

#### Call Setup/Clearing Packets:
```
0x0B (binary 00001011) - CALL REQUEST
0x0F (binary 00001111) - CALL ACCEPTED
0x13 (binary 00010011) - CLEAR REQUEST
0x17 (binary 00010111) - CLEAR CONFIRMATION
```

#### Flow Control Packets:
```
0x01 - RR (Receive Ready)
0x05 - RNR (Receive Not Ready)
0x09 - REJ (Reject)
```

#### Interrupt Packets:
```
0x23 - INTERRUPT
0x27 - INTERRUPT CONFIRMATION
```

#### Reset/Restart Packets:
```
0x1B - RESET REQUEST
0x1F - RESET CONFIRMATION
0xFB - RESTART REQUEST
0xFF - RESTART CONFIRMATION
```

---

## Why No Call Request Packet?

### Explanation:

The **X.25 Virtual Circuit 1.19 was already established** before this trace capture began. Evidence:

1. **First X.25 packet is DATA (0x00)** not CALL REQUEST (0x0B)
2. **Logical Channel 1.19 already allocated** - not negotiated
3. **Both nodes immediately use VC 1.19** - no setup phase
4. **No facility negotiation** - window sizes, packet sizes already agreed
5. **LAPB link re-establishment only** - X.25 layer remains active

### Typical X.25 Call Establishment Sequence (NOT in this trace):
```
Step 1: DTE → DCE: CALL REQUEST (0x0B)
        - Called address
        - Calling address
        - Facilities (window size, packet size)
        - User data (up to 16 bytes)

Step 2: DCE → DTE: CALL ACCEPTED (0x0F)
        - Confirmed facilities
        - Response user data

Step 3: Both sides: DATA packets (0x00)
        - Normal data transfer on established VC
```

### What This Trace Shows (Actual):
```
Pre-trace: [X.25 VC 1.19 already exists]

Step 1: LAPB SABM/UA exchange
        - Re-establish Layer 2 link

Step 2: Immediate DATA packets (0x00)
        - Resume communication on VC 1.19
        - No call setup needed
```

---

## Protocol Stack Summary

### Complete Protocol Hierarchy:

```
┌─────────────────────────────────────────┐
│  Application Layer                       │
│  (Routing protocol, user data)          │
├─────────────────────────────────────────┤
│  Layer 3: X.25 Packet Layer             │
│  - Virtual Circuit: 1.19                │
│  - Packet Types: DATA (0x00) only      │
│  - NO Call Request/Accepted packets     │
├─────────────────────────────────────────┤
│  Layer 2: LAPB (Link Access)            │
│  - Control: SABM, UA, I-frames          │
│  - Sequence numbering: N(S), N(R)       │
├─────────────────────────────────────────┤
│  Layer 1: HDLC Physical                  │
│  - Flags: 0x7E (not shown in trace)    │
│  - FCS: CRC-16 (computed, not shown)    │
│  - DMA transfer over serial line        │
└─────────────────────────────────────────┘
```

---

## Packet Statistics

### Total Packets Analyzed: 125

**By Category:**
- LAPB Control (SABM/UA/Ready): 42 packets (33.6%)
- X.25 DATA on VC 1.19: 68 packets (54.4%)
- Status/Control: 14 packets (11.2%)
- CRC Error: 1 packet (0.8%)

**By Direction:**
- Sent (100 → 102): 67 packets (53.6%)
- Received (102 → 100): 58 packets (46.4%)

**By Size:**
- 4 bytes (control frames): 57 packets (45.6%)
- 8 bytes (continuation): 11 packets (8.8%)
- 14 bytes (data segments): 11 packets (8.8%)
- 16 bytes (standard data): 42 packets (33.6%)
- 52 bytes (large multi-buffer): 4 packets (3.2%)

**Retransmissions:**
- Same frame sent 2x: 15 instances
- Same frame sent 3x: 4 instances
- Same frame sent 4-5x: 2 instances

---

## Conclusion

**The trace file `trace-conn-100-102.txt` contains NO X.25 Call Request packets.**

The communication captured shows:
1. LAPB link re-establishment after a CRC error
2. Continued data exchange on pre-existing X.25 Virtual Circuit 1.19
3. SINTRAN proprietary routing protocol over X.25

**To capture an X.25 Call Request packet, you would need a trace that includes:**
- Initial connection establishment
- First-time virtual circuit setup
- Call negotiation with facilities

**This trace begins AFTER the X.25 call was already established.**

---

## Full Path to Document

**Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\Complete_Packet_Type_Analysis.md**
