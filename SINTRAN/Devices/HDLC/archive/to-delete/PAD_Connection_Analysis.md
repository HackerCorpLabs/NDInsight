# PAD Connection Analysis: TAD to PAD Communication

## Overview

Analysis of SINTRAN PAD (Packet Assembly/Disassembly) connection establishment between TAD (Terminal Access Device) and remote PAD, showing HDLC frame sequences for X.25 packet switching protocol.

## XMSG API Functions in PAD Communication

**New XMSG Functions Observed:**
- **XFRCV (000015)**: Frame receive/listen for incoming data
- **XFGST (000017)**: Frame get status/state information  
- **XFOPN (000012)**: Frame open connection
- **XFWRI (000007)**: Frame write data to buffer
- **XFSND (000014)**: Frame send data to destination
- **XFREA (000006)**: Frame read data from buffer
- **XFREL (000003)**: Frame release buffer (previously seen)
- **XFDCT (000001)**: Frame disconnect (previously seen)
- **XFMST (000011)**: Frame master status/control

**PAD Communication Pattern:**
```
XFOPN → XFGET → XFWRI → XFSND → XFRCV (loop for responses)
```

## HDLC Frame Analysis

### Frame Type Classification

#### 1. PAD Connection Request Frames (Routing Protocol)
```
Frame: [09 44 21 13 00 0E 00 66 00 64 00 04 04 00 DA 10]
       09    = Frame header/type
       44    = Control byte (0x44 = PAD connection request)
       21 13 = Protocol identifier (same as routing frames)
       00 0E = Length field (14 bytes payload)
       00 66 = Destination node 102 (0x66) 
       00 64 = Source node 100 (0x64)
       00 04 = Sequence number 4
       04 00 DA 10 = PAD connection parameters/data
```

#### 2. X.25 Control Frames (Short Format)
```
Frame: [01 3F 00 64]
       01 = X.25 frame type indicator
       3F = Control field (0x3F = SABM - Set Asynchronous Balanced Mode)
       00 64 = Node address 100

Frame: [01 73 00 64] / [01 73 00 66]
       01 = X.25 frame type indicator  
       73 = Control field (0x73 = UA - Unnumbered Acknowledgment)
       00 64/66 = Source node (100/102)

Frame: [09 01 00 64]
       09 = Frame header (same as routing)
       01 = Control field (connection established)
       00 64 = Node address 100
```

### Frame Sequence Analysis

#### Phase 1: PAD Connection Advertisement (Repeated)
**Frames sent 5 times** (14:39:06 - 14:39:11):
```
TX: [09 44 21 13 00 0E 00 66 00 64 00 04 04 00 DA 10]
```
- **Control 0x44**: PAD connection request
- **Retransmitted** due to no response
- **Node 100→102**: TAD requesting PAD services from node 102

#### Phase 2: X.25 Link Establishment
**SABM (Set Asynchronous Balanced Mode):**
```
TX: [01 3F 00 64]  - Node 100 sends SABM
RX: [01 3F 00 66]  - Node 102 responds with SABM
```

**UA (Unnumbered Acknowledgment):**
```
TX: [01 73 00 64]  - Node 100 sends UA (twice)
RX: [01 73 00 66]  - Node 102 responds with UA
```

#### Phase 3: Connection Established
```
TX: [09 01 00 64]  - Node 100 signals connection ready
RX: [09 01 00 66]  - Node 102 confirms connection
```

### Control Byte Analysis

**PAD-Specific Control Bytes:**
- **0x44**: PAD connection request (new - not seen in routing)
- **0x3F**: X.25 SABM (Set Asynchronous Balanced Mode)
- **0x73**: X.25 UA (Unnumbered Acknowledgment)  
- **0x01**: Connection established/ready state

**Frame Type Indicators:**
- **0x09**: Routing/control protocol frames (same as before)
- **0x01**: X.25 protocol frames (new PAD-specific format)

### Buffer Management During PAD Connection

**DMA Operations:**
- **CommandTransmitterStart**: Multiple retransmissions of PAD request
- **CommandInitialize**: Receiver initialization before X.25 handshake
- **CommandReceiverStart**: Activate receiver for responses
- **Buffer recycling**: Same addresses reused (0x06D498, 0x017B80-90)

## Unit Test Frame Examples

### Test Case 1: PAD Connection Request
```csharp
[TestMethod]
public void TestPADConnectionRequest()
{
    var frame = new byte[] { 0x09, 0x44, 0x21, 0x13, 0x00, 0x0E, 0x00, 0x66, 0x00, 0x64, 0x00, 0x04, 0x04, 0x00, 0xDA, 0x10 };
    // Control byte 0x44 = PAD connection request
    // Protocol 0x2113 = routing protocol
    // Length 0x000E = 14 bytes payload
    // Route: Node 100 → Node 102
    // Sequence: 4
}
```

### Test Case 2: X.25 SABM Frame
```csharp
[TestMethod] 
public void TestX25SABMFrame()
{
    var frame = new byte[] { 0x01, 0x3F, 0x00, 0x64 };
    // Frame type 0x01 = X.25 protocol
    // Control 0x3F = SABM (Set Asynchronous Balanced Mode)
    // Node: 100
}
```

### Test Case 3: X.25 UA Frame
```csharp
[TestMethod]
public void TestX25UAFrame()
{
    var frame = new byte[] { 0x01, 0x73, 0x00, 0x64 };
    // Frame type 0x01 = X.25 protocol  
    // Control 0x73 = UA (Unnumbered Acknowledgment)
    // Node: 100
}
```

### Test Case 4: PAD Connection Established
```csharp
[TestMethod]
public void TestPADConnectionEstablished()
{
    var frame = new byte[] { 0x09, 0x01, 0x00, 0x64 };
    // Frame type 0x09 = routing protocol
    // Control 0x01 = connection established
    // Node: 100
}
```

## Protocol Differences: Routing vs PAD

### Routing Frames (Previous Analysis):
- Control bytes: 0x00, 0x20, 0x21, 0x22, 0x29, 0x41, 0x42, 0x49
- Purpose: Network topology exchange
- Frame format: Always 0x09 header with 0x2113 protocol

### PAD Frames (New Analysis):
- Control bytes: 0x44 (PAD request), 0x3F (SABM), 0x73 (UA), 0x01 (established)
- Purpose: X.25 packet switching connection establishment  
- Frame formats: 0x09 (routing-style) + 0x01 (X.25-style)

## Key Findings

### 1. Clear PAD Connection Sequence
- **PAD request repeated 5 times** until X.25 handshake begins
- **Standard X.25 SABM/UA exchange** for link establishment
- **Connection confirmation** using routing-style frames

### 2. Dual Protocol Usage
- **Routing protocol** (0x09 header) for PAD service requests
- **X.25 protocol** (0x01 header) for actual link establishment
- **Mixed format** indicates PAD operates above routing layer

### 3. New Control Bytes Identified
- **0x44**: PAD-specific connection request
- **0x3F**: X.25 SABM command
- **0x73**: X.25 UA response

### 4. Retransmission Behavior
- **PAD requests repeated** when no response received
- **X.25 frames acknowledged** properly in handshake
- **Connection state confirmed** by both nodes

## Conclusion

The PAD connection trace reveals a sophisticated two-layer protocol:
1. **Upper layer**: PAD service requests using routing protocol format
2. **Lower layer**: X.25 standard for actual data link establishment

This confirms SINTRAN's implementation of X.25 packet switching with PAD services for terminal connections, showing clear integration between routing and X.25 protocols.

## Reverse PAD Connection Analysis: Node 102 → Node 100

### Additional HDLC Frame Sequences (From trace-connect-pad-2.txt)

#### Frame Type Classification - Reverse Direction

#### 1. Initial Routing Exchange (102 ← 100)
```
RX: [09 00 21 13 00 19 00 64 00 66 FF FF 00 01 DE 08]
    09    = Frame header/type
    00    = Control byte (command frame - from 100)
    21 13 = Protocol identifier  
    00 19 = Length field (25 bytes payload)
    00 64 = Destination node 100
    00 66 = Source node 102  
    FF FF = Broadcast/special addressing
    00 01 DE 08 = Routing data
```

#### 2. Response Frame (102 → 100)  
```
TX: [09 20 21 13 00 13 00 66 00 64 FF FF 00 01 DE 0E]
    09    = Frame header/type
    20    = Control byte (response frame - from 102)
    21 13 = Protocol identifier
    00 13 = Length field (19 bytes payload) 
    00 66 = Destination node 102 (source)
    00 64 = Source node 100 (dest)
    FF FF = Broadcast/special addressing
    00 01 DE 0E = Response data
```

#### 3. Status Poll Frame (102 → 100)
```
TX: [09 29 00 64]
    09 = Frame header
    29 = Control byte (status poll - same as previous analysis)
    00 64 = Target node 100
```

#### 4. PAD Connection Attempt (102 ← 100)
```
RX: [09 00 21 13 00 0E 00 64 00 66 00 00 04 00 DA 14]
TX: [09 22 21 13 00 0E 00 64 00 66 00 00 04 00 DA 14]
    00/22 = Command/Acknowledgment control bytes
    00 0E = Length 14 bytes (same as original PAD request)
    04 00 DA 14 = PAD connection parameters (different from DA 10)
```

#### 5. Advanced Routing/Data Frames
```
TX: [09 42 21 13 00 07 00 66 00 64 00 00 FF D8 DE 42]
    42 = Control byte (data frame with ack required - new pattern)
    00 07 = Length 7 bytes
    FF D8 DE 42 = Data payload
```

#### 6. Final Status Frame
```  
TX: [09 49 00 64]
    49 = Control byte (final response/status - matches previous analysis)
    00 64 = Node 100
```

### Frame Sequence Analysis - Reverse Direction

#### Phase 1: Routing Protocol Exchange
1. **Node 100 → 102**: Command frame (0x00) with broadcast addressing
2. **Node 102 → 100**: Response frame (0x20) acknowledging
3. **Node 102**: Status poll (0x29) to node 100

#### Phase 2: PAD Connection Negotiation  
1. **Node 100 → 102**: PAD request (control 0x00) with params DA 14
2. **Node 102 → 100**: PAD acknowledgment (control 0x22)
3. **Multiple retransmissions** of data frames (control 0x42)
4. **Final status** (control 0x49)

#### Phase 3: X.25 SABM Attempts (No Response)
- **Multiple SABM frames** sent: `[01 3F 00 64]`
- **No UA response received** - connection fails
- **Retransmitted 5+ times** with no success

### Key Differences: Forward vs Reverse Connection

#### Forward Direction (100 → 102) - SUCCESSFUL:
- PAD request control **0x44** (successful)
- X.25 SABM/UA exchange **completed**
- Connection **established**

#### Reverse Direction (102 → 100) - FAILED:
- PAD request control **0x00/0x22** (different approach)
- Advanced data frames **0x42** (data with ack required)
- X.25 SABM **repeated without response**
- Connection **failed**

### New Control Bytes Identified (Reverse Direction):
- **0x00**: Command frame (routing level)
- **0x20**: Response frame (routing level)  
- **0x22**: Data acknowledgment (PAD level)
- **0x42**: Data frame with acknowledgment required
- **0x49**: Final response/status

### New Unit Test Cases - Reverse Direction

#### Test Case 5: Routing Command Frame
```csharp
[TestMethod]
public void TestRoutingCommandFrame()
{
    var frame = new byte[] { 0x09, 0x00, 0x21, 0x13, 0x00, 0x19, 0x00, 0x64, 0x00, 0x66, 0xFF, 0xFF, 0x00, 0x01, 0xDE, 0x08 };
    // Control 0x00 = Command frame at routing level
    // Broadcast addressing FF FF
    // Route: 100 → 102
}
```

#### Test Case 6: Routing Response Frame  
```csharp
[TestMethod]
public void TestRoutingResponseFrame()
{
    var frame = new byte[] { 0x09, 0x20, 0x21, 0x13, 0x00, 0x13, 0x00, 0x66, 0x00, 0x64, 0xFF, 0xFF, 0x00, 0x01, 0xDE, 0x0E };
    // Control 0x20 = Response frame
    // Route: 102 → 100
}
```

#### Test Case 7: Data Frame with ACK Required
```csharp
[TestMethod] 
public void TestDataFrameWithAck()
{
    var frame = new byte[] { 0x09, 0x42, 0x21, 0x13, 0x00, 0x07, 0x00, 0x66, 0x00, 0x64, 0x00, 0x00, 0xFF, 0xD8, 0xDE, 0x42 };
    // Control 0x42 = Data frame with acknowledgment required
    // Advanced routing/data payload
}
```

#### Test Case 8: PAD Acknowledgment Frame
```csharp
[TestMethod]
public void TestPADACKFrame()
{
    var frame = new byte[] { 0x09, 0x22, 0x21, 0x13, 0x00, 0x0E, 0x00, 0x64, 0x00, 0x66, 0x00, 0x00, 0x04, 0x00, 0xDA, 0x14 };
    // Control 0x22 = Data/PAD acknowledgment
    // PAD parameters DA 14 (different from DA 10)
}
```

### Protocol Analysis Summary

#### Successful PAD Connection (100 → 102):
- Uses **dedicated PAD control byte 0x44**
- **Direct X.25 handshake** after PAD request
- **Clean connection establishment**

#### Failed PAD Connection (102 → 100):  
- Uses **generic routing controls 0x00/0x20/0x22**
- **Extended negotiation** with data frames 0x42
- **X.25 SABM timeout** - no response from node 100
- **Connection failure** - node 100 may not support incoming PAD requests

This suggests **asymmetric PAD capability**: Node 102 can provide PAD services to node 100, but not vice versa.