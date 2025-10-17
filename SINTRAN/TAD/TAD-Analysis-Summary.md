# TAD (Terminal Access Device) Analysis Summary

## Overview

This document provides a summary of the comprehensive TAD protocol analysis for SINTRAN III X.25 communication.

**Analysis Date:** 2025-10-13

**Analyst:** AI-assisted analysis of SINTRAN NPL source code

---

## Documentation Files Created

### 1. TAD-Protocol-Analysis.md
**Purpose:** Complete API and protocol reference

**Contents:**
- **Message Type Enumeration:** 30+ message types with codes and purposes
- **Message Format Structure:** Header format, alignment rules, parsing
- **TAD Driver Entry Points:** 17 main entry points with line numbers
- **RPIT Entry Points:** 31 routine entry points for message processing
- **Data Structure Fields:** Complete input/output datafield specifications
- **Critical Flag Bits:** DFLAG, FLAGB, TINFO bit definitions
- **Connection Lifecycle:** Initialize → Send → Receive → Disconnect
- **Example Message Sequences:** Connection, data transfer, break handling, disconnection, error recovery

**Key Findings:**
- TAD uses XMSG for buffer management and port communication
- Messages have even/odd byte alignment with automatic padding
- Two priority levels: normal (queued) and high (immediate)
- Buffer pool managed via POOLLI chain with MGETPOOL/MPUTPOOL routines

**File Size:** ~50 KB, comprehensive reference

---

### 2. TAD-Message-Formats.md
**Purpose:** Detailed message format specifications

**Contents:**
- **General Message Format:** Pad byte, message type, byte count, data field
- **Message Header Creation:** WRMHEAD routine (line 162-178)
- **Message Parsing:** GETMES routine (line 224-242)
- **Data Messages:** 7BDAT format with break handling
- **Control Messages:** 7TMOD, 7TTYP, 7DESC, 78MOD, 7OPSV
- **Break and Echo Messages:** 7BMMX (3 or 23 bytes), 7ECKM (1 or 21 bytes)
- **Request and Response Messages:** 7RFI, 7REJE, 7ISRQ, 7ISRS, 7ERRS
- **System Control Messages:** 7SYCN, 7USCN, 7CPCO
- **Connection Control Messages:** 7RESE, 7RECO, 7DCON
- **Escape Messages:** 7ESCA, 7CERS, 7RLOC
- **Nowait Mode Messages:** 7NOWT, 7TNOW, 7NWRE
- **Special Messages:** 7DUMM, 7TREP, 7UMOD
- **Message Priority Levels:** Normal vs. high-priority processing
- **Buffer Management Integration:** XMSG operations for buffers

**Format Details:**
- Each message documented with:
  - Purpose and code
  - Binary format diagram
  - Data field structure
  - Source code references
  - Processing logic (send/receive)
  - Example messages

**File Size:** ~75 KB, complete format reference

---

### 3. TAD-Protocol-Flows.md
**Purpose:** Visual protocol flow diagrams

**Contents (17 Mermaid Diagrams):**

#### Connection Flows:
1. **Full Connection Sequence:** INIBDR → INISND → BDRINP with XMSG calls
2. **Connection State Machine:** 8 states (Disconnected → Opening → Connected)

#### Data Transfer Flows:
3. **Output Data Flow:** User → BDPUT → Buffer → XMSG → Remote
4. **Input Data Flow:** Remote → XMSG → BDRINP → BDGET → User
5. **8-Byte Transfer:** BB8OUT/BB8INP monitor call flows

#### Buffer Management:
6. **Buffer Pool Management:** Initialize → Allocate → Get → Use → Return
7. **Buffer State Transitions:** 5 states (Free → Active → Sending → Processed)

#### Escape and Break:
8. **Escape Sequence:** Detect → Process → Response
9. **Break Message Flow:** Set strategy → Detect → Notify
10. **Echo Strategy Flow:** Configure → Apply → Echo

#### Error Handling:
11. **Message Rejection Flow:** Parse → Validate → Reject → RFI
12. **Ready-For-Input (RFI) Flow:** Empty buffer → Request → Receive
13. **Fatal Error Recovery:** Detect → Cleanup → Logout → Disconnect

#### Disconnection:
14. **Graceful Disconnection:** BDDSCN → Release → XFDCT
15. **Remote-Initiated Disconnection:** 7DCON → DSTOTA → Cleanup
16. **Forced Disconnection:** STOTAD → Logout → TDDSCN

#### Advanced Features:
17. **Nowait Mode:** State machine and status reporting
18. **ISIZE Query:** Request → Process → Response
19. **Complete TAD Session:** 5-phase terminal session (Connect → Login → Command → Escape → Logout)
20. **Priority Message Handling:** High vs. normal priority flowchart

**File Size:** ~45 KB, 17 comprehensive diagrams

---

## Key Technical Insights

### 1. X.25 TAD Message Structure

**Discovery:** TAD messages are variable-length with automatic byte alignment

```
┌──────────┬────────────┬────────────┬─────────────┐
│ Pad (0-1)│ Type (1)   │ Count (1)  │ Data (0-255)│
└──────────┴────────────┴────────────┴─────────────┘
```

- **Pad byte inserted if starting on odd boundary**
- **Message type = 7-bit ASCII string** (e.g., "BDAT")
- **Byte count = data field size only** (not including header)
- **Maximum data size = 255 bytes per message**

### 2. XMSG Integration Architecture

**Discovery:** TAD is built on top of XMSG message-passing layer

**XMSG Operations Used:**
- `XFOPN` - Open port
- `XFALM` - Allocate message space
- `XFGET` - Get buffer
- `XFSCM` - Set current buffer
- `XFSND` - Send buffer
- `XFRCV` - Receive buffer
- `XFREL` - Release buffer
- `XFDCT` - Disconnect port
- `XFWHD` - Write header
- `XFRHD` - Read header
- `XFPST` - Port status
- `XFWDF` - Wake driver function

**Buffer Flow:**
1. Allocate pool during `INIBDR`
2. Get buffer from pool (`MGETPOOL`)
3. Fill with messages (`CREMES`, `BYTPUT`)
4. Send via XMSG (`XFSND`)
5. Return to pool (`MPUTPOOL`)

### 3. Two-Priority Message System

**Discovery:** TAD processes messages at two priority levels

**High Priority (Immediate):**
- Escape signals (7ESCA, 7RLOC)
- Connection control (7DCON)
- Response messages (7CERS, 7RECO, 7ISRS, 7ERRS, 7TREP)
- Nowait control (7NWRE)

**Normal Priority (Queued):**
- Data messages (7BDAT)
- Configuration (7TMOD, 7TTYP, 7DESC, 78MOD, 7OPSV)
- Dummy messages (7DUMM)

**Processing Difference:**
- High: Received immediately into `TMPBUF`, processed before returning
- Normal: Received only when `BUFFID=0`, queued in input buffer

**Source:** `MP-P2-TAD.NPL:445-496`

### 4. Connection Establishment Protocol

**Discovery:** 3-phase connection with automatic configuration

**Phase 1: Initialize (INIBDR)**
- Open XMSG port
- Allocate buffer pool
- Reserve N buffers (typically 4-8)

**Phase 2: Send Initial (INISND)**
- Get buffer from pool
- Write dummy message header
- Send to partner
- Configure wake-up for BDRINP

**Phase 3: Configuration Exchange**
- Remote sends: 7TMOD, 7TTYP, 7DESC, 7OPSV
- Local applies settings
- Connection ready for data

**Critical:** Remote must send configuration messages before data

### 5. Break and Echo Strategy Tables

**Discovery:** Custom character tables for break/echo (20 bytes = 160 bits)

**Table Format:**
- 8 words (16 bits each)
- Each bit represents one character (0x00-0x7F)
- Bit set = character is break/echo character

**Example:** Break on CR (0x0D)
```
Word 0: bits 0-15   (chars 0x00-0x0F) - bit 13 set
Word 1: bits 0-15   (chars 0x10-0x1F)
...
Word 7: bits 0-15   (chars 0x70-0x7F)
```

**Storage:** `PBRK7` array in input datafield, `PECH7` in output datafield

**Source:** `RP-P2-TAD.NPL:717-727` (CBRECTA)

### 6. Nowait Mode Operation

**Discovery:** Asynchronous I/O mode with status reporting

**State Variable:** `ISTATE`
- `0` = Normal synchronous mode
- `<0` = Nowait mode active
- `2` = Waiting to redo monitor call

**Status Messages:**
- `7NOWT` [00] = Success
- `7TNOW` [error] = Failure
- `7NWRE` = Restart request

**Bit 17 in status = break character indicator**

**Source:** `RP-P2-TAD.NPL:1088-1102` (NOWTSTA)

### 7. RFI (Ready For Input) Mechanism

**Discovery:** Automatic flow control for input data

**Triggers:**
- User reads from empty buffer
- Reject data message
- Nowait input request

**Processing:**
1. Check output buffer available
2. If yes: Create 7RFI message, send
3. If no: Move input buffer to output, send 7RFI
4. If no buffers: Set `DFLAG 5WRQI` (driver sends later)
5. Mark RFI sent: `DFLAG 5RQI`

**Response:** Remote sends 7BDAT with data

**Source:** `RP-P2-TAD.NPL:1115-1147` (SNDRFI)

### 8. Escape Processing

**Discovery:** Two-stage escape handling with enable/disable

**Escape Types:**
- `7ESCA` = Escape character received
- `7RLOC` = Remote/local toggle (NORD-NET)

**Processing:**
1. Check `DFLAG 5IESC` (escape enabled?)
2. If enabled:
   - Extract escape char from `CESCP`
   - Call escape handler
   - Send `7CERS` response
3. If disabled:
   - Send `7CERS` (disabled response)

**Delayed Escape:**
- `FLAGB 5ESCLOFF` set during delay
- Input blocked until escape sent
- Cleared after response

**Source:** `MP-P2-TAD.NPL:602-625`, `RP-P2-TAD.NPL:514-528`

### 9. Error Recovery Strategies

**Discovery:** Multi-level error handling

**Message Errors:**
- Inconsistent size (A=3) → Send `7REJE`
- Unknown type → Send `7REJE`
- Rejected data message → Also send `7RFI`

**Buffer Errors:**
- Pool empty → Wait for buffer return or set `5WRQI`
- Buffer full → Send current, get new

**Connection Errors:**
- XMSG error → Convert with `CNVERR`, disconnect
- Fatal error → `FAERR` → `DSTOTA` → logout/disconnect

**Source:** `RP-P2-TAD.NPL:1161-1201` (SNDREJ), `MP-P2-TAD.NPL:843-845` (FAERR)

### 10. OPSYS Version and Protocol Negotiation

**Discovery:** Version exchange during connection

**Format:** `7OPSV` [3 bytes]
- Byte 0: SINTRAN OS version
- Bytes 1-2: TAD protocol number

**Protocol Features:**
| Protocol | Features |
|----------|----------|
| 1-2 | Basic TAD |
| 3 | Break strategy 8,9; Screen control; Mail system |
| 4+ | UMOD message support |

**Processing:** Stored in `OSVTPN` field, checked before protocol-specific features

**Source:** `MP-P2-TAD.NPL:223-235` (BDOPSV)

---

## Emulator Implementation Checklist

### Required Message Support

**Connection Phase:**
- [ ] 7TMOD - Terminal mode
- [ ] 7TTYP - Terminal type
- [ ] 7DESC - Escape character
- [ ] 7OPSV - OS version (recommend protocol 3)

**Data Transfer:**
- [ ] 7BDAT - Data messages
- [ ] 7RFI - Ready for input
- [ ] 7REJE - Message reject

**Break/Echo:**
- [ ] 7BMMX - Break message (strategies 1-7)
- [ ] 7ECKM - Echo message (strategies 1-7)

**Control:**
- [ ] 7SYCN - System control
- [ ] 7USCN - User control
- [ ] 7CPCO - Completion code

**Connection Control:**
- [ ] 7RESE - Reset
- [ ] 7RECO - Reset confirm
- [ ] 7DCON - Disconnect

**Escape:**
- [ ] 7ESCA - Escape signal
- [ ] 7CERS - Escape response

**Query:**
- [ ] 7ISRQ - ISIZE request
- [ ] 7ISRS - ISIZE response

**Optional (Protocol 3+):**
- [ ] 7NOWT/7TNOW - Nowait status
- [ ] 7NWRE - Nowait restart
- [ ] 7TREP - TREP status
- [ ] 78MOD - 8-bit mode

**Optional (Protocol 4+):**
- [ ] 7UMOD - UMOD strategy

### Buffer Management

- [ ] Implement buffer pool (4-8 buffers)
- [ ] Buffer size: Configurable (typically 512-2048 bytes)
- [ ] Track buffer states (Free/Input/Output/Temp/Mail)
- [ ] Implement POOLLI chain for free buffers

### Message Processing

- [ ] Parse message headers (handle even/odd alignment)
- [ ] Support priority levels (high vs. normal)
- [ ] Implement message queue for normal priority
- [ ] Process high-priority messages immediately

### State Management

- [ ] Track connection state (Disconnected → Connected)
- [ ] Track ISTATE for nowait mode
- [ ] Maintain DFLAG, FLAGB, TINFO flags
- [ ] Track RSPNUM for response waiting

### XMSG Integration

- [ ] Map to your transport layer (X.25, TCP, etc.)
- [ ] Implement buffer exchange mechanism
- [ ] Support wake-up notification for input
- [ ] Handle partner addressing

---

## Testing Recommendations

### Phase 1: Connection Testing
1. Verify INIBDR allocates buffers
2. Verify INISND sends dummy message
3. Verify remote sends 7TMOD, 7TTYP, 7DESC
4. Verify settings applied correctly

### Phase 2: Data Transfer Testing
1. Send simple 7BDAT messages
2. Verify byte alignment handling
3. Test buffer full conditions
4. Test RFI mechanism

### Phase 3: Control Message Testing
1. Test break strategies (1-6)
2. Test echo strategies (1-6)
3. Test escape processing
4. Test system/user control

### Phase 4: Error Handling Testing
1. Send invalid messages (verify 7REJE)
2. Test buffer exhaustion (verify 5WRQI)
3. Test XMSG errors (verify CNVERR)
4. Test fatal errors (verify FAERR → disconnect)

### Phase 5: Disconnection Testing
1. Test graceful disconnect (BDDSCN)
2. Test remote disconnect (7DCON)
3. Test forced disconnect (STOTAD)
4. Verify cleanup (buffers released, port closed)

### Phase 6: Advanced Feature Testing
1. Test ISIZE query/response
2. Test nowait mode operation
3. Test custom break/echo tables (strategy 7)
4. Test 8-bit mode (78MOD)
5. Test completion codes (7CPCO)

---

## Source Code Statistics

### MP-P2-TAD.NPL (Driver)
- **Lines:** 989
- **Entry Points:** 17
- **Key Functions:**
  - INIBDR (382): Initialize connection
  - INISND (398): Send initial buffer
  - BDRINP (439): Input driver
  - BDROUT (800): Output driver
  - BERESP (638): Escape response
  - STOTAD (660): Stop TAD

### RP-P2-TAD.NPL (Routines)
- **Lines:** 1,480
- **Entry Points:** 31+
- **Key Functions:**
  - GETPOOL (44): Get buffer from pool
  - PUTPOOL (20): Return buffer to pool
  - CREMES (198): Create message
  - GETMES (224): Get message
  - BYTPUT (365): Put byte
  - BYTGET (431): Get byte
  - SNDBUF (87): Send buffer
  - SNDRFI (1115): Send RFI
  - SNDREJ (1161): Send reject

### Total Analysis
- **Total Lines Analyzed:** 2,469
- **Message Types Documented:** 30+
- **Entry Points Documented:** 48
- **Mermaid Diagrams Created:** 17
- **Documentation Pages:** ~170 KB total

---

## Related Documentation

### XMSG Analysis
- Location: `Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\XMSG_Metadata_Buffer_Analysis.md`
- Purpose: XMSG buffer and port management patterns
- Relevance: TAD relies heavily on XMSG for all buffer operations

### HDLC Analysis
- Location: `Z:\NorskData\Source Code\Sintran L\Analysis\hdlc-analysis\`
- Purpose: Low-level X.25/HDLC protocol implementation
- Relevance: TAD sits on top of XMSG which uses HDLC for transport

### SCSI Analysis
- Location: `Z:\NorskData\Source Code\Sintran L\NPL\`
- Files: SCSI-*.md
- Purpose: Similar driver analysis methodology
- Relevance: Demonstrates SINTRAN driver architecture patterns

---

## Implementation Notes

### Critical Implementation Details

1. **Byte Alignment:** Always check if `TDBTPT BIT "0"` and insert pad byte (0x00) if starting on odd boundary

2. **Buffer IDs:** Never reuse buffer IDs - always use XMSG-provided IDs

3. **Response Waiting:** When `RSPNUM != 0`, connection is waiting for specific response message

4. **High Priority:** Messages like 7ESCA, 7DCON must be processed immediately, not queued

5. **RFI Flow Control:** When buffer empty, always send 7RFI to request more data

6. **POOLLI Chain:** Maintain linked list of free buffers with proper head/tail management

7. **ISTATE Tracking:** Critical for nowait mode - never mix synchronous and asynchronous operations

8. **Flag Bit Management:** DFLAG, FLAGB, and TINFO bits must be maintained correctly for proper operation

9. **Error Conversion:** XMSG errors must be converted using `CNVERR` before reporting to user

10. **Cleanup on Disconnect:** Must release ALL buffers (TMPBUF, BUFFID, MBFID) and clear BXTADD before XFDCT

---

## Conclusion

The TAD protocol analysis reveals a sophisticated terminal access system with:

- **Message-based communication** over X.25 networks
- **Two-priority message system** for responsive control
- **Automatic flow control** with RFI mechanism
- **Flexible break/echo strategies** with custom tables
- **Comprehensive error recovery** at multiple levels
- **Efficient buffer management** via XMSG integration

The protocol supports both synchronous and asynchronous (nowait) operation, escape handling, remote/local switching, and extensive terminal configuration.

For emulator implementation, the most critical aspects are:
1. Correct message format handling (alignment, padding)
2. Proper buffer pool management
3. High-priority message processing
4. RFI flow control
5. State machine management

All documentation is complete and ready for emulator development.

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\TAD-Analysis-Summary.md`

**Complete Documentation Set:**
1. `TAD-Protocol-Analysis.md` - API and protocol reference
2. `TAD-Message-Formats.md` - Message format specifications
3. `TAD-Protocol-Flows.md` - Protocol flow diagrams
4. `TAD-Analysis-Summary.md` - This summary document

**Total Documentation:** ~170 KB, comprehensive coverage of TAD protocol
