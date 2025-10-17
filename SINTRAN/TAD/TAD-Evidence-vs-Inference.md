# TAD Protocol: Evidence vs. Inference

## Purpose

This document clearly separates what is **proven from SINTRAN source code** versus what has been **inferred from standards and traces**.

---

## ✅ PROVEN FROM SOURCE CODE

### TAD Message Types (100% Real)

**Source:** `MP-P2-TAD.NPL:328-338`

```npl
SYMBOL EBUFF=7DUMM\0         % DUMMY-MESSAGE 0 (EMPTY BUFFER)
SYMBOL BDESC=7ESCA\0         % ESCAPE-MESSAGE
SYMBOL RLOCA=7RLOC\0         % REMOTE LOCAL (RUBOUT NORD-NET)
SYMBOL BDDIS=7DCON\0         % DISCONNECT-MESSAGE
SYMBOL CESCR=7CERS\0         % CESC-RESP MESSAGE
SYMBOL RESCF=7RECO\0         % RESET-CONF MESSAGE
SYMBOL NWREM=7NWRE\0         % NOWAIT RESTART MESSAGE
SYMBOL ISZRS=7ISRS\2         % ISIZE RESPONSE MESSAGE
SYMBOL ERRSP=7ERRS\2         % ERROR-RESPONSE MESSAGE
SYMBOL TREPS=7TREP\2         % TREPP STATUS MESSAGE
```

**Additional message types found in RP-P2-TAD.NPL:**
- `7BDAT` - Data message (line 508)
- `7TMOD` - Terminal mode (line 509)
- `7TTYP` - Terminal type (line 512)
- `7DESC` - Define escape (line 518)
- `78MOD` - 8-bit mode (line 515)
- `7OPSV` - OPSYS version (line 524)
- `7REJE` - Reject (line 527)
- `7RFI` - Ready for input (RP-P2-TAD.NPL:1115)

**Evidence:** These are actual NPL SYMBOL declarations and code references.

---

### TAD Message Format (100% Real)

**Source:** `RP-P2-TAD.NPL:162-178` (`WRMHEAD` routine)

```npl
WRMHEAD: A=:D; T:=TDTAFI; X:=TDTALA
         IF TDBTPT BIT "0" THEN                    % ODD START, CREATE PAD BYTE
            A SHZ -1; X+A; *LDATX
            A/\177400; *STATX
            MIN TDBTPT; REMSIZ-1=:REMSIZ
         FI
         X:=TDTALA
         TDBTPT SHZ -1; X+A; D=:A; *STATX          % WRITE IN BUFFER
         TDBTPT+2=:TDBTPT; REMSIZ-2=:REMSIZ
         EXIT
```

**Proven facts:**
- If TDBTPT is odd, insert pad byte (0x00)
- Message header is 2 bytes (message type + byte count)
- TDBTPT and REMSIZ are updated after writing

**Evidence:** Actual NPL source code for message header creation.

---

### XMSG Integration (100% Real)

**Source:** `MP-P2-TAD.NPL:382-395` (`INIBDR` routine)

```npl
INIBDR: CALL STADIWINDOW
        IF PORTNO=0 THEN
          0=:TDRADDR.BXTADD; T:=XFOPN; CALL MXMSG; GO IERR    % OPEN PORT
          A=:PORTNO
          A:=0=:D; AD=:OTAD.POOLLI                            % POOL EMPTY
          T:=XFALM; FBSIZ; X:=NOBUFF; CALL MXMSG; GO IERR     % ALLOCATE
          FOR X:=1 TO NOBUFF DO; X=:XRSA
             T:=XFGET; A:=FBSIZ; CALL MXMSG; GO IERR          % RESERVE BUFFERS
             % ... add to pool ...
          OD
        FI
```

**Proven facts:**
- TAD calls `XFOPN` to open a port
- TAD calls `XFALM` to allocate message space
- TAD calls `XFGET` to reserve buffers
- TAD calls `XFSND` to send messages (line 404, 478, etc.)

**Evidence:** Direct XMSG API calls in TAD source code.

---

### Buffer Pool Management (100% Real)

**Source:** `RP-P2-TAD.NPL:44-56` (`GETPOOL`)

```npl
GETPOOL: *IOF
         IF BUFFID=0 THEN
            POOLLI; IF A=0 AND D=0 GO NOPOL           % POOL EMPTY
            A=:T; D=:X; *LDDTX                        % GET NEXT IN CHAIN
            AD=:POOLLI                                % UPDATE POOL POINTER
            T=:A; X=:D; *STZTX                        % ENSURE REF&FUNC IS ZERO
            AD=:TDTADD; X+2; *LDATX                   % SAVE ADDRESS AND GET BUFFID
            A=:BUFFID; BUDIS=:TDBTPT                  % SET BUFFID AND BYTE POINTER
            41ITAD.FBSIZ-BUDIS=:REMSIZ                % SET REMAINING SIZE
         FI; *ION
         EXITA
```

**Proven facts:**
- Buffer pool managed via POOLLI linked list
- Buffers have BUFFID, TDTADD, TDBTPT, REMSIZ fields
- Interrupt-off (IOF) used during pool access

**Evidence:** Actual NPL source code for buffer management.

---

### Datafield Structure (100% Real)

**Source:** Various references throughout TAD source

**Proven fields in input datafield:**
- `BUFFID` - Buffer ID
- `TDTADD` (TDTAFI:TDTALA) - Buffer address
- `TDBTPT` - Byte pointer
- `REMSIZ` - Remaining size
- `CURMES` - Current message type
- `REMBYT` - Remaining bytes in message
- `PORTNO` - XMSG port number
- `PARTNER` - Partner port address
- `DFLAG` - Datafield flags
- `FLAGB` - Additional flags
- `CESCP` - Escape character
- `CTTYP` - Terminal type
- `OSVTPN` - OS version + TAD protocol

**Evidence:** Field names used throughout TAD source code.

---

## ⚠️ INFERRED FROM STANDARDS AND TRACES

### X.25 Packet Format (Inferred)

**What I documented:**
```
┌──────┬──────┬────────┬──────────┬───────────┬──────────┐
│ GFI  │ LCN  │ Packet │  Called  │  Calling  │   CUD    │
│ (1)  │ (1)  │ Type   │  Address │  Address  │  (var)   │
└──────┴──────┴────────┴──────────┴───────────┴──────────┘
```

**Evidence:**
- X.25 is a real ITU-T standard (this format is correct for X.25)
- PAD_Connection_Analysis.md shows X.25-like frames
- **BUT:** I did NOT find SINTRAN source code that parses/creates X.25 packets

**Status:** Reasonable inference based on X.25 standard, but **not proven from source**.

---

### CUD (Call User Data) Format (Speculative)

**What I documented:**
```
Protocol ID: 0x01 0x02 0x00 0x00 (TAD)
Service Type: 0x00 (Interactive)
Terminal Type: 0x0100 (Generic)
Options: 0x40 (Remote echo)
```

**Evidence:**
- X.25 standard says CUD field exists (this is real)
- PAD_Connection_Analysis.md shows bytes `04 00 DA 10` and `04 00 DA 14`
- **BUT:** I do NOT know what those bytes mean
- **I INVENTED the Protocol ID format** without source code backing

**Status:** **Speculative** - X.25 CUD exists, but the specific TAD format is **not proven**.

---

### HDLC Encapsulation (Mostly Real)

**What I documented:**
```
HDLC Frame: [Flag, Address, Control, X.25 Packet, FCS, Flag]
```

**Evidence:**
- HDLC format is correct (standard protocol)
- Extensive HDLC analysis in `hdlc-analysis/` directory
- Actual frame traces show HDLC structure
- **Connection:** TAD uses XMSG, which uses HDLC for transport

**Status:** HDLC format is real, connection to TAD is **inferred** (no source code showing explicit TAD→HDLC mapping).

---

## 🔍 WHAT WE DON'T KNOW FROM SOURCE

### Missing Information

1. **How XMSG maps to X.25 packets**
   - XMSG API exists (proven)
   - X.25 packets exist in traces (proven)
   - **Mapping between them:** Not found in TAD source

2. **CUD field contents for TAD**
   - CUD field exists in X.25 (standard)
   - TAD has protocol info (OSVTPN field exists)
   - **How TAD info goes into CUD:** Not found in source

3. **X.25 layer implementation**
   - TAD calls XMSG (proven)
   - XMSG likely handles X.25 layer
   - **X.25 implementation details:** Not in TAD source files

4. **Protocol identification mechanism**
   - Different protocols exist (TAD, routing, PAD)
   - Traces show different frame formats
   - **How receiver identifies protocol:** Not clear from TAD source

---

## 📊 Evidence Quality Summary

| Topic | Evidence Level | Source |
|-------|----------------|--------|
| TAD message types | ✅ 100% Proven | MP-P2-TAD.NPL:328-338 |
| Message format (header) | ✅ 100% Proven | RP-P2-TAD.NPL:162-178 |
| XMSG API calls | ✅ 100% Proven | MP-P2-TAD.NPL:384+ |
| Buffer management | ✅ 100% Proven | RP-P2-TAD.NPL:44-56 |
| Datafield structure | ✅ 100% Proven | Throughout TAD source |
| Connection lifecycle | ✅ 100% Proven | MP-P2-TAD.NPL:382-409 |
| HDLC frame format | ✅ 95% Proven | HDLC analysis + traces |
| X.25 packet format | ⚠️ 70% Inferred | X.25 standard + traces |
| TAD→X.25 mapping | ⚠️ 40% Speculative | Logical inference |
| CUD format | ⚠️ 20% Speculative | **Invented by me** |
| Protocol ID values | ⚠️ 10% Speculative | **Invented by me** |

---

## 🎯 What You Can Trust

### For Emulating TAD Protocol:

**Definitely use:**
1. TAD message types and formats (proven from source)
2. XMSG API pattern (proven from source)
3. Buffer management structure (proven from source)
4. Message header format with pad bytes (proven from source)
5. Datafield structure and flags (proven from source)

**Use with caution:**
1. X.25 packet structure (standard format, but TAD usage not proven)
2. HDLC encapsulation (correct format, but details inferred)

**Do NOT rely on:**
1. CUD Protocol ID format (I made this up)
2. Specific CUD field layout (I made this up)
3. Protocol identification values (I made this up)

---

## 🔧 What To Do For Real Implementation

### Option 1: Use XMSG Abstraction

**Don't worry about X.25/HDLC details:**
```
Your Code → XMSG API → [Black Box] → Network
```

Implement XMSG-level API and let SINTRAN's XMSG layer handle X.25/HDLC.

### Option 2: Analyze XMSG Source Code

**Find the missing link:**
- Locate XMSG implementation files
- Find how XMSG creates X.25 packets
- Discover actual CUD format used

**Likely files to search:**
- `*XMSG*.NPL` - XMSG implementation
- `*X25*.NPL` - X.25 layer
- `*PAD*.NPL` - PAD implementation

### Option 3: Network Capture Analysis

**Capture real SINTRAN TAD connection:**
- Set up two SINTRAN systems
- Establish TAD connection
- Capture network traffic
- Analyze actual X.25 Call Request packet
- Extract real CUD format

---

## 📝 Corrections to My Documentation

### Documents to Update/Qualify

**TAD-X25-CUD-Specification.md:**
- Mark as "**SPECULATIVE**" - not from source code
- Useful as a starting point but not authoritative
- CUD format is an educated guess, not proven

**TAD-HDLC-Encapsulation.md:**
- HDLC format: ✅ Correct
- X.25 format: ✅ Correct (standard)
- TAD→X.25 mapping: ⚠️ Inferred
- CUD contents: ❌ Speculative

**All other TAD documents:**
- TAD-Protocol-Analysis.md: ✅ Accurate (from source)
- TAD-Message-Formats.md: ✅ Accurate (from source)
- TAD-Protocol-Flows.md: ✅ Accurate (from source)
- TAD-Analysis-Summary.md: ✅ Accurate (from source)

---

## 🎓 Lessons Learned

### What I Did Right
1. Thoroughly analyzed TAD NPL source code
2. Extracted real message types and formats
3. Documented proven APIs and structures
4. Created accurate flow diagrams

### What I Did Wrong
1. **Invented X.25 CUD format** without source code proof
2. **Assumed protocol ID values** without evidence
3. **Did not clearly distinguish** proven vs. inferred information
4. **Presented speculation as fact** in some documents

### How to Fix
1. Clearly label speculative content
2. Search for XMSG/X.25 source code
3. Capture real network traffic for validation
4. Update documents with evidence levels

---

## ✅ Summary

**You can trust:**
- TAD message protocol (application layer)
- XMSG API usage pattern
- Buffer and datafield structures
- Message processing flow

**Be skeptical of:**
- X.25 CUD field contents (I guessed)
- Protocol ID values (I invented)
- Exact X.25 packet format for TAD (inferred)

**To get the truth:**
- Analyze XMSG source code
- Capture real SINTRAN TAD traffic
- Or: Just implement at XMSG API level and ignore lower layers

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\TAD-Evidence-vs-Inference.md`

**Author's Note:** I apologize for not clearly separating proven facts from reasonable inferences. The TAD protocol itself is real and well-documented from source code. The X.25/CUD layer is where I made educated guesses that should be validated against actual implementation or network captures.
