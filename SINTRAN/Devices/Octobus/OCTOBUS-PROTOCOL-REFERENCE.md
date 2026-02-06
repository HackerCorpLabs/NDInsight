# Octobus Protocol Reference

**Source**: SINTRAN III NPL Source Code Analysis (s3vs-4 build)
**Files Analyzed**: PH-P2-OPPSTART.NPL, MP-P2-N500.NPL, CC-P2-N500.NPL, N500-SYMBOLS.SYMB.TXT

---

## 1. Register Layout

The Octobus interface uses two 4-register controllers (Input and Output) at base address 100400 (octal).

### Input Controller (Base 100400)

| Register | Address | Name | Access | Description |
|----------|---------|------|--------|-------------|
| +0 | 100400 | InputReadData | Read | Read received data from FIFO |
| +1 | 100401 | InputWriteData | Write | Write data (rarely used for input) |
| +2 | 100402 | InputReadStatus | Read | Read receiver status |
| +3 | 100403 | InputWriteControl | Write | Control register (InterruptEnable, Reset) |

### Output Controller (Base 100404)

| Register | Address | Name | Access | Description |
|----------|---------|------|--------|-------------|
| +4 | 100404 | OutputReadData | Read | Read transmitted data (loopback) |
| +5 | 100405 | OutputWriteData | Write | Write data to transmit FIFO |
| +6 | 100406 | OutputReadStatus | Read | Read transmitter status |
| +7 | 100407 | OutputWriteControl | Write | Control register (InterruptEnable, Reset) |

**Key Evidence from NPL** (PH-P2-OPPSTART.NPL:4054-4055):
```npl
T:=HDEV+DCONT; 20; *IOXT    % CLEAR INTERFACE (DCONT=3, writes to +3)
T+4; *IOXT                   % (writes to +7 with same value 20 = octal for Reset)
```

---

## 2. Frame Bit Structure

Octobus frames are 16-bit words with the following bit layout:

### Control/Data Frame Format

| Bit | Symbol | Name | Description |
|-----|--------|------|-------------|
| 15 | CBIT | Control Bit | 1=Control frame, 0=Data frame |
| 14-8 | - | Reserved/Data | Upper data bits or reserved |
| 7 | EBIT | Enable Bit | 1=Enable processing, 0=Disabled |
| 6-0 | - | Command/Data | Command code or data bits |

**Symbol Definitions** (N500-SYMBOLS.SYMB.TXT):
```
CBIT = 000017 (octal) = bit number 15
EBIT = 000007 (octal) = bit number 7
```

### Frame Construction Example

From PH-P2-OPPSTART.NPL:3930-3932:
```npl
A SH 10 BONE CBIT BONE EBIT=:X       % Build control frame
T:=100405; A\/CMMACLE; *IOXT         % Send "masterclear Samson system"
A:=X\/CMACONT; *IOXT                 % Send "continue accp"
```

**Breakdown**:
1. `A SH 10` - Shift A left by 10 bits (station address in upper bits)
2. `BONE CBIT` - Set bit 15 (Control bit)
3. `BONE EBIT` - Set bit 7 (Enable bit)
4. `=:X` - Store result in X
5. `A\/CMMACLE` - OR A with CMMACLE command code
6. `*IOXT` - Execute IOX with T=address, A=data

---

## 3. Command Codes (CM* Symbols)

All command codes are defined in N500-SYMBOLS.SYMB.TXT with the `CM` prefix.

### Core System Commands

| Symbol | Octal | Decimal | Binary | Description |
|--------|-------|---------|--------|-------------|
| CMACK | 000000 | 0 | 000000 | Acknowledge |
| CMSYS | 000016 | 14 | 001110 | System parameter |
| CMRES | 000036 | 30 | 011110 | Reset |
| CMRUN | 000033 | 27 | 011011 | Run |
| CMCON | 000035 | 29 | 011101 | Continue |
| CMSTO | 000034 | 28 | 011100 | Stop |

### Master Clear Commands

| Symbol | Octal | Decimal | Binary | Description |
|--------|-------|---------|--------|-------------|
| CMMAC | 000041 | 33 | 100001 | Master Clear |
| CMACO | 000042 | 34 | 100010 | Acknowledge/Continue |

### CPU Control Commands

| Symbol | Octal | Decimal | Binary | Description |
|--------|-------|---------|--------|-------------|
| CMCPU | 000071 | 57 | 111001 | CPU command base |
| CMBUS | 000043 | 35 | 100011 | Bus command |
| CMRAS | 000050 | 40 | 101000 | RAS command |
| CMTES | 000072 | 58 | 111010 | Test command |
| CMDIS | 000062 | 50 | 110010 | Disable command |

### Memory Commands

| Symbol | Octal | Decimal | Binary | Description |
|--------|-------|---------|--------|-------------|
| CMREA | 000020 | 16 | 010000 | Read address |
| CMLPA | 000021 | 17 | 010001 | Load physical address |
| CMDRW | 000026 | 22 | 010110 | Direct read word |
| CMDWW | 000024 | 20 | 010100 | Direct write word |
| CMRWC | 000025 | 21 | 010101 | Read word count |
| CMWWC | 000023 | 19 | 010011 | Write word count |
| CMLMA | 000040 | 32 | 100000 | Load memory address |
| CMTMA | 000052 | 42 | 101010 | Transfer memory address |
| CMRMP | 000054 | 44 | 101100 | Read memory page |
| CMWMP | 000053 | 43 | 101011 | Write memory page |

### Special Commands

| Symbol | Octal | Decimal | Binary | Description |
|--------|-------|---------|--------|-------------|
| CMMIC | 000066 | 54 | 110110 | Microcode command |
| CMLDC | 000052 | 42 | 101010 | Load command |
| CMTEC | 000017 | 15 | 001111 | Test command code |
| CMSPE | 000070 | 56 | 111000 | Special command |
| CMRSE | 000060 | 48 | 110000 | Reset special |
| CMATE | 000044 | 36 | 100100 | Autotest enable |
| CMLOO | 000067 | 55 | 110111 | Loopback command |
| CMBUF | 000063 | 51 | 110011 | Buffer command |
| CMENK | 000061 | 49 | 110001 | Enable kick |
| CMLDM | 000051 | 41 | 101001 | Load memory |
| CMSET | 000055 | 45 | 101101 | Set command |
| CMCCD | 000073 | 59 | 111011 | Clear command descriptor |

### Error/Status Commands

| Symbol | Octal | Decimal | Binary | Description |
|--------|-------|---------|--------|-------------|
| CMBER | 177767 | -9 | (neg) | Bus error |
| CMHWF | 000200 | 128 | 10000000 | Hardware fault |
| CMTRE | 177721 | -47 | (neg) | Transfer error |

---

## 4. Composed Command Symbols

Some commands are composed from base codes plus suffixes:

### CMMACLE (Master Clear Local Element)
```
CMMACLE = CMMAC (000041) composed with control bits
Used for: "masterclear Samson system" frame
```

### CMACONT (Acknowledge Continue)
```
CMACONT = CMACO (000042) composed with control bits
Used for: "continue accp" frame
```

### CMCPURES (CPU Reset)
```
From MP-P2-N500.NPL:3337:
CMCPURES SHZ 10=: X.MCOMMAND    % Send "Reset CPU"
```

### CMSYSPAR (System Parameter)
```
From MP-P2-N500.NPL:3592:
CMSYSPAR SHZ 10\/N100IDENT=:X.MCOMMAND   % Send OMD number to mf-controller
```

---

## 5. Transmission Mechanism

### Immediate Transmission (Single Frame)

Single frames are transmitted immediately when written to the OutputWriteData register (+5):

```npl
T:=100405; A\/CMMACLE; *IOXT    % IOX writes A to address T
```

**What triggers transmission**: Writing to register +5 (100405) immediately queues the data for transmission.

### Multi-Byte Message Transmission

For multi-byte messages, the MBSEND routine is used:

From MP-P2-N500.NPL:3332-3340:
```npl
% Build Octobus message
5STATION=:"LMFIELD".MOCTSTATION                     % Station number
OMDACCP =:        X.MOCTOMD                         % OMD number
0       =:        X.MBROADCAST                      % Not broadcast
1       =:        X.MMSGLENGTH                      % Message length = 1 byte
CMCPURES SHZ 10=: X.MCOMMAND                        % Send "Reset CPU"
"LMDF"=:B; T:=5OMDNO; X:=OCTORING
"LMFIELD+DPITPHYS"=:D; A:=DPITBANK
CALL MBSEND; 0/\0
```

### Kick Mechanism

The "Kick" mechanism is used to signal ND-500 CPUs via Octobus:

From MP-P2-N500.NPL:3270-3297:
```npl
XKICK500: A=:CKICKTYPE; *TRA STS
       A SH -5/\170=:CLVL
       ...
LV12KICK:                                       % Entry point on level 12
       *ION
       A:=L=:"LREG"
       T:=5STATION; X:=OCTORING; A:=CKICKTYPE   % Set up kick parameters
       CALL SKICK; GO ERR                       % Call octobus kick routine
```

**Kick Types** (from SYMBOL-1-LIST.SYMB.TXT):
- 0KICK = 000000 - Base kick
- FKICK = 000002 - Fast kick
- PKICK = 000016 - Priority kick
- KICKD = 000020 - Kick data
- KICKH = 000016 - Kick high
- KICK = 000064 - Default kick

**Kick Routines**:
- `N100KICK` - Kick from ND-100
- `IDLEKICK` - Idle kick
- `CLRKICK` - Clear kick
- `XKICK500` - Execute kick to ND-500
- `SKICK` - Send kick (address 037254)

---

## 6. Message Structure

### Octobus Message Header Fields

From MP-P2-N500.NPL usage patterns:

| Field | Description | Example |
|-------|-------------|---------|
| MOCTSTATION | Station number | 5STATION |
| MOCTOMD | OMD (Object Module Descriptor) number | OMDACCP, MFOMDNO |
| MBROADCAST | Broadcast flag | 0 = not broadcast |
| MMSGLENGTH | Message length in bytes | 1, 3, 7 |
| MCOMMAND | Command code (shifted) | CMCPURES SHZ 10 |

### Message Length Values

| MMSGLENGTH | Meaning |
|------------|---------|
| 1 | 1 byte payload |
| 3 | 2 byte payload |
| 7 | 4 byte payload |

### Example Message Construction

**Reset CPU Message** (MP-P2-N500.NPL:3333-3340):
```
Station: 5STATION
OMD: OMDACCP
Broadcast: 0 (unicast)
Length: 1 byte
Command: CMCPURES (CPU Reset)
```

**System Parameter Message** (MP-P2-N500.NPL:3623-3626):
```
Station: 5STATION
OMD: OMDACCP
Broadcast: 0 (unicast)
Length: 7 (4 bytes payload)
Command: CMSYSPAR | N100IDENT
```

---

## 7. Interface Initialization (OCSTART)

From PH-P2-OPPSTART.NPL:4043-4086:

### Initialization Sequence

1. **Check if interface exists**:
```npl
T:=HDEV+2; *IOXT; TRA IIC    % Read status, check for IOX error
IF A=7 THEN                   % IOX error = no interface
    0=:TMR; 0=:OCTICONT(0)   % Reset timer
    0=:OCTOCONT(0); GO OUT   % No Octobus
FI
```

2. **Clear both controllers**:
```npl
T:=HDEV+DCONT; 20; *IOXT     % Clear input controller (DCONT=3)
T+4; *IOXT                    % Clear output controller (+7)
```
Note: Value 20 (octal) = Reset/MasterClear bit

3. **Allocate buffer pool and tables**

4. **Create ident entries**

### ND-500 Detection (CH5CPUPRESENT)

From PH-P2-OPPSTART.NPL:3903-3945:

```npl
T:=100406; *IOXT              % Read Octobus output status
IF A=0 THEN                   % Octobus present?
    DO                        % Wait for data ready
        *IOXT
    WHILE A NBIT 3            % Check bit 3 (DataReady)
    OD
    ASTATION\/COMD=:5STATION
    A SH 10 BONE CBIT BONE EBIT=:X
    T:=100405; A\/CMMACLE; *IOXT         % Send masterclear
    A:=X\/CMACONT; *IOXT                 % Send continue
    ...
FI
```

---

## 8. Status Register Bits

### Input Status (Register +2, 100402)

| Bit | Name | Description |
|-----|------|-------------|
| 7 | Done/Ready | Data available in receive FIFO |
| 3 | DataReady | FIFO has data (polled) |
| 0 | Busy | Receiver busy |

### Output Status (Register +6, 100406)

| Bit | Name | Description |
|-----|------|-------------|
| 7 | Done/Ready | Transmission complete |
| 3 | ReadyForTransfer | Transmitter ready for data |
| 0 | Busy | Transmitter busy |

### Control Register (Registers +3, +7)

| Bit | Symbol | Name | Description |
|-----|--------|------|-------------|
| 4 | - | MasterClear/Reset | Reset controller (value 20 octal) |
| 0 | - | InterruptEnable | Enable interrupts |

---

## 9. Interrupt Handling

### Ident Codes

| Controller | Ident Code (Octal) | Description |
|------------|-------------------|-------------|
| Input | 40 | Input controller interrupt |
| Output | 41 | Output controller interrupt |

### Interrupt Enable

Interrupts are enabled by setting bit 0 in the control register:
- Input: Write to +3 (100403)
- Output: Write to +7 (100407)

### IDENT Operation

The IDENT instruction returns the controller's ident code when the controller has an active interrupt. After IDENT is read, the interrupt is acknowledged (cleared).

---

## 10. Loopback Mode

When the interface is in loopback mode (or no external device is connected), data written to the output appears on the input:

```
Write to OutputWriteData (+5) -> Data echoed to InputReadData (+0)
```

This is used for CONFIG tool testing where it checks that both ident codes (40 and 41) are returned.

---

## 11. OCTORING and Related Structures

### OCTORING
The OCTORING is a ring buffer structure used for Octobus message queuing.

### Related Symbols (from N500-SYMBOLS.SYMB.TXT):
- 5STATION - Current station number
- 5OMDNO - OMD number for current operation
- OMDACCP - OMD number for ACCP
- MFOMDNO - MF controller OMD number
- OCTICON - Octobus input controller table
- OCTOCONT - Octobus output controller table

---

## 12. Error Handling

### Error Codes from Octobus Messages

From MP-P2-N500.NPL:3378-3419 (5OMBREAD comments):

**ACCP Error Codes**:
```
errcode: hwfault = 200B (octal) = Fatal hardware fault
errtype: accperr = 1 = Memory error reported from ACCP
```

**MF-Controller SEC Codes**:
- 20B = Corrected memory error
- 30B = Memory timeout
- 31B = Unknown error
- 50B = Memory write parity
- 51B = Memory I/O error
- 77B = Fatal error in MF-controller

**MP Error Codes**:
- errtype: mperr = 2 = Error reported from microprogram

---

## Summary: Transmission Flow

1. **Build Frame**: Combine station address, control bits (CBIT, EBIT), and command code
2. **Set Address**: Load T register with output register address (100405 for data, 100407 for control)
3. **Execute IOX**: Write data via IOXT instruction
4. **Wait for Completion**: Poll status register or wait for interrupt
5. **Read Response**: Check input controller for response data

**Key Points**:
- Single frames transmit immediately on write to +5
- Multi-byte messages use MBSEND with message structure
- Kicks use SKICK/XKICK500 for signaling ND-500
- Control frames have bit 15 (CBIT) set
- Enable bit (bit 7, EBIT) must be set for processing

---

*Generated from SINTRAN III NPL source code analysis*
*Last Updated: 2026-01-31*
