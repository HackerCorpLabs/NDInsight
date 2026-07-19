# Octobus Protocol Reference

**Source**: SINTRAN III NPL Source Code Analysis (s3vs-4 build)
**Files Analyzed**: PH-P2-OPPSTART.NPL, MP-P2-N500.NPL, CC-P2-N500.NPL, N500-SYMBOLS.SYMB.TXT

> **CORRECTIONS 2026-07-15** (byte-level carve of the L-VSX-500 resident image; see
> `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md` for the
> full consolidated reference and the carve evidence):
>
> 1. **Section 2 (Frame Bit Structure) is INCOMPLETE/WRONG**: bits 14-8 are NOT
>    "Reserved/Data". The full frame is C=bit 15, **B(broadcast)=bit 14,
>    DEST/SOURCE station=bits 13-8**, E=bit 7, **K(kick)=bit 6, M(multibyte)=bit 5,
>    S(start/end)=bit 4**, low bits = code/number/data. Byte-proven by the SOCTO
>    receive dispatch in `re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm` (035555-035577)
>    and by SKICK (037254) building `C | K | station<<8 | kickno`.
> 2. **RETRACTED 2026-07-16 - section 9's ident codes 40/41 are CORRECT.**
>    The earlier version of this point "corrected" them to 37B/40B based on the
>    ITB13 byte evidence (ITB13+37B/+40B hold the IOCT0/OOCT0 datafield
>    addresses 123511/123537). That reading was DISPROVEN LIVE by two
>    independent ND diagnostics run against the RetroCore emulator on
>    2026-07-16: TPE OCTOBUS B00 LIST-OCTOBUS-DEVICES prints the hardware
>    table (100400 -> 40/41 ... 100430 -> 46/47, receive/transmit, LVL 13),
>    and CONFIGURATION D05 reported "Wrong identcode(s) found on level 13D.
>    Expected identcodes: 40B and 41B. Found identcodes: 37B and 40B" while
>    the emulator answered 37B/40B - and NO ERRORS once it answered 40B/41B.
>    The ITB13 slot index is therefore NOT the ident code itself (plausibly
>    the table holds ident N at ITB13+N-1 - UNVERIFIED). The 41B/42B values
>    near the frame sender remain on-wire CM codes (CMMAC/CMACO), unrelated.
> 3. **The driver bodies ARE carved** (earlier claims of "uncarved" are obsolete):
>    SOCTO=035546, SOCTW=036342, SKICK=037254, MBSEND=037425, OMBREAD=037660,
>    CONOMD=040062, ECONID=040467 in `026-S3IMPIT.asm` (load base 32000B; symbol
>    03xxxx addresses resolve against the PIT-mapped image, NOT commoncode).
> 4. **Kick numbers on the wire** (triple-verified: NPL symbols + ND-05.020.01 p.336
>    + ND-5800 microcode OCB_DEC_K): N100KICK=1, activate=2, CLRKICK=3, clock=4,
>    NUCLEUS=5, IDLEKICK=6. The "kick types" table in section 5 lists assembler
>    symbols (0KICK/FKICK/PKICK/...) whose relationship to these wire numbers is
>    NOT established - do not conflate them.
> 5. Control-register values byte-confirmed: 4 = transmit enable (bit 2),
>    1 = interrupt enable (bit 0), 20B = clear/master-clear (bit 4).

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

> **CORRECTED 2026-07-15** (was: bits 14-8 "Reserved/Data", bit 7 "Enable"). The
> full layout below is byte-proven by the carved SOCTO/SKICK driver code and
> matches ND-05.020.01 Appendix 2 section 2.5. EBIT is the EMERGENCY bit, not
> "enable" (EBIT|CMMAC = the manual's emergency code 241B).

| Bit | Symbol | Name | Description |
|-----|--------|------|-------------|
| 15 | CBIT | Control Bit | 1=Control frame, 0=Data frame |
| 14 | - | Broadcast (B) | 1=broadcast to station type, 0=unicast |
| 13-8 | - | DEST/SOURCE | Destination station when sending; source when receiving |
| 7 | EBIT | Emergency (E) | 1=emergency message (highest priority) |
| 6 | - | Kick (K) | 1=kick message (kick number in bits 5-0) |
| 5 | - | Multibyte (M) | 1=SOMB/EOMB framing |
| 4 | - | Start (S) | 1=start of multibyte, 0=end |
| 3-0 / 6-0 | - | Number/Data | OMD/ident/kick number or CM* command code |

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

> **CORRECT AS ORIGINALLY WRITTEN - LIVE-VERIFIED 2026-07-16.** This table was
> temporarily "corrected" to 37B/40B based on the ITB13 byte evidence; that
> reading was then DISPROVEN by two independent ND diagnostics (TPE OCTOBUS B00
> LIST-OCTOBUS-DEVICES and CONFIGURATION D05: "Expected identcodes: 40B and
> 41B") - see the retraction in the corrections banner at the top of this file.

| Controller | Ident Code (Octal) | Description |
|------------|-------------------|-------------|
| Input (receive) | 40 | Input controller interrupt (level 13) |
| Output (transmit) | 41 | Output controller interrupt (level 13) |

Interfaces 2-4 (100410/100420/100430) use ident pairs 42/43, 44/45, 46/47
(TPE LIST-OCTOBUS-DEVICES table, live 2026-07-16).

### Interrupt Enable

Interrupts are enabled by setting bit 0 in the control register:
- Input: Write to +3 (100403)
- Output: Write to +7 (100407)

### IDENT Operation

The IDENT instruction returns the controller's ident code when the controller has an active interrupt. IDENT is a ONE-SHOT acknowledge (live-verified vs TPE OCTOBUS B00, 2026-07-16): it clears the interrupt request AND the interrupt-enable bit of the acknowledged controller - software must re-enable (re-arm) to get the next interrupt. Interrupts are EVENT-triggered (a word arrived / a transfer completed); an idle ready-for-transfer state with the enable set does NOT interrupt.

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
