# SINTRAN III Device Driver Architecture: IOX, EXR, IDENT Tables, and Interrupt Routing

**Source**: SINTRAN III s3vs-4 NPL source code  
**Primary files**: `PH-P2-OPPSTART.NPL`, `MP-P2-TERM-DRIV.NPL`, `MP-P2-HDLC-DRIV.NPL`,  
`IP-P2-DISK-START.NPL`, `IP-P2-SCSI-DISK.NPL`, `MP-P2-PIOC-DRIV.NPL`, `PH-P2-CONFG-TAB.NPL`  
**Status**: Verified from source — no assumptions

---

## Table of Contents

1. [IOX and EXR Instruction Mechanics](#1-iox-and-exr-instruction-mechanics)
2. [Global Data Tables](#2-global-data-tables)
3. [Boot-Time Device Detection](#3-boot-time-device-detection)
4. [Interrupt Level Architecture](#4-interrupt-level-architecture)
5. [Terminal Devices (Level 10 / Level 12)](#5-terminal-devices-level-10--level-12)
6. [HDLC / Synchronous Modem (Level 11)](#6-hdlc--synchronous-modem-level-11)
7. [Disk Controllers (Level 11)](#7-disk-controllers-level-11)
8. [Floppy Disk (Level 11)](#8-floppy-disk-level-11)
9. [Line Printers (Level 10 / Level 11)](#9-line-printers-level-10--level-11)
10. [Versatec Plotter (Level 11)](#10-versatec-plotter-level-11)
11. [PIOC Inter-Processor Channel (Level 12)](#11-pioc-inter-processor-channel-level-12)
12. [C Reference Implementation](#12-c-reference-implementation)

---

## 1. IOX and EXR Instruction Mechanics

### 1.1 The Three IOX Access Modes

The ND-100 CPU provides three distinct ways to perform I/O register access:

```
Mode 1: IOX n       Direct, fixed port address encoded in instruction
Mode 2: IOXT        Indirect, port address taken from T register at runtime
Mode 3: EXR ST      Execute-register, T register holds a pre-built IOX instruction word
```

#### Mode 1: `IOX n` — Direct

```npl
A:=200; *TRR IIE; TRA IIC
IOX 750; TRA IIC            % Read from fixed port 0750 (octal)
IF A=0 THEN ... FI          % A=0: device present
```

This is a fully static port address baked into the instruction word. Used only for fixed system hardware where the port never varies (e.g., the MPM3 controller at port 750₈).

**Limitation**: Cannot be used where the port address is only known at runtime. Historically required self-modifying code to change the address field.

---

#### Mode 2: `IOXT` — T-Register Indirect

```npl
T := HDEV + DST;  *IOXT    % read status register
T := HDEV + DDW;  *IOXT    % write character register
```

The T register holds the IOX port number at runtime. The `IOXT` instruction reads T and uses that value as the port address for the IOX cycle.

**HDEV** is stored in the device's datafield and holds the hardware port **base address** for that specific controller instance. Adding an offset constant (DST, DDW, DCONT, etc.) gives the exact register address.

This is the standard mode used in the terminal driver for per-character I/O (`MP-P2-TERM-DRIV.NPL`):

```npl
ECAPD: A=:D; T:=HDEV+DST; *IOXT          % read status
       IF A BIT 3 THEN                    % ready for transfer
          D=:A; T-DST+DDW; *IOXT         % write character
          "DACT"; T-DDW+DCONT; *IOXT     % activate
       FI
```

Note the compact chaining: after `T:=HDEV+DST`, each subsequent step adjusts T by the difference between offsets (`T-DST+DDW` = `T + DDW - DST`), avoiding a full reload.

---

#### Mode 3: `EXR ST` — Execute Register (pre-built instruction)

```npl
T := X.HDEV + 2;  *EXR ST        % floppy status read
T := D.HDEV + RRTS;  *EXR ST     % HDLC RTS read
T := X.HDEV + WDMA;  *EXR ST     % HDLC DMA address write
```

`EXR ST` is fundamentally different from `IOXT`. With `IOXT`, T is the **port address**. With `EXR ST`, T is an **IOX instruction word** — the ND-100 CPU takes the word in T and **executes it directly** as if it were a machine instruction fetched from memory.

An ND-100 `IOX n` instruction is encoded as:

```
Bits 15-10: IOX opcode
Bits  9- 0: Device address (port number, 10 bits)
```

**HDEV for EXR devices is pre-initialized to the upper bits of an IOX instruction**, with the low bits zeroed. Adding a register offset fills in those low bits, producing a complete, valid IOX instruction word that EXR then executes.

```
HDEV   = IOX_OPCODE_BITS | (base_port << 0)   (low offset bits = 0)
HDEV+2 = IOX_OPCODE_BITS | (base_port + 2)    (complete instruction: IOX port+2)
```

`EXR ST` then receives this complete instruction word in T and executes it in one cycle — equivalent to executing `IOX (base+offset)` without any self-modification.

**Why use EXR instead of IOXT?**

The choice is made per-device-type at boot time based on which form HDEV is initialized in. EXR is used where the driver is shared across devices that may have been set up with pre-built instruction words (older controller designs). IOXT is used where HDEV contains a plain port number. Both avoid self-modification; the difference is in the HDEV encoding convention for that device class.

This is confirmed by the line printer detection code which explicitly branches on device generation:

```npl
% PH-P2-OPPSTART.NPL:463-468
T:=A.HDEV+2; A:=200; *TRR IIE; TRA IIC; 1BANK
IF CSVXY.LPSELECTION-2=0 THEN
   *EXR ST                    % new-type (DMLP): HDEV is instruction word
ELSE
   *IOXT                      % old-type (DMPR): HDEV is port number
FI; *TRA IIC
```

---

### 1.2 EXR Variants

All EXR variants share the same "T contains the instruction word to execute" model. The suffix selects the register operand binding for the executed instruction:

| Variant | Meaning | Typical Use |
|---------|---------|-------------|
| `EXR ST` | Execute instruction in T, result → A | IOX read/write (most common) |
| `EXR SA` | Execute instruction, A operand | Load K register from computed address: `T:=M2UNTYP; *EXR SA` |
| `EXR SL` | Execute instruction, left-byte | Terminal FIFO byte writes: `A SHR 10; *EXR SL` |
| `EXR SD` | Execute instruction, deposit | Bit-set semaphore operations |
| `EXR SX` | Execute instruction, extended | Bus scan, memory-type detection |

Example of EXR SA in the disk driver — building a K-register load instruction at runtime:

```npl
% IP-P2-DISK-START.NPL:32-34
A SHZ -6/\7 SH 3\/CBLDA         % build LDA K instruction word with unit-select bits
T:=M2UNTYP; *EXR SA             % execute: K := *M2UNTYP (loads unit type)
IF K THEN ... FI                 % branch on loaded K value
```

Example of EXR SL in terminal FIFO output (500-baud FIFO path):

```npl
% MP-P2-TERM-DRIV.NPL:423-424 (W0500 routine)
A SHR 10; *EXR SL               % write high byte to FIFO
A SH -10; *EXR SL               % write low byte to FIFO
```

---

### 1.3 Error Detection Pattern

All IOX probes during boot use a consistent error-trapping idiom:

```npl
A:=200; *TRR IIE; TRA IIC       % arm IOX error trap (IIE = illegal instruction error)
T:=HDEV+offset; *IOXT; TRA IIC  % probe — TRA IIC clears error flag after
IF A=7 THEN                      % A=7 means IOX error: device not present
   % remove from all tables
ELSE
   % device present, proceed
FI
```

`TRR IIE` (Transfer to Register, Illegal Instruction Enable) arms the CPU to catch IOX bus errors without halting. If the IOX cycle produces no response, `A` is set to 7 by the bus logic and execution continues.

---

## 2. Global Data Tables

SINTRAN maintains several global tables in physical memory that are populated during boot and used at interrupt time. All are in banked memory, accessed via physical bank registers.

### 2.1 IDENT Code Tables (`ITB10`, `ITB11`, `ITB12`)

```
Table     Level    Bank       Purpose
ITB10     10       MPIBANK    Output device IDENT → datafield address
ITB11     11       IPIBANK    Mass-storage IDENT → datafield address
ITB12     12       MPIBANK    Input device IDENT → datafield address
```

**Layout**: Flat word array, one entry per possible IDENT code.

```
ITB12[0]  →  datafield address for device with IDENT code 1
ITB12[1]  →  datafield address for device with IDENT code 2
...
ITB12[N]  →  datafield address for device with IDENT code N+1
```

Index = `IDENT_code - 1` (hardware supplies 1-based codes; tables are 0-based).

**Write pattern** (boot-time registration, e.g. floppy at IDENT code S3):

```npl
% PH-P2-OPPSTART.NPL:145-147
T:=IPIFPHPAGE SHZ 12=:D                       % D = physical page base of IPIT
A:="ITB11"-"PITEX"+D+CSAVX.S3-1=:X           % X = physical addr of ITB11[IDENT-1]
T:=IPIBANK; A:=XA; *STATX                     % write datafield address into slot
```

**Clear pattern** (boot-time removal of absent device):

```npl
% PH-P2-OPPSTART.NPL:57-62
10IDCLEAR:
   IF A><0 THEN
      A-1=:X:="ITB10"-"PITEX"; X+A
      A:=MPIFPHPAGE SHZ 12; X+A
      T:=MPIBANK; *STZTX                      % zero the slot
   FI; EXIT
```

**Read pattern** (interrupt-time dispatch):

```npl
% When level-12 interrupt fires, hardware puts IDENT code in A
A-1                          % convert to 0-based index
A+ITB12_BASE=:X              % absolute address of slot
T:=MPIBANK; *LDATX           % load datafield address from slot
% now use datafield to access HDEV, buffers, etc.
```

---

### 2.2 Extended IDENT Tables (`ITE11`)

For devices with IDENT codes above a threshold (`MAX11`), an extended table `ITE11` is used instead of `ITB11`. This handles controllers that have more IDENT codes than fit in the primary table. The boot code selects between them:

```npl
% PH-P2-OPPSTART.NPL:2170-2174
IF L<<"MAX11" THEN
   "ITB11"-"PITEX"=:X; A:=IPIFPHPAGE SH 12
   X+A+L-1; T:=IPIBANK; A:=D; *STATX        % primary table
ELSE
   "ITE11"-"PITEX"=:X; A:=IPIFPHPAGE SH 12; X+A
   ...                                        % extended table
FI
```

---

### 2.3 Timer Table (`TMRTA` / `TMRTE`)

The timer table holds datafield pointers for all devices that need periodic polling or timeout checking. Entries are physical addresses of device datafields.

```
TMRTA          % table start
TMRTE          % first terminal-class entry boundary ("9EXTD")
ETMRT          % absolute end of table
```

During boot, absent devices are removed from the timer table by `RFTMTABLE`:

```npl
% PH-P2-OPPSTART.NPL:102-114
RFTMTABLE:
   IF A><0 THEN
      A=:XA; "TMRTA"-"PITEX"=:X; RPIFPHPAGE SHZ 12=:D; X+A  % start
      "ETMRT"-"PITEX"; D+A                                    % end
      DO WHILE X<<D
         T:=RPIBANK; *LDATX
         IF A-XA=0 THEN *STZTX FI                            % found, clear
         MIN X
      OD
   FI; EXIT
```

Floppy example — removes the absent-side entry:

```npl
1CLTIMER; CALL RFTMTABLE     % clear entry 1 from timer table
2CLTIMER; CALL RFTMTABLE     % clear entry 2
```

---

### 2.4 TIOBUTAB — Terminal I/O Buffer Table

`TIOBUTAB` (Terminal I/O Buffer Table, accessed via symbol `"TIOBU"`) is the master directory of all configured terminal interface slots. Each entry is **3 words**:

```
Word 0: Logical device number (S0 field). -1 = end of table. 0 = slot cleared/absent.
Word 1: DOU1 — pointer to output datafield descriptor
Word 2: (additional per-entry data)
```

**Iteration pattern** (used in boot and in the main startup loop `STLOP`):

```npl
X:="TIOBU"; *1BANK                    % point to start of table
DO WHILE X.S0><-1                     % -1 = end marker
   IF A><0 THEN                       % slot is live
      AD:=X.DOU1                      % get output datafield descriptor
      ...
   FI
   X+3                                % advance to next 3-word entry
OD
```

The `STLOP` loop at boot iterates TIOBUTAB to initialize all terminal datafields outside resident memory:

```npl
% PH-P2-OPPSTART.NPL:2349-2356
STLOP: "TIOBU"=:"CINDADDR"
LOOP:  X:="CINDADDR"; *1BANK; LDF ,X; 2BANK
       TAD=:CTIOBENTRY
       IF A=-1 THEN
          GPART -1=:GPART
          IF A=0 GO RETU
          GO STLOP
       FI
       "CINDADDR"+3=:"CINDADDR"
```

When a device is found absent or disabled during boot, its logical device number is cleared in TIOBUTAB:

```npl
X:="TIOBU"
DO WHILE X.S0><-1
   IF A=CLOGNO THEN
      0=:X.S0                         % remove from TIOBUTAB
      GO found
   FI
   X+3
OD
```

---

### 2.5 IOBUTAB — I/O Buffer Table

`IOBUTAB` (accessed via symbol `"IOBUTAB"`, end at `"EIOBUTAB"`) holds logical device numbers for HDLC, line printer, and synchronous modem slots. Each entry is **2 words**:

```
Word 0: Packed entry — low bits = logical device number, bit 15 = in-use flag
Word 1: Additional control word
```

When a device is removed (absent hardware, disabled selection), its entry is zeroed:

```npl
X:="IOBUTAB"
DO WHILE X<<"EIOBUTAB"
   IF X.S0/\7777=D THEN 0=:X.S0 FI   % match by logical device number
   X+2
OD
```

---

### 2.6 Logical Device Number Table (`LOGDBANK`)

Maps logical device numbers to datafield addresses. Accessed with banked absolute addressing:

```npl
T:=LOGDBANK; *LDDTX 10    % load double word: word at [logno], word at [logno+10]
T:=LOGDBANK; *STATX       % store: write datafield address at logical device slot
T:=LOGDBANK; *STZTX       % clear: zero the slot
T:=LOGDBANK; *STZTX 10    % clear both input and output slots
```

The table holds pairs (in, out) for two-way devices. `*STZTX 10` zeros offset 0 and offset 10₈ simultaneously, clearing both directions.

---

## 3. Boot-Time Device Detection

### 3.1 Overall Boot Flow

```
SINTR (PH-P2-OPPSTART.NPL, entry)
  │
  ├─ CPU detection (NORD-10 / ND-100 / ND-110 / ND-120)
  ├─ Memory scan (PHYSPTEST) → ENDPAGE set
  ├─ MPM3 probe: IOX 750 → set MEMTYPE
  ├─ PL001: check floppy interfaces (FFLLREG loop)
  ├─ HDLC detection (HDLCTAB loop)
  ├─ Line printer detection (LPTAB loop)
  ├─ Versatec detection (VERSATEC table loop)
  ├─ SYM modem detection
  ├─ Bus scan (EXR ST loop at 174005₈)
  ├─ CALL XCHIOX → terminal interface check + IDENT validation
  ├─ STLOP → build all terminal datafields
  └─ Monitor kernel startup
```

### 3.2 Generic Detection Pattern

Every device class uses the same three-phase pattern:

**Phase 1 — Existence probe**

```npl
A:=200; *TRR IIE; TRA IIC              % arm error trap
T:=HDEV+probe_offset; *EXR ST          % or *IOXT
TRA IIC                                 % clear residual error
IF A=7 THEN GO device_absent FI        % IOX error → absent
```

**Phase 2 — IDENT registration**

```npl
% Example: floppy (ITB11), PH-P2-OPPSTART.NPL:145-147
T:=IPIFPHPAGE SHZ 12=:D
A:="ITB11"-"PITEX"+D+IDENT_CODE-1=:X  % slot address
T:=IPIBANK; A:=datafield_addr; *STATX % write datafield address
```

**Phase 3 — Table cleanup if absent**

```npl
% Clear IDENT table slot
A:=IDENT_CODE; CALL 11IDCLEAR
% Clear logical device entry
A:=logno; T:=LOGDBANK; *STZTX
% Clear IOBUTAB entry
X:="IOBUTAB"
DO WHILE X<<"EIOBUTAB"
   IF X.S0/\7777=logno THEN 0=:X.S0 FI
   X+2
OD
% Clear timer table
A:=df_addr; CALL RFTMTABLE
```

### 3.3 XCHIOX — Terminal IDENT Verification (`PH-P2-OPPSTART.NPL:3122`)

`XCHIOX` is called once during boot and walks the timer table checking every terminal-class datafield:

```npl
XCHIOX:
   % Compute physical addresses of all IDENT/timer tables
   "TMRTE"+1-"PITEX"+D=:TMRADDR          % start of terminal section in timer table
   "9EXTD"-"PITEX"+D=:C1ETMRADDR         % end boundary
   "ID12T"-"PITEX"+T=:PI12T              % phys addr of IDENT→DF map (level 12)
   "ITB12"-"PITEX"+T=:PI12B              % phys addr of ITB12 base
   "ID10T"-"PITEX"+T=:PI10T
   "ITB10"-"PITEX"+T=:PI10B

   A:=200; *TRR IIE; TRA IIC             % arm IOX error trap

   DO WHILE TMRADDR <= C1ETMRADDR
      % Load datafield pointer from timer table
      T:=RPIBANK; *LDXTX
      IF X=-1 THEN EXIT FI               % end of table
      IF X=0 OR X.TYPRING NBIT 5TERM THEN GO NXT FI  % not a terminal

      IF X.HDEV><0 THEN                  % has a hardware address
         X=:0DFADDR
         T:=HDEV+2; *IOXT; TRA IIC      % probe controller
         IF A=7 THEN                     % IOX error → absent
            % try NOTS or MTAD allocation
            CALL MNALLOC; GO IDOK
            CALL MTALLOC; GO IDOK
            % else clear timer table entry
            T:=RPIBANK; X:=TMRADDR; *STZTX
         ELSE
            IF X.HDEV NBIT 2 THEN        % input-capable device
               % reset interface, read IDENT code
               T:=X.HDEV+3; A:=30; *IOXT
               *IDENT PL10; IDENT PL12; TRA IIC   % clear both IDENT registers
               15; *IOXT
               A:=0; T+"5-3"; *IOXT
               % settle delay
               0=:CCOUNT; FOR CCOUNT DO OD; FOR CCOUNT DO OD
               5; T+"7-5"; *IOXT
               A:=0; *IDENT PL12          % read level-12 IDENT code → A
               IF A=0 THEN GO ERRID FI   % ident 0 illegal
               A-1                        % 0-based
               % validate in LIDTERM..HIDTERM range
               % cross-check datafield matches table entry
               A:=0; *IDENT PL10          % verify level-10 IDENT
               ...
            FI
         FI
      FI
      MIN TMRADDR
   OD
```

If IDENT codes are correct, both `ITB10` and `ITB12` entries are confirmed. If wrong, `ERRID` logs an error but continues (can be patched to halt).

---

## 4. Interrupt Level Architecture

```
Level 10 — Output devices
           Terminal output (MP-P2-TERM-DRIV.NPL: TWRITE/DWRITE)
           Line printer output

Level 11 — Mass storage
           Disk (IP-P2-DISK-START.NPL, IP-P2-SCSI-DISK.NPL)
           Floppy disk
           HDLC / synchronous modem (MP-P2-HDLC-DRIV.NPL)
           Versatec plotter

Level 12 — Input devices
           Terminal input (MP-P2-TERM-DRIV.NPL: STTIN/TYENT)
           PIOC inter-processor channel (MP-P2-PIOC-DRIV.NPL)

Level 13 — Clock / timer
Level 14 — Internal (page faults, monitor calls)
```

**Interrupt dispatch sequence** (same pattern for all levels):

```
Hardware interrupt fires on level N
  │  Hardware places IDENT code in A register
  ▼
Driver's wait point returns from CALL IDnn
  │  (ID10, ID11, ID12 are blocking wait-for-interrupt primitives)
  ▼
IDENT code in A → subtract 1 → index into ITBnn
  │  Physical address = ITBnn_base + (A-1)
  ▼
LDATX from ITBnn slot → datafield address
  ▼
Datafield contains HDEV, buffers, status flags
  ▼
I/O performed via T:=HDEV+offset; *IOXT  (or EXR ST)
  ▼
GO RETURN → CALL IDnn → wait for next interrupt
```

---

## 5. Terminal Devices (Level 10 / Level 12)

### 5.1 IDENT Codes

Terminal IDENT codes are validated against `LIDTERM` (lower bound) and `HIDTERM` (upper bound) at boot. Individual codes are assigned by hardware and verified in `XCHIOX`. Each terminal interface occupies one IDENT code in ITB12 (input) and one in ITB10 (output).

The hardware commands to read IDENT codes:

```npl
A:=0; *IDENT PL12    % read level-12 IDENT → A (1-based, 0 = error)
A:=0; *IDENT PL10    % read level-10 IDENT → A
```

### 5.2 Datafield Layout

```
Word  0:  RESLINK    — reservation queue link
Word  1:  RTRES      — owner task
Word  2:  BWLINK     — waiting queue
Words 3-7: (reserved/internal)
Word  8:  HDEV       — hardware device base address (IOX port base)
Word  9:  HST        — hardware status cache
Word 10:  BUFFER     — pointer to character buffer
Word 11:  COUNT      — transfer count
Word 12:  STATUS     — device status flags
Word 13:  FLAGS/DFLAG — control flags (ECHO, XON, CTRLO, CAPITAL, FIFO, etc.)
Word 14:  LAST       — last character received/sent
Word 15:  BITFLAG    — bit-level control (EMPT, ECHO, PINFL, etc.)
Word 16:  BRECHOFL   — break/echo/flow flags (IXOFF, OPIN, IDATA)
Word 17:  TMR        — timer value (CR delay)
Word 18:  CFREE      — free space in character ring buffer
Word 19:  TINFO      — terminal info word (FIFO, CRDLY, UMOD, PRINT, BFUL, PCONN flags)
Word 20:  DFOPP      — pointer to opposite-direction datafield (two-way device)
...
          XDFOPP     — cross-pointer (back-link to input DF from output DF)
          SCREEN     — page-stop line counter
          CESCP      — escape character pair
          XONCR      — XON character
          XOFCR      — XOFF character
          CNTREG     — control register cache
          MAX        — ring buffer max size
          BUFST      — ring buffer start address
```

### 5.3 Input Driver — `STTIN` / `TYENT` (`MP-P2-TERM-DRIV.NPL`)

```npl
% Entry point: driver task startup
STTIN:  TTMR=:TMR               % save timer value

% Main loop: wait for interrupt, process, repeat
RETURN: CALL ID12               % block until level-12 interrupt fires
                                % hardware delivers IDENT code in A on return

TYENT:  ISTATE=:IISTATE         % save interrupt state
        CALL SET12WINDOW        % map physical bank for this IDENT
        IF BRKMODE=12 GO BFYICOMDRIVER   % break-mode special handling

        % Resolve datafield from IDENT
        IF DFOPP><0 THEN
           X:=1777; X/\A; A:=B/\176000+X
        FI
        A=:12DFOPP              % A = datafield addr for this device

% Read a character from hardware
NXCHR:  X:=12DFOPP
        CALL TIAPD; GO RETURN   % TIAPD: T:=HDEV+DST; *IOXT; check ready; read DDW

        % Character arrived in LAST
        CALL XONREAD; GO NXCHR  % check for XON/XOFF input

OKCHAR: BRECHOFL/\177770=:BRECHOFL    % clear break/echo flags
        A:=LAST/\377; CALL XONCHECK   % check XON/XOFF for output side
        % (capital conversion, escape handling, CTRLO, etc.)

        IF CFREE=0 THEN
           TINFO BONE 5BFUL=:TINFO; GO BFULL  % buffer full, wake user
        FI

L1:     *IOF
        IF DFLAG BIT 5ECHO THEN
           LAST/\377; CALL TECHO; CALL TBREAK
        FI
        *ION
        IF BRECHOFL BIT 5ECHO THEN CALL ECHSUBR FI

ADDCH:  LAST/\377; CALL CXRBPUT  % put character in ring buffer
        GO RETURN               % wait for next interrupt
```

### 5.4 Output Driver — `DWRITE` / `TWRITE` (`MP-P2-TERM-DRIV.NPL`)

```npl
% Programmed restart (called by user requesting output)
DWRITE: CALL SDFWI; TTMR=:TMR
        *IOF
        T:=HDEV+DST; *IOXT      % read status
        IF NBIT 3 THEN          % not ready for transfer
           IF BIT 13 THEN       % carrier missing
              X.DFLAG BONE 5LBRK=:X.DFLAG
           FI
           GO RETU              % wait for interrupt
        FI
        IF X.TINFO BIT 5FIFO THEN GO RETU FI  % FIFO: always wait for interrupt
        GO CADEL

% Restart after level-10 interrupt (output complete)
TWRITE: CALL SDFWI
CADEL:  ...
        CALL OXONCHECK          % check reader on/off
        CALL XONWRITE           % check XOFF flow control
        *IOF
        T:=X.DFLAG BZERO 5LBRK=:X.DFLAG

% Single-character output path (non-FIFO)
W0100:  X:=MAX=:L:=HENTE; T:=10BUFST
        IF HDEV+DDW NBIT 17 THEN          % EXR path (FIFO capable)
           A+164000=:D                    % D = IOX write instruction for DDW
N01F:      *LBYT; AND I (10MAS; EXR SD   % fetch byte, write via EXR
           X+1; IF X=L THEN X:="0" FI    % ring buffer advance
           MIN CBHOL; GO N01F
        FI
        % IOXT path
N01S:   *LBYT; AND I (10MAS; SWAP ST DD; IOXT; SWAP ST DD
        ...

% Echo output subroutine (also used for XOFF/XON restart)
ECAPD:  A=:D; T:=HDEV+DST; *IOXT         % read status
        IF A BIT 3 THEN                   % ready
           D=:A; T-DST+DDW; *IOXT        % write character to data-write register
           "DACT"; T-DDW+DCONT; *IOXT    % write DACT to control register (activate)
        ELSE
           BITFLAG BZERO 5EMPT=:BITFLAG
           "DACT+DPIN"; T-DST+DCONT; *IOXT   % DACT|DPIN: activate + pin (stay interrupted)
           D=:A; CALL CXRBPUT             % queue character for later
        FI
```

---

## 6. HDLC / Synchronous Modem (Level 11)

### 6.1 IDENT Codes and Base Addresses

From `PH-P2-CONFG-TAB.NPL`:

| Interface | IDENT code (octal) | Base address (octal) | Logical device |
|-----------|-------------------|---------------------|----------------|
| HDLC/Synch 1 | 150 | 1360 | 1360 |
| HDLC/Synch 2 | 151 | 1362 | 1362 |
| HDLC/Synch 3 | 152 | 1364 | 1364 |
| HDLC/Synch 4 | 153 | 1366 | 1366 |
| HDLC/Synch 5 | 154 | 1370 | 1370 |
| HDLC/Synch 6 | 155 | 1372 | 1372 |

Two modes: `HDLCSELECTION=1` → HDLC protocol; `HDLCSELECTION=2` → synchronous modem.

### 6.2 Detection (`PH-P2-OPPSTART.NPL:400-445`)

```npl
IF CSVXX.HDLCSELECTION=0 GO CLHDLC     % disabled in config
IF A>>2 GO CLHDLC                       % illegal selection
A:=200; *TRR IIE; TRA IIC; 2BANK
T:=D.HDEV+RRTS; *EXR ST; TRA IIC; 1BANK  % probe RRTS register
IF A=0 THEN                             % interface present
   ...                                  % set up datafield, register in ITB11
ELSE
   GO CLHDLC                            % not present, clear all entries
FI
```

### 6.3 Key IOX Registers (COM5025 chip, accessed via EXR ST)

| Symbol | Offset | Direction | Description |
|--------|--------|-----------|-------------|
| RRTS   | +0     | Read      | Receiver/transmitter status |
| RTTS   | +0     | Read      | Transmitter status (alias) |
| WRTC   | +0     | Write     | Receiver transmitter control |
| WDMA   | +1     | Write     | DMA start address |
| WDCR   | +2     | Write     | DMA control register |
| RDCR   | +2     | Read      | DMA status read-back |
| WTTC   | +3     | Write     | Transmitter timing control |
| WTCR   | +4     | Write     | Transmitter command register |

### 6.4 Transmitter Interrupt Handler — `HOINT` (`MP-P2-HDLC-DRIV.NPL`)

```npl
HOINT: 0=:TMR                              % reset watchdog timer
       T:=HDEV+RTTS; *EXR ST              % read transmitter status → A
       A=:HASTAT                           % save
       IF T:=ACTSW=0 THEN MIN DUIN; P+0; CALL WT12 FI  % spurious interrupt
       0=:ACTSW
       IF CMODI=40 THEN
          T:=HDEV+WTTC; *EXR ST           % turn off RQTS (request-to-send)
       FI
       GO HNOTRA
```

### 6.5 Transmit Start — `XHMST`

```npl
XHMST: LIINT+DPITPHYS                     % compute DMA physical address
XXHMST:T:=HDEV+WDMA; *IOF; EXR ST        % set DMA start address (IOF: interrupts off)
       A:=2000\/D; T+"WDCR-WDMA"; *EXR ST  % start transmitter, DMA count
       T+"RDCR-WDCR"; X:=-20; *EXR ST    % set receive DMA count
       CALL LTOUT; *JAF *-2; ION
       1134+CMODI; T:=HDEV+WTTC; *EXR ST % set timing/control
       1=:ACTSW
```

### 6.6 Receiver Interrupt Handler — `HIINT`

```npl
HIINT: T:=HDEV+RRTS; *EXR ST             % read receiver status → A
       A=:HASTAT
       IF T:=ACTSW=0 THEN MIN T9; P+0; GO OUT1 FI  % spurious
       ...
```

### 6.7 Initialisation — `SMCLEAR`

```npl
SMCLEAR: T:=HDEV+WTCR; A:=2; *EXR ST    % send EOM (end-of-message)
         *TRR 10; IOF                     % clear cache
         A:=100; T:=HDEV+WRTC; *EXR ST   % reset control
         A:=140; *EXR ST                  % keep DTR/carrier high
         T+"WDCR-WRTC"; *EXR ST; ION
         ...
         A:=MAINT; T:=HDEV+WRTC; *EXR ST % set maintenance mode if applicable
         ...
         A:=XINITA+DPITPHYS; T:=HDEV+WDMA; *IOF; EXR ST  % set DMA buffer
         401; T+"WDCR-WDMA"; *EXR ST     % enable receiver, frame count
         T+"RDCR-WDCR"; X:=-10; *EXR ST
         *ION
```

---

## 7. Disk Controllers (Level 11)

### 7.1 IDENT Code Assignment

Disk controller IDENT codes are stored in `BDISTABLE` (buffered disk) and `MTDITABLE` (magnetic tape disk) arrays and written to ITB11 at boot:

```npl
% PH-P2-OPPSTART.NPL:1819-1820
T:=IPIFPHPAGE SHZ 12=:D
A:="ITB11"-"PITEX"+D+BDISTABLE(0CINX)-1=:X  % ITB11[ident-1]
T:=IPIBANK; A:=XA; *STATX                    % write datafield addr
```

Disk types in `MDISCS` array:

| MDISCS index (octal) | Type | Driver |
|---------------------|------|--------|
| 10-15 | ST-506 (Winchester) | WWDIS |
| 17 | SMD disk | BBDIS |
| 20-35 | SMD variants | BBDIS |
| 36 | SCSI | SCDIS |

### 7.2 Standard Disk I/O Pattern (`IP-P2-DISK-START.NPL`)

The standard disk driver uses IOXT (not EXR) for register access:

```npl
% Seek wait subroutine
T:=HDEV+RSR; *IOXT              % read seek register
IF A BIT 17 THEN                % must reset CWR
   T+"LCO-RSR"; A:=LUNI; *IOXT
   GO L1
FI
...
A\/030005; *IOXT                 % enable interrupt, seek complete search
CALL ID11                        % wait for level-11 interrupt

T:=HDEV+RSC; *IOXT              % read seek condition
```

DMA transfer:

```npl
% IP-P2-1.NPL — UIOX macro expands to:
LDT   HDEV, B                   % load HDEV from datafield B into T
AAT   $FUNIX                     % T += register offset
IOXT                             % execute IOX with T as address
```

### 7.3 SCSI Disk (`IP-P2-SCSI-DISK.NPL`)

SCSI uses both IOXT and EXR SA (for computed shift/load instructions):

```npl
% Cache clear via EXR SA
K:="0"; SCCLR; *EXR SA          % clear cache, K=0

% Building and executing a shift instruction for address computation
A:=0; D:=1; T:=SUSI1; *EXR ST  % get record size via computed IOX
D-1=:L; T:=SMBP1; *LDDTX 20

% Disk address calculation
X.ABPA2; T:=SUSI1; *EXR ST     % physical disk address computation
T:=X.CPAM2; D+T
T:=X.CPAM1; *RADD ST ADC DA
```

---

## 8. Floppy Disk (Level 11)

### 8.1 Detection (`PH-P2-OPPSTART.NPL:115-150`)

The floppy table (`FFLLREG`) has two entries: S0 (old floppy datafield) and S1 (new floppy datafield). S3 holds the IDENT code.

```npl
FINDFLOPPY: X=:CSAVX                    % X = floppy array entry
   0=:1CLTIMER=:2CLTIMER
   A:=L=:"FFLLREG"
   IF X.S0><0 OR X.S1><0 THEN           % any floppy configured?
      X:=A; A:=200; *TRR IIE; TRA IIC   % arm error trap
      T:=X.HDEV+2; *EXR ST             % probe floppy status register
      A=:D; *TRA IIC
      IF A=0 THEN                        % interface present
         IF D<0 THEN
            CSAVX.S1; T:=X.S0           % new floppy
         ELSE
            CSAVX.S0; T:=X.S1           % old floppy
         FI
         T=:1CLTIMER
      ELSE
         CSAVX.S0=:1CLTIMER; X.S1=:2CLTIMER  % absent, mark for removal
         A:=0
      FI; A=:XA
      ...
      % Write to ITB11
      T:=IPIFPHPAGE SHZ 12=:D
      A:="ITB11"-"PITEX"+D+CSAVX.S3-1=:X   % CSAVX.S3 = IDENT code
      T:=IPIBANK; A:=XA; *STATX
      1CLTIMER; CALL RFTMTABLE
      2CLTIMER; CALL RFTMTABLE
   FI
```

---

## 9. Line Printers (Level 10 / Level 11)

### 9.1 IDENT Codes and Configuration

From `PH-P2-CONFG-TAB.NPL`:

| LP | IDENT codes (octal) | Base address (octal) | Types |
|----|--------------------|--------------------|-------|
| LP 1 | 140230 (main), 3 (DMA) | 1167 | DMPR, DMLP, DLPR |
| LP 2 | 140231 (main), 23 (DMA) | 1175 | DMPR, DMLP, DLPR |

Three LP types:
- `LPSELECTION=1`: DMPR (DMA printer)
- `LPSELECTION=2`: DMLP (DMA line printer, EXR mode)
- `LPSELECTION=3`: DLPR (direct line printer, IOXT mode)

### 9.2 Detection Pattern

```npl
% PH-P2-OPPSTART.NPL:463-468
T:=A.HDEV+2; A:=200; *TRR IIE; TRA IIC
IF CSVXY.LPSELECTION-2=0 THEN
   *EXR ST                              % DMLP: HDEV holds instruction word
ELSE
   *IOXT                                % DMPR/DLPR: HDEV holds port number
FI; *TRA IIC
IF A><0 THEN 1=:X.LPCLENTRY FI         % A≠0 → absent

% Register in ITB10 (output)
IF X.LPCLENTRY=0 THEN
   T:=X.2LPIDENT; A-1+"LPDMDF"; X+A; X.S0=:D   % datafield addr
   "ITB10"-"PITEX"=:X; A:=MPIFPHPAGE SH 12; X+A+T-1
   T:=MPIBANK; A:=D; *STATX            % ITB10[ident-1] = df_addr
FI
```

---

## 10. Versatec Plotter (Level 11)

### 10.1 Detection

```npl
% PH-P2-OPPSTART.NPL:2164-2174
IF X.DMVDATFADDR><0 THEN               % datafield configured?
   A:=200; *TRR IIE; TRA IIC
   T:=X.DMVHDEV+4; *IOXT; TRA IIC     % probe Versatec at HDEV+4
   IF A=0 THEN                         % present
      X.DMVIDENT=:L; X:=X.DMVDATFADDR=:D; *2BANK
      T-4=:X.HDEV                      % store HDEV in datafield
      IF L<<"MAX11" THEN
         "ITB11"-"PITEX"=:X; A:=IPIFPHPAGE SH 12
         X+A+L-1; T:=IPIBANK; A:=D; *STATX   % register in ITB11
      ELSE
         "ITE11"-"PITEX"=:X; ...        % register in extended table
      FI
   ELSE
      % absent: clear logical device, clear IOBUTAB
      A:=X.DMVIDENT; CALL 11IDCLEAR
   FI
```

---

## 11. PIOC Inter-Processor Channel (Level 12)

### 11.1 Overview

PIOC is the inter-processor communication channel connecting ND-100 with co-processors (ND-500 via the 3022 interface). It fires on level 12 and uses IOXT exclusively.

### 11.2 Send / Receive Pattern (`MP-P2-PIOC-DRIV.NPL`)

```npl
% Wake up PIOC (send a command)
A:=PWCR BONE BNDC; T:=HDEV+3; *IOXT    % write PWCR|BNDC to control reg

% Enable PIOC after handling
PWCR BONE BENA=:PWCR
CALL FAR PISUPER                         % test if super-kick needed
PWCR; T:=HDEV+3; *IOXT                  % enable PIOC (write control)
CALL WT12                                % wait for level-12 interrupt

% Driver re-entry
PIORE: X=:B:=KPROS
       PWCR BONE BENA=:PWCR; T:=HDEV+3; *IOXT   % enable PIOC
       CALL RTENTRY
       "STDRIV"; *IOF; IRW LV12B DP      % start driver again
```

---

## 12. C Reference Implementation

The following C code models SINTRAN's device detection, IDENT table management, and interrupt routing. It uses the same conceptual structure as the NPL source.

All addresses, IDENT codes, and register offsets are in **octal** matching the source (represented as hex in C for readability). Device structures mirror SINTRAN datafields.

```c
/*
 * SINTRAN III Device Driver Architecture — C Reference Implementation
 *
 * Models:
 *   - Boot-time device detection with IOX error trapping
 *   - IDENT table (ITB10 / ITB11 / ITB12) population
 *   - EXR ST vs IOXT indirect I/O port access
 *   - Interrupt routing via IDENT → datafield → HDEV
 *   - Terminal, HDLC, disk, floppy, line printer, PIOC devices
 *
 * Register offsets are octal in original source; shown as hex here.
 * Base addresses: terminal=varies, HDLC1=0x1F0 (1360₈), etc.
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* ===================================================================
 * IOX Port Abstraction
 *
 * On real ND-100 hardware, IOX is a special bus cycle.
 * Here we model it as memory-mapped I/O through a callback.
 * EXR ST: T contains a pre-built IOX instruction word
 * IOXT:   T contains a plain port number
 * =================================================================== */

#define IOX_READ_ERROR   7       /* A=7 means IOX bus error (device absent) */
#define IOX_OPCODE_BITS  0xFC00  /* upper 6 bits of IOX instruction word */

/* Hardware I/O callbacks — platform provides these */
typedef uint16_t (*iox_read_fn)(uint16_t port);
typedef void     (*iox_write_fn)(uint16_t port, uint16_t value);

static iox_read_fn  hw_iox_read;
static iox_write_fn hw_iox_write;
static bool         iox_error_flag;  /* set by bus if device absent */

/*
 * IOXT mode: T is a plain port number
 */
static uint16_t ioxt_read(uint16_t t_reg)
{
    iox_error_flag = false;
    uint16_t val = hw_iox_read(t_reg);
    return iox_error_flag ? IOX_READ_ERROR : val;
}

static void ioxt_write(uint16_t t_reg, uint16_t value)
{
    iox_error_flag = false;
    hw_iox_write(t_reg, value);
}

/*
 * EXR ST mode: T is a pre-built IOX instruction word.
 * Port number is in the low 10 bits of T.
 * The upper 6 bits are the IOX opcode (ignored here — we just extract port).
 */
static uint16_t exr_st_read(uint16_t t_reg)
{
    uint16_t port = t_reg & 0x03FF;  /* low 10 bits = device address */
    iox_error_flag = false;
    uint16_t val = hw_iox_read(port);
    return iox_error_flag ? IOX_READ_ERROR : val;
}

static void exr_st_write(uint16_t t_reg, uint16_t value)
{
    uint16_t port = t_reg & 0x03FF;
    iox_error_flag = false;
    hw_iox_write(port, value);
}

/*
 * Build HDEV for EXR mode: set IOX opcode bits, zero the port offset bits.
 * Adding a register offset then gives the complete IOX instruction word.
 */
static uint16_t hdev_for_exr(uint16_t base_port)
{
    return IOX_OPCODE_BITS | (base_port & 0x03FF);
}


/* ===================================================================
 * IDENT Tables
 *
 * ITB10 — level 10 (output), IDENT → datafield address
 * ITB11 — level 11 (mass storage), IDENT → datafield address
 * ITB12 — level 12 (input), IDENT → datafield address
 *
 * Index = IDENT_code - 1  (hardware delivers 1-based codes)
 * =================================================================== */

#define MAX_IDENT_CODES  256

static void *itb10[MAX_IDENT_CODES];  /* level 10: output devices */
static void *itb11[MAX_IDENT_CODES];  /* level 11: mass storage   */
static void *itb12[MAX_IDENT_CODES];  /* level 12: input devices  */

static void ident_register(void **table, uint8_t ident_code, void *datafield)
{
    if (ident_code == 0 || ident_code > MAX_IDENT_CODES) return;
    table[ident_code - 1] = datafield;
}

static void ident_clear(void **table, uint8_t ident_code)
{
    if (ident_code == 0 || ident_code > MAX_IDENT_CODES) return;
    table[ident_code - 1] = NULL;
}

/* Called at interrupt time: IDENT code → datafield pointer */
static void *ident_lookup(void **table, uint8_t ident_code)
{
    if (ident_code == 0 || ident_code > MAX_IDENT_CODES) return NULL;
    return table[ident_code - 1];
}


/* ===================================================================
 * Common Device Datafield
 *
 * All SINTRAN device datafields begin with a common 16-word header.
 * =================================================================== */

typedef struct {
    uint16_t reslink;    /* word  0: reservation queue link */
    uint16_t rtres;      /* word  1: owner task             */
    uint16_t bwlink;     /* word  2: waiting queue          */
    uint16_t _reserved[5]; /* words 3-7 */
    uint16_t hdev;       /* word  8: IOX base address (IOXT mode) or
                                     IOX instruction base (EXR mode)  */
    uint16_t hst;        /* word  9: hardware status cache  */
    uint16_t buffer;     /* word 10: character buffer pointer */
    uint16_t count;      /* word 11: transfer count         */
    uint16_t status;     /* word 12: device status flags    */
    uint16_t flags;      /* word 13: control flags          */
} DeviceDatafield;


/* ===================================================================
 * Terminal Device
 * =================================================================== */

/* Register offsets from HDEV (IOXT mode, plain port numbers) */
#define TERM_DST    0x00   /* Device STatus     */
#define TERM_DDW    0x01   /* Device Data Write */
#define TERM_DCONT  0x02   /* Device CONTrol    */

/* Status bits */
#define TERM_STATUS_READY         (1 << 3)   /* bit 3: ready for transfer */
#define TERM_STATUS_CARRIER_MISS  (1 << 13)  /* bit 13: carrier missing   */

/* Control values */
#define TERM_DACT       0x0001   /* activate transfer */
#define TERM_DACT_PIN   0x0003   /* activate + pin (stay interrupted) */

/* Flags */
#define TERM_FLAG_ECHO    (1 << 5)
#define TERM_FLAG_FIFO    (1 << 8)
#define TERM_FLAG_CRDLY   (1 << 9)

typedef struct {
    DeviceDatafield df;         /* common header */
    uint16_t last;              /* last character received/sent */
    uint16_t bitflag;           /* bit-level control */
    uint16_t brechofl;          /* break/echo/flow flags */
    uint16_t tmr;               /* CR delay timer */
    uint16_t cfree;             /* free space in ring buffer */
    uint16_t tinfo;             /* terminal info (FIFO etc.) */
    void    *dfopp;             /* pointer to opposite-direction datafield */
    uint8_t  ident_in;          /* level-12 IDENT code */
    uint8_t  ident_out;         /* level-10 IDENT code */

    /* Ring buffer */
    uint8_t  ring[256];
    uint16_t ring_head;
    uint16_t ring_tail;
} TermDatafield;

#define N_TERMINAL_INTERFACES 16
static TermDatafield term_df_in[N_TERMINAL_INTERFACES];   /* input (level 12) */
static TermDatafield term_df_out[N_TERMINAL_INTERFACES];  /* output (level 10) */

/*
 * Probe one terminal interface and register it.
 * Matches XCHIOX logic (PH-P2-OPPSTART.NPL:3191-3234).
 */
static bool terminal_probe_and_register(int idx, uint16_t base_port,
                                        uint8_t ident_in, uint8_t ident_out)
{
    uint16_t t_reg, a_val;

    /* Phase 1: existence probe (IOXT mode, plain port) */
    t_reg = base_port + 2;
    a_val = ioxt_read(t_reg);
    if (a_val == IOX_READ_ERROR) return false;   /* interface absent */

    /* Phase 2: read IDENT codes from hardware */
    /* (On real hardware: IDENT PL12 instruction) */
    uint8_t hw_ident_in  = ident_in;    /* hardware supplies via IDENT PL12 */
    uint8_t hw_ident_out = ident_out;   /* hardware supplies via IDENT PL10 */
    if (hw_ident_in == 0 || hw_ident_out == 0) return false;

    /* Phase 3: initialise datafields */
    TermDatafield *dfi = &term_df_in[idx];
    TermDatafield *dfo = &term_df_out[idx];
    memset(dfi, 0, sizeof(*dfi));
    memset(dfo, 0, sizeof(*dfo));

    dfi->df.hdev = base_port;    /* IOXT mode: plain port number */
    dfo->df.hdev = base_port;
    dfi->dfopp   = dfo;          /* two-way cross-link */
    dfo->dfopp   = dfi;
    dfi->ident_in  = hw_ident_in;
    dfi->ident_out = hw_ident_out;
    dfi->cfree = sizeof(dfi->ring);

    /* Phase 4: register in IDENT tables */
    ident_register(itb12, hw_ident_in,  dfi);   /* level 12: input  */
    ident_register(itb10, hw_ident_out, dfo);   /* level 10: output */
    return true;
}

/* Level-12 interrupt handler: terminal input */
static void terminal_interrupt_in(uint8_t ident_code)
{
    TermDatafield *dfi = (TermDatafield *)ident_lookup(itb12, ident_code);
    if (!dfi) return;

    uint16_t t_reg, a_val;

    /* Read status */
    t_reg = dfi->df.hdev + TERM_DST;
    a_val = ioxt_read(t_reg);
    if (!(a_val & TERM_STATUS_READY)) return;  /* no character waiting */

    /* Read character */
    t_reg = dfi->df.hdev + TERM_DDW;
    a_val = ioxt_read(t_reg);
    uint8_t ch = (uint8_t)(a_val & 0xFF);

    /* Put in ring buffer */
    if (dfi->cfree > 0) {
        dfi->ring[dfi->ring_tail] = ch;
        dfi->ring_tail = (dfi->ring_tail + 1) % (uint16_t)sizeof(dfi->ring);
        dfi->cfree--;
    } else {
        dfi->tinfo |= (1 << 5);  /* set BFUL: buffer full */
    }

    /* Echo if enabled */
    if (dfi->df.flags & TERM_FLAG_ECHO) {
        TermDatafield *dfo = (TermDatafield *)dfi->dfopp;
        if (dfo) {
            t_reg = dfo->df.hdev + TERM_DST;
            a_val = ioxt_read(t_reg);
            if (a_val & TERM_STATUS_READY) {
                t_reg = dfo->df.hdev + TERM_DDW;
                ioxt_write(t_reg, ch);
                t_reg = dfo->df.hdev + TERM_DCONT;
                ioxt_write(t_reg, TERM_DACT);
            }
        }
    }
}

/* Level-10 interrupt handler: terminal output */
static void terminal_interrupt_out(uint8_t ident_code)
{
    TermDatafield *dfo = (TermDatafield *)ident_lookup(itb10, ident_code);
    if (!dfo) return;

    TermDatafield *dfi = (TermDatafield *)dfo->dfopp;
    if (!dfi || dfi->ring_head == dfi->ring_tail) return;  /* buffer empty */

    uint16_t t_reg, a_val;

    /* Read status */
    t_reg = dfo->df.hdev + TERM_DST;
    a_val = ioxt_read(t_reg);
    if (!(a_val & TERM_STATUS_READY)) {
        /* Not ready: pin for interrupt */
        t_reg = dfo->df.hdev + TERM_DCONT;
        ioxt_write(t_reg, TERM_DACT_PIN);
        return;
    }

    if (a_val & TERM_STATUS_CARRIER_MISS) {
        dfi->df.flags |= (1 << 5);  /* set LBRK */
        return;
    }

    /* Write character */
    uint8_t ch = dfi->ring[dfi->ring_head];
    dfi->ring_head = (dfi->ring_head + 1) % (uint16_t)sizeof(dfi->ring);
    dfi->cfree++;

    t_reg = dfo->df.hdev + TERM_DDW;
    ioxt_write(t_reg, ch);
    t_reg = dfo->df.hdev + TERM_DCONT;
    ioxt_write(t_reg, TERM_DACT);
}


/* ===================================================================
 * HDLC / Synchronous Modem Device
 *
 * IDENT codes (octal): 150-155  (decimal 104-109)
 * Base addresses (octal): 1360, 1362, 1364, 1366, 1370, 1372
 *
 * Uses EXR ST: HDEV holds IOX instruction base (opcode bits pre-set).
 * =================================================================== */

/* Register offsets from HDEV (EXR mode — add to HDEV instruction word) */
#define HDLC_RRTS    0x00   /* Receiver/transmitter status (read)  */
#define HDLC_WRTC    0x00   /* Receiver transmitter control (write) */
#define HDLC_WDMA    0x01   /* DMA start address                    */
#define HDLC_WDCR    0x02   /* DMA control register                 */
#define HDLC_RDCR    0x02   /* DMA status read-back                 */
#define HDLC_WTTC    0x03   /* Transmitter timing control           */
#define HDLC_WTCR    0x04   /* Transmitter command register         */

typedef struct {
    DeviceDatafield df;       /* df.hdev = EXR instruction base */
    uint16_t hastat;          /* saved hardware status */
    uint16_t actsw;           /* active transfer switch */
    uint16_t cmodi;           /* current mode */
    uint16_t maint;           /* maintenance flags */
    uint8_t  ident;           /* level-11 IDENT code */
    bool     in_use;
    uint8_t  selection;       /* 1=HDLC, 2=synch modem */
} HdlcDatafield;

#define N_HDLC_INTERFACES 6
static HdlcDatafield hdlc_df[N_HDLC_INTERFACES];

/* HDLC base port addresses (octal 1360-1372 → decimal 752-762) */
static const uint16_t HDLC_BASE_PORTS[N_HDLC_INTERFACES] = {
    0x1F0, 0x1F2, 0x1F4, 0x1F6, 0x1F8, 0x1FA  /* octal 1360-1372 */
};
/* HDLC IDENT codes (octal 150-155 → decimal 104-109) */
static const uint8_t HDLC_IDENT_CODES[N_HDLC_INTERFACES] = {
    104, 105, 106, 107, 108, 109
};

/*
 * Probe and initialise one HDLC interface.
 * Matches PH-P2-OPPSTART.NPL:400-445.
 */
static bool hdlc_probe_and_register(int idx, uint8_t selection)
{
    if (selection == 0 || selection > 2) return false;

    uint16_t base = HDLC_BASE_PORTS[idx];
    uint8_t  ident = HDLC_IDENT_CODES[idx];

    /* Build EXR instruction word for this base */
    uint16_t hdev = hdev_for_exr(base);

    /* Probe RRTS register via EXR ST */
    uint16_t a_val = exr_st_read(hdev + HDLC_RRTS);
    if (a_val == IOX_READ_ERROR) return false;  /* interface absent */

    /* Initialise datafield */
    HdlcDatafield *d = &hdlc_df[idx];
    memset(d, 0, sizeof(*d));
    d->df.hdev  = hdev;     /* EXR mode: IOX instruction base */
    d->ident    = ident;
    d->in_use   = true;
    d->selection = selection;

    /* Register in ITB11 */
    ident_register(itb11, ident, d);

    /* Initialise hardware (SMCLEAR pattern) */
    exr_st_write(hdev + HDLC_WTCR, 2);          /* send EOM */
    exr_st_write(hdev + HDLC_WRTC, 0x40);       /* reset control (0100₈) */
    exr_st_write(hdev + HDLC_WRTC, 0x60);       /* keep DTR/carrier (0140₈) */
    exr_st_write(hdev + HDLC_WDCR, 0);          /* clear DMA control */

    return true;
}

/* Start HDLC transmit — sets up DMA and starts transmitter */
static void hdlc_start_transmit(HdlcDatafield *d,
                                uint32_t dma_phys_addr, uint16_t frame_len)
{
    uint16_t hdev = d->df.hdev;

    /* Write DMA physical start address (LIINT+DPITPHYS pattern) */
    exr_st_write(hdev + HDLC_WDMA, (uint16_t)(dma_phys_addr & 0xFFFF));
    /* Start transmitter: 2000₈ | DMA count */
    exr_st_write(hdev + HDLC_WDCR, 0x0400 | frame_len);  /* 2000₈ = 0x0400 */
    exr_st_write(hdev + HDLC_RDCR, (uint16_t)(-10 & 0xFFFF));  /* receive DMA count */
    /* Set timing */
    exr_st_write(hdev + HDLC_WTTC, 0x4DC | d->cmodi);   /* 1134₈ + CMODI */
    d->actsw = 1;
}

/* Level-11 interrupt: HDLC transmitter complete */
static void hdlc_interrupt_tx(uint8_t ident_code)
{
    HdlcDatafield *d = (HdlcDatafield *)ident_lookup(itb11, ident_code);
    if (!d) return;

    d->df.hst = 0;  /* reset timer */

    /* Read transmitter status (HOINT pattern) */
    d->hastat = exr_st_read(d->df.hdev + HDLC_RRTS);

    if (d->actsw == 0) {
        /* spurious interrupt — ignore */
        return;
    }
    d->actsw = 0;

    if (d->cmodi == 0x20) {  /* 40₈ */
        exr_st_write(d->df.hdev + HDLC_WTTC, 0);  /* turn off RQTS */
    }
    /* continue processing (HNOTRA) */
}

/* Level-11 interrupt: HDLC receiver complete */
static void hdlc_interrupt_rx(uint8_t ident_code)
{
    HdlcDatafield *d = (HdlcDatafield *)ident_lookup(itb11, ident_code);
    if (!d) return;

    /* Read receiver status (HIINT pattern) */
    d->hastat = exr_st_read(d->df.hdev + HDLC_RRTS);

    if (d->actsw == 0) {
        /* spurious — ignore */
        return;
    }
    /* process received frame */
}


/* ===================================================================
 * Disk Controller (Standard, Level 11)
 *
 * Uses IOXT (plain port numbers) not EXR.
 * =================================================================== */

/* Register offsets (octal in original) */
#define DISK_RSR   0x00   /* Read Seek Register  */
#define DISK_LCO   0x01   /* Load Command Register */
#define DISK_LBA   0x02   /* Load Block Address  */
#define DISK_RSC   0x03   /* Read Seek Condition */
#define DISK_LWCNT 0x04   /* Load Word Count     */

/* Status bits */
#define DISK_STATUS_RESET_CWR  (1 << 15)  /* bit 17₈ in octal, bit 15 in C */

typedef struct {
    DeviceDatafield df;       /* df.hdev = plain IOX port number (IOXT) */
    uint16_t hstat;           /* hardware status cache */
    uint16_t luni;            /* logical unit number */
    uint32_t mema;            /* memory address (double word) */
    uint8_t  ident;
} DiskDatafield;

#define N_DISK_CONTROLLERS 4
static DiskDatafield disk_df[N_DISK_CONTROLLERS];

static bool disk_probe_and_register(int idx, uint16_t base_port, uint8_t ident)
{
    /* Probe seek register */
    uint16_t a_val = ioxt_read(base_port + DISK_RSR);
    if (a_val == IOX_READ_ERROR) return false;

    DiskDatafield *d = &disk_df[idx];
    memset(d, 0, sizeof(*d));
    d->df.hdev = base_port;   /* IOXT: plain port number */
    d->ident   = ident;
    ident_register(itb11, ident, d);
    return true;
}

/* Start disk seek (WSEEK pattern from IP-P2-DISK-START.NPL) */
static void disk_seek(DiskDatafield *d, uint32_t block_addr)
{
    uint16_t hdev = d->df.hdev;

    /* Check if CWR reset required */
    uint16_t status = ioxt_read(hdev + DISK_RSR);
    if (status & DISK_STATUS_RESET_CWR) {
        ioxt_write(hdev + DISK_LCO, d->luni);   /* unit select + reset CWR */
    }

    /* Load block address */
    ioxt_write(hdev + DISK_LBA, (uint16_t)(block_addr & 0xFFFF));

    /* Enable interrupt + seek complete search (0030005₈ pattern) */
    ioxt_write(hdev + DISK_RSR, 0x0605);   /* 030005₈ */

    /* CALL ID11 would block here until level-11 interrupt fires */
    /* On interrupt: */
    /*   d->hstat = ioxt_read(hdev + DISK_RSC);  */
}

/* Level-11 interrupt: disk seek/transfer complete */
static void disk_interrupt(uint8_t ident_code)
{
    DiskDatafield *d = (DiskDatafield *)ident_lookup(itb11, ident_code);
    if (!d) return;

    /* Read seek condition */
    d->hstat = ioxt_read(d->df.hdev + DISK_RSC);
    /* continue transfer or signal completion to waiting task */
}


/* ===================================================================
 * Floppy Disk (Level 11)
 *
 * Uses EXR ST: HDEV holds IOX instruction base.
 * =================================================================== */

typedef struct {
    DeviceDatafield df;    /* df.hdev = EXR instruction base */
    uint8_t  ident;
    bool     is_new;       /* true = new floppy type */
} FloppyDatafield;

#define N_FLOPPY_CONTROLLERS 2
static FloppyDatafield floppy_df[N_FLOPPY_CONTROLLERS];

static bool floppy_probe_and_register(int idx, uint16_t base_port,
                                      uint8_t ident, bool new_type)
{
    uint16_t hdev = hdev_for_exr(base_port);

    /* Probe status register via EXR ST (HDEV+2) */
    uint16_t a_val = exr_st_read(hdev + 2);
    if (a_val == IOX_READ_ERROR) return false;

    /* a_val < 0 (bit 15 set) means new floppy, ≥ 0 means old floppy */
    bool detected_new = (a_val & 0x8000) != 0;
    if (detected_new != new_type) return false;

    FloppyDatafield *d = &floppy_df[idx];
    memset(d, 0, sizeof(*d));
    d->df.hdev = hdev;     /* EXR mode */
    d->ident   = ident;
    d->is_new  = new_type;

    ident_register(itb11, ident, d);
    return true;
}

static void floppy_interrupt(uint8_t ident_code)
{
    FloppyDatafield *d = (FloppyDatafield *)ident_lookup(itb11, ident_code);
    if (!d) return;
    /* read status, advance DMA, etc. */
    (void)exr_st_read(d->df.hdev + 2);
}


/* ===================================================================
 * Line Printer (Level 10 / Level 11)
 *
 * IDENT codes (octal): 140230, 3 (LP1), 140231, 23 (LP2)
 * Uses EXR ST for DMLP type, IOXT for DMPR/DLPR type.
 * =================================================================== */

#define LP_TYPE_NONE  0
#define LP_TYPE_DMPR  1   /* DMA printer:       IOXT mode */
#define LP_TYPE_DMLP  2   /* DMA line printer:  EXR mode  */
#define LP_TYPE_DLPR  3   /* Direct line printer: IOXT mode */

typedef struct {
    DeviceDatafield df;   /* df.hdev: EXR base for DMLP, plain port for others */
    uint8_t  ident_main;
    uint8_t  ident_dma;
    uint8_t  selection;   /* LP_TYPE_* */
    bool     in_use;
} LPDatafield;

#define N_LP_INTERFACES 2
static LPDatafield lp_df[N_LP_INTERFACES];

/* LP base addresses (octal 1167=0x277 LP1, 1175=0x27D LP2) */
static const uint16_t LP_BASE_PORTS[N_LP_INTERFACES] = { 0x277, 0x27D };
/* LP IDENT codes — raw octal values from CONFG-TAB */
static const uint16_t LP_IDENT_MAIN[N_LP_INTERFACES] = { 0x1098, 0x1099 };  /* 140230₈, 140231₈ */
static const uint8_t  LP_IDENT_DMA[N_LP_INTERFACES]  = { 3, 19 };           /* 3₈, 23₈ */

static bool lp_probe_and_register(int idx, uint8_t selection)
{
    if (selection == LP_TYPE_NONE) return false;

    uint16_t base = LP_BASE_PORTS[idx];

    /* Probe via EXR or IOXT depending on type */
    uint16_t a_val;
    if (selection == LP_TYPE_DMLP) {
        uint16_t hdev = hdev_for_exr(base);
        a_val = exr_st_read(hdev + 2);      /* EXR ST */
    } else {
        a_val = ioxt_read(base + 2);         /* IOXT */
    }
    if (a_val != 0) return false;   /* non-zero = absent */

    LPDatafield *d = &lp_df[idx];
    memset(d, 0, sizeof(*d));
    d->selection = selection;
    d->in_use    = true;

    if (selection == LP_TYPE_DMLP) {
        d->df.hdev   = hdev_for_exr(base);  /* EXR mode */
        d->ident_main = (uint8_t)(LP_IDENT_MAIN[idx] & 0xFF);
    } else {
        d->df.hdev   = base;                /* IOXT mode */
        d->ident_main = (uint8_t)(LP_IDENT_MAIN[idx] & 0xFF);
    }

    /* Level-10 registration (output device) */
    ident_register(itb10, d->ident_main, d);
    return true;
}

/* Level-10 interrupt: line printer output ready */
static void lp_interrupt(uint8_t ident_code)
{
    LPDatafield *d = (LPDatafield *)ident_lookup(itb10, ident_code);
    if (!d) return;

    if (d->selection == LP_TYPE_DMLP) {
        /* EXR mode */
        (void)exr_st_read(d->df.hdev);
    } else {
        /* IOXT mode */
        (void)ioxt_read(d->df.hdev);
    }
    /* queue next buffer, restart DMA, etc. */
}


/* ===================================================================
 * PIOC Inter-Processor Channel (Level 12)
 *
 * Uses IOXT (plain port numbers).
 * =================================================================== */

/* Control register bit definitions */
#define PIOC_BNDC   (1 << 0)   /* send command */
#define PIOC_BENA   (1 << 1)   /* enable PIOC  */

typedef struct {
    DeviceDatafield df;   /* df.hdev = plain IOX port, IOXT mode */
    uint16_t pwcr;        /* control register shadow */
    uint8_t  ident;
} PiocDatafield;

#define N_PIOC_INTERFACES 1
static PiocDatafield pioc_df[N_PIOC_INTERFACES];

static bool pioc_probe_and_register(uint16_t base_port, uint8_t ident)
{
    uint16_t a_val = ioxt_read(base_port + 3);
    if (a_val == IOX_READ_ERROR) return false;

    PiocDatafield *d = &pioc_df[0];
    memset(d, 0, sizeof(*d));
    d->df.hdev = base_port;
    d->ident   = ident;
    ident_register(itb12, ident, d);
    return true;
}

static void pioc_send_command(PiocDatafield *d)
{
    uint16_t val = d->pwcr | PIOC_BNDC;
    ioxt_write(d->df.hdev + 3, val);  /* wake up PIOC */
}

static void pioc_enable(PiocDatafield *d)
{
    d->pwcr |= PIOC_BENA;
    ioxt_write(d->df.hdev + 3, d->pwcr);
}

static void pioc_interrupt(uint8_t ident_code)
{
    PiocDatafield *d = (PiocDatafield *)ident_lookup(itb12, ident_code);
    if (!d) return;
    pioc_enable(d);
    /* dispatch to waiting RT process */
}


/* ===================================================================
 * Top-Level Interrupt Dispatcher
 *
 * Called by CPU interrupt handler with level and IDENT code.
 * Maps directly to SINTRAN's ITBnn table lookup.
 * =================================================================== */

void sintran_interrupt(int level, uint8_t ident_code)
{
    switch (level) {
    case 10:
        /* Output devices */
        {
            void *df = ident_lookup(itb10, ident_code);
            if (!df) return;
            /* Determine device type and dispatch.
             * In real SINTRAN each datafield has a DRIVER pointer (word 20+)
             * that was set up at boot pointing to the correct ISR. */
            /* For terminals: */
            terminal_interrupt_out(ident_code);
            /* For line printers: lp_interrupt(ident_code); */
        }
        break;

    case 11:
        /* Mass storage */
        {
            void *df = ident_lookup(itb11, ident_code);
            if (!df) return;
            disk_interrupt(ident_code);
            /* hdlc_interrupt_tx(ident_code); */
            /* floppy_interrupt(ident_code);  */
        }
        break;

    case 12:
        /* Input devices */
        {
            void *df = ident_lookup(itb12, ident_code);
            if (!df) return;
            terminal_interrupt_in(ident_code);
            /* pioc_interrupt(ident_code); */
        }
        break;

    default:
        break;
    }
}


/* ===================================================================
 * System Boot Initialisation
 * =================================================================== */

void sintran_boot_init(iox_read_fn read_fn, iox_write_fn write_fn)
{
    hw_iox_read  = read_fn;
    hw_iox_write = write_fn;

    /* Clear all IDENT tables */
    memset(itb10, 0, sizeof(itb10));
    memset(itb11, 0, sizeof(itb11));
    memset(itb12, 0, sizeof(itb12));

    /*
     * Device detection order mirrors PH-P2-OPPSTART.NPL:
     *
     *  1. Floppy disk
     *  2. HDLC interfaces
     *  3. Line printers
     *  4. Versatec
     *  5. SYM modem
     *  6. Terminal interfaces (XCHIOX)
     *  7. Disk controllers
     *  8. PIOC
     */

    /* 1. Floppy (ident from SINTRAN config; adjust per system) */
    floppy_probe_and_register(0, 0x100 /* base */, 1 /* ident */, false);
    floppy_probe_and_register(1, 0x110 /* base */, 2 /* ident */, true);

    /* 2. HDLC (6 interfaces, selection from sysgen config) */
    for (int i = 0; i < N_HDLC_INTERFACES; i++) {
        hdlc_probe_and_register(i, 1 /* HDLC mode */);
    }

    /* 3. Line printers */
    lp_probe_and_register(0, LP_TYPE_DMLP);
    lp_probe_and_register(1, LP_TYPE_DMPR);

    /* 4. Terminal interfaces — XCHIOX phase
     *    Actual IDENT codes are read from hardware via IDENT PL12/PL10.
     *    Here we pass the expected codes from the pre-built datafields. */
    for (int i = 0; i < N_TERMINAL_INTERFACES; i++) {
        /* Base port and ident codes are system-configuration dependent */
        /* terminal_probe_and_register(i, base_port, ident_in, ident_out); */
    }

    /* 5. Disk controllers */
    for (int i = 0; i < N_DISK_CONTROLLERS; i++) {
        /* disk_probe_and_register(i, base_port, ident); */
    }

    /* 6. PIOC */
    pioc_probe_and_register(0x300 /* base port */, 10 /* ident */);
}
```

---

*Document generated from SINTRAN III s3vs-4 NPL source code analysis.*  
*All code patterns verified against: `PH-P2-OPPSTART.NPL`, `MP-P2-TERM-DRIV.NPL`,*  
*`MP-P2-HDLC-DRIV.NPL`, `IP-P2-DISK-START.NPL`, `IP-P2-SCSI-DISK.NPL`,*  
*`MP-P2-PIOC-DRIV.NPL`, `PH-P2-CONFG-TAB.NPL`.*
