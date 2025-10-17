# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains comprehensive analysis of SINTRAN/NORSK DATA HDLC communication systems, including:

1. **HDLC Controller Analysis**: Detailed examination of hardware register operations and DMA control
2. **Network Traffic Analysis**: Frame-by-frame analysis of actual HDLC communications between machines
3. **SINTRAN Source Code Analysis**: Deep dive into SINTRAN OS interrupt handlers and system calls
4. **Protocol Implementation**: Complete mapping of X.25/LAPB protocols over HDLC hardware
5. **Debugging Documentation**: Systematic analysis of transmission issues and status bit processing

## Files

- **HDLC interrupt 12 handler.txt**: Comprehensive analysis of HDLC level 12 interrupt handler behavior in SINTRAN systems, including:
  - Assembly language execution traces with register states
  - Line-by-line analysis of interrupt handling code
  - C# implementation examples emulating the assembly behavior
  - Hardware device register operations (WTTC, RTTS, DMA control)

- **s3vs-4-L-RONNY.symb**: Comprehensive SINTRAN OS system generation file (92,000+ lines) in NORD PL language containing:
  - Device configurations (SCSI disks, optical drives, streamers, magnetic tapes)
  - RT (Real-Time) process definitions and interrupt table entries
  - System library marks and processor configurations
  - Memory management and I/O control structures
  - Complete SINTRAN operating system components
  - HDLC interrupt handlers (HIINT for receiver, HOINT for transmitter)

- **cpu_documentation.md**: Complete NORSK DATA ND-100/110 CPU instruction set reference:
  - IOX (Input/Output Execute) instruction for device register access
  - EXR (Execute Register) instruction for indirect execution
  - BSET, BSKP bit manipulation instructions for status testing
  - Memory addressing modes and register operations
  - Essential for understanding assembly patterns in .symb files

- **ND-60.047.01_NORD_PL_Users_Guide_August_1973_ocr.pdf**: NORD PL programming language specification:
  - Syntax and semantics of NORD PL language used in .symb files
  - Compilation to MAC assembly language
  - Data types, control structures, and system interfaces
  - Required to understand SINTRAN system generation source code

- **SYMBOL-1-LIST.SYMB.TXT** & **SYMBOL-2-LIST.SYMB.TXT**: SINTRAN constant definitions:
  - Octal constant values for HDLC register bit masks
  - Critical constants: SILFO, TXUND, HX21M, HX21S, EMTY, BLDON
  - Essential for understanding register bit processing logic

## Analysis Documents

### HDLC Controller and Hardware Analysis
- **HDLC_Complete_Register_Analysis.md**: Definitive HDLC register bit usage analysis
- **HDLC_Status_Bit_Analysis.md**: Detailed status bit processing flows
- **HDLC_Hardware_Specification_Analysis.md**: Hardware device specifications and capabilities
- **HDLC_Register_Usage_Analysis.md**: Complete register operation documentation
- **HDLC_Controller_Critical_Bug_Analysis.md**: Root cause analysis of controller issues
- **HDLC_Emulator_Implementation_Guide.md**: Guidelines for emulating HDLC hardware

### Network Traffic Analysis
- **Analyzing_Traffic_100_to_102.md**: Frame-by-frame analysis of X.25 data exchanges
- **Complete_Frame_Analysis_100_to_102.md**: Comprehensive traffic flow documentation
- **trace-conn-100-102.txt**: Raw HDLC traffic logs (machine 100 → 102)
- **trace-conn-102-100.txt**: Raw HDLC traffic logs (machine 102 → 100)
- **trace-with-transmit.txt**: Extended traffic traces with transmission details

### SINTRAN Source Code Analysis
- **Deep_Analysis_of_DMA_Transmit_XSSDATA.md**: Complete analysis of XSSDATA transmission routine
- **Deep_Analysis_of_PROCPKT.md**: Packet processing routine analysis
- **HIINT_Deep_Analysis.md**: Receiver interrupt handler (HIINT) complete flow
- **HOINT_Deep_Analysis.md**: Transmitter interrupt handler (HOINT) complete flow
- **ACTSW_State_Analysis.md**: Activity switch state management analysis
- **PAD_Connection_Analysis.md**: PAD connection establishment and management

### DMA Operations Analysis
- **DMA_Send_Receive_Pseudocode.md**: Complete DMA operation pseudocode
- **DMA_Bits_Detailed_Explanation.md**: DMA control bit meanings and usage
- **DMA_High_Bits_Detailed_Analysis.md**: High-order DMA control bits analysis
- **DMA_Buffer_List_Interrupt_Analysis.md**: Buffer list management in DMA operations
- **DMA_Transmission_Stopping_Analysis.md**: Analysis of transmission halt conditions
- **SINTRAN_DMA_Operations_Diagram.md**: ASCII diagrams of DMA operations

### Critical Analysis and Debugging
- **Critical_Bit_Usage_Analysis.md**: Root cause analysis of packet issues
- **Final_HDLC_Bug_Analysis.md**: Definitive bug analysis and solutions
- **HASTAT_Bit_Processing_Analysis.md**: Hardware status bit processing flows
- **X21_Bits_Detailed_Analysis.md**: X.21 protocol bit explanations
- **HDLC_Constants_Analysis.md**: SINTRAN constant definitions and usage
- **SINTRAN_Variable_Name_Analysis.md**: Variable naming and usage patterns

## Architecture

This is a legacy systems analysis repository documenting NORSK DATA SINTRAN operating system components:

1. **HDLC Protocol Implementation**: Low-level interrupt handling for HDLC data link control with detailed assembly traces
2. **SINTRAN OS System Generation**: Complete operating system build configuration including:
   - RT process definitions with priority levels and memory allocations
   - Interrupt table entries (ITE10, ITE12, ITE13) for different device classes
   - Device driver assignments and I/O control structures
   - System library marks and processor configurations (ND-500 series)
3. **Hardware Register Operations**: Documentation of device I/O operations, DMA control, and memory management
4. **Legacy System Architecture**: NORSK DATA computer assembly language execution patterns and system calls

## SINTRAN Source Code Analysis

This repository contains extensive analysis of SINTRAN operating system source code, specifically focusing on HDLC communication routines:

### Key SINTRAN Routines Analyzed

#### Transmission Routines
- **XSSDATA**: Primary data transmission subroutine that handles user data requests, sets up DMA descriptors with COM5025 control bits, and manages complete transmission lifecycle
- **XSSND**: Special entry point for privileged users and HDLC retransmission operations
- **HOINT**: Output interrupt handler that processes transmission completion interrupts

#### Reception Routines
- **HIINT**: Receiver interrupt handler that processes all incoming HDLC frames with status validation, buffer management, and sophisticated error handling
- **PROCPKT**: Packet processing routine that handles received frame analysis and routing

#### DMA and Buffer Management
- **DMA List Processing**: Complete analysis of how SINTRAN sets up DMA descriptor lists with buffer addresses, control bits, and status flags
- **Buffer State Management**: Analysis of how SINTRAN tracks buffer states (Empty, Full) using LKEY field bits 10-8
- **Circular Buffer Logging**: Implementation of BUFF0-BUFF3 circular buffers for debugging and status tracking

#### State Management
- **ACTSW (Activity Switch)**: State variable that controls HDLC device activity (0=inactive, 1=active)
- **HASTAT**: Hardware status storage variable that maintains last read hardware status from both RRTS and RTTS registers
- **Connection State Tracking**: Analysis of PAD connection establishment and teardown procedures

### SINTRAN Constants and Variables

The analysis has mapped critical SINTRAN constants from SYMBOL-1-LIST.SYMB.TXT and SYMBOL-2-LIST.SYMB.TXT:

```assembly
% Core Status Variables (Memory locations):
HASTA = 000076  % HASTAT - Hardware status storage (16-bit)
ACTSW = 000074  % Activity switch: 0=inactive, 1=active

% Critical Status Bit Constants:
EMTY  = 004000  % 0x0800, bit 11 - List Empty (No Buffers)
HX21M = 060000  % 0x6000, bits 13-14 - X.21 Error Mask
HX21S = 000016  % 0x000E, bits 1,2,3 - Receiver State Check
BLDON = 000010  % 0x0008, bit 3 - Block Done Flag
SILFO = 100000  % 0x8000, bit 15 - Illegal Format
TXUND = 000002  % 0x0001, bit 1 - Transmitter Underrun
```

### Source Code Analysis Methodology

The analysis combines:
1. **Assembly Code Tracing**: Line-by-line execution flow with register state tracking
2. **C# Implementation**: Modern equivalents showing how assembly logic translates to structured code
3. **Status Bit Analysis**: Complete mapping of hardware status bits to SINTRAN processing decisions
4. **Error Condition Handling**: Documentation of all error paths and recovery mechanisms
5. **Interrupt Flow Documentation**: Complete IRQ processing from hardware event to software completion

## Working with These Files

### HDLC Communication Analysis
The repository focuses on analyzing HDLC (High-Level Data Link Control) communication with hardware controllers:

- **HDLC interrupt 12 handler.txt** contains detailed analysis of how SINTRAN communicates with HDLC controllers
- Key HDLC communication patterns documented:
  - DMA address setup via `IOX 15` (WriteTransmitterTransferControl)
  - Command transmission via `IOX 17` with encoded command types
  - Status polling through `AAT 12` (ReadTransmitterStatus) 
  - Error condition handling (BROCK, BLOVF flags)
  - End-of-message (EOM) signaling for half-duplex mode

### File Format Details
- The `.txt` file contains both assembly traces and explanatory C# code
- Register states are shown in brackets: `[A:value D:value T:value L:value X:value B:value]`
- Assembly opcodes are documented with their effective addresses and operations
- The `.symb` file uses SINTRAN configuration syntax with RT process definitions and interrupt tables

### HDLC Hardware Interface
**Key Register Operations:**
- **RRTS (Read Receiver Transfer Status)**: `T:=HDEV+RRTS; *EXR ST` - reads receiver status into A register
- **RTTS (Read Transmitter Status)**: `T:=HDEV+RTTS; *EXR ST` - reads transmitter status into A register  
- **BRRTS/BRTTS**: Banked versions for multi-controller support
- Status stored in `HASTAT` variable for analysis

**Register Offsets:**
- HDEV+10: RRTS (Read Receiver Transfer Status) - based on `RRTS = IOX+10`
- HDEV+12: RTTS (Read Transmitter Transfer Status) 
- HDEV+11: WRTC (Write Receiver Transfer Control)
- HDEV+13: WTTC (Write Transmitter Transfer Control)
- HDEV+15: DMA address register
- HDEV+17: DMA command register

**Status Bit Analysis:**
- **HX21M**: X.21 error mask for detecting protocol errors
- **HX21S**: X.21 clear indication bit  
- **BLDON**: Block done flag
- **EMTY**: Buffer empty condition
- **SILFO+TXUND**: Transmitter underrun flags

**Function codes**: 1=DATA_SEND, 52=INIT, 55=CTRL_BLOCK
**Interrupt levels**: Level 12 for transmitter, Level 13 for receiver

## IRQ Processing Flow

### After HDLC DMA Send (Transmitter IRQ):
```assembly
HOINT: T:=HDEV+RTTS; *EXR ST         % Read Transmitter Status  
       A=:HASTAT                      % Store status in HASTAT variable
       ; Process status bits and handle errors
```

### After HDLC DMA Receive (Receiver IRQ):  
```assembly  
HIINT: T:=HDEV+RRTS; *EXR ST         % Read Receiver Status
       A=:HASTAT                      % Store status in HASTAT variable
       ; Check X.21 errors, buffer conditions, etc.
```

### Status Storage and Logging:
- **HASTAT variable**: Primary storage for last hardware status (both RRTS and RTTS)
- **Circular logging buffers**: 
  - `BUFF0(BUFSIZ)`: First word in frame
  - `BUFF1(BUFSIZ)`: Device number used  
  - `BUFF2(BUFSIZ)`: Device status (HASTAT value)
  - `BUFF3(11)`: List keys when device stopped

### Key Status Bits and Processing:
- **EMTY** (Empty): `IF HASTAT/\"EMTY" >< 0` - Buffer empty condition, stops device
- **HX21M** (X.21 Error Mask): `IF A/\ HX21M >< 0` - Detects X.21 protocol errors
- **HX21S** (X.21 Clear): `IF A BIT HX21S` - X.21 clear indication, sets BLDON
- **BLDON** (Block Done): `HASTAT BONE BLDON=:HASTAT` - Marks completion
- **SILFO+TXUND**: `IF A/\ "SILFO+TXUND" = 0` - Transmitter underrun flags
- **LMASK**: `IF A /\ "LMASK" = 3` - Frame type/length mask

### Error Codes (from txt analysis):
- **Error 164**: BROCK - Buffer corruption/DMA error (bit 10)  
- **Error 237**: BLOVF - Buffer overflow (bit 13)

### Status Processing Functions:
- **SADTS**: `T:=MASTB; * ADSTA@3 STATX` - Store hardware status
- **DRERR**: `A\/DSTAT =: DSTAT; HDERC+1 =: HDERC` - OR status, increment error counter
- **STPCNT**: Counter for receiver stop events (lack of buffers)
- **LHAST**: Last hardware status exported to user (RRTS or RTTS value)

## Critical Findings

### HDLC Register Bit Processing (DEFINITIVE ANALYSIS)

**TRANSMISSION SUCCESS LOGIC (CORRECT):**
- **SILFO+TXUND = 0x8002** (bits 15,1) - tests for illegal format OR underrun
- **Success condition**: `IF (RTTS AND 0x8002) == 0` - both error bits clear
- **Your C# bit definitions are CORRECT** - X21D=bit13, X21S=bit14, ReceiverOverrun=bit15

**RECEPTION PROCESSING LOGIC:**
- **DataAvailable (bit 0)** must be 1 for packet processing
- **ListEmpty (bit 11)** when 1 stops receiver completely  
- **X.21 errors (bits 13-14)** trigger protocol error handling
- **HX21S constant confusion**: 0x000E tests receiver state (bits 1,2,3), NOT X.21 clear indication

**ROOT CAUSE ANALYSIS:**
1. **Transmission logic is CORRECT** - retransmission issues likely elsewhere
2. **Reception logic sound** - packet drops likely from incorrect status bit values
3. **Focus debugging on emulator status bit generation**, not bit interpretation

## Development Context

This repository documents the complete HDLC interrupt processing flow in SINTRAN:

- **IRQ → Status Read → Bit Analysis → Action** pipeline fully documented  
- **DMA operations** from SINTRAN OS perspective with ASCII diagrams
- **Actual SINTRAN constants** from symbol tables with octal/hex/binary values
- **Status logging** provides debugging visibility into hardware states
- **Error handling** covers both DMA controller errors and X.21 protocol issues
- **Circular buffers** maintain history of device interactions for diagnostics
- **Register bit usage** completely mapped to SINTRAN processing decisions
- **Focus on understanding real-time HDLC protocol state machines and error recovery**

**Key for HDLC Emulator Development:** The SINTRAN HDLC logic is sound. Issues stem from incorrect status bit values in hardware emulation, not from bit interpretation logic.

## Network Traffic Analysis

The repository contains detailed analysis of actual HDLC network traffic captured between machines:

### Traffic Trace Files
- **trace-conn-100-102.txt**: HDLC communication from machine 100 to machine 102
- **trace-conn-102-100.txt**: Return communication from machine 102 to machine 100
- **trace-with-transmit.txt**: Extended traces showing transmission patterns and retransmissions

### Traffic Analysis Features
- **Frame-by-Frame Breakdown**: Every packet analyzed with LAPB sequence numbers, X.25 headers, and payload data
- **Multi-Buffer Frame Analysis**: Documentation of how large frames are split across multiple DMA buffers with RSOM/REOM flags
- **Retransmission Patterns**: Analysis of when and why frames are retransmitted
- **Protocol Stack Mapping**: Complete mapping from raw HDLC frames through LAPB to X.25 packets
- **Timing Analysis**: Precise timestamps showing frame transmission and reception timing
- **Buffer Address Tracking**: DMA buffer addresses showing memory layout and buffer reuse patterns

### Key Traffic Patterns Documented
- **Connection Establishment**: SABM/UA exchange for LAPB connection setup
- **Data Transfer**: Information frames with proper N(S)/N(R) sequence numbering
- **Error Recovery**: RR/REJ frames for error detection and recovery
- **Connection Teardown**: DISC/UA exchange for clean connection closure
- **X.25 Virtual Circuits**: Call setup, data transfer, and call clearing at X.25 level

The traffic analysis provides ground truth for validating SINTRAN HDLC implementation against actual network behavior.

## Working with C# Analysis Files

This repository contains several C# files that demonstrate HDLC emulation logic:

- **NDBusHDLC.cs**: Main HDLC controller emulation implementation
- **HDLC_Status_Processing_Pseudocode.cs**: Pseudocode translations of SINTRAN assembly logic
- **DMA_Transmit_Debug_Logging.cs**: Debug logging implementations for DMA operations
- **Targeted_DMA_Debug_Additions.cs**: Specific debug additions for troubleshooting

### C# Code Conventions
- No LINQ usage allowed (per project requirements)
- Detailed debug logging with `#define DEBUG_DETAIL` and `#define DMA_DEBUG`
- Register operations closely mirror SINTRAN assembly patterns
- Bit manipulation follows NORSK DATA CPU instruction patterns
- do not infere. only document what is actually possible to prove by looking at the SINTRAN source code