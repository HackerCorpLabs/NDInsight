# SINTRAN HDLC DMA Operations - ASCII Diagrams

## DMA Receive Operation (SINTRAN OS Perspective)

```
                          HDLC RECEIVER DMA OPERATION
                         ================================

    SINTRAN OS                    HDLC Controller                Hardware
       |                              |                           |
       |                              |                           |
   ┌───────┐                     ┌────────────┐               ┌───────┐
   │Buffer │                     │    DMA     │               │  DCE  │
   │ List  │                     │ Controller │               │ Modem │
   └───────┘                     └────────────┘               └───────┘
       │                              │                           │
       │ 1. Setup RX Buffer List      │                           │
       │═══════════════════════════>  │                           │
       │    WRTC (Write RX Transfer   │                           │ 
       │    Control) - Buffer addrs   │                           │
       │                              │                           │
       │                              │ 2. Incoming Data          │
       │                              │ <═════════════════════════│
       │                              │    HDLC Frame            │
       │                              │                           │
       │                              │ 3. Store in Buffer        │
       │                              │    [DMA Transfer]         │
       │                              │                           │
       │ 4. IRQ Level 13 (Receiver)   │                           │
       │ <════════════════════════════│                           │
       │    HIINT: T:=HDEV+RRTS;*EXR ST                          │
       │                              │                           │
   ┌───▼────┐                         │                           │
   │ HASTAT │ 5. Store RRTS value     │                           │
   │= RRTS  │ <═══════════════════════│                           │
   └────────┘                         │                           │
       │                              │                           │
       │ 6. Bit Analysis:             │                           │
       │    IF A NBIT 0 THEN drop     │                           │
       │    IF A/\HX21M>0 THEN error  │                           │
       │    IF HASTAT/\"EMTY">0 THEN stop │                       │
       │                              │                           │
   ┌───▼────┐                         │                           │
   │Process │ 7. Handle received data │                           │
   │ Packet │                         │                           │
   └────────┘                         │                           │
       │                              │                           │
       │ 8. Update Buffer Pointers    │                           │
       │═══════════════════════════>  │                           │
       │                              │                           │

Status Bits Checked (RRTS → HASTAT):
┌─────────────────────────────────────────────────────────────┐
│ Bit 0  (DataAvailable)  │ Must be 1 - packet ready          │
│ Bit 11 (ListEmpty)      │ If 1 - stop receiver (no buffers) │
│ Bits 13-14 (X21D/X21S)  │ If set - X.21 protocol error     │
│ Bits 1,2,3 (HX21S mask) │ Receiver state for error cleanup │
└─────────────────────────────────────────────────────────────┘

Buffer Management:
BUFF0[i] = Frame sequence number
BUFF1[i] = Device number  
BUFF2[i] = HASTAT (status value)
BUFF3[i] = Device keys when stopped
```

## DMA Transmit Operation (SINTRAN OS Perspective)

```
                         HDLC TRANSMITTER DMA OPERATION
                        ==================================

    SINTRAN OS                    HDLC Controller                Hardware
       |                              |                           |
   ┌───────┐                     ┌────────────┐               ┌───────┐
   │Packet │                     │    DMA     │               │  DCE  │
   │Buffer │                     │ Controller │               │ Modem │
   └───────┘                     └────────────┘               └───────┘
       │                              │                           │
       │ 1. Setup TX Buffer List      │                           │
       │═══════════════════════════>  │                           │
       │    WTTC (Write TX Transfer   │                           │
       │    Control) - Buffer addrs   │                           │
       │                              │                           │
       │ 2. Start Transmission        │                           │
       │═══════════════════════════>  │                           │
       │    Trigger DMA TX start      │                           │
       │                              │                           │
       │                              │ 3. Read Buffer Data       │
       │                              │    [DMA Transfer]         │
       │                              │                           │
       │                              │ 4. Transmit HDLC Frame    │
       │                              │ ══════════════════════════>│
       │                              │                           │
       │                              │ 5. Transmission Complete  │
       │                              │    or Error occurred      │
       │                              │                           │
       │ 6. IRQ Level 12 (Transmitter)│                           │
       │ <════════════════════════════│                           │
       │    HOINT: 0=:TMR             │                           │
       │           T:=HDEV+RTTS;*EXR ST                           │
       │                              │                           │
   ┌───▼────┐                         │                           │
   │ HASTAT │ 7. Store RTTS value     │                           │
   │= RTTS  │ <═══════════════════════│                           │
   └────────┘                         │                           │
       │                              │                           │
       │ 8. Success/Error Decision:   │                           │
       │    IF A/\"SILFO+TXUND"=0 THEN│                           │
   ┌───▼────┐     SUCCESS             │                           │
   │ XRETRY │ 9a. Clear retry state   │                           │
   │ = 0    │     Log success         │                           │
   └────────┘                         │                           │
       │                              │                           │
       └─ OR ─┐                       │                           │
              │                       │                           │
   ┌──────────▼─┐ 9b. ERROR PATH      │                           │
   │ Retransmit │     Increment HDERC │                           │
   │   Packet   │     Set EUND code   │                           │
   └────────────┘                     │                           │

Status Bits Checked (RTTS → HASTAT):
┌─────────────────────────────────────────────────────────────┐
│ Bit 1  (TransmitterUnderrun) │ If 1 - retransmit           │
│ Bit 15 (Illegal Format/Key)  │ If 1 - retransmit           │
│ Success only when BOTH bits are 0 (SILFO+TXUND = 0x8002)   │
└─────────────────────────────────────────────────────────────┘

Error Handling:
HDERC++ (Hardware error counter)
EUND=0x0042 (Underrun error code)
```

## Complete DMA Transaction Flow

```
                    FULL HDLC DMA TRANSACTION CYCLE
                   ===================================

RECEIVE PATH:                           TRANSMIT PATH:
                                       
┌─────────────┐                        ┌─────────────┐
│   Buffer    │                        │   Packet    │
│   Setup     │                        │   Ready     │ 
│   (WRTC)    │                        │   (WTTC)    │
└──────┬──────┘                        └──────┬──────┘
       │                                      │
       ▼                                      ▼
┌─────────────┐                        ┌─────────────┐
│ Wait for    │                        │ Start DMA   │
│ Incoming    │                        │ Transfer    │
│ Data        │                        │             │
└──────┬──────┘                        └──────┬──────┘
       │                                      │
       ▼ [Packet arrives]                     ▼ [Data transmitted]
┌─────────────┐                        ┌─────────────┐
│ IRQ Level   │                        │ IRQ Level   │
│ 13 (HIINT)  │                        │ 12 (HOINT)  │
└──────┬──────┘                        └──────┬──────┘
       │                                      │
       ▼                                      ▼
┌─────────────┐                        ┌─────────────┐
│ Read RRTS   │                        │ Read RTTS   │
│ Store in    │                        │ Store in    │
│ HASTAT      │                        │ HASTAT      │
└──────┬──────┘                        └──────┬──────┘
       │                                      │
       ▼                                      ▼
┌─────────────┐                        ┌─────────────┐
│ Check Bits: │                        │ Check Bits: │
│ • DataAvail │                        │ • TXUND     │
│ • ListEmpty │                        │ • SILFO     │
│ • X21 Error │                        │             │
└──────┬──────┘                        └──────┬──────┘
       │                                      │
       ▼                                      ▼
   ┌───────┐     ┌──────────┐          ┌──────────┐     ┌───────┐
   │Process│     │   Drop   │          │ Success  │     │Retry  │
   │Packet │     │ Packet   │          │Continue  │     │Packet │
   └───────┘     └──────────┘          └──────────┘     └───────┘

Status Flow:
Hardware → RRTS/RTTS → HASTAT → Bit Tests → Action Decision
```

## Buffer List Structure (DMA Perspective)

```
                        DMA BUFFER LIST LAYOUT
                       ======================

RECEIVE BUFFERS:                    TRANSMIT BUFFERS:
                                   
┌──────────────┐                   ┌──────────────┐
│ Buffer Addr 1│ ◄─── WRTC         │ Buffer Addr 1│ ◄─── WTTC  
├──────────────┤      Setup        ├──────────────┤      Setup
│ Buffer Addr 2│                   │ Buffer Addr 2│
├──────────────┤                   ├──────────────┤
│     ...      │                   │     ...      │
├──────────────┤                   ├──────────────┤
│ Buffer Addr N│                   │ Buffer Addr N│
├──────────────┤                   ├──────────────┤
│ END MARKER   │                   │ END MARKER   │
└──────────────┘                   └──────────────┘
        │                                  │
        ▼                                  ▼
┌──────────────┐                   ┌──────────────┐
│   INCOMING   │                   │   OUTGOING   │
│     HDLC     │                   │     HDLC     │
│    FRAMES    │                   │    FRAMES    │
└──────────────┘                   └──────────────┘

List Status Bits:
• ListEmpty (bit 11) - No more RX buffers
• ListEnd (bit 10) - End of TX buffer list  
• BlockEnd (bit 8) - Current buffer complete
```

## Error Conditions and Recovery

```
                           ERROR HANDLING FLOW
                          ===================

RX ERRORS:                          TX ERRORS:
                                   
┌─────────────┐                    ┌─────────────┐
│ X.21 Error  │                    │ Underrun    │
│ (bits13-14) │                    │ Error (b1)  │
└──────┬──────┘                    └──────┬──────┘
       │                                  │
       ▼                                  ▼
┌─────────────┐                    ┌─────────────┐
│ Handle X.21 │                    │ Set EUND    │
│ Protocol    │                    │ Error Code  │
│ Error       │                    │ (0x0042)    │
└──────┬──────┘                    └──────┬──────┘
       │                                  │
       ▼                                  ▼
┌─────────────┐                    ┌─────────────┐
│If Receiver  │                    │ Increment   │
│Active (HX21S│                    │ HDERC       │
│bits 1,2,3): │                    │ Counter     │
│Terminate    │                    └──────┬──────┘
└──────┬──────┘                           │
       │                                  ▼
       ▼                            ┌─────────────┐
┌─────────────┐                    │ Retransmit  │
│Buffer Empty │                    │ Packet      │
│(bit 11):    │                    │ (XRETRY)    │
│Stop Device  │                    └─────────────┘
│STPCNT++     │
└─────────────┘

Common Recovery Actions:
• CALL SADTS - Store status in diagnostic buffers
• CALL DRERR - Handle device error reporting  
• CALL SCRET - Return status to calling process
```

This ASCII diagram shows the complete DMA operation flow from SINTRAN's perspective, including buffer management, interrupt handling, status bit checking, and error recovery mechanisms.