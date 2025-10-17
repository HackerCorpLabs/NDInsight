# HDLC Quick Reference Card ⚡

**One-page cheat sheet for SINTRAN HDLC implementation**

---

## Critical Success/Failure Tests

### Transmission Success
```csharp
// ONLY test in SINTRAN - HOINT Phase 3
bool success = (RTTS & 0x8002) == 0;  // Both SILFO and TXUND must be clear
```

### Reception Success
```csharp
// Multiple tests in HIINT:
bool dataAvailable = (RRTS & 0x0001) != 0;     // Bit 0 must be set
bool noX21Error = (RRTS & 0x6000) == 0;        // Bits 13-14 must be clear
bool buffersAvailable = (RRTS & 0x0800) == 0;  // Bit 11 must be clear
```

---

## Critical Constants

| Name | Octal | Hex | Bits | Purpose |
|------|-------|-----|------|---------|
| **SILFO** | 100000 | 0x8000 | 15 | Transmitter illegal format |
| **TXUND** | 000002 | 0x0002 | 1 | Transmitter underrun |
| **EMTY** | 004000 | 0x0800 | 11 | Receiver list empty (FATAL) |
| **HX21M** | 060000 | 0x6000 | 13-14 | X.21 error mask |
| **FSERM** | 002003 | 0x0403 | - | Single frame DMA key |
| **WRTC_FULL** | 001734 | 0x03DC | - | Full receiver interrupt enable |
| **WTTC_FULL** | 001134 | 0x0264 | - | Full transmitter interrupt enable |

---

## Register Map (Quick)

| IOX | Register | R/W | Purpose |
|-----|----------|-----|---------|
| +10 | **RRTS** | R | **Receiver status** (check this in HIINT) |
| +11 | **WRTC** | W | **Receiver control** (write 1734₈ for full mode) |
| +12 | **RTTS** | R | **Transmitter status** (check this in HOINT) |
| +13 | **WTTC** | W | **Transmitter control** (write 1134₈ for full mode) |
| +15 | **WDMA** | W | Write DMA list address |
| +17 | **WDCR** | W | Write DMA command + trigger |

---

## RRTS Bit Map (Receiver)

| Bit | Name | Test | Action |
|-----|------|------|--------|
| 0 | DataAvailable | **Must be 1** | Packet ready |
| 11 | ListEmpty | **Must be 0** | FATAL if set - stops receiver |
| 13-14 | X21D/X21S | **Must be 0** | X.21 protocol errors |

---

## RTTS Bit Map (Transmitter)

| Bit | Name | Test | Action |
|-----|------|------|--------|
| 1 | TransmitterUnderrun | **Must be 0** | Retry if set |
| 15 | Illegal | **Must be 0** | Retry if set |

---

## DMA Descriptor (4 Words)

```
Word 0: LKEY   = Block control (bits 10-8) + COM5025 bits (7-0)
Word 1: LBYTC  = Byte count
Word 2: LMEM1  = Memory bank (high address)
Word 3: LMEM2  = Buffer address (low address)
```

### LKEY Values

| Value | Octal | Purpose |
|-------|-------|---------|
| 0x0401 | 002001 | First block (TSOM only) |
| 0x0400 | 002000 | Middle block (no flags) |
| 0x0402 | 002002 | Last block (TEOM only) |
| 0x0403 | 002003 | Single block (TSOM+TEOM) - **FSERM** |

---

## Interrupt Enable Values

### WRTC (Receiver Control)
```assembly
100₈   (0x40)   - Minimal (cleanup mode)
140₈   (0x60)   - Maintenance mode
1734₈  (0x3DC)  - Full operational mode ← USE THIS
```

### WTTC (Transmitter Control)
```assembly
0      (0x00)   - Transmitter off
CMODI  (0x20)   - RQTS control only
1134₈+CMODI     - Full operational mode ← USE THIS
```

---

## Key Variables

| Variable | Address | Purpose | Critical Values |
|----------|---------|---------|-----------------|
| **ACTSW** | 000074 | Activity switch | 0=inactive, 1=active |
| **HASTAT** | 000076 | Hardware status | Stores RRTS/RTTS |
| **XRETRY** | 000105 | Retry counter | 0 to MAXR (77₁₀) |

---

## Common Status Patterns

### Success Transmission
```
RTTS = 0x0000 or 0x0200
(RTTS & 0x8002) = 0 ✅
```

### Failed Transmission (Underrun)
```
RTTS = 0x0002
(RTTS & 0x8002) = 0x0002 ❌
```

### Failed Transmission (Format Error)
```
RTTS = 0x8000
(RTTS & 0x8002) = 0x8000 ❌
```

### Success Reception
```
RRTS = 0x0001
Bit 0 = 1 ✅
Bits 13-14 = 0 ✅
Bit 11 = 0 ✅
```

### Fatal Reception (No Buffers)
```
RRTS = 0x0801
Bit 11 = 1 ❌ → ACTSW forced to 0
```

---

## Decision Trees

### HOINT (Transmitter)
```
Read RTTS → Store in HASTAT
↓
IF (HASTAT & 0x8002) == 0
├─ YES → SUCCESS (clear retry, mark complete)
└─ NO  → FAILURE (increment retry, retransmit or fatal)
```

### HIINT (Receiver)
```
Read RRTS → Store in HASTAT
↓
IF (HASTAT & 0x6000) != 0 → X.21 error → EXIT
↓
IF (HASTAT & 0x0800) != 0 → Buffer exhaustion → ACTSW=0 → EXIT
↓
IF (HASTAT & 0x0001) == 0 → No data → EXIT
↓
Process packet (CALL PROCPKT)
```

---

## Debugging Checklist

### Packets Not Transmitting?
- [ ] Check ACTSW = 1 (device active)
- [ ] Check WTTC = 1134₈+CMODI (interrupts enabled)
- [ ] Check DMA descriptor LKEY has correct COM5025 bits
- [ ] Check RTTS after transmission (should be 0x8002 clear)
- [ ] Check XRETRY counter (not exceeding MAXR)

### Packets Not Receiving?
- [ ] Check ACTSW = 1 (device active)
- [ ] Check WRTC = 1734₈ (interrupts enabled)
- [ ] Check RRTS bit 0 = 1 (data available)
- [ ] Check RRTS bit 11 = 0 (buffers available)
- [ ] Check RRTS bits 13-14 = 0 (no X.21 errors)

### Spurious Interrupts?
- [ ] Check ACTSW state before processing
- [ ] Check if device should be active
- [ ] Verify interrupt enable registers (WRTC/WTTC)

---

## Emergency Commands

### Stop Receiver
```assembly
0 → WRTC    % Disable all receiver interrupts
0 → ACTSW   % Mark device inactive
```

### Stop Transmitter
```assembly
0 → WTTC    % Disable all transmitter interrupts
0 → ACTSW   % Mark device inactive
```

### Start Full DMA Reception
```assembly
LIINT+DPITPHYS → WDMA        % Set DMA address
1001₈ → WDCR                 % Start receiver DMA
1734₈∨MAINT∧HXDOK → WRTC     % Enable full interrupts
1 → ACTSW                    % Mark active
```

### Start Full DMA Transmission
```assembly
LIINT+DPITPHYS → WDMA        % Set DMA address
2000₈ → WDCR                 % Start transmitter DMA
1134₈+CMODI → WTTC           % Enable full interrupts
1 → ACTSW                    % Mark active
```

---

## Critical Code Paths

### Transmit Path
```
XSSDATA → Validate → Setup DMA → XHMST → Hardware Start
    ↓
  Wait for interrupt (ID12)
    ↓
  HOINT → Read RTTS → Check 0x8002
    ├─ Success → NEXTS
    └─ Failure → DRERR → Retry (XSSND) or Fatal
```

### Receive Path
```
Hardware fills buffer → Interrupt
    ↓
  HIINT → Read RRTS → Validate
    ├─ Valid → PROCPKT → User delivery
    └─ Invalid → Drop packet
    ↓
  ZSTARC (restart receiver if ACTSW still active)
```

---

## See Full Documentation

- **Complete Specs**: [Register Reference](reference/Register-Reference.md)
- **Implementation**: [Emulator Guide](implementation/Emulator-Implementation-Guide.md)
- **Debugging**: [Debugging Guide](implementation/Debugging-Guide.md)
- **Everything**: [HDLC-ALL.md](HDLC-ALL.md)

---

**Print this page and keep it handy!** 🖨️

