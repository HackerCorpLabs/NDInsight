# Ethernet II Controller (ND-110063) — Hardware Description, High Level

**Card 324534 (print G), PCB 3094. Sources: schematics `../EthIIImages/` (byte-verified
at 300 dpi) and the technical manual ND-12.055.1 EN (original PDF). Detail-level
companion: [EthII-hardware-detail.md](EthII-hardware-detail.md).**

The Ethernet II is the ND-100-bus Ethernet controller: a complete single-board
68000 computer that SINTRAN/PIOCOS talks to through two IOXT registers, shared DRAM,
and a level-12 interrupt. The card boots nothing by itself — the ND-100 downloads all
68000 code (ENCOS) into the card's DRAM and releases it from reset.

## 1. System context

```mermaid
flowchart LR
    subgraph ND100["ND-100 host"]
        SINTRAN["SINTRAN / PIOCOS driver"]
    end
    subgraph CARD["Ethernet II card 3094"]
        REGS["Control / Status registers<br/>(IOXT, 2 write + 2 read)"]
        DRAM["512 KB shared DRAM<br/>(mailboxes + 68000 code)"]
        CPU68K["MC68HC000-12<br/>local processor"]
        LANCE["Am7990 LANCE"]
        SIA["Am7992B SIA"]
    end
    XCVR["External transceiver<br/>(+12V switched)"]
    NET(("Ethernet<br/>10 Mb/s"))

    SINTRAN -- "IOXT reg access + DMA to DRAM" --> REGS
    REGS -- "level-12 interrupt, ident 2240-2243" --> SINTRAN
    REGS --- CPU68K
    CPU68K --- DRAM
    LANCE -- "DMA" --- DRAM
    CPU68K --- LANCE
    LANCE --- SIA
    SIA --- XCVR
    XCVR --- NET
```

- **Bus attachment:** ND-100 I/O bus. Device addresses 140360/140364/140370/140374₈
  (thumbwheel-selected), ident codes 2240–2243₈, interrupt level 12.
- **Memory attachment:** the card's DRAM appears as an ND-100 memory bank
  (bank number set by thumbwheels, read back in the status register).

## 2. Board block diagram

```mermaid
flowchart TB
    subgraph HOSTIF["ND-100 bus interface (sheets 2-3)"]
        BUF["Bus buffers<br/>BD0-23, strobes"]
        SEL["Device select<br/>thumbwheels + F521 compare"]
        IDPROM["IDENT PROMs 089-00/01<br/>(code 2240-2243)"]
        INTFF["INT12 logic<br/>RFT + RIE flip-flops"]
        CWREG["Control-word bits<br/>reset/halt/doorbells"]
    end
    subgraph CORE["68000 core (sheet 1)"]
        CPU["MC68HC000-12<br/>12.5 MHz"]
        ENC["LS148 IPL encoder<br/>+ PAL 453-00 IACK"]
        CLK["50 MHz osc<br/>/2 /2 /4 dividers"]
    end
    subgraph MEM["Memory (sheet 4)"]
        DRAM2["512 KB DRAM + byte parity<br/>(standby powered)"]
        EPROM["2x 27512 sockets<br/>(EMPTY - unused)"]
        SRAMP["1 Kbit SRAM<br/>write-protect table"]
    end
    subgraph PERIPH["Peripherals (sheets 3, 5)"]
        MFP["MK68901 MFP<br/>timers, USART, GPIP ints"]
        LANCE2["Am7990 LANCE"]
        SIA2["Am7992B SIA + 40 MHz"]
        PWR["Transceiver +12V switch<br/>PWEN FF + 12V sense"]
        PCT["Test console PCT<br/>20 mA current loop"]
    end

    BUF --- SEL --- INTFF
    SEL --- CWREG
    CWREG -- "reset / halt / NMI-enable" --> CPU
    CWREG -- "doorbells" --> ENC & MFP
    INTFF -- "BINT12 + ident" --> BUF
    CPU --- ENC
    CLK --> CPU & MFP
    CPU --- DRAM2 & EPROM & SRAMP
    BUF -- "ND-100 memory-bank access" --> DRAM2
    LANCE2 -- "DMA (2nd priority)" --> DRAM2
    CPU --- MFP & LANCE2 & PWR
    MFP --- PCT
    LANCE2 --- SIA2 --- PWR
```

**DRAM access priority:** ND-100 (highest) → LANCE → 68000 (lowest). [M:16]

## 3. The two host-visible registers (IOXT)

| Access | Address | Register |
|---|---|---|
| read | device +0 or +2 | **Status**: bits 15-8 bank number, 5 halt, 4 reset, 2 INT12 pending, 0 interrupt enabled |
| write | device +1 or +3 | **Control**: bit 0 enable SCIP int, 2 ND interrupt (→MFP), 3 start OPCOM, 4 reset, 5 halt, 6 power-low enable, 8 disable parity check |

## 4. Interrupt architecture at a glance

```mermaid
flowchart LR
    subgraph TO68K["Interrupts INTO the 68000 (LS148 priority encoder)"]
        L7["7 NMI: power low<br/>(enable bit6 + Master Clear)"]
        L6["6: OPCOM doorbell<br/>(control bit 3)"]
        L5["5: memory parity error"]
        L4["4: test console ready"]
        L3["3: MFP vectored<br/>(ND-int, LANCE-err, WRIV,<br/>USART, Timer C RTC)"]
        L2["2: LANCE"]
    end
    subgraph TOND["Interrupt OUT to the ND-100"]
        SCIP["68000 writes SCIP address"]
        RFT["RFT latch"]
        RIE["RIE enable<br/>(control bit 0)"]
        INT12["BINT12 - level 12<br/>ident 2240-2243"]
    end
    SCIP --> RFT --> INT12
    RIE --> INT12
```

- Levels 2,4,5,6,7 are **autovectored** (PAL drives VPA); level 3 is **vectored by
  the MFP** (vector base 0x40 → octal vectors 105–117).
- Levels 5 and 6 are cleared **by the IACK cycle itself** (PAL outputs CLRMERR /
  CLROPCOM) — a hardware detail the firmware relies on.
- The IDENT answer on the ND-100 side clears **both** the INT12 latch and the enable.

## 5. Clocks and timers (complete list)

| Clock | Value | Purpose |
|---|---|---|
| 50 MHz osc → ÷2 ÷2 | 12.5 MHz | 68000 CPU clock, control PALs |
| → ÷4 | 3.125 MHz | MFP clock (XTAL1 + CLK) |
| 40 MHz osc → ÷2 | 20 MHz | SIA (Ethernet 10 Mb/s Manchester) |
| MFP Timer C | ≈128.07 Hz | the ONLY periodic system timer (RTC, vector 105₈) |
| MFP Timer D | firmware-set | USART baud (loops back to RC/TC) |
| BERR shifter | ~1.3 µs | 68000 bus-cycle timeout |
| DCL RC | ~200 µs | delayed clear after power-low |

There is **no other timer hardware** — no RTC chip, no interrupt/timer controller.

## 6. Life cycle

```mermaid
sequenceDiagram
    participant ND as ND-100 (SINTRAN)
    participant CW as Control register
    participant CPU as 68000
    participant DRAM as Shared DRAM

    ND->>CW: Master Clear / write reset+halt
    ND->>DRAM: download ENCOS firmware (as memory bank)
    ND->>DRAM: write SSP + PC into bytes 0-7
    ND->>CW: write control word, reset=0 halt=0
    CW->>CPU: release RESET/HALT
    CPU->>DRAM: fetch vectors from DRAM[0..7], boot firmware
    CPU->>CPU: init MFP (VR=0x40, Timer C RTC), LANCE
    loop Operation
        ND->>DRAM: mailbox command
        ND->>CW: bit2 ND-interrupt (MFP vec 116) or bit3 OPCOM (level 6)
        CPU->>DRAM: mailbox answer
        CPU->>CW: write SCIP address → INT12 to ND-100
        ND->>ND: IDENT (gets 2240+n) → clears INT12 + enable
    end
```

## 7. What differs from the Ethernet III

See [EthIII-hardware-highlevel.md](EthIII-hardware-highlevel.md). In one line:
Ethernet II = ND-100-bus card, 68000, all code downloaded; Ethernet III (110513) =
ND-5000-era MF-bus + octobus card, 68020, self-booting from EPROM.
