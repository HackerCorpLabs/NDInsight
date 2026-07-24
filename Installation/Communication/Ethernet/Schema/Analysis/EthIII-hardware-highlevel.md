# Ethernet III Controller (ND-110513) — Hardware Description, High Level

**Card 324232 (print H = latest, Nov 1990), 32 schematic sheets in
`../EthIIIImages/`. Sources: the print-H schematics (surveyed, not fully traced) and
the ND-895xxx installation descriptions in NDInsight. There is NO technical manual
available (ND-814006 was never scanned). Detail companion:
[EthIII-hardware-detail.md](EthIII-hardware-detail.md).**

The Ethernet III is the **ND-5000-generation** Ethernet controller — a 68020-based
single-board computer attached to the **MF bus** for data and to the **OCTOBUS** as a
controllable station. Unlike the Ethernet II it **boots from its own EPROM** (with a
resident OPCOM monitor); SINTRAN then downloads application modules (e.g. TCP/IP
Basic Module/III) into its DRAM at system load.

## 1. System context

```mermaid
flowchart LR
    subgraph HOST["ND-5000-era system"]
        SINTRAN["SINTRAN<br/>(TCP-BA-LOAD:MODE loads app sw)"]
        MFBUS[["MF bus<br/>(32-bit data path)"]]
        OCTO[["OCTOBUS<br/>(control fabric)"]]
    end
    subgraph CARD["Ethernet III card 324232"]
        MFA["MF-bus interface<br/>(MFA latch/control/driver/ident)"]
        OB["OCTOBUS station<br/>ND_D_OBCON gate array"]
        CPU["MC68020-16<br/>(33 MHz variant exists)"]
        EPROM["Boot EPROMs<br/>DEVICE + OPCOM"]
        DRAM["4 DRAM banks + parity"]
        LANCE["Am7990 LANCE + Am7992B SIA"]
    end
    XCVR["Transceiver (+12V switched)"]
    NET(("Ethernet 10 Mb/s"))
    CON["RS-232 service console"]

    SINTRAN --- MFBUS --- MFA
    SINTRAN --- OCTO --- OB
    OB -- "remote reset / halt / interrupt<br/>+ watchdog" --> CPU
    MFA --- DRAM
    CPU --- EPROM & DRAM & LANCE
    LANCE --- XCVR --- NET
    CPU --- CON
```

Product facts (installation descriptions, NDInsight):
- Ethernet III controller = **ND 110513** (Ethernet II was 110063).
- Runs the **TCP/IP Basic Module for ND-5000** in-card; loaded via
  `@MODE (TCP-IP)TCP-BA-LOAD:MODE`, sequenced with XMSG and ND-5000 start.
- Several Ethernet III controllers can run TCP/IP simultaneously
  (`AIP-CONFIG:SYMB` lists them and their TCP-device numbers).

## 2. Board block diagram (from sheet 4 "BLOCK LEVEL")

```mermaid
flowchart TB
    subgraph BUSIF["BUS_INTERF (sheets 5-11)"]
        LATCH["MFA latch/buffers"]
        MFACTL["MFA control + ident"]
        OBC["OCTOBUS station:<br/>ND_D_OBCON + S225 FIFOs<br/>+ '10 OCTO' command PAL<br/>+ LS292 watchdog"]
    end
    subgraph CTRL["CONTROL (sheets 12-19, 26-28)"]
        CPU2["MC68020-16<br/>MCA/MCD(31:00)"]
        INTC["Interrupt logic:<br/>LS148 + INT7-collector PAL<br/>+ MK68901 @ 3.6864 MHz"]
        EPROM2["EPROMs: DEVICE + OPCOM<br/>+ EEPROM parameters"]
        CLKS["60 MHz osc → 30/15/7.5/3.75<br/>break system, timeouts, reset"]
        PROT["Memory protection"]
        REGS["Register block"]
    end
    subgraph MEMB["MEMORY (sheets 20-25)"]
        B0["DRAM bank 0"]
        B1["DRAM bank 1"]
        B2["DRAM bank 2"]
        B3["DRAM bank 3"]
        PAR["Parity"]
    end
    subgraph DEV["DEVICE (sheets 29-32)"]
        L2["Am7990 LANCE<br/>(16-bit, behind '16 ARBIT' PAL)"]
        S2["Am7992B SIA + 40 MHz"]
        PWR2["Transceiver power ctrl"]
    end
    LATCH --- MFACTL --- CPU2
    OBC -- "RESET1 / IHALT / OCINT7" --> INTC
    CPU2 --- INTC & EPROM2 & PROT & REGS
    CLKS --> CPU2 & INTC
    CPU2 --- B0 & B1 & B2 & B3
    PAR --- B0
    L2 -- "DMA via 32-bit gasket" --- B0
    CPU2 --- L2
    L2 --- S2 --- PWR2
```

## 3. What is shared with Ethernet II, what is new

| Aspect | Ethernet II (110063) | Ethernet III (110513) |
|---|---|---|
| Host bus | ND-100 I/O bus (IOXT + level 12 + ident) | **MF bus** + **OCTOBUS station** |
| CPU | MC68HC000-12 @ 12.5 MHz | **MC68020-16** (33 MHz variant annotated) |
| Local bus | 16-bit | **32-bit** MCA/MCD |
| Boot | all code downloaded by ND-100 into DRAM | **self-boots from EPROM** ("DEVICE" + "OPCOM"), SINTRAN downloads app modules |
| Non-volatile | none | **EEPROM** 2-8 KB (parameters) |
| MFP | MK68901 @ 3.125 MHz, GPIP5/6/7 used | MK68901 @ **3.6864 MHz**, LANCE int on **GPIP4**, CCINT/DMATRAP on GPIP0/1 |
| Console | 20 mA current loop (PCT) | **RS-232 via MAX233** |
| Ethernet chipset | Am7990 + Am7992B | **same** Am7990 + Am7992B (16-bit, bus-gasketed) |
| DRAM | 512 KB, 1 bank | **4 banks** + parity |
| Watchdog | none | **LS292 watchdog** (octobus supervised) |
| Remote control | ND-100 control-word bits | **octobus commands**: reset / halt / interrupt / counters |
| Breakpoints | forced-parity (PARITYDIS+BREAKMODE) | **address comparators** (BREAK32/UBRK/LBRK force BERR) |
| Timer hardware | MFP timers only | MFP timers only (**no timer chip on either card**) |

## 4. Interrupt architecture at a glance

```mermaid
flowchart LR
    subgraph SRC["Sources"]
        MFPS["MK68901<br/>(GPIP: LANCE int, CCINT, DMATRAP...<br/>+ timers + USART)"]
        MC34["MF-bus MCINT3 / MCINT4"]
        OC["OCTOBUS OCINT"]
        CR["Console ready (RR/TR)"]
        SEV["Fatal group → 'INT7' PAL:<br/>trap, bus error, PFAIL, parity,<br/>OCINT7, external INT7s"]
    end
    E["LS148 priority encoder"]
    P["68020 IPL0-2"]
    MFPS --> E
    MC34 --> E
    OC --> E
    CR --> E
    SEV -- "collected + readable on MD(23:16)" --> E
    E --> P
```

The fatal/NMI-class sources are latched in a PAL20RA10 whose pending set is
**memory-mapped readable** (MD 23:16) — a proper interrupt-source status register,
where Ethernet II only had discrete flip-flops.

## 5. Clocks

| Clock | Value | Purpose |
|---|---|---|
| 60 MHz osc (66 for -33 variant) | ÷2 chain → 30 / 15 / 7.5 / 3.75 MHz | CPU (16CLK ≈ 15/16 MHz), system clocks |
| 3.6864 MHz xtal | — | MK68901 (timers + baud rates) |
| 40 MHz osc | ÷2 → 20 MHz | SIA (10 Mb/s Manchester) |
| 16 MHz | — | ND_D_OBCON octobus gate array |
| 2× LS292 counters | programmable | local bus timeout (TBERR), octobus watchdog (WDOGRES) |

No RTC chip, no interrupt/timer controller chip — periodic timing is MFP timers,
exactly like Ethernet II.

## 6. Print D → print H changes

Revised sheets (title blocks `324232HH` / `5452H`, 90.11.21; sheet 8: 88.11.01):
**8** MFA control, **11** OCTOBUS (ECO 380-136 rev B), **13** address decode,
**15** CPU, **17** clocks/break/reset, **26** DRAM control, **30** device decode,
**31** LANCE. The octobus attachment itself existed already in print D (1988) —
H reworked it.

## 7. Emulation notes

See `EthIII-architecture-survey.md` §6. Essentials: reuse of the RetroCore MK68901
(at 3.6864 MHz) and Am7990/7992 cores is straightforward; new work is the 68020
(DSACK sizing), the MF-bus slave, and an octobus station following the
`OctobusND5000Station` pattern (ND_D_OBCON speaks the station protocol: presence,
remote reset/halt/interrupt, watchdog). The 68020 address map is not yet extracted
(sheets 30 + 13 pending) — that is the prerequisite for emulation.
