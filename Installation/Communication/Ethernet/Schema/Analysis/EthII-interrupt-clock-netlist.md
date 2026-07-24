# Ethernet II (324534 print G) — Interrupt & Clock Netlist

**Gate-level transcription of the interrupt and clock circuitry of the ND Ethernet II
controller (card 324534, PCB 3094, ND product 110063), read from the print-G schematics
`../EthIIImages/` at native 300 dpi, cross-checked against the technical manual
ND-12.055.1 EN (original PDF pages, NOT the OCR'd Markdown — that file contains at least
one hallucinated section).**

Scope: everything feeding the two interrupt systems (68000 side, ND-100 side), the
68000↔MFP↔LANCE control connections, and every clock/timer source. DRAM timing, bus
buffering and the SIA analog path are out of scope (only touched where they generate
interrupts).

Evidence tags: `[S1..S5]` = schematic sheet 1..5 (+ grid ref), `[M:p]` = manual page,
`[INFERRED]` = PAL behavior inferred from pin names/context (no PAL dumps exist).

---

## 1. 68000 interrupt system

### 1.1 Priority encoder — LS148 @ 19F [S1 F1]

The 68000 (18D, MC68HC000-12) IPL0-2 (pins 25/24/23) come from an LS148 whose active-low
inputs are the interrupt sources. Input number = 68000 IPL level:

| LS148 input | Net | Source | 68k level | Vector type |
|---|---|---|---|---|
| 7 | `LEV7INT0` | PAL 455-02 @13F, from power-low | 7 (NMI) | autovector |
| 6 | `OPCOM0` | OPCOM FF (LS74 @7F) | 6 | autovector |
| 5 | `MERR0` | MERR FF (F74 @18F, sheet 4) | 5 | autovector |
| 4 | `CONSOLEINTR0` | MFP USART RR/TR ready pins via FF 7F + gate 26G | 4 | autovector |
| 3 | `901INTR0` | MFP MK68901 INTR pin 32 | 3 | **vectored** (MFP) |
| 2 | `LANINTR0` | LANCE AM7990 INTR pin | 2 | autovector |
| 1 | (H5 pull-up — unused) | — | 1 | — |
| 0 | unused | — | 0 | — |

Confirmed by manual level table [M:12]: 7=ND-100 power low, 6=ND-100 OPCOM, 5=parity
error, 4=test console (PTC), 3=MFP (and ND-100), 2=LANCE, 1=not used.

> ⚠ Manual p.27 says the PCT console "interrupts the 68000 on interrupt level 5 via the
> MFP" — this contradicts both the manual's own level table (p.12) and the schematic
> (RREDY/TREDY → level 4, direct, NOT through the MFP interrupt controller). The p.27
> sentence is a manual error.

### 1.2 IACK decode — PAL 453-00 (PAL16L8B) @ 21C [S1 F3-4]

Inputs: `A1 A2 A3 FC0 FC1 FC2 BGACK0 R/W0 BAS0 RESET0`.
During an IACK cycle FC=111 and A1-A3 = interrupt level. Outputs:

| Pin | Net | Function [INFERRED from names + manual] |
|---|---|---|
| 18 | `IAK9010` | MFP IACK (level-3 IACK cycle) → MK68901 pin 45 → MFP supplies vector |
| 17 | `AUTOVECTOR0` | Drives 68000 **VPA** (pin 21 via R) → autovector for levels 2,4,5,6,7 |
| 14 | `CLROPCOM0` | **Level-6 IACK clears the OPCOM FF in hardware** (7F R̄) |
| 13 | `CLRMERR0` | **Level-5 IACK clears the MERR FF in hardware** (18F R̄, sheet 4) |
| 19 | `CLRREDY0` | clears console-ready FF [INFERRED] |
| 12 | `CPUWR0` | CPU write strobe |
| 16 | `PRI1` | DRAM arbitration priority |
| 15 | `IAKE1` | interrupt-ack enable |

### 1.3 Interrupt sources, one by one

**LANINTR (level 2)** — AM7990 INTR pin (29D, sheet 5) with 2.7k pull-up. This is a
**LEVEL**: the LANCE holds INTR low while any unmasked CSR0 interrupt cause is set and
INEA=1. It is NOT cleared by IACK — only by the firmware writing CSR0 (or clearing
INEA / LANRESET). The same net is readable as **ETHSTAT bit 0** (S244 @9E, sheet 5,
active-low).

**901INTR (level 3)** — MK68901 INTR output, level; cleared by the MFP itself per its
IACK/ISR protocol (in-service registers, S-bit). Vector supplied by the MFP during
`IAK901` cycle.

**CONSOLEINTR (level 4)** — from the MFP USART's **pin-level** ready outputs RR
(`RREDY0`) and TR (`TREDY0`) (NOT the MFP interrupt controller) combined by FF 7F +
gate 26G [S3 A/B3]. Only meaningful when the test console strap `CONSPRES0` (D5/D6
jumper [S3 F/G4]) is fitted. `CLRREDY0` from PAL 453-00 clears the ready FF.

**MERR (level 5)** — sheet 4: two F280 parity trees over local-DRAM data (low byte @15H,
high byte @15G) → `PERRL1/PERRH1`, gated with LDS/UDS + `R/W0` (reads only) →
`RPERR1` → clocked into MERR FF (F74 @18F) on AS·T6. Cleared ONLY by `CLRMERR0`
(level-5 IACK). Also drives the red PERR LED (latched separately, cleared by
`CLRLED0` = reset/PMCL). `RPERR1` additionally arms the ND-100-bus `BERROR/PARERR`
drivers when the card is DMA master (sheet 2 @3A/16A/2B).

**OPCOM (level 6)** — LS74 FF @7F [S1 A5]: clocked by gate 7E = `BND031·OCW1`, i.e.
**every ND-100 control-word write with bit 3 = 1 sets the FF** (a latch, level output).
Cleared ONLY by `CLROPCOM0` (level-6 IACK). This is the ND-100 "start OPCOM" doorbell
[M:29].

**LEV7INT (level 7, NMI)** — PAL 455-02 (PAL16R4B) @13F [S1 F4], registered, clocked by
12CLK. Inputs include `RESET0 PLOW0 MERR0 OPCOM0 A01 MODCR0 LDS0 R/W0`. `PLOW0` comes
from gate 5F [S1 E1]: **`PLOWE1 AND MCL1`** — i.e. control-word bit 6 only ENABLES
power-low; the NMI actually fires when the ND-100 asserts Master Clear (BMCL) with the
enable set [M:18, M:29]. The same PAL implements the MODCR write register: `RAMMODE0`
(=EPROMMODE, EF0020) and `PARITYDIS0` (EF0022) from D00/A01/A02, plus `CONSPRES0`
conditioning. [INFERRED pin roles; register semantics confirmed M:25]

### 1.4 DTACK / BERR

- `DTACK` to the 68000 = AND (gate 13A) of `MDTACK0` (DRAM), `IDTACK0` (I/O decode),
  `PDTACK0` (**MFP's own DTACK output pin**, RN9 pull-up) [S1 F1].
- PAL 455-01 (PAL16R4B) @9F [S3 A4] sequences I/O cycles: `IORQ0 PALDTACK0 DAS0
  READY0` (DAS/READY are the LANCE bus handshake), inputs `A041 NONRAM1 BAS0 LANCE0
  PRI1 D001 DIVIOSEL0 R/W0`, clock 12CLK. [INFERRED]
- **Bus timeout**: LS165 shift register @12B [S1 E4-5]: when AS is asserted and no
  grant/request resolution (`NOREQ1·LANGR1` via 17B/16A) it shifts `H6`(=1) at
  ~6.25 MHz; after 8 stages Q7 → `BERR0`. ⇒ a hung 68000 bus cycle gets a bus error
  after ≈ 1.3 µs. (Clock label reads "6,?5CLK" — 6.25 MHz = 12.5/2 is the consistent
  reading; flagged uncertain.)

---

## 2. MFP (MK68901 @ 29G) connections [S3 D1-3]

| MFP pin | Net | Meaning |
|---|---|---|
| XTAL1 (17) | `3CLK1` | **3.125 MHz** timer clock (XTAL2 n.c.) |
| CLK (35) | `3CLK1` | bus clock, same 3.125 MHz |
| TDO (16) | `TERCLK1` | **Timer D output = USART baud clock**, looped into RC+TC |
| RC (10), TC | `TERCLK1` | USART rx/tx clock (÷16 mode set by firmware) |
| TAO/TBO/TCO | n.c. | Timers A/B/C have no external output |
| TAI (19), TBI (20) | n.c. | no external timer inputs |
| I0-I4 (22-26) | n.c. | unused GPIP inputs (confirmed [M:27] "4-0 not used") |
| I5 (27) | `LANERROR0` | **LANCE memory-cycle error**: protect violation, bus error, or address out of range during a LANCE DMA cycle [M:27] |
| I6 (28) | `NCINT0` | **ND-100 interrupt doorbell** = NAND(`BND021`,`OCW1`) via gate 8H [S1 E5] — a PULSE lasting the control-word write strobe, for every write with bit 2 = 1 |
| I7 (29) | `WRIV0` | **write-protect violation** = LANGR̄·AS·PROTA via 31H/27F [S3 E1] |
| SI (9) / SO | `RXDT1` / `TXDT1` | test-console current loop via HP4200/HP4100 optocouplers @14H/13H |
| RR (31) / TR (30) | `RREDY0` / `TREDY0` | ready pins → CONSOLEINTR level 4 (see 1.3) |
| RESET (21) | `PMCL0` | **MFP is hardware-reset by PMCL** (power-on MCL or ND-100 control bit 4 reset chain) |
| INTR (32) | `901INTR0` | → LS148 input 3 |
| IACK (45) | `IAK9010` | from PAL 453-00 |
| IEI (34) | tied high | sole interrupt controller (IEO n.c.) |
| DTACK | `PDTACK0` | one of the three 68000 DTACK sources |
| CS (40) | `CS9010` | from decoder Y6 chain [S3 B5] |
| DS (47) | `LDS0` | **odd addresses only** (manual: base EF00C1, odd displacements) |

MFP vector base: firmware writes VR=0x40 ⇒ vectors (octal) as in [M:28]:
117=GPIP7/WRIV, 116=GPIP6/NCINT, 114/113/112/111=USART rx-full/rx-err/tx-empty/tx-err,
107=GPIP5/LANERROR, 105=Timer C (RTC). Standard MK68901 channel numbering — the
schematic GPIP wiring reproduces the manual's table exactly.

---

## 3. 68000 I/O address decode [S1 16D, S3 A4-B5]

PAL 452-00 (TBP24S10 PROM @16D, sheet 1) decodes A(09-23) into `PROTS0 ROMS0
IOSPACEH0 IOSPACEL0 DRAMS0`. Both `IOSPACEL0` (EF00xx) and `IOSPACEH0` (EF01xx) drive
the two active-low enables of the F138 @106 ⇒ **EF01xx is a hardware mirror of EF00xx**
(manual: "decoded twice i.e. EF00XX = EF01XX, so that PIOC and Ethernet I software can
be used" [M:22]). There is NO device that exists only in EF01xx.

F138 @106 (A5-A7) outputs, each 32 bytes [M:22 Table 1]:

| Range (and EF01xx mirror) | Net | Register | R/W |
|---|---|---|---|
| EF00C0-FF | `CS9010` | MFP | R/W |
| EF00B8-BF | `ETHSTAT0` | hardware status: bit0=LAN interrupt (=LANINTR pin), bit2=power enable (=PWEN), **active when ZERO** (S244 @9E, sheet 5) | R |
| EF00B0-B7 | `LANRESET` | LANCE hardware reset (pulse; OR-ed with system reset) | W |
| EF00A8-AF | `XCVPW0` | transceiver 12V switch: D0 → PWEN FF @11E (sheet 5); force-cleared by `PWOFF0` (LM339 12V sense) | W |
| EF00A0-A7 | `LANCE0` | LANCE: RDP=EF00A0, RAP=EF00A2, 16-bit even accesses | R/W |
| EF0080-9F | `SCIP1` | **doorbell to ND-100**: any access clocks the RFT FF → INT12 (see §4) | W |
| EF0060-7F | `EAREN0` | read error address A1-16 (latches @25E/23D, sheet 4) | R |
| EF0040-5F | `MERRSTAT` (`SYREN0`) | read parity/error status word (format [M:24]: bit10 write-to-parity, 9/8 addr18/17, 7/6 NGACK/BGACK source code, 3/2 parity err hi/lo, 1/0 parity bit hi/lo) | R |
| EF0020-3F | `MODCR0` | mode registers via PAL 455-02: EF0020 EPROMMODE, EF0022 PARITYDIS, EF0024 BREAKMODE, EF0026 spare; cleared by RESET [M:25] | R/W |
| EF0010-1F | `PROFF1` | protection off | W |
| EF0000-0F | — | not used | — |

⇒ Consequences for emulation:
- EF0140/EF0160/EF01A0 are MERRSTAT / EAREN / LANCE-RDP, **not** timer registers.
- A **level-5 ISR reading EF0140+EF0160 is the memory-parity-error handler** reading
  MERRSTAT and the captured error address — not a timer ISR.
- There is **no timer device on the card other than the four MFP timers**.

---

## 4. ND-100 bus interface (interrupt part) [S2, S3]

### 4.1 Bus signal buffers (S240 @3B, sheet 2 E1-2)

`BMCL0→MCL1` (master clear), `BAPR0→BAPR1`, `BIOXE0→IOXE1/IOXE0`, `BINPUT0→INPUT1`,
`BINACK0→INACK1`, `BDAP0→BDAR1`, `BMEM0→BMEM1`. `INGRANT0` Ca23→Cc23 passes straight
through.

### 4.2 Device selection

- Two EECO thumbwheels (7J/9J, sheet 2) set the device number; F521 comparators
  @10B/7B compare the latched IOX address → `DEQL` (device equal). Address LSB
  (`LNA00`) is decoded by LS139 @5D [S3 D5] into `OCW0` (control-word write strobe,
  IOX odd offset) and `OSR0/OSR1` (status read strobe, IOX even offset). Address bit 1
  is NOT decoded ⇒ +0≡+2 (status), +1≡+3 (control) [M:29-30].
- A third thumbwheel @11J addresses two TBP18S030 PROMs (089-00 low byte, 089-01 high
  byte) which drive BD00-15 during `PIDENT0` ⇒ **the IDENT code comes from PROM,
  thumbwheel-selected** (positions 0-3 = idents 2240-2243₈, device addresses
  140360/140364/140370/140374₈).

### 4.3 Control-word register (write, IOX offset 1/3) [M:29]

Strobe `OCW1` clocks each of:

| Bit | Net | Storage | Effect |
|---|---|---|---|
| 0 | `BND001` → **RIE FF** (ALS74 @1B lower, [S3 F2]) | D-FF, re-captured on EVERY OCW | interrupt enable onto ND-100 bus; **R̄ = `CLINT0`** |
| 2 | `BND021` → gate 8H | none (pulse) | `NCINT0` pulse → MFP GPIP I6 (vector 116₈) — **re-fires on every write with bit2=1** |
| 3 | `BND031` → clocks OPCOM FF @7F | set-latch | 68k level 6; cleared by level-6 IACK (`CLROPCOM0`) |
| 4 | `BND041` → LS175 @"86" → `PRES0` | level | 68000 RESET chain (also resets MFP via `PMCL0` and LANCE); "RESET" LED |
| 5 | `BND051` → LS175 | level | 68000 HALT; "HALT" LED |
| 6 | `BND061` → LS175 → `PLOWE1` | level | power-low ENABLE; NMI fires on `PLOWE·MCL` (see §1.3) |
| 8 | `BND081` → LS175 → `CWDIS0` | level | disable parity check/write (`CWRP0` chain, sheet 1) |

### 4.4 INT12 generation (BINT12, sheet 3 E/F1-2)

```
SCIP access (68k) ──clk──> RFT FF (ALS74 @1B, D=1)         R̄ ── CLINT0
                                │ RFT1
control bit 0 ──D──> RIE FF ────┤                           R̄ ── CLINT0
        (clk = OCW)             │ RIE1
                                ▼
                        gate 2D:  INT121 = RFT AND RIE
                                │
                                ├── ALS38 o.c. @1C ──> Ca16 BINT120  (bus INT line, level 12)
                                └── INT121 ──> latched @9A on BAPR ──> LINT121 (ident claim)

CLINT0 = DCL0 (delayed master clear, ~200 µs after power-low [M:18])
         OR PIDENT0 (this card answers IDENT)          [gate 2D @S3 E1]
```

Key semantics (all byte-verified from the schematic):

1. **RFT is set by any 68000 access to the SCIP range regardless of RIE.** A doorbell
   fired while interrupts are disabled stays latched ("pending") and asserts BINT12
   the moment RIE is set. RFT survives control-word writes — **only** IDENT-answer,
   Delayed Clear (power fail / master clear) clear it.
2. **BINT12 (and status bit 2) = RFT·RIE**, not raw RFT. A pending-but-disabled
   doorbell is invisible in the status register.
3. **IDENT answer clears BOTH flip-flops** (RFT and RIE share `CLINT0`). After an
   IDENT the interrupt-enable bit reads back 0 until the driver writes the control
   word again.

### 4.5 IDENT daisy chain [S3 E1]

`INIDENT0` (Ca22) → gate 2A: if `LINT121` (latched pending) is set, assert `PIDENT0`
(answer: gate the PROM ident code onto BD, assert BDRY) and do NOT propagate;
otherwise pass to `OUTIDENT0` (Cc22). `PIDENT0` → `CLINT0` (clears RFT+RIE, drops
BINT12).

### 4.6 Status word (read, IOX offset 0/2) [M:30]

| Bit | Source |
|---|---|
| 15-8 | memory bank number (thumbwheel; bits 8-9 always 0) |
| 6 | 0 = memory is 512 KB |
| 5 | HALT state |
| 4 | RESET active |
| 2 | `INT12` = RFT·RIE (interrupt set for ND-100 on level 12) |
| 0 | `RIE` (interrupt enabled onto ND-100 bus) |

### 4.7 DMA-master error reporting [S2 F2]

When the card masters the ND-100 bus (`NGACK`) and read parity fails (`NPERR` =
RPERR·PARITYDIS̄), it asserts `BERROR0` (Cb21) + `PARERR0` (Cb18, o.c.) and drives an
error code on BD16/17/21 (S240 @3A).

---

## 5. Clocks and timers — complete inventory

| Source | Chain | Frequency | Consumers | Emulation relevance |
|---|---|---|---|---|
| 50 MHz XTAL osc @23C [S1 G4] | mux @22D (or `EXTERN0` test clock) → `50CLK0` | 50 MHz | ÷2 F112 @21D → `25CLK` 25 MHz → ÷2 → **`12CLK` 12.5 MHz** | 68000 CLK (T12CLK), PALs 453-00/455-01/455-02, DRAM ctl. **Print-G CPU clock is 12.5 MHz** (manual p.11 says 10 MHz — early print; MC68HC000-12 fitted) |
| ÷ from 12CLK | → **`3CLK` 3.125 MHz** [S1 note] | 3.125 MHz | **MFP XTAL1 + CLK** | MFP timer base — already fixed in emulator (C-4) |
| ÷ from 12CLK | `6.25CLK` (label partly illegible) | 6.25 MHz | BERR timeout shifter @12B (8 stages ⇒ ~1.3 µs) | optional: bus-error timeout |
| 40 MHz XTAL osc @17A [S5 F1] | ÷2 @20A (or `EXTLANCK0` test) → `SIA20CLK` | 20 MHz | AM7992B SIA → 10 Mb/s Manchester | fixed Ethernet bitrate; no emulation timer needed |
| **MFP Timer C** | prescale ÷100, TCDR=244 (firmware: TCDCR=0x50, TCDR=0xF4) | 3.125 MHz/100/244 ≈ **128.07 Hz** | RTC tick, vector 105₈ → `rtc_timer_isr` | the ONLY periodic system timer |
| **MFP Timer D** | firmware-programmed | baud ×16 | `TERCLK` → own USART RC/TC | console baud only |
| MFP Timers A, B | — | — | not used [M:27] | none |
| DL2181 delay line @27H [S1 D2] | taps | ns-range | DRAM CAS timing | none |
| RC on FF @8F (180k) [S1 E1] | `DCL0/DCL1` | ~200 µs after power-low [M:18] | Delayed Clear → `CLINT0` (clears SCIP/RFT+RIE) | reset semantics only |
| LANCE internal | — | 25.6 µs memory timeout [M:15] | LANCE CSR0.MERR | model via LANCE core |

**There is no AM9519, no "STC", and no timer controller chip anywhere on the card.**
The only timers are the MFP's four, of which C (RTC) and D (baud) are used.

---

## 6. Open/uncertain items

- PAL equations (452-00, 453-00, 455-01, 455-02, 454-00, 456-00) do not exist as
  dumps; all PAL behavior above is inferred from pin names, surrounding logic and the
  manual. Confidence is high for the IACK/clear pins, lower for PRI/IAKE/CLRREDY.
- The BERR-timeout clock label ("6,?5CLK") and the exact `CLINT0` gate inputs were
  read from a 300 dpi scan; both are consistent with the surrounding logic but a
  higher-resolution look at sheet 3 E1 / sheet 1 E4 would remove residual doubt.
- Ethernet III (324232) uses a completely different bus (MF bus) and is NOT covered
  here.
