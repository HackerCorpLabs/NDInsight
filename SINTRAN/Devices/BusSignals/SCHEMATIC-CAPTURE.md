# Schematic Capture Quick Reference

> **Purpose**: Single-page lookup for drawing the controller card schematic in KiCad. Every IC pin, every connector pin, every net name, every reference designator is fixed here so the schematic can be drawn top-to-bottom without flipping back through `CONTROLLER-DESIGN.md`.

> **Companion file**: `CONTROLLER-DESIGN.md` is the architecture and reasoning. **This** file is the practical bill-of-pins.

## Table of Contents

1. [PCB Stackup (2-layer)](#pcb-stackup-2-layer)
2. [Reference Designator Allocation](#reference-designator-allocation)
3. [Polarity Convention (CRITICAL)](#polarity-convention-critical)
4. [Net Naming Convention](#net-naming-convention)
5. [Olimex BB48R Header Pin Map (J2 = EXT1, J3 = EXT2)](#olimex-bb48r-header-pin-map-j2--ext1-j3--ext2)
6. [DIN 41612 Bus Connector (J1)](#din-41612-bus-connector-j1)
7. [Per-IC Pin Connection Tables](#per-ic-pin-connection-tables)
8. [Pi Zero Header (J4)](#pi-zero-header-j4)
9. [Power Section](#power-section)
10. [Pull Resistors and Decoupling](#pull-resistors-and-decoupling)
11. [Bill of Materials with Verified LCSC Numbers](#bill-of-materials-with-verified-lcsc-numbers)
12. [KiCad Library Setup](#kicad-library-setup)
13. [Schematic Capture Order (Suggested)](#schematic-capture-order-suggested)

---

## PCB Stackup (2-layer)

| Layer | Use |
|-------|-----|
| **Top (component side)** | All SMD components, primary signal routing, VCC traces |
| **Bottom (solder side)** | Continuous ground pour, a few crossover signal traces, power-rail traces where they cannot run on top |

**Strategy**:

- **Ground pour on the bottom layer** = continuous reference plane for all signals
- Star ground at the BB48R 3.3 V output (the BB48R has its own DCDC; everything downstream references its ground)
- **Power traces** (5V_BUS, 5V_USB, 5V_LOCAL, 3V3) run as wide top-layer traces (≥0.8 mm) — never as a thin trace through a sea of vias
- **DBUS 0-7** (8 lines, GPIO12-19 → latch chips) routed as a tight parallel bundle on top, all 8 traces same length, shortest possible (<30 mm)
- **BD 0-23** routed between latches and DIN 41612 connector on top, ≤50 mm each
- **Decoupling caps** placed within 2 mm of each IC's VCC pin on top, ground pad straight to the bottom plane via stitching vias
- **Avoid** running signals on the bottom layer except when crossing — keep the ground pour as continuous as possible
- **Trace widths**: 0.25 mm for signals (default), 0.5 mm for VCC/3V3, 1.0+ mm for 5V power
- **Drill / via**: 0.3 mm via, 0.6 mm pad — JLCPCB minimum

**JLCPCB order spec for the controller card**:

| Setting | Value |
|---------|-------|
| Layers | **2** |
| Dimensions | ~100 × 100 mm (final) |
| Material | FR-4 |
| Thickness | 1.6 mm |
| Surface finish | HASL (lead-free is fine) — ENIG is not needed for 2-layer |
| Min trace/space | 6/6 mil = 0.15 mm (JLCPCB stock) |
| Min hole | 0.3 mm |
| Solder mask | Green (cheapest) — any colour works |
| Silkscreen | White |
| Quantity | 5 or 10 |
| Assembly | SMT only (top side). Through-hole connectors hand-soldered. |

---

## Reference Designator Allocation

Fix these names in KiCad **before** placing any components. Once they are stable, the BOM, CPL, and PCB silkscreen all line up.

### Connectors (J)

| RefDes | Part | Function |
|--------|------|----------|
| **J1** | DIN 41612 Type C, 96-pin male, right-angle (3 rows × 32) | ND-100 backplane bus connector |
| **J2** | 2x27 female header, 0.1" pitch (sockets the BB48R **EXT1** side) | BB48R EXT1 (GPIO0-23 + power) |
| **J3** | 2x27 female header, 0.1" pitch (sockets the BB48R **EXT2** side) | BB48R EXT2 (GPIO24-47 + VBUS/VSYS) |
| **J4** | 2x20 male header, 0.1" pitch | Pi Zero 40-pin GPIO header |
| **J5** | 1x3 male header, 0.1" (2-pin shunt) | MODE_SELECT jumper (CPU mode / device mode) |
| **J6** | 1x4 male header, 0.1" (optional) | SWD breakout (BB48R has SWD on its top — only needed if you want SWD off-module) |

### Integrated Circuits (U)

| RefDes | Part | Package | Function |
|--------|------|---------|----------|
| **U1** | 74LVC574 | SOIC-20 | **Input latch** BD0-7 (CLK = /BAPR via U10 ch.1; per-chip OE = OE_IN_0_n) |
| **U2** | 74LVC574 | SOIC-20 | **Input latch** BD8-15 (per-chip OE = OE_IN_1_n) |
| **U3** | 74LVC574 | SOIC-20 | **Input latch** BD16-23 (per-chip OE = OE_IN_2_n) |
| **U4** | 74LVT245 | SOIC-20 | **Output driver** BD0-7 (3.3 V → 5 V; DIR fixed HIGH; OE = BD_OE_BUS_n) |
| **U5** | 74LVT245 | SOIC-20 | **Output driver** BD8-15 |
| **U6** | 74LVT245 | SOIC-20 | **Output driver** BD16-23 |
| **U7** | 74LVC574 | SOIC-20 | **Output latch** BD0-7 (CLK = LATCH0; OE tied LOW = always-driving into U4 A-side) |
| **U8** | 74LVC574 | SOIC-20 | **Output latch** BD8-15 (CLK = LATCH1) |
| **U9** | 74LVC574 | SOIC-20 | **Output latch** BD16-23 (CLK = LATCH2) |
| **U10** | 74LVC14 | SOIC-14 | Schmitt **inverter** for input sniffs (ch 1: /BAPR + latch CLK; ch 2-6: /BIOXE, /BDAP, /BDRY, /BMEM, /BINACK) |
| **U11** | 74LVC14 | SOIC-14 | Schmitt **inverter** for input sniffs (ch 1-2: /BMCL, /BINPUT; ch 3-6 spare) |
| **U12** | 74LVC06 | SOIC-14 | Open-drain **inverter** for output drive (ch 1-6: BAPR_OUT, BDRY_OUT, BINPUT_OUT, BDAP_OUT, BREQ, BINT 10) |
| **U13** | 74LVC06 | SOIC-14 | Open-drain **inverter** for output drive (ch 1-3: BINT 11, BINT 12, BINT 13; ch 4-6 spare) |
| **U14** | 74LVC125 | SOIC-14 | IDENT/GRANT daisy-chain pass-through (**non-inverting**, 2 channels of 4 used) |
| **U15** | LTC4412 | SOT-23-6 | Ideal diode controller for Pi Zero +5 V source-OR |

### IC count: 15 chips total

| Group | Count | Parts |
|-------|-------|-------|
| BD input latches | 3 | 74LVC574 (U1-U3) |
| BD output drivers | 3 | 74LVT245 (U4-U6) |
| BD output latches | 3 | 74LVC574 (U7-U9) |
| Control signal level shifters (input) | 2 | 74LVC14 (U10-U11) |
| Control signal level shifters (output) | 2 | 74LVC06 (U12-U13) |
| Daisy-chain pass-through | 1 | 74LVC125 (U14) |
| Pi Zero power management | 1 | LTC4412 (U15) |

> **Note**: The Olimex BB48R itself is a *module*, not a chip on this PCB. It plugs into J2/J3 sockets. The BB48R does not get a U-number — it is treated as a daughterboard.

### Diodes (D)

| RefDes | Part | Package | Function |
|--------|------|---------|----------|
| **D1** | SS14 Schottky | SMA (DO-214AC) | Source OR-ing for BB48R VBUS (USB-C OR bus 5 V) |
| **D2** | (LTC4412 + external PMOS) | -- | Pi Zero ideal diode (managed by U15) |
| **D3** | SMBJ5.0A TVS | SMB | Transient suppressor on +5V_LOCAL |
| **D4** | (slot for second TVS if needed) | SMB | Reserved |

### Polyfuses / Resistors / Caps (F, R, C)

| Range | Function |
|-------|----------|
| **F1** | 2 A polyfuse on bus 5 V input (1812 SMD) |
| **F2** | 2 A polyfuse on +5 V to Pi Zero (1812 SMD) |
| **R1-R20** | Pull-up / pull-down resistors (see [Pull Resistors](#pull-resistors-and-decoupling)) |
| **R21-R40** | LED current limiters (1 kΩ 0603) |
| **C1-C2** | Bus 5 V input bulk (47 µF 1210 + 0.1 µF 0603) |
| **C3** | +5V_LOCAL TVS node (10 µF 0805) |
| **C4-C5** | BB48R 3 V3 output bulk (10 µF + 0.1 µF) |
| **C6** | Pi Zero bulk 1000 µF aluminum polymer |
| **C7** | Pi Zero 470 µF tantalum |
| **C8** | Pi Zero 10 µF mid-frequency |
| **C9** | Pi Zero 0.1 µF HF |
| **C10-C25** | Per-IC decoupling (0.1 µF 0603, one per IC) |
| **C26-C30** | Distributed bulk decoupling (10 µF 0805) |

### LEDs

| RefDes | Colour | Function |
|--------|--------|----------|
| **LED1** | Green | +3.3 V present (BB48R DCDC output) |
| **LED2** | Yellow | Bus +5 V present |
| **LED3** | Yellow | USB +5 V present |
| **LED4** | Yellow | Pi Zero +5 V present (downstream of U15) |
| **LED5** | Blue | Status / heartbeat (driven by BB48R GPIO -- pulse 1 Hz) |
| **LED6-9** | Red | /BINT 10, 11, 12, 13 activity (optional, populate only on debug boards) |

---

## Polarity Convention (CRITICAL)

> **Read this section before drawing anything.** Getting polarity wrong will silently break the bus.

The ND-100 bus is **active LOW**: signals idle HIGH (~5 V) and assert LOW (~0 V). All level shifters on this card **invert** as they cross the 5 V ↔ 3 V3 boundary so the BB48R sees a clean **active-HIGH** world. This is intentional and uniform across **every** signal:

| Side | Voltage | Polarity | "Asserted" means |
|------|---------|----------|------------------|
| **Bus side** (5 V, faces J1) | 5 V CMOS | Active LOW | Voltage is LOW |
| **BB48R side** (3.3 V, GPIO) | 3.3 V CMOS | **Active HIGH** | Bit value is **1** |

### Why inversion is mandatory

1. **74LVC574 latch CLK**: needs a **rising** edge on the CLK pin to capture data. Bus /BAPR *falls* when an address is presented. The inverter is **structurally required** to give the latch a rising edge at the right moment.
2. **PIO `WAIT 1 PIN`** instructions are idiomatic and natural with active-high signals.
3. **Mask compare** in PIO: any non-zero bit in the trigger group means "something is asserted" -- trivial to test.
4. **Firmware reads** as `if (gpio & MASK)` rather than `if (!(gpio & MASK))`.
5. **Output drives** are simpler: BB48R writes **1** to assert, **0** to release. Open-drain inverter (74LVC06) handles the inversion automatically.
6. **Reset-safe state**: BB48R GPIOs reset to inputs with internal pull-down. The 74LVC06 input then sees 0, output is high-Z, bus floats HIGH (idle). Safe.

### Polarity rules

| Net suffix | Side | Polarity |
|------------|------|----------|
| `*_BUS` | 5 V | Active LOW (matches bus) |
| `*_3V3`, `*_IN_3V3`, `*_OUT_3V3` | 3.3 V (BB48R GPIO) | **Active HIGH** (1 = asserted) |
| `OE_*_n`, `BD_OE_BUS_n` | 3.3 V (internal control) | Active LOW (the `_n` suffix marks them) |

> The `_n` suffix is **only** used for internal control signals where active-LOW makes more sense (output enable pins, latch enables that are asserted-LOW). Bus signals after inversion drop the `_n` because they are active-HIGH after the level shifter.

### What about timing diagrams?

ASCII timing diagrams in this document and `CONTROLLER-DESIGN.md` show **bus-side** signals (5 V) with the active-LOW convention -- HIGH idle, drop LOW to assert. This matches the ND reference manuals and the physical bus. The BB48R GPIO state is **inverted** from the bus state and should be drawn separately if needed.

---

## Net Naming Convention

Every signal that crosses a level shifter has **two distinct nets** -- one on the 5 V bus side and one on the 3.3 V MCU side. Use the suffix to disambiguate. **Polarity is given by the side, per the table above.**

### Suffix convention

| Suffix | Meaning |
|--------|---------|
| `_BUS` | 5 V side, faces the DIN 41612 connector |
| `_3V3` | 3.3 V side, faces the BB48R |
| `_IN` | "Sniff" net entering the BB48R (read-only path) |
| `_OUT` | Net leaving the BB48R toward the bus (drive path) |
| (no suffix) | Power, ground, or signals that exist only on one voltage |

### Bus data lines

| 5 V net | 3.3 V net | Notes |
|---------|-----------|-------|
| `BD0_BUS` … `BD23_BUS` | `BD0_3V3` … `BD23_3V3` | The latch chips (U1-U6) cross the level boundary |

### Bus control signals

| 5 V net | 3.3 V net | Direction |
|---------|-----------|-----------|
| `BAPR_BUS` | `BAPR_IN_3V3` (sniff) and `BAPR_OUT_3V3` (drive) | Bidir |
| `BIOXE_BUS` | `BIOXE_IN_3V3` | In only |
| `BDAP_BUS` | `BDAP_IN_3V3` and `BDAP_OUT_3V3` | Bidir |
| `BDRY_BUS` | `BDRY_IN_3V3` and `BDRY_OUT_3V3` | Bidir |
| `BMEM_BUS` | `BMEM_IN_3V3` | In only |
| `BINPUT_BUS` | `BINPUT_IN_3V3` and `BINPUT_OUT_3V3` | Bidir |
| `BINACK_BUS` | `BINACK_IN_3V3` | In only |
| `BMCL_BUS` | `BMCL_IN_3V3` | In only |
| `BREQ_BUS` | `BREQ_OUT_3V3` | Out only |

### Interrupts

| 5 V net | 3.3 V net |
|---------|-----------|
| `BINT10_BUS` | `BINT10_OUT_3V3` |
| `BINT11_BUS` | `BINT11_OUT_3V3` |
| `BINT12_BUS` | `BINT12_OUT_3V3` |
| `BINT13_BUS` | `BINT13_OUT_3V3` |

### Daisy chains

| 5 V net | 3.3 V net |
|---------|-----------|
| `INIDENT_BUS` | `INIDENT_3V3` |
| `OUTIDENT_BUS` | `OUTIDENT_3V3` |
| `INGRANT_BUS` | `INGRANT_3V3` |
| `OUTGRANT_BUS` | `OUTGRANT_3V3` |
| `INCONTR_BUS` | (not used in V1 -- pad it through to OUTCONTR with a 0 Ω) |
| `OUTCONTR_BUS` | (not used in V1) |

### MCU-internal control signals (3.3 V only, no level shifter)

| Net | Function |
|-----|----------|
| `OE_IN_0_n`, `OE_IN_1_n`, `OE_IN_2_n` | Read-enable for input latches U1, U2, U3 (active LOW; per-chip select for the read path) |
| `LATCH0`, `LATCH1`, `LATCH2` | CLK for output latches U7, U8, U9 (rising-edge captures the byte from DBUS) |
| `BD_OE_BUS_n` | Master output enable for U4, U5, U6 output drivers (active LOW = drive bus) |
| `OBUF{0,1,2}_{0..7}` | Internal nets between output latch Q outputs (U7/U8/U9) and output driver A inputs (U4/U5/U6) |
| `OE_DAISY_IDENT_n` | Disable U14 IDENT pass-through (active LOW = high-Z, capture mode) |
| `OE_DAISY_GRANT_n` | Disable U14 GRANT pass-through (active LOW = high-Z, capture mode) |
| `INT_BB48R` | Pi Zero handshake out (active HIGH = "I have data") |
| `INT_FROM_ZERO` | Pi Zero handshake in (active HIGH = "I have data") |
| `MODE_SELECT` | CPU mode jumper (HIGH = device mode, LOW = CPU mode) |

### Power nets

| Net | Voltage | Source |
|-----|---------|--------|
| `5V_BUS` | +5 V | DIN 41612 J1 (rows A/B/C pin 2 and 31) |
| `5V_USB` | +5 V | BB48R VBUS (J3 pin 1) -- present only when USB-C is plugged in |
| `5V_LOCAL` | +5 V | Output of D1 (Schottky OR) -- powers BB48R, latches, level shifters |
| `5V_PIZERO` | +5 V | Output of U15 (LTC4412 ideal diode) -- powers Pi Zero only |
| `3V3` | +3.3 V | BB48R J2 pin 3 -- powers all 3.3 V logic on the controller card |
| `GND` | 0 V | All ground returns |

---

## Olimex BB48R Header Pin Map (J2 = EXT1, J3 = EXT2)

The BB48R is a daughterboard. It plugs into **two female sockets** on the controller PCB. **J2 sockets EXT1 (left edge of the module)** and **J3 sockets EXT2 (right edge)**. Each socket is a **2x27 female header** at 0.1" pitch (single-row by single-row, 27 pins per row, 0.6" row spacing). The pin numbering below matches the Olimex user manual.

### J2 = BB48R EXT1 socket (left edge)

| J2 Pin | BB48R label | GPIO | Controller use | Net name |
|--------|-------------|------|----------------|----------|
| 1 | 3V3_EN | -- | Tie HIGH (default on) | `3V3_EN` (or leave floating, has internal pull-up) |
| 2 | GND | -- | Ground | `GND` |
| 3 | +3.3V | -- | **3.3 V output from BB48R DCDC -- powers all 3.3 V logic** | `3V3` |
| 4 | GPIO0 (UART0_TX) | GPIO0 | Pi Zero handshake → Pi Zero | `INT_BB48R` |
| 5 | GPIO1 (UART0_RX) | GPIO1 | Pi Zero handshake ← Pi Zero | `INT_FROM_ZERO` |
| 6 | GPIO2 (I2C1_SDA) | GPIO2 | /BINT 12 drive (open-drain via U9) | `BINT12_OUT_3V3` |
| 7 | GPIO3 (I2C1_SCL) | GPIO3 | /BINT 13 drive (open-drain via U9) | `BINT13_OUT_3V3` |
| 8 | GPIO4 (SPI0_RX/MISO) | GPIO4 | Pi Zero SPI MISO | `SPI_MISO` |
| 9 | GPIO5 (SPI0_CSn) | GPIO5 | Pi Zero SPI CS | `SPI_CSn` |
| 10 | GPIO6 (SPI0_SCK) | GPIO6 | Pi Zero SPI SCK | `SPI_SCK` |
| 11 | GPIO7 (SPI0_TX/MOSI) | GPIO7 | Pi Zero SPI MOSI | `SPI_MOSI` |
| 12 | GPIO8 (QMI_CS1n) | GPIO8 | **MODULE-RESERVED PSRAM CS** | (no connect) |
| 13 | GPIO9 (SPI1_CSn) | GPIO9 | **MODULE-RESERVED SD CS** | (no connect) |
| 14 | GPIO10 (SPI1_SCK) | GPIO10 | **MODULE-RESERVED SD CLK** | (no connect) |
| 15 | GPIO11 (SPI1_TX) | GPIO11 | **MODULE-RESERVED SD CMD** | (no connect) |
| 16 | GPIO12 | GPIO12 | DBUS 0 (shared 8-bit MCU↔latch bus) | `DBUS0` |
| 17 | GPIO13 | GPIO13 | DBUS 1 | `DBUS1` |
| 18 | GPIO14 | GPIO14 | DBUS 2 | `DBUS2` |
| 19 | GPIO15 | GPIO15 | DBUS 3 | `DBUS3` |
| 20 | GPIO16 | GPIO16 | DBUS 4 | `DBUS4` |
| 21 | GPIO17 | GPIO17 | DBUS 5 | `DBUS5` |
| 22 | GPIO18 | GPIO18 | DBUS 6 | `DBUS6` |
| 23 | GPIO19 | GPIO19 | DBUS 7 | `DBUS7` |
| 24 | GPIO20 | GPIO20 | /BAPR sniff (PIO trigger bit 8) | `BAPR_IN_3V3` |
| 25 | GPIO21 | GPIO21 | /BIOXE sniff (PIO trigger bit 9) | `BIOXE_IN_3V3` |
| 26 | GPIO22 | GPIO22 | /BDAP sniff (PIO trigger bit 10) | `BDAP_IN_3V3` |
| 27 | GPIO23 | GPIO23 | /BDRY sniff (PIO trigger bit 11) | `BDRY_IN_3V3` |

### J3 = BB48R EXT2 socket (right edge)

| J3 Pin | BB48R label | GPIO | Controller use | Net name |
|--------|-------------|------|----------------|----------|
| 1 | VBUS | -- | +5 V from BB48R USB-C (output when USB plugged in) | `5V_USB` |
| 2 | VSYS | -- | BB48R DCDC input (5V_LOCAL → BB48R via D1) | `5V_LOCAL` |
| 3 | GND | -- | Ground | `GND` |
| 4 | GPIO24 (SPI1_RX) | GPIO24 | **MODULE-RESERVED SD DAT0** | (no connect) |
| 5 | GPIO25 (User_Led) | GPIO25 | **MODULE-RESERVED on-board LED** -- can still be driven as status output | (BB48R-internal) |
| 6 | GPIO26 | GPIO26 | /OE_IN_0 (read-enable input latch U1) | `OE_IN_0_n` |
| 7 | GPIO27 | GPIO27 | /OE_IN_1 (read-enable input latch U2) | `OE_IN_1_n` |
| 8 | GPIO28 | GPIO28 | /OE_IN_2 (read-enable input latch U3) | `OE_IN_2_n` |
| 9 | GPIO29 | GPIO29 | LATCH0 (CLK for output latch U7, BD0-7) | `LATCH0` |
| 10 | GPIO30 | GPIO30 | LATCH1 (CLK for output latch U8, BD8-15) | `LATCH1` |
| 11 | GPIO31 | GPIO31 | LATCH2 (CLK for output latch U9, BD16-23) | `LATCH2` |
| 12 | GPIO32 | GPIO32 | /BD_OE_BUS (master OE for U4/U5/U6 output drivers) | `BD_OE_BUS_n` |
| 13 | GPIO33 | GPIO33 | /BMEM sniff (via U7) | `BMEM_IN_3V3` |
| 14 | GPIO34 | GPIO34 | /BINACK sniff (via U7) | `BINACK_IN_3V3` |
| 15 | GPIO35 | GPIO35 | /BMCL sniff (via U7) | `BMCL_IN_3V3` |
| 16 | GPIO36 | GPIO36 | /BINPUT sniff (via U7) | `BINPUT_IN_3V3` |
| 17 | GPIO37 | GPIO37 | /INGRANT sniff (via U7) | `INGRANT_IN_3V3` |
| 18 | GPIO38 | GPIO38 | /INIDENT sniff (via U7) | `INIDENT_IN_3V3` |
| 19 | GPIO39 | GPIO39 | /BINT 10 drive (open-drain via U8) | `BINT10_OUT_3V3` |
| 20 | GPIO40 | GPIO40 | /BINT 11 drive (open-drain via U8) | `BINT11_OUT_3V3` |
| 21 | GPIO41 | GPIO41 | /BAPR_OUT drive (open-drain via U8) | `BAPR_OUT_3V3` |
| 22 | GPIO42 | GPIO42 | /BDRY_OUT drive (open-drain via U8) | `BDRY_OUT_3V3` |
| 23 | GPIO43 | GPIO43 | /BINPUT_OUT drive (open-drain via U8) | `BINPUT_OUT_3V3` |
| 24 | GPIO44 | GPIO44 | /BDAP_OUT drive (open-drain via U8) | `BDAP_OUT_3V3` |
| 25 | GPIO45 | GPIO45 | /BREQ drive (open-drain via U8) | `BREQ_OUT_3V3` |
| 26 | GPIO46 | GPIO46 | /OE_DAISY_GRANT (controls U10) | `OE_DAISY_GRANT_n` |
| 27 | GPIO47 | GPIO47 | /OE_DAISY_IDENT (controls U10) | `OE_DAISY_IDENT_n` |

### SWD (top of BB48R, separate 3-pin header)

The BB48R exposes SWDIO/SWCLK on a 3-pin header on top of the module (SWD1). You normally do **not** need to wire this on the controller card -- you can attach a SWD probe directly to the BB48R while it sits in the socket. Only add J6 if you want SWD permanently routed.

| SWD pin | Signal | Net |
|---------|--------|-----|
| 1 | SWDIO | (only if J6 populated) |
| 2 | SWCLK | (only if J6 populated) |
| 3 | GND | `GND` |

---

## DIN 41612 Bus Connector (J1)

96 pins total: 3 rows (A, B, C) × 32 pins per row. **Right-angle male, vertical THT** so the controller card plugs horizontally into the backplane sockets.

### Row A (32 pins)

| Pin | Signal | Net | Notes |
|-----|--------|-----|-------|
| 1 | GND | `GND` | |
| 2 | +5V | `5V_BUS` | Bus power |
| 3 | BD 1 | `BD1_BUS` | |
| 4 | BD 3 | `BD3_BUS` | |
| 5 | BD 5 | `BD5_BUS` | |
| 6 | BD 7 | `BD7_BUS` | |
| 7 | BD 9 | `BD9_BUS` | |
| 8 | BD 11 | `BD11_BUS` | |
| 9 | BD 13 | `BD13_BUS` | |
| 10 | BD 15 | `BD15_BUS` | |
| 11 | GND | `GND` | |
| 12 | BREF | `BREF_BUS` | Memory refresh -- input only on a controller card (not used by us) |
| 13 | PA 1 | `PA1` | Slot position bit 1 (read by BB48R via U7 sniff if needed) |
| 14 | PA 3 | `PA3` | Slot position bit 3 |
| 15 | BINT 10 | `BINT10_BUS` | Wired-OR interrupt level 10 |
| 16 | BINT 12 | `BINT12_BUS` | Wired-OR interrupt level 12 |
| 17 | PANREQ | `PANREQ_BUS` | Panel request -- not used on a device card |
| 18 | BINPUT | `BINPUT_BUS` | Bidir |
| 19 | BDRY | `BDRY_BUS` | Bidir |
| 20 | BAPR | `BAPR_BUS` | Bidir |
| 21 | INCONTR | `INCONTR_BUS` | Daisy-chain in (future) |
| 22 | INIDENT | `INIDENT_BUS` | Daisy-chain in |
| 23 | INGRANT | `INGRANT_BUS` | Daisy-chain in |
| 24 | GND | `GND` | |
| 25 | +15V | (not used on a digital card) | |
| 26 | An.Return | (not used) | |
| 27 | -15V | (not used) | |
| 28 | +12V | (not used on most digital cards) | |
| 29 | POW.SENSE | (not used) | |
| 30 | 5V St.by | (not used) | |
| 31 | +5V | `5V_BUS` | |
| 32 | GND | `GND` | |

### Row B (32 pins)

| Pin | Signal | Net | Notes |
|-----|--------|-----|-------|
| 1 | GND | `GND` | |
| 2 | +5V | `5V_BUS` | |
| 3 | BD 16 | `BD16_BUS` | |
| 4 | BD 17 | `BD17_BUS` | |
| 5 | BD 18 | `BD18_BUS` | |
| 6 | BD 19 | `BD19_BUS` | |
| 7 | BD 20 | `BD20_BUS` | |
| 8 | BD 21 | `BD21_BUS` | |
| 9 | BD 22 | `BD22_BUS` | |
| 10 | BD 23 | `BD23_BUS` | |
| 11 | GND | `GND` | |
| 12 | LOAD | `LOAD_BUS` | CPU-crate-only -- not used on a device card |
| 13 | RESTART | `RESTART_BUS` | CPU-crate-only |
| 14 | RUN | `RUN_BUS` | CPU-crate-only |
| 15 | CONTINUE | `CONTINUE_BUS` | CPU-crate-only |
| 16 | STOP | `STOP_BUS` | CPU-crate-only |
| 17 | BLANK | (not used) | |
| 18 | BPERR | (memory cards only) | |
| 19 | BINACK | `BINACK_BUS` | In only |
| 20 | BMCL | `BMCL_BUS` | In only -- master clear |
| 21 | BERROR | (future) | |
| 22 | BCRQ | (future) | |
| 23 | BMINH | (memory only) | |
| 24 | GND | `GND` | |
| 25-30 | (analog rails / power sense / standby) | (not used) | |
| 31 | +5V | `5V_BUS` | |
| 32 | GND | `GND` | |

### Row C (32 pins)

| Pin | Signal | Net | Notes |
|-----|--------|-----|-------|
| 1 | GND | `GND` | |
| 2 | +5V | `5V_BUS` | |
| 3 | BD 0 | `BD0_BUS` | |
| 4 | BD 2 | `BD2_BUS` | |
| 5 | BD 4 | `BD4_BUS` | |
| 6 | BD 6 | `BD6_BUS` | |
| 7 | BD 8 | `BD8_BUS` | |
| 8 | BD 10 | `BD10_BUS` | |
| 9 | BD 12 | `BD12_BUS` | |
| 10 | BD 14 | `BD14_BUS` | |
| 11 | GND | `GND` | |
| 12 | BREQ | `BREQ_BUS` | Wired-OR DMA request |
| 13 | PA 0 | `PA0` | Slot position bit 0 |
| 14 | PA 2 | `PA2` | Slot position bit 2 |
| 15 | BINT 11 | `BINT11_BUS` | |
| 16 | BINT 13 | `BINT13_BUS` | |
| 17 | BINT 15 | (highest priority -- not used by our cards) | |
| 18 | BDAP | `BDAP_BUS` | Bidir |
| 19 | BIOXE | `BIOXE_BUS` | In only -- IO execute strobe |
| 20 | BMEM | `BMEM_BUS` | In only -- memory cycle indicator |
| 21 | OUTCONTR | `OUTCONTR_BUS` | Daisy-chain out (future) |
| 22 | OUTIDENT | `OUTIDENT_BUS` | Daisy-chain out |
| 23 | OUTGRANT | `OUTGRANT_BUS` | Daisy-chain out |
| 24 | GND | `GND` | |
| 25-30 | (analog rails / power sense / standby) | (not used) | |
| 31 | +5V | `5V_BUS` | |
| 32 | GND | `GND` | |

---

## Per-IC Pin Connection Tables

### U1, U2, U3 — 74LVC574 Input Latch (×3, SOIC-20)

The 74LVC574 is an octal positive-edge-triggered D flip-flop with 3-state outputs. **Inputs are 5 V tolerant** at 3.3 V VCC. CLK is shared (driven by /BAPR buffered through U7), OE is the per-chip read enable.

Pinout (SOIC-20 standard):

| Pin | Name | U1 (BD0-7) | U2 (BD8-15) | U3 (BD16-23) |
|-----|------|------------|-------------|--------------|
| 1 | OE (active LOW) | `OE_IN_0_n` | `OE_IN_1_n` | `OE_IN_2_n` |
| 2 | D0 | `BD0_BUS` | `BD8_BUS` | `BD16_BUS` |
| 3 | D1 | `BD1_BUS` | `BD9_BUS` | `BD17_BUS` |
| 4 | D2 | `BD2_BUS` | `BD10_BUS` | `BD18_BUS` |
| 5 | D3 | `BD3_BUS` | `BD11_BUS` | `BD19_BUS` |
| 6 | D4 | `BD4_BUS` | `BD12_BUS` | `BD20_BUS` |
| 7 | D5 | `BD5_BUS` | `BD13_BUS` | `BD21_BUS` |
| 8 | D6 | `BD6_BUS` | `BD14_BUS` | `BD22_BUS` |
| 9 | D7 | `BD7_BUS` | `BD15_BUS` | `BD23_BUS` |
| 10 | GND | `GND` | `GND` | `GND` |
| 11 | CLK | `BAPR_BUS` (via U7 buffered) | `BAPR_BUS` (via U7 buffered) | `BAPR_BUS` (via U7 buffered) |
| 12 | Q7 | `DBUS7` | `DBUS7` | `DBUS7` |
| 13 | Q6 | `DBUS6` | `DBUS6` | `DBUS6` |
| 14 | Q5 | `DBUS5` | `DBUS5` | `DBUS5` |
| 15 | Q4 | `DBUS4` | `DBUS4` | `DBUS4` |
| 16 | Q3 | `DBUS3` | `DBUS3` | `DBUS3` |
| 17 | Q2 | `DBUS2` | `DBUS2` | `DBUS2` |
| 18 | Q1 | `DBUS1` | `DBUS1` | `DBUS1` |
| 19 | Q0 | `DBUS0` | `DBUS0` | `DBUS0` |
| 20 | VCC | `3V3` | `3V3` | `3V3` |

> **CLK input**: All 3 latches share the same /BAPR clock. Wire `BAPR_BUS` from J1 pin A20 → into U7 (74LVC14) for clean 5V→3V3 buffering, then the 74LVC14 output (`BAPR_3V3_BUFFERED`) goes to pin 11 of U1, U2, **and** U3 in parallel.

> **Data outputs (Q0-Q7)**: All three latches share the same DBUS0-7 net. Only one /OE_IN_n is asserted at a time, so only one latch drives the shared bus -- this is by design.

> **Decoupling**: One 0.1 µF cap (C10, C11, C12) per chip, between pin 20 (VCC) and pin 10 (GND), placed within 2 mm of the chip.

### U4, U5, U6 — 74LVT245 Output Driver (×3, SOIC-20)

74LVT245 is an octal bus transceiver with **3.3 V → 5 V level translation** and high drive (32 mA per pin). **DIR is fixed HIGH** (tied to 3V3) so the chip is one-way A → B (latch outputs → bus). All three OE pins are tied to a single net `BD_OE_BUS_n` so all 24 BD lines drive simultaneously.

The A-side is **NOT** driven directly from DBUS — it is fed by the output latches U7/U8/U9 (see below). This lets us load all 24 bits into the latches first (3 sequential byte writes), then drop `BD_OE_BUS_n` to put the full 24-bit value on the bus in one atomic step.

Pinout (SOIC-20):

| Pin | Name | U4 (BD0-7) | U5 (BD8-15) | U6 (BD16-23) |
|-----|------|------------|-------------|--------------|
| 1 | DIR | `3V3` (tied HIGH = A→B) | `3V3` | `3V3` |
| 2 | A0 | `OBUF0_0` (Q0 of U7) | `OBUF1_0` (Q0 of U8) | `OBUF2_0` (Q0 of U9) |
| 3 | A1 | `OBUF0_1` | `OBUF1_1` | `OBUF2_1` |
| 4 | A2 | `OBUF0_2` | `OBUF1_2` | `OBUF2_2` |
| 5 | A3 | `OBUF0_3` | `OBUF1_3` | `OBUF2_3` |
| 6 | A4 | `OBUF0_4` | `OBUF1_4` | `OBUF2_4` |
| 7 | A5 | `OBUF0_5` | `OBUF1_5` | `OBUF2_5` |
| 8 | A6 | `OBUF0_6` | `OBUF1_6` | `OBUF2_6` |
| 9 | A7 | `OBUF0_7` | `OBUF1_7` | `OBUF2_7` |
| 10 | GND | `GND` | `GND` | `GND` |
| 11 | B7 | `BD7_BUS` | `BD15_BUS` | `BD23_BUS` |
| 12 | B6 | `BD6_BUS` | `BD14_BUS` | `BD22_BUS` |
| 13 | B5 | `BD5_BUS` | `BD13_BUS` | `BD21_BUS` |
| 14 | B4 | `BD4_BUS` | `BD12_BUS` | `BD20_BUS` |
| 15 | B3 | `BD3_BUS` | `BD11_BUS` | `BD19_BUS` |
| 16 | B2 | `BD2_BUS` | `BD10_BUS` | `BD18_BUS` |
| 17 | B1 | `BD1_BUS` | `BD9_BUS` | `BD17_BUS` |
| 18 | B0 | `BD0_BUS` | `BD8_BUS` | `BD16_BUS` |
| 19 | OE (active LOW) | `BD_OE_BUS_n` | `BD_OE_BUS_n` | `BD_OE_BUS_n` |
| 20 | VCC | `3V3` | `3V3` | `3V3` |

> **DIR**: tied permanently to `3V3` via a 0 Ω jumper or direct trace. Never bring DIR out as a controllable signal -- the latches always feed the bus, never the other way around.

> **Default state**: pull-up R3 (10 kΩ to 3V3) keeps `BD_OE_BUS_n` HIGH = U4/U5/U6 high-Z = controller card does not drive the bus. Reset-safe.

> **Polarity**: 74LVT245 is **non-inverting**. The data on the A side is what appears on the B side (5 V level). The PIO pre-inverts the BD data (XOR with 0xFF) before writing to the output latches, so the bus sees the correct negative-logic representation. See the **Polarity Convention** section.

> **Decoupling**: One 0.1 µF cap (C13, C14, C15) per chip, between pin 20 (VCC) and pin 10 (GND).

### U7, U8, U9 — 74LVC574 Output Latch (×3, SOIC-20)

Three octal D flip-flops sitting between the shared 8-bit DBUS and the A-side inputs of the output drivers (U4/U5/U6). Each latch captures one byte on the rising edge of its individual `LATCHn` clock. After all three latches are loaded, the BB48R drops `BD_OE_BUS_n` and U4/U5/U6 drive all 24 bits onto the bus simultaneously.

OE is **tied LOW** (always enabled) -- the latch outputs are always active and feeding the 74LVT245 A-side. The "drive vs no-drive" decision is made at the 74LVT245 OE, not here.

Pinout (SOIC-20):

| Pin | Name | U7 (BD0-7) | U8 (BD8-15) | U9 (BD16-23) |
|-----|------|------------|-------------|--------------|
| 1 | OE (active LOW) | `GND` (tied LOW = always enabled) | `GND` | `GND` |
| 2 | D0 | `DBUS0` | `DBUS0` | `DBUS0` |
| 3 | D1 | `DBUS1` | `DBUS1` | `DBUS1` |
| 4 | D2 | `DBUS2` | `DBUS2` | `DBUS2` |
| 5 | D3 | `DBUS3` | `DBUS3` | `DBUS3` |
| 6 | D4 | `DBUS4` | `DBUS4` | `DBUS4` |
| 7 | D5 | `DBUS5` | `DBUS5` | `DBUS5` |
| 8 | D6 | `DBUS6` | `DBUS6` | `DBUS6` |
| 9 | D7 | `DBUS7` | `DBUS7` | `DBUS7` |
| 10 | GND | `GND` | `GND` | `GND` |
| 11 | CLK | `LATCH0` (BB48R GPIO29) | `LATCH1` (GPIO30) | `LATCH2` (GPIO31) |
| 12 | Q7 | `OBUF0_7` → U4 pin 9 | `OBUF1_7` → U5 pin 9 | `OBUF2_7` → U6 pin 9 |
| 13 | Q6 | `OBUF0_6` → U4 pin 8 | `OBUF1_6` → U5 pin 8 | `OBUF2_6` → U6 pin 8 |
| 14 | Q5 | `OBUF0_5` → U4 pin 7 | `OBUF1_5` → U5 pin 7 | `OBUF2_5` → U6 pin 7 |
| 15 | Q4 | `OBUF0_4` → U4 pin 6 | `OBUF1_4` → U5 pin 6 | `OBUF2_4` → U6 pin 6 |
| 16 | Q3 | `OBUF0_3` → U4 pin 5 | `OBUF1_3` → U5 pin 5 | `OBUF2_3` → U6 pin 5 |
| 17 | Q2 | `OBUF0_2` → U4 pin 4 | `OBUF1_2` → U5 pin 4 | `OBUF2_2` → U6 pin 4 |
| 18 | Q1 | `OBUF0_1` → U4 pin 3 | `OBUF1_1` → U5 pin 3 | `OBUF2_1` → U6 pin 3 |
| 19 | Q0 | `OBUF0_0` → U4 pin 2 | `OBUF1_0` → U5 pin 2 | `OBUF2_0` → U6 pin 2 |
| 20 | VCC | `3V3` | `3V3` | `3V3` |

> **CLK pulse**: PIO writes a byte to DBUS, then pulses LATCHn HIGH (briefly), capturing the byte on the rising edge. Width can be a single PIO cycle (~7 ns) -- the 74LVC574 needs ~3 ns minimum CLK width.

> **Default state**: pull-downs R4/R5/R6 (10 kΩ to GND) keep LATCH0/1/2 LOW so no spurious clock pulse occurs at power-up. Q outputs hold whatever was latched last, which on first power-up is undefined -- but `BD_OE_BUS_n` is HIGH (R3 pull-up), so U4/U5/U6 are high-Z and the undefined value never reaches the bus.

> **Decoupling**: One 0.1 µF cap (C22, C23, C24) per chip.

### Output write sequence (PIO)

```
1. PIO sets DBUS = byte0       (1 cycle, GPIO12-19 = pre-inverted byte0)
2. PIO pulses LATCH0 HIGH      (1 cycle) → U7 captures byte0
3. PIO drops LATCH0            (1 cycle)
4. PIO sets DBUS = byte1       (1 cycle)
5. PIO pulses LATCH1 HIGH      (1 cycle) → U8 captures byte1
6. PIO drops LATCH1            (1 cycle)
7. PIO sets DBUS = byte2       (1 cycle)
8. PIO pulses LATCH2 HIGH      (1 cycle) → U9 captures byte2
9. PIO drops LATCH2            (1 cycle)
10. PIO drops BD_OE_BUS_n      (1 cycle) → U4/U5/U6 drive all 24 bits to the bus simultaneously
... (bus cycle proceeds, BDRY etc.)
11. PIO raises BD_OE_BUS_n     (1 cycle) → U4/U5/U6 high-Z, bus released

Total: ~10 PIO cycles to load all three latches and start driving = ~67 ns @ 150 MHz
```

> The C code MUST pre-invert each byte (XOR with 0xFF) before pushing it to the PIO TX FIFO, because the bus uses negative logic for data and the 74LVT245 is non-inverting.

### U10, U11 — 74LVC14 Schmitt Inverter (×2, SOIC-14)

Hex Schmitt-trigger **inverter**. Inputs are 5 V tolerant at 3.3 V VCC. We use it to clean up incoming bus control signals AND to invert the polarity so the BB48R reads "1 = asserted". Schmitt trigger gives ~1 V hysteresis which is excellent for noisy bus edges.

**U10 channel 1 also produces the rising-edge clock for the input latches U1/U2/U3.** Bus /BAPR falls when an address is presented; after inversion, BAPR_3V3 rises -- which is exactly the rising edge the 74LVC574 needs to capture the data. U10 ch.1 drives both the BB48R sniff GPIO **and** the CLK pins of U1/U2/U3 in parallel.

We need to invert 8 input signals, and 74LVC14 has 6 channels per chip, so two chips: U10 (6 channels used) + U11 (2 channels used, 4 spare).

#### U10 channel allocation (6 channels)

| Channel | Source (5 V, active LOW) | Sink (3.3 V, active HIGH) |
|---------|--------------------------|---------------------------|
| 1 | `BAPR_BUS` (J1 A20) | `BAPR_IN_3V3` → BB48R GPIO20 (J2.24) **AND** U1/U2/U3 pin 11 (CLK) |
| 2 | `BIOXE_BUS` (J1 C19) | `BIOXE_IN_3V3` → GPIO21 (J2.25) |
| 3 | `BDAP_BUS` (J1 C18) | `BDAP_IN_3V3` → GPIO22 (J2.26) |
| 4 | `BDRY_BUS` (J1 A19) | `BDRY_IN_3V3` → GPIO23 (J2.27) |
| 5 | `BMEM_BUS` (J1 C20) | `BMEM_IN_3V3` → GPIO33 (J3.13) |
| 6 | `BINACK_BUS` (J1 B19) | `BINACK_IN_3V3` → GPIO34 (J3.14) |

#### U11 channel allocation (2 channels used, 4 spare)

| Channel | Source (5 V, active LOW) | Sink (3.3 V, active HIGH) |
|---------|--------------------------|---------------------------|
| 1 | `BMCL_BUS` (J1 B20) | `BMCL_IN_3V3` → BB48R GPIO35 (J3.15) |
| 2 | `BINPUT_BUS` (J1 A18) | `BINPUT_IN_3V3` → GPIO36 (J3.16) |
| 3-6 | (spare -- leave inputs tied to GND through 10 kΩ to keep them defined) | (no connect) |

74LVC14 pinout (SOIC-14):

| Pin | Function |
|-----|----------|
| 1 | 1A (input) |
| 2 | 1Y (output) |
| 3 | 2A |
| 4 | 2Y |
| 5 | 3A |
| 6 | 3Y |
| 7 | GND |
| 8 | 4Y |
| 9 | 4A |
| 10 | 5Y |
| 11 | 5A |
| 12 | 6Y |
| 13 | 6A |
| 14 | VCC = `3V3` |

> **Polarity**: bus /BAPR LOW (asserted) → U10 pin 2 (1Y) HIGH → BB48R reads 1. Exactly what we want. PIO `WAIT 1 PIN BAPR_PIN` reads natural.

> **Decoupling**: 0.1 µF (C16, C17) per chip.

### U12, U13 — 74LVC06 Open-Drain Inverter (×2, SOIC-14)

Hex **inverting** open-drain buffer. Inputs are 3.3 V CMOS (from BB48R). Outputs are open-drain, **5 V tolerant** -- they can be pulled up to 5 V on the bus side. We use it for two purposes simultaneously:

1. **Invert the polarity** so the BB48R writes `1` to mean "assert this bus signal"
2. **Open-drain to 5 V** so we can wire-OR with other cards on the same bus signal

We need 9 inverting open-drain channels (5 bidirectional bus drives + 4 BINTs), so two 74LVC06 chips (12 channels total, 3 spare).

74LVC06 pinout (SOIC-14):

| Pin | Function |
|-----|----------|
| 1 | 1A (input) |
| 2 | 1Y (open-drain inverted output) |
| 3 | 2A |
| 4 | 2Y |
| 5 | 3A |
| 6 | 3Y |
| 7 | GND |
| 8 | 4Y |
| 9 | 4A |
| 10 | 5Y |
| 11 | 5A |
| 12 | 6Y |
| 13 | 6A |
| 14 | VCC = `3V3` |

#### U12 channel allocation (6 channels)

| Channel | Input (3.3 V, active HIGH from BB48R) | Output (5 V open-drain, active LOW to bus) |
|---------|---------------------------------------|--------------------------------------------|
| 1 | `BAPR_OUT_3V3` (GPIO41) | `BAPR_BUS` (J1 A20) |
| 2 | `BDRY_OUT_3V3` (GPIO42) | `BDRY_BUS` (J1 A19) |
| 3 | `BINPUT_OUT_3V3` (GPIO43) | `BINPUT_BUS` (J1 A18) |
| 4 | `BDAP_OUT_3V3` (GPIO44) | `BDAP_BUS` (J1 C18) |
| 5 | `BREQ_OUT_3V3` (GPIO45) | `BREQ_BUS` (J1 C12) |
| 6 | `BINT10_OUT_3V3` (GPIO39) | `BINT10_BUS` (J1 A15) |

#### U13 channel allocation (6 channels, only 3 used)

| Channel | Input (3.3 V, active HIGH from BB48R) | Output (5 V open-drain, active LOW to bus) |
|---------|---------------------------------------|--------------------------------------------|
| 1 | `BINT11_OUT_3V3` (GPIO40) | `BINT11_BUS` (J1 C15) |
| 2 | `BINT12_OUT_3V3` (GPIO2) | `BINT12_BUS` (J1 A16) |
| 3 | `BINT13_OUT_3V3` (GPIO3) | `BINT13_BUS` (J1 C16) |
| 4-6 | (spare) | (no connect) |

> **Polarity behavior**: BB48R writes `1` to GPIO → 74LVC06 input HIGH → output transistor turns ON → output pulled to 0 V → bus signal asserted (LOW). BB48R writes `0` → 74LVC06 input LOW → output transistor OFF → output high-Z → bus pull-up takes over → bus signal idle (HIGH). Exactly the convention we want.

> **Reset-safe**: BB48R GPIOs power up as inputs (high-Z with internal pull-down). 74LVC06 inputs see 0 → outputs high-Z → bus pull-ups float HIGH → bus signals are idle. Power-up state is safe -- no spurious bus assertions.

> **Pull-ups for open-drain outputs**: The bus already has pull-ups on the wired-OR signals (BAPR, BREQ, BDRY, BINPUT, BDAP, BMCL, BINTs). These live on the **backplane** in our design. The controller card does not need additional pull-ups on the open-drain output side.

> **Decoupling**: 0.1 µF (C18, C19) per chip.

> **74LVC07 vs 74LVC06**: Both are hex open-drain. 74LVC07 is **non-inverting**, 74LVC06 is **inverting**. We pick the inverter so the BB48R sees a clean active-high world. If 74LVC06A is unobtainable from JLCPCB, the substitute is **74LVC07** + a **74LVC04** (hex inverter) in series -- two chips per channel instead of one. Avoid that if at all possible.

### U14 — 74LVC125 Daisy-Chain Pass-Through (SOIC-14)

The **only** 74LVC125 on the card. U14 is **non-inverting** by design -- the daisy chain must pass /INIDENT and /INGRANT through to /OUTIDENT and /OUTGRANT **with the same polarity**. No inversion here.

U14 handles the IDENT and GRANT daisy-chain pass-through (2 of 4 channels used; the other 2 are spare and could pick up INCONTR/OUTCONTR for future-proofing).

| Channel | Input (5 V from previous slot via J1) | OE control (3.3 V from BB48R) | Output (5 V to next slot via J1) |
|---------|---------------------------------------|-------------------------------|----------------------------------|
| 1 | `INIDENT_BUS` (J1 A22) | `OE_DAISY_IDENT_n` (GPIO47, J3.27) | `OUTIDENT_BUS` (J1 C22) |
| 2 | `INGRANT_BUS` (J1 A23) | `OE_DAISY_GRANT_n` (GPIO46, J3.26) | `OUTGRANT_BUS` (J1 C23) |
| 3 | (spare -- can be used for INCONTR/OUTCONTR pass-through) | -- | -- |
| 4 | (spare) | -- | -- |

> **Default state**: with `OE_DAISY_*_n` HIGH (BB48R idle), the buffers are **enabled** -- IN signal flows to OUT in 3-5 ns. Wait, actually 74LVC125 has **active-LOW** OE (it is enabled when OE pin is LOW). So we want the **default state to be LOW** so the buffers pass through. Add a **10 kΩ pull-down** on each `OE_DAISY_*_n` line so the chain works even when the BB48R is in reset or unprogrammed.

> **In CAPTURE mode** (when our card wants to handle the IDENT or take the GRANT), the BB48R drives `OE_DAISY_*_n` HIGH, putting the buffer into high-Z. The next slot then sees its `INIDENT/INGRANT` go HIGH (idle) via the bus pull-up.

> **Polarity is VERY important**: this section overrides any earlier text in `CONTROLLER-DESIGN.md` that might suggest the opposite. The BB48R defaults to GPIO inputs at reset, so we need the pull-down to keep the buffers enabled by default.

> **Decoupling**: 0.1 µF (C20) on U14.

### U15 — LTC4412 Ideal Diode Controller (SOT-23-6)

The LTC4412 is a low-loss PowerPath controller for source-OR-ing two power rails. We use it to control a P-channel MOSFET that switches the Pi Zero +5 V between USB-C-derived 5 V (when present) and bus 5 V (when USB is absent).

| Pin | Name | Function |
|-----|------|----------|
| 1 | VIN | `5V_LOCAL` (input) |
| 2 | SENSE | sense node (between VIN and PMOS source) |
| 3 | GATE | drives PMOS gate |
| 4 | GND | `GND` |
| 5 | CTL | (tie LOW for always-on) |
| 6 | STAT | open-drain status output (optional, drive LED4) |

External components:
- **PMOS** (e.g., DMP3098L SOT-23 or AO3401 SOT-23, 3 A, low Rds(on)): source = `5V_LOCAL`, drain = `5V_PIZERO`, gate = U15 pin 3
- **C8** = 10 µF on `5V_PIZERO` (Pi Zero side), close to PMOS drain

---

## Pi Zero Header (J4)

40-pin 2x20 male header at 0.1" pitch. Pin numbering matches the standard Raspberry Pi 40-pin pinout. **The Pi Zero plugs onto J4 from above** via female sockets on its own PCB (Pi Zero comes pre-soldered or with a header you solder yourself).

| J4 Pin | Pi Zero function | Connect to |
|--------|------------------|------------|
| 1 | +3.3V (Pi Zero internal regulator output) | **NO CONNECT** -- this is an output from Pi Zero, do not back-feed |
| 2 | +5V | `5V_PIZERO` |
| 3 | GPIO2 / I2C SDA | (no connect for now) |
| 4 | +5V | `5V_PIZERO` |
| 5 | GPIO3 / I2C SCL | (no connect) |
| 6 | GND | `GND` |
| 7 | GPIO4 | (no connect) |
| 8 | GPIO14 / TXD | (no connect) |
| 9 | GND | `GND` |
| 10 | GPIO15 / RXD | (no connect) |
| 11 | GPIO17 (input on Pi Zero) | `INT_BB48R` (BB48R GPIO0) |
| 12 | GPIO18 | (no connect) |
| 13 | GPIO27 (output from Pi Zero) | `INT_FROM_ZERO` (BB48R GPIO1) |
| 14 | GND | `GND` |
| 15 | GPIO22 | (no connect, optional Pi Zero → BB48R reset) |
| 16 | GPIO23 | (no connect) |
| 17 | +3.3V (output from Pi Zero) | **NO CONNECT** |
| 18 | GPIO24 | (no connect) |
| 19 | GPIO10 / SPI0_MOSI | `SPI_MOSI` (→ BB48R GPIO7) |
| 20 | GND | `GND` |
| 21 | GPIO9 / SPI0_MISO | `SPI_MISO` (→ BB48R GPIO4) |
| 22 | GPIO25 | (no connect) |
| 23 | GPIO11 / SPI0_SCLK | `SPI_SCK` (→ BB48R GPIO6) |
| 24 | GPIO8 / SPI0_CE0 | `SPI_CSn` (→ BB48R GPIO5) |
| 25 | GND | `GND` |
| 26 | GPIO7 / SPI0_CE1 | (no connect) |
| 27 | ID_SD (HAT EEPROM) | (no connect) |
| 28 | ID_SC (HAT EEPROM) | (no connect) |
| 29 | GPIO5 | (no connect) |
| 30 | GND | `GND` |
| 31 | GPIO6 | (no connect) |
| 32 | GPIO12 | (no connect) |
| 33 | GPIO13 | (no connect) |
| 34 | GND | `GND` |
| 35 | GPIO19 | (no connect) |
| 36 | GPIO16 | (no connect) |
| 37 | GPIO26 | (no connect) |
| 38 | GPIO20 | (no connect) |
| 39 | GND | `GND` |
| 40 | GPIO21 | (no connect) |

> **Mechanical**: 4× M2.5 mounting holes at the standard Pi Zero positions, with M2.5 nylon standoffs.

---

## Power Section

```
                     +-------+
  J1 row A/B/C       |       |
  pin 2/31 (5V_BUS)  |       |
       o------------>|       |    F1 (2A polyfuse)
                     |  D1   |---+----+----+----+----> 5V_LOCAL
                     |  SS14 |   |    |    |    |
       o------------>|       |   |    |    |    |
       |             |       |   |    |    |    |
  J3 pin 1           +-------+   |    |    |    |
  (5V_USB from                   |    |    |    |
   BB48R USB-C)                  |    |    |    |
                                 |    |    |    |
                                 |    |    |    |
  C1=47uF -----+                 |    |    |    |
  C2=0.1uF ----+                 |    |    |    |
                                 |    |    |    |
  D3 (TVS)                       |    |    |    |
  SMBJ5.0A ====+                 |    |    |    |
              GND                |    |    |    |
                                 |    |    |    |
                                 v    v    v    v
                              [BB48R][U1-U14 VCC pins][LEDs][U15 VIN]

                              U15 (LTC4412 + PMOS)
                              5V_LOCAL ────────> 5V_PIZERO ───> J4 pin 2/4
                                                      |
                                                      F2 (2A polyfuse)
                                                      |
                                                      C6=1000uF aluminum polymer
                                                      C7=470uF tantalum
                                                      C8=10uF
                                                      C9=0.1uF

  BB48R J3 pin 3 (GND) -----------> GND plane
  BB48R J2 pin 3 (3V3 output) -----> 3V3 plane
                                       |
                                       +-> all 74LVC and 74LVT VCC pins
                                       +-> per-IC 0.1uF decoupling
                                       +-> C4=10uF, C5=0.1uF bulk
```

### Power components recap

| RefDes | Part | Net in / Net out | Notes |
|--------|------|------------------|-------|
| **D1** | SS14 (SMA) | `5V_BUS` → `5V_LOCAL` | Anode = 5V_BUS, Cathode = 5V_LOCAL |
| **D1'** (second SS14) | SS14 (SMA) | `5V_USB` → `5V_LOCAL` | Anode = 5V_USB, Cathode = 5V_LOCAL. Two diodes form the OR-gate. |
| **F1** | 2A polyfuse 1812 | bus 5V before D1 | (alternative: between D1 and 5V_LOCAL) |
| **F2** | 2A polyfuse 1812 | between U15 PMOS drain and J4 | Pi Zero short protection |
| **D3** | SMBJ5.0A SMB | clamps 5V_LOCAL to GND | TVS for transient protection |
| **U15** | LTC4412 SOT-23-6 | controls PMOS for Pi Zero | |
| **PMOS** | DMP3098L or AO3401 | source = 5V_LOCAL, drain = 5V_PIZERO | controlled by U15 |
| **C1** | 47 µF 1210 X5R | 5V_LOCAL bulk | |
| **C2** | 0.1 µF 0603 X7R | 5V_LOCAL HF | |
| **C3** | 10 µF 0805 X5R | 5V_LOCAL distributed | |
| **C4** | 10 µF 0805 X5R | 3V3 bulk | |
| **C5** | 0.1 µF 0603 X7R | 3V3 HF | |
| **C6** | 1000 µF aluminum polymer (SMD or radial THT, 6.3 V or 10 V) | 5V_PIZERO bulk for WiFi TX | |
| **C7** | 470 µF tantalum case D | 5V_PIZERO boot inrush | |
| **C8** | 10 µF 0805 | 5V_PIZERO mid-frequency | |
| **C9** | 0.1 µF 0603 | 5V_PIZERO HF | |

---

## Pull Resistors and Decoupling

### Pull resistors (controller card only -- backplane has its own bus pull-ups)

| RefDes | Value | Net | Function |
|--------|-------|-----|----------|
| **R1** | 10 kΩ | `OE_DAISY_IDENT_n` to GND | Default LOW = 74LVC125 enabled = pass-through |
| **R2** | 10 kΩ | `OE_DAISY_GRANT_n` to GND | Default LOW = pass-through |
| **R3** | 10 kΩ | `BD_OE_BUS_n` to 3V3 | Default HIGH = U4/U5/U6 (74LVT245) high-Z = don't drive bus |
| **R4** | 10 kΩ | `LATCH0` to GND | Default LOW = no spurious clock pulse on output latch U7 at power-up |
| **R5** | 10 kΩ | `LATCH1` to GND | Same for U8 |
| **R6** | 10 kΩ | `LATCH2` to GND | Same for U9 |
| **R7** | 10 kΩ | `OE_IN_0_n` to 3V3 | Default HIGH = latch high-Z |
| **R8** | 10 kΩ | `OE_IN_1_n` to 3V3 | |
| **R9** | 10 kΩ | `OE_IN_2_n` to 3V3 | |
| **R10** | 10 kΩ | `MODE_SELECT` to 3V3 | Default = device mode |
| **R11** | 10 kΩ | `INT_BB48R` to GND | Pi Zero handshake idle LOW |
| **R12** | 10 kΩ | `INT_FROM_ZERO` to GND | |
| **R13-R17** | 4.7 kΩ × 5 | (optional) `BAPR_BUS`, `BDRY_BUS`, `BINPUT_BUS`, `BDAP_BUS`, `BREQ_BUS` to 5V_BUS | **Only if backplane lacks pull-ups**. Normally these live on the backplane. |
| **R18-R21** | 4.7 kΩ × 4 | (optional) `BINT10_BUS`-`BINT13_BUS` to 5V_BUS | Optional, backplane provides |
| **R22-R30** | 1 kΩ | LED current limiters (for LED1-LED9) | |

### Decoupling capacitors

| Cap | Value | Where |
|-----|-------|-------|
| C1 | 47 µF 1210 X5R | 5V_LOCAL bulk (input filter) |
| C2 | 0.1 µF 0603 X7R | 5V_LOCAL HF |
| C3 | 10 µF 0805 X5R | 5V_LOCAL distributed (mid-card) |
| C4 | 10 µF 0805 X5R | 3V3 bulk near BB48R |
| C5 | 0.1 µF 0603 X7R | 3V3 HF near BB48R |
| C6 | 1000 µF AlPolymer | 5V_PIZERO bulk |
| C7 | 470 µF tantalum | 5V_PIZERO boot inrush |
| C8 | 10 µF 0805 X5R | 5V_PIZERO mid-frequency |
| C9 | 0.1 µF 0603 X7R | 5V_PIZERO HF |
| **C10-C24** | 0.1 µF 0603 X7R × 15 | One per IC (U1-U15) -- placed within 2 mm of each IC's VCC pin |
| C25 | 10 µF 0805 X5R | Distributed bulk near U1-U6 (input latches and output drivers) |
| C26 | 10 µF 0805 X5R | Distributed bulk near U7-U9 (output latches) |
| C27 | 10 µF 0805 X5R | Distributed bulk near U10-U14 (control buffers + daisy chain) |

---

## Bill of Materials with Verified LCSC Numbers

> **Verification status**: Numbers marked ✓ are confirmed against the JLCPCB parts catalog at the time of writing. Numbers marked ⚠ should be verified at order time -- LCSC stock changes constantly. The "Type" column shows whether the part is a JLCPCB **Basic** part (no extended-parts setup fee) or **Extended** part ($3 setup fee per BOM).

### ICs

| RefDes | Part | LCSC | Package | Type | Qty | Approx unit cost |
|--------|------|------|---------|------|-----|------------------|
| **U1, U2, U3** (input latches) | 74LVC574A (Nexperia/TI) | C6097 ✓ | SOIC-20 | Extended ⚠ | 3 | $0.40 |
| **U4, U5, U6** (output drivers) | 74LVT245A (Nexperia) | C82393 ⚠ | SOIC-20 | Extended | 3 | $0.80 |
| **U7, U8, U9** (output latches) | 74LVC574A (same as U1-U3) | C6097 ✓ | SOIC-20 | Extended | 3 | $0.40 |
| **U10, U11** (input sniff Schmitt inverter) | 74LVC14A | C5181 ⚠ | SOIC-14 | Extended | 2 | $0.20 |
| **U12, U13** (output drive open-drain inverter) | 74LVC06A | C129539 ⚠ | SOIC-14 | Extended | 2 | $0.25 |
| **U14** (daisy chain pass-through) | 74LVC125A (non-inverting) | C6087 ✓ | SOIC-14 | Extended | 1 | $0.20 |
| **U15** (Pi Zero ideal diode) | LTC4412ES6 | C7414 ⚠ | SOT-23-6 | Extended | 1 | $1.50 |
| **PMOS** (paired with U15) | AO3401A | C15127 ✓ | SOT-23 | **Basic** ✓ | 1 | $0.04 |
| **Total IC count** | | | | | **15 chips** | **~$8** |

> **Same chip family for U1-U3 and U7-U9**: both are 74LVC574 octal D flip-flops, identical part. Six of the same part = simpler BOM, single LCSC line item, cheaper at quantity.

> **Inverters everywhere on the control path**: U10/U11 (74LVC14, Schmitt inverter on inputs) and U12/U13 (74LVC06, open-drain inverter on outputs) deliberately invert every control signal that crosses the 5 V ↔ 3 V3 boundary. This gives the BB48R a clean **active-HIGH** world (`1 = asserted`) for control signals. U14 (74LVC125, non-inverting) is the **exception** because the IDENT/GRANT daisy chain must pass through with unchanged polarity.

> **BD data path is NOT inverted in hardware**: U1-U3 (input latches) and U4-U6 (output drivers) and U7-U9 (output latches) are all **non-inverting** 74LVC574 / 74LVT245 parts. The BB48R sees the bus's **negative-logic** data form (1 bit value = LOW on bus = 0 in DBUS). The PIO program **inverts in firmware**: `MOV X, !PINS` on read, and the C code XORs with 0xFF before writing. See "BD Data Polarity" subsection below.

### BD Data Polarity (PIO inversion)

The ND-100 bus uses **negative logic** for the BD data lines: a logical "1" data bit is represented as voltage LOW on the bus, and "0" as voltage HIGH. This is opposite to the BB48R's positive-logic world.

The locked-in design uses **non-inverting** chips for the BD path (74LVC574 + 74LVT245) and handles the inversion in firmware:

| Direction | Where the inversion happens |
|-----------|-----------------------------|
| **Read** (bus → BB48R) | PIO instruction `MOV X, !PINS` reads GPIO12-19 and inverts as it loads X (zero extra cycles) |
| **Write** (BB48R → bus) | C code computes `dbus_byte = data_byte ^ 0xFF` before pushing to PIO TX FIFO (~1 ns per byte, negligible) |

PIO read snippet (loads byte0 from U1 into X):

```pio
set pins, 0b110     ; OE_IN_0_n LOW, others HIGH (assert read of U1)
mov x, !pins        ; read GPIO12-19, INVERT into X (X now holds the actual data byte)
in x, 8             ; push X into ISR
set pins, 0b111     ; OE_IN_0_n HIGH, all latches high-Z
```

C-side write helper:

```c
static inline void bd_write_byte(uint32_t pio_sm, uint8_t data_byte) {
    pio_sm_put_blocking(pio0, pio_sm, (uint32_t)(data_byte ^ 0xFFu));
}
```

> Note: this XOR is the **only** difference between the BB48R's view of BD data and the bus's view. Forget it and the bus reads garbage. The C helper above is the canonical place to do it -- never push raw `data_byte` to the PIO FIFO directly.

### Discretes (diodes, fuses, TVS)

| RefDes | Part | LCSC | Package | Type | Cost |
|--------|------|------|---------|------|------|
| D1, D1' | SS14 | C2480 ✓ | SMA (DO-214AC) | **Basic** ✓ | $0.05 |
| D3 | SMBJ5.0A | C8466 ⚠ | SMB (DO-214AA) | Extended | $0.08 |
| F1, F2 | MF-MSMF200-2 (2A polyfuse) | C71976 ⚠ | 1812 | Extended | $0.15 |

### Capacitors (all X5R or X7R, 16 V or 25 V)

| RefDes | Value | LCSC | Package | Type |
|--------|-------|------|---------|------|
| C1 | 47 µF 25V X5R | C19702 ✓ | 1210 | **Basic** ✓ |
| C3, C4, C20, C21 | 10 µF 25V X5R | C15850 ✓ | 0805 | **Basic** ✓ |
| C2, C5, C9, C10-C19 | 0.1 µF 50V X7R | C49678 ✓ | 0603 | **Basic** ✓ |
| C6 | 1000 µF 6.3V Al-polymer | C134716 ⚠ | SMD radial 8x10 | Extended |
| C7 | 470 µF 6.3V tantalum | C134694 ⚠ | Case D (7343) | Extended |
| C8 | 10 µF 25V X5R | C15850 ✓ (same as C3) | 0805 | **Basic** ✓ |

### Resistors (all 0603 1% unless noted)

| Value | LCSC | Type | Used for |
|-------|------|------|----------|
| 10 kΩ | C25804 ✓ | **Basic** ✓ | R1-R12 (pulls) |
| 4.7 kΩ | C23162 ✓ | **Basic** ✓ | R13-R21 (optional bus pulls) |
| 1 kΩ | C21190 ✓ | **Basic** ✓ | R22-R30 (LED current limit) |

### LEDs (0603 or 0805)

| RefDes | Colour | LCSC | Type |
|--------|--------|------|------|
| LED1 | Green 0805 | C84256 ✓ | **Basic** ✓ |
| LED2, LED3, LED4 | Yellow 0805 | C72038 ✓ | **Basic** ✓ |
| LED5 | Blue 0805 | C72041 ✓ | **Basic** ✓ |
| LED6-9 | Red 0805 | C84257 ✓ | **Basic** ✓ |

### Connectors (NOT in JLCPCB library -- hand-solder after delivery)

| RefDes | Part | Source | Notes |
|--------|------|--------|-------|
| J1 | DIN 41612 Type C 96-pin **male right-angle** | Mouser / Farnell / Digi-Key. Search "Harting 09 03 196 6921" or "ept 364-49096-94" or generic "DIN41612 Type C 96 pin male right angle". | $4-8 each |
| J2, J3 | 2x27 female header 0.1" 0.6" row spacing -- or buy as 1x27 strips and use two | Adafruit, Pollin, Mouser. Search "stacking header 27 pin" | $0.50 each |
| J4 | 2x20 male header 0.1" pitch | Generic 40-pin Pi GPIO header | $0.40 |
| J5 | 1x3 male header 0.1" + jumper shunt | Generic | $0.05 |

> **JLCPCB note**: For connectors, leave them out of the BOM/CPL files. JLCPCB will assemble only the SMD parts; you hand-solder the connectors after the boards arrive. Plan ~5 minutes per board for connector assembly.

### Total per-board parts cost (estimated)

| Category | Cost |
|----------|------|
| ICs (~11 chips) | ~$5.50 |
| Discretes | ~$0.60 |
| Caps (~25) | ~$1.20 |
| Resistors (~30) | ~$0.30 |
| LEDs (~9) | ~$0.45 |
| Connectors (hand-soldered) | ~$5.50 |
| PCB (qty 10, 2-layer 100x100mm HASL) | ~$0.50 |
| JLCPCB SMT assembly (qty 10, ~30 placements) | ~$5.00 |
| **Total per board (no Olimex BB48R, no Pi Zero)** | **~$19** |
| + Olimex BB48R | $15 |
| + Pi Zero 2 W (optional) | $15 |
| **Total per board (full)** | **~$49** |

---

## KiCad Library Setup

### Step 1: Install easyeda2kicad

```bash
pip install easyeda2kicad
```

### Step 2: Convert all LCSC parts to KiCad libraries

```bash
mkdir -p ~/kicad-libs/jlcpcb
cd ~/kicad-libs/jlcpcb

# Bus interface ICs
easyeda2kicad --full --lcsc_id=C6097    # 74LVC574A (U1-U3 input latches AND U7-U9 output latches -- 6 chips total of the same part)
easyeda2kicad --full --lcsc_id=C82393   # 74LVT245A (U4-U6 output drivers, 3 chips)
easyeda2kicad --full --lcsc_id=C5181    # 74LVC14A  (U10/U11 Schmitt inverter for input sniffs)
easyeda2kicad --full --lcsc_id=C129539  # 74LVC06A  (U12/U13 open-drain inverter for output drives)
easyeda2kicad --full --lcsc_id=C6087    # 74LVC125A (U14 daisy-chain non-inverting buffer)

# Power
easyeda2kicad --full --lcsc_id=C7414    # LTC4412
easyeda2kicad --full --lcsc_id=C15127   # AO3401 PMOS
easyeda2kicad --full --lcsc_id=C2480    # SS14
easyeda2kicad --full --lcsc_id=C8466    # SMBJ5.0A
easyeda2kicad --full --lcsc_id=C71976   # 2A polyfuse

# Caps
easyeda2kicad --full --lcsc_id=C19702   # 47uF 25V 1210
easyeda2kicad --full --lcsc_id=C15850   # 10uF 25V 0805
easyeda2kicad --full --lcsc_id=C49678   # 0.1uF 50V 0603
easyeda2kicad --full --lcsc_id=C134716  # 1000uF aluminum polymer
easyeda2kicad --full --lcsc_id=C134694  # 470uF tantalum

# Resistors
easyeda2kicad --full --lcsc_id=C25804   # 10K 0603
easyeda2kicad --full --lcsc_id=C23162   # 4.7K 0603
easyeda2kicad --full --lcsc_id=C21190   # 1K 0603

# LEDs
easyeda2kicad --full --lcsc_id=C84256   # Green 0805
easyeda2kicad --full --lcsc_id=C72038   # Yellow 0805
easyeda2kicad --full --lcsc_id=C72041   # Blue 0805
easyeda2kicad --full --lcsc_id=C84257   # Red 0805
```

This produces three files per LCSC ID: `.kicad_sym`, `.kicad_mod`, and `.step` (3D model).

### Step 3: Add libraries to your KiCad project

`Preferences → Manage Symbol Libraries → Add` for each `.kicad_sym` file. Same for footprint libraries (`Preferences → Manage Footprint Libraries`).

### Step 4: Install KiCad-JLCPCB-Tools plugin

`Tools → Plugin and Content Manager → search "JLCPCB"` → install. Right-click any component → JLCPCB Tools → Set LCSC Part.

### Step 5: Make symbols for parts NOT in the JLCPCB library

These need to be drawn or imported separately:

| Part | Where to find KiCad symbol |
|------|---------------------------|
| Olimex BB48R sockets (J2, J3) | Use generic `Connector_Generic:Conn_01x27` × 2 (or build a custom symbol with the EXT1/EXT2 pin labels from this doc) |
| DIN 41612 J1 | Search KiCad's built-in `Connector` library for `DIN41612` -- the 3x32 variant exists |
| Pi Zero header J4 | `Connector_Generic:Conn_02x20_Odd_Even` |

---

## Schematic Capture Order (Suggested)

Build the schematic in this order to avoid renumbering or shuffling later:

1. **Power section**: J1 power pins (5V_BUS, GND, +12V if any), D1/D1', F1, C1, C2, C25, D3, BB48R J3 VBUS/VSYS, U15+PMOS+F2, C6-C9, then 3V3 from BB48R J2 pin 3, C4, C5
2. **BB48R sockets**: Drop in J2 (EXT1) and J3 (EXT2), label every pin with the GPIO and the net per the BB48R Header Pin Map table
3. **DIN 41612 connector J1**: Drop in J1, label every pin with the bus net per the DIN 41612 table
4. **Input sniff buffers U10, U11 (74LVC14)**: Wire bus control signals → inverter inputs → BB48R sniff GPIOs (J2 pins 24-27 and J3 pins 13-18). U10 ch.1 output (`BAPR_IN_3V3`) also feeds CLK pins of U1/U2/U3.
5. **Input latches U1, U2, U3 (74LVC574)**: Wire BD0-23 from J1 to the D inputs, DBUS0-7 from the Q outputs to J2 pins 16-23, /OE_IN_n from J3 pins 6-8, CLK from `BAPR_IN_3V3` (output of U10 ch.1)
6. **Output latches U7, U8, U9 (74LVC574)**: Wire DBUS0-7 from J2 pins 16-23 to the D inputs, OE tied to GND, CLK from `LATCH0/1/2` (J3 pins 9-11), Q outputs to internal nets `OBUF{0,1,2}_{0..7}`
7. **Output drivers U4, U5, U6 (74LVT245)**: Wire `OBUF{0,1,2}_{0..7}` to A0-A7 inputs, BD0-23 from B0-B7 to J1, DIR tied HIGH to 3V3, OE from `BD_OE_BUS_n` (J3 pin 12)
8. **Output drive buffers U12, U13 (74LVC06)**: Wire BB48R drive GPIOs → inverter inputs → bus signals (open-drain to J1)
9. **Daisy-chain pass-through U14 (74LVC125)**: Wire INIDENT/INGRANT from J1 → U14 → OUTIDENT/OUTGRANT to J1, OE controlled by J3 pins 26-27
10. **Pi Zero header J4**: Wire SPI0 (J2 pins 8-11), INT pair (J2 pins 4-5), 5V_PIZERO, GND
11. **MODE_SELECT jumper J5**: 1x3 header with shunt, pull-up to 3V3 via R10
12. **Pull resistors**: R1-R12 per the table
13. **LEDs**: LED1-LED5 always populated, LED6-9 optional
14. **Decoupling**: One 0.1 µF cap per IC VCC pin (C10-C24, total 15), plus distributed bulk caps C25-C27
15. **ERC**: Run electrical rules check, fix all warnings (every IC must have power)
16. **Annotate**: Refresh annotation, verify the RefDes table above matches what KiCad assigned
17. **PCB layout**: Switch to PCB editor, place components, route, run DRC

---

## Cross-References

- **Architecture**: `CONTROLLER-DESIGN.md` -- the why behind every decision
- **Bus signal reference**: `ND-100-BUS-C-CONNECTOR.md` -- authoritative ND-100 bus signal documentation
- **Olimex datasheet**: `Olimex-rp2350/RP2350-PICO2-BB48-user-manual.pdf` -- BB48R hardware reference
- **Pin allocation**: see "Pin Allocation Summary" section in `CONTROLLER-DESIGN.md`
