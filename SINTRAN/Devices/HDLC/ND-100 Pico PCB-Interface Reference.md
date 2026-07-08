# ND-100/110 to Raspberry Pi Pico W — RS-422 Interface PCB Reference

**Document status:** Design reference / pre-layout  
**Sources:** ND-107300 HDLC Interface Description (890814), ND-107340 MEGALINK Interface Description (890814),  
AM26LS31 datasheet (TI SLLS114M), AM26LS32AC datasheet (TI SLLS115G)  
**Date:** 2026-04-12

---

## 1. Purpose

This document describes the design of a small interface PCB that connects a Norsk Data ND-100 or ND-110
minicomputer's HDLC/MEGALINK port to a Raspberry Pi Pico W microcontroller via an RS-422 transceiver
circuit. The ND-100/110 uses balanced differential signalling conforming to CCITT V.11 / EIA-422-B on a
DB-25 connector. The Pi Pico W operates at 3.3 V with single-ended TTL-level GPIOs.

The PCB performs four functions:

1. Terminates the DB-25 female connector facing the ND-100/110 (via a male-to-male straight-through cable)
2. Implements required strap connections between specific DB-25 pins at the device end
3. Converts balanced RS-422 differential signals to/from single-ended 5 V TTL using an AM26LS31 driver
   and AM26LS32AC receiver (both PDIP-16)
4. Shifts logic levels between 5 V (RS-422 chips) and 3.3 V (Pi Pico W) via a bidirectional level shifter

Data transfer uses the Pico's PIO state machines as the primary interface, with hardware SPI1 as a
fallback path for the data signals. Control/status signals use standard GPIO.

---

## 2. Signal Definitions and Directions

All directions are stated from the perspective of the **ND-100/110 CPU**.

| Signal | +/− | DB-25 Pin | ND Direction | Description |
|--------|-----|-----------|-------------|-------------|
| TXD+   | +   | 14        | → OUT       | Transmit data (non-inverting) — ND sends serial data |
| TXD−   | −   | 2         | → OUT       | Transmit data (inverting) |
| TXC+   | +   | 3         | → OUT       | Transmit clock (non-inverting) — ND drives TX bit clock |
| TXC−   | −   | 15        | → OUT       | Transmit clock (inverting) |
| RXD+   | +   | 16        | ← IN        | Receive data (non-inverting) — Pico sends serial data to ND |
| RXD−   | −   | 4         | ← IN        | Receive data (inverting) |
| RXC+   | +   | 18        | ← IN        | Receive clock (non-inverting) — Pico drives RX bit clock to ND |
| RXC−   | −   | 6         | ← IN        | Receive clock (inverting) |
| TBY+   | +   | 19        | → OUT       | Transmit Busy (non-inverting) — ND status output |
| TBY−   | −   | 7         | → OUT       | Transmit Busy (inverting) |
| RBY+   | +   | 5         | ← IN        | Receive Busy (non-inverting) — **STRAPPED** to TXC+ on PCB |
| RBY−   | −   | 17        | ← IN        | Receive Busy (inverting) — **STRAPPED** to TXC− on PCB |
| GND    | —   | 8         | —           | Signal ground |

> **Polarity note:** The MEGALINK document (ND-107340) is the authoritative source for +/− polarity.
> The ND-107300 table uses A/B labels which do NOT follow the RS-422 A=non-inverting convention —
> TB (pin 14) is TXD+ and TA (pin 2) is TXD−, opposite to what the A/B suffix would imply.

---

## 3. DB-25 Connector Pin Table — ND-100/110 Side

This is the DB-25 **female** connector on the PCB. The ND-100/110 connects via a straight male-to-male
DB-25 cable (part no. 325402 for the ND-107300 computer link, or 325400 for MEGALINK ND-110/ND-110).

| DB-25 Pin | Signal | +/− | ND Direction | PCB Action |
|-----------|--------|-----|-------------|------------|
| 1         | —      | —   | —           | Not connected |
| **2**     | TXD−   | −   | OUT from ND | → AM26LS32AC receiver 1B (pin 1) |
| **3**     | TXC+   | +   | OUT from ND | → AM26LS32AC receiver 2A (pin 6) AND strapped to pin 5 |
| **4**     | RXD−   | −   | IN to ND    | ← AM26LS31 driver 1Z (pin 3) |
| **5**     | RBY+   | +   | IN to ND    | **STRAP → pin 3 (TXC+)** |
| **6**     | RXC−   | −   | IN to ND    | ← AM26LS31 driver 2Z (pin 5) |
| **7**     | TBY−   | −   | OUT from ND | → AM26LS32AC receiver 3B (pin 9) |
| **8**     | GND    | —   | —           | PCB ground plane |
| 9         | —      | —   | —           | Not connected |
| 10        | —      | —   | —           | ⚠️ Strap role unverified — do not route |
| 11        | —      | —   | —           | ⚠️ Strap role unverified — do not route |
| 12        | —      | —   | —           | Not connected |
| 13        | —      | —   | —           | Not connected |
| **14**    | TXD+   | +   | OUT from ND | → AM26LS32AC receiver 1A (pin 2) |
| **15**    | TXC−   | −   | OUT from ND | → AM26LS32AC receiver 2B (pin 7) AND strapped to pin 17 |
| **16**    | RXD+   | +   | IN to ND    | ← AM26LS31 driver 1Y (pin 2) |
| **17**    | RBY−   | −   | IN to ND    | **STRAP → pin 15 (TXC−)** |
| **18**    | RXC+   | +   | IN to ND    | ← AM26LS31 driver 2Y (pin 6) |
| **19**    | TBY+   | +   | OUT from ND | → AM26LS32AC receiver 3A (pin 10) |
| 20        | —      | —   | —           | Not connected |
| 21        | —      | —   | —           | Not connected |
| 22        | —      | —   | —           | ⚠️ Strap role unverified — do not route |
| 23        | —      | —   | —           | ⚠️ Strap role unverified — do not route |
| 24        | —      | —   | —           | ⚠️ Strap role unverified — do not route |
| 25        | —      | —   | —           | Not connected |

### 3.1 Required PCB Straps at DB-25 Connector

The following pins must be bridged directly on the PCB at the DB-25 connector pads. These simulate the
presence of a DCE modem, allowing the ND-100/110 to assert its own Receive Busy (RBY) by looping back
the Transmit Clock (TXC).

| Strap | Bridge | Purpose |
|-------|--------|---------|
| A | Pin 3 ↔ Pin 5 | TXC+ tied to RBY+ |
| B | Pin 15 ↔ Pin 17 | TXC− tied to RBY− |

> ⚠️ Pins 10, 11, 22, 23, 24 appear in the ND-107300 strap tables but their functions are not
> confirmed from available documentation. These pins must NOT be routed until verified against
> the full ND-107300 hardware manual or ND-110 CPU card schematics.

---

## 4. PCB-Side Pin Table

This table describes every connection on the PCB, working inward from the DB-25 toward the Pi Pico W.

### 4.1 AM26LS31 — RS-422 Driver (Pico → ND) PDIP-16

Drives differential signals **from** the Pico **to** the ND-100/110.  
Enable: G (pin 4) → VCC 5V; G̅ (pin 12) → GND (always enabled).  
Bypass: 100 nF ceramic between pin 16 (VCC) and pin 8 (GND), placed as close to chip as possible.

| IC Pin | Name | Connects to | Signal |
|--------|------|------------|--------|
| 1      | 1A   | Level shifter HV side output | TXD from Pico (GPIO11) |
| 2      | 1Y   | DB-25 pin 16 | RXD+ to ND |
| 3      | 1Z   | DB-25 pin 4  | RXD− to ND |
| 4      | G    | VCC (5V)     | Enable HIGH |
| 5      | 2Z   | DB-25 pin 6  | RXC− to ND |
| 6      | 2Y   | DB-25 pin 18 | RXC+ to ND |
| 7      | 2A   | Level shifter HV side output | TXC from Pico (GPIO10) |
| 8      | GND  | GND          | Ground |
| 9      | 3A   | GND          | Unused — tie to GND |
| 10     | 3Y   | No connect   | Unused |
| 11     | 3Z   | No connect   | Unused |
| 12     | G̅    | GND          | Enable LOW |
| 13     | 4Z   | No connect   | Unused |
| 14     | 4Y   | No connect   | Unused |
| 15     | 4A   | GND          | Unused — tie to GND |
| 16     | VCC  | 5V + 100nF cap to GND | Power |

### 4.2 AM26LS32AC — RS-422 Receiver (ND → Pico) PDIP-16

Receives differential signals **from** the ND-100/110 and outputs single-ended 5 V TTL to the level shifter.  
Enable: G (pin 4) → VCC 5V; G̅ (pin 12) → GND (always enabled).  
Bypass: 100 nF ceramic between pin 16 (VCC) and pin 8 (GND), placed as close to chip as possible.

| IC Pin | Name | Connects to | Signal |
|--------|------|------------|--------|
| 1      | 1B   | DB-25 pin 2  | TXD− from ND |
| 2      | 1A   | DB-25 pin 14 | TXD+ from ND |
| 3      | 1Y   | Level shifter HV side input | RXD → Pico (GPIO12) |
| 4      | G    | VCC (5V)     | Enable HIGH |
| 5      | 2Y   | Level shifter HV side input | RXC → Pico (GPIO13) |
| 6      | 2A   | DB-25 pin 3  | TXC+ from ND |
| 7      | 2B   | DB-25 pin 15 | TXC− from ND |
| 8      | GND  | GND          | Ground |
| 9      | 3B   | DB-25 pin 7  | TBY− from ND |
| 10     | 3A   | DB-25 pin 19 | TBY+ from ND |
| 11     | 3Y   | Level shifter HV side input | TBY → Pico (GPIO09) |
| 12     | G̅    | GND          | Enable LOW |
| 13     | 4Y   | No connect   | Unused |
| 14     | 4A   | GND          | Unused — tie to GND |
| 15     | 4B   | GND          | Unused — tie to GND |
| 16     | VCC  | 5V + 100nF cap to GND | Power |

### 4.3 Level Shifter (5V ↔ 3.3V Bidirectional)

A standard bidirectional level shifter module (e.g. BSS138-based 4-channel or equivalent) is required
between the RS-422 chips (5V TTL) and the Pi Pico W (3.3V).

| HV Side (5V) | Direction | LV Side (3.3V) | Pico GPIO | Pin | Signal |
|-------------|-----------|----------------|-----------|-----|--------|
| AM26LS31 pin 1 (1A input) | ← | Pico GPIO11 | GPIO11 | 15 | TXD out |
| AM26LS31 pin 7 (2A input) | ← | Pico GPIO10 | GPIO10 | 14 | TXC out |
| AM26LS32AC pin 3 (1Y output) | → | Pico GPIO12 | GPIO12 | 16 | RXD in |
| AM26LS32AC pin 5 (2Y output) | → | Pico GPIO13 | GPIO13 | 17 | RXC in |
| AM26LS32AC pin 11 (3Y output) | → | Pico GPIO09 | GPIO09 | 12 | TBY in |

> RBY (GPIO08) is handled entirely by the PCB strap (pins 3↔5, 15↔17) and requires no active
> driving from the Pico. GPIO08 may be left unconnected or optionally used as a test point to
> monitor the RBY line state via an additional AM26LS32AC receiver channel if desired.

### 4.4 Pi Pico W GPIO Assignments

| GPIO | Pico Pin | Signal | Direction | PIO Role | SPI1 Fallback |
|------|----------|--------|-----------|----------|----------------|
| GPIO08 | 11 | RBY | (optional monitor) | — | — |
| GPIO09 | 12 | TBY | IN | Status input | GPIO only |
| GPIO10 | 14 | TXC | OUT | PIO TX clock output | SPI1_SCK (master TX) |
| GPIO11 | 15 | TXD | OUT | PIO TX data output | SPI1_TX / MOSI |
| GPIO12 | 16 | RXD | IN | PIO RX data input | SPI1_RX / MISO |
| GPIO13 | 17 | RXC | IN | PIO RX clock input | SPI1_SCK (slave RX) |

#### SPI1 Fallback Notes

SPI1 on GPIO10–13 can serve as a fallback for data transfer only. Because TX and RX use clocks
in opposite directions (Pico drives TXC for transmit; ND drives TXC for receive), SPI1 must be
reconfigured between TX and RX operations:

- **Transmit (Pico → ND):** SPI1 in master mode. GPIO10 = SCK output (drives RXC to ND),
  GPIO11 = MOSI (data to ND). The ND-100/110 is clocked by the Pico.
- **Receive (ND → Pico):** SPI1 in slave mode. GPIO10 = SCK input (ND's TXC drives the clock),
  GPIO12 = MISO (data from ND). The Pico is clocked by the ND-100/110.

This is not simultaneously bidirectional via SPI. PIO is the correct primary interface as it can
handle independent clocked TX and RX state machines concurrently.

---

## 5. ASCII Connection Diagram

```
 ND-100/110 CPU                    PCB                                        Pi Pico W
 DB-25 Male                        DB-25 Female                               3.3V GPIO
 (on CPU cable)  male─male cable   (on PCB)
                                   
 ┌─────────────┐                  ┌──────────────────────────────────────────────────────┐
 │             │                  │                                                      │
 │  TXD+  p14 ├──────────────────┤ p14 ──────────────────► 1A p2 ┐                     │
 │  TXD−  p2  ├──────────────────┤ p2  ──────────────────► 1B p1 ┤ AM26LS32AC          │
 │             │                  │                         1Y p3 ├─── [5V→3.3V] ──────►│ GPIO12 RXD
 │             │                  │                               │                     │
 │  TXC+  p3  ├──────────────────┤ p3  ──┬────────────────► 2A p6 ┤ AM26LS32AC         │
 │  TXC−  p15 ├──────────────────┤ p15 ──┼─┬──────────────► 2B p7 ┤                    │
 │             │                  │       │ │               2Y p5 ├─── [5V→3.3V] ──────►│ GPIO13 RXC
 │             │                  │       │ │                     │                     │
 │  RBY+  p5  ├──────────────────┤ p5  ──┘ │   [STRAP A]         │                     │
 │  RBY−  p17 ├──────────────────┤ p17 ────┘   [STRAP B]         │                     │
 │             │                  │                               │                     │
 │  TBY+  p19 ├──────────────────┤ p19 ──────────────────► 3A p10┤ AM26LS32AC          │
 │  TBY−  p7  ├──────────────────┤ p7  ──────────────────► 3B p9 ┤                    │
 │             │                  │                         3Y p11├─── [5V→3.3V] ──────►│ GPIO09 TBY
 │             │                  │                               │                     │
 │             │                  │           AM26LS32AC          │                     │
 │             │                  │           VCC p16 ── 5V       │                     │
 │             │                  │           GND p8  ── GND      │                     │
 │             │                  │           G   p4  ── 5V       │                     │
 │             │                  │           G̅   p12 ── GND      │                     │
 │             │                  │           100nF VCC─GND       │                     │
 │             │                  │                               │                     │
 │  RXD+  p16 ├──────────────────┤ p16 ◄──────────────── 1Y p2  ┐│                     │
 │  RXD−  p4  ├──────────────────┤ p4  ◄──────────────── 1Z p3  ┤│ AM26LS31    ◄── [3.3V→5V] ──────┤ GPIO11 TXD
 │             │                  │                        1A p1 ┤│                     │
 │             │                  │                               ││                     │
 │  RXC+  p18 ├──────────────────┤ p18 ◄──────────────── 2Y p6  ┤│ AM26LS31    ◄── [3.3V→5V] ──────┤ GPIO10 TXC
 │  RXC−  p6  ├──────────────────┤ p6  ◄──────────────── 2Z p5  ┤│                     │
 │             │                  │                        2A p7 ┘│                     │
 │             │                  │                               │                     │
 │             │                  │           AM26LS31            │                     │
 │             │                  │           VCC p16 ── 5V       │                     │
 │             │                  │           GND p8  ── GND      │                     │
 │             │                  │           G   p4  ── 5V       │                     │
 │             │                  │           G̅   p12 ── GND      │                     │
 │             │                  │           100nF VCC─GND       │                     │
 │             │                  │                               │                     │
 │  GND   p8  ├──────────────────┤ p8  ─────────────────────────── GND ────────────────┤ GND
 │             │                  │                                                      │
 └─────────────┘                  └──────────────────────────────────────────────────────┘

 STRAPS ON PCB (at DB-25 pads):
   STRAP A:  pin 3 (TXC+) ────────── pin 5  (RBY+)
   STRAP B:  pin 15 (TXC−) ───────── pin 17 (RBY−)

 PIO SIGNAL ROLES:
   GPIO10  TXC  OUT  ── PIO SM0: TX clock output  (master, drives ND RXC)
   GPIO11  TXD  OUT  ── PIO SM0: TX data output   (serial data to ND)
   GPIO12  RXD  IN   ── PIO SM1: RX data input    (serial data from ND)
   GPIO13  RXC  IN   ── PIO SM1: RX clock input   (ND drives this clock)

 SPI1 FALLBACK (GPIO10–13, reconfigure between TX/RX):
   TX mode (master):  SCK=GPIO10(out), MOSI=GPIO11(out)
   RX mode (slave):   SCK=GPIO10(in),  MISO=GPIO12(in)
```

---

## 6. Power Supply Summary

| Rail | Source | Consumers |
|------|--------|-----------|
| 5V | External (USB or regulator) | AM26LS31 VCC, AM26LS32AC VCC, Level shifter HV side |
| 3.3V | Pico W 3V3 pin | Level shifter LV side, Pico W itself |
| GND | Common | All components, DB-25 pin 8 |

Each IC requires a 100 nF low-ESR ceramic bypass capacitor between VCC (pin 16) and GND (pin 8),
placed as physically close to each chip as possible.

---

## 7. Outstanding Items — Do Not Finalise PCB Until Resolved

| # | Item | Risk |
|---|------|------|
| 1 | DB-25 pins 10, 11, 22, 23, 24 — strap function not confirmed from available docs | Could affect ND-100/110 interface operation |
| 2 | Confirm whether GPIO08 (RBY monitor) is needed or can be omitted | Minor — no electrical risk if left unconnected |
| 3 | Verify SPI1 slave mode clock tolerance vs ND-100/110 bit rate | Affects SPI fallback viability |
| 4 | Confirm 5V supply source and current budget (two PDIP-16 ICs at up to 80 mA each worst case) | Power design |
| 5 | Confirm termination resistor requirement on RXD pair (100Ω across pins 4/16) if cable is long | Signal integrity |

---

*Document compiled from: ND-107300 HDLC Interface Description, ND-107340 MEGALINK Interface Description,*
*AM26LS31 and AM26LS32AC datasheets (Texas Instruments), and analysis of the Pi Pico W GPIO layout.*
