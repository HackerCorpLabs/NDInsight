# Ethernet III (324232 print H) — Architecture Survey from the Schematics

**First-pass survey of the ND Ethernet III controller schematics (card 324232,
print H = `ND-324232-H1-EN.pdf`, 32 sheets, rendered at 300 dpi in
`../EthIIIImages/`), focused on what matters for emulation.** Unlike the Ethernet II
deep-dive, this is a survey: the architecture, chips, interrupt structure and clocks
were read from the key sheets; not every net was traced. No technical manual is
available (the Ethernet III NTM, ND-814006, is listed in the library index but the PDF
was never put online) — everything here is schematic-only evidence plus the
installation description ND-899127 (in `mirror/library/libsw/`, not yet studied).

## 1. Sheet index (titles from the title blocks)

Print-H revision state: sheets marked **[H]** carry ID `324232HH` / print `5452H`
(dated 90.11.21, sheet 8: 88.11.01) — these are the sheets changed since print D.
All others still carry `324232DD` / `5452D` (88.03.01) — **this is how the D→H diff
is encoded**; unchanged sheets keep their original title block.

| # | Title | Rev | # | Title | Rev |
|---|---|---|---|---|---|
| 1 | TOP BLOCK | D | 17 | CONTROL/ETC (break, clocks, timeout, reset) | **[H]** |
| 2 | MFBUS CONNECTIONS | D | 18 | CONTROL/INTR_CONTR | D |
| 3 | TEST CONNECTORS | D | 19 | CONTROL/MEMORY | D |
| 4 | BLOCK LEVEL | D | 20 | MEMORY/DRAM | D |
| 5 | BUS_INTERF | D | 21 | MEMORY/DRAM/BANK0 | D |
| 6 | BUS_INTERF/LATCH | D | 22 | MEMORY/DRAM/BANK1 | D |
| 7 | BUS_INTERF/MFA | D | 23 | MEMORY/DRAM/BANK2 | D |
| 8 | BUS_INTERF/MFA/CONTROL | **[H]** | 24 | MEMORY/DRAM/BANK3 | D |
| 9 | BUS_INTERF/MFA/DRIVER | D | 25 | MEMORY/DRAM/PARITY | D |
| 10 | BUS_INTERF/MFA/IDENT | D | 26 | CONTROL/MEMORY/DRAM_CTRL | **[H]** |
| 11 | BUS_INTERF/OCTOBUS (ECO 380-136 rev B) | **[H]** | 27 | CONTROL/PROTECTION | D |
| 12 | CONTROL | D | 28 | CONTROL/REG_BLOCK | D |
| 13 | CONTROL/ADDR_DECOD | **[H]** | 29 | DEVICE | D |
| 14 | CONTROL/BUFFER | D | 30 | DEVICE/DECOD | **[H]** |
| 15 | CONTROL/CPU | **[H]** | 31 | DEVICE/LANCE | **[H]** |
| 16 | CONTROL/EPROM | D | 32 | DEVICE/POWER_CTRL | D |

## 2. What the card is

A completely different machine from Ethernet II — not a variant:

- **Host bus: the MF bus** (MFAMBUS/MFA nets, sheets 2, 5-10), NOT the ND-100 I/O bus.
  There is no IOX/IOXT device-register interface, no IDENT daisy chain of the ND-100
  kind, no BINT12. The MFA section has its own control/driver/ident sheets (8-10)
  belonging to the MF-bus protocol.
- **Plus an OCTOBUS station** (sheet 11, heavily revised in print H, ECO 380-136):
  a custom ND gate array **ND_D_OBCON** (serial octobus rx: BRXREQ/BRXCLK/BRXDAT/
  BRFOSL; open-collector tx: TREQ~/TDAT~ via 7407; SIR/SIFF/SINT status), 4× 74S225
  FIFOs queuing octobus message data (OBMCD15:00 → MD), and a PAL20RA10 ("10 OCTO",
  ND id 745-00) decoding octobus commands into **RESET1~, IHALT~, OCINT7~,
  RESCOUNT~, PALRES~, RESOCINT~, ENOCRES~, TIMEOUT** — i.e. the octobus can remotely
  reset, halt and interrupt the card (the ACCP-style station control model known from
  the ND-5000 octobus world). A 74LS292 provides a **watchdog** (WDOGRES).
- **CPU: MC68020-16** (sheet 15; PGA, AVEC, DSACK0/1 dynamic bus sizing, FC0-2,
  IPEND), full 32-bit MCA(31:00)/MCD(31:00) local buses. Handwritten annotations on
  sheets 15/17 ("-33", "60 Mc (33)", "66 MHz") indicate a 33 MHz upgrade variant.
- **Memory: 4 DRAM banks** (sheets 21-24) + parity (25), EPROM (16), memory
  protection (27), DRAM controller revised in print H (26).
- **Resident firmware in EPROM** (sheet 16) — unlike Ethernet II (empty sockets,
  code downloaded by the ND-100): two populated 8-bit EPROM sockets on byte lane
  MCD24-31, labeled **"DEVICE EPROM"** (30H) and **"OPCOM"** (29H), 27512/27C101
  (up to 128 KB each; strap-substitutable by HM62256 SRAM for development,
  straps STR00-03), split by PROMSEL~ + MCA17. Plus an **EEPROM** AM2817A (2 KB) /
  HM58C65P (8 KB) @32J with EEPWR~/EEPROM~ and a ready/busy interrupt `EEINT` —
  non-volatile parameter store (Ethernet address/config is the likely use —
  inference, not stated on the sheet). The 68020 boots from its own PROM; whether
  SINTRAN additionally downloads server code into DRAM is a software question
  (check ND-899127 installation description).
- The octobus attachment is NOT a print-H addition: print D sheet 11 (88.03.01) is
  already titled BUS_INTERF/OCTOBUS; ECO 380-136 (print H, 90.11.21) reworked it.
- **Serial console: RS-232 via MAX233** (RSRXD/RSTXD) from the MFP USART — no more
  20 mA current loop.

## 3. Interrupt structure (sheet 18 CONTROL/INTR_CONTR)

- **MK68901 MFP @35G is present again** and is the interrupt hub:
  - **XTAL = 3.6864 MHz** (dedicated baud crystal @38G) — unlike Ethernet II's
    3.125 MHz. CLK pin = 3CLK.
  - GPIO(7:0) wired to card events; identified so far: GPIO0/1 ↔ CCINT / DMATRAP~,
    **GPIO4 ← LANINTR~ (the LANCE interrupt!)** (sheet 31).
  - USART → MAX233 RS-232 console; RR/TR (RREDY~/TREDY~) again form a separate
    console-ready interrupt via an F74+gates (CLRREDY~ clears), as on Ethernet II.
  - DT901~ (MFP DTACK) feeds the "7 DSACK" PAL16L8 @27H that merges all DSACK/DTACK
    sources (MDTACK~, MPMDTACK~, DT901~, RCOUNT~, DEVACK0/1~) into DSACK0~/DSACK1~.
- **74LS148 @32L** encodes IPL~(2:0) for the 68020 from: MFP INTR~, MF-bus command
  interrupts **MCINT~3 / MCINT~4**, octobus **OCINT~**, console IREDY~, and the
  level-7 group.
- **PAL20RA10 "9 INT7" @37G** (ND id 744-00) collects the fatal/NMI-class sources —
  ITRAP~, ITBERR~ (bus-error/trap from the "11 TRAP" PAL16L8 @38J), ERR~, PFAIL~,
  MPERR~ (parity), OCINT7~, XINT7~, RINT7~, ENTCO~, CINT7~ — latches them, and makes
  the pending set **readable on MD(23:16)**: a memory-mapped interrupt-source status
  register in a PAL.
- BERR generation: "LOCAL TIMEOUT" 74LS292 counter (sheet 17) → TBERR/LONGBERR, and a
  **hardware breakpoint system** ("BREAK SYSTEM", BREAK32~, UBRK/LBRK comparators)
  that forces BERR on address match — the evolved form of Ethernet II's
  PARITYDIS+BREAKMODE trick.

## 4. Device section (sheets 29-32)

- **Same Ethernet chipset as Ethernet II: AM7990 LANCE + AM7992B SIA**, 40 MHz osc →
  20 MHz SIA clock, isolation transformer, transceiver power switch (POWER_CTRL,
  sheet 32) with EXTPOWER/ID pins.
- The 16-bit LANCE sits on the 32-bit bus behind a "16 ARBIT" PAL16L8 (ND id 751-00)
  + F245/F373 buffer steering (LANAD/LANA buses, BR~/BGACK~/DBIR/UBUFEN~/LBUFEN~).
- A 74LS244 @17C puts **LANINTR~, PWEN~, ETHSTAT~ status onto MD(19:16)** — the
  Ethernet III's equivalent of the ETHSTAT register.

## 5. Clocks (sheet 17, with hand annotations)

| Source | Divided | Used for |
|---|---|---|
| 60 MHz osc @20K (print says 56.6/50-55 crossed out; handwritten "782605 60 MHZ"; 66 MHz for the -33 variant) | F163 chain → 30 / 15 / 7.5 / 3.75 MHz (handwritten "30Mc(33) 15Mc(16.5) 7.5Mc(8.25) 3.75Mc(4.12)") | MCLK / SCLK / SCLK2 / SCLK4 — 68020 CLK = 16CLK ≈ 15 MHz (16 MHz nominal) |
| 3.6864 MHz xtal @38G | — | MK68901 XTAL (timers + baud) |
| 40 MHz osc | ÷2 | SIA 20 MHz (10 Mb/s Ethernet) |
| 16 MHz | — | ND_D_OBCON octobus gate array |
| 74LS292 ×2 | programmable | local bus timeout (TBERR) and octobus watchdog (WDOGRES) |

Still **no dedicated timer/RTC chip and no AM9519** — periodic timing is MFP timers,
exactly as on Ethernet II.

## 6. Implications for the C# emulator (NDBusEthernetII.cs and beyond)

1. **No impact on Ethernet II emulation correctness.** Ethernet III is an MF-bus +
   octobus card; nothing in it changes the Ethernet II validation findings
   (`EthII-emulator-validation.md`). The two cards share only the LANCE/SIA chipset
   and the MK68901.
2. **The 3.6864 MHz mystery is solved**: that figure (which the Ethernet II emulator
   used for the MFP before fix C-4) is the **Ethernet III** MFP crystal. Any document
   or firmware constant mentioning 3.6864 MHz belongs to Ethernet III; Ethernet II is
   3.125 MHz. Useful fingerprint when attributing firmware or driver code to a card.
3. **Firmware fingerprinting**: an image programming the MFP expecting the LANCE
   interrupt on **GPIP4** (or vectors for MCINT/OCINT sources) targets Ethernet III;
   Ethernet II firmware expects LANERROR/NCINT/WRIV on GPIP5/6/7 and the LANCE on
   IPL2. Also RS-232 console (EthIII) vs current-loop PCT (EthII).
4. **The TPE "STC" remains fictional on both cards** — even the far larger Ethernet
   III has no timer controller chip; its extra counters (LS292) are bus-timeout/
   watchdog, not periodic timers. There is no ND Ethernet card on which the
   level-5 "STC timer" block would be real hardware.
5. **If an Ethernet III emulation is ever wanted** (e.g. for ND-120/CX or ND-5000-era
   SINTRAN configurations), the natural RetroCore composition is: 68020 core (needs
   DSACK sizing), the existing MK68901 model (with 3.6864 MHz), the existing AM7990/
   AM7992 cores behind a 16↔32-bit gasket, 4-bank DRAM + parity + protection, an MF-bus
   slave front end, and an **octobus station modeled on the existing
   NDBusOctobus/OctobusND5000Station pattern** — the ND_D_OBCON + "10 OCTO" PAL
   implements exactly the remote reset/halt/interrupt station semantics used by the
   ND-5000 stations. The readable MD(23:16) INT7-source register and MD(19:16)
   LANINTR/PWEN status must be modeled for its firmware to boot.
6. **Hardware breakpoints**: the BREAK32/UBRK/LBRK force-BERR system means Ethernet
   III diagnostic firmware may deliberately trigger bus errors at armed addresses —
   an emulator must not treat those as fatal model bugs.

## 7. Software-side facts (from NDInsight installation descriptions)

Found in `E:\Dev\Ronny\NDInsight\Installation\Installation-Description\` (OCR'd product
information sheets, ND-895xxx series):

- **Ethernet III controller = ND product 110513** (Ethernet II was 110063).
  Referenced in ND-895061 (TCP/IP), ND-895533/895535/895536/895537/895538/895566,
  ND-895520, ND-895207, ND-895479.
- It is the **ND-5000-generation** Ethernet controller: the "TCP/IP Basic Module for
  ND-5000" runs *in* the Ethernet III controller (ND-895071), and its startup is
  sequenced in LOAD-MODE:MODE together with starting the ND-5000 (after XMSG /
  @START-TADADM) — consistent with the MF-bus + octobus attachment seen in the
  schematics.
- **Load model is hybrid**: boot/monitor firmware is resident in the card's EPROMs
  ("DEVICE EPROM" + "OPCOM", sheet 16), while the network application software
  (e.g. TCP/IP Basic Module/III) is **downloaded from SINTRAN at system load** via
  `@MODE (TCP-IP)TCP-BA-LOAD:MODE`. Several Ethernet III controllers can run TCP/IP
  simultaneously; `AIP-CONFIG:SYMB` lists the local Ethernet controllers running
  TCP/IP and their TCP-device numbers.

## 8. Not yet analysed

Sheets 5-10 (MF-bus protocol detail), 12-14/19/26-28 (control/decode/registers,
DRAM control), 30 (device decode — needed for the exact 68020 I/O map), and all PAL
equations (ND ids 744-00, 745-00, 746-00, 751-00 seen; no dumps). A follow-up pass
on sheets 30 + 13 would recover the 68020 address map, which is the prerequisite for
any emulation work.
