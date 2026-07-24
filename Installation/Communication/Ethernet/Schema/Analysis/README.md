# Ethernet II Schematic Analysis

**Gate-level analysis of the Ethernet II (324534 print G) schematics, made to validate
the RetroCore emulator `NDBusEthernetII.cs` — interrupt wiring (ND-100 bus side and
68000 side), 68000↔MFP↔LANCE connections, and the clock/timer inventory.**

Produced 2026-07-23 from the 300 dpi sheet renders in [`../EthIIImages/`](../EthIIImages/),
cross-checked against the original ND-12.055.1 EN PDF (not the OCR'd Markdown, which
contains at least one hallucinated section).

| File | Contents |
|------|----------|
| [EthII-interrupt-clock-netlist.md](EthII-interrupt-clock-netlist.md) | Human-readable netlist: LS148 IPL encoder, IACK PAL, all six interrupt sources, MFP pinout, I/O decode (incl. the EF00xx/EF01xx dual-enable mirror), ND-100 control/status/INT12/IDENT logic, full clock tree |
| [EthII-netlist.json](EthII-netlist.json) | Machine-readable version of the same nets (for a later KiCad/Logisim export) |
| [EthII-emulator-validation.md](EthII-emulator-validation.md) | Line-referenced cross-check of `NDBusEthernetII.cs`: 19 CONFIRMED items, 7 mismatches (M1 STC removal proven, M2/M3 doorbell strobes, M4 power-low gating, M6 LANCE level, M7 RFT latch), 4 gaps (LANERROR/WRIV/console/MFP-reset), suggested order of work |
| [EthIII-architecture-survey.md](EthIII-architecture-survey.md) | Ethernet III (324232 print H) survey: sheet index with D→H revision markers, MC68020-16 + MF-bus + OCTOBUS-station architecture (ND_D_OBCON gate array), MK68901 @3.6864 MHz interrupt hub, AM7990/AM7992 device section, clock tree, and what it means for the C# emulator (3.6864 MHz mystery solved; no STC on any card) |
| [EthII-hardware-highlevel.md](EthII-hardware-highlevel.md) | Ethernet II hardware description, high level, with mermaid diagrams: system context, block diagram, registers, interrupt overview, clock table, life-cycle sequence |
| [EthII-hardware-detail.md](EthII-hardware-detail.md) | Ethernet II hardware description, gate level, with mermaid diagrams: IPL encoder, IACK PAL, MFP wiring, I/O decode, INT12 state machine, parity path, power-fail sequence — every claim sheet/manual-referenced |
| [EthIII-hardware-highlevel.md](EthIII-hardware-highlevel.md) | Ethernet III hardware description, high level, with mermaid diagrams: system context (MF bus + octobus), block diagram, II-vs-III comparison table, interrupt overview, clocks, D→H print changes |
| [EthIII-hardware-detail.md](EthIII-hardware-detail.md) | Ethernet III hardware description, detail level (as far as surveyed): 68020 core, MFP/LS148/INT7-PAL interrupt structure, break system + timeouts + clocks, octobus station, EPROM/EEPROM, LANCE arbitration — untraced areas explicitly marked |
| [EthIII-68020-address-map.md](EthIII-68020-address-map.md) | 68020 address decode traced from sheets 13 + 30: three-level PAL hierarchy, every chip-select and register strobe, IACK map (MFP vectored on level 2, MF-bus acks on 3/4), LANCE→MF-bus DMA path with the LANCE ADDRESS REGISTER — absolute addresses remain inside undumped PALs |

Key results:
- The "STC timer controller" in the emulator **does not exist in hardware** — the F138
  I/O decoder has two enables, making EF01xx a pure mirror of EF00xx (EF0140=MERRSTAT,
  EF0160=EAREN). Confirms fix-plan B-REV-1/2/3.
- The open C-1 question is settled: **IDENT clears BOTH the RFT (INT12) and RIE
  (enable) flip-flops** (shared CLINT0 reset) — the emulator's current behavior is
  hardware-correct.
- Only timers on the card: the MFP's four (C = RTC ≈ 128.07 Hz, D = USART baud;
  A/B unused). MFP clock = 3.125 MHz on both XTAL1 and CLK.

---

**Parent:** [../README.md](../README.md)
