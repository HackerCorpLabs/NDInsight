# Ethernet Controller Schematics (Ethernet II and Ethernet III)

**Norsk Data schematic prints for the Ethernet II card (324534 / PCB 3094) and the
Ethernet III card (324232), scanned by Jonny Oddene for Sintran Data.**

Copied from the sintran.com library mirror (`mirror-sintran-com`, category `libhw`,
originally `http://norsk-data.com/library/libhw/`).

---

## Files

| File | Card | Print | Date | Sheets | Contents |
|------|------|-------|------|--------|----------|
| [ND-324534-G1-EN.pdf](ND-324534-G1-EN.pdf) | 324534 (Ethernet II, PCB print 3094, replacement for 008) | G | Aug. 1988 (drawn 87.02.04, G added 88.08.17) | 5 | Full schematic set, see sheet list below |
| [ND-324232-D1-EN.pdf](ND-324232-D1-EN.pdf) | 324232 (Ethernet III, MF bus) | D | Mar. 1988 (sheet 1 drawn 88.03.01) | 32 | Full schematic set; sheet 1 = "ETHERNET III TOP BLOCK", sheet 2 = "MFBUS CONNECTIONS" |
| [ND-324232-H1-EN.pdf](ND-324232-H1-EN.pdf) | 324232 (Ethernet III, MF bus) | H | Nov. 1990 (per library index) | 32 | Later print of the same set |
| [EthIIImages/](EthIIImages/) | 324534 (Ethernet II) | G | — | 5 | The five Ethernet II sheets rendered as lossless 300 dpi PNG (native scan resolution, 1-bit CCITT source), one file per sheet, named by sheet title |
| [EthIIIImages/](EthIIIImages/) | 324232 (Ethernet III) | H | — | 32 | All 32 Ethernet III sheets as lossless 300 dpi PNG (`ND-324232-H1-sheet-NN.png`); sheet titles indexed in [Analysis/EthIII-architecture-survey.md](Analysis/EthIII-architecture-survey.md) |
| [Analysis/](Analysis/README.md) | both | G / H | 2026-07-23 | — | Gate-level Ethernet II interrupt + clock netlist (md + json), the line-referenced validation of the RetroCore `NDBusEthernetII.cs` emulator, and the Ethernet III architecture survey |

Note on D vs H: unchanged sheets keep their original title block (ID `324232DD`,
print `5452D`, 88.03.01). The sheets revised in print H carry ID `324232HH` / print
`5452H` (90.11.21; sheet 8: 88.11.01): **sheets 8, 11, 13, 15, 17, 26, 30, 31** —
i.e. the MFA control, the OCTOBUS interface (ECO 380-136), address decode, CPU,
clocks/reset, DRAM control, device decode and LANCE sheets. Full sheet index in
[Analysis/EthIII-architecture-survey.md](Analysis/EthIII-architecture-survey.md).

## Ethernet II (324534) sheet list

Verified by reading the PDF:

1. **CPU and Memory Control** — MC68HC000-12 local CPU, 50 MHz xtal (12.5 MHz CPU
   clock), DRAM refresh/arbitration, PALs 452-00…456-00
2. **Bus Buffers** — ND-100 bus buffering (BD0–BD23, BAPR, BDAP, BIOXE, BINACK,
   BDAR, BMEM…), EECO thumbwheel switches for ident/device selection
3. **Bus Arbitration / DMA and I/O Control** — MK68901 MFP, LANCE/console interrupt
   routing, NORD bus control, ROM I/O control
4. **Memory and Error Correction** — 512 KB dynamic RAM (HM51256P-10) with byte
   parity, 128 KB EPROM (2× 27512 sockets "reserved for future use"), parity
   checking and error logging
5. **Ethernet Interface** — AM7990 (LANCE) + AM7992B (SIA), 40 MHz Ethernet clock,
   PE64102 isolation transformer, transceiver power control (+12 V VSENSE)

The Ethernet III (324232) sheets beyond 1–2 have not been reviewed yet.

---

## Related

- [../../Ethernet/README.md](../README.md) — COSMOS Ethernet II Option (210580),
  ENCOS firmware and its reverse engineering
- `E:\Dev\Ronny\NDInsight\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md`
  — OCR'd technical manual for the Ethernet II controller (ND number 110063), the
  prose companion to the 324534 schematics
- Library mirror also holds `ND-899127-1-EN.pdf` (Ethernet III, Installation
  Description, Nov. 1988) in `mirror/library/libsw/`; the Ethernet III Controller
  NTM (ND-814006-1) is listed in the library index but the PDF is not available

---

**Parent:** [../README.md](../README.md)
