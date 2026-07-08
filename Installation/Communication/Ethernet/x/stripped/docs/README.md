# ND Ethernet II (PCB 3094) - 68000 Firmware RE Documentation

**Reverse-engineering documentation for the ENCOS 68000 firmware (`../encos-ser-all-banks-68k.bin`) and the RetroCore emulator work built on it.**

Read order: start with the QuickMap, use COMPLETE as the reference.

---

## Files

| File | Contents |
|------|----------|
| [ND_EthernetII_68000_Firmware_COMPLETE.md](ND_EthernetII_68000_Firmware_COMPLETE.md) | **The authoritative complete firmware reference** - intended to be sufficient to re-implement the controller. All 116 functions and data globals named. |
| [ND_EthernetII_68000_Firmware_QuickMap.md](ND_EthernetII_68000_Firmware_QuickMap.md) | Condensed quick map of the firmware (entry points, tables, ISRs) |
| [ND_EthernetII_68000_Firmware_ReverseEngineering.md](ND_EthernetII_68000_Firmware_ReverseEngineering.md) | The first-pass RE document - superseded in places by COMPLETE (kept for provenance; its header lists what was later resolved) |
| [ND_EthernetII_Emulator_Correctness_Analysis.md](ND_EthernetII_Emulator_Correctness_Analysis.md) | Review of the RetroCore `NDBusEthernetII.cs` + `MC68901MFP.cs` emulators against the firmware RE (2026-07-08) |
| [ND_EthernetII_Fix_Plan.md](ND_EthernetII_Fix_Plan.md) | Validated fix plan / TODO for the emulator, goal: `START-NETWORK-SERVER ENNS0` completes and registers with XROUT |
| [ND_EthernetII_RE-SESSION-HANDOFF-2026-07-08.md](ND_EthernetII_RE-SESSION-HANDOFF-2026-07-08.md) | RE session handoff (2026-07-08): current live failure and where to continue |

---

**Parent:** [../README.md](../README.md) - the stripped firmware binaries and BPUN container analysis
**Sibling:** [../protocode/](../protocode/README.md) - C# behavioral model of the firmware
