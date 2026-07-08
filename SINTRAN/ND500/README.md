# ND-500 Processor Documentation

**ND-500 CPU architecture, the 3022/5015 bus interface, and SINTRAN integration**

Folder overhauled 2026-07-08: every bus-interface claim was re-verified against the
SINTRAN NPL sources, the symbol tables and the hardware manuals; fabricated content
was retired to `old/`. Start with the Tier 1 documents.

---

## Tier 1 - Authoritative (start here)

| Document | Purpose |
|----------|---------|
| [ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md) | **THE spec** for ND-100 <-> ND-500 communication: IOX registers + four-mode decode, CONTROL/STATUS bits, activation protocol, message memory, level-12 ISR, boot/detection, microcode load, master clear/terminate/power-fail, locking, TAG hardware, SAMSON delta, emulator checklist |
| [ND500-EVIDENCE-AND-CONTRADICTIONS.md](ND500-EVIDENCE-AND-CONTRADICTIONS.md) | The citation trail: verbatim NPL/symbol-table/manual quotes behind every claim, plus verdicts on the 12 contradictions (C1-C12) found across the older docs |
| [ND500-EMULATOR-DISCREPANCY-AUDIT.md](ND500-EMULATOR-DISCREPANCY-AUDIT.md) | Burn-down list D01-D20: where the RetroCore NDBusND500IF.cs emulation differs from the spec, with severity and fix order |

**The headline correction:** SINTRAN never exchanges "TAG codes" with the ND-500.
The real protocol is: build a message in mailbox memory, load the 3022 MAR, poke
CONTROL; answers come back as messages dispatched by the level-12 driver. The
"high-level TAG codes 8/9/16" found in older docs and in the C# emulator were an
invention.

## Tier 2 - Verified deep dives

| Document | Purpose |
|----------|---------|
| [ND500-IF-USAGE-DEEP-ANALYSIS.md](ND500-IF-USAGE-DEEP-ANALYSIS.md) | IOX command usage, code loading, domain setup, scheduling via the interface |
| [ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md](ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md) | DMA (3022) and Octobus (SAMSON) implementation guide (note: its 5CPUTYPE bits-14/15 claim is wrong - see dossier C8) |
| [ND500-IF-LOCKING.md](ND500-IF-LOCKING.md) | Interface lock state machine (corrected 2026-07-08) |
| [ND500-MONITOR-CALL-MECHANISM.md](ND500-MONITOR-CALL-MECHANISM.md) | ND-500 -> ND-100 monitor call dispatch (5STDRIV -> DECOMESS -> MCHANDLE) |
| [ND500-MONITOR-CALL-PARAMETER-PASSING.md](ND500-MONITOR-CALL-PARAMETER-PASSING.md) | Parameter passing, response write-back, extended MON calls (>255) |
| [ND500-SCHEDULING-ANALYSIS.md](ND500-SCHEDULING-ANALYSIS.md) | Execution queue, timeslicer, process scheduling |
| [ND500-SWAPPER-LOADING-MECHANISM.md](ND500-SWAPPER-LOADING-MECHANISM.md) | How the swapper is loaded (INZ500, MSINIT, 5SWRT) |
| [ND500-SWAPPER-ANALYSIS.md](ND500-SWAPPER-ANALYSIS.md) | Swapper FIFO/queue mechanics (corrected 2026-07-08: 5SWAP runs on the ND-100) |
| [ND5000-SAMSON-ARCHITECTURE.md](ND5000-SAMSON-ARCHITECTURE.md) | ND-5000 (SAMSON) vs ND-500: Octobus, MFbus, ACCP |
| [WHERE-IS-5MPM-LOCATED.md](WHERE-IS-5MPM-LOCATED.md) | 5MPM is separate multiport hardware with BASE translation (canonical statement) |
| [SINTRAN-DOMAIN-SETUP-DEEP-DIVE.md](SINTRAN-DOMAIN-SETUP-DEEP-DIVE.md) | Domain / process-descriptor / segment-capability walkthrough |
| [ND500-INITIALIZATION-AND-EXECUTION-GUIDE.md](ND500-INITIALIZATION-AND-EXECUTION-GUIDE.md) | Operator guide: domains, PLACE-DOMAIN, PSEG/DSEG (section 2 rewritten 2026-07-08) |

## Binary artifacts

| Folder | Contents |
|--------|----------|
| [swapper/](swapper/README.md) | The ND-500 swapper binary itself (SWAPPER-K01 PSEG/DSEG), its disassembly, the resident monitor symbol table (N500-SYMBOLS.SYMB, 7157 symbols), and the RE analysis |

## Tier 3 - Raw NPL source analyses

Line-referenced analyses of the driver modules; foundational input for the deep
dives. Their embedded early IOX table has been superseded by the master reference.

| Document | Source module | Content |
|----------|---------------|---------|
| [MP-P2-N500.md](MP-P2-N500.md) | MP-P2-N500.NPL | Main driver: 5STDRIV, XACT500, XTER500, MCHANDEL, DVIO |
| [CC-P2-N500.md](CC-P2-N500.md) | CC-P2-N500.NPL | Command/control: 5MCST, SLOCK/SUNLOCK, ITO500XQ |
| [RP-P2-N500.md](RP-P2-N500.md) | RP-P2-N500.NPL | RT level: 5SWRT, XMSINIT, N500SCHEDULER |
| [XC-P2-N500.md](XC-P2-N500.md) | XC-P2-N500.NPL | CLE5STATUS, status masks, FIFO ops |

## Tier 4 - Retired (do not use as sources)

[old/README.md](old/README.md) explains why each was retired and what supersedes it:

| Document | Reason |
|----------|--------|
| [old/ND500-BOOT-DETECTION-MECHANISM.md](old/ND500-BOOT-DETECTION-MECHANISM.md) | Fabricated DETECTND500 with reversed detection polarity |
| [old/ND-500-INTERFACE.md](old/ND-500-INTERFACE.md) | Early guesswork register table |
| [old/MP-P2-N500_API_Documentation.md](old/MP-P2-N500_API_Documentation.md) | Duplicate of MP-P2-N500.md |

---

## ND-500 architecture in brief

- The ND-500 is a **byte-addressed** CPU (8-bit bytes, 16/32-bit operations, 32-bit
  memory bus for bandwidth); the ND-100 is a **word-addressed** 16-bit CPU. The
  "32-bit" in ND-500 marketing refers to the memory bus, not a word size.
- The two machines communicate through the **PCB 3022** (ND-100 side) / **PCB 5015**
  (ND-500 side) interface and shared message memory, with level-12 interrupts to the
  ND-100 - see the master reference.
- ND-500 programs run in **domains** (process spaces) with program/data segments and
  segment capabilities; monitor calls are serviced by SINTRAN on the ND-100.

## Related documentation

- `../OS/` - OS-level companions: [IOX-REGISTER-COMPLETE-REFERENCE.md](../OS/IOX-REGISTER-COMPLETE-REFERENCE.md),
  [MPM5-KEY-FINDINGS.md](../OS/MPM5-KEY-FINDINGS.md),
  [06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md),
  [08-MESSAGE-PASSING-DETAILED.md](../OS/08-MESSAGE-PASSING-DETAILED.md),
  [12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md](../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md)
- `../Emulator/` - emulator artifacts. WARNING: the interface model in
  ND500-QUICK-REFERENCE.md and DETAILED-TAG-MECHANISM-EXPLANATION.md is the OLD
  fabricated one; validate anything from that folder against the Tier 1 docs.
- `../NPL-SOURCE/` - the SINTRAN III sources and symbol tables (ground truth).
- `../../Reference-Manuals/500/` - the hardware manuals, especially
  "ND-30.013.02 Test Micro Program Descriptions for ND-500" (register-level spec of
  both interface cards) and "ND-10.004.01-MPM 5 Technical Description".

---

## Version history

| Date | Version | Changes |
|------|---------|---------|
| 2026-07-08 | 2.0 | Folder overhaul: added the three Tier 1 docs (bus-interface reference, evidence dossier, emulator audit); retired 3 fabricated/duplicate docs to old/; corrected IF-LOCKING, SWAPPER-ANALYSIS and the initialization guide; rewrote this index |
| 2026-01-29 | 1.8 | Added ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md |
| 2026-01-29 | 1.3-1.7 | Deep-dive additions (scheduling, monitor calls, swapper, SAMSON) |
| 2025-10-17 | 1.0 | Initial ND-500 documentation structure |

---

**Parent:** [../README.md](../README.md) - SINTRAN Documentation
**Related:** [../Emulator/](../Emulator/) - Emulator Implementation
