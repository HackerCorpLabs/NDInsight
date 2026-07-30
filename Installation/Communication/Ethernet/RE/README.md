# ND Ethernet II Controller - Reverse-Engineering Hub

Reverse-engineering material for the Norsk Data **Ethernet II controller** (ND-110063 / PCB 3094):
a MC68000 front-end board that runs the **ENCOS** firmware on top of a small real-time OS of the
**PIOCOS/PIOC** lineage. This hub indexes everything we know and points at the raw RE artifacts.

> Naming note: "Ethernet II" here is the Norsk Data *product* name (their 2nd-generation Ethernet
> board), NOT the DIX "Ethernet II" wire-framing standard. The board does emit DIX frames on the wire
> (via its Am7990 LANCE), but the firmware documented here is ND's card software.

## What runs where (verified from install object types, `ND-210580-02-EN`)

| Object | Type | Role | Runs on |
|--------|------|------|---------|
| `ENCOS-SER-B0..B3` | BPUN (bank images) | Server for 110063 | **The card (68K)** - the firmware itself |
| `ENCOS-ERR-II` | BRF | Supervisor for 110063 | Host ND-100 (RT program) |
| `ENCOS-MON-II` | PROG | Monitor for 110063 | Host ND-100 (`@(UTILITY)ENCOS-MON`) - loader/monitor utility |
| `ENNS0` | RT program | COSMOS Ethernet IOC Server, sysid 9800 | Host ND-100 (`@RT ENNS0`) - the live driver |
| `ENCOS-LOAD-D` | MODE/procedure | Loads the SER banks into the card at cold-start | Host ND-100 |

The card has **no EPROM**: the ND-100 host loads all 512 KB of code+data into card DRAM, then releases
the 68000 from reset. The combined image we reverse is `encos-ser-all-banks-68k.bin`.

## Contents of this hub

- [PIOCOS/](PIOCOS/README.md) - the RTOS: scheduler/coroutine core, LOC-XMSG client, memory map.
  - [PIOCOS/ARCHITECTURE.md](PIOCOS/ARCHITECTURE.md) - whole-RTOS map (layered stack diagram + memory map).
  - [PIOCOS/LOC-XMSG-CLIENT.md](PIOCOS/LOC-XMSG-CLIENT.md) - the on-card XMSG client + reply contract.

## Raw RE artifacts (existing, elsewhere in the tree)

- Firmware decode corpus: [../x/stripped/docs/ND_EthernetII_68000_Firmware_COMPLETE.md](../x/stripped/docs/ND_EthernetII_68000_Firmware_COMPLETE.md),
  [_QuickMap](../x/stripped/docs/ND_EthernetII_68000_Firmware_QuickMap.md),
  [_ReverseEngineering](../x/stripped/docs/ND_EthernetII_68000_Firmware_ReverseEngineering.md),
  plus `FIRMWARE-SUBFUNCTION-5-DECODE-2026-07-23.md`, `ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md`.
- Coroutine-kernel + XMSG bring-up decodes: [../../../../SINTRAN/XMSG/DOC/COSMOS-RE/](../../../../SINTRAN/XMSG/DOC/COSMOS-RE/)
  (esp. `FIRST-SUPERKICK-BRIDGE-DECODE-2026-07-23.md`, `ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md`,
  `ENNS0-Startup-RE-2026-07-23/`).
- Board schematic / netlist: [../Schema/](../Schema/).
- Emulator target (separate repo, RetroCore): the phased RTOS RE plan + early architecture diagram live
  at `RetroCore\Emulated.HW\ND\CPU\NDBUS\EthernetII\ETHII-68K-RTOS-RE-PLAN.md`, and the host-side
  transport spec at `...\EthernetII\ENNS0-MBOXH-XMSG-BRINGUP.md`.

## Conventions

- Every claim is tagged **[V]** verified in the firmware bytes / Ghidra / an ND manual, or **[U]**
  UNVERIFIED / inferred. Do not promote [U] to [V] without a byte/address/manual citation.
- Ghidra is the oracle for the firmware (no python disassemblers). All firmware addresses in HEX.
