# The generic DOMINO controller (DIOC) - what is documented, and what it would take to build one

**Date**: 2026-07-28
**Question this answers**: the ND manuals describe a generic hardware interface for octobus
messages with a per-controller implementation below it (MFbus controller, SCSI, Ethernet III).
Is it documented well enough to build these controllers and hook them to the octobus?

**Short answer**: yes for the generic controller and the MFbus register interface; yes for the
octobus protocol; partially for SCSI; **almost not at all for Ethernet III**.

---

## 1. The generic/device-dependent seam is drawn by the manual itself

`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-14001-1-EN DOMINO Standard Hardware Description.md`,
**Figure 22 "A typical DOMINO design environment"** - markdown `## Page 94`, printed page 78.
Original scan: `F:\NDDOC\ND\14\ND-14001-1-EN Domino Standard Hardware Description.pdf`, PDF page 94.

The figure marks every block with `*` (standardized hardware design) or the delta symbol
(device-dependent hardware design):

| Marking | Block |
|---|---|
| **Standard** | OCTObus |
| **Standard** | **OCTObus Adapter (OBA)** - centred on the OBCON gate array |
| **Standard** | Console and Trace Connector |
| **Standard** | **DOMINO Logic, CPU part**: MC68020, DRAM / EPROM / EEROM, RTC / Interrupt, Memory Protect, MFP (Multifunction Peripheral) chip |
| **Standard** | **MFbus Adapter (MFA)** |
| **Standard** | MFbus |
| **Device-dependent** | **DOMINO Logic, Device part**: logic, Request Arbiter |
| **Device-dependent** | the DEVICE itself |

**Everything except the device part and the device is fixed silicon common to every DOMINO
controller.** That is the whole answer to "can we build a generic 68k octobus controller": ND
already did, and documented the boundary.

## 2. The software layering has the same shape

`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-820026-1c-EN DOMINO and NUCLEUS Software Guide.md`,
Figure 5 "SERVER path to DOMINO controller":

```
DOMINO OPCOM         <- PROM. Common to every DIOC. Octobus and terminal interrupt drivers,
                        hardware tests, DOMINO Monitor command execution.
    |
DOMINOS              <- the OS. Processes, events, buffers, timers, trap ABI.
    |
DOMINO application   <- SCSI / Ethernet III / MFbus-specific. THIS is where controllers differ.
    |
Device
```

Above that, **NUCLEUS** is the generic message API - chapter 7 documents create port, create
port name, open port, open return port, delete port name, create message, read/write message,
send, receive, get info, close, get version, with status codes and a PLANC worked example.

**XMSG is not part of this stack on the controller side.** Figures 2 and 5 both put XMSG
between the DOMINO Monitor and the BOPCOM server *inside the ND-100*, with OCTObus below it:

```
DOMINO monitor --XMSG--> BOPCOM server (RT-program in ND-100) --OCTOBUS--> DOMINO controller
```

A 68k controller never sees XMSG.

## 3. One constraint that shapes any controller you build

ND-820026: OCTObus is "a serial bus intended for sending **short** messages. It is mainly used
for **process synchronization**. During initialization it passes configuration parameters."

So the division of labour is:

- **OCTObus** - control, synchronization, configuration. Short messages only.
- **MFbus / MPM shared memory** - the actual payload.

A controller that tries to move bulk data over the octobus is built wrong.

## 4. What a module must implement to be discovered

ND-14001 chapter 3, section 3.5 "Programming the MFA by the MFbus controller". Address format
is `slot number (5) | module type (6) | B | register`:

| Reg | Read | Write |
|---|---|---|
| 0 | Read Module Type (RMT) | Write Module Type (WMT) |
| 1 | Read Master Status (RMS) | Write Master Control (WMC) |
| 2 | Read ECO Level (RECOL) | **Write OCTObus Initial values (WOI)** |
| 3 | Read Device Status (RDS) | Write Device Control (WDC) |
| 6 | - | Write Limits (WLI) |

- **RMT** returns `slot no (5) | module type (6) | 1 | model (4)`. Module type and model are
  hardwired on the PCB. **An RMT of zero means an empty slot** - that is how the MFbus
  controller finds cards.
- **RMS** carries OE (OCTObus enable, a copy of WMC bit 7), DIS (interleave setting), DIB
  (interleave bank), PI (pirate), MB (mailbox), EN (enable master request).
- **WOI** is written **twice** because the BADAP data path is only 8 bits wide. It carries
  **STANO** (5-bit octobus station number), **PF** (station of the power-fail handler, split
  across the two bytes) and **BT** (broadcast type).
- **WDC** - "present DIOC designs do not use this register."
- **Write Limits** loads the limit RAMs with the DIOC's address limits in MFbus address space;
  after initialization they are read-only.

**The mailbox bit only works module-to-controller.** The manual is explicit that there is no
free bit in the other direction, so "any communication in that direction must be done via the
OCTObus". That is the architectural reason the ACCP's discovery exchange is an octobus message
rather than a register poll.

## 5. How a node gets its station number

ND-14001 section 4.8.1. Rules, **all octal**:

- devices on the **global** OCTObus: `0` to `17`
- devices on the **local** OCTObus (MFbus backwiring): `77` down to `20`
- station numbers must be unique; **0 and 63 are illegal** (chapter 4.4)

Global nodes are set by **thumbwheel switches**. **Local nodes are initialized by the MFbus
controller** into on-board registers.

Two-phase initialization:

- **Phase I** - automatic. Global nodes initialize; local nodes are inhibited. Global nodes
  elect a MASTER, normally the lowest station number, usually the ND-100.
- **Phase II** - the configurator broadcasts "Identify yourself". Only global nodes can answer;
  the rest have no station number and are inactive. The configurator then orders the MFbus
  controller with the highest station number to configure its crate.

What the MFbus controller then sends to its devices is **MFbus PIO register cycles, not octobus
frames**:

1. read **RMT** from every slot; zero means empty
2. write **WOI** twice - station number, power-fail handler, broadcast type
3. write **bit 7 of MASTA** at slot address + `4` octal, raising OBRES to reset OBCON and start
   the node

Then it sends "Identify yourself" to station `77` octal and works downwards to find a free
number, and finally a "Finish" message to the configurator.

**Slot address arithmetic**: slot number in octal, multiplied by 2, followed by four zeros. So
address `10` octal in slot 14 (decimal) = `16` octal, doubled = `34` octal, giving `340000`
octal, plus `10` octal = `340010` octal.

## 6. Per-controller documentation status

| Controller | Module no. | What exists |
|---|---|---|
| **MFbus controller** | - | **Good.** ND-14001 chapter 3, both directions (3.5 controller-side, 3.6 DOMINO-processor-side) |
| **SCSI** | - | `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-814009-1-EN DOMINO SCSI Operator Guide.md` is an **operator** guide, not a register spec. The real work is the RetroCore DOMINO SCSI DIOC/BDIO already built and verified |
| **Ethernet III** | **22B** | **Almost nothing.** Three passing mentions across the whole repository: the module-number table, one slot listing, and a note that TCP/IP, COSMOS and SIBAS-communication all run on it. No hardware description, no register map, no firmware. The Ethernet **II** manual (`E:\Dev\Ronny\NDInsight\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md`) is a different card |

A real configuration listing from ND-820026 showing how they appear:

```
SLOT 11 : Crate id 3 Octobus station 13B ---> SCSI CONTROLLER
SLOT 10 : Crate id 3 Octobus station 12B ---> ETHERNET III CONTROLLER
```

## 7. The ACCP is NOT a DIOC - do not derive one from the other

This matters for reuse and was nearly got wrong.

| | Standard DIOC (ND-14001 Figure 22) | ACCP (carved) |
|---|---|---|
| Processor | **MC68020** | 68000-class |
| Serial / timer / interrupt | **MFP** (Multifunction Peripheral) chip | **SCN2681 DUART at `0xDD0000`** |
| MFbus memory access | through the **MFA**, addressed directly | through the **ND-5000 datapath** - command port `0x220000`, address split across `0x440000` / `0x550000` |
| Documented in | ND-14001 (DOMINO standard hardware) | **ND-05.017.01 chapter 3.6** (ND-5000 hardware maintenance) |

The ACCP is ND-5000 CPU hardware, not a DOMINO I/O controller. Consequences:

- **Transfers from the ACCP carve**: the **octobus layer** - frame format, the E/K/M/S
  information byte, CMD routing, multibyte start/end framing, kick and ident semantics. This is
  genuinely common to everything on the bus.
- **Does not transfer**: the memory model, the serial hardware, the command port. All
  ACCP-specific.

**Implication for RetroCore**: factor the octobus layer out on its own rather than deriving a
generic DIOC from the ACCP machine, or subclassing one from the other.

## 8. What is missing

The **information byte encoding** was the big gap and is now closed - see section 1a of
`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`,
decoded from ND-05.017.01 section 3.3.1.

Still unlocated:

- **"OCTObus Protocol Specification"** - cited four times by ND-14001 on printed page 110, no
  ND number given in any related-manuals list. ND-14.002 is an unclaimed number in both
  manuals and is a plausible slot - **guess from numbering alone, not evidence.**
- **"Octobus Driver Programming Guide, written by DVT, 15. Oct. 1986"** - cited by ND-05.017.01
  chapter 8 for the per-function detail of the octobus driver.
- Any Ethernet III hardware documentation or firmware.

---

## Cross-references

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md` - the octobus protocol and the ACCP driver, including the decoded information byte
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-324716-FIRMWARE-RE-2026-07-27.md` - the ACCP firmware reverse engineering
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-HARDWARE-ADDRESS-MAP-2026-07-27.md` - the ACCP hardware register map
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-RETROCORE-MACHINE-IMPLEMENTATION-HANDOFF-2026-07-27.md` - the RetroCore ACCP machine
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-MACHINE-DEFECT-REPORT-2026-07-28.md` - defects found in that machine

## Provenance

Sections 1 through 6 are transcribed from ND-14001-1-EN and ND-820026-1c-EN and are
documentation, not inference. Figure 22's standard/device-dependent marking was read from the
original PDF scan as well as the markdown, because the markdown renders that figure as a mangled
table.

Section 7's comparison mixes documentation (the DIOC column, from ND-14001) with carved fact
(the ACCP column, from `octo.bin`). The conclusion that the ACCP is not a DIOC is **inference
from that comparison** - a different processor class, a DUART instead of an MFP, and a
CPU-datapath memory path instead of an MFA. It is well supported but nobody has found a
sentence in an ND manual that says it outright.
