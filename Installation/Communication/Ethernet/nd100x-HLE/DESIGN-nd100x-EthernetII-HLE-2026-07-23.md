# nd100x High-Level-Emulation (HLE) ND Ethernet II Controller - DESIGN

Date: 2026-07-23
Target: the WSL C emulator `nd100x` (`~/repos/nd100x`), the C port of the ND-100.
Scope: EVALUATION + DESIGN only. No production code. Ends in this document.

Convention (inherited from the RE docs):
- `[V]` = VERIFIED (decoded bytes / read from committed source / official manual).
- `[I]` = INFERRED (strongly supported, not fully traced).
- `ASSUMPTION:` = a design choice or belief NOT yet verified against source. Nothing
  speculative is stated as fact.
- Addresses hex unless suffixed `B` (octal, ND convention). Memory sizes in WORDS primary,
  bytes in parens. ND-100 is 16-bit word-addressed.

Primary sources (all read for this design):
- `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_MASTER_REFERENCE_2026-07-23.md` (18-section master ref)
- `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_TCP_Network_Bridge_PLAN_2026-07-23.md` (backend seam)
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs` (chip-level reference impl)
- `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\Schema\Analysis\EthII-interrupt-clock-netlist.md` (gate-level SCIP/INT12)
- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\` (ENNS0 + PISTA + superkick RE)
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL` (kernel PIOCM / PISTA driver, the ND-100 side that actually does the IOX)
- nd100x device model: `~/repos/nd100x/src/devices/` (device.c, devicemanager.c, hdlc/*)

---

## 1. FEASIBILITY VERDICT

**QUALIFIED YES.** The 68K + MFP + LANCE can be replaced by direct C code that speaks only
(a) the ND-100 <-> card register + DRAM-mailbox protocol and (b) raw Ethernet frames to a
network backend. The central RE insight HOLDS and is verified:

- **[V]** SINTRAN/ENNS0 never touches the LANCE, MFP, or 68K. ENNS0 has ZERO IOX/IOXT
  (`ANNOTATED-ENNS0-DISASSEMBLY-2026-07-23.md`, opcode-exact: only two MON 200B). All
  card access is SINTRAN monitor calls; the only code that does the actual IOX to the card
  is the SINTRAN kernel PIOCM (MON 255B) driver PISTA in `RP-P2-PIOC.NPL`. The kernel
  touches the card through exactly TWO surfaces: (1) the STATUS/PWCR I/O registers on
  device `HDEV=177775B`, and (2) the PIOC-memory bank window (mailbox words). It never
  reads a LANCE CSR or a DMA ring.
- **[V]** The card->host "server is alive / data arrived" signalling is entirely SCIP ->
  INT12 (level 12) plus the mailbox fields; the ND-100 driver polls the PRKEY cell and the
  mailbox, not the LANCE. (`EthII-interrupt-clock-netlist.md` section 4; master ref
  section 5c/6.)
- **[V]** On the wire side, the chip-level RetroCore model already proves the card is fully
  decoupled from the LANCE by the `IEthernetBackend` seam: `LANCE.OnPacketTransmit ->
  backend.SendPacket`; `backend.OnPacketReceived -> LANCE.EnqueueReceivedPacket`. The LANCE
  is a pure frame mover in and out of DRAM rings - an HLE that owns the DRAM can emit/accept
  frames directly and skip the rings entirely.

Therefore an HLE that (1) presents the STATUS/CONTROL registers + level-12 IDENT, (2) posts
PRKEY + drives the mailbox handshake exactly as the firmware does, (3) turns an ENNS0/COSMOS
"send" mailbox request into a raw frame to a backend, and (4) turns an inbound raw frame into
a mailbox delivery + SCIP superkick, is behaviorally indistinguishable to SINTRAN from the
real card - with NO 68K, NO MFP, NO LANCE rings/DMA.

**Why it is only QUALIFIED (the risks, expanded in section 4):**
- The mailbox/register/PRKEY protocol is VERIFIED. But the *content* that actually makes
  ENNS0 progress past its input-wait is a COSMOS/XMSG-over-Ethernet payload whose on-wire
  frame format is NOT yet fully reverse-engineered. The chip-level model reaches "ENNS0
  starts + registers + transmits real frames" today, but it does so by running the real 68K
  XMSG/XROUT firmware which builds those frames for us. An HLE must synthesize them itself.
- nd100x today has no concept of a device-owned memory-mapped bank window (all its devices
  are pure IOX register devices). The mailbox lives in the PIOC bank, so the plumbing to
  let the ND-100 and the HLE share those words must be added. This is mechanical, not
  research, but it is new surface in nd100x.

Net: the ND-100 <-> card CONTRACT is implementable now; reaching a *fully serving* COSMOS
node additionally needs the COSMOS Ethernet frame format closed (gap 1). A first HLE can be
built and validated against the RetroCore oracle up to and including "ENNS0 starts and
transmits", which is exactly the milestone the chip-level model already reaches.

---

## 2. THE ND-100 <-> CARD CONTRACT TO IMPLEMENT

This is the complete observable protocol the HLE must honor. Everything here is the ND-100
side; the 68K side is what the HLE *replaces*.

### 2.1 I/O registers (device `HDEV = 177775B`)

**[V]** (master ref section 4a/4b; `RP-P2-PIOC.NPL`; the card answers on device base
`HDEV`, control at `HDEV+3 = PWCR`). Access is IOXT with the register offset in T.

STATUS register (read, base+0 / base+2):

| Bit(s) | Name | Meaning |
|--------|------|---------|
| 15:8 | Bank number | which physical bank the PIOC DRAM window appears at (RetroCore default 0x10 = bank 16). The kernel reads STATUS only to extract this. |
| 6 | Memory is 512KB | ALWAYS 0 in the reference impl |
| 5 | Halt | processor halted |
| 4 | Reset Active | processor in reset |
| 2 | INT12 set for ND-100 | `= RFT AND RIE` (NOT raw RFT). The "ready/doorbell" edge. |
| 0 | Interrupt Enabled onto bus (RIE) | echoes CONTROL bit 0 |

CONTROL word (write, base+1 / base+3 = PWCR):

| Bit(s) | Name | Effect |
|--------|------|--------|
| 0 | Enable SCIP interrupt (RIE) | gates delivery of a pending SCIP -> INT12 |
| 2 | ND Interrupt | host->card command doorbell (real HW: MFP GPIP I6). In HLE = "process the request block now" |
| 3 | Start OPCOM | second host->card kick (real HW: 68K level-6). In HLE = same "process request" trigger |
| 4 | Reset | processor reset; the falling edge (out of reset) is the firmware (re)start |
| 5 | Halt | run/halt |
| 6 | Power low (PLOWE) | power-low ENABLE, not a trigger |

PISTA uses only three CONTROL values [V] (`RP-P2-PIOC.NPL`, `ENNS0-PIOCM-START-FINDINGS`):
`PWCR = 60B` = Halt+Reset; `PWCR = 0` = INITIATE (the one doorbell that starts the card);
`PWCR = 11B` = start command (= EnableScipInt | StartOpcom).

### 2.2 The SCIP doorbell -> INT12 (level 12) mechanism

**[V]** (`EthII-interrupt-clock-netlist.md` section 4.4/4.5; master ref 5c):
- The card raises INT12 by "writing SCIP" (real HW: 68K writes 0xEF0080/0xEF0180). In HLE
  there is no 68K; the HLE simply asserts the RFT latch when it wants to signal.
- `RFT` is a latched flip-flop set by the SCIP event regardless of RIE. It survives
  CONTROL-word writes. `BINT12 = RFT AND RIE`; STATUS bit 2 = RFT AND RIE.
- A doorbell fired while RIE=0 stays PENDING and asserts INT12 the moment the driver sets
  RIE (CONTROL bit 0).
- **IDENT answer clears BOTH RFT and RIE** (they share `CLINT0`); after IDENT the
  enable bit reads back 0 until the driver rewrites CONTROL. Master Clear / delayed
  power-low clear also clears them.
- IDENT code: the card answers level-12 IDENT with its identcode from PROM. RetroCore
  device number family `140360..` / IDENT `2240..`; the ENNS0 device LU is `2240B`
  (`ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B`). ASSUMPTION: exact identcode value to
  return must be taken from the RetroCore constant / PROM byte, not guessed (see gap 4).

HLE mapping onto nd100x primitives (section 3.4): RFT/RIE become two bools; asserting INT12
= `Device_GenerateInterrupt(dev, 12)` when `RFT && RIE`; the IDENT handler clears RFT+RIE
and returns identcode, exactly like `HDLC_Ident` clears its transfer-control latch.

### 2.3 The DRAM mailbox field map (PIOC bank window)

The mailbox is a set of words in the card DRAM, seen by the ND-100 through the PIOC bank
window. **[V]** field map (master ref section 6a; `ethii-mailbox-tracer.md`; the RetroCore
`EthMailboxTracer`). Addresses below are the 68K byte offsets used throughout the RE; the
ND-100 sees the same cells as WORDS in the PIOC bank (section 2.5 for the address relation).

| 68K byte off | Field | Direction | Meaning |
|--------------|-------|-----------|---------|
| 0x0404 | PRKEY cell (= PIOC word 1002B) | card -> host | firmware writes `PRKEY = 052163B (0x5473)` when ready. **[V]** exact: one instr `0x1CF4 move.w #$5473,($404).L` |
| (word 1001B) | PIOC datafield pointer (MASTA) | card -> host | published so the kernel can find the request block. ASSUMPTION on exact cell offset - see gap 3 |
| 0x406 | REQUEST | host -> card | nonzero = valid; kernel writes `MPIOC = 5` here (MASTA+NPFUN, NPFUN=1). Zero -> firmware D0 = -5 |
| 0x408 | SUBFUNCTION | host -> card | 0 = start-network-server; 5 = the ENNS0 status exchange (normal ACK) |
| 0x40A | MON_COUNTER | card -> host | bumped each post |
| 0x40C | MON_CODE | card -> host | 2 = sync/wait, 3 = ready, 4 = warm-boot (signed: +1 ACK, -2/-4/-5 err) |
| 0x40E | MON_PARAM | card -> host | (SUBFN5 replies PARAM = 0x1E = OPCOM vector#, a constant) |
| 0x410 | MON_COUNTER2 | card -> host | second counter, bumped each post |
| 0x412 | MON_REQFLAG | card -> host | set 1 by firmware on each post |
| 0x4C0 | STARTED_FLAG | card -> host | firmware sets 1 when server up |
| 0x454 | REGDUMP_FRAME | card -> host | 68K reg frame on trap (HLE: not needed) |
| 0x4BA | WARMBOOT_MAGIC | card -> host | 0x55555555 (HLE: not needed) |
| 0x0B56 | ND channel flags (8 words) | host -> card | per-channel doorbell the 68K scans; HLE reads to know which channel the host kicked |
| 0x1885E | MAC address (6 bytes) | host -> card | host writes card MAC at bring-up (HLE: source MAC for TX) |

**[V] CAUTION** carried from the RE: the TPE/diagnostic firmware uses a DIFFERENT map at the
SAME cells (0x406 = "CMD_TEST_NUM" in diagnostic vs REQUEST in production). Use the
PRODUCTION names only. LANCE ring headers (0x18000.., 0x18408..) are DELETED in HLE - there
is no LANCE.

### 2.4 The startup handshake state machine (what the HLE reproduces)

**[V]** from PISTA (`RP-P2-PIOC.NPL`, `ENNS0-PIOCM-START-FINDINGS`) and the firmware
fixpoint (`ENNS0-PRKEY-FIRMWARE-FIXPOINT`). PISTA (kernel, T=6) drives this; the HLE plays
the card:

```
Kernel PISTA                                 HLE card (replaces 68K)
------------                                 -----------------------
read STATUS (extract bank)
PWRIT word 1002B := 0  (zero PRKEY)
IOXT PWCR := 60B  (Halt+Reset) ------------> enter RESET state
IOXT PWCR := 0    (INITIATE)   ------------> RESET released:
                                               publish datafield ptr @ word 1001B
                                               write PRKEY 052163B @ word 1002B (0x0404)
                                               post MON_CODE = 3 (READY), set STARTED path
busy-poll word 1002B for PRKEY  <----------- (PRKEY now present)
  (3-second timeout: -3 =: TMR)
[PRKEY seen] read datafield ptr -> MASTA
write REQUEST = MPIOC(5) @ MASTA+NPFUN
write TRIG = 1 @ MASTA+NPTIG
IOXT PWCR := 11B (start command) ----------> process request block: read REQUEST/SUBFUNCTION,
                                               set STARTED_FLAG = 1, ACK via MON_CODE,
                                               SCIP -> INT12
mark ENNS0 STARTED
```

**[V] The exact past bug this fixes:** the real 68K firmware, if not restarted, reposts
MON_CODE=2 in its own monitor-sync loop instead of re-writing PRKEY after a second
RESET+INITIATE, deadlocking. The HLE has NO such loop - it writes PRKEY unconditionally on
every INITIATE, so the deadlock cannot occur. This is a *simplification* the HLE gets for
free.

### 2.5 ND-100 word address of the mailbox (the PIOC bank relation)

**[V]** constants (symbol tables K03/L07/M06 identical, `ethii-start-gate-prkey.md`):
`PIOC memory is mapped at bank offset (word + 2000B)`; `HDEV = 177775B`. So PIOC logical
word 1002B is at ND-100 physical `bank*0x10000 + (1002B + 2000B)` where `bank` = STATUS
bits 15:8. The 68K byte offset 0x404 (= word 0x202 = 1002B) is the SAME cell viewed as
bytes. The HLE must expose these words at the correct ND-100 physical address (section 3.4).
ASSUMPTION: the exact `+2000B` base and word/byte correspondence for EVERY mailbox cell (not
just PRKEY) should be re-derived per-cell from PISTA + the 68K writes before relying on it
(gap 3).

### 2.6 Send / receive request-response cycle (COSMOS frames)

- **Host -> card TX:** ENNS0/COSMOS puts an outbound message into the mailbox/postbox and
  rings CONTROL bit 2 (or OPCOM). The real firmware's XMTRINGAPP builds an Ethernet header
  (source MAC from 0x1885E, pad to 60 bytes) and DMAs it out. **[V]** observed on-wire:
  DST MAC low 2 bytes carry the little-endian ND sysid (sysid 17848 = 0x45B8 ->
  `08:00:26:b8:45:00`); SRC = card MAC `08:00:26:64:00:00`. The HLE reads the outbound
  payload from the mailbox/postbox and emits the frame straight to the backend. The exact
  postbox-ring layout that carries the payload is `[I]` (gap 3).
- **Card -> host RX:** a frame arrives from the backend. The real firmware's RCVCOMPLETE
  does a software MAC recheck against 0x1885E, and ONLY a valid inbound COSMOS/XMSG frame
  drives make-runnable (0x259A) -> postbox producer (0xEAA6) -> SCIP (0xEF0180) -> INT12 ->
  SINTRAN PDRIV -> XRTEN, waking ENNS0's LU-2240B input read (the "superkick / RTWAK").
  **[V]** (`FIRMWARE-SUPERKICK-TRIGGER-WHY-NO-INT12`, `MAKE-RUNNABLE-TRIGGER-AND-FIX`,
  `ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B`). The HLE must: MAC-filter the frame, place it
  in the RX mailbox/postbox in the format SINTRAN expects, and assert the SCIP superkick.
  The frame-format detail is gap 1.

---

## 3. HLE ARCHITECTURE FOR nd100x

### 3.1 How nd100x devices work (verified from source)

- A device is a `Device` struct (`src/devices/devices_types.h`) with function pointers
  `Reset/Tick/Read/Write/Ident/Boot/Destroy`, an address range `[startAddress,endAddress]`,
  `interruptBits`, `interruptLevel`, `identCode`, `logicalDevice`, and a `void *deviceData`.
- Registration: a `CreateXxxDevice(thumbwheel)` factory (see `CreateHDLCDevice`) allocates
  the struct + its private data, calls `Device_Init`, fills addresses/ident/level, wires the
  function pointers, and is added via `DeviceManager_AddDevice`.
- Dispatch (`devicemanager.c`): `DeviceManager_Read/Write` linear-scan by address and call
  `Device_Read/Write`; `DeviceManager_Ident(level)` scans for a device whose
  `interruptBits` has the level set, calls `Device_Ident`, and on a nonzero id CLEARS that
  level bit (the IDENT = interrupt-acknowledge). `DeviceManager_Tick` ORs every device's
  `Tick` return into the pending interrupt bits.
- Interrupts: `Device_GenerateInterrupt(dev, level)` sets `interruptBits |= (1<<level)`
  (levels 10-13). The device clears it in its `Ident` (see `HDLC_Ident` clearing its
  transfer-control latch + `Device_SetInterruptStatus(self,false,level)`).
- DMA to ND-100 memory: `Device_DMAWrite(coreAddr,data)` / `Device_DMARead(coreAddr)` wrap
  `WritePhysicalMemory/ReadPhysicalMemory` (flat `VolatileMemory.n_Array`, 24-bit word
  address). This is how a device reaches ND-100 core memory today.
- Networking template ALREADY EXISTS: `src/devices/hdlc/modem.c` runs all socket work on a
  background worker thread (connect/accept/reconnect/recv/send) via `ndlib/net_compat.h`,
  and hands received bytes to the emulation thread through a buffer. This is the exact
  pattern for an Ethernet backend and must be REUSED, not reinvented (mirrors the RetroCore
  `IEthernetBackend`/pcap/tcp seam).

### 3.2 Module structure (proposed, C-appropriate, matches house style)

New directory `src/devices/ethernet/` (mirrors `hdlc/`, `scsi/`):

- `deviceEthernetII.h/.c` - the `Device` factory `CreateEthernetIIDevice(thumbwheel)`,
  the STATUS/CONTROL register `Read`/`Write`, `Ident`, `Tick`, `Reset`. Owns the HLE state.
- `ethIIMailbox.h/.c` - the mailbox field accessors (get/set REQUEST, SUBFUNCTION, PRKEY,
  MON_CODE, STARTED_FLAG, channel flags, MAC) against the PIOC-bank backing store, plus
  the [MBX]-style trace. This is the DELETED-DRAM's replacement: a small backing buffer,
  NOT 512KB.
- `ethIIState.h/.c` - the startup + request/response STATE MACHINE (section 2.4), driven
  from `Write` (CONTROL doorbells) and from inbound frames.
- `ethIIBackend.h/.c` - `IEthernetBackend`-equivalent: `null`, `tcp` (point-to-point and
  relay), later `pcap`. REUSE the `hdlc/modem.c` worker-thread + `net_compat.h` machinery;
  frame framing = `[u16 BE length][raw Ethernet frame, no FCS]` exactly as the RetroCore TCP
  bridge plan specifies, so an nd100x node and a RetroCore node can share one segment.
- `ethIICosmos.h/.c` - COSMOS/XMSG-over-Ethernet frame encode/decode (the gap-1 surface,
  isolated here so the rest is testable without it).

### 3.3 State machine (the core of the HLE)

```
RESET        --(CONTROL PWCR=0 INITIATE)-->  READY_POSTED
  on entry to READY_POSTED:
    write datafield ptr @ word 1001B
    write PRKEY 052163B @ word 1002B
    MON_CODE := 3 (READY); MON_REQFLAG := 1; bump counters
    (do NOT assert SCIP yet - PISTA is memory-polling PRKEY, not waiting on INT12)
READY_POSTED --(CONTROL PWCR=11B start, REQUEST=5 present)--> RUNNING
  on transition:
    read REQUEST/SUBFUNCTION; STARTED_FLAG := 1; MON_CODE := +1 (ACK)
    assert SCIP (RFT); if RIE -> INT12
RUNNING:
  on CONTROL bit2/OPCOM with an outbound request in the postbox:
    read payload -> ethIICosmos_encode -> backend.SendPacket
  on backend.OnPacketReceived (worker thread -> queue -> drained in Tick):
    MAC-filter; ethIICosmos_decode; place in RX mailbox/postbox;
    assert SCIP superkick (RFT); if RIE -> INT12   (wakes ENNS0 LU-2240B input)
any state --(CONTROL PWCR=60B Halt+Reset)--> RESET
```

Reset unconditionally reposts PRKEY on the next INITIATE (fixes the real-firmware deadlock,
section 2.4). RIE/RFT semantics per section 2.2.

### 3.4 The one genuinely new piece: the PIOC bank window

The mailbox words must be readable/writable by the ND-100 at
`bank*0x10000 + (word + 2000B)`. Two options (DESIGN DECISION, flag for Ronny):

- **Option A (recommended): dedicated backing buffer + physical-memory intercept.** Add a
  hook in `ReadPhysicalMemory/WritePhysicalMemory` (cpu_mms.c) for the card's bank range
  that routes to the device's mailbox buffer. Keeps card memory separate from ND-100 RAM
  (matches real HW where the bank window is the CARD's DRAM, not main memory), and lets the
  HLE and ND-100 share exactly those words. Cost: a new intercept branch in the hot memory
  path (guard with a single range check so non-card accesses pay ~nothing).
- **Option B: co-opt real physical RAM at the bank.** Let the ND-100 read/write ordinary
  physical memory at bank 16; the HLE touches the same physical words via
  `Device_DMARead/Device_DMAWrite`. Zero memory-path change, but ASSUMPTION: nothing else in
  the SINTRAN image uses that bank, and STATUS must report the matching bank number. Simpler
  to prototype; verify the bank is truly free before trusting it.

VERIFIED nd100x facts that make either option viable: physical memory is a flat 24-bit word
array (`VolatileMemory.n_Array`); devices already reach it via `Device_DMARead/Write`. What
is NOT yet in nd100x: any device-owned memory-window abstraction. This is the main new
plumbing and is a design/impl task, not research.

### 3.5 What is DELETED versus the chip-level model

- **68000 core** - gone. No Musashi/instruction execution. The firmware's observable effects
  (PRKEY post, mailbox handshake, TX header build, RX MAC recheck, superkick) become direct
  C in the state machine.
- **MC68901 MFP** - gone. Its only ND-visible roles were the GPIP-I6 host->card doorbell
  (now = CONTROL bit 2 handled directly) and RTC timers (irrelevant to the ND-100 contract).
- **Am7990 LANCE + init block + RX/TX rings + descriptors + DMA + CSR emulation + byte-swap**
  - gone. Replaced by `backend.SendPacket` / `OnPacketReceived` plus a software MAC filter.
- **Protection table, MERRSTAT/EAREN, parity, EPROM, bus-error RAM-sizing** - gone
  (card-internal, never part of the ND-100 <-> card contract).

Kept / newly written: STATUS/CONTROL registers, RFT/RIE + INT12 + IDENT, the mailbox buffer
+ field map, the startup/request state machine, the backend seam, the COSMOS frame codec.

---

## 4. GAPS / UNKNOWNS (rigorously honest)

Ranked by how much each threatens a FAITHFUL HLE.

### Gap 1 (BLOCKER for a fully-serving node) - COSMOS/XMSG-over-Ethernet frame format
- **Status: NOT fully reverse-engineered.** What ENNS0 needs to progress past its LU-2240B
  input wait is a valid inbound COSMOS frame; the chip-level RE proved (`[V]`,
  `MAKE-RUNNABLE-TRIGGER-AND-FIX`, RX-inject test) that a wrong-EtherType frame is DISCARDED
  and does NOT superkick. The RetroCore model only gets past this because the REAL 68K XMSG
  firmware builds/parses these frames. The HLE has no firmware, so it must encode/decode the
  COSMOS frame itself.
- Known `[V]`: SRC = card MAC 08:00:26:64:00:00; DST low-2-bytes = LE sysid; frames are real
  802.3; the acceptance path is RCVCOMPLETE(0x5C42) -> XMRECEIVER(0xBED8) -> XMSG layer.
- Unknown: the exact EtherType / LLC / XMSG sub-header on the wire, and the mailbox/postbox
  byte layout the frame maps to.
- **How to close:** decode a real capture. Pcaps exist (`E:\Dev\Ronny\X25Emulator\pcap`,
  and the RetroCore pcap bridge emits `[ETH-TX]` frames). Cross-reference the XMSG wire
  protocol docs (`SINTRAN\XMSG\DOC\`, `xmsg-decode` skill) and the RetroCore
  `NDBusEthernetIIDecode.cs`. Deliverable: a byte-exact COSMOS-Ethernet frame spec.

### Gap 2 (BLOCKER for any bring-up) - the PIOC bank-window plumbing in nd100x
- **Status: not present in nd100x; ASSUMPTION-level design (section 3.4).** All existing
  nd100x devices are pure IOX register devices; none owns a memory-mapped bank window. The
  mailbox handshake cannot work until the ND-100 can read/write the mailbox words at the
  PIOC bank address and the HLE sees the same words.
- **How to close:** implement Option A (intercept) or Option B (co-opt RAM) and prove PISTA
  reads back the PRKEY the HLE wrote. Verify the exact `word + 2000B` base and STATUS bank
  number against `RP-P2-PIOC.NPL` and a live trace before trusting it.

### Gap 3 (partial) - exact per-cell mailbox/postbox layout + datafield pointer
- **Status: `[V]` for PRKEY/REQUEST/SUBFUNCTION/MON_*/STARTED_FLAG; `[I]` for the datafield
  pointer cell (word 1001B <-> MASTA) and the XMSG postbox RING base/layout that carries the
  actual TX/RX payload** (master ref 6c flags the ring DRAM base as HYPOTHESIS). The start
  handshake is closeable without the ring; the send/receive payload path needs it.
- **How to close:** decode PISTA's `MASTA+NPFUN/NPTIG` writes end-to-end and the firmware
  postbox producer (0xEACC) / make-runnable (0x259A/0xEAA6) against a live [MBX] trace;
  confirm the ring base.

### Gap 4 (minor) - exact identcode + STATUS bank constant to return
- **Status: `[I]`** device family 140360.. / IDENT 2240.. (ENNS0 LU 2240B). The precise
  identcode word and default bank number must be taken from the RetroCore constants / PROM
  byte, not guessed.
- **How to close:** read the value out of `NDBusEthernetII.cs` and the PROM identcode bytes;
  hard-code it as a named constant.

### Non-gaps (VERIFIED, will NOT block)
- The register bit meanings, SCIP/RFT/RIE/INT12/IDENT semantics (netlist section 4, verified
  on the RetroCore code side).
- The PRKEY value/instruction and the RESET->INITIATE->PRKEY->start sequence.
- That ENNS0 issues no IOX and the kernel PIOCM driver is the only IOX source.
- That the LANCE/68K/MFP are invisible to SINTRAN.

---

## 5. PHASED IMPLEMENTATION PLAN

Each phase is independently testable; the RetroCore chip-level model is the ORACLE (it boots
SINTRAN, ENNS0 starts, and it transmits real COSMOS frames today - master ref section 15,
`ethii-emulator-fixes.md`). Validate each nd100x phase by driving the SAME SINTRAN sequence
and comparing the ND-100-visible behavior.

**Phase 0 - Registers + IDENT skeleton.**
- `CreateEthernetIIDevice`, STATUS/CONTROL `Read`/`Write`, `Ident` returning the identcode,
  RFT/RIE bools, `Device_GenerateInterrupt(12)` when `RFT&&RIE`, IDENT clears RFT+RIE.
- Test: IOXT STATUS returns bank+RIE echo; a CONTROL write toggles RIE; a forced RFT with
  RIE set raises level 12 and IDENT clears it. Oracle: STATUS bit semantics vs RetroCore.

**Phase 1 - PIOC bank window (gap 2).**
- Implement Option A/B; expose the mailbox words at `bank*0x10000 + word + 2000B`.
- Test: ND-100 (or a unit test poking physical memory) reads back a value the HLE wrote to
  PRKEY's cell, and vice-versa.

**Phase 2 - PRKEY / ready handshake (state machine 2.4).**
- RESET on PWCR=60B; on PWCR=0 INITIATE, post datafield ptr + PRKEY + MON_CODE=3.
- Test: run SINTRAN `@RT ENNS0`; PISTA must see PRKEY within its 3-second window and issue
  the PWCR=11B start (no timeout). Oracle: RetroCore reaches PRKEY@0x0404 + MON_CODE=3 +
  STARTED_FLAG=1 during `@RT ENNS0`.

**Phase 3 - ENNS0 reaches server-start.**
- Handle the PWCR=11B start: read REQUEST=MPIOC(5)/SUBFUNCTION, set STARTED_FLAG, ACK
  MON_CODE, SCIP->INT12. Handle SUBFUNCTION=5 as a normal ACK (PARAM=0x1E).
- Test: ENNS0 completes its POSU reserve without the "No answer from interface" error.
  Oracle: RetroCore `@RT ENNS0` completes controller I/O with no error.

**Phase 4 - Frame TX to backend.**
- Wire `ethIIBackend` (tcp point-to-point first, `[u16 BE length][frame]`). On a host
  outbound request, encode via `ethIICosmos` and `backend.SendPacket`.
- Test: drive COSMOS to a `List-Routing-Info` and capture the emitted frame; compare
  byte-for-byte to a RetroCore `[ETH-TX]` capture (DST = LE sysid MAC, SRC = card MAC).
  This is where gap 1 first bites - the encode must match the oracle frame.

**Phase 5 - Frame RX + superkick delivery.**
- Backend worker -> queue -> drained in `Tick`; MAC-filter; decode; place in RX
  mailbox/postbox; assert SCIP superkick.
- Test: feed a captured/real COSMOS frame in; ENNS0's LU-2240B input read wakes (RTWT ->
  runnable) and it registers/serves. Oracle: RetroCore, when given a valid inbound frame,
  superkicks and ENNS0 proceeds; two nd100x nodes (or nd100x <-> RetroCore) over the TCP
  relay must see each other's `List-Routing-Info`.

**Phase 6 - MAC uniqueness + relay + interactive wiring.**
- Sysid-derived card MAC (low-2-bytes = LE sysid), the `RetroEtherRelay`-compatible fan-out
  hub, and a `net` device option in the interactive emulator (not just tests). Mirrors the
  RetroCore TCP bridge plan sections 3-6 so both emulators share one virtual segment.

Cross-emulator validation is the strongest oracle: because the TCP framing is identical to
the RetroCore bridge plan, an nd100x HLE node and a RetroCore chip-level node on the same
relay MUST interoperate - any divergence pins the bug to the HLE.

---

## 6. SUMMARY

- The 68K + MFP + LANCE ARE replaceable by HLE C code speaking only the mailbox/register
  protocol + raw frames. The ND-100 <-> card contract is VERIFIED and implementable in
  nd100x's existing `Device` model, reusing the `hdlc/modem.c` socket-worker pattern for the
  backend.
- Two things gate a FULLY-serving node: closing the COSMOS-Ethernet frame format (gap 1) and
  adding the PIOC bank-window plumbing nd100x lacks today (gap 2). Neither is research on the
  ND-100 <-> card protocol itself - that part is done.
- Build it in phases against the RetroCore chip-level model as a live oracle, ending in
  cross-emulator interop over the shared TCP relay.
