# Writing a TCP/IP driver for SINTRAN on the ND Ethernet II controller

**Date**: 2026-07-28
**Status**: the transmit path is **fully decoded** and the firmware **can** send Ethernet II frames
with the mode word at 0x1888A clear - proven from the header-build code, not inferred. What remains
is the receive completion path, and the coexistence problem in section 7.

Every claim here is marked **PROVEN** (read from the firmware image or demonstrated by a passing
test), **EVIDENCED** (strongly implied by code that has been read), or **UNKNOWN**. Nothing is
presented as fact because it sounds right.

---

## 1. The core problem

TCP/IP travels in **DIX / Ethernet II** frames: bytes 12-13 are an **EtherType** (0x0800 for IPv4),
which is >= 0x0600 by definition. An 802.3 frame uses those same bytes as a **length**.

**PROVEN**: the firmware has a mode word at **0x1888A**. While it is non-zero, the firmware treats
bytes 12-13 as an 802.3 length and **drops any frame whose value does not equal the payload
length** - which is every DIX frame. Each drop increments the counter at 0x188B0, whose ND label is
*"bad MA length field"*. Both directions are affected.

So the wire is fine, the LANCE is fine, and the firmware is the gate.

---

## 2. What is already proven at the hardware layer

Five unit tests, `Emulated.Tests.Chips\Am7990LanceTests.cs`, category `DIX`, **all passing**
(2026-07-27):

| Test | Establishes |
|---|---|
| `Test_TX_DixIpv4Frame_IsTransmittedByteExact` | the LANCE transmits a DIX frame byte-exact |
| `Test_TX_DixFrame_EtherTypeSurvivesAsTypeNotLength` | 0x0800 survives as a TYPE (>= 0x0600) |
| `Test_TX_DixFrame_ClearsOwnBit` | descriptor handling is identical for DIX |
| **`Test_TX_RealTcpFrame_ChecksumsVerifyOnTransmittedBytes`** | a complete IPv4+TCP packet goes **out** with **both checksums verifying on the emitted bytes** |
| **`Test_RX_RealTcpFrame_ChecksumsVerifyAfterDma`** | the same packet comes **in** and both checksums still verify after DMA |

The frame used is a real telnet-bound segment: ports 12345 -> 23, PSH+ACK, payload `GET /\n`,
60 bytes total. Checksums are recomputed over the transmitted/received buffer, so a byte-order or
DMA fault fails the test rather than passing silently.

**Conclusion: the hardware layer is not the obstacle in either direction.**

**Scope limit**: these drive the LANCE directly. They say nothing about the firmware's own TX/RX
paths. A pass here is NOT "the firmware can send DIX frames".

---

## 3. What the host must do to bring the card up

**PROVEN** unless marked. Command dispatch: `CMDSERVICE` @0x659C,
`code = request[0x0A] >> 2`, bounds-checked; out of range writes **-14** to `request[0x0C]`.
**Odd codes all reject** - only even codes are real commands.

| Code | Meaning |
|---|---|
| **0** | **SET STATION (MAC) ADDRESS** - 6 bytes at `request+0x14` copied to `LNMAPHYSIC` (0x1885E). A second parameter, if non-zero, also calls `STARTMA` |
| 2, 4, 6, 8, 10, 22 | real commands, **UNKNOWN** individually |
| 12 | ADD-GROUP-ADDRESS |
| 14 | DELETE-GROUP-ADDRESS |
| 16-21 | reject |

Boot handshake, in this order (**PROVEN** from `reset_entry`):

1. datafield pointer table published at 0x04CA
2. **PRKEY 0x5473 -> 0x0404** (ND-100 word 1002B); SINTRAN's `PISTA` busy-polls this
3. STARTED 1 -> 0x04C0
4. reply 3 -> 0x040C
5. SCIP doorbell (0x00EF0080) raised

---

## 4. What has to change to allow TCP/IP frames

1. **Clear the mode word at 0x1888A.** **UNKNOWN**: which host command does this, or whether it is
   only settable at init. It is a firmware DRAM location, so a host that can write card DRAM can
   set it directly - but the supported route has not been identified.
2. **Set a MAC** via command 0 (see section 6).
3. **Register any multicast/broadcast addresses** via command 12. **PROVEN**: there is **no
   hardcoded broadcast** - `FF:FF:FF:FF:FF:FF` is accepted only if registered, or if filtering is
   off (`0x18888 == 0`, and note nonzero = filtering ENABLED, the opposite of older repo notes).
4. **Submit frames** - the request and descriptor shapes are **fully decoded**; see section 5.
   Remember to leave >= 12 bytes of headroom in the buffer and to put the EtherType in the two
   bytes immediately before your IP header.

---

## 5. The transmit request - DECODED 2026-07-27

**PROVEN**: `XMTRINGAPPEND` (0x6054) has exactly one caller, so transmit is **not** reached from
`CMDSERVICE`. It comes through **`DATASERVIC` (0x6ACA)**, which drains the data sub-process queue
(`posi_getall` 0x514A) and walks the returned node list.

### Dispatch

Subfunction = `node[0x0A] >> 2 & 0x3F` - the same shifted-byte encoding `CMDSERVICE` uses.
Dispatched through the **DATA table at 0x189E0** (21 entries, 0..0x14), bounds-checked against the
**byte at 0x189DE**.

### Request node

| Offset | Size | Field |
|---|---|---|
| +0x00 | long | next link (cleared as the list is walked) |
| +0x04 | word | id - must equal `(0x188C8)`, else **-17** |
| +0x0A | byte | subfunction << 2 |
| +0x14 | long | **version - must be 1**, else **-21** |
| +0x18 | — | the transmit descriptor starts here |

**PROVEN**: the transmit subfunction index is **16 (0x10)**, i.e. `node[0x0A] = 0x40`. The table at
0x189E0 sends index 16 to the validator at 0x6B9E; indices 18 and 20 go to 0x6C6A
(POST-RX-BUFFER); every other index goes to the reject stub at 0x6D56.

### Transmit descriptor (node + 0x18) - COMPLETE

| Offset | Size | Field |
|---|---|---|
| +0x00 | long | **BUFFER BASE ADDRESS** - the frame buffer pointer |
| +0x06 | word | **header length** - offset inside the buffer where the HOST's data begins |
| +0x08 | word | **total length** - byte count from `base + hdrlen` onward, <= **0x5DC (1500)**, else **-22** |

Also in the node, not the descriptor: **+0x22, six bytes, the DESTINATION MAC** (copied into the
frame at 0x60E0).

### How the header is built - and why this settles the DIX question

**PROVEN** from `XMTRINGAPPEND` (0x6054), which reads the descriptor at 0x609C / 0x60CA.

The firmware does **not** take a ready-made header from the host. It **backs up** from the host's
data and writes the MAC header in place, so the host must leave `hdrlen` bytes of headroom in front
of its data:

| | mode (0x1888A) != 0 - 802.3 | mode (0x1888A) == 0 - DIX |
|---|---|---|
| header start | `base + hdrlen - 14` | `base + hdrlen - 12` |
| required hdrlen | >= 14 | >= 12 |
| bytes 12-13 | firmware **writes totallen** as an 802.3 LENGTH | firmware **writes nothing** |
| on-wire length | `14 + totallen` | `12 + totallen` |
| what totallen counts | payload only | **the 2 type bytes + payload** |

> **The earlier 14-vs-12 reading is now PROVEN, not interpretation.** With the mode word clear the
> firmware builds only dst+src and leaves bytes 12-13 alone - they come straight out of the host
> buffer. **The host places the EtherType there itself.** The firmware never invents, fills in, or
> validates a type field in this mode.

Consequences that matter for a driver:

- **Max IP payload in DIX mode is 1498, not 1500**, because the 1500 cap is applied to `totallen`
  and `totallen` includes the two type bytes.
- The source MAC is always copied verbatim from `LNMAPHYSIC` (0x1885E); **no bits are forced or
  masked** in any byte, so no protocol-family encoding is stamped into the address.
- If `g_maOperatingMode` (0x18886) == 4 (NORMAL), the on-wire length is padded up to **60** bytes.
  In loopback modes (1 and 3) there is no padding.

### LANCE handoff (end of XMTRINGAPPEND)

TX ring base **0x18410**, 8-byte entries, ring index at `(0x18408)+2` wrapping **mod 0x80** (128
descriptors). Per entry: address low word at +0x00, address bits 16-23 at +0x03, **negated** length
at +0x04, then `STP|ENP` (0x0300), then `OWN` (0x8000), then a poke of **0x48 to 0xEF00A0**.

**PROVEN**: 1500 is used ONLY as an MTU bound here. It is never used to classify a received frame,
and 1536 / 0x0600 - the DIX-vs-802.3 discriminator - appears nowhere in this path.

### Preconditions checked before any of the above

| Condition | Status |
|---|---|
| `(0x188C6) == 0` | **-16** data path not up |
| `LNMAIOACTI (0x18866) == 0` and `(0x18880) != 0` | **-10** |
| `LNMAIOACTI == 0` and `(0x18880) == 0` | **-8** |
| otherwise | 0, proceed |

### Posting receive buffers

**PROVEN**: subfunction **18 (0x12)** is POST-RX-BUFFER, handled at 0x6C6A. A version-1 buffer node
must declare `descriptor+0x04 == 0x5F0` (**1520 bytes**) or it is rejected with **-22**. 1520 is
comfortably more than a full 1514-byte Ethernet II frame, so **receive buffer size is not a
constraint on TCP/IP**.

### How a node reaches DATASERVIC - DECODED 2026-07-28

**PROVEN**: the enqueue primitive is at **0x8AC8** (`POSI_SEND`). Arguments are the queue object
pointer in the callee frame at +0x14 and the node (or chain head) in **A0**.

| Queue object | Drained by |
|---|---|
| 0x18834 | `CMDSERVICE` - the command sub-process |
| **0x18848** | **`DATASERVIC` - the data sub-process** (via `posi_getall` 0x514A) |

It has two paths. If `(0x1A2D0)` is non-zero it **defers**, appending the node to a local pending
list (0x1A2C8 for commands, 0x1A2CC for data). Otherwise it builds a message
`{+0x14 queue object, +0x18 word from 0x1A290, +0x1A node}` and posts it through the message
primitive at 0x11DC4, falling back to the deferred list if that returns -2 (no buffer).

The firmware's own transmit producer does exactly this at 0xB444-0xB456: load the accumulated node
chain from **0x1A2BC**, set the queue object to 0x18848, call `POSI_SEND`, clear 0x1A2BC. The batch
flusher at 0xB380 walks that chain and recognises transmit by **subfunction 0x10 (16)** - the same
index the table at 0x189E0 routes to 0x6B9E.

**This is what a test should call.** Build the node and descriptor, then call `POSI_SEND` with the
queue object 0x18848. That is the firmware's own enqueue, not a synthetic entry point, so
`DATASERVIC` then runs the whole validated path down into `XMTRINGAPPEND` exactly as in production.

### MEASURED on the running card, 2026-07-28

Boot harness `Nd100EthernetIIOracleDramDumpTests`, SINTRAN III L, ENNS0 reported started:

| Cell | Value | Meaning |
|---|---|---|
| `LNMAPHYSIC` 0x1885E | **08:00:26:64:00:00** | the station MAC SINTRAN writes (ND OUI 08:00:26) |
| 0x18886 | 0x0004 | `g_maOperatingMode` = NORMAL, so 60-byte padding is active |
| 0x18888 | 0x0001 | filtering ENABLED (nonzero polarity confirmed) |
| **0x1888A** | **0x0001** | **802.3 mode at rest - DIX is gated off** |
| 0x188C6 | 0x0001 | data path up (zero would give -16) |
| 0x188C8 | 0x0005 | expected id - a node must carry 5 at +0x04 |
| 0x1A290 | 0x0005 | same value from the message layer - cross-confirms the id |
| 0x1A2B0/B4/B8/**BC** | all 0 | deferred-send pending heads rest at zero |
| 0x1A2D0 | 0x0000 | selector zero, so `POSI_SEND` takes the direct-send path |

Three predictions from the static RE (filtering polarity, operating mode driving the pad, the mode
word starting non-zero) were confirmed against the live firmware.

**Two negative results, both measured, both important:**

1. **The card transmits NOTHING on its own once ENNS0 is started** - zero frames in a 20-second
   capture off the LANCE transmit hook. So there is no live traffic to observe, and any transmit
   test must inject a frame.
2. **Writing a node address into 0x1A2BC does not send it.** After 20 seconds the head still held
   the node, and it still did after three GPIP I6 doorbell strobes. Nothing on an idle card polls
   that cell - the producer writes it and drains it within the same call path, so it is **not** an
   injection point. Driving a transmit from outside therefore requires calling `POSI_SEND` (0x8AC8)
   directly.

### PROVEN ON THE RUNNING CARD - the A/B pair, 2026-07-29

Both halves of the mode-word model were driven through the real firmware and the emitted bytes
captured off the LANCE. Tests `Transmit_DixFrame_IsEmittedByTheLance` and
`Transmit_8023Frame_FirmwareWritesTheLengthField` in `Nd100EthernetIIOracleDramDumpTests`.

| | mode word | hdrlen | totallen | on-wire | bytes 12-13 |
|---|---|---|---|---|---|
| DIX | **0x0000** | 12 | 42 | **60** (12+42=54, padded) | **0x0800** - our EtherType survived |
| 802.3 | **0x0001** | 14 | 64 | **78** (=14+64, no pad needed) | **0x0040** = 64 - firmware wrote the length |

Every prediction held simultaneously: the 12-vs-14 backwards header build, the 60-byte minimum pad
under `g_maOperatingMode == 4`, `on-wire = hdrlen + totallen`, and the mode word deciding whether
the firmware writes bytes 12-13 at all. Destination MAC, source MAC (from `LNMAPHYSIC`) and the
payload were byte-exact after DMA in both cases.

> **The ND Ethernet II card transmits genuine Ethernet II / DIX frames when 0x1888A is cleared.**
> This is measured, not inferred. The central question behind the TCP/IP goal is answered: yes.

**How the frame was submitted**: the pending head 0x1A2BC is NOT an injection point (nothing polls
it). The test calls `POSI_SEND` (0x8AC8) directly on the 68K - stage a PLANC frame, node in A0,
queue object 0x18848 at frame+0x14, and a fake return link pointing at a parked `bra.s *` so the
CPU spins harmlessly instead of running loose. All registers are saved and restored, and the test
asserts the card is still running afterwards so a wrecked card cannot masquerade as a result.

**Harness constraint**: only ONE machine boot per `dotnet test` invocation. A second boot in the
same process reaches "SINTRAN III RUNNING" but login never answers, and the test host then crashes.
Run these tests one per invocation.

### The RECEIVE delivery node - DECODED 2026-07-29

`RCVCOMPLETE` (0x5C42) builds a delivery node whose descriptor mirrors the transmit one.

**PROVEN - the frame is NOT copied and the MAC header is NOT stripped.** `descriptor.base`
(node+0x18) is set to the LANCE buffer address, which points at byte 0 of the frame, i.e. at the
destination MAC. `descriptor.len` (node+0x1C) is the BUFFER size (from -BCNT), not the frame length.
The destination MAC is additionally duplicated to node+0x22 and the source MAC to node+0x28.

**The mode word 0x1888A is read here too - a THIRD consumer** (after the transmit header build and
the receive filter):

```
mode != 0   hdrlen = 14, totallen = frame bytes 12-13
            -> the 802.3 length field is TRUSTED VERBATIM as the payload length
mode == 0   hdrlen = 12, totallen = framelen - 12
            -> bytes 12-13 are handed to the host as ordinary payload
```

> So with the mode word clear the host **receives the EtherType as part of its data** and parses it
> itself - which is exactly what a DIX/TCP-IP stack needs. The firmware never validates bytes 12-13
> as a length in the delivery path.

Independent of the mode word there is a **size gate** before the classifier: frames shorter than 60
or longer than 1514 (0x5EA = 1500 + 14) are rejected, both bumping the saturating counter at
`g_nmaStatsBlock+0x2A`.

LANCE hardware errors (RMD1 ERR, bit 14) bump one counter each: CRC +0x1C, FRAM +0x1E, BUFF +0x22
(also sets bit 2 of the event word 0x1894C), OFLO +0x20 (bit 1 of 0x1894C).

**Every rejection is a SILENT drop**: the discard path clears the delivery-node pointer, still
clears the descriptor and advances the ring, and never reaches the host-notify step - so no SCIP and
no level-12 interrupt. By design.

### What is still unknown

- The RX completion path back to the host (`RCVCOMPLETE` 0x5C42 is the LANCE-side entry)
- What the ND-100 does to make the producer build nodes onto 0x1A2BC. That is the **host driver's**
  side, and it is **not needed to exercise the card**

---

## 6. MAC addresses, and running two controllers

### The card has no address PROM

**PROVEN** (from the controller documentation and command 0): the board has **no EPROM at all** -
all firmware is downloaded from the ND-100 - and the station MAC at `LNMAPHYSIC` (0x1885E) is
written by **host command 0**. So:

> **The MAC address is whatever SINTRAN writes. Nothing on the card supplies or defaults it.**

`LNMAPHYSIC` is also read by `RCVCOMPLETE` (0x5D78, the unicast receive match) and `XMTRINGAPPEND`
(0x60F0, the source MAC stamped into outgoing frames), so a wrong or duplicated value breaks
receive filtering *and* mislabels transmitted frames.

### Two (or four) controllers are supported by the bus

**PROVEN** from `NDBusEthernetII`'s thumbwheel decode - four distinct card positions:

| Thumbwheel | IOX range | Ident code | Level |
|---|---|---|---|
| 0 | 140360-140363 | 140034 | 12 |
| 1 | 140364-140367 | 140035 | 12 |
| 2 | 140370-140373 | 140036 | 12 |
| 3 | 140374-140377 | 140037 | 12 |

Distinct ident codes mean SINTRAN *can* tell them apart on a shared level-12 interrupt.

**EVIDENCED**: the board configuration record at 0x04CA carries a **PIOC number** (+0x32) and an
**ND-100 CPU number** (+0x34), both supplied by the host - the design clearly anticipates more than
one PIOC in a machine.

### What is NOT known

**UNKNOWN - and this needs an experiment, not an opinion:**

- Does SINTRAN detect and initialise **both** cards, or only the first?
- Does it hold a per-card MAC, and where does it get the values from?
- Does it assign distinct MACs automatically, or would both cards end up with the same address
  (which would break receive filtering on both)?

**The emulated machine has never been configured with two cards** - every instantiation in the
codebase is thumbwheel 0. So this cannot be answered by reading; it must be run.

### RETRACTED: the two-card failure below was an EMULATOR BUG, not ND behaviour

**Corrected 2026-07-29 after a documentation and code review.** The measurement below is accurate,
but the conclusion drawn from it was wrong. Do not cite it as evidence about SINTRAN.

**Multiple controllers are documented and supported.** `Reference-Manuals\Devices\ND-12.055.1 EN
Ethernet II Controller.md`: *"ND-100 based systems can drive a maximum of four Ethernet II
controllers."* `Installation\Communication\Ethernet\ND-210580-02-EN.md`: *"ENNS1 ==> EtherNet
Network Server 1. Where 1 is the same number as the thumbwheel setting of the Ethernet Interface."*
SINTRAN is generated for up to 3 Ethernet interfaces, and COSMOS reserves virtual sysids 9800-9803
for four concurrent network servers.

**Two defects in the experiment, one of them fatal:**

1. **`ND100Memory` only tracks ONE Ethernet card.** `Emulated.Machines\ND\ND100\ND100Memory.cs`
   lines 88-89 and 150-157 hold a single `NDBusEthernetII _ethernet` field, overwritten on each
   registration, and `FindMemoryBank` consults only that one. With two cards registered, card 0's
   DRAM is unreachable from the ND-100 - the driver writes firmware, comm block and MAC into card 1
   while card 0 is released from reset with empty DRAM. That is precisely the observed symptom.
2. **Both cards claim the same ND-100 memory window.** `NDBusEthernetII.cs:534` hardcodes
   `physicalPageStart = 0x400 * 2048` and `memoryBank = 16` for every instance. On real hardware
   each card sits in its own 512 KB bank, selected by the **7J/9J thumbwheels** (ND-12.055.1,
   Table 2) - a strap the emulator never reads.
3. Additionally, no `ENNS1` RT program was installed. The installer produces one per card by
   renaming the BRF symbol `ENNS0` -> `ENNS<channel>`; the harness only ever started `ENNS0`.

**What the emulator needs before this experiment can be repeated**: make `ND100Memory` hold a list
of Ethernet cards and iterate in `FindMemoryBank` (and the DMA-log gates at lines 400 and 517), and
derive `memoryBank` / `physicalPageStart` from the thumbwheel instead of hardcoding them. The
ident/IOX decode is already correct and per-instance, so nothing else needs to change.

**Separately - ND shipped TCP/IP exactly this way.** ND's own product sheets
(`HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md`) state *"Possibility of running TCP/IP in
several Ethernet III controllers simultaneously"*, *"you will have to assign an Internet address for
each controller"*, and conclude **one protocol per controller - dual stack needs two cards**. That
independently corroborates the global-mode-word finding: COSMOS and TCP/IP on one card is not how
the product worked, and two cards is the intended answer rather than a workaround.

---

### SUPERSEDED measurement, 2026-07-29 - kept for the record

Test `TwoControllers_StationMacAssignment`, run with `RETROCORE_ETH_CARDS=2` (a second
`NDBusEthernetII` at thumbwheel 1). Same boot and ENNS0 ladder that succeeds every time with one
card:

```
ENNS0 started = False
card 0 (thumbwheel 0) LNMAPHYSIC = 00:00:00:00:00:00   68KRunning=False
card 1 (thumbwheel 1) LNMAPHYSIC = 00:00:00:00:00:00   68KRunning=False
XMSG Routing/Naming error: Unknown name (of server or system)
```

**Adding a second controller does not merely duplicate the MAC - it prevents bring-up completely.**
Neither card is given a station address and neither 68K is left running. So the MAC question is
still unanswered: there was never a second initialised card to compare.

**UNKNOWN - do not assume either way**: whether this is SINTRAN's genuine behaviour (for example
both cards colliding on the `*XM-ENNS0` registration that the card performs over an MBOXH XMSG
conversation - the known source of "Unknown name") or an emulation limitation (both controllers
answering the same ident, or the machine configuration not really supporting a second ETH). One
card working and two failing is consistent with both explanations.

**Consequence for the coexistence strategy**: "run COSMOS on one card and TCP/IP on the other"
cannot be assumed to work. It is not currently demonstrable, and until the failure above is
understood the only demonstrated path to Ethernet II on this hardware is a single card with the
global mode word cleared - which excludes COSMOS at the same time.

### The experiment

1. Fix the 68K reset bug first (`cpu.SetHalt(halt || reset)` - the harness is otherwise 3-in-4
   flaky and any result is untrustworthy).
2. Add a second `NDBusEthernetII(1)` to the ND-100 machine configuration.
3. Boot SINTRAN and watch for: two ident responses on level 12, two firmware downloads, two PRKEY
   publications (0x0404 in each card's DRAM), and two command-0 calls.
4. Record the MAC written to each card's `LNMAPHYSIC`. **If they are identical, that is the
   answer** - SINTRAN has no per-card address and dual-card operation needs host-side work.

---

## 7. Will it still work with COSMOS?

**Now largely answered, and the answer is bad for single-card coexistence.**

**PROVEN**: 0x1888A is a single global word with **no per-request override**. The transmit path
reads it directly (`tst.w (0x1888a).l` at 0x6086) rather than taking a flag from the request node,
and it decides both the header size the firmware builds and whether a length field is written at
all. There is nowhere for a caller to say "this frame is DIX and that one is 802.3".

So clearing it for TCP/IP changes the format of **every** frame the card sends, including COSMOS's.
That makes the original goal - TCP/IP *alongside* COSMOS on one card - impossible without either:

- a firmware patch making the check per-frame (accept when bytes 12-13 >= 0x0600, validate
  otherwise - which is exactly what real dual-stack hardware does), or
- **the HLE**, where the logic is ours to define, or
- **two controllers** - one per protocol, which is precisely why section 6 matters.

That last option makes the two-card question strategic rather than academic: if SINTRAN drives two
cards with distinct MACs, running COSMOS on one and TCP/IP on the other sidesteps the coexistence
problem entirely.

---

## 8. Recommended order

```
1. Fix the 68K reset bug                    small, unblocks everything
2. Two-controller experiment (section 6)    answers both the MAC and the coexistence strategy
3. ~~Decode the DATASERVIC transmit request~~  DONE 2026-07-28 - COMPLETE, including the buffer
                                               pointer and the subfunction index (16)
4. Runtime mode-word flip experiment        confirms the decode on the running firmware
5. Write the driver                         nothing in the transmit path blocks this any more
```

Step 4 is now a confirmation rather than a discovery. Build one buffer, submit it twice - once with
0x1888A set and once clear - and check the emitted bytes: with the mode word set, bytes 12-13 of
the frame must be the payload length written by the firmware; with it clear, they must be whatever
the host put there. That distinguishes the two paths directly, without needing a wire.

---

## Related

- `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_HLE_SEAM_CONTRACT_2026-07-26.md`
- `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_DIX_TCPIP_PLAN_2026-07-27.md`
- `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\docs\PIOC-OS\` - the RTOS
