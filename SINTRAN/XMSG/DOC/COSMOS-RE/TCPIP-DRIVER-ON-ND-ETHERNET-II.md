# Writing a TCP/IP driver for SINTRAN on the ND Ethernet II controller

**Date**: 2026-07-27
**Status**: foundations PROVEN; two blockers remain, both named below.

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
4. **Submit frames** - the request shape is now known; see section 5. The remaining gap is the
   buffer pointer field, not the whole protocol.

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

### Transmit descriptor (node + 0x18)

| Offset | Size | Field |
|---|---|---|
| +0x06 | word | **header length** |
| +0x08 | word | **total length** - must be <= **0x5DC (1500)**, else **-22** |

### The mode word gates TRANSMIT too

```
tst.w (0x1888A)
  mode != 0  ->  header length must be >= 14      /* dst + src + type/length */
  mode == 0  ->  header length must be >= 12      /* dst + src only          */
  otherwise  ->  status -23
```

**This is significant**: 0x1888A is not only a receive filter, it changes what the firmware will
*accept for transmission*.

**INTERPRETATION - NOT PROVEN**: with the mode word clear the firmware appears not to require the
caller to supply the 2-byte type/length field (it may fill it itself); with the mode word set it
demands all 14 so it can validate the 802.3 length. **Confirm before relying on it** - this is
exactly the sort of plausible reading that has been wrong before in this project.

**PROVEN**: 1500 is used ONLY as an MTU bound here. It is never used to classify a received frame,
and 1536 / 0x0600 - the DIX-vs-802.3 discriminator - appears nowhere in this path.

### Preconditions checked before any of the above

| Condition | Status |
|---|---|
| `(0x188C6) == 0` | **-16** data path not up |
| `LNMAIOACTI (0x18866) == 0` and `(0x18880) != 0` | **-10** |
| `LNMAIOACTI == 0` and `(0x18880) == 0` | **-8** |
| otherwise | 0, proceed |

### What is still unknown

- Where the frame BYTES live - the descriptor carries lengths at +0x06/+0x08, but the buffer
  pointer field has not been identified
- Which subfunction index is transmit (the handler above is one entry of the 21)
- The RX completion path back to the host

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

**The most important open question, and currently UNKNOWN.**

The mode word at 0x1888A appears to be **global, not per-frame**. If it is, clearing it for TCP/IP
may break COSMOS, which presumably relies on the 802.3 length check. That would make the original
goal - TCP/IP *alongside* COSMOS on one card - impossible without either:

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
3. ~~Decode the DATASERVIC transmit request~~  DONE 2026-07-27 - only the buffer pointer field
                                               and the subfunction index remain
4. Runtime mode-word flip experiment        proves the DIX gate end-to-end - now much sharper,
                                            because the mode word demonstrably changes the
                                            ACCEPTED header length (14 vs 12), which is directly
                                            observable as status -23 on a wrong-sized header
5. Write the driver
```

Step 4 is now the cheapest decisive test: submit the same descriptor twice, once with the mode word
set and once clear, and watch the status code. No wire access needed.

---

## Related

- `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_HLE_SEAM_CONTRACT_2026-07-26.md`
- `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_DIX_TCPIP_PLAN_2026-07-27.md`
- `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\docs\PIOC-OS\` - the RTOS
