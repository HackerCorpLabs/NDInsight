# MAC address assignment on ND Ethernet II - single and multi-card

**Date**: 2026-07-29
**Scope**: how the 48-bit station address is formed, who writes it, and what happens when a machine
has more than one Ethernet controller.

Claims are marked **PROVEN** (vendor manual or measured on the running system), **EVIDENCED**
(strongly implied by code or documentation that has been read), or **UNKNOWN**.

---

## 1. The card has no address of its own

**PROVEN**: the ND Ethernet II controller has **no address PROM and no EPROM at all** - all firmware
is downloaded from the ND-100. The station address lives in card DRAM at **`LNMAPHYSIC` (0x1885E)**
and is written by the host with **command 0** (SET STATION ADDRESS): six bytes at `request+0x14` are
copied to 0x1885E, and a non-zero second parameter also calls `STARTMA`.

> **The MAC is whatever SINTRAN writes. Nothing on the card supplies or defaults it.**

`LNMAPHYSIC` is read by `RCVCOMPLETE` (0x5D78, the unicast receive match) and by `XMTRINGAPPEND`
(0x60EE, the source address stamped into outgoing frames), so a wrong or duplicated value breaks
receive filtering *and* mislabels transmitted frames.

**PROVEN** (measured 2026-07-29, `Nd100EthernetIIOracleDramDumpTests`): after a normal SINTRAN III L
boot with ENNS0 started, card 0 holds

```
0x1885E:  08 00 26 64 00 00
```

---

## 2. How the address is composed

**PROVEN** - `ND-60.197.01 EN Ethernet Basic Software Programmer Guide`, section 2.4 (Figure 5):

| Bytes | Contents |
|---|---|
| 0-2 | **08 00 26** - the Xerox/ND vendor prefix (OUI) |
| 3-4 | **ND system number**, stored in REVERSED byte order (characters 4-5 of the address string) |
| 5 | **physical user code** (character 6) |

`ENMFnumberToAddress` / `ENMFaddressToNumber` convert between an ND system number and this address
form: *"ND system numbers, in the range 0 - 177777B, are contained (in reversed order) in characters
4-5 of the address string. Physical user numbers are contained in character 6."*

Decoding the measured value confirms it exactly:

```
08 00 26   ND OUI
64 00      -> 0x0064 = 100 = this machine's ND CPU number ("CPU NUMBER: 100" at boot)
00         physical user 0
```

### The physical user byte

**PROVEN** - same section: the hardware **does not check the two most significant bits (7-6) of the
destination address**, so *"each Ethernet Interface has in fact four physical addresses"*:

| bits 7-6 | meaning |
|---|---|
| 00 | IEEE |
| 01 | **DIX** |
| 10 | user-written protocol |
| 11 | **ND (COSMOS)** |

The API exposes this directly: `attach.ENUMDEaddress` is documented as *"Physical user address (0-3)
in a specified Ethernet interface."*

> This is worth dwelling on: **the address scheme distinguishes PROTOCOL FAMILY, not card.** DIX and
> COSMOS traffic were designed to arrive at the same interface under different physical addresses.

---

## 3. The interface number is NOT part of the address

**PROVEN by absence and by construction.** The address is built from the ND *system* number and the
physical user code only. The interface (thumbwheel) number appears nowhere in it.

Where the interface number *does* appear is as a separate parameter alongside the address:

```
TYPE ENUMDE = RECORD
     ENUMRGaddress : ENUMDEaddress     % physical user address (0-3) in a specified interface
     INTEGER2      : ENUMDEpioc        % "the PIOC number (0-3)" - the thumbwheel setting
     ...
```

*"attach.ENUMDEpioc - Ethernet interface number to use. Given by thumbwheel setting on the Ethernet
interface."* The example program in the manual prompts the operator for `Local Ethernet Interface:`
and assigns it to `ENUMDEpioc`.

So the software addresses a card by **(interface number, physical user address)** - two separate
fields - and only the second of them reaches the wire.

---

## 4. Consequence for a multi-card machine

**Two controllers in one ND-100 share the machine's ND system number.** Since the system number and
the physical user code are the only variable parts of the address, two cards in the same machine
**would receive the same MAC** unless something assigns them different physical user codes.

**UNKNOWN - this has not been observed, and must not be assumed either way:**

- whether SINTRAN assigns a distinct physical user code per interface automatically,
- whether the operator is expected to configure it per interface at generation time,
- or whether both cards genuinely carry the same address and are distinguished only by which
  interface the software chose to talk to (which is possible, since the host picks the card by
  `ENUMDEpioc` rather than by address).

The last option is not absurd. The card is selected by interface number on transmit, and on receive
each card filters against its own `LNMAPHYSIC`; two cards with the same address on the same LAN
would both accept the same unicast frames, which is only a problem if both are attached to the same
segment. A two-card machine bridging **two different segments** would work with identical addresses.

There is one documented error that implies the system does track this:

> *"Illegal Physical Address. Either the system is not generated for this address, or the physical
> address is already in use."* (ND-60.197.01 status codes)

**EVIDENCED**: "already in use" implies a registry of physical addresses, and "the system is not
generated for this address" implies physical addresses are part of SINTRAN generation - i.e. the
operator declares which physical user addresses exist. That is consistent with per-interface
assignment being a **generation-time configuration choice**, not something derived automatically.
It is not proof.

---

## 5. What is NOT in the documentation

Stated plainly so nobody re-searches for it:

- **No `SET-STATION-ADDRESS` operator command exists.** The MAC is set by host command 0 from the
  driver, not by an operator command. `LNMAPHYSIC` appears nowhere in the vendor manuals - it is a
  firmware symbol recovered by reverse engineering.
- **No table anywhere assigns MACs to interfaces 1-3.**
- **No worked example of a two-card machine exists** in the repository - not in the manuals, not in
  the installation descriptions, not in the COSMOS operator guides.

---

## 6. How to settle it

The experiment is small and now unblocked. Until 2026-07-29 the emulator could not host two cards at
all: `ND100Memory` tracked a single Ethernet card in one field (last registration won) and
`NDBusEthernetII` hardcoded one 512 KB window for every instance, so a second card was unreachable
and neither card came up. Both are fixed - each card is now registered separately and derives its
own bank from the thumbwheel (bank 16 + 4 per card; thumbwheel 0 unchanged at 0x00200000). Regression
tests: `Nd100MultiEthernetConfigTests`.

To answer section 4:

1. Configure a machine with `ETH 0` and `ETH 1` (`RETROCORE_ETH_CARDS=2`).
2. Install/generate a second network server **ENNS1** - the installer produces one per card by
   renaming the BRF symbol `ENNS0` to `ENNS<channel>`, where the channel is the thumbwheel. Starting
   only ENNS0 will not bring up the second card.
3. Boot, start both servers, and read `0x1885E` from each card.
4. Compare. Identical values mean no per-card address; differing values mean SINTRAN assigns them,
   and the differing byte will show whether it is the physical user code (byte 5) as section 2
   predicts.

Test `TwoControllers_StationMacAssignment` performs steps 1, 3 and 4 and reports the outcome without
asserting a preferred answer. **Step 2 is not yet done** - no ENNS1 exists on the current image, so
the experiment is not yet conclusive.

---

## Related

- `TCPIP-DRIVER-ON-ND-ETHERNET-II.md` - the transmit path, the 0x1888A mode word, and the proven
  DIX/802.3 A/B pair
- `Reference-Manuals\ND-60.197.01 EN Ethernet Basic Software Programmer Guide.md` - section 2.4 is
  the authoritative address format
- `Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md` - four controllers maximum;
  thumbwheel 12J and the 7J/9J bank straps
- `Installation\Communication\Ethernet\ND-210580-02-EN.md` - the ENNS<thumbwheel> naming convention
