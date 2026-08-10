# Ethernet II feasibility on the ND Ethernet II card - the 0x1888A mode word decoded (2026-07-25)

Date: 2026-07-25
Subsystem: ND Ethernet II (PCB 3094 / ND-110063), ENCOS MC68000 firmware
Binary: `..\..\..\..\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`
(68000 big-endian, base 0x0; the active Ghidra program)

Question this answers: **can this card carry ordinary TCP/IP (Ethernet II / DIX) traffic, and can it
do so while COSMOS keeps running?**

Convention (same as [ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md](ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md)):
**[V]** = verified from the firmware disassembly, Ghidra address cited. **[U]** = unverified /
inferred, flagged for a live trace before relying on it.

---

> ## CORRECTION (2026-07-26) - THE TL;DR BELOW IS SUPERSEDED. READ THIS FIRST.
>
> The firmware-level analysis in this document is correct and stands. The **system-level conclusion
> was wrong**, because this document analysed only the LNMA firmware layer (0x5000-0x7000) and missed
> the **ENCOS server** that runs above it on the same 68000.
>
> Newly verified [V]:
> - The COSMOS frame is built **on the card**, by the ENCOS server at `0x83E6`-`0x849C` - NOT by the
>   ND-100. It writes `hdrLen = 14`, `totalLen`, and starts its LLC payload (`a8 a8 03 0b 02`) at
>   `base+14` (stores at 0x845E / 0x8464 / 0x847E / 0x8482 / 0x8488). Wire bytes 12-13 are written by
>   nobody but the firmware at 0x60AA.
> - The on-card RX classifier at `0xA18E` computes payload as `base + hdrLen` and then tests
>   `(A3)==0xA8`, `(1,A3)==0xA8`, `(2,A3)==0x03`. With `g_mode8023LengthField = 0` the firmware
>   reports `hdrLen = 12`, so the server reads the LENGTH FIELD as DSAP/SSAP and rejects every
>   COSMOS frame.
> - The server hard-codes +-14 header arithmetic elsewhere too (`0x8E50`, `0xADD4`).
>
> **Therefore setting `g_mode8023LengthField = 0` breaks COSMOS in BOTH directions, not just
> transmit** (section 8 below says receive survives - that is WRONG, it only survives the firmware
> gate, then dies at the server's LLC check).
>
> **And it cannot be fixed from the ND-100.** The ND-100 never sees a MAC frame: the card is an XMSG
> *client* of the ND-100 kernel over the PIOC XMSG box + SUPERKICK ring (PIOC word `1012B`, pattern
> `52525B/125252B`), consumed by `PDRIV` in
> `..\..\..\NPL-SOURCE\NPL\MP-P2-PIOC-DRIV.NPL`. No ND-100 image contains the mode-word address
> `142105B` or the ready-list address `142153B`. The list at 0x188D6 is drained by FIRMWARE code
> (0x6E7C / 0x6E96), so "the host receives the complete frame" in section 4 means the on-card server,
> not the ND-100.
>
> Revised verdict: **Ethernet II is supported by the low-level firmware but blocked by the on-card
> ENCOS server. A dual-stack machine needs either a patched ENCOS server or a card dedicated to one
> mode.** The one ND-100-side route that bypasses the server entirely is MON 255 EXEL `LDATX`/`STATX`
> against card DRAM - viable for control, but it moves one word per monitor call and so cannot carry
> 1514-byte frames at any useful rate.
>
> ## CONFIRMED BY ND'S OWN DOCUMENTATION (2026-07-26)
>
> Norsk Data shipped exactly this: **COSMOS TCP/IP Gateway for Ethernet, product 211185**, which runs
> on **the same ND 110063 Ethernet II card** and whose documented Data Link standard is
> **"Baseband Ethernet: DIX 2.0"** with ARP (RFC 826) - impossible under the ENCOS LLC path. It works
> by loading a **different 68000 image** into the card at cold start.
>
> ND's own hardware manual for this card
> (`..\..\..\..\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md`, Appendix C)
> tabulates the COSMOS stack as `LLC1 / MAC` at layer 2 and the ARPA stack as `LLC1 / DIX`, with the
> footnote "to be implemented late 1987 by Ethernet II". So the framing split this document derived
> from the firmware is documented by ND.
>
> Full documentary evidence, product family, layer split (TCP on ND-100, IP on the controller), the
> per-protocol image mechanism, and the coexistence question:
> **[HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md](HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md)**

> ## SECOND RE PASS (2026-07-26) - THE FOUR-ADDRESS SCHEME, THE LANCE, AND BROADCAST
>
> Two independent Ghidra passes (software-side and hardware-side) settled three things.
>
> ### A. The four-address multi-protocol scheme is NOT implemented on this card - VERDICT: NO [V]
>
> ND-60.197.01 *Ethernet Basic Software Programmer Guide*, document page 11 section 2.4, describes a
> scheme where "each Ethernet Interface has in fact four physical addresses ... to make it possible to
> support multiple link level protocols simultaneously, using the same hardware", with bits 7-6 of MAC
> byte 5 selecting `00` IEEE / `01` DIX / `10` user-written / `11` ND (COSMOS Ethernet Option).
> Imported at
> [`..\..\..\..\Reference-Manuals\ND-60.197.01 EN Ethernet Basic Software Programmer Guide.md`](../../../../Reference-Manuals/ND-60.197.01%20EN%20Ethernet%20Basic%20Software%20Programmer%20Guide.md).
>
> **That manual describes the earlier TWO-BOARD Ethernet I Interface. The scheme did not survive into
> the one-board ND 110063.** Evidence, from both directions:
>
> *Software side:*
> - `0x5D7E` unicast compare is an exact six-byte equality loop (`moveq #5,D2 / move.b (A0)+,D3 /
>   move.b (A1)+,D4 / cmp.b D4,D3 / bne / dbf`), raw bytes `16 18 18 19 b6 04 66 06 51 ca ff f6`.
>   No `andi`, no shift, no `btst`, neither operand pre-masked.
> - `0x545A` group-filter compare is the same exact six-byte loop.
> - Image-wide `search_bytes`: **0 hits** for `andi.b #$C0`, `#$3F`, `#$BF`, `#$7F`, `#$FC`;
>   0 hits for `and.b #$C0/#$3F,Dn`; 0 hits for `cmpi.b #$C0`; and **0 hits for `btst #6` anywhere in
>   the image**. The literal OUI `08 00 26` appears only as instruction bytes - it is not a constant.
> - The only MAC bit ever masked is the I/G bit (`and.b #1` at 0x5D64, 0x688A, 0x6980).
> - Nothing addresses byte 5 in isolation - **no xrefs to 0x18863 at all**.
> - There is no protocol-family concept anywhere. The only demultiplex is a single hardcoded 802.2 SAP
>   triple at `0xA1A4`: `DSAP==0xA8`, `SSAP==0xA8`, `control==0x03`.
>
> *Hardware side:*
> - `INITLANCE` @ **0x48EA** programs exactly ONE physical address, MODE = **0x0000**, LADRF = 0.
> - `LNNDTOMAAPPEND` @ **0xF05A** builds the station MAC as `08:00:26:<sysno lo>:<sysno hi>:` and then
>   **`clr.b` byte 5** at `0xF0A2` - a hard constant zero, i.e. the manual's *IEEE* family. The firmware
>   never writes 0x40, 0x80 or 0xC0 there. Peer addresses built at 0xB056/0xB156 use the same routine.
> - PADR is copied verbatim from `g_stationMacAddress`, six bytes, unmasked (0x4970-0x49DA).
>
> **Consequence:** on the Ethernet II card, protocol families are separated by **802.2 SAP**, not by
> address bits. This independently corroborates the "one protocol per card, one downloaded image at a
> time" conclusion.
>
> ### B. Broadcast CAN reach the card - open question O1 is RESOLVED [V]
>
> MODE = `0x0000`, so **PROM is clear but DRCVBC (disable-receive-broadcast, bit 14) is never written
> either** - only bits 15, 6, 5, 4, 3, 2, 1, 0 are ever touched. The Am7990 always accepts
> `FF:FF:FF:FF:FF:FF` regardless of LADRF (LADRF gates multicast only). **So ARP and other broadcast
> frames ARE delivered by the hardware to the firmware.** Whether they survive is then purely the
> software question already answered in section 5: the group filter at 0x542C has no broadcast case,
> so a broadcast is kept only if `FF:FF:FF:FF:FF:FF` is registered in the group list, or if
> `g_addressFilterEnable` is 0.
>
> Note the hardware honours that flag too: at `0x48F8-0x4912`, `g_addressFilterEnable == 0` causes
> `ori.w #0x8000` into MODE, i.e. **it sets LANCE PROM**. The promiscuous semantics recorded in this
> document are implemented at the silicon level as well as in software.
>
> ### C. LANCE facts recovered [V]
>
> - Registers: **`0xEF00A2` = RAP**, **`0xEF00A0` = RDP** (proven by `INTLANCE` @ 0x47F8 reading CSR0
>   and writing it back to clear, then testing ERR/MISS/BABL|MERR/RINT|TINT).
> - Init block **`LANCEINITB` @ DRAM 0x18810**: MODE +0, PADR +2, LADRF +8, RDRA +0x10, TDRA +0x14.
>   Built by `INITLANCE` @ 0x48EA.
> - CSR sequence: CSR3 = 0x0004 (BSWP), CSR1 = 0x8810, CSR2 = 0x0001 (block at 0x018810), CSR0 = INIT,
>   then `0x0042` (INEA|STRT) at 0x4890; stop writes `0x0004` at 0x48C8.
> - Ring lengths: `ori.w #0xE000` at 0x4A94 / 0x4AB8 -> RLEN = TLEN = 7 -> **128 descriptors** each,
>   matching the 0x18000 / 0x18408 anchors.
> - LADRF is zero-initialised then filled by a **CRC hash** (0x4754 -> 0x1342C / 0x1310C) over a runtime
>   list of subscribed group addresses at `0x18942` (subscribe 0x53AC, unsubscribe 0x53EC). With
>   nothing subscribed LADRF = 0 and all multicast is rejected.
> - `0x503A` is **not** the LANCE programmer - it starts two PIOC-OS objects, `LNMASPCOMM` (0x18834)
>   and `LNMASPDATA` (0x18848), via 0x1179C (magic `0xAAAF` check).
>
> ### D. CORRECTION - `0x18886` is the LANCE loopback-mode selector [V]
>
> This document (and the Ghidra database) previously named `0x18886` `g_txMinLengthPadMode`. **That
> name was too narrow.** `INITLANCE` at `0x492A` switches on it through a case table at `0x18954`
> (`[0]=0x495E [1]=0x493E [2]=0x495E [3]=0x494E [4]=0x495E`), where 0x493E sets LOOP+INTL (internal
> loopback), 0x494E sets LOOP (external loopback) and 0x495E clears both (normal operation). The value
> 4 written at init therefore means **normal, non-loopback operation**.
>
> The TX-pad reading is a genuine *second* consumer and is compatible: `XMTRINGAPPEND` at `0x6110` does
> `cmpi.w #4,(0x18886)` and pads short frames to 60 bytes only when it equals 4 - i.e. padding is
> applied only in normal mode. Both consumers must be kept in mind; everywhere this document says
> "keep `word[0x14] = 4`", read it as "keep the card in normal, non-loopback mode".
>
> ### E. The firmware carries its own symbol table [V]
>
> A vendor symbol table sits at approximately **file offset 0x66D00**, records of the form
> `flags | addr | 12-char name`. It is the source of the names `INITLANCE`, `INTLANCE`, `STARTIO`,
> `LANCEINITB`, `LNMAPHYSICALADDRESS`, `LNMASPCOMM`. Mining it wholesale is the single highest-value
> action available on this binary and is in progress.
>
> ### F. Unresolved conflict
>
> One agent reports `0x83E6`/`0x83E8` is a **buffer-array init** (walking a table at 0x1CF32, entry
> size 0x60); an earlier pass placed the **COSMOS TX frame build** at 0x83E6-0x849C and quoted decoded
> bytes for it (`847E: 14 bc 00 a8` = `move.b #0xa8,(A2)`). Both may be parts of one function, or one
> is wrong. The claim in the first correction banner above that the frame build lives at 0x83E6 rests
> on this and is **not yet settled** - a clean disassembly of 0x83E6-0x84A0 is needed.

## TL;DR

**Ethernet II is already supported by the firmware. It is not enabled by default, and it is turned on
purely by host configuration - no firmware patch is needed.** [V]

The word at DRAM `0x1888A` (now named `g_mode8023LengthField` in the Ghidra database) is not an
"802.3 vs Ethernet II" selector. It is a **"is there a length field in the header"** switch:

| Value | Receive | Transmit |
|---|---|---|
| `!= 0` (power-on default 1) | 14-byte header; frames 61..1514 accepted **only if** bytes 12-13 == payload length | firmware **writes** the length into bytes 12-13 |
| `== 0` | 14-byte check **skipped entirely**; every in-range frame passes | firmware writes **only dst+src (12 bytes)**; host owns bytes 12-13 |

With the mode word cleared the card becomes a **raw frame pass-through** in both directions, which is
exactly what a host-side TCP/IP stack needs. Ethernet II frames are dropped in the default mode only
as a side effect of the length-equality test - an EtherType is >= 0x0600 (1536) and the payload is
<= 1500, so the equality can never hold.

**The firmware contains no EtherType logic whatsoever** [V]: no 1536/0x0600 threshold, no
0x0800/0x0806/0x86DD constants used in frame parsing, no LLC DSAP/SSAP decode, no SNAP decode, no
protocol table. `86 dd` does not occur anywhere in the image; the only `08 06` byte match in the whole
binary is the instruction `btst.l #0xe,D6` at 0x5CC8.

---

## 1. The mode word - complete cross-reference

`g_mode8023LengthField`, word, DRAM `0x1888A`, card BSS, word aligned. Seven cross-references, all
absolute `.l` addressing - there are no indirect/pointer references, so the full set is known. [V]

| Address | Dir | Site | Effect |
|---|---|---|---|
| `0x7026` | W | LNMAINIT power-on init | `= 1` (802.3 length mode ON by default) |
| `0x679A` | W | SET-MODE-FLAGS command handler | `= command node word[0x18]`, unvalidated |
| `0x5D0A` | R | RCVCOMPLETE classifier | zero -> skip the length check entirely |
| `0x5E7C` | R | RCVCOMPLETE delivery-node build | selects hdrlen 14 vs 12 reported to the host |
| `0x6086` | R | XMTRINGAPPEND header build | selects whether a length word is written |
| `0x6BB6` | R | TX command validation | requires hdrlen >= 14 |
| `0x6BCC` | R | TX command validation | requires hdrlen >= 12 |

---

## 2. Receive classification - the actual algorithm [V]

Reconstructed instruction-by-instruction from `RCVCOMPLETE` @ 0x5C42:

```c
if (RMD1 & OWN)  yield();                        // 0x5C72
if (BCNT == 0)   yield();                        // 0x5C80
frameLen = (MCNT & 0x0FFF) - 4;                  // 0x5CA2  (FCS stripped)
if (RMD1 & ERR)  hardwareErrorDrop();            // 0x5CC8

/* size gate - runs BEFORE the mode word is consulted */
if (frameLen < 60 || frameLen > 1514) {          // 0x5CD0 / 0x5CDA, both UNSIGNED
    formatOk = 0; stats[0x2A]++;                 // runt / oversize counter
}
else if (g_mode8023LengthField == 0) {           // 0x5D0A
    formatOk = 1;                                // RAW MODE: no check at all
}
else if (frameLen <= 60) {                       // 0x5D14, UNSIGNED
    formatOk = 1;                                // BYPASS: short frames unchecked
}
else {
    formatOk = (be16(frame + 12) == frameLen - 14);   // 0x5D1C / 0x5D20
    if (!formatOk) stats[0x24]++;                     // length-mismatch counter
}

/* address check */
if (frame[0] & 1) addrOk = GroupAddressFilterMatch(frame);   // 0x5DCC
else              addrOk = (memcmp(frame, g_stationMacAddress, 6) == 0);   // 0x5D7E
if (!addrOk && g_addressFilterEnable == 0) addrOk = 1;       // 0x5D8C / 0x5DDE

if (!addrOk)          discard();   // GATE 1, 0x5DF2
if (!formatOk)        discard();   // GATE 2, 0x5DFA
if (rxPoolEmpty())    discard();   // GATE 3, 0x5E02 / 0x5E0E
deliverZeroCopy();                 // 0x5E1C
```

**Signed-vs-unsigned:** every magnitude comparison in this path is `BCS`/`BCC` (unsigned) or a plain
equality/`BNE`. There is no signed-comparison hazard anywhere in the classifier. [V]

**Byte order:** the length/type field is read with a single big-endian `move.w (0xc,A4),D1w` at
0x5D1C. No DMA byte-swap, no word-swap, no byte-wise reassembly. [V]

**Handling of the reserved gap 1501..1535:** in mode `!= 0` such a value can never equal a payload
length (payload <= 1500), so those frames are rejected by the same equality test as any DIX frame. In
mode `== 0` they are accepted and passed through uninterpreted. The firmware has **no explicit
handling of the gap** - it simply falls out of the equality test. [V]

### Which model does this match

Not Model A (proper length/type discrimination). The receive path is **Model B in the default mode**
(field interpreted exclusively as a length) and **Model D in mode 0** (raw frame forwarding, host
interprets the field). There is no Model C (no LLC inspection) and no Model E (no hardware protocol
classification is read from the descriptor - only OWN/ERR/CRC/FRAM/OFLO/BUFF). [V]

---

## 3. Transmit path [V]

`XMTRINGAPPEND` @ 0x6054:

```c
if (g_mode8023LengthField) {              // 0x6086
    hdr = base + hdrLen - 14;
    be16(hdr + 12) = totalLen;            // 0x60AA - firmware WRITES an 802.3 length field
    wireLen = 14 + totalLen;
} else {                                  // 0x60BC
    hdr = base + hdrLen - 12;
    /* nothing is written at hdr+12 - the host's first two payload bytes land there */
    wireLen = 12 + totalLen;
}
memcpy(hdr + 0, cmdNode + 0x22, 6);       // 0x60E0 - dst MAC supplied by the HOST
memcpy(hdr + 6, g_stationMacAddress, 6);  // 0x60EE - src MAC supplied by the FIRMWARE
if (g_txMinLengthPadMode == 4 && wireLen < 60) wireLen = 60;   // 0x6110
```

Validation before this, at 0x6BB6 / 0x6BCC / 0x6BEC:

| Check | Mode != 0 | Mode == 0 | Error |
|---|---|---|---|
| descriptor.hdrLen | >= 14 | >= 12 | `-0x17` |
| descriptor.totalLen | <= 1500 (0x5DC) | <= 1500 | `-0x16` |

So in mode 0 the host can place **any** 16-bit value at wire bytes 12-13, including 0x0800, 0x0806 and
0x86DD, simply by making it the first two bytes of its payload buffer. The firmware never inspects,
validates or rewrites it. 1500 is used only as an MTU bound; **1536 / 0x0600 appears nowhere in the
image**. [V]

---

## 4. What the host actually receives - raw pass-through proof [V]

In the accept path at 0x5E72:

- `descriptor.base` (delivery node + 0x18) = the LANCE buffer address = **byte 0 of the frame, i.e.
  the destination MAC**. The frame is **not copied** and the MAC header is **not stripped** -
  delivery is zero-copy.
- `descriptor.len` (node + 0x1C) = the **buffer** size derived from `-BCNT`, not the frame length.
- `descriptor.totalLen` (node + 0x20) = the real payload length: the 802.3 length field verbatim in
  mode != 0, or `frameLen - 12` in mode 0.
- `descriptor.hdrLen` (node + 0x1E) = 14 or 12 per the mode word.
- dst MAC is additionally duplicated to node + 0x22 (0x5EA8), src MAC to node + 0x28 (0x5EB4).

**The host therefore always receives the complete frame including bytes 12-13, in both modes.** The
FCS is already stripped (`- 4` at 0x5CAA). Padding is included in the buffer but `totalLen` is
authoritative.

Receive buffer size is fixed: `POST-RX-BUFFER` rejects any node whose `descriptor.len != 0x5F0`
(1520 bytes) with error `-0x16` (0x6C82). 1520 comfortably holds a full 1514-byte Ethernet II frame,
so **buffer size is not a constraint on TCP/IP**. [V]

---

## 5. Broadcast / ARP - the one real obstacle at this layer [V]

`GroupAddressFilterMatch` @ 0x542C walks a singly-linked list at `g_groupAddressListHead`
(`0x18942`); each node is `{ +0 long next, +4 six-byte MAC }`. It byte-compares all six bytes and
returns 1 on match, 0 otherwise. An empty list returns 0 for every group frame (0x5484).

**There is NO hardcoded broadcast case.** `FF:FF:FF:FF:FF:FF` is not special-cased in this routine or
anywhere in RCVCOMPLETE. A broadcast frame reaches the host only if:

- (a) `FF:FF:FF:FF:FF:FF` has been added to the group list by the host's SET-GROUP-ADDRESS command
  (0x6880 / 0x68A8), **or**
- (b) `g_addressFilterEnable` (`0x18888`) == 0, i.e. promiscuous.

Both are reachable with existing host commands, so **no firmware change is required for ARP** - but
see the open question in section 9 about the LANCE's own hardware filter.

---

## 6. Corrections to earlier docs in this folder

| Doc | Stale claim | Correct reading |
|---|---|---|
| [ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md](ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md) | "`0x18888` Promiscuous flag, init 1" | **Inverted.** Nonzero = address filtering ENABLED. Init 1 means NOT promiscuous. Evidence: 0x5D8C `tst.w 0x18888 / bne 0x5D9C` - on a MAC mismatch, nonzero takes the REJECT branch; the zero case falls into `move.w #1,(0x38,A6)` = accept. Same polarity at 0x5DDE. Renamed `g_addressFilterEnable`. |
| [ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md](ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md) | "`0x1888a` mode-8023 flag; gates TX hdr-len" | True but incomplete - it is a raw-12-byte-header vs 802.3-14-byte-header switch that also changes what the host is shown on receive (0x5E7C), and mode 0 is a full raw pass-through. |
| [ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md](ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md) sec 2 | "the 0x6034 branch ... advance the ring, re-append the buffer, yield" implying accept and discard differ in ring handling | Both accept AND discard reach the shared tail 0x5FF2 -> clear RMD -> 0x5FFC advance FREE/CONS. They differ only at 0x601A. Also: 0x5FFC is NOT dead code despite having no xrefs - execution RESUMES at the instruction after the `jmp (A5)`. **CORRECTED 2026-07-26:** the reason is NOT a "coroutine yield". PLANC-MC compiled routines return to **return address + 2** (`movea.l (SP)+,A6 ; movea.l (SP)+,A2 ; jmp (0x2,A2)`), which SKIPS the 2-byte `4E D5 jmp (A5)`. That trampoline is the **error-unwind path**, entered only when the runtime returns to +0. See the PLANC calling-convention section below. |

Both files have been annotated in place with dated correction notes.

---

## 7. TCP/IP capability matrix

| Capability | Status | Firmware evidence |
|---|---|---|
| Ethernet II receive | **Supported (host config)** | 0x5D0A - mode 0 skips gate 2 entirely |
| Ethernet II transmit | **Supported (host config)** | 0x60BC - no length word written; host owns bytes 12-13 |
| ARP receive | Supported, conditional | passes gate 2 in mode 0; needs broadcast MAC registered (0x542C) or filter off |
| ARP transmit | Supported | dst MAC is host-supplied at node+0x22 (0x60E0); no broadcast restriction found |
| IPv4 receive / transmit | Supported (mode 0) | same paths; no EtherType inspection exists |
| IPv6 receive / transmit | Supported (mode 0) | `86 dd` absent from the image, so nothing blocks it either |
| Broadcast reception | **Conditional** | 0x542C has no broadcast case - see section 5 and open question O1 |
| Raw frame delivery | Supported | 0x5E72 - zero-copy, MAC header intact, full frame |
| Raw frame transmission | Supported (mode 0) | 0x60BC |
| Adequate buffers | Supported | 1520-byte RX buffers enforced at 0x6C82 vs 1514 needed |
| Unknown EtherType forwarding | Supported (mode 0) | nothing classifies, so nothing is selectively dropped |
| COSMOS coexistence | **Needs ND-100 driver change** | see section 8 |

LLC/SNAP IP encapsulation is not needed here and was not pursued: mode 0 gives ordinary Ethernet II
directly, which is what real TCP/IP peers expect.

---

## 8. COSMOS coexistence - the actual constraint

`0x1888A` is a **single global word**. There is no per-frame, per-client or per-queue mode, and there
is exactly **one** receive ring (128 x 8-byte RMDs at 0x18008) and **one** host ready list (0x188D6).
Changing the mode does not create or select a second queue - it only changes which frames survive to
be delivered on the one queue. [V]

Consequences of running the card in mode 0 - **CORRECTED 2026-07-26**:

- COSMOS **receive** passes the firmware gate 2 unconditionally, **but then dies one layer up**: the
  on-card ENCOS server (0xA18E) computes payload as `base + hdrLen`, and with `hdrLen = 12` it reads
  the 802.3 length field as LLC DSAP/SSAP, fails the `0xA8/0xA8/0x03` test, and rejects the frame. [V]
- COSMOS **transmit** breaks: the ENCOS server writes its LLC payload at `base+14` (0x847E) and
  leaves bytes 12-13 for the firmware, which in mode 0 no longer fills them. [V]

**The earlier claim in this section - that coexistence is achievable with no firmware change, by
having the ND-100 build and parse the length field - is WRONG and is retracted.** The ND-100 does not
build the frame and never sees it; the ENCOS server on the card does both. Fixing this means patching
the ENCOS server (which hard-codes +-14 at 0x83E6-0x849C, 0x8E50, 0xA18E, 0xADD4), not the ND-100.

Note also that COSMOS is identified **entirely on the ND-100**. The captured COSMOS payload begins
`a8 a8 03` (LLC DSAP 0xA8 / SSAP 0xA8 / UI control 0x03), but **no firmware code reads those bytes**.
The card has no notion of COSMOS. [V]

---

## 9. Required host behaviour (the concrete recipe)

1. `SET-MODE-FLAGS` (handler 0x6786) with:
   - `word[0x18] = 0` -> raw / no length field
   - `word[0x16] = 0` (promiscuous) **or** keep 1 and register `FF:FF:FF:FF:FF:FF` via
     SET-GROUP-ADDRESS (0x6880 / 0x68A8)
   - `word[0x14] = 4` -> keep the 60-byte minimum TX padding
2. `ENABLE-RX-POOL`, then `POST-RX-BUFFER` nodes of **exactly 1520 bytes** (0x5F0).
3. Transmit: supply `hdrLen >= 12` with the EtherType as the first two bytes of the payload buffer;
   the firmware fills dst MAC (from the command node) and src MAC (from the station address).
4. Receive: read the whole frame from `descriptor.base`; `totalLen` at node+0x20 is `frameLen - 12`.

---

## 10. Open questions - these feed the ENNS0 driver analysis

- **O1 [U] - the decisive one.** Whether the Am7990 LANCE's own MODE register (PROM bit) and LADRF
  are programmed to accept broadcast. `STARTMA` @ 0x5850 only clears `0x18880` and calls 0x503A; the
  actual LANCE MODE/LADRF programming was not traced. **If the LANCE hardware filter drops broadcast,
  no host-level setting will help and ARP fails.** Resolve before committing.
- **O2 [U]** Does the ND-100 ENNS0 driver ever issue SET-MODE-FLAGS, and with what values? If it
  hardcodes `word[0x18] = 1` on every start, any Ethernet II configuration will be overwritten.
- **O3 [U]** Does ENNS0 expose any host-side path that would let a second consumer post RX buffers /
  read the ready list, or does it own the ready ring exclusively?
- **O4 [U]** SET-MODE preconditions at 0x6770 that return `-0xA` were not decoded.
- **O5 [U]** `g_txMinLengthPadMode` (`0x18886`) has exactly one read (`cmpi.w #4` at 0x6110); other
  legal values unknown.
- **O6 [U]** Consumer of the RX error event word `0x1894C` untraced; `txretry_list` producer/consumer
  untraced.
- **O7 [U]** Vendor's official field names for the stats block offsets are not in this image; the
  labels below are descriptive, not Norsk Data's.

---

## 11. Statistics block offsets recovered [V]

`g_nmaStatsBlock` @ `0x1888C`, 0x3A (58) bytes, source of the READ-STATS command. All counters are
saturating (`cmp.w #-1 / bcc skip / addq`).

| Offset | Size | Bumped when | Site |
|---|---|---|---|
| +0x14 | long | no free receive buffer (gate 3b) | 0x5ECA |
| +0x1C | word | CRC error (RMD1 bit 11) | 0x5F06 |
| +0x1E | word | framing error (RMD1 bit 13) | 0x5F3E |
| +0x20 | word | overflow (RMD1 bit 12 OFLO), also sets bit 1 of 0x1894C | 0x5FBA |
| +0x22 | word | buffer error (RMD1 bit 10 BUFF), also sets bit 2 of 0x1894C | 0x5F76 |
| +0x24 | word | **length / format mismatch** | 0x5D30 |
| +0x2A | word | runt (<60) or oversize (>1514) | 0x5CE2 |
| +0x2C | word | address reject | 0x5D9C |

**+0x24 is the diagnostic to watch:** it ticks once for every Ethernet II frame over 60 bytes that
the mode word discards. If IP traffic is arriving and being silently dropped, this counter is the
proof.

---

## 12. Ghidra database changes made during this analysis

Program: `encos-ser-all-banks-68k.bin`.

**Disassembled** (previously undefined bytes): 0x5EF4, 0x604C.

**Functions renamed / documented:**
- `maybe_format_lance_descriptor` -> `LanceRxDescriptorClear` (0x553C) + full doc block. Note it does
  NOT set OWN; re-arming is done separately by 0x5B60.
- `maybe_handle_group_address` -> `GroupAddressFilterMatch` (0x542C) + doc block recording the
  missing broadcast case.
- `RCVCOMPLETE` (0x5C42) given a full plate comment (purpose / inputs / outputs / global state /
  control flow / what the frame parser does and does not do).

**Globals renamed:**

| Address | Old | New |
|---|---|---|
| 0x1888A | `mode_include_length_field` | `g_mode8023LengthField` |
| 0x18888 | `lan_mode_flag` | `g_addressFilterEnable` (polarity corrected) |
| 0x18886 | `lan_function_code` | `g_txMinLengthPadMode` |
| 0x1885E | `lance_mac_address` | `g_stationMacAddress` |
| 0x18000 | `lance_rx_ring_ctrl` | `g_lanceRxRing` |
| 0x18408 | `lance_tx_ring_ctrl` | `g_lanceTxRing` |
| 0x1888C | `nma_stats_block` | `g_nmaStatsBlock` |
| 0x18942 | `DAT_00018942` | `g_groupAddressListHead` |

**Structures created** (category `/encos_ethernet`): `LanceRmd` (8 B), `EncosFrameDescriptor` (12 B),
`EncosRxRingHeader` (8 B, applied at 0x18000).

**Comments added** at: 0x1888A, 0x18888, 0x1888C, 0x1894C, 0x18000, 0x18408, 0x542C, 0x5C92, 0x5CA2,
0x5CD0, 0x5D0A, 0x5D14, 0x5D1C, 0x5D20, 0x5D2C, 0x5DF2, 0x5DFA, 0x5E02, 0x5E72, 0x5E7C, 0x5EF4,
0x5EFC, 0x5FF2, 0x601A, 0x6086, 0x679A, 0x6BB6, 0x6C82, 0x7026.

**Bookmarks** in categories `EncosRxPipeline` (0x5C42, 0x601A), `EncosRxClassifier` (0x5D20, 0x5DFA),
`EncosRxDrop` (0x5EF4, 0x5EFC), `EncosRxFilter` (0x542C), `EncosModeFlags` (0x1888A, 0x679A).

**Not done:** no enums were created - the Ghidra MCP interface in use exposes no enum-creation tool,
so the RMD1 bit meanings and the mode-word states were recorded as comments instead.

---

## Related

- [ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md](ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md) - the card's host
  command/ring contract (corrected by section 6 above)
- [ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md](ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md) - the original
  gate analysis and the FCS double-count fix
- [ENNS0-RXPOOL-PRODUCER-RE-2026-07-24.md](ENNS0-RXPOOL-PRODUCER-RE-2026-07-24.md) - who fills the
  receive pool
- [COSMOS-MULTI-NODE-NETWORK-2026-07-25.md](COSMOS-MULTI-NODE-NETWORK-2026-07-25.md) - the working
  two-node COSMOS network
- `..\..\..\..\Installation\Communication\Ethernet\x\stripped\docs\` - the rest of the 68000 firmware
  reverse engineering
- `..\..\..\..\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md` - the card manual

**Next step:** deep analysis of the SINTRAN ENNS0 driver to answer O2 and O3 - whether the ND-100
side can be made to drive the card in mode 0 without breaking COSMOS.
