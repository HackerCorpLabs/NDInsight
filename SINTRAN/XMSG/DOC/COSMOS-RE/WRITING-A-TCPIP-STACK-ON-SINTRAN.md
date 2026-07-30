# Writing a TCP/IP stack for SINTRAN III on the ND Ethernet II controller

**Date**: 2026-07-29
**Audience**: someone who knows networking but not Norsk Data, or knows ND but not TCP/IP.

## How to read this document

Every factual claim carries a marker. This matters more than usual, because a lot of what circulates
about this hardware is folklore.

| Marker | Meaning |
|---|---|
| **[P]** | PROVEN - measured on running hardware/firmware, or quoted verbatim from an ND manual |
| **[E]** | EVIDENCED - strongly implied by code or documentation that has been read |
| **[U]** | UNKNOWN - stated as an open question, never filled in with a plausible guess |
| **[I]** | ILLUSTRATIVE - code shape to communicate a design, NOT verified to compile |

If something you need is marked **[U]**, that is deliberate. Do not build on it without checking.

---

# Part 1 - Introduction

## 1.1 What we are building and why it is possible

The goal is an IP stack running on SINTRAN III, sending and receiving real Ethernet II frames through
an ND Ethernet II controller (ND-110063), so a NORD machine can speak TCP to the modern world.

The question that blocked this for a long time was whether the card could carry DIX frames at all, or
whether it was hard-wired for 802.3 (which COSMOS uses). **[P]** It can. Measured on the running
firmware, both directions:

```
DIX   (mode word 0x1888A = 0): on-wire len 60, frame bytes 12-13 = 0x0800  (our EtherType survived)
802.3 (mode word 0x1888A = 1): on-wire len 78 = 14+64, bytes 12-13 = 0x0040 (= the payload length)
```

So the card is not the obstacle. What follows is what you have to build on top.

## 1.2 The layers you are dealing with

```
  your applications        telnetd, ftpd, httpd, telnet client, ftp client
  ------------------------------------------------------------------------
  transport                TCP, UDP
  internet                 IP, ICMP, ARP
  ------------------------------------------------------------------------
  driver (RT program)      frame in/out, buffer management, interrupt handling
  ------------------------------------------------------------------------
  ENCOS firmware           runs on the card's 68000 - you do NOT write this
  Am7990 LANCE             the actual Ethernet chip
```

You write the top three boxes. The bottom two already exist and are decoded.

## 1.3 What is genuinely hard here

Be clear-eyed about the difficulty, because it is not where a modern programmer expects.

- **You are writing a driver for a co-processor, not a chip.** The card runs its own RTOS (PIOC-OS)
  on a 68000. You do not touch the LANCE. You post request nodes to the firmware and it does the
  rest. That is easier than a bare-metal NIC driver in some ways and stranger in others.
- **Memory is precious and segments are fixed-size.** This is not a system where you malloc a socket
  buffer.
- **There is no existing IP stack to borrow from.** COSMOS is not IP. XMSG is not IP. You are
  starting from Ethernet frames.
- **[P] The mode word is global.** One card carries either DIX or 802.3, never both simultaneously.
  ND's own answer to this was two cards. **Attribution matters here**: "one protocol per controller,
  dual stack needs two cards" is an **[I] inference** drawn from many converging statements, NOT a
  sentence any ND document contains. What IS **[V]** quoted is the per-controller cost line *"you
  will have to assign an Internet address for each controller"* and the requirement wording *"One ND
  110063 Ethernet II Controller **for TCP/IP**"*. So COSMOS and TCP/IP coexisting means two
  controllers, not clever multiplexing - but nobody wrote that down in those words.

## 1.4 What Norsk Data themselves shipped - and why it is the opposite of Part 5

Before designing anything, know that ND solved this problem, on this exact board, and their
architecture is the **reverse** of the one in Part 5. Full evidence in
[HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md](HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md).

**[V]** The product was **COSMOS TCP/IP Gateway for Ethernet, ND 211185**, versions C07 (1990) and
D02 (1992). Its own product info says:

> "Norsk Data has implemented the IP protocol of the COSMOS TCP/IP Gateway as a separate controller
> with its own processor (Motorola 68000) and its own memory (1/2 Mbyte) ... **The controller is the
> same as for COSMOS over Ethernet (ND-110063 Ethernet II Controller).**"

**[V] The layer split: TCP on the ND-100, IP on the card.** Quoted from the TELNET/FTP Client User
Guide ND-860284-1 section 1.3: *"The TCP software is implemented in ND-100 and the IP software is
implemented in a separate controller."* The on-card IP layer is called **AIP** (ARPA Internet
Protocol) and runs on PIOCOS over PIOC ports and XMSG - the same transport ENNS0 uses. Its error
texts give the stack away: `AIPpiocError : PIOCOS error`, `AIPportError : fatal in IOC port message
system`, `AIPxmsgError : XMSG error`, `AIPBADmaBuffer : BAD address of MA(Media Access) buffer`.

**[V] The ND-100 never sees a raw MAC frame in any shipped ND product.** Every path crosses to the
controller as XMSG / PIOCOS port messages. That is the single most important sentence in this
document for anyone choosing an architecture.

**[V] 211185 was DIX only - it did NOT support both framings.** Its documented protocol set:

| Layer | Standard |
|---|---|
| Application | Telnet RFC 854; FTP RFC 959, RFC 765 |
| Transport | TCP RFC 793 |
| Network | IP RFC 791; ICMP RFC 792; ARP RFC 826; IP Reassembly RFC 815 |
| **Data Link** | **Baseband Ethernet: DIX 2.0** |
| Physical | Ethernet Accessories: IS 802.3 |

*"ND's TCP/IP implementation for SINTRAN is based on the UNIX 4.2 BBN implementation."*

802.3 + LLC (DSAP/SSAP 0xA8, control 0x03) is the COSMOS load, a different product. ND's own hardware
manual ND-12.055.1 Appendix C prints them as two separate columns, "COSMOS stack" and "ARPA stack".
**[OPEN]** the ARPA layer-2 cell reads "**LLC1 / DIX**", not plain DIX, so whether LLC/SNAP
encapsulation was also offered is nowhere stated.

**[P] This corroborates the firmware carve exactly.** The mode word at `0x1888A` is a single global
word with no per-request override - which is precisely what a product line of "one protocol per
controller, selected by which image you download" requires. `0x1888A = 0` is very likely the exact
configuration the 211185 on-card image used. **[I]** - the documents state the DIX 2.0 result, never
the mechanism.

## 1.5 The 211185 on-card image: what is documented, and what is not

**[V] The image itself is NOT documented anywhere.** Neither 211185 product sheet names the on-card
image, its banks, or a firmware version - unlike the COSMOS Ethernet Option 210580, whose Program
Description lists `ENCOS-SER-B0..B3-B01:BPUN` explicitly. The words "firmware", "PROM" and "BPUN"
appear **nowhere** in the 211185, 211327 or 211154 sheets. **[V] No distribution media for any ND
TCP/IP product exists in this repository** - paper only, verified by sweeps for `*211185*`,
`*211327*`, `*211154*`, `*.IMAG`, `*BPUN*`, `*AIP-*`.

**[V] What IS documented is the cost, and the numbers are informative:**

```
Number of segments (ND-100)      3 + 4 * NbOfControllers
Space required on segment files  120 + 256 * NbOfControllers pages
```

**[I] Read that arithmetic.** Four segments per controller is the same shape as ENCOS's four BPUNs
(B0-B3). And 256 pages x 1KW = **512 KB, exactly the card's DRAM**. So the per-controller segment
cost almost certainly *is* the downloaded card image, loaded the same four-bank way as COSMOS, just a
different image. Inference from arithmetic, not a quote - but it is tight, and it means a recovered
211185 media set would drop straight into the existing four-bank load path.

The load step is a mode file: `@MODE (TCP-IP)TCP-IP-LO:MODE,,,` in C07, `TCP-IP-LOAD:MODE` in D02.
Diskette 2 carries a **PIOC-MONITOR**, "meant for use by ND service personnel only".

**[V] Protocol selection is by which image you download** - proven for the Ethernet III board, where
PROMAN downloads one of `PMA-ETH3-TCPI:IMAG` / `PMA-ETH3-COSM:IMAG` / `PMA-ETH3-SIBR:IMAG`,
described as *"Necessary when more than one type of product runs on processors with the same module
number."* **[I]** for Ethernet II - no document names an Ethernet II TCP/IP image file.

**Practical consequence.** Recovering `211185C-XX-01D` or `211185D-XX-01D` would be worth more than
any amount of further reverse engineering: it is a working DIX-2.0 image for this exact card, and it
would show the mode word being set and the entire host-to-card protocol that Part 4.0 lists as the
biggest unknown here.

> **UPDATE 2026-07-30 - RECOVERED.** An installed **211185 B05** (July 5, 1988) was found on the
> Tingo MFM hard-disk dump under user `TCP-IP`: the four on-card BPUNs, both servers, both clients,
> and the load mode files. All four BPUNs pass the documented BPUN checksum. Details in
> [TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md](TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md).
> The findings that matter for this guide:
>
> - **[V]** The cost-table arithmetic above is confirmed: four banks x 128 KB = the card's 512 KB.
>   The mode file loads them as four SINTRAN segments with `READ-BINARY` + `SE-LO-AD,,177777`,
>   exactly as COSMOS loads ENCOS.
> - **[V]** Bank 0 is **PIOCOS with the same module builds as ENCOS** - same OS, different payload,
>   now proven rather than inferred.
> - **[V]** The IP layer issues a media-access command literally named **`set DIX mode`** and reports
>   `attached to MEDIA ACCESS in DIX mode`. The framing question is settled from ND's own product.
> - **[V]** DIX attach can fail with `BAD - other user attached` - an exclusivity mechanism, relevant
>   to the coexistence question in Part 3.4.
> - **[V]** 437 named symbols with addresses were recovered, including a second build of the
>   media-access routines we carved from ENCOS (`RCVCOMPLET`, `XMTRINGAPP`, `INITLANCE`, `STARTMA`).

## 1.6 Do not confuse 211185 with the Ethernet III product

**[V]** ND had **two** TCP/IP products on **two** different boards, and only one of them is relevant:

| Product | Runs on | Board | CPU |
|---|---|---|---|
| **211185** COSMOS TCP/IP Gateway | **Ethernet II** - our board | ND 110063 | 100 CX / 110 CX / 120 CX / 500 / 5000 |
| **211327** TCP/IP Basic Module/III | Ethernet III, MF-bus, Domino | ND 110513 | **ND-5000 only** |

Neither 211327 sheet mentions Ethernet II or PCB 3094 at all. **[V]** And ND preferred the other
board: *"In order to run the X Window System with maximum performance it is necessary to have a
TCP/IP Basic Module/III running in an Ethernet III controller. Although it is possible to use an
Ethernet II controller with the COSMOS TCP/IP Gateway, **this is not recommended**."*

**[V] One hard ND limitation worth knowing** (ND-860284-1 p.14): *"an ND host cannot be an IP
(Internet) gateway, a feature that is standard in the BSD 4.2 version."* No IP routing. Real BSD
sockets and routing existed only under NDIX on the ND-500.

**[V] A shortcut, if the goal is to satisfy ND's own clients rather than build a stack.** The shipped
FTP/Telnet clients are pure XMSG clients requiring 0 segments and 0 RT descriptions; they locate the
stack by XMSG **port name** - `*TCP.` for a local stack, `*TCPGATE` for a remote one. So anything
that registers and serves those names satisfies them, without a faithful DIX MAC path at all. A far
smaller target than Parts 4 through 8.

---

# Part 2 - What you need to know before starting

## 2.1 The ND manuals that actually matter

| Manual | ND number | What you need from it |
|---|---|---|
| Ethernet II Controller | **ND-12.055.1** | The card: thumbwheels, banks, IOX ranges, ident codes, "maximum of four Ethernet II controllers" |
| Ethernet Basic Software Programmer Guide | **ND-60.197.01** | Section 2.4 - the 48-bit address format. The ENUMDE record. Physical user addresses |
| COSMOS Ethernet Option (program description) | **ND-210580-02** | Install procedure, ENNS`<n>` naming, the "must be generated with Ethernet Interface datafield" requirement |
| SINTRAN III Monitor Calls | **ND-860228-2** | Every MON call you will need from an RT program |
| SINTRAN III Reference Manual | **ND-60.128.5** | RT-LOADER, segments, command syntax |
| COSMOS Operator Guide | **ND-30.025.2** | Operational context; virtual sysids 9800-9803 |

## 2.2 SINTRAN concepts you cannot avoid

**[P]** From the SINTRAN documentation and observed practice:

- **RT program** - a real-time program with a priority, declared to SINTRAN and living on a segment.
  Your driver and probably your stack are RT programs.
- **Segment** - a fixed-size chunk of address space loaded from the segment file. Loaded by
  `RT-LOADER` with `READ-BINARY <file> <segment-number>`. Survives a warm start; rebuilt on a cold
  start by `HENT-MODE:MODE`.
- **Reentrant program** - a background program dumped with `DUMP-PROGRAM-REENTRANT` so multiple
  users can run it simultaneously. **This is the mechanism your client programs (telnet, ftp) should
  use.**
- **RT description** - the table entry SINTRAN uses to schedule the RT program. `Max/Min number of
  RT-descriptions` appears in every ND program description because they are a finite resource.
- **Logical device number (LDN)** - how SINTRAN names a device. **[P]** The Ethernet interfaces are
  LDN **2240B-2243B** for interfaces 0-3 on SINTRAN K and later; on J they are PIOC LDNs 1700B-1703B.
- **Interrupt level** - the ND-100 has hardware priority levels. **[P]** All four Ethernet
  controllers interrupt on **level 12**, distinguished by ident code 140034B-140037B.

## 2.3 The hardware, concretely

**[P]** From ND-12.055.1 and verified in the emulator:

| Thumbwheel 12J | IOX range | Ident code | Level | LDN (K+) |
|---|---|---|---|---|
| 0 | 140360B-140363B | 140034B | 12 | 2240B |
| 1 | 140364B-140367B | 140035B | 12 | 2241B |
| 2 | 140370B-140373B | 140036B | 12 | 2242B |
| 3 | 140374B-140377B | 140037B | 12 | 2243B |

A separate strap group (**7J/9J**) puts each card's **512 KB DRAM** into an ND-100 memory bank. **[P]**
The card's DRAM appears as **four 128 KB banks**, which is why the bank number advances by 4 per card
and why the firmware ships as four images (`ENCOS-SER-B0..B3`), all four loaded per install.

**[P] The card has no EPROM and no address PROM.** SINTRAN downloads the entire firmware and writes
the station MAC at bring-up.

**Prerequisite that will bite you: [P]** the interface must exist in the SINTRAN generation. The
COSMOS installer literally runs `@LIST-DEVICE 2241B 0` and greps for `NO SUCH DEVICE`. In the SINTRAN
source the interfaces are conditional-assembly flags `8ETR1`, `8ETR2`, `8ETR3`. **[U]** There is no
documented way to add one to an already-generated system.

---

# Part 3 - 802.3 versus DIX: what, why, and when

## 3.1 The actual difference

Both start identically:

```
 bytes  0-5    destination MAC
 bytes  6-11   source MAC
 bytes 12-13   *** the only difference ***
 bytes 14+     payload
```

- **DIX / Ethernet II**: bytes 12-13 are an **EtherType** - what protocol the payload is.
  `0x0800` = IPv4, `0x0806` = ARP, `0x86DD` = IPv6.
- **IEEE 802.3**: bytes 12-13 are a **length** - how many payload bytes follow. Protocol
  identification moves into an LLC/SNAP header inside the payload.

**The discriminator is 0x0600 (1536).** Maximum legal Ethernet payload is 1500, so any value >= 1536
cannot be a length and must be a type. That is how real dual-stack hardware tells them apart,
per-frame, with no configuration.

## 3.2 When to use which

- **TCP/IP requires DIX.** Every IPv4 host on earth expects `0x0800` at bytes 12-13. You do not get
  to choose.
- **COSMOS uses 802.3.** ND's networking predates the DIX consolidation.
- **You cannot have both on one card.** See below.

## 3.3 How the ND card handles it - the critical part

**[P]** The firmware has a **global** mode word at DRAM address **0x1888A**. It has *three*
consumers, not one:

**On transmit** (`XMTRINGAPPEND`, 68K 0x6054) - and note the firmware builds the MAC header
**backwards** from your data, so you must leave headroom:

```
mode != 0  (802.3):  header starts at base + hdrlen - 14, hdrlen must be >= 14
                     firmware WRITES your totallen into bytes 12-13 as an 802.3 length
                     on-wire length = 14 + totallen

mode == 0  (DIX):    header starts at base + hdrlen - 12, hdrlen must be >= 12
                     firmware writes NOTHING at bytes 12-13
                     on-wire length = 12 + totallen
                     -> bytes 12-13 are the FIRST TWO BYTES OF YOUR DATA
                     -> so YOU put the EtherType there
```

**On receive** (`RCVCOMPLETE`, 68K 0x5C42):

```
mode != 0  (802.3):  hdrlen = 14, totallen = frame bytes 12-13
                     (the 802.3 length field is trusted verbatim as the payload length)

mode == 0  (DIX):    hdrlen = 12, totallen = framelen - 12
                     (bytes 12-13 are handed to you as ordinary payload - you parse the EtherType)
```

**Consequences you must design around:**

1. **[P] Max IP payload in DIX mode is 1498, not 1500.** The firmware's 1500 cap is applied to
   `totallen`, and in DIX mode `totallen` includes the two EtherType bytes. Set your MTU to 1498 or
   your full-size packets will be rejected with status **-22**.
2. **[P] The mode word is global with no per-request override.** The transmit path reads
   `tst.w (0x1888A)` directly rather than taking a flag from your request. There is nowhere to say
   "this frame is DIX".
3. **[E]** Therefore: clearing it for TCP/IP changes *every* frame the card sends, including COSMOS
   traffic. Run TCP/IP on a second controller if you need both.

## 3.4 A wrinkle worth knowing: four addresses per interface

**[P]** ND-60.197.01 section 2.4: the hardware **ignores bits 7-6 of the destination address**, so
*"each Ethernet Interface has in fact four physical addresses"*:

| bits 7-6 | meaning |
|---|---|
| 00 | IEEE |
| **01** | **DIX** |
| 10 | user-written protocol |
| **11** | **ND (COSMOS)** |

The addressing scheme was designed to let DIX and COSMOS traffic arrive at one interface under
different addresses. **[U]** Whether that helps given the global frame-format mode word is untested -
the mode word governs framing, the address bits govern filtering, and they are independent
mechanisms. Worth an experiment before assuming two cards are mandatory.

---

# Part 4 - The card interface: how to actually move a frame

This is the concrete API your driver targets. All **[P]**, decoded from the firmware and verified by
sending and receiving real frames.

## 4.0 WHOSE address space? Read this before anything else

**Every address in this Part is a 68000 address inside the card's own 512 KB DRAM.** They were
decoded from, and verified against, the ENCOS firmware running on the card's 68000. `POSI_SEND` is a
subroutine of that firmware. **An ND-100 program cannot call it.**

Two facts make this usable from the host anyway:

- **[P]** The card's DRAM is mapped into ND-100 physical memory. The window is selected by the 7J/9J
  straps: bank 16 for the first card, +4 per further card, 512 KB each. So the ND-100 can read and
  write every structure above directly - the node, the descriptor, the mode word, the statistics.
- **[P]** There are two doorbells. Card to host: the 68000 writes `0xEF0080`, which raises ND-100
  level 12 with the card's ident. Host to card: the ND-100 sets the channel flag word at card DRAM
  `0x0B56` and strobes MFP GPIP6, which vectors the 68000 through `0x4E`.

**[U] What is NOT decoded is the host-side frame protocol** - which structure in that window the
ND-100 is supposed to fill in, and which flag it strobes, so that the firmware's own scheduler picks
the request up and routes it to the data queue. In our tests we reached the transmit path by calling
`POSI_SEND` directly from the emulator, which is a debugging shortcut, not something a real ND-100
program can do. Anyone building this must decode that host protocol first. It is the single largest
piece of missing work in this document.

> **UPDATE 2026-07-30 - DECODED.** The recovered 211185 B05 image (section 1.5) contains the
> **consumer** side of this protocol, and disassembly has now settled the container and the command
> set. The seam is **PIOCOS ports carrying request blocks (RBs)**, not a raw shared-memory node poked
> by the ND-100.
>
> **[V] Port object**: `+0x04` magic `0xAAAA` (checked on every operation), `+0x06` test-and-set
> spinlock, `+0x0A` type (2 = ring), `+0x12` write index, `+0x14` read index, `+0x16` a 64-longword
> slot ring indexed `& 0x3F`, with **bit 31 of each slot as the occupied flag**.
>
> **[V] Message header**: `+0x00` free-list link, `+0x08` reply-to port, `+0x0C` home port, `+0x14`
> capacity, `+0x18` length. The RB hangs off the message at **`+0x10`**.
>
> **[V] RB header**: `+0x00` type in bits 15..10, `+0x02` signed status, `+0x08` argument area. The
> reply is written back into the **same RB in place**.
>
> **[V] Request types are EVEN; the response type is request + 1.** Proven mechanically across all
> 54 reply stamps in the image - every handler's stamp equals its dispatch-table index plus one,
> which is also why all 13 odd table slots point at a single reject stub.
>
> **[V] `PORTSEND` destination selector**: `0` = the message's home port, `-1` = its reply port,
> otherwise an explicit port address. That is the request/response idiom.
>
> **[V] Errors** `0x49xx`: `4953` bad port, `4956` no buffer, `495C` ring full, `495D` length exceeds
> capacity.
>
> **[V] The media-access dispatch table is at `0x24A86`** with its bound at `0x24A84` (`0x1A`), and
> implements **11 commands, not the 8 named in the strings** - operations 22, 24 and 26 exist beyond
> the eight AIP names.
>
> **[V] `set DIX mode` is request type 12** (`RB[0] = 0x30`), handler at `0x7096`. Identified
> independently of string ordering: the handler tests the vendor-named `ACTIVEDIXU` and `ACTIVEMAUS`.
> Its argument is **6 bytes at `RB+0x08`**; `AIPINIT` fills them with `FF FF FF FF FF FF` and the
> handler validates **only bit 0 of byte 0** as the enable flag. On success it calls `STOPMA` then
> `STARTMA` - the "MEDIA-ACCESS RESTARTING" path. **[U]** what the other five bytes mean.
>
> Full detail in [TCPIP-B05-FIRMWARE-RE-2026-07-30.md](TCPIP-B05-FIRMWARE-RE-2026-07-30.md).

**[V-doc] Also know what Norsk Data themselves did**, because it is the opposite of the architecture
in Part 5: ND's TCP/IP product (211185, "AIP") ran the **IP layer on the card**, on PIOCOS, and the
ND-100 host talked to it over PIOC ports and XMSG - not raw frames. Evidence: ND's own error text
`AIPpiocError : PIOCOS error` in the TELNET/FTP guide ND-860284-1. If a host-side raw-frame path
turns out not to exist, that is why, and the XMSG route becomes the practical one.

## 4.1 Addresses you need (68000 side)

```
0x1885E   LNMAPHYSIC     station MAC (6 bytes) - written by host command 0
0x18886   operating mode 4 = NORMAL (pads short frames to 60)
0x18888   filtering       NONZERO = filtering ENABLED (note the polarity)
0x1888A   frame mode      0 = DIX, nonzero = 802.3
0x188C6   data path up    zero -> transmit returns -16
0x188C8   expected id     your request node must carry this at +0x04
0x18848   data queue object   (POSI_SEND target for transmit)
0x18834   command queue object
```

## 4.2 The transmit request

```
Request node:
  +0x00  long   next link (0)
  +0x04  word   id            - must equal (0x188C8), else status -17
  +0x0A  byte   subfunction << 2 ; 0x40 = TRANSMIT (subfunction 16)
  +0x14  long   version       - must be 1, else -21
  +0x18  ...    transmit descriptor (below)
  +0x22  6 by   DESTINATION MAC

Transmit descriptor (at node+0x18):
  +0x00  long   buffer base address
  +0x06  word   hdrlen    - offset into the buffer where YOUR data starts
  +0x08  word   totallen  - bytes from base+hdrlen onward; <= 1500 else -22
```

**Status codes** (worth handling explicitly, they are precise):

```
 -8, -10  data path not ready in various ways
 -16      (0x188C6) == 0, data path not up
 -17      id mismatch
 -21      version != 1
 -22      totallen > 1500
 -23      hdrlen too small for the current mode (< 14 in 802.3, < 12 in DIX)
```

## 4.3 Buffer layout for a DIX/IPv4 frame

With `hdrlen = 12`, laid out in your buffer:

```
  base+0   .. base+11   : 12 bytes of HEADROOM - firmware writes dst+src MAC here
  base+12  .. base+13   : EtherType 0x08 0x00      <- YOU write this
  base+14  ..           : your IP header, then TCP/UDP, then data
  totallen = 2 + (length of IP packet)
```

The source MAC is stamped by the firmware from `LNMAPHYSIC` - **[P]** copied verbatim, no bits
forced. You supply the destination in the node at +0x22.

## 4.4 The receive delivery

**[P]** The received frame is **not copied and the MAC header is not stripped**. The delivery
descriptor's `base` points at the LANCE buffer, i.e. at byte 0 = the destination MAC.

**Trap**: `descriptor.len` (node+0x1C) is the **buffer** size, not the frame length. The frame length
is in `totallen`. Getting this wrong gives you garbage tails.

Convenience: **[P]** the destination MAC is duplicated to node+0x22 and the source MAC to node+0x28,
so you can filter without re-parsing the frame.

**[P] Every rejection is a silent drop.** Size gate (< 60 or > 1514), filter mismatch, and all LANCE
errors (CRC, framing, buffer, overflow) clear the descriptor, advance the ring, bump a counter, and
**never notify the host** - no interrupt, no status. If your stack sees nothing arriving, poll the
statistics block at 0x1888C; it is the only evidence you get.

## 4.5 Posting a receive buffer

**[P]** Subfunction **18** = POST-RX-BUFFER. A version-1 buffer node must declare
`descriptor+0x04 == 0x5F0` (**1520 bytes**), else it is rejected with -22. 1520 comfortably exceeds a
full 1514-byte frame, so buffer size is not a constraint.

**Design note**: **[P]** RX pool exhaustion is a *silent* drop. Keep the pool topped up
aggressively - post a replacement buffer the moment you take one, not at the end of your processing.

---

# Part 5 - Architecture: the RT driver and the stack

**[E]/[I]** This section is design guidance. The card interface above is proven; how you organise a
SINTRAN RT program around it is engineering judgement, and I flag where I am reasoning rather than
reporting.

## 5.1 Overall shape

```
   +--------------------------------------------------------------+
   |  Application RT programs / reentrant background programs      |
   |     telnetd    ftpd    httpd    telnet    ftp                 |
   +--------------------------------------------------------------+
                 |  socket-like calls (your own API)
   +--------------------------------------------------------------+
   |  IPSTACK segment                                              |
   |     TCP state machines, UDP demux, socket table               |
   |     IP input/output, fragmentation, routing (one default GW)  |
   |     ICMP echo/unreachable                                     |
   |     ARP cache + request/reply                                 |
   +--------------------------------------------------------------+
                 |  frame in / frame out
   +--------------------------------------------------------------+
   |  ETHDRV - the RT driver                                       |
   |     interrupt handler on level 12 (ident 140034B)             |
   |     RX pool management, TX request nodes                      |
   +--------------------------------------------------------------+
```

## 5.2 Why the driver should be its own RT program

**[E]** Three reasons:

1. **Interrupt latency.** The level-12 handler must be short. It should do nothing but move a
   completed frame onto a queue and wake the stack.
2. **Buffer discipline.** RX pool exhaustion is silent; one component should own the pool and nothing
   else should be able to starve it.
3. **Restartability.** A stack bug should not require re-downloading the card firmware.

## 5.3 Suggested driver structure

**[I]**

```
ETHDRV:
    initialise:
        set station MAC via command 0
        clear the mode word 0x1888A          (DIX)
        register broadcast FF:FF:FF:FF:FF:FF via ADD-GROUP-ADDRESS (command 12)
             -- [P] there is NO hardcoded broadcast; unregistered broadcast is dropped
        post N receive buffers (subfunction 18, 1520 bytes each)

    on level-12 interrupt:
        identify the completion
        move the delivered frame descriptor onto RXQ
        POST A REPLACEMENT RECEIVE BUFFER IMMEDIATELY
        signal the stack RT program
        return

    transmit(frame, len, dstmac):
        take a free TX node
        fill node: id from (0x188C8), version 1, subfn 0x40, dst MAC at +0x22
        fill descriptor: base, hdrlen 12, totallen = len
        POSI_SEND(queue = 0x18848, node)
        on error status, map to your own errno and count it
```

## 5.4 Buffers

**[E]** Fixed-size buffers, single size, preallocated at load time. 1520 bytes is forced on you by
the RX path anyway; use the same size for TX so one pool serves both. No dynamic allocation - on a
machine of this era the failure mode of a fragmented heap is far worse than a slightly wasteful pool.

Suggested: **[I]** a free list threaded through the buffers themselves, one word at a fixed offset,
and a high-water counter you can inspect. If the free list ever empties you want to know, because the
card will not tell you.

## 5.5 What the stack must implement, in build order

**[E]** Ordered so each step is testable before the next exists:

1. **ARP** - you cannot send a single IP packet without it. Cache with timeout; queue one pending
   packet per unresolved address; broadcast requests; answer requests for your own address.
2. **IP input/output** - header validation, checksum, protocol demux. Routing can be one default
   gateway plus a local subnet mask; do not build a routing table you do not need.
3. **ICMP echo** - the moment `ping` works you have proven ARP, IP, checksums and the whole driver
   path. This is the single most valuable milestone.
4. **UDP** - trivial once IP works, and gives you a testbed with no state machine.
5. **TCP** - the large one. See below.

## 5.6 TCP specifically

**[E]** The parts that actually cost effort, in rough order of pain:

- **The state machine** - 11 states. Write it as an explicit table, not nested conditionals.
- **Retransmission** - a timer per connection, exponential backoff, and Karn's algorithm for RTT
  sampling. You need a periodic tick; **[U]** which SINTRAN timer facility to use for this is not
  something I have verified - check the monitor calls manual for the RT timing calls.
- **Sequence arithmetic** - all comparisons must be modulo 2^32. On a 16-bit machine this means
  32-bit arithmetic done carefully and consistently in one place.
- **Window management** - start with a fixed receive window equal to your buffer allocation and no
  window scaling. Correctness first.
- **The checksum** - one's-complement over a pseudo-header. Write it once, test it against known
  vectors, never touch it again.

**Simplifications that are legitimate for a first version**: no urgent data, no options beyond MSS,
delayed-ACK off, one segment in flight per connection if you want to defer congestion control
entirely. It will be slow and it will be correct. Make it correct first.

---

# Part 6 - The server model: telnetd, ftpd, httpd

## 6.1 The core problem

Multiple clients, one program. On a modern system you fork or thread. **[E]** On SINTRAN the natural
equivalents are:

- **One RT program with an explicit connection table** - a state machine per connection, no
  per-client process. This is the classic embedded approach and it fits SINTRAN well.
- **A reentrant background program per client** - SINTRAN's `DUMP-PROGRAM-REENTRANT` mechanism
  genuinely supports multiple simultaneous users of one program image.

**[E]** Recommendation: **connection table in one RT program** for servers. Reasons: RT descriptions
are a finite resource (ND program descriptions all state Max/Min counts), connection state is small,
and you avoid needing an accept-and-hand-off mechanism that SINTRAN does not obviously provide.

## 6.2 Server skeleton

**[I]**

```
SERVER (one RT program, one listening port):

  connection table entry:
      state           FREE / LISTENING / ESTABLISHED / CLOSING
      tcp_handle      your stack's connection id
      client_state    protocol-specific (see below)
      inbuf, outbuf   per-connection buffers
      timer           idle timeout

  main loop:
      wait for stack event
      case event of
          NEW CONNECTION:
              find FREE slot; if none -> refuse (RST) and count it
              initialise slot, send greeting
          DATA ARRIVED on slot N:
              feed bytes to the per-protocol state machine for slot N
          CLOSED / RESET on slot N:
              release slot
          TIMER TICK:
              scan slots for idle timeout
```

**[E]** The thing that makes this tractable: **never block**. Every handler takes bytes, updates
state, maybe queues output, and returns. No handler ever waits for more input.

## 6.3 telnetd

Simplest of the three. **[E]**

- Listen on TCP port 23.
- Telnet is a byte stream plus in-band option negotiation using **IAC (0xFF)** sequences. You must at
  minimum parse and refuse options you do not implement (`IAC DONT x` / `IAC WONT x`), otherwise
  clients hang waiting.
- Negotiate at least: SUPPRESS-GO-AHEAD, ECHO. Decide who echoes and be consistent.
- **The interesting ND-specific part**: connecting the TCP stream to a SINTRAN terminal session.
  **[U]** How to attach a stream to a SINTRAN login/terminal programmatically is not something I have
  verified. Relevant reading: the TAD mechanism (COSMOS uses it for exactly this - remote terminal
  access), and `MON 70B` which runs a SINTRAN command from a program. **Check before designing.**

## 6.4 ftpd

**[E]** Two connections per session, which is the whole complication:

- **Control connection** on port 21 - line-oriented commands, 3-digit reply codes.
- **Data connection** - a *second* TCP connection per transfer, either active (server connects out
  from port 20) or passive (server listens, tells client where).

Design consequence: your connection table needs to link a data connection back to its control
connection. Implement **PASV** if you only implement one mode - active mode requires the server to
initiate outbound connections, which is more machinery and is blocked by anything NAT-like.

Minimum command set for interoperability: `USER PASS TYPE PASV LIST RETR STOR QUIT`. Map `TYPE A`
(ASCII) to line-ending conversion - **[P]** SINTRAN text files end lines with **CR only, no LF**, so
ASCII mode must translate in both directions or every transferred file will look wrong.

Also **[P]**: SINTRAN MODE files are stored with **bit 7 set on every character**. If you serve those
without stripping the high bit, clients see garbage.

## 6.5 httpd

**[E]** Easiest to make work, hardest to make fast.

- Request line, headers until a blank line, optional body. Parse incrementally - do not assume the
  whole request arrives in one segment.
- HTTP/1.0 with `Connection: close` is a legitimate starting point and removes all keep-alive state.
- Serve from the SINTRAN file system; map URL paths to `(USER)NAME:TYPE`. Be careful about path
  traversal.
- **HTTPS: [E] do not attempt this.** TLS needs modern crypto (RSA/ECDHE, AES, SHA-2) and a
  certificate chain. On an ND-100 the handshake alone would take an unreasonable time, and a
  correct-and-secure TLS implementation is a much larger project than the entire TCP stack. If you
  need TLS, terminate it on a modern box and forward plaintext over the LAN.

## 6.6 Client programs: telnet, ftp

**[E]** Clients are easier than servers - one connection, and the user drives.

- **Reentrant background programs** are the right vehicle: `DUMP-PROGRAM-REENTRANT` so several users
  can run `telnet` at once. **[P]** This is a documented, working SINTRAN mechanism (it is how
  COSMOS's `CONNECT-TO` and `TRANSFER-FILE` are shipped).
- The client needs to multiplex terminal input against network input. **[U]** The SINTRAN idiom for
  waiting on two sources is not something I have verified - check the monitor calls manual for
  no-wait/asynchronous input variants before designing around a polling loop.

---

# Part 7 - Which language, with examples

## 7.1 The three candidates

| Language | What it is | Use it for |
|---|---|---|
| **MAC** | ND-100 assembler | Interrupt handlers, tight loops, anything touching hardware directly |
| **NPL** | NORD Programming Language - ND's systems language, used for SINTRAN itself | Kernel-adjacent code, drivers |
| **PLANC** | ND's structured systems language, ALGOL-ish, with records and pointers | **Most of the stack** |

**[E] Recommendation: PLANC for the stack, MAC only where you must.** PLANC has records, typed
pointers and enumerations, which is what protocol code needs. The ENCOS firmware on the card is
itself written in PLANC, which tells you ND considered it appropriate for exactly this kind of work.

## 7.2 PLANC - the one verbatim sample we have

**[P]** From ND-60.197.01, the record the Ethernet Basic Software uses for attach/detach. This is
real PLANC from the manual, not reconstructed:

```
TYPE ENUMDE = RECORD
     ENUMRGaddress : ENUMDEaddress
     INTEGER2      : ENUMDEpioc
     BYTES POINTER : ENUMDEsystem
     INTEGER2      : ENUMDEserver
```

with the manual's own field descriptions:

> `attach.ENUMDEpioc` - Ethernet interface number to use. Given by thumbwheel setting on the Ethernet
> interface.
> `attach.ENUMDEaddress` - Physical user address (0-3) in a specified Ethernet interface.

Note what that tells you about ND's own design: the **interface number** and the **physical address**
are separate fields. Your stack will want the same separation.

## 7.3 PLANC sketches

**[I] - syntax not verified against a PLANC manual. Treat as design, not as code to type in.**

An IP header as a record:

```
TYPE IPHDR = RECORD
     INTEGER1 : ip_verlen        % version<<4 | header length in words
     INTEGER1 : ip_tos
     INTEGER2 : ip_totallen
     INTEGER2 : ip_id
     INTEGER2 : ip_fragoff       % flags in top 3 bits
     INTEGER1 : ip_ttl
     INTEGER1 : ip_proto         % 1=ICMP 6=TCP 17=UDP
     INTEGER2 : ip_checksum
     INTEGER4 : ip_src
     INTEGER4 : ip_dst
ENDRECORD
```

A connection-table entry:

```
TYPE TCB = RECORD
     INTEGER2 : tcb_state        % see the state enumeration
     INTEGER4 : tcb_localip
     INTEGER2 : tcb_localport
     INTEGER4 : tcb_remoteip
     INTEGER2 : tcb_remoteport
     INTEGER4 : tcb_snd_una      % oldest unacknowledged
     INTEGER4 : tcb_snd_nxt      % next to send
     INTEGER4 : tcb_rcv_nxt      % next expected
     INTEGER2 : tcb_snd_wnd
     INTEGER2 : tcb_rcv_wnd
     INTEGER2 : tcb_rto          % retransmit timeout, ticks
     INTEGER2 : tcb_timer
ENDRECORD
```

The transmit request node, matching the layout in Part 4 exactly:

```
TYPE TXNODE = RECORD
     INTEGER4 : tx_link          % +0x00
     INTEGER2 : tx_id            % +0x04  must equal (0x188C8)
     INTEGER2 : tx_pad1          % +0x06
     INTEGER2 : tx_pad2          % +0x08
     INTEGER1 : tx_subfn         % +0x0A  0x40 = transmit
     ...
     INTEGER4 : tx_version       % +0x14  must be 1
     INTEGER4 : tx_bufbase       % +0x18  descriptor +0x00
     INTEGER2 : tx_pad3          % +0x1C
     INTEGER2 : tx_hdrlen        % +0x1E  descriptor +0x06
     INTEGER2 : tx_totallen      % +0x20  descriptor +0x08
     BYTES    : tx_dstmac(0:5)   % +0x22
ENDRECORD
```

**Verify the offsets your compiler actually produces.** The layout above is proven for the firmware;
whether your PLANC declaration lands on those offsets depends on the compiler's packing rules, and
that is **[U]**. Check with a test that writes a known pattern and dumps the bytes.

## 7.4 The one's-complement checksum

Needed by IP, ICMP, TCP and UDP. Get it right once. **[I]**:

```
FUNCTION cksum(BYTES POINTER : p; INTEGER2 : len) : INTEGER2
    INTEGER4 : sum := 0
    WHILE len > 1 DO
        sum := sum + (p(0)<<8 OR p(1))      % big-endian 16-bit word
        p := ADDR(p(2)); len := len - 2
    ENDWHILE
    IF len = 1 THEN sum := sum + (p(0)<<8) ENDIF
    WHILE (sum >> 16) <> 0 DO
        sum := (sum AND 0FFFFH) + (sum >> 16)
    ENDWHILE
    RETURN NOT sum AND 0FFFFH
ENDFUNCTION
```

**[P]** Two facts that make this easier on this machine than on a PC:

- The ND-100 is **big-endian**, and so is network byte order. **No byte swapping anywhere.** This
  removes an entire category of bug.
- Verification folds to `0xFFFF`: summing a header that already contains its checksum gives all-ones
  if it is correct.

## 7.5 MAC assembler - where it is genuinely needed

**[E]** Keep it to:

- The level-12 interrupt handler entry and exit.
- Any IOX sequence talking to the controller registers.
- The 32-bit sequence-number comparisons if PLANC's arithmetic makes them awkward.

Everything else in PLANC. Assembler in a protocol stack is where the bugs live.

---

# Part 8 - Bring-up order, and how to test each step

**[E]** Each step is independently verifiable. Do not skip ahead.

| Step | You have succeeded when |
|---|---|
| 1. Driver init | The card reports STARTED and `LNMAPHYSIC` reads back the MAC you wrote |
| 2. Transmit one frame | A packet capture on another machine shows your frame with EtherType 0x0800 |
| 3. Receive one frame | You can see a broadcast ARP from another host arriving |
| 4. ARP | Another host's `arp -a` shows your machine after you answer its request |
| 5. ICMP echo | **`ping` works from a modern machine.** The big milestone |
| 6. UDP | A trivial echo service responds |
| 7. TCP handshake | `telnet <ip> <port>` connects, even if it then does nothing |
| 8. TCP data | Bytes flow both ways and a capture shows correct sequence numbers |
| 9. telnetd | You get a login prompt from a modern terminal |

**Debugging aids that already exist: [P]** the statistics block at **0x1888C** - 23 saturating
counters including "bad MA length field" at 0x188B0, RX pool exhaustion at 0x188A0, and the CRC /
framing / buffer / overflow counters. When frames vanish silently, these are your only evidence.
Read them.

---

# Part 9 - Honest list of what is NOT known

Stated plainly so nobody builds on sand:

- ~~**[U]** How to attach a TCP stream to a SINTRAN terminal session~~ **ANSWERED 2026-07-30: TAD.**
  **[V]** ND's own FTP starter RT program carries the comment *"Mode file to load FTPRT who allocates
  a TAD and starts the FTP server"* (`tcp-ip-lo-1-b05.mode`). ND attached network sessions to SINTRAN
  terminals through TAD. The exact allocation sequence is still **[U]** - read `ftprt-b05.prog`.
- **[U]** The SINTRAN idiom for waiting on terminal input and network input simultaneously.
- **[U]** Which SINTRAN timer facility suits TCP retransmission timers.
- **[U]** Whether PLANC record declarations produce the exact byte offsets the firmware requires -
  verify empirically.
- **[U]** How to add an Ethernet interface to an already-generated SINTRAN (the `8ETRn` flags are
  set at generation time; no post-hoc procedure is documented).
- **[U]** Whether the four physical addresses per interface (bits 7-6) allow DIX and COSMOS to
  coexist on one card despite the global framing mode word. Untested, and would be valuable.
- **[U]** Where the completion status lands in a transmit node. `node+0x26` is **not** it - that
  offset holds the destination MAC bytes.
- **[U] The host-to-card frame protocol** - which structure the ND-100 fills and which flag it
  strobes so the firmware picks the request up. See Part 4.0. This is the largest gap in the
  document, and it may not have a raw-frame answer at all: **[V]** no shipped ND product ever passed
  a raw MAC frame to the ND-100 (section 1.4).
- **[U]** Whether an Ethernet II TCP/IP on-card image file even has a documented name. **[V]** No ND
  document present names one, and no media survives here (section 1.5).
- **[U]** Whether 211185's ARPA data link offered LLC/SNAP as well as plain DIX. ND-12.055.1's ARPA
  layer-2 cell reads "LLC1 / DIX"; the 211185 sheet says DIX 2.0 (section 1.4).

---

## Related documents

- `HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md` - what ND's own sheets and manuals say about
  211185 / 211327 / 211154, with every quote sourced. Read alongside Parts 1.4 to 1.6.
- `TCPIP-DRIVER-ON-ND-ETHERNET-II.md` - the full transmit/receive decode with measured values
- `MAC-ADDRESS-ASSIGNMENT-MULTI-CARD.md` - address format and the multi-card question
- `Reference-Manuals/ND-60.197.01 EN Ethernet Basic Software Programmer Guide.md`
- `Reference-Manuals/Devices/ND-12.055.1 EN Ethernet II Controller.md`
- `Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md`
