# OCTObus / OBCON - the documented protocol, and what the ACCP driver does

**Date**: 2026-07-27, **substantially updated 2026-07-28** (sections 1a, 1b, 5c - the
information byte is now decoded from documentation)
**Documented sources**:
- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-14001-1-EN DOMINO Standard Hardware Description.md`, chapter 4 "The OCTObus Adapter (OBA)" - the frame
- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md`, chapter 3 "Octobus Communication" - **the information byte encoding**

**Carved source**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`

The manual and the firmware agree, and that agreement is what makes this document usable.
Where only one of the two supports a claim, it says so.

**The part is called OBCON** - "An OCTObus Adapter (OBA) is implemented on the DIOC. Its
design is centred upon the OBCON gate array chip." That settles the NDOBCON-vs-OCTC naming
question in favour of OBCON.

---

## 1. The frame, as the software sees it

The wire frame is 32 bits (start + 30 + stop), but **software never sees that**. The driver
interface is exactly **16 bits** in each direction:

**Transmit** (written to the OCTObus output register):

```
 15   14   13........8   7..............0
| C | B  |  Dest/Type  |   Information   |
```

**Receive** (read from the OCTObus input register):

```
 15   14   13........8   7..............0
| C | B  |   Source    |   Information   |
```

- **C = 1** - the information byte is a **control byte**.
- **C = 0** - the information byte is pure data, i.e. **"kick" information**.
- **B = 1** - broadcast; the Dest field is one of six **node types**.
- **B = 0** - normal; the Dest field is one of 62 **node numbers** (0 and 63 illegal).

**This is the direct explanation of two ACCP console commands.** `SEND-OCTOBUS <Data (16)>`
writes one such word raw. `SEND-KICK-OCTOBUS <DESTINATION><Kick value (process)>` builds one
with C=0 - a kick. The 16 in "Data (16)" is the frame width, not an arbitrary field size.

---

## 1a. THE INFORMATION BYTE - DECODED [SOLVED 2026-07-28]

ND-14001 defers the meaning of the information byte four separate times, on printed page 110
(PDF page 126), to a document it calls the **"OCTObus Protocol Specification"**. That document
is not in this repository and carries no ND number in either manual's related-manuals list.

**It is no longer needed.** The encoding is documented in a different manual we already have:

**`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md`,
chapter 3 "Octobus Communication", section 3.3 "Introduction to the protocol", 3.3.1 "Message
format".**

The information byte splits into four flag bits and a 4-bit code:

```
 bit 7   bit 6   bit 5   bit 4   bits 3..0
   E       K       M       S       code
```

| C | E | K | M | S | code field | Message type |
|---|---|---|---|---|---|---|
| 1 | 1 | - | - | - | emergency code | **Emergency** - hardware only: power fail, master clear, reset |
| 1 | 0 | 1 | - | - | kick number | **Kick** |
| 1 | 0 | 0 | 0 | - | ident number | **Ident** |
| 1 | 0 | 0 | 1 | 1 | CMD number | **Start** of multibyte message |
| 1 | 0 | 0 | 1 | 0 | CMD number | **End** of multibyte message |
| 0 | - | - | - | - | data byte | Part of a multibyte message |

From the same manual, on what the three classes are for:

- **IDENT** activates a process in the destination station. Used to interrupt the ND-100 from
  the ND-5000 CPU. The destination must be prepared to receive idents from a specific station.
- **KICK** activates a process in the destination; accepted from any station. **Kick number 1
  in the ND-5000 CPU starts scanning the execution queue.**
- **MULTIBYTE** messages are routed to the specified **CMD** routine by the destination
  process. In the ND-5000 CPU, **multibyte messages with CMD number 3 are handled by the
  Access Processor (ACP)**.

CMD numbers run **0 to 15** - which is exactly the 4-bit code field. The ACCP console calls
this a "Subprocess"; the ND-5000 test program calls it a "CMD" and lists the connected ones
with `LIST-SUBPROC-TABLE`. **They are the same field.** A receiver must connect a CMD before
anything addressed to it will be delivered - which is the same requirement the ACCP driver
enforces through its registration table.

### The ND-5000 octobus driver function set

From ND-05.017.01 chapter 8, the `OCTOBUS-DRIVER` test command:

| Fn | Function |
|---|---|
| 1 | Send multibytes |
| 2 | Broadcast multibytes |
| 3 | Send kick |
| 4 | Send ident |
| 5 | Send emergency (not implemented) |
| 6 | Read transmit status |
| 7 | Connect kick |
| 8 | Connect ident |
| 9 | Connect CMD |
| 10 | Access octobus registers (not implemented) |

The send-multibyte parameters are **Destination**, **Cmd** (destination CMD, 0-15), **Own cmd**
(0-15) and the message content. The receive side (`LIST-SUBPROC-TABLE`) reports **Source** and
**Message size**. Source comes from the frame's hardware source field, so **size must travel
in-band** - and own-cmd must too, or a reply could not be addressed.

That manual also names the one remaining document, for the per-function detail:
**"Octobus Driver Programming Guide (written by DVT - 15. Oct. 1986)"**. Also not in this
repository. Between ND-14001 chapter 4 and ND-05.017.01 chapter 3 we no longer need it for
framing - only for the private meaning of individual CMD payloads.

### A field-layout conflict, resolved in favour of ND-14001

Section 3.3.1's own frame diagram renders the fields as `C(15) | B+Dest(14..09) |
Information(08..00)`, which does not add up - that is a 9-bit information field. ND-14001
Figure 30 gives `C(15) | B(14) | Dest(13..8) | Information(7..0)`, and the wire capture matches
ND-14001.

**Verified against the original scan, not just the OCR**:
`F:\NDDOC\ND\14\ND-14001-1-EN Domino Standard Hardware Description.pdf`, PDF page 125-128.
Figure 30 there reads `30..27 Priority | 26..21 Destination | 20 C | 19 B | 18..13 Source |
12..5 Information | 4..3 Parity | 2..1 Ack`, with the driver views as the 16-bit forms above.
Treat the 3.3.1 diagram as an OCR misalignment. **The sub-field decode in the table above is
unaffected** - it operates on the 8-bit information byte and is confirmed by the capture.

**PDF-to-markdown page mapping** (do not re-derive this): the markdown's `## Page N` markers
are 1:1 with **PDF pages**. The manual's own **printed** page number is **N - 16**. Confirmed
twice from the table of contents and once by reading the PDF directly.

---

## 1b. THE CAPTURED SCAN, FULLY DECODED [SOLVED 2026-07-28]

Applying section 1a to the six frames the ACCP sends per station (station 2 shown; stations
3-7 are identical with the destination changed):

| Frame | C | B | Dest | Info | Decode |
|---|---|---|---|---|---|
| `8235` | 1 | 0 | 2 | `0x35` = `0011 0101` | E=0 K=0 **M=1 S=1** code=**5** -> start of multibyte, CMD 5 |
| `0205` | 0 | 0 | 2 | `0x05` | data byte |
| `0202` | 0 | 0 | 2 | `0x02` | data byte |
| `0203` | 0 | 0 | 2 | `0x03` | data byte |
| `02NN` | 0 | 0 | 2 | `NN` | data byte |
| `8225` | 1 | 0 | 2 | `0x25` = `0010 0101` | E=0 K=0 **M=1 S=0** code=**5** -> end of multibyte, CMD 5 |

So it is **a 4-byte multibyte message addressed to CMD 5**, and the four data bytes are:

```
[own cmd = 0x05] [length = 0x02] [content byte 0 = 0x03] [content byte 1 = own station]
```

**Two independent derivations agree.** Section 4y carved the request from the driver side and
found a message descriptor yielding exactly **two** content bytes, `{0x03, own station}`. The
wire shows four data bytes. The difference is the two header bytes the driver itself prepends -
own-cmd and length - which is precisely what the `OCTOBUS-DRIVER` parameter list and the
`LIST-SUBPROC-TABLE` display require to exist.

**This supersedes the earlier reading in section 5b** that called `0x05` a "process number" and
treated `0x35`/`0x25` as inferred "open/select" and "execute/close" control bytes. Those were
correct in shape and wrong in name; the values are now decoded from documentation, not guessed.

### The one byte still unknown

**`0x03`** - the first content byte, i.e. the request itself, inside CMD 5's private
vocabulary. Nothing in ND-14001, ND-05.017.01 or the ACCP ROM says what it means. It is the
only undecoded field in the entire outgoing message.

### The reply, specified as frames

Everything below is derived from `MFCRECEIVE`'s gates (section 4y) re-expressed in wire terms
using section 1a. The responder is the MFbus device at the station being probed; the ACCP is at
its own station.

| # | C | Info | Value | Why |
|---|---|---|---|---|
| 1 | 1 | `0x35` | start of multibyte, CMD 5 | framing |
| 2 | 0 | `0x05` | own cmd, must be 5 | MFCRECEIVE byte-3 gate |
| 3 | 0 | `N` | length | lands in buffer byte 4 |
| 4 | 0 | `0x00` | **must be zero** | caller's check at 0x12D4 |
| 5.. | 0 | ... | N-1 further bytes | **content unknown** |
| last | 1 | `0x25` | end of multibyte, CMD 5 | framing |

Plus, outside the frame content:

- transmitted **from the station that was probed**, so the hardware source field satisfies
  MFCRECEIVE's `byte1 & 0x3F` test
- OBCON status must come back **`0x8300`**
- delivered within **10000** poll iterations
- **CMD 5 must be connected** on the ACCP side (registration-table entry index 5)

**Still unknown**: `N`, and every byte after the leading zero. What little we know: byte 0 is a
status/OK field (zero = good) and byte 1 at `0x001131E7` is consumed immediately afterwards.

### A checkable prediction about the CPU-model probe

Section 4y flags as **unproven** the inference that A0 in `DetectCpuModelBySignature` points at
`0x001131E6`, the reply buffer. Section 1b gives a way to test it.

The probe looks for `0x7F55` at **+0, +4 and +0x0C**, selecting model classes `0x5200` and
`0x5400`, with **`0x5800` (ND-5800) as the fall-through**. But byte 0 of that buffer is
*required to be zero*. So the word at **+0 is always `0x00xx` and can never match `0x7F55`**.

Two possibilities, and they are distinguishable:

1. A0 **is** the reply buffer - in which case the +0 branch is unreachable on this path, and the
   model can only ever be selected from +4 or +0x0C.
2. A0 is **not** the reply buffer, and the probe reads something else entirely.

This is the cheapest available test of that inference. **[PREDICTION, not yet run.]**

### The frame-to-buffer mapping - now READ, not fitted [SOLVED 2026-07-28]

Previously this was the last open item: the reply spec above was fitted to `MFCRECEIVE`'s
gates rather than read from the assembler. **It is now read**, from a *second, independent
consumer of the same buffer* - which is stronger evidence than reading the producer would
have been, because the two agree without having been made to.

`DOREC_MULTI_OCTO` is not a routine name; it is a **PLANC error-message string** at
`0x12554` (`"in DOREC_MULTI_OCTO$"`, file offset 75092), part of a 12-byte descriptor at
`0x12548` (`{pointer, 0, length}`). Its one referencing routine is **`0x9B98`**, which prints
`"$Unexpected multibyte message "` - the driver's own diagnostic for a message it cannot
route. To print it, that routine must decode the buffer, and it does:

```
9BA8  lea   (0x11635C).l,A1 ; D0=0x24 ; A0 := entry+0x06     <- registration entry index 3
9BC0  move SR,(0x18,A6) ; move #0x898,SR                     <- masks interrupts while reading
9BD4  tst.w (0x2,A0)  / 9BDC  tst.w (0x8,A0)                 <- same two flag words as MFCRECEIVE
9C1C  D1=0 : move.b (0x14,A0,D1),D0b   ; andi #0xFF ; lsl #8
9C2C  D3=1 : move.b (0x14,A0,D3),D2b   ; andi #0xFF ; add.w D0,D2 ; move.w D2,(0x16,A6)
9C3C  D4=3 : move.b (0x14,A0,D4),(0x15,A6)
9C44  D5=4 : move.b (0x14,A0,D5),(0x14,A6)
```

Byte indices **0, 1, 3, 4** off `(0x14,A0)` - **exactly** the indices `MFCRECEIVE` gates on,
in a routine written for a different purpose against a different registration entry. The
layout is therefore a property of the driver, not of one call site.

**The receive buffer, data area at `buf+0x14`:**

| Byte | Content | Evidence |
|---|---|---|
| 0 | **high half of the source station word** - always `0x00` for a legal station | written at 0x1041E from descriptor+4 |
| 1 | **low half of the source station word** = the **source station** | written at 0x10406; MFCRECEIVE 0x1570-0x157E gates it |
| 2 | **FLAGS byte** written by the driver - see below. Not payload. | cleared at 0x1042E, then bits set at 0x106CA / 0x10BAA |
| 3 | first data byte = **own-cmd**, must be `0x05` | written at 0x10446; MFCRECEIVE 0x1582 |
| 4 | second data byte = **length N** | written at 0x10464; MFCRECEIVE 0x158E |
| 5.. | remaining **N** data bytes = content | copied to `0x001131E6` (below) |

### CORRECTION [2026-07-30] - the previous version of this table was wrong

The earlier reading said byte 0 was the START frame's **information byte** (`0x35`), byte 1 its
**control/source half**, and byte 2 **unknown**. All three were wrong. They were inferred from
MFCRECEIVE's gates and from what the 0x9B98 printer *displays*, rather than read from the code
that **writes** the buffer. Measured live, a completed reply from station 2 gives

```
data area: 00 02 80 05 02 00 09 ...
           ^^^^^ = 0x0002, the source station as a 16-bit word - NOT 0x35
                 ^^ = 0x80, a flags byte - NOT unknown payload
```

Reading the writer at **0x10402-0x10464** settles it. It fills the data area from the message
descriptor, not from the frame:

- `0x10410` / `0x1041E` store the **high** byte of descriptor+4 at buffer byte 0, and
  `0x103EA` / `0x10406` store its **low** byte at byte 1. Descriptor+4 is set at **0x10AF4**
  from the already-masked 6-bit source station. So bytes 0-1 are simply **the source station
  as a 16-bit big-endian word**, which is why byte 0 is zero and byte 1 carries the station.
- There is no "information-byte-first" convention. That whole explanation was invented to
  account for a byte-1 placement that has a much duller cause: it is the low half of a word.
- The 0x9B98 printer reads bytes 0 and 1 and prints them as one word precisely *because* they
  are one word - the source station - not because it is reassembling a split frame.

**Byte 2 is a flags byte**, cleared at `0x1042E` when the header is written and then set bit by
bit:

| Bit | Meaning | Set at |
|---|---|---|
| 7 (`0x80`) | **message complete** | `bset #7` at **0x106CA**, in the close path |
| 6 (`0x40`) | **overran the buffer** - the write pointer met the end pointer | `bset #6` at **0x10BAA** |
| 5 (`0x20`) | written conditionally; **the condition is NOT carved** | `bclr`/`bset` at 0x106A4-0x106B8 |

A completed, non-overrunning message therefore reads `0x80`, which is exactly what the
emulator measures. This is pinned by
`ReceiveBuffer_HeaderIsSourceWordThenFlagsByte` in `AccpMfBusDiscoveryTests.cs`.

**Data frames contribute one byte each, not two.** Bytes 3,4,5... are the wire data bytes
`05 02 03 NN` in order - their `C=0` control halves are discarded. Only the START frame keeps
its control half, because that is where the source station lives. So the buffer is
`[2-byte start-frame record][1 unknown byte][data bytes verbatim]`.

The copy at 0x15B0-0x15E4 confirms the tail exactly. Two PLANC array descriptors are built:

```
source: origo = (0x14,A0)      lower = 5   upper = 5 + N - 1
dest:   origo = 0x001131E6     lower = 0   upper = N - 1
```

So **bytes 5 .. 5+N-1 land at `0x001131E6`**, N bytes. (Section 4y's "N-1 bytes" was an
off-by-one in the earlier reading - the descriptor limits are inclusive, so it is N.)

### Registration-table geometry, as a free result

Both routines index `g_obconProcessRegTable (0x11635C)` the same way, and the two entry
offsets pin the stride:

| Entry | Offset | Used by |
|---|---|---|
| 3 | `0x24` | `0x9B98`, the unexpected-message printer |
| 5 | `0x3C` | `MFCRECEIVE`, the MFbus discovery reply |

`0x3C - 0x24 = 0x18` across 2 entries -> **entry size `0x0C` (12 bytes)**, and `0x24/0x0C = 3`,
`0x3C/0x0C = 5`. The table is indexed by **CMD number** directly.

That **entry 3 is the one the printer watches is a documentation match**: ND-05.017.01 3.3.1
states that in the ND-5000 CPU, *multibyte messages with CMD number 3 are handled by the
Access Processor*. Entry 3 is the ACP channel; the ACCP logs anything arriving there that it
cannot route. Two unrelated sources landing on the same number is a good sign the whole CMD
model is right.

Fields within an entry, so far: `+0x02` and `+0x08` are word flags (both consumers poll
`(0x02)` and `(0x08)`; `(0x08) != 0` breaks out immediately, `(0x02)` must be 0 to proceed),
`+0x06` is the buffer pointer.

**Still unknown in the buffer:** only the **condition** behind flags bit 5. Byte 2 itself is
now accounted for.

---

## 1e. THE RECEIVE ISR TAKES ONE FRAME PER INTERRUPT [CARVED 2026-07-30]

This is the single most important implementation fact for anyone emulating the card, and it
is not stated in either manual.

**`0x0510` is the OCTObus receive ISR.** It does exactly this:

```
0x0510  movem.l {D0,D1},-(SP)
0x0514  move.w (0x00880000).l,D0w     <- reads ONE frame. No drain loop.
0x051A  ...classify D0...
0x05A6  movem.l (SP)+,{D0,D1} ; rte
```

Classification is a priority ladder on the information byte, matching section 1a:

| Test | Condition | Route |
|---|---|---|
| 0x051C | `D0 & 0x80C0 == 0x8040` | C=1, K=1 -> **kick** -> 0x05C0 |
| 0x0530 | `D0 & 0x80A0 == 0x8000` | C=1, M=0 -> **ident** -> 0x05C0 |
| 0x0542 | `D0 & 0xC0FF == 0xC0FF` | broadcast, info 0xFF -> master clear, **jmp 0x0C72** |
| 0x059C | `D0 & 0x8080 == 0x8080` | C=1, E=1 -> **emergency** -> ignored |
| else | | **multibyte** -> `jsr 0x00010832` |

**Consequence: an N-frame message needs N separate interrupts.** A ten-frame reply is not
delivered by one interrupt that drains the FIFO; it requires ten. An emulator that presents
the receive interrupt coarsely - once per instruction batch, or only when the level rises
above what is already pending - will silently lose frames, and `MFCRECEIVE` will time out
after its 10000 poll iterations with a half-assembled message.

That was a real defect in the RetroCore ACCP machine, fixed 2026-07-30: `Run()` presented
interrupts once per 1024 instructions and picked a single highest source, so while the DUART
asserted IRQ5 the octobus IRQ3 was never presented at all. The symptom was slice-size
dependent - single-stepping accepted the MFbus reply, `Run(16K)` did not.

### The per-station reassembly record - `0x0011641C + station * 0x20`

`0x10832` keeps one record per **source station**, stride **0x20**:

| Offset | Meaning |
|---|---|
| +0x02 | sticky **abandon** flag; ORed in at 0x10A88 and any data frame is discarded while non-zero |
| +0x06 | the START frame's information byte; **bit 5 is the open flag** tested at 0x109FC |
| +0x07 | the sender's own CMD, taken from the first data byte; `0xFF` while unset |
| +0x08 | ring **write pointer** into the buffer data area |
| +0x0C | message **base offset**; `0xFFFFFFFF` until the length byte is seen |
| +0x10 | ring **end pointer** |

Both pointers are wrapped by **0x1080E**, which subtracts the ring size held in the buffer
header at **buffer+0x0A**. The record is fully reset at **0x107F6-0x10804** when the message
closes.

**Beware when reading these live.** The driver clears the record on close, and
**0xF4E6** re-initialises *all 64 station records and all 16 registration entries*. Sampling
after a run therefore shows a wiped structure regardless of what happened - which is exactly
how a working receive path was misdiagnosed as "only the first content byte arrives".

---

## 1d. CONTENT BYTE `0x03` - a supported hypothesis, NOT proven [2026-07-30]

`0x03` is the last undecoded field in the outgoing discovery request. Nothing in ND-14001,
ND-05.017.01 or the ACCP ROM states what it means, and it is **hard-coded** at 0x125A
(`move.b #0x3,(0x16,A6)`), so no experiment on the emulator can vary it.

**HYPOTHESIS (unproven):** it is not a command code at all, but the ACCP's own **Access
Processor CMD number**, making the two content bytes an announcement of the form
*"reach me on CMD 3, at station N"*.

What supports it:

- ND-05.017.01 section 3.3.1 states that in the ND-5000 CPU, **multibyte messages with CMD
  number 3 are handled by the Access Processor (ACP)**. The ACCP *is* the access processor,
  so 3 is its channel number by documentation, not by guesswork.
- The ACCP has **exactly two CMDs connected**, measured live from the registration table at
  0x0011635C: **CMD 3** (buffer 0x00112C00) and **CMD 5** (buffer 0x00112D40). Every other
  entry is empty. So CMD 3 is genuinely a live receive channel on this card - a prerequisite
  for announcing it. Dumped by `Diag_WhichCmdsAreConnected`.
- The ACCP's "Unexpected multibyte message" logger at 0x9B98 watches **entry 3**, which is
  what you would expect the ACP channel to be wired to.
- The second content byte is already proven to be the sender's own station. A pair of
  *"my CMD, my station"* is a coherent message; *"opcode 3, my station"* is equally
  coherent, which is why this stays a hypothesis.

### Corroboration found 2026-07-30 - three independent sources

**1. Norsk Data's own symbol table defines 3 as the ACCP's OMD number.**
`SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB` line 6016: `OMDAC=000003`, mapped in
`ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md` as *"OMDACCP / OMDAC / 000003 / 3 / **ACCP OMD
number**"*. So the value the ACCP transmits is, by ND's own definition, its own OMD number.
That is documentation, not inference.

**2. SINTRAN has a named routine for exactly this operation.** `SUBR MFPREPARE` in
`NPL-SOURCE\NPL\MP-P2-N500.NPL` (~line 3586) builds a message to the MF controller and carries
the explicit source comment:

```
MFOMDNO =:        X.MOCTOMD              % OMD number
0       =:        X.MBROADCAST           % Not broadcast
3       =:        X.MMSGLENGTH           % Message length = 2 bytes
CMSYSPAR SHZ 10\/N100IDENT=:X.MCOMMAND   % Send OMD numer to mf-controller
5OMDNO SH 10=:X.MDP1
```

*"Send OMD numer to mf-controller"* is ND's own wording. **Announcing your own OMD number to
the MF controller is a real, named operation in this protocol family** - not something this
document invented. It also uses a 2-byte message body, like the ACCP's.

**3. The ACCP connects exactly the two CMDs the hypothesis needs, with sizes that fit their
roles.** Read from the two OBCON fn 0x49 ("connect CMD") requests at **0x0CF8** and **0x0D56**:

| CMD | Buffer | Size | Role |
|---|---|---|---|
| **3** | `0x00112C00` | `0x13F` = 319 bytes | large - the ACCP command library channel SINTRAN drives on OMD 3 |
| **5** | `0x00112D40` | `0x3F` = 63 bytes | small - the MFbus control / discovery channel |

Confirmed live by `Diag_WhichCmdsAreConnected`: every other entry is empty.

**4. There is no CMD-5 opcode vocabulary.** The payload slot `move.b #imm,(0x16,A6)` has
**exactly one site in the entire ROM** (0x125A). If `0x03` were an opcode you would expect a
family of them; the ACCP only ever sends this one value.

### Why this is still NOT proof

Two things genuinely do not line up, and are recorded rather than explained away:

- **The field positions differ.** In `MFPREPARE` the sender's own OMD goes in **DP1** - the
  *second* content field - while the *first* carries a command code (`CMSYSPAR|N100IDENT`).
  In the ACCP's message the second content byte is **proven** to be the station number read
  from 0x900001, not an OMD. The two messages are therefore not the same shape, and the
  analogy is suggestive rather than binding.
- **`MFOMD=000004`** in N500-SYMBOLS, while the ACCP uses CMD **5** for its MF channel.
  Whether the symbol `MFOMD` is the same thing as the NPL variable `MFOMDNO` is
  **UNVERIFIED**. This discrepancy is unexplained.

Above all: **nobody has yet observed an MF controller parsing that byte.** All of the evidence
above is about the sender.

What would settle it: the **Octobus Driver Programming Guide** (DVT, 15 Oct 1986), or an
MFbus-controller ROM showing what it does with the byte. Neither is in this repository -
`OctobusAccp\eprom\51200J.bin` and `51201J.bin` are only the two EPROM halves of `octo.bin`
itself, not a second card.

**Do not encode this hypothesis as fact in emulator code.** The peer in
`AccpMfBusControllerPeer.cs` treats the byte as opaque and must continue to.

---

## 1f. MOCTOMD IS THE *DESTINATION* OMD - this reframes 1d [2026-07-31]

A re-read of the NPL octobus builders in
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL` corrects the field reading
that section 1d used as its main counter-argument.

There are **four** builders, not one, and they split cleanly by destination:

| Routine | Line | `X.MOCTOMD` written with | Destination |
|---|---|---|---|
| `XRS5CPU` (Reset CPU) | 3334 | `OMDACCP` | the ACCP |
| (MF path) | 3525 | `MFOMDNO` | the MF controller |
| `MFPREPARE` | 3589 | `MFOMDNO` | the MF controller |
| `CON5IDENT` | 3623 | `OMDACCP` | the ACCP |

`X.MOCTOMD` therefore holds the **OMD number being addressed**, not the sender's own. The
sender's own OMD travels in the *payload* - `5OMDNO SH 10 =: X.MDP1` in MFPREPARE,
`5OMDNO SHZ 10 =: X.S5` in CON5IDENT.

**Consequence for 1d:** the counter-argument recorded there - "MFPREPARE puts own-OMD in DP1
while the ACCP's second byte is the station number, so the shapes differ" - was based on
reading MOCTOMD as the sender's OMD. It is not. Both ends do the same thing: **address the
peer by its OMD in the header, and announce your own identity in the payload.** The ACCP's
`[0x03][station]` is exactly that announcement, and 1d's hypothesis is stronger than it was.

**And the OMD/CMD numbers line up.** `OMDAC=000003` (N500-SYMBOLS.SYMB:6016) is the number
SINTRAN addresses the ACCP by, and the ACCP connects a receive buffer on **CMD 3** at ROM
0x0CF8. Same number, both sides, independently sourced. The acceptance gate at 0x10934 and
0x10A12 admits only CMD **0, 3 or 5** and calls the error path at 0x05C0 for anything else -
so the card really does listen on just those.

**STILL NOT PROVEN, and one discrepancy survives.** If CMD were universally the same namespace
as OMD, then the ACCP addressing the MF controller on CMD 5 would make the MF controller
OMD 5 - but the symbol table says `MFOMD=000004` (line 5497). Either CMD is a per-station
connection number that merely coincides with OMD for the ACCP, or `MFOMD` is not what
`MFOMDNO` resolves to. **`MFOMD` vs `MFOMDNO` remains UNVERIFIED** - `MBSEND` is not in the
NPL sources present here, so the header-to-wire mapping cannot be carved from them either.

Full OMD namespace found in N500-SYMBOLS.SYMB:

```
OMDAC=000003     ACCP
OMDSO=000003
MFOMD=000004     MF controller
OMDTA=177767     (-9)
OMDEN=177766     (-10)
OMDNO=177777     (-1, "none")
```

Also worth noting from `XRS5CPU`: SINTRAN reaches this path only for
`CPUAVAILABLE /\ 5CPUTYPE = SAMSON`, i.e. it is the Samson-specific CPU-reset route, and
`CON5IDENT`'s own header comment is *"To be able to receive multi byte messages/kicks from the
ACCP/Samson"*. Both name our card explicitly.

---

## 1g. FLAGS-BYTE BIT 5 = BROADCAST - CARVED [2026-07-31]

The last uncarved bit of the driver flags byte (section 1e) is now settled. It is the START
frame's **broadcast** bit, latched through the per-station record.

Set/clear on completion, at 0x106A4-0x106B8:

```
0001068e  move.w (A2),D5w              ; A2 = per-station record
00010690  move.w D5w,(0x50,A6)
000106a4  lea    (0x14,A1,D0*0x1),A0   ; A1 = buffer base, 0x14 = data area
000106a8  move.b (A0),D1b
000106aa  bclr.l #0x5,D1
000106ae  tst.w  (0x50,A6)
000106b2  beq.b  0x000106b8
000106b4  bset.l #0x5,D1
000106b8  move.b D1b,(A0)
```

Record +0x00 is written once, by the START handler at 0x10986-0x1099A, straight from frame
bit 14:

```
00010986  move.b (A0),D1b              ; A0 = frame; byte 0 is the HIGH byte
0001098c  moveq  #0x0,D2
0001098e  btst.l #0x6,D1               ; bit 6 of high byte = frame bit 14 = B
00010992  beq.b  0x00010996
00010994  moveq  #0x1,D2
00010996  movea.l (0x50,A6),A1
0001099a  move.w D2w,(A1)              ; record+0x00 := broadcast
```

Frame bit 14 is the B (broadcast) bit of the OBCON frame. **Corroborated independently on the
SINTRAN side**: every builder in 1f writes a dedicated `X.MBROADCAST` header field, set to
`0` with the comment "Not broadcast". Both ends model broadcast as a per-message property.

### Per-station record layout, refined

The START handler at 0x10996-0x109BA initialises the whole record, which pins several offsets
that were previously only inferred:

| Offset | Written at | Meaning |
|---|---|---|
| +0x00 | 0x1099A | broadcast flag (word), from frame bit 14 |
| +0x02 | 0x109B4 | cleared (abandon) |
| +0x04 | 0x109B8 | cleared |
| +0x06 | 0x1099C | information byte of the START frame; low nibble = CMD |
| +0x07 | 0x109AE | own CMD, initialised to **0xFF**, not 0 - so 0 is a legal value and 0xFF means "not yet received" |
| +0x08 | 0x109A2 | ring write pointer, `clr.l` |
| +0x0C | 0x109A6 | set to -1 |

Record base is `0x0011641C + station * 0x20`; the scale is carved from
`moveq #0x3f,D0 / move.b (A2),D1b / and.b D0b,D1b / asl.l #0x5,D1` at 0x10902-0x10914.

**Bonus**: a debug flag at `0x001143B4` gates a trace that prints the raw frame word followed
by the string " to ACCP" (descriptor at 0x123EA). Setting it makes the card narrate its own
octobus receive path.

**Locked by tests**: `ObconMessageTests.ReceiveFlags_Bit5IsTheBroadcastBitOfTheStartFrame` and
`ObconMessageTests.StationRecord_LayoutMatchesTheStartHandler` in
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Chips.NorskData\tests\ObconMessageTests.cs`.

---

## 1h. THE CMD-3 COMMAND CHANNEL - the card answers, and 0x3E converts the model [2026-07-31]

Everything before this drove CMD 5 (MFbus discovery). CMD 3 - the 319-byte buffer connected at
ROM 0x0CF8 - is the SINTRAN-to-ACCP **command** channel, and it round-trips.

### The card narrates it itself

The ROM has a console command for this, in the command table at 0x13357:

```
TRACE-COMMUNICATION-DATA <Trace Octobus communication to consol? (y/n)>
```

(the firmware's own spelling of "consol"). YES writes 1 to `0x001143B4` at 0x9D84, NO clears it
at 0x9D8E. Eleven sites read the flag, covering both directions - `Areceive` at 0x1095A / 0x10A3A
inbound, `OctoTxTracePrint_fromACCP_A` / `_B` at 0x11078 / 0x110C0 outbound.

**This is the card describing itself**, not our instrumentation describing what we chose to model.
It should be the first move on any future protocol question.

Observed, sending content `03 02` on CMD 3 with the tracer armed:

```
8233 0203 0202 0203 0202 8223   to ACCP
    Undefined ACCP command received:
    Octal: 003B   Hexadecimal: 03H
8233 0203 0204 02FF 0206 0210 0211 8223   from ACCP
```

### Content byte 0 on CMD 3 is an ACCP COMMAND NUMBER

The card echoed our `0x03` back in the complaint as `03H`. The dispatcher is a compare chain
(PLANC CASE); its default arm is at 0x6746 and emits `0xFF 0x06`.

**This does NOT settle the section-1d question.** That one is about an OUTBOUND message on
CMD 5; this is an INBOUND message on CMD 3. Different channel, different direction. The two need
not share a content layout, and nothing here shows they do. Do not let "byte 0 is a command on
CMD 3" quietly become "byte 0 is a command on CMD 5".

What it does show is that `0x03` is **not** a defined ACCP command - so if the discovery request's
`0x03` were an opcode in this same namespace, it would be an undefined one.

### Reply shape

`[status][code][...]`. Status `0xFF` is the failure marker on every arm carved so far:

| Condition | Emits | Carved at |
|---|---|---|
| undefined command | `FF 06` | 0x6746 |
| model never confirmed (0x1131FA = 0) | `FF 0B` | 0x66E2 -> 0x6714 |
| CPU class is 0 | `FF 31` | 0x66DA -> 0x672C |
| success (command 0x3E) | `00 <packed>` | 0x66E4 -> 0x6712 |

UNVERIFIED: the trailing `10 11` seen after `FF 06` in the observed reply. Not carved; probably
from the tail builder at 0x6A64, but that is a guess and is not asserted anywhere.

### Command 0x3E is the CPU-model converter

Carved at 0x66BA-0x6712:

```
000066ee  move.b (0x001131f6).l,D0b   ; CPU class - a BYTE
000066f8  lsl.w  #0x4,D0w             ; class << 4
000066fa  move.w (0x001131f8).l,D1w   ; identity word, e.g. 0x5900
00006700  asr.w  #0x8,D1w             ; -> 0x59
00006702  moveq  #0xf,D2
00006704  and.w  D2w,D1w              ; -> 0x09, bare model digit
00006706  or.w   D0w,D1w              ; (class << 4) | digit
```

**This reconciles the two CPU-model encodings** that ANSWER-CPU-MODEL-ENCODINGS-2026-07-30.md
warned must never be plumbed into each other:

- the MFbus discovery reply carries the **bare digit** (`0x09`), widened to an identity word by
  `0x5000 | (digit << 8)` at 0x12EC-0x12FA;
- SINTRAN's WRSYSINFO byte is **packed** - bits 0-3 model digit, bits 4-5 CPU type.

They are not rival conventions needing a decision. **Command 0x3E is the converter, and it lives
on the card.** The ACCP holds class and digit separately and packs them only when SINTRAN asks.

Class 3 + digit 9 packs to `0x39`, which is also ASCII `'9'` - the documented collision. It is
not text.

Note also: **the class at 0x001131F6 is a BYTE** (`move.b`). Reading it as a word yields `0x0300`
for class 3.

### Locked by tests

In `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpOctobusTraceTests.cs`:

- `TraceCommand_SetsTheFlagTheOctobusPathReads` - the console command reaches 0x001143B4 both ways.
- `Cmd3_IsAnAccpCommandChannelAndTheCardAnswers` - inbound CMD 3 is parsed as a command and answered.
- `Cmd3_Command3EReportsThePackedCpuModelByte` - 0x3E is defined, and the packed byte is computed
  the way the firmware computes it.
- `Diag_TraceOutputWhileTheBusIsBusy` - diagnostic, prints the narrated trace.

The peer gained `AccpMfBusControllerPeer.SendMessage(destinationCmd, ownCmd, content)` for
unsolicited traffic; before this it could only answer the discovery scan.

---

## 1i. THE COMPLETE ACCP COMMAND SET - 46 commands, enumerated and verified [2026-07-31]

The CMD-3 dispatcher (section 1h) is a PLANC CASE compiled to a chain of
`cmpi.b #imm,D0` + `bne`, each arm's `bne` pointing at the next.

**Enumerated by walking the chain**: head at **0x4D50**, following every branch target,
terminating at the default ("Undefined ACCP command received") arm at **0x6746**. 46 arms.

### Defined commands - three contiguous runs

```
0x0D - 0x18    (12)
0x1B - 0x2D    (19)
0x30 - 0x3E    (15)
               ---
                46
```

### Undefined in 0x00-0x3F (18)

```
00 01 02 03 04 05 06 07 08 09 0A 0B 0C   19 1A   2E 2F   3F
```

**`0x03` is not a defined ACCP command.** That is the one hard fact this enumeration
contributes to the section-1d question: if the discovery request's `0x03` were an opcode in the
CMD-3 namespace, it would have an arm here. It does not. (It remains true that CMD 5 outbound and
CMD 3 inbound need not share a content layout - see 1h.)

### Verified against the running card

`Cmd3_EveryUndefinedCommandIsRefusedIncluding0x03` sends all 18 undefined numbers to a booted
machine on CMD 3 and requires the card to print "Undefined ACCP command received" for every one.
It does. Only the undefined numbers are swept: the defined ones include microprogram start/stop
and control-store loads, so sweeping those would wedge the card partway through and make every
later result meaningless.

### Two methodology mistakes worth not repeating

Both were made here first, and both were caught by the running card rather than by reading:

1. **A byte-pattern scan over a guessed address window missed an arm.** Scanning for
   `0C 00 00 imm` from 0x4E00 upward found 46 arms but missed `0x13` at **0x4D50** - the chain
   starts lower than assumed. The sweep test caught it: `0x13` printed *nothing at all* where a
   refusal was expected, because it is a real, silently-handled command.

2. **The "cross-check" that appeared to confirm the scan was worthless.** The scan's 46 hits were
   matched against 46 cross-references to the dispatcher's common exit at 0x6878 and the agreement
   was taken as proof of completeness. It proves nothing: arms reach the exit *indirectly* (the
   0x13 arm branches to 0x4ED8, which branches to 0x6878), so the two counts can agree while the
   set is wrong - which is exactly what happened. Only walking the chain is sound.

Also: **`0x01` is not a command.** A `cmpi.b #0x01` + `beq` at 0x63DC looks like a chain node to a
pattern scan, but it is an inner test inside another handler. The chain walk excludes it correctly
and the card refuses `0x01`.

### Reply convention - confirmed empirically

Decoded from the peer's side, not inferred:

| Command | Reply content | Reading |
|---|---|---|
| `0x3E` | `00 39` | status 0 = OK, then the **packed** model byte: class 3, digit 9 = ND-5900 |
| `0x30` | `00 07 7F` | status 0, then `0x077F` - the selftest status word |
| `0x1F` | `FF 07 10 11` | status 0xFF = error, error code `07` |
| undefined | `FF 06 10 11` | error code `06` = undefined command |

So byte 0 is a status (`00` OK / `0xFF` error), byte 1 is an error code on the failure path, and
`10 11` is a constant trailer on the error replies. The `0x3E` carve in section 1h is **confirmed
by execution**: the card really does emit `(class << 4) | digit`.

### Do ACCP command numbers share the console command table's numbering? MIXED - do not assume

The console command table (entries near 0x130FE: a leading word plus a PLANC string descriptor)
carries codes 0x03-0x46. Almost every code in 0x1F-0x3E also has a CMD-3 dispatcher arm, and the
console-only codes are exactly the interactive ones - HELP, VALUE, MAIN-FORMAT, SHOW-REGISTERS,
DUMP-LOCAL-MEMORY, LOOP-ON-NEXT-COMMAND, SET-CLOCK-SPEED, RESET-CPU, TEST-MEMORY, TEST-BUSLOOP.
That is what a shared namespace would look like.

**But the evidence does not agree with itself:**

- `0x30` **matches**. Console name `READ-ACCP-STATUS`; the card replies `00 07 7F`, i.e. the
  selftest status word. Exactly the console name's operation.
- `0x3E` **does not match**. Console name `TEST-BUFFERS`; the card replies with the packed CPU
  model byte. Not a buffer test by any reading.

**SETTLED 2026-07-31 - they are SEPARATE enums that merely overlap.**

The earlier hedge here blamed the console-table parse. That was wrong: `ACCP-COMPLETE-REFERENCE.md`
part 3 carries the same table as **VERIFIED**, with the same record layout
`{word code, long origo, long lower, long upper}` and the proof that
`(0x13358 - 0x130FE) / 14 = 43` exactly. The parse was right and the mismatch is real.

The decisive probe is **command `0x3C`**. In the console table `0x3C` is
`TRACE-COMMUNICATION-DATA`, whose whole observable effect is the flag at `0x001143B4` - a single
bit that can be read directly. Sent over CMD 3 to a booted card:

```
--- ACCP command 0x3C ---
   reply cmd=3 ownCmd=3 content=[ FF 01 10 11 ]
trace flag before : 0
trace flag after  : 0
```

`0x3C` is a *defined* octobus command (it is inside the 0x30-0x3E run and returns error code `01`,
not the "undefined command" complaint), it does something, and that something is **not**
toggling the trace flag. So the console command numbering and the CMD-3 command numbering are two
different enums.

`0x3E` is the clean confirming case: `TEST-BUFFERS` on the console, packed CPU model over CMD 3.

`0x30` returning the selftest status `0x077F`, which matches the console name `READ-ACCP-STATUS`,
is then either coincidence or two related enums independently assigning a status read to the same
number. Not resolved, and not important enough to guess about.

### This answers an open question in ACCP-COMPLETE-REFERENCE.md

Part 3 records the console codes as sparse and asks what the holes mean:

> Used: 03, 06, 07, 09, 0A, 0C, 1F, 20-2F, 30-3F, 40, 41, 42, 46.
> Absent: 04, 05, 08, 0B, 0D-1E, 43, 44, 45.
> This looks like a **global ND command-code enum** that the console shares with something else -
> most likely the ACCP-ND100 command set [...] **UNVERIFIED**.

The CMD-3 octobus command set is `0x0D-0x18`, `0x1B-0x2D`, `0x30-0x3E`. It **does** cover the
console's largest hole (`0x0D-0x1E`), which is why the shared-enum reading is attractive. But the
`0x3C` probe rules it out for *this* consumer: the octobus command set is not the thing the
console enum is shared with. The holes remain unexplained, and the ACCP-ND100 guess is refuted for
the CMD-3 path specifically.

### Still open

The 46 commands are enumerated but **not named**. The console command table at 0x13357 has a
comparable number of entries (LOAD-CONTROL-STORE, READ-ACCP-STATUS, LOOK-AT-LOCAL-MEMORY, ...),
so a mapping from ACCP command number to console command is plausible - but it is UNVERIFIED and
has not been attempted. Naming them means carving each handler.

Known so far: **0x3E = report CPU model** (section 1h).

---

## 2. Acknowledge and retries - and the ACCP's error strings

The receiver drives two acknowledge bits at the end of every frame:

| Ack | Normal transmission | Broadcast | Default retries |
|---|---|---|---|
| 0 0 | node not present | nodes not present | 15 |
| 0 1 | **successful** | successful | - |
| 1 0 | destination busy | destination busy | 255 |
| 1 1 | parity error | ambiguous response | 15 / 0 |

Every one of these has a matching message in the ACCP firmware, which is strong mutual
confirmation:

| Ack | ACCP string |
|---|---|
| 0 0 | `"$Octobus destination not present$"` |
| 1 0 | `"$Octobus retry timeout$"` (the busy path, retried 255 times) |
| 1 1 | `"$Octobus parity error$"` |

The retry count is programmable in the **OCTObus Transmitter Control Register**, and the
**Transmit Status Register** reports whether a message was retransmitted. Neither register's
ACCP address is known yet.

---

## 3. Station numbering - all octal

- **Global OCTObus** (cable): stations **0-17B**, set by thumbwheel switch.
- **Local OCTObus** (MFbus backwiring): stations **77B down to 20B**, set by on-board
  registers written by the MFbus controller.
- Station numbers must be unique. 0 and 77B... (0 and 63 decimal) are illegal as
  destinations.

This is why the ACCP prints `"not found at Octobus stations 2-7."` and
`"$Not prepared for message from station "`, and it constrains what an emulated station
number may be.

**Power-fail semantics depend on the number**: a power-down message from stations 1-17B
means real power fail; the same message from 20B-76B means "fatal controller hardware
failure". The receiver distinguishes them purely by source.

---

## 4. Hardware-decoded messages - the CM* codes

These are acted on by the OBCON gate array **without software reading them**, and they can
pull a hung processor out via a non-maskable level-7 interrupt.

| Octal | Name | Effect |
|---|---|---|
| 241 | RESTART | asserts RESET, restarts the controller after a total reset |
| 242 | CONTINUE | deasserts HALT |
| 243 | STOP | asserts HALT; stays asserted until CONTINUE arrives |
| 244 | INT7 | generates a level-7 interrupt (OCINT7), clearable by software |
| 245 | RESCOUNT | resets the time reference counter |
| 376 | POWERUP | |
| 377 | POWERDOWN | |

`RESET-CPU` (ACCP command 0x46) and the START/STOP/CONTINUE-MICROPROGRAM commands map onto
this set conceptually. **Whether the ACCP issues them as OCTObus messages or through a local
register has NOT been established** - do not assume.

The **INT7 reset register (OCINT7) is write-only, carries no data, and any write clears the
INT7 condition.** Its address is given as **FF810E** - but that is the **DIOC** processor's
address map, not the ACCP's. The ACCP equivalent has not been found.

---

## 4y. THE MFBUS DISCOVERY EXCHANGE - payload and accepted reply [CARVED 2026-07-28]

This is the one octobus conversation the firmware has at boot, and it is fully specified. An
emulator that answers it correctly gets `MFbus controller` recognised and a real
`CPU model:` line instead of the ND-5800 fall-through.

### What the ACCP SENDS - 2 bytes

`MfBusControllerConfigCheck` @0x121C builds an OBCON **function 0x41** request per station
2..7. The message descriptor is `{origo = &(0x16,A6), lower = 0, upper = 1}` - so the payload
is exactly **two bytes**:

| Byte | Value | Set at |
|---|---|---|
| 0 | **`0x03`** | 0x125A `move.b #0x3,(0x16,A6)` |
| 1 | **the ACCP's own station number** = `HW_ACCP_STATION_CONFIG (0x900001) & 0x1F` | 0x1268 |

Request block: fn `0x41` at +0x00, status at +0x02, process `0x05` at +0x06, destination
station at +0x0C, the descriptor at +0x10.

### What the ACCP ACCEPTS - `MFCRECEIVE` @0x14B4

Every one of these is a hard gate; fail any and the station is rejected.

1. **OBCON status must be `0x8300`** (checked at 0x12AE before MFCRECEIVE is even called).
2. The reply is picked up from **registration-table entry index 5** -
   `g_obconProcessRegTable (0x11635C) + 0x3C`, buffer pointer at **entry+0x06**. Index 5
   matches the `+0x06 = 5` process number in the request, and the same table `ObconFn49`
   registers into. **Register process 5 or nothing will ever be delivered.**
3. **Poll limit `0x2710` = 10000 iterations**, waiting for `(0x08,buf) != 0` or
   `(0x02,buf) != 0`. On timeout MFCRECEIVE returns **2**.
4. **`(0x02,buf)` must be 0** - non-zero takes the error path at 0x1616.
5. Payload begins at **`(0x14,buf)`**:

| Offset | Test | Meaning |
|---|---|---|
| byte 1 | `& 0x3F` **must equal the station addressed** | the 6-bit OCTObus **source** field (frame bits 13..8) |
| byte 3 | **must be `0x05`** | process / subprocess, echoing what we sent |
| byte 4 | **N** - a length | payload runs from byte 5 |
| bytes 5.. | copied to **`0x001131E6`**, `N-1` bytes | the actual reply data |

6. Back in the caller (0x12D4): **`(0x001131E6)` byte 0 must be 0**, else branch to 0x1308.
   Byte 1 at `0x001131E7` is consumed next.

### Then the CPU model

> **[REFUTED - do not build on the inference below. It is kept only because the wrong version
> may already have been read.]** A0 is **NOT** the reply buffer. `0x112C` loads
> `lea (0x00114550).l,A0` - the signature matrix in ACCP local SRAM. This file's own section 4b
> already says "ANSWERED: no", and the whole derivation is now carved and live-verified in
> **part 5 of `ACCP-COMPLETE-REFERENCE.md`**. Model detection is emphatically NOT a pure
> protocol matter: the reported model is cross-checked against a class the ACCP derives from
> its own hardware, and a responder built on this paragraph would be wrong.

`DetectCpuModelBySignature` (0x110A region) probes `(A0)+0`, `+4`, `+0x0C` for the signature
**`0x7F55`** to pick the model class (see part 3 of `ACCP-EMULATION-STATUS-AND-HANDOFF.md`).
**[INFERENCE - well supported,
not proven]** A0 at that point is very likely **`0x001131E6`, the reply buffer** - i.e. the
signature is looked for **in the MFbus controller's reply, not in ND-5000 memory**. That would
make model detection a pure protocol matter with no memory probing at all.

**Why it is not proven**: Ghidra reports **no callers** for the 0x110A entry - it is reached by
fallthrough or branch from the enclosing code, so A0's origin has not been traced. Confirm
before building a responder around it.

---

## 4z. THE DRIVER API - `ObconRequestDispatch` @0xF686 [FOUND 2026-07-28]

Above the raw registers there is a **request-block driver with 17 function codes**. Every
octobus operation that is not a single raw frame goes through it. This is the interface an
emulator has to satisfy, and it was invisible until the image was fully disassembled.

`A0` -> request block; the word at `(A0)` is the function code. The routine is a linear
`cmpi.w` chain, codes **0x41-0x51**, each tail-calling its handler, all converging on 0xF80E.

### The request block

| Offset | Size | Field |
|---|---|---|
| +0x00 | word | **function code** |
| +0x02 | word | **status returned. SUCCESS == `0x8300`** - callers test `cmpi.w #-0x7D00`, which is that value |
| +0x06 | byte | kick value / process (fn 0x43) |
| +0x0C | word | destination station (fn 0x43) |
| +0x10 | 12 | PLANC array descriptor `{origo, lower, upper}` over the message buffer (fn 0x41) |

On failure the caller prints `"$Octobus transmit error "` (descriptor 0x124AA).

### The 17 function codes

| Code | Handler | What |
|---|---|---|
| 0x41 | 0xF8E6 | **multibyte message** - used by `Cmd3A_SendMultibyteOctobus` (sets it at 0x99C2) and by the microprogram-command region (0x6AC6). Very likely `DOSEND_MULTI_OCTO` / `DOREC_MULTI_OCTO` |
| 0x42 | 0xF8E6 | **same handler as 0x41** - it must branch internally on the code |
| 0x43 | 0xFADC | **send kick - PROVEN**, `Cmd3B_SendKickOctobus` sets it at 0x9B54 |
| 0x44 | 0xFB48 | unknown |
| 0x45 | 0xFBB4 | unknown |
| 0x46 | 0xFC20 | unknown |
| 0x47 | 0x1011C | unknown |
| 0x48 | 0x1018A | unknown |
| 0x49 | 0x101F8 | **boot init** - the only code issued at startup, twice (0x0D26, 0x0D82). Matches ND-14001 section 4.8.1: a node must be given station number, broadcast type, power-fail handler station and bus speed before use |
| 0x4A | 0x10154 | unknown |
| 0x4B | 0x101C2 | unknown |
| 0x4C | 0x102B4 | unknown |
| 0x4D | 0xF600 | unknown (reached by `bsr.w`) |
| 0x4E | 0xF620 | unknown (`bsr.w`) |
| 0x4F | 0xF5A2 | unknown (`bsr.w`) |
| 0x50 | 0xF5DC | unknown (`bsr.w`) |
| 0x51 | 0xF5EE | unknown (`bsr.w`) |

Five codes use `bsr.w` with a negative displacement rather than `jsr abs` - a scan that only
looks for `jsr` will miss them, as mine initially did.

**Layering**: `OctobusTransmitWord` / `OctobusReceiveWord` are the single-frame primitives the
`SEND-OCTOBUS` / `RECEIVE-OCTOBUS` console commands use directly. `ObconRequestDispatch` is
the layer above, for kicks, multibyte messages and initialisation.

### The dispatcher is a SOFTWARE layer - it touches no hardware

A byte sweep of four handler bodies - 0xF8E6 (fn 0x41/0x42), 0xFADC (fn 0x43), 0x101F8
(fn 0x49) and 0x102B4 (fn 0x4C) - finds **no access to any peripheral select in any of them**.
They are pure RAM.

That changes what an emulator has to do. The real octobus register I/O happens in exactly two
places:

1. **`OctobusTransmitWord` @0x7890 / `OctobusReceiveWord` @0x786C** - the raw frame primitives
2. **the IRQ3 (0x0510) and IRQ7 (0x0826) handlers** - message arrival and FIFO drain

Model those and the dispatcher runs as ordinary code on top.

### `ObconFn49` @0x101F8 - what "boot init" actually is

It does **not** program the bus. It registers an entry in a table at **0x0011635C, 12 bytes
per entry, indexed by the byte at request+0x06**:

```
index = (byte)(request+0x06);   entry = 0x0011635C + index*12
entry[0x00] = 1                      ; word, in-use / state
entry[0x02] = request[0x08]          ; longword copied straight through
```

Called twice at boot (0x0D26, 0x0D82). Two registrations - consistent with a card declaring
which processes may receive octobus messages, which is what the error string
`"$Not prepared for message from station "` complains about. The name `BootInit` describes
*when* it runs, not what it does; treat it as provisional.

**The request block is NOT one uniform struct.** Fn 0x49 reads its array descriptor at
**+0x0E**; `Cmd3A` (fn 0x41) writes one at **+0x10**. Read the layout from the handler you
care about.

**Still unknown**: which code, if any, exposes the Transmitter Control / Transmit Status
registers of section 2 (retry count, retransmit indication). Thirteen of the seventeen
handlers remain unread, and since none of the four read so far touches hardware, those
registers may not be reachable through this API at all.

---

## 4a. SETTLED 2026-07-28 - the octobus data registers are PROVEN

Section 4b below retracted the octobus attribution of `0x880000` and `0x770004` as unsound,
because it rested on frame width alone. **That retraction was right at the time and is now
superseded: the call chains from the console commands prove it outright.**

After the `noreturn` flag was cleared and `PlancFixFlow` re-run headless, the command handler
bodies became readable:

```
Cmd38_SendOctobus @0x97CA          console: SEND-OCTOBUS <Data (16)>
  -> PromptAndReadParameter   0x295C     read the operand
  -> ParseNumberInCurrentBase 0x47C8     convert, base from 0x001131FC
  -> OctobusTransmitWord      @0x9854    <-- writes 0x00770004
  loops while 0x0011313C != 0            (LOOP-ON-NEXT-COMMAND)

Cmd37_ReceiveOctobus @0x9748       console: RECEIVE-OCTOBUS
  -> print descriptor 0x12400 = "$Octobus receive fifo: "
  -> OctobusReceiveWord       @0x9770    <-- reads 0x00880000
  loops while 0x00113138 != 0
  -> format in the current base and print
```

`RECEIVE-OCTOBUS` printing **"Octobus receive fifo:"** immediately before reading `0x880000`
is the firmware naming the register itself.

| Address | Dir | Width | Role |
|---|---|---|---|
| `0x00770004` | write | word | **OCTObus transmit data** |
| `0x00770007` | read | byte | bit 3 = **transmit ready** |
| `0x00880000` | read | word | **OCTObus receive data (FIFO)** |
| `0x00660001` | read | byte | bit 2 = **receive data available** |

This is consistent with the interrupt paths rather than in conflict with them: IRQ3 takes one
message from `0x880000`, IRQ7 drains it in a loop while `0x660001` bit 2 stays set. It is a
FIFO with a data-available flag, and both earlier readings were right.

Routines renamed accordingly: `OctobusTransmitWord` @0x7890, `OctobusReceiveWord` @0x786C.
Both are PLANC **NATIVE** shape (A6 == A7, plain `rts`); `PlancApplyConvention` correctly
left both alone when it set `__planc` on 117 other routines.

**Still true and still important**: neither ready-poll has a timeout. Both are unbounded
`beq.b -10` spins. An emulator that never raises `0x770007` bit 3 or `0x660001` bit 2 will
hang the ACCP exactly as real hardware would.

**Still open**: the Transmitter Control Register and Transmit Status Register that ND-14001
section 4.4 refers to (retry count, retransmit indication) have not been located. `0x330000`
/ `0x330001` and `0xAA0000` remain the candidates.

---

## 4b. CORRECTIONS to section 5, made the same day

Section 5 was written before I re-read the firmware write-up of record (then
`ACCP-324716-FIRMWARE-RE-2026-07-27.md`, now **part 1 of `ACCP-COMPLETE-REFERENCE.md`**), which
was already further along than I realised. Three things in section 5
are wrong or overstated. They are corrected here rather than quietly edited, because the
wrong version may already have been read.

1. **`0x770004` / `0x770007` are NOT a new discovery.** I wrote that select 0x77 "was not in
   any earlier hardware list" and that my sweep found it. **False** - the RE document's
   section 2.4b already recorded it, from `movea.l #0x770004,A1` at 0x069E in the IRQ3
   handler, including that data arrives into it from `0x440000` and that bits 3 **and 4** of
   `0x770007` are a handshake **with a retry count of 10**. My sweep rediscovered it.

2. **The routines at 0x786C and 0x7890 sit inside `0x71F8-0x7C14`, which the RE document
   identifies as the CONTROL STORE loader**, not as the octobus driver - it is named by the
   firmware's own string `"C O N T R O L  S T O R E  E R R O R in buffered CI-bits 35 or 40."`.
   So calling them "the OCTObus transmit/receive path" on frame-width grounds alone was the
   exact mistake that document warns about twice: **generalising a port model from a single
   routine.** The mechanics I carved stand; the octobus attribution does not.

3. **What `0x880000` actually is, per the RE document, is better than my guess and partly
   overlaps it**: it is the **message / kick read port**, with `0x660001` bit 2 as its
   data-available flag. IRQ3 takes one message; IRQ7 drains it in a loop. So it is genuinely
   FIFO-like *and* genuinely the octobus message path - but that was established from the
   interrupt handlers and the `" from SAMSON"` / `" to SAMSON"` trace strings, which is real
   evidence, not from frame width.

**Also relevant and already known there**, which section 5 does not reflect:

- `0x220000` is a **general command/function port**, not an MF-bus port. The function code
  selects which target the `0x440000`/`0x550000` data pair talks to: `0x300F`/`0x400A`/
  `0x400C`/`0x000F` drive MF-bus memory, `0x0005` drives the AOB, `0x0018` performs a
  control-store operation.
- `0x330000` and `0x330001` are **write-only latches with RAM shadows** at `0x001144EE` and
  `0x001144EF`. The firmware never reads them back. `0x330000` bit 6 is a **write strobe**,
  bit 2 gates a control-store operation.
- `0x660001` is a **shared status byte whose bits belong to different functions**: bit 1 = AOB
  busy, bit 2 = message available, bit 4 = MF-bus complete.
- **`0x00900007` is real and breaks the nibble-replication rule** (`0x90` is not a repeated
  nibble). Section 1 of this document should not be read as a law.

---

## 5. What the ACCP driver actually does - carved

Three leaf routines were recovered and named. All three are `link`/`unlk` with **A6 == A7**
and a plain `rts` - the PLANC **NATIVE** shape (ND-820026.1 Figure 9), or hand-written
assembler. **Do not apply `__planc` to them.**

### `HwWaitRxReadyThenReadWord_880000` @0x786C

```
btst.b #2,(0x00660001).l    ; poll
beq.b  -10                  ; spin - NO TIMEOUT
move.w (0x00880000).l,...   ; 16-bit read, result in D0
rts
```

**Established**: `0x00880000` is a 16-bit input data register gated by bit 2 of the status
byte at `0x00660001`.
**Inferred**: this is the OCTObus receive path. The width matches the documented 16-bit input
frame exactly, the string `"Octobus receive fifo: "` exists, ND-14001 Figure 29 places a FIFO
inside OBCON, and the routine sits in the 0x6A74-0x7C14 driver region. Not proven.

### `HwWaitTxReadyThenWriteWord_770004` @0x7890

```
btst.b #3,(0x00770007).l    ; poll ready
beq.b  -10                  ; spin - NO TIMEOUT
move.w D0w,(0x00770004).l   ; 16-bit write
rts
```

**Established**: `0x00770004` is a 16-bit output data register; bit 3 of `0x00770007` is its
ready flag. **Select 0x77 was not in any previous hardware list** - the full-image sweep
found it.
**Inferred**: the transmit mirror of the above, matching the documented 16-bit output frame.

### `HwWaitComplete_660000_bit4` @0x78B2

Spins on bit 4 of `0x00660000` (the HIGH byte of the same status word) and returns. A
transaction-complete wait.

### The timeout is not here

**Neither poll has a timeout** - both are unbounded `beq.b -10` spins. Yet the firmware has
`"K I C K   T I M E O U T : "` and `"AOB not read by microprogram within timeout."`. So the
timeout is imposed by a **caller**, not by these primitives. An emulator that stalls either
of these ready bits will hang the ACCP exactly as real hardware would.

---

## 5a. THERE ARE FOUR UNBOUNDED POLLS, NOT TWO [MEASURED 2026-07-28 on the emulator]

Section 5 says "neither poll has a timeout" of the two octobus primitives. Correct as far as
it goes, but **there are four**, and the other two stop the boot just as hard. Found by
actually running the firmware in RetroCore and following the PC each time it stopped:

| # | Routine | Polls | PC when stalled |
|---|---|---|---|
| 1 | `OctobusTransmitWord` @0x7890 | `0x770007` bit 3 | - |
| 2 | `OctobusReceiveWord` @0x786C | `0x660001` bit 2 | - |
| 3 | `HwWaitComplete_660000_bit4` @0x78B2 | `0x660000` **bit 4** | **0x78C2** |
| 4 | the 32-bit read path @0x7374 | `0x660001` **bit 0** | **0x738C** |

Poll 4 was not previously recorded at all: `btst.b #0,(0x00660001)` / `beq.b -8` at 0x7384,
escapable only if `0x00113138` is non-zero. It guards the read of the `0x550000`/`0x440000`
pair and the subsequent `move.w #0x0005,(0x00220000)`.

**How far the boot gets, measured:**

| Bits held high | Console output | Stops at |
|---|---|---|
| `0x770007` bit 3 only | 513 chars, through "Start/stop microprogram test abc" | `0x78C2` |
| + `0x660000` bit 4 | 608 chars, "…abc failed at CSA: 00FFH" | `0x738C` |
| + `0x660001` bit 0 | **1769 chars — the whole selftest suite** | past ALU verify |

With all of them released the firmware runs BUS test, MIR test, Control Store sample test,
Start/stop microprogram test, A,MARG D,AIB test, "Loading control store with selftests…", and
the ALU verify series — every one failing with `Result : 00000000H` against a printed
`Expected:`, which is exactly right with no ND-5000 present.

**Bits that must be left LOW — raising them corrupts the model:**

- `0x660001` **bit 1** = AOB busy. High makes the AOB write path spin instead of proceeding.
- `0x660000` **bit 0** = control-store operation OK. High would make the microprogram
  selftests print **success without an ND-5000** — a fabricated pass.
- `0x660000` **bit 5** steers the firmware into a restart path.

**`0x660001` bit 2 (octobus receive available) should also stay low.** Measured: holding it
high gets the boot *less* far, not further — the console stops after "ACCP local ram test OK"
because the fabricated zero replies send the firmware down a different path.

Because polls 2 and 4 share the byte at `0x660001`, an emulator has to **compose** that
status byte rather than let whichever handshake was wired last win.

**The printed `Expected:` values are a ready-made Phase 6 oracle**: `1C587698H` for the BUS
test, and the 8-word microword pattern `7698H B027H 0AAAH 2C91H 0D8CH F58BH AFBEH 6195H` for
the MIR and control-store sample tests. When an ND-5000 model can return those, the selftests
flip from failed to passed with no test rewriting.

---

## 5b. THE MFBUS PRESENCE SCAN, CAPTURED OFF THE WIRE [MEASURED 2026-07-28]

Section 4z lists 17 driver function codes and notes that most handlers are unread. It is no longer
necessary to read them to know what the card actually *sends* at boot: the RetroCore NDOBCON model
records every transmitted frame, so the boot traffic can simply be dumped.

**With nothing answering, the ACCP transmits exactly 36 frames: six stations x six frames.**
The stations are **2, 3, 4, 5, 6 and 7** - which is precisely the range named in the console line
`MFbus controller not found at Octobus stations 2-7.` Those stations are the MPM5 shared-memory
window shared with the octobus controllers, so the scan is looking for shared memory.

The per-station sequence, with the station number in bits 13-8:

| # | Frame | C | B | Dest | Info | Reading |
|---|---|---|---|---|---|---|
| 1 | `8235` | 1 | 0 | 2 | `0x35` | control - open / select |
| 2 | `0205` | 0 | 0 | 2 | `0x05` | data |
| 3 | `0202` | 0 | 0 | 2 | `0x02` | data |
| 4 | `0203` | 0 | 0 | 2 | `0x03` | data |
| 5 | `0200` | 0 | 0 | 2 | `0x00` | data |
| 6 | `8225` | 1 | 0 | 2 | `0x25` | control - execute / close |

So the probe is **two control frames bracketing a four-byte payload `05 02 03 00`**. Station 3 gets
`8335 0305 0302 0303 0300 8325`, and so on to station 7. The "open/execute" bracketing matches the
shape already seen on the `0x220000` command port (§2.4 of the RE document), which is reassuring -
the same idiom at a different layer.

### The wire frames and §4y are THE SAME MESSAGE - full decode [RECONCILED 2026-07-28]

§4y carves the request from the driver side: OBCON function `0x41`, **process `0x05`** at request
+0x06, and a **two-byte payload** `{0x03, own station}`. §5b captured six frames per station off the
wire. Put side by side, every byte is accounted for - and the two derivations are independent, one
from the request block and one from the transmitted frames:

| Frame | C | Info | Role |
|---|---|---|---|
| 1 | 1 | `0x35` | control - start of multibyte message |
| 2 | 0 | `0x05` | **process number** (request +0x06) |
| 3 | 0 | `0x02` | **payload length in bytes** |
| 4 | 0 | `0x03` | payload byte 0 (0x125A) |
| 5 | 0 | `NN` | payload byte 1 = **our own station number** (0x1268) |
| 6 | 1 | `0x25` | control - end of multibyte message |

So the multibyte wire format is:

```
[C 0x35] [process] [length] [payload...] [C 0x25]
```

**This corrects the first reading in this section**, which called `05 02 03` a "fixed payload". It is
not: `05` is the process number, `02` is a length, and only `03` is payload. The distinction matters
because a responder has to parse the length rather than assume a shape.

The station-number byte was confirmed by experiment before §4y was available: setting the model's
station (the byte the firmware reads from `0x900001` low 5 bits at 0x122E) to 1 changed frame 5 from
`0200` to `0201`, and to 0x0B changed it to `020B`, while the rest stayed fixed. Independent
agreement between the wire capture and the carve.

Before that byte was modelled it read **00** - an illegal OCTObus station - because the stub
returned zero.

Before that byte was modelled it read **00** - an illegal OCTObus station - because the stub
returned zero. Any emulator must supply a legal station number here BEFORE the scan, or the card
identifies itself illegally in its very first transmission.

[INFERENCE] the readings "open / select" and "execute / close" for `0x35` / `0x25`, and the meaning
of the fixed `05 02 03`. What is ESTABLISHED is the frame values, the ordering, the six-station
sweep, the addressing, and the station-number byte.

**What a peer must do to satisfy the scan**: see §4y, which specifies it completely - status
`0x8300`, delivery via registration-table process **5**, and the field tests MFCRECEIVE applies.

**One gap remains between §4y and a working responder, and it is worth stating precisely.** §4y
describes the accepted reply in terms of a RAM buffer - `(0x14,buf)` byte 1 = source, byte 3 = `0x05`,
byte 4 = length, bytes 5.. copied to `0x001131E6`. But an emulated peer does not write that buffer;
it puts **frames on the wire**, and the ACCP's own receive path assembles them into it. So the
missing piece is the **frame-to-buffer mapping** used by `DOREC_MULTI_OCTO`:

- at what offset the driver stores the received frames,
- whether it stores the whole 16-bit frame or splits it,
- and specifically why the SOURCE field lands at buffer byte **1** rather than byte 0.

That last point is the tell: a received frame is `C|B|source<<8|info`, so its *low* byte is the
information byte and its *high* byte carries the source. Byte 1 holding the source means the driver
is not storing frames verbatim in big-endian order. Until that mapping is read, a responder can only
be built by guessing the layout - which is exactly the kind of guess this document exists to
prevent.

**Carve `DOREC_MULTI_OCTO` next.** Everything else in the exchange is now specified.

**A station that answers ends the scan.** With the model's default loopback auto-reply enabled the
card transmits only the first six frames and never probes 3-7, because the echoed frame arrives with
the source field reading station 2. **That is a lucky accident, not a correct reply** - the loopback
carries none of the information a real MFbus controller would return, which is exactly why the card
still reports `CPU model: ND-5800` as unconfirmed afterwards. Anything built on top of the loopback
should be treated as scaffolding.

**This is the concrete specification for a peer model**: recognise `0x35` addressed to your station,
absorb the four data bytes, act on `0x25`, and reply with whatever confirms presence and reports the
CPU model. The last part is still unknown and is the next thing to carve - the reply *content*, not
the framing.

Both behaviours are locked in as regression tests
(`Octobus_ProbesStations2Through7WhenNothingAnswers`,
`Octobus_ScanStopsAtTheFirstStationThatAnswers`) in
`Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpConsoleTests.cs`.

---

## 5c. THE ROM'S OWN VOCABULARY, AND WHERE A KICK GOES [CARVED 2026-07-28]

The ROM string table names the message classes itself, which is independent confirmation of
section 1a. Offsets are into
`E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`.

Console commands (offset 78679):

```
SEND-KICK-OCTOBUS <DESTINATION><Kick value (process)>
SEND-MULTIBYTE-OCTOBUS <Destination><Subprocess><Message>
SEND-OCTOBUS <Data (16)>
RECEIVE-OCTOBUS / RECEIVE-MULTIBYTE-OCTOBUS
SET-KICK-TIMEOUT <Kick timeout (ms)>
TRACE-COMMUNICATION-DATA <Trace Octobus communication to consol? (y/n)>
```

Error strings:

| Offset | String |
|---|---|
| 75232 | `Kick ` |
| 76232 | `Illegal kick ` |
| 76194 | `Illegal start of message ` |
| 76158 | `Illegal end of message ` |
| 75158 | `Unexpected multibyte message ` |
| 74603 | `AOB full, previous message not read. Message lost!` |
| 75092 | `in DOREC_MULTI_OCTO` |
| 75124 | `in DOSEND_MULTI_OCTO` |
| 74881 | `Not prepared for message from station ` |
| 76519 | `Octobus message to OMD 0 received. No tests implemented yet.` |

"Illegal start of message" and "Illegal end of message" are the S bit being wrong; "Not
prepared for message from station" is the connect-CMD requirement. `DOSEND_MULTI_OCTO` and
`DOREC_MULTI_OCTO` are the driver's own names for the multibyte send and receive paths - "MULTI"
being the multibyte class of section 1a.

### CORRECTION 2026-07-30 - the IRQ3 routine below is the TRANSMIT path, not receive

**The subsection that follows is wrong and is kept only because it was read.** It calls
`Irq3KickServiceAndTrace` @`0x6C0` the octobus receive doorbell. It is not. The vector at
`0x0698` sets the base registers explicitly:

```
0698  movea.l #0x00660000,A0      ; status base
069E  movea.l #0x00770004,A1      ; octobus TRANSMIT data register
06A4  btst.b  #3,(A0)             ; 0x660000 bit 3
06AA  bsr.w   0x06C0              ; the service loop below
...
06C0  btst.b  #3,(0x3,A1)         ; 0x770007 bit 3 = TX ready, spin up to 10
06EC  btst.b  #0,(0x1,A0)         ; 0x660001 bit 0 = data available from the CPU
06FE  move.w  (0x00440000).l,(A1) ; read the DATA port, WRITE it to 0x770004
```

So IRQ3 is the **CPU-to-octobus forwarding path**: the ND-5000 writes a word to AIB
(`0x440000`), and the ACCP's IRQ3 handler transmits it on the octobus. The globals
`0x00113116` and `0x00113144` are loaded from `(0x2,A1)` = `0x770006`, which is
**transmit-side**, so the conclusion below that they are a receive doorbell does not follow.

**The real receive path** is `OctobusReceiveWord` @`0x786C`, reading **`0x880000`** gated by
`0x660001` **bit 2**, with reassembly in `OctobusMessageAssemble` @`0x6C02`.

**What survives from the subsection below**: the negative result is unaffected - the octobus
receive path still never calls the MFbus memory routines, and that was established by byte-
searching the call sites, not from the IRQ3 reading.

**A consequence worth carrying forward**: a word the CPU writes to AIB is not necessarily
consumed by the ACCP as data. With IRQ3 enabled it may be forwarded straight onto the octobus.

### A kick is a doorbell for the octobus FIFO, not for shared memory [SUPERSEDED - see above]

`Irq3KickServiceAndTrace` @ **0x6c0** is the receive interrupt handler. It reads the octobus
data port into a caller-supplied buffer, optionally traces it, and sets two globals:

| Global | Written | Read by |
|---|---|---|
| `0x00113116` - the kick value | 0x764 | `OctobusStatusErrorReport` @ 0xa33a only |
| `0x00113144` - a flag | 0x76c | **`MFCRECEIVE` @ 0x14e4** and **`OctobusMessageAssemble` @ 0x6c5a** |

So a received kick means **"a message is available, come assemble it"**. It does not carry or
imply a memory address.

### The octobus receive path never touches MFbus memory

Tested both directions:

- `OctobusMessageAssemble` @ 0x6c02 calls exactly `OctobusStatusErrorReport`,
  `MfBusMemoryErrorReport`, `PlancLeafRuntime_112DE`, `ConsPrintString` and
  `MicroprogCmd_Helper_6940`. The only MFbus item is an **error reporter**.
- Every caller of the MFbus memory transaction routines (13 call sites to `0x7138`, 9 to
  `0x70aa`) sits in console-command and selftest code - `Cmd40_TestMemory`,
  `Cmd33_LookAtMemory`, `LookAtMfBusMainMemory_A`/`_B`. **None in the octobus message or kick
  path.**

**Conclusion for the ACCP: the protocol is genuinely on the wire, not hidden behind a doorbell
into MPM.**

**[OPEN, and deliberately so]** This does **not** settle it for SCSI or Ethernet III. ND-820026
states octobus is for **short** messages and synchronization, so those controllers must move
their payloads through MFbus memory - and a kick plus a shared-memory descriptor is the natural
way to do it. The ACCP is an ND-5000 CPU-support processor with no bulk data to move, so it is
the one card that would not show the pattern. Testing that needs SCSI or Ethernet III firmware,
or a live MPM dump.

### XMSG is not on the octobus

`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-820026-1c-EN DOMINO and NUCLEUS Software
Guide.md`, Figures 2 and 5, both place XMSG on the **ND-100 side**:

```
DOMINO monitor --XMSG--> BOPCOM server (RT-program in ND-100) --OCTOBUS--> DOMINO controller
```

XMSG and OCTObus are two legs of one path, not the same messages. A 68k controller never sees
XMSG, and the ACCP ROM string table contains no XMSG vocabulary at all.

**[OPEN]** Absence of XMSG *strings* does not prove absence of XMSG-shaped *structures in MPM*.
The ND-100 could lay XMSG-format data in shared memory and use octobus kicks as the doorbell,
with a controller ferrying bytes it never interprets. That remains untested for SCSI and
Ethernet III.

### How the ACCP reaches MFbus memory

`MfBusMemoryTransaction` @ **0x70cc** takes a 32-bit MFbus address at `(0x14,A6)` and drives it
through the ND-5000 datapath - **there is no direct memory-mapped MPM window on the 68k bus**:

```
0x220000 <- 0x300F, 0x400A, 0x000F     command port
0x440000 <- address low                 data low
0x550000 <- address high                data high
0x220000 <- 0x300F, 0x400C, 0x000F
wait on 0x660001 bit 4, bounded by a D1 countdown
```

Note this poll **is bounded**, unlike the four unbounded ones in section 5a.

Ghidra reports "no callers" for 0x70cc - it is fallthrough-entered from
`MfBusMemoryTransaction_VariantA` @ 0x70aa. The caller counts above were obtained by
**byte-searching for `jsr` patterns**, not from the call graph, because of that same flow
artefact. See section 4y for the identical problem at `DetectCpuModelBySignature`.

---

## 1c. THE REPLY IS DECODED - AND THE MODEL IS CROSS-CHECKED [CARVED 2026-07-30]

A live MFbus-controller peer was built against the ACCP machine and the discovery
exchange now **succeeds**. `MFbus controller not found at Octobus stations 2-7` is gone,
the scan stops at the first station that answers, and the firmware moves on to a second,
previously unreached failure: **`MFbus controller has incorrect CPU model setting.`**

### What the reply's content byte 1 is: the CPU model digit

Success path at 0x12D4, entered when data-area byte 0 is zero:

```
12E4  move.b (0x001131e7),(0x18,A6)   ; content byte 1
12EC  move.b (0x18,A6),D2b
12F0  andi.w #0xff,D2w
12F4  lsl.w  #0x8,D2w                 ; byte1 << 8
12F6  or.w   #0x5000,D2w              ; | 0x5000
12FA  move.w D2w,D0w                  ; returned as the reported model
```

**The reported model is `0x5000 | (content byte 1 << 8)`.** The digit maps straight onto
the family the ACCP served: 2 -> ND-5200, 4 -> ND-5400, 5 -> ND-5500, 7 -> ND-5700,
8 -> ND-5800, 9 -> ND-5900. A zero byte gives 0x5000, which is not a model.

**So content byte 0 is a status byte and content byte 1 is the model digit.** That closes
the "reply content unknown" item for the first two bytes.

### The second channel: byte 0 = 0xFF is an error report

When byte 0 is non-zero the code at 0x1308 requires it to be **0xFF**, then dispatches on
byte 1 as an error code:

| byte 1 | Behaviour |
|---|---|
| 1 | `bra.w 0x123C` - back into the station loop, i.e. keep scanning |
| 2 | prints two strings (descriptors 0x12BA2 + 0x12BEE), then 0x1428 |
| 3 | prints 0x12BA2 + 0x12C12, then 0x1428 |
| 4 | continues the same pattern at 0x13AA |

Any other byte-0 value branches to 0x1428, the generic error exit.

### The model is NOT believed - it is cross-checked against hardware

`0x110A` is the routine that calls discovery and then validates its answer:

```
1118  clr.w  (0x1131FA)              ; "model valid" = 0
111E  jsr    0x121C                  ; discovery -> D0 = reported model
112C  lea    (0x00114550).l,A0        ; THE SIGNATURE TABLE
1132  cmpi.w #0x7F55,(A0,D1)  D1=6   ; then +0x0C, then +4
```

| Class | Set at | `0x1131F6` | Default `0x1131F8` | Accepted reported models |
|---|---|---|---|---|
| 1 | 0x1150 | 1 | 0x5200 | **0x5200** only |
| 2 | 0x1184 | 2 | 0x5400 | **0x5400, 0x5500, 0x5700** |
| 3 | 0x11BE | 3 | 0x5800 | **0x5800, 0x5900** |

On a match the code sets `0x1131FA = 1` and stores the reported model into `0x1131F8`. A
stray `0x7F55` at the wrong offset instead **clears** `0x1131F6` and leaves `0x1131FA` at
zero. Finally:

```
1204  tst.w (0x001131fa) ; bne 0x1214
120C  ori.w #0x8000,(0x001131e2)     ; the "incorrect CPU model setting" flag
```

So a peer cannot claim an arbitrary model: **it must agree with the class the ACCP derives
from its own hardware.**

### CORRECTION - A0 is NOT the reply buffer

Section 4y flagged as "well supported, not proven" the inference that A0 in the
signature probe points at `0x001131E6`. **It is wrong.** `0x112C` loads
`lea (0x00114550).l,A0` - a different RAM area entirely. The signature table is
hardware-filled state, not message content, and no assumption about the reply buffer is
needed. The "+0 can never match because byte 0 must be zero" prediction in section 1b is
therefore moot; disregard it.

### CORRECTION - the copy is N bytes, not N-1

Section 4y says MFCRECEIVE copies "N-1 bytes". Wrong - that read the descriptor's upper
limit as a count. The two PLANC descriptors built at 0x15A4/0x15C2 are:

- source: origo `(0x14,A0)`, lower **5**, upper **5 + N - 1**
- destination: origo `0x001131E6`, lower **0**, upper **N - 1**

Both spans are **N bytes**.

### The buffer layout, dumped live

Read from the driver's own buffer (registration entry for CMD 5 at `0x00116398`, buffer
pointer `0x00112D40`, data area `0x00112D54`) after a six-byte reply:

| Byte | Value | Reading |
|---|---|---|
| 0 | `0x00` | **not** the START information byte - see below |
| 1 | `0x02` | source station - the peer's station. Confirms the documented byte 1 |
| 2 | `0x00` | unread by any consumer |
| 3 | `0x05` | own CMD. Confirms the gate |
| 4 | `0x06` | length N. Confirms the gate |
| 5 | `0x00` | content byte 0 |
| 6+ | `0x00` | **content bytes 1 upward never arrived** |

Two things this settles and one it opens:

- byte 1, byte 3 and byte 4 are confirmed **live**, not just from the gate tests.
- **`ObconReceiveBuffer.StartFrameInformationOffset` looks wrong.** Byte 0 read `0x00`,
  not the `0x35` we transmitted. Either the START information byte is not stored there, or
  it is stored elsewhere. Do not rely on that field until it is re-read.
- **[OPEN] Only the first content byte reaches the buffer.** A six-byte reply lands its
  length correctly but only content byte 0. That is why the model digit cannot get through
  and why the cross-check still fails. This is the next thing to chase, and it is a
  receive-path question - frame-level tracing of the ACCP's assembler, not more static
  reading.

### RESOLVED 2026-07-30 - the model rule is PROVEN, and the "one content byte" report was wrong

Fixed in RetroCore commit `dbdc291e5`. The ACCP now completes discovery **and** the CPU-model
check, and `Discovery_ReplyByte1SelectsCpuModel` passes. `model = 0x5000 | (byte1 << 8)` is no
longer inference.

**Root cause was interrupt presentation, not the receive path.** `Run()` called
`UpdatePendingInterrupt()` once per 1024-instruction batch, and that function picks a single
highest source with an `else-if` chain - so while the DUART asserted IRQ5, octobus IRQ3 was never
presented at all. Two firmware facts make that fatal: **the receive ISR takes exactly one frame
per interrupt** (a ten-frame reply needs ten interrupts), and **MFCRECEIVE abandons after 10000
poll iterations**, losing anything still undelivered.

**Two errors in the section above, corrected rather than edited away:**

1. **"Only the first content byte arrives" never happened.** The reassembly write pointer was
   still at its initial value of 5, so **zero** content bytes had been appended. Byte 5 read
   `0x00` because the buffer was **untouched** - and both replies tried happened to begin with
   `0x00`, which made an empty buffer look like a truncated one.
2. **Dumping the buffer at end of run proves nothing here.** The firmware clears the reassembly
   record on closing a message, and re-initialises all 64 per-station records and 16 registration
   entries at `0xF4E6`. A late sample shows a wiped structure whatever happened.

Sampled at the right moment the message was assembling correctly all along - data area
`00 02 80 05 06 00 08 A1 A2 A3 A4`, all six content bytes present, every gate passed, payload
copied to `0x001131E6`, model reaching the class check as `0x5900`.

**Test this with digit 9, not 8.** `0x5800` is also the class-3 default written to `0x001131F8`
at `0x11DA` before any comparison, so digit 8 passes whether or not the reply was consumed.
`0x5900` is reachable only through a consumed reply. A companion test pins that digits 2, 4, 5
and 7 are refused, so losing the cross-check cannot pass unnoticed.

### Harness

`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpMfBusControllerPeer.cs`
is a peer that answers the scan, and
`...\tests\AccpMfBusDiscoveryTests.cs` holds the tests. Six pass; the three model-digit
cases are `[Ignore]`d with the open item above as the stated reason rather than deleted.

**A direction trap worth repeating**: a reply must be composed with the PEER's station in
bits 13-8, because the guest reads that field as the SOURCE. Composing it with the ACCP's
station looks right in a log and then silently fails the source test.

---

## 6. Still open

**Updated 2026-07-28.** The framing questions that dominated this list are now closed by
section 1a. What remains:

**Protocol**

- **The meaning of content byte `0x03`** in the CMD 5 discovery request. Still the single
  undecoded field in the outgoing message. Section 1d records a SUPPORTED BUT UNPROVEN
  hypothesis - that it is the ACCP's own Access Processor CMD number - backed by the live
  finding that CMD 3 and CMD 5 are the ONLY two CMDs this card connects. Settling it needs
  either the *Octobus Driver Programming Guide* or an MFbus-controller ROM.
- ~~**`N` and the reply content**~~ - **SOLVED 2026-07-30**, section 1c. Content byte 0 is a
  status byte (0 = good, 0xFF = error report), content byte 1 is the **CPU model digit**, and
  `N` is not itself a gate.
- ~~**Only the first content byte of a reply reaches the driver buffer**~~ - **NOT A REAL
  DEFECT, and never was.** The observation was an artefact of *when* the buffer was sampled.
  The write pointer was still at its initial value 5, so ZERO content bytes had been appended;
  byte 5 read `0x00` because the buffer was untouched, and both replies tried happened to begin
  with `0x00`. Sampling at the right moment shows the whole body assembling correctly. The real
  fault was **interrupt presentation in the emulator** - see section 1e. Fixed 2026-07-30; the
  model digit now reaches the cross-check and ND-5900 is accepted.
- ~~**Where the START information byte is really stored**~~ - **ANSWERED: it is not stored at
  all.** Data-area bytes 0-1 are the source station as a 16-bit word, written from descriptor+4
  at 0x10410/0x1041E. The START frame's information byte goes into the per-station reassembly
  record at `+0x06` (section 1e), never into the buffer. `StartFrameInformationOffset` was
  wrong and has been removed from the code.
- ~~**What fills the signature table at `0x00114550`**~~ - **ANSWERED 2026-07-31, and any class
  is now selectable.** The builder at `0x7D26` has FOUR phases: clear, sixteen reads of
  `0x00220000`, a 16x16 transpose, and then a rewrite pass at `0x7DD0` (bit11:=bit10, field
  moves, a 7-bit Gray decode at `0x7CA2`) whose output is what the class chain reads. Feeding a
  computed sequence to the `0x220000` port - which must be modelled as ARMED by a write of
  `0x0007`, not as a free-running counter - establishes class 2 and gets ND-5500 accepted, live
  verified. Full carve in **part 5 of `ACCP-COMPLETE-REFERENCE.md`**. The remaining unknown is
  narrower than "what fills it": it is what the real ND-5000 datapath physically presents at
  `0x220000`. All-zero reads still give class 3 (ND-5800), which is correct behaviour for a
  machine with no datapath.
- ~~**Buffer byte 2**~~ - **ANSWERED: it is a driver FLAGS byte**, not payload. Cleared at
  0x1042E; bit 7 = message complete (`bset` at 0x106CA), bit 6 = buffer overrun (`bset` at
  0x10BAA). Only the **condition behind bit 5** (0x106A4-0x106B8) is still uncarved. See the
  correction in section 1b, which also fixes the earlier wrong reading of bytes 0 and 1.
- ~~**Whether A0 in `DetectCpuModelBySignature` is the reply buffer**~~ - **ANSWERED: no.**
  `0x112C` loads `lea (0x00114550).l,A0`. See the correction in section 1c.
- **Whether SCSI / Ethernet III use kick-plus-MPM-descriptor** where the ACCP does not. See
  section 5c.

**ACCP hardware**

- **The Transmitter Control Register and Transmit Status Register addresses on the ACCP.**
  Candidates from the sweep: `0x330000`/`0x330001` (heavily accessed, in the driver region)
  and `0xAA0000` (three word writes of a routine parameter at 0x7AE6/0x7AFA/0x7B0E).
- **`0x00770000`**, loaded with `lea` at five sites in 0x11030-0x11230 - far from the
  transmit routine. Either a second use of select 0x77, or the same device accessed as a
  window.
- Whether the ACCP's START/STOP/CONTINUE/RESET commands go out as hardware-decoded OCTObus
  messages (241-245) or through local registers.
- The ACCP's OCINT7 equivalent.

---

## Provenance

Sections 1-4 are transcribed from ND-14001-1-EN chapter 4 and are documentation, not
inference. Section 5 was read from the image on 2026-07-27; each claim there is marked
ESTABLISHED (read from the instruction bytes) or INFERRED (consistent with the manual but
not traced). The cross-matching of acknowledge codes to ACCP error strings in section 2 is my
correlation, not something either source states.

**Added 2026-07-28.** Sections 1a and 1b rest on a second documented source:

- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md`
  - chapter 3 "Octobus Communication", especially 3.3 / 3.3.1 (message format), 3.4 (octobus
    hardware on the ND-5000 CPU), 3.6 (ACCP)
  - chapter 8, the `OCTOBUS-DRIVER`, `LIST-SUBPROC-TABLE`, `READ-OCTOBUS-RECEIVE` and
    `TRANSMIT-OCTOBUS` test commands
- `F:\NDDOC\ND\14\ND-14001-1-EN Domino Standard Hardware Description.pdf` - the original scan,
  read directly to confirm that the four "see OCTObus Protocol Specification" deferrals are in
  the source and are **not** an OCR loss, and to settle Figure 30's bit layout.

The **information-byte decode in section 1a is documentation**, transcribed from 3.3.1. The
application of it to the captured frames in section 1b is arithmetic on documented fields, and
it agrees with an independent carve (section 4y) that was made before either source was found.

Section 5c is read from the image and from the ROM string table. Its negative results - that the
octobus receive path never calls the MFbus memory routines, and that no XMSG vocabulary exists
in the ROM - were obtained by byte-searching for `jsr` patterns rather than from Ghidra's call
graph, because several routines in this image report "no callers" due to the fallthrough-entry
artefact. **A negative from a byte search is strong but not absolute**: a path reached only
through an unresolved indirect jump would not appear.

Two documents remain unlocated and are named here so nobody re-derives the search:
**"OCTObus Protocol Specification"** (cited four times by ND-14001, no ND number given
anywhere) and **"Octobus Driver Programming Guide, written by DVT, 15. Oct. 1986"** (cited by
ND-05.017.01 chapter 8). ND-14.002 is unclaimed in both manuals' related-manuals lists and is a
plausible slot for the former - **that is a guess from numbering alone, not evidence.**
