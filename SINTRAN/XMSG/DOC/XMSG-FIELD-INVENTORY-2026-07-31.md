# XMSG field inventory — every field, its status, and what would settle it

**Date:** 2026-07-31
**Purpose:** the driver for "carve and test until all fields in all protocols are decoded and
understood". One row per field. A field is only **CARVED** when the kernel's own source or
symbol table says what it is — matching the wire is *confirmation*, never a substitute.

Status vocabulary, used strictly:

| Status | Means |
|---|---|
| **CARVED** | The kernel symbol table or source states it. Cite the symbol. |
| **WIRE** | Reproduced on captured traffic, but no kernel evidence for the *meaning*. |
| **INFERRED** | A reading that fits, with no independent support. |
| **UNKNOWN** | Not understood. Say so. |

---

## 1. What this session settled

The transported header layout is now **CARVED** from
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT` and independently **confirmed on
the wire**. This resolves the conflict left open in
`XMSG-SUBHEADER-NAMED-FROM-SYMBOLS-2026-07-29.md` section 4.

| Symbol | Octal | Word | Wire offset |
|---|---|---|---|
| `XMTHD` | 134 | 0 | 14-15 |
| `XMSTA` | 135 | 1 | 16-17 |
| `XMDSY` | 136 | 2 | 18-19 |
| `XMDPT` | 137 | 3 | 20-21 |
| `XMSSY` | 140 | 4 | 22-23 |
| `XMSPT` | 141 | 5 | 24-25 |
| `XMCSM` | 142 | 6 | **26-27 — header ends here** |

`XM5HE=7` (words) and `XM5HL=16` octal (14 bytes) fix the length. `XMLEN=147` is five words
*past* `XMCSM` and is therefore **not** a transported-header field at all.

Three consequences, each confirmed against all 1449 captured data frames:

1. **`XMCSM` is ONE word.** Our 32-bit reading at 26-29 straddles into the body.
2. **`Flags2` (wire 10-11) is a COPY of `XMCSM`** — equal on **1449/1449**. The long-standing
   rule "Flags2 == XMCSM >> 16" was that equality seen through the wrong split.
3. **The body starts at wire offset 28** = 13 (SINTRAN header) + 1 (Counter) + 14 (transported
   header). Confirmed arithmetically: on the frames where `XMCSM` is a length it equals the
   body length **exactly** (offset 0 across 492 frames) once 28 is used.

---

## 2. LAPB layer

| Field | Status | Note |
|---|---|---|
| Address | **WIRE** | `0x01` link mgmt, `0x09` even info, `0x89` odd info. Bit `0x80` = odd length. `0x07` seen on some ACK I-frames — **UNKNOWN**, tolerated. |
| Control | **WIRE** | mod-8 I/S/U. S-subtype via `control & 0x0F`. |
| FCS | **WIRE** | CRC-16 `0x8408`, residue `0xF0B8`. Verified on every frame parsed. |

---

## 3. SINTRAN header - it is SEVEN WORDS, not "13 bytes + a counter byte" [2026-07-31]

The ND-100 is word-addressed and the kernel manipulates this header with word-indexed
instructions (`LDA ,X 1` / `BSET` / `STA ,X 1` in `XSDGM` at `137603`), so the byte-oriented
description we have been carrying is a mis-framing. The header is **7 words**, exactly like
the transported header that follows it:

```
21 13 | 00 0e | 00 64 | 00 66 | 03 d3 | 00 80 | d9 c1
 w0      w1      w2      w3      w4      w5      w6
markers subtype dest    src     Flags1  Flags2  ProtoID:Counter
```

**`w6` packs the Protocol ID in its HIGH byte and the Counter in its LOW byte.** The Counter
does not "sit between the SINTRAN header and the transported header" as previously recorded -
it is the low half of the last header word. 7 words + 7 words = 14 words = 28 bytes, which is
exactly the body start already confirmed independently from `XMCSM == bodyLen`.

**This unifies the ACK model.** An ACK frame reads
`21 13 | 00 03 | 0066 | 0064 | 03d3 | 0001 | da 4b`. What `XMSG-PROTOCOL.md` calls the ACK's
"single trailing payload byte" (`0x4B` here) is **not payload** - it is the low half of `w6`,
the same field as a Data frame's Counter, with the Protocol ID above it. Data frames and ACKs
share one header shape; only the subtype and what follows differ.

**Consequence for the open offset-12 question:** the Protocol ID is not written on its own. It
is the high byte of a word whose low byte is the Counter, so whatever computes one computes
both - which fits the envelope model, where Counter and channel fall out of the same seed
relation. Look for code writing a COMBINED word, not a byte store to offset 12.

Corroboration from `XSACK` (`140076`): the routine opens with a run of `BSET` on T -
`ZRO 20`, `ONE 10`, `ONE 0`, `ZRO 30`, `ZRO 50`, `ZRO 60` (remember the printed field is
`bit<<3`, so those are bits 2,1,0,3,5,6) - which assembles `0b011` = **subtype `0x03`**, the
Ack code. The kernel builds these header words bit by bit in registers.

## 3b. SINTRAN header fields (wire 0-12)

| Off | Field | Status | Note |
|---|---|---|---|
| 0 | Marker1 `0x21` | **WIRE** | Always `0x21`. *Why* — **UNKNOWN**. |
| 1 | Marker2 | **WIRE** | `0x13` normal, `0x12` relayed. |
| 2 | "PacketType" | **RESOLVED - not a field** | The high byte of word 1 (`type:subtype`), `0x00` on all 3595 frames. See below. |
| 3 | Subtype | **WIRE** | `0x0E` Data, `0x03` Ack, `0x13`/`0x19` Reach, `0x07` NetworkError, **plus `0x0A` and `0x0C` - bulk file-transfer data, decoded 2026-07-31, see below.** |
| 1 | Marker2 | **WIRE** | `0x13` normal, `0x12` relayed - **and `0xFD`/`0xFE` on a fourth family that is NOT a subtype variant, see below.** |
| 4-5 | Dest node | **WIRE** | |
| 6-7 | Src node | **WIRE** | Logical source, not the LAPB neighbour. |
| 8-9 | Flags1 | **CARVED** | `XMSEQ` (`0o154`). Assigned from a per-link counter, masked to **15 bits**. See below. |
| 10-11 | Flags2 | **CARVED** | A copy of `XMCSM`. See section 4. |
| 12 | "Protocol ID" / channel | **CARVED - SOLVED** | HIGH byte of the header CHECKSUM. Not a channel at all. |
| 13 | "Counter" | **CARVED - SOLVED** | LOW byte of the same checksum. |

> **SOLVED 2026-07-31.** Word 6 is a ones-complement checksum over the other six header words:
> `w6 == ~ones_complement_sum(w0..w5, 0)` (16-bit, end-around carry). Carved at kernel
> `137314` and verified on **3595/3595** frames - every capture, every subtype, both
> directions. The "channel", the "epoch", and the per-link "seed" do not exist; they were
> artifacts of curve-fitting to checksum arithmetic. Full write-up and the code:
> `XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md`.

---

## 4. `XMCSM` — carried twice, and overloaded

**CARVED:** the kernel comments it *"datagram checksum; if not checksum, then message size"*.
The corpus shows both arms, and they split almost evenly:

| Arm | Frames | Shape |
|---|---|---|
| **size** | 718 | `XMCSM & 0x1FF == bodyLen` exactly. High bits are `0x0` (492) or `0x2` (226) — i.e. bit `0x400` is a flag sitting above a 9-bit length. |
| **not-size** | 731 | `XMCSM` is a small constant (`0x0080`, `0x0064`, …) while `bodyLen` varies. |

**CARVED 2026-07-31 — the kernel comment is literally true, and here is the code.** Sequence at
`134013`, with `XMSIZ=0o144`, `XMSTA=0o135`, `XMCSM=0o142` all from the symbol list:

```
134013  LDA ,B 144        ; A := XMSIZ
134014  STA ,B 142        ; XMCSM := XMSIZ          <- the SIZE arm, taken by DEFAULT
134015  LDATX
134016  BSKP ONE 110 DA   ; flag bit test
134017  JMP  -> 134034    ;   set -> keep the size, done
134021  BSKP ONE 120 DA   ; second flag bit test
134022  JMP  -> 134034    ;   set -> keep the size, done
134024  LDA ,B 135        ; A := XMSTA
134025  STA -104          ; save XMSTA
134026  STZ ,B 135        ; XMSTA := 0
134027  STZ ,B 142        ; XMCSM := 0              <- zero the field before computing
134030  JPL  I 36         ; -> mem[134066] = 137335
134031  STA ,B 142        ; XMCSM := result         <- the CHECKSUM arm
134032  LDA -111
134033  STA ,B 135        ; XMSTA restored
```

Zeroing `XMCSM` (and `XMSTA`) before computing and storing the result back is the standard
checksum-over-block idiom, and it explains why `XMSTA` is excluded from the covered bytes.

**The checksum itself, at `137335`** - a ones-complement sum with end-around carry:

```
137337  LDT ,B 145        ; T := XMDAB   data buffer address
137340  LDX ,B 146        ; X := XMDAW
137341  LDA ,B 147        ; A := XMLEN   the LENGTH
137342  SHA ZIN SHR 1     ; A >>= 1      bytes -> words
137343  RADD CLD SA DL    ; L := word count
137344  RADD CLD 0 DD     ; D := 0       accumulator
137345  SKP IF DL UEQ 0   ; while L != 0
137347  LDATX             ;   A := mem[X+T]
137350  RADD SA DD        ;   D += A
137351  RADD ADC CLD SD DD;   D += carry            <- END-AROUND CARRY
137352  AAX 1             ;   X++
137353  RADD CM1 0 DL     ;   L--
137354  JMP -> 137345
137355  BSKP ONE SSM      ; odd-length tail, masked with AND 21
```

So `XMLEN` (`0o147`) **is** a real length field - it is what bounds the checksum - it simply is
not at wire 30-31 where our layout put it.

This also resolves what the wire measurements could not. The "not-size" arm looked wrong
because its values are **constant** (`0x0080`, `0x0064`) across frames with different bodies,
which is not checksum-like. Under the carve those constants are the SIZE arm carrying `XMSIZ`
- the message **buffer** size, fixed per message class - not the body length. The
`XMCSM & 0x1FF == bodyLen` frames are the ones where the buffer size happens to equal the body.

**The discriminator: BOTH bit 9 AND bit 10 must be set to get a checksum.**

`nd100-dis` prints the `BSKP` bit field as `bit<<3`, so `110` octal is bit **9** and `120` is
bit **10** (divide the printed octal by 8 - established by sweeping the field 0..15 through the
decoder). And `BSKP` skips the NEXT instruction when the condition holds, so the branch sense
is the opposite of how it first reads:

```
134016  BSKP ONE 110 DA   ; skip next if bit 9 SET
134017  JMP -> 134034     ;   taken when bit 9 CLEAR  -> keep the SIZE
134021  BSKP ONE 120 DA   ; skip next if bit 10 SET
134022  JMP -> 134034     ;   taken when bit 10 CLEAR -> keep the SIZE
134024  ...               ; both SET -> compute the CHECKSUM
```

So the checksum is the narrow case: it is computed only when both flags are set, and the size
is the default. That fits the corpus, where 731 of 1449 frames carry a fixed `XMSIZ` constant.

**NARROWED 2026-07-31 (after the nd100-dis TX fix) - it is a PER-LINK property, not a message
option.** The rebuilt disassembler decodes the TX displacement field (bits 3-5), which was
previously hidden: `EL = (T & 0xFF)<<16 | (X + disp)`. That turns the opaque `LDATX` into a
named field access:

```
134004  LDA ,B 154        ; A := XMSEQ            (0o154 - the datagram sequence)
134005  JAP -> 134013     ; already assigned -> skip
134006  LDATX 4           ; A := phys[X+4]        the per-link counter
134007  STA ,B 154        ; XMSEQ := pre-increment value  <- THIS message's sequence
134010  AAA 1             ; A += 1
134011  AND 54            ; A &= 077777           <- FIFTEEN-bit mask (constant at 134065)
134012  STATX 4           ; phys[X+4] := A        write the counter back
134013  LDA ,B 144        ; A := XMSIZ
134014  STA ,B 142        ; XMCSM := XMSIZ
134015  LDATX 7           ; A := phys[X+7]        <- THE FLAG WORD
```

So the checksum flag lives at **offset 7 of the same per-link block** that holds the sequence
counter at offset 4. That makes it a **link/route property**, not a per-message option -
which **withdraws the earlier `XFSEC`/`XFROU` inference**: those are message options and
cannot be what is read here.

Still UNKNOWN: what that per-link block is and what bits 9/10 of its word 7 mean. `LDATX` is
privileged PHYSICAL addressing via the T:X pair, so the block is at a runtime physical
address. A live DAP capture with `X` known at `134015` would name it immediately.

`B` meanwhile is the **message-buffer base** - confirmed because `0o132`=`XMLIM`,
`0o133`=`XMCUR`, `0o134`=`XMTHD`, `0o144`=`XMSIZ`, `0o154`=`XMSEQ` all line up with the symbol
list.

**To finish this, capture it live over DAP** (both machines are up), the same way the resident
DATA cells were handled - see the `sintran-carving` resident doc. Another static pass will not
resolve a physical address chosen at runtime.

### Flags1 is a 15-bit counter [CARVED]

The mask at `134011` is the constant `077777` at `134065` = **0x7FFF**. So the datagram
sequence increments modulo **0x8000**, not 0x10000, and `XMSEQ` is assigned the value BEFORE
the increment (read, stash, +1, mask, write back), guarded so it happens once per message.

The corpus is **consistent but cannot confirm this**: the highest Flags1 on any Data frame is
`0x03D4`, far below either wrap point, and zero Data frames carry Flags1 >= 0x8000. The mask
is the evidence; the wire merely fails to contradict it. Anyone modelling wrap behaviour (the
envelope "epoch" term counts these wraps) must use 0x8000.

Reachability frames use `0xFFFF` as a link-start sentinel and are NOT the counter.

### Decoder note - fixed 2026-07-31

`nd100-dis` previously switched this group on `instr & 0xFFC7`, dropping the bits 3-5
displacement, so every TX instruction printed bare. The rebuilt version decodes it for all
seven variants (`LDATX`, `LDXTX`, `LDDTX`, `LDBTX`, `STATX`, `STZTX`, `STDTX`).

Audit of the change across the whole kernel: **116 of 236 TX instructions gained a
displacement, and NO mnemonic changed** - so the fix adds information rather than correcting
any earlier reading, and every conclusion in this document that predates it still stands.

**Bonus: this routine is the transported-header builder.** At `133727`, `LDA 125` reads
P-relative from `134054` - a genuine literal holding `020400` = the `0x2100` marker - and
`133730` stores it to `XMTHD`. That is the entry point for anything else about the transported
header.

This matters well beyond documentation: `XmsgEnvelope.BaseLowSigned` subtracts this field's low
byte, so on size-arm frames the envelope arithmetic is subtracting **a byte count**.

---

## 3c. "PacketType" at offset 2 is not a field [RESOLVED 2026-07-31]

Once the header is read as seven words, bytes 2 and 3 are one word, and offset 2 is simply its
high half. Measured across the corpus: **`0x00` on all 3595 frames**, and word 1 takes only
**eight** values, all of the form `0x00XX`:

```
0x0003  0x0007  0x000A  0x000C  0x000E  0x0013  0x0017  0x0019
```

- those are exactly the eight observed subtypes.

So there is no separate packet-type field. The subtype is a word whose high byte is always zero
because every subtype value fits in a byte, and "PacketType" was an artifact of parsing the
header as bytes. The inventory row is closed as **not a field** rather than left as UNKNOWN -
there was never anything there to learn.

(It follows that a *future* subtype above `0xFF` would occupy this byte, so a parser should read
the word rather than assume zero. But nothing in the corpus does.)

## 4b. Subtypes `0x0A` / `0x0C` - bulk file-transfer data [decoded 2026-07-31]

Found by a corpus scan that counted subtypes rather than assuming the documented five.
**226 FCS-valid frames each**, and they appear in **only four captures** - all of them
file transfers (`transfer-PULL-content`, `transfer-SPARSE-s3config`,
`transfer-SMALL-167bytes`, `transfer-file-COMPLETE`).

| | `0x0A` | `0x0C` |
|---|---|---|
| info length | **622, always** | **450, always** |
| Protocol ID | `0xD8` | `0xDA` |
| `Flags2` / `XMCSM` | `0x0406` | `0x0252` |

They carry a **normal XMSG sub-header** - `XMTHD` = `2100`, and `XMCSM` (wire 26-27) equals
`Flags2` exactly, as for every other frame - so the transported-header model applies to them
unchanged. Only the subtype and the fixed framing differ.

The payload is unmistakably **file content**. Stripping the parity bit from the high-bit
ASCII gives ordinary symbol-table text with CR/LF line endings:

```
c5 53 56 41 d2 bd 30 30 b7 b1 36 35   ->  "ESVAR=007165"
53 cc 41 4b 4b bd 30 30 30 b2 30 30   ->  "SLAKK=000200"
```

That is a `.SYMB` listing being pulled across the link. So these are the **bulk data path**
of a file transfer, distinct from the `0x0E` request/reply traffic - which is why every
earlier analysis missed them: they only occur once a transfer is actually moving content, and
every scan filtered on `subtype == 0x0E`.

### They are a PAIR, not two message types [2026-07-31]

The equal counts are not a coincidence. Dumped in arrival order per direction, they **strictly
alternate** and **every `Flags1` carries both**:

```
claude-transfer-PULL-content   dir 10362  n=84   ACACACACACACACACACACACACACACACACACACACAC
   Flags1: 0x1a1 0x1a1 0x1a2 0x1a2 0x1a3 0x1a3 0x1a4 0x1a4
   Flags1 values carrying BOTH subtypes: 42 of 42

claude-transfer-SPARSE-s3config dir 45164  n=356  ACACACACACACACACACACACACACACACACACACACAC
   Flags1 values carrying BOTH subtypes: 178 of 178
```

Same in all four transfer captures, in both directions. So **one datagram sequence number
produces two frames** - a `0x0A` of 622 bytes followed by a `0x0C` of 450 - and the two are
halves of a single logical transfer block rather than independent messages. `Flags1` is
therefore *not* one-per-frame on this traffic, which is worth knowing for anything that treats
it as a frame counter.

**Still UNKNOWN: why the split, and what distinguishes the halves.** Note the corpus cannot
answer the arithmetic questions: `XMCSM` is a *constant* on each side (`0x0406` for `0x0A`,
`0x0252` for `0x0C`) and both frame sizes are fixed, so nothing varies and any numeric relation
between `XMCSM` and the body lengths holds trivially. Resolving this needs a transfer captured
with a deliberately different block size, or the `*XFTRA` transfer loop carved.

## 4c. Marker2 `0xFD`/`0xFE` - a fourth frame family, NOT a subtype [OPEN]

Four frames in `li-rout-102-tree.pcapng`, from node 103, look like "subtype `0x17`" but are
nothing of the sort - **Marker2 is `0xFD` or `0xFE`, not `0x13`/`0x12`**:

```
21 fe 00 17 0066 0067 ffff fffd dd   + one trailing byte 0x1F
21 fd 00 17 0066 0067 ffff fffd dd   + one trailing byte 0x20
```

`Flags1` = `0xFFFF` and `Flags2` = `0xFFFD` (a negative XE* code shape).

**They are HEADER-ONLY frames [2026-07-31].** Their info length is **14 bytes = exactly seven
words**, so there is no body at all - what an earlier revision of this document listed as a
"trailing byte `0x1F`" is simply the low half of word 6, i.e. part of the checksum. That was a
byte-oriented mis-split, the same one that made the Counter look like a standalone field.

**Their header checksum validates under the ordinary rule** (verified as part of the 3595/3595
sweep), so the seven-word layout genuinely applies to them despite the unusual Marker2. That is
real evidence about the family rather than an assumption.

What remains UNKNOWN is what they MEAN: Marker2 `0xFD`/`0xFE` is outside the known set, so
offset 3 (`0x17` here) may not be a subtype field in this family, and `Flags2 = 0xFFFD` looks
like a negative XE* code but is unconfirmed. The dissector deliberately does not decode them -
see the marker-guard comment in `hdlc_tcp.lua`.

## 5. Sub-header

| Off | Field | Status | Note |
|---|---|---|---|
| 13 | Counter | **WIRE** | See section 3 - it is the LOW byte of header word 6, not a standalone byte. Earlier revisions of this document called it "between the two headers, belonging to neither"; that was the byte-oriented mis-framing. |
| 16-17 | `XMSTA` | **CARVED** | Low byte = `5M*` message state, high byte = `XF*` send options. Bit assignments confirmed from symbols. |
| 18-25 | `XMDSY`/`XMDPT`/`XMSSY`/`XMSPT` | **CARVED** | System and port. Port = magic low word: `(port << 7) | random`, `5PSHZ=7`, `5PMS1=177`. |
| 26-27 | `XMCSM` | **CARVED** | Section 4. |
| 28-29 | first body word | **WIRE** | **Not header.** Values are all application-layer: `0x07F0`/`0x07A2`/`0x07C0`/`0x07D2` (FA message types), `0x0041` XSLET, `0x014B` XSGSY, `0x0100` XRSOK. |
| 30-31 | (was called `XMLEN`) | **UNKNOWN** | `XMLEN=147` octal is five words past `XMCSM`, so this is body, not a header length. The "16-bit XMLEN" reading needs retiring. |

---

## 5b. Offset-12 hunt: what has been RULED OUT [2026-07-31]

Four independent strategies, all run against `XMSG-KERNEL-L03` and `XMSG-XROUT-L03`, all
negative. Recorded so nobody repeats them:

1. **Literal `0x2113` / `0x2112`** (`020423`/`020422` octal) - absent from both images, and no
   symbol in any symbol list carries the value.
2. **`SAA 41` + shift** (the marker's high byte `0x21` is octal `41`) - `SAA 41` occurs 5 times
   in XROUT and 0 times in the kernel, but every occurrence is immediately followed by a call,
   with neighbours `SAA 17`, `SAA 20`, `SAA 35`, `SAA 37`, `SAA 10`. Those are small numeric
   status/error codes passed in A, not a marker byte. **Red herring.**
3. **`SHA ZIN 10` + `ORA`** (the shift-left-8 byte-packing idiom). The most promising hit was
   in `XSDGM` itself at `137703`: `LDA ,X 16 / SHA ZIN 10 / ORA ,X 21 / RADD CLD SA DT`, which
   builds a genuine `high:low` word and passes it in T. **It is argument packing, not a header
   field** - the callee resolves through `mem[137745]` to `137515`, which is a **block-move
   helper**: it rounds a byte count up to words (`AAA 1 / SHA ZIN SHR 1`), unpacks T's high
   byte at `137524`, and runs a `MOVEW` loop. The XROUT sites are similar packing.
4. **Marker as a stored word anywhere in either image** - nothing.

Taken together these strengthen the earlier conclusion considerably: **the 7-word SINTRAN
header is not built by the XMSG kernel or by XROUT.** XMSG builds the transported header only
(`XMTHD`..`XMCSM`), and hands the message down.

**Next target: the RESIDENT image**, `versions/L-VSX-500/resident/SINTRAN-DATA_commoncode.bin`
(a `.dis` already exists beside it). A first pass finds several `SHA ZIN 10` sites and a word
`020423` at address `001005` - which is exactly `0x2113`, though it also disassembles as a
plausible `STD ,B 23`, so it must be confirmed by finding an instruction that references it
rather than trusted on sight.

## 6. Status - what is closed and what is left

### CLOSED this session (all carved, not fitted)

| Item | Result |
|---|---|
| Transported header layout | 7 words, `XMTHD`..`XMCSM`, wire 14-27 |
| `XMCSM` word size | ONE word at 26-27; `Flags2` is a copy of it (1449/1449) |
| Body start | wire offset 28 |
| SINTRAN header shape | SEVEN words; unified the Data and ACK models |
| **Header offset 12 + 13** | **the two halves of a ones-complement header CHECKSUM (3595/3595)** |
| `XMCSM` two arms | `XMSIZ` by default, checksum when two flag bits are set; both routines carved |
| `Flags1` | `XMSEQ`, assigned from a per-link counter, masked to **15 bits** |
| Header offset 2 | not a field - high half of the `type:subtype` word |
| Subtypes `0x0A`/`0x0C` | bulk file-transfer data, and a PAIR sharing one `Flags1` |
| `0xFD`/`0xFE` frames | header-only (14 bytes = 7 words); checksum validates normally |

### OPEN - needs the live machines or a new capture

1. **What selects the `XMCSM` arm.** Narrowed to bits 9 and 10 of `phys[X+7]`, a word in the
   same per-link block whose sequence counter sits at `X+4`. `LDATX` is physical addressing, so
   this is a runtime address - **static carving cannot finish it**. A DAP breakpoint at
   `134015` with `X` known would name it in one shot.
2. **What the `0xFD`/`0xFE` family means.** Layout confirmed, semantics unknown.
3. **Why `0x0A`/`0x0C` split into a pair.** The corpus cannot answer it: `XMCSM` and both frame
   sizes are constant, so nothing varies. Needs a transfer captured at a different block size,
   or the `*XFTRA` transfer loop carved.
4. **LAPB address `0x07`** on some ACK I-frames.

### OPEN - implementation debt (no research needed, just work)

5. **Retire the 32-bit `ControlService`.** It straddles `XMCSM` and the first body word; 108
   call sites across 34 files. Documented on `XmsgDataFields.ControlService`.
6. **`SintranProtocolId` member names are misnomers.** `Tad`/`Routing`/`Pad` name traffic, not
   a selector. Renaming is a broad breaking change - flagged, not done.
7. **Delete the superseded `XmsgEnvelope` members** (`LearnSeed`, `BaseLow`, `BaseLowSigned`,
   `ComputeCounter`, `ComputeEpoch`, `DeriveChannel`, `ChannelAnchor`) once callers migrate.
8. **Install the fixed `hdlc_tcp.lua`** into `C:\Program Files\Wireshark\plugins` (needs admin).

### OPEN - application layer, barely started

9. **QFORM field selectors** - needs the Ghidra FA handlers read.
10. **The COSMOS FA operations** (`Open-file`, `Read-file`, `Write-file`, `Reserve-file-entry`,
    `SIII-special`) - named in `cos-fa-serv-e04.prog` but not decoded.
11. **An `*XFTRA` client in `Xmsg.Api`** - does not exist; `p11`/`p16` are now known.

## 7. Disassembly: started, and the tool was broken

Target: what writes SINTRAN header offset 12. Findings so far:

- **The marker `0x2113` is almost certainly COMPUTED, not a literal** - which is why no scan
  finds it. Evidence: `XMTHD` is built from a genuine literal `0x2100` (`134054`, stored at
  `133730`); the SINTRAN header marker is `0x2113` and its relayed variant is `0x2112`. Same
  high byte `0x21`, low byte varying with hop - that is a constructed word, so searching for
  the assembled value was never going to work. **Look for code that builds on a `0x21` high
  byte instead.**
- Scanning the carved L segments for `0x2113`/`0x2112` returned **92 word-aligned candidates**
  with ordinary code contexts - unusable without narrowing. (`S3SMPIT`/`S3IMPIT` show a
  repeating `2c11 2111 2c13 2113` run that is clearly a table or code, not a marker.) This is
  the noisy-scan problem again.
- Segment inventory for the next pass: `076-S3XMK` is the installed XMSG kernel,
  `077-S3XROU` XROUT, `036-S3TAD` TAD, `135-XFTRAD` the file-transfer daemon. There is **no
  HDLC/network segment**, so the wrapper builder is most likely in the RESIDENT code
  (`SINTRAN-DATA_commoncode.bin`), which already has a `.dis` alongside it.
- **A constant scan is NOISY - verify each hit.** `020400` (the `X5THD` marker) is also the
  encoding of `STD 0,B`. Of six hits in the kernel, three were ordinary instructions and
  `134054` was the **genuine literal** - loaded P-relative by `LDA 125` at `133727` and stored
  to `XMTHD`, which is what identified the transported-header builder. An earlier revision of
  this document said all six were code; that was a generalisation from checking three.
  Confirm each hit by finding the instruction that references it, and prefer navigating by
  symbol.
- Useful symbols located in the kernel image: `XSDGM` 137601 (send datagram), `XSCTR` 137562,
  `XSACK` 140076, `XSFOR` 137560, `ZDCHN` 122204.

### Use `nd100-dis`, not the Python script

**Tool of record: `/home/ronny/repos/nd100-tools/nd100-dis` (WSL).** A real C disassembler
with full instruction decode plus IO-device and MON-call tables. `tools/nd100dis.py` in this
repo is a ~140-line ad-hoc script whose own header says it is "minimal, focused on control
flow / MON / IOX / status polls" - it should not be used for carving.

`nd100-dis` does **not** recognise this BPUN variant (it falls back to raw and mis-frames the
file - reports 23833 words where the loader places 23551), so the working recipe is to
flatten first and disassemble the flat image:

```
python bpun2raw.py XMSG-KERNEL-L03.BPUN kernel-l03.bin      # uses tools/bpun_load.py
nd100-dis -a -b 40960 -s <offset> -n <count> kernel-l03.bin
```

**`-b` and `-s` are parsed as DECIMAL** - the `-o` flag only affects output formatting. Base
`0o120000` is `-b 40960`; an address `A` needs `-s (A - 0o120000)` in decimal. Passing
`-b 120000` silently wraps mod 2^16 and lands you at `0o152300` with every address wrong.
Verified: `-b 40960 -s 4653` puts `ZCRMG` at `131055`, matching the published carve.

It is also more precise than the hand decodes: `174220` reads `BSET ONE SSK` (naming the K
flag) where the doc guessed `BSET ONE 2 D0`, and `146175` reads `RADD CLD SX DA` - which is
what `COPY` is an alias for.

**A defect in `tools/nd100dis.py`, found and fixed 2026-07-31** (kept because `disbpun.py`
and other scripts import it). The register-operation test read
`(w & 0o170000) == 0o144000`, which can never be true because `0o144000 & 0o170000` is
`0o140000`. `decode_reg` was dead code and the **entire ROP group fell through to the SKP
fallback** - `146151` printed as `SKP IF DD LSS SD` where it is `COPY SA DD`. **Any earlier
automated disassembly from that script mis-decoded every register operation.** Hand decodes
in the carve docs are unaffected. The fix also corrects the ROP register table, which is
**not** the memory-reference one: `{0:'0', 1:'D', 2:'P', 3:'B', 4:'L', 5:'A', 6:'T', 7:'X'}`,
calibrated against the three published hand decodes and verified to reproduce them exactly.

## 8. Method note

Everything in section 1 came from reading the kernel's own symbol table and then checking it
against traffic. Nothing in it came from fitting curves to captures — an earlier attempt this
session did exactly that, produced a confident and wrong explanation, and had to be withdrawn.
Where a row below says WIRE rather than CARVED, that gap is the point: it marks a field we can
reproduce but cannot yet explain.
