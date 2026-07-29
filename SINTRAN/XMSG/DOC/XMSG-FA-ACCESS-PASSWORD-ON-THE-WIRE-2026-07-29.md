# Remote file access working, and the password word on the wire (2026-07-29)

Two firsts in one capture:

1. **A COSMOS file-access request that SUCCEEDS.** Every earlier capture died at the opening
   letter with `XRNRO` because no peer existed. Here node 102 lists a real user's files on node
   100 over HDLC and gets the listing back.
2. **The SINTRAN password fold, confirmed on the wire.** The algorithm was carved from the
   L-VSX-500 disassembly and verified only against *stored* user-table words. This is the first
   time the same 16-bit word has been seen travelling between two machines - and it proves the
   password is never sent as text.

---

## 1. The setup [VERIFIED]

Three RetroCore machines, star topology, node 100 in the middle:

| Node | SINTRAN | Console | Link |
|---|---|---|---|
| 100 | K | 9010 | listens 10362 + 10364; has user `SECRET`, password `secret` |
| 102 | K | 9102 | HDLC to 100 on 10362 - the CLIENT |
| 103 | L | 9003 | HDLC to 100 on 10364 - no COSMOS file access (revision-F gate), TRANSFER-FILE only |

The working direction is **102 -> 100**. On 102:

```
LIST-FILES d100(secret(secret)).,,
FILE 0 : D100.(PACK-ONE:SECRET)TXT1:SYMB;1
```

Syntax note: the remote password goes in **nested parentheses after the user name**,
`d100(user(password))`. Omitting it, or giving the wrong one, answers `WRONG PASSWORD`.

## 2. The controlled experiment [VERIFIED]

Three requests, changing only the password:

| Password given | Result |
|---|---|
| `secret` (correct) | the file listing |
| `orange` (wrong) | `WRONG PASSWORD` |
| none | `WRONG PASSWORD` |

`orange` was chosen deliberately: `ORANGE` is one of the published test vectors in
`PASSWORD-ALGORITHM.md`, so its expected word was already known independently.

Expected words from the carved fold `acc = ROL16(acc,3) + toupper(c)`:

| Password | Decimal | Octal | Hex | Wire bytes |
|---|---|---|---|---|
| `secret` | 27946 | 66452B | **0x6D2A** | `6D 2A` |
| `ORANGE` | 14378 | 34052B | **0x382A** | `38 2A` |

## 3. The result: one byte [VERIFIED]

The correct-password request and the wrong-password request are **byte-identical apart from the
password word**. Full byte diff of the two raw HDLC frames (146 bytes each):

| Raw offset | correct | wrong | What it is |
|---|---|---|---|
| 12 | `29` | `39` | Flags1 - envelope, changes per frame |
| 16 | `7b` | `6b` | Counter - envelope, derived |
| 28 | `15` | `72` | XMSPT low byte - a new source port per attempt |
| 34 | `3f` | `40` | XMLEN low byte - see caveat below |
| **106** | **`6d`** | **`38`** | **the password word, high byte** |
| 143-144 | - | - | FCS |

Offset 107 is `2a` in BOTH, because `secret` = 0x6D**2A** and `ORANGE` = 0x38**2A** happen to
share a low byte. Coincidence, but it makes the diff a single byte.

**So the password word sits at raw offset 106-107** (SINTRAN-header-relative 103, trailer offset
71), immediately after a `B0 10` tag. Nothing else in the request moves.

That is simultaneously:

- **the location of the password field**, pinned by single-variable variation, and
- **live confirmation of the fold algorithm** - `ORANGE` produced exactly the 0x382A that the
  disassembly-derived implementation predicts, on the wire, on a different machine, three years
  of abstraction away from the carve.

**The plaintext never appears.** Neither `secret` nor `orange` is anywhere in the frame. The
client folds locally and sends the word.

## 4. The rest of the request [PARTIALLY DECODED]

The frame carries far more than the opening `XSLET` letter did:

```
XMDSY/XMDPT = 100 : 1453      (port 11, random 45)
XMSSY/XMSPT = 102 : 1557      (port 12, random 21)
XMCSM       = 0x007007F0      Flags2 = 0x0070, a class we had not seen
trailer     = tagged fields, NOT XROUT parameter tagging
```

Readable strings in the trailer: `BAK03  SYSTEM` (the local background process and user) and
`SECRET` (the remote user). The tag vocabulary (`92`, `f2`, `a2`, `8c`, `bd`, `b0`, `e1`) is a
different scheme from XROUT's parameter tags - most likely the QFORM tagging described in the
Ghidra carve of `cos-fa-serv-e04`. Decoding it properly is the next job; the carve
(`COSMOS-RE/Analysis/COS-FA-SERV-E04-Analysis.md`, 13 operations and typed-param protocol) is
the reference to check it against.

## 5. How a rejection is signalled [VERIFIED]

The reply distinguishes accepted from rejected by **adding a tagged error field**, not by changing
what is already there. Comparing the reply to the good request against the reply to the bad one:

```
accepted:  ... 92 0002  92 0001                 f2 00FF
rejected:  ... 92 0002  92 0001  f2 0001 A2 0030  f2 00FF
```

`A2` introduces a 16-bit integer; the value is `0x0030` = 48 decimal = **060 octal**, which the
SINTRAN III Reference Manual lists as **"Wrong password"** (ND-60.128.5, the octal/decimal error
table). So the file server reports failures using the ordinary SINTRAN file-system error numbers,
carried inside the FA reply rather than as an XMSG-level status.

The XMCSM class also moves - `0x0012` on the accepted reply, `0x0018` on the rejected one - so
there appear to be two independent signals. The error field is the one to read, because it says
*which* failure; the class only says that the shape differs.

For a client, then: **look for an `A2` field in the reply and treat its value as a SINTRAN error
number.** Absence of that field is success. Confirmed for the wrong-password case; other error
numbers (no such user, no such file, no access) have not yet been elicited, so treat the general
rule as [INFERRED] and the 48 = wrong password mapping as [VERIFIED].

## 6. The QFORM tag encoding, confirmed from the wire [VERIFIED]

The Ghidra carve of `cos-fa-serv-e04` read four emitter tag bytes straight out of the binary
(`0x92` INT16, `0x94` INT32, `0xA2`, `0xF2`) and proposed - marked `[INFERRED]` - that a tag is

```
tag = (type_class << 4) | length_in_bytes
```

The capture confirms that rule, using fields whose contents are known independently:

| Tag | Low nibble | Bytes that follow | Content | Verdict |
|---|---|---|---|---|
| `BD` | D = 13 | 13 | `BAK03  SYSTEM` - exactly 13 characters | length rule holds |
| `B0` `10` | 0 = escape | 16 | `SECRET` + NUL padding - exactly 16 | escape form |
| `92` | 2 | 2 | INT16 | matches the carve |
| `A2` | 2 | 2 | the error number (see section 5) | matches the carve |
| `F2` | 2 | 2 | field/selector | matches the carve |

Two results:

1. **The `(class << 4) | length` encoding is no longer inferred.** A 13-character string carried
   under tag `0xBD` cannot be coincidence, and it was not derivable from the emitter table alone -
   the carve only ever saw fixed-length emitters (2 and 4 bytes), so nothing in the binary
   exercised the length nibble across its range.
2. **A length of 0 is an escape: the NEXT byte is the real length.** `B0 10` introduces a 16-byte
   value. The carve did not have this - it could not, since no fixed-length emitter needs it. This
   is what lets QFORM carry values of 16 bytes and up in a 4-bit length field.

The carve also flagged that the request-PARSE side compares tags against `0x01/0x10/0x80` and
warned those might be internal type indices rather than wire bytes, `[UNVERIFIED]`. The capture
answers that too: the request on the wire uses the **same** `92/A2/F2/BD/B0` vocabulary as the
reply, so `0x01/0x10/0x80` are indeed internal indices and not raw wire tags.

Not everything parses yet. The trailer opens `80 00 00 01` and contains an `E1` and several `8C`
tags whose length nibbles do not obviously fit the surrounding structure, so the grammar above is
confirmed for the tags listed and **not yet complete**.

### 6.1 The trailer is NOT a flat tag list [VERIFIED NEGATIVE]

The obvious next move - walk the whole trailer as `tag, value, tag, value...` - was tried against
every data frame in the capture and **fails**:

| Walk started at trailer offset | Frames walked | Clean walks |
|---|---|---|
| 0 | 65 | **21** |
| 2 | 65 | 19 |
| 4 | 65 | 14 |
| 6 | 65 | 10 |
| 8 | 65 | 14 |

Two thirds of frames cannot be walked that way, and no fixed prefix rescues it - skipping bytes
only makes it worse, so the failure is not a missing header.

The tag histogram from those walks shows what is actually going wrong: it is full of "tags" in
class 4 (`41`, `42`, `43`, `45`, `4B`, `4E`...), which are simply the ASCII letters `A`, `B`, `C`,
`E`, `K`, `N`. The walker is marching into **string payloads and reading their characters as
tags**. That only happens if the reader is out of step with the real structure.

So: the `(class << 4) | length` rule is right for the individual fields proven by content, but the
container around them is not a flat list. Something - most likely a per-operation record layout, or
a structural/nesting meaning for class 8 - decides where tagged fields begin and end. **Do not
build a codec on a flat walk**; it would silently mis-parse two frames in three rather than fail.

A second hypothesis was tried and also fails. The carve records that the EMIT side uses tags
`92/94/A2/F2` while the PARSE side compares against different indices, which predicts that
**replies** might be flat even if requests are not. Walking the two directions separately:

| Direction | Data frames | Clean walks |
|---|---|---|
| replies (from the server) | 32 | 9 |
| requests (from the client) | 33 | 12 |

Both fail at about the same rate, so the framing is not direction-dependent either.

Closing this needs the request-parse side of the carve read properly:
`fa_parse_request_params` (0x29c0) and the dispatch tables `g_fa_param_dispatch_table` (0x9039) /
(0x9044), which is where the real framing is decided. That requires the `cos-fa-serv-e04` binary
loaded in Ghidra - as of this writing only the two 68k Ethernet/octobus images are open, so it
could not be read here.

**Status: byte-level inference on this capture is exhausted.** Two plausible models were tested
against all 65 data frames and both were falsified. The next step is disassembly, not more
staring at frames - and the capture is now the oracle to check that disassembly against, which is
a better position than either source alone.

### 6.2 SOLVED: F2 is a field SELECTOR, and 0xFF ends the list [VERIFIED]

Loading `cos-fa-serv-e04` into Ghidra settled it immediately. `fa_process_params_dispatch`
(0x35da) does this:

> loops over up to 30 (0x1e) parameter fields; for each, reads its tag byte; **if the tag is not
> `0xFF`** (the sentinel) and is in range, computed-jumps through the tag-indexed table
> `g_fa_param_dispatch_table` (0x9039).

So the body is not a flat list of self-describing values - it is a **selector/value stream**, and
`0xFF` terminates it. That is why a flat walk desynchronised: it treated selectors as values.

The capture confirms it exactly. Counting `F2 <id>` occurrences across every frame:

| Selector | Occurrences | Meaning |
|---|---|---|
| `F2 00FF` | 20 | **end of parameter list** - the carve's sentinel |
| `F2 0001` | 14 | field 1 |
| `F2 0002` | 12 | field 2 |
| `F2 0003` | 5 | field 3 |
| `F2 0004` | 5 | field 4 |

Small sequential field numbers plus a `0xFF` terminator - a parameter list, exactly as the
dispatcher describes. The 20 frames carrying the sentinel are the FA protocol frames; the rest of
the capture is acks and control.

Walking the request from section 1 with this model finally parses:

```
8000 0001 92 0002 92 0001     header, before the first selector
F2 0001   A2 07D0             field 1 = 2000
F2 0002   8C 06 92 0001 ...   field 2
F2 0003   BD "BAK03  SYSTEM"  field 3 = string(13), the local process and user
F2 0004   ... "SECRET" ... 6D2A   field 4 = the credentials block: remote user AND password
F2 00FF                       end
```

Field 4 carries **both** the remote user name and the folded password word - which is why the
password sits where it does, and why only that one byte moved when the password changed.

Remaining: the exact value grammar inside fields 2 and 4 (the `8C` and `E1` tags), and what the
`8000 0001 92 0002 92 0001` header means. But the container is no longer a mystery, and a reader
can now be written that finds field boundaries reliably instead of guessing.

## 6.3 CORRECTION: the tag rule is right for classes 1-7 only [VERIFIED]

Section 6 above says the `(class << 4) | length` encoding "is no longer inferred". That was
overstated. Disassembling the reader itself - `qform_read_tag_and_value` at `ram:0x7d01`, found by
following `fa_process_params_dispatch` (0x35da) through `0x3630` and `0x295b` - gives the real
rule, with the masks read out of its literal pool at `0x7d82..0x7d89`:

```
bit 7 CLEAR                    -> END OF STREAM            (BSKP 7 @ 0x7d14)
class = (tag & 0x70) >> 4                                  (mask @ 0x7d82/0x7d83)

class 1..7 : length  = tag & 0x0F                          (mask @ 0x7d84)
class 0    : subtype = tag & 0x17   <- NOT a length        (mask @ 0x7d85)
             length is ALWAYS escaped into the next byte

escape: a following 0x80 continues accumulation; the first non-0x80 byte is the length
scalar reader rejects length > 4  -> error 0x1FC4
overrun                           -> error 0x1FC6
```

The evidence in section 6 was sound - `0xBD` really does carry 13 bytes and `0xB0 0x10` really does
carry 16 - but both are class 3, so they only ever tested the classes-1-to-7 branch. The
generalisation to all tags was mine, and it was wrong. It could never have explained `0x80` or
`0x8C`.

### 0x8C is a NESTED RECORD - and that is why the flat walk failed [VERIFIED]

`0x8C` = class 0, subtype 4, escape length. So `F2 0002  8C 06  92 0001 ...` reads as: select
field 2, then a subtype-4 value **6 bytes long whose contents are themselves tagged**.

`0x8C` is a **constructed, length-delimited sub-record**. The flat walker descended into nested
payloads and read them as top-level tags, which is exactly the desynchronisation measured in
section 6.1 - and exactly why the failures looked like ASCII letters being mistaken for tags.
`0x80` is the same mechanism with subtype 0.

### 0xE1 is decodable but its meaning is unknown [PARTIAL]

`0xE1` = class 6, length 1: one header byte, one value byte. A parser can read or skip it correctly
today. What class 6 *means* is **not determined** - there is no `0xE1` emitter in this binary and
nothing found assigns semantics to class 6. Recorded as unknown rather than guessed.

### A fifth emitter the earlier carve missed [VERIFIED]

Searching the emitter literal-pool signature finds **five** emitters, not four:

| Emitter | Addr | Tag | Class | Value |
|---|---|---|---|---|
| `msg_put_param_word` | 0x7a55 | `0x92` | 1 | 2 bytes |
| `msg_put_param_dword` | 0x7a91 | `0x94` | 1 | 4 bytes |
| `msg_put_param_typed_a_word` | 0x7acd | `0xA2` | 2 | 2 bytes |
| **`msg_put_param_typed_a_dword`** | **0x7b09** | **`0xA4`** | **2** | **4 bytes - NEW** |
| `msg_put_param_typed_c` | 0x7b45 | `0xF2` | 7 | selector |

There is **no `0x8C` and no `0xE1` emitter**: this server never writes them. They are read-side
only, produced by the SINTRAN client. That matches the capture, where both appear in the request
and neither in the reply.

### Why the dispatch tables looked like code [RESOLVED]

An earlier attempt to dump `0x9038` returned instruction bytes. The tables are in the **data bank**,
not `ram:`. In `BANK2` they are exactly what the dispatcher describes: `9038` = bound 3, `9039..903c`
= handlers for invalid/field-1/field-2/field-3, and a second table at `9043`/`9044` with bound 1 -
which is where wire field 4, the credentials block, must land, since it exceeds the first table's
bound.

The reply-side table at `903d` clinches the selector model independently: each entry simply loads a
field number 0/1/2/3, and `0x3847` loads `0xFF`.

This also settles the carve's own `[UNVERIFIED]` caveat: the `0x01/0x10/0x80` values compared in
`fa_parse_request_params` are **field/operation numbers**, not wire tags.

### Still open

The `80 00 00 01` opener does **not** decode cleanly with the real rule: `80` plus escape byte `00`
consumes two bytes for an empty subtype-0 marker, and the next byte `00` has bit 7 clear, which
means end-of-stream - yet the message plainly continues. So either the body handed to this reader
starts after a 4-byte non-QFORM preamble, or the grouping in section 1 splits it wrongly. Not
resolved; flagged rather than smoothed over.

Also unresolved: the multi-byte (>= 128) escape-length accumulation at `0x7d48`. The single-byte
case is confirmed against the wire (`B0 10` = 16); no captured frame exercises the continuation.

## 7. Caveat

`XMLEN` reads `0x3F` on one request and `0x40` on the other while the visible trailer is the same
length. Not explained. Flagged rather than hand-waved.

---

## Provenance

Capture: `E:\Dev\Ronny\X25Emulator\pcap\fa-access-secret-102-to-100-2026-07-29.pcapng`
(128 payload frames, both links). Correct-password request = frame 9; wrong-password requests =
frames 138 and 164.

Fold algorithm:
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\PASSWORD-ALGORITHM.md`
(the implementation used here was self-checked against all three of its published vectors -
`ORANGE`, `TIGER42`, `sky-9` - before being applied).
