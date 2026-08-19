# POCSPROCES event dispatch + the start-net directory responder

**Firmware:** `encos-ser-all-banks-68k.bin` (ENCOS Ethernet II controller, maps flat from 0).
**Method:** read-only Ghidra disassembly, 2026-08-10. Everything here is VERIFIED from the image
except the two items marked INFERRED/NEXT. Function names in `code font` are the genuine ND vendor
names from the embedded symbol table (or a prior RE pass); addresses are hex.

This decode answers the question "how does the card decide which XROUT reply to send?" for the
Ethernet II HLE bring-up. Short answer: **the card routes on an EVENT BITMASK, never on the received
XROUT record type.**

---

## 1. The event-dispatch machine (`POCSPROCES @0xE380`, dispatch @0xE52E)

The server loop reads a pending-event bitmask from global **`0x1E1CA`** (and clears it), then splits
it into three fields and calls one field-dispatcher per field:

| Mask field | Bits | Field-dispatcher (ND name) | Address | Role |
|---|---|---|---|---|
| `0x0000007f` | 0-6   | `LNMAEVENTS` | `0x6DA8` | LAN-master events |
| `0x00ff0000` | 16-23 | `LNCNEVENTS` | `0xB1EE` | LAN-connection / **XROUT directory ops** |
| `0xbf000000` | 24-31 | `CSEVENTS`  | `0xE24E` | COSMOS routing events |

`LN` = LAN, `CS` = COSMOS. Each field-dispatcher tests individual bits of its field and
`bsr`/`jsr`s the matching sub-handler; an unhandled bit is simply skipped. So the whole COSMOS
routing/directory behaviour is a set of ~20 independent event handlers, one per bit.

### 1a. `LNCNEVENTS @0xB1EE` sub-handlers (bits 16-23) - the directory group

| Bit | Mask | Sub-handler | Address |
|---|---|---|---|
| 21 | `0x200000` | **`PocsCommandSubprocessMsgLoop`** (POCSSPCOMM) | `0xA6BA` |
| 23 | `0x800000` | `Possible_LnCnCommandSubprocessMsgLoop` | `0xA976` |
| 22 | `0x400000` | `LnCnDataSubprocessMsgLoop` | `0xA79E` |
| 20 | `0x100000` | `LnMaCommandSubprocessMsgLoop` | `0xA5B6` |
| 19 | `0x80000`  | `LnMaProcessDataCompletions` | `0xA23A` |
| 18 | `0x40000`  | `LnRetransmitTimerService` | `0x92F8` |
| 16 | `0x10000`  | (continues) | - |

### 1b. `CSEVENTS @0xE24E` sub-handlers (bits 24-31)

| Bit | Mask | Address | Note |
|---|---|---|---|
| 26 | `0x4000000`  | `0xD668` | |
| 28 | `0x10000000` | `0xDB32` | |
| 27 | `0x8000000`  | `0xD804` | |
| 29 | `0x20000000` | `0xD5E0` | gated on global `0x1E16C` |
| 31 | `0x80000000` | `0xD4C0` | |
| 24 | `0x1000000`  | `0xD5A2` | then tests global `0x1E22E` |

### 1c. `LNMAEVENTS @0x6DA8` sub-handlers (bits 0-6)

Verified so far: bit0 (`0x1`) -> `0x64C4`; bit5 (`0x20`) -> `0x5AC6`; bit3 (`0x8`) -> (continues).

---

## 2. The directory responder = `PocsCommandSubprocessMsgLoop @0xA6BA`

This POCS command sub-process (POCSSPCOMM, `LNCNEVENTS` bit 21) is the port-4 command loop that
answers the start-net-server directory sync. Matched against the oracle capture
(`ETHII-ORACLE-STARTNET-CAPTURE-2026-08-09.log`, lines 18610-19326) it does, per round:

```
XFRCV(4)                        receive the letter on port 4
XFREA @0x1D30                   read the incoming XROUT record (e.g. [0x0400] directory query)
XFMST(A=0xFFFF)                 status of the CURRENT (just-received) message - not a live handle
XFWRI @0x1E30                   build the reply record (e.g. [0x054A]{0x0102:peer}{0x0202:own sysid})
XFSND(T=0x260C = XFRRO|XFROU|XFSEC)   send to the kernel magic via XROUT
```

The XROUT record header (`serial<<8 | service`, e.g. `0x054A` = serial 5 + service 0x4A = XSDSY) is
**composed from fields, not a literal** - which is why searching the image for `0x054A` finds nothing
aligned. `0x260C` differs from the conn-to accept's `0x020C`: the directory reply routes through
XROUT (`XFROU`) as a remote-XROUT send (`XFRRO`).

### 2a. Inside `PocsCommandSubprocessMsgLoop` (decoded 2026-08-10)

It is a subfunction dispatcher, same shape as the transmit DATA path (skill sec 2):

```
@0xA6EE  subfunction = node[0x0A] >> 2   (& 0x3f)
         bounds-check vs byte @0x1D200 (= 0x0D = max 13)
         jump via longword table @0x1D202 [subfunction*4]
```

The dispatch table `@0x1D202` has 14 entries, and in the static image **all 14 point to the same
common handler `@0xA75E`** (inside this function). `0xA75E` recomputes the subfunction, copies
`node[0x0C]` to its frame, then per work item `bsr 0x80AA` and iterates the list.

**Dead end for the reply, useful negative result:** `0x80AA` = `maybe_report_event_ID` - it builds a
param block tagged `0x4944` ("ID") + the subfunction and calls kernel `0x11F78`. It is an
event/ID-registration helper, **NOT** the XMSG `0x054A` reply builder. So the directory reply is
composed on a different path, not down this subfunction table.

The buffer-search / top-down drill both dead-ended; the reply-selection was found on the XROUT
processor path instead (next section).

---

## 2b. `PROCESSXRO @0xCD4A` - the XROUT TRACE/STATUS handler (NOT the directory responder)

> **CORRECTION 2026-08-10c.** This section originally claimed `PROCESSXRO` was the start-net directory
> responder. That is WRONG and retracted. After decoding its `0xD050` arm it is clear `PROCESSXRO`
> only acts on parameter tags `0x0A`, `0x62`, `0x63` (`0x63` sub-op 3 returns the `0x2708`-byte
> `POCSTRACEB @0x2AB5E` trace buffer). The start-net directory query uses tags `0x11/0x27/0x03/0x04`
> plus `{0x0A02:1}`=op1, and **op1 composes no reply params here** - so PROCESSXRO does NOT build the
> `{0x0102}/{0x0202}` / `0x054A` directory reply. It is the XROUT trace/status handler. The directory
> responder is a DIFFERENT function, STILL UNIDENTIFIED (try `PROCESSXGA` / `PROCESSXMS`, or another
> `LNCNEVENTS` sub-handler; find the code that emits params `0x0102/0x0202` and header `0x054A`).
> The mechanism decoded below (parameter walk via the `0xBFF8` read engine, op dispatch, identity from
> runtime globals) is real and reusable - only the "this is the directory responder" framing was wrong.

`PROCESSXRO` is an XROUT record processor: the home of ONE incoming-record -> reply mapping (the
trace/status one).
It **walks the received XROUT record parameter-by-parameter**:

```
index = 4                                   ; start displacement (skip the 2-word header)
while index < recordLength (0x1c) and !flag:
    bsr 0xBFF8   with a READ descriptor { T=1, A=0xFFFF (current msg), disp=index, len=4,
                                          buf = frame 0x24 }     ; pull the next parameter
    tag  = byte (0x24)      ; parameter number
    val  = word (0x26)      ; parameter value
    if tag == 0x0A:         ; the 0x0A02 OPERATION-SELECTOR parameter
        switch (val):       ; <-- THE reply selection
            1  -> @0xCDE8
            2  -> @0xCE00   builds {tag=0x11, len=0x02, value = global 0x1E222}  (= {0x1102:sysid})
            3  -> @0xCE36
            0x15 -> @0xCE4E builds {tag=0xE4(-0x1c), ...}, jsr 0x13286
    else: other-parameter handling @0xCF8C
```

Key facts:
- The `0xBFF8` builder is used **both** to READ record parameters (`T=1` descriptors) and to compose
  the reply - it is the single XFSMC engine for the whole XROUT conversation.
- Reply selection is driven by the received record's **`0x0A02` operation-selector value**, not by the
  record header. The reply record header (`serial<<8 | service`, e.g. `0x054A` = serial 5 + service
  0x4A XSDSY) is COMPOSED from these fields - which is why `0x054A` appears as no literal in the image.
- **Identity is stamped from two globals:** `0x1E222` (here, in `PROCESSXRO`) and `0x1E21A`
  (`XMSGIOCGAT` sysid). Both are runtime values; never hardcode `0x2648`.

### 2c. The op arms (decoded 2026-08-10)

For tag `0x0A` (the `0x0A02` operation selector), `PROCESSXRO` switches on the op value `(0x26,A6)`.
The reply is staged at frame `0x24`, its byte-length in `(0xB8,A6)`, its pointer in `(0xB0,A6)`:

| Op value | Addr | Reply composed |
|---|---|---|
| 1 | `0xCDE8` | clear `node[1]`; reply len 0 (no parameters) |
| 2 | `0xCE00` | `{tag=0x11, len=0x02, value = global 0x1E222}` -> `{0x1102: own sysid}`; reply len 4 |
| 3 | `0xCE36` | clear `node[1]`; reply len 0 |
| 0x15 (21) | `0xCE4E` | the big directory record: copies 3 longword param blocks from name/string source `0x1E171` (if global `0x1E16C != 0`) or `0x1E19D` (else) via `jsr 0x13286`; reply len `0x32` (50 bytes) |
| default | `0xCF7A` | `node[1] = 0x2B` (reject); reply len 0 |

Other parameter tags are handled too: tag `0x62 @0xCF8C` stores `(0xBA)` into global `0x1E23A` and,
on value 0, reads the mode/config word `0x1888C` and sets reply len `0x38`/`0x3A`; anything else falls
to `@0xD050`.

**Identity / name globals** used to compose replies (all runtime, never hardcode): `0x1E222` and
`0x1E23A` (system numbers), `0x1E171` / `0x1E19D` (name/string blocks), plus `0x1E21A` (the XMSGIOCGAT
sysid used elsewhere) and the mode word `0x1888C`.

### 2d. What the HLE start-net responder must do

Per received record, walk its XROUT parameters; on the `0x0A02` operation selector, compose the reply
parameter set that op value dictates (table above), stamping the card's own identity from its
XMSGIOCGAT sysid and echoing peer sysnums; then XFWRI the composed reply over the current message and
XFSND it back (`T=0x260C = XFRRO|XFROU|XFSEC`) to the sender magic from `XFMST(A=0xFFFF)`. This is a
stateful multi-round loop - keep going until the query stream on port 4 drains.

---

## 3. The outer XFSMC (multicall) register mapping - CORRECTED

Decoded at the outer XFSMC trap in `FUN_0000c044 @0xC1B4`. The multicall arg block (base = `A6+0x8a`,
words T/A/D/X + user32bitAddress) is:

| Slot | Offset | Value |
|---|---|---|
| T   | `0x8a` | `0x24` (XFSMC) |
| A   | `0x8c` | **unused** |
| D   | `0x8e` | **unused** |
| X   | `0x90` | **NCALLS** (descriptor count) |
| user32bitAddress | `0x92` | **&descriptor array** |

So the multicall carries the descriptor COUNT in **X** and the array pointer in the
**user32bitAddress** field - not count-in-D. (RetroCore's `XmsgClient.PostMultiCall` had count-in-D
and 0-in-X, which the kernel reads as NCALLS=0 = "re-execute the previous multicall" - so the
descriptors never ran. Fixed 2026-08-10.)

Cross-check: the XFSCM leaf wrapper `@0x10EAA` (`moveq #8`) uses the same T/A/D/X word ordering with
options OR'd into the T word - confirming the trap arg-block layout.

---

## 4. What this means for the HLE

- The conn-to **connection accept** (edit-in-place XFSMC, the `@0xBFF8` recipe) is a DIFFERENT, simpler
  path than start-net. It is built + unit-verified in RetroCore (`DriveConnAcceptServer`).
- **start-net-server** = a stateful multi-round XROUT directory sync run by
  `PocsCommandSubprocessMsgLoop`. The card accumulates a `(name -> sysnum)` directory across letters
  and answers each record, echoing known peer sysnums and stamping its own sysid (`low16` of global
  `0x1E21A`). `0x45B8` in the round-1 reply is echoed - it is `17848` = the D2XX remote
  (`DEFINE-REMOTE,,D2XX 17848`), not card identity.
- "started, sysid 9800" is printed by XROUT only after the whole directory sync drains, not after one
  round.

Related: `ETHII-HLE-PROTOCOL-SPEC.md` (the byte-exact round-1 capture decode);
skill `nd-ethernet-ii` sec 8b (the `@0xBFF8` accept builder);
`XROUT-DIRECTORY-RECORD-TAGS-DECODE-2026-08-10.md` (the XROUT header/parameter format).

---

## 5. The directory responder - FOUND 2026-08-10d

The reply-selection was NOT `PROCESSXRO` (that is the XROUT trace/status handler) and NOT
`PocsCommandSubprocessMsgLoop` (an ID/registration helper). Both earlier identifications were wrong.
The directory responder is a **producer/consumer subsystem hung off CSEVENTS**, not a single function.

### Dispatch chain (all read-only-verified in `encos-ser-all-banks-68k.bin`)

```
POCSPROCES @0xE52E
  -> CSEVENTS @0xE24E                 (event group 3, pending mask bits 24-31, mask 0xbf000000)
       event bit 0x04000000 -> handler 0xd668 --\
       event bit 0x08000000 -> handler 0xd804 ---> P2 @0xcbcc  (directory-letter processor)
       event bit 0x10000000 -> handler 0xdb32 --/
       queue 0x1e22e non-empty -> sender A @0xdc5c  --\  drain + XFSND
       queue 0x2ab52 non-empty -> sender B @0xdeec  --/  via @0xBFF8
```

### P2 @0xcbcc - the directory-letter processor

Reads the incoming XROUT letter fields: word `(0x12,A0)`, long `(0xe,A0)` = name/sysnum payload,
param bytes `(0x22,A0)`/`(0x23,A0)`, flag = bit 15 of `(0x8,A0)`. Registers the entry into the
directory name-tables **0x2ab5a** (via `0x13500`, D0=4) and **0x1e232** (D0=0) using helper `0xc6fe`.
Then branches on the two param bytes at +0x26/+0x27:
- both 0, or both 1  -> `P1 @0xcb1c` (index register/lookup against list **0x2ab56**)
- else, dest-magic global `0x1e224` != 0 -> enqueue reply element (value = letter+0x12) onto queue
  **0x1e22e** (drained by sender A)
- else -> enqueue onto queue **0x28f32** (drained by sender B)

### Senders A @0xdc5c / B @0xdeec - the drainers

Each dequeues one element and builds a 4-byte XFWRI parameter
`[byte0 = paramNum (elem+0xc), byte1 = 0 or 0x29, value16 = elem+0x14]`, stages the `@0xBFF8` input
frame (XFSCM handle = elem+0x4, XFWRI len 4 = the param, a second XFWRI from base+hdrlen, XFSND to the
sender magic), then calls the reply-builder `@0xBFF8`. Sender B additionally maintains the XROUT
**sequence counter** global **0x1e1de** (copied into reply+0x2c, then `addq.l #1`) and a sub-state
jump table at **0x2d31c** indexed by `(elem+0x18)>>5 & 7`.

### Why 0x0102 / 0x0202 / 0x054A are absent as immediates

The parameter **number** is a runtime field of the enqueued element (`elem+0xc`), and the record
header is composed from the letter's serial/service - so the directory-reply constants are never
literals in the image. This is MECHANISM-PROVEN. **NOT yet byte-proven**: that the emitted params are
specifically `0x0102`/`0x0202` with header `0x054A`. Confirming that needs either tracing the
param-number field origin through the three event handlers, or a live boot capture of the port-4
XFSND payloads.

### Build consequence

The HLE start-net path is a **producer/consumer directory server**: route the port-4 XROUT letters
into a P2-equivalent that maintains a `(name -> sysnum)` table, drain reply param records with a
per-conversation sequence counter, and loop until port 4 empties. Only then does XROUT print
"started, sysid 9800".
