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

## 3. SINTRAN header (wire 0-12)

| Off | Field | Status | Note |
|---|---|---|---|
| 0 | Marker1 `0x21` | **WIRE** | Always `0x21`. *Why* — **UNKNOWN**. |
| 1 | Marker2 | **WIRE** | `0x13` normal, `0x12` relayed. |
| 2 | PacketType | **UNKNOWN** | Always `0x00` in the whole corpus. Never varies, so nothing can be inferred. |
| 3 | Subtype | **WIRE** | `0x0E` Data, `0x03` Ack, `0x13`/`0x19` Reach, `0x07` NetworkError. |
| 4-5 | Dest node | **WIRE** | |
| 6-7 | Src node | **WIRE** | Logical source, not the LAPB neighbour. |
| 8-9 | Flags1 | **WIRE** | One sequence per direction per link, +1 per Data frame. Monotonic — verified in arrival order. |
| 10-11 | Flags2 | **CARVED** | A copy of `XMCSM`. See section 4. |
| 12 | Protocol ID / channel | **UNKNOWN** | The `0xDE - class - epoch` expression predicts it 1449/1449, but nothing carved says the machine computes it that way. **This is the biggest open item.** |

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

**STILL UNKNOWN - and static carving CANNOT finish it.** `A` at that point comes from
`LDATX`, not from `XMSTA` (which the code loads separately at `134024`). `LDATX` is
**privileged PHYSICAL addressing via the T:X register pair**, so the tested word lives at a
runtime physical address, not a static one.

Tracing the operands as far as static analysis allows: `T` and `X` are loaded from B-relative
cells (`,B 101`, `,B 105`, `,B 133`), and **B is the message-buffer base** - confirmed because
`0o132`=`XMLIM`, `0o133`=`XMCUR`, `0o134`=`XMTHD` all line up with the symbol list. So the
flag word is reached through the buffer's own current/limit pointers and is a **runtime
value**.

Bits 9 and 10 of the `XF*` option word would be `XFSEC` (secure) and `XFROU` (routed), which
fits the "checksum only for secure routed messages" reading - but that is **INFERENCE** and
must not be recorded as fact.

**To finish this, capture it live over DAP** (both machines are up), the same way the resident
DATA cells were handled - see the `sintran-carving` resident doc. Another static pass will not
resolve a physical address chosen at runtime.

**Decoder note:** `nd100-dis` switches this group on `instr & 0xFFC7`, which deliberately
drops bits 3-5. The mnemonic is still right (the variant is in the low 3 bits: `LDATX`=0,
`LDXTX`=1, `LDDTX`=2, `LDBTX`=3, `STATX`=4), but `143300`/`143320`/`143340`/`143370` all print
identically as `LDATX` even though bits 3-5 differ in the real code. If those bits carry
meaning, the listing is hiding it.

**Bonus: this routine is the transported-header builder.** At `133727`, `LDA 125` reads
P-relative from `134054` - a genuine literal holding `020400` = the `0x2100` marker - and
`133730` stores it to `XMTHD`. That is the entry point for anything else about the transported
header.

This matters well beyond documentation: `XmsgEnvelope.BaseLowSigned` subtracts this field's low
byte, so on size-arm frames the envelope arithmetic is subtracting **a byte count**.

---

## 5. Sub-header

| Off | Field | Status | Note |
|---|---|---|---|
| 13 | Counter | **WIRE** | Sits *between* the SINTRAN header and the transported header — it belongs to neither. `(Counter + Flags1 + XMCSMlow) & 0xFF == seed` on every frame, which is a **checksum relation**. |
| 16-17 | `XMSTA` | **CARVED** | Low byte = `5M*` message state, high byte = `XF*` send options. Bit assignments confirmed from symbols. |
| 18-25 | `XMDSY`/`XMDPT`/`XMSSY`/`XMSPT` | **CARVED** | System and port. Port = magic low word: `(port << 7) | random`, `5PSHZ=7`, `5PMS1=177`. |
| 26-27 | `XMCSM` | **CARVED** | Section 4. |
| 28-29 | first body word | **WIRE** | **Not header.** Values are all application-layer: `0x07F0`/`0x07A2`/`0x07C0`/`0x07D2` (FA message types), `0x0041` XSLET, `0x014B` XSGSY, `0x0100` XRSOK. |
| 30-31 | (was called `XMLEN`) | **UNKNOWN** | `XMLEN=147` octal is five words past `XMCSM`, so this is body, not a header length. The "16-bit XMLEN" reading needs retiring. |

---

## 6. Ranked open items

1. **What writes SINTRAN header offset 12.** Everything about "channel", "epoch" and "class
   lanes" is model vocabulary until this is carved. Needs the XMSG kernel disassembled — the
   NPL tree has only the L07/M06 symbol lists, no XMSG body.
2. **What selects the `XMCSM` arm.** `XMSTA` ruled out.
3. **Is the not-size arm actually a checksum?** If yes, over what.
4. **Header offset 2** — always zero, so only carving can say what it is.
5. **Retire the 32-bit `ControlService`** in `Xmsg.Protocol` — 108 call sites across 34 files;
   see the comment on `XmsgDataFields.ControlService`.
6. **LAPB address `0x07`** on some ACK I-frames.

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
