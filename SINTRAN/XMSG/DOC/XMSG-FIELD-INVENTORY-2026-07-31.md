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

**UNKNOWN — what selects the arm.** `XMSTA` was the obvious candidate and it is **not** the
answer: `XMSTA=0x8684` appears in both arms (233 size, 30 not-size), as does `0x86E4`. Tested
and rejected this session.

**UNKNOWN — whether the not-size arm is really a checksum.** It is constant across frames with
different bodies, which is not checksum-like. It behaves more like a class/type constant. The
kernel comment offers only two arms, so either the comment is incomplete or our reading of the
constant arm is wrong.

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

## 7. Method note

Everything in section 1 came from reading the kernel's own symbol table and then checking it
against traffic. Nothing in it came from fitting curves to captures — an earlier attempt this
session did exactly that, produced a confident and wrong explanation, and had to be withdrawn.
Where a row below says WIRE rather than CARVED, that gap is the point: it marks a field we can
reproduce but cannot yet explain.
