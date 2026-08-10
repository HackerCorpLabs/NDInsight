# The XMSG sub-header, named from the L03 kernel symbols (2026-07-29)

The wire sub-header was reverse-engineered from captures and has been byte-correct for months, but
several of its fields carried our own invented names. The XMSG L03 symbol files
(`F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\`) name them, and in doing so decode two fields we had
been treating as opaque.

Sources: `XMSG-POFTABS-L03.SYMB` (structure definitions with comments) and `XMSG-SYMB-CX-L03.SYMB`
(the resolved octal symbol dump). Both are XMSG version L, 87.02.08.

---

## 1. The "marker" is not a marker [VERIFIED]

Every capture shows `21 00` at sub-header offset 14-15, and every document we have calls it a
marker. It is not.

```
SYMBOL X5THD=20400           % PUT IN XMTHD (VERSION=2, PROTOCOL=1)
```

`020400` octal = `0x2100`. That is exactly the bytes on the wire. So the field is **`XMTHD`, the
transport header word**, and it encodes **version 2, protocol 1**. It is a constant only because
every message we have captured is the same version of the same protocol.

Related: `X5VRS=020400` is documented as the left-hand byte of `XDROU` - "(OP=0, VERSION=2,
PROTOCOL=1)" - and `X5VER=000040` is the version alone. A future version of XMSG would change
these bytes, so anything matching on `21 00` as a literal marker is matching on a version number.

## 2. The two bytes after it are ONE word: XMSTA [VERIFIED]

We had been reading offset 16 as "frameFlags" and offset 17 as "Role", with the role byte's bits
named from the XF send options. That reading was right about the bits and wrong about the shape:
it is a single 16-bit status word, `XMSTA`, and both our bytes are halves of it.

`POFTABS` defines the low bits as message state, and the symbol dump gives a second `XF*` group
whose values are **bit numbers, not function codes**:

| Our name | Word half | Real content |
|---|---|---|
| frameFlags (off 16) | low byte of XMSTA | the `5M*` message-state bits |
| Role (off 17) | high byte of XMSTA | the `XF*` send-option bits |

The high-byte mapping falls straight out, because each option's bit number is 8 more than the bit
we observed in the role byte:

| Option | Bit number | Role-byte bit | Meaning |
|---|---|---|---|
| `XFTCM` | 8 | `0x01` | |
| `XFSEC` | 9 | `0x02` | secure message |
| `XFROU` | 10 | `0x04` | routed via XROUT |
| `XFFWD` | 11 | `0x08` | forwarded |
| `XFBNC` | 12 | `0x10` | bounce |
| `XFHIP` | 13 | `0x20` | high priority |
| `XFWAK` | 14 | `0x40` | wake up receiver |
| `XFWTF` | 15 | `0x80` | wait for transfer |

This is the first independent confirmation of the role-byte bit assignments - they were previously
derived from the COSMOS manual and matched against traffic, never against the kernel's own symbols.

The low byte is the message state: `5MRED=0` whole message read, `5MRTN=1` being returned,
`5MSEC=2` secure, `5MBNC=3` bounce, `5MHIP=4` high priority, `5MROU=5` sent by the routing program,
`5MPRV=6` sent by a privileged task, `5MRND=7` return on non-delivery. That explains the recurring
`0x86` / `0x82` / `0x92` / `0x96` values we have been logging without decoding.

## 3. The magic number, confirmed from the kernel source [VERIFIED]

```
% DEFINITION OF FORMAT OF MAGIC NUMBER (SECOND WORD-FIRST IS SYSTEM NUMBER)
SYMBOL 5PSHZ=7            % NUMBER OF BITS IN RANDOM NUMBER
SYMBOL 5PMS1=177          % MASK TO EXTRACT RANDOM PART
SYMBOL 5PMSK=-1-5PMS1     % MASK TO EXTRACT PORT NUMBER
SYMBOL XRLPN=0            % LOGICAL PORT NUMBER FOR ROUTING (NETWORK WIDE)
```

`system << 16 | port << 7 | random`, with a 7-bit random part - exactly the layout carved from the
kernel code earlier, now confirmed from the definitions rather than the implementation. `XRLPN=0`
states in the source what we had only observed: **port 0 is the routing port, network-wide.**

## 4. One CONFLICT, deliberately not resolved here [OPEN]

`POFTABS` states the transported header is **7 words / 14 bytes**, ending after `XMCSM`:

```
% WARNING: 1) Next block is sent directly over link. Do NOT split up!
SYMBOL XM5HL=XM5HE-XM5HS*2      % = 16 octal = 14 bytes
```

with `XMCSM` as a **single word** ("datagram checksum; if not checksum, then message size") and
`XMLEN` living at word +11, well outside the transported header.

Our working layout instead reads a **32-bit `XMCSM`** at offset 26-29 and a **16-bit `XMLEN`** at
30-31. Those are not compatible.

Both sides have real evidence:

- The symbol file is the kernel's own definition, and its `XMTHD` constant is confirmed byte-exact
  on the wire (section 1), so it is not describing a different protocol.
- Our layout's `XMCSM >> 24` and `XMCSM >> 16` feed the envelope formulas that reproduce Counter
  and Channel on **753 of 753** data frames. A 16-bit `XMCSM` cannot supply those shifts.

The likely reconciliation is that what we call a 32-bit `XMCSM` spans `XMCSM` plus the following
word, and what we call `XMLEN` is the first word of the message body rather than a header field -
but that is **INFERENCE and is not settled**. Anyone touching the frame parser should read this
section first and resolve it against a capture, not assume either side.

> ## RESOLVED 2026-07-31 — the symbol file was right, and the inference above was correct
>
> Settled against the corpus exactly as this section asked. **The symbol file wins:**
>
> - **`Flags2` (wire 10-11) equals the 16-bit `XMCSM` (wire 26-27) on 1449 of 1449 data
>   frames.** So the rule "Flags2 == XMCSM >> 16" was never evidence for a 32-bit field — it
>   was this exact equality, seen through the wrong split.
> - **Wire 28-29 is the first word of the message BODY.** Its values are all application-layer:
>   `0x07F0`/`0x07A2`/`0x07C0`/`0x07D2` (FA message types), `0x0041` XSLET, `0x014B` XSGSY,
>   `0x0100` XRSOK. Header fields do not look like that.
> - **The body therefore starts at wire offset 28** = 13 SINTRAN header + 1 Counter + 14
>   transported header. Confirmed arithmetically: on the frames where `XMCSM` carries a size it
>   equals the body length **exactly** once 28 is used (constant offset 0, 492 frames).
>
> The `XMCSM >> 24` in the envelope formula is still a real quantity — it is the HIGH BYTE of
> the true 16-bit `XMCSM` — so the arithmetic was not nonsense, only misnamed.
>
> Full field-by-field status and the remaining unknowns:
> `XMSG-FIELD-INVENTORY-2026-07-31.md`.

## 5. Other facts worth having

- **XROUT service to handler**: the symbol dump pairs all 33 services with their `RS*` handler
  addresses (`XSLET=65 -> RSLET`, `XSNAM=66 -> RSNAM`, ...). `XSMAX=96` bounds the range.
- **XMSG function to handler**: 48 functions (`X5FUN=48`) each with an `MF*` handler, via a 3-word
  descriptor `XFNEX/XFVAL/XFENT`. `XFPRI` bit 15 marks a privileged function, `XFLDR` bit 13 marks
  one legal for drivers.
- **Port element** is 7 words: `XPCHN/XPSTA/XPTPT/XPCHK/XPQHD/XPLEN/XPCMS`.
- **Port names** are at most 16 words / **32 bytes** (`5NLEN`/`6NLEN`).
- **Virtual system numbers** run 9800-9999 (`XSVSL`/`XSVSH`) - which is where the ENNS0 "sysid 9800"
  seen in the Ethernet bring-up comes from.
- **This build's limits**: 128 ports, 256 messages, 80 tasks, 4 links, max message 2500 bytes,
  max frame 312 bytes.
- **Datagram type bits** (`XD5CO/XD5SD/XD5ED/XD5DC` plus control sub-types) and the 16-byte frame
  header `XDHAC/XDROU/XDTYP/XDDNA/XDSNA/XDREF/XDSCR/XDCSM` are fully defined in `POFTABS`.

## 6. What these files do NOT contain

Searched explicitly: **no QFORM, no TAD protocol, no file-access or FA symbols.** These three files
stop at the XMSG kernel and XROUT layer. The COSMOS file-access tag table is not here and must come
from a COSMOS-side include or from the `cos-fa-serv-e04` binary.

---

## Provenance

`F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\XMSG-POFTABS-L03.SYMB` (718 lines),
`XMSG-SYSTABS-L03.SYMB` (62 lines), `XMSG-SYMB-CX-L03.SYMB` (2097 lines).
Values in the `.SYMB` dumps are OCTAL; the separate `XMSG-VALUES-L.SYMB` and `XMSG-PL-VALUES-L.INCL`
are DECIMAL (they carry an explicit `@DEC`), which is an easy and silent mistake to make.
