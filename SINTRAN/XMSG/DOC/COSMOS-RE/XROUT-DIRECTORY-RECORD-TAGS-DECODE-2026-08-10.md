# XROUT routing-directory record + tag decode

**Date:** 2026-08-10
**Scope:** decode the numeric RECORD TYPES (word0) and TAGS in the `*XM-ENNS0` start-net
conversation between the ND-100 XROUT kernel and the ENCOS Ethernet card, from authoritative
ND sources.
**Method:** static file search only. No emulator, no Ghidra.

---

## TL;DR

The "records" are ordinary **XROUT messages** and the "tags" are ordinary **XROUT parameter
blocks** - both defined by ND in the COSMOS Programmer Guide, appendix B. Nothing here is a
private ENNS0 format.

- **word0 is NOT a record type + a separate length.** It is the 2-byte XROUT message header:
  **byte 0 = serial number, byte 1 = service number / return status.** word1 is the length of
  the rest of the message in bytes. (VERIFIED)
- **A tag is NOT `field-index | data-type`.** It is a parameter block header:
  **byte 0 = parameter number AND type-sign, byte 1 = length of the parameter in bytes.**
  Integers carry a POSITIVE parameter-number byte; strings carry a NEGATIVE one (two's
  complement of the parameter number). (VERIFIED)

So the user's structure hypothesis is **half right**: the high byte IS the field/parameter
index. But the low byte is the **byte length**, not a data-type code, and the int-vs-string
distinction lives in the **sign of the high byte**, not in the low byte.

---

## Authoritative sources used

| What | Full path | Evidence |
|---|---|---|
| XROUT message format (appendix B) | `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-60.164.3 EN COSMOS Programmer Guide.md` | lines 10833-10861 |
| XROUT service parameter tables | same file | XSDRN 11033-11044, XSDLO 11046-11071, XSDSY 11073-11089, XSGSY 11099-11137, XSLKI 11139-11168, XSNET 11170-11187, XSNSI 11469-11485 |
| Service + error numbers (PLANC include, version M) | `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-PL-VALUES-M.INCL` | lines 168-206 (services), 245-301 (XR errors) |
| Same, version L | `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-PL-VALUES-L.INCL` | identical content |
| Service SYMBOLs (version M/L) | `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-VALUES-M.SYMB` / `XMSG-VALUES-L.SYMB` | XSDSY=74 (183), XSGSY=75 (185), XSGIN=82 (193), XSDLO=83 (194), XFMRT=44 (60) |

---

## 1. The message header (word0 + word1)

Appendix B, "XROUT Message Format" (manual lines 10837-10841), VERBATIM:

- byte 0 = a **serial number** returned unchanged by XROUT so the caller can match replies.
- byte 1 = the **service number** (`XSxxx`) being requested; **XROUT overwrites it with the
  return status** - **0 = OK** (`XRSOK`), other values are `XRxxx` errors.
- bytes 2-3 = **length of the remainder of the message in bytes.**

Service numbers are all >= 64, so on a REQUEST byte 1 always has **bit 6 (0x40) set**
(`XMSG-PL-VALUES-M.INCL` line 169: *"Bit 6 is set => service request"*). On a REPLY byte 1 is
a status 0..63, so bit 6 is clear. That is the request/reply discriminator.

### word0 decode (byte0 = serial, byte1 = service/status)

| word0 | serial | byte1 | Meaning | VERIFIED? |
|---|---|---|---|---|
| `0x0149` | 1 | `0x49` = 73 = **XSDRN** | Request: Define Remote Name (name->sysno) | VERIFIED (svc# `XMSG-PL-VALUES-M.INCL:179`, format 11033) |
| `0x014B` | 1 | `0x4B` = 75 = **XSGSY** | Request: Get Routing Info for system N | VERIFIED (`...:182`, format 11099) |
| `0x054A` | 5 | `0x4A` = 74 = **XSDSY** | Request: Define System Routing | VERIFIED (`...:180`, format 11073) |
| `0x0100` | 1 | `0x00` = **XRSOK** | Reply to a serial-1 request, status OK | VERIFIED (`...:246`, header rule 10840) |
| `0x0400` | 4 | `0x00` = **XRSOK** | Reply to a serial-4 request, status OK | VERIFIED (same) |
| `0x0500` | 5 | `0x00` = **XRSOK** | Reply to a serial-5 request, status OK | VERIFIED (same) |

Note the natural pairing: request `0x054A` (serial 5, XSDSY) is answered by `0x0500`
(serial 5, OK); request `0x014B` (serial 1, XSGSY) by `0x0100` (serial 1, OK). The serial in
byte 0 is what ties a reply to its request - exactly what the manual says byte 0 is for.

---

## 2. The parameter block ("tag")

Appendix B, "Each parameter block has the form" (manual lines 10843-10859), VERBATIM:

- byte 0 = **Parameter number AND type.** *"Integers have positive values, strings negative
  (two's complement of parameter number)."* `0` means skip this byte (fill/alignment).
- byte 1 = **Length of parameter in bytes.**
- byte 2.. = **parameter data.**

So a 16-bit-integer parameter block is `PP 02 <hi> <lo>`, i.e. tag word `0xPP02`, followed by
its value word - which is exactly the `{TAG, VALUE}` pairing observed. A string parameter is
`NN LL <LL bytes>` where `NN` is the negative (two's complement) parameter number.

### Integer tags (positive high byte, low byte = length 2)

| tag | param# | len | field meaning (candidate) | VERIFIED? |
|---|---|---|---|---|
| `0x0102` | 1 | 2 | XSGSY reply Out-1 = "first system number found >= requested (or 0)" | Format VERIFIED (11106). Field-meaning candidate. |
| `0x0202` | 2 | 2 | XSGSY reply Out-2 = "connection type" (0 unavail,1 neighbour,2 via,3 via netserver,4 local) | Format VERIFIED (11107,11114-11119). **See value conflict below.** |
| `0x0302` | 3 | 2 | XSGSY reply Out-3 = "extra info" (link index / system no / subaddress / local sys no) | Format VERIFIED (11108,11121-11128) |
| `0x0402` | 4 | 2 | XSGSY reply Out-4 = "network info" (hops in right byte, WANs in left byte) | Format VERIFIED (11109,11130-11137) |
| `0x0A02` | 10 | 2 | integer parameter #10 of whatever service that record carries | Format VERIFIED; field UNRESOLVED |
| `0x0B02` | 11 | 2 | integer parameter #11 | Format VERIFIED; field UNRESOLVED |
| `0x0D02` | 13 | 2 | integer parameter #13 | Format VERIFIED; field UNRESOLVED |
| `0x0E02` | 14 | 2 | integer parameter #14 | Format VERIFIED; field UNRESOLVED |
| `0x0F02` | 15 | 2 | integer parameter #15 | Format VERIFIED; field UNRESOLVED |
| `0x1102` | 17 | 2 | integer parameter #17 | Format VERIFIED; field UNRESOLVED |
| `0x2702` | 39 | 2 | integer parameter #39 | Format VERIFIED; field UNRESOLVED |
| `0x2753` | 39? | 83? | does NOT fit: byte0 39 (int) but len 0x53=83 is absurd for an integer | **UNRESOLVED - probably a VALUE word, not a tag** |

### String tags (negative high byte = two's-complement param number; low byte = string length)

| tag | high byte signed | param# | string len | field meaning (candidate) | VERIFIED? |
|---|---|---|---|---|---|
| `0xF60C` | `0xF6` = -10 | 10 | `0x0C` = 12 | 12-char string. `"LINE-PRINTER"` is exactly 12 chars -> fits. (Also plausible: a system/server name, e.g. XSNSI Out-2 "network server name".) | Format VERIFIED (10847); string content SPECULATION |
| `0xFD05` | `0xFD` = -3 | 3 | `0x05` = 5 | 5-byte string parameter #3 | Format VERIFIED; field UNRESOLVED |
| `0xF401` | `0xF4` = -12 | 12 | `0x01` = 1 | 1-byte string parameter #12 | Format VERIFIED; field UNRESOLVED |

The captured inline ASCII `0x4D2D 0x454E 0x4E53 0x3000` = `"M-ENNS0\0"` (8 bytes) is a string
parameter's DATA; the tag preceding it should be a negative param byte with length 8
(`0xNN 08`). It is not one of the tags in the list above, so its parameter number was not
supplied - only note that it decodes cleanly as string data under this same rule.

### Value cross-check (honest)

The hints say tag `0x0102` held `0x45B8` = 17848 ("other" system number) and tag `0x0202`
held `0x2648` = 9800 (the card's own ENNS virtual system number).

- `0x0102` = 17848 as **a system number** is consistent with XSGSY Out-1 ("first system number
  found") and with the system-number parameter of XSDRN/XSDSY. Good fit.
- `0x0202` = 9800 **conflicts** with XSGSY Out-2 = "connection type" (which is 0..4). 9800 is a
  system number, not a connection type. So either (a) that particular record is **XSDSY**
  (param 1 = system number, param 2 = via-system number) rather than an XSGSY reply, or (b) it
  is XSGSY but the two values belong to different records in the stream. I do not have the raw
  capture bytes in this repo path, so I cannot pin which service that specific `0x0102/0x0202`
  pair came from. **The FORMAT is certain; the exact per-field label for 0x0202 is not.**

This is why the conversation has far more tags than any single service defines (params up to
39, plus three string params): a "start-net" bring-up runs a **sequence** of XROUT services -
Define-Local-System (XSDLO), Define-Remote-Name (XSDRN, string + int), Define-System-Routing
(XSDSY), Get-Routing (XSGSY), and Start-Link/Start-Network-Server (XSLKI/XSNET) - each with its
own parameter numbering. Tag `0xPPLL` is decodable in isolation (param PP, length LL); the
field NAME requires knowing which service's record it sits in.

---

## 3. Verdict on the tag-structure hypothesis

> HYPOTHESIS: the tag is structured - high byte = field index, low byte = data type
> (0x02 = "16-bit integer"; 0xF6/0xFD = "text/string follows").

**PARTIALLY CONFIRMED, and corrected in detail** (manual line 10847, VERIFIED):

- High byte = **parameter number** (the field index) - **CONFIRMED.**
- Low byte = **length of the parameter in bytes** - it is NOT a type code. For integers it is
  always `0x02` (a 16-bit value), which *looks* like a type but is a length; for strings it is
  the actual character count (`0x01`, `0x05`, `0x0C`) - **REFUTED as stated.**
- Integer vs string is encoded in the **SIGN of the high byte** (positive = integer, negative
  two's-complement = string), NOT in the low byte. `0xF6`/`0xFD`/`0xF4` are negative
  *parameter numbers* (10, 3, 12), not a low-byte "string type". **REFUTED / relocated.**

Same idea applies one level up: the record's word0 low byte is a **service/status number**
(with bit 6 = request flag), not a "record type" in the TLV sense - **corrected.**

---

## 4. Where the SINTRAN XMSG symbols live (K / L / M)

**No K-version XMSG symbol or values file exists in the repo.** The K03 symbol set
(`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\K03\`) contains only the six general
SINTRAN symbol files (`SYMBOL-1-LIST`, `SYMBOL-2-LIST`, `FILSYS-SYMBOLS`, `N500-SYMBOLS`,
`RTLO-SYMBOLS`, `LIBRARY-MARKS`) - there is **no `XMSG-SYMBOL-LIST` and no `XMSG-VALUES` for
K.** Searched: `find ... -iname "*xmsg*"` across the whole NPL-SOURCE tree and the XMSG tree;
`find` for `*-K*` / `*K03*` XMSG names - none.

The XMSG symbols that DO exist:

| Version | Service/error VALUES (SYMBOL/CONSTANT + `%` meaning) | Kernel SYMBOL address list |
|---|---|---|
| **L** | `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-VALUES-L.SYMB` and `XMSG-PL-VALUES-L.INCL` | `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\XMSG-SYMBOL-LIST.SYMB.TXT` (and the L03 set under `SINTRAN\XMSG\XMSG-SYMBOL-L03.SYMB`, `XMSG-SYS-DEF-L.SYMB`, `XMSG-SYSTABS-L03.SYMB`, `XMSG-POFTABS-L03.SYMB`) |
| **M** | `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-VALUES-M.SYMB` and `XMSG-PL-VALUES-M.INCL` | `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\M06\XMSG-SYMBOL-LIST.SYMB.TXT` |

**Important:** the `.SYMB`/`.INCL` files name **functions (XF...), services (XS...), errors
(XR.../XX...)** and their numeric values. They do **NOT** name the per-parameter tag/field
symbols - those are not symbolic in ND's sources; the parameter numbers are documented only in
the COSMOS Programmer Guide's per-service parameter tables. So for the tag/field meanings the
**manual is the authority**, and the `.INCL` is the authority for the service numbers in word0.

**Version stability:** the service numbers (XSDSY=74, XSGSY=75, XSDRN=73, XSDLO=83...) are
**identical in the L and M VALUES files** (byte-for-byte same lines), and the parameter-block
encoding is architectural (defined in ND-60.164, not per SINTRAN version). So the record/tag
FORMAT is version-stable across L and M, and there is every reason (but no K symbol file to
prove it directly) to expect it unchanged on K - the COSMOS guide predates all three.

---

## 5. What is still open

- The exact service that each `0x0102/0x0202/...` block belongs to in the ENNS0 capture -
  needs the raw capture bytes (not located under this repo path during this search) walked
  against the header serial/service to segment the stream into per-service records.
- Field labels for parameters 10,11,13,14,15,17,39 and string params 3/12 - these belong to a
  service whose parameter table I have not matched (candidates with high parameter counts:
  XSNSI has 8, XSLIN/XSLSY/XSGLI/XSGSU were not all read). `0x2753` needs the raw bytes to
  decide tag-vs-value.
- Whether `0xF60C` is literally `"LINE-PRINTER"` or a server/system name - needs the data
  bytes that follow that specific tag.
