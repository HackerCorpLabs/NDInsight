# XROUT service calls captured from guest memory (2026-07-26)

The first direct observation of XROUT service messages as a task builds them - including
the `XSNAM` name registration, which never crosses a wire and so could never appear in an
HDLC capture.

**Method.** RetroCore traces `MON 200` (XMSG) at Device level, and its `XFWRI` decoder
dumps the user buffer being written into the message. Booting BIGDISK0-L, starting XMSG
and the COSMOS products, and reading that trace gives the exact bytes each task hands to
XROUT. Source: `Emulated.HW/ND/CPU/ND100/Sintran/MON_200_XMSG.cs`, captured by
`Nd100SintranEthernetIIBootHarnessTests.Boot_Login_StartXmsg_StartCosmos_ListServers`.

Words are printed as read from memory; ND is big-endian, so `[0x5342]` is bytes `53 42`.

---

## 1. `XSNAM` - a server registering its name [VERIFIED]

```
XFWRI  NBYTES=14 Address=0x0054
  [0x5342][0x000A][0xFF08][0x2A58][0x4D2D][0x4649][0x444F]
   53 42   00 0A   FF 08   2A 58 4D 2D 46 49 44 4F

  53      serial number
  42      service = 66 = XSNAM
  00 0A   length of remainder = 10
  FF 08   string parameter 1, 8 bytes
  2A 58 4D 2D 46 49 44 4F   "*XM-FIDO"
```

immediately followed by

```
XFSND  [Receiving port: 0x00000000  Sending port: 3]     T = 0x040C
```

`T` decodes as function `0x0C` = XFSND with option `0x0400` = XFROU: send the current
message buffer to the LOCAL XROUT rather than to a port. The sending port is 3, which is
exactly the port `*XM-FIDO` occupies in that boot's registry.

So the whole naming mechanism, as the manual describes it and as we had only read about,
in three lines of trace: build a buffer holding `XSNAM` plus the name, then `XFSND` it to
XROUT with `XFROU` from the port being named. Nothing goes on any wire.

`*TADADM` registers the same way, with more options set:

```
XFSND  [Receiving port: 0x00000000  Sending port: 4]     T = 0x260C
```

`0x2600` = XFSEC | XFROU | XFRRO (bits 9, 10 and 13), function XFSND.

---

## 2. The MESSAGE-BUFFER form has the four-byte header [VERIFIED]

This settles a distinction we previously had to infer. `XMSG-SERVER-NAMES-AND-LETTERS.md`
section 5 records that XROUT messages on the WIRE carry no `serial / service / length`
header - the parameter blocks start at the first trailer byte and the service travels in
XMCSM. The manual (appendix B section 2) describes a header, and we reasoned it must
describe the buffer form.

The trace confirms it directly: every buffer above begins with the three header fields.
So both statements are true and they describe different things:

| Form | Header | Service carried in |
|---|---|---|
| Message buffer, as a task builds it | YES - serial, service, length | the header's byte 1 |
| XMSG data frame, on the wire | NO - parameters start immediately | the frame's XMCSM word |

That is exactly the split modelled by `XroutMessageFraming` in the library, which was
written from inference and is now backed by observation on both sides.

---

## 3. The magic-number layout, confirmed from guest memory [VERIFIED]

A reply buffer carries a magic number as a four-byte integer parameter:

```
XFWRI  NBYTES=20
  01 00   00 10   01 04 00 64 01 F8   FE 08 2A 58 4D 2D 46 49 44 4F

  01      serial (echoed)
  00      service overwritten with status 0 = XRSOK
  00 10   length 16
  01 04   integer parameter 1, 4 bytes
  00 64 01 F8   magic number 0x006401F8
  FE 08   string parameter 2, 8 bytes = "*XM-FIDO"
```

`0x006401F8` decomposes with the carved layout as system `0x0064` = 100, port word
`0x01F8` = 504, and 504 = `(3 << 7) | 120`: **port 3, random 120**. Port 3 is `*XM-FIDO`
in that boot's registry, and 120 is inside the 1..126 range `ZRAND` can mint.

This is an independent confirmation of `XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md`:
the layout was carved from kernel instructions, and here the running kernel writes a
magic number into a buffer that decomposes exactly as predicted - from guest memory, with
no wire involved.

---

## 4. `XSDRN` matches the manual exactly [VERIFIED]

The `DEF-REMOTE,,D100 100` operator command produces:

```
XFWRI  NBYTES=14
  01 49   00 0A   FF 04 44 31 30 30   02 02 00 64

  01      serial
  49      service = 73 = XSDRN (define remote name)
  00 0A   length 10
  FF 04   string parameter 1 = "D100"        (appendix B 3.12: system name)
  02 02   integer parameter 2, 2 bytes = 100 (appendix B 3.12: system number)
```

Parameter numbering, types and order are precisely what appendix B section 3.12
specifies.

---

## 5. `XSGNI` walking the name table [VERIFIED]

`list-servers` enumerates the registry by repeating the get-next-name service:

```
01 45 00 04  01 02 00 00               service 0x45 = 69 = XSGNI, from magic 0
01 45 00 06  01 04 00 64 01 F9         next, from magic 0x006401F9
```

The second call starts from the previous answer's magic PLUS ONE (`0x01F8` became
`0x01F9`), which is how the manual's "first name whose magic number is greater than or
equal to this" is turned into a walk.

---

## What this does NOT show

- **`XSCRS`** (create connection port) was not seen in this capture. **RESOLVED
  2026-07-27** - captured in a later run of the same harness; see
  [XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md](XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md).
  The reason given here ("the trace window did not cover their startup") was wrong: the
  harness always drove the products, that run simply did not reach them. The capture also
  corrected an assumption - the free-SP count is built by repeated `XSNSP` of +1, not set
  by `XSCRS` parameter 2.
- The reply direction is only partly visible: `XFWRI` shows what a task WRITES, so
  XROUT's answers appear only when XROUT itself writes them.

## Reproducing

Run `Boot_Login_StartXmsg_StartCosmos_ListServers` in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs`
and read the Device-level log it captures. Search it for `XFWRI` to see buffers and
`XFSND` for the sends that follow them.
