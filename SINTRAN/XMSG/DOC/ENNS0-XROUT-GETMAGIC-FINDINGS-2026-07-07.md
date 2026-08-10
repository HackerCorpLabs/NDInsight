# ENNS0 / XROUT getMagic - Disassembly Findings (2026-07-07)

Follow-up to `ENNS0-XROUT-DISASSEMBLY-HANDOFF.md`. This records what the
disassembly session **actually established** (VERIFIED), what is **INFERRED**, and
what is still **UNKNOWN** and needs the correct image. Per repo policy every claim
is tagged; nothing here is a guess dressed up as fact.

---

## 1. Executive summary

- **VERIFIED:** The string `" subfunction getMagic"` (and `" XROUT   ID  "`,
  `defDTE`, `clrDTE`, `MONTR   ID`, `XGATE   ID`, `MESG`) is a **trace-record
  display label table**, not the code that builds a getMagic request. It lives in
  the ETHERNET COSMOS SERVER **trace/dump formatter**, which is present verbatim in
  all three ND-100 images (`ENCOSE0-DUMP.BPUN` @ ram:227a/2281, `encos-mon-ii`
  @ ram:2424/242b, `encos-mon-i`). The formatter only *decodes* trace records the
  running server logged; it does not originate the XROUT protocol.
- **VERIFIED:** `getMagic` maps to the XMSG/XROUT service **`XSGMG` = 71**
  ("Get magic from name", privileged) - from the official constants
  (`XMSG-API.md` section 6.4, `xmsg-constants.json`). `defDTE`/`clrDTE` are the
  network-server's define/clear DTE-address subfunctions (X.25/Ethernet address
  binding for a remote system).
- **VERIFIED:** The MC68000 controller firmware (`encos-ser-all-banks-68k.bin`)
  contains **zero** `XROUT`/`magic` strings. The XROUT registration is entirely an
  **ND-100-side** concern, exactly as the handoff assumed. The card is not involved.
- **INFERRED (high confidence):** `START-NETWORK-SERVER ENNS0` registers ENNS0 as
  the **network server/gateway** via `XSNET` (=85, "Start/stop network server").
  After that, XROUT drives the server with the subfunctions the trace labels name
  (getMagic / defDTE / clrDTE). The "Unknown name (of server or system)" failure is
  the emulated XROUT not implementing that network-server handshake, so the name
  never becomes resolvable.
- **UNKNOWN / NOT YET EXTRACTED:** the exact **byte layout** of the getMagic
  request/response and the name-registration message. That code is the supervisor's
  XMSG service dispatch, which is **not** in the trace formatter and **not**
  auto-analyzed in the currently-open images. See section 5 for the precise next
  step. I did **not** fabricate a layout.

---

## 2. What the images actually are

| Image (Ghidra) | Language | What it really is |
|---|---|---|
| `ENCOSE0-DUMP.BPUN` | ND-100:BE:16 | `BINARY-DUMP` of the loaded ENNS0/ENCOS segment. Contains supervisor **startup** strings (`ENNS0-POSU`, `Check if RTCOMMON is in interface memory`, MON PIOC error catalog) **and** the embedded trace/dump formatter. Not auto-analyzed. |
| `encos-mon-ii-b01.prog` | ND-100:BE:16 | Standalone **ETHERNET COSMOS SERVER trace/dump** utility (Ethernet II). Same formatter as in the DUMP. |
| `encos-mon-i-b01.prog` | ND-100:BE:16 | Same, Ethernet I. |
| `encos-ser-all-banks-68k.bin` | 68000:BE:32 | The controller firmware. No XROUT code. |

Key correction to the handoff: the `getMagic` strings in the `-mon` progs are the
**same trace-decoder labels**, so those progs are *not* a shortcut to the request
builder - they are the log viewer. The builder is in the supervisor's service code.

---

## 3. The trace/dump formatter (VERIFIED structure)

`ENCOSE0-DUMP.BPUN`, function entry `RADD SL,DX` @ ram:1fd9 (PLANC thunk), body
around ram:20e0-216c, string-descriptor table ram:216d-217a. It walks a trace
record (pointer in a local, typically `-0x7a,B`) and, per record, prints a header
label then formats fields. The record's high-byte selector fields choose the label:

- Record-type labels (one block per type): `MESG` (ram:2213 area), `XGATE ID`
  (ram:2247), `XROUT ID` (ram:227a), `MONTR ID` (ram:22be).
- Under `XROUT ID`, the subfunction labels printed are
  `subfunction getMagic` (ram:2281), `defDTE` (ram:2298 block), `clrDTE`, and
  `**SUBFUNCTION UNKNOWN** value` (ram:2294) for the default case.
- The formatter also prints `**TYPE UNKNOWN** value`, `status OK`,
  `**STATUS BAD** value` - i.e. each record carries a type, a subfunction, and a
  status word, all displayed.

A sibling formatter for the Network-PDU trace records is at ram:230a (string table
@ ram:2386, pointing into the `2498` NPDU string block: `in/from ND/out/to ND`,
`DT nr`, `AK nr credit`, `DR by user reason`, `!!! UNKNOWN NPDU TYPE !!!`).

**Note (honesty):** the ND-100 PLANC output interleaves string-descriptor pointer
words with code, so several stretches disassemble at half-word boundaries until
cleared. I decoded the label table and the record-field selection *shape* but did
**not** finish clearing every branch to pin each subfunction's exact numeric
constant from the formatter. The numeric identity in section 4 comes from the
official constants, which is a stronger source than the formatter's compare values.

---

## 4. Service-code identity (VERIFIED from official XMSG constants)

From `XMSG-API.md` section 6.4 / `xmsg-constants.json` (version M):

| Trace label | XROUT service | Code | Meaning |
|---|---|---|---|
| getMagic | `XSGMG` | 71 | Get magic number from a name (privileged) |
| (registration) | `XSNAM` | 66 | Give a name to this port |
| (registration) | `XSCRS` | 80 | Create service (name + number of service points) |
| (start server) | `XSNET` | 85 | Start/stop network server (privileged) |
| defDTE/clrDTE | (network-server DTE bind/unbind subfunctions) | - | X.25/Ethernet address mapping for a remote system |

Supporting XMSG function codes (`T` on `MON 200B`): `XFP2M`=23 port->magic,
`XFM2P`=22 magic->{system,port}, `XFSND`=12 send to remote port, `XFRCV`=13 receive.

Failure code: **`XEIMA` = -19** "Invalid magic number" -> on the wire this is
subtype `0x07` NetworkError with Flags2 `0xFFED` (see `XMSG-PROTOCOL.md` section on
subtype 0x07). The operator-visible "Unknown name (of server or system)" is the
XROUT reply `XRUNN` = 2 "unknown name" (`XMSG-API.md` section 6.6).

**Magic-number semantics (VERIFIED, `XMSG-API.md` sections 5 / 6):** a magic number
is a 32-bit `{port + system + random}` in the `A`/`D` register pair (`MAGNO`). The
random part prevents stale-reference reuse. **UPDATE 2026-07-26: the PACKING of
those three fields is no longer unknown** - it was carved from the XMSG L03 kernel
(`ZCRMG` / `ZRAND` / `MFM2P`) and is
`system << 16 | port << 7 | random`, with a 9-bit 1-based port and a 7-bit random.
See `XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md`. (The getMagic REQUEST/RESPONSE
message layout, section 5 below, is a separate open item and remains unextracted.) `getMagic`/`XSGMG` resolves a *name* to
that 32-bit magic. An emulated XROUT that returns 0 / rejects is exactly the
"stubbed magic" the handoff describes.

---

## 5. Actionable guidance for the emulator XROUT (grounded)

What the emulated XROUT/XMSG must do so `START-NETWORK-SERVER ENNS0` succeeds
(VERIFIED at the service level; byte layout still to be pinned per section 6):

1. Accept ENNS0's `XSNET` (85) start-network-server request instead of rejecting -
   record ENNS0 as the network server for the local system.
2. On `XSGMG` (71) getMagic for a known name, return a **well-formed non-zero**
   32-bit magic `{port, system, random}` in `A`/`D`, **not** `XEIMA` (-19).
3. Accept name registration (`XSNAM` 66 / `XSCRS` 80) so the ENNS0 name becomes
   listable in `list-net-ser` / `list-ver`.
4. Service `defDTE`/`clrDTE` (define/clear the remote-system DTE/Ethernet address
   binding) as no-op-success at minimum, so the handshake completes.

Until the byte layouts in section 6 are captured/disassembled, implement these as
**accept + plausible echo** and validate against a live capture of a working
`START-NETWORK-SERVER` on real hardware (the definitive oracle).

---

## 6. DECISIVE: the builder is NOT in ENCOSE0-DUMP (MON 200B caller walk, 2026-07-07)

I walked the `MON 200B` (XMSG) call sites in `ENCOSE0-DUMP.BPUN`. Result is
conclusive and rules this image out as the getMagic source:

- **The entire image contains exactly TWO XMSG calls** (`search_bytes d680` -> only
  ram:3127 and ram:312a). Both are inside ONE routine, now named
  **`xmsg_getconfig_routine` @ ram:30cd** (PLANC function, entry `RADD SL,DX` +
  `JPL I *0x3112` to csav; body ram:30cd-3163).
- That routine does `SAT 0x0; MON 200B` (= **XFDUM**, get XMSG config) then
  `SAT 0x1; MON 200B` (= **XFDCT**, disconnect from the message system), and
  formats the returned value with an `RDIV`/digit-table (`DAT_ram_03f0`) loop for
  display.
- Its **only caller** is **`xmsg_config_display` @ ram:3276** (call site ram:3292,
  indirect `JPL I *0x32cd`, whose pointer word holds 0x30cd). That caller is a
  display/format routine building string descriptors, not a message handler.
- There is **NO** `XFRCV` receive loop, **NO** `XFOPN`/`XFSND`/`XFWRI`, **NO**
  `XSNET`, and **NO** XROUT service-code dispatch anywhere in the image. A network
  server that registers via `XSNET` and services getMagic/defDTE/clrDTE would need
  dozens of XMSG calls; two calls (attach-config + detach) is a passive utility.

**Conclusion:** `ENCOSE0-DUMP.BPUN` is the ENCOS **monitor/dump utility** (program
name `ENCOSE0`), the same family as `encos-mon-i/ii`. It is **not** the ENNS0
network server, and the getMagic request-builder / service-handler is **not present
in this image** and cannot be recovered from it.

Caveat (honesty): `MON` encodes its call number as an instruction immediate, so
every XMSG call must appear literally as `0xD680` in the code - hence the count of 2
is exact. The only way to hide an XMSG call would be building a `MON` opcode in a
register and running it via `EXR`; I saw no evidence of that pattern here, but did
not exhaustively prove its absence.

### Where the builder actually is, and how to get it

1. **The ENNS0 server object**, not this dump: `encos-err-i/ii-b01.brf` (PLANC
   object, MAIN `ENNS0`, ENTR `POSUERR/READPIO/SEGLOAD/START_P/STOP_PI/SEND_KI/
   REC_KIC/INT2GET`) and/or the server data segment `encos-ser-i-b01.dseg`
   ("XMSG Server"). Link the BRF to an absolute image with the BRF loader in
   `SINTRAN\File-Formats\BRF-GHIDRA-LOADER-HANDOFF.md`, load it as ND-100:BE:16,
   then find the `XFRCV` (T=13) receive loop and the service-number switch that
   reaches the getMagic/defDTE/clrDTE handlers; the reply is built with
   `XFWRI`/`XFWRT`/`XFSND`. The BRF symbols name the handlers directly.
2. **The getMagic handshake is LOCAL, not on any wire.** ENNS0 is the COSMOS
   Ethernet II network server: it carries inter-system XMSG over the Ethernet
   (LANCE/Am7990) controller, *replacing* HDLC/Megalink as the link layer - so it
   never touches HDLC. And the getMagic / name-registration / `XSNET` exchange that
   is failing happens **on the ND-100, in memory**, between ENNS0 and XROUT via
   `MON 200B` (XMSG) monitor calls - it dies during local registration, before any
   Ethernet frame is sent. It therefore cannot be captured on HDLC or Ethernet.
   The correct live oracle is to **trace the `MON 200B` calls in the emulator**:
   breakpoint on opcode `0xD680` while running `START-NETWORK-SERVER ENNS0` (DAP
   instruction breakpoint or RetroCore CLI cpu-trace), and on each hit read `T`
   (XF function code), `A`/`D` (magic `MAGNO`), and the XMSG message buffer (XROUT
   service byte + name string). That yields the exact getMagic request/response and
   name-registration content directly from the running server.

---

## 7. Files

- This doc: `SINTRAN/XMSG/DOC/ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md`
- Prior handoff: `SINTRAN/XMSG/DOC/ENNS0-XROUT-DISASSEMBLY-HANDOFF.md`
- Constants: `SINTRAN/XMSG/DOC/XMSG-API.md` (section 6),
  `SINTRAN/XMSG/xmsg-constants.json`
- Wire subtype 0x07 / XEIMA: `SINTRAN/XMSG/DOC/XMSG-PROTOCOL.md`
