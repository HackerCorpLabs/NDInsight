# XMSG handoff (2026-07-27)

State of the XMSG work after the API-library + capture sessions of 26-27 July. Read this
first; it points at everything else.

**Status: the library is done and the capture surface is exhausted.** What remains is one
disassembly question and two carving oddities - no more emulator driving is needed.

---

## 1. What exists now

### The library - `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\`

| Project | Role |
|---|---|
| `Xmsg.Protocol` | wire: frames, packets, XROUT messages, codecs, enums |
| `Xmsg.Node` | the seam, TAD, server host, transports |
| `Xmsg.Live` / `Xmsg.Live.Runner` | live two-node runner over TCP/HDLC |
| **`Xmsg.Api`** | **NEW - the user-facing library.** `XmsgKernel` (COSMOS appendix A function set), `XmsgMagicNumber`, `XmsgMessageBuffer`, typed `XroutRequests` builders for every XS* service, RR-LIB `IRrClient`/`IRrServer` |
| **`Xmsg.Api.Node`** | **NEW - bridge.** Puts an `XmsgKernel` on the real datagram path (`XmsgKernelServer`) |
| **`Xmsg.Chat`** | **NEW - a worked APPLICATION on `Xmsg.Api`.** A named chat service with seats, roster and broadcast. The vocabulary is ours (no chat service exists on any ND image); everything under it is captured behaviour. Read it as the answer to "what does writing an XMSG program look like?" |

**340 tests green**, 0 warnings, `dotnet format` clean. Build/test:
`dotnet test SRC\Xmsg.Protocol.slnx -c Release`.

Write clients and servers against `Xmsg.Api`; do not hand-build frames.

### The capture harness - RetroCore

`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs`

| Test | What it captures |
|---|---|
| `Boot_Login_StartXmsg_StartCosmos_ListServers` | the registry + every server registration |
| `Boot_Login_StartXmsg_ProbeNameLookupServices` | `XSGIN` and which command emits which service |
| `Boot_Login_StartXmsg_ProbeRawXroutRequestBuilder` | the advanced-mode raw request builder |

All `[Explicit]`, ~3-5 min each, driven headless. RetroCore traces `MON 200` at Device
level and dumps the `XFWRI` buffer, so anything a task hands to the LOCAL XROUT is visible
- which is how the registration services were captured at all.

---

## 2. The findings, in dependency order

1. **MAGNO = `system << 16 | port << 7 | random`**, carved from the kernel. The low 7 bits
   are `ZRAND`, an LCG stepping `r' = (53r + 25) mod 128` with 0 and 127 redrawn, so a
   valid random is 1..126. Use it to validate a port field or spot a fabricated magic.
2. **Wire port fields ARE the magic low word.** TAD `7CORS` ships a whole MAGNO.
3. **Registry port numbers are load-order dependent, not well-known.** Two boots of the
   same image put different servers on port 4. Resolve names at run time.
4. **XROUT has two message forms.** The message buffer carries the 4-byte
   `serial/service/length` header; the wire form does NOT, and the service rides in XMCSM.
   Our decoder assumed a header and lost every parameter - fixed via
   `XroutMessageFraming.WithHeader` / `.BodyOnly`.
5. **Registration is local and invisible to any pcap.** `XSNAM` (named port) and `XSCRS`
   (connection port) are MON 200 calls followed by `XFSND` with `XFROU`.
6. **The free-SP count is built by `XSNSP` +1 per service point**, not set by `XSCRS`
   parameter 2 - every server registers with zero and counts up. This corrected a
   documented assumption.
7. **`XSGIN` resolves a name without privilege**: parameter 2 = system number always,
   parameter 1 = port number only for a port name.
8. **`XSGMG` (magic from name) is still uncaptured.** See open items.

Each has its own doc; all are indexed in `DOC\README.md`.

---

## 3. Open items

### 3.0 NEXT SESSION: capture the file servers

`*XFTRA` and the `*FA-*` family are the last big unknown - we have their registrations but not one
byte of their working traffic, and our own notes describe their purpose as inferred from their
names. Plan, with the capture order, the two methods and the traps:
**[PLAN-FILE-SERVER-CAPTURE-2026-07-28.md](PLAN-FILE-SERVER-CAPTURE-2026-07-28.md)**.

This also decides the remote-execution console's file transfer: today the honest recommendation is
to write our own agent, because the machine's file servers are undecoded.

### 3.1 `XSGMG` - one disassembly question away

The XMSG command program's raw builder assembles a byte-correct service-71 request, but
`MESSAGE-STATUS` shows the message stays **length 0**: `BUFFER-READY` fills the OUTPUT
BUFFER and nothing copies it into the message. Both send paths were properly tested and
both fail for that reason (`ROUTE-MESSAGE` -> status 8 XRMTL, `SEND-MESSAGE` -> status 6
XRMMP).

The carve found the program has exactly **three** `MON 200` sites: two hardcoded (`SAT 1`
XFDCT, `SAT 0` XFDUM) and one general wrapper that takes its function code from the
caller's T. So there is exactly one place left to look.

**Next step:** load `(SYSTEM)XMSG-COMMAND:PROG` into Ghidra with the ND-100 processor,
establish the load-address mapping, find the entry of the routine containing the `MON` at
file offset 60312, and list its callers with the T each sets. That says outright whether
any command issues `XFWRI`.

**Cheaper first probe:** log caller P and T on every `MON 200` in the emulator and sweep
the whole `X-C:` command surface. Not proof of absence, but fast.

**Independent route:** ENNS0 uses `getMagic`/`XSGMG` for inter-node resolution - blocked on
Ethernet II bring-up.

### 3.2 Two carving oddities

- `*TADADM`'s `XSNAM` length field says **8** for a 9-byte body. `*XM-FIDO` and every
  `XSCRS` buffer are self-consistent, so it is specific to what TADADM writes. XROUT
  accepts it.
- An `XSLET` from `LIST-CONNECTIONS` carries an **undocumented parameter 10**
  (`0A 02 00 66`). Appendix B section 3.4 lists parameters 1, 2 and 4 only.

### 3.3 Not XMSG, but it will bite you

The boot harness dies intermittently - host process gone, no exception, no dump, no
event-log entry, at a different point each time. My MON 200 decoder change was tested and
ruled out (reverted: passed; restored: passed; six later runs passed). Retry before
concluding anything about the command you just typed.

### 3.4 Declined, with reasons

- **TadServer -> `IRrServer`**: wrong fit. TAD has no request/response pairing; it is a
  terminal stream with window-of-1 ACK flow control where a short non-final frame crashed a
  real machine. It stays an `IXmsgServer`.

---

## 4. Traps for whoever continues

- **A pcap cannot answer registration questions.** `XSNAM`/`XSCRS`/`XSNSP`/`XSGIN` never
  reach HDLC. Use the MON 200 trace.
- **Count the prompts.** The XMSG command program asks one question at a time;
  `SET-CURRENT-MESSAGE` asks 2, `SEND-MESSAGE` asks 4. Answer too few and the NEXT command
  is swallowed as an argument, which looks exactly like the command failing. Two candidate
  tests were reported as "failed" this session when they had never actually run.
- **The builder commands need `SET-ADVANCED-MODE` first**, then `SET-PRIVILEGED`. Without
  it every one answers "Command not recognised" - including `SET-PRIVILEGED` itself.
- **`?` is not an inventory.** It lists only the commands new or modified in this product
  version (two of them).
- **Verify every opcode constant** against `ND-06.014.2A EN ND-100 Reference Manual` before
  trusting a scan. `JPL` is index 23, not 27; decoding it wrong produced a clean-looking
  result set that was pure artefact.
- **Trailing pad bytes after an odd-length final string are the caller's choice** -
  XMSG-COMMAND emits one, TADADM does not, XROUT accepts both. Padding BETWEEN parameters
  is mandatory.

---

## 5. Still needs Ronny

Install the updated Wireshark dissector (elevated shell):

```powershell
Copy-Item "E:\Dev\Ronny\NDInsight\SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua" `
          "C:\Program Files\Wireshark\plugins\hdlc_tcp.lua" -Force
```

Then Analyze -> Reload Lua Plugins. It now decomposes port fields into port + random and
predicts the next `ZRAND` value.

---

## 6. Where things are

- Docs + index: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\README.md`
- Skill (updated 2026-07-27): `C:\Users\ronny\.claude\skills\xmsg-decode\SKILL.md`
- Session walkthrough artifact:
  <https://claude.ai/code/artifact/2fea47cb-2947-48da-981a-bfe7846a8ab6>
- Commits: NDInsight branch `5000x` (`139436c` library, `2c34103` XSCRS, `851042b` XSGIN,
  `b679d39` dead end, `1e2467d` carve); RetroCore branch `ethernet-ii-controller-fixes`
  (`5808e9b36` decoder fix, `973953337` + `59dc3ef58` probes).
