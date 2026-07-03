# TAD-MISSING — gaps to a general, correct TAD server/client

**Status:** Honest gap list. This layer (the live TAD connect-to session) is reconstructed
almost entirely from ONE captured connect-to scenario, so most of it is inference. This
document records precisely what we know, what is missing, and which capture or source would
close each gap.

**Scope:** the on-the-wire TAD terminal-session protocol carried inside the XMSG data-frame
envelope. For the envelope itself (SINTRAN header, XMSG sub-header, secure ACK) see
[XMSG-PROTOCOL.md](XMSG-PROTOCOL.md); for the verified opcode table and per-message layouts
see [TAD-Message-Formats.md](../../TAD/TAD-Message-Formats.md).

**Primary evidence:** `new-conn-to-102-from-100.pcapng` (direct 100 to 102) and
`conn-to-102-from103-via100.pcapng` (relayed via 100). Both are a single connect-to each. The
implementation in `Xmsg.Live/Tad` tags every reconstructed value with `OBSERVED` /
`INFERRED` / `ASSUMED` / `VERIFIED` comments; this document is the prose companion to those
tags.

**Provenance legend**

- VERIFIED — stated by ND docs or the SINTRAN III symbol tables (K03/L07/M06).
- OBSERVED — seen in a capture. "OBSERVED (single capture)" means seen exactly once.
- INFERRED — reasoned from structure/analogy, not directly evidenced.
- ASSUMED — a working guess with no evidence yet.

---

## 1. Undocumented opcodes seen in the captures

These opcodes appear on the wire but are not in the verified opcode table
([TAD-Message-Formats.md](../../TAD/TAD-Message-Formats.md) section 2). We can frame them
(the `[opcode][count][data]` structure is verified) but their MEANING is unknown.

| Opcode | Where seen | Count/data OBSERVED | What we know | What is missing |
|-------:|-----------|---------------------|--------------|-----------------|
| `0x06` | client frame early in setup, XMCSM `0x04000000` | count 0 | Part of the XROUT-channel TAD chain that precedes negotiation. | Meaning; whether it is a connect-request marker. |
| `0x1B` | same chain as `0x06` | count 0 | Adjacent to `0x06`/`0x1C`. Not ESC here (that is data, not an opcode). | Meaning. |
| `0x1C` | same chain | count 1, data `00` | Carries one byte, value `00`. | Meaning of the byte. |
| `0x20` | server, bare-TAD `0x00080000` | count 0 | Server emits it once, mid-setup. | Meaning; possibly a connect-accept. |
| `0x07` | server, XROUT-channel chain | count 5, data `00 00 66 03 41` | The 5 bytes look like `sys(0066)=102` + `port(0341)` — i.e. an address handout. | Confirm it carries the server port/magic; see gap 3. |
| `0x0B` | server, same chain | count 2, data `03 02` | Two bytes, likely a version/param pair. | Meaning. |
| `0x15` | server, same chain | count 2, data `01 08` | Mirrors the `0x0108` frame-class word. | Meaning; relation to XMCSM `0x01080000`. |
| `0xFF` | client and server, end of an XROUT-channel chain | count 0 | Appears as the LAST message of the chain. | Whether `0xFF` is an end-of-chain terminator or a real opcode. |

Also reported elsewhere (Lua dissector notes / other captures) but NOT in the two connect-to
captures, so listed for completeness and not yet structurally confirmed here: `0x10`, `0x11`,
`0x12`, `0x56`, `0x60`, `0xFD`. `0x21` (CERS) and `0x1F` (OPSV) ARE documented — earlier notes
that flagged them as unknown were mistaken.

**What would close it:** the TAD server source `MP-P2-TAD.NPL` dispatch table and the client
`RP-P2-TAD.NPL` builders (both under the NPL-SOURCE tree) — cross-reference each numeric
opcode constant. A capture with a different client action (not just login) would show whether
`0xFF` terminates every chain.

---

## 2. Response rules and terminal-type variation

Only ONE terminal type was ever negotiated: TTYP = `0x0000` (OBSERVED, single capture). The
server's responses (echo strategy via ECKM, break strategy via BMMX, mode handling) were only
ever seen for that one terminal type.

- We do NOT know how the server's echo/mode negotiation varies by terminal type. Different
  terminals almost certainly get different ECKM/BMMX tables.
- The 20-byte echo/break classification tables (ECKM/BMMX count 21/23 variants) never appear
  in these captures — only the short count-1/count-3 forms do. Their per-character bit
  semantics are unverified (also gap 1 in TAD-Message-Formats.md section 20).
- The OS/proto version handshake was OBSERVED as client `4C 01 04` and server `4C 00 00`. How
  the negotiated protocol level gates optional features (7UMOD/78MOD at level 4+) is VERIFIED
  in the source but never exercised on the wire here.

**What would close it:** captures of connect-to from several distinct terminal types; the
terminal-driver classification tables referenced by `BDECHO`/`BDBREA`.

---

## 3. XROUT name resolution and connection-port / magic allocation

Partially OBSERVED. The connect-to opens with an XROUT XSLET letter (XMCSM `0x04000041`, low
byte `0x41` = XSLET) to the directory service, naming the target by remote name (for example
`D102`). This is name resolution via DEF-REMOTE / XSDRN (name to system, many-to-one).

Known (OBSERVED, single capture):

- The letter body is a fixed template `FF 07 2A 54 41 44 41 44 4D 00` ("...TADADM...") followed
  by the target name as string parameter 2 (`FE <len> <name>`). The `FF 07` header and the
  `0x2A` separator are not decoded.
- The server answers with its own XSLET letter (frame #60: serial `01`, service `02`) and,
  slightly later, an XROUT-channel chain containing opcode `0x07` with data `00 00 66 03 41`
  that looks like the server's `system(102) + port(0341)` address.

Missing:

- HOW the client turns the directory reply into the destination `XMDSY`/`XMDPT` it then uses
  on every data frame. We see the resulting port (`0x0341` server side, `0x0288` client side)
  but not the allocation/exchange rule.
- The 32-bit magic number (port + system + random, XMSG-PROTOCOL.md section 7) is never shown
  being minted or resolved (XFP2M / XFM2P). We only see the already-resolved ports on the wire.
- The `XMDPT` encoding is explicitly unconfirmed in the corpus (XMSG-PROTOCOL.md section 5
  refutes the `port << 7` reading).

**What would close it:** a capture that includes the XROUT directory request/reply pair with
its parameter blocks fully populated, plus the kernel `5P-P2-MON60.NPL` XFP2M/XFM2P paths.

---

## 4. Flow control (RFI credit semantics)

RFI (`0x02`, Ready For Input) is VERIFIED as a pure flow-control credit ("I have a fresh input
buffer; you may send", TAD-Message-Formats.md section 6.1). On the wire we OBSERVE the server
appending RFI to the end of several data bursts.

Missing:

- The CREDIT quantity semantics: is one RFI worth one message, one buffer, or N bytes? The
  captures never stall, so we never see back-pressure.
- Whether the client must withhold BDAT until it has received an RFI, and how many BDATs one
  RFI authorises.
- Interaction with the ISRQ/ISRS input-size query pair (never seen in these captures).

**What would close it:** a capture with a slow reader / full buffer so credit exhaustion and
resumption are visible; `SNDRFI` and its callers in `RP-P2-TAD.NPL`.

---

## 5. Timeouts and retransmission at the TAD layer

Not observed at all. No loss, stall or retransmit occurs in either connect-to capture.

- The LAPB layer beneath (`Xmsg.Live/LapbLink`) already models retransmit with an INFERRED
  timeout/retry budget. Whether TAD adds its OWN timers (for example on an unanswered CERS or
  RECO wait) is unknown.
- The secure-delivery retransmit count (peer spec says ~3x) is unconfirmed here
  (XMSG-PROTOCOL.md section 6.1).

**What would close it:** a capture taken over a lossy/delayed link; the `SNDWT` (send-and-wait)
sites in `RP-P2-TAD.NPL` (CERS, RECO, USCN all use it).

---

## 6. Clean teardown (RESE / RECO and DCON)

Partially OBSERVED, mostly INFERRED.

- DCON (`0x09`, disconnect indication) IS present: the client emits a bare-TAD DCON at the end
  of `new-conn-to-102-from-100.pcapng`. So the disconnect OPCODE is OBSERVED, but a full,
  clean, mutually-confirmed teardown handshake is not — we do not see an acknowledging
  disconnect from the peer, nor the LAPB DISC that would follow.
- RESE (`0x16`) / RECO (`0x17`) reset is OBSERVED as server-sends-RESE, client-answers-RECO
  during setup. The reset is used here for re-synchronisation, not teardown.
- The ORDER and completeness of a graceful connect+disconnect is INFERRED (marked
  `INFERRED (no capture yet)` in code) pending a dedicated connect+disconnect capture that is
  being made.

**What would close it:** the pending connect+disconnect capture; `DSTOTA` in `MP-P2-TAD.NPL`.

---

## 7. Error paths (REJE, ERRS, CPCO completion codes)

None of the error/response messages appear in the connect-to captures, so all are structure-
only (VERIFIED layout, no OBSERVED instance):

- REJE (`0xFE`) — reject; echoes the bad opcode. We never provoke one, so we do not know the
  exact conditions the real server rejects on (beyond the source notes).
- ERRS (`0xFB`) — 16-bit SINTRAN error code. The error-code enumeration is unknown (gap 2 in
  TAD-Message-Formats.md section 20).
- CPCO (`0xFA`) — 32-bit completion code, high word first (endianness ASSUMED, TAD formats
  gap 4).
- The relationship between a rejected data message and the follow-up RFI (source shows REJE of
  a BDAT also sends RFI) is VERIFIED in source but never OBSERVED.

**What would close it:** captures that deliberately trigger errors (bad opcode, wrong state,
size mismatch); the `ERRSP` / completion-code constants in the symbol tables.

---

## 8. Summary — trust level of the current implementation

| Area | Trust | Note |
|------|-------|------|
| `[opcode][count][data]` framing | VERIFIED | TAD-Message-Formats.md section 1. |
| Documented opcode meanings | VERIFIED | Against K03/L07/M06 symbol tables. |
| Data-frame envelope (sub-header, secure ACK) | VERIFIED | XMSG-PROTOCOL.md sections 5, 6, 9. |
| Client XSLET setup letter bytes | OBSERVED (single capture) | Templated from one frame. |
| Phase model (Idle..Disconnected) | INFERRED | No phase field exists on the wire. |
| Negotiated parameter VALUES | OBSERVED (single capture) | Only TTYP `0x0000`, OPSV `4C 01 04`. |
| Server responses | OBSERVED (single capture) | Replay only; see `TadReplayServer`. |
| Undocumented opcodes | not understood | Section 1 above. |
| Flow control / timeouts / errors / clean teardown | missing | Sections 4-7 above. |

The `Xmsg.Live/Tad` code is faithful to what was captured and refuses to fabricate beyond it;
it is NOT a general correct TAD implementation and must not be presented as one until the gaps
above are closed.
