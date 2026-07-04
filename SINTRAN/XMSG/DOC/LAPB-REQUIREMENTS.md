# LAPB requirements — ND point-to-point link (compact validation spec)

> **Authoritative source.** The single normative LAPB Layer-2 spec now lives at WSL
> `/home/ronny/repos/os/x25emu/docs/lapb-nd-spec.md` (the X25Emulator repo); where this compact
> note and that spec disagree, the spec wins. The C# `LapbLayer`
> (`SINTRAN/XMSG/SRC/Xmsg.Live/LapbLayer.cs`) has been rebuilt to conform to it — see
> `LAPB-CONFORMANCE.md` in this folder for the section 8.1 checklist mapped to code and tests, and
> the byte vectors S1-S8 in `SINTRAN/XMSG/SRC/Xmsg.Live.Tests/LapbConformanceVectorsTests.cs`.
> This note is retained as a quick companion checklist.

Self-contained requirements for the LAPB/HDLC layer used between Norsk Data SINTRAN III
machines (and our C# node) over an `nd100x --hdlc` byte stream. Written so a reviewer
(human or LLM) can validate an implementation against it WITHOUT other documents.
Normative words: **MUST / MUST NOT / SHOULD**. Source of truth: 13 FCS-verified captures
(1947 frames) + live runs; details in `XMSG-PROTOCOL.md` §2–3. Items marked [ND] deviate
from ITU-T X.25 LAPB; items marked [STD] are standard LAPB the wire is assumed to follow
where the corpus is silent.

## 1. Framing (async HDLC)

- F1. Frames MUST be delimited by `0x7E` flags: `7E | stuffed(frame | FCS) | 7E`.
- F2. Byte-stuffing MUST escape exactly `{0x7E, 0x7D}` as `0x7D, byte^0x20` — **including
  the two FCS bytes** — and de-stuffing MUST be the exact inverse.
- F3. FCS-16: CRC-CCITT poly `0x1021` (reflected `0x8408`), init `0xFFFF`, one's-complement,
  transmitted LOW byte first, computed over `address|control|info` (flags excluded).
  Receiver check: CRC folded over `frame|FCS` == residue `0xF0B8`.
- F4. Any `0x7E` MUST terminate frame accumulation (it can never occur inside a stuffed
  frame). FCS-invalid frames MUST be discarded — and SHOULD be counted/logged, never
  silently (a silent drop hid a live fault once).
- **Test vector:** `01 3F 00 64` → FCS `0x092E` → wire `7E 01 3F 00 64 2E 09 7E`.

## 2. Frame formats

- A1. [ND] Address byte encodes frame ROLE, not station: `0x01` = link management
  (SABM/UA), `0x09` = data transfer (I/RR/RNR/REJ). There is no command/response
  addressing.
- A1b. [ND] **Address bit `0x80` = odd-info-length marker.** An I-frame whose
  information field has an ODD byte count MUST carry address `0x89`; even-length
  I-frames carry `0x09`. VERIFIED across all captures (230+ data frames, zero true
  counterexamples; the apparent ones are byte-stuffing artifacts in the dumps and
  non-XMSG noise in test1). S/U frames always have 2-byte (even) info, so `0x81`/`0x89`
  never occur on them. **A real ND machine silently DISCARDS an odd-length I-frame sent
  with `0x09`** (before LAPB sequence processing — V(R) does not advance; the next
  frame then draws REJ), and the discard is deterministic on retransmission — this was
  the root cause of two live stalls. RX side: accept both values, route on
  `address & 0x7F`, and MAY validate the parity bit against the actual length.
  Mechanism [INFERRED]: the ND-100 word-oriented DMA needs to know whether the final
  16-bit word of the info field carries one or two valid bytes; a wrong flag off-by-ones
  the length/FCS reconstruction. ITU-T LAPB has no such bit — pure ND extension.
- A2. Control byte (modulo-8): I-frame `bit0=0`, `ctrl = N(R)<<5 | P<<4 | N(S)<<1`;
  S-frame `bits1..0=01`, `ctrl = N(R)<<5 | PF<<4 | type<<2 | 01` with RR=0 RNR=1 REJ=2
  (i.e. low nibble RR `0x1`, RNR `0x5`, REJ `0x9`); U-frames: SABM `0x3F`, UA `0x73`,
  DISC `0x43`, DM `0x0F`, FRMR `0x87` [STD for the last three — not in corpus].
- A3. **S-frame subtype MUST be decoded via `control & 0x0F`.** `control & 0x03` alone
  cannot distinguish RR/RNR/REJ. Trap: REJ's low nibble (0x09) equals the data ADDRESS
  byte — logs and parsers MUST NOT conflate them, and MUST NOT label every S-frame "RR".
- A4. [ND] SABM, UA and RR MUST carry a 2-byte info field = the sender's **node (CPU)
  number**, big-endian decimal (100=`0x0064`, 102=`0x0066`, 103=`0x0067`). The state
  machine MUST be initialized with the local node number and stamp it on every
  locally-originated link-management frame. Other U/S frames: unverified on the wire —
  stamp the same way, tolerate either on receive.
- A5. I-frame information field = opaque upper-layer (SINTRAN/XMSG) payload. The LAPB
  layer MUST NOT read or depend on it; max info field 312 bytes.

## 3. Link establishment

- E1. [ND] The link is balanced: BOTH stations send SABM and each answers the peer's
  SABM with UA (info = own node number). The link is up when both directions completed
  SABM→UA.
- E2. **On every SABM sent-and-UA-received, and on every SABM received (answer UA):
  V(S)=V(A)=V(R)=0 and the retransmit queue is cleared.** Sequence "adoption" from the
  peer's first I-frame MUST NOT be implemented (a live desync source; the ND kernel
  resets on SABM).
- E3. On receiving the peer's node number (from SABM/UA/RR info), record it as the
  PHYSICAL neighbour id, exposed read-only. It MUST NOT be used as a routing source —
  on relayed traffic the upper layer's logical source differs from the LAPB peer.
- E4. Periodic RR keepalive (carrying the node number) SHOULD be sent when idle.

## 4. Data transfer state machine

- D1. Maintain V(S) (next send), V(A) (oldest unacked), V(R) (next expected), all mod 8.
- D2. Send window: at most k unacked I-frames outstanding, k ≤ 7. Every sent I-frame
  MUST be retained in a retransmit queue keyed by N(S) until acknowledged — **a single
  "last frame" buffer is non-conformant** (this node routinely has 2–4 in flight).
- D3. Every received I/RR/RNR/REJ MUST process N(R) as a CUMULATIVE ack: frames V(A)
  through N(R)−1 acknowledged and dropped from the queue; V(A)=N(R). N(R) outside
  [V(A), V(S)] is a protocol error (log; recover by link reset) [STD].
- D4. In-sequence I-frame (N(S)==V(R)): deliver payload up, V(R)+=1, acknowledge with
  RR(V(R)) (or piggyback N(R) on the next I-frame).
- D5. Duplicate I-frame (N(S) behind V(R)): discard payload, answer RR with current V(R).
  Out-of-sequence I-frame (gap): discard, send REJ(V(R)) once until it clears.
- D6. **Received REJ(N(R)): MUST immediately retransmit ALL unacknowledged I-frames
  starting at N(R)** (go-back-N).
- D7. **Received RNR: peer is busy** — ack per N(R), then queue (MUST NOT send new
  I-frames); probe with RR; resume on RR. Note: a busy ND peer legally DISCARDS
  I-frames sent into RNR — this is how a lossless TCP link "loses" frames.
- D8. **T1 retransmission timer MUST run in the live event loop** (start on send when
  queue was empty; restart on partial ack; stop when V(A)==V(S)). On expiry: retransmit
  the oldest unacked I-frame (or poll with RR P=1); N2 retries then declare link failure
  and re-establish. An implementation whose runner "passes no tick" fails this.
- D9. [STD] Any received S/I frame with P=1 MUST be answered promptly with an F=1
  supervisory carrying current N(R). (ND P/F usage unproven in captures; implement the
  standard behaviour.)
- D10. DISC received → answer UA, go disconnected [STD, not in corpus].

## 5. Layering rule

- L1. Exactly ONE LAPB state machine per physical link. It knows nothing of XMSG; the
  XMSG datagram sequence (Flags1/secure-ACK) is an independent end-to-end layer and MUST
  NOT share state, counters or identity with LAPB. U/S frames never carry XMSG bytes.

## 6. Validator checklist (ask these of the implementation)

1. Does S-frame parsing extract `control & 0x0F`? Do logs print RR/RNR/REJ distinctly?
2. Is there a retransmit queue covering the whole window (not one buffer)? Show the
   go-back-N path triggered by REJ.
3. Where is T1 ticked in the LIVE runner (not just in a test)? What are T1/N2 values?
4. Is RNR handled (busy flag blocks new I-frames, RR probe)?
5. Is P=1 answered with F=1?
6. On SABM (sent or received): are V(S)/V(A)/V(R) reset to 0 and the queue cleared?
   Grep for any "adopt peer sequence" logic — it must be gone.
7. Are FCS bytes byte-stuffed on TX? Does the test vector in §1 round-trip?
8. Are FCS-invalid RX frames counted/logged?
9. Is the state machine constructed with the 16-bit node number, and stamped on
   SABM/UA/RR info fields? Is the peer's number kept OUT of routing decisions?
10. Can two I-frames be outstanding and the FIRST be retransmitted on REJ(first)?
    (Regression test for the depth-1-buffer bug.)
11. Does any code path send an I-frame while peer-busy (RNR) is set?
12. Is the LAPB module free of references to XMSG types/fields (layering, §5)?

## 7. Minimal conformance scenarios (unit-testable)

- S1. Balanced bring-up: exchange SABM/UA both ways → link up, all counters 0.
- S2. Send I(ns=0), I(ns=1); receive REJ nr=0 → both frames retransmitted in order.
- S3. Send I(ns=0); receive RNR nr=1 → ack recorded, sends held; RR nr=1 → resume.
- S4. Send I(ns=0); NO response → T1 fires → retransmit; ×N2 → link declared down,
  SABM re-sent.
- S5. Receive I(ns=V(R)+1) (gap) → REJ(V(R)) emitted, payload not delivered.
- S6. Receive RR P=1 → RR F=1 with current N(R) emitted immediately.
- S7. Mid-session SABM received → UA sent, counters reset, queue cleared; next I goes
  out with ns=0.
- S8. TX frame whose FCS bytes contain 0x7E/0x7D → correctly stuffed and decodable by
  the RX path.

*Compiled 2026-07-04 from `XMSG-PROTOCOL.md` §2–3.6/§17 and the confirmed live defects
in the master review (`XMSG-MASTER-REVIEW-ANSWERS-2026-07-03.md`).*
