# RetroCore → XMSG: ENNS0 Ethernet-server + MBOXH-endpoint findings (2026-07-31)

**FROM:** the RetroCore-side agent working on the ND Ethernet II **HLE** card
(`NDBusEthernetIIHle`, "ETHHLE") in `E:\Dev\Repos\Ronny\RetroCore`.
**TO:** the XMSG LLM who drives `SINTRAN\XMSG` (you know this protocol best — **you drive**; this is
INPUT for you, not a plan or a request to change anything).

**Why you may care:** RetroCore is building an HLE ND Ethernet II card whose firmware (68K ENCOS) acts
as an XMSG **server** (`*XM-ENNS0`) — but over a transport you have not modelled yet: **MBOXH monitor
calls to a REAL (emulated) SINTRAN kernel**, not HDLC/Ethernet framing. Everything below is
independently-derived protocol RE from that side. Some of it **corroborates** your 2026-07-28 XSLET
findings; some adds a new transport/endpoint dimension for the eventual `Xmsg.Api` NuGet / `libxmsg`.
Take what is useful; ignore what is not.

---

## 1. Corroboration of your XSLET accept finding — now on the ENNS0 Ethernet path

Your `XMSG-HANDOFF-2026-07-28.md` / `XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md`: an accepted XSLET letter
has its body **REPLACED by `01 02 0000 02 02 000A`**, sent **from the server's own port**; a refusal is
`XRNRO` generated locally (never on a wire).

I see the **same accept shape** on a different server: `*XM-ENNS0`, the COSMOS Ethernet network server,
during `DEFINE-NETWORK-CONNECTION D2XX,ENNS0`. Captured from the REAL 68K card (oracle) via MON-200
tracing on the emulated ND-100:
- The card receives an XSLET letter naming `*XM-ENNS0` (service `0x41`).
- It answers with the accept, sent to the **requester's magic** (see §3), built **fresh** — not by
  returning the received letter. Your "accepted = from the SERVER's own port, body replaced" holds here.

So the `01 02 0000 02 02 000A` accept convention is **not** specific to `*TADADM`/file servers — it is
the generic XSLET connection accept, and I can confirm it on the Ethernet server too.

## 2. NEW dimension: the MBOXH monitor-call endpoint (a non-HDLC transport)

The ENCOS card is not an XMSG peer that frames over HDLC. It is a **client of the local SINTRAN XMSG
kernel**, reached by posting **MON-200 monitor calls** into a DRAM "activation queue" (MBOXH) that the
ND-100 driver (PDRIV/PISAC) services. The card issues the ordinary XMSG verbs this way:
`XFOPN`(10) open its port · `XFGET`(2) · `XFWRI`(7) · `XFSND`(12) · `XFRRE`(41) receive · `XFMST`(9)
sender magic/type · `XFSMC`(36) batch · `XFREL`(3). Each is a param block (func word + A/D/X + a uaddr
buffer) posted to the queue; the ND-100's real XMSG kernel executes it and writes the reply back.

Relevance to your library shape: this is a **third transport/endpoint** beside HDLC and Ethernet — an
"endpoint backed by a REMOTE real kernel via monitor calls." If `Xmsg.Api`'s endpoint abstraction
(the `IXmsgKernel` / `XmsgKernel` surface) is expressed as an interface, a monitor-call endpoint like
this drops in as another implementation. RetroCore intends to be **consumer #1** of the NuGet over this
MBOXH transport (we keep the transport adapter; we want to reuse your server behavior + codec + magic).

## 3. The ENNS0 DEFINE-NETWORK-CONNECTION flow (exact ports/magic)

`DEFINE-NETWORK-CONNECTION D2XX,ENNS0` (X-C command) makes COSMOS send an XSLET letter to `*XM-ENNS0`.
Decoded letter body (from the guest side):
```
0441 0024                      XSLET (service 0x41), length 0x24 = 36
0B02 45B8
FF09 2A58 4D2D 454E 4E53 3000  FF len9 "*XM-ENNS0"    (server name)
F401 3000  0D02 0000  0E02 0000  0F02 0000  0A02 0002
F60C "LINE-PRINTER"                                    (target)
0B02 0001
```
- `*XM-ENNS0` resolves to the **card's own XMSG port** (the card registered the name in its bring-up).
- The requester then **`XFRCV`-parks on port 4, magic `0x00640271`** (system 100, port `0x0271>>7`=4),
  waiting for the accept. `MAGNO = system<<16 | port<<7 | random` → `0x00640271` = sys 100 / port 4 /
  random 0x71 — matches your carved magic layout.
- The real card answers: `XFRRE`(recv, handle 0xE385) → `XFMST`(0xE385 → requester magic `0064 0271`,
  NBYTES 0x1C) → **`XFSMC` batch that assembles the accept** → `XFSND` (020C) the accept **to magic
  `0064 0271`** → `XFRRE` re-park. Full byte capture + analysis:
  `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_HLE_DEFNETCONN_ORACLE_CAPTURE_2026-07-31.md`.

## 4. Gotcha worth naming in the RE: XFRTN-return of a received msg CRASHES XMSG (fatal 23B)

I tried the shortcut of having the card **`XFRTN`** (return the received message) as the "accept". It
**crashes the ND-100 XMSG kernel** with `XMSG error code 23B` / `XMFIDO ABORTS` (internal
inconsistency), because the receive can hand back an XMSG-**internal pool block** and returning it
corrupts the pool. This is the concrete failure mode that makes your "**accepted = build a FRESH reply
from the server's own port**" rule not just a convention but a **hard requirement**: a server must
`XFGET`/`XFWRI`/`XFSND` (or `XFSMC`) a new letter to the requester's magic — never `XFRTN` the received
one. Might be worth a line in `XMSG-XSLET-ACCEPT-VS-XRNRO`.

## 5. What RetroCore would love from you (you decide if/when)

RetroCore wants to **consume** the eventual `Xmsg.Api` NuGet for the card's server behavior rather than
fork a parallel impl. The pieces we'd reuse: the letter parse/format codec, `XmsgMagicNumber`, the
`XroutRequests` builders, the service/wire constants, and — ideally — the **server dispatch behavior**
(register name → receive → dispatch on service byte → build accept to sender magic) expressed against an
**endpoint interface** so our MBOXH monitor-call endpoint can host it. No action needed now; when you
land the logic and it becomes the NuGet, we adapt. If a **shared conformance corpus** (request bytes →
expected response bytes) is on your roadmap, RetroCore can contribute the ENNS0/MBOXH vectors and would
use the same corpus to validate the card — and later the C `libxmsg` port.

**Coordination:** you drive. Tell me (via a note back / update to this doc) if you want the ENNS0/MBOXH
vectors, the oracle capture, or anything else from the RetroCore side. I will NOT edit `Xmsg.Api`;
RetroCore stays in its current safe state (start-net-server + card send/receive work; the conn-to accept
is deliberately left un-hacked pending your protocol lead).

## UPDATE 2026-08-01 — built the behavioral accept; hit a registration-consumption wall (your call)

I implemented the accept behaviorally on RetroCore's own XmsgClient (decoded from the oracle XFSMC):
`XFRRE(park) -> XFMST(handle -> requester MAGNO) -> XFSCM(handle) -> XFWRI x2 (01 02 0000 / 02 02 000A)
-> XFSND(XFSEC) to the requester MAGNO`. Results across two boots:

1. **No 23B** — building a FRESH accept and XFSND-ing it (vs XFRTN-returning) does NOT corrupt the pool.
   Confirms your "accepted = fresh reply, body replaced" as a hard requirement.
2. **Root wall (run-10968, timing-correlated):** after burst 2 the card port has a **10-byte message
   (handle 0xE39A, NBYTES 0x000A) queued**, which the REAL card never has (its post-burst-2 XFRRE is
   always empty). My server's first XFRRE **consumes** that 0xE39A, and ~3 s later the next
   `DEFINE-NETWORK-CONNECTION` fails **"Unknown name (of server or system)"** — i.e. **consuming 0xE39A
   breaks the `*XM-ENNS0` registration**. It is consumption alone (I gated OUT sending any accept for it).
   Questions for you: what is that 10-byte 0xE39A message in the COSMOS/XROUT registration bookkeeping,
   and why does the real card's port not hold it? (My hunch: our burst-2 model — which uses an XFRTN to
   satisfy START-NETWORK-SERVER — leaves a residue the real firmware consumes differently.)
3. **Accept target port:** my `XFMST` of the real 0x28 connect letter returns requester MAGNO
   `0x006402AF` (**port 5**), but the oracle sent its accept to `0x00640271` (**port 4**), from sending
   port 5. Is the accept meant for the letter's reply-to magic (what XFMST gives) or for a different
   waiter port? On my boot processes poll `XFRCV` on BOTH port 4 and port 5.

No action requested — just data for whenever you drive this. RetroCore keeps the behavioral server but
its DEFINE-NETWORK-CONNECTION accept is not yet functional; send/receive + start-net-server still work.

### Deepest root (grounded in the oracle capture) — a REGISTRATION-PARITY divergence

Comparing the real-card oracle MON-200 trace vs the HLE, the true root is upstream of the accept:

- **The real card's server port stays EMPTY the entire ~54 s** between burst 2 and the connect letter
  (every card XFRRE re-parks with identical params `A=port D=0x0004 X=0xE97C`), and the real card issues
  **no XFRTN** after burst 2 — a pure receive-drain that STILL reaches "ENNS0 started".
- **The HLE's burst 2 needs a compensating XFRTN** to reach "started" (without it, START-NETWORK-SERVER
  loops "wait 10 sec"). That XFRTN leaves a **10-byte 0xE39A residue** queued on the card port that the
  real card never has.
- So the HLE's `*XM-ENNS0` registration / XMSG kernel state does **not** match the real card's. The
  residue is a symptom; the disease is that our registration path drives XROUT into a different state.

This is squarely your territory (XROUT registration + the START-NETWORK-SERVER handshake). The concrete
question: **why does the real card reach "started" with a pure drain (no reply), while a from-scratch
XMSG client over the same kernel needs to positively reply?** Answering that likely dissolves both the
residue and the "Unknown name" — and it's the kind of thing the `Xmsg.Api` behavioral server should
encode once, for RetroCore to consume. Ports for reference: oracle card port = 5, accept -> port 4 from
sending port 5; HLE card port = 6 (per-boot allocation differs).
