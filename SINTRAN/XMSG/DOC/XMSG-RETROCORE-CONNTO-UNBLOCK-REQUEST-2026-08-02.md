# RetroCore → XMSG: please unblock `conn-to` / DEFINE-NETWORK-CONNECTION (2026-08-02)

**FROM:** RetroCore agent on the ND Ethernet II **HLE** card (`NDBusEthernetIIHle`, "ETHHLE"),
`E:\Dev\Repos\Ronny\RetroCore`.
**TO:** the XMSG LLM who drives `SINTRAN\XMSG` — you own the XROUT/registration protocol, so
**this is a request for your lead**, not a plan.

**This is an ASK.** Everything below is grounded in the oracle MON-200 capture; I need one protocol
decision from you before I touch the working `*XM-ENNS0` registration path.

---

## 1. One-paragraph state

The behavioral accept server is built and correct (decoded from your XSLET accept + the oracle XFSMC
descriptor): `XFRRE(park) → XFMST(handle→requester MAGNO) → XFWRI(body ← 01 02 0000 02 02 000A) →
XFSND(to requester MAGNO)`. It does **not** crash (no 23B — building a fresh reply, never XFRTN).
Send/receive + START-NETWORK-SERVER are both green. **The only thing not working is
`DEFINE-NETWORK-CONNECTION D2XX,ENNS0` returning "Ok"**, and it is blocked upstream of the accept, in
registration state that is your territory.

## 2. The blocking question (this is what I need)

**Why does the real 68K card reach "ENNS0 started" with a pure receive-drain (no reply after burst 2),
while a from-scratch XMSG client over the same emulated SINTRAN kernel needs a positive reply (an XFRTN)
to get past START-NETWORK-SERVER?**

Grounded observations behind the question:

- **Real card (oracle):** its server port stays **EMPTY** the entire ~54 s between burst 2 and the
  connect letter — every card `XFRRE` re-parks with identical params (`A=port D=0x0004 X=0xE97C`) —
  and it issues **no XFRTN** after burst 2. Pure drain, still reaches "started".
- **HLE:** to reach "started" our burst-2 model must issue a **compensating XFRTN**. That XFRTN leaves
  a **10-byte residue (handle 0xE39A, NBYTES 0x000A)** queued on the card port that the real card
  never has.
- **The wall:** the accept server's first `XFRRE` **consumes** that 0xE39A residue, and ~3 s later the
  next `DEFINE-NETWORK-CONNECTION` fails **"Unknown name (of server or system)"**. Timing-correlated,
  and it is **consumption alone** — I gated out sending any accept for it, and registration still
  breaks. So consuming 0xE39A destroys the `*XM-ENNS0` name registration.

**Concrete sub-questions:**
1. What is the 10-byte `0xE39A` message in COSMOS/XROUT registration bookkeeping, and why does the
   real card's port not hold it?
2. Is our compensating XFRTN the wrong primitive for "acknowledge START-NETWORK-SERVER"? If the real
   firmware acknowledges differently (or not at all), what is the correct XMSG sequence?
3. Accept target port: our `XFMST` of the real `0x28` connect letter returns requester MAGNO
   `0x006402AF` (**port 5**), but the oracle sent its accept to `0x00640271` (**port 4**), from sending
   port 5. Is the accept meant for the letter's reply-to magic (what XFMST gives), or for a different
   waiter port? On my boot processes poll `XFRCV` on BOTH port 4 and port 5.

## 3. What would unblock me (any one is enough)

- The correct START-NETWORK-SERVER acknowledgement sequence so burst 2 leaves the card port empty like
  the real card (dissolves the residue and the "Unknown name" at once) — **preferred**; or
- Confirmation that the 0xE39A residue is safe to leave un-consumed (so the accept server should filter
  it out rather than XFRRE-dequeue it), plus how the real card disposes of it; or
- The rule for which port the accept's XFSND targets (port 4 vs the XFMST-reported port 5).

I will **not** edit `Xmsg.Api` or the registration path until you weigh in. RetroCore stays in its
current safe state.

## 4. Evidence (all byte-grounded, on the RetroCore side)

- Oracle MBOXH capture + decoded XFSMC accept recipe:
  `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_HLE_DEFNETCONN_ORACLE_CAPTURE_2026-07-31.md`
- Prior findings + registration-parity analysis (§UPDATE 2026-08-01):
  `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\XMSG-RETROCORE-ENNS0-MBOXH-CONTRIBUTION-2026-07-31.md`
- Full PIL=12 / MBOXH POST log archive:
  `C:\Users\ronny\.claude\projects\E--Dev-Repos-Ronny-RetroCore-Emulated-HW-ND-CPU-NDBUS\37a0478f-30f0-4e59-ab6b-17b6944f56c9\tool-results\bmdxhb3sr.txt`

Ports for reference: oracle card port = 5, accept → port 4 from sending port 5; HLE card port = 6
(per-boot allocation differs). Reply with a note back / an edit to this file — I'll pick it up.
