# Why XROUT classifies a direct Ethernet peer as "WAN?" and blocks connect-to (2026-07-24)

> **RESOLVED (2026-07-25).** The `WAN?` never cleared because the peer's inbound frames were
> being DISCARDED by the receiver firmware, so the liveness round-trip that would confirm the
> LAN neighbour never completed. Root cause was NOT a routing-layer defect but a LANCE
> FCS double-count on TRANSMIT (see the correction banner in
> `ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md` and `Am2990Lance.cs`). With that fixed, the
> round-trip completes, XROUT promotes `WAN? -> LAN`, and `connect-to d102` succeeds. This
> doc's analysis of the WAN?/marker semantics and the correct operator config
> (DEFINE-NETWORK-CONNECTION for the LAN peer, NOT DEFINE-SYSTEM-ROUTE) remains valid. The
> full multi-node config - LAN + HDLC + relay (ENABLE-ROUTE-THROUGH, DEFINE-SYSTEM-ROUTE
> ,,Dxx <via>) - is consolidated in `COSMOS-MULTI-NODE-NETWORK-2026-07-25.md`.

Reverse-engineering of the XROUT routing layer to explain why two emulated ND-100
nodes on ONE Ethernet (100 <-> 102) never become LAN neighbours: `LIST-ROUTES`
keeps `L:/T:/A: *->WAN?->102`, and `connect-to d102` fails with "Remote system
D102 not available".

Convention: **[V]** = quoted from an ND manual / disassembly / captured frame in
this repo. **[I]** = inferred (supported, not directly quoted). **[OPEN]** = not
resolved from the sources here.

Primary sources (all in this repo):
- ND-60.134.02 SINTRAN III Communication Guide - the XROUT service spec
  (`Reference-Manuals\ND-60.134.2 EN SINTRAN III Communication Guide.md`).
- SINTRAN III Handbok for driftsansvarlig ND-30.003.7 NO - the COSMOS Basic
  Module operator procedures, section 5.1.2
  (`Reference-Manuals\SINTRAN III Handbok for driftsansvarlig.md`).
- COSMOS Ethernet Option install description ND-210580-02
  (`Installation\Installation-Description\ND-210580-02-EN.md`).
- XMSG protocol RE section 9.1 (`SINTRAN\XMSG\DOC\XMSG-PROTOCOL.md`).
- Same-day RX-forward root cause
  (`SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md`).

---

## 0. Headline

1. **`WAN?` is not "wide area" per se - it is an UNCONFIRMED / unresolved route
   record.** The `?` means XROUT has a *table* entry for system 102 (put there by
   `DEFINE-NETWORK-CONNECTION`) but has **never received confirmation that 102 is a
   live, reachable neighbour on the same Ethernet.** Until that confirmation
   arrives, XROUT cannot promote the entry to a `LAN` (Neighbour) route and prints
   the fallback `WAN?`. `[I]` on the exact glyph; `[V]` on the mechanism below.

2. **`DEFINE-NETWORK-CONNECTION` does NOT create a LAN-neighbour route.** It creates
   a **"via network server" (connection-type 3)** entry: "reach system 102 through
   the ENNS0 Ethernet server". A LAN-neighbour route (connection-type **1 =
   Neighbour**) is something XROUT **learns dynamically when the link to that
   machine starts up** - it is never typed in by the operator. `[V]` ND-60.134
   section 4.4.2.13.

3. **The confirmation that promotes the route to a live neighbour never completes,
   because inbound Ethernet frames are not reaching XROUT at runtime.** The
   same-day RX-forward decode shows the 68000 firmware **discards** received frames
   at gate 3 (empty receive-buffer pool) and therefore never signals the ND-100 /
   XROUT. So XROUT never sees 102's traffic come back up, the liveness (`A:`) probe
   is never answered, and the route stays unconfirmed = `WAN?`. This is the true
   blocker; it is one layer below the routing config, not in it.

4. **There is no separate COSMOS "network number" to set** in this SINTRAN-L
   Ethernet model. `[V]` neither ND-60.134 nor the driftsansvarlig Ethernet
   procedure use one - the WAN/LAN attribute is the *server's* flag
   (`START-NETWORK-SERVER ...,,,N` = LAN), and the per-destination class comes from
   the connection-type enum, not a network id. (See section 4.)

---

## 1. How XROUT decides LAN vs WAN vs "WAN?"  [V for the enum, I for the glyph]

The route table is queried by the XROUT service **XSGMC** ("Get Routing
Information for a Machine", ND-60.134 4.4.2.14) / its newer XSGSY form. The reply
is a 4-byte record; the operator's `LIST-ROUTES` L:/T:/A: rows are formatted from
it:

| Byte / param | Meaning (ND-60.134 4.4.2.14 `[V]`; enum extended in XMSG-PROTOCOL 9.1 `[V]`) |
|---|---|
| 1 | System number (or next higher known; 0 = none) |
| 2 | **Connection type: 0=Unavailable, 1=Neighbour, 2=Via (relay), 3=Via network server, 4=Local** |
| 3 | Extra info (link index / relay machine / server subaddress - depends on byte 2) |
| 4 | Network info: `<= 0377B` => hop count in low byte; `>= 0400B` => **#WANs in high byte**, #hops in low byte |

- **`LAN`** is printed when the destination resolves to a same-network reachable
  path with **zero WANs crossed** (param 4 high byte = 0) - typically
  connection-type **1 (Neighbour)** or a network-server route whose server is LAN.
- **`WAN`** is printed when the path crosses one or more wide-area gateways: param 4
  high byte `>= 1`, i.e. the route goes through a link/server flagged
  **`XL5WA = 14` "Wide area gateway"** (the `Y` answer to "Wide Area Network(Y/N)?"
  at `START-NETWORK-SERVER`). `[V]` XL5WA symbol + XSNSI "Network type 0=local,
  1=wide"; see `XMSG-COMMAND-START-NETWORK-SERVER-DECODE-2026-07-23.md` section 8a.
- **`WAN?`** (trailing `?`) `[I]`: the record exists but XROUT could **not confirm
  the path** - the destination is neither a confirmed Neighbour (1) nor confirmed
  reachable via the server, so the class cannot be computed and XROUT emits the
  unresolved marker. The `A:` (actual-path) row is what carries live confirmation
  (see section 3); when that liveness never resolves, all three rows fall back to
  `WAN?`. The `?` is "unconfirmed/assumed", not a positive wide-area result.

`[V]` The three `LIST-ROUTES` columns are three different lookups:
`L:` = local route table, `T:` = tables (what the queried XROUT holds),
`A:` = **actual path / liveness probe** ("is the system really reachable now") -
this mapping is verified live in XMSG-PROTOCOL section 9.1.1: `list-route` sends
**two** XSGSY datagrams, `Flags1=0000/ctr=0x13` drives L:/T:, and
`Flags1=0001/ctr=0x12` is the liveness probe that drives `A:`. If the second
(liveness) datagram is not answered, `list-route` prints the table rows but reports
**no live access** with `A:*->`.

---

## 2. What DEFINE-NETWORK-CONNECTION actually does - and why it is not enough alone

`[V]` ND-60.134 4.4.2.13 (XSDMC "Define Machine Routing") is the definitive text:

> "Whereas Define Remote Name (XSDRN) defines a mapping of a machine name to an
> XMSG machine number, Define Machine Routing (XSDMC) specifies **how to get to
> that machine. This is not necessary if the machine is directly connected, since
> XROUT will find out when the link to that machine starts up**, but is necessary
> ... for both the local machine and machines connected via neighbours."

`[V]` Driftsansvarlig 5.1.2 (Ethernet, page 195-196) confirms the same for the
Ethernet path and gives the canonical XMSG-START file:

```
DEFINE-REMOTE-NAME,,PARTNER-1,6888        <- name -> system number (XSDRN)
START-NETWORK-SERVER ENNS0,,,N            <- N = LAN (not WAN)
DEFINE-NETWORK-CONNECTION,PARTNER-1,ENNS0 <- reach PARTNER-1 *via the ENNS0 server*
```

and states: "Nar du definerer ruter til fjernmaskiner, er det **ikke** nodvendig a
definere ruter til **tilstotende** maskiner" = *you do NOT define routes to
adjacent (directly-connected) machines* - those are learned.

So the operator sequence in the report is **structurally correct** for Ethernet:
`DEFINE-REMOTE-NAME` (D100/D102 -> system numbers), `START-NETWORK-SERVER ENNS0
,,,N`, `DEFINE-NETWORK-CONNECTION D102 ENNS0`. What that produces in the table is a
**connection-type-3 "via network server"** record for 102 - a *declaration of
intent*, "102 is out there behind ENNS0". It is **NOT** a promise that 102 is
reachable. XROUT only upgrades it to a live/Neighbour route after it actually
**hears from 102 over the Ethernet** (the dynamic learning in the quote above).

That upgrade is exactly what is missing here.

### The `DEFINE-SYSTEM-ROUTE,,D102` mis-step (observed "no route to system 102")
`[V]` Driftsansvarlig 5.1.2 (page 195): `DEFINE-SYSTEM-ROUTE <machine> {via
machine}` is the **HDLC / Megalink** command; its last parameter defaults to
**NONE = direct connection**. Typing `DEFINE-SYSTEM-ROUTE,,D102` with no via-arg
therefore declared 102 as a **directly-cabled HDLC neighbour** - but there is no
HDLC link, so the route resolves to "no route to system 102". That command is the
wrong tool for an Ethernet peer; the Ethernet equivalent is
`DEFINE-NETWORK-CONNECTION`. Do not use `DEFINE-SYSTEM-ROUTE` for the Ethernet
case.

---

## 3. Root cause: the neighbour is never CONFIRMED because RX never reaches XROUT

Dynamic neighbour learning (section 2) requires bidirectional XROUT-to-XROUT
traffic: the local XROUT must receive 102's link-startup / routing / liveness
frames and vice-versa. On the wire this is the `A:` liveness probe of section 1 -
the `Flags1=0001` XSGSY datagram that must be **answered by 102**.

`[V]` The same-day decode `ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md` proves that
inbound Ethernet frames are **discarded inside the 68000 firmware RX handler
(RCVCOMPLETE @0x5C42)** and never delivered to the ND-100:

- For a correctly-addressed 802.3 COSMOS frame the discard is **gate 3**: the
  card's free XMSG receive-buffer pool at DRAM `0x188C6` is **empty**, so
  RCVCOMPLETE has nowhere to put the message, clears its "message built" pointer,
  and takes the `0x6034` branch that re-appends the buffer and yields -
  **no `jsr 0x134E6`, no SCIP, no INT12, no notification to XROUT.**
- The pool is filled by the ND-100 / ENNS0 side posting receive buffers (receive
  credits). If ENNS0 has not posted them, **every** inbound frame is silently
  dropped.

Consequence for routing: 102's liveness reply (and 102's own routing/hello) is
delivered to the card but dropped before XROUT sees it. The local XROUT therefore:
- never gets the confirmation that would promote the type-3 record to a live/
  Neighbour route, and
- never answers 102's probe either (same discard on 102's card), so the block is
  symmetric - matching the symmetric `WAN?` on both nodes.

**That is why the route sticks at `WAN?`: the routing config is right, but the
XROUT-level round-trip that would confirm the LAN neighbour never completes,
because the received frames are discarded one layer below XROUT.**

Note on the task premise: the unit test proves the firmware *can* deliver a frame
**when a receive buffer is posted**; it does not prove that at runtime, with ENNS0
in its current state, the pool is non-empty. The RX-forward decode says it is
empty at runtime. These are consistent: capability != runtime delivery. `[I]`

---

## 4. Is a COSMOS "network number" involved?  [V - no]

No. Searched ND-60.134 (XROUT service spec), the driftsansvarlig COSMOS Basic
Module chapter, and ND-210580. The SINTRAN-L Ethernet routing model has **no
per-node network-number that both peers must share for LAN**. The pieces that
exist are:
- **System number** (`DEFINE-REMOTE-NAME name,number`) - the node identity.
- **Server LAN/WAN flag** - the `,,,N` / `,,,Y` at `START-NETWORK-SERVER`
  (`XL5WA`). `N` = LAN. Both nodes here use `N`, which is correct.
- **Connection-type enum** - computed by XROUT, not configured.

So "different network numbers" is NOT the cause. (Marking this resolved rather than
leaving it a candidate: the sources do not contain such a field.)

---

## 5. Correct operator / config sequence to make two ND systems LAN neighbours

Per driftsansvarlig 5.1.2 + ND-210580 page 6, on **each** node (values shown for
node 100; mirror on 102):

```
'  --- in the XMSG-STARTUP mode file, BEFORE starting the server ---
@(UTILITY)XMSG-COMMAND
SET-PRIVILEGED
DEFINE-REMOTE-NAME,,D100,100        ' name -> system number, for EVERY node incl. self
DEFINE-REMOTE-NAME,,D102,102
'  --- then the server + connection declarations ---
START-NETWORK-SERVER ENNS0,,,N      ' N = LAN  (NOT Y - Y flags XL5WA = wide area)
DEFINE-NETWORK-CONNECTION D102 ENNS0,,0,0,0,0   ' reach 102 via the Ethernet server
EXIT
```

- Do **NOT** use `DEFINE-SYSTEM-ROUTE` for the Ethernet peer (section 2) - that is
  the HDLC/Megalink command and produces "no route" or a bogus direct-link entry.
- `,,,N` (LAN) is mandatory for the LAN classification; `,,,Y` would positively
  mark everything behind ENNS0 as WAN (`XL5WA`).
- `DEFINE-REMOTE-NAME` for BOTH nodes (including self) must run **before**
  `START-NETWORK-SERVER`, in the XMSG-STARTUP file (name<->number is what the route
  record binds to).

**This config is necessary but by itself will still show `WAN?` until the
XROUT-level round-trip completes.** The remaining, decisive step is not an operator
command - it is that **received frames must actually reach XROUT** so the neighbour
is confirmed:

- ENNS0 on each node must post receive buffers so the card's `0x188C6` pool is
  non-empty (else gate-3 discard - section 3). This is the ENNS0 LU-2240B
  input-path issue tracked in
  `ENNS0-Startup-RE-2026-07-23\ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B-2026-07-23.md`.
- Once inbound frames are delivered up, the `A:` liveness probe (Flags1=0001 XSGSY)
  will be answered, XROUT promotes the type-3 record to a confirmed reachable
  neighbour, and `LIST-ROUTES` flips `WAN?` -> `LAN`; `connect-to d102` then
  succeeds.

Verification loop after fixing the RX path: on node 100 run `LIST-ROUTES,,D102`
and watch the `A:` row change from `*->` (no access) to a resolved LAN path; that
row is the live-confirmation indicator (section 1 / XMSG-PROTOCOL 9.1.1).

---

## 6. Emulator implication

- **No new emulator field drives the LAN/WAN glyph itself** - LAN/WAN/`WAN?` is
  computed by the guest XROUT from the connection-type enum + #WANs, not read from a
  controller status register. `[V]` (the enum/#WANs come from XROUT's own tables).
- **The emulator IS implicated, but at the RX-delivery layer, not the routing
  layer.** The blocker is the gate-3 receive-buffer-pool discard documented in
  `ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md`. That decode concluded the C# LANCE
  RX / descriptor / RINT / 68K interrupt path is correct and the discard is
  *correct firmware logic* reacting to an empty pool - i.e. the fix is to get
  ENNS0 to post receive buffers (guest/driver sequence), not to patch the notify
  path. Instrumentation (`[68K-RX] DELIVERED` @0x6020 / `DISCARDED` @0x6034 + pool
  dump at 0x5C42) is already in `NDBusEthernetII.cs` to confirm the gate on the
  next two-node run.
- **Recommended validation:** run the two-node bridge, drive `connect-to d102`, and
  read the `[68K-RX]` lines on the receiver. `DELIVERED` => RX reaches XROUT and the
  route should confirm (then any residual failure is pure XROUT config); `DISCARDED
  + POOL EMPTY` => the receive-buffer posting is still the blocker and the `WAN?`
  will persist regardless of routing commands.

No code change is warranted in the XROUT/routing layer. No SINTRAN image was
modified.

---

## 7. Evidence index

| Claim | Source |
|---|---|
| Direct-connected machine route learned dynamically "when the link starts up" | ND-60.134 4.4.2.13 (XSDMC) `[V]` |
| Connection-type enum 0/1/2/(3)/4 | ND-60.134 4.4.2.14 `[V]` + XMSG-PROTOCOL 9.1 `[V]` |
| Param 4 encodes #WANs crossed | XMSG-PROTOCOL 9.1 line 1006 `[V]` |
| Do NOT define routes to adjacent Ethernet machines; use DEFINE-NETWORK-CONNECTION | Driftsansvarlig 5.1.2 page 195-196 `[V]` |
| DEFINE-SYSTEM-ROUTE default via = NONE = direct (HDLC command) | Driftsansvarlig 5.1.2 page 195 `[V]` |
| `,,,N` = LAN, `,,,Y` = WAN gateway (XL5WA=14) | ND-210580 p.6 + START-NETWORK-SERVER decode section 8 `[V]` |
| L:/T:/A: = table / tables / liveness; two XSGSY datagrams | XMSG-PROTOCOL 9.1.1 `[V]` |
| Inbound frames discarded at gate 3 (empty pool), no SCIP to XROUT | ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md `[V]` |
| "WAN?" = unconfirmed/unresolved route marker | INFERRED `[I]` (no manual quotes the glyph) |
| No COSMOS "network number" field in this model | absence across ND-60.134 / driftsansvarlig / ND-210580 `[V-negative]` |

**This file:**
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\XROUT-LAN-NEIGHBOUR-ROUTING-RE-2026-07-24.md`
