# ENNS0 start-net + DEF-NETWORK-CONN - what each message IS and DOES

**Date:** 2026-08-10  **Sources:** ND-60.164.3 COSMOS Programmer Guide sec 3.1-3.17;
the decoded golden flow `ennS0-exchange/lle-oracle-exchange.json`.

This turns the byte diff into a semantic model: what every message in the exchange means, why
it is sent, and what its reply is for. Service numbers from XMSG-PL-VALUES-M.INCL; a record's
word0 = serial byte | service/status byte (service on a request, 0=XRSOK on a reply).

---

## The three commands and the XROUT services each drives

### A. `DEF-REMOTE,,D102 102`  ->  XSDRN (73, "Define Remote Name")
- Message: request, string p1 = "D102", int p2 = 102.
- Does: puts the mapping name "D102" -> system number 102 in XROUT's NAME table. Manual 3.12:
  "All letters that are addressed to that system (parameter 2 in XSLET/XSLEK) will be forwarded
  to the specified system." Reply XRSOK.
- Purpose: teach the local XROUT what the operator's system NAME means as a NUMBER. Pure local
  table update; nothing leaves the machine.

### B. `start-net-server,enns0,,,N`  ->  XSLEK, then XSNAM (by the card), then XSNET
This brings the ENNS0 card up as a "network server" - a stand-in for an HDLC/Megalink link that
carries XMSG frames over Ethernet instead (manual 3.17).

1. `0x0154` **XSLEK** (84, "Send Letter and Kick") p1="*XM-ENNS0", p3="ENNS0", p10=1.
   - A letter to the *XM-ENNS0 server that ALSO kicks the server awake if it is not running
     (manual 3.5). This is what actually starts the card's server. The card replies, and by
     replying "gives away its own magic number" (the letter mechanism, sec 1.3 / 3.4).
2. `0x5342` **XSNAM** (66, "Name a Port") p1="*XM-ENNS0", sent FROM the card's server port.
   - Publishes the name so future letters addressed to "*XM-ENNS0" route to that port (manual
     3.1: the naming call must be sent from the port it names; XROUT uses the sending port).
3. `0x0255` **XSNET** (85, "Start Network Server") p1 = the server's MAGIC number.
   - Manual 3.17: param 1 = "Magic number of server port... must have been obtained previously
     by direct communication with the server, e.g. using XSLEK." Registers the card's magic in
     the ROUTING layer as a network server. Reply `0x0200` XRSOK p1=state p2=... .
   - Oracle magic p1 = 0x006402FE (system 100, port word 0x02FE -> port 5); HLE = 0x00640271
     (port 4). BOTH succeed here.
- Result: console "Network server ENNS0 started, sysid 9800". The card's virtual system number
  is 9800 (COSMOS reserves 9800-9803 for the four Ethernet interfaces).

### C. `DEF-NETWORK-CONN D102 ENNS0`  ->  XSLET, then XSDSY
Binds a remote system to be REACHED VIA the ENNS0 network server.

1. `0x0441` **XSLET** (65, "Send Letter") p1="*XM-ENNS0", p11=remote sysno, p10=2.
   - A letter to the *XM-ENNS0 server (op 2) naming the remote system. Purpose: ask the card to
     take part in / acknowledge the connection; the card replies disclosing its own sysid, which
     becomes the "via" system for the route.
   - ORACLE reply `0x0400` XRSOK, **p17 = 0x2648 = 9800 (the card's own sysid)**.
2. `0x054A` **XSDSY** (74, "Define System Routing") p1 = remote sysno, p2 = 0x2648 (9800).
   - Manual 3.14: "the specified system [p1] is marked as being available VIA the system defined
     in parameter 2." So: "reach the remote system via 9800 (the ENNS0 server)." Reply `0x0500`
     XRSOK -> the route now exists.
- `List-Routing-Info` / XSGSY (75) then reports the remote with connection type 3 = "via network
  server" (manual 3.15 par 2), initially unconfirmed (`WAN?`) until a live round-trip.

So the whole point of DEF-NETWORK-CONN is: **letter the card, get its sysid back, then XSDSY the
remote "via" that sysid.** Without the card's sysid coming back, no XSDSY, no route.

---

## Where the HLE breaks - and what it means

HLE reply to step C-1 is `0x0102` = serial 1, status 2 = **XRUNN "Unknown name (of server or
system)"** (XMSG-PL-VALUES-M.INCL:248) instead of the oracle's `0x0400{p17=9800}`. XRUNN is a
NAME-RESOLUTION failure: XROUT cannot resolve "*XM-ENNS0" to deliver the letter, so nothing ever
reaches the card and no sysid comes back -> no XSDSY -> "Unknown name".

The paradox: XSNAM registered "*XM-ENNS0" successfully at start (byte-identical to the oracle,
XRSOK). So the name EXISTED. By DEF-NETWORK-CONN time (~65 s later) XROUT can no longer resolve
it. The manual gives the mechanism for a name to vanish (server-names doc / manual 3.9): **XROUT
clears a name by itself when it notices the port has closed** (and the kernel clears it on
close). So the leading hypothesis is:

> The HLE card's server port is treated by XROUT as CLOSED/DEAD between start and
> DEF-NETWORK-CONN - consistent with the measured 3281-vs-38 level-12 IDENT storm and the
> 1524 always-empty XFRRE on port 4 - so XROUT clears the "*XM-ENNS0" name, and the later
> XSLET letter bounces with XRUNN.

This reframes the fix from "reply to the letter" (the card never even receives it) to "keep the
card's named server port genuinely ALIVE and receiving so XROUT does not clear its name."
INFERRED, not yet proven - the next experiment is to confirm the name is gone at
DEF-NETWORK-CONN time (send an XSGIN "*XM-ENNS0" lookup, or watch for XROUT's port-closed
notice in the trace) BEFORE changing card code.

---

## One-glance map

| Command | Service (num) | Key params | Reply | Meaning |
|---|---|---|---|---|
| DEF-REMOTE | XSDRN (73) | "name", sysno | XRSOK | name -> number in the name table |
| start-net-server | XSLEK (84) | "*XM-ENNS0","ENNS0",op1 | (card magic) | letter+kick: start the card server |
| " (card) | XSNAM (66) | "*XM-ENNS0" | XRSOK | publish the server port's name |
| " | XSNET (85) | server magic | XRSOK | register the magic as a network server |
| DEF-NETWORK-CONN | XSLET (65) | "*XM-ENNS0",remote,op2 | ORACLE 0x0400{p17=9800} / HLE XRUNN | letter the card; get its sysid |
| " | XSDSY (74) | remote, via=9800 | XRSOK | route: remote reachable via the server |
| List-Routing-Info | XSGSY (75) | sysno | 4 ints | read the routing table |
