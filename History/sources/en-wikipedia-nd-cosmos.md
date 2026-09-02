# Source: English Wikipedia, "ND-COSMOS"

- **Page**: <https://en.wikipedia.org/wiki/ND-COSMOS>
- **Fetched**: 2026-08-27 via the MediaWiki API, by Ronny's request.

**Status: SECONDARY, short (708 characters), and uncited** - the article has a
"Sources" heading with nothing under it. But three of its claims can be checked
against material this repo already holds, and two of them hold up.

## What it claims

1. ND-COSMOS was ND's proprietary networking system, and **the second generation
   NORDNET system**.
2. Tight integration with ND-NOTIS and with SINTRAN III.
3. "In many ways ... a strong parallel to DEC's DECnet Phase IV."
4. **Peer-to-peer**, and the NOTIS document store could have redundant servers,
   giving good reliability.
5. **The name was internally a pun on the ND Satellite**, a small computer ND had
   recently released.
6. It ran over a wide variety of link layers: **Ethernet, X.25, HDLC, Bisync and
   asynchronous serial ports**.

## Checking it against what we hold

**NORDNET is real, and it is confirmed by a primary ND document.** It is not just
a wiki name. `Reference-Manuals/ND-60.134.2 EN SINTRAN III Communication Guide.md`
shows it twice over:

- It has its own ND publication number in the document list:
  **ND-60.081, "Nordnet Sys.Docum."**
- It has user-facing commands: *"He may use **NORDNET** commands to run remote
  batch in another **NORD** computer."*
- And it coexists with XMSG at startup, with an ordering rule:
  *"This operation should be included in the normal start up sequence and executed
  before starting NORDNET or SPOOLING, since these can 'steal' the POF space
  reserved for XMSG when they fix their segments."*

So NORDNET is a distinct ND networking product that runs alongside XMSG. That
supports the article's framing, though **it does not prove the "second generation"
relationship** - no document we hold says COSMOS succeeded NORDNET.

**The link-layer list matches our own measurements.** The XMSG work in
`SINTRAN/XMSG/` has COSMOS traffic running over **both Ethernet and HDLC** on real
machines, measured rather than read. `SINTRAN/Devices/HDLC/` covers the HDLC side,
and Bisync appears in `ND-06.014.2A EN ND-100 Reference Manual` among others. Two
of the five named link layers are directly corroborated by our own running
systems, which is stronger evidence than the article itself.

**The ND Satellite pun incidentally confirms a machine.** ND-Satellite was on our
list as a bare name from Norwegian Wikipedia with nothing behind it. This page
describes it as a small computer ND had recently released - and
`Hardware/3D-Models/` holds a measured ND100-Satellite model. It is real.

## Not checked

The DECnet Phase IV comparison, the peer-to-peer claim and the redundant NOTIS
servers are unverified here. The peer-to-peer point is testable against our own
XMSG work rather than against paper, and worth returning to.

---

## Verbatim extract

ND-COSMOS was the proprietary computer networking system developed by Norsk Data as the second generation NORDNET system.
It offered very tight integration with the ND-NOTIS applications, as well as the SINTRAN III operating systems. In many ways, it was a strong parallel to DEC's DECnet Phase IV system.
The network system was peer-to-peer, and the NOTIS document storage system could also have redundant servers, which made for very good service reliability.
The name was internally a pun on its recently released small computer, known as the ND Satellite.
It could function on a wide variety of different link layers, including Ethernet, X.25, HDLC, Bisync, and asynchronous serial ports.


== Sources ==
