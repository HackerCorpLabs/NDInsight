# ENNS0 start-net exchange - decoded JSON reference (LLE golden vs HLE)

Structured, decoded timelines of the ENNS0 `start-net-server` + `DEF-NETWORK-CONN` XMSG
exchange, so the native-C# HLE card can be diffed cell-for-cell against the real-68K oracle
("LLE"). Built 2026-08-10 by `parse_ennS0_exchange.py` (in the session scratchpad) from the two
device logs.

| File | Card | Source device log |
|---|---|---|
| `lle-oracle-exchange.json` | real-68K oracle (**golden**) | `retrocore-ethii/oracle-startnet-device.txt` |
| `hle-exchange.json` | native-C# HLE (under test) | `retrocore-hle-dram/run-130792/hle-startnet-device.txt` |

Each JSON has `counts`, the reassembled `console`, and a `timeline[]` of events:
`call` (XMSG func + regs/ports), `record` (a decoded XROUT message: serial, service-name or
status, and the parameter blocks), `mailbox_kick`, `control`, `prkey`. XROUT decode per
`../XROUT-DIRECTORY-RECORD-TAGS-DECODE-2026-08-10.md`.

## Services seen (XMSG-PL-VALUES-M.INCL)

XSDRN=73 (define remote name), **XSLEK=84** (send letter AND KICK), **XSLET=65** (send a letter,
NO kick), XSNAM=66 (name this port), XSNET=85 (start network server), **XSGSY=75** (get routing
info), **XSDSY=74** (define routing for system N).

## The registration BOTH cards do (identical - NOT the bug)

1. 7x / 5x `XSDRN` (DEFINE-REMOTE) -> XRSOK
2. `0x0154` **XSLEK** `*XM-ENNS0`/`ENNS0` p10=1  (the start KICK-letter) -> XRSOK
3. `0x5342` **XSNAM** `p1="*XM-ENNS0"`           (names the port) -> XRSOK   <-- PRESENT IN BOTH
4. `0x0255` **XSNET** `p1=<magic>`               (start network server) -> `0x0200` XRSOK p1=3 p2=1

## The ONE divergence that breaks DEF-NETWORK-CONN

During `DEF-NETWORK-CONN`, XROUT sends the network server an XSLET directory query and expects a
reply carrying the server's own sysid; it then defines the route with XSDSY:

LLE / oracle (WORKS):
```
0x0441 request XSLET   p11=0x45B8 p1="*XM-ENNS0" p10=0x0002     <- query for remote 0x45B8
0x0400 reply   XRSOK   p17=0x2648 ...                           <- serial 4 OK, own sysid 9800
0x054A request XSDSY   p1=0x45B8 p2=0x2648                      <- define: remote via 9800
0x0500 reply   XRSOK                                            <- route built
```

HLE (FAILS):
```
0x0441 request XSLET   p11=0x0066 p1="*XM-ENNS0" p10=0x0002     <- query for remote 0x0066 (D102)
0x0102 reply   XR#2                                             <- serial 1, status 2 ERROR, no p17
(no XSDSY ever issued)
```

The HLE's answer to the XSLET query is malformed three ways: **wrong serial** (1, must echo the
request's 4), **error status 2** instead of XRSOK, and **no `p17` own-sysid (0x2648)**. Without
the sysid, XROUT never issues XSDSY (oracle issues it 8x; HLE 0x), no route is defined, and
`DEF-NETWORK-CONN` reports "Unknown name of server or system".

Runtime symptom: the HLE also spins - level-12 IDENT 3281 vs 38, empty XFRRE 1524 vs 6 - the
card never settles into a real blocking receive.

## Fix target (chosen approach: implement the XSLET receive path)

The network server must answer the `0x0441` XSLET directory query with `0x0400` (echo serial 4,
status XRSOK, `p17` = own sysid `OwnSysid`), which makes XROUT issue XSDSY and define the route.
Verify by re-running `Nd100EthernetIIHleDramDumpTests.Boot_Login_EnnS0_DumpHleDram`, re-parsing
its device log with `parse_ennS0_exchange.py`, and diffing the new `hle-exchange.json` against
`lle-oracle-exchange.json` - the `0x0441 -> 0x0400{p17} -> 0x054A -> 0x0500` block must appear.
