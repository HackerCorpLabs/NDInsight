# COSMOS multi-node network: LAN + HDLC + relay (2026-07-25)

How to bring up a COSMOS/XMSG network of several ND-100 (SINTRAN III) systems across
MORE than one transport, and the emulator fix that made LAN peers work at all. This
consolidates the learnings from the ND Ethernet II RX-discard hunt and the XROUT routing
RE into one operational reference.

Convention: **[V]** = verified (from an ND manual / firmware disassembly / a captured or
live session in this repo). **[I]** = inferred (supported, not directly quoted).

Worked topology (all three verified live + reproduced offline):

```
                D100  (system 100)  --- RELAY (route-through) ---
                 |                                              |
       COSMOS Ethernet II (LAN)                          HDLC serial line
                 |                                              |
                D102  (system 102)                        D103  (system 103)
```

- `100 <-> 102` over COSMOS Ethernet II (ENNS0 network server, Am7990 LANCE, PCB 3094).
- `100 <-> 103` over an HDLC serial line.
- `102 <-> 103` only via `100`, which must be permitted to forward between its two links.

---

## 1. The prerequisite emulator fix (LANCE FCS double-count) [V]

Before any of this works, the Ethernet peers must actually exchange frames. They did not,
because of a bug in the shared Am7990 LANCE:

- **TX** appended a 4-byte Ethernet FCS to the frame handed to the network backend
  (`appendFcs = (mode & DTCR) == 0`, true by default).
- **RX** assumes backend frames are FCS-less and adds 4 to the descriptor MCNT
  (`MCNT = result + 4`).

So the FCS was **double-counted**: a 60-byte frame went out as 64 bytes, the receiver
computed `MCNT = 68`, and the ND Ethernet II firmware's 802.3 length-consistency check
(RCVCOMPLETE **gate 2**, 0x5D18: received-data-length must equal the 802.3 length field)
rejected it. Every unicast COSMOS frame was silently discarded -> the XROUT liveness
round-trip never completed -> routes stuck at `WAN?` -> `connect-to` failed with
"Remote system not available".

**Fix** (`Emulated.HW\AMD\LANCE\Am7990\Am2990Lance.cs`): hand the network backend the
**FCS-less** frame (the universal software-Ethernet convention that the RX path already
assumes); keep the FCS only on the internal-loopback path for CRC realism. On real
hardware MCNT already includes the FCS, so `D5 = MCNT - 4 = 60` and gate 2 passes via its
`<= 60` shortcut - the fix makes the emulator match that.

Rule of thumb going forward: **frames crossing the `IEthernetBackend` seam are FCS-less.**
Any code that puts a frame on a backend (pcap/udp/tcp/in-process bridge) must not include
the FCS; any code that receives from a backend accounts the 4 FCS bytes into MCNT itself.

Reproduced + regression-guarded offline by
`Emulated.Tests\ND100\Nd100TwoNodeEthernetHarnessTests.cs` (+ `InProcessEthernetBridge.cs`):
`conn-to d102` = `=== CONNECTION ESTABLISHED ===`; RX disposition 207 delivered / 0 discarded.

---

## 2. Bring-up ladder per node (SINTRAN `@`, then the X program) [V, Ronny live 2026-07-24]

Same on every node; local identity (100/102/103) comes from the booted image, not the
commands.

```
SIN
START-X
EXIT
START-TAD          ' = START-TADADM: make the TAD service available (needs COS-TADADM RT-loaded)
TADA
SET-AVAIL
X                  ' enter the XMSG command program (X-C:)
DEF-REMOTE,,D100 100
DEF-REMOTE,,D101 101
DEF-REMOTE,,D102 102
DEF-REMOTE,,D103 103
```

Then the transport-specific routing (sections 3-5).

### TAD after a cold start [V, ND-30.025.02 COSMOS Operator Guide]

`START-TADADM` fails if the TAD program is not resident. After a cold start, RT-load it
first (the TAD administrator program is **`COS-TADADM`**; standard `HENT-MODE:MODE`):

```
@RT-LOADER
READ-BINARY COS-TADADM 36     ' 36 = example segment no.; YOUR system's may differ
YES
EXIT
@START-TADADM
```

Prefer running the system's own `LOAD-MODE:MODE` / `COS-*-LOAD:MODE` files if present -
they use the correct segment numbers. `SEGMENT NUMBER nnn IS NOT CLEARED` -> RT-LOADER
`CLEAR-SEGMENT nnn` then retry. TAD rides on XMSG, so `START-X` must have run first.

---

## 3. LAN peer (Ethernet) [V]

On the node that has the Ethernet card, start the server as a **LAN** server and declare
the connection - do **NOT** use `DEFINE-SYSTEM-ROUTE` for an Ethernet peer (that is the
HDLC command; default via = NONE = direct cable, which fails on Ethernet).

```
start-net-server,enns0,,,N                 ' N = LAN server (Y = WAN gateway, sets XL5WA=14)
DEF-NETWORK-CONN D102 ENNS0,,0,0,0,0        ' reach 102 via the ENNS0 Ethernet server
```

`DEFINE-NETWORK-CONNECTION` creates a connection-type-3 "via network server" entry - a
declaration of intent. XROUT only promotes it to a live **LAN (Neighbour)** route after a
liveness round-trip actually completes over the wire (which is why section 1's fix was the
real blocker). The MAC is derived as `08:00:26:<system-number-LE16>:00`
(100 -> `08:00:26:64:00:00`, 102 -> `08:00:26:66:00:00`; `08:00:26` is the registered
Norsk Data OUI).

---

## 4. HDLC peer (direct serial neighbour) [V]

On the node at each end of the HDLC line, declare the other end as a **direct** neighbour -
`DEFINE-SYSTEM-ROUTE` with **no via-arg** = directly-cabled connection
(Driftsansvarlig 5.1.2 p.195):

```
DEFINE-SYSTEM-ROUTE,,D103        ' on 100: 103 is the direct HDLC neighbour (no via)
DEFINE-SYSTEM-ROUTE,,D100        ' on 103: 100 is the direct HDLC neighbour (no via)
```

(The physical HDLC line must be up; declaring the route does not by itself start the link
protocol.) `[I]` The link-bring-up step for XMSG-over-HDLC is not yet RE'd here.

---

## 5. The relay: making 100 forward between its two links [V]

With 100 reaching 102 (LAN) and 103 (direct HDLC), it still will NOT relay between them
until pass-through forwarding is enabled. On the **relay node (100)**:

```
ENABLE-ROUTE-THROUGH             ' allow routing THROUGH this node (clears XRFNA "passthru stop")
```

`[V]` `Enable-Route-Through` is in the 210373M XMSG command program's command list
(`SINTRAN\XMSG\DOC\XMSG-COMMAND-REFERENCE.md`); its opposite is `Disable-Route-Through`.
It corresponds to the XMSG symbol `XRFNA` ("if #0 forwarding not allowed, i.e. passthrough
stop", `ND-820023-1-EN SINTRAN III-VSX System Documentation`).

Then, because routes are **per-direction**, each leaf declares the far system **via the
relay** so replies can return:

```
' on 102:
DEFINE-SYSTEM-ROUTE,,D103 D100   ' reach 103 via 100
' on 103:
DEFINE-SYSTEM-ROUTE,,D102 D100   ' reach 102 via 100
```

Summary of who does what:

| Node | Role | Key commands |
|---|---|---|
| 100 | relay | `ENABLE-ROUTE-THROUGH`; `DEF-NETWORK-CONN D102 ...`; `DEFINE-SYSTEM-ROUTE,,D103` |
| 102 | LAN peer | `DEF-NETWORK-CONN D100 ...`; `DEFINE-SYSTEM-ROUTE,,D103 D100` |
| 103 | HDLC peer | `DEFINE-SYSTEM-ROUTE,,D100`; `DEFINE-SYSTEM-ROUTE,,D102 D100` |

---

## 6. Reading `LIST-ROUTING-INFO` (li-rout) [V]

Three lines per destination: `L:` local tables, `T:` tables, `A:` actual (probed) path.

- `*` = here (the local system).
- `*->LAN->100` = reachable as a confirmed LAN neighbour via 100.
- `*->WAN?->102` = **unconfirmed** (the trailing `?`), NOT a real wide-area path - the
  liveness round-trip has not completed. Section 1's fix is what lets this flip to `LAN`.
- `*->LAN->100->103` on `L:` but `T:/A:` say `->100, but no access to system 103` = the
  local route is fine but the RELAY (100) cannot reach 103 - fix on 100 (its route to 103
  and/or `ENABLE-ROUTE-THROUGH`), not on the asking node.

Verification: after the config, `li-rout` on 102 shows a clean `->100->103` for 103, and
`conn-to d103` from 102 launches the `COSMOS CONNECT-TO PROGRAM` and connects.

---

## 7. Sources

| Claim | Source |
|---|---|
| LANCE FCS double-count fix | `Am2990Lance.cs` + `Nd100TwoNodeEthernetHarnessTests.cs` (this repo) |
| RCVCOMPLETE gate 2 (802.3 length) | `ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md` (+ correction banner), firmware dis 0x5CA2-0x5D24 |
| WAN? = unconfirmed; DEF-NETWORK-CONN vs DEFINE-SYSTEM-ROUTE | `XROUT-LAN-NEIGHBOUR-ROUTING-RE-2026-07-24.md`; Driftsansvarlig 5.1.2 p.195 |
| `Enable-Route-Through` / `XRFNA` | `XMSG-COMMAND-REFERENCE.md` (210373M); `ND-820023-1-EN SINTRAN III-VSX System Documentation` |
| TAD `COS-TADADM` RT-load / `START-TADADM` | `ND-30.025.02 COSMOS Operator Guide` (sections 3.2.2, cold starts) |
| MAC = `08:00:26:<sysno-LE16>:00`; OUI | live capture + IEEE OUI (Norsk Data A.S.) |
