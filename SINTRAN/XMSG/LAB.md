# The XMSG lab — machines, ports, and the traps

**Read this before touching a machine.** The machine-readable form is `lab-topology.json` beside
this file; this document adds the provenance, the traps, and what is still unverified.

Everything here was read from the machine or its `RetroCore.ini` on the date given. Nothing is
assumed. Where a fact is NOT established, it says so — treat an untagged claim as suspect and
re-verify rather than trusting it.

---

## 1. The machines

| Machine | Folder | Terminal | HDLC | Ethernet |
|---|---|---|---|---|
| **D100** (sys 100) | `F:\RC\RonnyTest\HDLC1` | 9010 | ctrl 1 listen **10364**, ctrl 2 listen 10362 | `tcp:127.0.0.1:5010` |
| **D102** (sys 102) | `F:\RC\RonnyTest\HDLC2` | 9102 | ctrl 1 dials `localhost:10362` | `tcp:127.0.0.1:5010` |
| **D103** (sys 103) | `F:\RC\RonnyTest\HDLC3` | 9003 | ctrl 1 dials `localhost:10366` | `tcp:127.0.0.1:5010` |

Each launches `RetroCore.exe` with **no arguments**, and must be started **with its own folder as
the working directory** — it reads `RetroCore.ini` and `MachineConfig.json` from there.

VERIFIED 2026-08-08 by reading all three ini files.

### D200 — the Ethernet controller emulator (added 2026-08-09)

A fourth node, **D200, system number 200**. Ronny said on 2026-08-09 that it is a **high-level
Ethernet controller emulator** another agent is writing, and that he has already defined it by
hand on D100 and D102. It is on the Ethernet segment, which is why it takes a
`DEF-NETWORK-CONN ... ENNS0` line.

Everything else about it is **UNVERIFIED BY US** — we have not read a config file or talked to it.
We do not know whether it dials hub `127.0.0.1:5010` like the three SINTRAN machines, whether it
has a terminal port, or what it answers. It is not a RetroCore SINTRAN machine, so the
folder/exe/disk columns above may never apply to it at all. `lab-topology.json` carries it with
every unknown field as `null` rather than a plausible guess.

What we DID change: the two definition lines are now in the restart script's defaults
(`tools/restart-xmsg-cosmos.ps1`) and in the documented sequence, so a later XMSG restart puts
D200 back without anyone having to remember it:

```
DEF-REMOTE,,D200 200
DEF-NETWORK-CONN D200 ENNS0,,0,0,0,0
```

**Another agent owns this node.** Do not reconfigure or restart it.

## 1b. D103 has NO ENNS0 - it cannot use the Ethernet segment at all [MEASURED 2026-08-11]

`LIST-RT-DESCRIPTION ENNS0` on D103 answers `ILLEGAL PARAMETER`. The same command on D100
returns a real descriptor (`PASSIVE`, segment `101B`, start address `32241B`). The RT program is
simply not installed on D103, so `START-NET-SERVER,ENNS0` and every `DEF-NETWORK-CONN` there
fail with "Error in communicating with XROUT" - there is nothing to communicate with.

D103 is therefore an HDLC-only machine in practice, whatever its `RetroCore.ini` says about
`ETH 0`. Do not read those failures as a routing or naming problem; they are a missing install.

**And a second, separate trap on the same machine:** its XMSG kernel table area can be
uninitialised. `LIST-SYSTEMS` in X-C printed `XMSG kernel table area initialised.` - creating
the tables at that moment. Every `DEF-REMOTE` run before that had landed on a table that did not
exist and silently did nothing, which is what makes XROUT answer "Unknown name (of server or
system)" for a name you just defined. If you see that, run a listing command in X-C first, then
define again and check for `Ok`.

**Bringing D103 in over HDLC works** (verified: it listed D19999's files). It needs the relay
listening on 10366 before D103 dials, and `START-LINK,1360,,,-1,,` on BOTH ends - LU 1360, not
the 1362 the restart script starts by default, because 1360 is controller 1 and that is the port
the relay uses. `START-LINK,1360` alone is refused: the command prompts for a timeout, so the
full parameter list is required.

## 2. The trap that costs the most time: LU 1360 is ONE controller

**SINTRAN on D100 runs an XMSG link on exactly one HDLC controller, number 1, and calls it LU
`1360`.** Whichever TCP port controller 1 listens on is the only port an XMSG link can ever come up
on. Dial the other controller and you get a healthy TCP connection, a peer that SABMs forever, and
`TXData/Retry/RXBad = 0/0/0` — it looks like a protocol fault and is not one.

 - VERIFIED: D103 runs LU 1360 on its controller 1 and works, which pins the mapping.
 - **The refusal message proves NOTHING about the LU.** MEASURED 2026-08-08: `START-LINK,1360` —
   the LU that was carrying a live link at that very moment — was refused with
   *"Illegal/Reserved Logical Unit Number (LUN) for link"*. The same text appears when the link is
   simply already started. An earlier note here treated that message, seen for `1364`, as evidence
   about LU numbering; **that reading was wrong** and is withdrawn. The message also varies with
   XROUT's state ("Remote system is not accessible" at other times).
 - UNVERIFIED, and now with LESS evidence than before: whether a second link LU exists or can be
   generated. Check `LIST-LINKS` before drawing any conclusion from a `START-LINK` refusal.

**D100's ini was modified 2026-08-08**: HDLC 1 and 2 listen ports were SWAPPED so our relay owns
controller 1. Backup `RetroCore.ini.bak-2026-08-08`, originals commented in place. **Cost: D102
dials 10362, which is now controller 2 and has no SINTRAN link — D102 currently has no working
line.** Restore the commented lines to give it back.

## 3. Bringing XMSG up — and the rule that unblocked everything

```
XMSG-COMMAND
DEFINE-REMOTE-NAME,,<name>,<number>      every system, including this machine's own name
DEFINE-SYSTEM-ROUTE,,<dest>,<nextHop>    NON-ADJACENT systems ONLY
START-LINK,1360,,,-1,,                   LU is OCTAL; -1 = retry forever
```

**COSMOS Operator Guide ND-30.025.02 section 2.5.4: a system on the far end of your own cable is
ADJACENT and must NOT be given a route.** Only systems reached *through* another node get one.
Giving an adjacent system a route is what made every peer report "not accessible" for days. There
is **no `DEFINE-LINK` command** — a grep across the COSMOS guides finds nothing.

For the D100 / D19999 / D103 chain:

```
D100:  names for D100, D19999, D103;  route D103 via D19999;  NO route to D19999
D103:  names for D103, D19999, D100;  route D100 via D19999;  NO route to D19999
```

`STOP-XMSG` / `START-XMSG` (from `SINTRAN-SERVICE`) **clears the entire name table, including the
machine's own name** — re-run the whole sequence afterwards. Note the XROUT name table survives a
machine restart while the XMSG kernel tables do not, so `LIST-NAMES` can look healthy while
`LIST-SYSTEMS` and `LIST-LINKS` are empty.

## 4. Reading the machine

All read-only and free: `LIST-NAMES`, `LIST-SYSTEMS`, `LIST-LINKS`, `LIST-FRAMES` (has a `Nettype`
column that names each frame — `INIT`, `INNAK`, `ACK`), `LIST-ROUTING-INFO`, `LIST-UTILIZATION`,
`LIST-GENERATION-VARIABLES`, `LIST-FRIEND-SYSTEMS`.

**`LIST-LINKS` health check — read `Sysid`, not `State`.** `State Run` with `Sysid` equal to the
machine's OWN system number means its HDLC has TCP-connected to ITSELF, which happens when the
listener it dials is down. Everything else in the row looks perfect. Only a machine restart clears
it.

`LIST-ROUTING-INFO` sometimes repeats one system's block forever; ESC breaks it (`USER BREAK`), and
`STOP-XMSG`/`START-XMSG` clears the underlying state. **Cause UNKNOWN** — do not assume our traffic
provokes it.

## 5. Our relay

```
Xmsg.Live.Runner --config topology-d19999-relay.json \
                 --relay-listen 10366 --relay-inbound-node 103 127.0.0.1 10364 19999 <seconds>
```

Use **`topology-d19999-relay.json`**, not `topology-d19999.json` — the latter describes the older
Ethernet arrangement and advertises node 103 as "via 100", which makes D100 report
`*Loop suspected*`.

**Order matters: start the relay's listener BEFORE the machine that dials it.** Stop a relay and the
dialling machine may wedge in a TCP self-connection (see the `Sysid` check above).

Proven working 2026-08-08: `A: *->19999->103`, 10 datagrams relayed both directions, 0 dropped —
`DOC/captures/TRANSIT-PROVEN-2026-08-08/`.

## 6. Capturing

```
tshark -i \Device\NPF_Loopback -f "tcp port 10362 or tcp port 10364 or tcp port 10366" -w out.pcapng
tshark -r out.pcapng -Y hdlc_lapb                      # project dissector, already installed
```

`tshark` is at `C:\Program Files\Wireshark\tshark.exe`; the dissector source is
`SINTRAN/Devices/HDLC/WireShark/hdlc_tcp.lua`. **Start the capture before touching anything.** A
capture overturned two confident wrong theories in a single run on 2026-08-08; reasoning about the
code did not.

## 7. Specs and references that are NOT in this repo

 - **ND LAPB spec (normative):** WSL `/home/ronny/repos/os/x25emu/docs/lapb-nd-spec.md`. It
   supersedes `DOC/LAPB-REQUIREMENTS.md` here.
 - **COSMOS Operator Guide ND-30.025.02** IS in this repo, under `Operations/Cosmos/` — section 2.5
   is the XMSG start-up chapter. It answered in one read what two binary carves could not.

## 8. Known-open, stated plainly

 - `INNAK` (subtype `0x17`) is never answered; the runner logs `*** NO REPLY BUILT ***`. It is
   demonstrably NOT fatal — a full round trip works while ignoring every one — but the correct reply
   has never been captured.
 - D100's XROUT degrades over time (section 4). Cause unknown.
 - Marker 2 `0x12` on a relayed datagram is accepted in practice, but that it is *correct* is
   UNVERIFIED.
