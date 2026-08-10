# Capture session, 2026-08-07 — XMSG-COMMAND read commands on D100

**Machine:** D100, SINTRAN III VSX/500 K, terminal port 9010, driven over the retroterm MCP.
**Wire capture:** `xrout-services.pcapng`, loopback, TCP 10362 / 10364 / 10366.
**Commands run:** all read-only. Nothing changed machine state.

---

## The headline: this machine IS the product our manual documents

```
XMSG command program (210373L) of 87.12.30 13:17
Compiled for version 1987.02.08 (Release L)
XMSG kernel version 1987.02.08 (Release L)
Options:
   ND-100/CX only. XPIT. Trace. Watchdog.
   Inter-system:  Network gateway/IOC.
```

Product `210373L`, Release L kernel — exactly the manual imported earlier today. Everything below
is therefore a check of that manual against the software it describes, not against a later version.

The `Options:` line confirms manual section 5's own opening text: `Watchdog` prints when XMFIDO is
included, `Network gateway/IOC` when the gateway code is. Both are present on this machine.

---

## 1. `X3FSZ` — a variable the generation file does not carry

`LIST-GENERATION-VARIABLES` reported **24** variables. The file `XMSG-SYS-DEF-L.SYMB` carries 23.
The extra one:

```
X3FSZ, Maximum frame size in words (input)................:   312
X4FSO, Maximum frame size in words (output)...............:   312
```

**This settles the LAPB "312" question.** Two independent frame-size limits, both stated in WORDS,
both 312, one per direction. There is no reading left in which a bare 312 in the LAPB spec is a
byte count — which is what made 452 recorded frames look illegal and prompted raising our limit to
622. See `XMSG-GENERATION-VARIABLES-2026-08-07.md`.

Still open, and now the only part: the variables say "frame" while the LAPB requirement bounds the
INFORMATION FIELD. Settling that needs the ND LAPB spec read against these two, not a capture.

## 2. Every transcribed value matched

All 23 values decoded from the parity-encoded `XMSG-SYS-DEF-L.SYMB` matched the running kernel
exactly. `LIST-UTILIZATION` independently confirms the limits from the kernel's own tables:

| Table | Limit | Variable |
|---|---:|---|
| Task | 80 | `X4TSK` |
| Port | 128 | `X5PRT` |
| Message | 256 | `X4MES` |
| Name | 768 | `X4NAM` |
| System | 512 | `X4SIR` |
| Link | 4 | `X5LNK` |
| Message buffer | 25 pages | `X4BPG` |

**`X4LTO` is confirmed as the real name**, not the manual's `X4TMO`. The decision to trust the
machine-readable file over the scanned prose was right.

## 3. `XSLIN`'s P16-P18 meanings confirmed

`LIST-LINKS` prints the triple as its own header:

```
Link table status: 4 entries. 1 in use. Max 1 used.
```

Table size, in use, high-water mark — the same three quantities in the same order that manual
section 7.5 assigns to parameters 16, 17 and 18, and the 4 matches `X5LNK`. `LIST-UTILIZATION`
prints the same shape for every kernel table under `Limit / Max used / In use`.

**Not the wire encoding.** That run asked the LOCAL XROUT, which answers without a datagram. To
capture an actual `XSLIN` reply, `LIST-LINKS` must be pointed at a REMOTE system.

## 4. `XmsgLinkState` confirmed live

The link row reports `State Run`, matching `XmsgLinkState.Run = 4`. These six values had no ND
source until this morning's manual import; the running system now shows one of them in use.

```
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run 19999 40RR 40RR 1362  10/Off       0       0         49/0/15
```

`Sysid 19999` is OUR node — the link D100 holds to the relay run earlier today. `Timeout 10`
matches `X4LTO`.

## 5. The command list, from the machine itself

`?` at the `X-C:` prompt prints what changed in this release:

```
New        Privileged         Enable-Route-Through
New            "              Disable-Route-Through
New            "              Define-Alternative-Link
New            "              Remove-Alternative-Link
New            "              Enable-Checksum
New            "              Disable-Checksum
New            "              List-Connections
New            "              List-Utilization
New            "              List-Generation-Variables
Modified       "              List-Network-Servers
Modified       "              List-Links
Modified       "              List-Systems
```

**Nine new commands, not five.** Manual section 5's prose says "five new commands have been added";
the program reports nine. The program is the authority on itself. The manual's own section headings
do cover all nine, so this looks like the prose sentence being wrong rather than an OCR fault.

`LIST-LINKS` prompting `Record address?` for the local case is also exactly as section 5.11
describes.

---

## 6. The relay: both links Active, and exactly what still blocks transit

Once D103's HDLC link 1360 was started, the relay brought BOTH links up for the first time:

```
inbound link Starting -> Active (LAPB connected)      <- D103
outbound link Starting -> Active (LAPB connected)     <- D100
```

`DatagramsRelayed` stayed at zero, and the reason is a ROUTE definition, not code. Neither machine
has anywhere to send that requires crossing us:

**D100's system table** puts 103 one LAN hop away, over the Ethernet that is currently dead:

```
Address  Sysno  State    Link Subaddr  ...  Hops WAN/LAN
1211535    103      4       0       0  ...   0/1
```

`Link 0` - no link assigned - and `0/1` meaning one LAN hop. D100 has no route pointing at us, so
`LIST-LINKS` for system 103 answers `XMSG Kernel error: Remote system is not accessible`
(that is `XERNA`, which ND classify `SIII_SUSPEND`).

**D103's routing table** knows only itself and us:

```
   To     Route
    103  L: *
  19999  L: *->19999
```

So D103 has no third node to address THROUGH us either. Both ends can reach us; neither has a
reason to send anything past us.

**What would produce transit:** one route definition, on either machine.
 - On D100: declare 103 reachable via 19999, so `LIST-LINKS 103` crosses the relay. This is the
   natural one, since D100 already knows 103 and merely has it pointed the wrong way.
 - Or on D103: declare 100 (or 101) reachable via 19999.

Both are `DEF-NETWORK-CONN`-class changes to a real machine's XMSG configuration, so they are
Ronny's call, not something to do unasked.

**Naming note from Ronny:** when a command prompts for a system, give the system NAME, not the
number. The attempts above used `103` and were accepted as a number, but the name is the intended
form.

## 7. TRANSIT ACHIEVED - the relay carried real traffic

```
[relay] hdlc-out:127.0.0.1:10364 -> hdlc-in:10366 for node 103
[relay] hdlc-out:127.0.0.1:10364 -> hdlc-in:10366 for node 103
```

Two datagrams from D100 addressed to D103 arrived on the outbound link, crossed this node, and
went out the inbound link re-marked as relayed with a recomputed checksum. **The first transit
datagram this project has carried.** Evidence: `relay-transit-run.log` and
`relay-transit2.pcapng` (both links).

Getting there needed two things, one ours and one theirs.

### 7a. OUR BUG: the relay never announced itself

The relay path brought both LAPB links to Active and then sat there, and BOTH peers reported
`Remote system is not accessible` - including D103 reporting it about US, while its own routing
table said `*->19999` and the link was up.

The cause: a peer does not register us at the XMSG level just because the link is up. Something has
to announce. The single-link path does this on link-up; the relay path did not, so neither peer ever
learned we existed. Fixed by announcing on BOTH links as each goes Active - announcing only outbound
would leave the inbound peer unable to reach anything at all, us included.

Measured before and after on D103's system table, entry for 19999:

```
before:  State 0   Link 152164   Sequence 0/0      <- link up, but unknown to XMSG
after:   State 4   Link 152164   Sequence 3/2      <- registered and exchanging
```

State 4 is `XmsgLinkState.Run`.

### 7b. THEIR CONFIG: a route giving a reason to cross

Even registered, neither machine had anywhere to send that required crossing us. The command is
`DEFINE-SYSTEM-ROUTE`, and its prompts are unambiguous about the order:

```
XROUT system?          <- blank = configure the LOCAL XROUT
System?     D100       <- the DESTINATION
Via system? D19999     <- the NEXT HOP
```

Run on D103 as `System=D100, Via=D19999`, and on D100 as `System=D103, Via=D19999`. Both answered
`Ok`, and the very next `LIST-LINKS` for D103 from D100 crossed the relay.

Note the one-line form `DEFINE-SYSTEM-ROUTE,,D19999 D100` is ambiguous about which argument is the
destination and which the next hop - the `,,` makes the positions hard to count. The interactive
prompts settle it: **System is the destination, Via is the next hop.**

### 7c. What did NOT happen

No return leg. D103 receives the relayed datagram but does not answer, so D100 still waits at its
`Link number?` prompt. Its route to D100 had only just been defined, so this is most likely a
registration matter on D103's side rather than anything in the relay - the forwarding itself is
demonstrably working in the direction that was exercised.

## What this session did NOT get

- **No `XSLIN` wire capture.** Every command was answered by the LOCAL XROUT, so nothing crossed a
  link. Pointing these at a remote system is the next step.
- **No FA operations.** Not attempted this sitting.
- **No checksum pair.** `ENABLE-CHECKSUM` / `DISABLE-CHECKSUM` change machine state and were left
  for last; not run.
