# HANDOFF - HLE Ethernet II: DEF-NETWORK-CONN SOLVED, now prove send/receive

**Date:** 2026-08-11
**Status:** DEF-NETWORK-CONN returns "Ok" on the native-C# HLE card (committed). Send/receive
proven at the CARD BOUNDARY. Full node-to-node conn-to NOT yet proven over real Ethernet.
Live-test boot config PREPARED but NOT booted (a test was running on D100; user said hold).

Full path of this file:
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\HANDOFF-HLE-SEND-RECEIVE-TEST-2026-08-11.md`

---

## What is DONE and committed

**DEF-NETWORK-CONN `<system>` ENNS0 now returns "Ok"** on `NDBusEthernetIIHle` (the native C#
Ethernet II card). Committed to RetroCore as `e8df4f132`. Validated on a real SINTRAN boot
(run-64680): D102 / D200 / D19999 all return "Ok"; `List-Routing-Info` shows
`102/200/19999 -> *->WAN?->*`; device log shows `[0x0400]x12 -> [0x054A]x6 -> [0x0500]x6`.
8/8 descriptor unit tests pass.

The two defects that were fixed (file
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\EthernetII\NDBusEthernetIIHle.cs`):

1. **Idle re-poll.** An XROUT-routed letter arrives with NO doorbell, so the Recv listen gate
   that only fired on the doorbell never consumed it. Added a fallback poll
   (`CA_IDLE_REPOLL_TICKS = 300_000`, `_caIdlePollLeft`): consume immediately if
   `_caReceiveArmed`, else count down and re-poll.
2. **Route-define answer.** The card must NOT build the XSDSY itself - it is unprivileged, so
   the kernel answers XRPRV (0x0A privilege error). Instead the card matches the route-define
   letter (`rhdr == 0x0441`) BEFORE the connect-letter test, sends the verbatim oracle
   `[0x0400]` directory reply (`BuildRouteDefineDirectoryReply`), and lets the PRIVILEGED
   command program build the XSDSY. Reply descriptors: XFSCM, XFWRI 40 bytes from
   `ROUTE_REPLY_BUF = 0x1d600`, XFSND|XFSEC to the requester magic.

## What is PROVEN about send/receive

- **At the card boundary:** `Nd100EthernetIIHleHubJoinTests.HleCard_JoinsTcpHub_AndFramesCrossBothDirections`
  passes - frames cross both directions when the HLE card joins a TCP hub. This proves the
  card's network attach and framing, not a full COSMOS conn-to.
- **Full two-node guest-ring conn-to is NOT proven.** The harness
  `Nd100TwoNodeEthernetIIHleHarnessTests` moves 0 packets because both emulated nodes boot the
  SAME disk image, so both program MAC `00:00:00:00:00:00` - a same-image identity collision.
  The harness log says so explicitly: *"both cards programmed the SAME MAC ... connect-to
  cannot distinguish the nodes. Give ImagePathB a distinct-sysno image."* This is a harness
  limitation, not a defect in the fix.

## The live-test plan (pivot away from the two-node harness)

Because the two-node harness collides on identity, the plan is a LIVE test: boot one local
RetroCore ND machine with the HLE card on the operator's Ethernet hub (`127.0.0.1:5010`),
NO HDLC line, and conn-to the REAL D102 that is already on that hub.

### CONFIG PREPARED (uncommitted, in RetroCore working copy)

- `E:\Dev\Repos\Ronny\RetroCore\RetroCore\scripts\ND100\ND100-configs.ini`
  new config `ND100-ETHHLE-HUB`: `cpu=ND100CX`, `device add ETHHLE 0 --net=tcp:127.0.0.1:5010`,
  FX/SMD/TERM 5/TERM 6, and **NO `device add HDLC`** (that is the "kill off hdlc1" test
  condition - proves ENNS0 + conn-to work over Ethernet ALONE).
- `E:\Dev\Repos\Ronny\RetroCore\RetroCore\INI\ND100Script.ini`
  new boot label `ND-BOOT-ETHHLE-HUB` (attaches `%BD0_L%` = `D:\BIGDISK0-L.IMG`, boots BD0),
  and **menu choice 9**: "SINTRAN HLE Ethernet on hub (no HDLC)".

Verified (not assumed): the `device add` parser (`DebugCommands.Devices.cs` line 760) extracts
`--net=` and passes it to the `ETHHLE` case (`ND100Machine.cs` line 444) ->
`FromSpec("tcp:127.0.0.1:5010")` -> joins the hub.

### *** CORRECTION 2026-08-11 - cpu_number/system_type do NOT set identity on ND-100 ***

An earlier draft of this config added `--cpu_number=100 --system_type=100` with a comment
claiming it relabels an already-installed pack to D100 (MAC ..64..) at boot. **That is WRONG and
has been removed.** Those flags are used **only when the machine is FIRST INSTALLED** - they seed
the identity that then lives on the SINTRAN disk. Booting an already-installed pack ignores them;
the system number SINTRAN reports (and the number the card programs into its station MAC at
bring-up) comes from the **INSTALLED DISK IMAGE (BD0)**. The identity CAN be changed later
(re-install / SINTRAN config), it is just not re-applied by re-passing the flags at boot. User
stated this directly (2026-08-11: *"cpu number and system type in ini file doesnt change what
sintran sees, that's in the sintran disk after system has been installed"*; 2026-08-17:
*"--cpu_number/--system_type only used when machine is installed first time, can change it
later"*). See memory [[nd100cx-identity-comes-from-disk-not-ini]].

**Consequence for the test:** the BD0 pack attached in `ND-BOOT-ETHHLE-HUB` must be installed
with a system number DIFFERENT from D102's, or the local node and the real D102 collide on
identity exactly like the two-node harness did. `%BD0_L%` (`D:\BIGDISK0-L.IMG`) is the current
attach - its installed system number must be confirmed (or a distinct-number pack chosen)
before booting. THIS IS THE OPEN ITEM.

## NEXT ACTIONS (in order, once the user gives the go-ahead)

1. Confirm which BD0 pack has a system number distinct from D102's (ask the user; the two-node
   collision proves this matters). Point `ND-BOOT-ETHHLE-HUB` at that pack.
2. Launch RetroCore, Boot menu -> choice 9.
3. Drive over the retroterm MCP (ONE session, ESC first, `SYSTEM\r\r` login):
   - `start-net-server enns0`
   - `DEF-NETWORK-CONN D102 ENNS0` (expect "Ok")
   - `conn-to d102`
4. Watch for real frames crossing to D102 over Ethernet (card TX/RX counters moving, and D102
   answering the conn-to).

## HARD CONSTRAINTS from the user this session

- Do NOT kill D100 (9010) - a test was running on it.
- Do NOT boot the emulator until the user says the running test is clear.
- The three live processes (hub 5010, D100 9010, D102 9102) must be left untouched.
- Leftover testhosts on this box are from `E:\Dev\Repos\Ronny\RetroCore-spin\` (a DIFFERENT
  working copy) - NOT ours, leave them alone.
