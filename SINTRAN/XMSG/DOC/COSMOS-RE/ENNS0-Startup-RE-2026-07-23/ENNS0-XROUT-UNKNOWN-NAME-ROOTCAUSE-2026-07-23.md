# ENNS0 START-NETWORK-SERVER "Unknown name" - Root-Cause Analysis (2026-07-23)

Read-only analysis. Every claim tagged VERIFIED (checked in code/log/console) or
INFERRED (reasoned, not proven). Per repo policy nothing is guessed on the wire.

Inputs used:
- Console: `...RRETROCORE...NDBUS...37a0478f...\scratchpad\ethii-console.txt`
- Device log (13474 lines): `...\scratchpad\ethii-controller-log.txt`
- Emulator: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\...`

---

## TL;DR verdict

**The "Unknown name" is produced by the REAL SINTRAN XROUT kernel running in the
guest, NOT by an emulator MON-call stub.** The C# `MON_200_XMSG` handler is a no-op
stub, but it is **compiled out** - so it never runs; every MON 200B is dispatched to
genuine SINTRAN. The failure is a **true XROUT name-registration gap**: the network
server name **`*XM-ENNS0`** was never registered with XROUT, so START-NETWORK-SERVER's
lookup correctly returns `XRUNN` (2) "Unknown name (of server or system)".

This means the long-standing "our node stubs the magic number" theory
(`subtype-07-network-error`) does **NOT** explain THIS failure - that memory is about
the *separate* C# HDLC multi-node case (connect-to / list-systems between node 100 and
a C# node 103). The local ENNS0 path never reaches that code.

The fix is therefore **not** a MON-call stub in RetroCore. It is either a missing
COSMOS network-configuration step, or an ENNS0 self-registration code path that did
not execute (see "Remaining ambiguity").

---

## 1. The XROUT registration/lookup path (Task 1) - VERIFIED

### 1a. MON 200B is handled by REAL SINTRAN, not the C# layer  -- DECISIVE

`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Instructions.SystemControlInstructions.cs`
lines 77-96, method `MON()`:

```csharp
#if false // TODO: Add to system configuration - allow "sintran layer" ...
    bool blnExecutedOK = false;
    if (cpu.sintran != null)
        blnExecutedOK = cpu.sintran.ExecuteSintranMonitor(monitor_number);
    if (blnExecutedOK) regs.PC++;      // would SKIP the real MON
#else
    if (Logger.IsLogEnabled(Logger.LogLevel.Device))
        cpu.sintran?.TraceMonitorCall(monitor_number, regs);   // <-- LOG ONLY
    ((CpuND100)cpu).InternalInterruptLvl14(..., MONITOR_CALL, monitor_number, ...);
#endif
```

VERIFIED: The `#if false` branch (which would call the C# handler and, on success,
skip the real MON) is disabled. The active `#else` branch only *traces* MON 200/201
(`TraceMonitorCall`) and then raises the level-14 internal interrupt so the **real
SINTRAN XMSG/XROUT kernel** services the call.

Consequently `MON_200_XMSG()` in `MON_200_XMSG.cs:1040` (body: `return true;` - a
pure stub) is **never executed**. The XROUT reply "Unknown name" originates inside the
genuine SINTRAN III XROUT (banner in console: "XROUT: XMSG version M00 (88.03.25)
started"). The emulator only observes.

### 1b. The exact XROUT request that fails - VERIFIED from the byte trace

The XMSG trace decoder (`MON_200_XMSG_TRACE`, logs `XMSG - iFunc=...`) captured the
START-NETWORK-SERVER exchange at 13:28:23 (Device log lines ~6204-6240):

- 6204 `XFPRV` (request privilege) - the "*- WARNING: You can now bypass system
  protection mechanisms -*" line.
- 6205 `XFOPN` open port, 6207 `XFGET` get message, 6208 `XFSCM` set current message.
- 6210 `XFWRI` NBYTES=28 into the message, bytes (big-endian words):
  `0154 0018 FF09 2A58 4D2D 454E 4E53 3000 FD05 454E 4E53 3000 0A02 0001`
  Decoded (VERIFIED ASCII, INFERRED param framing):
  - `FF09` = param descriptor, 9 chars -> `2A 58 4D 2D 45 4E 4E 53 30` = **`*XM-ENNS0`**
  - `FD05` = param descriptor, 5 chars -> `45 4E 4E 53 30` = **`ENNS0`**
  - `0A02 0001` = trailing integer parameter(s).
- 6218 `XFSND` **Receiving port: 0x00000000** Sending port: 4  -> sent to **port 0 =
  the XROUT well-known routing port**.
- 6230 second `XFWRI`/`XFSND` (same `*XM-ENNS0` payload) to port 1, then `XFRCV`
  ports 1 & 4, then 6238 `XFREA` reads the reply.
- Immediately after, CONOUT prints "Server not yet started ..." then the error.

VERIFIED: the name XROUT is asked to resolve/start is **`*XM-ENNS0`** (friendly name
`ENNS0`). This is the XSNET (85) start-network-server / name lookup path (service
mapping from `XMSG-API.md` section 6.4; consistent with the 07-07 finding).

### 1c. The name was NEVER registered - VERIFIED

Full-run scan of the XMSG trace:
- The ONLY XROUT service send (to port 0) *before* the failure carrying a `*XM-` name
  is at 13:27:55 (line 90-92): `XFWRI ... FF08 2A58 4D2D 4649 444F` = **`*XM-FIDO`**
  then `XFSND` to port 0. i.e. **XMFIDO self-registers** its name when `start-x`
  brings up XMSG.
- During `@rt enns0` (13:28:03 -> 13:28:23), the complete set of XMSG calls ENNS0
  issued was: `XFPRV`x1, `XFOPN`x1, `XFWDF`x1 (define wake-up, drivers only),
  `XFDBK`x1 (define bank, drivers only), plus `XFDUM`/`XFDCT` housekeeping.
  **No `XFWRI`+`XFSND`-to-port-0 with the `*XM-ENNS0` name. No XSNAM/XSCRS name
  creation.** (uniq -c over the trace between the two banners confirms it.)

So ENNS0 came up as a *driver port* (XFOPN/XFWDF/XFDBK) but **never created the named
service `*XM-ENNS0`** in XROUT. When START-NETWORK-SERVER then asks XROUT for
`*XM-ENNS0`, XROUT has no such name -> `XRUNN` "Unknown name". This is correct XROUT
behaviour given an unregistered name.

---

## 2. Emulator stub vs genuine gap (Task 2) - VERIFIED it is NOT the MON stub

- VERIFIED: `MON_200_XMSG()` is a stub (`return true;`) but is **dead code** because
  the `ExecuteSintranMonitor` dispatch is `#if false` (section 1a). No emulator code
  fabricates the XROUT reply. The genuine SINTRAN XROUT does.
- VERIFIED: the controller/PIOC handshake, historically the suspected blocker, **works
  this run**. Device log around 13:28:09: "68000 CPU started" (line 774), "[PRKEY]
  *** firmware posted PRKEY (052163B) @0x0404 ***" (842), STARTED_FLAG=0x0001 (848),
  MON_CODE=0x03 (READY) with a clean SCIP INT12 monitor postbox (886-892). So the
  Ethernet card is up; the failure is downstream of the card, purely in XROUT naming.
- The `0x200440` supervisor-program-space bus error still fires (line ~224,
  13:28:07.820) but per `ethii-start-gate-prkey` it is a handled/recovered 68K trap,
  not the crash - and the card reaches READY afterwards, so it is not the cause of the
  missing name registration.

Therefore: **genuine registration gap**, not an emulator MON stub, and not (this run)
a controller-handshake failure.

### Where the old "magic stub" actually lives (for completeness)

The C# node that stubs the magic number is the **separate Xmsg.Live / X25Emulator HDLC
node** (`SINTRAN/XMSG/SRC/Xmsg.Live/XmsgNode.cs`), used when a C# machine pretends to
be a remote system (node 103) over the `--hdlc` TCP bridge. That path handles
`XSGSY`/`XSGMG` for inter-node `list-systems`/`connect-to`. It is unrelated to the
in-guest ENNS0 local registration failing here. Do not "fix" it for this bug.

---

## 3. Pcap cross-reference (Task 3)

Not applicable to this failure. Per `enns0-getmagic-trace-vs-builder` (VERIFIED
2026-07-07): the ENNS0<->XROUT name-registration/`XSNET` handshake is **local on the
ND-100, in memory, via MON 200B** - it dies *before* any Ethernet or HDLC frame, so it
cannot appear in any wire capture. The pcaps at `E:\Dev\Ronny\X25Emulator\pcap` cover
the inter-node HDLC case (the magic-stub scenario), which is a different code path.
The correct oracle here is the in-emulator MON 200B trace (already captured above),
not a pcap.

---

## 4. Fix recommendation (Task 4)

**This is not a RetroCore MON-call bug.** No change to `MON_200_XMSG.cs` or the MON
dispatch would be correct - the guest SINTRAN XROUT is behaving correctly by rejecting
an unregistered name. Recommended, in priority order:

1. **Most likely (INFERRED): a missing COSMOS network-configuration step.**
   `XMSG-COMMAND-REFERENCE.md` lists a whole network-definition family that must run
   before START-NETWORK-SERVER can resolve the server:
   `Define-Network-Connection`, `Define-Network-Local-Endpoint`,
   `Define-Network-Direct-Connection`, `Define-Network-Remote-Endpoint`, etc.
   The harness only did `start-x` + `rt enns0` + `START-NETWORK-SERVER` - none of the
   Define-Network-* steps. On real COSMOS the local system number and the network
   server / endpoints are defined (by generation or these commands) so that
   `*XM-ENNS0` exists in XROUT before it is started. **Action: reproduce the real
   COSMOS Ethernet-II bring-up procedure** (find the site's XMSG/COSMOS config script
   that issues the Define-Network-* / DEFINE-LOCAL-SYSTEM commands) and run it before
   START-NETWORK-SERVER. This is a procedure/config gap, not emulator code.

2. **Alternative (INFERRED, must be checked): ENNS0's own name-registration path did
   not execute.** If on real hardware `rt enns0` is supposed to self-register
   `*XM-ENNS0` (the way XMFIDO self-registers `*XM-FIDO` at start-x), then the fact
   that ENNS0 issued only driver-port calls (XFOPN/XFWDF/XFDBK) and no XSCRS/XSNAM
   means its registration code was skipped. Given the card reaches READY and ENNS0's
   driver init shows no XROUT error, this would point to an ENNS0-internal decision
   (e.g. it defers name creation to START-NETWORK-SERVER, which then needs
   pre-defined config - collapsing back to option 1).

3. **Definitive next diagnostic (grounded, from the 07-07 finding).** DAP / cpu-trace
   the MON 200B calls during BOTH `rt enns0` and START-NETWORK-SERVER, and for every
   `XFSND` to port 0 decode the **XROUT service byte** (first byte of the message) +
   the name string, and the reply's XROUT error byte. Specifically confirm whether an
   `XSCRS`(80)/`XSNAM`(66) for `*XM-ENNS0` is ever attempted and what XROUT answers.
   The trace decoder already shows the buffers; add the service-byte decode to
   `MON_200_XMSG_TRACE` (XFWRI/XFSND branch) or read the buffer at the `XFSND` bp.
   This disambiguates option 1 vs 2 with certainty.

---

## 5. What changed vs prior memory (record-of-truth updates)

- CONFIRMED and sharpened `ethii-start-gate-prkey`'s reconciled picture: PRKEY/PIOC
  gate works; the blocker is XROUT "Unknown name". Added: the failing name is
  `*XM-ENNS0`, it is never registered, and the reply comes from REAL SINTRAN (C# MON
  stub is `#if false` dead code).
- SCOPED `subtype-07-network-error` / "our node stubs the magic number": that is the
  HDLC multi-node case, NOT this local ENNS0 failure. The two must not be conflated.
- No emulator MON-call change is warranted; the issue is COSMOS network config /
  ENNS0 registration, to be pinned by the DAP service-byte trace in step 4.3.

Findings file: `C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Ronny-NDInsight\b17a7474-33c0-4d7f-b9cb-f921c3ad419b\scratchpad\ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md`
