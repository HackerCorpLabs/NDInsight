# Audit findings, 2026-08-06 - partial

Two of the planned sweeps in [PLAN-PROTOCOL-AUDIT-2026-08-06.md](PLAN-PROTOCOL-AUDIT-2026-08-06.md)
completed before a usage limit stopped the rest. Their results are below. **The other two
sweeps - protocol unknowns across `SRC`+`DOC`, and the source-material inventory - never ran.**

**Provenance.** These findings come from analysis agents, not from a line-by-line read by the
author. Every claim carries a path and line so it can be checked. Two headline claims WERE
spot-checked and one of them was wrong - see the correction in section 2. **Verify before
acting.**

---

## 1. The best finding: LAPB build and parse keep separate copies of the same constants

`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Live\LapbLayer.cs:98-110` names every LAPB
control-field constant:

```csharp
private const byte SabmBase = 0x2F;   DiscBase = 0x43;   UaBase = 0x63;
private const byte DmBase = 0x0F;     FrmrBase = 0x87;
private const byte RrNibble = 0x01;   RnrNibble = 0x05;  RejNibble = 0x09;
private const byte PollFinalBit = 0x10;
```

`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Hdlc\LapbFrame.cs` - the **parse** side - re-types
the identical values as bare literals: `0x2F`/`0x43`/`0x63`/`0x0F`/`0x87` at lines 307-315,
`0x1`/`0x5`/`0x9` at 281-285, `0x10` at 181 and 305, `& 0x0F` at 279, and the N(S)/N(R) shifts
and `& 0x07` masks at 153-167.

**VERIFIED by direct read on 2026-08-06.** Both halves exist exactly as described.

**Why this is the top item.** One side builds frames, the other parses them, and they hold
independent copies of the same numbers. An edit to one silently desynchronises the other - which
is precisely the failure mode that has cost this project the most time this week
(`TadTerminalResponder` was a hand-copy of `TadSession` and carried an identical checksum bug).

**Proposed fix:** a `public static class LapbControl` in `Xmsg.Hdlc` (the lower project, which
`Xmsg.Live` already references) holding the five U-frame bases, three S-frame nibbles,
`PollFinalBit`, `SequenceMask`, `NsShift`, `NrShift`. Delete the private copies in `LapbLayer.cs`.

**Risk:** low. These are compile-time constants with no behavioural coupling. The LAPB tests
should pass unchanged; if any moves, the two copies had already diverged and that is the bug.

---

## 2. CORRECTION - the HDLC framing bytes are NOT four bare copies

The agent reported `0x7E` / `0x7D` / `0x20` as duplicated bare literals across four files and
called it "the worst duplication in the audit". **That is wrong.** Checked directly:

 - `Xmsg.Hdlc\HdlcDeframer.cs:24,29,34` - `private const byte Flag/Escape/EscapeMask`. Named.
 - `Xmsg.Live\HdlcEncoder.cs:29,34,39` - the same three, also named.

The remaining grep hits are XML doc comments, which are documentation, not code. So this is
**two private copies of correctly-named constants in two projects** - mild duplication worth
tidying, not a bare-literal defect and nowhere near the top of the list.

Recorded because the mis-ranking matters: an agent's severity ordering is a suggestion, not a
measurement.

---

## 3. FRMR reason bits should be a `[Flags]` enum

`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Live\LapbLayer.cs:112-115` - VERIFIED by direct
read:

```csharp
// FRMR reason bits (spec 2.3.3), OR-combined into the diagnostic's third byte.
private const byte FrmrReasonW = 0x01;   // control field invalid / not implemented
private const byte FrmrReasonY = 0x04;   // I-field length exceeded the maximum
private const byte FrmrReasonZ = 0x08;   // N(R) invalid (outside [V(A), V(S)])
```

The comment says OR-combined; that is the definition of a flags enum. Proposed:

```csharp
[Flags]
public enum LapbFrmrReason : byte
{
    None = 0,
    ControlFieldInvalid = 1 << 0,        // W
    InformationFieldTooLong = 1 << 2,    // Y
    ReceiveSequenceInvalid = 1 << 3,     // Z
}
```

The gap at `1 << 1` is deliberate - the wire positions are spec-fixed, not sequential.

**The missing bit is a real question, not a bug.** Standard LAPB puts reason **X** ("information
field not permitted in this frame type") at `0x02`. We neither send nor recognise it. The agent
flagged that its bit position is standard-LAPB-derived and **not verified against the ND spec
section 2.3.3**. Do not add it as fact - add it marked UNVERIFIED, or read section 2.3.3 first.

---

## 4. Other literals worth naming (agent-reported, NOT individually verified)

Ranked by whether getting it wrong corrupts a frame.

| Where | What | Proposed |
| --- | --- | --- |
| `Xmsg.Ethernet\NdLinkLayer.cs:312` | `if (header.ReceiverLinkId != 0)` | `!= UnknownPeerLinkId` - the constant is declared in the same file at line 55, and this is the peer-learning gate whose comment records a live defect from getting the branch wrong |
| `Xmsg.Ethernet\NdLinkHeader.cs:386` | `(Kind & 0xF0) == 0x60` | `NpduType == NdNpduType.DisconnectRequestByNetworkService` - `NpduType` already IS the high nibble, so no new constant is needed |
| `Xmsg.Hdlc\Fcs16.cs:45,84` + `Xmsg.Live\HdlcEncoder.cs:57` | `0xFFFF` init and one's-complement, re-implemented in the encoder | `Fcs16.InitialValue` + a shared `Fcs16.ComputeTransmitted(...)` |
| `Xmsg.Hdlc\LapbFrame.cs:139,149` + `Fcs16.cs:77,82` | frame overhead `4` and `2`, unnamed in two files | `Fcs16.FcsLength`, `LapbFrame.HeaderLength`, `LapbFrame.MinimumFrameLength` |
| `Xmsg.Ethernet\NdLinkLayer.cs:427` | `ConnectionConfirmKindUnverified = 0x1F` declared 300 lines from the enum it belongs to | move into `NdLinkFrameKind` (`NdLinkHeader.cs:96-143`) keeping its UNVERIFIED remarks |
| `Xmsg.Ethernet\TcpEthernetBackend.cs:46` + `UdpEthernetBackend.cs:44` | `3094` declared twice with identical doc | one `NdEthernetDefaultPort` |

**Deliberately NOT to be changed**, and the agent was right to say so:

 - `LapbLayer.MaxInformationLength` is derived from the fragment constants with a full derivation
   note. Correct as-is.
 - `Ieee8023Frame.cs`, `NdMacAddress.cs`, `HdlcDeframer.cs`, `LapbOptions.cs` are already fully
   named.
 - `NdLinkFrameKind` is **missing** CC / WO / DR-by-user / DC on purpose - their wire bytes have
   never been captured and the file says so rather than guessing. Leave the gap.

---

## 5. Allocation findings (agent-reported, NOT verified)

No LINQ and no `foreach` anywhere in the audited scope - the index-loop discipline is intact.
What the agent found instead was allocation on the receive path:

 1. `Xmsg.Live\HdlcEncoder.cs:64,76` - a `List<byte>` plus `ToArray()` per transmitted frame,
    when line 64 already computes the exact worst-case size. A `Span` overload removes both.
 2. `Xmsg.Live\Seam\LapbLayerAdapter.cs:353-362` and `Xmsg.Live\LiveNode.cs:221-230` - each
    received frame is re-wrapped in flags just to reuse the splitter: three allocations per
    frame. `HdlcDeframer.Unstuff` is private (`HdlcDeframer.cs:127`); making it public removes
    the wrapper and the list.
 3. The receive accumulator is a `List<byte>` filled one `Add` per byte - the hottest loop in
    the stack.
 4. `LapbLayer.cs:940,965` - a 4-byte array per supervisory/unnumbered frame; `stackalloc` would
    do, but only after the `LapbTransmit` delegate takes a `ReadOnlySpan<byte>`.

**Not a defect, leave it:** `LapbLayer.cs:385` `_pending.Enqueue(info.ToArray())` - the copy is
required because the payload must survive until acknowledged, and the remarks say so.

---

## 6. COSMOS Ethernet / ENNS0 gaps (separate agent, NOT verified)

A different sweep inventoried the Ethernet transport docs. Its top items, all offline-closable
unless noted:

 1. **ENNS0, the ND-100-side COSMOS Ethernet driver, has never been read.** It is the parent of
    most other gaps - without it we can only echo frames we have seen. Already linked and ready
    for Ghidra at `Installation\Communication\Ethernet\x\linked\ENNS0-LNK.PROG`, entry `ENNS0`
    at octal 32241.
 2. **Wire bytes for NPDU types WO, DR-by-user and DC are unknown**, as is the low nibble of the
    kind byte. We cannot originate a window update or a user disconnect.
 3. ~~**The C# node cannot relay (route-through)**~~ - **THIS CLAIM IS WRONG. Checked
    2026-08-06.** The agent took it from a doc line dated 2026-08-01 that has since gone stale.
    Relay IS implemented: `Xmsg.Node\Seam\SintranDatagramRelay.cs` does the stamping and the
    checksum recompute, `Xmsg.Node\Seam\DatagramRelay.cs` does the routing decision including the
    never-send-back-out-the-arrival-link rule, and `Xmsg.Node.Tests\DatagramRelayTests.cs` covers
    both. The **actual** gap is narrower: nothing outside the tests ever constructs a
    `DatagramRelay`, so it is a complete tested component with no production caller. Wiring it in
    needs a node holding two links and a route table from `topology.json`. The source doc has been
    corrected.
 4. **`ENNS0_STARTED_FLAG` semantics are wrong in our emulator comment** - `0x4C0` read `0x0000`
    on all 324 polls *while the link worked*, contradicting the "1 = server ready" comment.
 5. **The "goes deaf after idle" failure has never been traced with the named cells** - the only
    trace covers a healthy link. Cheapest outstanding live experiment.

The same sweep listed a dozen items as **already closed** by later docs - reference field order,
word 6 as a checksum, `0x1F` = CC, route-through on Ethernet, the four stray padding bytes. Do
not re-open those.

---

## 7. Source-material inventory - the three best unmined items

The inventory sweep completed. Full detail is in the agent's report; these are the items I
verified myself and the one claim I had to correct.

**VERIFIED PRESENT, and none of them cited anywhere in `DOC\`:**

| Item | Size | Why it matters |
| --- | --- | --- |
| `F:\ND\SINTRAN-L-XMSG\FLOPPY\210373L_X-Message.pdf` | 3,325,116 B, 44 pages | **ND's own X-Message manual**, product 210373 - the exact product on our wire. A pure scan with no text layer, so it needs the OCR pipeline before it can be grepped |
| `E:\Dev\Ronny\NDIX-C\baseline\bin\cps\xmsgerrors.h` | 2,956 B | see below - already paid off |
| `E:\Dev\Ronny\NDIX-C\baseline\bin\cps\xrouterrors.h` | 2,924 B | the same for XROUT |
| `E:\Dev\Ronny\NDIX-C\baseline\bin\cps\systemerrors.h` | 20,124 B | SINTRAN file-system error codes, which FA replies carry |
| `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\COS-FSART-E02.BPUN` | 66,074 B | FSART, implicated in the known d102 hang |
| `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\COS-FAU-VSX-E03.BPUN` | 90,486 B | the file-access unit behind the FA operations we still infer |

**CORRECTED:** the sweep flagged `XMSG-PL-VALUES-M.INCL` / `XMSG-VALUES-M.SYMB` as under-used and
warned "the constants we quote may be a revision behind the machine we test against". **We are
not.** `xmsg-constants.json` declares `xmsg_version: M` and names that exact include as its
source - checked. An L-vs-M diff is still worth doing, but to learn what CHANGED between
revisions, not because we are stale.

### 7a. The error headers already paid for themselves

`xmsgerrors.h` and `xrouterrors.h` are ND-authored (Norsk Data Ltd, NDIX release 3, dated
1988-08-12). They carry something our `XmsgError` / `XroutError` enums do not: **a recovery
disposition for every code** - what a client is supposed to DO about it.

```c
struct SIIIerror xmsgerrors[] = {
    XMSUX,   "No error",                                   SIII_OK,
    XMXEACB, "All Channels Busy.",                          SIII_RETRY,
    XMXEIMA, "Invalid magic number.",                       SIII_RETRY,
    XMXEILF, "Illegale function code in monitor call.",     SIII_GIVE_UP,
    ...
```

Across the two files, 99 classified entries: 57 `SIII_GIVE_UP`, 35 `SIII_RETRY`, 2 `SIII_UNKNOWN`,
2 `SIII_SUSPEND`, 2 `SIII_OK`, 1 `SIII_SLEEP`.

**Directly relevant to the open post-close XEIMA (section "Known open questions"):** XEIMA is
classified **`SIII_RETRY`**, not `SIII_GIVE_UP`. So a client that meets "invalid magic number" is
expected to try again rather than abandon the conversation. That fits what we see exactly - every
FA operation completes, D100 objects after the close, and nothing is harmed because there is
nothing left to retry. It does not explain WHY the close draws it, but it does explain why it has
never hurt.

Worth importing the dispositions into our enums as XML documentation, and worth reading the
plain-English descriptions - they are ND's own wording for codes we currently carry as bare names.

---

## What to do first

1. **Section 1** - unify the LAPB control constants. Verified, low risk, removes a live
   build/parse desync hazard.
2. **Section 3** - the FRMR `[Flags]` enum, without inventing reason X.
3. **Section 4 rows 1-2** - the two literals on branch conditions, where a wrong branch has
   already caused a defect once.
4. Everything else is worth doing but is tidying, not risk reduction.

**Still not run:** the protocol-unknowns sweep and the source-material inventory. Those are the
two that answer "what don't we know" - the question that prompted this. Relaunch them from the
plan document.
