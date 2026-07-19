# Handoff — TAD multi-chunk output: receiver decoded, fix built, awaiting one live test

Date: 2026-07-08
Scope: the long-standing bug where a logged-in TAD reply longer than one terminal buffer renders
only its FINAL chunk on the real ND-100 (node 100) running COSMOS connect-to.

---

## TL;DR

- **Root cause found by decoding the receiver binary** (`cos-conn-to-e02.prog`, node 100): the
  connect-to client has **NO `count==0xFF` semantics** — it renders `count` bytes per BDAT element,
  flat. The "255-byte = buffer full, more follows" sentinel we built was a **fiction**; the binary
  never implemented it. That is why our byte-identical continuation frames were silently dropped.
- **Fix built and unit-tested** (218 green): a new default output mode `CompleteSegments` streams a
  long reply as N *complete* BDAT segments (each ≤240 bytes, no `0xFF`, no mid-RFI), window-of-1,
  only the final segment carrying `SYCN 000A + prompt + RFI`.
- **Status: NOT yet proven on real 100.** It is decode-backed, not capture-confirmed. One live
  `3`-command test is the remaining step.

---

## What was ruled out first (so nobody re-treads it)

Every wire-level lever was implemented and **disproven live**, in order:

1. **Zero-secure-ACK shortcut** — works for single-frame replies, breaks multi-chunk. (Superseded.)
2. **1:1 DUMM pacing** (window=1, release next continuation on 100's 7DUMM) — implemented, byte-
   correct on the wire, **still only the final chunk rendered**.
3. **46 ms intra-pair gap** (real host spaces continuation pairs ~46 ms) — implemented and verified
   (~49 ms achieved), **no change**.
4. **Seed-poisoning bug** (per-frame seed re-learn corrupted one chunk's envelope) — real bug, fixed
   (learn-seed-once), **did not change the display symptom**.
5. **Setup-chain fidelity** — the whole session setup was byte-diffed against two real captures and
   made faithful: client-driven bring-up ladder (ESCA→ESRS+RESE, RECO→RESE, RECO→BANNER), OPSV
   `1F 03 4C 00 00`, LUN value byte `0x02`, frameFlags 92/96 alternation. **No change to the symptom.**

Conclusion after (1)–(5): our frames were byte-identical to the real host's, and setup was faithful,
yet only the final chunk rendered. The discriminator had to be inside 100's receiver code — invisible
on the wire.

Eyewitness anchor: the user CONFIRMED the real April `li-fi` listing displayed IN FULL on 100's
screen, so bare-255 continuations DO render for the real host. The mechanism was real; our
reproduction of it was not.

---

## The receiver decode (the actual answer)

Tooling note: the ND-100 runs the OLD-style runner. Ghidra MCP was used for static decode. Live
SINTRAN debugging (dap-debugger MCP) was **ruled out by the user as too complex**. `retrocore` MCP
drives the SDL2CLI NuGet runner, NOT the ND-100.

Decoded in `cos-conn-to-e02.prog` (annotated in the Ghidra DB, written up in
`COSMOS-RE/Analysis/COS-CONN-TO-E02-Analysis.md` §5b):

- **`tad_rx_dispatch_table` @ ram:0x330e**, entry `[0x01]` = `0x2b62` = `tad_rx_BDAT_01`.
- **`tad_rx_BDAT_01` @ ram:0x2b62** — renders `count` bytes per BDAT element (count read via `LBYT`
  @2b72, used only as the render-loop limit @2b73-2b74). **No comparison to 0xFF anywhere.** So a
  255-count element is treated exactly like any other; there is no "more follows" concept.
- **Render gate @ ram:0x2b76** — draws only if `ctrl[0x11]==0 AND (DAT_0c81!=0 OR DAT_0c7f!=0)`.
  `ctrl[0x10]`=RFI-ready, `ctrl[0x11]`=output-suppress (paired), `0c7f`=connection-active (=1 after
  login). NOT the differentiator: our final chunk passes this gate, so our continuations do too.
- **Msg-type gate @ ram:0x46ee** — a received message is DISCARDED unless XFRCV msg-type == 3 ==
  XMTHI (high-priority). NOT the differentiator either (our final chunk passes → our frames are
  XMTHI-classified). But it is a real acceptance rule: terminal traffic must arrive high-priority.

The BDAT handler's coroutine (`FUN_ram_2b66`, state in local -0x7b) and the multi-element dispatch
walk are where the exact per-chunk runtime difference would live — but that needs runtime flag
VALUES, which static analysis cannot supply and live debug is ruled out. So we stopped chasing the
"why the sentinel drops" and instead removed the sentinel entirely, since the decode proves it was
never a receiver concept.

---

## The fix

File map (all under `SINTRAN/XMSG/SRC/`):

- **`Xmsg.Servers/Tad/TadServer.cs`**
  - New `enum TadOutputMode { CompleteSegments, SentinelStream }`.
  - `public TadOutputMode OutputMode { get; set; } = TadOutputMode.CompleteSegments;` (NEW DEFAULT).
  - `public bool AdvancesOutputOnAck => OutputMode == CompleteSegments;`
  - `const int SegmentChunk = 240;`
  - `DrainSessionOutput` branches: `CompleteSegments` → new `DrainSegmentedOutput`; else the old
    sentinel/46 ms path (unchanged).
  - `DrainSegmentedOutput`: window-of-1 (return while `OutstandingOutputCount > 0`); non-final
    segment = plain `BDAT(≤240)`, NO SYCN, NO RFI, NO 0xFF; final segment = `BDAT(tail) + SYCN 000A
    + [prompt BDAT] + RFI`.
- **`Xmsg.Node/Services/IXmsgServer.cs`** — new `bool AdvancesOutputOnAck { get; }`.
- **`Xmsg.Node/Services/XmsgServerHost.cs`** — new `DrainOnAck()` drains ONLY servers with
  `AdvancesOutputOnAck` (so sentinel servers are never drained on an ACK).
- **`Xmsg.Node/XmsgNode.cs`** — after applying an incoming ACK (`ConfirmDelivered`), calls
  `ServerHost.DrainOnAck()` and appends the released frames. This is the pacing signal for segmented
  mode. Sentinel mode still advances on 100's 7DUMM via the existing Data-frame dispatch path and is
  NOT drained on an ACK (that was its historical early-terminator bug).

Why segmented mode advances on the ACK (not the DUMM): the segments are complete self-contained
writes, so there is no "commit" handshake to wait for — window-of-1 on delivery (ACK) is the correct,
simplest pacing.

Unrelated fix carried in the same TadServer change earlier this session: the connect-to hang after a
peer XMSG restart — `XmsgServerHost.ResyncAcceptDown` (XENSE step-down recovery ported to the
ServerHost path). Independent of the display fix; keep it.

---

## Tests (all green: 218)

- `ServerHost_SegmentedOutput_FirstFrameIsCompleteSegment_NotSentinel` — first frame of a >255-byte
  reply is a complete `BDAT ≤240`, never 255, no RFI (more follows).
- `ServerHost_SegmentedOutput_ReleasesNextSegmentOnAck` — after 100 ACKs segment 1, the host releases
  segment 2 (window-of-1). Proves the live-critical ACK-drain path the single-shot in-memory harness
  cannot otherwise exercise.
- `ServerHost_EchoDiagnostic_FirstChunkIsOneContinuation` — retargeted to `OutputMode = SentinelStream`
  so the old mode stays covered for A/B.

Harness caveat: the in-memory pipe is single-shot (it does not pump the client ACKs that release
later segments), so whole-reply delivery is asserted via a direct `ConfirmDelivered` + `DrainOnAck`
drive, not an end-to-end pipe run. Whole-reply rendering is a live-machine property.

---

## The one remaining step (for whoever runs the live rig)

1. Build and run the runner against real 100 (the `3` echo-diagnostic command emits a ~590-byte
   3-frame reply: `===== ECHO FRAME 1/2/3 OF 3 =====`).
2. Log in, type `3`, and LOOK AT 100's screen.

**Expected:** all three ECHO FRAME blocks render (not just frame 3).

- If YES: the bug is solved. Then you can drop the "keep replies < 255 bytes" constraint on the
  `TadServer.CommandRegistry` and restore the rich multi-line `stat` / `help` / `list` / `who` /
  `wall` output. Capture a pcap and promote CompleteSegments from "decode-backed" to
  "capture-confirmed" in the docs.
- If NO: bare complete segments are still not enough. Fallback (untested) = the per-segment-RFI
  pager: each segment a complete `BDAT(≤240) + RFI` that 100 treats as a finished screen, gated on
  its response between screens. To try it, add a third `TadOutputMode` value; the drain structure is
  already in place.

To A/B against the old behavior on the live rig, set `tadServer.OutputMode =
TadOutputMode.SentinelStream` before the session.

---

## Independent verification (2026-07-08)

Re-verified from a clean build before committing:
- `dotnet build Xmsg.Live.Runner` — 0 warnings, 0 errors.
- `dotnet test` all three XMSG suites: Xmsg.Node.Tests 76, Xmsg.Live.Tests 70, Xmsg.Protocol.Tests
  72 = **218 passed, 0 failed** (matches the claim above).
- The `CompleteSegments` symbols (`TadOutputMode`, `OutputMode` default = CompleteSegments,
  `SegmentChunk`, `DrainSegmentedOutput`, `AdvancesOutputOnAck`) and the `DrainOnAck` ACK-drain path
  in `XmsgServerHost`/`XmsgNode` are all present as described.

Committed in this changeset (source + these docs + the receiver-decode analysis + the 22.16
retraction). STILL NOT live-proven on real 100 — the one `3`-command live test remains the gate.

## References

- `COSMOS-RE/Analysis/COS-CONN-TO-E02-Analysis.md` §5b — the receiver decode (msg-type gate, render
  gate, no-255-semantics), with the Ghidra addresses.
- `SINTRAN/TAD/TAD-Message-Formats.md` §22.16 — the real multi-chunk burst (two captures), retained
  as the sentinel-mode reference.
- `SINTRAN/XMSG/DOC/XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md` — byte-exact real setup ladder
  (already matched by our host).
- Auto-memory `xmsg-envelope-solved.md` — running state.
