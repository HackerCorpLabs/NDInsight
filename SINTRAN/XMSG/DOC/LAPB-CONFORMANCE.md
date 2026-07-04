# LAPB conformance — C# `LapbLayer` vs the authoritative ND LAPB spec

This document records how the C# LAPB implementation satisfies the authoritative ND LAPB
Layer-2 spec (canonical copy at WSL `/home/ronny/repos/os/x25emu/docs/lapb-nd-spec.md`). It
walks the spec section 8.1 validator checklist (12 items) and maps each to the code and the
unit test that proves it.

**Implementation:** `SINTRAN/XMSG/SRC/Xmsg.Live/LapbLayer.cs` (state machine),
`SINTRAN/XMSG/SRC/Xmsg.Live/LapbOptions.cs` (timers/window),
`SINTRAN/XMSG/SRC/Xmsg.Hdlc/LapbFrame.cs` (control-byte decode),
`SINTRAN/XMSG/SRC/Xmsg.Live/Seam/LapbLayerAdapter.cs` (live clock wiring).

**Tests:** all under `SINTRAN/XMSG/SRC/Xmsg.Live.Tests/` —
`LapbFrameDecodeTests`, `LapbLayerTests`, `LapbFrmrTests`, `LapbTimerTests`,
`LapbConformanceVectorsTests` (byte vectors S1-S8).

---

## Section 8.1 validator checklist

Each item answers YES with the code location and the proving test.

1. **S-frame subtype via `ctrl & 0x0F`; RR/RNR/REJ distinguished; REJ `0x9` not conflated with
   data address `0x09`.** `LapbFrame.DecodeSupervisory` decodes the low nibble by field position.
   Test: `LapbFrameDecodeTests.SupervisoryFrame_DecodesSubtype_NrAndPollFinal`,
   `.RejControlByte_NotConfusedWithDataAddress`.

2. **Retransmit queue covers the whole window (not one buffer); go-back-N triggerable by REJ.**
   `LapbLayer._txInfo[8]` stores a payload per N(S); `GoBackN` replays them.
   Test: `LapbLayerTests.Rej_GoBackN_RetransmitsAllOutstanding`, `LapbConformanceVectorsTests.S2`.

3. **T1 ticked in the LIVE runner; T1/N2 values.** `LapbLayerAdapter` injects a monotonic
   millisecond clock (`CurrentMillis`) into `Tick` from the pump loop; defaults T1=3000, N2=10
   (`LapbOptions`). Test: `LapbTimerTests` (all).

4. **RNR handled: busy blocks new I-frames; RR(P=1) probe.** `HandleSupervisory` sets/clears
   `_peerBusy`; `TransmitPending` gates on `!_peerBusy`; `OnT1Expiry` polls RR(P=1).
   Test: `LapbLayerTests.Rnr_HoldsSends_ThenRrResumes`, `LapbConformanceVectorsTests.S3`.

5. **P=1 answered with F=1 and current N(R).** `HandleSupervisory` / `HandleInformation` emit
   RR(F=1) when `frame.PollFinal`. Test: `LapbLayerTests.ReceivedPoll_AnsweredWithFinal`,
   `LapbConformanceVectorsTests.S6`.

6. **On SABM (sent/received/mid-session) V(S)/V(R)/V(A) reset to 0 and queue cleared; NO
   adopt-peer-sequence path.** `Reset` is called on UA-accepted and on every received SABM;
   sequence adoption was removed. Test: `LapbLayerTests.MidSessionSabm_ResetsButStaysConnected`,
   `.RxUa_Connects_ResetsVariables_EmitsRr_LearnsPeer`, `LapbConformanceVectorsTests.S7`.

7. **Constructed with the 16-bit node number; stamped on SABM/UA/RR; peer node kept OUT of
   routing.** `LapbLayer(ushort ownNode, ...)`; `SendUnnumbered` / `EmitSupervisory` stamp the
   node; `PeerNode` is read-only and never a routing key. Test:
   `LapbLayerTests.MultipleTrunks_KeepIndependentSequenceAndIdentity`.

8. **Two I-frames outstanding and the FIRST retransmitted on REJ(0) (depth-1-buffer
   regression).** Test: `LapbConformanceVectorsTests.S2`,
   `LapbLayerTests.Rej_GoBackN_RetransmitsAllOutstanding`.

9. **No code path sends an I-frame while peer-busy.** `TransmitPending` requires `!_peerBusy`.
   Test: `LapbLayerTests.Rnr_HoldsSends_ThenRrResumes` (held send asserts empty output).

10. **LAPB module free of X.25/XMSG types (payload independence).** `LapbLayer` references only
    `System`, `System.Collections.Generic` and `NDInsight.Sintran.Xmsg.Hdlc` (`LapbFrame`); the
    information field is opaque `ReadOnlyMemory<byte>` / `ReadOnlySpan<byte>`.

11. **Bad N(R) -> FRMR reason Z; over-long I-field -> FRMR reason Y.** `AcknowledgeThrough` sends
    FRMR(Z); `HandleInformation` sends FRMR(Y) for info > 312 bytes. Test:
    `LapbFrmrTests.BadReceiveSequence_SendsFrmrReasonZ_EntersFrmrSent`,
    `.OverlongInformation_SendsFrmrReasonY_NotDelivered`.

12. **Mid-session SABM -> UA + reset (NOT DM + disconnect).** `HandleUnnumbered` SABM case sends
    UA, resets, stays CONNECTED. Test: `LapbConformanceVectorsTests.S7`,
    `LapbLayerTests.MidSessionSabm_ResetsButStaysConnected`.

---

## Byte-level vectors (section 8.2)

S1-S7 are asserted frame-for-frame in `LapbConformanceVectorsTests` (local node 100, peer 102).
S8 is a framing-layer vector (byte-stuffing / FCS) covered by the HDLC tests
(`HdlcEncoderTests`), not the LAPB layer.

Our implementation emits one spec-permitted extra RR keepalive on entering CONNECTED (spec 3.4,
live-necessary so the ND peer advances to RUN); the vectors account for it and otherwise assert
the required frames exactly.

---

## Deviation from prior behaviour (live re-verification pending)

The previous `LapbLayer` ADOPTED the peer's sequence from its first I-frame — a live-verified
workaround for the epoch-1 reconnect (the ND kernel appearing to keep its sequence across our TCP
reconnect). The spec forbids adoption (section 3.2) and this implementation removes it, hard-zeroing
on every SABM. The offline gates (S1-S7, `SeamParityTests`) pass, but they cannot prove the LIVE
reconnect. Live re-verification against machine 100 — bring-up, reachability/list-route, connect-to,
and a reconnect after a simulated machine restart — remains the operator's gate. If the live
reconnect regresses, the documented fallback is a default-OFF `LapbOptions.AdoptPeerSequenceOnReconnect`
flag, not yet implemented.
