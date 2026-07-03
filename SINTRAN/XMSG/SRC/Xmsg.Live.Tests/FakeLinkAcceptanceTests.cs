using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Codec;
using NDInsight.Sintran.Xmsg.ListRouting;
using NDInsight.Sintran.Xmsg.Live;
using NDInsight.Sintran.Xmsg.Live.Seam;
using NDInsight.Sintran.Xmsg.Live.Tad;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Live.Tests
{
    /// <summary>
    /// THE PORT ACCEPTANCE TEST (seam plan §5). A hand-written fake <see cref="ILink"/> — with NO
    /// HDLC framing, NO LAPB state machine, NO transport, no async — drives the full XMSG stack
    /// (<see cref="XmsgCodec"/> + <see cref="XmsgLayer"/> + the TAD responder) in BOTH directions:
    /// a payload pushed UP produces a decoded SINTRAN frame back DOWN. Because the entire upper stack
    /// depends only on <see cref="ILink"/> (plus <c>Xmsg.Protocol</c>) and never touches
    /// <c>Xmsg.Hdlc</c> or <see cref="LapbLayer"/>, this test IS the proof that porting into
    /// X25Emulator is "swap the link adapter" — bind these same upper layers to X25Emulator's ILink
    /// implementation and nothing above the seam changes.
    /// </summary>
    public sealed class FakeLinkAcceptanceTests
    {
        // The exact captured connect request 100 -> 103 (proto DA) — same bytes the parity test uses.
        private const string ConnectRequestHex =
            "2113000E0067006400000400DA13210086E400670000006402F7040000410010FF072A54414441444D00FE0444313033";

        private static readonly Func<DateTime> FixedClock = () => new DateTime(2026, 7, 2);

        [Fact]
        public void FakeLink_DrivesFullXmsgStack_BothDirections()
        {
            // Compose the WHOLE upper stack over a fake link — no HDLC, no LAPB, no transport.
            FakeLink link = new FakeLink("fake:acceptance");
            LinkXmsgTransport transport = new LinkXmsgTransport(link);   // codec sends DOWN through the link
            XmsgCodec codec = new XmsgCodec(link.Name, transport);
            XmsgLayer layer = new XmsgLayer(codec, 103, 0x00);
            ConfigureLayer(layer);

            // UP wiring: a payload delivered by the link is parsed by the codec (which raises up to the
            // layer). This is the same one-liner the real runner uses — the only thing that differs
            // from production is the ILink implementation under it.
            link.PayloadReceived += delegate (ILink deliveringLink, byte[] payload, int length)
            {
                codec.ProcessBytes(payload.AsSpan(0, length));
            };

            Assert.True(link.Start());
            Assert.Equal(LinkStatus.Active, link.Status);

            // DRIVE UP: push the connect request in as if it had just arrived on the (absent) wire.
            link.DeliverUp(Convert.FromHexString(ConnectRequestHex));

            // DRIVE DOWN (assert): the layer + TAD responder answered, and the codec serialised that
            // answer back down through the link. At least the connect-accept must have been sent.
            Assert.NotEmpty(link.SentFrames);

            // Every frame the stack emitted is a well-formed SINTRAN frame (marker 0x21 0x13/0x12) —
            // proof the bytes went all the way through the codec, not some half-formed fragment.
            for (int i = 0; i < link.SentFrames.Count; i++)
            {
                byte[] sent = link.SentFrames[i];
                Assert.True(sent.Length >= 2, "sent frame too short to be a SINTRAN frame");
                Assert.Equal(0x21, sent[0]);
                Assert.True(sent[1] == 0x13 || sent[1] == 0x12, "second marker must be 0x13 or 0x12");
            }
        }

        [Fact]
        public void FakeLink_NotActive_RefusesSendWithFalse()
        {
            // Contract: a send on a not-Active link returns false (never throws).
            FakeLink link = new FakeLink("fake:idle");
            Assert.Equal(LinkStatus.Stopped, link.Status);
            Assert.False(link.SendSintranFrame(new byte[] { 0x21, 0x13 }, 2));
        }

        /// <summary>Configures the layer exactly as the runner/parity test do (routing + TAD responder).</summary>
        private static void ConfigureLayer(XmsgLayer layer)
        {
            layer.AcknowledgeData = false;
            layer.RoutingTable = new InMemoryRoutingTable(RoutingEntries());
            layer.TadResponder = new TadTerminalResponder(103, FixedClock);
            layer.AcknowledgeTadFrames = true;
        }

        private static RoutingTableEntry[] RoutingEntries()
        {
            return new RoutingTableEntry[]
            {
                new RoutingTableEntry(100, XroutConnectionType.Neighbour, 1, 1, 0),
                new RoutingTableEntry(102, XroutConnectionType.Via, 100, 2, 0),
                new RoutingTableEntry(103, XroutConnectionType.Local, 103, 0, 0),
            };
        }

        /// <summary>
        /// A minimal in-memory <see cref="ILink"/> for the acceptance test: it carries XMSG L3 with no
        /// framing, no LAPB, no transport. <see cref="DeliverUp"/> simulates an inbound payload;
        /// <see cref="SendSintranFrame"/> captures the outbound frames the stack produces. It honours
        /// the contract's status gating and no-throw send semantics.
        /// </summary>
        private sealed class FakeLink : ILink
        {
            private readonly string _name;
            private LinkStatus _status;

            /// <summary>The SINTRAN frames the upper stack sent DOWN through this link.</summary>
            public List<byte[]> SentFrames { get; }

            public event LinkPayloadReceived? PayloadReceived;
            public event LinkStatusChanged? StatusChanged;

            public FakeLink(string name)
            {
                _name = name;
                _status = LinkStatus.Stopped;
                SentFrames = new List<byte[]>();
            }

            public string Name
            {
                get { return _name; }
            }

            public LinkStatus Status
            {
                get { return _status; }
            }

            public bool Start()
            {
                // No LAPB handshake to wait for — the fake link is Active immediately.
                SetStatus(LinkStatus.Starting, "start requested");
                SetStatus(LinkStatus.Active, "fake link up");
                return true;
            }

            public void Stop()
            {
                if (_status == LinkStatus.Stopped || _status == LinkStatus.Stopping)
                {
                    return;
                }

                SetStatus(LinkStatus.Stopping, "stop requested");
                SetStatus(LinkStatus.Stopped, "stopped");
            }

            public void Dispose()
            {
                Stop();
            }

            public bool SendSintranFrame(byte[] frame, int length)
            {
                // Contract: refuse (logged false, never throw) when the link is not Active.
                if (_status != LinkStatus.Active)
                {
                    return false;
                }

                if (frame == null || length < 0 || length > frame.Length)
                {
                    return false;
                }

                // Copy exactly the valid bytes — the caller's buffer may be reused.
                byte[] copy = new byte[length];
                Array.Copy(frame, 0, copy, 0, length);
                SentFrames.Add(copy);
                return true;
            }

            public bool SendX25Packet(byte[] packet, int length)
            {
                // This fake carries XMSG; an X.25 send is refused per the contract.
                return false;
            }

            /// <summary>Test hook: simulate one complete L3 payload arriving up from the (absent) wire.</summary>
            /// <param name="payload">The SINTRAN information field bytes.</param>
            public void DeliverUp(byte[] payload)
            {
                PayloadReceived?.Invoke(this, payload, payload.Length);
            }

            private void SetStatus(LinkStatus next, string reason)
            {
                if (next != _status)
                {
                    LinkStatus previous = _status;
                    _status = next;
                    StatusChanged?.Invoke(this, previous, next, reason);
                }
            }
        }
    }
}
