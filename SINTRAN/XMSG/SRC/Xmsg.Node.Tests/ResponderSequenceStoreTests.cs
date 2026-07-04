using System;
using System.Collections.Generic;
using System.IO;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Node;
using NDInsight.Sintran.Xmsg.Node.Tad;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Node.Tests
{
    /// <summary>
    /// Verifies the <see cref="FileResponderSequenceStore"/> load/save round-trip and the ND-100
    /// sequence rule (XMSG-PROTOCOL.md section 18.8): the responder CONTINUES its per-link outgoing Flags1
    /// from the persisted store across connects - it is NOT reset per connect. It zeroes ONLY on
    /// first-ever contact or a peer ReachabilityRequest (the peer's XMSG restarted). Hard-resetting to
    /// 0x0000 against a climbed peer lands behind its expected-from-us and the accept is silently dropped;
    /// the historical fatal-24B crash was an ECHO of the asker's Flags1 (a wrong value), not continuation.
    /// </summary>
    public sealed class ResponderSequenceStoreTests
    {
        // A simple in-memory store for the responder test (no disk).
        private sealed class MemoryStore : IResponderSequenceStore
        {
            private readonly Dictionary<ushort, ushort> _map = new Dictionary<ushort, ushort>();

            public ushort LoadNextFlags1(ushort remoteNode)
            {
                return _map.TryGetValue(remoteNode, out ushort v) ? v : (ushort)0x0000;
            }

            public void SaveNextFlags1(ushort remoteNode, ushort nextFlags1)
            {
                _map[remoteNode] = nextFlags1;
            }
        }

        private const string ConnectHex =
            "2113000E0067006400000400DA13210086E400670000006402F7040000410010FF072A54414441444D00FE0444313033";

        [Fact]
        public void FileStore_PersistsAcrossInstances()
        {
            string path = Path.Combine(Path.GetTempPath(), "xmsg-seq-test-" + Guid.NewGuid().ToString("N") + ".state");
            try
            {
                // First "process": store node 100 -> next Flags1 0x0007.
                FileResponderSequenceStore a = new FileResponderSequenceStore(path);
                Assert.Equal(0x0000, a.LoadNextFlags1(100));   // first-ever contact starts at 0
                a.SaveNextFlags1(100, 0x0007);

                // Second "process" (new instance = a restart): must read back 0x0007.
                FileResponderSequenceStore b = new FileResponderSequenceStore(path);
                Assert.Equal(0x0007, b.LoadNextFlags1(100));
                Assert.Equal(0x0000, b.LoadNextFlags1(102));   // an unseen node is still fresh
            }
            finally
            {
                if (File.Exists(path))
                {
                    File.Delete(path);
                }
            }
        }

        [Fact]
        public void Responder_ContinuesFromStoredSequence()
        {
            // The accept CONTINUES our persisted per-link outgoing Flags1 - it is NEVER reset per connect
            // (GOD-LLM answer, capture-VERIFIED; XMSG-PROTOCOL.md section 18.8). Hard-resetting to 0x0000
            // against a CLIMBED peer lands BEHIND its expected-from-us (XSRSQ) and the accept is silently
            // dropped (LAPB RR but no datagram ACK, no session-setup) - the live stall on 2026-07-04. A
            // correctly-continued value is safe at any epoch; the historical fatal-24B crash was an ECHO
            // of the asker's Flags1 (a wrong value), not a continued own value.
            MemoryStore store = new MemoryStore();
            store.SaveNextFlags1(100, 0x0004);   // continued value from prior sessions on this link

            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2), store);
            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames =
                responder.OnConnect(XmsgFrame.Parse(Convert.FromHexString(ConnectHex)));

            byte[] accept = frames[0].ToArray();
            // The accept's Flags1 (header offset 8-9) equals the stored value, NOT 0x0000.
            Assert.Equal(0x0004, (accept[8] << 8) | accept[9]);
        }

        [Fact]
        public void ReachabilityRequest_ResetsSequenceForThatNode()
        {
            // 100's XMSG restart is signalled by a ReachabilityRequest; it zeroes 100's expected-from-
            // us. A stale stored value must be reset to 0x0000 so our next session is in step.
            MemoryStore store = new MemoryStore();
            store.SaveNextFlags1(100, 0x0002);   // stale value from a prior (dropped) run

            string? logged = null;
            XmsgNode node = new XmsgNode(102, 0x00);
            TadTerminalResponder responder = new TadTerminalResponder(102, () => new DateTime(2026, 7, 2), store);
            responder.Log = line => logged = line;
            node.TadResponder = responder;

            // ReachabilityRequest 100 -> 102 (subtype 0x19, source node 0x0064 = 100).
            node.HandleFrames(XmsgFrame.Parse(Convert.FromHexString("2113001900660064FFFF0001DE08")));

            Assert.Equal(0x0000, store.LoadNextFlags1(100));
            // The reset is loudly logged (greppable "RESET") so a later reconnect issue is easy to trace.
            Assert.NotNull(logged);
            Assert.Contains("RESET", logged);
        }

        [Fact]
        public void Responder_FirstContact_StartsAtZero()
        {
            // No stored value -> a fresh contact starts at 0x0000 (matches a fresh peer).
            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2), new MemoryStore());
            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames =
                responder.OnConnect(XmsgFrame.Parse(Convert.FromHexString(ConnectHex)));

            byte[] accept = frames[0].ToArray();
            Assert.Equal(0x0000, (accept[8] << 8) | accept[9]);
        }
    }
}
