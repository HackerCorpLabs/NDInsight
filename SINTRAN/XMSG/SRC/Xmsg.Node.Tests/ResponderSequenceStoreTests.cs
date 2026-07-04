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
    /// sequence rule (XMSG-PROTOCOL.md §18.5): a responder runs its OWN Flags1 from 0x0000 per connect
    /// and does NOT resume a persisted value — resuming a climbed value put the accept at epoch 1 and
    /// crashed 100's XMSG (fatal 24B).
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
        public void Responder_StartsAtZero_IgnoringStoredSequence()
        {
            // Even with a climbed stored value, the accept MUST start at Flags1 0x0000 (§18.5): the
            // responder runs its OWN fresh sequence per connect and never resumes a persisted value.
            // Resuming a high value (e.g. 0x0019) put the accept at epoch 1 (channel 0xD9) and crashed
            // 100's XMSG with the fatal 24B (XXPER) — a discontinuity in our stream.
            MemoryStore store = new MemoryStore();
            store.SaveNextFlags1(100, 0x0019);   // a climbed value from prior sessions

            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2), store);
            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames =
                responder.OnConnect(XmsgFrame.Parse(Convert.FromHexString(ConnectHex)));

            byte[] accept = frames[0].ToArray();
            // Flags1 (header offset 8-9) is 0x0000, so the accept rides epoch 0 (channel 0xDA), never 0xD9.
            Assert.Equal(0x0000, (accept[8] << 8) | accept[9]);
        }

        [Fact]
        public void ReachabilityRequest_ResetsSequenceForThatNode()
        {
            // 100's XMSG restart is signalled by a ReachabilityRequest; it zeroes 100's expected-from-
            // us. A stale stored value must be reset to 0x0000 so our next session is in step.
            MemoryStore store = new MemoryStore();
            store.SaveNextFlags1(100, 0x0002);   // stale value from a prior (dropped) run

            XmsgNode node = new XmsgNode(102, 0x00);
            node.TadResponder = new TadTerminalResponder(102, () => new DateTime(2026, 7, 2), store);

            // ReachabilityRequest 100 -> 102 (subtype 0x19, source node 0x0064 = 100).
            node.HandleFrames(XmsgFrame.Parse(Convert.FromHexString("2113001900660064FFFF0001DE08")));

            Assert.Equal(0x0000, store.LoadNextFlags1(100));
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
