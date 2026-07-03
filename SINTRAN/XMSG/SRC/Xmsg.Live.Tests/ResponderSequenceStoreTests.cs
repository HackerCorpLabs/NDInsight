using System;
using System.Collections.Generic;
using System.IO;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Live;
using NDInsight.Sintran.Xmsg.Live.Tad;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Live.Tests
{
    /// <summary>
    /// Verifies that our outgoing datagram sequence persists per remote node across restarts, so a
    /// restarted responder continues in step with 100's persistent expected-from-us (XSRSQ) instead
    /// of resetting to 0x0000 and being silently dropped (the Run B failure).
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
            // 100 expects our next frame at 0x0007 (we sent 7 before, across a prior run).
            MemoryStore store = new MemoryStore();
            store.SaveNextFlags1(100, 0x0007);   // 100 = 0x0064, the connect's source SYSTEM

            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2), store);
            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames =
                responder.OnConnect(XmsgFrame.Parse(Convert.FromHexString(ConnectHex)));

            byte[] accept = frames[0].ToArray();
            // The accept must carry Flags1 0x0007 (continuing our sequence), NOT 0x0000. Flags1 is at
            // header offset 8-9.
            Assert.Equal(0x0007, (accept[8] << 8) | accept[9]);
            // And the store advanced to 0x0008 for the next frame.
            Assert.Equal(0x0008, store.LoadNextFlags1(100));
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
