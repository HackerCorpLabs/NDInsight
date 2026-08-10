using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg;               // XmsgFrame
using NDInsight.Sintran.Xmsg.Protocol.Fa;   // FaClientAction
using NDInsight.Sintran.Xmsg.Node.Seam;     // XmsgNodeHost
using NDInsight.Sintran.Xmsg.Servers.Fa;    // FaWriteDriver, FaWriteTarget

namespace NDInsight.Sintran.Xmsg.Live.Runner
{
    /// <summary>
    /// Drives one file push from this runner to a remote <c>*FA-SERVER</c>, and says out loud what
    /// it is doing.
    /// </summary>
    /// <remarks>
    /// <para><b>The first thing we ORIGINATE rather than answer</b></para>
    /// <para>
    /// Everything the runner has done so far has been a reply: a machine asked, we answered. A push
    /// is the other way round, and that changes what can go wrong. The ladder is fifteen requests
    /// over seven operations, each one waiting for the server, so a single wrong field stalls
    /// rather than failing - which is why every step is logged as it is sent.
    /// </para>
    /// <para><b>It cannot start cold</b></para>
    /// <para>
    /// The link has to be up first. On the HDLC seam that means LAPB is
    /// <see cref="NDInsight.Sintran.Xmsg.Live.LapbLayerState.Connected"/>; the caller decides,
    /// because how "up" is known
    /// differs per transport - the Ethernet link cannot address a peer at all until the peer has
    /// addressed it.
    /// </para>
    /// </remarks>
    internal sealed class FaPushRun
    {
        private readonly FaWriteDriver _driver;
        private readonly string _localPath;
        private readonly string _fileSpec;
        private readonly int _contentLength;

        private readonly ushort _serverNode;

        private bool _started;
        private bool _reported;

        /// <summary>
        /// Set when the push threw, so it stops instead of throwing again every tick.
        /// </summary>
        private bool _crashed;

        /// <summary>
        /// Creates a push.
        /// </summary>
        /// <param name="localPath">
        /// The local file whose bytes are being sent, for the log.
        /// </param>
        /// <param name="content">
        /// The bytes to write.
        /// </param>
        /// <param name="target">
        /// Where they are going.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when any argument is null.
        /// </exception>
        public FaPushRun(string localPath, byte[] content, FaWriteTarget target)
        {
            if (localPath == null) { throw new ArgumentNullException(nameof(localPath)); }
            if (content == null) { throw new ArgumentNullException(nameof(content)); }
            if (target == null) { throw new ArgumentNullException(nameof(target)); }

            _localPath = localPath;
            _serverNode = target.ServerNode;
            _fileSpec = target.FileSpec;
            _contentLength = content.Length;
            _driver = new FaWriteDriver(target, content);
        }

        /// <summary>
        /// Gets whether the push has finished, either way.
        /// </summary>
        public bool Finished
        {
            get { return _crashed || _driver.Done || _driver.Failure.Length > 0; }
        }

        /// <summary>
        /// Feeds a received datagram to the push.
        /// </summary>
        /// <param name="frame">
        /// The frame the node received.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="frame"/> is null.
        /// </exception>
        /// <remarks>
        /// Everything arriving on the node goes through here, including traffic that has nothing to
        /// do with the push - a TAD session on the same link, for instance. The driver decides what
        /// is its own; failing the push on somebody else's frame would fail it for a reason that has
        /// nothing to do with it.
        /// </remarks>
        public void OnFrame(XmsgFrame frame)
        {
            if (frame == null) { throw new ArgumentNullException(nameof(frame)); }

            if (Finished)
            {
                return;
            }

            _driver.OnFrame(frame);
        }

        /// <summary>
        /// Sends whatever the push wants to send next.
        /// </summary>
        /// <param name="host">
        /// The node, which supplies the transport and stamps Flags 1, the counter and the channel.
        /// </param>
        /// <param name="linkReady">
        /// <see langword="true"/> once the link can carry a frame we originate.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="host"/> is null.
        /// </exception>
        /// <remarks>
        /// Called once per loop tick. The driver returns nothing while it is waiting for the
        /// server, so this paces itself against the ladder rather than against a timer.
        /// </remarks>
        public void Pump(XmsgNodeHost host, bool linkReady)
        {
            if (host == null) { throw new ArgumentNullException(nameof(host)); }

            if (ReportIfFinished())
            {
                return;
            }

            if (!linkReady)
            {
                return;
            }

            // The LINK knowing the peer is not enough. The XMSG layer learns its seed when the
            // first datagram is DISPATCHED, which is a tick after the link learned the peer's id,
            // and a frame built in that gap cannot be addressed at all. Measured the hard way: the
            // push fired in the gap and the exception took the whole runner down.
            if (!host.ServerHost.CanReach(_serverNode))
            {
                return;
            }

            if (!_started)
            {
                _started = true;
                Console.WriteLine(
                    $"[push] sending {_localPath} ({_contentLength} bytes) to {_fileSpec} " +
                    $"in {_driver.BlockCount} block(s)");
            }

            // What the driver is ABOUT to do, read before building, because building advances the
            // ladder and the operation would then be the next one.
            FaClientAction action = _driver.NextAction();
            if (action == FaClientAction.Wait)
            {
                return;
            }

            string describedOperation = action == FaClientAction.SendRequest
                ? _driver.CurrentOperation.ToString()
                : action.ToString();

            // A FILE TRANSFER MUST NEVER TAKE THE NODE DOWN. This runs on the link's own loop
            // tick, so an exception escaping here kills the pump and every session on it - which
            // is exactly what happened the first time this ran against a real machine. A push that
            // fails is a failed push; the node keeps serving.
            try
            {
                IReadOnlyList<XmsgFrame> frames = _driver.BuildNext(host.ServerHost);
                if (frames.Count == 0)
                {
                    return;
                }

                for (int i = 0; i < frames.Count; i++)
                {
                    byte[] bytes = frames[i].ToArray();
                    host.Transport.Send(new ReadOnlySpan<byte>(bytes));
                }

                Console.WriteLine(
                    $"[push] {describedOperation}: {frames.Count} frame(s) sent");
            }
            catch (Exception ex)
            {
                _crashed = true;

                // Marked reported here as well, or the next tick would see Finished with an empty
                // Failure and cheerfully announce that the file had been written.
                _reported = true;
                Console.WriteLine(
                    $"[push] *** FAILED *** {describedOperation} threw: {ex.Message}");
                Console.WriteLine("[push] the node is still running; only the push has stopped.");
                return;
            }

            ReportIfFinished();
        }

        /// <summary>
        /// Prints the outcome once, the first time the push is over.
        /// </summary>
        /// <returns>
        /// <see langword="true"/> when the push is finished.
        /// </returns>
        private bool ReportIfFinished()
        {
            if (!Finished)
            {
                return false;
            }

            if (_reported)
            {
                return true;
            }

            _reported = true;

            if (_driver.Failure.Length > 0)
            {
                Console.WriteLine($"[push] *** FAILED *** {_driver.Failure}");
            }
            else
            {
                Console.WriteLine(
                    $"[push] finished: {_contentLength} bytes written to {_fileSpec}. " +
                    "Check it with LIST-FILES on the machine.");
            }

            return true;
        }
    }
}
