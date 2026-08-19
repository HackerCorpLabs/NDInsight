using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg;               // XmsgFrame
using NDInsight.Sintran.Xmsg.Protocol.Fa;   // FaClientAction
using NDInsight.Sintran.Xmsg.Node.Seam;     // XmsgNodeHost
using NDInsight.Sintran.Xmsg.Servers.Fa;    // FaReadDriver, FaReadSource

namespace NDInsight.Sintran.Xmsg.Live.Runner
{
    /// <summary>
    /// Drives one file pull from a remote <c>*FA-SERVER</c> into a local file, and says out loud
    /// what it is doing.
    /// </summary>
    /// <remarks>
    /// <para><b>The mirror of <see cref="FaPushRun"/></b></para>
    /// <para>
    /// Same shape, same guards, same reasons for them - a transfer must never take the node down,
    /// it cannot start before the link can carry a frame we originate, and it paces itself against
    /// the ladder rather than a timer. The differences are that the file arrives instead of
    /// leaving, and that its length is not known until the server's open reply.
    /// </para>
    /// <para><b>The local file is written ONCE, at the end</b></para>
    /// <para>
    /// Not block by block as the content arrives. A partly written file on disk looks exactly like
    /// a complete one to whatever reads it next, and a pull that dies halfway would leave a
    /// truncated file with a plausible name. Writing at the end means a file exists only when the
    /// whole thing arrived.
    /// </para>
    /// </remarks>
    internal sealed class FaPullRun
    {
        private readonly FaReadDriver _driver;
        private readonly string _localPath;
        private readonly string _fileSpec;
        private readonly ushort _serverNode;

        /// <summary>
        /// Whether the pull may start from the REMEMBERED seed instead of waiting to be addressed.
        /// </summary>
        /// <remarks>
        /// <para>
        /// This is <c>--originate-from-seed</c>, and until 2026-08-17 only the SYNC daemon honoured
        /// it. The pull gated on <c>CanReach</c> alone, which is true only once the PEER has
        /// addressed us - so on a link where nothing arrives first the pull sat silent for ever. It
        /// did not even print "reading &lt;file&gt;", because that line comes after the gate, and a
        /// run that never starts looks exactly like a run that started and got no answer.
        /// </para>
        /// <para>
        /// Measured the same day: a push over the same seam, in the same minute, carried 42368
        /// bytes - because it went through the sync daemon, which built its readiness with the
        /// seed. The one-shot pull never sent a frame.
        /// </para>
        /// </remarks>
        public bool OriginateFromSeed { get; set; }
        /// <summary>
        /// Gets the node this transfer talks to.
        /// </summary>
        /// <remarks>
        /// Exposed so the runner can open the XMSG link from the REMEMBERED seed before pumping.
        /// Without that, a transfer can only start against a peer that has spoken to us first,
        /// which is useless for a daemon and makes a transfer against an idle peer unobservable.
        /// </remarks>
        public ushort RemoteNode
        {
            get { return _serverNode; }
        }


        private bool _started;
        private bool _reported;

        /// <summary>
        /// Set when the pull threw, so it stops instead of throwing again every tick.
        /// </summary>
        private bool _crashed;

        /// <summary>
        /// Set once the block count is known, so it is announced exactly once.
        /// </summary>
        private bool _announcedLength;

        /// <summary>
        /// Creates a pull.
        /// </summary>
        /// <param name="localPath">
        /// Where to write the file once it has all arrived.
        /// </param>
        /// <param name="source">
        /// Where the file is coming from.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when any argument is null.
        /// </exception>
        public FaPullRun(string localPath, FaReadSource source)
        {
            if (localPath == null) { throw new ArgumentNullException(nameof(localPath)); }
            if (source == null) { throw new ArgumentNullException(nameof(source)); }

            _localPath = localPath;
            _serverNode = source.ServerNode;
            _fileSpec = source.FileSpec;
            _driver = new FaReadDriver(source);
        }

        /// <summary>
        /// Gets whether the pull has finished, either way.
        /// </summary>
        public bool Finished
        {
            get
            {
                // Not finished while a goodbye is owed - see FaPushRun for the measurement.
                if (_driver.ReleasePending) { return false; }

                return _crashed || _driver.Done || _driver.Failure.Length > 0;
            }
        }

        /// <summary>
        /// Gets whether the transfer finished BADLY, as opposed to merely finishing.
        /// </summary>
        /// <remarks>
        /// <c>Finished</c> is true either way - it answers "is it over", not "did it work" - and
        /// reading it as success is how a refused transfer came to exit 0. MEASURED 2026-08-18: a
        /// push the machine refused with SINTRAN error 39 printed a byte count and returned success.
        /// </remarks>
        public bool Failed
        {
            get { return _crashed || _driver.Failure.Length > 0; }
        }

        /// <summary>
        /// Gets why the transfer failed, or an empty string when it did not.
        /// </summary>
        /// <remarks>
        /// Carries the SERVER's own words where there are any - a refusal decodes to a SINTRAN
        /// error number and its meaning. Worth passing on rather than replacing with a summary:
        /// "No such user name in main directory" tells the operator what to fix, "the push failed"
        /// does not.
        /// </remarks>
        public string Failure
        {
            get { return _driver.Failure; }
        }

        /// <summary>
        /// Gets the SINTRAN error number behind the failure, or zero when there is none.
        /// </summary>
        /// <remarks>
        /// Taken from the driver rather than scraped back out of <see cref="Failure"/>. The text is
        /// for a person; a caller that has to make a DECISION on the number needs the number, and
        /// one caller does - the sync daemon treats 62, "File already exists", as an answer rather
        /// than a fault.
        /// </remarks>
        public int SintranError
        {
            get { return _driver.SintranError; }
        }

        /// <summary>
        /// Feeds a received datagram to the pull.
        /// </summary>
        /// <param name="frame">
        /// The frame the node received.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="frame"/> is null.
        /// </exception>
        /// <remarks>
        /// Everything arriving on the node goes through here, including traffic that has nothing to
        /// do with the pull. The driver decides what is its own.
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
        /// Sends whatever the pull wants to send next.
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
            // first datagram is DISPATCHED, a tick after the link learned the peer's id, and a
            // frame built in that gap cannot be addressed at all.
            //
            // CanReach alone means "the peer has addressed us", which never happens on a link
            // where we speak first - so with it as the only gate a pull waits for ever and says
            // nothing while it waits. OriginateFromSeed opens the link from the REMEMBERED seed
            // instead, which is the same choice the sync daemon has always had.
            bool reachable = OriginateFromSeed
                ? host.ServerHost.OpenLinkFromRememberedSeed(_serverNode)
                : host.ServerHost.CanReach(_serverNode);

            if (!reachable)
            {
                return;
            }

            if (!_started)
            {
                _started = true;
                Console.WriteLine(
                    $"[pull] reading {_fileSpec} from node {_serverNode} into {_localPath}");
            }

            // The length arrives in the open reply, several steps in. Announced once, because it
            // is the number that decides everything after it - how many blocks are asked for and
            // where the file is trimmed.
            if (!_announcedLength && _driver.FileLength > 0)
            {
                _announcedLength = true;
                Console.WriteLine(
                    $"[pull] the server says {_fileSpec} is {_driver.FileLength} bytes, " +
                    $"so {_driver.BlockCount} block(s)");
            }

            // What the driver is ABOUT to do, read before building, because building advances the
            // ladder and the operation would then be the next one.
            FaClientAction action = _driver.NextAction();
            if (action == FaClientAction.Wait)
            {
                return;
            }

            // Name the OPERATION for a request, not just "SendRequest" - a ladder that stalls is
            // diagnosed by which step it stalled on, and every step is a SendRequest.
            string describedOperation = action == FaClientAction.SendRequest
                ? _driver.CurrentOperation.ToString()
                : action.ToString();

            // A FILE TRANSFER MUST NEVER TAKE THE NODE DOWN. This runs on the link's own loop tick,
            // so an exception escaping here would kill the pump and every session on it.
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

                // Every step logged as it goes out, the same as the push. Without this a stalled
                // ladder is invisible: the first live run showed "reading BIGPSH3:TXT" and then
                // nothing at all, and there was no way to tell a letter that was never sent from
                // one that was never answered.
                Console.WriteLine($"[pull] {describedOperation}: {frames.Count} frame(s) sent");
            }
            catch (Exception ex)
            {
                _crashed = true;

                // Marked reported here as well, or the next tick would see Finished with an empty
                // Failure and cheerfully announce that the file had arrived.
                _reported = true;
                Console.WriteLine(
                    $"[pull] *** FAILED *** {describedOperation} threw: {ex.Message}");
                Console.WriteLine("[pull] the node is still running; only the pull has stopped.");
                return;
            }

            ReportIfFinished();
        }

        /// <summary>
        /// Prints the outcome once, the first time the pull is over, and saves the file.
        /// </summary>
        /// <returns>
        /// <see langword="true"/> when the pull is finished.
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
                Console.WriteLine($"[pull] *** FAILED *** {_driver.Failure}");
                return true;
            }

            byte[] content = _driver.Content();

            try
            {
                System.IO.File.WriteAllBytes(_localPath, content);
            }
            catch (Exception ex)
            {
                Console.WriteLine(
                    $"[pull] read {content.Length} bytes but could not write '{_localPath}': "
                    + ex.Message);
                return true;
            }

            Console.WriteLine(
                $"[pull] finished: {content.Length} bytes read from {_fileSpec} into {_localPath}");

            // A non-zero count means the server repeated content and we discarded it as a repeat.
            // That is normal after a lost acknowledgement, but it is also the only way a byte could
            // go missing without anything else noticing, so it is said out loud rather than buried.
            Console.WriteLine(
                "[pull] compare it against the original before trusting it - our own log is not "
                + "evidence that the bytes are right.");

            return true;
        }
    }
}
