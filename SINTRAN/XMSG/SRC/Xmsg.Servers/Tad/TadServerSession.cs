using System;
using System.Collections.Generic;

namespace NDInsight.Sintran.Xmsg.Servers.Tad
{
    /// <summary>
    /// The login phases a TAD server session walks before the command loop is reachable.
    /// </summary>
    public enum TadServerLoginPhase
    {
        /// <summary>Awaiting the username line (the banner's "ENTER " prompt).</summary>
        Username,

        /// <summary>Awaiting the password line (no-echo).</summary>
        Password,

        /// <summary>A valid login completed.</summary>
        LoggedIn,
    }

    /// <summary>
    /// All per-session state for one TAD terminal connection the server answers: the client endpoint,
    /// the allocated session port and TAD number, the login/negotiation state, the connect-letter
    /// metadata, and the pending tty output queue. One instance exists per concurrent connect.
    /// </summary>
    /// <remarks>
    /// The transport SEQUENCING is NOT here - the node owns the per-link Flags 1 - so two sessions to
    /// the same node interleave on one continuous outgoing counter. Output text is queued here and the
    /// server drains it to frames, which is what makes tty inject / wall async and terminal-agnostic.
    /// </remarks>
    public sealed class TadServerSession
    {
        private readonly List<string> _pendingOutput;

        /// <summary>
        /// Initialises a session for a connect from a given client endpoint.
        /// </summary>
        /// <param name="remoteNode">The client's node number (header source).</param>
        /// <param name="clientSystem">The client's system number (sub-header source system).</param>
        /// <param name="clientPort">The client's source port (stable for the session's lifetime).</param>
        /// <param name="sessionWirePort">The unique session port allocated for this session.</param>
        /// <param name="tadNumber">The TAD number (ttyN) shown to operators and used by who/tell/wall.</param>
        public TadServerSession(ushort remoteNode, ushort clientSystem, ushort clientPort, ushort sessionWirePort, int tadNumber)
        {
            RemoteNode = remoteNode;
            ClientSystem = clientSystem;
            ClientPort = clientPort;
            SessionWirePort = sessionWirePort;
            TadNumber = tadNumber;
            Phase = TadServerLoginPhase.Username;
            PendingUsername = string.Empty;
            Username = string.Empty;
            OsVersion = Array.Empty<byte>();
            ConnectService = string.Empty;
            ConnectTargetName = string.Empty;
            _pendingOutput = new List<string>();
        }

        /// <summary>Gets the client's node number (header source).</summary>
        public ushort RemoteNode { get; }

        /// <summary>Gets the client's system number (sub-header source system).</summary>
        public ushort ClientSystem { get; }

        /// <summary>Gets the client's source port (stable for the whole session).</summary>
        public ushort ClientPort { get; }

        /// <summary>Gets the unique session port allocated for this session.</summary>
        public ushort SessionWirePort { get; }

        /// <summary>Gets the TAD number (ttyN) for this session.</summary>
        public int TadNumber { get; }

        /// <summary>Gets or sets the current login phase.</summary>
        public TadServerLoginPhase Phase { get; set; }

        /// <summary>Gets or sets the username entered but not yet validated (during the password phase).</summary>
        public string PendingUsername { get; set; }

        /// <summary>Gets or sets the logged-in username (empty until a valid login).</summary>
        public string Username { get; set; }

        /// <summary>Gets or sets the count of wrong-credential attempts.</summary>
        public int LoginFaults { get; set; }

        /// <summary>Gets or sets a value indicating whether the MOTD has been sent.</summary>
        public bool MotdSent { get; set; }

        /// <summary>Gets a value indicating whether this session has completed login.</summary>
        public bool IsLoggedIn
        {
            get { return Phase == TadServerLoginPhase.LoggedIn; }
        }

        // --- Connect resync (XENSE) state -------------------------------------------------------

        /// <summary>Gets or sets the retained connect frame (to rebuild the accept during a resync).</summary>
        public XmsgFrame? ConnectFrame { get; set; }

        /// <summary>Gets or sets the Flags 1 our accept currently uses (stepped down on each XENSE).</summary>
        public ushort AcceptFlags1 { get; set; }

        /// <summary>Gets or sets a value indicating whether the session-setup has been seen (resync stops).</summary>
        public bool SessionSetupSeen { get; set; }

        // --- Negotiation metadata (for the "stat" command) --------------------------------------

        /// <summary>Gets or sets a value indicating whether the TMOD/TTYP/DESC/OPSV chain was received.</summary>
        public bool NegotiationSeen { get; set; }

        /// <summary>Gets or sets the terminal type (TTYP 0x0D).</summary>
        public ushort TerminalType { get; set; }

        /// <summary>Gets or sets the terminal mode (TMOD 0x0C).</summary>
        public byte TerminalMode { get; set; }

        /// <summary>Gets or sets the escape character (DESC 0x0F).</summary>
        public byte EscapeChar { get; set; }

        /// <summary>Gets or sets the host OS-version bytes (OPSV 0x1F).</summary>
        public byte[] OsVersion { get; set; }

        /// <summary>Gets or sets the connect-letter service string (for example "*TADADM").</summary>
        public string ConnectService { get; set; }

        /// <summary>Gets or sets the connect-letter target system name (for example "D102").</summary>
        public string ConnectTargetName { get; set; }

        // --- tty output queue -------------------------------------------------------------------

        /// <summary>
        /// Gets a value indicating whether this session has queued output waiting to be drained.
        /// </summary>
        public bool HasPendingOutput
        {
            get { return _pendingOutput.Count != 0; }
        }

        /// <summary>
        /// Queues a segment of terminal text for this session (a command reply or an injected message).
        /// </summary>
        /// <param name="text">
        /// The text to append to the output queue.
        /// </param>
        public void Enqueue(string text)
        {
            if (!string.IsNullOrEmpty(text))
            {
                _pendingOutput.Add(text);
            }
        }

        /// <summary>
        /// Takes and clears all queued output text, concatenated in order.
        /// </summary>
        /// <returns>
        /// The queued text, or an empty string when nothing was pending.
        /// </returns>
        public string TakePendingOutput()
        {
            if (_pendingOutput.Count == 0)
            {
                return string.Empty;
            }

            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            for (int i = 0; i < _pendingOutput.Count; i++)
            {
                sb.Append(_pendingOutput[i]);
            }

            _pendingOutput.Clear();
            return sb.ToString();
        }
    }
}
