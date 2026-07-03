using System;

namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// Operational status of a link.
    /// </summary>
    public enum LinkStatus
    {
        /// <summary>The link is stopped / not running.</summary>
        Stopped,
        /// <summary>The link is starting up (LAPB establishment in progress).</summary>
        Starting,
        /// <summary>The link is established and can carry L3 traffic.</summary>
        Active,
        /// <summary>The link is shutting down.</summary>
        Stopping,
        /// <summary>The link failed and is unusable.</summary>
        Error
    }

    /// <summary>
    /// The L3 protocol a link is configured to carry.
    /// </summary>
    /// <remarks>
    /// This is <b>adapter-internal</b> configuration — it is NOT part of <see cref="ILink"/> and no
    /// code above the seam reads it (binding enforcement is entirely the adapter's business). The
    /// consuming emulator has the same concept under a different name (ProtocolMode); the naming is
    /// reconciled at port time and does not touch upper layers.
    /// </remarks>
    public enum LinkBinding
    {
        /// <summary>The link carries SINTRAN/XMSG L3 frames.</summary>
        Xmsg,
        /// <summary>The link carries X.25 L3 packets.</summary>
        X25
    }

    /// <summary>
    /// Raised when one complete L3 payload (a LAPB I-frame information field)
    /// is delivered up from a link. The link does not interpret the payload.
    /// </summary>
    /// <param name="link">The link that received the payload (sender-first).</param>
    /// <param name="payload">Buffer holding the payload bytes.</param>
    /// <param name="length">Number of valid bytes in <paramref name="payload"/>.</param>
    public delegate void LinkPayloadReceived(ILink link, byte[] payload, int length);

    /// <summary>
    /// Raised when a link's operational status changes. Both the previous and the new status are
    /// carried (plus a short human-readable reason) so no information is lost — consumers that only
    /// care about the new value ignore the rest.
    /// </summary>
    /// <param name="link">The link whose status changed (sender-first).</param>
    /// <param name="oldStatus">The status the link was in before this transition.</param>
    /// <param name="newStatus">The status the link is in after this transition.</param>
    /// <param name="reason">A short human-readable reason for the transition (for logs).</param>
    public delegate void LinkStatusChanged(ILink link, LinkStatus oldStatus, LinkStatus newStatus, string reason);

    /// <summary>
    /// The bottom seam of the stack: an established data link that delivers L3
    /// payloads up as events and accepts L3 frames down as method calls. Nothing
    /// above this interface knows about HDLC framing, byte-stuffing, FCS, or LAPB.
    /// </summary>
    /// <remarks>
    /// This is a strict subset of the consuming emulator's link interface; code
    /// above the seam must depend only on the members declared here.
    /// </remarks>
    public interface ILink : IDisposable
    {
        /// <summary>Gets the unique name identifying this link (stamped on log lines).</summary>
        string Name { get; }

        /// <summary>Gets the current operational status.</summary>
        LinkStatus Status { get; }

        /// <summary>
        /// Starts the link: initiates LAPB establishment and begins delivering payloads.
        /// </summary>
        /// <returns>True when the link started; false when it could not start.</returns>
        bool Start();

        /// <summary>Stops the link and tears it down. Idempotent.</summary>
        void Stop();

        /// <summary>
        /// Sends one SINTRAN/XMSG L3 information field as a LAPB I-frame.
        /// </summary>
        /// <param name="frame">Buffer holding the information-field bytes.</param>
        /// <param name="length">Number of valid bytes in <paramref name="frame"/>.</param>
        /// <returns>
        /// True when queued for transmission; false when the link is not Active
        /// or is bound to X.25 (logged, never thrown).
        /// </returns>
        bool SendSintranFrame(byte[] frame, int length);

        /// <summary>
        /// Sends one X.25 L3 packet as a LAPB I-frame.
        /// </summary>
        /// <param name="packet">Buffer holding the packet bytes.</param>
        /// <param name="length">Number of valid bytes in <paramref name="packet"/>.</param>
        /// <returns>
        /// True when queued for transmission; false when the link is not Active
        /// or is bound to XMSG (logged, never thrown).
        /// </returns>
        bool SendX25Packet(byte[] packet, int length);

        /// <summary>Occurs when a complete L3 payload is delivered up.</summary>
        event LinkPayloadReceived PayloadReceived;

        /// <summary>Occurs when the link status changes.</summary>
        event LinkStatusChanged StatusChanged;
    }
}
