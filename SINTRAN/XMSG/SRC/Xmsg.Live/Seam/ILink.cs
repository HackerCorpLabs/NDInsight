using System;

namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// The bottom seam of the stack: an established data link that delivers L3 payloads UP and
    /// accepts L3 frames DOWN. Shaped to the target <c>ILink</c> in X25Emulator so the XMSG stack
    /// drops in with only this adapter swapped.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The link exposes BOTH <see cref="SendSintranFrame"/> (XMSG/SINTRAN L3) and
    /// <see cref="SendX25Packet"/> (X.25 L3) because the HDLC/LAPB transport is common to both; a
    /// given link instance is <em>bound</em> to one L3 protocol (<see cref="Binding"/>) and throws
    /// from the other send method. This models the real ND machine: the installed software decides
    /// which L3 the link carries, not the wire.
    /// </para>
    /// <para>
    /// Events go UP as named delegates (sender/link-id first, no <c>EventHandler</c>/<c>EventArgs</c>);
    /// method calls go DOWN. Nothing above the link knows about HDLC framing, byte-stuffing or FCS.
    /// </para>
    /// </remarks>
    public interface ILink
    {
        /// <summary>
        /// Up-event: one complete L3 payload (a SINTRAN information field) arrived on this link.
        /// </summary>
        /// <param name="linkId">The link identity (sender-first).</param>
        /// <param name="payload">The payload bytes (the SINTRAN information field).</param>
        /// <param name="length">The number of valid bytes in <paramref name="payload"/>.</param>
        public delegate void LinkPayloadReceived(string linkId, ReadOnlyMemory<byte> payload, int length);

        /// <summary>
        /// Up-event: the link's coarse status changed.
        /// </summary>
        /// <param name="linkId">The link identity (sender-first).</param>
        /// <param name="status">The new status.</param>
        public delegate void LinkStatusChanged(string linkId, LinkStatus status);

        /// <summary>Occurs when a complete L3 payload is delivered up.</summary>
        event LinkPayloadReceived PayloadReceived;

        /// <summary>Occurs when the link status changes.</summary>
        event LinkStatusChanged StatusChanged;

        /// <summary>Gets this link's identity (stamped on every up-event).</summary>
        string LinkId { get; }

        /// <summary>Gets the L3 protocol this link is bound to carry.</summary>
        LinkBinding Binding { get; }

        /// <summary>
        /// Brings the link up (initiates LAPB establishment) and starts pumping received frames.
        /// </summary>
        void Start();

        /// <summary>
        /// Stops pumping and tears the link down.
        /// </summary>
        void Stop();

        /// <summary>
        /// Sends one SINTRAN/XMSG L3 information field as a LAPB I-frame.
        /// </summary>
        /// <param name="infoField">The information-field bytes (SINTRAN header onward).</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when this link is bound to X.25 (<see cref="Binding"/> = <see cref="LinkBinding.X25"/>).
        /// </exception>
        void SendSintranFrame(ReadOnlySpan<byte> infoField);

        /// <summary>
        /// Sends one X.25 L3 packet as a LAPB I-frame.
        /// </summary>
        /// <param name="packet">The X.25 packet bytes.</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when this link is bound to XMSG (<see cref="Binding"/> = <see cref="LinkBinding.Xmsg"/>).
        /// </exception>
        void SendX25Packet(ReadOnlySpan<byte> packet);
    }
}
