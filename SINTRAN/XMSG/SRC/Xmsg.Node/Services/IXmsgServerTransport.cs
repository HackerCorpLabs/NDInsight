using System;

namespace NDInsight.Sintran.Xmsg.Node.Services
{
    /// <summary>
    /// The narrow transport surface a high-level XMSG server (for example the TAD terminal server or,
    /// later, the XM-FIDO file server) is allowed to use. The node - NOT the server - owns all the
    /// low-level sequencing: the per-link outgoing datagram sequence (Flags 1), the sub-header Counter
    /// and the sub-protocol channel are all filled in by <see cref="BuildDatagram"/> via the envelope
    /// model, and the secure-ACK of incoming frames is done by the node too.
    /// </summary>
    /// <remarks>
    /// This is the deliberate firewall between the replaceable server layer and the XMSG transport:
    /// a server supplies only addressing, frame class (XMCSM), role and payload, and never sees a
    /// seed, a counter or a channel. That keeps the whole servers layer swappable without touching
    /// the protocol code. It is distinct from the seam's byte-level <c>Codec.IXmsgTransport</c>: this
    /// one builds whole XMSG datagrams; that one moves raw bytes.
    /// </remarks>
    public interface IXmsgServerTransport
    {
        /// <summary>
        /// Gets this node's number (for example 102), so a server can stamp its own identity.
        /// </summary>
        ushort NodeNumber { get; }

        /// <summary>
        /// Allocates a globally-unique session (subprocess) port for a new session, so ports never
        /// collide across servers or concurrent sessions. The layout is the verified
        /// <c>(logical &lt;&lt; 7) | incarnation</c> form.
        /// </summary>
        /// <returns>
        /// A fresh session port.
        /// </returns>
        ushort AllocateSessionPort();

        /// <summary>
        /// Allocates a monotonic session number (the operator-visible <c>ttyN</c> / TAD number).
        /// </summary>
        /// <returns>
        /// A fresh session number (1-based).
        /// </returns>
        int AllocateSessionNumber();

        /// <summary>
        /// Builds one outgoing datagram to a client endpoint. The node assigns the per-link Flags 1
        /// (the single continuous outgoing sequence to <paramref name="remoteNode"/>, shared by every
        /// server and session on that link), and derives the Counter and channel from the envelope
        /// model, using the seed learned when the link was first seen.
        /// </summary>
        /// <param name="remoteNode">The client's node number (header destination, for example 100).</param>
        /// <param name="clientSystem">The client's system number (sub-header destination system).</param>
        /// <param name="clientPort">The client's port (sub-header destination port).</param>
        /// <param name="sourcePort">Our source port (the server's well-known port or a session port).</param>
        /// <param name="controlService">The XMCSM control/service word (its high half is the frame class).</param>
        /// <param name="frameFlags">The sub-header frame-flags byte.</param>
        /// <param name="role">The sub-header role byte.</param>
        /// <param name="payload">The trailer payload bytes (a TAD chain, a letter body, and so on).</param>
        /// <returns>
        /// The assembled datagram, ready to transmit.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// Thrown when no link to <paramref name="remoteNode"/> has been established (no seed learned).
        /// </exception>
        XmsgFrame BuildDatagram(
            ushort remoteNode,
            ushort clientSystem,
            ushort clientPort,
            ushort sourcePort,
            uint controlService,
            byte frameFlags,
            byte role,
            byte[] payload);
    }
}
