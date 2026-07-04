namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// SINTRAN header packet-subtype code (offset 3). Identifies the message kind;
    /// it is not a length.
    /// </summary>
    /// <remarks>
    /// Values from XMSG-PROTOCOL.md section 4.1. These subtypes occur across the 1947-frame
    /// verified corpus, plus the network-error subtype seen on live rejects.
    /// </remarks>
    public enum SintranPacketSubtype : byte
    {
        /// <summary>
        /// Network error / reject frame (<c>0x07</c>).
        /// </summary>
        /// <remarks>
        /// Carries a negative XE* error code in Flags2 (for example XEIMA -19 invalid magic, XENSE -34
        /// sequence error). Observed live when a node rejects a datagram; not part of the four core
        /// corpus subtypes but a real on-wire value.
        /// </remarks>
        NetworkError = 0x07,

        /// <summary>
        /// Delivery acknowledgment / flow-control frame.
        /// </summary>
        /// <remarks>
        /// 14-byte frame whose Flags1 echoes the acknowledged data frame's datagram sequence.
        /// </remarks>
        Ack = 0x03,

        /// <summary>
        /// Data message carrying the XMSG sub-header and user payload.
        /// </summary>
        Data = 0x0E,

        /// <summary>
        /// Reachability reply (answer to a reachability request).
        /// </summary>
        ReachabilityReply = 0x13,

        /// <summary>
        /// Reachability request (probe testing whether a remote node is accessible).
        /// </summary>
        ReachabilityRequest = 0x19,
    }
}
