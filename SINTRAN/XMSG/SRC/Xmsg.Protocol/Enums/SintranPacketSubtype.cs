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
        /// First fragment of a message too large for one LAPB frame (<c>0x0A</c>).
        /// </summary>
        /// <remarks>
        /// <para>
        /// Carries the same 28-byte header as <see cref="Data"/> - SINTRAN header plus the full
        /// addressing words - and then as much of the message body as fits. Flags2 is the TOTAL
        /// message length, not this fragment's length. The rest arrives as
        /// <see cref="MessageContinuation"/> with the same Flags1 datagram sequence.
        /// </para>
        /// <para>
        /// Observed on every COSMOS file-transfer data message, which is 1030 bytes and splits
        /// 594 + 436. Capture: <c>claude-transfer-file-COMPLETE-102-to-100-2026-07-29.pcapng</c>.
        /// </para>
        /// </remarks>
        MessageFirstFragment = 0x0A,

        /// <summary>
        /// Continuation of a message begun by <see cref="MessageFirstFragment"/> (<c>0x0C</c>).
        /// </summary>
        /// <remarks>
        /// Does NOT repeat the addressing words: the body resumes at offset 14, after the 13-byte
        /// SINTRAN header and the single counter byte. Flags2 is the byte OFFSET into the message at
        /// which this fragment resumes, so <c>offset + fragment length == total length</c>.
        /// </remarks>
        MessageContinuation = 0x0C,

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
