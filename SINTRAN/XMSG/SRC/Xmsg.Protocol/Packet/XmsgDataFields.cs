namespace NDInsight.Sintran.Xmsg.Packet
{
    /// <summary>
    /// The full field set of a data (subtype <c>0x0E</c>) XMSG packet: SINTRAN header addressing,
    /// the XMSG sub-header envelope, and the trailing payload. Passed to
    /// <see cref="XmsgPacketBuilder.CreateData"/> so a data packet is built from one intention-
    /// revealing value instead of a dozen positional arguments.
    /// </summary>
    /// <remarks>
    /// These are the exact fields the captured connect handshake is built from (see the
    /// conn-to-d102 accept / port-assign vectors). Field NAMES follow the wire glossary:
    /// XMDSY/XMDPT = destination system/port, XMSSY/XMSPT = source system/port, XMCSM = the
    /// control/service dispatch word. The channel (Protocol ID) and the datagram sequence
    /// (Flags 1) live in the SINTRAN header; the per-direction Counter lives in the sub-header.
    /// </remarks>
    public struct XmsgDataFields
    {
        /// <summary>Destination node number (SINTRAN header offsets 4-5).</summary>
        public ushort DestinationNode;

        /// <summary>Source node number (SINTRAN header offsets 6-7).</summary>
        public ushort SourceNode;

        /// <summary>Flags 1 — the per-direction datagram sequence (offsets 8-9).</summary>
        public ushort Flags1;

        /// <summary>Flags 2 — the frame-class word (offsets 10-11); <c>0x0400</c> for the setup frames.</summary>
        public ushort Flags2;

        /// <summary>The sub-protocol selector / channel (Protocol ID, offset 12).</summary>
        public SintranProtocolId ProtocolId;

        /// <summary>Sub-header per-direction counter (offset 0).</summary>
        public byte Counter;

        /// <summary>Sub-header frame-flags byte (offset 3); <c>0x86</c> for the setup frames.</summary>
        public byte FrameFlags;

        /// <summary>Sub-header role byte (offset 4); low nibble 4 = asker, 0 = responder.</summary>
        public byte Role;

        /// <summary>XMDSY destination system number (offsets 5-6, big-endian).</summary>
        public ushort DestinationSystem;

        /// <summary>XMDPT destination port (offsets 7-8, big-endian).</summary>
        public ushort DestinationPort;

        /// <summary>XMSSY source system number (offsets 9-10, big-endian).</summary>
        public ushort SourceSystem;

        /// <summary>XMSPT source port (offsets 11-12, big-endian).</summary>
        public ushort SourcePort;

        /// <summary>XMCSM control/service dispatch word (offsets 13-16).</summary>
        public uint ControlService;

        /// <summary>
        /// The trailing payload bytes after the 19-byte sub-header (the TAD chain / parameter
        /// blocks). XMLEN (sub-header offset 18) is set from this array's length.
        /// </summary>
        public byte[] Payload;
    }
}
