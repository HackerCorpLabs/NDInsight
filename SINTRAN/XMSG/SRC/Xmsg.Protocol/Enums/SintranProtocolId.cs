namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// Sub-protocol selector carried at SINTRAN header offset 12.
    /// </summary>
    /// <remarks>
    /// Values from XMSG-PROTOCOL.md section 4.3. This is a stable selector, not a counter.
    /// </remarks>
    public enum SintranProtocolId : byte
    {
        /// <summary>
        /// Network routing / inter-node control (ROUTING).
        /// </summary>
        Routing = 0xDE,

        /// <summary>
        /// Terminal Access and Directory - terminal sessions (TAD).
        /// </summary>
        Tad = 0xDD,

        /// <summary>
        /// Terminal data forwarding (DC).
        /// </summary>
        Dc = 0xDC,

        /// <summary>
        /// Terminal data forwarding, DC variant (DB).
        /// </summary>
        Db = 0xDB,

        /// <summary>
        /// X.25 PAD virtual-circuit data (PAD).
        /// </summary>
        Pad = 0xDA,

        /// <summary>
        /// DC variant (observed; semantics inferred).
        /// </summary>
        D9 = 0xD9,

        /// <summary>
        /// DC variant (observed; semantics inferred).
        /// </summary>
        D8 = 0xD8,
    }
}
