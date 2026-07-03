namespace NDInsight.Sintran.Xmsg.Packet
{
    /// <summary>
    /// The universal XMSG envelope identity (XMSG-PROTOCOL.md section 18.5): the sub-protocol
    /// channel (Protocol ID) is not independently allocated — it is DERIVED from the datagram
    /// sequence, the per-direction counter, and the XMCSM service word. This class computes that
    /// derivation, so the responder builds session-data frames on the correct channel instead of
    /// replaying a canned one (a canned channel from another session has the wrong Base and crashes
    /// the peer's XMSG with XXPER).
    /// </summary>
    /// <remarks>
    /// <para><b>Identity.</b> <c>Base = Flags1 + Counter</c> (16-bit), and
    /// <c>Channel = 0xDE - (XMCSM &gt;&gt; 24) - (Base &gt;&gt; 8)</c>.</para>
    /// <para><b>Verified</b> against the <c>conn-to-d102-from-100</c> responder frames — e.g. the
    /// connect-accept (Base 0x0214, XMCSM 0x04000041 → D8), the DUMM session frame (Base 0x020C,
    /// XMCSM 0x01080000 → DB), the 0x20 control frame (Base 0x020C, XMCSM 0x00080000 → DC), and the
    /// MOTD frame (Base 0x020C, XMCSM 0x01080000 → DB) — and against our own live-accepted accept
    /// (Base 0x0014, XMCSM 0x04000041 → DA). Within one stream Flags1 increments while Counter
    /// decrements in lockstep, holding Base (and therefore the channel) constant.</para>
    /// </remarks>
    public static class XmsgEnvelope
    {
        /// <summary>
        /// The constant the channel derivation subtracts from (the top of the Protocol-ID range).
        /// </summary>
        public const byte ChannelAnchor = 0xDE;

        /// <summary>
        /// Computes the envelope base <c>Flags1 + Counter</c> (16-bit, wrapping).
        /// </summary>
        /// <param name="flags1">The datagram sequence (SINTRAN header offsets 8-9).</param>
        /// <param name="counter">The per-direction sub-header counter (offset 0).</param>
        /// <returns>The 16-bit base.</returns>
        public static ushort ComputeBase(ushort flags1, byte counter)
        {
            return (ushort)(flags1 + counter);
        }

        /// <summary>
        /// Derives the sub-protocol channel (Protocol ID) from the envelope model.
        /// </summary>
        /// <param name="flags1">The datagram sequence.</param>
        /// <param name="counter">The per-direction counter.</param>
        /// <param name="controlService">The XMCSM control/service word (its top byte selects the class).</param>
        /// <returns>The derived Protocol ID.</returns>
        public static SintranProtocolId DeriveChannel(ushort flags1, byte counter, uint controlService)
        {
            ushort baseValue = ComputeBase(flags1, counter);
            byte xmcsmHigh = (byte)(controlService >> 24);
            byte baseHigh = (byte)(baseValue >> 8);
            byte channel = (byte)(ChannelAnchor - xmcsmHigh - baseHigh);
            return (SintranProtocolId)channel;
        }
    }
}
