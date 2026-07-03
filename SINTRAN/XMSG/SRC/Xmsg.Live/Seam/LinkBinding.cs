namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// The L3 protocol an HDLC/LAPB link is bound to carry.
    /// </summary>
    /// <remarks>
    /// A SINTRAN HDLC link runs LAPB over HDLC and carries <b>either X.25 or XMSG at L3, decided by
    /// the software installed on the ND machine</b> — the same physical link type does both, and the
    /// transport itself does not reveal which. So the binding is <em>configuration</em>, set at
    /// composition time, not something sniffed per packet (see XMSG-TRANSPORT-SEAM-PLAN.md section 5).
    /// Our bridge is bound to <see cref="Xmsg"/>.
    /// </remarks>
    public enum LinkBinding
    {
        /// <summary>The link carries XMSG (SINTRAN inter-node messaging) at L3.</summary>
        Xmsg,

        /// <summary>The link carries X.25 packets at L3.</summary>
        X25,
    }
}
