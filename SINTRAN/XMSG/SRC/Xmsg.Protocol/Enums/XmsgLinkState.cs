namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// Inter-system link states driven by the XSLKI "Start-Link" operation and
    /// reported in parameter 1 of a link status request.
    /// </summary>
    /// <remarks>
    /// Values from XMSG-API.md section 5.1.
    /// </remarks>
    public enum XmsgLinkState : int
    {
        /// <summary>
        /// Link is dead (crashed).
        /// </summary>
        Dead = 0,

        /// <summary>
        /// Link is initialising.
        /// </summary>
        Init = 1,

        /// <summary>
        /// Link is calling (sending SABM frames).
        /// </summary>
        Call = 2,

        /// <summary>
        /// SABM has been seen; the link sends RR frames.
        /// </summary>
        Conn = 3,

        /// <summary>
        /// RR has been seen; data phase active and the neighbour is marked reachable.
        /// </summary>
        Run = 4,

        /// <summary>
        /// Link is being killed.
        /// </summary>
        Kill = 5,
    }
}
