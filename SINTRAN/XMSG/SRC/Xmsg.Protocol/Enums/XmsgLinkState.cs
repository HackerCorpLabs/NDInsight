namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// Inter-system link states driven by the XSLKI "Start-Link" operation and
    /// reported in parameter 1 of a link status request.
    /// </summary>
    /// <remarks>
    /// Values from XMSG-API.md section 5.1.
    /// Audited against the version-L symbol files (XMSG-VALUES-L.SYMB and
    /// XMSG-PL-VALUES-L.INCL): neither file defines any link-state symbol, so
    /// there is nothing to add here and no ND symbol name to attach. The
    /// symbol files only define the XSLKI service code that starts a link.
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
