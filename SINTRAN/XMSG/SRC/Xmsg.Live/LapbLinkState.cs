namespace NDInsight.Sintran.Xmsg.Live
{
    /// <summary>
    /// The connection state of a modulo-8 LAPB ABM link.
    /// </summary>
    public enum LapbLinkState
    {
        /// <summary>
        /// No link established; no sequence state is valid.
        /// </summary>
        Disconnected,

        /// <summary>
        /// A SABM has been transmitted and the matching UA is awaited.
        /// </summary>
        SabmSent,

        /// <summary>
        /// The link is established; <c>V(S)</c> and <c>V(R)</c> are being maintained.
        /// </summary>
        Connected,
    }
}
