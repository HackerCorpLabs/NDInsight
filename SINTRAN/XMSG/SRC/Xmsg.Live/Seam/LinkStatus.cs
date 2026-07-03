namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// The coarse up/down status an <see cref="ILink"/> reports through its StatusChanged event.
    /// </summary>
    /// <remarks>
    /// This is the seam-level status the layer above cares about (is the pipe usable), distinct
    /// from the detailed <see cref="LapbLinkState"/> the LAPB state machine tracks internally.
    /// </remarks>
    public enum LinkStatus
    {
        /// <summary>The link is not established; frames cannot be sent.</summary>
        Down,

        /// <summary>The link is established (LAPB Connected); information frames flow.</summary>
        Up,
    }
}
