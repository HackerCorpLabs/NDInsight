namespace NDInsight.Sintran.Xmsg.Ethernet
{
    /// <summary>
    /// Raised when a frame arrives whose ND link-layer kind byte is not one we handle.
    /// </summary>
    /// <param name="kind">
    /// The kind byte as it arrived.
    /// </param>
    /// <remarks>
    /// An unknown kind is worth surfacing rather than dropping quietly: the Ethernet path to D19999
    /// dies on a <c>0x60</c> nobody has decoded, and a silent drop is how that stayed invisible.
    /// </remarks>
    public delegate void UnknownFrameKindReceived(byte kind);

    /// <summary>
    /// Raised when the peer asks for the link to be taken down.
    /// </summary>
    /// <param name="kind">
    /// The kind byte that carried the request.
    /// </param>
    public delegate void DisconnectRequested(byte kind);

    /// <summary>
    /// Raised when the peer's identity has been learned from an inbound frame.
    /// </summary>
    public delegate void PeerLearned();

    /// <summary>
    /// Raised after a frame has been handed to the transport.
    /// </summary>
    /// <param name="frame">
    /// The buffer the frame was built in. It is REUSED after this call returns, so a handler that
    /// needs to keep the bytes must copy them.
    /// </param>
    /// <param name="length">
    /// How many bytes of <paramref name="frame"/> make up the frame.
    /// </param>
    public delegate void FrameTransmitted(byte[] frame, int length);
}
