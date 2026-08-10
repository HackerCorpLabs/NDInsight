namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// What a client should do next in a file-access session.
    /// </summary>
    /// <remarks>
    /// Not a flags enum: a session does exactly one thing at a time. The values are the steps of
    /// the exchange model, not bits to combine.
    /// </remarks>
    public enum FaClientAction
    {
        /// <summary>
        /// Nothing to do - the session is waiting for the peer.
        /// </summary>
        Wait = 0,

        /// <summary>
        /// Send the connect letter to <c>*FA-SERVER</c> and wait for its confirmation.
        /// </summary>
        SendConnectLetter = 1,

        /// <summary>
        /// Send the next request in the ladder.
        /// </summary>
        SendRequest = 2,

        /// <summary>
        /// Acknowledge the reply just received, with a short acknowledgement.
        /// </summary>
        /// <remarks>
        /// A reply is a NEW exchange, so it is answered like any other message. Skipping this is
        /// one of the two ways to fall behind the peer's exchange count and draw an XENSE reject.
        /// </remarks>
        SendShortAck = 3,

        /// <summary>
        /// Send the file content, after the write request has been answered.
        /// </summary>
        SendData = 4,

        /// <summary>
        /// Close the conversation.
        /// </summary>
        SendClose = 5,

        /// <summary>
        /// The session finished successfully.
        /// </summary>
        Done = 6,

        /// <summary>
        /// The session failed. See the session's failure text.
        /// </summary>
        Failed = 7,
    }
}
