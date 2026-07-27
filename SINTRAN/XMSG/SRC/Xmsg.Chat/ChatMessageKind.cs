namespace NDInsight.Sintran.Xmsg.Chat
{
    /// <summary>
    /// The message types the chat service exchanges.
    /// </summary>
    /// <remarks>
    /// <para><b>This is OUR protocol, not ND's</b></para>
    /// No chat service exists on any SINTRAN image we have - the registry holds terminal access,
    /// file transfer, spooling and file access, and nothing else. So this vocabulary is invented
    /// here. What is NOT invented is everything underneath it: claiming a name, admitting callers
    /// against a free-connection count, learning a sender's address from an arrived message, and
    /// port-to-port delivery are all captured from a running machine.
    /// <para><b>Direction</b></para>
    /// A client only ever sends <see cref="Join"/>, <see cref="Say"/> and <see cref="Leave"/>;
    /// everything else travels from the server.
    /// </remarks>
    public enum ChatMessageKind : byte
    {
        /// <summary>
        /// Not a valid message. Guards against a zeroed buffer being read as a real one.
        /// </summary>
        None = 0,

        /// <summary>
        /// Client to server: asks to enter the room, carrying the nickname to be known by.
        /// </summary>
        /// <remarks>
        /// This is the only message addressed by NAME, so it is the only one that travels through
        /// XROUT. It costs the server one seat of its free-connection count.
        /// </remarks>
        Join = 1,

        /// <summary>
        /// Server to client: you are in, and here is the greeting.
        /// </summary>
        /// <remarks>
        /// Sent straight to the caller's own address, which the server learned from the arrived
        /// join. It is the reply that tells the client the server's address, so everything after
        /// this flows directly between the two ports.
        /// </remarks>
        Welcome = 2,

        /// <summary>
        /// Server to client: refused, with a reason.
        /// </summary>
        Reject = 3,

        /// <summary>
        /// Client to server: say something to the room.
        /// </summary>
        Say = 4,

        /// <summary>
        /// Server to clients: somebody said something.
        /// </summary>
        Said = 5,

        /// <summary>
        /// Client to server: leaving.
        /// </summary>
        Leave = 6,

        /// <summary>
        /// Server to clients: somebody entered the room.
        /// </summary>
        Joined = 7,

        /// <summary>
        /// Server to clients: somebody left the room.
        /// </summary>
        Left = 8,
    }
}
