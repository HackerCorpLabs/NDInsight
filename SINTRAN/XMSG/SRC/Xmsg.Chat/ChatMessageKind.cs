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

        /// <summary>
        /// A member asking to be known by a different name from now on.
        /// </summary>
        /// <remarks>
        /// Sent by the member; the server decides. A rename can be refused for the same reason a
        /// join can - somebody else already answers to that name - so it is a request, not a
        /// statement.
        /// </remarks>
        Rename = 9,

        /// <summary>
        /// The room being told that somebody is now known by a different name.
        /// </summary>
        /// <remarks>
        /// Carries the NEW name in <c>Nickname</c> and the old one in <c>Text</c>. Both are needed:
        /// a client showing a transcript has the old name on screen and would otherwise have no way
        /// to connect the two.
        /// </remarks>
        Renamed = 10,

        /// <summary>
        /// Asking the room who is in it, and the room's answer.
        /// </summary>
        /// <remarks>
        /// <para><b>One kind, both directions</b></para>
        /// A client sends it with an empty <c>Text</c>; the room answers with the same kind and the
        /// members' names in <c>Text</c>, separated by single spaces. A second kind for the reply
        /// would buy nothing - the client knows it asked, and the room never asks.
        /// <para><b>Why the names go in the TEXT</b></para>
        /// The text is the only variable-length field in the format, and its length is two bytes
        /// big-endian, so a full room fits comfortably. The <c>Nickname</c> field carries the ASKER
        /// on the way out and is empty on the way back.
        /// </remarks>
        Who = 11,
    }

    /// <summary>
    /// Facts about <see cref="ChatMessageKind"/> that the decoder needs at runtime.
    /// </summary>
    /// <remarks>
    /// <para><b>Why this exists at all</b></para>
    /// <c>ChatMessage.TryDecode</c> rejects a kind above the last one defined, and that bound has
    /// now been left behind TWICE by a new kind added above it. Each time the effect was the same
    /// and silent: the new kind decoded as a malformed message and was dropped, so one end sent it
    /// and the other never saw it, with nothing failing anywhere. It cost a build cycle on a real
    /// ND-100 the first time and a test run the second.
    /// <para><b>The fix is that the bound has a name and a test</b></para>
    /// <see cref="Highest"/> is what the decoder compares against, and
    /// <c>ChatWhoTests.TheDecoderBoundCoversEveryKind</c> checks it against the enum itself. Adding
    /// a twelfth kind without touching this constant now fails a test instead of losing messages.
    /// </remarks>
    public static class ChatMessageKinds
    {
        /// <summary>
        /// The largest value <see cref="ChatMessageKind"/> defines.
        /// </summary>
        public const byte Highest = (byte)ChatMessageKind.Who;
    }
}
