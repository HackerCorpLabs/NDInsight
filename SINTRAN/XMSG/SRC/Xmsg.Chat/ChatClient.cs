using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Api;

namespace NDInsight.Sintran.Xmsg.Chat
{
    /// <summary>
    /// One user in a chat room: joins a service by name, then talks to it directly.
    /// </summary>
    /// <remarks>
    /// <para><b>The client never learns an address in advance</b></para>
    /// It writes to a NAME. XROUT forwards that letter and tells the client nothing about where it
    /// went - that is the whole point of the letterbox model. The server's address arrives with the
    /// welcome, and only then can the client speak to it directly. A client that is refused, or
    /// that writes to a full room, never learns anything about the server at all.
    /// <para><b>No blocking</b></para>
    /// <see cref="Poll"/> returns what has arrived and nothing more. There is no scheduler to
    /// suspend on, so a caller drives this from its own loop.
    /// </remarks>
    public sealed class ChatClient
    {
        private readonly XmsgKernel _kernel;
        private readonly XroutDirectory _directory;
        private readonly string _nickname;
        private readonly byte[] _scratch;

        private XmsgPortNumber _port;
        private XmsgMagicNumber _magic;
        private XmsgMagicNumber _server;
        private bool _hasPort;
        private bool _joined;

        /// <summary>
        /// Initialises a chat client on a kernel and a name table.
        /// </summary>
        /// <param name="kernel">
        /// The kernel this user's port belongs to.
        /// </param>
        /// <param name="directory">
        /// The XROUT stand-in used to post the join letter.
        /// </param>
        /// <param name="nickname">
        /// The name to be known by in the room.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when any argument is null.
        /// </exception>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="nickname"/> is empty.
        /// </exception>
        public ChatClient(XmsgKernel kernel, XroutDirectory directory, string nickname)
        {
            if (kernel == null)
            {
                throw new ArgumentNullException(nameof(kernel));
            }

            if (directory == null)
            {
                throw new ArgumentNullException(nameof(directory));
            }

            if (nickname == null)
            {
                throw new ArgumentNullException(nameof(nickname));
            }

            if (nickname.Length == 0)
            {
                throw new ArgumentException("A nickname is required.", nameof(nickname));
            }

            _kernel = kernel;
            _directory = directory;
            _nickname = nickname;
            _scratch = new byte[1024];
        }

        /// <summary>
        /// Gets the nickname this client joined under.
        /// </summary>
        public string Nickname
        {
            get { return _nickname; }
        }

        /// <summary>
        /// Gets a value indicating whether the server has welcomed this client.
        /// </summary>
        public bool IsJoined
        {
            get { return _joined; }
        }

        /// <summary>
        /// Opens a port and posts a join letter to a named room.
        /// </summary>
        /// <param name="roomName">
        /// The registered name of the chat service.
        /// </param>
        /// <returns>
        /// <see cref="XroutError.XRSOK"/> when the letter was forwarded,
        /// <see cref="XroutError.XRUNN"/> when no such name is registered, or
        /// <see cref="XroutError.XRNSP"/> when the room is full.
        /// </returns>
        /// <remarks>
        /// A successful return means the letter reached the server, NOT that you are in the room.
        /// The server still decides; <see cref="Poll"/> brings back the welcome or the refusal.
        /// </remarks>
        public XroutError Join(string roomName)
        {
            if (roomName == null)
            {
                throw new ArgumentNullException(nameof(roomName));
            }

            if (!_hasPort)
            {
                XmsgStatus opened = _kernel.OpenPort(out _port);
                if (opened.IsError)
                {
                    return XroutError.XRNSP;
                }

                XmsgStatus converted = _kernel.ConvertPortToMagic(_port, out _magic);
                if (converted.IsError)
                {
                    return XroutError.XRUNM;
                }

                _hasPort = true;
            }

            ChatMessage join = new ChatMessage(ChatMessageKind.Join, _nickname, string.Empty);
            byte[] bytes = new byte[join.ByteCount];
            join.Encode(bytes);

            return _directory.SendLetter(roomName, _magic, bytes);
        }

        /// <summary>
        /// Says something to the room.
        /// </summary>
        /// <param name="text">
        /// The line to say.
        /// </param>
        /// <returns>
        /// True when the line was sent; false when this client has not been welcomed yet.
        /// </returns>
        public bool Say(string text)
        {
            if (!_joined)
            {
                return false;
            }

            SendToServer(new ChatMessage(ChatMessageKind.Say, _nickname, text));
            return true;
        }

        /// <summary>
        /// Leaves the room and closes the port.
        /// </summary>
        /// <remarks>
        /// Tell the server. It is what returns the seat to the room's free count - a client that
        /// just vanishes holds its seat until something else clears it.
        /// </remarks>
        public void Leave()
        {
            if (_joined)
            {
                SendToServer(new ChatMessage(ChatMessageKind.Leave, _nickname, string.Empty));
                _joined = false;
            }

            if (_hasPort)
            {
                _kernel.ClosePort(_port);
                _hasPort = false;
            }
        }

        /// <summary>
        /// Collects everything that has arrived for this client.
        /// </summary>
        /// <returns>
        /// The messages received, oldest first. Empty when nothing was waiting.
        /// </returns>
        public IReadOnlyList<ChatMessage> Poll()
        {
            List<ChatMessage> received = new List<ChatMessage>();
            if (!_hasPort)
            {
                return received;
            }

            while (true)
            {
                XmsgReceiveResult arrived = _kernel.Receive(_port, XmsgWaitOptions.None);
                if (!arrived.Received)
                {
                    return received;
                }

                XmsgMagicNumber sender = _kernel.GetMessageStatus(arrived.Message).Sender;

                int read;
                _kernel.Read(arrived.Message, _scratch, 0, out read);
                _kernel.ReleaseBuffer(arrived.Message);

                ChatMessage message;
                if (!ChatMessage.TryDecode(new ReadOnlySpan<byte>(_scratch, 0, read), out message))
                {
                    continue;
                }

                if (message.Kind == ChatMessageKind.Welcome)
                {
                    // The welcome is what reveals the server's address. Keep it: everything this
                    // client sends from now on goes straight there, with no name lookup.
                    _server = sender;
                    _joined = true;
                }

                received.Add(message);
            }
        }

        private void SendToServer(ChatMessage message)
        {
            // Shared with ChatServer.SendTo - see ChatWire, which is where this sequence lives now.
            ChatWire.Send(_kernel, _server, _port, message);
        }
    }
}
