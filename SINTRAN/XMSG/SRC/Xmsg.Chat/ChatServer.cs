using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Api;

namespace NDInsight.Sintran.Xmsg.Chat
{
    /// <summary>
    /// A named chat service: claims a name with a number of seats, admits users who write to that
    /// name, and relays what they say to everybody in the room.
    /// </summary>
    /// <remarks>
    /// <para><b>Why a connection port and not a plain name</b></para>
    /// A chat room has a size, and a connection port is the mechanism SINTRAN already has for that:
    /// XROUT keeps a free-connection count, forwards a join only while it is above zero, and
    /// decrements it each time. So "the room is full" is enforced by the system before the join
    /// ever reaches this class. That behaviour was captured from a running machine - the real file
    /// access server builds its 30 seats exactly this way.
    /// <para><b>Why the seat count is built one at a time</b></para>
    /// Servers on the real machine register with ZERO seats and then add them individually. This
    /// class does the same through <see cref="XroutDirectory"/>, so the count it presents to XROUT
    /// is built the way the observed servers build theirs rather than declared up front.
    /// <para><b>What travels through XROUT, and what does not</b></para>
    /// Only the join. XROUT forwards it and steps aside; the server learns the newcomer's address
    /// from the arrived message and answers it directly. Everything after that - every line said,
    /// every notice - goes port to port with no name lookup at all.
    /// </remarks>
    public sealed class ChatServer
    {
        private readonly XmsgKernel _kernel;
        private readonly XroutDirectory _directory;
        private readonly List<Member> _members;
        private readonly byte[] _scratch;

        private XmsgPortNumber _port;
        private XmsgMagicNumber _magic;
        private string _name;
        private string _greeting;
        private int _seats;

        /// <summary>
        /// Initialises a chat server on a kernel and a name table.
        /// </summary>
        /// <param name="kernel">
        /// The kernel the server's port belongs to.
        /// </param>
        /// <param name="directory">
        /// The XROUT stand-in to register the name with.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when either argument is null.
        /// </exception>
        public ChatServer(XmsgKernel kernel, XroutDirectory directory)
        {
            if (kernel == null)
            {
                throw new ArgumentNullException(nameof(kernel));
            }

            if (directory == null)
            {
                throw new ArgumentNullException(nameof(directory));
            }

            _kernel = kernel;
            _directory = directory;
            _members = new List<Member>();
            _scratch = new byte[ChatMessageBufferSize];
            _name = string.Empty;
            _greeting = string.Empty;
        }

        /// <summary>
        /// The receive buffer size. Generous for a line of chat, and the reason a nickname and text
        /// are length-prefixed rather than unbounded.
        /// </summary>
        private const int ChatMessageBufferSize = 1024;

        /// <summary>
        /// Gets the registered name, or an empty string before <see cref="Open"/>.
        /// </summary>
        public string Name
        {
            get { return _name; }
        }

        /// <summary>
        /// Gets the number of users currently in the room.
        /// </summary>
        public int MemberCount
        {
            get { return _members.Count; }
        }

        /// <summary>
        /// Gets the nicknames currently in the room, in the order they joined.
        /// </summary>
        /// <returns>
        /// A fresh array; the caller may keep it.
        /// </returns>
        public string[] Members()
        {
            string[] names = new string[_members.Count];
            for (int i = 0; i < _members.Count; i++)
            {
                names[i] = _members[i].Nickname;
            }

            return names;
        }

        /// <summary>
        /// Opens the room: takes a port, claims the name, and adds the seats.
        /// </summary>
        /// <param name="name">
        /// The name users will join, conventionally starting with an asterisk.
        /// </param>
        /// <param name="seats">
        /// How many users may be in the room at once.
        /// </param>
        /// <param name="greeting">
        /// The line sent back to each user who is admitted.
        /// </param>
        /// <returns>
        /// <see cref="XroutError.XRSOK"/>, or the reason the name could not be claimed.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="name"/> is null.
        /// </exception>
        /// <exception cref="ArgumentOutOfRangeException">
        /// Thrown when <paramref name="seats"/> is not positive.
        /// </exception>
        public XroutError Open(string name, int seats, string greeting)
        {
            if (name == null)
            {
                throw new ArgumentNullException(nameof(name));
            }

            if (seats <= 0)
            {
                throw new ArgumentOutOfRangeException(nameof(seats), "A room needs at least one seat.");
            }

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

            // Claim the name with NO seats, then add them one at a time - the sequence the real
            // servers use. Registering with the total up front would work here, but it would not be
            // what the machine does, and this class is meant to be readable next to a real capture.
            XroutError claimed = _directory.RegisterConnectionPort(name, _kernel, _magic, 0, true);
            if (claimed != XroutError.XRSOK)
            {
                _kernel.ClosePort(_port);
                return claimed;
            }

            _name = name;
            _greeting = greeting ?? string.Empty;

            for (int i = 0; i < seats; i++)
            {
                XroutError added = _directory.AdjustFreeConnections(_name, 1);
                if (added != XroutError.XRSOK)
                {
                    return added;
                }
            }

            _seats = seats;
            return XroutError.XRSOK;
        }

        /// <summary>
        /// Handles everything waiting on the server's port.
        /// </summary>
        /// <returns>
        /// The number of messages handled.
        /// </returns>
        /// <remarks>
        /// Call this from whatever loop the host program already has. It never blocks: the kernel
        /// has no scheduler to suspend on, so an empty port simply returns zero.
        /// </remarks>
        public int Poll()
        {
            int handled = 0;

            while (true)
            {
                XmsgReceiveResult arrived = _kernel.Receive(_port, XmsgWaitOptions.None);
                if (!arrived.Received)
                {
                    return handled;
                }

                XmsgMagicNumber sender = _kernel.GetMessageStatus(arrived.Message).Sender;

                int read;
                _kernel.Read(arrived.Message, _scratch, 0, out read);
                _kernel.ReleaseBuffer(arrived.Message);

                ChatMessage message;
                if (ChatMessage.TryDecode(new ReadOnlySpan<byte>(_scratch, 0, read), out message))
                {
                    Handle(sender, message);
                }

                // A message we cannot decode is dropped on purpose - one confused client must not
                // stop the room. It still counts as handled so a caller draining the port
                // terminates.
                handled++;
            }
        }

        /// <summary>
        /// Sends a line to the room as the server itself, with no nickname attached.
        /// </summary>
        /// <param name="text">
        /// The line to send.
        /// </param>
        public void Announce(string text)
        {
            Broadcast(new ChatMessage(ChatMessageKind.Said, string.Empty, text), -1);
        }

        /// <summary>
        /// Closes the room: tells everybody, releases the name and the port.
        /// </summary>
        public void Close()
        {
            for (int i = 0; i < _members.Count; i++)
            {
                SendTo(_members[i].Magic, new ChatMessage(ChatMessageKind.Left, _members[i].Nickname, "room closed"));
            }

            _members.Clear();

            if (_name.Length > 0)
            {
                _directory.ClearName(_name);
                _name = string.Empty;
            }

            _kernel.ClosePort(_port);
        }

        private void Handle(XmsgMagicNumber sender, ChatMessage message)
        {
            switch (message.Kind)
            {
                case ChatMessageKind.Join:
                    HandleJoin(sender, message);
                    break;

                case ChatMessageKind.Say:
                    HandleSay(sender, message);
                    break;

                case ChatMessageKind.Leave:
                    HandleLeave(sender);
                    break;

                default:
                    // Server-to-client kinds have no meaning arriving here. Ignore rather than
                    // answer, so a confused or hostile client learns nothing from probing.
                    break;
            }
        }

        private void HandleJoin(XmsgMagicNumber sender, ChatMessage message)
        {
            if (message.Nickname.Length == 0)
            {
                SendTo(sender, new ChatMessage(ChatMessageKind.Reject, string.Empty, "a nickname is required"));
                ReleaseSeat();
                return;
            }

            if (IndexOfNickname(message.Nickname) >= 0)
            {
                SendTo(sender, new ChatMessage(ChatMessageKind.Reject, message.Nickname, "that nickname is taken"));
                ReleaseSeat();
                return;
            }

            if (IndexOfMagic(sender) >= 0)
            {
                // Already in the room on this port: a duplicate join costs a seat we must return.
                SendTo(sender, new ChatMessage(ChatMessageKind.Reject, message.Nickname, "already joined"));
                ReleaseSeat();
                return;
            }

            _members.Add(new Member(message.Nickname, sender));

            // The welcome goes straight to the address the join arrived from. This is the reply
            // that reveals the server's own address, and it is why nothing after it needs XROUT.
            SendTo(sender, new ChatMessage(ChatMessageKind.Welcome, message.Nickname, _greeting));

            Broadcast(new ChatMessage(ChatMessageKind.Joined, message.Nickname, string.Empty), _members.Count - 1);
        }

        private void HandleSay(XmsgMagicNumber sender, ChatMessage message)
        {
            int index = IndexOfMagic(sender);
            if (index < 0)
            {
                // Not in the room. Silence is the right answer: replying would confirm the port is
                // a chat server to something that never joined.
                return;
            }

            Broadcast(new ChatMessage(ChatMessageKind.Said, _members[index].Nickname, message.Text), -1);
        }

        private void HandleLeave(XmsgMagicNumber sender)
        {
            int index = IndexOfMagic(sender);
            if (index < 0)
            {
                return;
            }

            string nickname = _members[index].Nickname;
            _members.RemoveAt(index);

            // Give the seat back. Forget this and the room fills up permanently: XROUT stops
            // forwarding joins long before anybody notices the members left.
            ReleaseSeat();

            Broadcast(new ChatMessage(ChatMessageKind.Left, nickname, string.Empty), -1);
        }

        private void Broadcast(ChatMessage message, int skipIndex)
        {
            for (int i = 0; i < _members.Count; i++)
            {
                if (i == skipIndex)
                {
                    continue;
                }

                SendTo(_members[i].Magic, message);
            }
        }

        private void SendTo(XmsgMagicNumber destination, ChatMessage message)
        {
            int size = message.ByteCount;

            XmsgMessageIdentifier buffer;
            XmsgStatus reserved = _kernel.ReserveBuffer(size, XmsgBufferOptions.None, out buffer);
            if (reserved.IsError)
            {
                return;
            }

            byte[] bytes = new byte[size];
            message.Encode(bytes);

            int written;
            _kernel.Write(buffer, bytes, 0, false, out written);
            _kernel.Send(destination, _port, XmsgSendFlags.None);
        }

        private void ReleaseSeat()
        {
            if (_name.Length > 0)
            {
                _directory.AdjustFreeConnections(_name, 1);
            }
        }

        private int IndexOfNickname(string nickname)
        {
            for (int i = 0; i < _members.Count; i++)
            {
                if (string.Equals(_members[i].Nickname, nickname, StringComparison.OrdinalIgnoreCase))
                {
                    return i;
                }
            }

            return -1;
        }

        private int IndexOfMagic(XmsgMagicNumber magic)
        {
            for (int i = 0; i < _members.Count; i++)
            {
                if (_members[i].Magic.Equals(magic))
                {
                    return i;
                }
            }

            return -1;
        }

        private readonly struct Member
        {
            internal Member(string nickname, XmsgMagicNumber magic)
            {
                Nickname = nickname;
                Magic = magic;
            }

            internal string Nickname { get; }

            internal XmsgMagicNumber Magic { get; }
        }
    }
}
