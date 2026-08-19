using System;
using System.Collections.Generic;
using System.Text;

using NDInsight.Sintran.Xmsg.Api;
using NDInsight.Sintran.Xmsg.Protocol;

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
        // THE RULES LIVE IN ChatRoom, not here. Who is in the room, which names are free and who
        // must be told what are identical for a port conversation and for somebody typing at a
        // SINTRAN terminal; only the plumbing differs. Written twice they would drift, and the
        // drift would be in the awkward cases.
        private readonly ChatRoom _room;

        // The magic number each member is reachable at, keyed by the same handle the room uses.
        // The room deliberately knows nothing about addresses.
        private readonly Dictionary<long, XmsgMagicNumber> _addresses;
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
            _room = new ChatRoom();
            _addresses = new Dictionary<long, XmsgMagicNumber>();
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
            get { return _room.Count; }
        }

        /// <summary>
        /// Gets the nicknames currently in the room, in the order they joined.
        /// </summary>
        /// <returns>
        /// A fresh array; the caller may keep it.
        /// </returns>
        public string[] Members()
        {
            string[] names = _room.CopyNicknames();

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

                // A ROUTED message is one XROUT forwarded, and forwarding it cost this port one of
                // its free seats - spent before the room saw a single byte of the body. So the seat
                // is owed back by the ARRIVAL, not by any particular kind of message, and that is
                // why the accounting lives here rather than in the handlers.
                bool spentASeat = arrived.MessageType == XmsgMessageType.XMROU;
                long id = Handle(sender);
                bool wasMember = _room.Contains(id);

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
                //
                // Note that the seat below is settled for it too. An undecodable letter is exactly
                // the case that used to leak: the body never reaches a handler, so no handler could
                // ever have given the seat back.
                if (spentASeat && !(!wasMember && _room.Contains(id)))
                {
                    // The letter did not seat anybody new, so the seat it spent is free again. The
                    // condition is deliberately "became a member", not "was accepted": a member who
                    // sends a second letter spends a second seat, and only one of them is theirs.
                    ReleaseSeat();
                }

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
            long[] leaving = _room.CopyMemberIds();
            for (int i = 0; i < leaving.Length; i++)
            {
                string who;
                _room.TryGetNickname(leaving[i], out who);
                SendTo(_addresses[leaving[i]], new ChatMessage(ChatMessageKind.Left, who, "room closed"));
            }

            for (int i = 0; i < leaving.Length; i++)
            {
                string ignored;
                _room.TryLeave(leaving[i], out ignored);
            }

            _addresses.Clear();

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

                case ChatMessageKind.Rename:
                    HandleRename(sender, message);
                    break;

                case ChatMessageKind.Leave:
                    HandleLeave(sender);
                    break;

                case ChatMessageKind.Who:
                    HandleWho(sender);
                    break;

                default:
                    // Server-to-client kinds have no meaning arriving here. Ignore rather than
                    // answer, so a confused or hostile client learns nothing from probing.
                    break;
            }
        }

        private void HandleJoin(XmsgMagicNumber sender, ChatMessage message)
        {
            long id = Handle(sender);

            string refusal;
            if (!_room.TryJoin(id, message.Nickname, out refusal))
            {
                // NO ReleaseSeat here. The seat came with the ARRIVAL, not with the join, and Poll
                // settles it there for every letter - including the ones that never reach a handler
                // at all, which is the case this used to miss.
                SendTo(sender, new ChatMessage(ChatMessageKind.Reject, message.Nickname, refusal));
                return;
            }

            _addresses[id] = sender;

            // The welcome goes straight to the address the join arrived from. This is the reply
            // that reveals the server's own address, and it is why nothing after it needs XROUT.
            SendTo(sender, new ChatMessage(ChatMessageKind.Welcome, message.Nickname, _greeting));

            Broadcast(new ChatMessage(ChatMessageKind.Joined, message.Nickname, string.Empty), id);
        }

        /// <summary>
        /// Answers a member asking who else is in the room.
        /// </summary>
        /// <param name="sender">
        /// The member's address.
        /// </param>
        /// <remarks>
        /// <para><b>Members only, and silent to anybody else</b></para>
        /// A stranger gets nothing back, for the same reason <see cref="HandleSay"/> stays silent:
        /// answering would confirm to something that never joined that this port is a chat room, and
        /// would hand it the membership list as well.
        /// <para><b>The answer is one line, not one message per member</b></para>
        /// The names go in the text separated by single spaces. A message each would multiply the
        /// scarcest thing XMSG has - the ten data transmit blocks measured in
        /// <c>DOC/BRINGUP-ORDER-AND-TRAPS-2026-08-18.md</c> - by the size of the room.
        /// </remarks>
        private void HandleWho(XmsgMagicNumber sender)
        {
            string asker;
            if (!_room.TryGetNickname(Handle(sender), out asker))
            {
                return;
            }

            string[] names = _room.CopyNicknames();

            // Built by hand rather than with string.Join so the separator rule is visible: single
            // space between names, nothing before the first and nothing after the last. The PLANC
            // client splits on exactly that.
            StringBuilder list = new StringBuilder();
            for (int i = 0; i < names.Length; i++)
            {
                if (list.Length > 0) { list.Append(' '); }
                list.Append(names[i]);
            }

            SendTo(sender, new ChatMessage(ChatMessageKind.Who, string.Empty, list.ToString()));
        }

        private void HandleSay(XmsgMagicNumber sender, ChatMessage message)
        {
            string speaker;
            if (!_room.TryGetNickname(Handle(sender), out speaker))
            {
                // Not in the room. Silence is the right answer: replying would confirm the port is
                // a chat server to something that never joined.
                return;
            }

            Broadcast(new ChatMessage(ChatMessageKind.Said, speaker, message.Text), NoSkip);
        }

        /// <summary>
        /// Answers a member asking to be known by a different name.
        /// </summary>
        /// <param name="sender">
        /// The member's address.
        /// </param>
        /// <param name="message">
        /// The request, carrying the wanted name.
        /// </param>
        /// <remarks>
        /// <para>
        /// A rename can fail for exactly the reasons a join can, so it is answered the same way -
        /// with a <see cref="ChatMessageKind.Reject"/> naming the reason - rather than ignored.
        /// </para>
        /// <para>
        /// NO SEAT IS INVOLVED here, and no handler in this class touches one any more: seats are
        /// settled in <see cref="Poll"/>, against the arrival that actually spent one. Releasing a
        /// seat here would hand back one the member is still sitting in, the room would then admit
        /// one more person than it has room for, and the fault would show up much later as XROUT
        /// refusing joins for no visible reason.
        /// </para>
        /// </remarks>
        private void HandleRename(XmsgMagicNumber sender, ChatMessage message)
        {
            long id = Handle(sender);

            // A STRANGER GETS SILENCE, exactly as in HandleSay, and the check has to happen HERE
            // rather than being left to the room: TryRename reports "you are not in the room" as a
            // refusal like any other, so falling through would answer a Reject to something that
            // never joined - confirming the port is a chat server to anybody who probes it.
            //
            // This is also what CHATSV.PLNC does, which is the reason it is written out. That
            // program tests membership once, before it looks at the message kind at all, so every
            // kind gets the same silence. The two implementations share no code, so a rule that
            // lives only in the C# is a rule the PLANC will drift away from.
            string existing;
            if (!_room.TryGetNickname(id, out existing))
            {
                return;
            }

            string previous;
            string refusal;
            if (!_room.TryRename(id, message.Nickname, out previous, out refusal))
            {
                if (refusal.Length == 0)
                {
                    // Asking for the name you already have: nothing changed, and it is not an
                    // error either, so there is nothing to say.
                    return;
                }

                // NO ReleaseSeat here - see the remarks.
                SendTo(sender, new ChatMessage(ChatMessageKind.Reject, message.Nickname, refusal));
                return;
            }

            // Everybody hears it, INCLUDING the member who asked - that is their confirmation, and
            // it means one message kind covers both jobs.
            Broadcast(new ChatMessage(ChatMessageKind.Renamed, message.Nickname, previous), NoSkip);
        }

        private void HandleLeave(XmsgMagicNumber sender)
        {
            long id = Handle(sender);

            string nickname;
            if (!_room.TryLeave(id, out nickname))
            {
                return;
            }

            _addresses.Remove(id);

            // Give the seat back. Forget this and the room fills up permanently: XROUT stops
            // forwarding joins long before anybody notices the members left.
            ReleaseSeat();

            Broadcast(new ChatMessage(ChatMessageKind.Left, nickname, string.Empty), NoSkip);
        }

        private void Broadcast(ChatMessage message, long skipId)
        {
            long[] ids = _room.CopyMemberIds();
            for (int i = 0; i < ids.Length; i++)
            {
                if (ids[i] == skipId)
                {
                    continue;
                }

                XmsgMagicNumber address;
                if (_addresses.TryGetValue(ids[i], out address))
                {
                    SendTo(address, message);
                }
            }
        }

        private void SendTo(XmsgMagicNumber destination, ChatMessage message)
        {
            // Shared with ChatClient.SendToServer - see ChatWire, which is where this sequence
            // lives now.
            ChatWire.Send(_kernel, destination, _port, message);
        }

        private void ReleaseSeat()
        {
            if (_name.Length > 0)
            {
                _directory.AdjustFreeConnections(_name, 1);
            }
        }

        /// <summary>
        /// The room's handle for a client, derived from the address it writes from.
        /// </summary>
        /// <param name="magic">
        /// The client's magic number.
        /// </param>
        /// <returns>
        /// A stable handle for that client.
        /// </returns>
        /// <remarks>
        /// The room tells members apart by a number and does not care what it means. Here it is
        /// the magic number, which identifies a (system, port) endpoint - the same thing that
        /// decided membership before the rules were pulled out.
        /// </remarks>
        private static long Handle(XmsgMagicNumber magic)
        {
            return magic.Value;
        }

        /// <summary>
        /// Passed to <c>Broadcast</c> when everybody should hear it, including the member who
        /// caused it.
        /// </summary>
        /// <remarks>
        /// A real handle is never negative, so this cannot collide with one.
        /// </remarks>
        private const long NoSkip = -1;

    }
}
