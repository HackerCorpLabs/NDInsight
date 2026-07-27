using System;

namespace NDInsight.Sintran.Xmsg.Api
{
    /// <summary>
    /// The XMSG user function set as seen by a task - the C# shape of the MON 200B interface.
    /// </summary>
    /// <remarks>
    /// <para><b>What this is</b></para>
    /// One method per user function of appendix A of the COSMOS Programmer Guide, in the manual's
    /// own order: manipulating ports (section 3.1), manipulating message buffers (section 3.2) and
    /// the miscellaneous functions (section 4). Register plumbing is replaced by named parameters
    /// and typed results, but nothing else is reinterpreted: every method returns the T-register
    /// status as an <see cref="XmsgStatus"/> and no method throws for a protocol-level failure.
    /// <para><b>What this is not</b></para>
    /// The privileged and system functions (XFPRV aside), the driver functions (XFSTD, XFDBK,
    /// XFCRD, XFWDF), and the absolute-memory functions (XFABR, XFABW, XFDIB, XFRIB, XFWIB) are
    /// deliberately absent: they exist to serve drivers and XROUT itself inside SINTRAN and have no
    /// meaning for a task on this side of the wire.
    /// <para><b>Currency</b></para>
    /// Most buffer functions accept <see cref="XmsgMessageIdentifier.Current"/>, which resolves to
    /// the port-current message for port-referencing calls and the task-current message otherwise.
    /// Implementations must reproduce that rule; see <see cref="XmsgMessageIdentifier"/>.
    /// </remarks>
    public interface IXmsgKernel
    {
        /// <summary>
        /// Opens a port and returns the identifier XMSG assigned to it (XFOPN).
        /// </summary>
        /// <param name="port">
        /// On return, the new port's identifier. The new port becomes the task's default port.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus OpenPort(out XmsgPortNumber port);

        /// <summary>
        /// Closes a port, or every port owned by the task (XFCLS).
        /// </summary>
        /// <param name="port">
        /// The port to close; <see cref="XmsgPortNumber.Default"/> closes the most recently opened
        /// port and <see cref="XmsgPortNumber.All"/> closes them all.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        /// <remarks>
        /// Closing a port releases its queued non-secure messages, marks its secure messages
        /// non-secure and returns them to their senders, and clears the port's name from XROUT's
        /// name table if it had one.
        /// </remarks>
        XmsgStatus ClosePort(XmsgPortNumber port);

        /// <summary>
        /// Reports what is waiting on one port (XFPST).
        /// </summary>
        /// <param name="port">
        /// The port to check; the default port when unspecified.
        /// </param>
        /// <param name="options">
        /// The wait, wake-up and high-priority options to apply.
        /// </param>
        /// <returns>
        /// The queue length plus, when a message waits, its type, identifier and sender hash.
        /// </returns>
        /// <remarks>
        /// The call also manipulates wake-up state: finding a message clears both the task's
        /// general wake-up bit and the port's wake-up bit; finding none clears them and then
        /// applies the requested options.
        /// </remarks>
        XmsgPortStatus GetPortStatus(XmsgPortNumber port, XmsgWaitOptions options);

        /// <summary>
        /// Scans all of the task's ports for a waiting message (XFGST).
        /// </summary>
        /// <param name="lastPort">
        /// The LAST port to be scanned. The search starts at the port after this one in the task's
        /// port list and wraps, which is what makes round-robin service possible: pass the port you
        /// just serviced.
        /// </param>
        /// <param name="options">
        /// The wait, wake-up and high-priority options to apply.
        /// </param>
        /// <param name="port">
        /// On return, the port a message is waiting on.
        /// </param>
        /// <returns>
        /// The completion status; not-terminated when no port has a message.
        /// </returns>
        XmsgStatus GetGeneralStatus(XmsgPortNumber lastPort, XmsgWaitOptions options, out XmsgPortNumber port);

        /// <summary>
        /// Releases every XMSG resource held by the task (XFDCT).
        /// </summary>
        /// <returns>
        /// The completion status.
        /// </returns>
        /// <remarks>
        /// Closes all the task's ports and frees its XMSG space. SINTRAN performs this
        /// automatically when a user task returns to the command processor and when any task logs
        /// out or terminates.
        /// </remarks>
        XmsgStatus Disconnect();

        /// <summary>
        /// Reserves a message buffer (XFGET).
        /// </summary>
        /// <param name="byteCount">
        /// The requested capacity in bytes. Zero reserves a descriptor only; such a buffer cannot
        /// be sent out of the system.
        /// </param>
        /// <param name="options">
        /// The wait option, and the exclusive option for buffers pre-allocated with XFALM.
        /// </param>
        /// <param name="message">
        /// On return, the identifier of the reserved buffer.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus ReserveBuffer(int byteCount, XmsgBufferOptions options, out XmsgMessageIdentifier message);

        /// <summary>
        /// Releases a message buffer back to the pool (XFREL).
        /// </summary>
        /// <param name="message">
        /// The buffer to release, or <see cref="XmsgMessageIdentifier.Current"/>.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus ReleaseBuffer(XmsgMessageIdentifier message);

        /// <summary>
        /// Copies user data into a message buffer (XFWRI).
        /// </summary>
        /// <param name="message">
        /// The target buffer, or <see cref="XmsgMessageIdentifier.Current"/>.
        /// </param>
        /// <param name="source">
        /// The user data to copy in.
        /// </param>
        /// <param name="displacement">
        /// The displacement in bytes, or -1 to append at the buffer's current displacement. Odd
        /// values are rounded up, leaving a zero byte in the gap.
        /// </param>
        /// <param name="resetLength">
        /// The XFRES option: reset the message length to zero before writing.
        /// </param>
        /// <param name="bytesWritten">
        /// On return, the number of bytes actually written.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus Write(
            XmsgMessageIdentifier message,
            ReadOnlySpan<byte> source,
            int displacement,
            bool resetLength,
            out int bytesWritten);

        /// <summary>
        /// Writes the six-byte user header of a message buffer (XFWHD).
        /// </summary>
        /// <param name="message">
        /// The target buffer, or <see cref="XmsgMessageIdentifier.Current"/>.
        /// </param>
        /// <param name="header">
        /// Exactly six bytes, corresponding to the A, D and X registers in that order.
        /// </param>
        /// <returns>
        /// The completion status; an error when the buffer is smaller than six bytes.
        /// </returns>
        XmsgStatus WriteHeader(XmsgMessageIdentifier message, ReadOnlySpan<byte> header);

        /// <summary>
        /// Copies user data out of a message buffer (XFREA).
        /// </summary>
        /// <param name="message">
        /// The source buffer, or <see cref="XmsgMessageIdentifier.Current"/>.
        /// </param>
        /// <param name="destination">
        /// The caller's buffer; its length is the requested byte count.
        /// </param>
        /// <param name="displacement">
        /// The displacement in bytes, or -1 to resume from the buffer's current displacement. Odd
        /// values are rounded up.
        /// </param>
        /// <param name="bytesRead">
        /// On return, the number of bytes actually read.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus Read(
            XmsgMessageIdentifier message,
            Span<byte> destination,
            int displacement,
            out int bytesRead);

        /// <summary>
        /// Reads the six-byte user header of a message buffer (XFRHD).
        /// </summary>
        /// <param name="message">
        /// The source buffer, or <see cref="XmsgMessageIdentifier.Current"/>. When it is not the
        /// current message, the named buffer becomes task-current.
        /// </param>
        /// <param name="header">
        /// A six-byte destination receiving the A, D and X register contents in that order.
        /// </param>
        /// <returns>
        /// The completion status; an error when the buffer is smaller than six bytes.
        /// </returns>
        XmsgStatus ReadHeader(XmsgMessageIdentifier message, Span<byte> header);

        /// <summary>
        /// Sends the current message buffer to a remote port (XFSND).
        /// </summary>
        /// <param name="destination">
        /// The magic number of the receiving port. Ignored when
        /// <see cref="XmsgSendFlags.Route"/> is set, because the message then goes to XROUT.
        /// </param>
        /// <param name="fromPort">
        /// The sending port; the default port when unspecified.
        /// </param>
        /// <param name="flags">
        /// The send options.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        /// <remarks>
        /// There is no message parameter: the port-current message is sent if one exists, otherwise
        /// the task-current message. Two option interactions are easy to get wrong and are spelled
        /// out in the manual:
        ///  - with Route set, HighPriority acts as RemoteRoute (the message goes to a remote XROUT
        ///    whose system number is taken from the A register).
        ///  - with Route clear, RemoteRoute acts as HighPriority.
        /// </remarks>
        XmsgStatus Send(XmsgMagicNumber destination, XmsgPortNumber fromPort, XmsgSendFlags flags);

        /// <summary>
        /// Writes two status bytes into a message and returns it to its sender (XFRTN).
        /// </summary>
        /// <param name="message">
        /// The buffer to return, or <see cref="XmsgMessageIdentifier.Current"/>.
        /// </param>
        /// <param name="statusBytes">
        /// The first two bytes of the message's user header, typically a reply status.
        /// </param>
        /// <param name="fromPort">
        /// The sending port; the default port when unspecified.
        /// </param>
        /// <param name="flags">
        /// The send options.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        /// <remarks>
        /// Identical to <see cref="Send"/> except that the destination is implicit - the port the
        /// message was last sent from - and the two data bytes are written for you.
        /// </remarks>
        XmsgStatus ReturnMessage(
            XmsgMessageIdentifier message,
            ushort statusBytes,
            XmsgPortNumber fromPort,
            XmsgSendFlags flags);

        /// <summary>
        /// Receives the next message queued on a port (XFRCV).
        /// </summary>
        /// <param name="port">
        /// The receiving port; the default port when unspecified.
        /// </param>
        /// <param name="options">
        /// The wait and wake-up options.
        /// </param>
        /// <returns>
        /// The message type, sender hash, identifier and length; or a not-terminated status when
        /// the queue is empty and no wait was requested.
        /// </returns>
        /// <remarks>
        /// A received message becomes task-current, and port-current as well when it was sent
        /// secure. When the wake-up option fires, the message is NOT received - repeat the call.
        /// </remarks>
        XmsgReceiveResult Receive(XmsgPortNumber port, XmsgWaitOptions options);

        /// <summary>
        /// Receives the next message and its first two user bytes (XFRRH).
        /// </summary>
        /// <param name="port">
        /// The receiving port; the default port when unspecified.
        /// </param>
        /// <param name="options">
        /// The wait and wake-up options.
        /// </param>
        /// <returns>
        /// The same shape as <see cref="Receive"/>, except that the extra value is the first two
        /// user bytes instead of the message length. Those bytes are undefined if the message is
        /// shorter than two bytes.
        /// </returns>
        XmsgReceiveResult ReceiveAndReadHeader(XmsgPortNumber port, XmsgWaitOptions options);

        /// <summary>
        /// Receives the next message and immediately reads it into a user buffer (XFRRE).
        /// </summary>
        /// <param name="port">
        /// The receiving port; the default port when unspecified.
        /// </param>
        /// <param name="destination">
        /// The caller's buffer; its length is the requested byte count. Reading starts at the first
        /// byte of the message and stops at the message length if that is shorter.
        /// </param>
        /// <param name="options">
        /// The wait and wake-up options.
        /// </param>
        /// <param name="bytesRead">
        /// On return, the number of bytes copied into <paramref name="destination"/>.
        /// </param>
        /// <returns>
        /// The same shape as <see cref="Receive"/>.
        /// </returns>
        XmsgReceiveResult ReceiveAndRead(
            XmsgPortNumber port,
            Span<byte> destination,
            XmsgWaitOptions options,
            out int bytesRead);

        /// <summary>
        /// Reports the state of a message buffer, including its sender (XFMST).
        /// </summary>
        /// <param name="message">
        /// The buffer to inspect, or <see cref="XmsgMessageIdentifier.Current"/>.
        /// </param>
        /// <returns>
        /// The buffer's status, including the magic number of the port it came from - which is how
        /// a server learns the client's address after an XROUT letter.
        /// </returns>
        XmsgMessageStatus GetMessageStatus(XmsgMessageIdentifier message);

        /// <summary>
        /// Makes a buffer the task-current message, and optionally port-current too (XFSCM).
        /// </summary>
        /// <param name="message">
        /// The buffer to make current.
        /// </param>
        /// <param name="port">
        /// The port for which the message should additionally become port-current. The default
        /// port is assumed when unspecified; pass a negative port number to set task currency only.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus SetCurrentMessage(XmsgMessageIdentifier message, XmsgPortNumber port);

        /// <summary>
        /// Performs no operation, used to check that XMSG is reachable (XFDUM).
        /// </summary>
        /// <returns>
        /// The completion status.
        /// </returns>
        XmsgStatus Dummy();

        /// <summary>
        /// Converts a magic number into the port and system it identifies (XFMP2P).
        /// </summary>
        /// <param name="magic">
        /// The magic number to decode.
        /// </param>
        /// <param name="port">
        /// On return, the port number.
        /// </param>
        /// <param name="system">
        /// On return, the system number.
        /// </param>
        /// <returns>
        /// The completion status, which also classifies the magic number: 3 for a system, 2 for a
        /// local port owned by a privileged task, 1 for a remote port or an unprivileged local one.
        /// </returns>
        /// <remarks>
        /// This function exists because the packing of a magic number is not part of the published
        /// interface. Always decode through it rather than by masking bits.
        /// </remarks>
        XmsgStatus ConvertMagicToPort(XmsgMagicNumber magic, out XmsgPortNumber port, out int system);

        /// <summary>
        /// Converts a local port number into its magic number (XFP2M).
        /// </summary>
        /// <param name="port">
        /// The local port to encode.
        /// </param>
        /// <param name="magic">
        /// On return, the magic number.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        /// <remarks>
        /// Only ports opened by tasks in the LOCAL system can be converted; any task may convert
        /// its own ports, and a privileged task may convert another local task's port.
        /// </remarks>
        XmsgStatus ConvertPortToMagic(XmsgPortNumber port, out XmsgMagicNumber magic);

        /// <summary>
        /// Makes the calling task privileged for XMSG (XFPRV).
        /// </summary>
        /// <param name="password">
        /// The version-code password (XPASW) the caller must present.
        /// </param>
        /// <returns>
        /// The completion status.
        /// </returns>
        /// <remarks>
        /// Only a driver, a direct task, a foreground program, or a background program logged in as
        /// user SYSTEM can succeed. Privilege is a prerequisite for the privileged functions and
        /// for several XROUT services.
        /// </remarks>
        XmsgStatus MakePrivileged(ushort password);
    }
}
