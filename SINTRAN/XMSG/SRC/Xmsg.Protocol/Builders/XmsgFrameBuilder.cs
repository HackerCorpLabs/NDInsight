using System;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// Fluent builder for a full XMSG wire frame (SINTRAN header, XMSG sub-header,
    /// and optional XROUT body).
    /// </summary>
    /// <remarks>
    /// Assembles the layers described in XMSG-PROTOCOL.md sections 4-5 for a data
    /// message (subtype <see cref="SintranPacketSubtype.Data"/>). Use
    /// <see cref="ReachabilityRequest(ushort, ushort, byte)"/> and related helpers
    /// for the short control frames.
    /// </remarks>
    public sealed class XmsgFrameBuilder
    {
        private readonly SintranHeader _header;
        private readonly XmsgSubHeader _subHeader;
        private XroutMessage? _body;

        /// <summary>
        /// Initialises a builder for a normal data frame.
        /// </summary>
        public XmsgFrameBuilder()
        {
            _header = new SintranHeader();
            _header.Marker1 = SintranHeader.Marker1Value;
            _header.Marker2 = SintranHeader.Marker2Normal;
            _header.Subtype = SintranPacketSubtype.Data;
            _header.Flags2 = 0x0400; // data-frame class word observed in the corpus
            _header.ProtocolId = SintranProtocolId.Routing;

            _subHeader = new XmsgSubHeader();
        }

        /// <summary>
        /// Sets the destination and source node numbers in the SINTRAN header.
        /// </summary>
        /// <param name="destinationNode">
        /// The destination node number.
        /// </param>
        /// <param name="sourceNode">
        /// The source node number.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder Between(ushort destinationNode, ushort sourceNode)
        {
            _header.DestinationNode = destinationNode;
            _header.SourceNode = sourceNode;
            return this;
        }

        /// <summary>
        /// Sets the datagram sequence number (Flags 1) of the data frame.
        /// </summary>
        /// <param name="datagramSequence">
        /// The per-direction datagram sequence number.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder WithDatagramSequence(ushort datagramSequence)
        {
            _header.Flags1 = datagramSequence;
            return this;
        }

        /// <summary>
        /// Sets the sub-protocol selector (Protocol ID).
        /// </summary>
        /// <param name="protocolId">
        /// The sub-protocol carried by the frame.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder WithProtocol(SintranProtocolId protocolId)
        {
            _header.ProtocolId = protocolId;
            return this;
        }

        /// <summary>
        /// Sets the Flags 2 frame-class word of the SINTRAN header.
        /// </summary>
        /// <param name="flags2">
        /// The frame-class word.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder WithFlags2(ushort flags2)
        {
            _header.Flags2 = flags2;
            return this;
        }

        /// <summary>
        /// Sets the XMSG sub-header per-direction counter and frame flags.
        /// </summary>
        /// <param name="counter">
        /// The per-direction counter byte.
        /// </param>
        /// <param name="frameFlags">
        /// The frame-flags byte.
        /// </param>
        /// <param name="role">
        /// The role byte (low nibble 4 = asker, 0 = responder).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder WithSubHeaderControl(byte counter, byte frameFlags, byte role)
        {
            _subHeader.Counter = counter;
            _subHeader.FrameFlags = frameFlags;
            _subHeader.Role = role;
            return this;
        }

        /// <summary>
        /// Sets the XMSG endpoint addressing (XMDSY/XMDPT/XMSSY/XMSPT).
        /// </summary>
        /// <param name="destinationSystem">
        /// The destination system number (XMDSY).
        /// </param>
        /// <param name="destinationPort">
        /// The destination port (XMDPT).
        /// </param>
        /// <param name="sourceSystem">
        /// The source system number (XMSSY).
        /// </param>
        /// <param name="sourcePort">
        /// The source port (XMSPT).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder WithEndpoints(ushort destinationSystem, ushort destinationPort, ushort sourceSystem, ushort sourcePort)
        {
            _subHeader.DestinationSystem = destinationSystem;
            _subHeader.DestinationPort = destinationPort;
            _subHeader.SourceSystem = sourceSystem;
            _subHeader.SourcePort = sourcePort;
            return this;
        }

        /// <summary>
        /// Sets the XMCSM control/service dispatch word.
        /// </summary>
        /// <param name="controlService">
        /// The control/service word.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public XmsgFrameBuilder WithControlService(uint controlService)
        {
            _subHeader.ControlService = controlService;
            return this;
        }

        /// <summary>
        /// Attaches an XROUT message body and sets the sub-header user-data length.
        /// </summary>
        /// <param name="body">
        /// The XROUT message body.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="body"/> is null.
        /// </exception>
        public XmsgFrameBuilder WithBody(XroutMessage body)
        {
            if (body == null)
            {
                throw new ArgumentNullException(nameof(body));
            }

            _body = body;
            byte[] bytes = body.ToArray();
            // XMLEN is the low byte of the user-data length (XMSG-PROTOCOL.md section 5).
            _subHeader.UserDataLength = (byte)bytes.Length;
            return this;
        }

        /// <summary>
        /// Builds the data-frame <see cref="XmsgFrame"/>.
        /// </summary>
        /// <returns>
        /// The constructed frame.
        /// </returns>
        public XmsgFrame Build()
        {
            XmsgFrame frame = new XmsgFrame();
            frame.Header = _header;
            frame.SubHeader = _subHeader;
            frame.Body = _body;
            return frame;
        }

        /// <summary>
        /// Builds a reachability-request control frame (subtype <c>0x19</c>).
        /// </summary>
        /// <param name="destinationNode">
        /// The node whose reachability is being probed.
        /// </param>
        /// <param name="sourceNode">
        /// The probing node.
        /// </param>
        /// <param name="counterByte">
        /// The trailing per-direction counter byte.
        /// </param>
        /// <returns>
        /// The constructed reachability-request frame.
        /// </returns>
        public static XmsgFrame ReachabilityRequest(ushort destinationNode, ushort sourceNode, byte counterByte)
        {
            return BuildShort(SintranPacketSubtype.ReachabilityRequest, destinationNode, sourceNode, 0xFFFF, 0x0001, counterByte);
        }

        /// <summary>
        /// Builds a reachability-reply control frame (subtype <c>0x13</c>).
        /// </summary>
        /// <param name="destinationNode">
        /// The node the reply is sent to (the original requester).
        /// </param>
        /// <param name="sourceNode">
        /// The replying node.
        /// </param>
        /// <param name="counterByte">
        /// The trailing per-direction counter byte.
        /// </param>
        /// <returns>
        /// The constructed reachability-reply frame.
        /// </returns>
        public static XmsgFrame ReachabilityReply(ushort destinationNode, ushort sourceNode, byte counterByte)
        {
            return BuildShort(SintranPacketSubtype.ReachabilityReply, destinationNode, sourceNode, 0xFFFF, 0x0001, counterByte);
        }

        private static XmsgFrame BuildShort(SintranPacketSubtype subtype, ushort destinationNode, ushort sourceNode, ushort flags1, ushort flags2, byte counterByte)
        {
            XmsgFrame frame = new XmsgFrame();
            frame.Header.Subtype = subtype;
            frame.Header.DestinationNode = destinationNode;
            frame.Header.SourceNode = sourceNode;
            frame.Header.Flags1 = flags1;
            frame.Header.Flags2 = flags2;
            frame.Header.ProtocolId = SintranProtocolId.Routing;
            frame.TrailingBytes = new byte[] { counterByte };
            return frame;
        }
    }
}
