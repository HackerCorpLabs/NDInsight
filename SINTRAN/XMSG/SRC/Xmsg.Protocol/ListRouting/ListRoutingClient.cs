using System;

using NDInsight.Sintran.Xmsg;

namespace NDInsight.Sintran.Xmsg.ListRouting
{
    /// <summary>
    /// Client side of the stateless XROUT <c>XSGSY</c> list-routing service: builds a
    /// "get routing info for system N" request and decodes the reply.
    /// </summary>
    /// <remarks>
    /// The request is a subtype-<c>0x0E</c> data frame carrying a single parameter
    /// block (the system number being queried). See XMSG-PROTOCOL.md section 9.1 and
    /// XMSG-API.md section 4.3.
    /// </remarks>
    public sealed class ListRoutingClient
    {
        /// <summary>
        /// XMCSM control/service word identifying an XSGSY request.
        /// </summary>
        /// <remarks>
        /// VERIFIED (XMSG-PROTOCOL.md section 9.1): the low byte <c>0x4B</c> (75) is the
        /// XROUT service code for <c>XSGSY</c> "get routing info".
        /// </remarks>
        public const uint XmcsmXsgsyRequest = 0x0100014Bu;

        /// <summary>
        /// XMCSM control/service word identifying an XSGSY reply.
        /// </summary>
        public const uint XmcsmXsgsyReply = 0x01000100u;

        // OBSERVED-from-capture default framing bytes for a freshly built request.
        // These are NOT documented requirements; they are the values seen on the wire
        // in the canonical capture (asker 102 -> 100). Every one is overridable via a
        // BuildRequest parameter so a caller can reproduce any capture byte-identically.
        private const byte DefaultRequestFrameFlags = 0x86; // INFERRED: frame-flags byte observed on the request.
        private const byte DefaultRequestRole = 0x84;       // INFERRED: role low-nibble 4 = asker.
        private const ushort DefaultRequestFlags2 = 0x0100; // INFERRED: SINTRAN flags2 observed on the request.

        /// <summary>
        /// Builds the information field of an XSGSY request that queries routing info
        /// for a given system.
        /// </summary>
        /// <param name="querySystem">
        /// The system number to look up.
        /// </param>
        /// <param name="destinationNode">
        /// The SINTRAN destination node number.
        /// </param>
        /// <param name="sourceNode">
        /// The SINTRAN source (asker) node number.
        /// </param>
        /// <param name="destinationSystem">
        /// The XMDSY destination system number.
        /// </param>
        /// <param name="destinationPort">
        /// The XMDPT destination port.
        /// </param>
        /// <param name="sourceSystem">
        /// The XMSSY source (asker) system number.
        /// </param>
        /// <param name="sourcePort">
        /// The XMSPT source (asker) port.
        /// </param>
        /// <param name="flags1">
        /// The SINTRAN Flags 1 word (datagram sequence).
        /// </param>
        /// <param name="counter">
        /// The XMSG sub-header per-direction counter byte.
        /// </param>
        /// <param name="flags2">
        /// The SINTRAN Flags 2 frame-class word; defaults to the observed request value.
        /// </param>
        /// <param name="frameFlags">
        /// The XMSG sub-header frame-flags byte; defaults to the observed request value.
        /// </param>
        /// <param name="role">
        /// The XMSG sub-header role byte; defaults to the observed asker value.
        /// </param>
        /// <param name="protocolId">
        /// The SINTRAN Protocol ID; defaults to the observed request channel.
        /// </param>
        /// <param name="controlService">
        /// The XMCSM control/service word; defaults to <see cref="XmcsmXsgsyRequest"/>.
        /// </param>
        /// <returns>
        /// A new array holding the request information field.
        /// </returns>
        public byte[] BuildRequest(
            ushort querySystem,
            ushort destinationNode,
            ushort sourceNode,
            ushort destinationSystem,
            ushort destinationPort,
            ushort sourceSystem,
            ushort sourcePort,
            ushort flags1,
            byte counter,
            ushort flags2 = DefaultRequestFlags2,
            byte frameFlags = DefaultRequestFrameFlags,
            byte role = DefaultRequestRole,
            // INFERRED default: proto 0xDB (DB) is the channel the request was captured on; overridable.
            SintranProtocolId protocolId = SintranProtocolId.Db,
            uint controlService = XmcsmXsgsyRequest)
        {
            SintranHeader header = new SintranHeader();
            header.Marker1 = SintranHeader.Marker1Value;
            header.Marker2 = SintranHeader.Marker2Normal;
            header.PacketType = 0x00;
            header.Subtype = SintranPacketSubtype.Data;
            header.DestinationNode = destinationNode;
            header.SourceNode = sourceNode;
            header.Flags1 = flags1;
            header.Flags2 = flags2;
            header.ProtocolId = protocolId;

            XmsgSubHeader sub = new XmsgSubHeader();
            sub.Counter = counter;
            sub.FrameFlags = frameFlags;
            sub.Role = role;
            sub.DestinationSystem = destinationSystem;
            sub.DestinationPort = destinationPort;
            sub.SourceSystem = sourceSystem;
            sub.SourcePort = sourcePort;
            sub.ControlService = controlService;
            sub.Pad = 0x00;

            // VERIFIED (captures): a request carries exactly one parameter block,
            // param#1 = the system number being queried.
            Span<byte> trailer = stackalloc byte[XsgsyWire.ParamBlockSize];
            XsgsyWire.WriteParamBlock(trailer, 1, querySystem);

            return XsgsyWire.BuildInfoField(header, sub, trailer);
        }

        /// <summary>
        /// Reads the queried system number from a decoded XSGSY request frame.
        /// </summary>
        /// <param name="request">
        /// The request frame.
        /// </param>
        /// <returns>
        /// The system number carried in parameter 1.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="request"/> is null.
        /// </exception>
        /// <exception cref="ArgumentException">
        /// Thrown when the frame carries no parameter 1.
        /// </exception>
        public ushort ParseRequestQuery(XmsgFrame request)
        {
            if (request == null)
            {
                throw new ArgumentNullException(nameof(request));
            }

            byte[] trailer = XsgsyWire.ExtractTrailer(request);
            if (!XsgsyWire.TryReadParam(trailer, 1, out ushort system))
            {
                throw new ArgumentException("XSGSY request has no parameter 1 (system number).", nameof(request));
            }

            return system;
        }

        /// <summary>
        /// Decodes an XSGSY reply frame into a routing-table entry.
        /// </summary>
        /// <param name="response">
        /// The reply frame.
        /// </param>
        /// <returns>
        /// The decoded <see cref="RoutingTableEntry"/>; a system number of <c>0</c>
        /// with <see cref="XroutConnectionType.Unavailable"/> means no entry.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="response"/> is null.
        /// </exception>
        public RoutingTableEntry ParseResponse(XmsgFrame response)
        {
            if (response == null)
            {
                throw new ArgumentNullException(nameof(response));
            }

            byte[] trailer = XsgsyWire.ExtractTrailer(response);

            // VERIFIED param meanings (COSMOS Programmer Guide ND-60.164, XSGSY;
            // XMSG-PROTOCOL.md section 9.1):
            //   p1 = system number, p2 = connection-type enum, p3 = extra info,
            //   p4 = network info (low byte = #hops, high byte = #WANs).
            XsgsyWire.TryReadParam(trailer, 1, out ushort system);
            XsgsyWire.TryReadParam(trailer, 2, out ushort connectionType);
            XsgsyWire.TryReadParam(trailer, 3, out ushort extraInfo);
            XsgsyWire.TryReadParam(trailer, 4, out ushort networkInfo);

            byte hops = (byte)(networkInfo & 0xFF);
            byte wans = (byte)((networkInfo >> 8) & 0xFF);
            return new RoutingTableEntry(system, (XroutConnectionType)connectionType, extraInfo, hops, wans);
        }
    }
}
