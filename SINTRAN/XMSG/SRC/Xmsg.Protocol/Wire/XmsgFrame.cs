using System;

using NDInsight.Sintran.Xmsg.SubProtocol;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// A complete XMSG wire frame: the SINTRAN header, an optional XMSG sub-header,
    /// and either an XROUT message body (data frames) or trailing control bytes
    /// (short/control frames).
    /// </summary>
    /// <remarks>
    /// This models the HDLC information field only; HDLC framing, byte-stuffing and
    /// the FCS are out of scope (see XMSG-PROTOCOL.md sections 2-3). A data frame
    /// (subtype <see cref="SintranPacketSubtype.Data"/>) carries a
    /// <see cref="XmsgSubHeader"/>; short frames (Ack / reachability) carry a single
    /// trailing counter/command byte in <see cref="TrailingBytes"/>.
    /// </remarks>
    public sealed class XmsgFrame
    {
        private static readonly byte[] EmptyTrailing = new byte[0];

        /// <summary>
        /// Initialises an empty frame with a default normal-frame header.
        /// </summary>
        public XmsgFrame()
        {
            Header = new SintranHeader();
            TrailingBytes = EmptyTrailing;
        }

        /// <summary>
        /// Gets or sets the SINTRAN header.
        /// </summary>
        public SintranHeader Header { get; set; }

        /// <summary>
        /// Gets or sets the XMSG sub-header, or <c>null</c> when the frame carries none.
        /// </summary>
        public XmsgSubHeader? SubHeader { get; set; }

        /// <summary>
        /// Gets or sets the XROUT message body, or <c>null</c> when the frame carries none.
        /// </summary>
        public XroutMessage? Body { get; set; }

        /// <summary>
        /// Gets or sets the trailing control bytes for short frames (for example the
        /// single counter/command byte of an Ack or reachability frame).
        /// </summary>
        public byte[] TrailingBytes { get; set; }

        /// <summary>
        /// Gets or sets an exact copy of the original information-field bytes this
        /// frame was decoded from, or <c>null</c> for a frame built from scratch.
        /// </summary>
        /// <remarks>
        /// When non-<c>null</c> this is authoritative: <see cref="ToArray"/> reproduces
        /// these bytes verbatim so that decode/re-encode is guaranteed byte-identical
        /// for ANY captured frame, including sub-protocols this model does not fully
        /// decode structurally (TAD, PAD, DB, and any un-modeled XROUT tail). The
        /// structured properties (<see cref="Header"/>, <see cref="SubHeader"/>,
        /// <see cref="Body"/>, <see cref="TrailingBytes"/>) remain populated for
        /// human-readable inspection. Set this to <c>null</c> (call
        /// <see cref="ClearRawBytes"/>) to force re-serialisation from the model.
        /// </remarks>
        public byte[]? RawBytes { get; set; }

        /// <summary>
        /// Gets or sets the decoded TAD message chain for a TAD frame (protocol
        /// <see cref="SintranProtocolId.Tad"/>), or <c>null</c> for other protocols.
        /// </summary>
        /// <remarks>
        /// TAD frames carry a chain of terminal messages directly after the SINTRAN
        /// header rather than an XMSG sub-header, so a TAD frame populates this property
        /// instead of <see cref="SubHeader"/> and <see cref="Body"/>. Purely informational;
        /// serialisation is driven by <see cref="RawBytes"/>.
        /// </remarks>
        public TadChain? Tad { get; set; }

        /// <summary>
        /// Gets or sets the decoded ROUTING short-command view for a routing control frame
        /// (protocol <see cref="SintranProtocolId.Routing"/> that is not a data letter), or
        /// <c>null</c> otherwise.
        /// </summary>
        /// <remarks>
        /// Full XSGSY routing-table replies arrive as XROUT letters and appear in
        /// <see cref="Body"/>; this property covers only the one-byte routing control
        /// commands. Purely informational; serialisation is driven by
        /// <see cref="RawBytes"/>.
        /// </remarks>
        public RoutingCommandInfo? Routing { get; set; }

        /// <summary>
        /// Decodes a full XMSG frame from an HDLC information-field span.
        /// </summary>
        /// <param name="frame">
        /// The information-field bytes starting at the SINTRAN header (Marker 1).
        /// </param>
        /// <returns>
        /// The decoded <see cref="XmsgFrame"/>.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="frame"/> is shorter than a SINTRAN header.
        /// </exception>
        public static XmsgFrame Parse(ReadOnlySpan<byte> frame)
        {
            XmsgFrame result = new XmsgFrame();

            // Retain an exact copy of the input so ToArray can reproduce it byte-for-byte
            // regardless of how far the structured decode below gets. This is what makes
            // decode/re-encode lossless for every captured sub-protocol variant.
            result.RawBytes = frame.ToArray();

            result.Header = SintranHeader.Parse(frame);

            ReadOnlySpan<byte> rest = frame.Slice(SintranHeader.Size);

            if (result.Header.Subtype == SintranPacketSubtype.Data && rest.Length >= XmsgSubHeader.Size)
            {
                // Every data (0x0E) frame carries the SAME XMSG sub-header, for ALL
                // Protocol IDs (0xD8..0xDE). Verified across all 13 captures: the
                // Protocol ID byte is a channel/tag, NOT a distinct frame layout. So there
                // is ONE data-frame decode path — sub-header then a content-dispatched
                // trailer — regardless of whether the channel is DC, TAD, PAD or ROUTING.
                result.SubHeader = XmsgSubHeader.Parse(rest.Slice(0, XmsgSubHeader.Size));
                ReadOnlySpan<byte> trailer = rest.Slice(XmsgSubHeader.Size);

                if (trailer.Length > 0)
                {
                    DecodeDataTrailer(result, trailer);
                }
            }
            else if (rest.Length > 0)
            {
                // Short/control frame (Ack, reachability request/reply) - keep the
                // trailing counter/command byte(s) verbatim.
                result.TrailingBytes = rest.ToArray();

                // A ROUTING (0xDE) short frame's trailing byte is a routing control
                // command; expose it as a typed view (informational, RawBytes remains
                // authoritative for serialisation).
                if (result.Header.ProtocolId == SintranProtocolId.Routing)
                {
                    result.Routing = RoutingCommandInfo.Parse(rest);
                }
            }

            return result;
        }

        /// <summary>
        /// Decodes the trailer of a data frame (the bytes after the XMSG sub-header) by
        /// its content, choosing between an XROUT letter, a TAD message chain, or raw bytes.
        /// </summary>
        /// <param name="result">
        /// The frame being populated; its <see cref="SubHeader"/> is already set.
        /// </param>
        /// <param name="trailer">
        /// The trailer bytes following the 19-byte XMSG sub-header.
        /// </param>
        /// <remarks>
        /// Dispatch is by the XMCSM control/service word (XMSG-PROTOCOL.md; the reference
        /// dissector's LI ROUTING / XSLET handling):
        ///  - XMCSM <c>0x0100014B</c> or <c>0x01000100</c> — LI ROUTING (XSGSY) reply, an
        ///    XROUT letter of 4-byte parameter blocks.
        ///  - XMCSM low byte <c>0x41</c> — XSLET (send letter), an XROUT letter.
        ///  - otherwise — a TAD message chain (terminal session data / control).
        /// Any part that cannot be decoded is retained verbatim (letter tail in
        /// <see cref="TrailingBytes"/>, TAD tail in <see cref="SubProtocol.TadChain.Remainder"/>),
        /// and <see cref="RawBytes"/> still guarantees a byte-identical re-serialisation.
        /// </remarks>
        private static void DecodeDataTrailer(XmsgFrame result, ReadOnlySpan<byte> trailer)
        {
            uint xmcsm = result.SubHeader != null ? result.SubHeader.ControlService : 0u;
            bool isLetter = xmcsm == 0x0100014Bu
                || xmcsm == 0x01000100u
                || (xmcsm & 0xFFu) == 0x41u;

            if (isLetter && trailer.Length >= XroutMessage.HeaderSize)
            {
                // The XROUT letter parser is strict (parameter-number range, declared
                // length); on any malformed letter fall back to keeping the tail verbatim.
                try
                {
                    result.Body = XroutMessage.Parse(trailer);
                }
                catch (Exception)
                {
                    result.Body = null;
                    result.TrailingBytes = trailer.ToArray();
                }

                return;
            }

            // Non-letter channels (TAD session, DC/PAD terminal data, unknown XMCSM):
            // decode as a TAD message chain. TadChain never throws and keeps any odd
            // trailing byte in its Remainder, so nothing is lost.
            result.Tad = TadChain.Parse(trailer);
        }

        /// <summary>
        /// Serialises this frame into a new array.
        /// </summary>
        /// <returns>
        /// A new array containing the serialised information field.
        /// </returns>
        public byte[] ToArray()
        {
            // When decoded from a capture, the retained raw bytes are authoritative so
            // that re-encoding is byte-identical even for un-modeled sub-protocol tails.
            if (RawBytes != null)
            {
                byte[] copy = new byte[RawBytes.Length];
                Array.Copy(RawBytes, copy, RawBytes.Length);
                return copy;
            }

            byte[] subBytes = EmptyTrailing;
            byte[] bodyBytes = EmptyTrailing;

            if (SubHeader != null)
            {
                subBytes = new byte[XmsgSubHeader.Size];
                SubHeader.Serialize(subBytes);
                if (Body != null)
                {
                    bodyBytes = Body.ToArray();
                }
                else
                {
                    bodyBytes = TrailingBytes;
                }
            }
            else
            {
                // No sub-header: any payload is the raw trailing control bytes.
                bodyBytes = TrailingBytes;
            }

            int total = SintranHeader.Size + subBytes.Length + bodyBytes.Length;
            byte[] buffer = new byte[total];
            Header.Serialize(buffer);

            int cursor = SintranHeader.Size;
            for (int i = 0; i < subBytes.Length; i++)
            {
                buffer[cursor++] = subBytes[i];
            }

            for (int i = 0; i < bodyBytes.Length; i++)
            {
                buffer[cursor++] = bodyBytes[i];
            }

            return buffer;
        }

        /// <summary>
        /// Discards the retained raw bytes so that <see cref="ToArray"/> re-serialises
        /// this frame from its structured properties instead of the captured copy.
        /// </summary>
        public void ClearRawBytes()
        {
            RawBytes = null;
        }
    }
}
