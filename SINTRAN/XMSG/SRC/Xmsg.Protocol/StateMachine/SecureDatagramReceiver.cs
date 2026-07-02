using System;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// Receive-side of the XFSEC secure-datagram model: accepts a data frame,
    /// delivers it, and produces the subtype-<c>0x03</c> delivery ACK that echoes
    /// the data frame's Flags 1 (datagram sequence).
    /// </summary>
    /// <remarks>
    /// The ACK is sent in the opposite direction to the data frame it acknowledges,
    /// with Flags 2 = <c>0x0001</c> and a single trailing byte that is the receiver's
    /// own per-direction counter (which decrements per ACK). See XMSG-PROTOCOL.md
    /// section 6.
    /// </remarks>
    public sealed class SecureDatagramReceiver
    {
        private byte _counter;

        /// <summary>
        /// Raised when a data frame is delivered to the local application.
        /// </summary>
        /// <param name="datagramSequence">
        /// The datagram sequence (Flags 1) of the delivered frame.
        /// </param>
        public delegate void DatagramReceived(ushort datagramSequence);

        /// <summary>
        /// Occurs when a data frame is delivered.
        /// </summary>
        public event DatagramReceived? OnReceived;

        /// <summary>
        /// Initialises a new receiver.
        /// </summary>
        /// <param name="initialCounter">
        /// The starting value of the per-direction counter placed in each ACK.
        /// </param>
        public SecureDatagramReceiver(byte initialCounter)
        {
            _counter = initialCounter;
        }

        /// <summary>
        /// Gets the current per-direction counter value (the byte placed in the next ACK).
        /// </summary>
        public byte Counter
        {
            get { return _counter; }
        }

        /// <summary>
        /// Accepts a data frame, delivers it, and builds the delivery ACK for it.
        /// </summary>
        /// <param name="dataFrame">
        /// The received data frame (subtype <see cref="SintranPacketSubtype.Data"/>).
        /// </param>
        /// <returns>
        /// The subtype-<c>0x03</c> ACK frame echoing the data frame's Flags 1.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="dataFrame"/> or its header is null.
        /// </exception>
        public XmsgFrame ReceiveDataFrame(XmsgFrame dataFrame)
        {
            if (dataFrame == null)
            {
                throw new ArgumentNullException(nameof(dataFrame));
            }

            if (dataFrame.Header == null)
            {
                throw new ArgumentNullException(nameof(dataFrame), "Frame header is null.");
            }

            SintranHeader dataHeader = dataFrame.Header;

            // Deliver the message to the local application.
            OnReceived?.Invoke(dataHeader.Flags1);

            // Build the ACK: opposite direction, echo Flags 1, Flags 2 = 0x0001.
            XmsgFrame ack = new XmsgFrame();
            ack.Header.Marker1 = SintranHeader.Marker1Value;
            ack.Header.Marker2 = SintranHeader.Marker2Normal;
            ack.Header.Subtype = SintranPacketSubtype.Ack;
            ack.Header.DestinationNode = dataHeader.SourceNode;   // reply to the sender
            ack.Header.SourceNode = dataHeader.DestinationNode;
            ack.Header.Flags1 = dataHeader.Flags1;                // echo the datagram sequence
            ack.Header.Flags2 = 0x0001;
            ack.Header.ProtocolId = dataHeader.ProtocolId;
            ack.TrailingBytes = new byte[] { _counter };

            // The per-direction counter decrements per ACK (section 6).
            _counter = (byte)(_counter - 1);
            return ack;
        }
    }
}
