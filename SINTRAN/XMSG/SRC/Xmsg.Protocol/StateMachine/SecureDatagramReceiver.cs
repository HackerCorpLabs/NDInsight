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
        /// Re-seeds the per-direction ACK counter. A TAD session calls this when the connect
        /// arrives, with <c>connect-counter + 0x0A</c>, so the first ACK trailing byte matches the
        /// captured value and the subsequent decrement reproduces the captured sequence (see the
        /// remarks on <see cref="ReceiveDataFrame"/> for the capture provenance).
        /// </summary>
        /// <param name="value">The value the next ACK will carry as its trailing byte.</param>
        public void SeedCounter(byte value)
        {
            _counter = value;
        }

        /// <summary>
        /// Accepts a data frame, delivers it, and builds the delivery ACK for it.
        /// </summary>
        /// <param name="dataFrame">
        /// The received data frame (subtype <see cref="SintranPacketSubtype.Data"/>).
        /// </param>
        /// <param name="ackChannel">
        /// Optional Protocol-ID (header offset 12) to place the ACK on. When <c>null</c> (the
        /// default) the ACK echoes the data frame's own Protocol-ID — correct for the simple
        /// reachability/list-route paths. For a TAD <c>connect-to</c> session the ACK does NOT
        /// ride the data channel: it rides a per-session constant channel equal to
        /// <em>connect-channel + 4</em>. This is VERIFIED from both connect captures (all one
        /// directional / asker side): a <c>D9</c>-rooted session ACKs on <c>DD</c>
        /// (<c>new-conn-to-102-from-100.pcapng</c>), a <c>DA</c>-rooted session ACKs on <c>DE</c>
        /// (<c>conn-to-102-from103-via100.pcapng</c>), and the ACK channel stays constant even
        /// when the acknowledged data frame itself was on <c>DC</c>. Echoing the data channel
        /// (the old +0 behaviour) is the malformed ACK that crashed 100 (XXPER).
        /// </param>
        /// <returns>
        /// The subtype-<c>0x03</c> ACK frame echoing the data frame's Flags 1.
        /// </returns>
        /// <remarks>
        /// The trailing byte is this receiver's own per-direction counter, which DECREMENTS by 1
        /// per ACK. VALIDATED across all captures: the ACK trailing byte runs as a smooth
        /// decrementing counter (e.g. 0x17, 0x16, 0x15 …), it is NOT a per-frame function of the
        /// acknowledged counter. What IS tied to the data is the SEED: the first ACK trailing =
        /// first-acknowledged-counter + <c>0x0A</c> (8 of 13 captures where first-DATA and
        /// first-ACK pair cleanly, including both connect captures: 0x0D-&gt;0x17, 0xCE-&gt;0xD8).
        /// So a TAD session must <see cref="SeedCounter"/> the receiver to
        /// <c>connect-counter + 0x0A</c> before the first ACK; the decrement then reproduces the
        /// captured sequence. Sending <c>0x00</c> (the un-seeded default) is a malformed ACK that
        /// crashed 100 (XXPER).
        /// </remarks>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="dataFrame"/> or its header is null.
        /// </exception>
        public XmsgFrame ReceiveDataFrame(XmsgFrame dataFrame, SintranProtocolId? ackChannel = null)
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
            // ACK channel: the caller-supplied session ACK channel (connect+4 for TAD) when
            // given; otherwise echo the data frame's own channel (reachability/list-route).
            ack.Header.ProtocolId = ackChannel ?? dataHeader.ProtocolId;
            ack.TrailingBytes = new byte[] { _counter };

            // The per-direction counter decrements per ACK (section 6, VALIDATED as a smooth
            // decrementing sequence across all captures).
            _counter = (byte)(_counter - 1);
            return ack;
        }
    }
}
