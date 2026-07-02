using System;

namespace NDInsight.Sintran.Xmsg.Hdlc
{
    /// <summary>
    /// LAPB frame classification by control-field format.
    /// </summary>
    public enum LapbFrameKind
    {
        /// <summary>
        /// Information transfer frame (control bit 0 = 0); carries an info field.
        /// </summary>
        Information,

        /// <summary>
        /// Supervisory frame (control bits 1..0 = <c>0b01</c>): RR / RNR / REJ.
        /// </summary>
        Supervisory,

        /// <summary>
        /// Unnumbered frame (control bits 1..0 = <c>0b11</c>): SABM / UA / DISC / DM / FRMR.
        /// </summary>
        Unnumbered,
    }

    /// <summary>
    /// A single FCS-valid LAPB frame de-framed from a capture: its address, control
    /// classification, and — for information frames — the information field.
    /// </summary>
    /// <remarks>
    /// The information field is the bytes between the control byte and the two-byte FCS.
    /// For a SINTRAN frame it begins with the 13-byte SINTRAN header and can be fed
    /// directly to the XMSG frame decoder.
    /// </remarks>
    public sealed class LapbFrame
    {
        private readonly byte[] _info;

        /// <summary>
        /// Initialises a LAPB frame from its unstuffed, FCS-valid bytes.
        /// </summary>
        /// <param name="key">
        /// The TCP flow the frame was reassembled from.
        /// </param>
        /// <param name="frameBytes">
        /// The full unstuffed frame: <c>addr + ctrl + info + fcs_lo + fcs_hi</c>.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="frameBytes"/> is null.
        /// </exception>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="frameBytes"/> is shorter than four bytes.
        /// </exception>
        public LapbFrame(StreamKey key, byte[] frameBytes)
        {
            if (frameBytes == null)
            {
                throw new ArgumentNullException(nameof(frameBytes));
            }

            if (frameBytes.Length < 4)
            {
                throw new ArgumentException("A LAPB frame needs at least address, control and a 2-byte FCS.", nameof(frameBytes));
            }

            Key = key;
            Address = frameBytes[0];
            Control = frameBytes[1];

            // Info field excludes addr(1), ctrl(1) and the trailing 2-byte FCS.
            int infoLength = frameBytes.Length - 4;
            _info = new byte[infoLength];
            Array.Copy(frameBytes, 2, _info, 0, infoLength);

            if ((Control & 0x01) == 0)
            {
                Kind = LapbFrameKind.Information;
                SendSequence = (Control >> 1) & 0x07;
                ReceiveSequence = (Control >> 5) & 0x07;
            }
            else if ((Control & 0x03) == 0x01)
            {
                Kind = LapbFrameKind.Supervisory;
                SendSequence = -1;
                ReceiveSequence = (Control >> 5) & 0x07;
            }
            else
            {
                Kind = LapbFrameKind.Unnumbered;
                SendSequence = -1;
                ReceiveSequence = -1;
            }

            PollFinal = (Control & 0x10) != 0;
        }

        /// <summary>
        /// Gets the TCP flow this frame was reassembled from.
        /// </summary>
        public StreamKey Key { get; }

        /// <summary>
        /// Gets the LAPB address byte.
        /// </summary>
        public byte Address { get; }

        /// <summary>
        /// Gets the LAPB control byte.
        /// </summary>
        public byte Control { get; }

        /// <summary>
        /// Gets the frame classification derived from the control byte.
        /// </summary>
        public LapbFrameKind Kind { get; }

        /// <summary>
        /// Gets the send sequence number N(S) for an information frame, or -1 otherwise.
        /// </summary>
        public int SendSequence { get; }

        /// <summary>
        /// Gets the receive sequence number N(R) for information/supervisory frames, or
        /// -1 for unnumbered frames.
        /// </summary>
        public int ReceiveSequence { get; }

        /// <summary>
        /// Gets a value indicating whether the poll/final bit (control bit 4) is set.
        /// </summary>
        public bool PollFinal { get; }

        /// <summary>
        /// Gets the information field bytes (between the control byte and the FCS).
        /// </summary>
        /// <remarks>
        /// Empty for supervisory frames and for unnumbered frames without an info field.
        /// </remarks>
        public ReadOnlyMemory<byte> Info
        {
            get { return _info; }
        }

        /// <summary>
        /// Gets a value indicating whether the information field is a SINTRAN frame,
        /// that is it starts with marker <c>0x21</c> followed by <c>0x13</c> or <c>0x12</c>.
        /// </summary>
        public bool IsSintranInfo
        {
            get
            {
                return _info.Length >= 2
                    && _info[0] == 0x21
                    && (_info[1] == 0x13 || _info[1] == 0x12);
            }
        }
    }
}
