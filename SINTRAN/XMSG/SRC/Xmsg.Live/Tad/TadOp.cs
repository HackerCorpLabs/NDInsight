namespace NDInsight.Sintran.Xmsg.Live.Tad
{
    /// <summary>
    /// The TAD opcode byte constants used by the session state machine.
    /// </summary>
    /// <remarks>
    /// VERIFIED: opcode values are taken from <c>TAD/TAD-Message-Formats.md</c> section 2,
    /// which verifies them against the SINTRAN III K03/L07/M06 symbol tables. The mnemonic
    /// naming (for human display) is provided separately by
    /// <see cref="SubProtocol.TadOpcodes"/>.
    /// </remarks>
    public static class TadOp
    {
        /// <summary>
        /// BDAT — terminal data block (<c>0x01</c>).
        /// </summary>
        public const byte Bdat = 0x01;

        /// <summary>
        /// RFI — ready for input / flow-control credit (<c>0x02</c>).
        /// </summary>
        public const byte Rfi = 0x02;

        /// <summary>
        /// ECKM — echo strategy (<c>0x03</c>).
        /// </summary>
        public const byte Eckm = 0x03;

        /// <summary>
        /// BMMX — break strategy / max break (<c>0x04</c>).
        /// </summary>
        public const byte Bmmx = 0x04;

        /// <summary>
        /// DCON — disconnect indication (<c>0x09</c>).
        /// </summary>
        public const byte Dcon = 0x09;

        /// <summary>
        /// TMOD — terminal mode flags (<c>0x0C</c>).
        /// </summary>
        public const byte Tmod = 0x0C;

        /// <summary>
        /// TTYP — terminal type id (<c>0x0D</c>).
        /// </summary>
        public const byte Ttyp = 0x0D;

        /// <summary>
        /// DESC — define escape character (<c>0x0F</c>).
        /// </summary>
        public const byte Desc = 0x0F;

        /// <summary>
        /// RESE — reset connection request (<c>0x16</c>).
        /// </summary>
        public const byte Rese = 0x16;

        /// <summary>
        /// RECO — reset confirm (<c>0x17</c>).
        /// </summary>
        public const byte Reco = 0x17;

        /// <summary>
        /// OPSV — OS / protocol version handshake (<c>0x1F</c>).
        /// </summary>
        public const byte Opsv = 0x1F;
    }
}
