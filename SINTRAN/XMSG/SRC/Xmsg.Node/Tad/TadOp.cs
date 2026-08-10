namespace NDInsight.Sintran.Xmsg.Node.Tad
{
    /// <summary>
    /// The TAD opcode byte, identifying what a TAD message carries.
    /// </summary>
    /// <remarks>
    /// <para>
    /// VERIFIED: opcode values are taken from <c>TAD/TAD-Message-Formats.md</c> section 2,
    /// which verifies them against the SINTRAN III K03/L07/M06 symbol tables. The mnemonic
    /// naming (for human display) is provided separately by
    /// <see cref="SubProtocol.TadOpcodes"/>.
    /// </para>
    /// <para>
    /// The values are sparse and not a contiguous range, so a byte off the wire that matches no
    /// member here is an opcode we have not decoded rather than a malformed message. Cast and
    /// compare, but never assume the set is complete.
    /// </para>
    /// </remarks>
    public enum TadOp : byte
    {
        /// <summary>
        /// BDAT - terminal data block (<c>0x01</c>).
        /// </summary>
        Bdat = 0x01,

        /// <summary>
        /// RFI - ready for input / flow-control credit (<c>0x02</c>).
        /// </summary>
        Rfi = 0x02,

        /// <summary>
        /// ECKM - echo strategy (<c>0x03</c>).
        /// </summary>
        Eckm = 0x03,

        /// <summary>
        /// BMMX - break strategy / max break (<c>0x04</c>).
        /// </summary>
        Bmmx = 0x04,

        /// <summary>
        /// ESCA - escape received (<c>0x08</c>). Asker-sent; the host answers with opcode <c>0x20</c>.
        /// </summary>
        Esca = 0x08,

        /// <summary>
        /// DCON - disconnect indication (<c>0x09</c>).
        /// </summary>
        Dcon = 0x09,

        /// <summary>
        /// TMOD - terminal mode flags (<c>0x0C</c>).
        /// </summary>
        Tmod = 0x0C,

        /// <summary>
        /// TTYP - terminal type id (<c>0x0D</c>).
        /// </summary>
        Ttyp = 0x0D,

        /// <summary>
        /// CESC - command-escape / session control state (<c>0x0E</c>). Carries a 1-byte state that
        /// steps 0x00 (auth-prompt) to 0x01 (auth-complete) during the login handshake.
        /// </summary>
        Cesc = 0x0E,

        /// <summary>
        /// DESC - define escape character (<c>0x0F</c>).
        /// </summary>
        Desc = 0x0F,

        /// <summary>
        /// SYCN - session sync / login-state word (<c>0x13</c>).
        /// </summary>
        /// <remarks>
        /// The 16-bit value steps 0x0002 (connected/ENTER) to 0x0003 (password prompt) to 0x0006
        /// (password OK) to 0x000A (logged in). VERIFIED from conn-to-d102 frames 62/64/68/70;
        /// reaching 0x000A is what marks the TAD "logged in" so SINTRAN stops applying the
        /// 1-minute "not logged in" idle drop.
        /// </remarks>
        Sycn = 0x13,

        /// <summary>
        /// RESE - reset connection request (<c>0x16</c>).
        /// </summary>
        Rese = 0x16,

        /// <summary>
        /// RECO - reset confirm (<c>0x17</c>).
        /// </summary>
        Reco = 0x17,

        /// <summary>
        /// DUMM - dummy / no-op keep-the-stream-moving message (<c>0x18</c>).
        /// </summary>
        Dumm = 0x18,

        /// <summary>
        /// OPSV - OS / protocol version handshake (<c>0x1F</c>).
        /// </summary>
        Opsv = 0x1F,

        /// <summary>
        /// CERS - escape / CESC response (<c>0x21</c>). Asker-sent after each host burst / CESC change.
        /// </summary>
        Cers = 0x21,
    }
}
