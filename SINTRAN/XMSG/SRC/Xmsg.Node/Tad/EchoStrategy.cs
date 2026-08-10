namespace NDInsight.Sintran.Xmsg.Node.Tad
{
    /// <summary>
    /// The 7ECKM echo-strategy value (spec TAD-Message-Formats.md 5.2 / 22.12). Echo is
    /// host-controlled but client-executed: the host sends this value and the asker applies it to its
    /// own local echo.
    /// </summary>
    /// <remarks>
    /// On the wire an ECKM message is <c>00 03 01 value</c> (the leading <c>0x00</c> is the
    /// spec 2.1 prefix). Only these three values appear in the captured logins; the NPL catalog defines
    /// strategies 1-7 with an optional 20-byte custom table, never seen on these links.
    /// </remarks>
    public enum EchoStrategy : byte
    {
        /// <summary>
        /// Echo off / line-discipline teardown (<c>0x00</c>) — sent in the logout ladder alongside
        /// <c>BMMX 000000</c> and <c>CESC 00</c>.
        /// </summary>
        Teardown = 0x00,

        /// <summary>
        /// Echo ON (<c>0x01</c>) — the asker echoes keystrokes locally. Sent in the banner burst and to
        /// restore echo in the password verdict (both the OK and the wrong-password reset).
        /// </summary>
        LocalEcho = 0x01,

        /// <summary>
        /// Echo OFF (<c>0xFF</c>) — the asker displays nothing while typing. Sent right before the
        /// "PASSWORD- " RFI so the password is entered blind.
        /// </summary>
        NoEcho = 0xFF,
    }
}
