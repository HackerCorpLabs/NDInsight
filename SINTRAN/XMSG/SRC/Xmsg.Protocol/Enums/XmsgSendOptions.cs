using System;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// The XMSG sub-header "role" byte, decoded as the high byte (bits 8-15) of the MON 200B send-option
    /// word. Each bit selects a send option.
    /// </summary>
    /// <remarks>
    /// This is the SAME bit set as <see cref="XmsgOption"/>, shifted down by 8: XmsgOption is the
    /// 16-bit MON 200B T-register word (source of truth, generated from the official constants) and
    /// this enum is its high byte as it appears on the wire. Convert with
    /// <see cref="XmsgOptionConversion"/>; the lockstep is enforced by a unit test, so never
    /// re-number one enum without the other.
    /// The option-bit names and values are VERIFIED from XMSG-VALUES-M.SYMB (the XF* symbols); the
    /// placement of that byte into the wire role octet is INFERRED, but it decodes every observed role
    /// value (0xE4/0x84/0x94/0x40/0x00/0x60/0xC4/0x54) with zero exceptions. Note in particular that
    /// <see cref="RoutedLetter"/> (bit <c>0x04</c>) is XFROU "routed", NOT an "asker" marker - the old
    /// <c>(role and 0x0F) == 0x04</c> asker test was wrong, because the host-originated 0xFD notification
    /// rides role <c>0x54</c> which also has that bit set. See XMSG-PROTOCOL.md section 18.4.
    /// Observed compositions: connect letter <c>0xE4</c> = WaitForTransfer|WakeOnStatus|HighPriority|RoutedLetter;
    /// asker data <c>0x84</c> = WaitForTransfer|RoutedLetter; asker control <c>0x94</c> =
    /// WaitForTransfer|Bounce|RoutedLetter; host letter <c>0x40</c> = WakeOnStatus; host terminal data
    /// <c>0x00</c> = None; XSGSY reply <c>0x60</c> = WakeOnStatus|HighPriority; XSGSY request <c>0xC4</c> =
    /// WaitForTransfer|WakeOnStatus|RoutedLetter; 0xFD notify <c>0x54</c> = WakeOnStatus|Bounce|RoutedLetter.
    /// </remarks>
    [Flags]
    public enum XmsgSendOptions : byte
    {
        /// <summary>
        /// No options set (<c>0x00</c>) - host terminal-data frames.
        /// </summary>
        None = 0,

        /// <summary>
        /// XFTCM (bit 0, <c>0x01</c>) - meaning UNKNOWN; never observed set in the corpus.
        /// </summary>
        Tcm = 1 << 0,

        /// <summary>
        /// XFSEC secure (bit 1, <c>0x02</c>) - never observed set in the corpus.
        /// </summary>
        Secure = 1 << 1,

        /// <summary>
        /// XFROU routed letter (bit 2, <c>0x04</c>) - routed via XROUT to a remote port.
        /// </summary>
        RoutedLetter = 1 << 2,

        /// <summary>
        /// XFFWD forward (bit 3, <c>0x08</c>) - never observed set in the corpus.
        /// </summary>
        Forward = 1 << 3,

        /// <summary>
        /// XFBNC bounce / return-on-failure (bit 4, <c>0x10</c>).
        /// </summary>
        Bounce = 1 << 4,

        /// <summary>
        /// XFHIP high priority (bit 5, <c>0x20</c>).
        /// </summary>
        HighPriority = 1 << 5,

        /// <summary>
        /// XFWAK wake-on-status (bit 6, <c>0x40</c>).
        /// </summary>
        WakeOnStatus = 1 << 6,

        /// <summary>
        /// XFWTF wait-for-transfer (bit 7, <c>0x80</c>).
        /// </summary>
        WaitForTransfer = 1 << 7,
    }
}
