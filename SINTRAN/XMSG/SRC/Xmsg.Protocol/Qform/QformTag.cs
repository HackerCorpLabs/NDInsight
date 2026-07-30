using System;

namespace NDInsight.Sintran.Xmsg.Protocol.Qform
{
    /// <summary>
    /// The type class of a QFORM tag byte, taken from bits 4 to 6.
    /// </summary>
    /// <remarks>
    /// <para>
    /// QFORM is the typed-parameter format the COSMOS file servers use inside an XMSG message body.
    /// A tag byte is <c>1cccnnnn</c>: bit 7 must be set, bits 4-6 are the class, and the low nibble
    /// means different things per class - see <see cref="QformReader"/>.
    /// </para>
    /// <para>
    /// Class meanings were read out of the reader routine itself, <c>qform_read_tag_and_value</c> at
    /// <c>ram:0x7d01</c> in <c>cos-fa-serv-e04</c>, whose literal pool at <c>0x7d82..0x7d89</c> holds
    /// the masks. Classes 4 and 5 have never been observed and class 6's meaning is unknown, so they
    /// are named neutrally rather than guessed at.
    /// </para>
    /// </remarks>
    public enum QformClass
    {
        /// <summary>
        /// Class 0 - a CONSTRUCTED value: length-delimited, and its content is itself tagged.
        /// The low nibble is a subtype (masked with 0x17), NOT a length; the length always comes
        /// from the following byte. Observed as tag 0x80 (subtype 0) and 0x8C (subtype 4).
        /// </summary>
        Constructed = 0,

        /// <summary>
        /// Class 1 - integer. Emitted as 0x92 (2 bytes) and 0x94 (4 bytes).
        /// </summary>
        Integer = 1,

        /// <summary>
        /// Class 2 - a second integer class, emitted as 0xA2 (2 bytes) and 0xA4 (4 bytes). Carries
        /// the SINTRAN error number in a file-access rejection: 0x0030 = 48 = wrong password.
        /// </summary>
        TypedInteger = 2,

        /// <summary>
        /// Class 3 - byte string. Observed as 0xBD (13 bytes, "BAK03  SYSTEM") and as 0xB0 with an
        /// escaped length of 16 holding a user name plus NUL padding.
        /// </summary>
        ByteString = 3,

        /// <summary>
        /// Class 4 - never observed on the wire or in the binary.
        /// </summary>
        Class4 = 4,

        /// <summary>
        /// Class 5 - never observed on the wire or in the binary.
        /// </summary>
        Class5 = 5,

        /// <summary>
        /// Class 6 - observed as 0xE1 (one value byte). Mechanically decodable, but its MEANING is
        /// unknown: the file server never emits it, so nothing in that binary defines it.
        /// </summary>
        Class6Unknown = 6,

        /// <summary>
        /// Class 7 - a field SELECTOR, emitted as 0xF2 with a 2-byte field number. The selector
        /// 0x00FF ends the parameter list.
        /// </summary>
        Selector = 7,
    }

    /// <summary>
    /// One decoded QFORM field: its tag, its class, and where its value sits in the body.
    /// </summary>
    public readonly struct QformField
    {
        /// <summary>
        /// Initialises a decoded field.
        /// </summary>
        /// <param name="tag">
        /// The raw tag byte.
        /// </param>
        /// <param name="valueOffset">
        /// Offset of the value's first byte within the body.
        /// </param>
        /// <param name="valueLength">
        /// Length of the value in bytes.
        /// </param>
        /// <param name="depth">
        /// Nesting depth; 0 is top level, 1 is inside one constructed value.
        /// </param>
        public QformField(byte tag, int valueOffset, int valueLength, int depth)
        {
            Tag = tag;
            ValueOffset = valueOffset;
            ValueLength = valueLength;
            Depth = depth;
        }

        /// <summary>
        /// The raw tag byte as it appears on the wire.
        /// </summary>
        public byte Tag { get; }

        /// <summary>
        /// Offset of the value's first byte within the body.
        /// </summary>
        public int ValueOffset { get; }

        /// <summary>
        /// Length of the value in bytes.
        /// </summary>
        public int ValueLength { get; }

        /// <summary>
        /// Nesting depth. 0 is top level; 1 is inside one constructed value.
        /// </summary>
        public int Depth { get; }

        /// <summary>
        /// The tag's type class, from bits 4 to 6.
        /// </summary>
        public QformClass Class
        {
            get { return (QformClass)((Tag & 0x70) >> 4); }
        }

        /// <summary>
        /// The subtype of a constructed value (class 0), masked with 0x17. Meaningless for other
        /// classes, where the low nibble is a length instead.
        /// </summary>
        public int ConstructedSubtype
        {
            get { return Tag & 0x17; }
        }

        /// <summary>
        /// True when this field's value is itself a tagged QFORM stream.
        /// </summary>
        public bool IsConstructed
        {
            get { return Class == QformClass.Constructed; }
        }
    }
}
