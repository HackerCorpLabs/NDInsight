using System;
using System.Collections.Generic;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// An XROUT standard message (the "letter") body: a 4-byte header followed by
    /// even-aligned TLV parameter blocks.
    /// </summary>
    /// <remarks>
    /// <para><b>Header</b></para>
    /// The first four bytes of the message body (XMSG-API.md section 4.1):
    ///  - byte 0 serial number, echoed unchanged by XROUT so replies can be matched.
    ///  - byte 1 service number on request; XROUT overwrites it with the return status on reply.
    ///  - bytes 2-3 length of the remainder of the message in bytes (big-endian).
    /// Every parameter block starts on an even byte boundary; a <c>0x00</c> fill byte
    /// is inserted where needed to realign.
    /// </remarks>
    public sealed class XroutMessage
    {
        /// <summary>
        /// Size of the fixed XROUT message header (serial, service, length) in bytes.
        /// </summary>
        public const int HeaderSize = 4;

        private readonly List<XroutParameter> _parameters;

        /// <summary>
        /// Initialises an empty XROUT message.
        /// </summary>
        public XroutMessage()
        {
            _parameters = new List<XroutParameter>();
        }

        /// <summary>
        /// Gets or sets the serial number (byte 0). The high bit should be 0.
        /// </summary>
        public byte Serial { get; set; }

        /// <summary>
        /// Gets or sets the service number on request, or the return status on reply (byte 1).
        /// </summary>
        public byte Service { get; set; }

        /// <summary>
        /// Gets or sets the declared remainder length in bytes (bytes 2-3).
        /// </summary>
        /// <remarks>
        /// This is the value parsed from or computed for the wire. It equals the total
        /// serialised length of all parameter blocks including alignment fill bytes.
        /// </remarks>
        public ushort Length { get; set; }

        /// <summary>
        /// Gets the parameter blocks of this message in order.
        /// </summary>
        public IReadOnlyList<XroutParameter> Parameters
        {
            get { return _parameters; }
        }

        /// <summary>
        /// Appends a parameter block to this message.
        /// </summary>
        /// <param name="parameter">
        /// The parameter to append.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="parameter"/> is null.
        /// </exception>
        public void AddParameter(XroutParameter parameter)
        {
            if (parameter == null)
            {
                throw new ArgumentNullException(nameof(parameter));
            }

            _parameters.Add(parameter);
        }

        /// <summary>
        /// Parses an XROUT standard message from a payload span.
        /// </summary>
        /// <param name="source">
        /// The message-body bytes starting at the serial number; must contain at
        /// least <see cref="HeaderSize"/> bytes.
        /// </param>
        /// <returns>
        /// The parsed <see cref="XroutMessage"/>.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="source"/> is shorter than <see cref="HeaderSize"/> bytes.
        /// </exception>
        public static XroutMessage Parse(ReadOnlySpan<byte> source)
        {
            if (source.Length < HeaderSize)
            {
                throw new ArgumentException("XROUT message requires at least a 4-byte header.", nameof(source));
            }

            XroutMessage message = new XroutMessage();
            message.Serial = source[0];
            message.Service = source[1];
            message.Length = BigEndian.ReadUInt16(source.Slice(2, 2));

            // Bound the remainder by the declared length, clamped to what is present.
            int available = source.Length - HeaderSize;
            int remainderLength = message.Length;
            if (remainderLength > available)
            {
                remainderLength = available;
            }

            ReadOnlySpan<byte> remainder = source.Slice(HeaderSize, remainderLength);

            int pos = 0;
            while (pos < remainder.Length)
            {
                sbyte typeByte = (sbyte)remainder[pos];
                if (typeByte == 0)
                {
                    // Fill/skip byte used to keep the next block on an even boundary.
                    pos++;
                    continue;
                }

                if (pos + 1 >= remainder.Length)
                {
                    // Truncated - no length byte available; stop parsing.
                    break;
                }

                int length = remainder[pos + 1];
                int dataStart = pos + 2;
                if (dataStart + length > remainder.Length)
                {
                    // Truncated data; stop parsing rather than reading past the end.
                    break;
                }

                bool isString = typeByte < 0;
                int parameterNumber = isString ? -typeByte : typeByte;

                byte[] data = new byte[length];
                for (int i = 0; i < length; i++)
                {
                    data[i] = remainder[dataStart + i];
                }

                message._parameters.Add(new XroutParameter(parameterNumber, isString, data));
                pos = dataStart + length;
            }

            return message;
        }

        /// <summary>
        /// Computes the serialised remainder (all parameter blocks with alignment fill).
        /// </summary>
        /// <returns>
        /// A new array holding the parameter blocks as they appear after the 4-byte header.
        /// </returns>
        public byte[] BuildRemainder()
        {
            // First pass: compute the total size including even-alignment fill bytes.
            int size = 0;
            for (int i = 0; i < _parameters.Count; i++)
            {
                if ((size & 1) != 0)
                {
                    size++; // fill byte to reach an even boundary
                }

                size += 2 + _parameters[i].Length; // type + length + data
            }

            byte[] buffer = new byte[size];
            int cursor = 0;
            for (int i = 0; i < _parameters.Count; i++)
            {
                XroutParameter parameter = _parameters[i];
                if ((cursor & 1) != 0)
                {
                    buffer[cursor++] = 0x00; // fill byte
                }

                buffer[cursor++] = parameter.TypeByte;
                buffer[cursor++] = (byte)parameter.Length;
                byte[] data = parameter.Data;
                for (int j = 0; j < data.Length; j++)
                {
                    buffer[cursor++] = data[j];
                }
            }

            return buffer;
        }

        /// <summary>
        /// Serialises this message (header plus parameter blocks) into a new array.
        /// </summary>
        /// <returns>
        /// A new array containing the serialised message body.
        /// </returns>
        /// <remarks>
        /// The <see cref="Length"/> property is updated to the computed remainder length.
        /// </remarks>
        public byte[] ToArray()
        {
            byte[] remainder = BuildRemainder();
            Length = (ushort)remainder.Length;

            byte[] buffer = new byte[HeaderSize + remainder.Length];
            buffer[0] = Serial;
            buffer[1] = Service;
            BigEndian.WriteUInt16(buffer.AsSpan(2, 2), Length);
            for (int i = 0; i < remainder.Length; i++)
            {
                buffer[HeaderSize + i] = remainder[i];
            }

            return buffer;
        }
    }
}
