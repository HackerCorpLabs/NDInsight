using System;

using NDInsight.Sintran.Xmsg.Codec;

namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// Adapts an <see cref="ILink"/> into the codec's downward <see cref="IXmsgTransport"/> sink:
    /// each <see cref="IXmsgTransport.Send"/> becomes one <see cref="ILink.SendSintranFrame"/>.
    /// </summary>
    /// <remarks>
    /// This is the single wire between the pure protocol seam (which knows only
    /// <see cref="IXmsgTransport"/>) and the link seam (which knows LAPB). Keeping it a one-line
    /// adapter means the codec never depends on <c>Xmsg.Live</c>, and the whole protocol stack
    /// migrates without touching this class — only the <see cref="ILink"/> implementation swaps.
    /// </remarks>
    public sealed class LinkXmsgTransport : IXmsgTransport
    {
        private readonly ILink _link;

        /// <summary>
        /// Initialises the transport over a link.
        /// </summary>
        /// <param name="link">The link to send SINTRAN frames on.</param>
        /// <exception cref="ArgumentNullException">Thrown when <paramref name="link"/> is null.</exception>
        public LinkXmsgTransport(ILink link)
        {
            _link = link ?? throw new ArgumentNullException(nameof(link));
        }

        /// <inheritdoc />
        public void Send(ReadOnlySpan<byte> bytes)
        {
            _link.SendSintranFrame(bytes);
        }
    }
}
