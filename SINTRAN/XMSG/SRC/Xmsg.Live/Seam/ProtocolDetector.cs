using System;

namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// Classifies which L3 protocol a link's payload belongs to (X.25 vs XMSG). Present only as a
    /// <b>seam</b> for now: the real mechanism is per-link binding, so the shipped implementation
    /// simply returns the link's configured binding without inspecting bytes.
    /// </summary>
    /// <remarks>
    /// See XMSG-TRANSPORT-SEAM-PLAN.md section 5. Byte-level sniffing is deliberately deferred and
    /// is known to be unreliable: <c>0x21 0x13</c> is also a valid X.25 GFI/LCN ("logical channel
    /// 19"), so a single-byte test cannot distinguish the two. A future heuristic (byte0 == 0x21 AND
    /// byte1 ∈ {0x12,0x13} AND byte3 a known XMSG subtype AND the sub-header marker <c>0x21 0x00</c>
    /// at its expected offset) can replace this stub without restructuring anything above it.
    /// </remarks>
    public interface IProtocolDetector
    {
        /// <summary>
        /// Returns the L3 binding a payload on the given link should be handled as.
        /// </summary>
        /// <param name="linkId">The link the payload arrived on.</param>
        /// <param name="payload">The payload bytes (unused by the per-link-binding stub).</param>
        /// <returns>The L3 binding to route the payload to.</returns>
        LinkBinding Classify(string linkId, ReadOnlySpan<byte> payload);
    }

    /// <summary>
    /// The per-link-binding detector: always reports the fixed binding it was constructed with,
    /// ignoring the payload. This is the "real mechanism" for now — each HDLC link is configured to
    /// carry X.25 or XMSG, matching the ND machine's installed software.
    /// </summary>
    public sealed class BoundProtocolDetector : IProtocolDetector
    {
        private readonly LinkBinding _binding;

        /// <summary>
        /// Initialises the detector with the link's fixed L3 binding.
        /// </summary>
        /// <param name="binding">The binding to report for every payload on this link.</param>
        public BoundProtocolDetector(LinkBinding binding)
        {
            _binding = binding;
        }

        /// <inheritdoc />
        public LinkBinding Classify(string linkId, ReadOnlySpan<byte> payload)
        {
            // Per-link binding: the configured L3 protocol, regardless of payload bytes. No sniffing.
            return _binding;
        }
    }
}
