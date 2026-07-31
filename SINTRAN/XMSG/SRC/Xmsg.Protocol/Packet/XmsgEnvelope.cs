namespace NDInsight.Sintran.Xmsg.Packet
{
    /// <summary>
    /// The complete XMSG envelope arithmetic. Per link (node pair) there is one constant byte — the
    /// <b>seed</b> — and per direction one variable — <b>Flags 1</b> (the datagram sequence, +1 per
    /// data frame, starting at 0x0000). Everything else (the sub-header Counter and the sub-protocol
    /// Channel / Protocol ID) is derived arithmetically.
    /// </summary>
    /// <remarks>
    /// <para><b>The rule (VERIFIED against all 601 captured Data frames + 602 ACKs, zero exceptions —
    /// see <c>SINTRAN/XMSG/DOC/XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md</c>):</b></para>
    /// <code>
    /// baseLow = (seed - (Flags2 &amp; 0xFF)) &amp; 0xFF        // Flags2 == XMCSM &gt;&gt; 16, always
    /// Counter = (baseLow - Flags1) &amp; 0xFF
    /// epoch   = (Flags1 - baseLow + 0xFF) &gt;&gt; 8          // cumulative counter-wrap count; 0 when fresh
    /// Channel = 0xDE - (XMCSM &gt;&gt; 24) - epoch
    /// Base    = Flags1 + Counter = baseLow + 0x100*epoch  // the old "envelope Base", now derived
    /// </code>
    /// <para><b>Fresh-responder consequence:</b> at epoch 0 a terminal-data frame (XMCSM 0x01080000)
    /// derives to channel <b>0xDD</b>, not DB. DB only appears at epoch 2 (a node with a high running
    /// sequence, e.g. the conn-to-d102 capture). The same XROUT letter class rides DD (epoch 0),
    /// DC (epoch 1) and DB (epoch 2) purely by epoch. This is why the fresh live node's terminal data
    /// belongs on DD with <c>Counter = (seed - 8 - Flags1) &amp; 0xFF</c>.</para>
    /// <para><b>SCOPE LIMIT, found 2026-07-30.</b> The Channel line above holds only where the LOW BYTE
    /// of Flags 2 is a message-class marker - <c>0x00</c> control, <c>0x08</c> terminal data. That was
    /// the whole corpus when the rule was derived. The COSMOS file-server captures broke the
    /// assumption: there Flags 2 is the message BODY LENGTH, so <c>baseLow = seed - (Flags2 &amp; 0xFF)</c>
    /// becomes a function of how long the message happened to be, and the epoch derived from it counts
    /// wraps of nothing. Measured over the current corpus:</para>
    /// <code>
    /// Flags2 low byte IS a class marker:  800 frames, 0 channel mismatches
    /// Flags2 low byte is a length:        170 frames, 70 channel mismatches
    /// </code>
    /// <para>So the rule is exactly right on 800 frames and inapplicable on the rest, rather than
    /// approximately right everywhere. The Counter line is unaffected. What the channel does on
    /// length-valued Flags 2 is <b>UNKNOWN</b> - observed wire offsets there are 0, 1, 2 and 3, and no
    /// replacement formula has been fitted on purpose. Track the channel per direction on that traffic,
    /// or capture enough of it to derive the rule properly. See
    /// <c>ChannelOffsetDiagnosticTests</c> and <c>EnvelopeConformanceTests</c>.</para>
    /// <para><b>Seed:</b> learn it from any received Data frame via <see cref="LearnSeed"/> rather than
    /// hardcoding. Observed seeds: 100↔102 = 0x14, 100↔103 = 0x13, 102↔103 = 0x11 (direct) / 0x12
    /// (relayed). What the seed byte encodes is UNKNOWN; learn-from-peer sidesteps it.</para>
    /// </remarks>
    public static class XmsgEnvelope
    {
        /// <summary>
        /// The constant the channel derivation subtracts from (the top of the Protocol-ID range).
        /// </summary>
        public const byte ChannelAnchor = 0xDE;

        /// <summary>
        /// Learns the per-link seed byte from a received frame:
        /// <c>seed = (Counter + Flags1 + (Flags2 &amp; 0xFF)) &amp; 0xFF</c>.
        /// </summary>
        /// <param name="flags1">
        /// The frame's Flags 1 (datagram sequence).
        /// </param>
        /// <param name="counter">
        /// The frame's sub-header Counter.
        /// </param>
        /// <param name="flags2">
        /// The frame's Flags 2 (equals XMCSM &gt;&gt; 16).
        /// </param>
        /// <returns>
        /// The link seed byte.
        /// </returns>
        public static byte LearnSeed(ushort flags1, byte counter, ushort flags2)
        {
            return (byte)(counter + flags1 + (flags2 & 0xFF));
        }

        /// <summary>
        /// The per-class base low byte: <c>(seed - (Flags2 &amp; 0xFF)) &amp; 0xFF</c>. Control frames
        /// (Flags2 low 0x00) use <c>seed</c>; terminal-data frames (Flags2 low 0x08) use <c>seed - 8</c>.
        /// </summary>
        /// <param name="seed">
        /// The link seed.
        /// </param>
        /// <param name="flags2">
        /// The frame's Flags 2.
        /// </param>
        /// <returns>
        /// The base low byte.
        /// </returns>
        public static byte BaseLow(byte seed, ushort flags2)
        {
            // Truncation to a byte is correct HERE because the only consumer of this form is
            // the Counter, which is itself a byte and so is exact modulo 256 either way.
            return (byte)BaseLowSigned(seed, flags2);
        }

        /// <summary>
        /// The base low value WITHOUT truncation to a byte, for the epoch calculation.
        /// </summary>
        /// <remarks>
        /// <para>
        /// When the low byte of <paramref name="flags2"/> exceeds <paramref name="seed"/> this is
        /// NEGATIVE, and it has to stay negative. <see cref="ComputeEpoch"/> divides
        /// <c>Flags1 - baseLow</c> by 256, so truncating a negative base into 0..255 moves the
        /// boundary and loses a borrow: the epoch comes out one too low and the derived channel
        /// one too high.
        /// </para>
        /// <para>
        /// CORRECTED 2026-07-31. The masked form was verified only against TAD and routing
        /// captures, where every observed Flags 2 low byte is at or below the seed, so the case
        /// never arose. File-server traffic carries 11-15 distinct class words per session, many
        /// above the seed, and mismatched constantly. With the truncation removed the model is
        /// exact on the whole capture corpus - 1449/1449 data frames across every capture and
        /// every link - against 1267/1449 with it.
        /// See <c>SINTRAN/XMSG/DOC/XMSG-CHANNEL-FORMULA-DIVERGES-ON-FILE-SERVER-TRAFFIC-2026-07-31.md</c>.
        /// </para>
        /// </remarks>
        /// <param name="seed">
        /// The link seed.
        /// </param>
        /// <param name="flags2">
        /// The frame's Flags 2.
        /// </param>
        /// <returns>
        /// The signed base low value; negative when the Flags 2 low byte exceeds the seed.
        /// </returns>
        public static int BaseLowSigned(byte seed, ushort flags2)
        {
            return seed - (flags2 & 0xFF);
        }

        /// <summary>
        /// Computes the sub-header Counter: <c>(baseLow - Flags1) &amp; 0xFF</c>.
        /// </summary>
        /// <param name="seed">
        /// The link seed.
        /// </param>
        /// <param name="flags1">
        /// The datagram sequence for this frame.
        /// </param>
        /// <param name="flags2">
        /// The frame's Flags 2.
        /// </param>
        /// <returns>
        /// The sub-header Counter byte.
        /// </returns>
        public static byte ComputeCounter(byte seed, ushort flags1, ushort flags2)
        {
            return (byte)(BaseLow(seed, flags2) - flags1);
        }

        /// <summary>
        /// Computes the epoch (cumulative counter-wrap count) for a frame:
        /// <c>(Flags1 - baseLow + 0xFF) &gt;&gt; 8</c>. Zero while Flags1 ≤ baseLow (a fresh direction).
        /// </summary>
        /// <param name="seed">
        /// The link seed.
        /// </param>
        /// <param name="flags1">
        /// The datagram sequence for this frame.
        /// </param>
        /// <param name="flags2">
        /// The frame's Flags 2.
        /// </param>
        /// <returns>
        /// The epoch (0, 1, 2, …).
        /// </returns>
        public static int ComputeEpoch(byte seed, ushort flags1, ushort flags2)
        {
            // int arithmetic; the +0xFF keeps small (Flags1 - baseLow) negatives at epoch 0.
            // Uses the SIGNED base - see BaseLowSigned for why truncating here is a defect.
            return (flags1 - BaseLowSigned(seed, flags2) + 0xFF) >> 8;
        }

        /// <summary>
        /// Derives the sub-protocol channel from the full seed model:
        /// <c>Channel = 0xDE - (XMCSM &gt;&gt; 24) - epoch</c>.
        /// </summary>
        /// <param name="seed">
        /// The link seed.
        /// </param>
        /// <param name="flags1">
        /// The datagram sequence for this frame.
        /// </param>
        /// <param name="flags2">
        /// The frame's Flags 2.
        /// </param>
        /// <param name="controlService">
        /// The XMCSM control/service word.
        /// </param>
        /// <returns>
        /// The derived Protocol ID.
        /// </returns>
        public static SintranProtocolId DeriveChannel(byte seed, ushort flags1, ushort flags2, uint controlService)
        {
            int epoch = ComputeEpoch(seed, flags1, flags2);
            byte channel = (byte)(ChannelAnchor - (byte)(controlService >> 24) - epoch);
            return (SintranProtocolId)channel;
        }

        /// <summary>
        /// Computes the envelope base <c>Flags1 + Counter</c> (16-bit, wrapping) — the legacy view,
        /// equal to <c>baseLow + 0x100*epoch</c>.
        /// </summary>
        /// <param name="flags1">
        /// The datagram sequence.
        /// </param>
        /// <param name="counter">
        /// The sub-header counter.
        /// </param>
        /// <returns>
        /// The 16-bit base.
        /// </returns>
        public static ushort ComputeBase(ushort flags1, byte counter)
        {
            return (ushort)(flags1 + counter);
        }

        /// <summary>
        /// Legacy channel derivation from an already-known Counter:
        /// <c>Channel = 0xDE - (XMCSM &gt;&gt; 24) - (Base &gt;&gt; 8)</c>, where <c>Base = Flags1 + Counter</c>.
        /// Equivalent to the seed model when the Counter is the model-correct one (Base&gt;&gt;8 == epoch).
        /// </summary>
        /// <param name="flags1">
        /// The datagram sequence.
        /// </param>
        /// <param name="counter">
        /// The per-direction counter.
        /// </param>
        /// <param name="controlService">
        /// The XMCSM control/service word.
        /// </param>
        /// <returns>
        /// The derived Protocol ID.
        /// </returns>
        public static SintranProtocolId DeriveChannel(ushort flags1, byte counter, uint controlService)
        {
            ushort baseValue = ComputeBase(flags1, counter);
            byte channel = (byte)(ChannelAnchor - (byte)(controlService >> 24) - (byte)(baseValue >> 8));
            return (SintranProtocolId)channel;
        }
    }
}
