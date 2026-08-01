namespace NDInsight.Sintran.Xmsg.Packet
{
    /// <summary>
    /// The full field set of a data (subtype <c>0x0E</c>) XMSG packet: SINTRAN header addressing,
    /// the XMSG sub-header envelope, and the trailing payload. Passed to
    /// <see cref="XmsgPacketBuilder.CreateData"/> so a data packet is built from one intention-
    /// revealing value instead of a dozen positional arguments.
    /// </summary>
    /// <remarks>
    /// These are the exact fields the captured connect handshake is built from (see the
    /// conn-to-d102 accept / port-assign vectors). Field NAMES follow the wire glossary:
    /// XMDSY/XMDPT = destination system/port, XMSSY/XMSPT = source system/port, XMCSM = the
    /// control/service dispatch word. The channel (Protocol ID) and the datagram sequence
    /// (Flags 1) live in the SINTRAN header; the per-direction Counter lives in the sub-header.
    /// </remarks>
    public struct XmsgDataFields
    {
        /// <summary>
        /// Destination node number (SINTRAN header offsets 4-5).
        /// </summary>
        public ushort DestinationNode;

        /// <summary>
        /// Source node number (SINTRAN header offsets 6-7).
        /// </summary>
        public ushort SourceNode;

        /// <summary>
        /// Flags 1 — the per-direction datagram sequence (offsets 8-9).
        /// </summary>
        public ushort Flags1;

        /// <summary>
        /// Flags 2 (offsets 10-11) — a COPY of the 16-bit XMCSM; <c>0x0400</c> on the setup frames.
        /// </summary>
        /// <remarks>
        /// <para>
        /// CARVED FACT, 2026-07-31 - this is not an independent "frame-class word". It equals the
        /// true 16-bit XMCSM at wire 26-27 on <b>1449 of 1449</b> data frames in the corpus. See
        /// <see cref="ControlService"/> for the full word layout carved from the kernel symbols.
        /// </para>
        /// <para>
        /// The kernel comments XMCSM as <c>"datagram checksum; if not checksum, then message
        /// size"</c>, and that is CARVED as literally true - the code is at kernel address
        /// <c>134013</c> in <c>XMSG-KERNEL-L03.BPUN</c>:
        /// </para>
        /// <para>
        /// It first stores <c>XMSIZ</c> (<c>0o144</c>) into <c>XMCSM</c> (<c>0o142</c>) - the SIZE
        /// size, which is the default. Two <c>BSKP</c> flag-bit tests then decide: if either is set it
        /// keeps the size, otherwise it saves <c>XMSTA</c>, ZEROES both <c>XMSTA</c> and
        /// <c>XMCSM</c>, calls the routine at <c>137335</c>, and stores the result into
        /// <c>XMCSM</c>. Zeroing the field before computing and storing the result back is the
        /// standard checksum-over-block idiom, and it is why <c>XMSTA</c> is excluded from the
        /// covered bytes.
        /// </para>
        /// <para>
        /// The routine at <c>137335</c> is a ones-complement sum with END-AROUND CARRY
        /// (<c>RADD SA DD</c> followed by <c>RADD ADC CLD SD DD</c>) over <c>XMLEN/2</c> words
        /// starting at <c>XMDAB</c>/<c>XMDAW</c>, with an odd-length tail path. So <c>XMLEN</c>
        /// (<c>0o147</c>) IS a real length field - it bounds the checksum - it simply is not at
        /// wire 30-31 where this layout once put it.
        /// </para>
        /// <para>
        /// This explains the corpus: 718 frames satisfy <c>XMCSM &amp; 0x1FF == bodyLen</c>, and
        /// 731 carry a CONSTANT (<c>0x0080</c>, <c>0x0064</c>) while the body varies. The
        /// constants are <c>XMSIZ</c> - the message BUFFER size, fixed per
        /// message class - not the body length.
        /// </para>
        /// <para>
        /// UNKNOWN: which flag bit picks size or checksum. The decision is the <c>BSKP ONE 110 DA</c> /
        /// <c>BSKP ONE 120 DA</c> pair at <c>134016</c>/<c>134021</c> on a value fetched by
        /// <c>LDATX</c>. The location is carved; the bit identity is not. Do NOT assume a frame's
        /// XMCSM is a length without checking - roughly half are checksums.
        /// </para>
        /// <para>
        /// This matters for the envelope arithmetic: <see cref="XmsgEnvelope.BaseLowSigned"/>
        /// subtracts the low byte of this field, so on file-server frames it is subtracting a BYTE
        /// COUNT. Any reasoning that treats it as a pure class selector will be wrong on exactly
        /// that traffic.
        /// </para>
        /// </remarks>
        public ushort Flags2;

        /// <summary>
        /// The sub-protocol selector / channel (Protocol ID, offset 12).
        /// </summary>
        public SintranProtocolId ProtocolId;

        /// <summary>
        /// Sub-header per-direction counter (offset 0).
        /// </summary>
        public byte Counter;

        /// <summary>
        /// Sub-header frame-flags byte (offset 3); <c>0x86</c> for the setup frames.
        /// </summary>
        public byte FrameFlags;

        /// <summary>
        /// Sub-header role byte (offset 4); low nibble 4 = asker, 0 = responder.
        /// </summary>
        public byte Role;

        /// <summary>
        /// XMDSY destination system number (offsets 5-6, big-endian).
        /// </summary>
        public ushort DestinationSystem;

        /// <summary>
        /// XMDPT destination port (offsets 7-8, big-endian).
        /// </summary>
        public ushort DestinationPort;

        /// <summary>
        /// XMSSY source system number (offsets 9-10, big-endian).
        /// </summary>
        public ushort SourceSystem;

        /// <summary>
        /// XMSPT source port (offsets 11-12, big-endian).
        /// </summary>
        public ushort SourcePort;

        /// <summary>
        /// XMCSM plus the first word of the message body, read as one 32-bit value
        /// (sub-header offsets 13-16, i.e. wire offsets 26-29).
        /// </summary>
        /// <remarks>
        /// <para>
        /// CARVED FACT, 2026-07-31 - <b>this field spans a boundary and is misnamed</b>. The XMSG
        /// kernel symbol list (<c>SINTRAN/NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT</c>)
        /// defines the transported header as SEVEN words ending at XMCSM:
        /// </para>
        /// <para>
        /// Word layout, octal symbol values and the wire offset each lands on:
        ///  - <c>XMTHD=134</c> word 0 - wire 14-15.
        ///  - <c>XMSTA=135</c> word 1 - wire 16-17 (low byte = 5M* state, high byte = XF* options).
        ///  - <c>XMDSY=136</c> word 2 - wire 18-19.
        ///  - <c>XMDPT=137</c> word 3 - wire 20-21.
        ///  - <c>XMSSY=140</c> word 4 - wire 22-23.
        ///  - <c>XMSPT=141</c> word 5 - wire 24-25.
        ///  - <c>XMCSM=142</c> word 6 - wire 26-27, and the header ENDS here.
        /// </para>
        /// <para>
        /// <c>XM5HE=7</c> (words) and <c>XM5HL=16</c> octal = 14 bytes confirm the length, and
        /// <c>XMLEN=147</c> sits five words past XMCSM - far outside the transported header, so it
        /// is not the 16-bit length field this layout assumed at wire 30-31 either.
        /// The source carries the warning
        /// <c>"Next block is sent directly over link. Do NOT split up!"</c> against that block.
        /// </para>
        /// <para>
        /// So the true <c>XMCSM</c> is ONE word at wire 26-27, commented in the kernel as
        /// <c>"datagram checksum; if not checksum, then message size"</c>. Wire offsets 28-29 are
        /// the FIRST WORD OF THE MESSAGE BODY, not header.
        /// </para>
        /// <para>
        /// Confirmed on the wire, independently of the symbol file, across the whole capture
        /// corpus: Flags 2 (wire 10-11) equals the 16-bit XMCSM (wire 26-27) on <b>1449 of 1449</b>
        /// data frames - so the long-documented rule "Flags2 == XMCSM &gt;&gt; 16" was that exact
        /// equality seen through this wrong 32-bit split. And the word at 28-29 takes values that
        /// are plainly application-layer: <c>0x07F0</c>/<c>0x07A2</c>/<c>0x07C0</c>/<c>0x07D2</c>
        /// (the FA message types - see <c>FaMessageType</c>), <c>0x0041</c> XSLET,
        /// <c>0x014B</c> XSGSY, <c>0x0100</c> XRSOK.
        /// </para>
        /// <para>
        /// This is kept as a 32-bit field FOR NOW because splitting it correctly touches 108 call
        /// sites across 34 files and changes what callers pass (a single <c>0x00800141</c> becomes
        /// XMCSM <c>0x0080</c> plus body word <c>0x0141</c>). Treat the value as
        /// <c>(XMCSM &lt;&lt; 16) | firstBodyWord</c> until that refactor lands. Consequently
        /// <c>ControlService &gt;&gt; 16</c> IS the real XMCSM, and <c>&gt;&gt; 24</c> is its high
        /// byte - both are genuine quantities despite the misleading name.
        /// </para>
        /// <para>
        /// Doc: <c>SINTRAN/XMSG/DOC/XMSG-SUBHEADER-NAMED-FROM-SYMBOLS-2026-07-29.md</c> section 4
        /// (which raised the conflict) and
        /// <c>SINTRAN/XMSG/DOC/XMSG-CHANNEL-FORMULA-DIVERGES-ON-FILE-SERVER-TRAFFIC-2026-07-31.md</c>
        /// (which resolved it).
        /// </para>
        /// </remarks>
        public uint ControlService;

        /// <summary>
        /// The trailing payload bytes after the 19-byte sub-header (the TAD chain / parameter
        /// blocks). XMLEN (sub-header offset 18) is set from this array's length.
        /// </summary>
        public byte[] Payload;
    }
}
