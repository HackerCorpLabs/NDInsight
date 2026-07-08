//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: all constants that were CONFIRMED from the disassembly of
// encos-ser-all-banks-68k.bin, plus a clearly-marked provisional set. Addresses
// with a "Possible" prefix were NOT confirmed in the loaded (all-banks) image;
// they come from the bank-0 diagnostic firmware or the task brief.
//

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Firmware code addresses (68000 address space). CONFIRMED entries were
    /// verified against the loaded image (reset vector, PLANC symbol table, or
    /// direct disassembly). "Possible*" entries were NOT confirmed here.
    /// </summary>
    public static class FirmwareAddresses
    {
        // ---- CONFIRMED in encos-ser-all-banks-68k.bin ----

        /// <summary>Reset entry (from vector 1 at 0x000004). CONFIRMED.</summary>
        public const uint ResetEntry = 0x00001CFE;

        /// <summary>Initial supervisor stack pointer (vector 0). CONFIRMED.</summary>
        public const uint InitialSsp = 0x000005C8;

        /// <summary>Sets the 0x412 monitor request flag. CONFIRMED.</summary>
        public const uint NdMonitorSetFlag = 0x00001A30;

        /// <summary>Fills monitor postbox counters and writes SCIP 0xEF0080. CONFIRMED.</summary>
        public const uint PostAndSignalNd100Scip = 0x00001A48;

        /// <summary>Saves the CPU register frame to 0x454. CONFIRMED.</summary>
        public const uint SaveCpuContextTo0x454 = 0x00001A66;

        /// <summary>LANCE init routine (symbol INITLANCE). CONFIRMED name.</summary>
        public const uint InitLance = 0x000048EA;

        /// <summary>LANCE CSR programming block inside/around INITLANCE. CONFIRMED.</summary>
        public const uint LanceCsrSetupBlock = 0x00004ABE;

        /// <summary>Fatal error handler (symbol FATALERROR). CONFIRMED.</summary>
        public const uint FatalError = 0x00004C26;

        /// <summary>Append to RX ring (symbol RCVRINGAPP). CONFIRMED.</summary>
        public const uint RcvRingAppend = 0x00005B60;

        /// <summary>TX kick site: CSR0 = 0x0048 (INEA|TDMD). CONFIRMED.</summary>
        public const uint LanceTxKick = 0x0000616E;

        /// <summary>XMSG postbox producer + SCIP mirror doorbell. HYPOTHESIS (strong).</summary>
        public const uint XmsgPostboxSendRing = 0x0000EACC;

        /// <summary>XMSG receiver (symbol XMRECEIVER). HYPOTHESIS.</summary>
        public const uint XmReceiver = 0x0000BED8;

        /// <summary>Create XMSG port (symbol PORTCREATE). HYPOTHESIS.</summary>
        public const uint PortCreate = 0x0000E73C;

        /// <summary>XMSG send (symbol XMPSEND). HYPOTHESIS.</summary>
        public const uint XmpSend = 0x000106F0;

        /// <summary>Postbox scheduler init (symbol POSIINITIA). CONFIRMED name.</summary>
        public const uint PosiInitialize = 0x00011732;

        /// <summary>Postbox scheduler start (symbol POSISTART). CONFIRMED name.</summary>
        public const uint PosiStart = 0x0001179C;

        /// <summary>Postbox append (symbol POSIAPPEND). CONFIRMED name.</summary>
        public const uint PosiAppend = 0x00011DC4;

        // ---- CONFIRMED by later disassembly (function-renaming pass) ----

        /// <summary>
        /// MFP (MC68901) register init, called from reset_entry (jsr 0x396A).
        /// CONFIRMED: programs the MFP at base 0xEF00C0 and writes VR = 0x40 at
        /// register offset 0x17. This is the REAL MFP setup in this image (the
        /// brief's 0x25F0 was the wrong, diagnostic-firmware address).
        /// </summary>
        public const uint InitMfpRegisters = 0x0000396A;

        /// <summary>
        /// RTC / timer interrupt service routine. CONFIRMED: increments tick
        /// counters (0xFC2/0xFCA), fires expired timers from a queue at 0xFD6,
        /// ends in RTE. Wired to the MFP Timer C source (vector 105).
        /// </summary>
        public const uint RtcTimerIsr = 0x00003A68;

        /// <summary>
        /// Ethernet CRC-32. CONFIRMED: bit-by-bit loop with reflected polynomial
        /// 0x6DB88320 (the 0xEDB88320 variant) over a buffer.
        /// </summary>
        public const uint CalcCrc32 = 0x00004660;

        /// <summary>LANCE TX kick site: CSR0 = 0x0048 (INEA|TDMD). CONFIRMED.</summary>
        public const uint LanceTxKickSite = 0x0000616E;

        /// <summary>
        /// XROUT message-type dispatcher. CONFIRMED: validates a 4-bit type code
        /// against XROUT_dispatch_maxindex, then jumps through the 8-entry table
        /// at <see cref="FirmwareDataAddresses.XroutDispatchTable"/>.
        /// </summary>
        public const uint XroutMsgDispatch = 0x00009924;

        // ---- Provisional: NOT confirmed in this image (bank-0 diagnostic firmware) ----

        /// <summary>Brief anchor: MFP setup. NOT valid in this image (lands on an RTE fragment). Use <see cref="InitMfpRegisters"/> (0x396A).</summary>
        public const uint PossibleMfpSetup = 0x000025F0;

        /// <summary>Brief anchor: timer init. NOT valid in this image.</summary>
        public const uint PossibleTimerInit = 0x00002598;

        /// <summary>Brief anchor: test dispatch loop. NOT valid in this image.</summary>
        public const uint PossibleTestDispatchLoop = 0x000030CA;

        /// <summary>Brief anchor: MFP reinit after RESET. NOT valid in this image.</summary>
        public const uint PossibleMfpReinit = 0x00003338;

        /// <summary>Brief anchor: hardware init/dispatch. NOT valid in this image (TRAP #2 fragment).</summary>
        public const uint PossibleHwInitAndDispatch = 0x00004610;

        /// <summary>Brief anchor: TRAP/vector table init. NOT valid in this image.</summary>
        public const uint PossibleTrapVectorInit = 0x000057F2;

        /// <summary>Brief anchor: test dispatch table. NOT confirmed in this image.</summary>
        public const uint PossibleTestDispatchTable = 0x00000948;
    }

    /// <summary>
    /// Shared-DRAM data addresses that are CONFIRMED in this image.
    /// </summary>
    public static class FirmwareDataAddresses
    {
        /// <summary>Monitor/console postbox block base (68000 &lt;-&gt; ND-100). CONFIRMED.</summary>
        public const uint MonitorPostbox = 0x0000040A;

        /// <summary>CPU register dump frame written on every trap. CONFIRMED.</summary>
        public const uint RegisterDumpFrame = 0x00000454;

        /// <summary>Warm-boot sentinel; equals 0x55555555 after a caught trap. CONFIRMED.</summary>
        public const uint WarmBootMagic = 0x000004BA;

        /// <summary>Warm-boot restart counter. CONFIRMED.</summary>
        public const uint WarmBootRestartCounter = 0x000004BE;

        /// <summary>LANCE Am7990 initialization block base. CONFIRMED (pointer).</summary>
        public const uint LanceInitBlock = 0x00018810;

        /// <summary>
        /// Ethernet MAC address source (6 bytes). CONFIRMED: INITLANCE copies
        /// these into the init block PADR field (LanceInitBlock+2). Zero in the
        /// static image - populated at runtime, most likely written by the
        /// ND-100/SINTRAN host into the shared window during bring-up.
        /// </summary>
        public const uint LanceMacAddress = 0x0001885E;

        /// <summary>LANCE RX descriptor ring (RCVRING). CONFIRMED (symbol table).</summary>
        public const uint RcvRing = 0x00018000;

        /// <summary>LANCE TX descriptor ring (XMTRING). CONFIRMED (symbol table).</summary>
        public const uint XmtRing = 0x00018408;

        /// <summary>
        /// XROUT dispatcher max type index (byte). CONFIRMED = 0x07 (8 handlers).
        /// </summary>
        public const uint XroutDispatchMaxIndex = 0x0001D16E;

        /// <summary>
        /// XROUT dispatch table: 8 x 32-bit handler pointers. CONFIRMED.
        /// See <see cref="XroutHandlers"/> for the decoded targets.
        /// </summary>
        public const uint XroutDispatchTable = 0x0001D170;

        /// <summary>Global XMSG node id / magic used in built messages (read by XMRECEIVER). CONFIRMED addr.</summary>
        public const uint XmsgNodeId = 0x0001E21A;

        /// <summary>
        /// ND-100 -&gt; 68000 doorbell channel flags: 8 words at 0x0B56. CONFIRMED
        /// (nd_host_interrupt_handler @ 0x250E scans these). The ND-100 sets flag
        /// [ch] then raises MFP GPIP6; the 68000 dispatches the channel's handler.
        /// </summary>
        public const uint NdChannelFlags = 0x00000B56;

        /// <summary>Number of ND-100 doorbell channels. CONFIRMED (loop 0xE..0 by 2 = 8 words).</summary>
        public const int NdChannelCount = 8;

        /// <summary>Magic value stored at <see cref="WarmBootMagic"/>. CONFIRMED.</summary>
        public const uint WarmBootMagicValue = 0x55555555;

        /// <summary>
        /// LANCE RX buffer size. CONFIRMED = 0x5F0 = 1520 bytes (max Ethernet
        /// frame), from append_rx_buffers_to_ring at 0x5BCA.
        /// </summary>
        public const int RxBufferSize = 0x5F0; // 1520 bytes
    }

    /// <summary>
    /// XROUT message-type dispatch handlers (targets of the 8-entry jump table at
    /// <see cref="FirmwareDataAddresses.XroutDispatchTable"/>). CONFIRMED addresses;
    /// the exact semantics of handlers 2..7 are HYPOTHESIS. Index = high nibble of
    /// the message type byte. Entries 3 and 4 share the same handler.
    /// </summary>
    public static class XroutHandlers
    {
        public const uint Handler0_Register = 0x000099E2;   // set type 0x3000, alloc connection id
        public const uint Handler1_Deregister = 0x00009A56; // subtype==2 -> find/remove connection
        public const uint Handler2 = 0x00009AEA;
        public const uint Handler3_4 = 0x00009BE6;          // shared by indices 3 and 4
        public const uint Handler5 = 0x00009CE0;
        public const uint Handler6 = 0x00009CFE;
        public const uint Handler7 = 0x00009D8E;

        /// <summary>The 8 table entries in order (indices 0..7).</summary>
        public static readonly uint[] Table =
        {
            Handler0_Register, Handler1_Deregister, Handler2, Handler3_4,
            Handler3_4, Handler5, Handler6, Handler7,
        };
    }

    /// <summary>
    /// I/O register addresses (68000 side). CONFIRMED from NDBusEthernetII.cs and
    /// observed absolute-long accesses in this image.
    /// </summary>
    public static class IoAddresses
    {
        public const uint Proff = 0xEF0010;      // protection off (W)
        public const uint Modcr = 0xEF0020;      // mode control (R/W)
        public const uint MerrStat = 0xEF0040;   // memory/parity error status (R); cleared at boot
        public const uint Earen = 0xEF0060;      // error-address latch (R)
        public const uint Scip = 0xEF0080;       // write -> INT12 to ND-100 (CONFIRMED)
        public const uint ScipMirror = 0xEF0180; // alternate SCIP doorbell (CONFIRMED)
        public const uint LanceRdp = 0xEF00A0;   // LANCE register data port (CONFIRMED)
        public const uint LanceRap = 0xEF00A2;   // LANCE register address port (CONFIRMED)
        public const uint Xcvpw = 0xEF00A8;      // transceiver 12V power (CONFIRMED)
        public const uint LanReset = 0xEF00B0;   // LANCE hardware reset (region present)
        public const uint EthStat = 0xEF00B8;    // hardware status (R)
        public const uint MfpBase = 0xEF00C0;    // MFP MC68901 base (odd displacements)
    }

    /// <summary>
    /// LANCE Am7990 receive/transmit ring geometry. CONFIRMED from RCVRINGAPPEND
    /// (0x5B60) and RCVCOMPLETE (0x5C42): descriptors are 8 bytes; the RX ring
    /// header is at 0x18000 (count +0, producer index +2, consumer index +4) and
    /// the 128 descriptors start at 0x18008 (128 * 8 = 0x400, ending exactly at
    /// the TX ring 0x18408). Indices wrap modulo 128.
    /// </summary>
    public static class LanceRing
    {
        public const uint RxHeader = 0x00018000;      // +0 free count, +2 producer idx, +4 consumer idx
        public const uint RxDescBase = 0x00018008;    // first RMD
        public const int RxDescCount = 0x80;          // 128 descriptors (index wraps mod 128)
        public const int DescSize = 8;                // RMD/TMD are 8 bytes each

        public const uint TxHeader = 0x00018408;      // XMTRING: +0 count, +2 producer idx
        public const uint TxDescBase = 0x00018410;    // first TMD (confirmed XMTRINGAPP 0x6054)
        public const int MinFrameLen = 0x3C;          // 60 bytes: XMTRINGAPP pads short frames when 0x18886==4

        // RMD/TMD field offsets within an 8-byte descriptor.
        public const int Rmd0_LadrWord = 0; // low 16 bits of the 24-bit buffer address
        public const int Rmd1_FlagsWord = 2; // high byte = flags (OWN/ERR/STP/ENP...), low byte = HADR
        public const int Rmd1_HadrByte = 3; // high 8 bits of the 24-bit buffer address
        public const int Rmd2_BcntWord = 4; // buffer length as two's complement (-len)
        public const int Rmd3_StatusWord = 6; // RX: MCNT (message length, 12 bits); TX: status

        // RMD1/TMD1 flag bits (high byte of the word at offset +2).
        public const ushort Own = 0x8000; // OWN: 1=chip owns, 0=host owns (CONFIRMED btst #15)
        public const ushort Err = 0x4000; // ERR (CONFIRMED btst #14)
        public const ushort Fram = 0x2000; // framing error
        public const ushort Oflo = 0x1000; // overflow
        public const ushort Crc = 0x0800;  // CRC error
        public const ushort Buff = 0x0400; // buffer error
        public const ushort Stp = 0x0200;  // start of packet
        public const ushort Enp = 0x0100;  // end of packet

        public const ushort McntMask = 0x0FFF; // RCVCOMPLETE masks RMD3 with 0xFFF
        public const int FcsBytes = 4;         // RCVCOMPLETE subtracts 4 (the Ethernet FCS)
    }

    /// <summary>
    /// LANCE Am7990 CSR numbers written through RAP/RDP. Standard Am7990.
    /// </summary>
    public static class LanceCsr
    {
        public const ushort Csr0_Control = 0; // status/control
        public const ushort Csr1_IadrLow = 1; // init block address low
        public const ushort Csr2_IadrHigh = 2; // init block address high
        public const ushort Csr3_BusControl = 3; // bus control (BSWP)

        // CSR0 bit values observed / standard
        public const ushort Csr0_Init = 0x0001; // INIT (CONFIRMED written)
        public const ushort Csr0_Strt = 0x0002; // STRT
        public const ushort Csr0_Stop = 0x0004; // STOP
        public const ushort Csr0_Tdmd = 0x0008; // transmit demand (CONFIRMED in 0x0048)
        public const ushort Csr0_Inea = 0x0040; // interrupt enable (CONFIRMED in 0x0048)
        public const ushort Csr0_Rint = 0x0400; // receive interrupt
        public const ushort Csr0_Tint = 0x0200; // transmit interrupt
        public const ushort Csr0_Idon = 0x0100; // initialization done (bit 8)
        public const ushort Csr0_Miss = 0x1000; // missed packet, no RX buffer available (bit 12)
        public const ushort Csr0_Rxon = 0x0020; // receiver on (bit 5)
        public const ushort Csr0_Txon = 0x0010; // transmitter on (bit 4)

        /// <summary>CSR3 = 0x0004 (BSWP) written by INITLANCE for 68000 big-endian. CONFIRMED.</summary>
        public const ushort Csr3_Bswp = 0x0004;

        /// <summary>CSR0 write to start transmit: INEA|TDMD. CONFIRMED at 0x616E.</summary>
        public const ushort Csr0_TxKick = 0x0048;
    }

    /// <summary>
    /// MC68901 MFP vectored interrupt numbers (level 3). CONFIRMED host-side.
    /// </summary>
    public static class MfpVectors
    {
        public const byte WriteViolation = 117;   // GPIP I7
        public const byte Nd100Request = 116;     // GPIP I6 (ND-100 -> 68000)
        public const byte UsartRxFull = 114;
        public const byte UsartRxError = 113;
        public const byte UsartTxEmpty = 112;
        public const byte UsartTxError = 111;
        public const byte LanceMemError = 107;    // GPIP I5
        public const byte RealTimeClock = 105;    // Timer C
    }

    /// <summary>
    /// 68000 interrupt levels for this controller. CONFIRMED host-side.
    /// </summary>
    public static class Cpu68kLevels
    {
        public const int Lance = 2;
        public const int Mfp = 3;         // vectored
        public const int PtcConsole = 4;
        public const int ParityError = 5;
        public const int NdOpcom = 6;     // vector 0x1E (addr 0x78)
        public const int PowerLowNmi = 7;
    }

    /// <summary>
    /// Result / status codes. The production server firmware does not expose the
    /// diagnostic RESULT_* codes; these are provisional and marked Unconfirmed
    /// where used.
    /// </summary>
    public static class ResultCodes
    {
        public const ushort Ok = 0x0000;
        public const ushort Unimplemented = 0xFFFF; // model sentinel, not a firmware value
    }
}
