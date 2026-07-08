//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: the firmware itself - reset flow, hardware init, main loop, and the
// interrupt handlers. Each method notes the 68000 address it models. Where the
// disassembly did not prove a step, the model marks it Unconfirmed.
//

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Behavioral model of the ENCOS 68000 server firmware. Drives the shared
    /// memory, LANCE and interrupt models to reproduce the reversed protocol.
    /// </summary>
    public sealed class NDEthernetIIFirmware
    {
        private readonly FirmwareTrace _trace;
        private readonly SharedMemory _mem;
        private readonly LanceControllerModel _lance;
        private readonly MfpControllerModel _mfp;
        private readonly InterruptController _irq;
        private readonly FirmwareCommandDispatcher _dispatcher;

        private bool _running;

        public NDEthernetIIFirmware(
            FirmwareTrace trace,
            SharedMemory mem,
            LanceControllerModel lance,
            MfpControllerModel mfp,
            InterruptController irq,
            FirmwareCommandDispatcher dispatcher)
        {
            _trace = trace;
            _mem = mem;
            _lance = lance;
            _mfp = mfp;
            _irq = irq;
            _dispatcher = dispatcher;
        }

        /// <summary>Latest result the 68000 posted for the host to read.</summary>
        public HostResult? LastResult { get; private set; }

        /// <summary>Pending command the ND-100 wrote (consumed on the next interrupt).</summary>
        public HostCommand? PendingCommand { get; set; }

        public bool Running => _running;

        /// <summary>
        /// reset_entry - 68000 @ 0x00001CFE. CONFIRMED.
        /// Initializes the monitor postbox, checks the warm-boot magic, installs
        /// the OPCOM handler pointer, brings up hardware, and enters the wait loop.
        /// </summary>
        public void ResetEntry()
        {
            _trace.Info("reset_entry @ 0x1CFE (SSP=0x5C8)");

            // Monitor postbox init: (0x40E)=1, (0x40C)=0, clear (0x406). CONFIRMED.
            _mem.WriteWord((uint)MonitorPostbox.Param, 1);
            _mem.WriteWord((uint)MonitorPostbox.Code, 0);
            _mem.WriteWord(0x0406, 0);

            // Warm-boot detection: cmpi.l #0x55555555,(0x4BA). CONFIRMED.
            uint magic = _mem.ReadLong(FirmwareDataAddresses.WarmBootMagic);
            if (magic == FirmwareDataAddresses.WarmBootMagicValue)
            {
                _trace.Info("warm boot detected (0x55555555 @ 0x4BA) - reporting restart");
                _mem.WriteLong(FirmwareDataAddresses.WarmBootMagic, 0);
                ushort n = _mem.ReadWord(FirmwareDataAddresses.WarmBootRestartCounter);
                _mem.WriteWord(FirmwareDataAddresses.WarmBootRestartCounter, (ushort)(n + 1));
            }

            // Clear MERRSTAT: move.b #0,(0xEF0040). CONFIRMED.
            _trace.IoWrite(IoAddresses.MerrStat, 0);

            InitializeHardware();

            // Arm the magic and enter the wait/dispatch state.
            _mem.WriteLong(FirmwareDataAddresses.WarmBootMagic, FirmwareDataAddresses.WarmBootMagicValue);
            _running = true;
            _trace.Info("reset_entry complete - firmware armed, waiting for interrupts (STOP #0x2500)");
        }

        /// <summary>
        /// InitializeHardware - hardware bring-up cluster @ 0x000047B0-0x00004B24.
        /// CONFIRMED for transceiver power + LANCE; MFP/timer are Unconfirmed here.
        /// </summary>
        public void InitializeHardware()
        {
            InitializeMfp();
            InitializeLance();
        }

        /// <summary>
        /// InitializeMfp - init_mfp_registers (68000 @ 0x396A, called from
        /// reset_entry). CONFIRMED: programs the MC68901 at base 0xEF00C0 and
        /// writes VR = 0x40 at register offset 0x17.
        /// </summary>
        public void InitializeMfp()
        {
            _mfp.VectorBase = 0x40; // 0x396A writes VR=0x40 (CONFIRMED)
            _trace.Info("init_mfp_registers @ 0x396A: MFP base 0xEF00C0 programmed, VR=0x40");
            _trace.IoWrite(IoAddresses.MfpBase + 0x17, 0x40); // VR
        }

        /// <summary>
        /// InitializeLance - LANCE CSR bring-up. CONFIRMED (0x00004ABE block).
        /// Delegated to the INITLANCE command handler to keep one source of truth.
        /// </summary>
        public void InitializeLance()
        {
            _dispatcher.Dispatch(new HostCommand(0x0000, 0, 0)); // INITLANCE
        }

        /// <summary>
        /// MainLoopStep - the production firmware is event/postbox driven; the
        /// reset path ends in STOP #0x2500. A "step" here services any pending
        /// command. The classic poll loop of the brief is NOT present in this image.
        /// </summary>
        public void MainLoopStep()
        {
            if (PendingCommand is HostCommand cmd)
            {
                PendingCommand = null;
                HostResult result = _dispatcher.Dispatch(cmd);
                PostResultToHost(result);
            }
        }

        /// <summary>Per-channel handlers registered by the firmware (nd_channel_context_table).</summary>
        private readonly System.Action?[] _ndChannelHandlers = new System.Action?[FirmwareDataAddresses.NdChannelCount];

        /// <summary>
        /// Register a handler for one of the 8 ND-100 doorbell channels (models the
        /// runtime population of nd_channel_context_table / nd_channel_struct_table).
        /// </summary>
        public void RegisterNdChannel(int channel, System.Action handler)
        {
            _ndChannelHandlers[channel] = handler;
        }

        /// <summary>
        /// OnNdHostInterrupt - nd_host_interrupt_handler (68000 @ 0x250E), wired to
        /// MFP GPIP6. CONFIRMED: scans the 8 channel flag words at nd_channel_flags
        /// (0x0B56); for each set flag it clears the flag and schedules that
        /// channel's registered handler. Mirrors the loop D1 = 0xE..0 by 2.
        /// </summary>
        public void OnNdHostInterrupt()
        {
            _trace.Interrupt("MFP GPIP6 (vector 0x4E)", "nd_host_interrupt_handler @ 0x250E");

            for (int ch = 0; ch < FirmwareDataAddresses.NdChannelCount; ch++)
            {
                uint flagAddr = FirmwareDataAddresses.NdChannelFlags + (uint)ch * 2;
                if (_mem.ReadWord(flagAddr) == 0)
                    continue;

                _mem.WriteWord(flagAddr, 0); // clear the flag (handler @ 0x2532)
                _trace.Info($"ND channel {ch} doorbell -> dispatch handler");
                _ndChannelHandlers[ch]?.Invoke();
            }

            // Legacy single-command path still supported for callers that use it.
            MainLoopStep();
        }

        /// <summary>
        /// Raised for each frame the firmware pulls out of the RX ring
        /// (RCVCOMPLETE). Args: full frame bytes (without FCS) and the 6-byte
        /// destination MAC the firmware read from the buffer.
        /// </summary>
        public event System.Action<byte[], byte[]>? OnFrameReceived;

        /// <summary>
        /// OnLanceInterrupt - LANCE level-2 handler. The raw ISR (0x211C) is a thin
        /// PLANC trampoline that schedules the event handlers; the real work is
        /// RCVCOMPLETE (0x5C42) on RINT and XMTCOMPLETE (0x61D2) on TINT. We model
        /// that directly: read CSR0, consume received frames, reclaim TX buffers.
        /// </summary>
        public void OnLanceInterrupt()
        {
            ushort csr0 = _lance.Csr[0];
            _trace.Info($"lance_irq_handler @ 0x211C/0x1E9A: CSR0=0x{csr0:X4}");

            if ((csr0 & LanceCsr.Csr0_Rint) != 0)
                ProcessRxComplete();

            if ((csr0 & LanceCsr.Csr0_Tint) != 0)
                ProcessTxComplete();

            _lance.ClearInterrupt();
        }

        /// <summary>
        /// ProcessRxComplete - models RCVCOMPLETE (68000 @ 0x5C42). Walks the RX
        /// ring from the consumer index (0x18004); for each descriptor the chip has
        /// released (OWN=0) with a non-zero byte count, it reads the buffer address
        /// (RMD1-high : RMD0), takes length = (RMD3 &amp; 0xFFF) - 4 (strips the FCS),
        /// reads the destination MAC from the first 6 bytes, raises OnFrameReceived,
        /// advances the consumer index (mod 128), and re-arms the buffer (OWN=1)
        /// for reuse (RCVRINGAPPEND).
        /// </summary>
        public void ProcessRxComplete()
        {
            for (int guard = 0; guard < LanceRing.RxDescCount; guard++)
            {
                ushort cons = _mem.ReadWord(LanceRing.RxHeader + 4);
                int idx = cons & (LanceRing.RxDescCount - 1);
                uint desc = LanceRing.RxDescBase + (uint)idx * LanceRing.DescSize;

                ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
                if ((flags & LanceRing.Own) != 0)
                    break; // chip still owns this one - nothing more to consume

                ushort mcnt = (ushort)(_mem.ReadWord(desc + LanceRing.Rmd3_StatusWord) & LanceRing.McntMask);
                if (mcnt == 0)
                    break; // no frame here

                uint bufLow = _mem.ReadWord(desc + LanceRing.Rmd0_LadrWord);
                uint bufHigh = _mem.ReadByte(desc + LanceRing.Rmd1_HadrByte);
                uint bufAddr = (bufHigh << 16) | bufLow;
                short negBcnt = (short)_mem.ReadWord(desc + LanceRing.Rmd2_BcntWord);
                int bufSize = -negBcnt;

                int frameLen = mcnt - LanceRing.FcsBytes; // firmware subtracts the 4-byte FCS
                if (frameLen < 0) frameLen = 0;

                var frame = new byte[frameLen];
                for (int i = 0; i < frameLen; i++)
                    frame[i] = _mem.ReadByte(bufAddr + (uint)i);

                var destMac = new byte[6];
                for (int i = 0; i < 6 && i < frameLen; i++)
                    destMac[i] = frame[i];

                // Software address check, matching RCVCOMPLETE at 0x5D5E: unicast
                // frames (group bit 0) are compared byte-for-byte against our MAC at
                // 0x1885E; group/multicast frames are handled separately. Only
                // frames "for us" are delivered up to the XMSG/port layer.
                bool group = (destMac[0] & 0x01) != 0;
                bool forUs = group || MacMatchesOurs(destMac);
                _trace.Info($"RCVCOMPLETE @ 0x5C42: desc[{idx}] frame {frameLen}B (mcnt {mcnt}) dest {Mac(destMac)} forUs={forUs} group={group}");

                if (forUs)
                {
                    OnFrameReceived?.Invoke(frame, destMac);
                    PostReceivedFrameToHost(frame); // notify the ND-100 (postbox + SCIP)
                }

                // Advance consumer index, then re-arm the same buffer for the chip.
                _mem.WriteWord(LanceRing.RxHeader + 4, (ushort)((cons + 1) & (LanceRing.RxDescCount - 1)));
                _lance.AppendRxBuffer(bufAddr, bufSize);
            }
        }

        /// <summary>
        /// ProcessTxComplete - models XMTCOMPLETE (68000 @ 0x61D2). Walks the TX
        /// ring from its consumer index (0x1840C); for each descriptor the chip has
        /// released (OWN=0), it reclaims the buffer and advances the index. Here we
        /// just advance past released descriptors and log it.
        /// </summary>
        public void ProcessTxComplete()
        {
            for (int guard = 0; guard < LanceRing.RxDescCount; guard++)
            {
                ushort cons = _mem.ReadWord(LanceRing.TxHeader + 4);
                int idx = cons & (LanceRing.RxDescCount - 1);
                uint desc = LanceRing.TxDescBase + (uint)idx * LanceRing.DescSize;

                ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
                if ((flags & LanceRing.Own) != 0)
                    break; // chip still transmitting

                ushort bcnt = _mem.ReadWord(desc + LanceRing.Rmd2_BcntWord);
                if (bcnt == 0)
                    break; // empty slot - nothing queued here

                _mem.WriteWord(desc + LanceRing.Rmd2_BcntWord, 0); // mark reclaimed
                _mem.WriteWord(LanceRing.TxHeader + 4, (ushort)((cons + 1) & (LanceRing.RxDescCount - 1)));
                _trace.Info($"XMTCOMPLETE @ 0x61D2: reclaimed TX desc[{idx}]");
            }
        }

        /// <summary>Compare a destination MAC against our address at 0x1885E (RCVCOMPLETE 0x5D5E).</summary>
        private bool MacMatchesOurs(byte[] mac)
        {
            for (uint i = 0; i < 6; i++)
                if (mac[i] != _mem.ReadByte(FirmwareDataAddresses.LanceMacAddress + i))
                    return false;
            return true;
        }

        /// <summary>
        /// The last XMSG message the firmware built for a received frame
        /// (XMRECEIVER 0xBED8), for inspection/testing.
        /// </summary>
        public XmsgMessage? LastXmsgMessage { get; private set; }

        /// <summary>
        /// Notify the ND-100 that a frame arrived. Models XMRECEIVER (0xBED8): build
        /// the XMSG message (flags 0x4000, id from xmsg_node_id 0x1E21A, subtype 4,
        /// payload = the frame), hand it to the XMSG layer (XMPFRRE 0x10C4C), which
        /// posts it through the postbox ring and rings SCIP so the ND-100 takes
        /// INT12. CONFIRMED header fields; the exact on-wire XMSG framing the ND-100
        /// decodes is documented separately - modelled here as the message + doorbell.
        /// </summary>
        public void PostReceivedFrameToHost(byte[] frame)
        {
            var msg = new XmsgMessage
            {
                Flags = 0x00004000,                                  // XMRECEIVER bset #14
                NodeId = _mem.ReadLong(FirmwareDataAddresses.XmsgNodeId), // *(0x1E21A)
                Subtype = 4,                                         // XMRECEIVER (0x24,A3)=4
                Payload = frame,
            };
            LastXmsgMessage = msg;
            _trace.Info($"XMRECEIVER @ 0xBED8: XMSG msg flags=0x{msg.Flags:X} id=0x{msg.NodeId:X} subtype={msg.Subtype} payload={frame.Length}B -> XMPFRRE -> postbox + SCIP");
            SignalHostViaScip();
        }

        private static string Mac(byte[] m) =>
            $"{m[0]:X2}:{m[1]:X2}:{m[2]:X2}:{m[3]:X2}:{m[4]:X2}:{m[5]:X2}";

        /// <summary>
        /// OnMfpInterrupt - level-3 vectored dispatch by MFP vector number.
        /// </summary>
        public void OnMfpInterrupt(int vector)
        {
            switch (vector)
            {
                case MfpVectors.Nd100Request: OnNdHostInterrupt(); break;
                case MfpVectors.LanceMemError: _trace.Info("MFP 107: LANCE mem error"); break;
                case MfpVectors.WriteViolation: OnBusError(0); break;
                case MfpVectors.RealTimeClock: OnRtcTick(); break;
                default: _trace.Unconfirmed($"MFP vector {vector} handler not traced in this image"); break;
            }
        }

        /// <summary>Free-running tick counter, mirrors the firmware's 0xFC2/0xFCA words.</summary>
        public uint RtcTicks { get; private set; }

        /// <summary>
        /// OnRtcTick - rtc_timer_isr (68000 @ 0x3A68). CONFIRMED. Increments the
        /// tick counters and (in the real firmware) fires expired entries from the
        /// timer queue at 0xFD6. Here we just advance the tick and log it.
        /// </summary>
        public void OnRtcTick()
        {
            RtcTicks++;
            _trace.Info($"rtc_timer_isr @ 0x3A68: tick={RtcTicks} (firmware bumps 0xFC2/0xFCA, fires timer queue 0xFD6)");
        }

        /// <summary>
        /// DispatchXroutMessage - maybe_xrout_msg_dispatch (68000 @ 0x9924).
        /// CONFIRMED table, HYPOTHESIS semantics for handlers 2..7. Selects one of
        /// the 8 handler blocks by the message type index (0..7). Index &gt; max
        /// (0x07) is rejected exactly as the firmware does.
        /// </summary>
        public void DispatchXroutMessage(int typeIndex)
        {
            if (typeIndex < 0 || typeIndex > 7)
            {
                _trace.Info($"XROUT type {typeIndex} out of range (max 7) - rejected");
                return;
            }

            uint handler = XroutHandlers.Table[typeIndex];
            _trace.Info($"XROUT dispatch: type {typeIndex} -> handler 0x{handler:X6}");

            switch (typeIndex)
            {
                case 0: _trace.Info("xrout_handler0_register: set type 0x3000, allocate connection id"); break;
                case 1: _trace.Info("xrout_handler1_deregister: find and remove connection"); break;
                default: _trace.Unconfirmed($"xrout_handler{typeIndex} @ 0x{handler:X6}: semantics not fully reversed"); break;
            }
        }

        /// <summary>
        /// Crc32 - Ethernet CRC-32 as computed by calc_crc32 (68000 @ 0x4660).
        /// CONFIRMED algorithm: reflected, polynomial 0xEDB88320, **init 0**, no final
        /// XOR (the working word is seeded with 0 at 0x466E). This matches the
        /// firmware, NOT the standard CRC-32 test vector: standard CRC-32 uses init
        /// 0xFFFFFFFF + final XOR and gives 0xCBF43926 for "123456789"; this routine
        /// (init 0, no final XOR) gives a different value on purpose.
        /// </summary>
        public static uint Crc32(System.ReadOnlySpan<byte> data)
        {
            uint crc = 0; // firmware seeds the working word with 0 at 0x466E
            for (int i = 0; i < data.Length; i++)
            {
                crc ^= data[i];
                for (int bit = 0; bit < 8; bit++)
                {
                    // eori.l #0x6DB88320 when the low bit is set (0xEDB88320 reflected form)
                    if ((crc & 1) != 0)
                        crc = (crc >> 1) ^ 0xEDB88320u;
                    else
                        crc >>= 1;
                }
            }
            return crc;
        }

        /// <summary>
        /// OnBusError - bus-error vector. CONFIRMED host-side that startup probes
        /// memory and the first bus error marks the probe complete; the exact probe
        /// loop is Unconfirmed in this image.
        /// </summary>
        public void OnBusError(uint faultAddress)
        {
            _trace.Unconfirmed($"bus error at 0x{faultAddress:X6} - startup memory-probe recovery not traced in this image");
        }

        /// <summary>
        /// PostResultToHost - fill the result staging area and ring the doorbell.
        /// </summary>
        public void PostResultToHost(HostResult result)
        {
            LastResult = result;
            _trace.MailboxWrite("ResultStatusCode", result.StatusCode);
            SignalHostViaScip();
        }

        /// <summary>
        /// SignalHostViaScip - post_and_signal_nd100_scip @ 0x00001A48. CONFIRMED.
        /// Bumps the monitor counters and writes SCIP 0xEF0080 (INT12 to ND-100).
        /// </summary>
        public void SignalHostViaScip()
        {
            ushort c0 = _mem.ReadWord((uint)MonitorPostbox.Counter);
            _mem.WriteWord((uint)MonitorPostbox.Counter, (ushort)(c0 + 1));
            ushort c1 = _mem.ReadWord((uint)MonitorPostbox.Counter2);
            _mem.WriteWord((uint)MonitorPostbox.Counter2, (ushort)(c1 + 1));
            _irq.WriteScip(IoAddresses.Scip); // 0x1A5C move.b #1,0xEF0080
        }

        public void Halt() => _running = false;
    }
}
