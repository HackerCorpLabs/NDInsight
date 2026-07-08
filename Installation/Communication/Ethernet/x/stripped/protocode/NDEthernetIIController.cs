//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: the top-level model. It wires shared memory, the firmware model,
// the LANCE and MFP models and the interrupt controller together and exposes a
// small host-facing surface (the ND-100's view of the controller).
//

using System;

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Top-level ND Ethernet II controller behavioral model. This is the object a
    /// host (or a unit test) drives: reset it, release it, hand it commands and
    /// Ethernet frames, and read back results.
    /// </summary>
    public sealed class NDEthernetIIController
    {
        private readonly FirmwareTrace _trace;
        private readonly SharedMemory _mem;
        private readonly LanceControllerModel _lance;
        private readonly MfpControllerModel _mfp;
        private readonly InterruptController _irq;
        private readonly FirmwareCommands _commands;
        private readonly FirmwareCommandDispatcher _dispatcher;
        private readonly NDEthernetIIFirmware _firmware;

        private bool _inReset = true;

        /// <summary>Raised when the 68000 rings the ND-100 doorbell (INT12).</summary>
        public event Action? OnInterruptToNd100;

        /// <summary>
        /// Raised for each Ethernet frame the firmware pulls out of the RX ring
        /// (RCVCOMPLETE). Args: frame bytes (FCS stripped) and the destination MAC.
        /// </summary>
        public event Action<byte[], byte[]>? OnFrameReceived;

        public NDEthernetIIController(Action<string>? traceSink = null)
        {
            _trace = new FirmwareTrace(traceSink);
            _mem = new SharedMemory(_trace);
            _mfp = new MfpControllerModel(_trace);
            _irq = new InterruptController(_trace, _mfp);
            _lance = new LanceControllerModel(_trace, _mem);
            _commands = new FirmwareCommands(_trace, _mem, _lance, _irq);
            _dispatcher = new FirmwareCommandDispatcher(_commands, _trace);
            _firmware = new NDEthernetIIFirmware(_trace, _mem, _lance, _mfp, _irq, _dispatcher);

            _irq.OnScipToNd100 += () => OnInterruptToNd100?.Invoke();
            _firmware.OnFrameReceived += (frame, mac) => OnFrameReceived?.Invoke(frame, mac);
        }

        /// <summary>Shared DRAM (ND-100 loads firmware here before release-from-reset).</summary>
        public SharedMemory Memory => _mem;

        /// <summary>The firmware model (for inspection/testing).</summary>
        public NDEthernetIIFirmware Firmware => _firmware;

        /// <summary>The LANCE model (for inspection/testing and RX-ring setup).</summary>
        public LanceControllerModel Lance => _lance;

        /// <summary>Master Clear: hold the 68000 in reset and clear the peripherals.</summary>
        public void Reset()
        {
            _inReset = true;
            _mfp.Reset();
            _lance.Reset();
            _irq.Reset();
            _firmware.Halt();
            _trace.Info("controller Reset (68000 held in reset)");
        }

        /// <summary>
        /// Release the 68000 from reset. On the real card the ND-100 has already
        /// loaded all code/data into DRAM; the 68000 now fetches vectors and runs.
        /// </summary>
        public void ReleaseFromReset()
        {
            _inReset = false;
            _trace.Info("release-from-reset: 68000 fetches vectors at 0x0 (PC=0x1CFE)");
            _firmware.ResetEntry();
        }

        /// <summary>ND-100 posts a command (writes shared memory + requests interrupt).</summary>
        public void HostWriteCommand(HostCommand command)
        {
            if (_inReset)
            {
                _trace.Info("HostWriteCommand ignored - controller in reset");
                return;
            }
            _firmware.PendingCommand = command;
            HostInterrupt68000();
        }

        /// <summary>ND-100 reads the last result the 68000 posted (null if none).</summary>
        public HostResult? HostReadResult()
        {
            HostResult? r = _firmware.LastResult;
            if (r is HostResult res)
                _trace.MailboxRead("ResultStatusCode", res.StatusCode);
            return r;
        }

        /// <summary>ND-100 raises its interrupt request (Control Word bit 2 -> MFP I6).</summary>
        public void HostInterrupt68000()
        {
            if (_inReset) return;
            _irq.Nd100RequestInterrupt();
            _firmware.OnMfpInterrupt(MfpVectors.Nd100Request);
        }

        /// <summary>
        /// The firmware registers a handler for one of the 8 ND-100 doorbell
        /// channels (nd_channel_context_table, populated at runtime).
        /// </summary>
        public void RegisterNdChannel(int channel, Action handler) =>
            _firmware.RegisterNdChannel(channel, handler);

        /// <summary>
        /// ND-100 rings one of the 8 doorbell channels: set the channel flag word
        /// at nd_channel_flags (0x0B56 + channel*2) and raise GPIP6. The 68000's
        /// nd_host_interrupt_handler (0x250E) then dispatches that channel.
        /// </summary>
        public void SignalNdChannel(int channel)
        {
            if (_inReset) return;
            _mem.WriteWord(FirmwareDataAddresses.NdChannelFlags + (uint)channel * 2, 1);
            _irq.Nd100RequestInterrupt();
            _firmware.OnNdHostInterrupt();
        }

        /// <summary>Advance the model one step (services pending work, LANCE IRQs).</summary>
        public void Tick()
        {
            if (_inReset) return;

            if (_lance.InterruptPending)
            {
                _irq.LanceInterrupt();
                _firmware.OnLanceInterrupt();
            }

            _firmware.MainLoopStep();
        }

        /// <summary>
        /// Deliver an inbound Ethernet frame to the LANCE (host/network side). The
        /// LANCE address-filters it and, if accepted, DMAs it into the next
        /// chip-owned RX buffer in shared memory and raises RINT. Returns true if
        /// the frame was accepted into the ring.
        /// </summary>
        public bool ReceiveEthernetFrame(ReadOnlySpan<byte> frame)
        {
            return _lance.ReceiveFrame(frame);
        }

        /// <summary>
        /// Transmit a frame the way the firmware does (XMTRINGAPP @ 0x6054): build
        /// the header into the TX buffer (dest MAC + our PADR as source), pad, set
        /// up the TMD with OWN, then poke CSR0=0x48 (INEA|TDMD). <paramref name="body"/>
        /// is everything after the 12-byte MAC header (ethertype + payload).
        /// </summary>
        public void TransmitEthernetFrame(ReadOnlySpan<byte> destMac, ReadOnlySpan<byte> body, uint txBufferAddr)
        {
            _lance.BuildTxFrame(txBufferAddr, destMac, body);
            _lance.WriteRap(LanceCsr.Csr0_Control); // RAP = 0
            _lance.WriteRdp(LanceCsr.Csr0_TxKick);  // CSR0 = 0x0048 (INEA|TDMD) -> chip transmits
        }

        /// <summary>Pull the next transmitted frame off the LANCE (host/network side).</summary>
        public bool TryTransmitEthernetFrame(out byte[] frame)
        {
            return _lance.TryTransmit(out frame);
        }
    }
}
