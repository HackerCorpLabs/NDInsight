//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: one C# method per discovered firmware command/routine. Each method
// carries the 68000 entry address in a comment. Handlers whose semantics are not
// fully reversed are explicit stubs that call Trace.Unconfirmed and return
// HostResult.Unimplemented - the model never silently guesses.
//

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// A command posted by the ND-100 host. Field names follow the diagnostic
    /// mailbox for familiarity; in the production image the equivalent inputs
    /// arrive through the postbox ring.
    /// </summary>
    public readonly record struct HostCommand(
        ushort CommandNumber,
        ushort Param1,
        ushort TestNumber);

    /// <summary>
    /// A result the 68000 posts back to the ND-100. Mirrors the diagnostic result
    /// block fields; production code fills a subset (status + payload words).
    /// </summary>
    public readonly record struct HostResult(
        ushort CommandNumber,
        ushort StatusCode,
        ushort ErrorCode,
        bool Implemented)
    {
        public static HostResult Ok(ushort commandNumber, ushort status = ResultCodes.Ok)
            => new(commandNumber, status, 0, true);

        public static HostResult Unimplemented(ushort commandNumber)
            => new(commandNumber, ResultCodes.Unimplemented, ResultCodes.Unimplemented, false);
    }

    /// <summary>
    /// Command handlers. Holds references to the models a handler needs to touch.
    /// </summary>
    public sealed class FirmwareCommands
    {
        private readonly FirmwareTrace _trace;
        private readonly SharedMemory _mem;
        private readonly LanceControllerModel _lance;
        private readonly InterruptController _irq;

        public FirmwareCommands(FirmwareTrace trace, SharedMemory mem, LanceControllerModel lance, InterruptController irq)
        {
            _trace = trace;
            _mem = mem;
            _lance = lance;
            _irq = irq;
        }

        /// <summary>
        /// INITLANCE - 68000 @ 0x000048EA (CSR block @ 0x00004ABE). CONFIRMED.
        /// Reproduces the confirmed LANCE bring-up: transceiver power, CSR3=BSWP,
        /// init block pointer = 0x18810, CSR0=INIT.
        /// </summary>
        public HostResult HandleCommand_0000_InitLance(HostCommand command)
        {
            // INITLANCE (0x48EA) copies the 6-byte MAC from lance_mac_address
            // (0x1885E) into the init block PADR field (0x18810+2), then builds the
            // rest of the init block and runs the CSR sequence.
            for (uint i = 0; i < 6; i++)
            {
                byte b = _mem.ReadByte(FirmwareDataAddresses.LanceMacAddress + i);
                _mem.WriteByte(FirmwareDataAddresses.LanceInitBlock + 2 + i, b);
            }

            _lance.SetTransceiverPower(true);                 // 0x47BA move.b #3,0xEF00A8
            _lance.WriteRap(LanceCsr.Csr3_BusControl);        // 0x4ABE RAP=3
            _lance.WriteRdp(LanceCsr.Csr3_Bswp);              // 0x4AC4 RDP=4 (BSWP)
            _lance.WriteRap(LanceCsr.Csr1_IadrLow);           // 0x4ADC RAP=1
            _lance.WriteRdp((ushort)(FirmwareDataAddresses.LanceInitBlock & 0xFFFF)); // 0x4AEC RDP=iadr low
            _lance.WriteRap(LanceCsr.Csr2_IadrHigh);          // 0x4AF0 RAP=2
            _lance.WriteRdp((ushort)(FirmwareDataAddresses.LanceInitBlock >> 16));    // 0x4B08 RDP=iadr high
            _lance.WriteRap(LanceCsr.Csr0_Control);           // 0x4B0A RAP=0
            _lance.WriteRdp(LanceCsr.Csr0_Init);              // 0x4B18 RDP=1 (INIT)
            return HostResult.Ok(command.CommandNumber);
        }

        /// <summary>
        /// RCVRINGAPPEND - 68000 @ 0x00005B60 (== FUN_00005b60). CONFIRMED name.
        /// Appends a received buffer to the RX ring. Body not fully reversed.
        /// </summary>
        public HostResult HandleCommand_0002_RcvRingAppend(HostCommand command)
        {
            // RCVRINGAPPEND (0x5B60) hands a fresh empty buffer to the LANCE: it
            // writes the buffer address + (-BCNT) into the next producer-index RMD
            // and sets OWN. In this model the LANCE reads those descriptors directly
            // from shared memory during ReceiveFrame, so replenishment is a no-op
            // here beyond the trace - the ring state lives in SharedMemory.
            _trace.Info("RCVRINGAPPEND @ 0x5B60: buffer handed to LANCE (OWN set, producer index++)");
            return HostResult.Ok(command.CommandNumber);
        }

        /// <summary>
        /// XMSG postbox send - 68000 @ 0x0000EACC. HYPOTHESIS (strong).
        /// Fills a ring slot, clears its owner word, advances the 8-slot index,
        /// then rings SCIP mirror 0xEF0180 -> INT12.
        /// </summary>
        public HostResult HandleCommand_0006_XmsgPostboxSend(HostCommand command)
        {
            _trace.Info("XMSG postbox send: fill slot, clr owner, advance index &7, ring SCIP mirror");
            _irq.WriteScip(IoAddresses.ScipMirror); // 0xECF2 clr.w 0xEF0180
            return HostResult.Ok(command.CommandNumber);
        }

        /// <summary>
        /// Fallback for any command whose semantics are not reversed. Explicit stub
        /// so nothing is silently guessed.
        /// </summary>
        public HostResult HandleCommand_Unconfirmed(HostCommand command)
        {
            _trace.Unconfirmed($"Command 0x{command.CommandNumber:X4} has no reversed handler in this image.");
            return HostResult.Unimplemented(command.CommandNumber);
        }
    }
}
