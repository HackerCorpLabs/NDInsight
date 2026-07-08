//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: the command dispatch table. It is built from the CONFIRMED PLANC
// symbol-table routines discovered in the loaded image, NOT from a fabricated
// numeric test-command table. The production server firmware does not expose a
// numeric test-dispatch table (that belongs to the bank-0 diagnostic firmware),
// so entries are keyed by routine and carry an explicit Confidence field.
//

using System.Collections.Generic;

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// One dispatch-table entry. <paramref name="CommandNumber"/> is a synthetic
    /// index for the model (the firmware dispatches by routine, not by a numeric
    /// command in this image); <paramref name="HandlerAddress"/> is the real 68000
    /// entry; <paramref name="Confidence"/> is CONFIRMED / HYPOTHESIS / UNCONFIRMED.
    /// </summary>
    public sealed record FirmwareCommandEntry(
        ushort CommandNumber,
        uint HandlerAddress,
        string HandlerName,
        string Confidence,
        string Notes);

    /// <summary>
    /// Dispatches host commands to <see cref="FirmwareCommands"/> handlers, and
    /// exposes the discovered routine table.
    /// </summary>
    public sealed class FirmwareCommandDispatcher
    {
        private readonly FirmwareCommands _commands;
        private readonly FirmwareTrace _trace;

        /// <summary>
        /// The discovered routine table. Addresses are from the PLANC symbol table
        /// at 0x66E00 (record layout [code-addr:32][zero:32][name]); several were
        /// verified against pre-existing Ghidra auto-analysis (marked CONFIRMED).
        /// </summary>
        public static readonly IReadOnlyList<FirmwareCommandEntry> Table = new[]
        {
            new FirmwareCommandEntry(0x0000, FirmwareAddresses.InitLance,        "INITLANCE",       "CONFIRMED-name", "LANCE init; CSR block at 0x4ABE"),
            new FirmwareCommandEntry(0x0001, FirmwareAddresses.FatalError,       "FATALERROR",      "CONFIRMED",      "== FUN_00004c26"),
            new FirmwareCommandEntry(0x0002, FirmwareAddresses.RcvRingAppend,    "RCVRINGAPPEND",   "CONFIRMED",      "== FUN_00005b60; RX ring append"),
            new FirmwareCommandEntry(0x0003, FirmwareAddresses.XmReceiver,       "XMRECEIVER",      "HYPOTHESIS",     "XMSG receiver"),
            new FirmwareCommandEntry(0x0004, FirmwareAddresses.PortCreate,       "PORTCREATE",      "HYPOTHESIS",     "create XMSG port"),
            new FirmwareCommandEntry(0x0005, FirmwareAddresses.XmpSend,          "XMPSEND",         "HYPOTHESIS",     "XMSG send"),
            new FirmwareCommandEntry(0x0006, FirmwareAddresses.XmsgPostboxSendRing,"XMSG_POSTBOX_SEND","HYPOTHESIS",  "0xEACC ring producer + SCIP 0xEF0180"),
            new FirmwareCommandEntry(0x0007, FirmwareAddresses.PosiInitialize,   "POSIINITIALIZE",  "CONFIRMED-name","== FUN_00011732"),
            new FirmwareCommandEntry(0x0008, FirmwareAddresses.PosiStart,        "POSISTART",       "CONFIRMED-name","== FUN_0001179c"),
            new FirmwareCommandEntry(0x0009, FirmwareAddresses.PosiAppend,       "POSIAPPEND",      "CONFIRMED-name","== FUN_00011dc4"),
        };

        public FirmwareCommandDispatcher(FirmwareCommands commands, FirmwareTrace trace)
        {
            _commands = commands;
            _trace = trace;
        }

        /// <summary>
        /// Dispatch a host command. In this model the CommandNumber selects one of
        /// the discovered routines; unknown numbers fall through to an explicit
        /// "unconfirmed" stub so nothing is silently guessed.
        /// </summary>
        public HostResult Dispatch(HostCommand command)
        {
            _trace.Info($"dispatch_host_command cmd=0x{command.CommandNumber:X4}");

            switch (command.CommandNumber)
            {
                case 0x0000: return _commands.HandleCommand_0000_InitLance(command);
                case 0x0002: return _commands.HandleCommand_0002_RcvRingAppend(command);
                case 0x0006: return _commands.HandleCommand_0006_XmsgPostboxSend(command);
                default:
                    return _commands.HandleCommand_Unconfirmed(command);
            }
        }
    }
}
