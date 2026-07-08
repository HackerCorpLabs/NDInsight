//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: the shared-memory mailbox/postbox field addresses.
//
// IMPORTANT HONESTY NOTE:
// The MailboxAddress enum below reproduces the diagnostic-firmware map from the
// task brief (0x400 command block / 0x440 result block / 0x880 status block).
// That map was NOT confirmed in the loaded production image
// (encos-ser-all-banks-68k.bin). In THIS image the low DRAM is used differently:
//   * 0x40A = monitor/console postbox (see MonitorPostbox below)  -- CONFIRMED
//   * 0x454 = CPU register dump frame                              -- CONFIRMED
//   * 0x4BA = warm-boot magic 0x55555555                           -- CONFIRMED
// The diagnostic enum is kept so the model can talk to the diagnostic firmware,
// but consumers of the production path should use MonitorPostbox / XmsgPostbox.
//

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Diagnostic-firmware mailbox field addresses (from the task brief).
    /// UNCONFIRMED in the production all-banks image - see file header.
    /// Direction convention: "ND writes" = host produces, 68000 consumes.
    /// </summary>
    public enum MailboxAddress : uint
    {
        // Command mailbox: ND-100 writes, 68000 reads. (diagnostic firmware)
        CmdSemaphore = 0x0400,
        CmdStatus = 0x0402,
        CmdParam1 = 0x0404,
        CmdTestNumber = 0x0406,

        // Result mailbox: 68000 writes, ND-100 reads. (diagnostic firmware)
        ResultSemaphore = 0x0440,
        ResultStatusCode = 0x0442,
        ResultErrorCode = 0x0444,
        ResultTestNumber = 0x0446,
        ResultLoopCount = 0x0448,
        ResultErrorCount = 0x044A,
        ResultErrorAddressHigh = 0x044C,
        ResultErrorAddressLow = 0x044E,
        ResultExpectedDataHigh = 0x0450,
        ResultExpectedDataLow = 0x0452,
        ResultFoundData = 0x0456,

        // Status/config block. (diagnostic firmware)
        StatusSemaphore = 0x0880,
        StatusCode = 0x0882,
        FunctionCode = 0x0884,
        ErrorCode = 0x0886,
        TestNumber = 0x0888,
        LoopCount = 0x088A,
        ErrorCount = 0x088C,
        ErrorAddress = 0x088E,
        ExpectedData = 0x0890,
        FoundData = 0x0892,
        MainLoopAddress = 0x08A2,
        CommandBuffer = 0x0908,
        CommandTestNumber68K = 0x090E,
    }

    /// <summary>
    /// Production monitor/console postbox at 0x40A. CONFIRMED field layout from
    /// nd_monitor_set_flag (0x1A30), post_and_signal_nd100_scip (0x1A48) and the
    /// reset entry (0x1CFE). All fields are produced by the 68000; the ND-100
    /// consumes them and is woken by the SCIP doorbell.
    /// </summary>
    public enum MonitorPostbox : uint
    {
        /// <summary>+0: event counter, incremented on each post. 68000 -> ND-100.</summary>
        Counter = 0x040A,

        /// <summary>+2: code / sub-code. 68000 -> ND-100.</summary>
        Code = 0x040C,

        /// <summary>+4: parameter. 68000 -> ND-100.</summary>
        Param = 0x040E,

        /// <summary>+6: second counter, incremented on each post. 68000 -> ND-100.</summary>
        Counter2 = 0x0410,

        /// <summary>+8: request flag (set to 1 by nd_monitor_set_flag). 68000 -> ND-100.</summary>
        RequestFlag = 0x0412,
    }
}
