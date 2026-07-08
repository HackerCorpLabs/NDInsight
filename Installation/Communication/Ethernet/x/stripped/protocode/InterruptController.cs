//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: interrupt routing between ND-100, MFP, LANCE and the 68000, plus
// the 68000 -> ND-100 SCIP doorbell. Routing is CONFIRMED from NDBusEthernetII.cs
// and the observed SCIP write sites.
//

using System;

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Routes interrupts in both directions. The 68000-facing side raises IPL
    /// levels; the ND-100-facing side is the SCIP doorbell (INT12).
    /// </summary>
    public sealed class InterruptController
    {
        private readonly FirmwareTrace _trace;
        private readonly MfpControllerModel _mfp;

        /// <summary>Raised when the 68000 writes SCIP - the ND-100 sees INT12.</summary>
        public event Action? OnScipToNd100;

        /// <summary>Pending 68000 interrupt level (0 = none). Highest wins in a real PIC.</summary>
        public int PendingCpuLevel { get; private set; }

        public InterruptController(FirmwareTrace trace, MfpControllerModel mfp)
        {
            _trace = trace;
            _mfp = mfp;
        }

        // ---- ND-100 -> 68000 ----

        /// <summary>
        /// ND-100 requests an interrupt (Control Word bit 2). CONFIRMED: asserts
        /// MFP GPIP I6 -> vector 116 -> 68000 level 3.
        /// </summary>
        public void Nd100RequestInterrupt()
        {
            _mfp.RaiseNd100Request();
            PendingCpuLevel = Cpu68kLevels.Mfp;
        }

        /// <summary>
        /// ND-100 OPCOM (Control Word bit 3). CONFIRMED: 68000 level 6 autovector
        /// (vector 0x1E, handler pointer at addr 0x78 installed by reset entry).
        /// </summary>
        public void Nd100Opcom()
        {
            PendingCpuLevel = Cpu68kLevels.NdOpcom;
            _trace.Interrupt("ND-100 OPCOM (Control Word bit3)", "68000 level 6 (vec 0x1E)");
        }

        /// <summary>ND-100 power-low (Control Word bit 6) -> level 7 NMI.</summary>
        public void Nd100PowerLow()
        {
            PendingCpuLevel = Cpu68kLevels.PowerLowNmi;
            _trace.Interrupt("ND-100 power-low", "68000 level 7 NMI");
        }

        // ---- LANCE -> 68000 ----

        /// <summary>LANCE asserts its INTR pin -> 68000 level 2 (autovector).</summary>
        public void LanceInterrupt()
        {
            PendingCpuLevel = Cpu68kLevels.Lance;
            _trace.Interrupt("LANCE INTR", "68000 level 2");
        }

        // ---- 68000 -> ND-100 ----

        /// <summary>
        /// The 68000 writes SCIP (0xEF0080 or mirror 0xEF0180) to raise INT12 on
        /// the ND-100. CONFIRMED: post_and_signal_nd100_scip and the XMSG postbox
        /// producer both do this.
        /// </summary>
        public void WriteScip(uint address)
        {
            _trace.IoWrite(address, 0x0001);
            _trace.Interrupt("68000 SCIP write", "ND-100 INT12");
            OnScipToNd100?.Invoke();
        }

        /// <summary>68000 acknowledges its pending level (IACK). Returns the MFP vector for level 3.</summary>
        public byte AcknowledgeCpu(int level)
        {
            if (PendingCpuLevel == level)
                PendingCpuLevel = 0;

            if (level == Cpu68kLevels.Mfp)
                return _mfp.AcknowledgeVector(); // vectored
            return 0; // autovector for all other levels
        }

        public void Reset()
        {
            PendingCpuLevel = 0;
        }
    }
}
