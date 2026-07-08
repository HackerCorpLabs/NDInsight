//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: MC68901 MFP behavioral model, limited to what the firmware/host
// interaction requires. The MFP register programming block was NOT located in
// this image, so vector base (VR) and IER/IMR values are provisional.
//

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Minimal MC68901 MFP model. Only the GPIP interrupt lines, USART wake, the
    /// vector base, and Timer C (RTC) are modelled - these are the paths the ND
    /// Ethernet-II firmware uses. Register-accurate emulation is out of scope.
    /// </summary>
    public sealed class MfpControllerModel
    {
        private readonly FirmwareTrace _trace;

        public MfpControllerModel(FirmwareTrace trace)
        {
            _trace = trace;
        }

        /// <summary>
        /// MFP vector base register (VR high nibble). CONFIRMED = 0x40:
        /// init_mfp_registers (68000 @ 0x396A, called from reset_entry) writes
        /// 0x40 to MFP register offset 0x17 (VR) at base 0xEF00C0.
        /// </summary>
        public byte VectorBase { get; set; } = 0x40; // CONFIRMED (0x396A writes VR=0x40)

        /// <summary>Interrupt-enable mask by MFP source bit. Provisional.</summary>
        public ushort InterruptEnable { get; set; }

        /// <summary>Pending MFP interrupt source vector, or 0 if none.</summary>
        public byte PendingVector { get; private set; }

        // GPIP lines (active-low on real HW; here true = asserted for clarity).
        public bool Gpip5_LanceError { get; private set; }
        public bool Gpip6_Nd100Request { get; private set; }
        public bool Gpip7_WriteViolation { get; private set; }

        /// <summary>ND-100 raises its request line (GPIP I6 -> vector 116). CONFIRMED path.</summary>
        public void RaiseNd100Request()
        {
            Gpip6_Nd100Request = true;
            PendingVector = MfpVectors.Nd100Request;
            _trace.Interrupt("ND-100 (GPIP I6)", "MFP vector 116");
        }

        /// <summary>LANCE memory error (GPIP I5 -> vector 107).</summary>
        public void RaiseLanceError()
        {
            Gpip5_LanceError = true;
            PendingVector = MfpVectors.LanceMemError;
            _trace.Interrupt("LANCE mem-error (GPIP I5)", "MFP vector 107");
        }

        /// <summary>Write violation by 68000 (GPIP I7 -> vector 117).</summary>
        public void RaiseWriteViolation()
        {
            Gpip7_WriteViolation = true;
            PendingVector = MfpVectors.WriteViolation;
            _trace.Interrupt("write-violation (GPIP I7)", "MFP vector 117");
        }

        /// <summary>
        /// Timer C tick used as the real-time clock (vector 105). CONFIRMED: the
        /// handler is rtc_timer_isr (68000 @ 0x3A68) - it bumps tick counters at
        /// 0xFC2/0xFCA and fires expired entries from the timer queue at 0xFD6.
        /// </summary>
        public void RaiseRtcTick()
        {
            PendingVector = MfpVectors.RealTimeClock;
            _trace.Info("RTC tick -> rtc_timer_isr @ 0x3A68 (increments 0xFC2/0xFCA, fires timer queue 0xFD6)");
            _trace.Interrupt("Timer C", "MFP vector 105");
        }

        /// <summary>USART receive-buffer-full from the PTC test console (vector 114).</summary>
        public void ReceiveSerialByte(byte data)
        {
            PendingVector = MfpVectors.UsartRxFull;
            _trace.Unconfirmed("USART/PTC console RX handler not traced in this image");
            _trace.Interrupt($"USART RX 0x{data:X2}", "MFP vector 114");
        }

        /// <summary>
        /// Return and clear the vector the 68000 fetches during a level-3 IACK.
        /// 0 = spurious.
        /// </summary>
        public byte AcknowledgeVector()
        {
            byte v = PendingVector;
            PendingVector = 0;
            Gpip5_LanceError = false;
            Gpip6_Nd100Request = false;
            Gpip7_WriteViolation = false;
            return v;
        }

        public void Reset()
        {
            PendingVector = 0;
            InterruptEnable = 0;
            Gpip5_LanceError = false;
            Gpip6_Nd100Request = false;
            Gpip7_WriteViolation = false;
        }
    }
}
