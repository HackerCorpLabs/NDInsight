//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: AMD Am7990 LANCE model. The receive path now performs a REAL DMA of
// the incoming frame into the RX ring buffers in shared DRAM, exactly matching the
// descriptor layout the firmware uses:
//   * RCVRINGAPPEND (68000 @ 0x5B60) builds 8-byte RMDs at 0x18008, sets -BCNT and
//     the OWN bit, and advances the producer index (mod 128).
//   * RCVCOMPLETE   (68000 @ 0x5C42) waits for OWN==0, reads the 24-bit buffer
//     address (RMD1 high byte : RMD0 low word), takes length = (RMD3 & 0xFFF) - 4
//     (strips the 4-byte FCS) and reads the destination MAC from the first 6 bytes.
// So this model, on receive, finds the next chip-owned descriptor, writes the frame
// (plus a placeholder FCS) into that descriptor's buffer, fills RMD3/flags, clears
// OWN, and raises RINT - which is precisely what the real LANCE does before the
// firmware's level-2 handler runs.
//
// Hardware address filtering (done by the LANCE, not the firmware) is modelled:
// a frame is accepted if the card is promiscuous (MODE bit 15), or the destination
// MAC is broadcast, or it matches our PADR, or it is any multicast (LADRF hashing
// is simplified to accept-all-multicast and flagged).
//
// The transmit path mirrors XMTRINGAPP (68000 @ 0x6054): BuildTxFrame writes the
// Ethernet header into the TX buffer (dest MAC + the SOURCE MAC taken from our
// PADR at 0x1885E), pads short frames to 60 bytes, builds the 8-byte TMD at
// 0x18410 with OWN, and a CSR0 TDMD poke makes the chip DMA the buffer to the wire.
//

using System;

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Am7990 LANCE model with real RX DMA into the shared-memory descriptor ring.
    /// It holds a reference to <see cref="SharedMemory"/> so it can read the init
    /// block / descriptors and write received frames the way the chip does.
    /// </summary>
    public sealed class LanceControllerModel
    {
        private readonly FirmwareTrace _trace;
        private readonly SharedMemory _mem;

        private ushort _rap;         // register address port selection
        private int _chipRxIndex;    // the chip's own next-RMD position (wraps mod 128)

        public LanceControllerModel(FirmwareTrace trace, SharedMemory mem)
        {
            _trace = trace;
            _mem = mem;
        }

        /// <summary>CSR0..CSR3 last-written values.</summary>
        public ushort[] Csr { get; } = new ushort[4];

        /// <summary>Init block base last programmed (CSR1/CSR2). CONFIRMED = 0x18810.</summary>
        public uint InitBlockAddress { get; private set; }

        public bool Initialized { get; private set; }
        public bool InterruptPending { get; private set; }
        public bool TransceiverPowerOn { get; private set; }

        /// <summary>Count of frames dropped by the hardware address filter.</summary>
        public int FilteredCount { get; private set; }

        /// <summary>Count of frames dropped because no RX buffer was available (MISS).</summary>
        public int MissedCount { get; private set; }

        // ---- I/O port access (0xEF00A2 RAP, 0xEF00A0 RDP) ----

        /// <summary>Write RAP (0xEF00A2) - select which CSR the next RDP hits. CONFIRMED.</summary>
        public void WriteRap(ushort csrNumber)
        {
            _rap = (ushort)(csrNumber & 0x0003);
            _trace.IoWrite(IoAddresses.LanceRap, _rap);
        }

        /// <summary>Write RDP (0xEF00A0) - write the selected CSR. CONFIRMED sequence.</summary>
        public void WriteRdp(ushort value)
        {
            Csr[_rap] = value;
            _trace.IoWrite(IoAddresses.LanceRdp, value);

            switch (_rap)
            {
                case LanceCsr.Csr1_IadrLow:
                    InitBlockAddress = (InitBlockAddress & 0xFFFF0000) | value;
                    break;
                case LanceCsr.Csr2_IadrHigh:
                    InitBlockAddress = (InitBlockAddress & 0x0000FFFF) | ((uint)value << 16);
                    break;
                case LanceCsr.Csr0_Control:
                    HandleCsr0Write(value);
                    break;
            }
        }

        /// <summary>Read RDP (0xEF00A0) - read the selected CSR.</summary>
        public ushort ReadRdp()
        {
            ushort v = Csr[_rap];
            _trace.IoRead(IoAddresses.LanceRdp, v);
            return v;
        }

        private void HandleCsr0Write(ushort value)
        {
            // INIT: latch the init block and mark IDON + RXON.
            if ((value & LanceCsr.Csr0_Init) != 0)
            {
                Initialized = true;
                _chipRxIndex = 0;
                Csr[0] |= (ushort)(LanceCsr.Csr0_Idon | LanceCsr.Csr0_Rxon | LanceCsr.Csr0_Txon);
                _trace.Info($"LANCE INIT: init block = 0x{InitBlockAddress:X6}, RX/TX on");
            }

            // TX kick: INEA|TDMD (0x0048 observed at 0x616E). The chip now reads
            // any chip-owned TX descriptor and transmits its buffer.
            if ((value & LanceCsr.Csr0_Tdmd) != 0)
            {
                _trace.Info("LANCE TDMD - transmit demand");
                TransmitFromRing();
            }
        }

        /// <summary>XCVPW (0xEF00A8) transceiver power. CONFIRMED write of 0x03/0x00.</summary>
        public void SetTransceiverPower(bool on)
        {
            TransceiverPowerOn = on;
            _trace.IoWrite(IoAddresses.Xcvpw, (ushort)(on ? 0x03 : 0x00));
        }

        /// <summary>LANRESET (0xEF00B0) hardware reset. Region present; HYPOTHESIS.</summary>
        public void HardwareReset()
        {
            Initialized = false;
            InterruptPending = false;
            Array.Clear(Csr, 0, Csr.Length);
            _trace.IoWrite(IoAddresses.LanReset, 0);
            _trace.Unconfirmed("LANRESET handler not traced in this image");
        }

        // ---- RX ring setup (mirrors RCVRINGAPPEND @ 0x5B60) ----

        /// <summary>
        /// Hand an empty receive buffer to the LANCE, writing an RMD exactly as
        /// RCVRINGAPPEND (68000 @ 0x5B60) does: RMD0 = buffer low word, RMD1 HADR =
        /// buffer high byte, RMD2 = -size (two's complement BCNT), then set OWN and
        /// advance the producer index (0x18002) mod 128 and decrement the free
        /// count (0x18000). Call this once per buffer to build the ring, or let the
        /// firmware's own RCVRINGAPPEND do it - both write the same memory.
        /// </summary>
        public void AppendRxBuffer(uint bufferAddr, int size)
        {
            ushort producer = _mem.ReadWord(LanceRing.RxHeader + 2);
            int idx = producer & (LanceRing.RxDescCount - 1);
            uint desc = LanceRing.RxDescBase + (uint)idx * LanceRing.DescSize;

            _mem.WriteWord(desc + LanceRing.Rmd0_LadrWord, (ushort)(bufferAddr & 0xFFFF));
            _mem.WriteByte(desc + LanceRing.Rmd1_HadrByte, (byte)((bufferAddr >> 16) & 0xFF));
            _mem.WriteWord(desc + LanceRing.Rmd2_BcntWord, (ushort)(-size)); // -BCNT

            // Advance producer index (mod 128) and decrement free count, then OWN.
            _mem.WriteWord(LanceRing.RxHeader + 2, (ushort)((producer + 1) & (LanceRing.RxDescCount - 1)));
            ushort count = _mem.ReadWord(LanceRing.RxHeader);
            _mem.WriteWord(LanceRing.RxHeader, (ushort)(count - 1));

            ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
            flags |= LanceRing.Own; // hand to the chip
            _mem.WriteWord(desc + LanceRing.Rmd1_FlagsWord, flags);

            _trace.Info($"AppendRxBuffer: desc[{idx}] <- buf 0x{bufferAddr:X6} size {size}, OWN set");
        }

        // ---- Receive: real DMA into the shared-memory RX ring ----

        /// <summary>
        /// The card's own MAC address (PADR), read from the init block at
        /// LanceInitBlock+2 (6 bytes). CONFIRMED INITLANCE copies this from 0x1885E.
        /// </summary>
        public byte[] GetPadr()
        {
            var mac = new byte[6];
            for (int i = 0; i < 6; i++)
                mac[i] = _mem.ReadByte(FirmwareDataAddresses.LanceInitBlock + 2 + (uint)i);
            return mac;
        }

        /// <summary>MODE word from the init block (+0). Bit 15 = promiscuous (PROM).</summary>
        public ushort GetMode() => _mem.ReadWord(FirmwareDataAddresses.LanceInitBlock);

        /// <summary>
        /// Deliver an inbound Ethernet frame to the LANCE. This models the chip:
        /// address-filter, then DMA the frame into the next chip-owned RX buffer,
        /// write RMD3/flags, clear OWN, and raise RINT. Returns true if accepted.
        /// </summary>
        public bool ReceiveFrame(ReadOnlySpan<byte> frame)
        {
            if (!Initialized)
            {
                _trace.Info("RX dropped - LANCE not initialized");
                return false;
            }

            if (frame.Length < 14)
            {
                _trace.Info($"RX dropped - runt frame ({frame.Length} bytes, < 14)");
                return false;
            }

            // ---- Hardware address filter (LANCE, not firmware) ----
            if (!AddressAccepted(frame))
            {
                FilteredCount++;
                _trace.Info($"RX filtered - dest MAC {MacToString(frame)} not for us (PADR {MacToString(GetPadr())})");
                return false;
            }

            // ---- Find the next chip-owned RX descriptor (OWN == 1) ----
            int idx = FindNextOwnedRxDescriptor();
            if (idx < 0)
            {
                MissedCount++;
                Csr[0] |= LanceCsr.Csr0_Miss;
                InterruptPending = true; // MISS still raises the interrupt line
                _trace.Interrupt("LANCE RX MISS (no buffer available)", "68000 level 2");
                return false;
            }

            uint desc = LanceRing.RxDescBase + (uint)idx * LanceRing.DescSize;

            // 24-bit buffer address = RMD1 high byte (HADR) : RMD0 low word (LADR).
            uint bufLow = _mem.ReadWord(desc + LanceRing.Rmd0_LadrWord);
            uint bufHigh = _mem.ReadByte(desc + LanceRing.Rmd1_HadrByte);
            uint bufAddr = (bufHigh << 16) | bufLow;

            // Buffer size = -(RMD2), the two's-complement byte count the firmware set.
            short negBcnt = (short)_mem.ReadWord(desc + LanceRing.Rmd2_BcntWord);
            int bufSize = -negBcnt;

            // DMA the frame in, then append a 4-byte placeholder FCS (the firmware
            // strips 4 bytes as the FCS in RCVCOMPLETE). Truncate to buffer size.
            int frameLen = frame.Length;
            int mcnt = frameLen + LanceRing.FcsBytes; // message length includes FCS
            bool overflow = mcnt > bufSize;
            int copyLen = overflow ? bufSize : frameLen;

            for (int i = 0; i < copyLen; i++)
                _mem.WriteByte(bufAddr + (uint)i, frame[i]);
            // Placeholder FCS bytes if they fit (real chip DMAs the on-wire FCS).
            for (int i = 0; i < LanceRing.FcsBytes && copyLen + i < bufSize; i++)
                _mem.WriteByte(bufAddr + (uint)(copyLen + i), 0x00);

            // ---- Write back RMD3 (MCNT) and the flags (clear OWN, set STP/ENP) ----
            _mem.WriteWord(desc + LanceRing.Rmd3_StatusWord, (ushort)(mcnt & LanceRing.McntMask));

            ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
            flags &= unchecked((ushort)~LanceRing.Own); // hand the buffer back to the 68000
            flags |= (ushort)(LanceRing.Stp | LanceRing.Enp); // single-buffer frame
            if (overflow) flags |= (ushort)(LanceRing.Err | LanceRing.Oflo);
            _mem.WriteWord(desc + LanceRing.Rmd1_FlagsWord, flags);

            // Advance the chip's RX pointer (mod 128) and raise RINT.
            _chipRxIndex = (idx + 1) & (LanceRing.RxDescCount - 1);
            Csr[0] |= LanceCsr.Csr0_Rint;
            InterruptPending = true;

            _trace.Info($"RX DMA: frame {frameLen}B (mcnt {mcnt}) -> desc[{idx}] buf 0x{bufAddr:X6}, OWN cleared, dest {MacToString(frame)}");
            _trace.Interrupt("LANCE RX (frame in ring)", "68000 level 2 (RINT)");
            return true;
        }

        /// <summary>Logical address filter (LADRF), 8 bytes from init block +8.</summary>
        public byte[] GetLadrf()
        {
            var f = new byte[8];
            for (int i = 0; i < 8; i++)
                f[i] = _mem.ReadByte(FirmwareDataAddresses.LanceInitBlock + 8 + (uint)i);
            return f;
        }

        /// <summary>
        /// LANCE hardware address filter. Accept if promiscuous, broadcast, our
        /// PADR, or a multicast whose hash bit is set in the LADRF.
        ///
        /// Multicast uses the standard Am7990 hash: CRC-32 of the 6 destination
        /// bytes, take the top 6 bits (crc >> 26) as an index 0..63 into the 64-bit
        /// LADRF; accept if that bit is set. NOTE: this is documented Am7990
        /// behaviour - this firmware's own multicast-add routine (which would set
        /// the LADRF bits with the same hash) is not wired to a caller in the static
        /// image, so the hash orientation here is standard-chip, not firmware-proven.
        /// </summary>
        private bool AddressAccepted(ReadOnlySpan<byte> frame)
        {
            if ((GetMode() & 0x8000) != 0) // MODE bit 15 = PROM (promiscuous)
                return true;

            bool broadcast = true;
            for (int i = 0; i < 6; i++)
                if (frame[i] != 0xFF) { broadcast = false; break; }
            if (broadcast) return true;

            if ((frame[0] & 0x01) != 0) // group/multicast bit set = multicast
            {
                byte[] ladrf = GetLadrf();
                uint crc = NDEthernetIIFirmware.Crc32(frame.Slice(0, 6)); // CRC over the 6 dest bytes
                int hash = (int)(crc >> 26) & 0x3F;                       // top 6 bits -> 0..63
                bool set = (ladrf[hash >> 3] & (1 << (hash & 7))) != 0;
                _trace.Unconfirmed($"multicast hash={hash} LADRF-bit={(set ? 1 : 0)} (standard Am7990 hash; firmware multicast-add not wired in static image)");
                return set;
            }

            byte[] padr = GetPadr();
            for (int i = 0; i < 6; i++)
                if (frame[i] != padr[i]) return false;
            return true;
        }

        /// <summary>
        /// Scan the RX ring from the chip's current position for the next descriptor
        /// whose OWN bit is set (chip-owned = a buffer the firmware handed us).
        /// Returns the descriptor index, or -1 if none are available.
        /// </summary>
        private int FindNextOwnedRxDescriptor()
        {
            for (int step = 0; step < LanceRing.RxDescCount; step++)
            {
                int idx = (_chipRxIndex + step) & (LanceRing.RxDescCount - 1);
                uint desc = LanceRing.RxDescBase + (uint)idx * LanceRing.DescSize;
                ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
                if ((flags & LanceRing.Own) != 0)
                    return idx;
            }
            return -1;
        }

        // ---- Transmit: mirrors XMTRINGAPP (68000 @ 0x6054) ----

        private readonly System.Collections.Generic.Queue<byte[]> _txWire = new();
        private int _chipTxIndex; // the chip's own next TMD position (wraps mod 128)

        /// <summary>
        /// Build a transmit frame and hand it to the LANCE exactly as XMTRINGAPP
        /// (0x6054) does:
        ///   * write the Ethernet header into the buffer - destination MAC into
        ///     bytes 0..5 and the SOURCE MAC = our PADR (from 0x1885E) into bytes
        ///     6..11 (CONFIRMED: XMTRINGAPP copies 6 bytes from 0x1885E as src),
        ///   * copy the caller's body (ethertype + payload) from byte 12 on,
        ///   * pad to the 60-byte minimum,
        ///   * build the TMD at the producer index (addr, -BCNT, STP|ENP, OWN) and
        ///     advance the producer index (mod 128).
        /// The caller then pokes CSR0 TDMD (WriteRdp with 0x0048) to transmit.
        /// </summary>
        public void BuildTxFrame(uint bufferAddr, ReadOnlySpan<byte> destMac, ReadOnlySpan<byte> body)
        {
            byte[] src = GetPadr(); // our MAC = source address (0x1885E), CONFIRMED

            for (int i = 0; i < 6; i++) _mem.WriteByte(bufferAddr + (uint)i, destMac[i]);      // dest
            for (int i = 0; i < 6; i++) _mem.WriteByte(bufferAddr + 6 + (uint)i, src[i]);       // src = PADR
            for (int i = 0; i < body.Length; i++) _mem.WriteByte(bufferAddr + 12 + (uint)i, body[i]);

            int len = 12 + body.Length;
            if (len < LanceRing.MinFrameLen) // pad short frames to 60 bytes
            {
                for (uint p = (uint)len; p < LanceRing.MinFrameLen; p++) _mem.WriteByte(bufferAddr + p, 0);
                len = LanceRing.MinFrameLen;
            }

            ushort producer = _mem.ReadWord(LanceRing.TxHeader + 2);
            int idx = producer & (LanceRing.RxDescCount - 1);
            uint desc = LanceRing.TxDescBase + (uint)idx * LanceRing.DescSize;

            _mem.WriteWord(desc + LanceRing.Rmd0_LadrWord, (ushort)(bufferAddr & 0xFFFF));
            _mem.WriteByte(desc + LanceRing.Rmd1_HadrByte, (byte)((bufferAddr >> 16) & 0xFF));
            _mem.WriteWord(desc + LanceRing.Rmd2_BcntWord, (ushort)(-len)); // -BCNT

            ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
            flags |= (ushort)(LanceRing.Stp | LanceRing.Enp | LanceRing.Own); // start+end+own
            _mem.WriteWord(desc + LanceRing.Rmd1_FlagsWord, flags);

            _mem.WriteWord(LanceRing.TxHeader + 2, (ushort)((producer + 1) & (LanceRing.RxDescCount - 1)));
            ushort count = _mem.ReadWord(LanceRing.TxHeader);
            _mem.WriteWord(LanceRing.TxHeader, (ushort)(count - 1));

            _trace.Info($"BuildTxFrame: desc[{idx}] <- buf 0x{bufferAddr:X6} len {len}, dest {MacToString(destMac)} src {MacToString(src)}, OWN set");
        }

        /// <summary>
        /// The chip side of transmit (triggered by TDMD): walk the TX ring from the
        /// chip's position, and for each chip-owned (OWN=1) descriptor read the
        /// buffer (24-bit addr, length = -BCNT), put the frame on the wire, clear
        /// OWN and set TINT.
        /// </summary>
        private void TransmitFromRing()
        {
            bool any = false;
            for (int step = 0; step < LanceRing.RxDescCount; step++)
            {
                int idx = (_chipTxIndex + step) & (LanceRing.RxDescCount - 1);
                uint desc = LanceRing.TxDescBase + (uint)idx * LanceRing.DescSize;
                ushort flags = _mem.ReadWord(desc + LanceRing.Rmd1_FlagsWord);
                if ((flags & LanceRing.Own) == 0) continue; // host-owned / empty

                uint bufLow = _mem.ReadWord(desc + LanceRing.Rmd0_LadrWord);
                uint bufHigh = _mem.ReadByte(desc + LanceRing.Rmd1_HadrByte);
                uint bufAddr = (bufHigh << 16) | bufLow;
                short negBcnt = (short)_mem.ReadWord(desc + LanceRing.Rmd2_BcntWord);
                int len = -negBcnt;

                var frame = new byte[len];
                for (int i = 0; i < len; i++) frame[i] = _mem.ReadByte(bufAddr + (uint)i);
                _txWire.Enqueue(frame);

                flags &= unchecked((ushort)~LanceRing.Own); // hand descriptor back to the 68000
                _mem.WriteWord(desc + LanceRing.Rmd1_FlagsWord, flags);

                _chipTxIndex = (idx + 1) & (LanceRing.RxDescCount - 1);
                any = true;
                _trace.Info($"TX DMA: desc[{idx}] buf 0x{bufAddr:X6} len {len} -> wire, OWN cleared");
            }

            if (any)
            {
                Csr[0] |= LanceCsr.Csr0_Tint;
                InterruptPending = true;
                _trace.Interrupt("LANCE TX complete", "68000 level 2 (TINT)");
            }
        }

        /// <summary>Host pulls the next transmitted frame off the wire.</summary>
        public bool TryTransmit(out byte[] frame)
        {
            if (_txWire.Count > 0)
            {
                frame = _txWire.Dequeue();
                return true;
            }
            frame = Array.Empty<byte>();
            return false;
        }

        public void ClearInterrupt()
        {
            InterruptPending = false;
            Csr[0] &= unchecked((ushort)~(LanceCsr.Csr0_Rint | LanceCsr.Csr0_Tint | LanceCsr.Csr0_Miss));
        }

        public void Reset()
        {
            Initialized = false;
            InterruptPending = false;
            _txWire.Clear();
            _chipRxIndex = 0;
            _chipTxIndex = 0;
            FilteredCount = 0;
            MissedCount = 0;
            Array.Clear(Csr, 0, Csr.Length);
            InitBlockAddress = 0;
        }

        private static string MacToString(ReadOnlySpan<byte> mac) =>
            $"{mac[0]:X2}:{mac[1]:X2}:{mac[2]:X2}:{mac[3]:X2}:{mac[4]:X2}:{mac[5]:X2}";
    }
}
