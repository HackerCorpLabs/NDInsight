//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: the 512 KB shared DRAM plus its mirror window, with big-endian
// access helpers (the 68000 is big-endian). The ND-100 sees the same DRAM
// through its bank window at 0xF80000 (mirror of 0x000000-0x07FFFF).
//

using System;

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Models the controller's 512 KB local/shared DRAM. Big-endian word/long
    /// helpers match the MC68000. The mirror region (0xF80000+) folds back onto
    /// the low DRAM so ND-100-window addresses resolve to the same bytes.
    /// </summary>
    public sealed class SharedMemory
    {
        /// <summary>Local DRAM size in bytes. CONFIRMED (512 KB, image is 0x80000).</summary>
        public const int DramSize = 512 * 1024;

        public const uint DramBase = 0x000000;
        public const uint IoBase = 0xEF0000;
        public const uint ProtectBase = 0xF00000;
        public const uint DramMirrorBase = 0xF80000;

        private readonly byte[] _dram = new byte[DramSize];
        private readonly FirmwareTrace _trace;

        public SharedMemory(FirmwareTrace trace)
        {
            _trace = trace;
        }

        /// <summary>Direct span over the DRAM (for loaders and descriptor models).</summary>
        public Span<byte> Dram => _dram;

        /// <summary>
        /// Fold any address that targets the mirror window back onto low DRAM.
        /// I/O and protection-table addresses are NOT memory and must not reach here.
        /// </summary>
        private static uint ToDramOffset(uint address)
        {
            if (address >= DramMirrorBase)
                address -= DramMirrorBase; // mirror of 0x000000-0x07FFFF
            return address & (DramSize - 1);
        }

        public byte ReadByte(uint address)
        {
            return _dram[ToDramOffset(address)];
        }

        public ushort ReadWord(uint address)
        {
            uint o = ToDramOffset(address);
            // Big-endian: high byte first.
            return (ushort)((_dram[o] << 8) | _dram[o + 1]);
        }

        public uint ReadLong(uint address)
        {
            uint o = ToDramOffset(address);
            return (uint)((_dram[o] << 24) | (_dram[o + 1] << 16) | (_dram[o + 2] << 8) | _dram[o + 3]);
        }

        public void WriteByte(uint address, byte value)
        {
            _dram[ToDramOffset(address)] = value;
        }

        public void WriteWord(uint address, ushort value)
        {
            uint o = ToDramOffset(address);
            _dram[o] = (byte)(value >> 8);
            _dram[o + 1] = (byte)value;
        }

        public void WriteLong(uint address, uint value)
        {
            uint o = ToDramOffset(address);
            _dram[o] = (byte)(value >> 24);
            _dram[o + 1] = (byte)(value >> 16);
            _dram[o + 2] = (byte)(value >> 8);
            _dram[o + 3] = (byte)value;
        }

        /// <summary>Load a firmware image (as the ND-100 does before release-from-reset).</summary>
        public void LoadImage(ReadOnlySpan<byte> image, uint at = 0)
        {
            for (int i = 0; i < image.Length; i++)
                _dram[(at + (uint)i) & (DramSize - 1)] = image[i];
            _trace.Info($"Loaded {image.Length} bytes of firmware image at 0x{at:X6}");
        }

        /// <summary>Clear all DRAM (controller Master Clear does not do this on real HW; model convenience).</summary>
        public void Clear()
        {
            Array.Clear(_dram, 0, _dram.Length);
        }
    }
}
