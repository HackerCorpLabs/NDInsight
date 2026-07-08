//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
//
// This file: shared-buffer / postbox / LANCE descriptor models.
// Only the XmsgPostboxSlot layout is CONFIRMED (from 0xEACC). The LANCE
// descriptor field layout is the standard Am7990 layout but the firmware's
// exact ring base/length values were not dumped -> TODO_REVERSED_DETAIL.
//

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// XMSG message built by XMRECEIVER (68000 @ 0xBED8) for a received Ethernet
    /// frame, before it is posted to the ND-100 via XMPFRRE / the postbox. The
    /// header fields are CONFIRMED from the disassembly; <see cref="Payload"/> is
    /// the frame the message carries (the exact on-wire XMSG framing the ND-100
    /// decodes is documented in the repo's XMSG protocol notes).
    /// </summary>
    public sealed class XmsgMessage
    {
        /// <summary>+0x14 flags. CONFIRMED = 0x00004000 (bit 14 set) for RX frames.</summary>
        public uint Flags { get; set; } = 0x00004000;

        /// <summary>+0x18 id, read from xmsg_node_id (0x1E21A). CONFIRMED source.</summary>
        public uint NodeId { get; set; }

        /// <summary>+0x24 subtype / count. CONFIRMED = 4 for RX frames.</summary>
        public uint Subtype { get; set; } = 4;

        /// <summary>The received frame this message carries (payload).</summary>
        public byte[] Payload { get; set; } = System.Array.Empty<byte>();
    }

    /// <summary>
    /// Generic shared buffer descriptor for the ND-100 &lt;-&gt; 68000 exchange.
    /// Provisional shape; the CONFIRMED primitive is the single owner word (see
    /// <see cref="XmsgPostboxSlot"/>).
    /// </summary>
    public sealed class SharedBufferDescriptor
    {
        public uint Address { get; set; }
        public ushort Length { get; set; }
        public ushort Flags { get; set; }
        public bool OwnedByFirmware { get; set; }
        public bool OwnedByHost { get; set; }
    }

    /// <summary>
    /// XMSG postbox ring slot. CONFIRMED layout from maybe_xmsg_postbox_send_ring
    /// (0xEACC): tst.w owner; write 3 payload words; clr.w owner to release; the
    /// producer index advances modulo 8 (addq #1; andi #7). 8 bytes per slot.
    /// </summary>
    public sealed class XmsgPostboxSlot
    {
        /// <summary>+0: ownership word. 0 = free/handed to consumer; non-zero = in use.</summary>
        public ushort Owner { get; set; }

        /// <summary>+2 message word 0.</summary>
        public ushort Payload0 { get; set; }

        /// <summary>+4 message word 1.</summary>
        public ushort Payload1 { get; set; }

        /// <summary>+6 message word 2.</summary>
        public ushort Payload2 { get; set; }

        public bool IsFree => Owner == 0;

        /// <summary>Release the slot to the consumer (clr.w owner).</summary>
        public void Release() => Owner = 0;
    }

    /// <summary>
    /// An 8-slot postbox ring with a producer index advanced modulo 8.
    /// CONFIRMED ring size (andi #7) from 0xEACC. There are (HYPOTHESIS) two such
    /// channels: PO100ports and PO100messages.
    /// </summary>
    public sealed class XmsgPostboxRing
    {
        public const int SlotCount = 8; // CONFIRMED: index masked with &7

        private readonly XmsgPostboxSlot[] _slots = new XmsgPostboxSlot[SlotCount];
        private int _producerIndex;

        public XmsgPostboxRing()
        {
            for (int i = 0; i < SlotCount; i++)
                _slots[i] = new XmsgPostboxSlot();
        }

        public XmsgPostboxSlot Current => _slots[_producerIndex];

        public XmsgPostboxSlot this[int index] => _slots[index & (SlotCount - 1)];

        /// <summary>Advance the producer index (addq #1; andi #7). CONFIRMED.</summary>
        public void Advance()
        {
            _producerIndex = (_producerIndex + 1) & (SlotCount - 1);
        }

        public int ProducerIndex => _producerIndex;
    }

    /// <summary>
    /// LANCE Am7990 initialization block. Base = 0x18810 (CONFIRMED). The block is
    /// zero in the static image and is BUILT AT RUNTIME by INITLANCE (0x48EA):
    ///  - MODE (+0) is assembled bit-by-bit (bit15 from flag 0x18888, sets bit 2,
    ///    clears bits 6/5/4/3/1/0).
    ///  - PADR (+2) is the 6-byte MAC copied from lance_mac_address (0x1885E).
    ///  - RDRA/TDRA point at the RX/TX rings (RcvRing 0x18000 / XmtRing 0x18408).
    /// TODO_REVERSED_DETAIL: exact final MODE value and ring lengths (RLEN/TLEN)
    /// depend on runtime config and are not fixed in the static image.
    /// </summary>
    public sealed class LanceInitBlock
    {
        /// <summary>Base address of this init block in DRAM. CONFIRMED = 0x18810.</summary>
        public uint BaseAddress { get; set; } = FirmwareDataAddresses.LanceInitBlock;

        public ushort Mode { get; set; }          // +0  MODE (built at runtime by INITLANCE)
        public byte[] Padr { get; set; } = new byte[6]; // +2 MAC, copied from 0x1885E (CONFIRMED source)
        public byte[] Ladrf { get; set; } = new byte[8]; // +8 logical (multicast) address filter
        public uint Rdra { get; set; }            // +18 RX ring (RcvRing 0x18000) + RLEN
        public uint Tdra { get; set; }            // +22 TX ring (XmtRing 0x18408) + TLEN
    }

    /// <summary>
    /// LANCE Am7990 RX descriptor (4 words). Standard Am7990 layout; ownership is
    /// the OWN bit in MD1. The firmware allocates RX buffers of
    /// <see cref="FirmwareDataAddresses.RxBufferSize"/> = 1520 bytes each
    /// (CONFIRMED at append_rx_buffers_to_ring 0x5BCA).
    /// </summary>
    public sealed class LanceRxDescriptor
    {
        public ushort BufferAddrLow { get; set; } // MD0
        public ushort Md1 { get; set; }           // MD1: OWN(bit15), STP, ENP, high addr bits
        public ushort BufferLength { get; set; }  // MD2 (two's complement byte count; 1520-byte buffers)
        public ushort MessageLength { get; set; } // MD3 (received length / errors)

        public const ushort Own = 0x8000;
        public const ushort Stp = 0x0200;
        public const ushort Enp = 0x0100;

        /// <summary>OWN=1 means the LANCE owns it; OWN=0 means the 68000 owns it.</summary>
        public bool OwnedByLance => (Md1 & Own) != 0;
    }

    /// <summary>
    /// LANCE Am7990 TX descriptor (4 words). Standard Am7990 layout.
    /// TODO_REVERSED_DETAIL: confirm against dumped ring.
    /// </summary>
    public sealed class LanceTxDescriptor
    {
        public ushort BufferAddrLow { get; set; } // TD0
        public ushort Td1 { get; set; }           // TD1: OWN, STP, ENP, high addr bits
        public ushort BufferLength { get; set; }  // TD2 (two's complement byte count)
        public ushort Td3 { get; set; }           // TD3: errors / status

        public const ushort Own = 0x8000;
        public const ushort Stp = 0x0200;
        public const ushort Enp = 0x0100;

        public bool OwnedByLance => (Td1 & Own) != 0;
    }
}
