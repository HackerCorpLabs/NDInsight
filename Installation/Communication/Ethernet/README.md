# COSMOS Ethernet II Option (ND-210580) - Installation

**The COSMOS Ethernet II Option, product 210580 revision B01 (1987): product description, distribution floppy image, extracted distribution files, and the reverse engineering of the controller's 68000 firmware.**

The product runs the ENCOS Ethernet/XMSG server on the ND-110063 Ethernet II
controller (PCB 3094), loaded and supervised from the ND-100 side.

---

## Files

| File | Contents |
|------|----------|
| [ND-210580-02-EN.md](ND-210580-02-EN.md) / [ND-210580-02-EN.pdf](ND-210580-02-EN.pdf) | Norsk Data Program Description sheet for product 210580 (dated 87.03.24) |
| `210580B01-XX-01D.img` | Distribution floppy image (directory name on disk: `210580B01-XX-01D`) |
| [x/](x/README.md) | Files extracted from the distribution floppy (ENCOS firmware BPUNs, ND-100-side programs and BRFs) |
| [x/stripped/](x/stripped/README.md) | The four firmware banks stripped out of their BPUN containers as raw 68000 binaries, plus the complete firmware reverse-engineering docs and a C# behavioral model |
| [Schema/](Schema/README.md) | Schematic prints for the Ethernet II card (324534 / PCB 3094, 5 sheets) and the Ethernet III card (324232, 32 sheets, prints D and H) |

---

## Related

- `E:\Dev\Repos\Ronny\RetroCore` `NDBusEthernetII.cs` - the RetroCore emulator of the ND-110063 controller that this material feeds (see the correctness analysis and fix plan under [x/stripped/docs/](x/stripped/docs/README.md))

---

**Parent:** [../README.md](../README.md)
