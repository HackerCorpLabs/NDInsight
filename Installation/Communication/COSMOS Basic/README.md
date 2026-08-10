# COSMOS Basic Module (ND-10374) - Installation

**The COSMOS Basic Module, ND number 210374, revision E04: distribution media, product sheet, and a verified installation guide.**

---

## Files

| File | Contents |
|------|----------|
| [COSMOS-Basic-Install-Guide.md](COSMOS-Basic-Install-Guide.md) | Installation and start guide for rev E04 on SINTRAN III/VSX (verified on VSX/500 version L). Records an actual, successful installation. |
| [COS-BOOT-WIRING.md](COS-BOOT-WIRING.md) | How COSMOS hooks into cold/warm start (the COSMOS-specific boot integration; the generic boot mechanism is in [../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md](../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md)) |
| [ND-895036-2-EN.md](ND-895036-2-EN.md) / [ND-895036-2-EN.pdf](ND-895036-2-EN.pdf) | Product Information sheet for COSMOS Basic Module 210374G (describes the later rev G; the procedure matches this E04 media, with `-G` file names becoming `-E`) |
| `210374E04-XX-01D.img` | Distribution floppy image (directory name on disk: `210374E04-XX-01D`) |
| [x/](x/README.md) | Files extracted from the distribution floppy (PROG/BPUN/MODE) |

---

## The message system underneath

COSMOS Basic (210374) runs on **X-MESSAGE** (210373), the message system that carries every
COSMOS service. Its own program description is filed with the other install documents:

| Document | Contents |
|----------|----------|
| [../../Installation-Description/ND-210373L-EN.md](../../Installation-Description/ND-210373L-EN.md) | **X-MESSAGE version L (210373L, 1988-02-02, 37 pages)** — installation and loading procedure, plus register-level specs for the XMSG functions and XROUT services that changed in version L, the XMFIDO watchdog letter layouts, and the new/changed error codes. The two products are adjacent ND numbers and ship together; this is the one to read when a COSMOS service fails at the message layer. |

---

**Parent:** [../README.md](../README.md)
