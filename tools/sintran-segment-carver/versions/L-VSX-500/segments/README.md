# Carved SINTRAN III (L07) system segments

Individual **system segments** carved from the SINTRAN III VSX/500 **L07** SMD disk
image (`SEGFIL0`), one big-endian `.bin` per segment, for reverse engineering.
See `../../../README.md` (carver), `../../../ghidra-tasks/` (RE hand-offs),
`../re/` (symbol files + results), and `manifest.json` (raw metadata).

> **Correction note (this L set was just re-carved).** The previously committed L
> `.bin` files were wrong twice over: carved from a **mis-based raw-disk location**
> **and** at **half length**. This set is the first *correct* L carve — extracted from
> `(SYSTEM)SEGFIL0:DATA` at `MADR*2048` for `SEGLE*2048` bytes, big-endian. Every file
> is now **full length** (`Bytes = Pages * 2048`, a page = 2048 bytes). All content-type
> labels that were "confirmed" against the old (wrong) bytes were **re-checked** against
> the corrected bytes; several segments that read as `empty` before now carry data (and
> vice-versa) — see the table.

## Content types in this segment set

The 8-word segment-table entry (`SEGLI PRESE LOGAD SEGLE MADR FLAG SGSTA BPAGL`)
has **no CPU/instruction-set field** — SINTRAN distinguishes a segment's kind by its
**role fixed at system generation** (segment number/name/PIT), not by inspecting the
bytes. `content_type` below is derived from name/description + PIT, and confirmed
with a disassembler pass where marked "(confirmed)".

- **`nd100-code`** — ND-100 kernel code (big-endian, word-addressed). Load in an
  ND-100 big-endian Ghidra processor at the octal `load_address`; for `nd100-dis`
  byte-swap first (little-endian only).
- **`nd100-data`** — not code: page-table images, name tables, segment-table images,
  and `Save of…`/`Image of…` checkpoint copies.
- **`nd500-code`** — ND-500 32-bit, **byte-addressed** machine code. Only `S3SM5`
  (+ its save `S3SSM5`) here. Use `nd500-dis`
  (`/home/ronny/repos/ragge/pcc-nd500/bin/nd500-dis`), **not** the ND-100 tools.
  See the special-case section below and `030-S3SM5-DISASSEMBLY-PROMPT.md`.
- **`nd500-data`** — ND-500 domain images, name tables, and the 5PIT tables SINTRAN
  keeps to manage the ND-500 (data the ND-100 moves, not ND-100 code).
- **`microcode`** — `S3IU110`/`S3IU120` (+ saves): **CPU writable-control-store
  microprogram** for the ND-110 / ND-120. This is loaded into the *CPU's* control
  store at boot — the closest thing here to "firmware loaded into hardware", but it
  is the main CPU's microcode, **not** an I/O-controller's firmware.
- **`empty`** — carved but zero-filled = the subsystem is not installed / not currently
  loaded on this pack (e.g. `S3PATCH`, `S3FUDRT`).

**Ethernet / intelligent-controller firmware:** none is present in this `SEGFIL0`.
Intelligent I/O controllers (Ethernet 68000, HDLC, etc.) load their firmware from
**separate distribution files**, not from a SINTRAN system segment — so there is no
"load into the Ethernet controller's shared memory" segment in this set. (The only
"loaded into hardware" content here is the ND-110/120 CPU microcode above.)

## Special case: 030-S3SM5 — the ND-500 System Monitor (handled differently)

`030-S3SM5.bin` (segment **30 octal**, "ND-500 System Monitor segment") is **ND-500
32-bit byte-addressed machine code**, not ND-100. SINTRAN never executes it — the
**ND-500 processor** does. SINTRAN (ND-100) only *manages* it: loads/swaps/maps it via
`5PIT` (PIT 5) and the ND-500 domain machinery (`X5DP1`/`X5DP2`, `N500DF`, the swapper
/ PLACE-DOMAIN). The "handle it differently" is baked into the segment's **role at
generation** (segment 30 = the ND-500 monitor), not detected from the code.

- ND-500 MON calls (> 0377, e.g. `MON 410/500/511/515`) trap to **this** monitor on
  the ND-500 side — never to the ND-100 level-14 `GOTAB` (0..255 only).
- Full linear ND-500 disassembly: [`030-S3SM5.asm`](030-S3SM5.asm) (octal, addr+bytes).
- Proof it is ND-500: `nd100-dis` finds ~108 control-flow ops in the whole segment vs
  ~6373 in same-size ND-100 `006-S3FS.bin`; `nd500-dis` yields clean ND-500 mnemonics.

## All carved segments

| Seg (oct) | Name | content_type | Load (oct) | Pages | Bytes | MADR | SEGFIL | PIT | Conf | bin | asm | Description |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 003 | S3CP | nd100-code | 30000 | 52 | 106496 | 1408 | 0 | 11 | high | [bin](003-S3CP.bin) | — | Command segment |
| 004 | S3RTL | nd100-code | 30000 | 30 | 61440 | 1972 | 0 | 11 | medium | [bin](004-S3RTL.bin) | — | RT-Loader segment |
| 005 | S3ERRS | nd100-data | 144000 | 1 | 2048 | 2002 | 0 | 7 | medium | [bin](005-S3ERRS.bin) | — | System segment for error program |
| 006 | S3FS | nd100-code | 26000 | 53 | 108544 | 1355 | 0 | 4 | high | [bin](006-S3FS.bin) | — | File system segment |
| 007 | S3DMAC | nd100-code | 64000 | 28 | 57344 | 2003 | 0 | 11 | high | [bin](007-S3DMAC.bin) | — | DMAC segment |
| 010 | S3RTFIL | nd100-code | 0 | 64 | 131072 | 2031 | 0 | 2 | high | [bin](010-S3RTFIL.bin) | — | Rtfii segment |
| 011 | S3ERRL | empty | 0 | 8 | 16384 | 2095 | 0 | 1 | high | [bin](011-S3ERRL.bin) | — | Error log segment |
| 012 | S3SFS | nd100-data | 26000 | 53 | 108544 | 401 | 0 | 1 | medium | [bin](012-S3SFS.bin) | — | Save of file system segment |
| 013 | S3SCP | nd100-data | 26000 | 53 | 108544 | 454 | 0 | 1 | high | [bin](013-S3SCP.bin) | — | Save of command segment |
| 014 | S3ERRP | nd100-code | 30000 | 16 | 32768 | 2103 | 0 | 11 | high | [bin](014-S3ERRP.bin) | — | Error program segment |
| 016 | S3SRPIT | nd100-data | 32000 | 51 | 104448 | 115 | 0 | 1 | high | [bin](016-S3SRPIT.bin) | — | Save of RPIT |
| 017 | S3SMPIT | nd100-data | 32000 | 51 | 104448 | 166 | 0 | 1 | high | [bin](017-S3SMPIT.bin) | — | Save of MPIT |
| 020 | S3SDT5 | empty | 0 | 63 | 129024 | 2119 | 0 | 14 | high | [bin](020-S3SDT5.bin) | — | ND-500 standard domains segment |
| 021 | S3NMS5 | empty | 0 | 63 | 129024 | 2182 | 0 | 13 | medium | [bin](021-S3NMS5.bin) | — | ND-500 name tables segment |
| 022 | S3RFAC | nd100-code | 26000 | 50 | 102400 | 2245 | 0 | 3 | high | [bin](022-S3RFAC.bin) | — | Remote file access segment |
| 024 | S3SSGT | nd100-data | 0 | 16 | 32768 | 321 | 0 | 1 | medium | [bin](024-S3SSGT.bin) | — | Save of segment table |
| 025 | S3IRPIT | nd100-data | 32000 | 51 | 104448 | 1069 | 0 | 1 | medium | [bin](025-S3IRPIT.bin) | — | Image of RPIT |
| 026 | S3IMPIT | nd100-data | 32000 | 51 | 104448 | 1120 | 0 | 1 | medium | [bin](026-S3IMPIT.bin) | — | Image of MPIT |
| 027 | S3ISGT | nd100-data | 0 | 16 | 32768 | 1275 | 0 | 1 | high | [bin](027-S3ISGT.bin) | — | Image of segment table |
| 030 | S3SM5 | nd500-code | 40000 | 48 | 98304 | 1227 | 0 | 5 | medium | [bin](030-S3SM5.bin) | [asm](030-S3SM5.asm) | ND-500 System Monitor segment |
| 031 | S3SSPD | nd100-data | 164000 | 1 | 2048 | 112 | 0 | 7 | high | [bin](031-S3SSPD.bin) | — | Save of spooling data fields |
| 036 | S3TAD | nd100-code | 110000 | 10 | 20480 | 2295 | 0 | 11 | high | [bin](036-S3TAD.bin) | — | TADADM segment |
| 037 | S3RTD | nd100-code | 0 | 64 | 131072 | 2305 | 0 | 1 | high | [bin](037-S3RTD.bin) | — | RT-Loader data segment |
| 040 | S3FUDRT | empty | 164000 | 4 | 8192 | 2369 | 0 | 7 | low | [bin](040-S3FUDRT.bin) | — | File user data segment for RT prog. |
| 041 | S3IMED | nd100-data | 26000 | 1 | 2048 | 2373 | 0 | 1 | high | [bin](041-S3IMED.bin) | — | Image of edit routines |
| 043 | S3PATCH | empty | 174000 | 2 | 4096 | 2374 | 0 | 2 | high | [bin](043-S3PATCH.bin) | — | Used for patching purposes |
| 044 | S3IDPIT | nd100-data | 4000 | 45 | 92160 | 1019 | 0 | 1 | medium | [bin](044-S3IDPIT.bin) | — | Image of DPIT |
| 045 | S3ISYS | nd100-data | 144000 | 3 | 6144 | 1064 | 0 | 1 | high | [bin](045-S3ISYS.bin) | — | Image of system segment |
| 046 | S3S5PIT | nd500-data | 26000 | 5 | 10240 | 268 | 0 | 1 | medium | [bin](046-S3S5PIT.bin) | — | Save of 5PIT segment |
| 050 | S3I5PIT | nd500-data | 26000 | 5 | 10240 | 1222 | 0 | 1 | medium | [bin](050-S3I5PIT.bin) | — | Image of 5PIT segment |
| 053 | S3SDPIT | nd100-data | 4000 | 45 | 92160 | 64 | 0 | 1 | medium | [bin](053-S3SDPIT.bin) | — | Save of DPIT |
| 054 | S3SSYS | nd100-data | 144000 | 3 | 6144 | 109 | 0 | 1 | high | [bin](054-S3SSYS.bin) | — | Save of system segment |
| 060 | S3SECOM | nd100-data | 26000 | 2 | 4096 | 113 | 0 | 1 | high | [bin](060-S3SECOM.bin) | — | Save of extended common |
| 061 | S3IECOM | nd100-data | 26000 | 2 | 4096 | 1067 | 0 | 1 | high | [bin](061-S3IECOM.bin) | — | Image of extended common |
| 062 | S3SSM5 | nd500-code | 40000 | 48 | 98304 | 273 | 0 | 1 | medium | [bin](062-S3SSM5.bin) | — | Save of ND-500 System Monitor |
| 065 | S3SIPIT | nd100-data | 32000 | 51 | 104448 | 217 | 0 | 1 | high | [bin](065-S3SIPIT.bin) | — | Save of IPIT |
| 066 | S3IIPIT | nd100-data | 32000 | 51 | 104448 | 1171 | 0 | 1 | high | [bin](066-S3IIPIT.bin) | — | Image of IPIT |
| 070 | S3SSM | nd100-code | 30000 | 36 | 73728 | 507 | 0 | 1 | high | [bin](070-S3SSM.bin) | — | Save service/mail segment |
| 071 | S3SM | nd100-code | 30000 | 36 | 73728 | 1460 | 0 | 11 | high | [bin](071-S3SM.bin) | — | Service/mail segment |
| 072 | S3SDMWD | nd100-data | 2000 | 4 | 8192 | 667 | 0 | 1 | high | [bin](072-S3SDMWD.bin) | — | Save of disk mirroring WD segment |
| 073 | S3IDMWD | nd100-data | 2000 | 4 | 8192 | 1620 | 0 | 1 | medium | [bin](073-S3IDMWD.bin) | — | Image of disk mirroring WD segment |
| 074 | S3SXMK | nd100-data | 120000 | 24 | 49152 | 543 | 0 | 1 | medium | [bin](074-S3SXMK.bin) | — | Save of XMSG kernel |
| 075 | S3SXROU | nd100-data | 120000 | 40 | 81920 | 567 | 0 | 1 | medium | [bin](075-S3SXROU.bin) | — | Save of XMSG kernel |
| 076 | S3XMK | nd100-code | 120000 | 24 | 49152 | 1496 | 0 | 2 | high | [bin](076-S3XMK.bin) | — | XMSG kernel |
| 077 | S3XROU | nd100-code | 0 | 40 | 81920 | 1520 | 0 | 2 | high | [bin](077-S3XROU.bin) | — | XMSG xrouter segment |
| 100 | S3SDNAM | nd100-data | 164000 | 6 | 12288 | 661 | 0 | 1 | high | [bin](100-S3SDNAM.bin) | — | Save of device name table |
| 101 | S3DNAM | nd100-data | 164000 | 6 | 12288 | 1614 | 0 | 7 | high | [bin](101-S3DNAM.bin) | — | Device name table |
| 102 | S3SXMFI | nd100-data | 0 | 54 | 110592 | 607 | 0 | 1 | high | [bin](102-S3SXMFI.bin) | — | Save of XMSG watchdog (XMFID0) |
| 103 | S3XMFI | nd100-code | 0 | 54 | 110592 | 1560 | 0 | 1 | high | [bin](103-S3XMFI.bin) | — | XMSG watchdog (XMFID0) |
| 104 | S3SNKSE | nd100-data | 30000 | 52 | 106496 | 671 | 0 | 11 | high | [bin](104-S3SNKSE.bin) | — | Save of NUCLEUS server |
| 105 | S3INKSE | nd100-data | 30000 | 52 | 106496 | 1624 | 0 | 11 | high | [bin](105-S3INKSE.bin) | — | Image of NUCLEUS server |
| 106 | S3SNKNA | nd100-data | 0 | 64 | 131072 | 723 | 0 | 1 | high | [bin](106-S3SNKNA.bin) | — | Save of NUCLEUS name server |
| 107 | S3INKNA | nd100-data | 0 | 64 | 131072 | 1676 | 0 | 1 | high | [bin](107-S3INKNA.bin) | — | Image of NUCLEUS name server |
| 110 | S3SU110 | microcode | 0 | 32 | 65536 | 337 | 0 | 1 | medium | [bin](110-S3SU110.bin) | — | Save of ND-110 Microprogram |
| 111 | S3IU110 | microcode | 0 | 32 | 65536 | 1291 | 0 | 1 | medium | [bin](111-S3IU110.bin) | — | Image of ND-110 Microprogram |
| 112 | S3SU120 | microcode | 0 | 32 | 65536 | 369 | 0 | 1 | medium | [bin](112-S3SU120.bin) | — | Save of ND-120 Microprogram |
| 113 | S3IU120 | microcode | 0 | 32 | 65536 | 1323 | 0 | 1 | medium | [bin](113-S3IU120.bin) | — | Image of ND-120 Microprogram |
| 114 | S3SERWC | nd100-data | 0 | 16 | 32768 | 787 | 0 | 1 | medium | [bin](114-S3SERWC.bin) | — | Save of ERS Watchdog program |
| 115 | S3IERWC | nd100-data | 0 | 16 | 32768 | 1740 | 0 | 1 | medium | [bin](115-S3IERWC.bin) | — | Image of ERS Watchdog program |
| 116 | S3SERWD | nd100-data | 0 | 56 | 114688 | 803 | 0 | 1 | high | [bin](116-S3SERWD.bin) | — | Save of ERS Watchdog data |
| 117 | S3IERWD | nd100-data | 0 | 56 | 114688 | 1756 | 0 | 2 | high | [bin](117-S3IERWD.bin) | — | Image of ERS Watchdog data |
| 120 | S3SPRMA | nd100-data | 30000 | 52 | 106496 | 859 | 0 | 11 | high | [bin](120-S3SPRMA.bin) | — | Save of Processor Manager server |
| 121 | S3IPRMA | nd100-data | 30000 | 52 | 106496 | 1812 | 0 | 11 | high | [bin](121-S3IPRMA.bin) | — | Image of Processor Manager server |
| 122 | S3SEVMS | nd100-data | 30000 | 52 | 106496 | 911 | 0 | 11 | high | [bin](122-S3SEVMS.bin) | — | Save of Event Message server |
| 123 | S3IEVMS | nd100-data | 30000 | 52 | 106496 | 1864 | 0 | 11 | high | [bin](123-S3IEVMS.bin) | — | Image of Event Message server |
| 124 | S3SBOPC | nd100-data | 30000 | 52 | 106496 | 963 | 0 | 11 | medium | [bin](124-S3SBOPC.bin) | — | Save of Bopcom Server |
| 125 | S3IBOPC | nd100-data | 30000 | 52 | 106496 | 1916 | 0 | 11 | medium | [bin](125-S3IBOPC.bin) | — | Image of Bopcom Server |
| 126 | S3SMTSE | nd100-data | 30000 | 4 | 8192 | 1015 | 0 | 11 | high | [bin](126-S3SMTSE.bin) | — | Save of MT server |
| 127 | S3IMTSE | nd100-data | 30000 | 4 | 8192 | 1968 | 0 | 11 | high | [bin](127-S3IMTSE.bin) | — | Image of MT server |
| 130 | CFT | nd100-code | — | 37 | 75776 | 8001 | 0 | None | high | [bin](130-CFT.bin) | — |  |
| 131 | SEG131 | nd100-code | — | 9 | 18432 | 8038 | 0 | None | low | [bin](131-SEG131.bin) | — |  |
| 132 | CCT | nd100-code | — | 48 | 98304 | 8047 | 0 | None | high | [bin](132-CCT.bin) | — |  |
| 133 | SEG133 | nd100-code | — | 72 | 147456 | 8095 | 0 | None | low | [bin](133-SEG133.bin) | — |  |
| 134 | SNA3270 | nd100-code | — | 122 | 249856 | 8167 | 0 | None | high | [bin](134-SNA3270.bin) | — |  |
| 135 | XFTRAD | nd100-code | — | 32 | 65536 | 8289 | 0 | None | high | [bin](135-XFTRAD.bin) | — |  |
| 136 | FSASG | nd100-code | — | 33 | 67584 | 8321 | 0 | None | high | [bin](136-FSASG.bin) | — |  |
| 137 | COSPOOL | nd100-code | — | 18 | 36864 | 8354 | 0 | None | high | [bin](137-COSPOOL.bin) | — |  |
| 140 | SEG140 | nd100-code | — | 113 | 231424 | 8372 | 0 | None | low | [bin](140-SEG140.bin) | — |  |
| 141 | SEG141 | nd100-code | — | 37 | 75776 | 8485 | 0 | None | low | [bin](141-SEG141.bin) | — |  |

## Content-type summary

- **empty**: 5
- **microcode**: 4
- **nd100-code**: 24
- **nd100-data**: 42
- **nd500-code**: 2
- **nd500-data**: 2

`empty` is now determined per file from `manifest.json` (`nonzero:false`). Only
`S3ERRL` (011), `S3SDT5` (020), `S3NMS5` (021), `S3FUDRT` (040) and `S3PATCH` (043)
are all-zero on this pack.

`manifest.json` has the full raw metadata (load address, madr, segle, flag, symbol_file) per segment.

## Resident code (not in SEGFIL0)

The memory-resident SINTRAN **common code / data** and the **RT-loader** are *not*
paged segments in `SEGFIL0` — they live in resident memory — so they are extracted
separately into `../resident/`:

- `../resident/SINTRAN-DATA_commoncode.bin` — resident common code.
- `../resident/MACM-AREA-DATA_rtloader.bin` — RT-loader.

See `../../../EXTRACTING-RESIDENT-CODE.md` for the extraction method.
