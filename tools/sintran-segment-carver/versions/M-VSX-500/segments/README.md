# Carved SINTRAN III **M06** system segments (SINTRAN-M-2026)

Segments carved from the SINTRAN III VSX **M** disk `SINTRAN-M-2026/HDD/BIGDISK0-M.IMG`.
Reference set alongside `../../L-VSX-500/` and `../../K-VSX-500/`.

## Provenance
- **Segment table** read live at bank 2 / 0o134000 (`phys:0x2B800`); pointer `dspace:0x8D0`.
  M's table runs to segment **0137 octal** (full modern SINTRAN, like L).
- **Names L-derived** (by segment number from L07) — M's own `LIST-SEGMENT` not yet
  captured; trust the number, treat the name as indicative.
- Load = `LOGAD*1024 & 0xffff`; carved **full-length** from `(SYSTEM)SEGFIL0:DATA` at
  byte offset `MADR*2048` for `SEGLE*2048` bytes. A page is **2048 bytes** (1KW).
  Bins are stored **big-endian** (on-disk order) — byte-swap to little-endian before
  feeding `nd100-dis`, never for Ghidra.
- **Re-carve note**: an earlier M carve read only **1024 bytes/page** (half length). This
  set is the corrected **full-length** extraction, so several segments that previously read
  as all-zero now show data (and vice-versa) — see the content-type column.
- **M HAS the ND-120/ND-110 microcode segments** (`110-113` octal: S3SU110/S3IU110/
  S3SU120/S3IU120, 32 pages each) — unlike K05. The ND-120 microcode SINTRAN downloads
  is in `113-S3IU120.bin`. Its exact **version is not yet decoded** (the segment holds
  the microcode in WCS-download format, not a raw 64-bit microword array — same as L;
  reading microword o020 needs WCS reconstruction, see `../../../../SINTRAN/OS/25-ND120-MICROCODE-VERSION.md`).
- Load/disassembly rules per `../../L-VSX-500/segments/README.md`.

| Seg (oct) | Name | content_type | Load (oct) | Pages | Bytes | MADR | bin | Description |
|---|---|---|---|---|---|---|---|---|
| 003 | S3CP | nd100-code | 30000 | 52 | 106496 | 1444 | [bin](003-S3CP.bin) | Command segment |
| 004 | S3RTL | nd100-code | 30000 | 30 | 61440 | 2044 | [bin](004-S3RTL.bin) | RT-Loader segment |
| 005 | S3ERRS | nd100-data | 144000 | 1 | 2048 | 2074 | [bin](005-S3ERRS.bin) | System segment for error program |
| 006 | S3FS | nd100-code | 26000 | 53 | 108544 | 1391 | [bin](006-S3FS.bin) | File system segment |
| 007 | S3DMAC | nd100-code | 64000 | 28 | 57344 | 2075 | [bin](007-S3DMAC.bin) | DMAC segment |
| 010 | S3RTFIL | nd100-code | 0 | 64 | 131072 | 2103 | [bin](010-S3RTFIL.bin) | Rtfii segment |
| 011 | S3ERRL | empty | 0 | 8 | 16384 | 2167 | [bin](011-S3ERRL.bin) | Error log segment |
| 012 | S3SFS | nd100-data | 26000 | 53 | 108544 | 401 | [bin](012-S3SFS.bin) | Save of file system segment |
| 013 | S3SCP | nd100-data | 26000 | 53 | 108544 | 454 | [bin](013-S3SCP.bin) | Save of command segment |
| 014 | S3ERRP | nd100-code | 30000 | 16 | 32768 | 2175 | [bin](014-S3ERRP.bin) | Error program segment |
| 016 | S3SRPIT | nd100-data | 32000 | 51 | 104448 | 115 | [bin](016-S3SRPIT.bin) | Save of RPIT |
| 017 | S3SMPIT | nd100-data | 32000 | 51 | 104448 | 166 | [bin](017-S3SMPIT.bin) | Save of MPIT |
| 020 | S3SDT5 | empty | 0 | 63 | 129024 | 2191 | [bin](020-S3SDT5.bin) | ND-500 standard domains segment |
| 021 | S3NMS5 | empty | 0 | 63 | 129024 | 2254 | [bin](021-S3NMS5.bin) | ND-500 name tables segment |
| 022 | S3RFAC | nd100-code | 26000 | 50 | 102400 | 2317 | [bin](022-S3RFAC.bin) | Remote file access segment |
| 024 | S3SSGT | nd100-data | 0 | 16 | 32768 | 321 | [bin](024-S3SSGT.bin) | Save of segment table |
| 025 | S3IRPIT | nd100-data | 32000 | 51 | 104448 | 1105 | [bin](025-S3IRPIT.bin) | Image of RPIT |
| 026 | S3IMPIT | nd100-data | 32000 | 51 | 104448 | 1156 | [bin](026-S3IMPIT.bin) | Image of MPIT |
| 027 | S3ISGT | nd100-data | 0 | 16 | 32768 | 1311 | [bin](027-S3ISGT.bin) | Image of segment table |
| 030 | S3SM5 | nd500-code | 40000 | 48 | 98304 | 1263 | [bin](030-S3SM5.bin) | ND-500 System Monitor segment |
| 031 | S3SSPD | nd100-data | 164000 | 1 | 2048 | 112 | [bin](031-S3SSPD.bin) | Save of spooling data fields |
| 036 | S3TAD | nd100-code | 110000 | 10 | 20480 | 2367 | [bin](036-S3TAD.bin) | TADADM segment |
| 037 | S3RTD | nd100-code | 0 | 64 | 131072 | 2377 | [bin](037-S3RTD.bin) | RT-Loader data segment |
| 040 | S3FUDRT | empty | 164000 | 4 | 8192 | 2441 | [bin](040-S3FUDRT.bin) | File user data segment for RT prog. |
| 041 | S3IMED | nd100-data | 26000 | 1 | 2048 | 2445 | [bin](041-S3IMED.bin) | Image of edit routines |
| 043 | S3PATCH | nd100-code | 174000 | 2 | 4096 | 2446 | [bin](043-S3PATCH.bin) | Used for patching purposes |
| 044 | S3IDPIT | nd100-data | 4000 | 45 | 92160 | 1055 | [bin](044-S3IDPIT.bin) | Image of DPIT |
| 045 | S3ISYS | nd100-data | 144000 | 3 | 6144 | 1100 | [bin](045-S3ISYS.bin) | Image of system segment |
| 046 | S3S5PIT | nd500-data | 26000 | 5 | 10240 | 268 | [bin](046-S3S5PIT.bin) | Save of 5PIT segment |
| 050 | S3I5PIT | nd500-data | 26000 | 5 | 10240 | 1258 | [bin](050-S3I5PIT.bin) | Image of 5PIT segment |
| 053 | S3SDPIT | nd100-data | 4000 | 45 | 92160 | 64 | [bin](053-S3SDPIT.bin) | Save of DPIT |
| 054 | S3SSYS | nd100-data | 144000 | 3 | 6144 | 109 | [bin](054-S3SSYS.bin) | Save of system segment |
| 060 | S3SECOM | nd100-data | 26000 | 2 | 4096 | 113 | [bin](060-S3SECOM.bin) | Save of extended common |
| 061 | S3IECOM | nd100-data | 26000 | 2 | 4096 | 1103 | [bin](061-S3IECOM.bin) | Image of extended common |
| 062 | S3SSM5 | nd500-code | 40000 | 48 | 98304 | 273 | [bin](062-S3SSM5.bin) | Save of ND-500 System Monitor |
| 065 | S3SIPIT | nd100-data | 32000 | 51 | 104448 | 217 | [bin](065-S3SIPIT.bin) | Save of IPIT |
| 066 | S3IIPIT | nd100-data | 32000 | 51 | 104448 | 1207 | [bin](066-S3IIPIT.bin) | Image of IPIT |
| 070 | S3SSM | nd100-code | 30000 | 36 | 73728 | 507 | [bin](070-S3SSM.bin) | Save service/mail segment |
| 071 | S3SM | nd100-code | 30000 | 36 | 73728 | 1496 | [bin](071-S3SM.bin) | Service/mail segment |
| 072 | S3SDMWD | nd100-data | 2000 | 4 | 8192 | 667 | [bin](072-S3SDMWD.bin) | Save of disk mirroring WD segment |
| 073 | S3IDMWD | nd100-data | 2000 | 4 | 8192 | 1656 | [bin](073-S3IDMWD.bin) | Image of disk mirroring WD segment |
| 074 | S3SXMK | nd100-data | 102000 | 31 | 63488 | 543 | [bin](074-S3SXMK.bin) | Save of XMSG kernel |
| 075 | S3SXROU | nd100-data | 0 | 33 | 67584 | 574 | [bin](075-S3SXROU.bin) | Save of XMSG kernel |
| 076 | S3XMK | nd100-code | 102000 | 31 | 63488 | 1532 | [bin](076-S3XMK.bin) | XMSG kernel |
| 077 | S3XROU | nd100-code | 0 | 33 | 67584 | 1563 | [bin](077-S3XROU.bin) | XMSG xrouter segment |
| 100 | S3SDNAM | nd100-data | 164000 | 6 | 12288 | 661 | [bin](100-S3SDNAM.bin) | Save of device name table |
| 101 | S3DNAM | nd100-data | 164000 | 6 | 12288 | 1650 | [bin](101-S3DNAM.bin) | Device name table |
| 102 | S3SXMFI | nd100-data | 0 | 54 | 110592 | 607 | [bin](102-S3SXMFI.bin) | Save of XMSG watchdog (XMFID0) |
| 103 | S3XMFI | nd100-code | 0 | 54 | 110592 | 1596 | [bin](103-S3XMFI.bin) | XMSG watchdog (XMFID0) |
| 104 | S3SNKSE | nd100-data | 30000 | 52 | 106496 | 671 | [bin](104-S3SNKSE.bin) | Save of NUCLEUS server |
| 105 | S3INKSE | nd100-data | 30000 | 52 | 106496 | 1660 | [bin](105-S3INKSE.bin) | Image of NUCLEUS server |
| 106 | S3SNKNA | nd100-data | 0 | 64 | 131072 | 723 | [bin](106-S3SNKNA.bin) | Save of NUCLEUS name server |
| 107 | S3INKNA | nd100-data | 0 | 64 | 131072 | 1712 | [bin](107-S3INKNA.bin) | Image of NUCLEUS name server |
| 110 | S3SU110 | microcode | 0 | 32 | 65536 | 337 | [bin](110-S3SU110.bin) | Save of ND-110 Microprogram |
| 111 | S3IU110 | microcode | 0 | 32 | 65536 | 1327 | [bin](111-S3IU110.bin) | Image of ND-110 Microprogram |
| 112 | S3SU120 | microcode | 0 | 32 | 65536 | 369 | [bin](112-S3SU120.bin) | Save of ND-120 Microprogram |
| 113 | S3IU120 | microcode | 0 | 32 | 65536 | 1359 | [bin](113-S3IU120.bin) | Image of ND-120 Microprogram |
| 114 | S3SERWC | nd100-data | 0 | 52 | 106496 | 787 | [bin](114-S3SERWC.bin) | Save of ERS Watchdog program |
| 115 | S3IERWC | nd100-data | 0 | 52 | 106496 | 1776 | [bin](115-S3IERWC.bin) | Image of ERS Watchdog program |
| 116 | S3SERWD | nd100-data | 0 | 56 | 114688 | 839 | [bin](116-S3SERWD.bin) | Save of ERS Watchdog data |
| 117 | S3IERWD | nd100-data | 0 | 56 | 114688 | 1828 | [bin](117-S3IERWD.bin) | Image of ERS Watchdog data |
| 120 | S3SPRMA | nd100-data | 30000 | 52 | 106496 | 895 | [bin](120-S3SPRMA.bin) | Save of Processor Manager server |
| 121 | S3IPRMA | nd100-data | 30000 | 52 | 106496 | 1884 | [bin](121-S3IPRMA.bin) | Image of Processor Manager server |
| 122 | S3SEVMS | nd100-data | 30000 | 52 | 106496 | 947 | [bin](122-S3SEVMS.bin) | Save of Event Message server |
| 123 | S3IEVMS | nd100-data | 30000 | 52 | 106496 | 1936 | [bin](123-S3IEVMS.bin) | Image of Event Message server |
| 124 | S3SBOPC | nd100-data | 30000 | 52 | 106496 | 999 | [bin](124-S3SBOPC.bin) | Save of Bopcom Server |
| 125 | S3IBOPC | nd100-data | 30000 | 52 | 106496 | 1988 | [bin](125-S3IBOPC.bin) | Image of Bopcom Server |
| 126 | S3SMTSE | nd100-data | 30000 | 4 | 8192 | 1051 | [bin](126-S3SMTSE.bin) | Save of MT server |
| 127 | S3IMTSE | nd100-data | 30000 | 4 | 8192 | 2040 | [bin](127-S3IMTSE.bin) | Image of MT server |
| 130 | CFT | nd100-code | 0 | 48 | 98304 | 8073 | [bin](130-CFT.bin) |  |
| 131 | SEG131 | nd100-code | 0 | 37 | 75776 | 8121 | [bin](131-SEG131.bin) |  |
| 132 | CCT | nd100-code | 0 | 32 | 65536 | 8158 | [bin](132-CCT.bin) |  |
| 133 | SEG133 | nd100-code | 0 | 18 | 36864 | 8190 | [bin](133-SEG133.bin) |  |
| 134 | SNA3270 | nd100-code | 0 | 113 | 231424 | 8208 | [bin](134-SNA3270.bin) |  |
| 135 | XFTRAD | nd100-code | 0 | 113 | 231424 | 8391 | [bin](135-XFTRAD.bin) |  |
| 136 | FSASG | nd100-code | 0 | 33 | 67584 | 8358 | [bin](136-FSASG.bin) |  |

## Content-type summary

- **empty**: 4
- **microcode**: 4
- **nd100-code**: 22
- **nd100-data**: 42
- **nd500-code**: 2
- **nd500-data**: 2

Content types come from the full-length re-carve. `empty` means the whole segment reads
all-zero on this disk (verified against `manifest.json` `nonzero:false`): `011 S3ERRL`,
`020 S3SDT5`, `021 S3NMS5`, `040 S3FUDRT`. Formerly-empty segments that now hold data
were reclassified — the ND-500 monitor pair `030 S3SM5` / `062 S3SSM5` as `nd500-code`,
the 5PIT pair `046`/`050` as `nd500-data`, and the code segments `071 S3SM`, `076 S3XMK`,
`130 CFT`, `131 SEG131`, `133 SEG133`, `134 SNA3270`, `136 FSASG` disassemble as
control-flow-dense `nd100-code`.

## Resident code (not in SEGFIL0)

The SINTRAN resident common code/data lives in memory, not in `SEGFIL0`, so it is not one
of the numbered segments above. It was extracted separately into `../resident/`:

- `../resident/SINTRAN-DATA_commoncode.bin` — resident common code/data area.
- `../resident/MACM-AREA-DATA_rtloader.bin` — MACM-area RT-loader resident data.

Extraction method (offsets, sizes, byte order) is documented in
`../../../EXTRACTING-RESIDENT-CODE.md`.
