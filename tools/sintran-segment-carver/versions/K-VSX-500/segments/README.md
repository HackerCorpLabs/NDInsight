# Carved SINTRAN III **K05** system segments (SINTRAN-K05-XMSG-2026)

Segments carved from the SINTRAN III VSX **K** disk
`SINTRAN-K05-XMSG-2026/HDD/BIGDISK0-K.IMG` (generated 1998-07-06). Reference set for
cross-version comparison with `../../L-VSX-500/`.

## Provenance
- **Names** are authoritative — from the live `@RT-LOADER LIST-SEGMENT` (see
  `../inputs/list-segment.txt`); RT programs in `../inputs/list-rt-programs.txt`.
- **Segment table** read live at bank 2 / 0o136000 (`phys:0x2BC00`); pointer at
  `dspace:0x8D0` (version-stable). K's table ends at segment **066 octal**
  (fewer/renamed segments vs L: S3COM not S3CP, S3FSSV, S3OPCSV, S3SERD/S3IERD, ...).
- **Full-length carve**: each segment is `(SYSTEM)SEGFIL0:DATA` read at `MADR*2048`
  for `SEGLE*2048` bytes (big-endian, native). One page = 2048 bytes, so
  `Bytes = Pages * 2048`.
- **K05 has NO ND-120/ND-110 microcode segments** — those were added in L07
  (`S3IU120` etc). K downloads CPU microcode via the standalone `MACM-1718K` loader.
- Load/disassembly rules same as the L set — see `../../L-VSX-500/segments/README.md`.

| Seg (oct) | Name | src | content_type | Load (oct) | Pages | Bytes | MADR | bin | Description |
|---|---|---|---|---|---|---|---|---|---|
| 003 | S3COM | K | nd100-code | 30000 | 52 | 106496 | 477 | [bin](003-S3COM.bin) | Command segment |
| 004 | S3RTL | K | nd100-code | 30000 | 30 | 61440 | 529 | [bin](004-S3RTL.bin) | RT-Loader segment |
| 005 | S3ERRS | K | nd100-code | 130000 | 1 | 2048 | 559 | [bin](005-S3ERRS.bin) | System segment for error program |
| 006 | S3FS | K | nd100-code | 26000 | 53 | 108544 | 560 | [bin](006-S3FS.bin) | File system segment |
| 007 | S3DMAC | K | nd100-code | 64000 | 28 | 57344 | 614 | [bin](007-S3DMAC.bin) | DMAC segment |
| 010 | S3RTFIL | K | nd100-code | 0 | 64 | 131072 | 642 | [bin](010-S3RTFIL.bin) | Rtfii segment |
| 011 | S3ERRL | K | empty | 0 | 8 | 16384 | 706 | [bin](011-S3ERRL.bin) | Error log segment |
| 012 | S3FSSV | K | nd100-data | 26000 | 53 | 108544 | 191 | [bin](012-S3FSSV.bin) | Save of file system segment |
| 013 | S3OPCSV | K | nd100-data | 26000 | 53 | 108544 | 244 | [bin](013-S3OPCSV.bin) | Save of command segment |
| 014 | S3ERRP | K | nd100-code | 30000 | 10 | 20480 | 714 | [bin](014-S3ERRP.bin) | Error program segment |
| 016 | S3SRPIT | K | nd100-data | 26000 | 53 | 108544 | 69 | [bin](016-S3SRPIT.bin) | Save of RPIT |
| 017 | S3SMPIT | K | nd100-data | 26000 | 53 | 108544 | 122 | [bin](017-S3SMPIT.bin) | Save of MPIT |
| 020 | S3SDT5 | K | empty | 0 | 63 | 129024 | 724 | [bin](020-S3SDT5.bin) | ND-500 standard domains segment |
| 021 | S3NMS5 | K | empty | 0 | 63 | 129024 | 787 | [bin](021-S3NMS5.bin) | ND-500 name tables segment |
| 022 | S3RFAC | K | nd100-code | 26000 | 44 | 90112 | 850 | [bin](022-S3RFAC.bin) | Remote file access segment |
| 024 | S3SSGT | K | nd100-data | 0 | 16 | 32768 | 175 | [bin](024-S3SSGT.bin) | Save of segment table |
| 025 | S3IRPIT | K | nd100-data | 26000 | 53 | 108544 | 350 | [bin](025-S3IRPIT.bin) | Image of RPIT |
| 026 | S3IMPIT | K | nd100-data | 26000 | 53 | 108544 | 403 | [bin](026-S3IMPIT.bin) | Image of MPIT |
| 027 | S3ISGT | K | nd100-data | 0 | 16 | 32768 | 461 | [bin](027-S3ISGT.bin) | Image of segment table |
| 030 | S3SM5 | K | nd500-code | 40000 | 48 | 98304 | 894 | [bin](030-S3SM5.bin) | ND-500 System Monitor segment |
| 031 | S3SSPD | K | nd100-data | 150000 | 1 | 2048 | 68 | [bin](031-S3SSPD.bin) | Save of spooling data fields |
| 033 | S3XMSGP | K | nd100-code | 120000 | 24 | 49152 | 942 | [bin](033-S3XMSGP.bin) |  |
| 034 | S3XMSGD | K | nd100-code | 0 | 40 | 81920 | 966 | [bin](034-S3XMSGD.bin) |  |
| 036 | S3TAD | K | nd100-code | 110000 | 10 | 20480 | 1006 | [bin](036-S3TAD.bin) | TADADM segment |
| 037 | S3RTD | K | nd100-code | 0 | 64 | 131072 | 1016 | [bin](037-S3RTD.bin) | RT-Loader data segment |
| 040 | S3FUDRT | K | empty | 150000 | 4 | 8192 | 1080 | [bin](040-S3FUDRT.bin) | File user data segment for RT pr |
| 041 | S3IMED | K | nd100-data | 26000 | 1 | 2048 | 1084 | [bin](041-S3IMED.bin) | Image of edit routines |
| 043 | S3PATCH | K | nd100-code | 174000 | 2 | 4096 | 1085 | [bin](043-S3PATCH.bin) | Used for patching purposes |
| 044 | S3IDPIT | K | nd100-data | 4000 | 35 | 71680 | 27 | [bin](044-S3IDPIT.bin) | Image of DPIT |
| 045 | S3ISYS | K | nd100-data | 130000 | 3 | 6144 | 65 | [bin](045-S3ISYS.bin) | Image of system segment |
| 046 | S3S5PIT | K | nd500-data | 26000 | 5 | 10240 | 297 | [bin](046-S3S5PIT.bin) | Save of 5PIT segment |
| 050 | S3I5PIT | K | nd500-data | 26000 | 5 | 10240 | 456 | [bin](050-S3I5PIT.bin) | Image of 5PIT segment |
| 061 | S3IERD | K | nd100-data | 112000 | 2 | 4096 | 63 | [bin](061-S3IERD.bin) | Image of extended common |
| 062 | S3SSM5 | K | nd500-code | 40000 | 48 | 98304 | 302 | [bin](062-S3SSM5.bin) | Save of ND-500 System Monitor |
| 065 | S3SIPIT | L? | nd100-data | 0 | 64 | 131072 | 6712 | [bin](065-S3SIPIT.bin) | Save of IPIT |

## Content-type summary

- **nd100-code**: 13
- **nd100-data**: 14
- **nd500-code**: 2
- **nd500-data**: 2
- **empty**: 4

On this K disk the four ND-500 / error-log / RT file-user segments 011 S3ERRL,
020 S3SDT5, 021 S3NMS5 and 040 S3FUDRT carve to all-zero (`empty`) — they are
allocated on `SEGFIL0` but had not been populated in this image.

## Resident code (not in SEGFIL0)

The SINTRAN resident common code/data does **not** live on `SEGFIL0` — it is
extracted directly from the running image into `../resident/`:

- `../resident/SINTRAN-DATA_commoncode.bin` — resident SINTRAN common code
- `../resident/MACM-AREA-DATA_rtloader.bin` — MACM-area / RT-loader resident data

The extraction method (addresses, banks, byte order) is documented in
`../../../EXTRACTING-RESIDENT-CODE.md`.
