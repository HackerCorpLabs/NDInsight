# disc-scsi-70c3d994

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `70c3d99411a1862777837b8b22f17f4e192d4b3c54753a936cd930bbfebdc5af` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000107B |
| KLIOX value | 170400 |
| implied HDEV | n/a (not a literal IOX - SCSI uses IOXT) |
| YSWTY | 3 = SCSI (SCSWD) |

**Extracted from (read-only):**

- `D:\ND\HDD\scsi-k.img`
- `D:\ND\HDD\SCSI-K.image`
- `D:\ND\HDD\tor-disk.img`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
