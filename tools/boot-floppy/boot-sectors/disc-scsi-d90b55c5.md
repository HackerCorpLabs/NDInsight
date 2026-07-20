# disc-scsi-d90b55c5

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `d90b55c504841c4e85b1703e0525a299be8470444f6af6edcbb3c8546bf6bd70` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000107B |
| KLIOX value | 170400 |
| implied HDEV | n/a (not a literal IOX - SCSI uses IOXT) |
| YSWTY | 3 = SCSI (SCSWD) |

**Extracted from (read-only):**

- `D:\ND\HDD\scsi-1.img`
- `D:\ND\HDD\test.IMG`
- `D:\ND\HDD\disk.image`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
