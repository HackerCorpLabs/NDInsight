# disc-smd-ec962fc2

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `ec962fc20d9b75bc9f586d795ac605b5b564d25b7362d99e825dbab21748d672` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000117B |
| KLIOX value | 165544 |
| implied HDEV | 1540 octal |
| YSWTY | 1 = SMD (big disc, ZBDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\BIGDISK0-L.IMG`
- `D:\ND\HDD\BIGDISK0-SCSI.IMG`
- `D:\ND\HDD\c3-k-bd.img`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
