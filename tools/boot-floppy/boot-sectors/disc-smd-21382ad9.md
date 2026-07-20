# disc-smd-21382ad9

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `21382ad9a556275294dd79c75d2cd94bce13ac960cdd13230c0b9be865bd73c0` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000114B |
| KLIOX value | 165544 |
| implied HDEV | 1540 octal |
| YSWTY | 1 = SMD (big disc, ZBDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\a.IMG`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
