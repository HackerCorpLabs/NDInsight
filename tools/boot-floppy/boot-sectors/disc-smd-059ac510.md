# disc-smd-059ac510

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `059ac51007f01ee0b4a98c72336f9167743be22e8fd792dfdac133ce8b30cded` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000117B |
| KLIOX value | 165544 |
| implied HDEV | 1540 octal |
| YSWTY | 1 = SMD (big disc, ZBDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\BIGDISK0.IMG`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
