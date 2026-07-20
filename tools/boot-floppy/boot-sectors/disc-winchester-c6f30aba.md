# disc-winchester-c6f30aba

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `c6f30abab5caaf57c5f3065a0ce2c1ea6ceac03490b0e1d7048808f7f5bef4d7` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000117B |
| KLIOX value | 164504 |
| implied HDEV | 500 octal |
| YSWTY | 2 = Winchester (ZWDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\disk-dump.img`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
