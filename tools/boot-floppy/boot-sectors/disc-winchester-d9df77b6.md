# disc-winchester-d9df77b6

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `d9df77b66a71bd6755f4b6042dbca544c10398f5c7d544e2577c59ecc3dc48b3` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000107B |
| KLIOX value | 164504 |
| implied HDEV | 500 octal |
| YSWTY | 2 = Winchester (ZWDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\tingo_raw_debug.img`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
