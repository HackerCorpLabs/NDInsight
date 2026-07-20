# disc-winchester-0ab983b4

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `0ab983b49bef91f1fdb26eed0b56a23590dcb2173fad6ba7df380f999f40b506` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000117B |
| KLIOX value | 164504 |
| implied HDEV | 500 octal |
| YSWTY | 2 = Winchester (ZWDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\1325.img`
- `D:\ND\c3\2024\c3-recovered.img`
- `D:\ND\HDD\disk-dump-1k.img`
- `D:\ND\c3\2024\c3_2024_1.img`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
