# disc-winchester-e2da6491

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `e2da6491aef8452279a4e23c35f03a13bc7589b9166117b02b7c8ceeec35db56` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000114B |
| KLIOX value | 164504 |
| implied HDEV | 500 octal |
| YSWTY | 2 = Winchester (ZWDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\WD0-M.IMG`
- `D:\ND\c3\sintran_m\sintran_m.img`
- `D:\ND\HDD\HD0.IMG`
- `D:\ND\HDD\COPYTEST.IMG`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
