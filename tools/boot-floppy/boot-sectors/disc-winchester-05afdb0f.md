# disc-winchester-05afdb0f

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `05afdb0f9a69a48e8d204042dab1f3caf00f39227ad4bf9d6af5ce872d9128e0` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000117B |
| KLIOX value | 164504 |
| implied HDEV | 500 octal |
| YSWTY | 2 = Winchester (ZWDIS) |

**Extracted from (read-only):**

- `D:\ND\HDD\WD0.img`
- `D:\ND\HDD\WD0-L.img`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
