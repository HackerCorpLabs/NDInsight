# disc-unknown-1da707c1

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `1da707c1defd5aa6f5d1c092ac0b24069f08e4ec414a5ccb00d9c4bb55184b8a` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | not located |
| KLIOX value | - |
| implied HDEV | n/a (not a literal IOX - SCSI uses IOXT) |
| YSWTY | ? = ? |

**Extracted from (read-only):**

- `D:\ND\HDD\BIGDISK0-H.IMG`
- `D:\ND\HDD\WD.IMG`
- `D:\ND\HDD\BDH.IMG`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
