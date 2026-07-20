# disc-scsi-a47ce5c4

Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 of an installed SINTRAN III system disc.

| | |
|---|---|
| sha256 | `a47ce5c4abf04c366dfa719157d69d08f67676d701258eddb8ae8e88baa283b9` |
| size | 2000 bytes (1000 words) |
| word 0 | `150405` (PIOF) |
| KLIOX word index | 000114B |
| KLIOX value | 000000 |
| implied HDEV | n/a (not a literal IOX - SCSI uses IOXT) |
| YSWTY | 3 = SCSI (SCSWD) |

**Extracted from (read-only):**

- `D:\ND\HDD\sintran_iii_m05_st31200n.image`

**How this page was produced on the real machine** - SINTRAN's own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` (0300B = 192 words) plus the disc-type-specific *swap driver* (`SWDSI` = 1350B = 744 words) into it, patches the parameter words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.
