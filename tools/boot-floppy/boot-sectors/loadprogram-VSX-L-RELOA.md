# loadprogram-VSX-L-RELOA.bin — the UNPATCHED disc LOAD PROGRAM, as shipped

192 words (0300B), big-endian, taken **out of the SINTRAN III/VSX version L
distribution floppy**, not off a disc.

| | |
|---|---|
| sha256 | `d4438d3183f28578a5fa705f030b2bb6054388edef27a4037d2978ff6423f13d` |
| size | 384 bytes = 192 words = `0300B` |
| ND-100 load address | `062417B .. 062716B` |
| symbol | `RELOA` (`062417`) … `LDEND` (`062717`) |
| source file | `SINTRAN-L-1:DATA` inside `D:\ND\S\VSXL1.IMG` |
| BPUN record | record #2 of 22, `)9READ` at file offset 29151, load address `026000B`, `144001B` words, checksum verified |
| file offset of `RELOA` inside `SINTRAN-L-1.DATA` | 58370 |

## How it was extracted

```
ndtool -x -o R1 D:\ND\S\VSXL1.IMG            # NOTE: no -p, parity strip destroys binaries
python tools/decode_9read.py R1/SINTRAN-L-1.DATA --region 1 062417 0300 reloa-L.bin
```

## Why this is the disc bootstrap

* `RELOA=062417` / `LDEND=062717` are **in the shipped symbol table**
  `NDInsight/SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT`.
* The code matches `PH-P2-OPPSTART.NPL` (`% "LOAD" PROGRAM`, label `RELOA`)
  instruction for instruction.
* It is **unpatched**: `KLIOX` still holds the literal source value `IOX 4`
  (`164004`) and the parameter block `NOBLK/ADR2B/DYBLS/LDRAD/XSWTP/YSWTY`
  (`062510B..062516B`) is all zero.
* Word-diffed against a **real installed L-version SMD system disc**
  (`D:\ND\HDD\BIGDISK0-L.IMG`, page-0 sha8 `ec962fc2`) the two agree in
  **176 of the first 192 words**; the 16 that differ are exactly the words
  SINTRAN patches at COLD-START — including `KLIOX 164004 -> 165544`
  (`IOX 1544` = SMD `HDEV 1540 + 4`), `KLHDE 0 -> 001540`, `YSWTY 0 -> 1`
  ("big disc"), `KLRC1 0 -> 177774` (= -4).

See `../DISC-BOOTSTRAP.md` for the full write-up.
