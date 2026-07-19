# segments-ref/ - canonical byte + disassembly layer (bytes exist ONCE here)

Single source of truth for the segment code behind the MON-call deliverable. Per-call folders
(`../mon-analysis/<call>/`) do not embed `.bin` slices - they **reference** the locations here.

Each `<seg>/` holds four files, all regenerated from the canonical `.bin` by
`../../../../make-segment-ref.py` (a full L rebuild is ~17 s):
- `<seg>.asm` - whole-segment disassembly, load base applied, in-range symbols inserted as
  `>>> NAME(TABLE)` marker lines. **A symbol marker does NOT prove a routine entry** (see below).
- `<seg>.hex` - one word/line: `addr(oct)  word(oct)  hi lo(oct)  byteoff(dec)`.
- `<seg>.symbols.txt` - in-range symbols with their source table.
- `<seg>.meta.md` - load base, word count, sha256, canonical `.bin` path.

**69 of the 79 carved L segments have a bundle here.** The 10 without one (`CFT`, `CCT`,
`SNA3270`, `COSPOOL`, `XFTRAD`, `FSASG`, `SEG131/133/140/141`) have `load_address: null` in
their `.meta.json` - the carver never resolved a load base, so they cannot be disassembled yet.
They are COSMOS/application segments, not SINTRAN kernel, and host no MON worker.

## What is committed to git (the rest is regenerated)

Ground truth = `../../segments/*.bin` (committed). To avoid ~140 MB of regenerable bloat:
- **`.hex` is NOT committed** - it is only a formatted view of bytes already in the `.bin`, and
  needs no special tooling to regenerate.
- **`.asm` is committed only for the 8-segment MON core** (below) - `.asm` is the one artifact a
  visitor cannot reproduce without `nd100-dis` (not in this repo), and these 8 back every MON
  call. The other ~61 `.asm` are one `make-segment-ref.py` command away.
- **`.symbols.txt` + `.meta.md` are committed** for all 69 (small, browsable).

See `../../../.gitignore`.

## The 8-segment MON core

The entire MON deliverable - all 216 implemented calls plus the whole dispatch chain - is backed
by these eight segments:

| Segment | Load | Role in the MON path |
|---------|------|----------------------|
| 044-S3IDPIT | 4000B | **`MCTAB` / `9MCTA` @ 005620B** - the real monitor-call table (216 workers) |
| 026-S3IMPIT | 32000B | **dispatch:** `ENT14 @072167B` + `GOTAB @071233B` (`MGOTA`) + `MFELL @072114B`; also ND-500 level-12 handlers 500..515 |
| 003-S3CP | 30000B | 78 MON workers (SYMBOL-1-LIST): `UECOM`/`COMSB`/`UELOG`, etc. |
| 006-S3FS | 26000B | 72 MON workers (FILSYS): `RDISK`/`WDISK`/`OPFIL`/`MAGTP`/`CPUST`, etc. |
| 025-S3IRPIT | 32000B | 59 MON workers (SYMBOL-2-LIST) |
| 030-S3SM5 | 40000B | 4 workers (N500-SYMBOLS) + ND-500 monitor 410/411/416/417 |
| 004-S3RTL | 30000B | 3 RT-loader workers |
| SINTRAN-DATA_commoncode | 0 | resident common code + data (NOTE: `071233B` here is NOT the GOTAB) |

## The MON dispatch model (byte-verified 2026-07-13)

```
MON N -> ENT14 072167B      level-14 entry (in 026-S3IMPIT / 017-S3SMPIT, NOT commoncode)
      -> X := MEM[MGOTA+N]  MGOTA = 071233B
      -> JMP ,X (072260B)   a DIRECT JUMP - no call, no "CALLPROC bridge"
      -> GOTAB[N] : 32/256 slots = resident fast handlers (incl. MON 200B XMSG)
                   224/256 slots = MFELL 072114B
      -> MFELL   : IRW 20 DP := CALLP 032201B ; MST PID/PIE   = a program-LEVEL switch
      -> MCTAB[N]  (005620B + N, in 044-S3IDPIT)
      -> worker
```

**A MON call's worker address comes from `MCTAB[N]`**, never from `GOTAB[N]` (which is `MFELL`
for 224 of 256 calls) and never from a symbol-name guess. Full derivation:
`../mon-analysis/317B-ExecuteCommand/README.md`; overview: `SINTRAN/CARVING-HANDOFF.md` section 3a.

**Do not read GOTAB out of `SINTRAN-DATA_commoncode`.** Its `071233B` is not the GOTAB (slot 0 =
`000000`, slot 1 = `120303B`; the real values are `MFELL 072114B` and `M1 071633B`). The real
GOTAB is in the monitor-PIT segments, where `ENT14` also lives.

## Byte-offset contract (bin offset from an octal address)
```
word_index  = (octal_addr - load_base)      # subtract in OCTAL
byte_offset = word_index * 2                 # decimal, big-endian hi byte first
Read N words: dd if=<canonical.bin> bs=1 skip=<byte_offset> count=<N*2> | od -An -tx1
Or: grep '^<addr>  ' <seg>.hex  and read the byteoff column.
ND-500 (030-S3SM5): 32-bit units - use the .hex byte offset directly, NOT the *2 rule.
```

## Validation stack (no trust required)
1. **Byte-identity** - `<seg>.asm` decodes the whole canonical `.bin`; `sha256` in `.meta.md`.
2. **Table anchors** - a table is only "the" table if known slots match: GOTAB slot0=`MFELL 072114B`,
   slot1=`M1 071633B`, slot2=`M2 071635B`; MCTAB `005B->RDISK 102021B`, `200B->XMSG 007516B`,
   `144B->MAGTP 026354B`.
3. **Entry test caveat** - "symbol lands on a `021xxx STD I` prologue" holds only for JPL-called
   routines; dispatch-reached handlers save no link (`UECOM 050701B` starts `146141 RADD CLD SL DD`).
4. **Overlay check** - the same virtual address decodes in many segments; pick the segment where
   2-3 sibling symbols all land coherently, not one that merely "disassembles to something".
5. **Second disassembler** - Ghidra (Raw Binary, ND-100 big-endian 16-bit, base = load column) agrees.
