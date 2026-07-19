# MON 144B MAGTP -- DeviceFunction

Device-dependent function call: parameter 1 is a *function code* interpreted by
the addressed device's driver (magnetic tape, SCSI streamer, floppy-with-volume,
Versatec). The handler `MAGTP` sets up a per-call frame, range-checks the
function code, and dispatches into a per-device function group. It is the "monster"
call that accreted new sub-functions across SINTRAN versions.

All addresses and values in this document are **octal** unless a decimal suffix is
given. This analysis is written against the **real SINTRAN L bytes** carved from
`006-S3FS.bin`; the L07 symbol tables under
`../../../../../../SINTRAN/NPL-SOURCE/SYMBOLS/L07/` are used only for symbol names.

## Dispatch (byte-verified)

`prove-mon.py 144` output (ground truth):

```
GOTAB base = 071233 octal in tools/sintran-segment-carver/versions/L-VSX-500/resident/SINTRAN-DATA_commoncode.bin
MON 144B:
  GOTAB[144] : file byte 0xe5fe of commoncode.bin, raw = 00 00  -> 000000 octal
  = 000000 -> FALL-THROUGH (no direct handler; dispatched via MFELL/CALLPROC)
```

So `GOTAB[144B] = 000000`. This is a **FALL-THROUGH** monitor call: the level-14
dispatcher finds a zero in the GOTAB word (at commoncode virtual `71377B =
71233B + 144B`) and routes the call through `MFELL` -> `CALLPROC` to a
second-level monitor process. The overlay named by `prove-mon.py`
(`025-S3IRPIT.bin`) is the tool's *default* overlay guess and is **not** the
MAGTP body; the real body lives in the file-system segment.

Handler location (VERIFIED against `FILSYS-SYMBOLS.SYMB.TXT`, L07):

| Item | Value |
|------|-------|
| Segment file | `../../../segments/006-S3FS.bin` (load base `26000B`) |
| Entry symbol | `MAGTP = 26354B` |
| Entry bytes on disk | `04 17 4C 0F` big-endian = `002027 046017` octal (`STZ ,X 27 / LDA ,X 17`) |
| Carve window in this folder | `26354B .. 27037B` (307 words = 614 bytes) |
| File offset of entry in `006-S3FS.bin` | word `26354B - 26000B = 354B` |

The first two entry words disassemble exactly as the `.bin` head bytes
(`0417 4c0f`), confirming the slice is aligned to `MAGTP` and byte-honest.

Sub-entry symbols that fall inside the window (from `FILSYS-SYMBOLS`, L07):

| Symbol | Address | Length to next |
|--------|---------|----------------|
| `MAGTP` | `26354B` | `21B` words to `500RF` |
| `500RF` | `26375B` | `2B` words to `500WF` |
| `500WF` | `26401B` | `4B` words to `XRFIL` |
| `XRFIL` | `26405B` | `2B` words to `XWFIL` |
| `XWFIL` | `26407B` | -- |

These are the "how did I enter" prologue variants (read-file / write-file /
500-side read / 500-side write) that all merge into the common body at `26411B`.

## Instruction walkthrough

The `.ASM` in this folder ([`144B-MAGTP.ASM`](144B-MAGTP.ASM)) is the byte-honest
disassembly of the 307-word closure. Register conventions: `B` = current
monitor-process frame pointer, `X` = a secondary/parameter block pointer, `A`/`T`
scratch. Frame slots are referenced as `,B nn` (displacement into the process
frame) and `,X nn`. Key blocks by address:

**Entry prologue / sub-entry fan-in (`26354B .. 26410B`)**
Five distinct entry points set an operation selector in frame slot `,X 27` and a
sub-selector in `A`, then converge:
- `MAGTP 26354B`: `STZ ,X 27` (selector 0), snapshots `,X 17` into `,X 31`, calls
  a shared worker via `JPL I 63` (-> pointer word, target `26442B`), then a second
  `JPL I 61` (-> `26443B`). It seeds `,B 17/21/23/27` and jumps `JMP I 50`
  (-> `26444B`) into the common setup.
- `500RF 26375B` / `500WF 26401B` / `XRFIL 26405B` / `XWFIL 26407B`: each loads a
  small immediate (`SAA 0/1/2`) into `A` and/or `,X 27` to distinguish read vs
  write and 500-side vs file, then falls into the merge at `26411B`.

**Common setup (`26411B .. 26445B`)**
`STA ,X 25` stores the sub-selector; re-snapshots `,X 17`->`,X 31`; two indirect
worker calls `JPL I 26` (-> `26442B` and `26445B`); tests `,B 21` (a flag) with
`JAZ`/`JAF` and sets it to 0 or 1. `JMP I 12` (-> `26444B`) rejoins.
Note words `26433B..26445B` are the **pointer/indirect-address words** those
`JPL I` / `JMP I` instructions point at (they disassemble as spurious
instructions but are actually address constants -- e.g. `26442B: 004141`,
`26444B: 026446`). Treat `26433B..26445B` as data, not code.

**Function-code range check (`26446B .. 26467B`)** -- the heart of the dispatch:
```
026446  LDA ,B 20        ; A = function code (parameter 1)
026447  SAT 100          ; T = 100B
026450  SKP IF DA GRE ST ; skip if code >= 100B
026451  JMP 026470       ; code < 100B -> "high/standard" path
026452  SAT 177          ; T = 177B
026453  SKP IF DT GRE SA ; skip if 177B >= code (i.e. code <= 177B)
026454  JMP 026470       ; code > 177B -> also to 026470
026455  LDA ,B 25        ; (in-range 100B..177B branch) reload sub-selector
026456  JAF ...          ; ... choose default function slot (100B / 74B)
026461  SAT 1 / EQL      ; special-case code 1
026463  JMP 026554       ; -> error/soft-exit path
026464  LDA 74
026465  STA ,B 30        ; store resolved function index in ,B 30
026466  LDA ,B 20
026467  JMP 026607       ; dispatch tail for the 100B..177B group
```
So codes **>= 100B and <= 177B** are the extended/aliased group; everything else
goes to `26470B`.

**Standard-function masking + table index (`26470B .. 26506B`)**
```
026470  LDT 71           ; T = default index
026471  STT ,B 30
026473  LDA ,B 25        ; sub-selector
026474  AND 66           ; mask -> select a device-function *group*
026475  AAA -77          ; bias
026476  SKP IF 0 GRE SA
026477  JMP 026554       ; out of range -> soft exit
026500  RADD CLD SX DA
026501  LDT I 62         ; index a per-device function pointer table
026502  SKP IF DT UEQ 0  ; is the slot non-zero?
026503  JMP 026507
026504  LDA 60 / STZ I 60
026506  JMP 026553
```
`LDT I 62` is the indirect index into the addressed device's function table. When
the selected slot is zero the code takes the "unsupported function" exit.

**Device-status decode blocks (`26507B .. 26652B`)**
Two near-identical blocks (`26507B..26567B` and `26615B..26653B`) call a shared
worker (`JPL I 57` -> `26566B`; `JPL I 35` -> `26652B`), then use `BSKP ONE/ZRO`
bit-skip instructions on `,X 3` / `,X 7` (device status words) to translate
hardware status bits into SINTRAN error codes: `SAA 132` (`26513B`/`26621B`),
`SAA 174` (`26530B`/`26551B`), `SAA 133` (`26636B`). These are the
tape/streamer status-to-errorcode maps. `26554B..26556B` is the common soft-error
exit: `LDA 14 / STA ,B 12 / JPL I 13` (report code in `,B 12`).

**Buffer transfer tail (`26737B .. 27036B`)** -- two `MOVEW` blocks:
```
026741  SAA 22           ; build source/dest descriptors
026742  RADD CLD SA DL
026743  SAX 7
026744  LDD I 101        ; load a 32-bit pointer (double)
026745  RADD SX DD
026750  MOVEW            ; block move (buffer <-> device frame)
026751  LDX I 72
026752  LDA ,X 34 / STA ,B 46
...
026776  JPL I 56         ; -> 027054 (finalize / STATX device status latch)
```
`STATX` at `26774B` latches device status; `MOVEW` at `26750B` and `27031B`
perform the actual buffer copy described by parameter 2. The routine ends at the
closure boundary `27036B` (`SUB I ,B 42`) with control returning via the
`JPL I` workers into the monitor-process framework rather than a classic MON
skip-return in this window.

## Parameter / register contract

Frame-relative, from the code (VERIFIED unless marked):

| Item | Location | Notes |
|------|----------|-------|
| Function code (param 1) | `,B 20` | VERIFIED: loaded at `26446B`, range-checked `100B/177B`, masked `AND 66` |
| Sub-selector / device group | `,B 25` | VERIFIED: `LDA ,B 25` at `26455B`/`26473B`, drives table index |
| Resolved function index | `,B 30` | VERIFIED: `STA ,B 30` at `26465B`/`26471B` |
| In/out flag | `,B 21` | VERIFIED: set 0/1 at `26426B..26431B`, tested at `26762B` |
| Buffer (param 2) | via `,X`/pointer words at `26744B` (`LDD I 101`) and `27025B` | VERIFIED that a double-word pointer is loaded for `MOVEW`; exact param->slot binding UNVERIFIED (set up by the fall-through/`CALLPROC` prologue outside this window) |
| Logical device number (param 3) | UNVERIFIED in this slice | driver selection happens before entry |
| Device-dependent params 4/5 | UNVERIFIED in this slice | consumed by the per-device table target |
| Error code out | `,B 12` | VERIFIED: `STA ,B 12` at `26555B` before `JPL I 13` (error reporter) |
| Device status | `,X 3`, `,X 7` | VERIFIED: `BSKP` bit tests decode these |

Skip-return / A-register result convention: **UNVERIFIED** here. The classic MON
skip-return and the "bytes transferred in A" result are established by the
`MFELL`/`CALLPROC` wrapper and the calling monitor entry, neither of which is in
this 307-word window. Do not assert a specific A/skip contract from this slice
alone.

## Cross-reference

- Manual: **ND-860228 SINTRAN III Monitor Calls**, *DeviceFunction (144B / 100
  decimal)*. Parameter list (function code, buffer, logical device number, two
  device-dependent parameters) matches [`README.md`](README.md) and the
  `LDA ,B 20` function-code usage seen in the code.
- Function-code table and driver-side behaviour: [`MAGTP-emulation.md`](MAGTP-emulation.md).
- Per-device driver body (jump-table targets, actual buffer handling): lives in
  the device driver source (e.g. `IP-P2-SCSI-MAGTP.NPL`), **outside** this window.
- **NPL caveat:** the L07 NPL source under
  `../../../../../../SINTRAN/NPL-SOURCE/` is a *different revision* than the
  carved L image; NPL addresses are NOT byte truth for this handler. Only the
  symbol *names* (MAGTP/500RF/500WF/XRFIL/XWFIL) were used, and their addresses
  were re-confirmed against the carved bytes on disk.

## Integrity

Validator (`validate-mon-carves.py`), exact result line:

```
ok    mon-emulation/144B-MAGTP/144B-MAGTP.ASM  [26354..27036, 307w]

1/1 checked passed; 0 FAILED; 0 skipped (ND-500)
```

Zero direct branches escape the file: control-flow closure holds over
`26354B..27036B` (307 words). The `.bin` is 614 bytes = 307 words, big-endian, and
its first four bytes (`0417 4c0f`) equal the disassembled entry
`002027 046017` -- the slice is aligned to `MAGTP` and byte-honest.

## Confidence and open questions

- **HIGH confidence:** GOTAB[144B]=000000 fall-through; handler = `MAGTP=26354B`
  in `006-S3FS.bin`; function-code range check (`100B/177B`, `AND 66`); error code
  in `,B 12`; two `MOVEW` buffer-transfer tails; closure integrity.
- **MEDIUM confidence:** slot bindings for `,B 20/21/25/30` -- named from their
  usage in the code, not from an external frame map.
- **LOW / UNVERIFIED:** exact parameter-to-frame-slot binding for buffer / LDN /
  device params (established by the `MFELL`/`CALLPROC` prologue, not in this
  window); the A-register / skip-return output convention; the concrete contents
  and target routines of the per-device pointer table indexed by `LDT I 62`.

**Anomalies:**
1. [`README.md`](README.md) still describes the carve as **"192 bytes = 96 words,
   slice 26354B..26513B"**. That is STALE: the actual `.bin` on disk is **614
   bytes = 307 words**, window `26354B..27036B`, which is what the validator
   passes on. The README's 96-word figure predates the control-flow-closure
   re-carve and should be updated.
2. `prove-mon.py` prints overlay `025-S3IRPIT.bin` as its default guess; the real
   MAGTP body is in `006-S3FS.bin`. The overlay line is a tool default, not a
   finding.
3. Words `26433B..26445B`, `26557B..26565B`, `26642B..26652B` disassemble as
   instructions but are **pointer/address constants** consumed by the neighbouring
   `JPL I` / `JMP I` / worker calls. They are data embedded in the code stream
   (normal ND-100 PLANC layout) and must not be read as executable.
