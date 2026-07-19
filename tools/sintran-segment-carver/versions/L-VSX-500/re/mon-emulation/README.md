# ND-500 MON-call emulation - OPEN (50B) and MAGTP (144B)

Emulation-ready reverse engineering of two SINTRAN III **L-VSX-500** monitor-call
handlers, carved and byte-verified from the L binaries (not the NPL source, which
is a different revision). Produced for the team writing an ND-500 MON-call
emulator whose linker crashes at ~9,400 instructions.

- [`050B-OPEN/`](050B-OPEN/README.md) - MON 50B OpenFile (the critical one)
- [`144B-MAGTP/`](144B-MAGTP/README.md) - MON 144B DeviceFunction (magtape-style)

## Handler locations (byte-verified in `../segments/006-S3FS.bin`, load base `26000B`)

| MON  | Name  | GOTAB[MON] (commoncode) | File-system handler | Notes |
|------|-------|-------------------------|---------------------|-------|
| 50B  | OPEN  | `000000` (fall-through) | `OPENF = 123525B`   | shared worker `FOPEN = 067432B`; siblings `DOPEN` (220B) `103026B`, `OPENS` (235B) `126176B` |
| 144B | MAGTP | `000000` (fall-through) | `MAGTP = 26354B`    | function-code dispatch, range-checked at `26446B` |

Both GOTAB entries read `000000`, i.e. they fall through `MFELL -> CALLPROC` to the
second-level file-system dispatch - confirmed from the bytes at commoncode virtual
`71303B` (50B) and `71377B` (144B), where `GOTAB` base = `71233B`, one word/entry.

## The three semantic questions - answers up front

### Q1. OPEN with an empty / all-zero file name  ->  ERROR, no fallback
**Confidence: HIGH (it is an error), MEDIUM (exact code).**
MON 50B OPEN has **no** "empty name -> default / scratch / init-file" behaviour.
Unnamed opens are *separate* calls (ScratchOpen 235B -> `OPENS`, DirectOpen 220B ->
`DOPEN`). OPENF parses the name, validates the access code (error `104B` "No such
access code" - VERIFIED at `123555B`), then calls `FOPEN` for the directory search.
An all-zero name terminates at the first NUL, matches nothing, and OPEN returns a
non-zero error - most consistently **`056B` "No such file name"**. It never
silently succeeds. The linker's real bug is upstream: the descriptor
`[len=17, ptr=0xB0001DE8]` points at ND-500 memory that was never populated with
the name. The emulator's OPEN must **return an error for an empty name**, and the
fix is to make that pointer reference populated memory.
Details: [`050B-OPEN/OPEN-emulation.md`](050B-OPEN/OPEN-emulation.md).

### Q2. MAGTP (144B) function-code dispatch + buffer contract
**Confidence: HIGH.** Parameter block (5 words): `func code, buffer, LDN, dev-param1,
dev-param2`. The handler range-checks the code (`SAT 100 / SAT 177 / AND 66` at
`26446B..26467B`) and indexes the device driver's function table. Function codes
(octal): `0`=READ `1`=WRITE `2`=READ PARITY `7`=ERASE `12`=WRITE EOF `13`=REWIND
`14`=WRITE SKIP `20`=READ STATUS/TEST UNIT READY `24`=READ LAST STATUS `25`=READ
ERROR COUNTERS `26/27`=READ/WRITE BYTE RECORD `37`=READ EXT STATUS `42`=READ FORMAT
`50/51`=READ/WRITE MULTIPLE RECORDS `60/61`=READ/WRITE DOUBLE AMOUNT `73`=TEST UNIT
READY `75`=INQUIRY. Data moves through the param-2 buffer; status returns in A.
Details: [`144B-MAGTP/MAGTP-emulation.md`](144B-MAGTP/MAGTP-emulation.md).

### Q3. (a) RFILE EOF, (b) illegal/deprecated MON call
**(a) Confidence: HIGH on the code, MEDIUM on the short-block nuance.**
The EOF error code is **`3` "End of File"** (VERIFIED, official error table).
`RFILE = 102130B` and `WFILE = 102132B` share one body, split by a read/write skip
flag (`102130B: BSET ZRO SSK` vs `102132B: BSET ONE SSK` - byte-verified). RFILE
returns the actual number of bytes transferred (an INT4 count). Per ND file-system
semantics, error `3` is raised when a read **begins at or past end-of-file (zero
bytes available)**, not merely because the final block is short: a partial final
block returns its byte count with no error, and EOF (`3`) arrives on the *next*
read. The zero-vs-short branch itself is in the deeper block-transfer worker
(reached via the `102314B` literal) and was not byte-traced, so the short-block
nuance is documented + structurally consistent rather than fully disassembled.

**(b) Confidence: HIGH.** The guess "error 124 for MON 321B" is **wrong on two
counts**:
1. `124B` is a file-system error meaning **"Not opened for sequential read"** - it
   has nothing to do with illegal monitor calls.
2. In L-VSX-500 **MON 321B is not illegal**: `GOTAB[321B] = 112376B` is a non-zero
   handler address (it dispatches to real code), not the MONERR/illegal path.
A genuinely illegal monitor call is handled by `MONERR` (`T=:A; CALL 9ERR(#00)`)
and reported as **internal error `0` "Illegal monitor call"** (internal-error
class), which aborts the program - it does not return `124B`.

## Verification / reproduction

- Carved slices re-read from `../segments/006-S3FS.bin` and compared byte for byte;
  `144B-MAGTP.bin` is `cmp`-identical to the prior verified carve in
  `../mon-analysis/144B-DeviceFunction/`.
- GOTAB read from `../resident/SINTRAN-DATA_commoncode.bin` (big-endian, base 0),
  word at `(71233B + MON#)`.
- Byte-swap for disassembly (nd100-dis is little-endian only):
  `python3 -c "import sys;d=bytearray(open(sys.argv[1],'rb').read());d[0::2],d[1::2]=d[1::2],d[0::2];open(sys.argv[2],'wb').write(d)" IN.bin OUT.le.bin`
  then `nd100-dis -a -S -o -b <decimal load addr> OUT.le.bin`
  (`42837` for OPENF `123525B`, `11500` for MAGTP `26354B`).

## Caveats

- FILSYS (S3FS-segment) symbols match the L binary; **resident** symbol addresses
  are offset by a uniform revision delta, so resident call targets (e.g. `003752B`)
  are given as raw addresses, not names, and were not disassembled.
- Behaviour not reachable from the carved S3FS window (directory search internals,
  per-device MAGTP jump table, RFILE block-transfer EOF branch) is marked
  UNVERIFIED in the per-call emulation notes rather than guessed.
