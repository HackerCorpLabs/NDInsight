# XMSG-COMMAND: where its MON 200 calls are (2026-07-27)

Static carve of `(SYSTEM)XMSG-COMMAND:PROG` (111103 bytes, product 210373M - the program
the `X-C:` prompt runs), asking one question: **can it issue `XFWRI`?** If it cannot, the
raw request builder can never send a hand-built XROUT request, and the `XSGMG` route
through this program is closed by construction rather than by failed experiment.

**Method.** Extracted with `ndtool -x SYSTEM/XMSG-COMMAND:PROG D:\BIGDISK0-L.IMG`, then
scanned for the `MON` instruction (`153000B | number`; `MON 200B` = `153200B` = `0xD680`),
big-endian words. Tooling caveat worth repeating: two of my scans were wrong before they
were right - see "Corrections" below.

---

## 1. There are exactly THREE MON 200 instructions [VERIFIED]

In 55551 words of program, at file offsets 25520, 26528 and 60312. Every XMSG call the
program can ever make goes through one of these three.

## 2. Two of them are hardcoded [VERIFIED]

```
offset 25518 : 171001   SAT 1        ; T := 1 = XFDCT
offset 25520 : 153200   MON 200

offset 26526 : 171000   SAT 0        ; T := 0 = XFDUM
offset 26528 : 153200   MON 200
```

`SAT` = `171000B`, "Set argument to T; T := ARG" (ND-06.014.2A page 8484). So these two
sites can only ever be disconnect and the dummy call. Neither can be `XFWRI`.

## 3. The third is the general call wrapper [VERIFIED]

```
offset 60256 : 030343   STF  P-29    ; save the CALLER's T, A, D
   ...
offset 60304 : 044606   LDA  P-58
offset 60306 : 131064   JAZ  +52
offset 60308 : 034311   LDF  P-55    ; reload T, A, D
offset 60310 : 054313   LDX  P-53
offset 60312 : 153200   MON 200
offset 60314 : 030312   STF          ; store the returned T, A, D
offset 60316 : 014314   STX
```

`LDF`/`STF` move the floating accumulator, which on the ND-100 IS the register triple
**(T, A, D)** - exactly the XMSG calling convention: T = function plus option bits, A and D
= parameters. So this routine takes its function code from **the caller's registers**, not
from an immediate.

Two supporting facts confirm the identification:

- The three-word block it loads (offset 60198) is **zero in the file** - a run-time
  variable, not a constant.
- The words immediately before that block are ASCII: `62562 71157 071040 061557 062145` =
  `"error co" "de "` - the `*- XMSG error code: -27 -*` message we saw the program print
  when `OPEN-PORT` was called without privilege. The error reporter lives with the caller.

**Therefore any `XFWRI` must arrive through site 3, in T, from a caller.**

## 4. Which callers, and with what T - NOT ESTABLISHED [INCONCLUSIVE]

This is where a hex scan runs out. To enumerate the callers I need the wrapper's entry
address and every `JPL` that reaches it, and `JPL`'s 8-bit P-relative displacement means
distant callers must go indirect through address cells - which requires the file-offset to
load-address mapping and a real disassembly. That is a Ghidra job (the repo has an ND-100
loader and the `nd100-ghidra` skill), not a PowerShell job.

What I can say:

- `SAT 7` (T := 7 = `XFWRI`) appears 14 times in the image, but `SAT` is a general-purpose
  instruction and none of those sites is followed by anything resembling an XMSG call
  sequence.
- Correlating "`SAT n` followed by a `JPL`" is **too coarse to discriminate**: `JPL` is the
  general subroutine-call instruction, so nearly every `SAT` value in the program appears
  before one. It neither supports nor refutes an `XFWRI` path.
- The function code could also arrive via `LDT` from a variable. Of the 17 `LDT`-before-
  `JPL` sites, none loads a static constant 7; seven are B/X/indirect and cannot be
  resolved without running the program.

**So the carve narrows the question but does not close it.** It remains true that no
observed run ever issued `XFWRI` from this program, and that `MESSAGE-STATUS` reports
length 0 after `BUFFER-READY`
([XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md](XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md)
section 7) - the empirical answer stands, and the carve adds that everything funnels
through a single wrapper, so there is exactly one place left to look.

### The next step, precisely

Load the image into Ghidra with the ND-100 processor, establish the load-address mapping,
find the entry of the routine containing the `MON` at file offset 60312, and list its
callers with the T each one sets. That answers the question outright.

A cheaper dynamic alternative: instrument the emulator to log the caller's P and T on every
`MON 200`, then sweep the whole `X-C:` command surface. Absence across the sweep is not
proof of absence in code, but it is much cheaper than the disassembly and would confirm or
break the hypothesis quickly.

---

## Corrections

Two scans in this session produced confident nonsense before being fixed. Both are recorded
because either would have produced a wrong finding:

1. **PowerShell byte shift.** `($b[$i] -shl 8) -bor $b[$i+1]` on `[byte]` operands silently
   yielded only the low byte, so the first dump around each `MON 200` showed all words as
   `0x00xx` - which looked like data and nearly led to "the program contains no code
   there". Casting to `[int]` first fixed it and the real instructions appeared.
2. **Wrong JPL opcode index.** `JPL` is `134000B`, which is opcode index **23**
   (`47104 / 2048`), not 27. The first "SAT n followed by JPL" scan searched index 27
   (`154000B`, the SAB/SAD argument family) and produced a small, clean-looking set -
   `{2, 10, 12, 13, 14, 18}` - that matched the functions observed live almost too well. It
   was an artefact. The corrected scan is the coarse one described above.

The lesson for anyone continuing: verify every opcode constant against
`ND-06.014.2A EN ND-100 Reference Manual` before trusting a scan, and sanity-check that a
decoded window looks like plausible code.
