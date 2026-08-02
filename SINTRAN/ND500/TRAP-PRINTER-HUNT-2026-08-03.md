# Hunting the trap printer: what renders "At program address: 1 10533B"

**Date:** 2026-08-03
**Goal:** find what value the ND-500 trap report is handed for "At program address", because
`1 10533B` does not map to any swapper PSEG instruction boundary and six explanations are
already dead.
**Status:** printer region located and bounded; the exact field source not yet pinned.

---

## 1. Premise verified, not assumed

**The message is printed by the ND-500 monitor program (`MON-DEBUG:PROG`), not by SINTRAN.**
Searched every carved segment in `versions/L-VSX-500/segments/` and both resident images for
the literal `At program address` - **zero hits**. The only image in the tree containing it is
`nd-500-mon-j04-bank2.bin`.

This mattered because the running text arrives interleaved with SINTRAN's own output, and the
notes elsewhere say "SINTRAN prints a PROTECT VIOLATION for shadow process 5SWAP". SINTRAN
prints *that* line; the field report underneath it is the monitor's.

## 2. The string-output primitive `[V]`

**`147211`** - print a byte substring. Verified by reading the loop and by the pointer word
`bank1[154371] = 147211`:

```
147211  entry (JPL -> 177300 ENTER)
147214  LDA ,B -171 / STA ,B -167     start byte index
147216  LDA ,B -170 / STA ,B -165     end index
147224  LDX ,B -167                   loop:
147225  LDT ,B -172                     T := string base (a WORD address)
147226  LBYT                            A := byte (T,X)
147230  SAT 1                           T := 1  (output device)
147232  MON 2                           OutByte
147233  MON 65                          error -> QERMS
147235  X+1 -> B-167
147240  SKP IF DX EQL ST / JMP 147224   until end
```

Parameters are a **word base + byte start + byte end**, passed as the 3-word `F` register
(`STF ,X 6`) - the caller builds it with `SAA <len>` / `SWAP CLD SA DD` / `LDT <base>`.

**It has exactly 25 call sites, all inside `151156`-`154363`.** That bounds the whole report
printer to about 1700 words.

## 3. The numeric renderer `[V]` - this is what produces `10533B`

`154251`-`154365`. Reading it directly:

```
154261  LDX ,B -155                  field index
154262  LDA I ,B ,X -150             <- fetch field value from an ARRAY at B-150
154263  SAD SHR 20                   take the high half (shift right 16)
154265  SAA 5 / STA ,B -153          digit counter = 5 (six digits)
154267  loop:  LDD ,B -130
154272    LDD 100 / RAND             mask
154303    SBYT                       store one digit byte into the buffer at B-126
154305    SAD SHR 3                  shift right 3  <- OCTAL
154324    counter-1, repeat until 0
154330  SAA 5 / SWAP / LDT ,B -126   F := (buffer, 0, 5)
154335  JPL -> 154371 = 147211       print the six digits
154346  LDX ,B -155 / X+1
154351  SAT 7 / SKP IF DX EQL ST     <- loop over EIGHT fields
```

Two things worth having:

- **`SAD SHR 3` with a mask and `SBYT` is an octal digit emitter.** This is where the `B`-suffixed
  numbers in the report are rendered.
- **`LDA I ,B ,X -150` with the outer loop bounded by `SAT 7` means the report prints EIGHT
  numeric fields out of an array at `B-150`.** "At program address" is one entry of that array.
  Finding which index, and where the array is filled, is the remaining step - that is where the
  answer to `1 10533B` lives.

---

## 4. THREE SEARCH METHODS THAT ALL RETURNED NOISE - do not repeat them

Every one of these produced a confident-looking result that was an artefact. Recording them
because each cost real time and the third one nearly got published.

1. **Scanning bank 1 for a word equal to a bank-2 string address.** The pool lives around
   `0o050100`-`0o052100`, and `0o050xxx` **is the `LDT` opcode**. Every "pointer" found in the
   printer region disassembled as an ordinary `LDT <disp>` instruction. Earlier the same method
   over a wider range returned **1217 hits**.

2. **Scanning for a 151-word window of plausible code addresses** (looking for a dispatch
   array): 8 false candidates in bank 1, 65 in bank 2. The bank-2 ones were ASCII text whose
   word values happen to land in the code-address range.

3. **Scanning for `SAA <ascii>` immediates of the characters a template parser must handle**
   (`$ : D I O B`). All six landed within `146472`-`146673`, which reads as an overwhelming
   signal - and is **the MON 60 thunk table**. That table is a dense run of `SAA 0o000`
   .. `SAA 0o177`, so *every* character code in the ASCII range appears in it exactly once,
   adjacent, by construction. The cluster was guaranteed by the table's shape and carried no
   information at all.

**The common failure:** searching for a VALUE in a range that ordinary ND-100 instruction
encodings already occupy, or that a dense table already spans. On this architecture a value
scan is only decisive when the constant is distinctive *and* the hit count is tiny - the one
that worked all session was `0o011547`, which occurs exactly once.

**What worked instead, both times:** search for the *behaviour*, not the data. The command
dispatch fell out of the one-occurrence descriptor-array base; the printer fell out of the
`MON 2` (OutByte) call inventory - bank 1 has only 68 `MON` instructions total and only 5 are
`OutByte`, so the output path was a 5-way choice rather than a 1217-way one.

---

## 5. Next step

Identify the array at `B-150` in the printer's frame: which of its eight entries is
"At program address", and what writes it. That is the last hop to the answer, and it is
bounded - the whole printer is 1700 words with 25 known call sites.
