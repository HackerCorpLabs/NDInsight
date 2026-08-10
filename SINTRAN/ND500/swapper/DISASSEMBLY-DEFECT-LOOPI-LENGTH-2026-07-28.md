# Defect in swapper-k01-pseg.asm: LOOPI instruction length is wrong

**Status:** CONFIRMED, three independent ways.
**Affects:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-pseg.asm`
**Impact:** instruction boundaries after any `loopi` are wrong, so every instruction the
listing shows after one is unreliable.

## The defect

At `1000077760` the listing renders:

```
1000077760: 374 337 106 316 000 376 354 030 105 014     w loopi  b.30,$376,$1777777777775406042414
1000077772: 242 030                                     w3 or    $30
```

It consumes **10 bytes** and places the next instruction at `1000077772`. That is wrong.
The instruction is **7 bytes** and the next instruction is at `1000077767`.

## Evidence

**1. The manual: 0xFCDF is the BYTE-displacement form.**
`Reference-Manuals\ND-05.009.4 EN ND-500 Reference Manual.md`, the LOOPI table (line 7437
onward):

```
| H LOOPI:B  | halfword loop increment | OFCDFH | 176337B |
| H LOOPI:H  | halfword loop increment | OFD1FH | 176437B |
```

`374 337` = 0xFCDF = `H LOOPI:B`. LOOPI has only `:B` and `:H` forms - unlike GO there is
no `:W` - so a four-byte displacement cannot occur on this instruction at all.

**2. The displacement arithmetic works out exactly.**
Same manual, section 8.13 (line 3846): the displacement "is signed, and is the distance
from the first byte of the current instruction to the first byte of the addressed
instruction."

Byte `354` = 0xEC = **-20** signed. `0o77760` = 32752 decimal; 32752 - 20 = 32732 =
`0o77734`.

**3. The live CPU branches exactly there.**
A RetroCore instruction trace of the running swapper (2026-07-28) shows the loop body
repeating and the branch landing on `0x08007FDC` = `0o77734`. Byte-for-byte agreement.

## Correct decode

```
374 337   opcode 0xFCDF  H LOOPI:B
106       operand 1, index: b.30            (1 byte)
316 000 376  operand 2, limit: constant 376B (3 bytes)
354       displacement -20 -> 0o77734        (1 byte)
                                             = 7 bytes total
```

The remaining bytes the listing swallowed - `030 105 014` - are the start of the NEXT
instructions. Reading on from `0o77767`, every opcode the emulator decodes matches the raw
bytes: `030`, `014`, `030`, `040`, then `303` (the call at `0o77777`), which is the one
address where the wrong and right decodes happen to converge.

## What this invalidates

Any analysis that read instructions from this listing immediately after a `loopi`. In
particular, an earlier note in
`SINTRAN\ND500\OCTOBUS-SWAPPER-HANDOFF-2026-07-25.md` described the caller at
`1000077777` as executing `w3 or $30` / `by test $40` / `bi2 clr` before its call. Those
instructions do not exist - they are an artifact of the mis-alignment.

Findings that do NOT depend on this are unaffected: the `loopi` itself, everything before
it, and the call at `0o77777` (both decodes agree on that address).

## Recommended action

Regenerate the disassembly with a corrected LOOPI/LOOP/LOOPD length rule (`:B` forms take
a 1-byte displacement, `:H` forms 2 bytes), then re-check any carving that walked past a
loop instruction.
