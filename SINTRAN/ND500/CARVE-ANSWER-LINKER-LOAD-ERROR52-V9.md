# CARVE ANSWER (V9) - B0040C3C accepts a name with a `"` OR a `.`; the arg contract, stated

Answers "H2 refined: B0040C3C IS LOAD's arg-resolver" (relayed 2026-07-18). Builds on
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V8.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V8.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom.asm`. ND-500 VAs (hex).
Tags: **[V]** = read from these bytes; **[I]** = inferred; **[OPEN]** = not proven.

## TL;DR - I under-stated the success condition; there are TWO accept exits

`B0040C3C` succeeds if the parsed argument contains **either a `"` (0x22) OR a `.` (0x2E)** - not
only a `.`. A bare colon `B:NRF` has neither, so it errors 52. This reframes the whole thread:
the arg contract is "quoted string, or a dot-bearing (name.type) spec", and SINTRAN colon form
`B:NRF` satisfies neither at this parser. [V]

## 1. The two accept exits (byte-exact)  [V]

After the stage-loop, exits converge at `B0040D27`; then (`B0040D27`-`B0040D84`):
```
B0040D2F w2 comp r3 ; B0040D31 if > go B0040D44   ; lo>hi -> straight to DEABF
         (lo<=hi:)  QUOTE SCAN:
B0040D33 by1 := $0x22 ('"')
B0040D38 by comp2 @b.0x18+,r1 ; B0040D3C if >< go B0040D40   ; char != '"' -> keep scanning
B0040D3E bi1 clr ; B0040D3F ret                              ; char == '"' -> ACCEPT (exit A)
B0040D40 d loopi b.0x3C,b.0xC0 -> B0040D38                   ; scan [lo..hi] for a '"'
         (no '"' found:) fall to
B0040D44 ... B0040D5C call DEABF (B004D4F4) ... 
B0040D75 (K gate; B0040C44 bookkeeping)
B0040D7D by comp2 b.0x49,$0x4 ; B0040D81 if >< go B0040D85   ; b.0x49 != 4 -> ERROR 52
B0040D83 bi1 clr ; B0040D84 ret                              ; b.0x49 == 4 (a '.') -> ACCEPT (exit B)
```
- **Exit A (`B0040D3F`):** the argument contains a `"` (0x22) somewhere in `[lo..hi]`. [V]
- **Exit B (`B0040D84`):** the stage counter hit 4, which only a `.` (0x2E) does (`;`->2,`)`->3,
  `.`->4; `:` sets flag b.0x4C only). [V, from V5]
- Everything else -> `B0040D85` error 52. `B:NRF` (no `"`, no `.`) -> error. [V]

## 2. Who sets the `{0xB0048FEC,5,9}` = "B:NRF" window  [V]

`B003C8F4` (LOAD's arg handler) builds the descriptor `b.0x20` as a **raw slice of the command
line** `0xB0048FEC` `[lo=5, hi=9]` = "B:NRF" (indices 5-9 of "LOAD B:NRF"). It is handed down through
`B003CFDA` (recursive spec parser: entry `B003CFDA`, two `B0040C3C` call sites at `B003D064` and
`B003D08F`->`B003D0E2`). `B003CFDA` DOES make the dispatcher calls (`B004D4F4` selectors `0x28`/`0x23`
at `B003D012`/`B003D03F`), but on the taken path the descriptor reaching `B0040C3C` is still the raw
`{0xB0048FEC,5,9}` colon window - **no dot-form buffer and no quote are ever inserted.** [V]

So nothing in the emulator's path puts a `"` or `.` into the argument, which is exactly why it
errors. The open question is only whether real HW's argument HAS one - and section 3 gives a live
test that settles it in one line.

## 3. The refined arg contract + a decisive one-line live test

For `B0040C3C` to accept LOAD's object argument, the descriptor it parses must contain a `"` (exit A)
or a `.` (exit B). Candidates for how that happens on real HW, now testable:
1. **The argument is QUOTED.** If the linker's LOAD expects `LOAD "B:NRF"` (double-quote 0x22), the
   quote-scan exit A fires. **TEST: type `LOAD "B:NRF"` (double quotes) in your rig.** If it gets
   past error 52, exit A is the intended contract and the emulator's window-extraction is dropping
   the quotes. [decisive, [I] until you run it]
2. **The argument is dot-form.** `LOAD B.NRF` (or a canonicalizer that emits `name.type`). **TEST:
   type `LOAD B.NRF`.** If it passes, exit B is the contract and colon->dot canonicalization is
   missing. [decisive]
3. If BOTH `"B:NRF"` and `B.NRF` still error 52, the descriptor window itself is wrong (lo/hi or
   ptr), independent of content - then dump the descriptor bytes B003C8F4 builds on a known-good HW
   run. [fallback]

Note SINTRAN's own filespec quoting is the apostrophe `'` (0x27), but this parser scans for `"`
(0x22) - so if quoting is the answer it is the linker's string-literal quote, not SINTRAN's. Run
test 1 and 2; whichever passes names the contract. [V for the 0x22 vs 0x27 distinction.]

## 4. Honest status

This is round ~6 of the error-52 thread. The banked, committed fixes (i1=0; current-user->SYSTEM
fallback) stand. This gate has converged to a single, now-testable question: **does LOAD's argument
need a `"` or a `.` on real HW** (exits A/B), which one line at your prompt (`LOAD "B:NRF"` /
`LOAD B.NRF`) will answer. Recommend pausing further static drilling until that test picks the exit;
the answer then tells us exactly what B003C8F4/the input path must produce.

## Evidence register
- Two exits: quote scan `B0040D33`-`B0040D40` (`04 CD 22 / 2D E5 18 D0 / C6 04 / 84 / 80 / BF 4F 70 F3`);
  stage gate `B0040D7D`-`B0040D84`.
- `B003CFDA` recursive parser entry `B003CFDA`; `B0040C3C` call sites `B003D064` & `B003D0E2`;
  dispatcher `B004D4F4` selectors `0x28`(`B003D012`)/`0x23`(`B003D03F`).
- (From prior rounds) window `{0xB0048FEC,5,9}`="B:NRF" set by B003C8F4; stage map `B0040C74`-`B0040CC7`.
