# CARVE ANSWER (V8) - LOAD is a real command; dispatch is data-driven; the arg-type fork is in the descriptor/executor layer

Answers the "wrong-path routing CONFIRMED" follow-up (relayed 2026-07-18). Builds on
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V7.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V7.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom` (+ `.dom.asm`). ND-500 VAs (hex).
Addressing: PSEG file = VA - 0xB0000000 + 0x1000; DSEG file = VA - 0xB0000000 + 0x57800.
Tags: **[V]** = read from these bytes; **[I]** = inferred; **[OPEN]** = not proven.

## TL;DR

Your runtime proof (LOAD never touches the object-open subtree; dies in the B003Cxxx symbol/spec
resolver at B0040C3C) is accepted. New static facts this round:
- **LOAD IS a real, first-class command** (not unrecognized). The DSEG carries the command-name
  tables; a restricted set exactly matches your session's commands. So the fork is NOT "LOAD
  unrecognized -> treated as symbol"; LOAD is dispatched as a command, and its **argument** is what's
  being sent to the wrong (symbol/spec) parser. [V]
- Your dispatch corrections are confirmed and my earlier `0xB00530BC`/`B004ACD9` model is **dead**
  (poisoned prior, deleted). The real dispatch is **data-driven + indirect (`jumpg`/CPS)**, which is
  why static caller-tracing saturates. [V]
- I mapped the command NAME tables but **not yet the handler table**; the file-vs-symbol routing lives
  one layer deeper, in the per-command descriptor handed to executor `B002EF7F`. That layer is
  faster for you to read live than for me to decode statically - see the probe.

## 1. LOAD is a real command - the DSEG command tables  [V]

The command-name strings live in the **DSEG** (not in the .asm; data part starts at file 0x58000).
Packed, alphabetical full set at file **0x88a54** (`APPEND-DOMAIN`, ... `LOAD`, ... `SPECIAL-LOAD`,
`MULTI-DOMAIN-HANDLER`). `LOAD` is at file **0x88b52**. [V]

A **restricted command set** is at file **0x88cf4**: `CLOSE EXIT LIST-DOMAINS LIST-ENTRIES
LIST-STATUS LOAD OPEN-DOMAIN SET-ADVANCED-MODE` - **exactly the commands your session used
(OPEN-DOMAIN, LOAD).** Its lookup table is at file **0x88c94**, 8 entries of `{name_VA:4, 0:4,
name_len:4}`; all 8 verified (len = full name length):
```
CLOSE  ->name 0xB00314F4 len5   EXIT ->0xB00314F9 len4   LIST-DOMAINS ->0xB00314FD len12
LIST-ENTRIES ->0xB0031509 len12 LIST-STATUS ->0xB0031515 len11
LOAD   ->name 0xB0031520 len4   OPEN-DOMAIN ->0xB0031524 len11  SET-ADVANCED-MODE ->0xB003152F len16
```
[V - all 8 name pointers land on the ASCII at DSEG file 0x88cf4+]. **Correction to self:** these are
NAME pointers, not handler pointers - so this identifies LOAD as a command but does not yet give its
handler. [V]

## 2. Dispatch is data-driven + CPS - why static tracing failed  [V]

- The command executor uses a shared prologue at `B0031504`-`B003152B`: it builds a context block in
  `0xB003DFE4`-`0xB003DFF8` (writes a continuation `0xB0031531` to `0xB003DFE4` at `B0031504`, saves
  `b`/`tos`), then **`jumpg $0xB003DFE8`** to a common dispatcher. [V]
- The continuation `B0031531` runs the actual command: `w test 0xB003E3F0 ... w bmove 0xB003EE90,
  r.0x14 ; call B002EF7F` (`B0031540`-`B0031549`). So a **command DESCRIPTOR at `0xB003EE90`** is
  handed to executor **`B002EF7F`**. [V]
- Control flow throughout is **indirect `jumpg` through saved pointers** (`b.0xD0`, `b.0x120`,
  `0xB003DFE8`, ...). Direct-call caller-tracing therefore cannot reconstruct the dispatch - matches
  your live finding that `B004ACD9`/`0xB00530BC` are never reached. **The `0xB00530BC` keyword-table
  model is retracted.** [V]

## 3. Where the file-vs-symbol fork must be  [I, from the structure]

LOAD is dispatched as a command; it dies parsing its ARGUMENT `B:NRF` in the symbol/spec resolver
(`B003CFDA` cluster -> `B0040C3C`). The argument-type decision is made by the command
descriptor/executor layer (`B002EF7F` + a per-command descriptor like `0xB003EE90`). Two shapes fit,
and only a live dump distinguishes them:
- (a) LOAD's descriptor says "arg = file spec", and your run mis-reads it / a shared field is wrong,
  so the arg goes to the symbol parser; or
- (b) LOAD's descriptor legitimately sends the arg through the symbol/spec resolver FIRST (e.g. to
  allow `LOAD (symbol)` forms), and on real HW that resolver ACCEPTS `B:NRF` (returns without error)
  whereas your `B0040C3C` errors 52 - i.e. we are back to a B0040C3C-internal difference, but reached
  legitimately.
I cannot choose between (a) and (b) statically without decoding the descriptor format + `B002EF7F`.
[OPEN]

## 4. Probes (yours - far faster live than my static decode)

1. **Find LOAD's handler + descriptor.** Break where the matched command index is turned into a
   handler (right after the keyword match in `B0047535`'s flow), and dump the handler address and the
   descriptor pointer it loads (analogous to `0xB003EE90`). Do the SAME for `OPEN-DOMAIN` (works) and
   **diff the two descriptors** - the differing field is the arg-type selector.
2. **Dump the context block `0xB003DFE4`-`0xB003DFF8`** for LOAD vs OPEN-DOMAIN at the `jumpg
   0xB003DFE8` dispatch - shows the continuation/handler each takes.
3. **Break at `B002EF7F` entry** for LOAD and read its descriptor arg (`r.0x14` <- `0xB003EE90`-style)
   - that descriptor encodes how the argument is parsed. Compare to OPEN-DOMAIN's.
4. If (b) is true (LOAD legitimately calls the symbol resolver): break at the `B003CFDA`/`B0040C3C`
   entry on a KNOWN-GOOD real-HW LOAD and capture what descriptor/mode word it is passed - that mode
   is what makes the same parser accept `B:NRF` there.

## Honest status / recommendation

This is round ~5. Confirmed, compounding progress: K-flag -> parse-stage gate -> DEABF-output
disproven -> wrong-subsystem routing -> **LOAD is a real command whose ARGUMENT is mis-parsed, in a
data-driven/CPS dispatch**. The remaining fork lives in the per-command descriptor + executor
`B002EF7F`, a layer your live rig can dump in one break but that costs me several static layers
(DSEG descriptor format + CPS executor) to decode. **Recommend: you run probe 1/3 (dump LOAD's vs
OPEN-DOMAIN's descriptor) and relay the diff; I decode `B002EF7F` + the descriptor format against
that.** That splits the work where each side is fastest.

## Evidence register

- DSEG command names file 0x88a54 (full set) / 0x88cf4 (restricted set); LOAD file 0x88b52 & 0x88d20.
- Restricted lookup table file 0x88c94, 8x `{name_VA,0,len}`, all verified.
- Executor prologue `B0031504`-`B003152B` (raw `1acf b003 1531 c4b0 03df e4 ... b4c4 b003 dfe8`);
  continuation `B0031531`-`B0031549` (`... fe79 c4b0 03ee 90 8503 / c3 b0 02 ef 7f 00` = call B002EF7F).
- (Retracted) `0xB00530BC` keyword-table model; `B004ACD9` never reached (your live break count 0).
- (From V7) object OPEN `B004E874`/`B004E80F`; error subtree `B003CFDA`->`B0040C3C`.
