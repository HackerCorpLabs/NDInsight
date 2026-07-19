# CARVE ANSWER (V6) - the '.' is the DEABF-RESOLVED name; B003DCE2 splits on '.', it never adds one

Answers the B003DCE2 canonicalizer follow-up (relayed 2026-07-18). Builds on
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V5.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V5.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom.asm`. All addresses ND-500 VAs (hex).
Tags: **[V]** = read from these disasm bytes; **[I]** = inferred; **[OPEN]** = not proven.

---

## TL;DR

Your instinct "B003DCE2 appends a default that contains a `.`" is **not** how it works. B003DCE2
**splits its input on `.` (0x2E); it never inserts one** [V]. And `b.0xBC` is **not** a default-type
source - it is a scratch descriptor assembled from the input pointer [V]. The `.` that makes
parse-stage 4 has to be **already present in the name B003DCE2 is handed** - and the only thing in
this pipeline that turns `B:NRF` into a dot-form is **DEABF's own resolution** (your point: DEABF
resolves `B:NRF` -> `B.NRF` - that resolved string HAS the dot). So the real-HW `.` is the
**DEABF-resolved on-disk name**, and your bug is that the descriptor reaching B0040C3C is the **raw
editor-line window `{0xB0048FEC,5,9}` = "B:NRF"** instead of the **resolved `B.NRF`**.

Also: **point 5 is incomplete - B003DCE2 is NOT MON-free.** It calls the same dispatcher `B004D4F4`
(the DEABF one) at `B003DDCD` (selector `0x28`) and `B003DDEE` (`0x23`). Those are the file-system
resolution calls whose emulation you need to check. [V that the calls exist; selector decode [I].]

---

## 1. B003DCE2 contract - byte for byte  [V]

Entry `B003DCE2 ents $0x104`; clears working cells; copies the input descriptor and validates chars:
```
B003DCFA w bmove b.0x14,r.0x14,$0x3   ; input descriptor {ptr=b.0x14, lo=b.0x18, hi=b.0x1C}
B003DCFF call B0040711                 ; pure char validator (your point 5: no MON here - correct)
B003DD06 w test r1 ; if = go +8        ; r1!=0 -> error 0x2F return (B003DD0A-0F)
```
[V, lines 73845-73852]. Then the **`.`-splitter**:
```
B003DD26 by1 := $0x2E                  ; '.'
B003DD2B by comp2 @b.0x14+,r1          ; scan input for '.'
B003DD2F if = go +8  -> B003DD37       ; FOUND '.'
B003DD31 d loopi b.0x40,b.0xB8         ; keep scanning
B003DD35 go +0x2E   -> B003DD63        ; NOT FOUND
```
[V, lines 73864-73869].
- **`.` found (B003DD37-DD61):** builds two descriptors - `b.0x78` = the part BEFORE the dot,
  `b.0x84` = the dot and after. The `b.0xBC` you saw is the **scratch** used to assemble these:
  `B003DD3F w3:=b.0x14 =: b.0xBC` then `B003DD43 by bmove b.0xBC,b.0x78,$0xC`; and again
  `B003DD52 w1:=b.0x14 =: b.0xBC` / `B003DD56 bmove b.0xBC,b.0x84`. So **b.0xBC = the input pointer
  b.0x14, reused as a build register - NOT a persistent default-type cell.** [V, lines 73873-73885]
- **`.` NOT found (B003DD63-68):** `b.0x78 := {0,0,0}` (empty), `b.0x84 := the whole input
  descriptor`. [V, lines 73887-73888]

**Net:** B003DCE2 never manufactures a `.`. It only partitions an existing `.` into name/type halves.
With no `.` in its input, it carries the input through unchanged (as `b.0x84`). So a colon-only
`B:NRF` stays dot-less through B003DCE2. [V]

## 2. Point 5 correction - B003DCE2 DOES call the dispatcher  [V]

After the split/quote handling, B003DCE2 reaches a dispatcher block:
```
B003DDBD w move $0x28,b.0xCC            ; selector 0x28
B003DDC3 by bmove b.0x14,b.0xBC        ; name descriptor
B003DDC8 by bmove b.0x20,b.0xD4        ; second descriptor (b.0x20 = raw line 0xB0048FEC)
B003DDCD call B004D4F4,$0x9,b.0xCC,...  ; SAME dispatcher that runs DEABF
B003DDE2 if -k go +8
B003DDEE call B004D4F4,$0x2,b.0xCC(=0x23)  ; second op
```
[V, lines 73926-73934]. `B004D4F4` is exactly the dispatcher your break identified as DEABF at
`B0040D5C` (there the selector `b.0xD8 = 0xAE`). Here the selectors are `0x28` and `0x23`. By the
same hex=decimal-MON pattern that gave `0xAE = 174 = 256B`, `0x28 = 40 = 50B (OPEN)` and
`0x23 = 35 = 43B` - **[I], confirm by breaking at B003DDCD and reading b.0xCC + B004D4F4's actual
target.** The point that matters: **there IS file-system resolution inside B003DCE2's path, via the
dispatcher - your "no mis-emulated MON in the canonicalizer" conclusion skipped it.** [V for the
calls existing.]

## 3. The three buffers - which pass errors  [V math + I attribution]

- `0xB0035CEC` "File name:NRF" with window hi = b.0x1C = 8 -> indices [0,8] = **"File name"** (the 9
  chars 'F','i','l','e',' ','n','a','m','e'). That is the **PROMPT LABEL**, not the argument. The
  pass you broke on (instr 190891) is B003DCE2 processing the prompt template, not `B:NRF`. [V - the
  window arithmetic; "File name:NRF" = prompt "File name" + default type ":NRF" is [I].]
- `0xB0048FEC` "LOAD B:NRF" = the raw editor line.
- `{0xB0048FEC,5,9}` = **"B:NRF"** = the window that reaches the error-52 B0040C3C.

So there are **multiple B003DCE2 passes**; the one that errors is a LATER pass whose input is the
raw-line window `B:NRF`, not the prompt pass you caught. You must trace the pass whose B003DCE2
OUTPUT is `{0xB0048FEC,5,9}` and see what its dispatcher call (section 2) returned. [I - needs your
multi-pass trace.]

## 4. Q (net): what puts a '.' in B0040C3C's input on real HW  [I, well-supported]

Chain of byte facts:
1. B0040C3C's parse-stage 4 requires a literal `.` in its input descriptor. [V, V5]
2. B003DCE2 never adds a `.`; it only splits on one. [V, section 1]
3. DEABF (the dispatcher op) RESOLVES `B:NRF` -> `B.NRF` - a **dot-form** on-disk name (your own
   observation). [your live data]

Therefore the real-HW `.` is **the DEABF-resolved name**, and the correct data flow is: the
resolution op inside B003DCE2 (section 2) returns a descriptor pointing at the **resolved `B.NRF`**,
which B003DCE2 hands on as its output, so B0040C3C parses `B.NRF` (dot -> stage 4 -> pass) and then
the downstream `MON 50B OPEN` (B004CABC/B004E874) runs. On your emulator, the resolution op is **not
propagating the resolved dot-form** - B003DCE2 falls through to carrying the **raw colon window
`B:NRF`**, so B0040C3C sees no `.` -> error 52. **The fix is in what the B003DCE2 dispatcher call
returns, not a default-append and not b.0x49.** [I - coherent with every byte above; the exact
output-descriptor propagation is what your trace must confirm.]

## 5. Probes (give me these back and I can close it)

1. **Break at `B003DDCD`** in the erroring round (the pass whose input window is `B:NRF`, not the
   prompt pass). Dump `b.0xCC` (selector), the name descriptor `b.0xBC` {ptr,lo,hi}+bytes BEFORE the
   call, and AFTER the call dump `b.0xBC/0xC0/0xC4/0xD4/0xD8/0xDC`. Does any returned descriptor point
   at a **dot-form** `B.NRF`? That is the resolved name that must flow out.
2. **Dump B003DCE2's OUTPUT descriptor** (`r.0x14` at its `ret`, = caller `b.0x20`) for that same
   pass - is it the resolved `B.NRF` or the raw `{0xB0048FEC,5,9}`?
3. **Trace every B003DCE2 + B0040C3C call in one LOAD round** with input+output descriptors; find the
   pass whose B0040C3C input is `{0xB0048FEC,5,9}` and walk back to that B003DCE2's dispatcher result.
4. If `B003DDCD`'s op is your `MON 50B OPEN` (selector 0x28), verify the emulated OPEN returns
   SINTRAN's canonical/expanded name descriptor (dot-form), not an echo of the raw colon input - that
   is the most likely emulator divergence.

## Evidence register (linker-b01.dom.asm, PSEG VAs)

- B003DCE2 prologue + validator `B003DCE8`-`B003DD0F`; `.`-scan `B003DD26`-`B003DD35`
  (`04 CD 2E / 2D E5 14 D0 / C4 08 / BF 50 6E F5 / C0 2E`).
- split-found build `B003DD37`-`B003DD61`; b.0xBC scratch `B003DD3F`/`B003DD52`.
- no-`.` path `B003DD63`-`B003DD68` (`FE 79 00 5E 03 / FE 79 45 61 03`).
- dispatcher calls `B003DDBD`-`B003DDF8` (`1A CD 28 73 ... C3 B0 04 D4 F4 09 ...` and `1A CD 23 73`).
- (from V5) gate `B0040D7D` `comp2 b.0x49,$4`; '.'->4 `B0040CC1`; downstream OPEN `B004CABC`/`B004E874`.
