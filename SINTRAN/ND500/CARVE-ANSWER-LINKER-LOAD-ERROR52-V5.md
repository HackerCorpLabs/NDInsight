# CARVE ANSWER (V5) - error 52 is a PARSE-STAGE gate (b.0x49 == 4), NOT a K-flag gate

Answers the B0040C3C / DEABF / error-52 trace (relayed 2026-07-18). Supersedes the K-dispatcher
framing in the request.
Binary: `D:\ND\500\nd-linker\linker-b01.dom` (+ `.dom.asm`, `nd500-dis` linear sweep).
Addressing: all addresses are ND-500 VAs (hex, `0xB00xxxxx`). PSEG file = VA - 0xB0000000 + 0x1000.
Tags: **[V]** = I read these exact disasm bytes/lines; **[I]** = inferred; **[OPEN]** = not proven.

---

## TL;DR - the one correction that matters

**Your central model is off by one gate.** You have: "K clear (dispatcher RETs) + b.0x49 == 1 ->
both success routes fail -> error 52; so if we fix the dispatcher to return K-set, it loads."
That is **not** what the bytes do:

- The final decision at `B0040D7D` is **`comp2 b.0x49,$0x4`** and it runs on **BOTH** K paths. The K
  flag only decides whether the little helper `B0040C44` runs *first* - and `B0040C44` **returns**
  (it is a save-link/bookkeeping sub, not the loader). So **preserving DEABF's K is
  necessary-but-NOT-sufficient**: with K set you still land on `B0040D7D`, and with `b.0x49 == 1`
  you still fall to the error-52 path. [V]
- **The real success condition is `b.0x49 == 4`.** `b.0x49` is a parse-STAGE counter, and only a
  **`.` (0x2E)** advances it to 4. `;`->2, `)`->3, and **`:` (0x3A) does NOT advance it** (it sets a
  side flag `b.0x4C`). So a name whose only separator is `:` (like `B:NRF`) ends at stage 1 and can
  never pass this gate. [V]

So the question is not "how do we get K set", it is **"what does B0040C3C actually parse on real
HW, and does that string reach parse-stage 4 (contain a `.`)"**. Decisive probe at the bottom.

---

## 1. Q3 first (what string is parsed) - it is NOT the raw line you dumped  [V]

`B0040C3C` is called from the LOAD sub-context `B003D0E2`:
```
B003D0BB: call B003CFDA            ; name pre-processor
B003D0C3: w bmove r.0x14,b.0x20    ; b.0x20 := B003CFDA result descriptor
B003D0D3: w bmove b.0x20,r.0x18    ; pass it as callee arg r.0x18  (= callee b.0x18)
B003D0DD: w move  b.0x108,r.0x30   ; + a mode word
B003D0E2: call B0040C3C            ; parse/resolve
```
[V, lines 72774-72785]. So **B0040C3C's `b.0x18` is the OUTPUT of `B003CFDA`** (which itself follows
`B003DCE2` at `B003D0B3`), not the terminal line buffer. Your dump of `0xB0048FEC` -> "LOAD B:NRF"
is the raw editor line; the descriptor B0040C3C parses is the post-`B003CFDA` name with its own
`{ptr, lo=b.0x1C, hi}` window. `b.0x1C = 5` is that window's low index - it points PAST "LOAD " into
"B:NRF". **Confirm what B003CFDA produced before trusting b.0x49.** [V for the call chain; the exact
post-B003CFDA contents = OPEN, needs a live dump of the r.0x18 descriptor at B003D0E2.]

---

## 2. Q2 - the gate, byte for byte  [V]

Parser prologue (`B0040C3C`):
```
B0040C3C ents $0x100
B0040C4F w bmove b.0x18,b.0xAC,$0x3   ; copy the {ptr,lo,hi} string descriptor
B0040C59 by set1 b.0x49              ; b.0x49 := 1   <-- STAGE STARTS AT 1
```
Per-char stage loop (`B0040C74`-`B0040CC7`, one byte via `@b.0x18+`):
```
B0040C79 by1 comp $0x3B  (';')  -> B0040C7E by move $0x2,b.0x49   ; ';' => stage 2
B0040C86 by1 comp $0x29  (')')  -> B0040C8B by move $0x3,b.0x49   ; ')' => stage 3
B0040CA9 by1 comp $0x3A  (':')  -> B0040CB8 w set1 b.0x4C         ; ':' => set FLAG only, stage UNCHANGED
B0040CBC by1 comp $0x2E  ('.')  -> B0040CC1 by move $0x4,b.0x49   ; '.' => stage 4
```
[V, lines 77678-77710]. After DEABF, the gate:
```
B0040D5C call B004D4F4 (DEABF dispatcher)
B0040D75 if -k go $0x8            ; K clear -> jump to B0040D7D (skip B0040C44)
B0040D77 call B0040C44            ; K set   -> bookkeeping sub (saves link->b.0xB8, w1->b.0xC, RET)
B0040D7D by comp2 b.0x49,$0x4     ; <<< BOTH paths arrive here
B0040D81 if >< go $0x4            ;   b.0x49 != 4 -> B0040D85  (ERROR 52 formatter)
B0040D83 bi1 clr
B0040D84 ret                      ;   b.0x49 == 4 -> success return
```
[V, lines 77760-77767]. `B0040C44` [V, lines 77656-77659]:
```
B0040C44 entd ; l=: b.0xB8 ; w1 =: b.0xC ; ret     ; save-link continuation helper, RETURNS
```
Its partner `B0040C4B: clrk ; jumpg b.0xB8` is the matching K-clear longjmp-return through the saved
link - a continuation pair, still not a file loader. [V]

**Conclusion (Q2):** yes, `b.0x49` is *meant* to be 4; the gate is a parse-stage check, not a K
check. K only gates the bookkeeping. Your observed live symptom (b.0x49 = 1 -> error 52) is exactly
what these bytes do, which confirms the reading. [V]

---

## 3. Q1 - what "error 52" is (and why the message is blank)  [V mechanism]

The error path does NOT scan an error-code table. `B0040D85` sets a pointer to `B004DADC` and jumps
through a dispatch at `B004DB78` indexed by `b.0x49`:
```
B0040D85 w stz b.0x40 ; w stz b.0x38
B0040D8B by move $0xB004DADC+,b.0x48     ; b.0x48 := &B004DADC
B0040DA0 jumpg $0xB004DB78+              ; indexed dispatch
```
[V, lines 77767-77774]. And **`B004DADC` is executable code - the linker's variadic MESSAGE
FORMATTER** (`callg b.0x40, $N, ...` output-sink calls, dispatching on format codes 0x6A/0x8F/...),
not a table of error strings. [V, lines 92730-92799]. So **"error 52" is not a catalogued
file-system error**; it is this routine printing its internal parse-stage-mismatch through the
generic formatter, and the message slot for that code is empty in `UE-ERMSG-EN-C06.ERR` - exactly the
blank text you saw. The `(-677:52)` pair is the formatter's two-number rendering of one status word
(prior linker-b01 analysis: display = one word split DIV/MOD 64); I did **not** decode the exact
-677/52 arithmetic, so treat "52 = catalogued error N" as unproven. [V for "formatter, blank slot";
[OPEN] for the precise numeric encoding.]

**Net Q1:** error 52 is linker-INTERNAL (a name that failed to reach parse-stage 4), printed via the
generic formatter with no message string. It is not a SINTRAN file error and no caller intercepts it
(you already confirmed the print is inside B0040C3C).

---

## 4. Q4 - where the object 50B OPEN actually is  [V]

Not in `B0040C3C`. The real `MON 50B OPEN` (seg-31 index 0x28 = `call $0xF8000028`) is downstream, at:
```
B004CABC call $0xF8000028,$0x4,$0xB0054314,b.0x2C,...   ; MON 50B OPEN
B004E874 call $0xF8000028,$0x4,b.0xEC,b.0x2C,b.0x34,b.0x3C ; MON 50B OPEN
```
[V, lines 91363, 94046]. `B0040C3C` is the **name resolve/validate** stage (syntax parse + DEABF
existence check); the object is only 50B-opened after it returns success. Because B0040C3C errors 52
first, the flow never reaches `B004CABC`/`B004E874` - which is exactly why you "never 50B OPEN the
object." Fix the parse gate and the existing OPEN site runs. [V]

---

## 5. What is (and is NOT) the fix

- **Insufficient:** making the dispatcher `B004D4F4` return K-set. `b.0x49 == 4` is still required on
  the K-set path. [V]
- **Root cause is upstream of this gate:** either (a) real HW feeds `B0040C3C` a name that reaches
  stage 4 (contains a `.`), and your emulator feeds a `:`-only name (stage 1); or (b) the descriptor
  you dumped is not the one B0040C3C parses (section 1). Both point at **`B003CFDA` / `B003DCE2` and
  the mode word `b.0x108`** as where the canonical, dot-bearing name is supposed to be built. [I]
- I did **not** prove which. Do not "patch b.0x49 := 4" blindly - that would mask a real
  name-canonicalisation difference and likely break the other 10+ callers of B0040C3C. [I]

---

## 6. Decisive probes for nd500x (in order)

1. **At `B003D0E2`, dump the 3-word descriptor actually passed** (callee `r.0x18` = ptr/lo/hi), and
   read the bytes it spans. That is the true B0040C3C input. Does it contain a `.`? If yes, your
   b.0x49 read was off; if no, B003CFDA is producing the wrong (`:`-only) form.
2. **Single-step `B003CFDA` (B003CFDA) and `B003DCE2`** with a known-good real-HW trace of the same
   `LOAD B:NRF` and diff the descriptor they emit into `b.0x20`. The divergence is your bug.
3. **Watch `b.0x49` across the B0040C74-CC7 loop** for that real descriptor: it must hit 4 via a
   `0x2E`. If the real canonical name is e.g. `(pack)dir.B:NRF` or a dot-qualified object spec, the
   `.` is where stage 4 comes from.
4. Only after b.0x49 == 4 passes: confirm `B004CABC`/`B004E874` issue the `MON 50B` on `B.NRF`.

## Evidence register (all from `linker-b01.dom.asm`, PSEG VAs)

- Stage init `B0040C59` (`FC 87 C1 49`); ';'->2 `B0040C7E` (`19 02 C1 49`); ')'->3 `B0040C8B`
  (`19 03 C1 49`); ':'->flag `B0040CA9`-`B0040CB8` (`30 CD 3A ... 4D 53`); '.'->4 `B0040CC1`
  (`19 04 C1 49`).
- Gate `B0040D75`-`B0040D84` (`D2 08 / C3 B0 04 0C 44 00 / 2D C1 49 04 / C6 04 / 84 / 80`).
- Helper `B0040C44` (`9C / FD C0 6E / 20 43 / 80`) + continuation `B0040C4B` (`FE 03 / B4 6E`).
- DEABF dispatch `B0040D5C` -> `B004D4F4`.
- Formatter `B004DADC`-`B004DBFx` (`callg b.0x40` variadic, format codes `0x6A`/`0x8F`).
- Caller `B003D0BB`-`B003D0E2` (B003CFDA -> b.0x20 -> B0040C3C).
- Downstream OPEN `B004CABC` / `B004E874` (`call $0xF8000028` = MON 50B).
