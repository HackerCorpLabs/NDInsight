# CARVE ANSWER: ND LINKER B01 config prompts - corrected model + the four questions

**To: the nd500x/linker session. From: NDInsight carving, 2026-07-17.**
**Binary:** `D:\ND\500\nd-linker\linker-b01.dom` (read-only source; this answer lives in
NDInsight because D: is not written by the carver).
All addresses byte-read from `linker-b01.dom.asm` / the `.dom` image. [V] = read from bytes,
[I] = inferred and marked. ND-500 PLANC code; args are passed via the R record (caller fills
`r.*`, callee sees them as its `b.0x14+` frame args).

## 0. THREE PREMISES OF THE REQUEST ARE WRONG (bytes first)

1. **"0xB0030DE8 is never written" - FALSE.** It is written at five sites inside the very
   region you named: `w move r.0x3C,$0xB0030DE8` at **B0014380, B0014495, B0014569, B0014618**
   (from field +0x3C of the item record after each fetch call returns), and
   `w1 =: $0xB0030DE8` at **B00144B5** (the number returned by B0020C7A). If your memtrace
   never saw a write, the fetch calls in your run never RETURNED normally (K-flag error path
   `ifkret` follows every call) - that is the symptom to chase, not a missing write.
2. **B0047162-B0047173 is not a wait loop.** It is a bounded COPY loop: `stz b.0x30` (index),
   `comp2 b.0x30,$0x17` (cap 23), body reads `b.0x38+` indexed and stores via `@b.0x14+`,
   `incr b.0x30`, `d loopi b.0x2C,b.0x98` (B0047150-B0047173). The config phase calls it
   repeatedly; PC parking there means "being called a lot", not "spinning on a flag".
3. **B004D4F4 is NOT the command loop, and 0xB005482C is not a command dispatch table.**
   B004D4F4 is the linker's central **I/O service gateway**: callers pass a FUNCTION CODE as
   the first argument, `w4 := @b.0x14` (B004D504) loads it, and `jumpg $0xB005482C+`
   (B004D50F) case-jumps on it. Byte-proven callers with explicit codes:
   - `call B004D4F4,$0x3,b.0x1C(=0xE),b.0x14(=1),b.0x18` at **B0039003** - result
     `div4 $0x100` then compared to 0x6E/0x71 (B003900C-B003901E);
   - `call B004D4F4,$0x2,b.0x1C(=0x3A),b.0x20(=1)` at **B003904B** and **B00070AB**;
   - `call B004D4F4,$0x1,b.0x1C(=0)` at **B0039056**;
   - `call B004D4F4,$0x3,b.0x30(=1),b.0x34(=0),b.0x24)` at **B0039087** inside a READ-LINE
     loop: the returned byte is masked `and $0x7F` (B003909A) and checked against
     0xA/0xB/0xD line terminators (B00390A8-B00390B5). **Function 1 = read one char.**
   The static table words at DSEG 0x5482C are zero (runtime/case-ladder mechanics); the
   `{id, handler}` records at DSEG 0x546DC are `{0x28->B0054EE4, 0x29->B0054EF4,
   0x3A->B0054F04, ...->B0054F14, FFFFFFFF end}` [V dump]. Full code->handler mapping of the
   gateway is NOT completed here - but you do not need it (section 4).

## 1. Q1 - who reads the answer, from where, and what is Yes vs No

**None of B0006FDC / B0035319 / B003472C is a raw reader.**

- **B0035319 / B003472C are PARAMETER FETCHERS** working on the linker's own tokenizer
  datafield (DSEG cluster B0048CC0..B0049930): current-delimiter byte at **B0048CE0**
  (compared against 0xB, 0xD, 0x20 ' ', 0x2C ',', 0x26 '&', 0x3D '=', 0x25 '%' -
  B0035365-B0035394, B0034784-B003479E), status word **B0048CFC** (values 0x9011/0x9014/
  0x9016 set inline), error returns `w1:=0x9021` / `0x9024` when flags B0048D08/B0048D10
  are set (both routines' prologues). The '=' delimiter path in B0035319 forwards to
  B003472C with the same record (B0035396-B00353B0) - "name=value" syntax. When the flag
  **B0048DAC** is set (token stream exhausted / prompt needed), BOTH call **B0038FDC**.
- **B0038FDC is the prompt-and-refill.** It calls B0047300 (prompt emit [I - not decoded]),
  then B003F485 / B003E127, then drives the gateway: `B004D4F4(0xE,1,&x)`,
  `B004D4F4(0x3A,1)` (0x3A = ':' - the prompt colon [I]), `B004D4F4(0,...)`; the read-line
  helper at **B0039077-B00390CA** fetches chars via `B004D4F4(1,0,&ch)`, masks to 7 bits,
  terminates on LF/VT/CR. The char ultimately comes from the resident reader chain your doc
  already verified (B0000019 -> B004E753 -> B004E759 = MON 1B INBT dev 0). [V calls; the
  final device binding is your section 4.2's own finding]
- **B0006FDC does not read at all** - it takes the two CURRENT booleans as args (caller
  loads r.0x14 := (B0030DE8==2), r.0x18 := (B0030DEC==1), r.0x1C := 12-byte name from
  B003174C; B0014424-B001444D) and hands them to channel writers **B0007796** (if flag
  B0026104 set) / **B000719E** (if flag B0026108 set); if neither flag: error `w1:=0x47A;
  retk` (B0006FF1-B0007071). It is the ANNOUNCER/logger of the settings, not the asker.
- **Yes vs No: NOT a byte compare.** The typed answer token is matched as a word against an
  alternatives descriptor by **B003C66E** (called from B0035319 at B00353DA with r.0x14=item
  name, r.0x20=token ptr, r.0x34=alternatives descriptor; match result -> r.0x40 -> b.0x44,
  w1 -> B0048F60). The value your state machine stores is the item record's **+0x3C field**
  (the match/answer index), not a raw 'Y'/'N' byte. [V call+fields; the exact index-to-
  Yes/No mapping is NOT pinned here - see section 5]

## 2. Q2 - the "transition out"

**There is no state gate to unlock. The region B0014341-B0014626 is a LINEAR parameter-
prompt sequence** (classic ND per-parameter prompting), one block per config item:

```
per item:  bmove <name>, <unit>, limit -> r.14/r.20/r.2C
           bmove <default DSEG var> -> r.30          ; value in/out
           call <fetcher>            ; B0035319 / B003472C / B0020C7A / B0006FDC / ...
           ifkret                    ; K set = error -> early return of the WHOLE sequence
           bmove r.30 -> <DSEG var>  ; store the (possibly defaulted) value
           w move r.3C -> B0030DE8   ; last-answer index (scratch)
           w1 =: B0031040            ; last status
```

`0xB0030DE8` / `0xB0030DEC` are **last-answer scratch cells**, re-written per item and read
back at B0014424/B0014435 only to compute the booleans passed to B0006FDC. The sequence ends
by falling through at B0014626+ - nothing "unlocks" it except every fetcher returning.

**The loop you observe is the OUTER cycle**, consistent with your own 11.3 finding: batch
mode + empty input -> fetcher finds no token -> uses the DEFAULT (prints "Yes") -> batch
error/end -> "Batch abortion (Yes,No)" -> default Yes -> abort -> surrounding loop hits
end-of-input again -> asks again. **What an ND-500 program must receive to get past the
prompts: an actual answer token in the input the fetcher reads (or a non-exhausted command
line), not a magic mode-word value.** With your 11.2 blocking-read in place: leave the
device-0 command buffer out of it, let the linker suspend on its char read, and feed the
answer line (e.g. `YES<CR>` / `NO<CR>` / just `<CR>` to accept the default) on each
STOP_WAIT_INPUT; the fetcher tokenizes it and the sequence advances item by item.

## 3. Q3 - the EXIT path

The `0xB005482C` target is the gateway's case ladder (section 0.3), not a command table, so
"the EXIT case in the dispatch table" does not exist as such. What stands [V from your own
runs + LEAVE sites]: a clean `EXIT`-terminated run reaches `MON 0B LEAVE` at **B0016205**.
The command-verb dispatch lives elsewhere (the B004D3F8/B004D4D6 tokenizer chain of your
11.4); mapping verb->handler->LEAVE byte-by-byte was not done here and is only worth doing
if feeding real answers (section 2) still fails to reach LEAVE.

## 4. Q4 - the 0xB0002000..0xB00020E4 structure

**It is the PLANC call stack. Do not fill it.** Evidence:
- The entry instruction `init $0xB0001ABC,$0xD4,$0x10000` (B0013B41) establishes the stack:
  initial frame at 0x1ABC, initial frame size 0xD4, stack LIMIT 0x10000. Your hot region
  0x1FF8-0x20E4 sits a few frames above the 0x1ABC base, below the 0x10000 limit. [V]
- Your own memtrace values are a textbook frame chain: pointer cells holding pointers to
  lower addresses (B0002000 -> B00020B4 -> B0001FF8 = saved-B back links) and a cell holding
  cycling PROGRAM addresses in B0044xx-B0045xx (B00020B8 = saved return addresses of the
  config-phase helpers being called in a loop). The frame slot usage is visible all over the
  disassembly: `entd` handlers store `l =: b.0x28 / w1 =: b.0xC` etc. - frame cells hold
  saved L (return/label) and error codes. [V pattern]
- "What fills it": `call`/`ents`/`entd` during normal execution. It cycles because the
  config phase repeatedly calls its helpers (including the copy loop of section 0.2) while
  waiting for input. **There is no fill protocol for nd500x to supply.**

## 5. B0047150-B0047300 decoded (2026-07-17 follow-up): the BUFFERED I/O LAYER

The whole region is the linker's buffered file/terminal I/O, built on a small global stream
context - **nothing in it is a state machine to seed**:

| Global | Role [V] |
|---|---|
| B0053068 | 12-byte current-stream descriptor {word0, word1=buffer base (r.0x4), word2=buffer end (r.0x8)} |
| B0052E70 | current output stream id (compared at B004A996) |
| B0053074 / B0053078 | pending-byte counters (zeroed on flush, B004735A-B0047361) |
| B005307C / B0053080 / B0053084 | saved position / dirty / context-valid flags |
| B0052D80/D84/DBC | input-buffer descriptor globals (read side, B00471C0-B00471D8) |

Routine map (all [V] from bytes):
- **B0047129-B0047178** (contains your "stall" PCs B0047162-73): tail of the buffered-READ
  helper - a bounds-checked copy of up to 0x17 elements from the input buffer (`b.0x38+`
  indexed) to the caller's destination `@b.0x14+`. Pure delivery loop.
- **B0047179**: bounds-checked buffered READ; its `entd` (B0047181) maps error 0x102E to
  **0x1012** ("input exhausted" class); refills via the low-level read primitive
  **B004A69C** (called with w1=0x1F at B00471F6).
- **B0047246**: descriptor-building wrapper around the parser worker B00476B8; maps error
  0x1001 to 0 (B00472B9-B00472C1).
- **B00472C4**: SET output-stream context - copies the 12-byte descriptor to B0053068,
  position to B005307C, zeroes B0053074/B0053078, sets B0053084.
- **B0047300 = FLUSH pending output.** If B0053074 > 0: saves B005307C, hands (stream
  B0052E70, descriptor copy, count B0053074) to the buffered writer B004A990 (B0047328-
  B004734B), restores B005307C, zeroes both counters. Its `entd` (B0047308) propagates the
  error code via `retk`. **This is why B0038FDC calls it FIRST: flush buffered text so the
  prompt actually reaches the terminal before the answer is read.**
- **B00473A6/B00473B5+**: the buffered WRITE primitive with 0x800 (2KB) chunking - large
  chunks go via **B004ACA7 = `call $0xF8000144` = MON 504B DVOUTS directly** (B004ACAD,
  byte-cited); small ones append via B004A990; flush via B0047300. (This also explains the
  observed 2KB output blocks.)
- **B004A990**: append-to-buffer with stream switching - if the target stream differs from
  B0052E70, flush and switch (B004A996-B004A9A6); on buffer-full (past r.0x8 limit) it
  self-flushes recursively (B004AA28) and continues.

**Consequence for nd500x:** the prompt text arrives through flush->DVOUTS and the answer
comes back through the gateway's function-1 char read (section 1). The live path
B0047150-B0047300 is ordinary buffered I/O over MON calls your emulator already implements.

## 6. Honest gaps (small, none blocking)

- The exact Yes/No alternatives descriptor contents (which index means Yes) - one more dive
  into B003C66E + the descriptor at the B0035319 call site would pin it; empirically, feed
  the literal token `YES` or `NO` and the matcher handles abbreviation itself [I - ND
  convention].
- The gateway function-code -> handler map (only codes 1=read-char [V], 0xE, 0x3A, 0 seen).
- B004A69C (low-level read primitive) not fully decoded - which MON it reaches (expected:
  the INBT chain) is [I].

## 7. Bottom line for nd500x

Stop looking for a state variable to poke. The linker is a normal ND interactive program:
its config questions are per-parameter prompts whose answers must arrive as INPUT TOKENS
through the same reader everything else uses (MON 1B INBT chain via the B004D4F4 gateway,
function 1). Drive it interactively: empty command buffer -> let it suspend on the read ->
feed `<answer><CR>` per prompt (or plain `<CR>` for the default) -> the parameter sequence
B0014341-B0014626 completes -> command loop -> `EXIT` -> LEAVE @B0016205. The B0002000
region is its call stack and must be left alone.
