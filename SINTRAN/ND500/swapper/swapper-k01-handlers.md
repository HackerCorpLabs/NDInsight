# SWAPPER-K01 - The 29-entry function-code dispatch table and its handlers

Reverse-engineering of the ND-500 SWAPPER-K01 domain's message dispatch table:
the 29 handler routines reached through the `jumpg` at DSEG 0x26198, one per
function code 0..28 (0o0..0o34).

All addresses are PSEG virtual (base 0x08000000). The disassembly listing cited
throughout is `SINTRAN/ND500/swapper/swapper-k01-pseg.asm` (octal addresses;
octal address `1000000000` == virtual 0x08000000, so octal `1000101554` ==
virtual 0x0800836C). Raw segment bytes are
`SINTRAN/ND500/swapper/SWAPPER-K01.PSEG` and `.../SWAPPER-K01.DSEG`.

Evidence tags used below:
- **PROVEN** - read directly from the bytes / listing cited on the same line.
- **INFERRED** - a name or role deduced from the proven shape; not a fact.
- **UNKNOWN** - the bytes do not establish it; the settling experiment is given.

> **CORRECTION 2026-08-17**: every mention of the slot-table bound cell
> originally printed its hex address as 0x128A4. The octal data address is
> 224344B, which is **0x128E4** (2*32768+2*4096+4*512+3*64+4*8+4 = 76004).
> All occurrences below are now fixed to 0x128E4. Consistent with execution
> on the microword engine (fn 22B was dispatched through the jumpg with
> [0x128E4] staged to 100B; the gate-shut branch was not exercised); see
> `SINTRAN/ND500/CARVE-ANSWER-N5SWAP-FUNCTION-VOCABULARY-2026-08-17.md`.

---

## 1. Summary

The swapper runs a message loop. For each request it fetches a **function code**
into DSEG 0x240B8, bound-checks it, and `jumpg`s through a 29-word pointer table
at DSEG 0x26198 into a compact block of 29 handler stubs packed in PSEG
0x0800836C..0x080086E2.

Each handler stub is a thin marshaller: it copies fields out of the message
buffer (DSEG 0x240BC) into a local parameter frame and `call`s **one worker
routine**; the worker is the semantic identity of the function code. On worker
error the stub re-enters the loop head (`call 0x08008... = octal 1000101241`);
on success it `go`es to a common tail. The worker routines have **no symbol-table
names** (they are local PSEG labels; `N500-SYMBOLS.SYMB` does not cover them),
so their meaning is established only from their bytes.

Counts for the 29 reachable handlers (0..28):
- Characterized **PROVEN shape** (marshalling + worker identity + what data
  structures / paging primitives / MON calls the worker's call-tree reaches):
  **27 of 29**.
- Purpose **INFERRED** (a plausible function name from the proven shape): **25**.
- Purpose **UNKNOWN** (bytes insufficient to name): **2** (index 11/12/20 are a
  proven no-op group - shape known, "meaning" is just "do nothing"; index 23 is
  a genuinely opaque small bit-op).

Key correction to prior art (PROVEN, section 3): the jumpg is gated by
`comp2 [0x240B8],$34` (fn <= 0o34 = 28), **not** by the `comp2 ...,$24` (=20)
that `swapper-k01-deep-analysis.md` cited. All 29 table entries are reachable.

---

## 2. The dispatch mechanism (PROVEN)

### 2.1 Intake -> index -> jumpg

The function code is not a raw RIOM header field. Per the parallel ND-100-side
carve (proven there; see `SINTRAN/ND500/ND500-SWAPPER-ANALYSIS.md`) it arrives
as the OUT result of the swapper's MON 377B **sub-function 1** call, is stored at
DSEG 0x240B0, then copied to 0x240B8. The copy is visible in this PSEG:

```
line 10545  1000101171  w1 := $1000440260      ; w1 := [0x240B0]  (sub-fn-1 result)
line 10546  1000101177  w1 =: $1000440270      ; [0x240B8] := w1  (dispatch index)
```

(`$1000440260` = 0o440260 = 0x240B0; `$1000440270` = 0o440270 = 0x240B8. PROVEN.)

The loop body then validates and dispatches:

```
line 10566  1000101267  w test  $1000440270          ; test fn code
line 10567  1000101275  if <<  go $4                 ; fn < 0 ...
line 10568  1000101277  w set1 b.54                   ; b.54 := (fn >= 0)
line 10569  1000101301  w stz  b.60
line 10570  1000101303  w comp2 $1000440270,$34       ; fn vs 0o34 = 28
line 10571  1000101312  if >>  go $4                  ; fn > 28 ...
line 10572  1000101314  w set1 b.60                   ; b.60 := (fn <= 28)
line 10573  1000101316  w1 := b.54
line 10574  1000101320  w1 and b.60                   ; valid := (0<=fn) AND (fn<=28)
line 10575  1000101322  w1 =: b.50
line 10576  1000101324  w test r1
line 10577  1000101326  if ><  go $22                 ; valid -> go process (RIOM + jumpg)
line 10578  1000101330  w1 := b.44                    ; else b.44 = 0o55 (reject code)
line 10579  1000101332  call $1000003057              ; reject via MON 377B sub-fn 2
```

After the RIOM of the message payload and two pre-dispatch special cases, the
actual jump:

```
line 10593  1000101504  w comp2 $1000440270,$5   ; special-case fn 5  -> go $25
line 10595  1000101515  w comp2 $1000440270,$3   ; special-case fn 3  -> go $14
line 10597  1000101526  w comp2 $1000224344,$24  ; DIFFERENT var (0x128E4) vs 20
line 10598  1000101535  if <=  go $1643          ; branch away when [0x128E4] <= 20
line 10599  1000101540  w1 := $1000440270        ; index = fn code [0x240B8]
line 10600  1000101546  jumpg $1000460630+       ; jump via table @ 0x08026198
```

**Bound-check resolution (PROVEN).** Two different `comp2` sit near the jumpg:
- `comp2 [0x240B8],$34` (line 10570) tests the **function code** against
  0o34 = 28. Together with the `<0` test (10566) and the AND (10574) it is the
  gate on whether the jumpg is taken at all: only 0 <= fn <= 28 reach it; an
  out-of-range code is rejected at 10578-10579. **This proves all 29 table
  entries (index 0..28) are reachable.**
- `comp2 [0x128E4],$24` (line 10597) tests a **different** variable
  ($1000224344 = 0x128E4), not the function code, against 20, and branches to a
  separate path ($1643). `swapper-k01-deep-analysis.md` mistook this second
  comparison for the function-code bound; the bytes show it is unrelated to the
  index range.

The pre-dispatch special cases at 10593/10595 (fn 5 and fn 3) branch into the
tail of the loop (targets $25/$14 land at 10597/10599, i.e. they skip part of
the setup and still reach the jumpg). PROVEN that fn 3 and fn 5 receive distinct
handling in the loop head; the exact effect is a minor loop-plumbing detail.

### 2.2 Per-handler stub shape (PROVEN)

Every stub follows the same template (idx 5 shown, line 10601):

```
1000101554  w1 := $1000440274        ; w1 := [0x240BC]  (message buffer word)
1000101562  r  := b.10               ; r -> local parameter frame
1000101564  w1 =: r.24               ; frame.24 := arg
1000101566  call $1000077534         ; call the worker (identity of the fn code)
1000101574  if -k go $10             ; worker ok (K clear) -> skip re-loop
1000101576  call $1000101241         ; worker failed -> re-enter loop head
1000101604  go   $1572               ; common tail (all stubs converge)
```

- `[0x240BC]` (0o440274) is the message payload; stubs extract whole words or
  16-bit sub-fields (`h1 := r.6`, `h1 := r.20`, `by1 laddr r.20`, ...).
- `$1000101241` is the **loop head / reject-and-restart** entry, re-entered on
  every worker error.
- The marshalling stubs all `go` to a single common tail at octal 1000103576
  (0x0800877E); the `$66`-group and no-op stubs converge at octal 1000103376
  (0x080086FE), which itself `go`es back to the loop. PROVEN by arithmetic:
  e.g. stub-5 `go $1572` @1000101604 -> 1000103576; stub-10 `go $1537`
  @1000101637 -> 1000103576.

---

## 3. The 29-entry table (index -> target -> name)

Extracted from the raw 116 bytes at `SWAPPER-K01.DSEG` file offset 0x26198
(each entry is a 32-bit big-endian PSEG virtual address). **Verified against the
raw bytes AND against `swapper-k01.dseg.md` section 5.12 - both agree exactly.**

Raw bytes (DSEG 0x26198, 29 words, then two trailing zero words at 0x2620C):

```
00026198: 0800 83d8 0800 83f7 0800 8474 0800 83a2
000261a8: 0800 83bd 0800 836c 0800 8672 0800 86e2
000261b8: 0800 8412 0800 8431 0800 8387 0800 864c
000261c8: 0800 864f 0800 844c 0800 848f 0800 84bd
000261d8: 0800 8655 0800 84eb 0800 8524 0800 8542
000261e8: 0800 8652 0800 8567 0800 8582 0800 85e3
000261f8: 0800 8602 0800 868e 0800 86aa 0800 86c6
00026208: 0800 861f
```

Worker addresses are given in PSEG octal (as they appear in the listing's
`call` operands); worker virtual = 0x08000000 + (worker_octal - 0o1000000000).

| Idx | Target (virtual) | Entry octal | Worker octal | No sym name? |
|----:|------------------|------------:|-------------:|--------------|
| 0  | 0x080083D8 | 1000101730 | 1000053713 | yes (local) |
| 1  | 0x080083F7 | 1000101767 | 1000052116 | yes |
| 2  | 0x08008474 | 1000102164 | 1000060231 | yes |
| 3  | 0x080083A2 | 1000101642 | 1000060473 | yes |
| 4  | 0x080083BD | 1000101675 | 1000061273 | yes |
| 5  | 0x0800836C | 1000101554 | 1000077534 | yes |
| 6  | 0x08008672 | 1000103162 | 1000003057 | yes (shared) |
| 7  | 0x080086E2 | 1000103342 | 1000003057 | yes (shared) |
| 8  | 0x08008412 | 1000102022 | 1000046576 | yes |
| 9  | 0x08008431 | 1000102061 | 1000046242 | yes |
| 10 | 0x08008387 | 1000101607 | 1000042045 | yes |
| 11 | 0x0800864C | 1000103114 | (none - no-op) | -   |
| 12 | 0x0800864F | 1000103117 | (none - no-op) | -   |
| 13 | 0x0800844C | 1000102114 | 1000046113 | yes |
| 14 | 0x0800848F | 1000102217 | 1000064760 | yes |
| 15 | 0x080084BD | 1000102275 | 1000065035 | yes |
| 16 | 0x08008655 | 1000103125 | 1000003057 | yes (shared) |
| 17 | 0x080084EB | 1000102353 | 1000110007 | yes |
| 18 | 0x08008524 | 1000102444 | 1000064476 | yes |
| 19 | 0x08008542 | 1000102502 | 1000035055 | yes |
| 20 | 0x08008652 | 1000103122 | (none - no-op) | -   |
| 21 | 0x08008567 | 1000102547 | 1000064543 | yes |
| 22 | 0x08008582 | 1000102602 | 1000011511 | yes |
| 23 | 0x080085E3 | 1000102743 | 1000063175 | yes |
| 24 | 0x08008602 | 1000103002 | 1000006134 | yes |
| 25 | 0x0800868E | 1000103216 | 1000003057 | yes (shared) |
| 26 | 0x080086AA | 1000103252 | 1000003057 | yes (shared) |
| 27 | 0x080086C6 | 1000103306 | 1000003057 | yes (shared) |
| 28 | 0x0800861F | 1000103037 | 1000064637 | yes |

Notes (PROVEN):
- No index is a duplicate pointer, but SIX indices (6, 7, 16, 25, 26, 27) share
  worker 0x0800062F (octal 1000003057) - the generic "MON 377B sub-function 2
  with fixed argument 0o66" path. Three more (11, 12, 20) are proven no-ops that
  jump straight to the common exit.
- `N500-SYMBOLS.SYMB` was consulted for every worker offset; none of the worker
  entry addresses matches a symbol value - that symbol table is the general
  ND-500 monitor set and does not label these local swapper routines.
- Reachability: with the correct bound (fn <= 0o34 = 28) **all of 0..28 are
  reachable**. There is no unreachable tail.

---

## 4. Shared data structures the workers touch (PROVEN)

These bases recur across the workers; understanding them is what makes the
per-handler analysis meaningful. Addresses are as they appear in the PSEG
(`laddr`/`rladdr` immediates).

| Handle | Base immediate | Stride | Index / bound | Role (INFERRED) |
|--------|----------------|--------|---------------|------------------|
| Table A "slot table" | `$1000700000` (0x08038000) | 0o144 = 100 | small id, bound-checked `[7 .. [0x128E4]]` | primary per-segment/per-process descriptor table |
| Table B "seg-4 array" | `$4000004400` (seg 4, off 0x900) | 0o400 = 256 | id, field `.110` masked to 16 bits | large per-segment table; `.110` is a descriptor word |
| Table C "seg-6 map" | `$6000000000` (seg 6) | 0o10 = 8 | id*8 | page/map entries (getbf state bitfields) |
| Table D "seg-5" | `$5000204000` (seg 5) | - | - | secondary array (used by idx 10) |
| List head | `$1000224730` (0x080249D8) | 0o374 links | - | linked-insert list of descriptors |
| Hot flags | `$1000436554` / `$1000436560` (0x23D6C / 0x23D70) | - | tested | gate whether MON 377B is emitted / swapper suspended |
| Stats block | `$1000461014`+ (~0x2620C) | per-code | - | per-request counters; loop bumps `$1000461200` (total msgs) |

Bound variable `[0x128E4]` (`$1000224344`) is the count of valid Table-A slots:
workers reject ids `< 7` or `> [0x128E4]` with error code 0o2067 (e.g. idx 0
worker, lines 1000053727-1000053753). PROVEN.

The **hot flags** cross-reference is proven: worker 0x0800062F (1000003057)
tests `[0x23D6C]` and `[0x23D70]` before issuing its MON 377B, skipping the call
when either is set (lines 1000003077-1000003115); the same flags are tested by
the paging routines at 1000000223/1000000241 etc. PROVEN: 0x23D6C/0x23D70 gate
MON-377B emission.

---

## 5. Per-handler analysis (index 0..28)

For each: entry, the message field(s) marshalled (PROVEN from the stub), the
worker and what its call-tree reaches (paging primitives dctsb/pctsb/dcc/dmon,
zpgu/zwip page-table ops, rphs; MON 377B; the tables above; stats), an INFERRED
name, and any ND-100-side MSW* correlation (correlation, **not** proof).

MSW* named codes proven on the ND-100 side (from the coordinator carve):
`MSWFI=0`, `MSWSTART=0o7=7`, `MSWPFAULT=0o12=10`, `MSWSWAIT=0o24=20`,
`MSWDO=0o34=28`.

---

### idx 0 - entry 0x080083D8 (1000101730), worker 1000053713
- **PROVEN**: stub loads `[0x240BC]`, sets `r.34 := 1`, calls worker; stores a
  returned word to `b.24`. Worker bound-checks the id `[7 .. [0x128E4]]`, indexes
  Table A (id*0o144), reads descriptor byte fields, calls 1000006650 and
  1000052116 (the page-release worker). Call-tree reaches MON 377B, dctsb/pctsb,
  zpgu/zwip.
- **INFERRED name**: finish / free one segment slot (per-slot teardown).
- **MSW correlation**: index 0 == `MSWFI` (ND-100 side). Correlation only.

### idx 1 - entry 0x080083F7 (1000101767), worker 1000052116
- **PROVEN**: stub passes `[0x240BC]`. Worker indexes Table A (id*0o144), walks
  the segment's pages via Table C (seg 6, *0o10), issues `zpgu`/`zwip` (zero
  page-used / write-in-progress), decrements page counters `[0x225022]`/
  `[0x225020]`, calls 1000034517. Reaches MON 377B once.
- **INFERRED name**: release / unmap the working set (pages) of one segment.

### idx 2 - entry 0x08008474 (1000102164), worker 1000060231
- **PROVEN**: stub passes `[0x240BC]`. Worker indexes Table A (id*0o144),
  performs a `riom` (DMA transfer, line 1000060340) of descriptor words to/from
  `[0x437240]`, loops with `wconv`, calls 1000033447 and 1000001755. Call-tree
  reaches MON 377B, dctsb, zpgu/zwip.
- **INFERRED name**: transfer / copy segment descriptor block (DMA) + page fixup.

### idx 3 - entry 0x080083A2 (1000101642), worker 1000060473
- **PROVEN**: stub passes `[0x240BC]`. Worker computes a count from `h1`
  (`h1 - 1`), loops that many times; each iteration indexes an array via
  `rladdr @b.60`, calls the generic MON-377B worker 1000003057, issues
  `zpgu`/`zwip`. Uses Table C (seg 6).
- **INFERRED name**: release a *range* of segments/pages (loop over a count).
- Note: fn 3 is one of the two loop-head special cases (section 2.1).

### idx 4 - entry 0x080083BD (1000101675), worker 1000061273
- **PROVEN**: stub passes `[0x240BC]`. Worker (large frame 0o544) scans a table
  via Table C (seg 6, *0o10), reads state bitfields (`getbf r2.(4),$26,$3` etc),
  compares against states 3/4/7, calls 1000050341, 1000035012. Call-tree reaches
  5 MON 377B, dctsb/pctsb, heavy zwip/zpgu (the deepest paging of any handler).
- **INFERRED name**: scan the segment table and swap working sets in/out
  (a state-machine sweep).

### idx 5 - entry 0x0800836C (1000101554), worker 1000077534
- **PROVEN**: stub passes `[0x240BC]`. Worker (frame 0o140) first calls
  1000110007 (the internal-table (re)builder), sets flags `[0x246464]`/
  `[0x246470]`, calls 1000000215, 1000047365, 1000065427, 1000077515,
  1000066175, 1000071273, 1000075410, 1000076133; issues `cwip` (connect
  write-in-progress, line 1000100120). Uses Table B (seg 4). Reaches MON 377B,
  dctsb/pctsb. This is the most sub-call-rich handler.
- **INFERRED name**: initialize / activate the swapper working set for a
  segment (bring-up).

### idx 6 - entry 0x08008672 (1000103162), worker 1000003057
- **PROVEN**: stub sets a local to constant **0o66**, zeroes and tests an
  adjacent (always-zero) local, then `call 1000003057` with 0o66. Worker
  1000003057 stores the arg to `[0x437234]`, tests the hot flags 0x23D6C/0x23D70,
  and issues **MON 377B sub-function 2** (lines 1000003165-1000003207).
- **INFERRED name**: generic swap request carrying fixed code 0o66.

### idx 7 - entry 0x080086E2 (1000103342), worker 1000003057
- **PROVEN**: identical shape to idx 6 (constant 0o66 -> worker 1000003057 ->
  MON 377B sub-fn 2). Falls through to the common exit at 1000103376.
- **INFERRED name**: generic swap request (fixed code 0o66).
- **MSW correlation**: index 7 == `MSWSTART`. Correlation only - if accurate,
  "start" is realized as a MON-377B sub-fn-2 request with sub-code 0o66.

### idx 8 - entry 0x08008412 (1000102022), worker 1000046576
- **PROVEN**: stub extracts `h2 := r.6` (16-bit id), `wconv`, passes it. Worker
  indexes Table B (seg 4, *0o400), reads descriptor `.110`, does a linked insert
  into list `[0x249D8]` (call 1000000137), then calls 1000011511 and 1000004562
  and the generic 1000003057. Call-tree reaches MON 377B, **rphs**, dctsb/pctsb,
  zpgu/zwip.
- **INFERRED name**: connect / page-in a segment (establish it via RPHS).

### idx 9 - entry 0x08008431 (1000102061), worker 1000046242
- **PROVEN**: stub passes `[0x240BC]`. Worker indexes Table B (seg 4), calls
  1000011511, 1000046113 (list insert), 1000006650; issues `dcc`; increments
  stats `[0x461170]`/`[0x461174]`; sets Table-A field `.26` bit. Reaches MON 377B,
  rphs, dctsb/pctsb.
- **INFERRED name**: allocate and link a segment, updating request counters.

### idx 10 - entry 0x08008387 (1000101607), worker 1000042045
- **PROVEN**: stub passes `[0x240BC]`. Worker (frame 0o2104) reads a Table B
  (seg 4) descriptor, a Table A (seg, *0o144) record and a Table D (seg 5)
  entry, checks state fields (`getbf`, compares 0x15/0x16), increments stats
  `[0x461134]`, and returns error codes 0o2067 / 0o1030 / 0o1031 on bad state.
  The worker's call-tree reaches **no** paging primitive and **no** MON 377B.
- **INFERRED name**: page-fault notification / accounting (look up the faulting
  segment, validate its state, bump a counter) - a bookkeeping handler that
  itself does no paging.
- **MSW correlation**: index 10 == `MSWPFAULT`. The "page-fault" name and the
  proven "look up segment, check state, count" shape agree well. Correlation.

### idx 11 - entry 0x0800864C (1000103114) - NO-OP
- **PROVEN**: the entire handler is `go $262` -> lands at 1000103376 (`go $13`)
  -> 1000103411 (backward `go` to loop restart). No worker, no field access, no
  MON, no paging.
- **INFERRED name**: reserved / no-op function code (acknowledged, no action).

### idx 12 - entry 0x0800864F (1000103117) - NO-OP
- **PROVEN**: `go $257` -> same common exit 1000103376 as idx 11. No action.
- **INFERRED name**: reserved / no-op.

### idx 13 - entry 0x0800844C (1000102114), worker 1000046113
- **PROVEN**: stub extracts `h1 := r.24`, multiplies by 0o400 and forms a Table B
  (seg 4) address, passes it. Worker inserts an element into the linked list at
  `[0x249D8]` (stride 0o374, via call 1000000137), comparing byte keys. Call-tree
  reaches **no** paging and **no** MON.
- **INFERRED name**: enqueue / link a segment descriptor onto the swap list.

### idx 14 - entry 0x0800848F (1000102217), worker 1000064760
- **PROVEN**: stub extracts `h2 := r.6` (`wconv`) and `h3 := r.20` - two ids -
  and passes both. Worker bound-checks the id `[7..[0x128E4]]`, returns 0o2067 if
  bad, else calls 1000004562. Call-tree reaches MON 377B, dctsb/pctsb, zpgu/zwip.
- **INFERRED name**: attach / connect a segment identified by an id pair.

### idx 15 - entry 0x080084BD (1000102275), worker 1000065035
- **PROVEN**: byte-for-byte the same stub and worker shape as idx 14, except the
  worker's terminal call is **1000006650** (vs 1000004562 for idx 14). Same bound
  check, same 0o2067 error, same wconv of `r.6`/`r.20`.
- **INFERRED name**: the mirror of idx 14 - the opposite-direction connect /
  detach (paired primitive). The single differing call target is the only proven
  distinction.

### idx 16 - entry 0x08008655 (1000103125), worker 1000003057
- **PROVEN**: constant 0o66 -> worker 1000003057 -> MON 377B sub-fn 2 (same as
  idx 6/7).
- **INFERRED name**: generic swap request (fixed code 0o66).

### idx 17 - entry 0x080084EB (1000102353), worker 1000110007
- **PROVEN**: stub builds two small buffers with `smove` from `[0x461114]` and
  `[0x224044]`, then calls worker. Worker 1000110007 recomputes `[0x461110]`,
  `[0x507454..507474]`, rebuilds the array at `[0x461230]` and `bmove`s three
  3-word blocks. Call-tree reaches **no** paging and **no** MON.
- **INFERRED name**: (re)initialize / reset the swapper's internal bookkeeping
  tables.

### idx 18 - entry 0x08008524 (1000102444), worker 1000064476
- **PROVEN**: stub extracts `by1 laddr r.20`, passes it. Worker stores three
  fields out of the record: `h1(r.12) -> [0x224114]`, `h2(r.2) -> [0x246434]`,
  `h3(r.4) -> [0x246440]`. No paging, no MON.
- **INFERRED name**: set swapper configuration / parameter words from the message.

### idx 19 - entry 0x08008542 (1000102502), worker 1000035055
- **PROVEN**: stub extracts `h1 := r.20`, zeroes `r.26`, sets `r.30 := 0o77`
  (a mode), passes them. Worker bound-checks the id, indexes Table A (seg,
  *0o144) and Table B (seg 4, *0o400), reads `getbf` state, tests `[0x224744]`,
  calls 1000017351 and 1000014725; returns 0o2067 / 0o1031 on error. Call-tree
  reaches MON 377B, dctsb/pctsb, zwip.
- **INFERRED name**: swap / fix a segment by id, with mode 0o77.

### idx 20 - entry 0x08008652 (1000103122) - NO-OP
- **PROVEN**: `go $254` -> common exit 1000103376. No worker, no action.
- **INFERRED name**: reserved / no-op.
- **MSW correlation**: index 20 == `MSWSWAIT` (0o24). If accurate, "swapper wait"
  is realized in-domain as a no-op acknowledgement (the actual wait is presumably
  handled on the ND-100 side). Correlation only.

### idx 21 - entry 0x08008567 (1000102547), worker 1000064543
- **PROVEN**: stub passes `[0x240BC]`. Worker bound-checks the id `[7..[0x128E4]]`
  (0o2067 on error), then writes two fields into the Table B (seg 4) descriptor:
  `h3 -> r2.(12)`, `h4 -> r2.(10)`. No paging, no MON.
- **INFERRED name**: set two fields of a segment's seg-4 descriptor.

### idx 22 - entry 0x08008582 (1000102602), worker 1000011511
- **PROVEN**: stub extracts `h2/h4 := r.6` (`wconv`), forms a Table B (seg 4,
  *0o400) address, reads `.110 & 0xFFFF`, builds a local record (`r.34 :=` code
  pointer 0x24073, etc.), passes it. Worker 1000011511 indexes Table A (*0o144),
  scans descriptors, calls 1000010432 and the generic 1000003057. Call-tree
  reaches MON 377B, **rphs**, dctsb/pctsb.
- **INFERRED name**: page-in / establish a segment's working set through the RPHS
  path.

### idx 23 - entry 0x080085E3 (1000102743), worker 1000063175
- **PROVEN**: stub extracts `h1 := r.6` (`wconv`), passes it. Worker 1000063175
  (frame 0o440) sets up a pointer in `b.114` and performs `putbi @b.114,$10`
  (set a bit through an indirect pointer). No paging, no MON, no table indexing
  that resolves to the known structures.
- **UNKNOWN**: the target of the `putbi` is an indirected pointer built at
  runtime; the byte set cannot be named from static bytes alone.
- **Settling experiment**: trace `b.114`/`b.370` at 1000063217-1000063242 under
  nd500x for a live idx-23 request to see which object's bit is set.

### idx 24 - entry 0x08008602 (1000103002), worker 1000006134
- **PROVEN**: stub passes `[0x240BC]`, stores a result to `b.24`. Worker
  bound-checks the id `[7..[0x128E4]]`, allocates the Table A slot (id*0o144),
  `bmove`s a **0o144-word template** from `[0x437274]` into the slot
  (line 1000006240), then initializes the descriptor's bitfields from the DSEG
  flags `[0x224770]`/`[0x224754]`/`[0x224760]`/`[0x224764]` via `putbi`, and
  copies id/word fields in. No paging, no MON.
- **INFERRED name**: create / define a new segment descriptor (initialize a
  fresh Table-A slot from a template).

### idx 25 - entry 0x0800868E (1000103216), worker 1000003057
- **PROVEN**: constant 0o66 -> worker 1000003057 -> MON 377B sub-fn 2.
- **INFERRED name**: generic swap request (fixed code 0o66).

### idx 26 - entry 0x080086AA (1000103252), worker 1000003057
- **PROVEN**: constant 0o66 -> worker 1000003057 -> MON 377B sub-fn 2.
- **INFERRED name**: generic swap request (fixed code 0o66).

### idx 27 - entry 0x080086C6 (1000103306), worker 1000003057
- **PROVEN**: constant 0o66 -> worker 1000003057 -> MON 377B sub-fn 2.
- **INFERRED name**: generic swap request (fixed code 0o66).

### idx 28 - entry 0x0800861F (1000103037), worker 1000064637
- **PROVEN**: stub extracts `h1 := r.6` (`wconv`) and `h2 := r.20`, passes them.
  Worker forms a Table B (seg 4, *0o400) address, reads `.110 & 0xFFFF`, builds a
  local record (`r.40 :=` code pointer 0x04562, `r.44 := wconv(b.26)`), and calls
  1000011511 (the same RPHS page-in worker used by idx 22). Call-tree reaches
  MON 377B, **rphs**, dctsb/pctsb.
- **INFERRED name**: perform the swap - drive the RPHS page-in for a segment.
- **MSW correlation**: index 28 == `MSWDO` (0o34, the max code). "Do the swap"
  and the proven RPHS-page-in shape agree well. Correlation only.

---

## 6. Function-code meaning summary

`Paging?` = worker call-tree reaches dctsb/pctsb/dcc/zpgu/zwip/rphs.
`MON?` = reaches a MON 377B. Names are INFERRED unless marked.

| Fn | Entry | Worker | Paging? | MON? | INFERRED meaning | MSW* (corr) |
|---:|-------|--------|:-------:|:----:|------------------|-------------|
| 0  | 0x83D8 | 1000053713 | yes | yes | free/finish one segment slot | MSWFI |
| 1  | 0x83F7 | 1000052116 | yes | yes | release working set of a segment | |
| 2  | 0x8474 | 1000060231 | yes | yes | transfer/copy segment descriptor (DMA) | |
| 3  | 0x83A2 | 1000060473 | yes | yes | release a range of segments/pages | |
| 4  | 0x83BD | 1000061273 | yes | yes | scan table, swap working sets in/out | |
| 5  | 0x836C | 1000077534 | yes | yes | initialize/activate swapper working set | |
| 6  | 0x8672 | 1000003057 | no  | yes | generic swap request, code 0o66 | |
| 7  | 0x86E2 | 1000003057 | no  | yes | generic swap request, code 0o66 | MSWSTART |
| 8  | 0x8412 | 1000046576 | yes | yes | connect/page-in a segment (RPHS) | |
| 9  | 0x8431 | 1000046242 | yes | yes | allocate+link a segment; bump counters | |
| 10 | 0x8387 | 1000042045 | no  | no  | page-fault notification / accounting | MSWPFAULT |
| 11 | 0x864C | (no-op)   | no  | no  | reserved / no-op | |
| 12 | 0x864F | (no-op)   | no  | no  | reserved / no-op | |
| 13 | 0x844C | 1000046113 | no  | no  | link a segment descriptor onto a list | |
| 14 | 0x848F | 1000064760 | yes | yes | attach/connect segment (id pair) | |
| 15 | 0x84BD | 1000065035 | yes | yes | mirror of fn 14 (detach/other dir) | |
| 16 | 0x8655 | 1000003057 | no  | yes | generic swap request, code 0o66 | |
| 17 | 0x84EB | 1000110007 | no  | no  | (re)initialize internal tables | |
| 18 | 0x8524 | 1000064476 | no  | no  | set swapper config/parameter words | |
| 19 | 0x8542 | 1000035055 | yes | yes | swap/fix a segment by id (mode 0o77) | |
| 20 | 0x8652 | (no-op)   | no  | no  | reserved / no-op | MSWSWAIT |
| 21 | 0x8567 | 1000064543 | no  | no  | set two seg-4 descriptor fields | |
| 22 | 0x8582 | 1000011511 | yes | yes | page-in working set (RPHS) | |
| 23 | 0x85E3 | 1000063175 | no  | no  | UNKNOWN (indirect bit-set) | |
| 24 | 0x8602 | 1000006134 | no  | no  | create/define a segment descriptor | |
| 25 | 0x868E | 1000003057 | no  | yes | generic swap request, code 0o66 | |
| 26 | 0x86AA | 1000003057 | no  | yes | generic swap request, code 0o66 | |
| 27 | 0x86C6 | 1000003057 | no  | yes | generic swap request, code 0o66 | |
| 28 | 0x861F | 1000064637 | yes | yes | perform swap (RPHS page-in) | MSWDO |

---

## 7. Cross-reference to swapper state constants (Task 3)

The prior-art constants asked about are STATE values, not function codes.
`N500-SYMBOLS.SYMB` (verified) gives:

- `SWPWA = 000005` (octal 5) - swap-wait state value.
- `SWPPI = 000006` (octal 6) - swap-ping state value.
- `SWPPA = 004673`, `LACTP = 145457`.

**PROVEN**: these are absolute constant values in the symbol table. **UNKNOWN /
NOT SUPPORTED by the bytes**: any claim that function code 4 == LSWPWAIT,
6 == LSWPPING, or 12 == LACTIVE. The handler bytes never assign the literal
function-code number to a segment state; the "prior art" values 4/6/12 do not
even match the symbol table (`SWPWA`=5, `SWPPI`=6), and the function-code index
space (0..28, the MSW* family) is a **different namespace** from these state
constants. Do not conflate them.

The one solid state cross-reference the bytes DO support: the workers read/write
per-segment **state bitfields** via `getbf`/`putbi` on Table C (seg 6) entries
(e.g. worker 1000061273 compares states 3/4/7; worker 1000006134 initializes
them from DSEG flag words). Correlating those field values with `SWPWA`/`SWPPI`
etc. is the productive next step, but requires reading the field encodings, not
the function-code table.

---

## 8. Open items

1. **idx 23 (fn 0o27) worker 1000063175** - UNKNOWN. Which object's bit does the
   `putbi @b.114,$10` set? Trace `b.114` under nd500x for a live request.
2. **`$66` constant (0o66)** shared by fn 6/7/16/25/26/27 - what sub-code 0o66
   means to the MON 377B sub-function 2 receiver (the ND-100 5SWAP side) is not
   in this PSEG. Six function codes funnelling to the identical request is
   proven; whether they are true aliases or the caller distinguishes them by the
   message payload (which these stubs do NOT vary) is UNKNOWN.
3. **fn 11/12/20 no-ops** - proven to do nothing in-domain. Whether the ND-100
   side expects a side effect (and this is a stub/unimplemented slot) or these
   are genuinely "acknowledge only" is UNKNOWN from the ND-500 side.
4. **fn 3 / fn 5 loop-head special cases** (lines 10593/10595) - the exact extra
   effect of the `go $25` / `go $14` branches before the jumpg is loop plumbing;
   worth a byte-level trace if fn 3/5 behaviour ever matters.
5. **MSW* correlations** for fn 0/7/10/20/28 are consistent with the proven
   shapes but remain correlations; only the ND-100-side `SWMSG.SWPST` mapping
   (being written to `SINTRAN/ND500/ND500-SWAPPER-ANALYSIS.md` section 12) can
   promote them to proof.
6. **Worker names** - none of the 20 distinct workers has a symbol-table name.
   If a richer `.SYMB` (with SWAPPER-K01 local labels) surfaces, re-run the
   offset lookup in section 3 to name them.
