# fn-5 completion staging carve — four answers (relayed 2026-08-17)

Provenance: byte-verified by a carve sub-agent during ND500UC milestone 15
against `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-pseg.asm`
(line numbers cited). The sub-agent could not reach its requester, so the
report was relayed via the main session. All grades PROVEN unless noted.
No loopi sits inside any cited range — the listing is in sync there.

## ITEM 1 — 1000065112 exact body (PROVEN, asm lines 8721-8769)

Page = 100: `w1 laddr $1000620000+; and $777777777; udiv $4000`
(0x32000/0x800). Then:

1000065163 move [0o225070],r.24 ; 65172 w1 =: r.30 ; 65174 call 1000001422;
ifkret ; 65203 `040 105 w1 =: b.24` ; 65205 r:=b.10 ; 65207 move
[0o225070],r.24 ; 65216 `032 106 206 move b.30,r.30` ; 65221 bi1 clr ;
65222 call 1000001755.

Between the 1422 return and the 1755 call the only ops are a store of w1,
r-base loads, two mem-to-mem moves, bi1 clr — none touch r1. So
1755(A, 100, value = the exact PTE 1422 returned): an IDENTITY rewrite,
A[100] unchanged.

Descriptor write (8742-8762): `set1 b.40; w1:=b.40; w2:=[0o225074];
h rladdr $5000204000+; h1 putbf r.0,$16,$2` = state := 1; then
`w1 := b.24; h1 putbf r.0,$0,$16` = base := PTE & 0x3FFF (putbf width 14
bits). Descriptor = {state 1, base = low 14 bits of A[100]}.

So with A[100] <= 0x3FFF the slot-1 sweep target and slot-4's base are the
SAME frame -> double unlink -> announce 0o21. ESCAPE HATCH (from item 2):
the sweep uses the RAW 16-bit halfword, the descriptor keeps 14 bits.
Stage A[100] = 0x4000|f (bit 14 set, bit 15 clear to avoid sign-extension):
sweep unlinks halfword value 0x4000|f, slot-4 base becomes f — two
DIFFERENT chain entries, both stageable. seg6 entry for index 0x4000|f is
at seg6 offset 0x20008+8f, inside the staged 132-page (0x42000) segment 6.
Closed staging exists even with s_A=1, s_D=4.

## ITEM 2 — 1000034517 (PROVEN, lines 4826-4885)

(a) yes: b.34 := passed cell (`032 105 107`, line 4849); walk 34646
`h1 := @b.34`; advance 34711-34730 `w1*$10; rladdr $6000000000+;
h2 laddr r.0; b.34 := r2` = &seg6[link*8].h0.

(b) yes: unlink 34734-34752 `h2 := @b.34; *$10; rladdr; h3 := r.0;
h3 =: @b.34`. Extra fact: 34772-35006 then ZEROES the removed frame's own
link (`bi3 clr; h3 =: r.0`).

(c) yes: 34646-34700 tests the link cell for 0 BEFORE comparing to the
target — chain exhaustion (or empty head) announces 0o21 (`032 021 112`
loads the code at 4850); frame 0 can never match, always announces.

(d) yes: 1000027120 (lines 3878-3897) `w1 := r.32; test; if = go $46` ->
ret when record.32 == 0. No announce path in it.

Sweep masking (72767 state!=0 arm, lines 9728-9731): `040 110 w1 =: b.40;
... 374 020 206 h1 =: r.30; call 34517`. NO mask — the raw halfword PTE
(as 1422's `h1 := r.0` returned it) is the search target, compared as
halfword (`374 034 106 h1 comp b.30` at 34704). Stage sweep-visible PTEs
as the exact halfword to unlink.

## ITEM 3 — 1000002762 (PROVEN, lines 475-494)

`h3 := r.136; h3 =: r2.(0)` (seg6[frame*8] bytes 0-1 = link := old record
head); `h4 =: r.136` (record head := frame); `h1 := r.124; h1 =: r2.(2)`
(bytes 2-3 = record self-index). A push writes BOTH halfwords of the
entry's word0, then 1000002675 (record.6 += 1; announce 0o50 only when the
sum hits 0o177777). It never touches other frames' entries.

seg6 word@8 (frame 1) is written only if the VALUE 1 appears as a chain
link, an unlink target, a 2762 push frame, or a phase-1 base (state==0 arm
also writes seg6[base].h2 := slot at 73442; state!=0 arm writes .6/.4/.2
fields). Contract: keep the value 1 out of every staged PTE/base/link and
entry 1 stays zero -> phase-2 bound b.64 = 1; frame 1 then does not need
to be in the chain at all (drops the earlier "frame 1 last" rule).

## 66411 ORDERING (PROVEN, lines 8916-9055), by address

- step1 66603-66651: B[0] := 1422(A,102)
- step2 66736-67022: loop B[0..2] := 1422(A,103..105)
- step3 67111-67147: loop 1755(A, 102+k, value = page number itself —
  `w1 := b.50; w1 + b.30` at 67124, untouched to the call)
- step4 PROBE 67151-67236: each iteration writes B[b.34] := 1422(A,
  b.54+b.34) immediately (1755 at 67217), so B[0] := A[pA] = F is stored
  DURING the probe
- step5 REWRITE 67240-67303: loop 1755(A, b.54+k, value = b.54+k, set
  67260-67262) — the page-number identity overwrite of A[pA..] happens
  AFTER the probe.

So: step 5 writes the page number pA+k (F at A[pA] is overwritten with
pA), and B[0] keeps F. Bases differ: step3 uses b.30 (pages 102..105),
step5 uses b.54 (pages pA..).

Knock-on for the chain: slot-1's sweep will find identity PTEs
{102,103,104,105,pA} plus A[100] and anything else nonzero — all those
halfword values must be in the [0o224370] chain, along with bases
{Q(slot1), 2, 3, f(slot4)}, the 0x4000|f alias, and phase-2's {G, B, F}.
All distinct, nonzero, and != 1.

---

# SECOND RELAY (same day, later): 66411 stamp order + corrected staging

Byte-verified follow-up. SUPERSEDES parts of the section above: the F-page
(F[0]=G) and the 0xF8 page are obsolete; G drops out entirely. The park at
P=0x080002DB is the last byte of `call $1000033313` (raw at PSEG 0x2D6,
spanning 0o1326-0o1333) — the 1165 walk hit a ZERO directory entry and
dove into the frame allocator.

## ITEM 1 — 66411 order and the stamps (PROVEN, raw bytes)

The three stamps write the descriptor STATE field (putbf position 14 width
2 = bits 14-15 — the exact field 1422/1755 read as state; word-mode arm:
putbf $36,$2 = bits 30-31). Stamp1 at 0o66517 state:=0; stamp2 at 0o66652
set1 = 1; stamp3 at 0o67024 move $2 = 2.

Exact order: (s0) 66517 state:=0 -> (a1) 66603 b.74:=1422(A,102) ->
(a2) 66630 1755(B,0,b.74) — state-0 page-0 write = **descriptor BASE :=
PTE_A(102) =: D; any staged base is DISCARDED here** -> (s1) 66652
state:=1 -> (b) 66736-67022 k=0..2: 1755(B,k,1422(A,103+k)) — state-1
PHYSICAL writes at D*2048+2k: **dir[0..2] := PTE_A(103..105) =: T0,T1,T2**
-> (s2) 67024 state:=2 -> (c) 67111 identity 1755(A,102+k)x4 -> (d) 67151
probe: 1755(B,i,1422(A,pA+i)) via the 1165 walk: dir[i>>10] -> leaf write
T0*2048+2(i&0o1777) -> (e) 67240 rewrite A[pA..] identity -> (f) 67305
capacity (b.34+1)*256 + header bmove.

**66411 builds slot B as a TWO-LEVEL table from A's pages 102-105.**
Required at 66411 entry: slot-6 descriptor initial value IRRELEVANT (state
stamped to 0 first, base overwritten). What matters: Q[102]=D, Q[103]=T0,
Q[104]=T1, Q[105]=T2 — four distinct nonzero frames, each with a ZEROED
physical page.

## ITEM 2 — 1165 walk (PROVEN, lines 173-231)

16-bit dir index = `w shl r1,$66` (>>10), `and $77`, `*2`; addr =
b.24*$4000 (2048) + idx; dmof `h1 := r.0` halfword read. Zero entry ->
`r:=b.10; move b.34,r.24 (slot); call 1000033313` at 1000001326, then
marks the new frame type-3 in seg6 (putbf r.4,$26,$3) and dmof-writes it
into the dir entry. Nonzero entry -> returned as mid base; the caller then
dmof-accesses PTE at entry*2048 + (page&0o1777)*2 — **a nonzero dir entry
must name a physical page you own**. Allocator path: 33313 -> 1000032100
(lines 4396+): reads Table-A record [0o700000+slot*0o144] fields +6/+10;
with zero fields falls into the uncarved pool-refill chain (announce 0o15
after 2 rounds at 32202-32236) — off-design for the harness; made
UNREACHABLE by the staging below. The only 1165 walks on the whole path
are 66411's probe (dir[0]) and 70054's three probes (dir[0],[1],[2]).

## ITEM 3 — corrected slot-6 staging and full state timeline

Stage: Q[102]=D, Q[103]=T0, Q[104]=T1, Q[105]=T2 (zeroed phys pages);
Q[pA]=F (any nonzero halfword, no page needed); Q[pA+1]=0; Q[100]=0x40FC;
slot-6 descriptor: anything.

Timeline:
- 66411 exit: slot6 = {state 2, base D}; D-page = dir {T0,T1,T2,0...};
  T0-page = {F, 0, ...}.
- 70054: [0o436564]=1 so probes pages k*0o2000 (1024) for k=2,1,0 ->
  dir[2]=T2 -> T2[0]=0; dir[1]=T1 -> T1[0]=0; dir[0]=T0 -> T0[0]=F
  nonzero -> b.44=1, no announce 0o110, no allocator. Then loop b.24=1..2
  (lines 9199-9255): each flips state 1, reads D[b.24] (=T1 then T2),
  rewrites it, flips state 2, 34517-unlinks T1 and T2 from [0o224370],
  pushes them onto [0o436604]. Then the b.44==1 collapse (9256-9323):
  reads D[0]=T0 under state 1, sets state 0, reads base D, 1755(B,0,T0)
  -> base := T0, state := 1, 34517-unlinks D, pushes D onto [0o436604].
  Exit: slot6 = {state 1, base T0}; [0o436604] = D->T2->T1->0.
- 75410: [0o436604] nonzero -> the drain loop RUNS (3 iterations): pushes
  D, T2, T1 back onto the [0o224370] chain and maps A[247]:=D, A[248]:=T2,
  A[249]:=T1 (1755 on slot 1, pages [0o246410]++ from 247); b.40 ends 4 ->
  releases 4*2048 bytes = DSEG pages 246-249 into the heap; the 3213 range
  check stays self-consistent. **Map DSEG writable through 0x7D000** (was
  0x7C000).
- 65112: slot 6 untouched; slot4 := {state 1, base 0xFC} per the alias
  scheme.
- 72767 phase 1: slot-1 sweep unlinks VALUES {pA, 102, 103, 104, 105,
  0x40FC, D, T2, T1} (A[102..105] are identity page numbers after (c);
  A[247..249] hold D,T2,T1 from the drain), plus base unlinks
  {Q, 2, 3, 0xFC}.
- 72767 phase 2 (slot6 {state 1, base T0}, b.64=1 via the
  value-1-exclusion rule): unlinks F (=T0[0]) then base T0; state==1 ->
  putbf field-26 := 2, seg6[T0].h2 := 6, record push, then `w1 comp $2;
  if >< go $530` at 1000074655 jumps STRAIGHT to ret 1000075407 — the
  final 1024-page sweep and the state:=2 stamps are SKIPPED on the
  state-1 path. Announce-free.

FULL CHAIN to stage at [0o224370] (order free, all distinct nonzero
halfwords, value 1 excluded, each *8 inside staged seg6):
{Q, 2, 3, 0xFC, pA, 102, 103, 104, 105, 0x40FC, D, T1, T2, F, T0} — 15
entries. Physical zeroed pages needed: Q-table page (with staged entries),
D, T0, T1, T2, and 0xFC (slot-4 sweep). Drop the 0xF8 and F pages.

Trip-wire kept: T1, T2, D come back as A[247..249], so the slot-1 sweep
covers them — they are re-pushed by 75410 BEFORE 72767 runs, so the
double-appearance is legal (unlink, re-push, unlink).

---

# THIRD RELAY (same day, later): record layout, 70054 gates, heap, 0o2121

Byte-verified. Four answers to the staging agent's follow-ups.

## The 0o2121 retk (sub 1000004050, lines 4056-4147)

The observed ring is sub 1000004050 instruction-for-instruction. The K=1
exit is `retk` with error code 0o2121 loaded at 1000004143 (`w1 := $2121`)
— the last-resort frame stealer (top-of-Table-A, floor-guarded via
[0o224366], then 1000035012 -> 27204/34517). [0o224366]=0o177777 (set by
71273 at 71523) is compared UNSIGNED (65535 floor), so with [0o224340]=0
it can never succeed while Table A is empty. The "2121" family = resource
exhaustion retk, not announce (same code at 1000032465).

## ITEM 1 — 71273 record layout (PROVEN, lines 9423-9468, 9480+)

- Record n = 4 bytes at intake+0o40+4n. TYPE = byte at +1, masked &7.
  PAGE COUNT = halfword at +2.
- Loop #1 REMAPS the type via a BYTE-indexed read of the shipped table at
  DSEG 0x24014 (`w1 := [0o440024+bi4]; putbf r2.(0),$20,$3`): shipped
  bytes `00 00 00 00 00 00 00 03 00 00 00 03 ...` mean type 4 fetches the
  word at 0x24018 = 3 -> type 4 is REWRITTEN TO 3. With the shipped table
  no staged type survives into the 4..7 arm — that arm is unreachable.
- Loop #2 iterates a LOCAL copy whose entry-0 count was DECREMENTED by 1
  (72145-72170). Count 1 -> 0 iterations. For N chain entries from record
  0 you would need count N+1. BUT DON'T: the per-record tail (72614-72643)
  runs even with 0 iterations and writes seg6[entry0].h0 := old head, then
  [0o224370] := b.30 = 1 — the head=1 orphaning observed. ANY hw44>=1
  else-arm record orphans the staged chain, and count>=2 writes seg-6
  entry 1 (blowing phase 2's b.64 bound to 513). **REVERT TO hw44 = 0** —
  the staged-chain contract only holds there. ([0x12854]=6 staged hw51=6
  is harmless — 6 clebi writes into [0o700000+n*0o144] — but 0 is
  cleaner.)

## ITEM 2 — 70054 gates (PROVEN, lines 9165-9323)

After the probes: `b.24 := b.44; comp2 b.24,$2; if >> go $345` -> 70562.
Harvest loop 70220-70557 only when b.44 <= 2; collapse 70570+ only when
b.44 == 1 (70562 `comp2 b.44,$1; if >< go $420` -> 71205). The observed
end state (slot6 still {state 2, base D}, [0o436604]=0) is the 71205 path:
b.44 = 3, i.e. the FIRST probe returned nonzero. Probes with
[0o436564]=1: k=2 -> T2[0]; k=1 -> T1[0]; k=0 -> T0[0]. b.44=1 requires
**T2[0]==0 AND T1[0]==0 AND T0[0]!=0**. Check the PHYSICAL first
halfwords at T2*2048 and T1*2048 and that D,T0,T1,T2,Q pages are pairwise
distinct and pre-zeroed — a page overlap (e.g. T2 sharing D's page:
D[0]=T0 nonzero aliasing into T2[0]) is the likely culprit. No mode cell
can cause it ([0o436564]=1 is forced by hw8=0 via 66175).

## ITEM 3 — heap mechanics (PROVEN)

4007 (lines 648-660) = size normalize: 0 -> 8, else ceil(x/64)*8. ALLOC
(5371 at 5410) normalizes the raw request (112 -> 16); RELEASE (4256 at
4266-4274) first multiplies the byte size by 8 then normalizes — so
alloc/release are SYMMETRIC, and 4256 inserts address-ordered with
both-side coalescing. The split path (6013-6052) plus a ~size/4-word
zero-fill (6062-6117; the exact fill target uses `laddr @b.30` — flagged
OPEN, a possible free-list-header clobber, the one leak suspect).
Diagnostic: dump the [0o246400] chain (word@+0 = next, word@+4 = size)
after each of the four passes.

THE ROBUST FIX is upstream: with item 2 fixed (T1[0]=T2[0]=0, T0[0]!=0),
70054 harvests T1,T2,D onto [0o436604]; 75410's drain maps them to
A[247..249] and seeds ONE release of b.40=4 blocks = **8192 bytes instead
of 2048** — four passes never exhaust, 26741/4050 are never entered, the
0o2121 retk is unreachable. (26741 at 3840-3877 only scavenges the
per-code lists [0o224554+] via the priority array [0o437570+], returns 0
with all-empty lists — hence the useless triple loop before 4050's retk.)
