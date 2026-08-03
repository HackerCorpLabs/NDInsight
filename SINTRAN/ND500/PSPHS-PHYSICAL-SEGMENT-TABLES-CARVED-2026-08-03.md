# SGLOA writes the physical-segment tables - PSPHS / PSLLI / PSULI / PSMOD

**Date:** 2026-08-03
**Segment:** `030-S3SM5` (ND-100 code, load base `040000B`)
**Supersedes:** section 3 of `SGLOA-SEGMENT-PLACE-CARVED-2026-08-03.md`, which read these
stores as "fields of a structure reached indirectly through `B-176`". That was wrong - see
section 1.

---

## 0. The short version

At PLACE time `SGLOA` writes **four entries in four separate resident tables**, all indexed by
the same segment number held in `B-176`:

| Table | Address | What SGLOA stores |
|---|---|---|
| `PSPHS` | `177401B` | the segment's **PHysical Start** - `M[B-174]` OR `M[B-75]` |
| `PSLLI` | `175341B` | **0** (lower limit) |
| `PSULI` | `175441B` | size-1, or `-1` when the size word is absent (upper limit) |
| `PSMOD` | `175541B` | `1` or `0`, taken from **bit 9** of a descriptor word (mode flag) |

`PSPHS` is the table the **`RPHS`** instruction reads - *Read from PHysical Segment*, which is
the instruction the 5SWAP trap fires on (`RPHS @1000010525`, see
`TRAP-PRINTER-HUNT-2026-08-03.md`). So PLACE writes the entry, and the swapper's `RPHS` reads
it. Those two ends of the open question now touch.

---

## 1. CORRECTION: `B-176` is an INDEX, not a pointer `[V]`

The earlier note read `STA I ,X 71` as "store through the pointer in `B-176`". The instruction
word says otherwise.

`007071B` = `STA` (`004000B`) + `1593`. In binary `1593` is `110 0011 1001`:

| Bit | Value | Meaning |
|---|---|---|
| 10 | 1 | X |
| 9 | 1 | I |
| 8 | 0 | B |
| 7-0 | `71B` | displacement |

Bit assignment cross-checked against two unambiguous lines in the same routine:
`046003 = LDA ,X 3` sets only bit 10 (so bit 10 is X), and `044603 = LDA ,B -175` sets only
bit 8 with a low byte of `203B` = -125 = `-175B` (so bit 8 is B and the displacement is 8 bits
signed). Bit 9 is therefore I.

`X=1, I=1, B=0` is **post-indexed indirect**: `EA = M[P + disp] + X`.

So the displacement names a **pointer word in the routine's own literal pool**, and `X` - loaded
from `B-176` immediately before each store - is added to it as an **array index**. Reading it
the other way round turns four table writes into one structure and hides the table names
completely.

---

## 2. The four stores, resolved `[V]`

`LDX ,B -176` reloads the index before every store, so all four use the same index.

```
143443  LDA ,B -174           A := the value computed by the open/interrogate chain
143444  ORA ,B -75            A := A | flags
143445  LDX ,B -176           X := segment number
143446  STA I ,X 71           M[143446+71] = M[143537] = 177401 = PSPHS  ->  PSPHS[X] := A

143447  LDX ,B -6
143450  LDD ,X 13             D := double word at [X+13]   (the size)
143451  JAF 3        -> 143454
143452  SKP IF DD UEQ 0
143453  JMP 4        -> 143457
143454  RADD CLD SD DA        A := D
143455  AAA -1                A := A - 1
143456  JMP 2        -> 143460
143457  SAA -1                A := -1        (size word absent / zero)
143460  LDX ,B -176
143461  STZ I ,X 57           M[143461+57] = M[143540] = 175341 = PSLLI  ->  PSLLI[X] := 0
143462  STA I ,X 57           M[143462+57] = M[143541] = 175441 = PSULI  ->  PSULI[X] := A

143463  LDX ,B -6
143464  LDA ,X 3
143465  BSKP ONE 110 DA       test bit 9 (110B / 8 = 9)
143466  JMP 3        -> 143471
143467  SAA 1                 A := 1
143470  JMP 2        -> 143472
143471  RADD CLD 0 DA         A := 0
143472  LDX ,B -176
143473  STA I ,X 47           M[143473+47] = M[143542] = 175541 = PSMOD  ->  PSMOD[X] := 0/1
```

Note `STZ` and `STA` at `143461`/`143462` carry the **same displacement** `57B` but sit at
addresses one apart, so they resolve to **two different pool words** and therefore two
different tables. Read as one structure they look like a pointless clear-then-store of a single
field; read correctly they are the lower and upper limit of a range.

### The literal pool

```
143536  055330   DOOVE      (called by JPL I 75 at 143441)
143537  177401   PSPHS
143540  175341   PSLLI
143541  175441   PSULI
143542  175541   PSMOD
143543  076012   PLRES      (called by JPL I 42 at 143501)
143544  177441   DSPHS      (not used by these four stores)
143545  036633   -          (no symbol; called by JPL I 34 at 143511)
```

The code jumps over this pool - `143533` branches past it - which is the ordinary SINTRAN
shape and the reason a straight-line disassembly prints nonsense mnemonics here.

---

## 3. Why the symbol identification is trustworthy `[V]`

All seven named pool words hit `SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB` on the **first** try,
with no near-misses, and they turn out to be one family rather than seven unrelated names.
Pulling every symbol in the neighbourhood shows the layout:

```
175341  PSLLI      175401  DSLLI
175441  PSULI      175501  DSULI
175541  PSMOD      175601  DSMOD
175641  USREG
...
177401  PSPHS      177441  DSPHS
177501  CSYDS
```

Three properties make this more than a name match:

1. **Every `PS*` table has a `DS*` twin** - Program segment and Data segment, which is exactly
   the ND-500's two-segment process model.
2. **The spacing is uniform: `40B` = 32 words.** Each table therefore holds **32 entries**, and
   the P table and its D twin are adjacent. A wrong identification would not produce a regular
   grid.
3. **All six `PS*`/`DS*` symbols and `PSPHS`/`DSPHS` are unique** in the symbol table - no
   aliases from other overlays. (Addresses above `177600B` are heavily aliased, which is why
   this evidence would be worth nothing up there; at `175xxx` and `1774xxx` it is clean.)

Contents on disk read as all zeros for all five tables, which is expected and confirms they are
**data**, not code: resident data cells are runtime-only in a carve
(`EXTRACTING-RESIDENT-CODE.md` section 8).

---

## 4. What this does to the swap-file question `[I]`

The standing puzzle was: *why do the swapper's tables say this page lives on the swap file when
nothing appears to have written it there?* Three candidates had already been eliminated -
`DEFINE-SWAP-FILE`/`SWFDE` only opens and closes, the segment's single `WFILE` is
caller-directed, and `LSWPAGE` takes its direction from `XABSFUNC=60B` = read.

`PSPHS[seg]` is a fourth answer with a different shape: **nothing needs to have written the
page, because PLACE writes the ADDRESS.** `SGLOA` records where the segment lives and the
content is paged in on demand - which is what the measured `MON 377B` `LSWPAGE` traffic is.

**This is inference, not proof.** What is proven is the four writes and their targets. What is
NOT proven:

- what `M[B-174]` actually holds when it reaches `PSPHS` (it comes out of the open/interrogate
  chain at `143352`-`143414` and has not been carved);
- what `M[B-75]` contributes through the `ORA`;
- whether the swapper's `RPHS` reads `PSPHS` directly or through a copy;
- whether a zero `PSPHS[seg]` is what the failing `RPHS` actually saw.

The last one is the cheap test and it is now well posed: **`RPHS` traps, `PSPHS` feeds `RPHS`,
so read `PSPHS[seg]` at the moment of the trap.** If it is zero, PLACE never wrote it for that
segment and the fault is a missing write, not a missing page.

---

## 5. Next step

Dump `PSPHS`/`PSLLI`/`PSULI`/`PSMOD` live at the trap. With `P1` now printed by both emulators
the trap already names `RPHS`; adding the table read turns "the swapper read a physical segment
that was not there" into a statement about a specific 32-entry array with a known address.
