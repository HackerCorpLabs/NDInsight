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

`PSPHS` reads as *PHysical Start*, and **`RPHS`** is *Read from PHysical Segment* - the
instruction the 5SWAP trap fires on (`RPHS @1000010525`, see `TRAP-PRINTER-HUNT-2026-08-03.md`).

**Do NOT read that as "PSPHS is the table RPHS reads" - it is not established, and the two live
on different sides of the machine.** `RPHS` runs on the ND-500 and resolves its address through
"the physical segment table pointer" (`ND-05.009.4` 16.31), which is the ND-500's own `PSTP` /
PST hardware structure. `PSPHS` is an **ND-100-side** table inside segment `030-S3SM5`, written
by SINTRAN. Whether SINTRAN builds the ND-500 PST from `PSPHS`, or the two are merely parallel
bookkeeping, was **UNPROVEN** when this was written. **SETTLED 2026-08-11: parallel
bookkeeping — the PST entries are swapper-maintained and the base is a SINTRAN allocation
handed over via control-store cell `0o21`; see
`CARVE-ANSWER-PST-WRITER-VS-PSPHS-2026-08-11.md`.** See section 4 for what this doc itself
established.

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

## 5. The tables are used everywhere - and three routines touch the WHOLE family `[V]`

Scanning the segment for words equal to any of the five table addresses gives **62 pointer-pool
references**. These are not a corner of the segment; they are core bookkeeping.

Most references are to a single table. **Four sites name the whole family in one pool**, which
is the signature of a routine that reads or writes a complete segment descriptor:

| Pool at | Contents |
|---|---|
| `054151`-`054156` | PSMOD, PSPHS, PSLLI, PSULI (+ DSPHS at `054020`) |
| `072025`-`072033` | PSPHS, PSMOD, PSLLI, PSULI, DSPHS |
| `134514`-`134522` | PSPHS, DSPHS, PSLLI, PSULI |
| `143537`-`143544` | PSPHS, PSLLI, PSULI, PSMOD, DSPHS - **this is `SGLOA`, the writer** |

So one writer is known and **three whole-family consumers are not yet carved**. Those three are
the highest-value next targets: whatever reads a complete descriptor is what the descriptor is
FOR, and one of them is where the "does SINTRAN build the ND-500 PST from this" question gets
answered.

A caution against a tempting shortcut: the `FUNCS` vocabulary contains `RPHSG` and `WPHSG` -
read/write physical segment at the ND-100 monitor level. It is tempting to assume `PSPHS` exists
to serve those, and equally tempting to assume it serves the ND-500's `RPHS` instruction. Both
are guesses until one of the three pools above is read.

---

## 5a. The first consumer read: `054040` is an ADDRESS TRANSLATOR `[V for the mechanics]`

The `054151` pool belongs to a routine running from about `054040`. It reads all four tables
through the same post-indexed pattern `SGLOA` writes them with:

```
054051  LDA I ,X 100   -> M[054151] = PSMOD   ->  A := PSMOD[X]
054055  LDA I ,X 76    -> M[054153] = PSPHS   ->  A := PSPHS[X]
054056  AND 76         -> M[054154] = 007777  ->  mask to 12 bits
054057  JAZ 62         -> zero page number: bail to 054141
054060  LDT I ,X 75    -> M[054155] = PSLLI   ->  T := PSLLI[X]
054061  SKP IF DD MGRE ST                        bound check against the LOWER limit
054063  LDT I ,X 73    -> M[054156] = PSULI   ->  T := PSULI[X]
054064  SKP IF DT MGRE SD                        bound check against the UPPER limit
054065  JMP 54         -> out of range: bail to 054141
054066  RADD CLD SX DT ->  T := X
054067  SHT 13         ->  T := T << 11          page number -> BYTE address (2048-byte pages)
```

Three things this settles:

1. **`PSPHS` entries carry a PAGE NUMBER in the low 12 bits** - the `AND 007777` says so, and the
   `SHT 13` (shift left 11) converts a page number to a byte address at the 2048-byte page size
   used everywhere else in this machine.
2. **`PSLLI` / `PSULI` really are limits** - they are used for exactly one thing here, a
   two-sided bound check, which is what `SGLOA` writing 0 and size-1 into them predicted.
3. **A zero `PSPHS` entry is a recognised failure case**, not an address - `JAZ` jumps out.
   That is the shape of "this segment is not placed", and it is what the cheap test in section 4
   would be looking for.

**What this does NOT settle:** the routine's name and its callers. The pointer scan for entries
into `054000`-`054050` returns twenty-plus candidates and most are certainly noise (small
addresses alias with ordinary data). Until a caller is identified, "this is what `RPHSG`/`WPHSG`
use" remains a guess - a well-shaped one, since a monitor-level physical-segment read needs
precisely this translation, but a guess.

It does, however, weigh **against** the ND-500-PST reading: this is the ND-100 translating a
physical-segment address **in software, for its own use**. Nothing here writes a hardware table.

---

## 5b. All four whole-family sites are now read - place, unplace, copy, translate `[V]`

| Site | Role | What it does with the tables |
|---|---|---|
| `143440` | **PLACE** (`SGLOA`) | writes PSPHS, PSLLI:=0, PSULI:=size-1, PSMOD |
| `071720` | **PLACE + UNPLACE** | writes the same four, and **clears `PSPHS[X] := 0`** at `072007` |
| `134430` | **COPY** | reads PSLLI/PSULI at one index, writes them at another |
| `054040` | **TRANSLATE** | reads all four, bound-checks, page number -> byte address |

### `071720` is a second writer, and it has the release path

Same four tables, same index register idiom, and the PSLLI/PSULI logic is **identical** to
`SGLOA`'s down to the instruction sequence:

```
071723  LDX ,B -75
071724  STA I ,X 101   -> M[072025] = PSPHS   PSPHS[X] := (X with bit 13 set) | M[B-200]
071732  STA I ,X 74    -> M[072026] = PSMOD   PSMOD[X] := 0/1, from bit 15 this time
071742  STZ I ,X 65    -> M[072027] = PSLLI   PSLLI[X] := 0
071743  STA I ,X 65    -> M[072030] = PSULI   PSULI[X] := size-1
...
072007  STZ I ,X 16    -> M[072025] = PSPHS   PSPHS[X] := 0     <- the RELEASE
```

Note `PSMOD` is taken from **bit 15** here and from **bit 9** in `SGLOA` - different source
words, same 0/1 result.

### The three-way agreement that settles what a zero entry means `[V]`

- `SGLOA` and `071720` **write** `PSPHS[seg]` when a segment is placed.
- `072007` **clears** `PSPHS[seg]` to zero.
- `054040` **bails out** (`JAZ`) when `PSPHS[seg]` reads zero.

Three independent code paths, one meaning: **`PSPHS[seg] == 0` is "this segment is not
placed".** It is not an address, and it is not a default that happens to work.

That makes the live test in section 4 decisive rather than suggestive: read `PSPHS[seg]` at the
trap, and zero is a definite answer, not an ambiguous one.

### `134430` copies a descriptor between two slots

```
134454  LDA I ,X 45  -> PSLLI[X1]     X1 from B-200
134456  STA I ,X 43  -> PSLLI[X2]     X2 from B-177
134460  LDA I ,X 42  -> PSULI[X1]
134462  STA I ,X 40  -> PSULI[X2]
```

A descriptor duplicated from one physical segment number to another - the shape of creating a
process or domain from an existing one.

### And a negative worth stating plainly

**None of the four sites writes anything toward the ND-500's hardware PST.** The harness shows
SINTRAN does set `PSTP = 0x0003A000`, so a hardware table certainly exists and is populated -
but not from here. Every operation on `PSPHS`/`PSLLI`/`PSULI`/`PSMOD` in this segment is
ND-100-side bookkeeping.

This is four-for-four and it is the strongest evidence yet **against** "PSPHS is the table RPHS
reads". It is still an absence rather than a proof: whatever writes the PST at `0x3A000` has not
been found, and until it is, the possibility that it reads `PSPHS` cannot be fully excluded.
**Finding the PSTP writer is the single question that closes this.**

---

## 5c. THE `PSTP` WRITER - found, and it closes the question `[V]`

**Nothing writes `PSTP` as a register.** It is derived at **microprogram start** from a
**control-store cell**:

```
cell 0o21 (PSTBASE)  ->  page number  ->  PSTP = page << 11   (byte address)
cell 0o20 (OFFSET)   ->  context (register) block base, already a byte address
```

`ND-05.020.01` section 6.6 states the register's own semantics outright:

> "**PSTP - 0 - Physical Segment Table Pointer - 30 bits - Read/Write.** This register, shifted
> 11 places to the left, points to the start of the Physical Segment Table."

The control store itself is filled by SINTRAN over the octobus with **LOCSM / `CMWWC` (`023B`)**,
which DMAs microwords from MPM into the store. So the value in cell `0o21` is whatever SINTRAN
put there.

### It is PATCHED by SINTRAN, not shipped `[V]`

The shipped ND-5800 microcode (`MICRO-5800-B30.DATA`, 262144 bytes = 16384 microwords) carries:

```
cell 0x10 (0o20, OFFSET)  LARG = 0x00000000
cell 0x11 (0o21, PSTBASE) LARG = 0x00000002
```

The live machine runs `PSTP = 0x0003A000`, i.e. page `0x74`. **`0x74` is not `2`**, so something
overwrote the shipped value between load and start.

That conclusion does not rest on the file alone. **A physical segment table base is a runtime
allocation** - where SINTRAN puts the PST depends on the machine's memory configuration - so it
*cannot* be a constant baked into a shipped microcode image. The file check simply confirms what
the reasoning already requires.

### Consequence: the `PSPHS` question is CLOSED

The ND-500's physical segment table base comes from a **control-store cell that SINTRAN
patches**, not from `PSPHS`. Combined with 5b - four `PSPHS` sites, none writing toward the
ND-500 - the earlier caution can now be stated positively:

**`PSPHS`/`PSLLI`/`PSULI`/`PSMOD` are the ND-100's own segment bookkeeping. They are not the
source of the ND-500 hardware PST, and `PSPHS` is not "the table `RPHS` reads".**

Still not carved: the exact ND-100 instructions that patch cell `0o21`. That is a smaller
question than it was - the mechanism, the cell, the transport and the proof that a patch happens
are all in hand.

---

## 5d. FLAGGED - our `PSTP` holds BYTES where the hardware register holds PAGES

`ND-05.020.01` 6.6 is explicit that the register is shifted 11 places left to give the table
start, so the hardware `PSTP` contains a **page number**. RetroCore stores the **byte address**:
`OctobusND5000Station` computes `pstpBytes = pstBasePage << 11` and writes that into
`regs.PSTP`, and the MMU then uses `PSTP + index*4` directly as a physical address.

Internally consistent, so the walk is correct. But `ReadRegister(PSTP)` returns the same byte
value, so **a guest reading `PSTP` back gets 2048x the hardware value** - `0x3A000` where real
hardware would report `0x74`. Reachable through the ND-5000 monitor's own documented commands
(`LOOK-AT-HARDWARE A,IMM,PSTP`, and the `MEMORY-CONFIGURATION` display quoted in the same
manual).

Not fixed here: it is a separate change with its own blast radius, and no guest read of `PSTP`
has been observed yet. Recorded so it is not rediscovered as a mystery.

---

## 6. Next step

1. ~~Carve the three whole-family consumers.~~ **DONE - see 5a and 5b.**
2. ~~Find what writes `PSTP`.~~ **DONE - see 5c. It is control-store cell `0o21`, patched by
   SINTRAN. The `PSPHS`-vs-PST question is closed.**
3. Dump `PSPHS[seg]` live at the trap. Now decisive rather than suggestive: three code paths
   agree that zero means "not placed", so a zero reading is a definite answer.
4. Optional, small: carve the ND-100 instructions that patch cell `0o21`, to name the routine.
5. Decide what to do about 5d (`PSTP` byte-vs-page), if a guest is ever seen reading it back.
