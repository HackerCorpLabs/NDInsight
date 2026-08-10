# RECOVER-DOMAIN's worker, the PLACE sequence, and where the segment-load defect is NOT

**Date:** 2026-08-02
**Subject:** `MON-DEBUG:PROG` (ND-500/5000 monitor J04), bank 1
**Depends on:** [`COMMAND-DISPATCH-TABLE-CARVED-2026-08-02.md`](COMMAND-DISPATCH-TABLE-CARVED-2026-08-02.md)
**Status:** `[V]` for the call graph and the negative result; `[I]` where marked.

---

## 1. The result that matters

**The ND-500 monitor program never reads domain segment content, and was never
supposed to.** The routine that performs a segment placement contains **no `MON`
instruction and no `IOX` instruction anywhere in its 445 words**. It issues MON 60
subfunction `006` (ISEGLOAD) once per segment and lets SINTRAN do the fetching.

So the standing observation - *"SINTRAN receives two `006 ISEGLOAD` requests and the
floppy is never bulk-read; placement is requested, segment content is never fetched"* -
**cannot be a monitor-side defect.** It is on the SINTRAN side of MON 60, or in our
emulation of it. This closes off the caller as a suspect.

---

## 2. The call graph, carved

```
RECOVER-DOMAIN  (ordinal 8)
  -> handler        bank1 003577      marshal domain name, JPL I
     -> worker      bank1 030302      calls subfn 142 x3, and 043011
        -> 043011                     routine, calls subfn 130
        -> ...
   PLACE sequence   bank1 043547..044062
        043547  JPL 146737  -> subfn 140
        043552  JPL 146530  -> subfn 055   START-PLACE
        043571  JPL 042115  -> the SEGMENT LOADER        <-- first segment
        044031  JPL 042115  -> the SEGMENT LOADER        <-- second segment
        044062  JPL 146533  -> subfn 056   END-PLACE

   SEGMENT LOADER  bank1 042115..043010   (445 words)
        042230  JPL 146335  -> subfn 006   ISEGLOAD
        042535  JPL 146335  -> subfn 006   ISEGLOAD
```

**This reproduces the live MON 60B trace exactly.** That trace recorded
`055 START-PLACE, 006, 006, 056 END-PLACE` - a correctly bracketed two-segment
placement (`:PSEG` + `:DSEG`). The static call graph produces the same four calls in
the same order, from a single loop over two segments. Request side and code side now
agree, independently.

---

## 3. How the thunk call sites were resolved

MON 60 calls are not `MON` instructions in this program. They are `JPL I <disp>`
through a pointer word into the thunk table at `146310`-`147067` (123 x 3 words):

```
+0   SAA <subfunction code>    (170400 | code)
+1   JMP I 1                   (125001)
+2   146244                    -> the MON 60 gateway
```

`thunkmap.py` walks every `JPL` in bank 1, resolves one level of indirection, and
accepts a target only if **all three** thunk words match - so an address that merely
lands in the range cannot be mistaken for a thunk.

It finds **159 call sites**, which is exactly the count the original analysis arrived
at independently (`nd-500-mon-j04.prog.md`, "159 resolvable call sites"). Two
different methods reaching the same number is the check that makes the map usable.

Subfunction `006` has exactly two call sites in the whole image, `042230` and
`042535`, and both are inside `042115`.

---

## 4. What the loader hands ISEGLOAD

Both call sites marshal an **identical** parameter shape into the callee frame using
the documented `LDX ,B -176` / `STx ,X n` idiom:

```
042515  044616   LDA ,B -162
042516  054602   LDX ,B -176
042517  006006   STA ,X 6      ; param 1 := value of B-162
042520  146135   RADD CLD SB DA
042521  172651   AAA -127
042522  006007   STA ,X 7      ; param 2 := ADDRESS  B-127
042523  146135   RADD CLD SB DA
042524  172623   AAA -155
042525  006010   STA ,X 10     ; param 3 := ADDRESS  B-155
042526  146135   RADD CLD SB DA
042527  172621   AAA -157
042530  006011   STA ,X 11     ; param 4 := ADDRESS  B-157
042531  170411   SAA 11
042532  144151   SWAP CLD SA DD
042533  050643   LDT ,B -135
042534  032012   STF ,X 12     ; param 5 := double built from B-135
042535  135034   JPL I 34      ; -> subfn 006 ISEGLOAD
```

Five parameters. **Three of them (2, 3, 4) are the ADDRESSES of caller locals** - they
are output slots for SINTRAN to write back into, computed as `B + offset` by the
`RADD CLD SB DA` / `AAA -n` pair.

The `RADD CLD SB DA` / `AAA -n` / `STA ,X n` idiom is how this program passes an
address rather than a value; `LDA ,B -n` / `STA ,X n` passes a value. That is the only
distinction between parameter 1 and parameters 2-4 here.

**Note the shape.** A call that passes the addresses of caller locals for the callee to
fill in, where the write-back is then empty, is the same shape as the swapper's 7-arg
MON 377B (`LSWPAGE`) already recorded in
`nd500-swapfile-required-then-5swap-trap`. Whether that is a real connection or a
coincidence of a common calling convention is **`[OPEN]`** - it is flagged here only so
the parallel is not discovered twice.

Difference between the two sites: the first is preceded by
`LDT ,B -154 / BSET ZRO 160 DT / STT ,B -154`, clearing bit 14 of a flag word at
`B-154`; the second is not. `[I]` that this selects which of the two segments is being
placed - the parameter marshalling is otherwise byte-identical, so *something* must
distinguish them, but the flag's meaning is not carved.

---

## 5. Where the defect must be, and what is already known there

The SINTRAN-side worker is carved at
`tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/60B-N500M/60B-006B-ISEGLOAD/`.
Its recorded contract:

1. `A:=5P1; T:=200; CALL FRUSMOVE` - copy the segment **name** (param 1, <= 200B bytes)
   from the user into the MON 60 buffer.
2. If `5D51 <> 0`, copy a shared-part info block from param 5.
3. **`GO FAR 5NOPAR` - "common path performs the place".**

Two things follow:

- Param 1 being a name is consistent with what the loader passes (a value that would be
  a pointer to the name string). `[I]` - the loader's `B-162` was not traced back to a
  name buffer.
- **Step 3 is where the actual fetch happens, and step 3 is not carved.** That folder's
  own status line says "L07 body byte-location pending (bank-2 5IFUNC)". The handler
  itself only copies a name; it cannot be what reads a segment off the disc.

**So the next carve target is `5NOPAR`, the MON 60 common place path** - not
RECOVER-DOMAIN, not the loader, and not ISEGLOAD's own handler body.

---

## 6. What this does NOT establish

- **The worker at `030302` is only partly read.** Its three calls to subfunction `142`
  and its call to `043011` (which issues subfunction `130`) were identified, but neither
  routine was carved. `142` and `130` are named only by the existing name-based
  subfunction correspondence, which section 14 of the main analysis still marks as not
  code-proven.
- **The route from `030302` to the PLACE sequence at `043547` was not traced.** The
  PLACE sequence and the loader are confirmed by call-site identity and by matching the
  live trace, not by walking the control flow from the worker into them. A different
  command could in principle drive the same PLACE code.
- **The 159-site map names subfunctions from the thunk's `SAA` code**, which is exact.
  What each code *means* still comes from the name-based table, not from the SINTRAN
  worker bodies.

---

## 7. Reproducing it

```powershell
cd E:\Dev\Ronny\NDInsight\SINTRAN\ND500\nd-500-mon
wsl python3 <tools>/thunkmap.py nd-500-mon-j04-bank1.bin
wsl python3 <tools>/resolve.py  nd-500-mon-j04-bank1.bin nd-500-mon-j04-bank2.bin 42115 460
```

`resolve.py` exists because the disassembly's `-> 042324` annotation on a `JPL I` names
the **pointer word**, not the routine. Disassembling the pointer as code yields
plausible nonsense - the trap the carving skill records as "indirect jumps land on
POINTER words, not code". Both scripts live in `tools/sintran-segment-carver/`.

One gotcha worth recording: `JPL` is opcode `0o134`, not `0o130` (`0o130` is the
conditional-jump group). A resolver with that wrong silently resolves **nothing at
all** and reads as "this routine makes no calls", which is a false negative that looks
exactly like a finding.
