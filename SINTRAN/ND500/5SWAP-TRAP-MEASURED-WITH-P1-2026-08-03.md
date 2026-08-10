# The 5SWAP trap, measured with P1 - it is NOT the RPHS

**Date:** 2026-08-03
**Method:** `Nd500SwapFile_CreateAndDefine_Capture` with `RETROCORE_NLL_FLOPPY=1`,
`RETROCORE_ND5000_RUNTHREAD=1`, `RETROCORE_HARNESS_TIMEOUT_SCALE=5`. 24 minutes, trap reproduced.
**Supersedes:** the "faulting instruction = `RPHS @1000010525`" conclusion in
`TRAP-PRINTER-HUNT-2026-08-03.md` and in the memory notes.

---

## 0. The measurement

```
          ND-500(0) Trap
          Protect violation
PROTECT VIOLATION
At program address:        1     10533B

[after-recover-domain] ... trapsPosted=1 lastTRAPN=44B lastTrapP=0x0800115B
lastTrapAddr=0x00000004 cpuP1=0x0800115B ... PS=3 CED=0 CAD=0 THA=0x00000000
swpfu[LNEWSWAP:7 LSWPAGE:1] restarts=8/8

lastProtectViolation=MMU read protection violation at 0x00000004
  P1=0x0800115B <- failing instruction (P/regs.PC=0x0800115E)
  CED=0x00000000 CAD=0x00000000
  reason=capability is ZERO - segment 0 is not in domain 0 (data capability table, CED=0, PS=3)
  operand: mode=LOCAL_SHORT reg=0 disp=20 B=0x08024420 R=0x28010828
           I1=0x00000080 I2=0x08024364 I3=0x00000000 I4=0x08029074 -> ea=0x08024434
```

---

## 1. What is now MEASURED rather than inferred `[V]`

| Fact | Value |
|---|---|
| Trap class | **PROTECT VIOLATION**, TRAPN `44B` - not a page fault |
| Faulting address | **`0x00000004`** - segment 0, offset 4 |
| Why it faulted | **the capability for segment 0 is ZERO in domain 0** (data capability table, CED=0, PS=3) |
| Trapping P (`P1`) | `0x0800115B` = **`0o1000010533`** |
| Restart P (`regs.PC`) | `0x0800115E` = `0o1000010536` |
| Faulting operand | `LOCAL_SHORT`, `B=0x08024420`, disp 20 -> `ea=0x08024434` |

---

## 2. THE RPHS THEORY IS DEAD `[V]`

The standing conclusion was: the reported address `1 10533B` is `P`, `P` runs ahead, and the real
faulting instruction is the `RPHS` at `0o1000010525`, six bytes earlier.

**Three independent parts of the measurement contradict that:**

1. **`cpuP1 = 0x0800115B` = `0o1000010533`** - the trapping P IS the reported address. There is
   no six-byte gap. Whatever the report shows, it is not "six bytes past the fault".
2. **The fault is a capability failure on segment 0, reading address 4.** `RPHS` resolves its
   source through the physical segment table rooted at `I4`; it does not consult a domain
   capability table at all. A "segment 0 is not in domain 0" reason cannot come from an `RPHS`
   source read.
3. **The operand is `LOCAL_SHORT` off `B`** - an ordinary local-variable access
   (`B=0x08024420`, disp 20). `RPHS` takes one operand, the domain number, and carries everything
   else in `I1`-`I4`.

**And the decisive one: fixing `RPHS` did not change the trap.** `RPHS`/`WPHS` were dead in both
emulators until today (RetroCore `f0cf3a436`); with them correctly implemented the trap
reproduces identically, same address, same class. If `RPHS` had been the faulting instruction,
implementing it would have changed something.

Fixing `RPHS` was still correct - it could never execute, which is a real defect - but **it was
not this defect.**

---

## 3. What the trap actually is `[V for the facts, I for the story]`

**A read of address `0x00000004` - segment 0, offset 4 - by a domain that holds no capability
for segment 0.** That is the shape of a **null-pointer dereference**: a pointer that should have
been filled in is zero (or near zero), and the code follows it.

Supporting, and unexplained, from the same line: `I3 = 0x00000000` while `I4 = 0x08029074`. If
those are the `RPHS` index registers left set up by the nearby code, then `I4` is **not a valid
physical segment number** - segment numbers are 0-31 and this is a full address - and `I3`, the
offset on the physical segment, is zero. Marked inference: `I1`-`I4` are general index registers
and other instructions use them too.

---

## 4. The instruction-boundary puzzle is still open `[OPEN]`

`swapper-k01-pseg.asm` frames this region as:

```
1000010525: 377 365 304    rphs  $1777777777777777777704
1000010530: 010 001        h1 := $1
1000010532: 115 054        w set1 $54
1000010534: 300 057        go    $57
1000010536: 030 102        r:=   b.10
```

By that framing there is **no instruction at `10533`** - it is inside the two-byte instruction at
`10532`. But the emulator decoded an instruction AT `10533` that is three bytes long, landing on
`10536`, which the disassembly also calls a boundary. Both framings reconcile at `10536`; they
differ by one byte at `10532`.

One of the two is wrong and it matters, because the identity of the faulting instruction depends
on it. Two reasons to suspect the **disassembly**:

- `rphs $1777777777777777777704` is an absurd operand - all ones. `RPHS` takes **one** operand
  (`ND-05.009.4` 16.31). A disassembler that assumed more would over-consume bytes and mis-frame
  everything after it - **which is exactly the assumption both emulators made until today.**
- There is already a known length defect in this disassembly:
  `DISASSEMBLY-DEFECT-LOOPI-LENGTH-2026-07-28.md`.

**Next step:** re-disassemble this region with the corrected `RPHS` operand count and see whether
the framing shifts to put a real instruction at `10533`. If it does, the emulator is right and
the asm needs regenerating; the "does not land on an instruction boundary" observation that
started the whole `P`-vs-`P1` hunt would then be an artefact of the same wrong assumption.

---

## 5. What survives from the earlier work

- **`P1` itself is real and useful.** `ND-05.017.01` ch.6 STEP 2 documents both registers, and
  having `P1` printed is what made this measurement a one-line read instead of a reconstruction.
- **`P` and `P1` do differ** - measured here as `0o10536` vs `0o10533`.
- **What does NOT survive:** the claim that the SINTRAN trap report shows `P` and therefore runs
  ahead of the fault. In our emulator the report shows **`P1`**, because the servicer posts the
  trapping P into the mailbox (`lastTrapP` = `cpuP1` = `0x0800115B`). Whatever real hardware
  does, for our traces **the reported address IS the trapping instruction**.

---

## 5-RETRACTION. `b.24` IS **`0x0A`**, A VALID SEGMENT NUMBER - MY "GARBAGE I4" CLAIM WAS WRONG

**Measured directly with the frame probe** (`ND500_FRAMEPROBE`, added 2026-08-03). At the `rphs`
itself:

```
[FRAMEPROBE] PC=0x08001155 (octal 1000010525) B=0x08024420 R=0x28010828
  I1=0x00000080  I2=0x08024364  I3=0x00000000  I4=0x0000000A
                                                  ^^^^^^^^^^ TEN. A valid physical segment number.
```

Sections 5a and 5b below claimed `I4 = 0x08029074`, "an address where a 0-31 segment number
belongs", and built a lead on it. **That was wrong.** The value came from the
`lastProtectViolation` string, whose `I1`-`I4` are captured by the **last operand decode**, not
by the faulting instruction - so they belonged to some other instruction entirely. I read a
register snapshot as if it were the trap's state.

The frame words confirm it from the other side: `b.24` = `B + 0o24` = `0x08024434`, and that word
reads `0x0000000A`.

**So the swapper asks for physical segment 10, offset 0, 128 bytes, into domain address
`0x08024364`. Nothing about that is obviously garbage.** The one remaining oddity is
`b.30 = 0` (hence `I3 = 0`), and an offset of zero may be perfectly legitimate.

### What this cost, and the rule that would have prevented it

A diagnostic string that bundles several registers is not a snapshot of any one instruction
unless it says so. `LastOperandAddressing` is refreshed on every operand decode; embedding it in
a protect-violation message makes it *look* like the fault's own registers. **Read the values at
the instruction, not from a message assembled elsewhere.**

---

## 5-ANSWER. PARAMETER PASSING IS SETTLED - THE CALLER'S RECORD **IS** THE CALLEE'S FRAME `[V]`

Section 5b said explicitly that the relation between the caller's `r.24` and the callee's `b.24`
was NOT established and must not be guessed. The probe measures it:

```
[FRAMEPROBE] PC=0x080013C1  (caller, before the call)   B=0x08024330   b.10 = 0x08024420
[FRAMEPROBE] PC=0x0800111A  (callee entry, after ents)  B=0x08024330
[FRAMEPROBE] PC=0x08001153  (callee, at w4 := b.24)     B=0x08024420
```

**The record pointer the caller builds through `b.10` (`0x08024420`) becomes the callee's `B`.**
So the caller's `r.NN` and the callee's `b.NN` are the *same storage*, and the argument record is
simply the new frame.

Cross-check on the value: the caller writes `r.24` at `0x08024434`; at the caller probe that word
holds `0x0000000A`, and the callee reads `b.24` from `0x08024434` and gets `0x0000000A`. Same
address, same value, no mangling.

**Consequence: `CALL`/`ENTS` parameter passing is NOT the defect**, and the "is the value already
wrong at the caller, or corrupted crossing the call" fork is closed - it is neither, because the
value is not wrong.

---

## 5a. THE `RPHS` REGISTER SETUP IS CONFIRMED FROM THE SWAPPER'S OWN CODE `[V]`

**(The register ROLES below stand - `w1`-`w4` really are `I1`-`I4`, and `I1 = 0o200 = 0x80`
matched exactly. Only the `I4 = 0x08029074` value and everything inferred from it are retracted;
see the retraction above.)**

The block immediately before the `rphs` loads exactly the four registers `ND-05.009.4` 16.31
names, and the **measured values match the code that sets them**:

```
1000010507: w3 :=  b.30            \
1000010511: w3 *   $400            /  I3 = b.30 * 0o400   -> MEASURED I3 = 0x00000000
1000010515: w1 :=  $200               I1 = 0o200 = 0x80    -> MEASURED I1 = 0x00000080   EXACT
1000010521: w2 :=  b.34               I2 = domain address  -> MEASURED I2 = 0x08024364
1000010523: w4 :=  b.24               I4 = segment number  -> MEASURED I4 = 0x08029074
1000010525: rphs   ...
```

`w1`-`w4` ARE `I1`-`I4`. `I1 = 0o200 = 128 = 0x80` matches the measured value **exactly**, which
is independent confirmation of the register roles the manual gives - and confirmation that the
new `RPHS` implementation reads the right registers.

### Two anomalies in what the swapper loads `[V for the values, I for the meaning]`

1. **`I4 = 0x08029074` is not a physical segment number.** Physical segment numbers are 0-31
   (`MAX_PST`); this is a full virtual address. `b.24` holds an ADDRESS where a segment number
   is expected.
2. **`I3 = 0`**, because `b.30` is zero and `0 * 0o400 = 0`.

So the swapper reaches its swap-in read with a **garbage segment number and a zero offset**.
That fits the long-standing shape of this defect - the swapper's descriptor fields were never
filled in - and it is now attached to two specific named locals, `b.24` and `b.30`, in a routine
whose entry (`1000010432`, `ents $504`) is already proven by two independent call sites.

**This is the strongest lead in the file.** It does not say the `rphs` faulted - section 2 rules
that out - but it says the data feeding it is wrong, which is a defect regardless of which
instruction trips first.

---

## 5b. `b.24` and `b.30` are INPUTS, and both callers build the same record `[V]`

**They are never loaded in the routine body.** Between `ents $504` at `1000010432` and the
`rphs` at `1000010525` there is no write to `b.24` or `b.30` - only reads:

```
1000010440  w1 := b.30          (then /8 -> b.50)
1000010507  w3 := b.30          (then *0o400 -> I3)
1000010523  w4 := b.24          (-> I4)
```

So both are **incoming parameters**. `b.24` is the value that ends up as the physical segment
number, and it measured `0x08029074`.

### Both call sites build an identical five-field record

`1000011656` and `1000063023`, the only two callers:

```
r := b.10
    r.24 := <value>              caller 1: b.30       caller 2: a masked field (and $377)
    r.30 := <value>              caller 1: b.264      caller 2: b.360
    r.34 := laddr <local>        caller 1: b.64       caller 2: b.142    <- an ADDRESS
    r.40 := 0
    r.44 := 0o77                 same constant in both
call $1000010432
```

Three things worth noting:

- **`r.34` is the ADDRESS of a caller local** (`laddr`), i.e. a **write-back slot** - the same
  shape as the ISEGLOAD and 7-arg MON 377B calls that also pass addresses for SINTRAN to fill
  in, and whose write-back measured empty.
- **`r.44 = 0o77` in both** - a constant, so most likely an operation or function code.
- **`r.40 := 0`** in both.

Inside the routine, `1000010462 call $1000001422` is made with `r.24` and `r.30` populated, and
its result is tested (`w test r1`, `if = go $120`) before the code proceeds to the `rphs`. So
that inner call is the natural place for a field to be filled in - or not.

### The honest boundary

**How the record fields relate to `b.24`/`b.30` is NOT established.** The callers write `r.24`
and the callee reads `b.24`, and I have not proven they are the same storage. ND-500 parameter
passing for `ents` frames has not been carved here, and guessing it is exactly how earlier
sessions went wrong. What is proven: `b.24`/`b.30` are inputs, both callers build the same
record, and `b.24` arrives holding an address where a 0-31 segment number belongs.

---

## 6. Where to look next

The question is no longer "which instruction faulted" but **"why does domain 0 have a zero
capability for segment 0, and who wrote the pointer that led there"**:

1. ~~Find what fills `b.24` and `b.30`.~~ **DONE and it is not the defect** - `b.24 = 0x0A`, a
   valid segment number, and the caller's record IS the callee's frame. See the retraction and
   the parameter-passing answer above. The only survivor is `b.30 = 0` giving `I3 = 0`, and a
   zero offset may well be correct.
1b. **The open question is now: what reads address `0x00000004`?** The registers going into the
   `rphs` are sane, so the fault is elsewhere. Probe the instruction at `0x0800115B` itself and
   read the values it uses, rather than trusting any bundled diagnostic string.
2. A zero capability for **segment 0** is almost certainly CORRECT - segment 0 offset 4 is a null
   pointer and nothing legitimately reads it. So the bug is the pointer, not the capability
   table. Worth one confirming dump, not an investigation.
3. `swpfu[LNEWSWAP:7 LSWPAGE:1]` - exactly one `LSWPAGE` was processed before the trap. That is
   the swap-page path running once and then the domain faulting. Worth checking whether the page
   it delivered is what the null pointer should have pointed at.
