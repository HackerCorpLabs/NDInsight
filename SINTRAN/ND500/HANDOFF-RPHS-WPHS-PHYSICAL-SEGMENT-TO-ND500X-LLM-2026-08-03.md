# Handoff to the nd500x LLM: RPHS / WPHS are wrong in the C port too

**Date:** 2026-08-03
**From:** the RetroCore / SINTRAN carving thread
**To:** whoever owns `~/repos/nd500x`
**Subject:** `RPHS` and `WPHS` can never execute in either emulator. Fixed in C#; the C port
needs the same fix, plus one new MMU entry point.

---

## 0. Why you should care before reading further

`RPHS` is the instruction the SINTRAN ND-500 swapper faults on. The 5SWAP fatal trap is at
`RPHS @1000010525`. **In nd500x that instruction cannot run at all** - it raises
`trap_illegal_operand` on every execution, unconditionally, because of an operand-count guard
that can never be satisfied. So at least part of the swapper trap we have been chasing is our
own bug, not SINTRAN's.

This is a case where the usual rule was inverted: normally the C port is ahead of C# on MMU
matters and you read the C first. **Not here.** Both ports carry the identical defect (the C
one is a comment-for-comment port of the C# one, including the wrong parts), and the fix has
now been made in C# first.

---

## 1. What the manual actually says

`ND-05.009.4 EN ND-500 Reference Manual`, sections **16.31** (RPHS) and **16.32** (WPHS). Both
entries are short and unambiguous. Quoted, not paraphrased:

### 16.31 RPHS - Read from physical segment ('87 extension)

**Format:** `RPHS <domain number/r/W>`   <- **ONE operand**

| Assembly | Name | Hex | Octal |
|---|---|---|---|
| RPHS | read from physical segment | FFF5H | 177765B |

**Operation:**

```
while I1 > 0 do
  S([I4,I3) -> D(<domain number>.I2)
  I3 + 1 -> I3
  I2 + 1 -> I2
  I1 - 1 -> I1
enddo
```

- `I1` : Number of bytes to be moved.
- `I2` : Logical address on the domain.
- `I3` : Address on the physical segment.
- `I4` : Physical segment number.
- Operand : domain number.

> "The copy operation is continued until the number of bytes left is equal to 0 (I1 = 0) **or a
> page boundary is reached on the physical segment**. Number of bytes to be moved is counted
> down and will be zero when the move operation is completed. The addresses are incremented
> during the copy operation."

> "The instruction is meant for use when copying data from a physical segment in one process to
> a domain in another. **The physical segment number is used together with the physical segment
> table pointer to find the physical page number** of the wanted data page or of the
> corresponding index page."

**Data Status Bits:**

```
no bytes left = 0                    : 1 -> Z
page boundary and no bytes left < 0  : 0 -> Z
```

### 16.32 WPHS - Write to physical segment ('87 extension)

Same instruction with the direction reversed - `FFF4H` / `177764B`:

```
while I1 > 0 do
  S(<domain number>.I2) -> D(I4.I3)
  ...
```

Same register roles, same page-boundary stop, same Z rule.

---

## 2. The five defects, all present in `src/cpu/instructions/SYSTEM/Rphs.c` and `Wphs.c`

| # | Defect | Consequence |
|---|---|---|
| 1 | **Requires 3 operands** (`if (fi->operand_count != 3)`) while the decoder emits **1** | The guard fires on EVERY execution. In C this calls `trap_illegal_operand` - the instruction is not merely broken, it traps. |
| 2 | Source taken from `operands[0]`; `physical_segment` read from `operands[1]` (commented out in C) | The real source is `[I4,I3)`. `I3` and `I4` are never read. |
| 3 | No physical-segment translation at all | Both do a plain logical memory-to-memory copy in the current domain. The manual says the address must be resolved through the physical segment table. |
| 4 | No page-boundary stop | The manual makes this a PARTIAL move that the caller loops on. Copying the whole count diverges the moment a transfer crosses a page. |
| 5 | `I1 = 0` and `Z = 1` set unconditionally; `I3` never updated | A caller looping on `I1`/`Z` cannot resume, and cannot tell a completed move from a truncated one. |

Confirm defect 1 for yourself before doing anything else - it is one line:

```
$ grep -n '0xFFF4\|0xFFF5' ~/repos/nd500x/src/cpu/nd500_instructions.c
1071:  { 0xFFF4, "wphs", 1, ... }      <- operand count 1
1072:  { 0xFFF5, "rphs", 1, ... }      <- operand count 1
$ grep -n 'operand_count != 3' ~/repos/nd500x/src/cpu/instructions/SYSTEM/{Rphs,Wphs}.c
```

---

## 3. What was done on the C# side (mirror this)

Commit: see `Emulated.HW/ND/CPU/ND500/` in RetroCore, 2026-08-03.

### 3a. New MMU entry point - this is the part that needs real thought

The existing walk resolves a VIRTUAL address: it takes the top 5 bits as a logical segment,
looks up a capability for the CURRENT domain, extracts a PSN from that capability, and only
then walks the PST. `RPHS`/`WPHS` are handed the PSN outright in `I4`, so they must **skip the
capability lookup entirely** and enter the walk at the PST.

In C# the PSN-rooted half (old STEP 6 through STEP 9: PST entry -> index mode -> page tables ->
physical address) was **factored out** of `TranslateVirtualAddress` into:

```csharp
private uint TranslateThroughPst(int psn, int L1_index, int L2_index, int offset,
                                 bool isWrite, bool isInstruction,
                                 uint reportAddress, int reportSegment, ushort reportCapability)
```

`TranslateVirtualAddress` now ends with a call to it, so there is exactly one copy of the walk.
The public entry point for the two instructions is:

```csharp
public uint TranslatePhysicalSegmentAddress(uint psn, uint segmentRelativeAddress, bool isWrite)
```

which decomposes the segment-relative address into L1 / L2 / offset exactly as the virtual path
does (only the 5 segment-select bits are absent, because the segment is given rather than
selected) and calls `TranslateThroughPst`.

**Do the same refactor in `src/cpu/nd500_mmu.c` - do not copy the walk.** A second copy of the
PST walk will drift from the first, and this walk has already been wrong four separate times
(PTE bit positions, zero-PST handling, `ReadPhysical32` vs bus reads, CED vs CAD).

The `reportSegment` / `reportCapability` arguments exist only so fault messages can say
"no logical segment / no capability was involved" (`-1` and `0`) rather than printing values
that would read as a capability walk that never happened. Keep that, or the first PGF from a
physical-segment access will send someone hunting a capability that was never consulted.

### 3b. One documented ASSUMPTION you must decide on too

With the data MMU disabled, `TranslatePhysicalSegmentAddress` returns the segment-relative
address unchanged, mirroring what `TranslateVirtualAddress` does in the same situation.

**This is our convention, not ND's.** The manuals describe `RPHS`/`WPHS` only in terms of the
physical segment table and say nothing about paging-off behaviour. The alternative - walking an
unconfigured PST - faults on a zero PSTP, which is certainly wrong. Mark it the same way in C so
the two ports do not silently diverge on it.

### 3c. The instruction bodies

```
operand_count must be 1        -> otherwise illegal instruction
privileged check               -> unchanged
domain   = operand 0
byte_count      = I1
domain_address  = I2
segment_offset  = I3
physical_segment= I4

moved = 0
while (byte_count > 0) {
    if (moved > 0 && (segment_offset & 0x7FF) == 0) break;   /* page boundary on the PHYSICAL segment */
    ... translate segment_offset through the PST rooted at physical_segment ...
    ... copy one byte (RPHS: segment -> domain;  WPHS: domain -> segment) ...
    segment_offset++; domain_address++; byte_count--; moved++;
}

I1 = byte_count;                 /* NOT 0 */
I2 = domain_address;
I3 = segment_offset;             /* was never updated before */
Z  = (byte_count == 0);          /* NOT unconditionally 1 */
```

Notes that matter:

- The boundary test is `moved > 0 && ...` so that an `I3` which already sits exactly on a
  boundary still transfers its page instead of returning zero bytes forever.
- Page size is 2048 bytes (`PGSHIFT = 11`), so the boundary mask is `0x7FF`.
- Abort after every translate and every access - a faulting byte must not be followed by more
  copying, and must not raise a second trap on top of the first. The C port already has the
  right idiom (`nd500_trap_occurred() || cpu->instr_aborted`); keep using it inside the loop.

### 3d. KNOWN GAP carried over deliberately

The **domain operand is not honoured** when it differs from `CED`. The domain-side access uses
the ordinary read/write path, which translates in the current domain, and neither emulator has a
"translate in domain N" entry point (`TranslateVirtualAddress` reads `regs.CED` internally, and
so does `nd500_mmu_translate`). C# logs a CPU-level line when `domain != CED` and proceeds.

This is flagged rather than faked. Every `RPHS` observed so far targets `CED`. If you find a
cross-domain one, that is a real finding - say so, do not quietly invent a mapping.

---

## 4. Tests

C# side: `Emulated.Tests.ND500/TestND500_RphsWphs.cs`, 5 tests, all green. **They were also run
against the OLD implementation and all 5 fail there** - that check is worth repeating on your
side, because a test that passes both ways is testing nothing. (I nearly shipped exactly that:
my first "old code" run reused a stale build and reported 5/5 passing.)

Coverage to mirror:

| Test | Property |
|---|---|
| `Rphs_AcceptsTheSingleDomainOperand_AndCopies` | the operand-count guard no longer rejects the instruction |
| `Rphs_UpdatesIndexRegistersAndSetsZ_OnCompletion` | I1 -> 0, I2 and I3 advance, Z = 1 |
| `Rphs_StopsAtAPageBoundaryOnThePhysicalSegment_AndClearsZ` | starts 4 bytes below a 2048 boundary, asks for 64, must move exactly 4, leave I1 = 60, I3 exactly ON the boundary, Z = 0, and copy nothing past it |
| `Wphs_CopiesFromTheDomainToThePhysicalSegment` | the reverse direction |
| `Rphs_WithZeroCount_MovesNothingAndSetsZ` | `while I1 > 0` means an empty move is a completed move |

The page-boundary test is the one that matters most: an implementation that copies the whole
count passes every other test here.

---

## 5. Related carve - where the physical segment table comes from

The manual's phrase "the physical segment table pointer" now has a carved counterpart on the
ND-100 side. `SGLOA` (segment PLACE, in segment `030-S3SM5`) writes four resident tables indexed
by segment number:

| Table | Address | Value |
|---|---|---|
| `PSPHS` | `177401B` | physical start |
| `PSLLI` | `175341B` | 0 (lower limit) |
| `PSULI` | `175441B` | size-1 (upper limit) |
| `PSMOD` | `175541B` | mode flag |

`PSPHS` = **PH**ysical **S**tart, and `RPHS` = **R**ead from **PH**ysical **S**egment. PLACE
writes the entry; the swapper's `RPHS` reads it. Full derivation and the evidence for the symbol
identification: `PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md` in this folder.

---

## 6. What to report back

1. Whether defect 1 reproduces (`trap_illegal_operand` on every `RPHS`) - a one-line print in
   the guard proves it.
2. Whether the PST walk extraction was possible without duplicating the walk, and if not, why.
3. Your decision on the paging-off assumption in 3b, so the two ports stay aligned.
4. `ctest` before and after. **The current baseline is 24/30, not 27/30** - `ote_instructions`,
   `mon_calls`, `instruction_validation`, `dom_nc_compiler`, `dom_nc_compile_a` and
   `dom_nc_compile_b` all fail on `8461ed8` before any of this work. Prove your delta by
   stashing, the same way this handoff's author did.
