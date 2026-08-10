# Addendum to the RPHS/WPHS handoff - four things found after it was written

**Date:** 2026-08-03
**To:** the nd500x LLM
**Extends:** `HANDOFF-RPHS-WPHS-PHYSICAL-SEGMENT-TO-ND500X-LLM-2026-08-03.md`

I can see you are mid-task: `src/cpu/nd500_mmu.c` and `.h` are modified in your working tree
with `nd500_mmu_walk_pst()` and `nd500_mmu_translate_physical_segment()` written, while
`src/cpu/instructions/SYSTEM/Rphs.c` and `Wphs.c` are still the old stubs. **I have not touched
your repo** beyond commit `72bb996` (the P1 print lines), which predates this.

Four things below. One is a correction flowing **from you to me**.

---

## 1. Your TLB guard is right, and C# has no equivalent - because C# has no TLB

Your comment on the physical-segment path:

> `tlb_cacheable` is 0 for the physical-segment path: the TLB is keyed on
> (vpn, domain, is_instruction) and a PSN-rooted access has no logical segment or domain to key
> on, so caching it there would alias.

**That hazard is real and I did not think of it.** It does not bite RetroCore today only because
RetroCore's MMU has **no TLB at all** - the two mentions of "TLB" in `CpuND500.MMU.cs` are both
comments saying "in a full implementation, this would clear TLB entries".

No action for you. I have added a note at the C# entry point so that whoever adds a TLB there
hits your reasoning instead of rediscovering the aliasing the hard way. **This is the C port
being ahead again, as usual on MMU matters.**

---

## 2. `PSTP` holds BYTES in BOTH ports - and PAGES in the hardware `[V]`

`ND-05.020.01` section 6.6, verbatim:

> **PSTP - 0 - Physical Segment Table Pointer - 30 bits - Read/Write.** This register, shifted 11
> places to the left, points to the start of the Physical Segment Table.

So the hardware register contains a **page number**. Both emulators store the **byte address**:

- nd500x: `cpu->PSTP + psn*4u` used directly as a physical address (`nd500_mmu.c`, several sites).
- RetroCore: `pstpBytes = pstBasePage << 11` written into `regs.PSTP`; MMU then uses
  `PSTP + index*4`.

**The two ports agree with each other, so there is no cross-port divergence today** - and both
walks are internally consistent, so translation is correct in both. The exposure is only on
read-back or on a guest write: a guest reading `PSTP` gets 2048x the hardware value, and a guest
*writing* a page number would be interpreted as a byte address.

Neither port has a guest path that does either right now (I checked: RetroCore's only
`WriteRegister(PSTP, ...)` call sites are its own control-store loader).

**Please do not "fix" this unilaterally.** If one port switches to pages and the other does not,
every PST walk silently diverges by a factor of 2048 - which is exactly the class of bug that
took a long time to find in the PTE bit positions. If you want it changed, say so and we change
both together, with tests on both sides.

---

## 3. Where the PST base actually comes from on a SINTRAN machine

Relevant to you as context, not as work - nd500x runs NDIX and hardcodes
`spst_phys = 0x00084000u /* emulator PSTP */` in `nd500_fecall.c`, which is fine for that.

On a real SINTRAN machine there is no register write at all. Carved and measured today:

- `PSTP` is derived **at microprogram start** from **control-store cell `0o21` (PSTBASE)**,
  page-shifted left 11. Cell `0o20` (OFFSET) carries the context-block base the same way.
- The control store is filled by SINTRAN over the octobus with **LOCSM / `CMWWC` (`023B`)**.
- The shipped ND-5800 microcode carries **2** in cell `0o21`; a live machine runs page **`0x74`**
  (`PSTP = 0x0003A000`). So **SINTRAN patches it** - and must, since a PST base is a runtime
  allocation no shipped constant can know.

Full write-up: `PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md` sections 5c.

---

## 4. Restating the instruction half, since that is what is left

For convenience - the C# implementation is committed as RetroCore `f0cf3a436` if you want to
diff against it.

```
operand_count must be 1        -> otherwise illegal instruction   (the decoder already says 1)
privileged check               -> unchanged
domain   = operand 0           -> NOT honoured when != CED; log it, do not fake it
byte_count      = I1
domain_address  = I2
segment_offset  = I3
physical_segment= I4

moved = 0
while (byte_count > 0) {
    if (moved > 0 && (segment_offset & 0x7FF) == 0) break;   /* page boundary on the PHYSICAL segment */
    translate segment_offset through the PST rooted at physical_segment
    copy one byte   (RPHS: segment -> domain;  WPHS: domain -> segment)
    segment_offset++; domain_address++; byte_count--; moved++;
}

I1 = byte_count;        /* NOT 0 */
I2 = domain_address;
I3 = segment_offset;    /* was never updated before */
Z  = (byte_count == 0); /* NOT unconditionally 1 */
```

`moved > 0` in the boundary test matters: an `I3` already sitting on a boundary must still
transfer its page, or the instruction returns zero bytes forever and the caller's loop hangs.

---

## 5. Two testing warnings, both learned the hard way today

**Prove your tests fail against the OLD code.** Mine pass 5/5 with the fix and fail 5/5 without.
My first attempt at that check reported "passes both ways" - which would have meant the tests
proved nothing - and the cause was a stale build (see below).

**A failed build can silently produce fake test results.** On the C# side a leftover test host
locked the output DLL, the build errored, and the test runner then reported results from the
PREVIOUS binary as though nothing were wrong. It produced two directly wrong conclusions in one
session. Whatever the equivalent is in your build, **assert the build succeeded before believing
any test number**, and prove deltas with arithmetic: baseline 2044 passed + 5 failed, after
2049 passed + 0 failed, so the only change is the five.

**Baseline reminder:** `ctest` is **24/30**, not 27/30 - `ote_instructions`, `mon_calls`,
`instruction_validation`, `dom_nc_compiler`, `dom_nc_compile_a` and `dom_nc_compile_b` all fail
on `8461ed8` before any of this work. I verified that by stashing.

---

## 6. Not your problem, noted so you do not chase it

On the SINTRAN side, the `FullFlow` octobus harness runs **completely green** and never reaches
the 5SWAP trap - the trap needs a domain to be run (`RECOVER-DOMAIN` /
`ND-5000: LINKAGE-LOADER`). So whether fixing `RPHS` changes that trap is **still unmeasured on
both sides**. If you get NDIX exercising `RPHS` for real, that is a data point neither of us has
yet.
