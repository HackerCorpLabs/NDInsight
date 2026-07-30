# BUG REPORT - nd500x: PTE-format refactor regressed DOM loading (loader not realigned)

**File:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\BUGREPORT-nd500x-PTE-refactor-regressed-DOM-loading-2026-07-25.md`
**Date:** 2026-07-25
**Repo:** `~/repos/nd500x` (WSL)  **Branch:** `fix/deabf-i1-success-and-load-investigation`
**Severity:** HIGH - all `.dom` program loading is broken; `dom_nc*` ctests fail.
**For:** the MMU-redesign team (heads-up: the refactor missed a set of sites).

## Summary

Commit **`6b3d4fb` "refactor(mmu): align page-table entry format to ND-500 hardware (pte.h)"**
changed the PTE bit layout from the old private format `[31:2]=PFN, [0]=prot` to the hardware
format `pg_prot@31, pg_pfnum@[29:0]`. It correctly realigned the readers/writers it knew about
(`nd500_mmu.c`, `nd500_segment_alloc.c` write_pte/read_pte_pfn/find_highest_used_pfn,
`commands.c`) - but it **did NOT update the inline PTE construction in `ndlib_dom_loader.c`**.

As a result the DOM loader writes PROGRAM (and SINTRAN-window / DATA) page-table entries in the
**old** format, while the MMU now decodes them in the **new** format. The PFN is misread, PROGRAM
pages map to the wrong physical memory, and any `.dom` crashes almost immediately.

This is independent of any in-flight PS_ADI / demand-grow work - it is purely the format mismatch.

## Affected commits

- `6b3d4fb` - the PTE-format refactor (introduced the mismatch by omission).
- `09e3108` "feat(mmu): build guest DIT/PST + translate can read them (gated, WIP)" - HEAD; still
  carries the regression (the loader was not touched).

## Symptom / repro

```
cd ~/repos/nd500x/build && ctest -R dom_nc
  # dom_nc_compiler, dom_nc_compile_a, dom_nc_compile_b  -> FAIL
```

Running any DOM directly, e.g. the NC C compiler, dies after ~8 instructions:

```
cd /home/ronny/ND500USERS
printf 'LOGIN GUEST\nNC-A06\nEXIT\n' | \
  ./../repos/nd500x/build/bin/nd500x --monitor --config /home/ronny/ND500USERS/nd500x.ini
...
-- NC-A06 placed (domain 1, start 0x08000004) --
[TRAP] CALL at PC=0x08000019: Target 0x080030CD opcode=0x28 is not an entry point (PROGRAM SPACE)
[STOP] trap at PC=0x08000019 ... -- program exited (8 instructions) --
```

The `opcode=0x28 is not an entry point` is the tell: the CALL target is fetched from PROGRAM
space that is mapped to the wrong physical page, so the byte there is not a valid ENT* opcode.

## Root cause (exact locations)

`ndlib_dom_loader.c` builds page tables with inline PTE packing in the OLD format at three sites:

| Line | Context | Current (OLD format) |
|------|---------|----------------------|
| 137  | SINTRAN window (seg 31) page table | `uint32_t pte = (pfn << 2) | 0;` |
| 428  | PROG page table (code)             | `uint32_t pte = (pfn << 2) | 1;` |
| 474  | DATA page table                    | `uint32_t pte = (pfn << 2) | 0;` |

Meanwhile the MMU/`write_pte` now expect `((prot & 1) << 31) | (pfn & 0x3FFFFFFF)`
(`nd500_mmu.c` read_pte ~L678-700; `nd500_segment_alloc.c` write_pte ~L159-165). Writer and
reader disagree -> wrong PFN -> wrong physical page.

## Fix (verified)

Encode these inline PTEs in the hardware format (or call the shared `write_pte`):

```c
/* prot = 0 (RW) or 1 (RO code) */
uint32_t pte = ((prot & 1u) << 31) | (pfn & 0x3FFFFFFFu);
```

Concretely: line 137 -> `pfn & 0x3FFFFFFFu` (prot 0); line 428 -> `(1u<<31) | (pfn & 0x3FFFFFFFu)`
(prot 1); line 474 -> `pfn & 0x3FFFFFFFu` (prot 0).

**Evidence it fixes it:** after realigning the window + PROG sites, the DOM loads and NC runs
(`preprocessing: ok / syntax check: ok / semantic check: ok`), and

```
ctest -R "dom|nc_|mmu|segment"   ->  100% (6/6) pass
```

(Cross-check: the 3 suite failures `ote_instructions`, `mon_calls`, `instruction_validation` are
PRE-EXISTING and unrelated - they fail with or without any of these changes.)

## Suggested guard (optional)

Consider replacing all inline PTE packing with a single shared helper (the `write_pte` used in
`nd500_segment_alloc.c`) so a future format change has exactly one site. The three loader sites are
the ones that slipped precisely because they hand-pack the PTE instead of going through the helper.

## Note on overlap

I have local, uncommitted work on this same branch (a PS_ADI growable-DATA change for the NC
compiler's >1 MB data segment, plus the loader PTE realignment above). If the redesign reworks the
loader, my loader edits can be discarded - this report describes the underlying tree regression so
your team can fix it in whatever way fits the redesign, independent of my changes.
