# PLAN — nd500x growable seg-0/1 DATA, Option 2 redesign

**File:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/PLAN-nd500x-growable-DATA-option2-redesign-2026-07-26.md`
**Date:** 2026-07-26
**Author task:** design (not implement) a >1 MB-capable seg-0/1 DATA mapping that is byte-identical to the proven-good PS_ASI path for the real NC compiler.
**Main tree (read-only, NOT modified):** `/home/ronny/repos/nd500x`
**Oracles run for evidence:** `/home/ronny/repos/nd500x-good/build/bin/nd500x` (aa5cd5e, GOOD), `/home/ronny/repos/nd500x-allocdbg/build/bin/nd500x` (951237d-equiv, BAD, env `ND500X_ALLOCDBG`).

---

## 0. TL;DR

- **Root cause is PINNED at the mechanism level, and it is NOT what the bug report / stopgap comment say.** The bug report frames it as "growable serves ZEROED demand pages vs old path mapped contiguous content" (a *content* difference). The evidence says the real divergence is **trap-vs-silent-map**: under the proven-good PS_ASI mapping the seg-0 access to `va=0x02200000` **raises a page-fault trap that NC's own handler fields and recovers from**; under the BAD PS_ADI growable-alias mapping the *same access is silently satisfied* by `grow_on_fault` allocating a fresh zeroed page, so NC's page-fault handler never runs, NC diverges, and later executes `jumpg` with a bad EA → `PC=0` → emulator halt.
- **Consequence for the redesign:** any Option 2 that *maps* (eagerly or lazily) the region NC expects to fault in will re-break NC. The essential invariant is **"fault beyond the segment's owned extent,"** not "pre-zero the reserve." The 1 MB PS_ASI cap is incidental; NC's actual first out-of-region access is at 34 MB (`0x02200000`), so a two-level (PS_ADI) map that owns *more than 1 MB but far less than 34 MB* is simultaneously >1 MB-capable AND byte-identical to NC.
- **Recommended Option 2:** build seg-0/1 DATA as **PS_ADI two-level, eagerly mapping `data_pages` (adopted, contiguous) + a bounded contiguous zeroed reserve, and DO NOT register seg-0/1 as growable** — so any access past the owned extent still traps to NC exactly as PS_ASI does today. The reserve size is the single knob that unlocks >1 MB.
- **Confidence:** HIGH on the mechanism (convergent direct evidence). MEDIUM on the exact instruction index (bug-report-sourced `~1,196,939`; I corroborated the mechanism but did not independently re-derive the instruction number). See §1.4 and §5.

---

## 1. Root cause (pinned to mechanism; instruction index labeled)

### 1.1 What actually differs between GOOD (aa5cd5e) and BAD (951237d)

I confirmed by reading both trees that the **only** good-vs-bad difference is the seg-0/1 DATA mapping:

- `nd500x-good` `src/ndlib/ndlib_dom_loader.c` builds seg-0/1 DATA as **PS_ASI single-level + `DATA_GROWTH_RESERVE_PAGES` (512) contiguous reserve** (good tree lines 454–480), and aliases seg-0 to seg-1's PS_ASI PST via a plain data capability.
- `nd500x-good` `src/cpu/nd500_segment_alloc.c` **already contains** the full PS_ADI growable MON path (`grow_on_fault`, `watermark_alloc_page`, `alloc_backed_segment`, `nd500_mmu_set_pst_entry(..., PS_ADI, ...)` at good line 447). So the MON-connected / scratch segments (seg 2, 4, 11 …) are PS_ADI-growable **in BOTH builds**.

Therefore the seg-4 read-before-write-zeroed event that the prior experiment flagged (`va=0x246C3438`, `/tmp/.../scratchpad/allocdbg.log:600`) is **identical in GOOD and BAD** and is *not* the divergence. The regression `951237d` changed *only* the DOM seg-0/1 DATA path (PS_ASI+reserve → `nd500_segment_adopt_growable_data` + `nd500_segment_register_growable_alias`). The current main-tree HEAD `/home/ronny/repos/nd500x/src/ndlib/ndlib_dom_loader.c:409–529` is the Option-1 stopgap that restored PS_ASI+reserve.

### 1.2 The decisive observation (logged, reproduced twice)

Running GOOD (`/home/ronny/repos/nd500x-good/build/bin/nd500x`) with the repro emits, and still reaches a clean NC termination:

```
[MMU] TRAP: PS_ASI page fault! L1=34 must be 0! vaddr=0x02200000
-CROSS-A
program terminated
-- program exited (2375216 instructions) --
```
(`/tmp/.../scratchpad/repro_good.log` shows the same with 2313497 instructions on an earlier run; re-run today gave 2375216 — the count varies with pre-existing `B.CAT`/`B.LIST` state, the outcome does not.)

- `0x02200000` decodes as: segment `= (0x02200000>>27)&0x1F = 0`; L1 `= (0x02200000>>20)&0x7F = 34`; L2 `= 0`; offset `0`. That is page index `(34<<9)|0 = 17408` → byte `34 MB` into seg-0.
- Under PS_ASI, L1 must be 0 (`src/cpu/nd500_mmu.c:471–475`), so this access **traps** (`trap_page_fault`).
- `trap_page_fault` → `raise_trap(TRAP_PGF,…)` (`src/cpu/cpu.c:776–777`) **dispatches to the guest's THA handler**: it reads `THA + trapNumber*4`, verifies an `ENTT` (`0xBC`) at the handler, saves state, and sets `PC` to the handler (`src/cpu/cpu.c:704–753`). NC's DOM header supplies THA (`src/ndlib/ndlib_dom_loader.c:391`).
- The GOOD run does **not** print `[TRAP] No trap handler …` (the `handlerAddr==0` path, `cpu.c:712–721`). So NC's PGF handler **was found and dispatched**, NC handled the fault, and execution continued to `-CROSS-A / program terminated` — NC's normal, successful completion.

In BAD, the same access does **not** trap. The instrumented log shows it silently grown through the seg-0→seg-1 growable alias:

```
/tmp/.../scratchpad/allocdbg.log:604  ALLOC pfn=1594 ... GROW-L2    va=0x02200000
/tmp/.../scratchpad/allocdbg.log:606  ALLOC pfn=1595 ... GROW-DATA  va=0x02200000
/tmp/.../scratchpad/allocdbg.log:608  GROW va=0x02200000 dom=1 seg=0 l1=34 l2=0 -> data_pfn=1595 ... via slot{dom=1,seg=0,l1tbl=0x001F4000}
/tmp/.../scratchpad/allocdbg.log:609-612 VALWATCH WRITE8 phys=0x0031D800.. val=0x18,0x00,0x24,0x58  (NC writes 0x18002458)
```

So in BAD the access is a **write** that is silently satisfied by a fresh zeroed page (`watermark_alloc_page` memsets, `src/cpu/nd500_segment_alloc.c:146–157`), *no trap is raised*, and NC's PGF handler never runs.

### 1.3 Mechanism (pinned)

> NC installs a page-fault (PGF) trap handler via its DOM THA. During code generation it accesses seg-0 `va=0x02200000` (34 MB, L1=34), which lies far past its ~122 KB initialized DATA (`data_pages=61`, `allocdbg.log:3`). Under the proven-good PS_ASI seg-0/1 mapping this access **traps** (PS_ASI forbids L1≠0); nd500x routes TRAP_PGF to NC's own handler, NC recovers, and finishes cleanly. Under the BAD PS_ADI growable-alias mapping the seg-0 slot is registered growable (`register_growable_alias`, `allocdbg.log:608 slot{dom=1,seg=0}`), so `grow_on_fault` (`nd500_mmu.c:519,570` → `nd500_segment_alloc.c:219–232`) **silently maps a zeroed page** and suppresses the trap. NC therefore takes a control path the GOOD run never enters, and ~1.1 M instructions later runs `jumpg` with EA `0x100001A4` → `PC=0` → `[STOP] Invalid instruction 0x00 at PC=0` (`repro_bad.log`).

The `b.24`/"shorter hex number" symptom in the bug report is **downstream** of this: it is a value NC computes *after* missing its page-fault handler, not the primary divergence.

### 1.4 What remains inferred vs verified

- **Verified (direct, reproduced):** GOOD faults at `0x02200000` and recovers via NC's THA handler to a clean exit; BAD silently grows that same page and crashes at `PC=0`; the seg-0/1 DATA mapping is the sole good-vs-bad difference; the MON/seg-4 growable path is identical in both.
- **Inferred / bug-report-sourced (labeled):** the exact first-divergent instruction index `~1,196,939` and the `b.24` branch come from the bug report's differential PC trace. I did **not** re-run an instruction-aligned byte-for-byte differential (it needs instruction-count instrumentation added to both builds and a full 1.2 M-instruction trace diff). The mechanism above is established by convergent evidence without it; the instruction index is not load-bearing for the redesign. If a reviewer wants the belt-and-suspenders proof, see §5 for the exact experiment.

---

## 2. Why the old path works and the new one fails

| Axis | GOOD PS_ASI + reserve (aa5cd5e) | BAD PS_ADI growable alias (951237d) |
|---|---|---|
| seg-0/1 owned extent | 512 pages max (L1 forced 0), `nd500_mmu.c:471` | up to 128 MB, demand-grown, no bound |
| access past owned extent (e.g. `0x02200000`) | **traps** → NC's PGF handler runs | **silently mapped** (zeroed) → no handler |
| init data | adopted contiguous, PFN `data_base+p` | adopted contiguous, PFN `data_base+p` (same!) |
| reserve/stack region (<1 MB) | contiguous zeroed block | (unused for `0x02200000`; that VA is 34 MB) |
| NC outcome | clean `-CROSS-A` exit | `jumpg`→`PC=0` halt |

Key corrections to the prevailing explanation:

1. **It is not "zeroed-demand vs pre-zeroed-content."** Both paths hand back zeroed pages there; the good path never maps `0x02200000` at all — it faults. The `DATA_GROWTH_RESERVE_PAGES` reserve (good `ndlib_dom_loader.c:454`) only covers the first ≤1 MB (stack/heap just above data); it is irrelevant to `0x02200000` (34 MB).
2. **The 1 MB PS_ASI cap is incidental, not the mechanism.** What NC depends on is the *existence of a fault boundary* below 34 MB, plus its PGF handler. PS_ASI happens to put that boundary at 1 MB.
3. **The seg-0→seg-1 growable alias is the specific culprit.** `register_growable_alias` (`nd500_segment_alloc.c:304–324`) put seg-0 into `g_growable`, so `grow_on_fault`'s `growable_find(domain,0)` succeeds and the fault is resolved instead of trapped.

**Inference (labeled):** NC almost certainly uses the seg-0 page fault as an intentional signal — its runtime detects "I've run past my DATA segment" and switches to MON-allocated scratch (GSWSP/FSCNT seg 2+), rather than actually needing 34 MB of contiguous seg-0. This is consistent with NC's real scratch living in the demand-grown MON segments (seg 2/4/11 in `allocdbg.log`). Not required for the fix, but it explains why "just give NC the memory it asks for" is wrong.

---

## 3. Staged redesign plan

### 3.1 Design invariant (the acceptance contract)

> **Seg-0/1 DATA must trap (TRAP_PGF, dispatched to the guest THA) on any access beyond the segment's owned page extent, exactly as PS_ASI does today — while being able to *own* more than 512 pages (1 MB).**

Byte-identical-to-NC reduces to two concrete, testable facts against the aa5cd5e oracle:
- (I1) The first access whose *resolution differs* from PS_ASI must not occur before NC's genuine first out-of-region access (`0x02200000`). Practically: the owned extent must be `< 17408` pages (34 MB) and `≥` NC's real high-water seg-0 page (well under 1 MB, per the logs). Keeping the default owned extent at the current ~512–573 pages guarantees I1.
- (I2) Init-data pages and the low reserve are byte-for-byte the same (same adopted PFNs; reserve zeroed). PS_ADI with an eager contiguous reserve satisfies this trivially.

### 3.2 Options considered

- **(a) Keep PS_ASI, chain beyond 1 MB.** Not viable: a segment has exactly one PST entry, and PS_ASI hard-traps L1≠0 (`nd500_mmu.c:471`). Extending it means either faking multiple PST entries per segment or changing PS_ASI semantics — both violate the architecture and the MMU core. **Reject.**
- **(b) PS_ADI, eagerly pre-map a bounded contiguous reserve, NO grow-on-fault for seg-0/1.** Owns `data_pages + reserve` contiguous zeroed pages; faults beyond. >1 MB-capable (reserve can span multiple L2 tables, L1>0). Byte-identical to NC when reserve keeps the boundary below 34 MB. Simple, provably contiguous/zeroed. **RECOMMENDED.**
- **(c) Fix the growable path's demand semantics to reproduce old content.** The old path produces *no content* — it faults. To "reproduce" it, `grow_on_fault` would have to know the segment's owned bound and *refuse* to grow past it (fault instead). That is just (b) implemented lazily. Viable as a variant but adds a bound-check in the hot fault path. **Fold into (d).**
- **(d) Hybrid: PS_ADI, eager-map init data, LAZY-grow only within a bounded window `[data, data+reserve)`, fault beyond the window.** Memory-thrifty version of (b): identical fault semantics, but reserve pages are allocated on first touch instead of up front. Slightly more complex; same correctness. **Recommended fallback if the eager reserve's memory cost matters (it does not at 512 pages / 1 MB in a 16 MB machine).**

### 3.3 Recommended: Option (b), exact change sites

All changes are in the two files the bug report scopes; **no MMU-core change is required** — the PS_ADI L1/L2-invalid paths already call `trap_page_fault` when `grow_on_fault` declines (`nd500_mmu.c:523–558`, `574–593`).

**Change site 1 — `/home/ronny/repos/nd500x/src/cpu/nd500_segment_alloc.c`: add a non-growable, bounded PS_ADI DATA builder.**
Add a sibling to `nd500_segment_adopt_growable_data` (current lines 241–293), e.g. `nd500_segment_map_bounded_data(cpu, m, domain, segment, psn, data_phys_base, data_pages, reserve_pages, watermark_floor_base)`, that:
1. `watermark_init` + raise `g_next_free_pfn` above `watermark_floor_base` (reuse lines 250–254).
2. Allocate the L1 table page (`watermark_alloc_page`, line 263).
3. For `p in [0, data_pages)`: adopt `data_base_pfn + p` into `l1=p>>9, l2=p&0x1FF`, creating L2 tables as needed (reuse the loop at 276–289, prot 0/RW).
4. **New:** allocate `reserve_pages` **contiguous** fresh pages (a single `watermark`-region run) and map them at `p in [data_pages, data_pages+reserve_pages)` with the same `l1=p>>9,l2=p&0x1FF` derivation, prot 0. Zeroing is already done by `watermark_alloc_page`.
5. `nd500_mmu_set_pst_entry(cpu, psn, PS_ADI, l1_pfn)` (line 291).
6. **Do NOT** add an entry to `g_growable` for this segment (do not set `g->in_use`). This is the crux: with no `g_growable` slot, `growable_find(domain, seg)` returns NULL, so `grow_on_fault` returns 0 and the MMU traps past the owned extent — the PS_ASI-equivalent behavior.
Return the L1 PFN so the alias (below) can share it.

**Change site 2 — same file: do NOT register seg-0/1 as growable.**
`nd500_segment_register_growable_alias` (lines 304–324) must **not** be used for the seg-0→seg-1 DATA alias. The alias becomes a plain shared PST/data-capability (see change site 3). Leaving seg-0/1 out of `g_growable` is what preserves the trap.

**Change site 3 — `/home/ronny/repos/nd500x/src/ndlib/ndlib_dom_loader.c`: replace the PS_ASI DATA block and the alias.**
- Replace the per-segment PS_ASI DATA block (current HEAD lines 462–506) with a call to `nd500_segment_map_bounded_data(...)` for each `has_data` segment, passing `reserve_pages = DATA_RESERVE_PAGES` (see §3.4). Keep PROG segments PS_ASI (unchanged, lines 429–457) — only DATA changes.
- Replace the FORTRAN alias (current HEAD lines 520–526) so seg-0's data capability points at **seg-1's PS_ADI PST index** (share the same two-level tables), via `nd500_mmu_set_data_capability(cpu, domain, 0, seg1_psn | DC_WRP)` — a plain capability alias, **not** `register_growable_alias`. Because seg-0/1 are absent from `g_growable`, `0x02200000` (seg field 0) faults exactly as today.
- The removed "PS_ADI growable pass-2" comment (lines 528–529) is dropped.

**Untouched:** the entire MON-connected/scratch path (`alloc_backed_segment`, `nd500_mon_connect_file_as_segment`, `nd500_mon_allocate_segment`, `grow_on_fault`, `watermark_alloc_page`, `nd500_segment_writeback`). Those stay PS_ADI-growable — they are correct and already present in aa5cd5e.

### 3.4 The one knob: `DATA_RESERVE_PAGES`

- **Default = 512** (or the current `DATA_GROWTH_RESERVE_PAGES`): keeps NC byte-identical (owned extent ≈ data+512 pages, still far below the 34 MB fault). Under PS_ADI this now actually *maps* pages 512–572 that PS_ASI left faulting, but the logs show NC never touches the 1 MB–34 MB range, so I1 holds — **must be confirmed by the acceptance test in §3.5**.
- **>1 MB capability:** raising `DATA_RESERVE_PAGES` past 512 now works (PS_ADI spans L1>0), giving a program a larger owned seg-0/1 DSEG. Must stay `< 17408` pages to keep NC's fault (and, more generally, `<` the smallest legitimate fault any co-running program relies on).
- **Where to source the size (open):** the vendor DOMs don't declare the uninitialized DSEG extent (FLA/FUA/MINP/MAXP = 0, `ndlib_dom_loader.c:50–58`). Options: (i) a fixed generous default (simplest), (ii) a per-domain config override in `nd500x.ini`, (iii) derive from the DOM segment descriptor if a length field is ever found. Recommend (i)+(ii) now; (iii) if/when a real >1 MB-seg-0/1 program is identified (see §4).

### 3.5 Acceptance / test procedure

1. **NC byte-identical (primary gate).** Run the repro (`§Repro` of the bug report) against the rebuilt tree. **Must** reproduce GOOD: emit `[MMU] TRAP: PS_ADI L1 page not valid! vaddr=0x02200000` (now PS_ADI wording) — i.e. a trap, not a grow — dispatch to NC's PGF handler (no `[TRAP] No trap handler`), and reach `-CROSS-A / program terminated` with an instruction count within a few % of the aa5cd5e oracle. **Must NOT** print `[STOP] Invalid instruction 0x00 at PC=0`.
2. **First-divergent-read oracle diff (rigor).** With instruction-count instrumentation (env-gated, in the experiment copy only), assert that no seg-0/1 data read resolves to a different PFN/value than the aa5cd5e oracle before `0x02200000`. Equivalent cheap proxy: assert the seg-0/1 fault set is identical to GOOD (first seg-0 fault VA = `0x02200000`).
3. **>1 MB seg-0/1 (secondary gate).** Craft a DOM whose seg-0/1 DATA legitimately spans >512 pages (e.g. a large initialized array or a program that writes 1.5 MB into seg-1 before any MON scratch), set `DATA_RESERVE_PAGES` to cover it, and confirm it runs without a spurious fault AND without silently masking a fault it should take.
4. **No regression in MON path.** Re-run an FSCNT/GSWSP-heavy workload (the linker link, `nd500-linker` skill flow) to confirm the untouched growable MON path still works.

### 3.6 Risks

- **R1 (medium): the default reserve now maps pages PS_ASI left faulting (1 MB–1.14 MB).** If NC (or another vendor DOM) relies on a fault anywhere in 1 MB–34 MB, mapping there would break it. Mitigation: gate 1 confirms NC; keep the default reserve minimal (512) and treat any increase as per-program opt-in. **Safest variant:** cap the owned extent at exactly 512 reachable pages by default (reserve = 512 − data_pages) to reproduce PS_ASI's reachable set precisely, and only exceed it for a program that declares a larger DSEG.
- **R2 (low): PS_ADI double protection check.** Writes check both L1 and L2 prot bits (`nd500_mmu.c:597`). The builder must set L1 PTEs prot=0 (RW) even though data is RW at L2 — matching the existing adopt code's note (`nd500_segment_alloc.c:187–189, 284`). Carry that over.
- **R3 (low): watermark collision.** New pages must be taken strictly above `watermark_floor_base` (the loader's final cursor), since PS_ADI pages are invisible to `find_highest_used_pfn` (`nd500_segment_alloc.c:120–142, 250–254`). Reuse the existing floor logic verbatim.
- **R4 (low): seg-0/1 that legitimately needs demand growth.** If a future program *wants* lazy growth in seg-0/1 (not just a bigger fixed reserve), use variant (d) with a hard upper bound instead of unbounded `grow_on_fault`.

---

## 4. Open questions / unverified

- **Q1 — Is a >1 MB seg-0/1 DSEG actually needed by any real program?** The segment_alloc.c header (`nd500_segment_alloc.c:96–99`) cites the ND linker faulting at `vaddr=0x18402004`, which decodes to **segment 3** (`0x18402004>>27 = 3`) — a MON-*connected* (FSCNT) segment, already handled by the still-growable MON path, *not* seg-0/1 DOM DATA. So the task's premise that the 1 MB seg-0/1 cap "re-breaks the linker's output domain" appears to be a **misattribution**: the linker's large write is to a MON segment, which the stopgap did not touch. The genuine >1 MB-*seg-0/1* case may be hypothetical. This does not change the recommended design (it satisfies the requirement either way) but it lowers the urgency and argues for the conservative default in §3.6/R1.
- **Q2 — Exact instruction index of the divergence.** Bug-report-sourced (`~1,196,939`); not independently re-derived here. See §5.
- **Q3 — What NC's PGF handler actually does** (grow via MON? abort sub-op? longjmp to a retry?). Not traced. Knowing it would let us prove that no reachable reserve size between NC's true high-water and 34 MB is safe/unsafe. The conservative default (R1 safest variant) sidesteps needing this.
- **Q4 — Whether seg-1 (as opposed to the seg-0 alias) is ever accessed past its extent by NC.** The observed fault is seg-0 (`0x02200000`). The design removes growability from both seg-0 and seg-1 for symmetry, but only seg-0's behavior is directly evidenced.

---

## 5. The rigor experiment (if the exact first-divergent read must be nailed)

To positively pin the instruction-aligned first divergent data read (the experiment's still-open item):

1. In the **allocdbg copy only** (`/home/ronny/repos/nd500x-allocdbg`), env-gate a print of `cpu->instruction_count` on (a) every seg-0/1 `grow_on_fault` resolution and (b) every `trap_page_fault` for segment 0/1. Rebuild only that copy.
2. Add the same instruction-count print to a throwaway instrumented copy of `nd500x-good` at its PS_ASI `L1!=0` trap (`nd500_mmu.c:471`).
3. Run both with the repro; confirm the seg-0 `0x02200000` event fires at the **same** instruction count in both, and that it equals the bug report's divergence point (`~1,196,939`). Expected: GOOD prints the trap, BAD prints the grow, at the same instruction — which upgrades Q2 from inferred to verified.

This was **not** run here because the mechanism is already established by convergent direct evidence (§1.2–1.3) and the exact index is not load-bearing for the redesign. It is documented so a reviewer can close it cheaply.

---

## 6. Summary of concrete change sites (for the implementer)

| File (absolute) | Location | Change |
|---|---|---|
| `/home/ronny/repos/nd500x/src/cpu/nd500_segment_alloc.c` | new fn near lines 241–293 | Add `nd500_segment_map_bounded_data()` — PS_ADI, adopt data + eager contiguous zeroed reserve, **no `g_growable` entry** |
| `/home/ronny/repos/nd500x/src/cpu/nd500_segment_alloc.c` | lines 304–324 | Do **not** call `register_growable_alias` for seg-0/1 |
| `/home/ronny/repos/nd500x/src/ndlib/ndlib_dom_loader.c` | lines 462–506 | Replace PS_ASI DATA block with a call to the new bounded PS_ADI builder |
| `/home/ronny/repos/nd500x/src/ndlib/ndlib_dom_loader.c` | lines 520–526 | seg-0 alias = plain shared DC to seg-1's PS_ADI PST (not growable) |
| `/home/ronny/repos/nd500x/src/cpu/nd500_mmu.c` | 471–475, 519–558, 574–593 | **No change** — existing trap-on-decline behavior is exactly what we want |

Header prototype for the new builder goes in `/home/ronny/repos/nd500x/src/cpu/nd500_mmu.h` next to the existing `nd500_segment_adopt_growable_data` declaration (lines 211–225).
