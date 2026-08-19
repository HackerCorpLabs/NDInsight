# CARVE ANSWER — the ND-500 hardware PST vs SINTRAN's PSPHS tables: parallel bookkeeping; the PST is swapper-maintained, its BASE is a SINTRAN software allocation

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-ANSWER-PST-WRITER-VS-PSPHS-2026-08-11.md`
**Date:** 2026-08-11.
**Question (marked UNPROVEN in `PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md` §0):**
does SINTRAN build the ND-500's MMU-walked Physical Segment Table (the PSTP-based one) from
the `PSPHS`/`PSLLI`/`PSULI`/`PSMOD` tables written at PLACE time, or are they parallel
bookkeeping? Find what writes the ND-500-resident PST.

**Sources and grades:**
- **[V, prior carves — re-read, not re-derived]** `PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md`
  (§5b/§5c), microcode-track docs `MICROCODE-ANSWER-C1-PCB-PST-BUILDER-2026-07-20.md`,
  `SWAPPER-START-CPU-MMU-SETUP-CARVE-2026-07-21.md` (via `D4-PLAN-PHASES-AND-TASKS-2026-07-20.md`
  phase-2 rows), `CARVE-ANSWER-DEFMC-MPM-BASE-051302-2026-08-10.md`.
- **[MANUAL]** `Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md`.
- **[V]** `SINTRAN\ND500\swapper\swapper-k01-pseg.asm` (grep-level checks only, cited as such).

---

## 0. Verdict

**Parallel bookkeeping — CONFIRMED, and the question can now be stated closed at the
corpus level:**

1. **`PSPHS`/`PSLLI`/`PSULI`/`PSMOD` are ND-100-side bookkeeping and are NOT the source of
   the ND-500 PST.** Already four-for-four in the 2026-08-03 carve (all four whole-family
   sites — place, unplace, copy, translate — are ND-100 software; none writes toward ND-500
   memory), and §5c closed the base question: **the PST base reaches the CPU as
   control-store cell `0o21` (PSTBASE), patched by SINTRAN before microprogram start;
   `PSTP = cell21 << 11`.** Nothing new contradicts this; everything new supports it.

2. **The PST's LOCATION is a SINTRAN software allocation, not a fixed address.** The
   System Monitor's memory configuration path allocates it (`ALLOC @171076` → `SSYSE`
   store, `030-S3SM5`; the live MEM-CONF read-back lists `PST 4252B` alongside the register
   block — `CARVE-ANSWER-DEFMC-MPM-BASE-051302-2026-08-10.md` §6.4 [V]), and the ND-500
   Monitor prints it as "`Phys.seg.table..: 00000644000B ==> phys.ND5000 addr.`"
   (ND-05.017.01 page 119). So SINTRAN CHOOSES where the PST lives and tells the CPU
   (cell `0o21`); the values in `PSPHS` play no part in that.

3. **The PST's ENTRIES are maintained by the SWAPPER (ND-500 process 0), not by SINTRAN
   and not by the microcode.** Grade: **[V by elimination + MANUAL positive statement]**:
   - The microcode never writes PST/PTE entries — zero `IMM,*`/`DMM,*` page-table writes in
     the whole B30 image; the walk is hardware
     (`MICROCODE-ANSWER-C1-PCB-PST-BUILDER-2026-07-20.md`, D4-plan rows 2.2/2.6) [V, prior].
   - Nothing in the carved SINTRAN side writes it: the four PSPHS-family sites are the
     complete whole-family set in `030-S3SM5`, and scans for the PST in the shared window
     found nothing — the PST is in ND-500-local memory that ND-100 code does not address
     per-entry [V, prior].
   - The manual states the swapper does it, three separate times
     (`ND-05.017.01`, file line numbers):
     - `:4648` "**The swapper is also using physical addressing when accessing the physical
       segment table.**"
     - `:4658`/`:4662` "One of these page addresses may have been **wrongly updated by the
       swapper** or the system monitor … the physical page address in the PST has been
       **wrongly updated by the swapper process** earlier."
     - `:4801-4807` the page-fault flow: process idled, **swapper started to handle the
       page fault**; the swapper takes the logical page number from the fault LA.
   - Corroborating swapper-side signature [V, grep this session]: the swapper PSEG contains
     25+ `dctsb`/`pctsb` sites (TSB/translation-buffer flushes — the instruction you issue
     right after editing translation tables) and **zero `wphs`** — its table writes are
     ordinary stores into its own mapped data space, not physical-segment writes, which is
     why no "PST writer instruction" ever showed up in opcode hunts.

4. **RESIWR/13B/14B do NOT carry PST entries.** The 13B/14B RESIRD/RESIWR family carries
   SEGMENT CONTENT (and, at swapper-load time, the loader's program/data page LISTS at
   `0x6E800`/`0x6E000` — those are the swapper LOADER's tables, not the MMU PST; see the
   sintran-carving skill §12 note). No 14B burst was ever observed naming the PST, and the
   PST lives outside the shared window [V, prior]. **[I]** only in the narrow sense that
   absence-of-observation is the evidence.

## 1. The resulting model (for the emulator and the next carver)

```
PLACE time  (ND-100):  SGLOA/071720 write PSPHS/PSLLI/PSULI/PSMOD   = ND-100 bookkeeping
                       (RPHSG/WPHSG-style monitor access + limits); NOT sent to the ND-500.
CONFIG time (ND-100):  ALLOC/SSYSE pick physical areas: register block, PST, WIP/PGU;
                       SINTRAN patches control-store cell 0o21 = PST page number.
CPU start   (micro):   INIT_SAM: MM,PSTP := cell21 (page number; hardware shifts <<11).
RUN time    (ND-500):  the MMU walks PST[PS] in hardware on every reference;
                       the SWAPPER writes/updates PST and page-table entries in ND-500
                       memory as pages come and go (and flushes the TSB: dctsb/pctsb),
                       asking SINTRAN for disk I/O (LSWPAGE) and pages (LALLOPAGE/G5PAG).
```

The name echo `PSPHS` ("Physical segment PHysical Start") vs the ND-500 `RPHS` instruction
stays a red herring — different machines, different tables (already flagged in the
2026-08-03 carve; nothing found since supports the tempting reading).

## 2. What would still upgrade this to fully byte-proven [OPEN]

1. **The exact swapper store sites that write a PST entry** (in `swapper-k01-pseg.asm`,
   likely near the `dctsb`/`pctsb` clusters and the Table-A/B/D bookkeeping the handler
   carve mapped). The manual + elimination make the writer's identity solid; the
   instructions themselves are not yet named.
2. **The ND-100 instructions that patch control-store cell `0o21`** (flagged OPEN in the
   2026-08-03 doc §5c already; unchanged).
3. **Who seeds the INITIAL PST content before the swapper runs** (first entries for the
   swapper's own segments). ND-05.017.01 `:3681` (startup flow chart) says "If first page,
   ND-100 fill in some parameters. (PST, CNTXT BL)" — i.e. the manual claims the ND-100
   seeds the very first PST entries during startup. That seeding code has NOT been located
   in the carve; treat the ND-100-seeds-first-page claim as [MANUAL, un-carved].

## 3. Cross-references

- `PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md` — the four-site carve + PSTP closure
  this doc rests on (its §0 "UNPROVEN" caution is superseded by this verdict; its §5c stands).
- `CARVE-ANSWER-DEFMC-MPM-BASE-051302-2026-08-10.md` §6.4 — MEM-CONF/ALLOC/SSYSE.
- `MICROCODE-ANSWER-C1-PCB-PST-BUILDER-2026-07-20.md`, `MICROCODE-ANSWER-PSTP-AND-SEGMENT-2026-07-20.md`.
- `swapper\swapper-k01-deep-analysis.md`, `swapper-k01-handlers.md` — where item 2.1 of §2
  should be carved.
