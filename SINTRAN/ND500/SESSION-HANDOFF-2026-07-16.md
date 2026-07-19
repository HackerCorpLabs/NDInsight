# ND-500 / SINTRAN L07 - Session Handoff (2026-07-16)

Handoff for continuing the ND-500 monitor/swapper reverse-engineering and the SINTRAN
MON-call ground-truth work that feeds the nd500x emulator. All paths absolute.

Repo root: `/mnt/e/Dev/Ronny/NDInsight` (Windows `E:\Dev\Ronny\NDInsight`).
Original binary package (external, DO NOT WRITE): `/mnt/d/ND/500/ND-500(0) System Package for SINTRAN IIIVSX L/`.

---

## 1. What was delivered this session (all on disk)

### ND-500/5000 MONITOR J04 (the ND-100-side operator front end)
- `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/nd-500-mon/` (315 files) - disassembly, full analysis
  (`nd-500-mon-j04.prog.md`, now 1530 lines with the bank-2 command table), big-endian bank images
  for Ghidra, recovered symbol residue, control-store/DMA debug handoff, bring-up feedback, and
  `mon60-callers/` = INDEX + SUBFUNCTION-TABLE + **101 per-subfunction folders** (each .asm + .pseudo.c + README).
- HEADLINE (byte-proven): the monitor never touches the bus; it drives the ND-500 through exactly ONE
  `MON 60` at `146256B`. Front door = `MON 60`/N500M -> FPT2ENTRY ("ENTER ND-500 SYSTEM MONITOR") -> 5MPM.

### ND-500 swapper domain (SWAPPER-K01)
- `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/swapper/` - `swapper-k01-deep-analysis.md` (role + gates),
  `swapper-k01-handlers.md` (the 29-entry function-code table decoded), pseg/dseg analyses, binaries.
- Role (byte-proven): ND-500-side paging/swap worker DOMAIN, a CLIENT of SINTRAN (RIOM DMA intake,
  29-way private dispatch on a function code, MON 377B = segment-31 monitor call 255 = N5SWAP trapped OUTWARD).
- Routing: the function code (DSEG `0x240B8`, index `0..0o34` = 0..28) is carried by SWMSG field **SWPST**
  (`SWPFU` refuted). ND-100 side in `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/ND500-SWAPPER-ANALYSIS.md` section 12.

### Bus-interface validation (for the RetroCore CPU-connect goal)
- `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/ND500-BUS-INTERFACE-VALIDATION.md` - 3 contradictions in the
  reference doc, gap list, and a 7-row emulator bring-up checklist. Crash fix stated as a PROVEN identity:
  the emulator's TAG-OUT DMA must route through the SAME physical-address logic it already uses for
  `LDATX`/`LDDTX`/`STATX` (both reach the same 5MPM mailbox word) - not a separate `bank*0x10000` calc.
  See also the control-store/DMA crash handoff under `nd-500-mon/`.

### SINTRAN MON-call ground truth for nd500x (the "5 questions" + device-0)
- `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/CARVE-ANSWERS-Q1-Q3-DEVICE0-CMDBUF.md` and `-Q4-Q5-RSIO-DESCF.md`.
- The prior "NOT FOUND in NPL" answers were a search-scope error: the NPL tree is the `s3vs-4` build,
  missing the workers; the L07 CARVE has them. Segment-layer map (INBT/M1/CPNT/RSIO/DESCF -> segments-ref .asm).

### Repo hygiene
- All absolute repo paths converted to repo-relative (54 files); superseded swapper docs moved to
  `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/old/`; hub `ND500-STATUS-AND-INDEX.md` and READMEs updated.
- Everything is UNCOMMITTED on branch `xmsg-protocol-docs-and-library` (checkpoint commit `d834435` swept an
  earlier batch; the rest is working-tree). "Clean up commits later" per the user.

---

## 2. The device-0 / INBT thread (the emulator blocker) - state

Question: what does a `MON 1B INBT` read of logical device 0 (the command buffer) return after the line is
consumed? nd500x's handler returns raw `-1`/EOF, which busy-spins the ND linker.

Established this session (byte-proven via the nd100x native-breakpoint live trace - see the rig in memory):
1. **INBT is real code at `032471B`** (breakpoint fired during an L07 boot). The `1B-InByte` "CORRECTED" note
   that withdrew it as a data artefact OVER-corrected - restore INBT=`032471B` as the MON 1B level-4 worker
   (distinct from `MCTAB[1B]=YFGET=026576B`).
2. **Two sentinels, two layers:** INBT tests `-1`/`177777B` (device "no byte available"); the `47B`/`0x27` is
   the command-processor string end, which INBT never sees. Parking `CPNT` on `47B` and breaking in INBT would
   watch the wrong sentinel.
3. **The device-0 line ends in CR (`015B`/`0x0D`), not `47B`.** The command processor converts the `47B` source
   marker to CR when preparing the buffer (`050773 SAA 15; SBYT`), then resets `CPNT`. So a device-0 read yields
   `args + CR`. This indicts the current handler's raw `-1`/EOF; the fix is to carry the CR.
4. **Q1b REVERSED:** the boot INBT path is the hardware `IOXT` path (0 accesses to `144xxx`), so device 0 must
   be a SEPARATE memory path - the emulator's `if (device_no == 0)` looks LEGITIMATE, not a fiction. This
   reverses the earlier static inference. (Lesson: a correction can be wrong too; do not act on an inference.)

OPEN (the nd500x CPU team is closing this on THEIR side, not blocked): what a device-0 read returns AFTER the
CR (EOF / suspend / repeat-CR). That is a READ-side mechanism not encoded in the buffer; they will trace the
ND-500 linker's post-CR reads at `1B INBT @ B004E759` (no ND-100 guest needed). If their read-side disagrees
with "args + single CR at line end", that is a write-side-vs-read-side divergence to reconcile against the carve.

All of the above (Q4 byte-proof, the two-sentinel split, CR-termination) is recorded in the `nd500-mon` skill
(`/home/ronny/.claude/skills/nd500-mon/SKILL.md`).

---

## 3. Collaboration with the nd500x CPU/emulator team

- nd500x (`/home/ronny/repos/nd500x`) emulates the ND-500 side and services SINTRAN MON calls in `libmon`.
  This session I acted as the CARVER (L07 byte-truth) feeding their handler work.
- **Skill co-ownership split (agreed):** on `nd500-mon/SKILL.md`, the carver owns the carve / SINTRAN-truth
  sections (device 0 / INBT / command buffer / sentinels / MON contracts); the nd500x team owns the
  implementation sections. One authoritative account per fact + a pointer, never a duplicate (two agents editing
  it produced decaying opposite accounts within an hour).
- They took the **50B OPEN descriptor bug** (empty filename from `[len=5, ptr=0xB0001E7F]`, and `OPEN 'Linker'
  -> ./GUEST/LINKER` failing). Carve-side prior art to feed them:
  `/mnt/d/ND/500/nd-linker/linker-b01-startup-filenames.md` (the names are static literals + a PLANC descriptor
  path) and `.../re/mon-analysis/50B-OpenFile/`.

---

## 4. Open items / next steps

1. **Device-0 post-CR** - awaiting the nd500x team's `B004E759` read-side trace; reconcile if it diverges from
   "args + CR". (Not blocking.)
2. **50B OPEN descriptor bug** - nd500x team leading; offer the linker-startup-filenames carve if they want it.
3. **Commit hygiene** - working tree on `xmsg-protocol-docs-and-library` is uncommitted (per user "later").
4. **RetroCore bring-up** - the validation doc's checklist + the MAR/LDATX identity are the actionable path to
   connecting the ND-500 CPU; the control-store/DMA crash handoff has the ranked hypotheses.

---

## 5. Key tooling facts (see memory for the live-trace rig)

- `nd100-dis` reads LITTLE-endian; `.prog`/segment binaries are BIG-endian -> byte-swap before disassembly.
  `nd100-dis -s` takes decimal or `0x`hex, NOT bare hex.
- Live SINTRAN L07 trace: `nd100x` NATIVE breakpoint (NOT DAP - libdap `attached`-gate defect), boot `SMD0-L.IMG`.
- The NPL tree is `s3vs-4` (missing S3FS/S3CP/level-4 workers); L07 truth is the segment carve, not the NPL.
