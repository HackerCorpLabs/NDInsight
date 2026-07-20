# Architect Briefing — Domain Creation / Loading and how ND-500-MON drives it

**For**: the ND-500 BUS-INTERFACE LLM and the ND-5000 OCTOBUS LLM.
**From**: architect/analyst session. **Date**: 2026-07-19.
**Sources**: `CARVE-PROMPT-DOMAIN-CREATION-AND-LOADING.md`,
`ND500-DOMAIN-HANDLING-TEST-COMMAND-SEQUENCE.md` (both in this folder), the byte-verified
carve reference, and the `nd-500-bus-interface` / `octobus-nd5000` skills.

Grades used throughout: **[V]** byte-verified carve, **[?]** working model to validate,
**[TC]** TO-CARVE (not yet byte-verified — do NOT implement as fact).

---

## 1. The one thing to internalize: the Monitor is TWO programs

`N500:` domain commands split across two code bases, and **which one handles a command decides
whether the interface is touched at all**:

- **(a) The background program** — an ND-100 RT program. It prints `N500:`, parses commands,
  and does ordinary SINTRAN **file I/O** on the description files (`DESCRIPTION-FILE:DESC`,
  `:PSEG`/`:DSEG`/`:LINK`). Commands that only read/write domain metadata never leave the
  ND-100. **[?]** (the split is real per the skill; the exact per-command routing is [TC]).
- **(b) The SINTRAN-resident part** — `MON 60B` → `N500M` → `5IFUNC[subfn]` → `FUNCS[subfn]`
  → 3022/5MPM (or octobus) → answer. **[V]** This is the interface path. Only reached when a
  command actually **activates, places, runs, stops, or reads live hardware**.

**Consequence for both harnesses**: a large part of the domain command surface (lookup,
listing, parser checks) is validated with **zero bus/octobus traffic**. Do not expect — or
fabricate — interface activity for those. The interface only lights up at PLACE/RUN and the
supervisor commands.

---

## 2. The verified command→interface chain (for the activating commands) [V]

```
N500: <cmd>
  -> MON 60B   (A -> 1-entry addr list; params[0] = subfn | COMAUTO bit15)
  -> MCTAB[60B] = N500M @030416B        (NOT via GOTAB; MCTAB@005620B is the real dispatch)
  -> range check <= 177B (COMAUTO stripped)
  -> 5IFUNC[subfn] handler -> 5NOPAR common -> FPT2ENTRY
  -> ND-500 SYSTEM MONITOR 5FP2E -> FUNCS[subfn] @142031B (128 entries, byte-verified)
  -> 3022 / 5MPM mailbox -> microcode services -> answer
```

Return polarity **[V]**: SKIP (P+2) = success / A=0; DIRECT (P+1) = error / A=status.
Auto-retry gate **[V]**: gateway re-issues MON 60B while `A == 2032B (ECSLOAD)` or `4017B` →
the emulator MUST keep `RSTA5` bit 9 (5CLOST) clear once the store is loaded, or every
command loops "Loading Control Store".

**What is NOT verified [TC]**: the specific FUNCS/5IFUNC subfunction *numbers and parameter
blocks* for PLACE-DOMAIN, RECOVER-DOMAIN, LIST-DOMAIN, LIST-STANDARD-DOMAINS,
DEFINE-STANDARD-DOMAIN, and the DESCRIPTION-FILE record layout as the code reads it. The carve
prompt is out for exactly these. **Until that lands, neither peer should hard-code a
PLACE-DOMAIN subfunction sequence and call it done.** Known subfunction names so far: RESRV
(process reserve), RELIS (release), RFLAG=100B, SFLAG=101B, RSTAT=041B, RMVER=057B, GETCP=170B.

---

## 3. Domain CREATION vs domain LOADING — where each lives

- **Creation (write path)** = NLL running as a domain issues `COPY-DOMAIN` / description-file
  writes. That is *NLL executing on the ND-500/5000*, i.e. it only works **after** activation
  succeeds. So creation is downstream of loading — you cannot test G until D works. **[?]**
- **The installer cross-user copy** (`IN-NLL-XX-H02:PROG` copying a domain to DOMAIN-USER)
  is an **open mystery [TC]** — the manual gives NLL no cross-user path. This is the silent
  install failure (gotcha G12). Do NOT model it until carved; flag any emulator behavior that
  "succeeds" here as unverified.
- **Loading (activation)** = PLACE-DOMAIN sets up segment mapping + PC + trap regs, then RUN
  fires the interface kick and the MON-call round-trip begins. **[?]/[TC]** for the exact
  MON 60B order and register writes.

---

## 4. The command ladder = one script, two expectation tables

The full phased script (A–G) is in `ND500-DOMAIN-HANDLING-TEST-COMMAND-SEQUENCE.md`. Command
surface + console checkpoints are **identical** for 3022 and octobus; only the low-level
expectation differs. Summary of what each phase actually exercises:

| Phase | What it really tests | Interface traffic | Grade |
|---|---|---|---|
| A `@ND-500`/`EXIT`/`@ND-500` | Monitor start, process alloc/release, **first-contact auto CS-load + swapper start** | **HEAVY** — this is the CS-load gate + activation | [V] gate; [TC] full seq |
| B VERSION/WHO/LIST-ACTIVE/… | Read-only status | mostly ND-100 tables; VERSION reads micro version [TC mechanism] | [?] |
| C LIST-DOMAIN (virgin), TOO-LONG, LIST-DOMAIN(floppy) | Description-file path + parser | **NONE expected** (pure ND-100 FS) | [?] |
| D PLACE→LIST-SEG→LIST-PTE→RUN | **The core activation + MON-call round trip** | 3022 activation kick / octobus X5ACT+kick; level-12 back | [TC] |
| E Escape/CONTINUE/EXIT | suspend/resume/teardown | stop reaches the 500 — differs 3022 vs octobus | [TC] |
| F STOP-ND-500, MICRO-STOP/START, LOOK-AT-HARDWARE INTERFACE, LIST-TABLE LAST-N500-MSG, GET/SET-FLAG | supervisor register surface + message ring | **DIRECT register reads** + RFLAG/SFLAG (100B/101B) | [V] regs; [?] ring fmt |
| G RECOVER→COPY-DOMAIN→LIST→run | description-file **write** path | full NLL MON-call stream | [?] |

**Two strongest cross-checks** (call these out to both peers):
- **F5 `LOOK-AT-HARDWARE INTERFACE`** — direct 3022 register dump. Single best validator of
  the register map. (Octobus: the equivalent is the ACCP/station register surface.)
- **F6 `LIST-TABLE LAST-N500-MSG`** — the monitor's own ring of the last 64 messages to the
  ND-500. Assert it against the emulator's captured mailbox writes. Ring format is **[TC]**.

---

## 5. THE GATING DEPENDENCY — read this before planning any domain test

**Phase A1 (`@ND-500`, first contact) is exactly the CS-load path where the current boot
harness stalls (the Bug-B residual).** First ND-500 process → auto-load microcode + start
swapper → CS-load gate (RSTA5 bit 9). The classic ND-500 has NO microcode ROM, so the first
command after cold start ALWAYS triggers "Loading Control Store" — that is correct behavior,
not a misdetection. **[V]**

So the domain ladder is gated: **B–G cannot be reached until first-contact/CS-load completes
cleanly in the harness.** The residual "Loading Control Store" stall must be closed first.
Do not chase phase D/E/G mechanics while A1 still hangs — you'd be testing on a dead interface.

- ND-5000/octobus note **[V]**: SAMSON loads microcode via ACCP `LOCSD/LOCSM/STARTMIC` over
  octobus — **no 3022, no bit-9 gate**. So the octobus harness's first-contact path is a
  different mechanism and may clear this rung independently of the 3022 fix. Worth confirming
  which harness is actually blocked.

---

## 6. Concrete asks

### To the ND-500 BUS-INTERFACE LLM
1. Confirm whether the current boot-harness stall is at A1 first-contact CS-load, and report
   the post-ECCR-fix MAR and where status stalls now (this is the open Bug-B residual item).
2. Once A1 clears: stand up phases A–C in the boot harness first — they need **no** new
   FUNCS work (parser + description-file file I/O + the gate). That gives a green behavioral
   ladder without waiting on the domain carve.
3. Do NOT implement PLACE-DOMAIN / RECOVER-DOMAIN FUNCS subfunctions as fact — those are [TC].
   Wire phases D/E/G as *pending* asserts gated on the carve deliverable.
4. Wire F5/F6 as the register-map and message-ring cross-checks.

### To the ND-5000 OCTOBUS LLM
1. Run the SAME command script through the octobus boot harness — the command/console asserts
   are identical; only the low-level table differs (ACCP/X5ACT/X5FIF + kicks 1/3/6 vs 3022).
2. Confirm the octobus first-contact CS-load path (ACCP LOCSD/LOCSM/STARTMIC) reaches a clean
   `N500:` — since it bypasses the bit-9 gate, phase A may pass on octobus even if 3022 is
   still blocked. Report which.
3. Same [TC] caution for phases D/E/G.

### Shared
- The DESCRIPTION-FILE layout, PLACE-DOMAIN subfunction order, standard-domain table, and the
  installer cross-user copy are all **[TC]** and covered by the outstanding carve prompt.
  Treat every phase-D+ expectation as provisional until that carve returns byte-verified.
- Keep ONE command script, TWO interface-expectation tables (this is already how the octobus
  boot harness is structured).
