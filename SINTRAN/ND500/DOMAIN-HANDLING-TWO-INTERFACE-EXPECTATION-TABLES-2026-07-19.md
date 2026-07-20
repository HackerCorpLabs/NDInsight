# Domain-Handling Test Ladder — Two Interface Expectation Tables

**For**: ND-500 BUS-INTERFACE LLM (3022 column) and ND-5000 OCTOBUS LLM (octobus column).
**From**: architect/analyst. **Date**: 2026-07-19.
**Companion to**: `ND500-DOMAIN-HANDLING-TEST-COMMAND-SEQUENCE.md` (command + console strings)
and `DOMAIN-HANDLING-ARCHITECT-BRIEFING-2026-07-19.md` (the two-program model + verified/TC split).

One command script, two low-level tables. **Console checkpoint is IDENTICAL on both interfaces;
only the low-level column differs.** Grades: **[V]** byte-verified carve/live-harness, **[?]**
working model to validate, **[TC]** TO-CARVE — do NOT implement as fact.

Verified transport facts these tables rest on:
- **3022** [V]: CS-load gated on `RSTA5` bit 9 (5CLOST); `ECSLOAD=2032B`; ACT50 =
  `LMAR5:=bank(MS)`,`LMAR5:=addr(LS)`,`LCON5:=5`; enable = `LCON5:=10`,`LSTA5:=0`,`LCON5:=1`,`SLOC5`;
  5MCST stop = `UNLC5`,`LCON5:=40`,`RETG5:=2`; MICRO-START restart = `RETG5:=0`; completion via
  level-12 GOSW (`N5MPA` dispatcher). MAR = ND-100 **word** addr (`<<1` for byte).
- **Octobus** [V]: no 3022, no bit-9 gate. ACCP over OMD 3: `CMSYSPAR 016B`→MFACK (present),
  `RTEST 060B`, `CMALI 037B`, `LPARP 021B`/`VPARP 022B`, `STARTMIC 033B`/`STOPMIC 034B`,
  `CPURES 071B`, emergencies `241B/242B/244B`. Activation = **`X5ACT := 0` write** at ORCON
  `0x0A` in the per-CPU ext block (NOT a kick; kick K-bit = preempt only). Station 70B = 56 dec.
  5MPM window base = ADRZERO (live default ND-100 byte `0x420000`).

---

## Phase A — Monitor entry (first interface contact)

| # | Cmd | Console checkpoint | 3022 low-level | Octobus low-level |
|---|---|---|---|---|
| A1 | `@ND-500` | Banner `ND-500/5000 MONITOR  Version J04 88. 6.16 / 88. 8.17` then `N500:` [V string] | First cmd after cold start ALWAYS hits CS-load gate: `RSTA5` bit9 set → `ECSLOAD 2032B` → auto-load `(SYSTEM)CONTROL-STORE:DATA` via WA/BREAK/CSCNT, verify words 0-7, `MICRO-START RETG5:=0` clears bit9. Then swapper place/start. **[V] gate; [TC] full boot seq** | ACCP handshake (full wire trace [V]): `CMSYSPAR`→MFACK, `RTEST`→Messack, `CMALI`→Messnak-7, `DISKICK`/`STOPMIC`/`CPURES`→Messack, `LPARP(0x0800)`→Messack, `VPARP`→`[00 65 96 9B 49]` (self-consistency echo §5.3.16, byte-correct — NOT a version read). **NO version/model command exists on the octobus.** Then loops back to DISKICK. No bit9 gate. **[V]** |
| A2 | `EXIT` then `@ND-500` | `@` then banner again [V] | Process release+re-alloc (RESRV/RELIS via MON 60B). CS already loaded → NO second download; bit9 stays clear. **[?]** | Process release+re-alloc; micro stays running (no re-selftest expected). **[?]** |

**A1 status differs by interface** (updated 2026-07-19):
- **3022: A1 CLEARED** ✅ — root cause was the **TERM5 bug**: the emulator manufactured
  `ND500Finished` on a bare unlock (XTER500/XACT500, locked-not-busy, no completion), which stuck
  through the CS-load and made SINTRAN retry forever. Fixed in `NDBusND500IF.cs` Terminate case,
  committed **1ec4c3df0**. `@ND-500`, VERSION, and every reachable rung now reach `N500:`. The
  Bug-B CS-load VERIFY tail remains but is **SLOW-not-STUCK**: VERSION spins ~8000 events of
  `LCON5:=0x24` / ACTIVATE `mar=0` / "PROCMSG skip mar==0" for ~30-60 s (SINTRAN issues BARE
  activates with MAR never written; the emulator correctly skips them — no message), then completes
  and returns micro ver **11930**. Ref `ND500-BUS-INTERFACE-COMMAND-LADDER-ANALYSIS-2026-07-19.md`.
- **Octobus: A1 CLEARED** ✅ (2026-07-19). "Wrong microprogram" = `EWRON 002203B` was set by the
  swapper's CS-loader (`030-S3SM5:154025-44`) **range-checking the SYSTEM/CONTROL-STORE FILE's version
  against `[026354B,027337B]` = [11500,11999]**. The disk `BIGDISK0-L.IMG` shipped a **5200 microcode
  (version `0x2B16`=11030)** as CONTROL-STORE on an ND-5800 system → below range → EWRON. **Root cause
  was a DISK FIXTURE, not emulator plumbing.** Fix: swapped the disk's CONTROL-STORE to
  `MICRO-5800-B30.DATA` (`0x2E9A`=11930, in range) via ndtool → "Wrong microprogram"=0, the 128-block
  CS download now runs over the octobus. **CORRECTION of the earlier note:** the check is against the
  microcode FILE version, NOT an emulator/ACCP-reported model value — the "make the emulator report
  module/model 0→0x38" framing was wrong. The VPARP echo was always byte-correct (never the blocker).
  Ref `CARVE-ANSWER-OCTOBUS-WRONG-MICROPROGRAM-2026-07-19.md`.
- **Octobus NEXT-TIER (not A1):** VERSION still shows `Micro program.: 0` and `Module: MB.0 ALU.3 …`;
  3RMICV returns canned values because the mailbox cache is empty — a REPORTING gap, fed by G2 config
  (0x38 / 0x2E9A) rather than hard-coded. Separate from the A1 disk fix.

→ **Phases B–G: A1 CLEARED on BOTH interfaces (2026-07-19).** 3022 cleared via the TERM5 fix; octobus
cleared via the CONTROL-STORE disk-fixture swap (5200→ND-5800 microcode). Both now walk B/C. The
remaining gate to Phase D `RUN` on both is **production machine wiring** (attach a running `CpuND500` +
a RUN/init command), NOT ISA/execution work — the engine is already built and green. See the
microengine-track scoping doc.

---

## Phase B — Read-only status (light traffic)

| # | Cmd | Console checkpoint | 3022 low-level | Octobus low-level |
|---|---|---|---|---|
| B1 | `VERSION` | Four versions: subsystem, system part, swapper, **microprogram** [V] | Subsystem/system/swapper = ND-100-cached, no bus. Microprogram = read of loaded CS word-1 version. **[TC] mechanism** | Subsystem/system/swapper ND-100-cached. Microprogram version mechanism **[TC]** (NOT VPARP — that's a self-consistency echo; likely the mailbox `3RMICV` MICFU=1 reply, version 027232B). |
| B2 | `WHO-IS-ON` | List incl. own terminal [V] | ND-100 tables only — NO bus. **[V]** | Same — NO octobus. **[V]** |
| B3 | `LIST-ACTIVE-PROCESSES` | Own process (+ swapper) [V] | ND-100 process tables — NO bus. **[?]** | Same. Note current octobus shows proc 1, magic 0, blank name (swapper not yet named). **[V]** |
| B4 | `PROCESS-STATUS` | Per-proc idle/active + CPU time (SYSTEM only) [V] | ND-100 tables. **[?]** | Same. **[?]** |
| B5 | `LIST-STANDARD-DOMAINS` | Empty on virgin system [V] | Reads standard-domain table (resident/monitor seg) — NO bus. **[TC] table addr/format** | Same. **[TC]** |

---

## Phase C — Domain lookup, no execution (description-file path)

| # | Cmd | Console checkpoint | 3022 low-level | Octobus low-level |
|---|---|---|---|---|
| C1 | `LIST-DOMAIN` (virgin user) | `DESCRIPTION FILE ERROR: DESCRIPTION-FILE` / `NO SUCH FILE NAME` [V exact strings] | **NONE** — pure ND-100 file system. Clean negative control. **[?]** | **NONE** — identical. **[?]** |
| C2 | `ND-500-LINKAGE-LOADER` (>16 chars) | `TOO LONG PARAMETER` [V] | **NONE** — parser-level length check. **[?] (locate check = [TC])** | **NONE** — identical. **[?]** |
| C3 | `LIST-DOMAIN (…FLOPPY-USER)` | Domain `LINKAGE-LOAD-H02` + start address [V] | Reads FLOPPY description file — ND-100 FS only, NO ND-500 run. **[?]** | Identical. **[?]** |

Phase C is the **cheapest green rung** — no interface work at all. Both harnesses can assert C1/C2/C3 the moment A1 clears (C1 arguably even before, since it never reaches the interface).

---

## Phase D — Place and run (THE core activation) — mostly [TC]

| # | Cmd | Console checkpoint | 3022 low-level | Octobus low-level |
|---|---|---|---|---|
| D1 | `PLACE-DOMAIN …LINKAGE-LOAD-H02` | Silent success (prompt back) [V] | Segment map (logical→physical), PC:=start, trap regs init; swapper messages queued. **[TC] MON 60B subfn order + register writes** | Same setup; swapper messages into 5MPM. **[TC] subfn order + X5BEX chain build** |
| D2 | `LIST-ACTIVE-SEGMENTS OWN` | Segments w/ logical→physical map [V] | Verifies D1 from ND-100 side — reads ND-100 tables. **[?]** | Same. **[?]** |
| D3 | `LIST-PROCESS-TABLE-ENTRY OWN` | Process desc + capabilities [V] | ND-100 tables. **[?]** | Same. **[?]** |
| D4 | `RUN` | `NLL:` prompt appears [V] — proves activation+exec+MON-call round-trip | **THE kick**: ACT50 (`LMAR5` MS/LS, `LCON5:=5`) → microcode services msg at MAR → answer → level-12 back (`N5MPA`). Then MON-call stream. **[TC] real seq — NDBusND500IF TAG protocol is FABRICATED.** ⚠ 2026-07-19: PLACE-DOMAIN reads desc+segments and loads CS/swapper CLEAN, but `RUN` returns **"NO WELL DEFINED PROGRAM IN MEMORY"** with no active segments — consistent with the emulated ND-500 never running real microcode (F5 shows MICRO PROGRAM STOPPED) + the fabricated TAG. Phase D stays pending on the domain carve AND a real microengine. | **THE activation**: `X5ACT:=0` write at ORCON 0x0A → microcode IDLE loop picks up work, walks X5BEX → answer → level-12. Kick only if preempt. MON-call stream back. **[V] mechanism; [TC] domain-specific msg** |
| D5 | `EXIT` (at `NLL:`) | Back to `N500:` [V] | Normal domain term (MON 0 path); process stays allocated. **[TC]** | Same. **[TC]** |
| D6 | `LINKAGE-LOAD-H02` alone | `NLL:` again [V] | Implicit RECOVER-DOMAIN lookup chain, then D4-style activate. **[TC]** | Same. **[TC]** |

D4 `NLL:` is the single strongest end-to-end assert (activation + execution + MON-call terminal round-trip in one). But it is gated on the [TC] PLACE-DOMAIN carve — wire it as **pending**.

---

## Phase E — Interrupt/resume/teardown — [TC]

| # | Cmd | Console checkpoint | 3022 low-level | Octobus low-level |
|---|---|---|---|---|
| E1 | Escape (during run) | Back to `N500:`; files stay open [V] | Stop/suspend reaches 500. **[TC] — differs 3022 vs octobus** | Suspend via ACCP `STOPMIC 034B` (candidate). **[TC]** |
| E2 | `CONTINUE` | Domain resumes where stopped [V] | Re-activate without re-place. **[TC]** | ACCP `CONTMIC 035B`/`RESTMIC 036B` (candidate). **[TC]** |
| E3 | `EXIT` | `@` [V] | Process term, RELIS, resources freed. **[TC]** | Same. **[TC]** |

---

## Phase F — Supervisor stress (deepest register coverage)

| # | Cmd | Console checkpoint | 3022 low-level | Octobus low-level |
|---|---|---|---|---|
| F1 | `SET-ND-500-UNAVAILABLE` | silent [V] | Gate flag, no traffic. **[V]** | Same. **[V]** |
| F2 | `STOP-ND-500` | silent [V] | CPU stop. NEXT start = full warm start (micro reload + swapper place/start). **[V] behavior; [TC] seq** | ACCP `STOPMIC`/`CPURES`; next start re-runs ACCP bring-up. **[?]** |
| F3 | `@ND-500` + run domain | `NLL:` prompt | Full cold-path activation, deterministic (repeat D). **[TC]** | Same. **[TC]** |
| F4 | `MICRO-STOP` / `MICRO-START <addr>` | silent [V] | `MPSTO` (FUNCS 034) = `UNLC5`,`LCON5:=40`,`RETG5:=2`; `MPSTA` (FUNCS 025) restart `RETG5:=0` after start-addr via `LLOW5`/`LTAG5`. **[V]** | ACCP `STOPMIC 034B` / `STARTMIC 033B`. **[V]** |
| F5 | `LOOK-AT-HARDWARE INTERFACE` | Interface register dump [V] | **DIRECT reads of 3022 registers — single best register-map validator.** Manual requires MICRO-START after. **[V]** | ACCP/station register surface dump. **[?] — confirm what maps here** |
| F6 | `LIST-TABLE LAST-N500-MSG` | Ring of last 64 msgs to ND-500 [V] | ⚠ 2026-07-19 carve: the ring is **NOT in the 5MPM window** — the F6 raw-dump sites (0x420E30 / 0x424130) are reused process/watchdog message buffers, not the ring. [ASSUMPTION] the ring lives in **`ND-500-MON:PROG`'s own ND-100 memory**; carve its `LIST-TABLE` handler. Empty terminal render = ring unpopulated (no real ND-500 traffic, no microengine). **[TC] — re-scoped off MPM.** | Same re-scope: check the octobus monitor-program ring, not `SnapshotMpmAccess()`. **[TC]** |
| F7 | `GET-FLAG` / `SET-FLAG` | flag value round-trip [V] | ✅ RESOLVED (carve 2026-07-19): `RFLAG=100B`/`SFLAG=101B` **do NOT cross the 3022** — byte-proven `FUNCS[100B]=FUNCS[101B]=ERRFP=141574B` (no ND-500 op) in `030-S3SM5.bin`; handler `RRFLAG/WWFLAG` maps the process data segment via `M1MEXY` and reads `FF500=166004B` / writes `FT500=166002B` as plain ND-100 memory — no IOXT/ACTIVATE/message. Caller: MON 60 `A=100B/101B`, procno `,X 6`, flag `,X 7`. **Emulator correct.** **[V]** | Check the same on octobus (likely also resident). **[?]** |
| F8 | `SET-ND-500-AVAILABLE` | silent [V] | Gate reopened. **[V]** | Same. **[V]** |

**F7 RESOLVED (carve 2026-07-19):** GET/SET-FLAG are **legitimately ND-100-resident** (byte-proven,
row above) — NOT an interface transaction at all, so they were never going to be a "live round-trip."
The only periodic 3022 traffic is the **`3RMICV` watchdog** (MICFU=1, SENDE=−1=watchdog marker,
timer-driven `LTTMR=000023B`, re-sent on each ANSWER — NOT per-command; that's why it lands
incidentally in random command windows). Clean round-trip: lock → `LoadMarX2` MS+LS → MAR=`0x212098`
=byte `0x424130` → `LCON5:=5` ACTIVATE → MSGHDR → PROCMSG → ND500Finished. The `3RMED=10B`
(=8) that answered `5ERANSWER` is **NOT a bug** (bus session confirmed): it's the DEFAULT return for
an *unhandled* MICFU — the emulator's real `ResidentRead` is enum 11 = `13B`/`MSG_RESIRD`, which IS
handled; MICFU 8 legitimately falls through (plausibly a memory-not-present sizing probe).
Definitively resolving it needs the microengine (real ND-500 memory); shared servicer left untouched.
**There is no small command that deliberately crosses the 3022 — the watchdog is the only live
signal, and it's already correct.** Emulator divergence for the whole F7/poll area is cosmetic (poll
not labeled "watchdog"). Full: `CARVE-ANSWER-3022-FLAG-POLL-RING-2026-07-19.md`.

---

## Phase G — Domain CREATION via NLL (description-file WRITE path)

Runs entirely as ND-500/5000 execution (NLL is a domain), so every NLL command also generates
MON-call traffic. Console + interface expectation = same shape as D4's MON-call stream. **[?]**

```
N500: RECOVER-DOMAIN (…FLOPPY-USER)LINKAGE-LOAD-H02   → NLL:
NLL:  COPY-DOMAIN (…FLOPPY-USER)LINKAGE-LOAD-H02,"LINKAGE-LOAD-H02"
NLL:  LIST-DOMAIN            (new domain visible)
NLL:  EXIT                   (description file finalized)
N500: LINKAGE-LOAD-H02       → NLL:   (run locally created domain — proof)
```

Assert: after COPY-DOMAIN+EXIT, `LIST-DOMAIN` on the local user succeeds (proves the
description-file WRITE path) and the local domain runs. The installer cross-user copy
(`IN-NLL-XX-H02:PROG` → DOMAIN-USER) is a **separate open mystery [TC]** — do not model it yet.

---

## Minimum "domain handling works" green (both interfaces)

1. A1 banner + `N500:` (⇐ **3022 CLEARED** via TERM5 fix 1ec4c3df0; octobus still gated).
2. C1 exact error pair on virgin user (clean negative control).
3. D4 `NLL:` prompt (activation + exec + MON-call round-trip) — gated on [TC] carve.
4. D2 non-empty OWN segment mapping.
5. G: COPY-DOMAIN+EXIT → LIST-DOMAIN succeeds and local domain runs.
6. F6 ring matches captured interface messages (strongest cross-check; format [TC]).

## Suggested build order (both peers, same shape)
**A1 (unblock per interface) → C1/C2/C3 (no-traffic) → B (read-only) → F4/F5/F6 (register + ring)
→ D (activation, after carve + real microengine) → E → G.**
- F7 is NO LONGER a build-order rung (3022 shows GET/SET-FLAG are ND-100-cached; interface status
  is a [TC] carve question). The live cross-interface signal to assert on is the periodic
  `ReadMicroVersion`/`ResidentRead` poll, not a single command.
- Phase D+ stays PENDING on the outstanding domain carve. On 3022 it ALSO needs a real microengine:
  PLACE-DOMAIN loads clean, but RUN = "NO WELL DEFINED PROGRAM IN MEMORY" (no microcode + fabricated
  TAG). Green D4 requires the ND-500 to actually execute.
