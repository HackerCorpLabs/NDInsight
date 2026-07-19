# ND-500 3022 Level-12 Interrupt: Hold-Until-IDENT Challenge (R2-8 reversal / R2-9)

**Date:** 2026-07-17
**Author:** Architect session (implementer of `Nd500MicrocodeServicer` + `NDBusND500IF`)
**Status:** DOCUMENTED, DEFERRED — product change NOT yet made; awaiting real/live test
results before touching the interrupt model. Decision of record: *product-only change,
canary-test rework handed to the bus session* (see "Decision" below).

## 0. Plain-language explanation (read this first)

**This is the STANDARD interrupt model, not an ND-500 invention.** A normal interrupt
source has a `pending` bit set on an event and cleared by IDENT, with `line = pending AND
enable`. That is all we want here too. It only LOOKS like a special approach for two
reasons, both about sloppy current code rather than the ND-500 being different:

1. The ND-500 IF today has NO real `pending` bit — it fakes one with the `Finished` status
   bit (`line = enable AND Finished`). "Add a pending latch" just means give it the normal
   bit it never had.
2. The other RetroCore controllers also have no explicit `pending` bit — e.g. the paper-tape
   reader uses `line = enable AND ReadyForTransfer` and gets away with it because
   `ReadyForTransfer` NATURALLY behaves like a pending bit: set when a byte arrives, cleared
   only when the driver services (reads) it. Nothing disturbs it in between.

**The ONE real ND-500 wrinkle:** its natural status bit `Finished` is cleared by the next
`activate`, and SINTRAN wraps EVERY tag strobe in an activate (`LCON5:=44B`). So `Finished`
gets wiped out from under an interrupt that has not been acknowledged yet. Paper tape's
`ReadyForTransfer` is only cleared when the driver actually services it (safe to reuse as
pending); the ND-500's `Finished` is cleared by an UNRELATED activate (NOT safe to reuse).
That is the entire reason the ND-500 needs an EXPLICIT, separate pending bit while the other
controllers can cheat and reuse their status bit. Same interrupt model; the ND-500 just
can't let its status bit pull double-duty.

"Re-assert on re-enable" is NOT extra machinery either — with `line = pending AND enable`
and a real latch it falls out automatically (disable -> `pending AND 0 = 0`; re-enable ->
`pending AND 1 = 1`). It is a consequence, not a feature.

**Stripped to the essential (R2-9):** an interrupt not yet IDENT'd must not be silently
thrown away by an unrelated `activate`. The current code throws it away because `activate`
clears `Finished` and `Finished` was standing in for pending. Fix = a normal pending bit
that only IDENT / master-clear can clear.

### The three signals (the mental model)

There are THREE things, not two. Everyone starts with two — *interrupt enable* and *the
interrupt flag* (the line to the CPU) — and that is exactly why the old model is wrong. You
need a third bit in the middle.

1. **Enable** — a switch the DRIVER flips: "do I want to be interrupted right now?"
   (LCON5 bit 0.)
2. **The interrupt line / flag** — the wire the CPU sees RIGHT NOW: "there is an interrupt
   this instant." (What `SetInterruptBit` drives = `Level12Output`.)
3. **Pending** — a MEMORY bit: "an event happened and nobody has dealt with it yet." This
   is the new third thing. Not the enable, not the line.

The line is never a memory; it is a live computation: **line = pending AND enable**.

Analogy — the ND-500 is a coworker who finishes a job and wants to tell you (the CPU):
- **Pending** = a sticky note on your desk ("job done, deal with me"). Stays up until you
  actually handle it.
- **Enable** = your office door, open or closed. Closed = "not now."
- **The line** = the coworker knocking. He knocks only when a note is up AND the door open.

Walk it:
- Job finishes, door OPEN -> note up, he knocks. `pending=1 enable=1 line=1`.
- Job finishes, door CLOSED -> note STILL goes up, no knock. `pending=1 enable=0 line=0`.
  **The event is remembered even though nobody was interrupted** (the old model lost this).
- You OPEN the door -> note's still there -> he knocks. **This is "re-assert."** Opening
  the door made no new event; it un-gated the one already remembered.
- CLOSE mid-knock -> knocking stops, note stays. OPEN again -> knocks again. Every time the
  door opens he knocks, because the note never came down.
- You HANDLE it (IDENT) -> take the note down (`pending=0`); on the 3022 you also close the
  door (`enable=0`) and he stops. Now there is nothing to re-assert until a NEW job puts up
  a NEW note.

**The hold/re-assert rule in one sentence:** toggling enable off and on never erases the
memory of the event; it only gates whether the line is live right now. Only handling it
(IDENT) or master-clear erases the memory.

**Why two things (`enable AND Finished`) can't do it:** `Finished` means "the last job's
result is done" — a status the driver reads — and it does NOT behave like a sticky note:
- AFTER you handle it, the note should be gone but `Finished` is still 1 -> `enable AND
  Finished` would knock AGAIN for a job you already handled. Wrong.
- WHEN a new job starts, activate clears `Finished` but you have not picked up the previous
  result -> `enable AND Finished` would DROP the knock the instant a new job starts. Wrong.
Those are the two states `Finished` cannot represent ("handled but still finished" and "new
job started but old one unacknowledged"), so `pending` must be a SEPARATE bit you can raise
and lower independently. `pending` answers "has the CPU acknowledged this?"; `Finished`
answers "what is the operation status?" — different questions.

---

Cross-refs:
- Bus session write-up: `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\R2-VALIDATION-FROM-BUS-SESSION-2026-07-17.md` **Addendum 2** (the R2-8 reversal + R2-9 request).
- Product code: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusND500IF.cs`
  (`CheckTriggerInterrupt` @~1251, `SetOperationComplete` @~1506, `LoadControlRegister`
  case @~1840, `MasterClear`/`Terminate` cases, `Reset` @~1776).
- Base interrupt plumbing: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDeviceBase.cs`
  (`SetInterruptBit` @~190, `IDENT` @~223 — the line bit is `InterruptBits`, IDENT clears it).
- Tests affected: `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND500\nd500if\Nd500R2ReviewTestPackage.cs`
  (both `ClassicCanary_*` tests + `ClassicCanary_IntEnableOff_AsyncFinishedSets_ButNoLevel12`).

---

## 1. The hardware ruling (authority: Ronny, 2026-07-17)

ND-bus interrupts are **held until IDENT**:

- The level-12 **request latches at answer time** (when the operation completes /
  finished is set). It does NOT re-derive from `Finished` on every poll.
- Only **IDENT** (the ND-100's level-12 ISR reading the ident) or a **master-clear**
  may clear the latched request.
- The physical level-12 **line** is the *continuous gate*:
  `line = (CONTROL bit 0 int-enable) AND (latched request)`.
- Therefore: **disable-while-pending drops the line but keeps the request; re-enable
  RE-ASSERTS the line.** This is *level* semantics, not *edge* semantics.

This REVERSES the earlier R2-8 verdict ("late int-enable does not retro-raise = edge
semantics"). Edge was wrong; the bus holds and re-asserts.

## 2. What the emulator does today (the bug: R2-9)

`CheckTriggerInterrupt()` computes the line fresh on every call:

```csharp
bool doInterrupt = (statusRegister & InterruptEnabled) != 0
                && (statusRegister & ND500Finished)  != 0;
SetInterruptBit(doInterrupt);
```

Consequences vs the ruling:
- There is **no latched "request"** separate from the line — the code uses `Finished`
  as a proxy for "request pending".
- The **activate path** (`LoadControlRegister`, activate branch) does
  `ClearStatusBits(ND500Finished)` and then calls `CheckTriggerInterrupt()`, so a
  still-unacknowledged level-12 from a *prior* completion is **silently dropped** the
  moment a new activate clears `Finished`. **This is R2-9.**
- `Finished`-as-proxy is also wrong the other way: after an IDENT clears the line,
  `Finished` is still set (the driver reads it), so re-deriving `line = enable && Finished`
  would **spuriously re-raise** an already-acknowledged completion. A separate latch is
  required precisely so IDENT can clear the request without disturbing `Finished`.
- The gate is **not level-evaluated on plain LCON5 bit-0 writes**: `CheckTriggerInterrupt`
  is only called inside the *activate* branch, so a bare `LCON5:=1` (enable, no activate)
  does not re-assert a held request.

## 3. The intended fix (R2-9 + level-gate) — NOT yet applied

1. Add a latched field, e.g. `private bool _level12Pending;` — the request, modelled
   **separately from the line** (bit 12 of `InterruptBits`).
2. **Latch at completion:** `SetOperationComplete()` (and the `Terminate` case, which
   sets `Finished`) set `_level12Pending = true`, then re-evaluate the line.
3. `CheckTriggerInterrupt()` becomes a pure **line evaluator** and NEVER writes
   `_level12Pending`:
   ```csharp
   bool line = (statusRegister & InterruptEnabled) != 0 && _level12Pending;
   SetInterruptBit(line);
   ```
4. **Level-evaluate on LCON5 bit-0 writes:** call the line evaluator on every
   `LoadControlRegister` write after mirroring bit 0 into STATUS (not only in the
   activate branch), so enable transitions re-assert / drop the line.
5. **IDENT clears the request:** override `IDENT` in `NDBusND500IF` to
   `if (level == InterruptLevel) _level12Pending = false;` then `base.IDENT(level)`.
6. **Master-clear / Reset clears the request:** `_level12Pending = false` in `Reset()`
   (the MCLR5 read/write strobe paths already call `Reset()`).

## 4. Why it can't be a product-only edit (the test coupling — VERIFIED)

Both `ClassicCanary_*` tests in the bus session's committed package fake "the ND-100
took the interrupt" by writing the line bit directly:

```csharp
nd500IF.InterruptBits = 0;   // shortcut for "the level-12 ISR ran"
```

Under the **old** model that was sufficient (the line was recomputed from `Finished`,
which the next activate cleared). Under the **held-until-IDENT** model, `_level12Pending`
SURVIVES that shortcut — so the next LCON5 activate re-asserts level 12, and the enabled
canary's assertion

```csharp
Assert.That((ushort)(nd500IF.InterruptBits & LEVEL12), Is.Zero,
    "no completion interrupt before the process stops");   // line 181
```

would **FAIL**. To stay faithful, those shortcuts must become **real** acknowledges,
`nd500IF.DbgIdent(12)` (which clears `_level12Pending`). That is:
- both canaries' `InterruptBits = 0` sites (enabled canary lines ~157, ~174; OFF canary
  lines ~222, ~246) → `DbgIdent(12)`, AND
- the R2-8 flip the bus session already authorized: in
  `ClassicCanary_IntEnableOff_AsyncFinishedSets_ButNoLevel12`, the final
  `Is.Zero` ("late int-enable does not retro-raise") becomes `Is.EqualTo(LEVEL12)`
  ("re-enable re-asserts the held request"), with the comment updated from
  edge-semantics to hold-until-IDENT.

The bus session authorized only the single R2-8 assert flip; the `DbgIdent` rework of
their canary shortcuts is beyond that and touches their committed file under the freeze.

## 4b. Comparison with the other NDBus controllers (paper tape etc.) — "is the house model enough?"

**Verified 2026-07-17** by reading the other controllers. The ND-100 house interrupt
pattern, used by **every** other NDBus device, is:

- Line = `enable AND <device-ready condition>`, recomputed on control writes / status
  reads / completion (e.g. paper-tape reader `SetInterruptBit(enable && ReadyForTransfer)`
  at `NDBusPapertapeReader.cs:289/305/318`).
- **`IDENT` is overridden to clear the `InterruptEnabled` status bit**, then calls
  `base.IDENT(level)` (which clears the line bit). Confirmed in `NDBusPapertapeReader.cs:324`,
  `NDBusLinePrinter.cs:303`, `NDBusMagTape.cs:102`, `NDBusPaperTapeWriter.cs:251`,
  `NDBusFloppyPIO.cs:1103`, `NDBusRTC.cs:413`, `NDBusTerminal.cs:751`. The base class
  documents this as the intended override (`NDBusDeviceBase.cs:219`:
  *"Override in implementation to clear 'InterruptEnable' flag."*).

**`NDBusND500IF` is the ONLY NDBus controller that does not override `IDENT`.** That is the
single concrete deviation from the house pattern, and it is why the R2-8 (level-vs-edge)
symptom exists at all.

What the house pattern DOES deliver for us, for free, once `NDBusND500IF` adopts it:

- **Hold + re-assert on re-enable (R2-8, the reversal):** the ready condition survives a
  disable, so `disable -> line drops, condition stays; re-enable -> recompute -> line
  re-raises`. That is EXACTLY Ronny's "re-enable re-asserts". Fixed by (a) overriding
  `IDENT` to clear `InterruptEnabled`, and (b) recomputing the line on every LCON5 bit-0
  write.
- **Durable IDENT:** clearing `InterruptEnabled` on IDENT means the same completion will
  not spuriously re-fire until the driver re-enables — matching every other controller.
- **Test compatibility:** because the line stays `enable && Finished` (no separate
  persistent latch), the canaries' `InterruptBits = 0` shortcut still behaves — the next
  activate clears `Finished` and the line reads 0. **No canary rework needed for this
  half.** (Contrast the separate-latch design in section 4, which breaks it.)

What the house pattern does NOT deliver:

- **R2-9 (activate must not drop a pending, unacknowledged level):** the house model
  recomputes from a *condition* (`Finished` here) that the activate path CLEARS
  (`ClearStatusBits(ND500Finished)`), and the paper-tape model has the same shape (a new
  read activate clears `ReadyForTransfer`). So the house pattern still drops a pending
  level on activate. Fixing R2-9 STRICTLY requires the request to be a **latch that
  activate does not clear** — i.e. separate from `Finished` (section 3's `_level12Pending`)
  — and THAT is the only piece that forces the canary `DbgIdent` rework (section 4).

**Conclusion — the problem splits cleanly into two:**

| Sub-issue | Fix | Cost | Test rework | Impact |
|---|---|---|---|---|
| **R2-8** level-vs-edge reversal | Adopt house pattern: override `IDENT` to clear `InterruptEnabled` + recompute line on LCON5 bit-0 write | Small, consistent with 7 controllers | **None** | Correct re-assert semantics |
| **R2-9** activate drops pending level | Separate `_level12Pending` latch that activate does not clear | Larger, bespoke | Canary `InterruptBits=0` -> `DbgIdent(12)` (bus session's file) | ~nil today (masked by Path B + MP:3083 + TTMR) |

So: **the paper-tape/house model IS enough for R2-8 and is the right way to fix it** (it
removes a genuine inconsistency — ND500IF is the lone controller not clearing enable on
IDENT). It is **NOT enough for R2-9**; that one still needs the latch. The two can land
independently.

## 5. Decision of record (2026-07-17)

**DOCUMENTED and PARKED — await real test results before any code change** (Ronny,
2026-07-17). No product edit made this session. The plan, once results support it:

- **R2-8 half (house-pattern IDENT override):** land as an architect product-only change
  (override `IDENT` in `NDBusND500IF` to clear `InterruptEnabled`; recompute the line on
  every LCON5 bit-0 write). No canary rework needed — the existing tests stay green. This
  is the cheap, consistent, low-risk half and removes the lone-controller deviation.
- **R2-9 half (separate latch):** decision "product-only, canary rework to the bus
  session" — the architect lands the `_level12Pending` latch; the bus session converts its
  committed canary `InterruptBits = 0` shortcuts to `DbgIdent(12)` and flips the R2-8
  assert. Held because it breaks their suite until they do, and impact is ~nil today.
- **No direct hardware oracle exists (Ronny, 2026-07-18):** there are NO TPE test programs
  for the ND-500 bus interface, so the disable-while-pending -> re-enable -> IDENT sequence
  CANNOT be forced at the register level on real hardware. The only observation channel is
  running the ND-100 machine and issuing `N500:` (ND-500-MON) commands and watching what
  SINTRAN's OWN 3022 driver does. Normal `N500:` traffic is not guaranteed to exercise the
  edge cases, and the bus session reports the driver masks the difference (~nil impact).
  CONSEQUENCE: the model rests primarily on hardware AUTHORITY (Ronny's ruling), not on an
  independent edge-case trace. The available "real test" is therefore a CONSISTENCY +
  REGRESSION check, NOT an edge oracle: run the machine, issue `N500:` commands, LOG the
  3022 IOX traffic (LCON5 bit-0 writes, RSTA5 reads, IDENT / level-12 dispatch), and
  confirm (a) the finalized model matches the driver's real enable/ack pattern and (b) no
  `N500:` command regresses. Tooling: retrocore CLI/MCP (`cli_run` + `cpu_trace_on` on the
  3022 IOX range) against a booted SINTRAN with ND-500-MON.

## 5b. FINALIZED MODEL (Ronny + ChatGPT, 2026-07-18) — dedicated pending latch, full R2-9

The dedicated-latch model (section 3) is ADOPTED as the final architecture; the partial
house-pattern (section 4b) is explicitly REJECTED as an end-state (it keeps `Finished` as
the interrupt condition, which activate clears, so it never fixes R2-9). Authoritative
state model — four SEPARATE states, never conflated:

```
Finished           - software-visible operation status
InterruptPending   - a completion interrupt occurred and is NOT yet acknowledged
InterruptEnabled   - CONTROL/STATUS bit 0, a gate only
Level12Output      = InterruptPending AND InterruptEnabled
```

Rules (exact):
- **Operation completion:** `InterruptPending = true` (ALWAYS, even if enable is clear),
  then re-evaluate output.
- **Interrupt-enable changed (any LCON5 bit-0 write):** re-evaluate output; DO NOT touch
  `InterruptPending`.
- **Operation activated:** may `ClearStatusBits(ND500Finished)`; DO NOT touch
  `InterruptPending`; re-evaluate output. (A new activate is NOT an acknowledgement.)
- **IDENT (level 12):** `InterruptPending = false`; re-evaluate output.
- **Master clear / Reset:** `InterruptPending = false`; re-evaluate output.

The two states `enable && Finished` cannot represent, and which prove the latch is
mandatory: after IDENT `{Finished=1, Pending=0}`; after activate-before-IDENT
`{Finished=0, Pending=1}`.

### Architect refinements the spec is silent on (resolve during implementation)

- **(A) IDENT clears BOTH enable and the request — RESOLVED (Ronny, 2026-07-18).** The
  real 3022 does NOT leave interrupts armed after IDENT ("that would be odd; both the
  interrupt enable and the actual interrupt bit are usually cleared"). So the IDENT
  handler for level 12 clears ALL THREE: `_level12InterruptPending = false` (acknowledge
  the request), the `InterruptEnabled` bit (house convention — matches the 7 other
  controllers, `NDBusDeviceBase.cs:219`), and the line bit (`base.IDENT`). This aligns the
  ND-500 IF with the house IDENT convention AND adds the pending latch the others don't
  need. Consistency check: the hold/re-assert rule applies only to a disable/enable toggle
  WITHOUT an intervening IDENT; a real IDENT ends the hold, so after it both pending and
  enable are clear and nothing re-fires until a new completion + a fresh LCON5 enable — no
  contradiction. IMPLEMENTATION NOTE: `InterruptEnabled` is CONTROL bit 0 mirrored into
  STATUS bit 0, so the IDENT override must clear the enable at its source (control bit 0)
  as well as the status mirror, or the next LCON5 write could re-mirror a stale set bit.
- **(B) Terminate (TERM5) — MUST be decided.** Pinned S1 finding: TERM5 raises a SECOND
  level-12 pulse ("two interrupts per classic message"). The spec only latches on
  "completion"; it does not say whether the `Terminate` case (which sets `Finished`) also
  sets `InterruptPending`. Decide explicitly — latch pending in `Terminate` to preserve the
  second pulse, or consciously drop it. This is the one place the model could silently
  change existing observed behavior.
- **(C) Thread-safety (R2-1 class).** `_level12InterruptPending` is written from BOTH the
  IOX thread (activate/LCON5) and the CPU thread (async stop via
  `AnswerWritten -> SetOperationComplete`). The plain-bool spec assumes single-thread. The
  latch + `UpdateLevel12Output` must live under the same `_statusLock` (or be `Volatile`)
  or the fix reintroduces a lost/stale-wakeup race — exactly the R2-1 hazard already fixed
  for `statusRegister`.
- **(D) Concrete anchors.** Latch at `SetOperationComplete` (both sync
  `ExecuteND500Operation` and async `AnswerWritten` shapes) and — per (B) — the `Terminate`
  case. Clear at the new `IDENT` override + `Reset()` + BOTH MCLR5 strobe paths (the
  existing `SetInterruptBit(false)` at `NDBusND500IF.cs:1726` and `:1903` must also null the
  latch). `CheckTriggerInterrupt()` is repurposed/renamed to `UpdateLevel12Output()` (pure
  line evaluator, never writes the latch).

**Status unchanged: no code written this session; parked for real/live test results.**

## 6. Task ledger entry

- **R2-8:** REVERSED on hardware authority (was "edge / no retro-raise"; now
  "held-until-IDENT, re-enable re-asserts"). Fix = adopt the house-pattern `IDENT`
  override (clear `InterruptEnabled`) + recompute line on LCON5 bit-0 write. **No test
  rework.** Standalone-landable when a live result supports it.
- **R2-9:** OPEN — `CheckTriggerInterrupt` drops a pending unacknowledged level on the
  activate re-check; needs the latched-request model (`_level12Pending`, separate from
  `Finished`). Only this half breaks the canary shortcuts.
- **Coupled test rework (R2-9 only):** OWNED BY BUS SESSION — convert canary
  `InterruptBits = 0` shortcuts to `DbgIdent(12)` + flip the R2-8 assert. Architect lands
  the latch to match once the tests are ready and (per Ronny) a live result supports it.
- **House-pattern finding:** `NDBusND500IF` is the only NDBus controller not overriding
  `IDENT`; all 7 others clear `InterruptEnabled` on IDENT (`NDBusDeviceBase.cs:219` says
  to). Adopting it is the correct R2-8 fix on its own merits.
- **STATUS: no code changed this session. Parked pending real/live test results.**
