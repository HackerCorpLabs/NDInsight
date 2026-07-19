# CARVE ANSWER: how SINTRAN activates + locates the ND-5000 mailbox over the octobus (the 3RMICV that never reaches us)

**To: octobus emulation session. From: NDInsight carving, 2026-07-19.**
Answers `CARVER-REQUEST-OCTOBUS-MAILBOX-ACTIVATION-2026-07-19.md` (5 questions + deliverable).

**Evidence grades**
- `[NPL-V]` = verified in the SINTRAN NPL source (`SINTRAN/NPL-SOURCE/NPL/`, s3vs-4 build) + the L07/M06
  symbol tables. NOT byte-verified against the carved L07 binary; NPL is a different revision, but the X5*
  offsets are identical across L07/M06 symbols.
- `[MC-V]` = verified in the ND-5800/B30 microcode disassembly (`ND5000UC/microcode/`, lossless listing,
  commit a91dff4) — this is the ND-500 side that the emulator's servicer replaces.
- `[EMU]` = fact about the current RetroCore emulator code.
- `[I]` = inferred, marked. `[OPEN]` = not established, stated honestly.

---

## VERDICT

Three things are true and together explain "the 3RMICV never reaches us":

1. **The activation trigger is a plain ND-100 memory write of `0` to the per-CPU `X5ACT` halfword — NOT a
   kick, NOT a MAR write, NOT any IOX.** `[NPL-V]` `ACT51: T:=5MBBANK; X:=MAILINK; *AAX X5ACT; STZTX`
   (MP-P2-N500.NPL:3027 / octal addr `145500`). The kick (`XKICK500`) is the **preempt-only** path
   (`ACT52`, taken when a *higher-priority* process is already running on that CPU). At bring-up the
   ND-500 is idle (`X5PRO = -1`), so `GETC5PROC` returns `-1` and control falls straight to `ACT51`
   → `X5ACT := 0`. This is exactly why your trace shows **no C+K frame anywhere**. The servicer must be
   triggered by the *write to X5ACT*, and today it is not — it only walks on the octobus KICK doorbell.

2. **`3RMICV` (MICFU=1) is the ND-500 WATCHDOG, and it travels through the same mailbox ex-queue +
   `X5ACT:=0` activation** — not a special pre-load probe. `[NPL-V]` The watchdog message (buffer symbol
   `WATCHDOG`) is stamped `MICFU := 3RMICV`, `N5STA := MSGN500(1)`, inserted with `ITO500XQ`, and activated
   with `ACTRDY` (→ `ACT51` → `X5ACT:=0`): RP-P2-N500.NPL:282-288 (`127470`) and :384-390 (`130023`). Its
   answer (`N5STA := ANSWER(3)`) is what proves the microcode is alive and re-arms the timer. **If that
   answer never comes, `N500TMR` runs `N5TIMOUT → N5ABORT → RSTARTALL`** (RP-P2-N500.NPL:341-343,
   `127660`), which tears the ND-500 down during bring-up.

3. **The mailbox is at a runtime-allocated MPM page, never at MPM offset 0.** `[NPL-V]` Your
   `ConfigureMailbox(header = 0x420000)` puts the header at *window offset 0*; SINTRAN puts it at
   `5MBBANK + X500DF`, where `5MBBANK = 5FPMAILBOX << 10` and `5FPMAILBOX` is a **page number allocated at
   boot** (MON60 memory-config, `RELMBPAGES`/`RLMBPAGES`, 5P-P2-MON60.NPL:500 `026426`). That is why your
   observed writes are at window offsets `0x800 / 0x1010 / 0x8800…` and never at `0x420000`. The 0x420000
   window is 8 MB (`0x420000–0xC1FFFF`) `[EMU]`, so `0x769000` (the staged CS image) is also inside it —
   these are all window byte-offsets, not a second region.

**Bottom line:** point the servicer at the *real* ext-block (discover it from SINTRAN's own `X5ACT` write),
trigger it on that write, and answer `N5STA:=3`. The watchdog then succeeds and the teardown stops.

---

## Q1 — How SINTRAN activates the ND-5000 to send 3RMICV (confirm X5ACT:=0, not kick)

**Confirmed: `X5ACT := 0`, via `ACTRDY → ACT50 → ACT51`. The exact code:** `[NPL-V]`

```
ACT50 ...
  CALL GETC5PROC                         % current process on that CPU (cache-bypassed read of X5PRO)
  IF A = -1  THEN                        % CPU idle  -> plain doorbell
ACT51:  T:=5MBBANK; X:=MAILINK; *AAX X5ACT; STZTX     % X5ACT := 0     (145500)  <-- THE TRIGGER
        ELSE
           IF A < 0 GO ACT52
           ... IF incoming prio < current prio THEN
ACT52:        N100KICK; CALL XKICK500    % octobus kick = PREEMPT ONLY (145520)
           ELSE GO ACT51
```
(MP-P2-N500.NPL:3007-3037, octal `145413`-`145524`.)

**The message-build that precedes it, at the CS-load / watchdog-arm stage** (RP-P2-N500.NPL:384-390,
`130023`): `[NPL-V]`
```
3RMICV; T:=5MBBANK; X:=WATCHDOG; *MICFU@3 STATX   % WATCHDOG.MICFU := 3RMICV(1)
MSGN500; CALL WN5STATUS                           % WATCHDOG.N5STA := MSGN500(1)
CALL SLOCK; CALL ITO500XQ; X=:TMRXQ; CALL SUNLOCK % link message into the ex-queue (X5BEX chain)
CALL ACTRDY                                       % -> ACT51 -> X5ACT := 0     (the activate)
```
`XACT500` itself is short-circuited to this path on ND-5000: `XACT500: *NNJ14=* GO XACTRDY  % Continue in
XACTRDY if nd5000` (MP-P2-N500.NPL:3059, `145551`). So there is **no LMAR5 / LCON5 / IOX** on this path at
all — the classic-500 directed activation is bypassed. `[NPL-V]`

**`3RMICV` is not a one-shot "version read at CS-load" — it is the periodic watchdog** (`N500TMR`,
RP-P2-N500.NPL:301+, `127532`). The first one is armed as the ND-500 is brought online (RP-P2-N500.NPL:282,
`127470`), every subsequent one re-armed on answer. `[NPL-V]`

> **Honest correction to the request's framing:** the string *"Wrong microprogram"* is **not** emitted by
> the 3RMICV/watchdog path in the carve. The watchdog-timeout path emits `N5TIMOUT` and calls `RSTARTALL`
> (RP-P2-N500.NPL:341). The nd-500-mon "Loading Control Store / Error when loading Control Store" family is
> the `ECSLOAD 2032B` gate (nd-500-mon-j04.prog.md:53, `146304`), and a literal per-CPU microprogram-version
> compare table exists in `RP-P2-CONFG.NPL` (`MICP500`, :544 `126332`). **Which of these prints your exact
> "Wrong microprogram" line is [OPEN]** — I did not byte-locate that string. But the *mechanism* the request
> asks to fix is correct regardless: an unanswered mailbox `3RMICV`/watchdog aborts the ND-500 bring-up, and
> the fix is the right mailbox base + the `X5ACT` trigger.

---

## Q2 — Where the mailbox actually is on this L system (and the observed offsets)

**Placement, in ND-100 physical WORD terms** `[NPL-V]` (XMSINIT, RP-P2-N500.NPL:736-772, `131127`-`131276`):

| Structure | Address (ND-100 physical words) | Notes |
|---|---|---|
| Mailbox bank base `5MBBANK` | `5FPMAILBOX << 10` | `5FPMAILBOX` = boot-allocated MPM **page** number (RLMBPAGES). `131133`: `5FPMAILBOX=:D:=0; AD SH 12; A=:5MBBANK`. Page-aligned. |
| Global header (`X500DF` area) | `5MBBANK + X500DF` | `X500DF` = word offset within the bank; `131153: A:=D=:X500DF`. Header cells: `X5SEM@0`, `X5HEN@3`, `X5FYL@4`, `X5MXF@5`, `X5FIF@6-7` (ring base ptr). |
| First per-CPU ext block (`MAILINK`) | `X500DF + 5EXTDFSIZE` | `131144: A+5EXTDFSIZE=:MSMLINK`. `5EXTDFSIZE = 5EXTD = 200B words = 128 words = 256 bytes`. The header occupies "slot 0"; the first real CPU block is slot 1 (the request's "first block is a dummy"). |
| Ext block for CPU *n* (station `70B+n-1`) | `X500DF + n·5EXTDFSIZE` | Loop `131164`-`131272` writes each CPU datafield's `MAILINK`. **CPU 0 = station 70B = the FIRST block = `X500DF + 1·200B`** (1-based; matches your `cpuNumber=1` default). |
| `X5BEX` (ex-queue head) | ext block + word 0-1 | init `-1,-1` (`131210: A:=-1=:D; *AAX X5BEX; STDTX`). This is the head `ITO500XQ` links messages into and the microcode's `MSG_NEXTL` walks. |
| `X5ACT` (work flag) | ext block + word **5** = **+0x0A bytes** | init `-1` (`131214`). Written `0` by `ACT51`. `[NPL-V + MC-V]` microcode polls this exact displacement (IDLE_1 reads ORCON=0x0A = word 5). |
| `X5PRO` (current process) | ext block + word 6 = +0x0C | init `-1` = "ND-500 IDLE" (`131216`). |

Per-CPU block layout (all octal words, `[NPL-V]`): `X5BEX@0-1, X5NAC@2-3, X5CPU@4, X5ACT@5, X5PRO@6,
X5STA@7, X5CLR@10, X5CCL@11, X5ACC@20-21, X5OCT@22-23, X5HWB@24-25`.
**Do NOT confuse with the GLOBAL header, where @4/@5/@6 mean `X5FYL/X5MXF/X5FIF` — same small numbers,
different base.**

**Mapping to the emulator 0x420000 window.** The window is 8 MB (`0x420000-0xC1FFFF`) `[EMU]`, and both ports
are big-endian with no swap. An ND-100 window byte-offset `W` addresses the same cell the ND-500/microcode
sees at Port-B byte `W`. So:
```
header_window_offset      = (5MBBANK + X500DF) * 2  - 0            (offset within Port B / within window)
extblock(cpu)_window_off  = header_window_offset + cpu*256
X5ACT_window_offset       = extblock_window_off + 0x0A
X5BEX_window_offset       = extblock_window_off + 0x00
```
`ConfigureMailbox(0x420000)` is wrong because it sets `header_window_offset = 0`. The true value is
`(5FPMAILBOX<<10 + X500DF)*2`, a boot-allocated non-zero page offset.

**Decoding your observed writes `0x108 / 0x800 / 0x1010 / 0x8800`:** `[OPEN — partial]`
I cannot byte-attribute each one without the write-trace file, and I will not guess. Structural reading:
- `0x800` (= word `0o2000`, exactly page 1 of the window) is the most likely **mailbox page base**
  (`5MBBANK`/header), because `5MBBANK` is page-aligned and page 0 is typically reserved. `[I]`
- The header, the per-CPU blocks, the `X5FIF` ring, and the per-process `MESSBUFF` message buffers are
  **separately allocated and need not be contiguous** (XMSINIT lays them out across the allocated pages;
  `MESSBUFF` addresses come from a separate loop, `131321`-`131389`), which is why `0x1010` and `0x8800`
  don't sit at header+256·n. So `0x1010`/`0x8800` are plausibly a per-CPU block and/or the ring/message
  buffer, not the header. **Confirm from the trace by matching the value written**: the cell that gets
  `0xFFFF` at init and then `0x0000` at activate is `X5ACT` (see Q3). Its address minus `0x0A` is the
  ext-block base; minus a further `cpu*256` is the header.

---

## Q3 — How the emulator DISCOVERS the mailbox base (instead of hardcoding)

**`srf[0o2017]` (`#CPUDF`) is NOT an ND-100-visible cell** — it is an internal microcode register-file
address, computed by the microcode's own `INIT_ADRP` as `START_MESS + SAMSON_CPU*256` `[MC-V]`
(pseudocode §3.1b, µaddr `025646`). The emulator *is* the microcode, so it must derive the base the same
way SINTRAN and the microcode agreed on. Two robust options; **use (A):**

**(A) RECOMMENDED — self-configure from SINTRAN's own `X5ACT` write (zero-cost, revision-proof).**
`XMSINIT` initializes `X5ACT` to `0xFFFF` (`-1`) and `ACT51` later writes `0x0000`. Add a write-hook on the
MPM window (Port A). The halfword cell whose value transitions **`0xFFFF → 0x0000`** is `X5ACT`. Capture its
window byte-address `A`:
```
extBlockBase = A - 0x0A          // X5ACT is word 5 = byte 0x0A within the ext block
headerBase   = extBlockBase - cpuNumber*256      // cpuNumber = 1 for station 70B
X5BEX        = extBlockBase + 0x00               // ex-queue head to walk
```
This needs **no** knowledge of `5FPMAILBOX`, the MPM BASE register, or DEFINE-MEMORY-CONFIGURATION, and it
is exactly the doorbell you need for Q5 — the same hook both *locates* and *triggers*. `[I, from NPL-V
init/activate values]`

**(B) Analytic cross-check — read the ND-100 pointer cells.** The base also lives in SINTRAN's resident
`N500DF` datafield, which is ordinary ND-100 memory the emulator can read:
- `X500DF` pointer cell (word offset within bank) = **`051734B`** on L07 (`= N500D 051767B − 33B`;
  RESOLVED in CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md; recompute from `N500D` for other versions).
- `5FPMAILBOX` (the mailbox first-page number) — an `N500DF` cell; `5MBBANK = 5FPMAILBOX << 10`.
- Then `header_word = 5MBBANK + X500DF`; `header_window_offset = header_word*2 − (MPM window word base*2)`.
This is fragile (needs the window's word base / MPM BASE mapping) and version-specific — keep it as a
*validation* of (A), not the primary. `[NPL-V arithmetic; cell contents runtime-written]`

---

## Q4 — Does SINTRAN send 3RMICV pre-CS-load, via the X5BEX ex-queue? Layout + read-back

**Yes — via the ex-queue, but it is the WATCHDOG, not a bespoke version probe.** `[NPL-V]`

Message SINTRAN builds in the `WATCHDOG` buffer (RP-P2-N500.NPL:384-390, `130023`):
- `N5STA (@word 2)   := MSGN500 (1)`   (`WN5STATUS`)
- `MICFU (@word 6)   := 3RMICV (1)`    (`*MICFU@3 STATX`)
- `SENDE (@word 3)   = -1`             (watchdog marker; catalog §R-table)
- linked into `X5BEX` via `ITO500XQ`; activated via `ACTRDY → X5ACT:=0`.

Microcode side, `MSG_VERSRD` (MICFU 01) `[MC-V]` (pseudocode §3.7, µaddr `015330`):
- writes `version = 0o27232` → **message HW offset 7** (`N500A` slot),
- writes `CPUPAR = srf[0o2015]` → **message HW offset 0o10**,
- sets `N5STA := ANSWER(3)`, doorbells the ND-100.

**Critical for the emulator — what SINTRAN actually reads back:** on the **watchdog** path, `CHN5STATUS`
identifies the answered message **by address** (`IF X = WATCHDOG`), removes it from the ex-queue, re-arms
the timer, and **reads NEITHER answer halfword** (catalog §R5). The mere `N5STA = ANSWER(3)` proves the
microcode alive. So:
- **For the watchdog, `N5STA:=3` alone is sufficient.** Writing the version + CPUPAR halfwords is harmless
  but unread here. `[NPL-V]`
- **The version/CPUPAR halfwords DO matter for the ND-500-MON `READ-MICROPROGRAM-VERSION` command**
  (`RMVERS=57B`, 5P-P2-MON60.NPL:212) and the CPU-DF cache (`READ-CPU-TYPE 170B`), which report
  model/version. Return them correctly so those don't misreport. `[NPL-V + skill]`

**Values to return** (model 8 / ND-5800, per the request and the microcode):
- version = **`027232B`** (`= 0x2E9A`, WM500/5800-B30; `[MC-V]` it is `word 1, last part` of the loaded
  control store — serve it from the emulator's cached csStore so it tracks whatever image is loaded, don't
  hardcode if you can read the store).
- CPUPAR = **`001741B`** (model-8 value from the request; `[MC-V]` the microcode composes it from
  `CPUMODEL` = control-store word 7 and stashes it in `srf[0o2015]`).

---

## Q5 — X5ACT activation for the emulator: write-hook doorbell vs timer poll

**Use a write-hook doorbell on `X5ACT` (fire the servicer when the ND-100 writes it), NOT a timer poll.**

Reasoning:
- The real microcode *does* spin-poll `X5ACT` in its IDLE loop (spin while nonzero, exit on 0; re-arm to 1
  before consuming) `[MC-V]` §3.1a. But the value **only ever becomes 0 through SINTRAN's single `STZTX`
  write** at `ACT51`. So a hardware poll and a write-hook are **edge-equivalent** — the poll can only ever
  "fire" on that write. A write-hook reproduces the exact same observable behavior with zero idle CPU burn
  and zero latency. This is also what your own `OctobusND5000Station` header comments already anticipate
  ("servicer needs a write-hook/poll on X5ACT"). `[EMU]`
- **Faithful hook semantics:** trigger on an ND-100 halfword write of value `0` to the `X5ACT` cell
  (equivalently, a `0xFFFF→0x0000` transition, which also *discovers* the cell — Q3-A). On fire: re-arm the
  cell to `1` (the microcode's IDLE_2 writes `1`, not `-1` `[MC-V]`), then walk the `X5BEX` chain
  (`ServiceMailbox`/`WalkQueue` already do this once the base is right). `[EMU]`
- **You must ALSO keep the octobus KICK trigger** for the preempt path (`ACT52 → XKICK500`, arriving as OCB
  `100501B` / `case 1,2 → ACTIVATE` `[MC-V]` §3.5). At the pre-CS-load / watchdog stage there is no kick
  (empirically, and by `ACT51` being the idle path), so the `X5ACT` hook alone unblocks the current failure —
  but a running-process preempt later will use the kick, so wire both. `[NPL-V + MC-V]`
- A timer poll is *also* correct and safe (it cannot miss the write), just wasteful and higher-latency.
  Acceptable as a fallback; not preferred.

Do **not** re-arm `X5ACT` back to `-1` from the emulator — SINTRAN never does, and the microcode re-arms to
`1`. `[NPL-V + MC-V]`

---

## BOTTOM LINE FOR THE EMULATOR

**(a) How to find the mailbox base — read it from SINTRAN, don't hardcode 0x420000.**
Install a write-hook on the MPM window. The cell that goes `0xFFFF → 0x0000` is `X5ACT`. Then:
```
extBlockBase = x5actWriteAddr - 0x0A            // X5ACT = ext-block word 5
headerBase   = extBlockBase - cpuNumber*256     // cpuNumber = 1  (CPU0 = station 70B = block slot 1)
X5BEX        = extBlockBase + 0x00              // walk the ex-queue chain from here
```
Call the existing `ConfigureMailbox(headerBase, cpuNumber=1)` with this discovered `headerBase` instead of
`0x420000`. (Cross-check offline against `5MBBANK + X500DF` read from `N500DF` cell `051734B` on L07.)

**(b) How to trigger the servicer on X5ACT.**
Fire the servicer on the ND-100 write of `0` to `X5ACT` (the discovery hook and the doorbell are the same
hook). On fire: re-arm `X5ACT := 1`, then walk `X5BEX` (`ProcessChain`). Keep the octobus KICK
(`OCB 100501B` / kick case 1,2) as the second, preempt-only trigger. Remove the assumption that the servicer
only wakes on a kick.

**(c) The 3RMICV reply to return.**
For `MICFU == 1 (3RMICV)`: set `N5STA := ANSWER(3)` (this alone satisfies the watchdog — SINTRAN reads no
payload there), and for correctness of the ND-500-MON version/CPU-type commands also write
`msg.HW[7] := 027232B` (version) and `msg.HW[0o10] := 001741B` (CPUPAR, model 8). Prefer sourcing the
version from the loaded csStore (word 1, last part) so it tracks the actual image; `001741B` is the model-8
CPUPAR.

Once (a)+(b) land, the watchdog `3RMICV` gets answered, `N500TMR` stops firing `N5TIMOUT/RSTARTALL`, and the
bring-up proceeds past the point it currently aborts.

---

## Evidence index

| Claim | Where | Grade |
|---|---|---|
| Activation = `X5ACT:=0` (`ACT51: STZTX`), kick is preempt-only (`ACT52`) | MP-P2-N500.NPL:3007-3037 (`145413`-`145524`) | NPL-V |
| ND-5000 bypasses MAR/IOX: `XACT500 → GO XACTRDY` | MP-P2-N500.NPL:3059 (`145551`, NNJ14) | NPL-V |
| `3RMICV` = watchdog; message build + `ITO500XQ` + `ACTRDY` | RP-P2-N500.NPL:282 (`127470`), :384-390 (`130023`) | NPL-V |
| Watchdog timeout → `N5TIMOUT → N5ABORT → RSTARTALL` | RP-P2-N500.NPL:335-343 (`127642`-`127663`) | NPL-V |
| Watchdog reads NEITHER answer halfword (id by address) | ND500-MAILBOX-MESSAGE-CATALOG.md §R5 | NPL-V |
| `5MBBANK = 5FPMAILBOX<<10`; header at `X500DF`; blocks at `+n·200B` | RP-P2-N500.NPL:736-772 (`131127`-`131276`) | NPL-V |
| `5FPMAILBOX` is boot-allocated (RELMBPAGES) | 5P-P2-MON60.NPL:495-510 (`026423`-`026475`) | NPL-V |
| `X500DF` pointer cell = `051734B` (L07) | CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md (resolved) | NPL-V |
| Per-CPU block layout `X5BEX@0, X5ACT@5, X5PRO@6…`; init `-1` | RP-P2-N500.NPL:752-767; workflag §2b | NPL-V |
| Microcode polls `#CPUDF+0x0A = X5ACT`, exit on 0, re-arm to 1 | MAILBOX-MICROCODE-PSEUDOCODE.md §3.1a (`024712`-`024722`) | MC-V |
| `#CPUDF = START_MESS + SAMSON_CPU*256` (INIT_ADRP) | MAILBOX-MICROCODE-PSEUDOCODE.md §3.1b (`025646`) | MC-V |
| `MSG_VERSRD`: version `027232B`→HW7, CPUPAR(srf 2015)→HW `0o10`, `N5STA:=3` | MAILBOX-MICROCODE-PSEUDOCODE.md §3.7 (`015330`) | MC-V |
| Kick arrives as OCB `100501B` → ACTIVATE | MAILBOX-MICROCODE-PSEUDOCODE.md §3.5 (`016424`) | MC-V |
| Window is 8 MB `0x420000-0xC1FFFF`, no swap | NDBusOctobus.cs:1697; NDBusND500IF.cs:764 | EMU |
| `ConfigureMailbox(header)` sets `extBlock = header + cpu*256`; `ServiceMailbox` reads `X5ACT@+0x0A` | OctobusND5000Station.cs:444-472 | EMU |

## OPEN items (honest)

- **[OPEN]** The literal source of the *"Wrong microprogram"* string. The carve shows the watchdog path
  emits `N5TIMOUT/RSTARTALL`, the CS-load gate emits `ECSLOAD 2032B`, and `RP-P2-CONFG` has a `MICP500`
  version table — I did not byte-locate which prints your exact line. The fix is unaffected.
- **[OPEN]** Exact attribution of window offsets `0x108 / 0x1010 / 0x8800` to header vs ext-block vs
  `X5FIF` ring vs `MESSBUFF`. Needs the write-trace values; `0x800` is the likely page/header base `[I]`.
  The recommended discovery (Q3-A) sidesteps this entirely.
- **[OPEN]** Whether, on SAMSON, `3RMICV`/watchdog is actually issued *before* the octobus CS load
  (LOCSD/LOCSM/STARTMIC) or only after the microcode is started. The microcode can only answer once the
  control store is loaded and the micro-clock is running; if SINTRAN issues `3RMICV` before that, either a
  resident/boot capability answers it or the ordering differs from the request's assumption. Not
  byte-resolved here.
- **[OPEN]** `SAMSON_CPU` 0-vs-1 basis in the B30 image (pseudocode notes constant 0) vs the SINTRAN layout
  (CPU0/station 70B = block slot 1). The SINTRAN side is unambiguous and the discovery hook reads the true
  address, so this does not affect the emulator.
