# How the ND-500/5000 swapper is STARTED — carve result (2026-07-19)

> # ⚠ THIS DOCUMENT'S CENTRAL VERDICT IS RETRACTED (2026-07-20)
>
> **Do not build on §1, §2b, or §4 of this document.** Three of its conclusions were disproven the
> next day. It is kept for the history and because its §2a, §3 and §5 remain useful.
>
> | This doc claims | Actually true [V] | Evidence |
> |---|---|---|
> | §1/§4: "the swapper **is** 128-bit control-store microcode; a functional `CpuND500` cannot run it; model its EFFECT in C#" | The swapper is **ordinary ND-500 macrocode** — `SWAPPER-K01.PSEG`, 38,161 B, at physical `0x06F800`, byte-identical incl. the embedded `REV`/`-K01` strings. The 3022 track has it **executing** on the functional `CpuND500` | `ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` §12c (retraction), §12d (byte identification), §12e (it runs) |
> | §2b/§5.1: `LDSWA` contains a **CPU-type branch** selecting control-store load vs the 14B/21B wire | `LDSWA` (`143551`-`143621`) has **no CPU-type test at all**. Its only descriptor test is `143564 BSKP ZRO 30 DA` = bit 3 of `mem[mem[B-57]-22]`, the *"swapper already loaded"* done-bit | Carve 2026-07-20; `OPEN-QUESTIONS-REGISTER-2026-07-20.md` §2.0 |
> | §1/§2b: `> Loading Control Store` and `> Loading Swapper` mark two **branches** of a generation choice | They are **steps 0 and 3 of ONE state machine**, `500IN` @`075150`, gated by independent bits of a single done-mask (complete = `0o217`). Bit 0 is tested before bit 3 — which is exactly why both print, in that order, on the same machine | Carve 2026-07-20 |
>
> **The real generation discriminator** is `(mem[mem[B-56]+27] /*CPUAVAILABLE*/ & 000007) == 3 /*SAMSON*/`,
> used 20+ times in segment `030-S3SM5` — including the **bit-2** init step at `052235`
> (SAMSON → `153154`, classic → `052313`) — but **not** on the swapper-load or control-store-load
> paths, which run unconditionally.
>
> **How the swapper image actually arrives [V]:** `LDSWA`(143551) → `PLSWA`(144212) → `144002`
> (`MON 50` OPEN, `GFMAD`/`GFDEV`/`GFSEC`) → loop `143647` → `144117` → **`MON 131` (ABSTR)** =
> ordinary **disk-controller DMA into ND-100 physical memory**, destination page allocated by
> **`MON 61` (FIXC5)**. The whole path `143600`-`144400` contains only `BFILL`, `MON 50`, `MON 131`,
> two 2-word `MOVEW` descriptor copies and `MON 43` — **no IOX, no window store loop, no mailbox, no
> ACCP, and no SAMSON branch**. That is why the image is byte-identical to the file on disk and why
> no 14B RESIWR ever names its address.
>
> **Root cause of the error:** conflating `> Loading Control Store` (the CPU's own 128-bit firmware)
> with `> Loading Swapper` (an ND-500 *program*). Two different transfers, two different strings.
> §5 item 1 correctly flagged the `LDSWA` branch as `[TC]` — the failure was that later documents
> cited the §1 verdict as settled.
>
> Current status of every question here: `OPEN-QUESTIONS-REGISTER-2026-07-20.md`.
> What remains genuinely open from this doc is scoped as **Q-SWP-11** ("what do the `LDSWA`/`RUNSW`
> bytes actually contain") and **Q-SWP-12** (`55MESSIZE` + the classic 21B layout).


**Question:** On an ND-5800/5000 system, is "the swapper" started via (A) the mailbox/macrocode
context-block path (3START-family), or (B) the microcode / control-store / micro-clock path?

**Method:** static carve of SINTRAN-L (L07) NPL + symbols + the existing byte-verified carve docs.
Grades: **[V]** byte/NPL-verified here, **[?]** working model, **[TC]** to-carve / live-trace.
NPL is a *different revision* than the L07 bytes — treated as logic, never as final authority
(per `ND500-STATUS-AND-INDEX.md` §1).

---

## 1. VERDICT — refined hybrid, generation-dependent

**For the ND-5800/5000 CPU that this L07 image is provisioned for: mechanism (B) — the swapper is
128-bit CONTROL-STORE microcode, loaded and started via the LOAD-CONTROL-STORE / micro-clock path,
NOT a macrocode context block.** A functional (macro-instruction) `CpuND500` cannot run it.

But mechanism (A) is real and is the *classic ND-500* mechanism — the same source tree contains the
macrocode start wire (`MSWSTART` → `MICFU := 3START` = mailbox 23B). The generation of the CPU
selects which one runs.

**Single strongest piece of evidence for (B) on this image:** `LOAD-SWAPPER` on the L07 image prints
`> Loading Control Store` and drives the interface micro-clock (`LCON5` ctrl 0x20/0x24), leaving 3022
STATUS `ND500MicroClockStopped` (0x0200) — observed live in the boot harness, recorded in
`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` §3/§9/§10. The classic register-image start (21B) is
never issued for the swapper on this path; the `CpuND500` stays parked at PC=0.

**Single strongest piece of evidence that (A) is the CLASSIC mechanism (not this image):**
`MP-P2-N500.NPL:431-437` `SWMESS` / `IF A=MSWSTART THEN ... 3START; *MICFU@3 STATX` — the start-swapper
sub-function writes micro-function **3START (23B)** into the mailbox message and activates the ND-500,
with sender/receiver `5SWPROC`. That is a macrocode process start over the mailbox = (A).

This **confirms and refines** the prior peer analysis (which said (B)): (B) is correct *for the
ND-5800 target*, but the blanket claim needs the qualifier that (A) is the live mechanism on a genuine
classic-500 image and the source wire for it is present.

---

## 2. Call chain — "SINTRAN decides to start the swapper" → ND-500 runs

### 2a. Classic ND-500 path (mechanism A) — present in source, runs on a classic CPU

1. **`LOAD-SWAPPER`** (operator / init) → MON-60 subfn **7B `SWLOD` → `LDSWA`** (`LDSWA=143551` L07,
   `N500-SYMBOLS.SYMB.TXT:5096`). On a *classic* CPU this loads the swapper as a macro image:
   `14B` ResidentWrite (memory image) + `12B` + **`21B` (3WREG, P := swapper entry point)**.
   *(chain per `ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` §10.)*
2. **`START-SWAPPER`** → MON-60 subfn **54B `STSWP` → `RUNSW`** (`RUNSW=163621` L07,
   `N500-SYMBOLS.SYMB.TXT:4708`) → **`MSWSTART`**.
3. **`MSWSTART` handler = `SWMESS`, `MP-P2-N500.NPL:428-459` (`133635`+):**
   - `133642 IF A=MSWSTART THEN` (`SWFUN` read at `133640`) — "Start swapper".
   - `133661 3START; *MICFU@3 STATX` — write **MICFU := 3START (23B)** into the mailbox message [V].
   - `133663 5SWPROC; *SENDE@3 STATX; 5RECE@3 STATX` — sender/receiver = swapper process [V].
   - `133671 A:=300; *AAX 5PRIO... STATX` — priority 300 [V].
   - `133674 CPUNO ...` — target CPU number [V].
   - `133742 SWME1: CALL XACTRDY; CALL LOWACT500` — **activate the ND-500** [V].
4. **Activation** (`ACT50`, `MP-P2-N500.NPL:3086-3092`): set MAR to the message bank/addr, write
   `LCON5` CONTROL — ND-500 microcode DMA-fetches the message, sees `MICFU=3START=23B`, loads the
   process context (P from the 21B image) and **runs macrocode**. This is the step a real `CpuND500`
   could execute — and 23B `StartProcess` is already wired to
   `CpuND500.StartProcessFromRegisterImage` (`ND500-STATUS-AND-INDEX.md` §... / D4 §7).

### 2b. ND-5800/5000 path (mechanism B) — what THIS L07 image actually does

1. Same entry (`LOAD-SWAPPER` → `LDSWA`), but `LDSWA` contains a **CPU-type branch** (in segment
   `030-S3SM5`, *not* in the NPL tree — **[TC]**) that, for the SAMSON/ND-5800 CPU type, selects a
   **128-bit control-store load** instead of the `14B/21B` macro-image wire.
2. That branch prints `> Loading Control Store` / `> Loading Swapper` and drives the **micro clock**
   (`LCON5` ctrl 0x20/0x24 = micro-clock stop/start; `STOPMIC=34`, `5P-P2-MON60.NPL:193`), leaving
   STATUS bit 9 `5CLOST` behavior visible as `ND500MicroClockStopped` [V harness].
3. **No `21B` RegisterWrite, no `23B` StartProcess, no macrocode context block** is issued for the
   swapper on this path [V harness]. The swapper *is* the microcode; it starts when the micro clock
   runs the loaded control store. The functional `CpuND500` (macro interpreter) never executes it.
4. Corroboration that classic is disabled on B30: the 5800 B30 microcode marks the classic
   register-image micro-function as illegal (`MILLFU`/`MSG_ILLEG`; `MP-P2-N500.NPL:547 MILLFU: ILMICFUNC`,
   `ILMICFUNC=2001`), recorded in D4 §10 as "MICFU 21B = MSG_ILLEG (classic-only)". Exact B30 mapping =
   **[TC]** (from the microcode listing, not this NPL).

---

## 3. What the swapper IS (process-table / LIST-ACTIVE-PROCESSES)

- The swapper is **ND-500 process 0 = `5SWPROC`** and carries the process name **`"5SWAP"`**:
  `MP-P2-N500.NPL:447 IF X:="5SWAP"=CURPROG THEN` [V]. That literal is what appears in
  LIST-ACTIVE-PROCESSES.
- **Priority 300** (`MP-P2-N500.NPL:439`), **not timesliced** (`:445 X.PSTAT BZERO SLICE`) [V].
- Its per-process message slot is at `5SWPROC*55MESSIZE + SWMSG` in the mailbox
  (`MP-P2-N500.NPL:3015/3030`) [V].
- **Do NOT confuse "start the swapper" with "call the swapper":**
  - `MON 510B` **`SWMC` (CallSwapper, `142153B`)** packs a trap sub-code into the message and does
    `CALL 5ACTSWAPPER` (`5ACTS=145162B`) — it *activates an already-resident swapper* to service a
    request; it does not start it (510B-CallSwapper README + `.pseudo.c`, byte-verified control flow).
  - `MON 60B 076B` **`ITOSWP`** merely copies a message body to the swapper's buffer
    (`60B-076B-ITOSWP` README/.pseudo.c). Both are the swapper's *service* interface, downstream of it
    already existing.
- **`22B StartProcessZero` is NOT the swapper start** — it is `P0START` (`MP-P2-N500.NPL:618-627`),
  which inserts a **WATCHDOG** message (`X:=WATCHDOG; CALL ITO500XQ`) into the ex-queue and reactivates
  the ND-500 [V]. This definitively closes the long-standing "route 22B → CPU to start the swapper"
  rabbit hole (D4 §10; the emulator's synthetic 22B stub is correct).

---

## 4. Emulator implication

Because the target is the ND-5800/5000 (mechanism B), **there is no macrocode context block to run** —
a `CpuND500` that does not emulate 128-bit microcode cannot make the swapper appear by "loading
registers and running". The C# servicer must **model the swapper's EFFECT** in high-level code, the
same way `Nd500MicrocodeServicer` already reimplements the mailbox microcode:

- Make the control-store/micro-clock gate pass: `RSTA5` STATUS bit 9 `5CLOST` = CLEAR (control store
  "loaded"); bit 5 `5ILOCK` CLEAR; error bits CLEAR (`ND500-CONTROL-STORE-GATE.md`). Answer the
  `LOAD-SWAPPER` control-store-load with success so `LDSWA` does not loop on `ECSLOAD=2032B`.
- Populate the descriptor tables the swapper builds so PLACE-DOMAIN's MON-60 chain completes and RUN
  finds a "well defined program". Known shape (D4 §10, `swapper/swapper-k01-handlers.md`): Table A
  "slot table" @ `0x08038000` (0o144 words/entry, valid-slot count @ `[0x128A4]`), Table B (seg-4,
  0o400 stride), Table C (seg-6 page map), Table D (seg-5); 29-entry fn-code table @ DSEG `0x26198`.
  **[TC]:** whether the tables RUN / LIST-ACTIVE-SEGMENTS read are Table A or a mirror in the ND-100
  `S500S` process-descriptor array (process 0 = `5SWPROC`).
- Report the CPU module/model + micro-version the swapper compares during CS-load, driven from the
  loaded CS image (word-7 model / word-1 version) rather than a hardcoded constant, to avoid the
  `EWRON = 002203B` "Wrong microprogram" loop (`CARVE-ANSWER-OCTOBUS-WRONG-MICROPROGRAM-2026-07-19.md`).

**If instead you run a genuine CLASSIC-500 image (mechanism A):** the swapper *can* run on `CpuND500`.
Load the **21B register/context image** at 23B start — the full block, not just reg0→P (current code
applies only reg0→P; D4 §7 plan item). The context fields the start path sets:
**P** = swapper entry (from the 21B image), plus mailbox process fields
**MICFU=3START(23B)**, **SENDE/5RECE=5SWPROC**, **5PRIO=300**, **CPUNO**, `SWACTIVE`. The remaining
L/B/R/I1-4/A1-4/E1-4 come from the 21B image body and must be applied at start.

---

## 5. UNVERIFIED / open questions (for a live trace)

1. **[TC]** The exact CPU-type branch in `LDSWA` (segment `030-S3SM5`, not in the NPL tree) that
   selects control-store load vs the 14B/21B macro wire. This is *the* seam between (A) and (B) and is
   not statically resolved.
2. **[TC]** The B30 microcode's actual MICFU map — whether 21B/23B are truly `MSG_ILLEG` on the 5800,
   confirmed from the microcode listing rather than the NPL `MILLFU` table.
3. **[TC]** Whether RUN / LIST-ACTIVE-SEGMENTS read the swapper's Table A directly or a mirrored ND-100
   `S500S` descriptor array — determines exactly what the C# functional swapper must populate.
4. **[?]** The NPL revision drift: `MP-P2-N500.NPL` shows the classic `MSWSTART→3START` wire; the L07
   bytes for `LDSWA`/`RUNSW` were not disassembled in this pass (verdict on (A) rests on NPL logic +
   L07 symbol pins `RUNSW=163621`, `LDSWA=143551`, not on read L07 bytes).
5. **[TC]** The precise `55MESSIZE` (swapper message word count) and the swapper's register-image
   layout for the classic 21B context, needed for a byte-accurate mechanism-(A) start.
