# ND-500 (3022) vs ND-5000 (octobus): Shared-vs-Transport Map + Code-Consolidation Analysis

**For**: ND-500 BUS-INTERFACE LLM + ND-5000 OCTOBUS LLM. **From**: architect. **Date**: 2026-07-19.
**Purpose**: settle what is genuinely shared between the two interfaces vs transport-specific, so
nobody re-derives it or "fixes" shared code as if it were interface-local — and analyse whether the
two transports can collapse to one shared code base.

Grades: **[V]** byte/symbol-verified, **[?]** working model, **[TC]** to-carve.

---

## 1. The layering rule

**Reusable at the MESSAGE + SEMANTIC layer. NOT reusable at the TRANSPORT layer. Generation-specific
seam in the middle.** The two share the `Nd500MicrocodeServicer` (activate/answer engine) and the
5MPM shared-memory concept; they differ in the transport that carries activation/completion.

---

## 2. SHARED — message + semantic layer (reuse freely)

| Thing | Detail | Grade |
|---|---|---|
| **`Nd500MicrocodeServicer`** (activate/answer engine) | Literally shared code (S0 extraction). Both `NDBusND500IF` (3022) and `OctobusND5000Station` (octobus) implement `IServicerHost` against the same `ProcessMessage`/`ProcessChain`. | [V] |
| **5MPM message block layout** | Same field offsets both generations: `N5STA=2, SENDE=3, X5CPU=4, X5ACT=5, MICFU=6, STOPR=11, NUMPA=12, MCNO=13, MSWMC=14, TRAPN=16, SMCNO=37` (octal words). | [V] (SYMBOL; 5800 microcode pseudo-C independently confirmed) |
| **Status codes** | free=0, MSGN500=1, WAITING=2, ANSWER=3, 5ERANSWER=4. | [V] |
| **MON-call mechanism** | Answer-in-place: `N5STA:=3`, MICFU untouched, dispatch on STOPR (MOCALL=1 / TRAPCODE=2 / 5FMOCALL=3). | [V] |
| **ND-100 command dispatch** | `MON 60B → N500M → 5IFUNC → FUNCS`. SINTRAN, generation-agnostic. This is *why* "one command script, two expectation tables" works. | [V] |
| **5MPM window + ADRZERO** | Same 8 MB window, ND-100 byte `0x420000` default, big-endian both ports, one `_deviceRam` backing. | [V] |
| **Threaded `CpuND500` model** | Same run thread, `stopMode` park, `WakeRunThread`/`DrainDoorbells`, master-clear re-parks to `StopMode.WAIT`. | [V] |

**Proof of reuse:** Bug A (master-clear re-park, `f525a0df3`) fixed BOTH the 3022 and octobus
`HandleEmergency` paths with one change — they share `CpuND500.Reset()`.

---

## 3. NOT SHARED — transport / plumbing (separate per interface)

| Thing | 3022 | Octobus | Grade |
|---|---|---|---|
| **Activation trigger** | ACT50 register seq (`LMAR5` MS/LS, `LCON5:=5`) | `X5ACT:=0` write at ORCON 0x0A (kick = preempt only) | [V] |
| **Doorbell** | 5015 TAG | ACCP OCB `100501B/100401B` | [V] |
| **CS-load / microcode** | 144-bit words, WA/BREAK/CSCNT, **RSTA5 bit-9 gate**, ECSLOAD | 128-bit words, ACCP `LOCSD/LOCSM/STARTMIC`, **no bit-9 gate** | [V] |
| **Presence/selftest** | CH5CPUPRESENT (IOX-error trap + RSTA5) | ACCP `CMSYSPAR`→MFACK, `RTEST`, `CMALI` | [V] |
| **Register map** | 3022 IOX regs (RMAR5/RSTA5/RCON5…) | ACCP command bytes over frames + station regs | [V] |
| **Completion → ND-100** | level-12 GOSW (`N5MPA`) | level-12, different plumbing | [V] 3022; [?] octobus |
| **Master-clear trigger** | MCLR5 / TERM5 | ACCP emergencies `241B/242B/244B` | [V] |

---

## 4. The generation seam (inside the message layer — the trap)

Some MICFU **semantics differ by CPU generation (5800 vs classic), independent of transport** — so
"shared message layer" is NOT "identical message layer":
- 5800: `3MONO(34B)` = instruction-memory read, `33MON(46B)` = cache dump-dirty — NOT mon-call variants.
- `3RMICV` answers TWO halfwords on 5800 (version 027232B + CPU-parameter) — zero-payload may be insufficient.
- `3SWMESS`/`3FITRNSF` → `MSG_ILLEG` is generation-dependent.
- `3MONCO` delivers the restart value into the process's X1.

→ **Decide the modeled generation before implementing MICFU handlers.** The servicer is shared; its
dispatch has generation branches (`Nd500Generation`).

---

## 5. Practical implication for the two teams

- **A fix in shared code (servicer / message layout / `CpuND500`) usually applies to both** — check
  the sibling before assuming it's interface-local (Bug A precedent).
- **A fix in transport code (`NDBusND500IF` vs `OctobusND5000Station`) is genuinely separate** — the
  two A1 blockers proved it: 3022 = TERM5 manufactured-Finished; octobus = ND-100-internal
  model/version config. Same *symptom* (retry loop), completely different *cause*.
- **Tests: command ladder + console asserts are 100% reusable** (one script) — only the low-level
  expectation table forks.

---

## 6. Code-consolidation analysis — can the two transports collapse to one code base?

**Grounded in a full read-only map of `Nd500MicrocodeServicer.cs`, the `Servicer\` folder,
`NDBusND500IF.cs`, `OctobusND5000Station.cs`, `NDBusOctobus.cs` (file:line evidence).**

### VERDICT: the heavy lifting is ALREADY shared. Do NOT merge the two transports.

The S0 extraction (commit 82e83a148) already pulled **essentially all mailbox logic up into the
servicer.** Message parsing, MAR/5MPM address resolution, N5STA transitions, MICFU dispatch, the
chain walk, the GIVEINT ring insert, and the stop-answers are **not duplicated** across the two
transports — they live once in `Nd500MicrocodeServicer.cs` (`ProcessMessage` :220-658, `ProcessChain`
:146-196, `AnswerRingInsert` :879-912, `AnswerMonitorCallStop`/`AnswerTrapStop` :677-828). The
message BLOCK field offsets live once in `N5MailboxProtocol.cs` (`N5MessageOffsets` :126). This is
the intended post-S0 state, and it's the answer to "can we share instead of duplicate": **the
protocol layer is already one code base.**

The two transport classes exist for a real reason — **IOX/TAG/level-12/bit-9 (3022) vs
ACCP-frame/X5ACT/ADRZERO/GIVEINT (octobus)** are genuinely different hardware (map §3, and the code
map's §C). They are **not entangled**: the servicer never reaches into IOX/TAG or ACCP internals; it
only calls the 9 `IServicerHost` members. Merging them into one class would re-mix machinery that is
correctly separate. **Not recommended.**

### What actually remains duplicated (a thin glue band — small, surgical wins only)

| # | Duplicated glue | Locations | Nature | Worth it? |
|---|---|---|---|---|
| 1 | Big-endian window read/write skeleton | `NDBusND500IF.cs:1495-1526` vs `OctobusND5000Station.cs:688-714` | Structurally parallel; **but 3022 has DMA fallback, octobus has none** | Marginal — the fallback difference is real; arguably clearer left separate |
| 2 | `TryTakeSemaphore`/`ReleaseSemaphore` | `:2217-2226` vs `:726-743` | Same read-check-write + `0xFFFF` marker; octobus adds `lock(_mailboxLock)` | Low |
| 3 | `Nd500AddressBase` / `ServicerLog` | `:2179`,`:2211` vs `:718`,`:765` | Thematically identical / byte-identical delegation | Trivial |
| 4 | `(addr & 0xFFFFFF) << 1` word→byte conversion | `NDBusND500IF.cs:2284`, `OctobusND5000Station.cs:677`, servicer `:189`,`:907` | Copy-shaped idiom at **4 sites** | **Yes — one `WordAddrToByte` helper** |
| 5 | `0xFFFF` semaphore "taken" marker | `:2222`, `:732` | Magic constant duplicated | **Yes — name it in `N5MailboxProtocol`** |

### Higher-value than the dedup: stop transport detail leaking into the shared contract

The map found the shared `IServicerHost` is **transport-aware in three places** — this is worth
fixing before it calcifies (it's how generation/transport knowledge creeps back into shared code):

1. **`MailboxHeaderBase`/`CpuExtensionBlockBase` return `0` as a "classic n/a" sentinel**
   (`IServicerHost.cs:85-100`; 3022 returns 0 at `:2230-2231`), and **the servicer branches on
   `headerBase == 0`** to skip the ring/semaphore path (`Nd500MicrocodeServicer.cs:613-645`,
   `:841-860`). → Replace the magic-`0` sentinel with an **explicit capability flag**
   (`SupportsRingInsert` / `HasMailboxHeader`) so generation knowledge is declared, not inferred.
2. **`AnswerWritten`'s doc-comment bakes both completion models into the contract**
   (`IServicerHost.cs:51-60`). The code delegates cleanly; only the doc is transport-aware — tidy it.
3. **3022 vocabulary in the shared contract docs** (`ReadNd100Word` MAR `<<1` :29-33;
   `Nd500AddressBase` → `SharedMemoryStart`/`5MBBANK` :42-49). Neutralize the wording.

### Recommendation (priority order)
1. **Capability flag instead of the `headerBase==0` sentinel** — removes generation-inference from
   the shared engine. Highest correctness value.
2. **`WordAddrToByte` helper + named `0xFFFF` constant** in `N5MailboxProtocol` — cheap DRY, 4+2 sites.
3. **Leave the two transport classes as-is.** No merge. The window read/write and semaphore glue can
   stay duplicated — the differences (DMA fallback; locking) are real and the LoC saved is tiny.

**Net answer to Ronny:** you already HAVE one shared code base for everything that matters (the
servicer + protocol constants + `CpuND500` + the CPU bridge). What's left is ~6 lines of glue and one
leaky sentinel — a small cleanup, not a consolidation project. There is **no large duplication to
collapse and no case for a single unified transport.** Ownership note (code map §E): **no CODE-FREEZE
markers** in any of these files currently; newest change is a 2026-07-19 re-entrancy fix at
`OctobusND5000Station.cs:613`.
