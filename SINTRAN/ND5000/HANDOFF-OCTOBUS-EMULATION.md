# HANDOFF: ND-100 <-> ND-5000 Octobus Emulation — State & Next Steps

**Date**: 2026-07-16. Written for the next LLM/engineer taking over this work.
**Read first**: `OCTOBUS-ND100-ND5000-REFERENCE.md` (same folder) — the consolidated,
evidence-marked protocol reference. Everything below assumes you have read it.

---

## 1. Mission

Emulate the complete ND-100 <-> ND-5000 (SAMSON) communication path in RetroCore
so SINTRAN III VSX/500 can boot and drive a ND-5000 CPU. The octobus
frame/card/ACCP layer is DONE and verified; the shared-memory mailbox layer is NOT.

## 2. Current state (all verified working 2026-07-16)

### Code — `E:\Dev\Repos\Ronny\RetroCore` (branch `ethernet-ii-controller-fixes`, UNCOMMITTED)

| File | Status |
|---|---|
| `Emulated.HW\ND\CPU\NDBUS\NDBusOctobus.cs` | ND-100 3109 card. IOX 100400-100437, idents 40B/41B..46B/47B level 13 (LIVE-verified by TPE B00 + CONFIGURATION D05 diagnostics), frame routing, station-1 talk-back adapter, loopback for TPE tests. |
| `Emulated.HW\ND\CPU\NDBUS\OctobusFabric.cs` | The bus: 64-slot registry, dest->source rewrite, broadcast, timeout-as-null. |
| `Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs` | The ACCP/Access Module: AIB/AOB flags, emergencies 241B/242B/244B, kicks 1-6, multibyte OMD routing, ACCP commands (micro 1-3, multibyte CMCPURES/CMSYSPAR), `AttachCpu(IND500Cpu)` via TagWritten event. |
| `Emulated.Tests.ND100\ControllerOctobus\` | 54 tests, 52 pass + 2 pre-existing skips. Includes replays of carved CH5CPUPRESENT / XRS5CPU / CON5IDENT sequences. Run: `dotnet test Emulated.Tests.ND100\Emulated.Tests.ND100.csproj --filter "FullyQualifiedName~Octobus"` |

### Documentation (this folder + corrected legacy docs)

- `OCTOBUS-ND100-ND5000-REFERENCE.md` — master reference, [V]/[I]/[C] marked.
- `ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` — every ACCP-touching microcode routine.
- Corrected in place: `..\Devices\Octobus\OCTOBUS-PROTOCOL-REFERENCE.md`,
  `..\ND500\ND500-BUS-OCTOBUS-HW-INTERFACE.md`, the 324B carve README.

## 3. What is PROVEN (do not re-litigate)

- 16-bit frame: C=15, B=14, station=13-8, E=7, K=6, M=5, S=4 — NPL + manual + carved machine code (SOCTO dispatch, SKICK builder in `026-S3IMPIT.asm`).
- Kick numbers 1-6 (N100KICK=1, CLRKICK=3, IDLEKICK=6) — NPL symbols + manual p.336 + microcode OCB_DEC_K.
- CM* codes (CMMAC=41B, CMACO=42B, CMCPU=71B multibyte-only); emergencies = EBIT|code (241B/242B/244B).
- Card registers: +0 FIFO read, +1 frame write, +2 status (bit 3), +3 control (4=tx-en, 1=int-en, 20B=clear); output base 100404.
- Idents 40B/41B level 13 — settled by LIVE diagnostics (see §5 lesson 3).
- Microcode handshake: ACCP_READ/WRITE spin on AFLAG bits 9/10; kicks/idents arrive via AOB with ATRAP+OMESS; bit 15 terminates received multi-word messages.

## 4. THE REMAINING WORK (agreed plan, phases 1-4)

### Phase 1 — Shared-memory mailbox layer
Implement the X500DF per-CPU extension. Spec: `..\ND500\ND500-BUS-INTERFACE-REFERENCE.md` §6.5:
X5SEMA (offset 0, test-and-set semaphore, TSET=140123/TSETP=140516 semantics, owner word X5RES=47, -1 = ND-100), X5HEN=3 / X5FYL=4 / X5MXF=5 (FIFO head/fill/max), X5FIF=6 (element array). Backed by the same MPM RAM the ND-500 reaches via bit-31 addresses (`IND500Cpu.SetMpmMemory`). Model on the existing `NDSharedMemory` / `NDBusND500IF` patterns — load the **nd-500-bus-interface skill** for the 3022-generation equivalents.

**UPDATE 2026-07-17 (from the microcode session's lossless B30 verification —
`E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` §3.10 and
`..\ND500\MICROCODE-ANSWER-INIT-SAMSON-AND-13B-2026-07-17.md`):**
- [V] The 5800 microcode's message base is START_MESS = 0o20000 (+ per-CPU offset,
  patch constants SAMSON_CPU/OFFSET); during INIT_SAMSON, SYS_DATAF (025630) READS
  words at 0o20000+0x0A/+0x0C expecting the ND-100 to have written them already —
  so the ND-100 must populate the message area in MPM BEFORE the microcode finishes init.
- [V] SYSPAR is read FROM the ACCP during INIT_SAMSON (SYS_READ 017111: three
  ACCP_READ halfwords → SRF 0o2006-0o2010) — the `OctobusND5000Station` micro-command 1 /
  CMSYSPAR reply path is boot-critical, not optional.
- HARD REQUIREMENT (from the 13B zero-readback analysis): the mailbox backing store and
  the MPM-window/`SetMpmMemory` array MUST be the same bytes at the same offsets — a
  separate array or base mismatch reproduces the "reads back zeros" failure.
- [V] After servicing any message the microcode writes N5STA := 3 + calls GIVEINT — and
  GIVEINT is NOT just a frame send: it inserts the answered message's address into the
  X5FIF ring under X5SEM before strobing the interrupt (full 5-step sequence in Phase 2
  below). It never touches interface status registers.

### Phase 2 — Wire mailbox to kicks

**RE-CORRECTED 2026-07-17 per the architect session (GIVEINT decoded). Where this brief
disagrees with `..\ND500\ND500-MICROCODE-INTEGRATION-ARCHITECTURE-2026-07-16.md` §7.2 or
`E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` §3.1b, THOSE WIN.**

Activation (ND-100 -> ND-5000): SINTRAN links the message into the ex-queue at X5BEX
(per-CPU extension block word 0), clears X5ACT (0 = work); kick frame is the PREEMPT path
only (ACT52) — the servicer's real wakeup is a write-hook/poll on X5ACT, NOT
`KickReceived` alone. Servicer re-arms X5ACT := 1 (IDLE_2 semantics) BEFORE consuming the
queue; queue walk starts at X5BEX (-1 = empty), under X5SEM.

Answer (ND-5000 -> ND-100) — the GIVEINT sequence, ALL FIVE steps required
(pseudo-C §3.1b [V]; the earlier "N5STA + frame only" description was wrong — without the
ring insert SINTRAN's XN500 drain never sees the answer):
1. take X5SEM (test-and-set on global header word 0);
2. write N5STA := 3 (or 4 for trap-shaped stops) into the message;
3. insert the answered message's address into the X5FIF ring at slot[X5FYL] (32-bit
   slots), advance X5FYL := (X5FYL+1) mod X5MXF, full-check vs X5HEN;
4. release X5SEM;
5. send the interrupt frame to station 1 — the word is DYNAMIC, not 100401B:
   `((5OMDNO<<8 & 037400) >> 3) | 100001`, where 5OMDNO arrives as word 1 of the
   CMSYSPAR multibyte the station already receives (runtime-allocated by CONOMD; 10B is
   merely the observed live value — NEVER hardcode 100401B). Capture it in the station's
   CMSYSPAR handler.

**SHARED SERVICER — do NOT reimplement (drift risk):** "servicer runs" means the single
`Nd500MicrocodeServicer` extracted from `NDBusND500IF.ProcessMailboxMessage` by the
bus-interface session (phase S0/S1 of the architecture doc), consumed via `IServicerHost`.
Phase 2 GATES on that extraction landing. To keep moving before it lands, code Phase 2
against the `IServicerHost` interface with a stub — never a second MICFU engine, or every
future fix lands twice.

X5PRO: SINTRAN reads it cache-bypassed to choose flag-vs-kick (ACT51/ACT52). Write it in
Phase 1 (current process, -1 on idle — PRNOWR semantics); if deferred, leave a named TODO
plus a skipped test, because leaving it -1 forever means the preempt/kick path is never
exercised.

Drain on the ND-100 side: like the XN500 driver (`ND500-BUS-INTERFACE-REFERENCE.md` §7.5).

### Phase 3 — MON 60B validation
Replay the bring-up order from `..\ND500\nd-500-mon\ND500-BRINGUP-BUS-INTERFACE-FEEDBACK.md`: detect -> MEMDEF 040B -> CSLOAD 037B -> MICPSTART 025B -> swapper -> domain start; and DECOMESS -> MCHANDLE monitor-call returns. MON 60 subfunction table: `..\ND500\ND500-BUS-OCTOBUS-HW-INTERFACE.md` §2. NOTE: MON 60 never touches octobus registers directly — it rides the mailbox; that is why Phase 1 must come first.

### Phase 4 (independent) — ND-5000-originated messages
Generate TRAP_OCBM-format reports from the station: CPU-available 202B (makes SINTRAN's 5OMBREAD set `5ALIVE`), HW-fault 200B with the register-dump record (format: `ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` §7 + `MP-P2-N500.NPL:3372-3551`). Plus Messack/Messnak replies for ACCP library commands (codes: ND-05.020.01 ch. 5.3.11).

## 5. LESSONS / RULES (each cost real time — follow them)

1. **Octal truncation trap**: the debunked "C=bit9/B=bit8/dest=bits7-2" format came from octal values missing two trailing zeros. When a bit layout looks shifted by 6, check for dropped octal digits first.
2. **Wrong-image symbol trap**: 03xxxx SINTRAN symbols resolve against the PIT-mapped overlay (`026-S3IMPIT`), NOT commoncode. Validate any carve by multi-symbol coherence + actual IOX usage, never by "it disassembles".
3. **Live diagnostics beat static inference**: the ITB13+37B/+40B byte reading produced a wrong ident conclusion (37B/40B); TPE/CONFIGURATION diagnostics running against the emulator settled it as 40B/41B. When a real ND test program disagrees with your static analysis, the test program wins.
4. **Mark evidence**: every claim is [V] verified / [I] interpretation / [C] contradiction, with file:line. Fabricated-looking constants (power-of-two enums, "formula" idents) have burned this codebase twice — never invent values to make code compile.
5. **User's coding rules (absolute)**: no LINQ, no foreach; keep/extend comments (never strip citations); tests must be run and 100% green before claiming success; `dotnet format` for whitespace; never mention Claude/AI in commits; never kill processes; full paths when naming files.

## 6. Open questions (evidence does not exist in the repos yet)

- Broadcast type (BT) codes — "OCTObus Protocol Specification" document never found; MBSEND sets B when descriptor word X+2 != 0 (mechanism carved, meaning unknown).
- ACCP EPROM dump (68000 firmware) — would unlock full ACCP library emulation.
- ITB13 indexing scheme (why slot 37B holds the ident-40B datafield — plausibly table indexed from ident-1, UNVERIFIED).
- Exact microcode-sent kick word encodings (`100102|cpu` patterns in SENKICK) vs receiving-side kick numbers.

## 7. Key file map

| What | Where |
|---|---|
| Master protocol reference | `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md` |
| Microcode catalog | `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` |
| Mailbox/FIFO spec | `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` §6.5, §7.5 |
| MON 60 table + driver | `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-OCTOBUS-HW-INTERFACE.md` |
| Carved driver bodies | `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm` |
| NPL sources | `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\` (PH-P2-OPPSTART, MP-P2-N500, 5P-P2-MON60) |
| Hardware manual | `E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ND-5000 Hardware Description.md` (ch. 5 + App. 2) |
| Microcode disassemblies | `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` / `-A30.md` (lossless since ND5000UC commit a91dff4) |
| Mailbox servicer pseudo-C (microcode session) | `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` — N5STA protocol, MICFU dispatch, 13B/14B field layout, §3.10 verified offsets |
| INIT_SAMSON / 13B answer memo | `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\MICROCODE-ANSWER-INIT-SAMSON-AND-13B-2026-07-17.md` |
| Emulator + tests | `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\` + `Emulated.Tests.ND100\ControllerOctobus\` |
| Related skill | `nd-500-bus-interface` (3022/5015 generation patterns, MON 60B test shapes) |
