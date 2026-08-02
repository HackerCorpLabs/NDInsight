# Handoff — ACCP command dispatch: what is carved, what is left, and one index request

**Date:** 2026-08-02
**From:** the ACCP emulation effort (RetroCore `Machines.Accp` + this carve)
**To:** whoever picks up ACCP firmware RE next, and the owner of `ACCP-COMPLETE-REFERENCE.md`

---

## 1. What landed

`CARVE-ANSWER-OCTOBUS-ACCP-COMMAND-DISPATCH-AND-RTEST-2026-08-02.md`, in this folder. It carries:

- **The selftest status word is one address, `0x001131E2`** — read by both the boot console summary
  (`0xF1A4`) and the octobus `RTEST` arm (`0x6632`). Disassembly for both is in the doc.
- **The RTEST "contradiction" is closed, and a prior is withdrawn.** The card never contradicted
  itself; `CMSYSPAR` and `CPURES` clear the status word, and the original probe sent them first.
  Sent first, RTEST returns `00 07 7F` — exactly what the console printed. The earlier suggestion
  that RTEST might read a different word, or the ND-5000's status, is **wrong**; do not re-derive it.
- **The 46 dispatcher arm addresses**, confirmed three independent ways (chain walk, external
  naming from `N500-SYMBOLS.SYMB` + ND-05.020.01 §5.3, whole-image byte search). The byte search
  fixes the original scan: it adds `0x4D50` and drops the `0x63DC` false positive.
- **The reply convention, settled against the manual.** ack = `00`; nak = `FF <Messnak> 10 11`.
  `ALIVE` answering `FF 07 10 11` against §5.3.26's documented `7 = NOT alive` is what pinned it.
  Both halves were guesses before this.

## 2. What is still open

**The ~30 unnamed dispatcher arms.** 13 of the 46 commands have names from the manual and the ND-100
symbol table; the rest have none from any source. The arm addresses are all enumerated in the carve
doc, so the next step is mechanical: read the `cmpi.b #imm,D0` immediate at each site to build the
command→handler map, then carve handler semantics. That work was scoped and deliberately deferred —
it is a large RE effort, not a loose end that was forgotten.

**The `0x03` first content byte of the discovery request.** Two readings were eliminated on the
running card: it is not station-derived (it stayed `0x03` across stations `0x10`/`0x15`/`0x1F` while
byte 1 tracked), and it is not a per-message opcode (all six requests of one scan were byte-identical
`03 10`). The surviving reading — a fixed property of the card — is **narrowed, not proven**.
Proving it needs the **Octobus Driver Programming Guide** or a second card's ROM; neither is in
this repo. Bonus finding: byte 1 is the card's OWN station in all six requests, not the addressee.

## 3. One request, because I would not edit your file

The routing rule says a new carve must be added to its area's status-of-record in the same change.
For ND-5000 that is `ACCP-COMPLETE-REFERENCE.md` — **your live working file, which I read but do not
edit.** So the index line is not there.

Please add a pointer to
`CARVE-ANSWER-OCTOBUS-ACCP-COMMAND-DISPATCH-AND-RTEST-2026-08-02.md` wherever selftest status and
the command set are covered, or tell me you would rather I edit the reference directly in future and
I will.

If any of it duplicates something you already carved independently, say so and I will cut mine down
to a pointer rather than leave two accounts of the same word.

## 4. Emulator-side counterparts

- `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpSelftestStatusTests.cs`
- `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpCommandChannelTests.cs`
- `E:\Dev\Repos\Ronny\RetroCore\Nuget\_shared\docs\ACCP-FORWARD-PLAN-2026-08-01.md`

Suite state at handoff: **101/101 passing**, four consecutive clean single-invocation runs.

## 5. One caveat on that suite

An earlier intermittent crash was never reproduced, so the "memory pressure from many machine
instances" hypothesis is **unsupported**. What was observed instead: three test hosts from three
projects live at once (RetroFS, Accp, Emulated.Tests.ND500), including a second concurrent Accp run
contending for the same `bin\Debug` output. That is a plausible explanation and it is **UNVERIFIED**.
Full-suite timing numbers from 2026-08-01/02 are not clean for the same reason.
