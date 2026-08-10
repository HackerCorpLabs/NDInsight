# CARVE ANSWER — ACCP command dispatch chain, and where the selftest status really lives

**Date:** 2026-08-02
**Image:** `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
**Ghidra program:** Raw Binary, `68000:BE:32`, base 0
**Card:** Samson ACCP, ND-324716 / PCB 5616
**Produced from:** RetroCore emulation work — every claim below was cross-checked against a
running emulated card, not read out of the listing alone.

Claim tags: **[V]** byte-verified in the image or reproduced on the running card ·
**[I]** inferred · **[OPEN]** unresolved.

---

## 1. The selftest status word — one word, not two

**[V] There is exactly one selftest status word: `0x001131E2`.** Both the boot console summary
and the octobus `RTEST` reply read it.

`RTEST` (`0x30`, 060B) dispatcher arm:

```
00006616  cmpi.b  #0x30,D0
0000661a  bne.b   0x6644                  ; next arm in the chain (0x3D)
0000661c  clr.w   (0x001144ec).l          ; RTEST CLEARS these two first
00006622  clr.w   (0x001144ea).l
00006628  moveq   #0x0,D0
0000662a  jsr     0x6986                  ; emit the ack byte 0x00
00006632  move.w  (0x001131e2).l,D0w      ; <-- the status word
00006638  jsr     0x69d0                  ; emit it as a word
00006640  bra.w   0x6878                  ; common exit
```

Console selftest summary at `0xF1A4` — **same address**:

```
0000f1a4  move.w  (0x001131e2).l,D0w
0000f1aa  andi.l  #0xffff,D0
0000f1b0  tst.l   D0
0000f1b2  bne.b   0xf1d0                  ; non-zero -> print "failed" + the value
0000f1b4  lea     (0x11ae8).l,A0          ; zero     -> print the OK string
```

### The apparent contradiction, and its actual cause

The card prints `Selftest failed. Selftest status: 077FH` at boot, but an `RTEST` sent over the
octobus answered `00 00 00` — ack plus status `0x0000`. Two of the card's own outputs disagreeing.

**[V] The card was consistent; the measurement was not.** `RTEST` sent as the FIRST command, with
nothing before it, returns `00 07 7F` — exact agreement with the console. The original probe had
sent `CMSYSPAR` (`0x0E`) and `CPURES` (`0x39`) first, and **those clear `0x001131E2`**.

**A poisoned prior, recorded so it is not re-derived:** the earlier framing — *"RTEST may read a
different word, or the ND-5000's status rather than the ACCP's"* — is **wrong and withdrawn**. Both
paths provably read one address. The question was never *which word* but *when*.

**[V]** `RTEST` additionally clears `0x001144EC` and `0x001144EA` before replying (see the arm above).
A command with an undocumented side effect on a status word is exactly what made the first
measurement lie.

---

## 2. The command dispatcher — 46 arms

**[V] The dispatcher is a linear `cmpi.b #imm,D0` + `bne` chain, not a jump table.** This is the
normal PLANC `CASE` shape (see the `ghidra-planc` skill); do not go looking for a table.

The 46 arm addresses:

```
4d50 4edc 4fc0 519c 52c6 547e 558a 562e 568c 56bc 56ea 5736 57e8 583a 58a4 5980
59b6 5a46 5ab0 5b38 5bc8 5c44 5cc0 5d56 5dc0 5e64 5ece 5f38 5fd6 6016 608c 60f6
6178 61f4 6326 6390 63b8 6408 6438 6504 6534 6562 65b6 6616 6644 66b6
```

**[V] Confirmed by three independent methods**, which matters because the first scan was flawed:

1. **Chain walk** — following `bne` targets from the head of the dispatcher.
2. **External naming** — ND-100 `N500-SYMBOLS.SYMB` plus ND-05.020.01 §5.3 name 13 commands. All 13
   fall inside the code runs `0D-18`, `1B-2D`, `30-3E`, and **none lands in a gap** between runs.
3. **Whole-image byte search** for `0C 00 00 ?? 66` — exactly 46 sites. This correctly **includes**
   `0x4D50` (which the original flawed scan missed) and **excludes** `0x63DC` (a `beq` false positive).

### Command codes confirmed against the running card

| Code | Name | Source | Reply observed **[V]** |
|---|---|---|---|
| `0x0E` | CMSYSPAR (016B) | ND-05.020.01 §5.3 | `00` (ack). Clears `0x001131E2`. |
| `0x11` | LPARP (021B) | ND-05.020.01 §5.3 | not exercised |
| `0x12` | VPARP (022B) | ND-05.020.01 §5.3 | not exercised |
| `0x1B` | STARTMIC (033B) | ND-05.020.01 §5.3 | not exercised |
| `0x1C` | STOPMIC (034B) | ND-05.020.01 §5.3 | not exercised |
| `0x1D` | CONTMIC (035B) | ND-05.020.01 §5.3 | not exercised |
| `0x1E` | RESTMIC (036B) | ND-05.020.01 §5.3 | not exercised |
| `0x1F` | ALIVE (037B) | ND-05.020.01 §5.3 | `FF 07 10 11` (nak, code 7) |
| `0x2A` | LOCSM (052B) | ND-05.020.01 §5.3 | not exercised |
| `0x30` | RTEST (060B) | ND-05.020.01 §5.3 | `00 07 7F` when sent first |
| `0x31` | ENKICK (061B) | ND-05.020.01 §5.3 | not exercised |
| `0x32` | DISKICK (062B) | ND-05.020.01 §5.3 | not exercised |
| `0x39` | CPURES (071B) | ND-05.020.01 §5.3 | `00` (ack). Clears `0x001131E2`. |

**[OPEN]** ~30 of the 46 arms have no name from any source. The arm addresses are enumerated above;
mapping each to its command byte is a matter of reading the `cmpi.b` immediate at each site, which
has not been done.

---

## 3. The reply convention — settled

**[V]** ack = a single `0x00`.
**[V]** nak = `FF <Messnak code> 10 11`.

Both halves were previously **guesses** and are now confirmed. The confirming case is `ALIVE`: the
card answers `FF 07 10 11`, and ND-05.020.01 §5.3.26 documents exactly one nak for ALIVE —
**`7 = NOT alive (stopped)`**, which is correct for a card with no microprogram running. That single
agreement pins `0xFF` as the nak marker and byte 1 as the Messnak code.

**[V] Malformed input is handled, not fatal.** An OBCON message with an empty body produces
`Communication error at address 6FE4H` → `ACCP Software Reset performed` → a clean reboot.

---

## 4. Cross-references

The emulation-side counterpart of this carve, with the same disassembly reproduced in the test
`<remarks>` so it is readable at the point of use:

- `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpSelftestStatusTests.cs`
  — section 1 above; three tests including "RTEST sent first agrees with the console print".
- `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpCommandChannelTests.cs`
  — sections 2 and 3; drives the real SINTRAN command codes at the card.
- `E:\Dev\Repos\Ronny\RetroCore\Nuget\_shared\docs\ACCP-FORWARD-PLAN-2026-08-01.md`
  — the plan this work closed out, including what was left open and why.

Related NDInsight docs: `ACCP-COMPLETE-REFERENCE.md` (another agent's live file — read, do not edit),
`OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`, `SINTRAN-OCTOBUS-MESSAGE-CATALOG.md`.

---

## 5. Method note worth keeping

Two of the wrong turns here were the same shape: **a measurement whose premise was wrong looks
exactly like a subject that behaves wrongly.** The RTEST "contradiction" was command ordering, not
card behaviour. A separate discovery-scan test failed on `requests.Count > 1` because a peer that
answers short-circuits the scan after one request — nothing to do with the byte under test. Read the
output before drawing the conclusion.
