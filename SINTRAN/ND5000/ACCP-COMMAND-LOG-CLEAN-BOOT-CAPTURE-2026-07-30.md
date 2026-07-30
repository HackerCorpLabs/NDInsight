# ACCP command log - one clean SINTRAN boot over the octobus

**Date**: 2026-07-30
**From**: the SINTRAN-over-octobus side (ND-100 card, `OctobusND5000Station`).
**Answers**: section 6 of `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` ("the most useful single
artefact is the command log from one clean boot, with the AIB command numbers and the answers").

The capture is BIDIRECTIONAL: every OMD-3/OMD-4 multibyte command the ND-100 sent, and every
reply our station sent back, with the full payload bytes. `IN` = ND-100 to station.
`OUT` = station to ND-100.

Produced by `Nd100SintranNd5000OctobusBootHarnessTests.FullFlow_Octobus_Login_Nd500_Status_StartSwapper_Capture`
(`DumpAccpExchange("full-run")`). Machine-readable original:
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\sintran-octobus-accp-exchange-full-run.txt`

---

## Totals for this run

| | Count |
|---|---|
| Commands in (ND-100 to station) | **149** |
| Replies out | **150** |
| Commands with NO reply | **0** |
| `LCS0` (023B, control-store load) commands | **128** |
| `244B TERMINATE ACCP` emergencies | **1** (see the correction below - we first reported 0) |

The extra outbound message is the one UNSOLICITED reply in the whole run: the `TRAP_OCBM 202B`
model/version report we emit at `ENKICK`. It answers no command, which is why out exceeds in
by exactly one.

## Read this before comparing against your command-3 answer

Two things in the trace look wrong at a glance and are not:

1. **`LSSYSPAR` arrives on OMD 4 and is answered on OMD 3.** That is deliberate. The reply goes
   to the S5 reply-to OMD carried in the message body (`message[4]` = 5OMDNO), not to the OMD the
   command arrived on. SINTRAN's `5OMBREAD` only sets `CPUAVAILABLE.5ALIVE` for an ACK that
   arrives there, so answering on the arrival OMD would leave the monitor printing
   "No ND-500(0) CPU found".

2. **`ALIVE(037B)` is answered `Messnak err=7`.** Correct at that point in the ladder: 037B
   means "is the microprogram running", and at that moment it is not (this is before
   `STAMIC0`). Error 7 = not alive. SINTRAN expects the refusal and continues.

**Where the model digit actually crosses**: line 377 of the trace,
`OUT omd=3 [82 01 38 38 2E 9A]`. Byte 3 and byte 4 are both `0x38` (model, ND-5800) - current
model as seen on the ACCP/backplane side, then my-model read from loaded control-store word 7.
They are equal by construction on our side. Version `0x2E9A` is control-store word 1 (LARG) for
the 5800-B30 image. This is the byte your command 3 has to agree with.

## About the 244B evidence you asked for - CORRECTED 2026-07-30

**We first wrote that a clean run contains no 244B. That was wrong, and we are correcting it
before you write it down.** A clean run DOES send one, in the same place, after the same three
answered commands. Its own footer:

```
# commands=147 unanswered=0 accpIdle=False
# 244B TERMINATE snapshot: 244B TERMINATE after 3 ACCP commands, 0 unanswered.
  Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
```

The mistake was ours and worth naming: our first clean capture predated the footer field that
records this snapshot, so the line was simply not being written. We read a missing FIELD as a
missing EVENT.

The honest statement for your documentation:

- 244B is **not** evidence of a timeout. It arrives with a 100%-answered command history behind
  it, in every run we have, fixed or not.
- The G10 fix does **not** stop SINTRAN sending 244B. It stops the resulting `_accpIdle` from
  sticking: the flag ends `False` instead of `True`, so later kicks are no longer swallowed.
- Treat 244B as an unconditional bring-up step. Do not treat receiving one as a fault signal.

`ACCP-244B-TERMINATE-PREFIX-CAPTURE-2026-07-30.md` shows the CONSEQUENCE of the stuck flag (the
pre-fix run where every subsequent kick dies). The 244B itself is in either capture.

## The trace

```
IN  omd=3 [03 07 0E 01 03 00 00 00 00] cmd=LSSYSPAR(016B/LoadSysPar)
OUT omd=3 [00 00] Messack(status 0)
IN  omd=4 [04 01 30] cmd=READSELFT(060B/ReadSelftestStatus)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 07 0E 01 03 00 00 00 00] cmd=LSSYSPAR(016B/LoadSysPar)
OUT omd=3 [00 00] Messack(status 0)
IN  omd=4 [04 01 1F] cmd=ALIVE(037B)
OUT omd=4 [FF 07 00] Messnak err=7
IN  omd=4 [04 01 32] cmd=DISKICK(062B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 1C] cmd=STOPMIC(034B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 39] cmd=CPURES(071B/ResetSamsonCpu)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 32] cmd=DISKICK(062B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 1C] cmd=STOPMIC(034B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 05 11 00 00 08 00] cmd=LPARPNT(021B/LoadParamPtr)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 12] cmd=VERPAP(022B/VerifyParamPtr)
OUT omd=4 [00 65 96 9B 49] VPARP echo
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0000 pb+4=[0000 0000 0001 8000 0000 0000 194F 2E9A 4000 0001 DE01 6010]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0080 pb+4=[4000 0000 0201 5000 0000 0000 1558 0000 4000 0000 0801 4000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0100 pb+4=[4000 800E 7E01 0000 0000 A24F 0101 0005 4000 000E 7E01 A000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0180 pb+4=[4000 0006 7E01 2000 0000 024F 0181 0005 4000 0004 3C01 0000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0200 pb+4=[4000 000E 7E01 2000 0000 020F 0201 0004 F000 0000 6521 2000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0280 pb+4=[4000 0002 7E01 8000 0000 A049 0281 0004 4000 0002 7E01 8000]
OUT omd=4 [00 00] Messack(status 0)

    ... the LCS0 / Messack pair repeats to 128 loads in total. Only the first six
    carry a CMWWC content line here because our multiport-window dump is armed for
    the first six; the remaining 122 are identical in shape:
        IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
        OUT omd=4 [00 00] Messack(status 0)

IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #1 N=1 csWord=0x0000 SigmaR25=0xC7EA cs[0..7]=0000,0000,0001,8000,0000,0000,194F,2E9A
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #2 N=1 csWord=0x1000 SigmaR25=0xC410 cs[0..7]=4000,000C,5401,2000,0000,0000,1003,0000
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #3 N=1 csWord=0x2000 SigmaR25=0x8604 cs[0..7]=5000,0001,9602,9000,0000,F000,2001,0000
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #4 N=1 csWord=0x3000 SigmaR25=0x8A8C cs[0..7]=D001,8000,56B0,B000,0000,0000,33DB,0000
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 32] cmd=DISKICK(062B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 1C] cmd=STOPMIC(034B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 03 36 00 00] cmd=STAMIC0(066B/StartMicDirect)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 31] cmd=ENKICK(061B)
OUT omd=4 [00 00] Messack(status 0)
OUT omd=3 [82 01 38 38 2E 9A] TRAP_OCBM 202B model/version report (model=0x38 version=0x2E9A)
IN  omd=4 [04 01 10] cmd=REOD(020B/ReadEodLevels)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 3D] cmd=PRGMVERS(075B/ReadAccpPrgmVersion)
OUT omd=4 [00 00] Messack(status 0)
```

Each `DUC0` also produces 18 `DUCS-OVR` byte-level lines showing how the microprogram writes the
dumped control-store word back into the multiport window one byte at a time. They are in the
machine-readable file but stripped here as noise; ask if the byte-splitting order matters to you.

## The ladder in words

1. `LSSYSPAR` twice, `READSELFT` - system parameters and self-test status. The first `LSSYSPAR`
   is what earns `5ALIVE`.
2. `ALIVE` refused (microprogram not started yet), then `DISKICK` + `STOPMIC` + `CPURES` +
   `DISKICK` + `STOPMIC` - put the CPU and the ACCP into a known stopped state.
3. `LPARPNT` then `VERPAP` - the parameter-area pointer handshake. `VERPAP` is the ONE command a
   canned answer cannot pass, because it echoes a 32-bit word SINTRAN wrote into shared multiport
   memory.
4. **128x `LCS0`** - the control store, loaded through the multiport window.
5. **4x `DUC0`** - dump-back checksum verification at control-store words 0, 0x1000, 0x2000,
   0x3000.
6. `DISKICK` + `STOPMIC` + `STAMIC0` + `ENKICK` - start the microprogram, then hand the octobus
   over to it. Our `TRAP_OCBM` model report goes out here.
7. `REOD` + `PRGMVERS` - read EOD levels and the ACCP program version.

In the whole run exactly **one** kick is ever sent, and not in this ladder: `CLRKICK` (kick 3) at
`stop-system`. Activation is the `X5ACT := 0` write, never a kick.

## Related documents

- `QUESTIONS-TO-ACCP-TEAM-2026-07-30.md` - the questions this answers section 6 of
- `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - their replies
- `ACCP-ND5000-CPU-INTERFACE-SPEC-2026-07-30.md` - the interface spec
- `OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md` - our gap register (G1-G10)
- `STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` - the `stop-system` / `CLRKICK` analysis
