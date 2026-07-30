# ACCP machine - defect report from the first full boot log

**Date**: 2026-07-28
**Subject**: the RetroCore ACCP machine, which now boots the real firmware to the `ACCP:` prompt
**Evidence**: the firmware image `octo.bin` and the Ghidra database; every claim below is read
from instruction bytes or computed, and says which.

Reaching the prompt is a real milestone. This lists what the log shows is still wrong, ranked
by how much damage it does if left.

---

## D1 [CRITICAL] The four "completed OK" tests are FALSE PASSES - root cause found

> `Register test abcd completed OK`
> `TSB test completed OK`
> `Instruction Cache test completed OK`
> `Data Cache test completed OK`

With no ND-5000 attached none of these can legitimately pass. Here is exactly why they do.

`MfBusCmdDataPairStatus` @0x7374 - the routine every one of them calls:

```
737C  tst.w  g_skipBusyWaitFlag (0x113138)
7382  bne    skip                        ; flag set -> do not wait
7384  btst.b #0,(0x00660001) ; beq -10   ; else spin for "command done"
738E  move.w (0x00550000),D0 ; swap D0   ; high half
7396  move.w (0x00440000),D0             ; low half
73A0  move.w #0x5,(0x00220000)           ; function code 0x0005
73A8  return D0                          ; the 32-bit data pair
```

`Selftest_Tsb` @0xE818 then does:

```
E846  jsr    MfBusCmdDataPairStatus
E850  tst.l  D0
E852  bne.b  fail                        ; NON-ZERO = failure
      ; falls through on ZERO ->
E854  print "completed OK"
```

**Zero means PASS.** The emulator returns 0 for reads of `0x440000` / `0x550000`, so D0 is 0,
so the test reports success without touching any ND-5000.

### The crux - the same read has two opposite meanings

The identical data pair is interpreted **as data** by one group of tests and **as an error
code** by another:

| Group | Interprets the read as | Reading 0 gives |
|---|---|---|
| BUS / MIR / Control Store / Control Cache | test **data**, compared against the LCG pattern | correct FAIL (`Result: 00000000H`) |
| Register / TSB / Instruction Cache / Data Cache | an **error code**, 0 = no error | **false PASS** |

That is precisely the pattern in the log, and it means the failures and the passes have the
same cause. **Fixing the failures is not the job - fixing the passes is.**

### What to do

Reads of `0x440000` / `0x550000` with no responder attached must not yield a clean zero.
Options, in preference order:

1. Model a **no-responder / timeout** result and return a distinct non-zero error code. This
   is closest to hardware and makes both groups behave correctly.
2. Failing that, return a fixed sentinel (e.g. `0xFFFFFFFF`) so the second group fails loudly
   instead of passing silently.

**Do not** leave it at 0. A test that fails is the model behaving correctly; a test that
passes without the hardware is the model lying, and it will silently validate broken behaviour
in every later phase.

Also note `g_skipBusyWaitFlag` (0x113138) at 0x737C: when non-zero the routine **skips the
0x660001 bit 0 wait entirely**. Worth knowing before tuning status bits.

---

## D2 [HIGH] A wrong expected value - and the correct one is now certain

Log 1 printed `... 0D8CH F58BH AFBEH ...`; this log prints `... 0D8CH F538H AFBEH ...`.

The expected values are **not stored constants** - a byte search for them in the ROM finds
nothing. They are generated. `SelftestPatternLcgSeed` @0xB3DC sets:

```
g_lcgMultiplier (0x00114584) = 0x00010DCD = 69069
g_lcgIncrement  (0x00114588) = 0x0000006F = 111
g_lcgSeed       (0x0011458C) = 69069        (passed in D0 by the caller)
```

so the pattern is a linear congruential generator:

```
next = (seed * 69069 + 111) mod 2^32
```

**Fully deterministic** - no hardware input, no timer, no uninitialised memory. The firmware
therefore cannot vary here, on real or emulated hardware.

Computed 2026-07-28:

```
v1                  = 0x1C587698     <- matches the BUS test line exactly
low words of v1..v8 = 7698 B027 0AAA 2C91 0D8C F58B AFBE 6195
```

**`F58BH` is correct; `F538H` is wrong.** Either this build regressed or the log was mistyped -
but the ground truth is now fixed and those eight words belong in a unit test as literals.
(69069 is the Marsaglia/VAX multiplier; ND used the same family for XMSG's ZRAND.)

---

## D3 [MEDIUM] Banner spacing is being collapsed

The ROM string at 0x11729 is the **spaced** form:

```
******   S A M S O N   A C C E S S   P R O C E S S O R   ******
```

Log 1 reproduced it. This log shows `******  SAMSON ACCESS PROCESSOR  ******`. If the output
path is collapsing runs of spaces, it will also corrupt every column-aligned diagnostic the
firmware prints - and several of the selftest reports are column-aligned.

---

## D4 [CONFIRMED - same register as D1] `Result: 00000024H` is a STALE READ-BACK

Carved 2026-07-28 at 0xE186, in the test that follows the "Loading control store with
selftests..." announcement:

```
E186  move.l #0x55555555,(0x18,A6)          ; the expected value
E18E  jsr    ControlStore_Helper_79BC        ; prepare
E194  jsr    MfBusCmdDataPairStatus @0x7374  ; D0 = (0x550000)<<16 | (0x440000)
E1A6  cmp.l  (0x18,A6),D0
E1AA  bne    fail                            ; -> "failed / Result / Expected"
```

So `Result: 00000024H` **is the value your machine returned from the data pair**: high word
`0x0000` from `0x550000`, low word `0x0024` from `0x440000`.

**This is the same register as D1, and it proves the model is inconsistent.** In the D1 path
the pair reads back `0x00000000`; here it reads back `0x00000024`. It cannot be both unless the
pair is returning **stale state rather than a modelled value**.

### The likely mechanism, and it is specific

`0x24` is exactly `$`, the ND string terminator that ends every message this firmware prints.
`AobSingleWordWrite` @0x72A0 writes words to `0x440000`, and the firmware has an ND-100 output
path (`Nd100OutputQueue` @0x1C8A, plus the `"ND100 output ... buffer overflow"` strings). If
console or ND-100 output passes through the AOB, the **last word written to `0x440000` before
this test is the `$` that terminated the previous string** - and a read-back-what-was-written
model hands it straight back.

**Marked as the leading hypothesis for the mechanism; the value 0x24 arriving from 0x440000 is
proven.**

### What this means for the D1 fix

D1 and D4 are one bug: **`0x440000` / `0x550000` are implemented as read-back-what-was-written
(or as uninitialised storage), and the firmware treats those reads as real responses.** That
single behaviour produces both symptoms - a clean 0 becomes a false "no error" pass, and a
stale 0x24 becomes a bogus data mismatch.

Fix them together: reads of the data pair with no responder attached must return a **modelled
no-responder result**, never the last value written and never a bare zero.

---

## D4-original [superseded] `Result: 00000024H` - 0x24 is the newline marker

> `Loading control store with selftests... failed`
> `Result   : 00000024H  Expected : 55555555H`

**0x24 is exactly `$`**, the ND newline/terminator byte embedded in every string in this
firmware. A control-store read-back handing back 0x24 suggests a data register or the staging
buffer `g_microwordStagingBuffer` (0x001144F0) is returning a byte that passed through the
console/string path. Check whether `0x440000` / `0x550000` are shared with, or not cleared
between, the string writer and the control-store path.

**Marked as a hypothesis, not proven** - 0x24 could be coincidence. But it is a specific enough
coincidence to check first.

---

## D6a [HIGH - NEW REGISTER] `HW_ACCP_STATION_CONFIG` @ 0x00900001 is being missed

Carving D6 turned up **a hardware register that was not in the handoff table**, and the machine
almost certainly returns 0 for it.

`MfBusControllerConfigCheck` @0x121C, first thing it does:

```
122E  move.b (0x00900001).l,(0x19,A6)     ; the board's own config byte
1260  moveq  #0x1F,D0
1262  and.b  D0b,D1b                      ; mask to 5 BITS
1268  move.b D1b,(0x17,A6)                ; -> the ACCP's OWN STATION NUMBER
```

Five bits = 0..31, matching the octal station ranges in ND-14001 section 4.8.1 (global 0-17B by
thumbwheel, local 20B-77B by register). On real hardware this is the thumbwheel/DIP setting.

**An emulator must supply a sane, unique value here. Zero is an illegal OCTObus station
number**, and returning 0 gives the card an invalid identity before it says a word on the bus.

Note this is `0x900001`, distinct from the already-known `0x900007`. Select 0x90 has at least
two registers, and `0x90` is not a replicated nibble - do not let the address decoder assume
the nibble rule.

---

## D6b [HIGH] The MFbus discovery protocol - exactly what to answer

The "not found at Octobus stations 2-7" message is not a vague failure. It is a precise scan
you can satisfy:

```
1236  station = 1
123C  station = station + 1                 ; -> 2 first pass
1290  cmp.b (0x1A,A6),D2   with D2 = 7
1294  bcs   exit                            ; loop while station <= 7  -> STATIONS 2..7
12A4  lea   (0x1C,A6),A0                    ; the OBCON request block
12A8  jsr   ObconRequestDispatch @0xF686
12AE  cmpi.w #-0x7D00,(0x1E,A6)             ; status == 0x8300 ?
12B4  bne   next station
12BC  jsr   MFCRECEIVE @0x14B4              ; on success, read the reply
```

Request block built at (0x1C,A6) - **and it independently confirms the layout carved from
`Cmd3B_SendKickOctobus`**:

| Offset | Value | Field |
|---|---|---|
| +0x00 | `0x0041` | function code - **multibyte message** |
| +0x02 | - | **status; must return `0x8300` for "found"** |
| +0x06 | `0x05` | process / subprocess |
| +0x0C | station | destination, 2..7 |
| +0x0E | `0x05` | |
| +0x10 | 12 bytes | array descriptor over the message buffer |

**To get past this**: answer OBCON function 0x41 addressed to a station in 2..7 with status
`0x8300` *and* a reply `MFCRECEIVE` @0x14B4 can parse. Anything less and the firmware is right
to report "not found" - which means the current message is the model behaving correctly, not a
bug. Fix D6a first; the station identity comes before the scan.

---

## D5 + D6 [RESOLVED - both are artifacts, one root cause] `ND-5800` and the contradictory MFbus line

**Both lines come from the same thing: nothing answered a signature probe.** Neither is a
defect, and neither is a real report. Carved from `DetectCpuModelBySignature` @0x110A.

The firmware probes memory through A0 for the 16-bit signature **`0x7F55`** at byte offsets 0,
4 and 0x0C, in a three-class chain:

| Class | Condition | Sets |
|---|---|---|
| 1 (0x1150) | signature at +0 and +4 | `g_cpuModelClass`=1, `g_cpuModelCode`=`0x5200` |
| 2 (0x1184) | signature NOT at +0x0C or +4 | `g_cpuModelClass`=2, `g_cpuModelCode`=`0x5400` |
| **3 (0x11D2)** | signature NOT at +0x0C or +4 | `g_cpuModelClass`=3, **`g_cpuModelCode`=`0x5800`** |
| - | signature FOUND at a probe point | `clr.b g_cpuModelClass` (= unknown) |

Each class cross-checks D0 (the expected model) against its permitted set - class 1: `0x5200`;
class 2: `0x5400`/`0x5500`/`0x5700`; class 3: `0x5800`/`0x5900`. On a match it sets
`g_cpuModelConfirmed` (0x1131FA) = 1. Then:

```
1204  tst.w  g_cpuModelConfirmed
120A  bne    done
120C  ori.w  #0x8000,(g_controlStoreErrorLatch 0x001131E2)
```

**Class 3 / ND-5800 is the LAST branch in the chain.** With every probe read returning 0, no
signature is ever found, the chain falls all the way through to class 3, D0 fails to match, so
`g_cpuModelConfirmed` stays 0 and the error bit is set. That produces **exactly** the two lines
in the log, in that order.

So: `CPU model: ND-5800` is the fall-through default, not a detection, and
`MFbus controller has incorrect CPU model setting.` is its direct consequence. **Do not treat
the ND-5800 line as evidence of anything** until something answers the probe - which matters,
because ND-5800 versus classic ND-500 is the 128-bit versus 144-bit microword distinction.

Bonus finding: `g_cpuModelCode` is **hex-coded decimal**. `0x5800` prints as "5800" because the
console's default base is 16 (`g_numberBase` = 0x10, set at 0x213A).

**To make this line meaningful**, answer the `0x7F55` signature probe at the right offset for
the model you want to present. That is a Phase 5/6 task, not a bug fix.

---

## NOT A DEFECT - do not "fix" these

### The `a` / `ab` / `abc` / `abcd` letters are genuine firmware output

`Selftest_MirTest` @0xB8C8 at 0xB908:

```
B908  move.b #0x61,(0x14,A0)      ; 0x61 = 'a'
B90E  jsr    ConsPutCharQueued
```

One progress letter per completed sub-phase. `MIR test a failed` = got through phase a;
`Register test abcd completed OK` = all four phases ran. The accumulation is correct.

### The BUS / MIR / Control Store / Control Cache failures are the correct outcome

Those tests target an ND-5000 that is not modelled. Their `Expected:` values are the ready-made
oracle for the day one is attached.

---

## Free assertions for the test suite

| Assertion | Value |
|---|---|
| BUS test expected | `0x1C587698` |
| MIR / Control Store 8 words | `7698 B027 0AAA 2C91 0D8C F58B AFBE 6195` |
| Selftest status word | `0x043F` (a failure bitmask - one cheap assertion instead of diffing console text) |
| RAM walk-test results | `g_ramTestErrors_firstHalf` 0x11312A (**32-bit**), `_secondHalf` 0x11312E (32-bit), `g_ramTestDone` 0x113132 (**16-bit**) |

---

## Provenance

D1's mechanism was read from the disassembly of 0x7374 and 0xE818. D2's LCG parameters were
read from 0xB3DC and the sequence computed independently; v1 matches the firmware's own printed
value exactly, which is what validates the computation. D3 is a string comparison against
0x11729. D4, D5 and D6 are labelled as hypotheses. The "not a defect" items were read from
0xB908.
