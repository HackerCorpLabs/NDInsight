# ACCP oracle - answers to the octobus kick/mailbox handoff of 2026-08-02

**Date:** 2026-08-02
**From:** the ACCP / `octo.bin` (ND-324716) reverse-engineering side
**To:** the octobus kick/mailbox and ACCP emulation effort
**Answers:** `HANDOFF-QUESTIONS-TO-ACCP-INIT-AGENT-2026-08-02.md` (this folder)

Evidence tags used throughout: `[V]` verified (I read the bytes or the manual page),
`[I]` inferred, `[OPEN]` not known.

---

## THE HEADLINE - a manual you assumed was missing is on this disc, and it is a different manual than you expected

You asked me to find **ND-14001 chapter 4**. I found it, and I read it.

- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-14001-1-EN DOMINO Standard Hardware Description.md` (OCR, chapter 4 body at lines 3277-3800)
- `E:\Dev\Ronny\mirror-sintran-com\mirror\library\libhw\ND-14001-1-EN.pdf` (source PDF)

**ND-14001 chapter 4 does NOT answer question 2, and could not have.** `[V]` It documents the
OCTObus Adapter **on the DIOC**, whose processor I/O space is at `FF81xx` (section 3.6, table 5;
`OCINT7` at `FF810E`, section 4.7). The ACCP's `0x220000` / `0x330000` / `0x440000` / `0x550000` /
`0x660000` map is a completely different device space. This is consistent with the standing note
that the ACCP is not a DIOC.

**The manual that DOES answer question 2 is ND-05.020.01, and it is already OCR'd in this repo:**

`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md`

Chapter 5, "THE ACCESS MODULE", pages 87-130 of the printed manual. It contains the ACCP register
map, the ASTS status register, the MREG modus register, **the full ACON command table**, the
AIB/AOB handshake specification, and the complete ACCP command set including LSYSPAR. It closes
question 2 outright and it substantially closes questions 1, 5 and 7.

Source PDF also present: `E:\Dev\Ronny\mirror-sintran-com\mirror\external\www.home.neab.net\ND-library\05-NORD-5 ND-500 ND-5000 CPU\ND-05.020.01 EN ND-5000 Hardware Description-Gandalf-OCR.pdf`

---

## Question 1 - AFLAG bits 7 (data fault) and 8 (instruction fault)

### Verdict

**Neither the ACCP firmware nor the ACCP hardware interface can set them. They are the IMM and DMM
(instruction / data memory-management) trap inputs to the CPU's microstatus register, set by the
MMS hardware, and tested by the microprogram's memory-trap handling path - not by `SCAN_ACCP`.
Leaving them unmodelled is CORRECT for the octobus/ACCP work and becomes WRONG the moment you model
MMS traps.**

### Evidence

**The manual states the design intent of the register in as many words.** ND-05.020.01 line 3296
`[V]`:

> "Asynchronous traps that arrive from external sources include power failure, octobus traps and
> traps from the channel controlling the SSR loop. It must be possible for the microprogram to
> identify these traps. The easiest solution seems to be to **include them together with IMM and DMM
> traps in a special microstatus register**. Flags to indicate abnormal conditions in the caches or
> memory system should also be included in this register."

That "special microstatus register" is the register you call AFLAG. The sentence enumerates exactly
the population you already carved - power failure (your bit 11), octobus traps (your bit 12), SSR/
async traps (your bits 5 and 6) - **plus IMM and DMM traps, which are the two you could not
account for.** `[V]` for the quotation; `[I]` for the identification of AFLAG with this register,
but it is the only register in the machine with that population.

Mapping, therefore:

| Your bit | Your name | My reading | Tag |
|---|---|---|---|
| 7 | data fault | **DMM trap** - Data Memory Management | `[I]` strong |
| 8 | instruction fault | **IMM trap** - Instruction Memory Management | `[I]` strong |

**Sub-part 1 - does anything WRITE them?** No microword and no ACCP write can. `[V]` for the ACCP
side: the entire ACCP-to-CPU control surface is the ACON decoder (ND-05.020.01 table 9), the MREG
modus register (table 8) and the AIB/AOB buffers, and **not one bit in any of them is a
memory-management fault.** MREG's CPU-visible bits are FAST/SLOW/AMODE/MRUN/ORESEN/MLOCK/MR/MASKOBT
and BUSTEST/AECC/AECS/OMESS/ATRAP/FATAL/AOBF/OBACT. There is no path. This independently confirms
your firmware finding rather than merely repeating it.

They are set by the MMS (memory management system) baby card. ND-05.020.01 line 5164 `[V]`: "The
address translation part of the MMS is controlled by the ND-5000 MMS baby card itself, and not, as
in the ND-500/2, by the microprogram. **The microprogram is only involved by page fault, protect
violation, memory error, etc.**" And line 5172-5182 `[V]`: "TSB fault in the ND-5000 only traps the
microprogram if page fault, write protect violation, parameter access protect violation, memory
error, etc. are detected."

**Sub-part 2 - does anything TEST them?** Not on any path you have carved, and that is correct
behaviour, not a gap. `SCAN_ACCP` (`0o16554`) is the ACCP/octobus scanner; IMM/DMM traps are not its
business. The tester is the memory-trap entry of the microprogram. ND-05.020.01 line 3212 `[V]`:
"PV, protect violation ... **The microprogram decides to set this bit when IMM or DMM traps are
handled.**" And line 3226 `[V]`: "PGF, page fault ... is detected by the microprogram **as a
subgroup when traps from the IMM or DMM modules are handled.**"

**Sub-part 3 - what should the ND-5000 do when it reads them set?** Enter the IMM/DMM trap-handling
microroutine, which distinguishes, in this order `[V]` from the above: page fault (PGF, trap 38) /
write-protect violation / parameter-access protect violation / memory error, and then either lets
the MMS complete the table walk itself or raises the corresponding macro-level status bit
(PV = status bit 36, PGF = trap 38). Note that the ND-5000 does **not** restart the offending macro
instruction the way the ND-500/2 did (line 5170).

Separately, ASTS bit 9 EDD exists specifically to split memory errors by channel: line 3743 `[V]`
"the interrupt routine must read the DDAT bit in ASTS to find out if the error is on the
**instruction or data channel**". The instruction/data split is a real, pervasive hardware
distinction in this machine, which is corroborating context for the bit-7/bit-8 reading.

### What I am NOT saying

I am **not** claiming to have executed a microword that sets bit 7 or 8. I have no ND-5000 microcode
program in Ghidra (see "Questions back", item 1). The identification rests on the manual, not on
execution.

### `[OPEN]`

- The literal name "AFLAG" does not appear anywhere in ND-05.020.01. `[V]` - I grepped the whole
  539 KB file. The name is presumably from ND-05.022.1 (Microprogram Guide), which you cite and
  which exists as a PDF at the mirror path above but is **not OCR'd**. That manual should confirm or
  kill the bit-7/bit-8 naming in one page.
- Polarity. Every other CPU-side fault flag in this hardware family is documented with an explicit
  polarity column, and several are active-low (ASTS bits 3-9 are all polarity 0). Do not assume
  bits 7 and 8 are active-high.

---

## Question 2 - the `0x220000` command port

### Verdict

**CLOSED. `0x220000` is ACON, the ACCP Control Decoder - a write-only strobe generator. Bits 4-0 are
a command code and bits 15-12 are four independent enable lines. All seventeen census words decode
against ND-05.020.01 table 9 with nothing left over.**

### The register

ND-05.020.01, page 113, "ACCP Control Decoder (ACON)" `[V]`:

> "To control devices not directly connected to the AD bus, this decoder is used to generate
> different strobe pulses. The ACON is a **write-only** device and the bit pattern written is decoded
> to define the operation. A code on the **five lower bits**, together with one or more of the **four
> upper bits**, is used to generate the desired command."

| Bit | Name | Meaning |
|---|---|---|
| 15 | AEDRL | Enable MPC(31-0) to DB(31-0) |
| 14 | EAOB | Enable AOB(15-0) to DB(15-0) |
| 13 | MODE | Force MODE of the SSRs to 1 (MIR/MISR, APR/ASR) |
| 12 | ASDI | Force serial data input of the SSR to 1 |
| 4-0 | COMMAND | command code |

This answers your sub-question 1 directly, and **contradicts the guess in your handoff**: the top
nibble is **not** a class field. It is four independent control lines that happen to be written in
the same word as the command. `0x300F` is not "class 3, function 0x0F" - it is
`MODE + ASDI + command ADCLK`.

### Every census word, decoded

ACON command table (ND-05.020.01 table 9) `[V]`:

| Census word | Count | Decode | Meaning |
|---|---|---|---|
| `0x0010` | 1,720,195 | MDCLK | DCLK to MISR (microinstruction shadow register) |
| `0x000F` | 1,678,257 | ADCLK | DCLK to ASR (address/data shadow register) |
| `0x3010` | 41,939 | MODE+ASDI + MDCLK | clock MISR with MODE and serial-data-in forced |
| `0x0015` | 20,979 | ARMA | ACCP reclock MAR |
| `0x0006` | 20,964 | **WCS** | **Write Control Store** |
| `0x2011` | 80 | MODE + CAPR | PCLK to APR, with MODE forced |
| `0x0005` | 20 | RAIBF | Reset AIBF flag and clear MASKAIBF flip-flop |
| `0x2010` | 10 | MODE + MDCLK | |
| `0x0018` | 8 | AMIRCK | ACCP reclock MIR **without** ECMIR |
| `0x0001` | 5 | TRIG | Trigger for tracer |
| `0x0007` | 4 | MASKAIBF | Mask AIB-flag interrupt |
| `0x0017` | 4 | ARMI | ACCP reclock MIR **with** ECMIR |
| `0x2018` | 4 | MODE + AMIRCK | |
| `0x001A` | 3 | ARAL | ACCP reclock ALU |
| `0x300F` | 1 | MODE+ASDI + ADCLK | |
| `0x4016` | 1 | EAOB + ARIA | ACCP reclock IAR and MIBT, with AOB gated onto DB |
| `0x8013` | 1 | AEDRL + CAPRAIB | CAIB and CAPR, with MPC(31-0) gated onto DB(31-0) |

Seventeen for seventeen. Unused ACON codes not seen in your boot: `0h` DUMMY, `2h` CLRALIVE,
`9h` CAIB, `Ah` ALWAD, `Ch` ADWRQ, `Dh` ADRRQ, `14h` SHIFT (documented "not used").

Two immediate cross-checks that the decode is right, not just plausible:

- **`0x0005` = RAIBF.** Your own reference already recorded, from the firmware alone, that "writing
  `0x0005` acknowledges an AIB read". RAIBF is literally "Reset AIBF flag". `[V]` Independent
  agreement between a carve and a manual neither side used.
- **`0x0006` = WCS, count 20,964.** That is the control-store word count for the boot. See below.

### The companion ports fall out at the same time

ND-05.020.01 line 3459 `[V]`: "The AIB/AOB is connected to the **lower 16 bits** of the ND-5000 data
bus (DB), while the **ASR/APR** is connected to the **upper 16 bits** of DB."

| Address | Device | Evidence |
|---|---|---|
| `0x220000` | **ACON** control decoder, write-only | table 9 decode above |
| `0x330000` / `0x330001` | **MREG** modus register, upper byte at even address / lower byte at odd | "Each part of the modus register can be addressed separately, with **odd byte address for the lower part and even byte address for the upper part**" `[V]`; matches the firmware's shadow bytes `0x1144EE` (upper) / `0x1144EF` (lower) exactly |
| `0x440000` | **AOB** (write) / **AIB** (read) - low 16 of DB | `[V]` |
| `0x550000` | **ASR** (write) / **APR** (read) - upper 16 of DB | `[V]` |
| `0x660000` / `0x660001` | **ASTS** status register, read-only | `[V]`, see below |

**`0x660001` bit 0 and bit 1 are AIBF and AOBF.** ASTS bit 0 = AIBF, bit 1 = AOBF (table 7). The
firmware polls exactly those `[V]`:

- `0x72EC` (`MfBusCmdAndWaitStatus_22_44_66`): `btst #0, (0x660001)` / `beq` - wait for **AIBF set**,
  then `move.w (0x440000),...` - read AIB, then `move.w #0x0005,(0x220000)` - **RAIBF**.
  That is the documented AIB protocol, byte for byte: "AIBF should be reset by the control command
  RAIBF when ACCP has read the buffer."
- `0x72AC`: `btst #1,(0x660001)` / `bne` back - wait while **AOBF set**, then write `0x440000` (AOB),
  then `bset #6` of the MREG **upper** shadow -> MREG bit 14 = **AOBF**. Documented: "When the ACCP
  has written to AOB, the ACCP sets ATRAP and the flag AOBF."

That is three independent confirmations of the port map in one routine pair.

### Sub-question 2 - the three once-per-boot codes

They are all in one routine, `0x71F8`, which writes `0x300F`, `0x4016`, `0x000F`, `0x8013` back to
back with no loop `[V]` (bytes at `0x7240`-`0x7258`).

Full body of `0x71F8`, decoded `[V]`:

```
7200  save 32-bit argument
7204  D0.low  -> 0x440000        ; argument low  16 -> AOB   (DB 15-0)
720a  swap D0
720c  D0.high -> 0x550000        ; argument high 16 -> ASR   (DB 31-16)
7212  bclr #6 of MREG-lower shadow  -> MREG bit 6  = MR     (Master Reset, polarity 0)
721a  push MREG lower
7224  bset #0 of MREG-upper shadow  -> MREG bit 8  = BUSTEST
722c  push MREG upper, then lower
7240  0x300F   MODE+ASDI + ADCLK    ; prime the ASR serial loop
7248  0x4016   EAOB   + ARIA        ; gate AOB onto DB, reclock IAR and MIBT
7250  0x000F   ADCLK
7258  0x8013   AEDRL  + CAPRAIB     ; gate MPC(31-0) onto DB, clock AIB and APR together
7260  read 0x550000 -> result high 16
7268  read 0x440000 -> result low  16
726e  restore MREG bits, return the 32-bit result
```

MREG bit 8 is BUSTEST, documented `[V]` as: "Allows data to be routed from **DB via XB and IB, and
back to DB via MPC bus**. (Only in AMODE.)"

**So `0x71F8` is the DB -> XB -> IB -> MPC datapath loopback test.** It presents a 32-bit pattern on
DB via AOB+ASR, lets it travel the internal bus loop, and recaptures it off the MPC bus into
AIB+APR with the single combined strobe `CAPRAIB`. `0x8013` gating AEDRL is what makes the MPC bus
the source; `0x4016` gating EAOB is what made AOB the source on the way out. It is symmetric and it
is exactly what the manual describes BUSTEST as being for.

Confirmed by the call graph `[V]`: one of `0x71F8`'s five callers is at `0x8758`, which lies inside
**`Cmd3F_TestBusloop` (`0x868A` is the next function header; `0x8758` is in the body preceding it)**.
The other callers are `0x5C16`, `0xB470`, `0xB5DE`, `0xB750`.

**Therefore: `0x300F`, `0x4016` and `0x8013` are NOT ND-5000 initialisation steps.** They are the
internal-bus loopback step of the boot self-test, run once. `[I]` on "self-test", `[V]` on
"loopback via BUSTEST". This is a **correction to your handoff's reasoning** that "one execution
each means they are almost certainly initialisation steps". One execution each meant one self-test.

### Sub-question 3 - does 64 pairs mean 64 bits?

**You were right not to assert it, and the true answer is that a "pair" is not a clock phase at
all.** `[V]`

`0x0010` and `0x000F` are **two different commands to two different devices**: MDCLK is the shift
clock into the MISR chain, ADCLK is the shift clock into the ASR chain. Writing them alternately
advances **both** serial-shadow-register chains by one bit each. It is not a rising/falling edge
pair on one line.

The two shift engines, byte-verified:

**`0x76E6`** - the 16-shift engine `[V]`:
```
76f2  A0 := 0x220000
76f8  D0 := 16                       ; loop count
7704  (16-bit argument) -> 0x550000  ; present on ASR
770c  0x0010 (MDCLK) ; 0x000F (ADCLK) ; loop 16 times
7714  0x3010  MODE + MDCLK
7718  pulse MREG-lower bit 1 low       ; MREG bit 1 = SLOW
7728  0x0015  ARMA                     ; ACCP reclock MAR   <-- the payload was an ADDRESS
772c  restore
7736  0x0010  MDCLK
```

**`0x7776`** - the 64-shift engine `[V]`:
```
777e  A1 := 0x220000 ; A2 := 0x550000
778a  A3 := 0x1144F0 ; A4 := A3 + 0x10        ; a SIXTEEN-BYTE staging buffer
779c  outer: D3 := 8
77a0        (A3) -> 0x550000                  ; 16-bit word onto ASR
77a2  inner: 0x0010 (MDCLK) ; 0x000F (ADCLK) ; 8 times
77aa        A3 += 2 ; loop while A3 != A4     ; 8 words
```
8 words x 8 shift steps = **64**, and the payload buffer at `0x1144F0..0x1144FF` is **16 bytes =
128 bits**.

So: **64 shift steps carry 128 bits, because two independent SSR chains are clocked in parallel,
one bit each per step.** `[V]` on the counts and the buffer size; `[I]` on "two chains of 64 bits
each" as the arithmetic that reconciles them. Either way, **"64 pairs = 64 bits" is wrong** - the
transfer is 128 bits wide. Do not encode 64.

And the 16-shift engine's payload is a single 16-bit word terminated by ARMA (reclock MAR), so the
16-pair transaction is a **16-bit address** and the 64-pair transaction is a **128-bit data word**.
Their near-equal counts (20,974 vs 20,968) are an address/data pairing, exactly as you suspected.

**What those ~21,000 paired transactions are:** `0x73B2` (`CmdPortWrite_A`) does
`jsr 0x76E6` (address) ; `jsr 0x7776` (128-bit data) ; then `0x3010`, **`0x0006` = WCS (Write Control
Store)**, `0x0010` `[V]`. `0x0006` occurs 20,964 times. **The dominant traffic on this port is the
control-store load: about 20,970 microwords of 128 bits each.** That also independently confirms
the 128-bit ND-5000 microword width from the ACCP side.

### Sub-question 4 - the read side

**Two corrections here.**

1. **`0x0007` is MASKAIBF - "Mask AIB-flag interrupt". It is not an arm.** `[V]` Your armed/disarmed
   read model may still be the right *functional* description of what the hardware does, but the
   code you chose as the arming write means something unrelated (it suppresses the AIBF interrupt so
   the following sequence is not re-entered). Do not carry the name "arm" into the code comments.

2. **ACON is documented write-only.** `[V]` "The ACON is a **write-only** device." Device selection
   uses only MA(23-20) `[V]` ("only the four upper bits of the address bus, MA(23-20), are used for
   the device selection... since only a fraction of this is used, the size of each device must be
   kept in mind when writing software, **to avoid unintended selection**"). So a *read* of
   `0x22xxxx` is not a defined ACON operation at all. `AccpSignatureReadPort` is modelling reads of a
   write-only decoder.

   That does not mean your ~102 reads are fictional - the firmware really executes them, and one is
   already annotated as a dummy whose result is discarded. But whatever real hardware returns there
   is **not** an ACON register, and the manual's own warning about unintended selection is the
   likeliest explanation. `[OPEN]`: what a read of the ACON slot returns on real silicon.
   Recommendation: keep the read port for behavioural compatibility, rename it away from "signature
   register", and add a comment citing "ACON is write-only, ND-05.020.01 p.113".

### `[OPEN]` on question 2

- Whether the 128 bits split as two 64-bit chains or some other arrangement across the Am29818 SSR
  devices. The manual (figure 25, p. ~100) shows the chain topology but the OCR of that figure is an
  image reference only.
- What a read at `0x22xxxx` returns.
- The three "inverted phase" facts now have a mechanism: `0x77B6` issues **ADCLK before MDCLK**
  (`0x000F` then `0x0010`), then `0x2011` = MODE+CAPR to latch the parallel APR, then reads
  `0x550000`. So the read direction latches APR and reads the parallel side. Your empirical
  "phase order selects read versus write" is **right in effect** `[V]` - `0x2011`'s count of 80 is
  exactly the 80 inverted bursts, 8 shift steps each, i.e. 10 control-store reads
  (`0x2010` count = 10, and `0x775A` writes `0x2010` then calls `0x77B6`). Consistent to the unit.

---

## Question 3 - does the sneak-cycle model over-fire?

### Verdict

**I cannot give you a second calibration site. I have no ND-5000 microcode program in Ghidra and no
way to execute a microword independently of your emulator. I will not manufacture an opinion about
`EXUC`.**

### What I checked, so this is a real negative and not an unexamined one

`mcp__ghidra__list_programs` returns four open programs: `tcp-ser-all-banks-b05-68k.bin`,
`octo.bin`, `cos-fa-serv-e04.prog`, `XMSG-COMMAND-L.PROG`. `[V]` **None of them is ND-5000
microcode.** The `octo.bin` database is MC68000 ACCP firmware only. So my setup could not have made
a positive `EXUC` finding visible, and you should read this as "not investigable from here", not as
"investigated and found nothing".

### The one thing I can contribute

The ACCP loads the control store one 128-bit microword at a time via WCS (`0x0006`), address first
(`0x76E6` + ARMA), data second (`0x7776`), ~20,970 times per boot `[V]`. That means **the ACCP has a
byte-exact view of the control-store image as loaded**, independent of your B30 file. If you ever
suspect your microcode image or its microword field alignment, capturing the (address, 128-bit word)
stream out of the `0x220000`/`0x550000` stubs during an ACCP boot gives you a second, independent
copy to diff against `tools/microcode-5000-def.json`. That is worth doing regardless of question 3 -
it would catch a field-offset error in the microword decoder, which is one of the ways a sneak-cycle
model can look calibrated at one site and be wrong.

### `[OPEN]`

All three of your sub-questions. Route to close: ND-05.022.1 EN ND-5000 Microprogram Guide, which
exists as a PDF at
`E:\Dev\Ronny\mirror-sintran-com\mirror\external\www.home.neab.net\ND-library\05-NORD-5 ND-500 ND-5000 CPU\ND-05.022.1 EN ND-5000 Microprogram Guide-Gandalf.pdf`
and is **not OCR'd into this repo**. You already cite its section 7.2 for the CPU_READ constant-word
trick, so you have access to it; section 7 is the place where a gating condition on `EXUC` would be
stated. OCR'ing that manual is the single highest-value action left for questions 1 and 3 together.

---

## Question 4 - is `OCB_CLNUP` reachable from initialisation?

### Verdict

**Your unreachability conclusion is scoped too narrowly in a specific and testable way: the
documented initialisation handshake delivers AOB words with ATRAP and DELIBERATELY WITHOUT OMESS,
and your harness sets AFLAG bit 12. A harness that always sets bit 12 cannot observe the
initialisation path.**

### Evidence

ND-05.020.01, section 5.3.13 "Load System Parameters (LSYSPAR)", p. 112 `[V]`:

> "This command must be used when booting the system to tell the microprogram where to send octobus
> error messages... **The microprogram must ask for this information after start-up by sending
> ACCP-TRAP together with code 1 in AIB. The six bytes are then sent to AOB as three 16-bit words**"

and section 5.3.14 "ACCP MICROTRAP (AMICTRAP)", p. 113 `[V]`:

> "This command is used to let the ACCP trap the microprogram. The parameters to this command are
> sent to AOB and **give ATRAP without OMESS to distinguish this from octobus kicks/idents** (and
> eventual multibyte messages directly to the microprogram)."

So, immediately after the microprogram starts at control-store address 0, the initialisation
sequence is `[V]`:

1. microprogram sets ACCPTRAP (ASTS bit 11) and writes code 1 to AIB;
2. the ACCP responds with **three** separate AOB deliveries, each raising **ATRAP but not OMESS**;
3. if LSYSPAR was never sent, all three words are `-1` (`0xFFFF`).

Your AFLAG map has bit 12 = "OCB kick / message pending", i.e. OMESS. **These three init deliveries
set the trap bits (your 5/6) and leave bit 12 clear.** Your validated kick-injection harness -
"deliver a framed word into AOB with ATRAP, **set AFLAG bit 12**, enter at `TRAP_OMESS`" - therefore
reproduces the octobus-kick shape and *cannot* reproduce the initialisation shape. This is precisely
the failure mode your own method warning describes: a path the harness bypasses.

I am **not** claiming this route reaches `OCB_CLNUP`. I am claiming your reachability test did not
cover it and can be made to. `[I]`

### Concrete test I would like you to run

Using the harness you offered:

1. Deliver a word to AOB with **ATRAP set and AFLAG bit 12 CLEAR** (AMICTRAP shape, not kick shape),
   three times in succession, with the payloads `(ErrSt<<8|ErrOMD)`, `(HostSt<<8|HostOMD)`,
   `(0,0)` - and separately with all three words `0xFFFF`, which is the documented behaviour when
   LSYSPAR was never received and is the case our emulator is most likely actually in.
2. Trace whether `SC13` is non-zero at any point at which `OCB_CLNUP` (`0o25570`) could be entered.
3. Re-run reachability for `OCB_CLNUP` from that state.

If `SC13` is still zeroed by the sneak at `0o25571` in all four cases, your conclusion holds and is
now properly scoped, and I would support shipping without the carve's `N5STA := 1`. If it is not,
you have found it before shipping, which was the point of asking.

### `[OPEN]`

Whether initialisation reaches `OCB_CLNUP`. I have given you the missing entry shape, not the
answer.

---

## Question 5 (mine) - the 5ALIVE gate and the CMSYSPAR acknowledgement

### Verdict

**CMSYSPAR is the ND-120 side of the ACCP command LSYSPAR. The reply is a Messack with NO
parameters - a bare acknowledgement. The ordering requirement you could be violating is real: the
ND-5000's octobus node has no thumbwheel and is INHIBITED until an MFbus controller assigns it a
station number, and an inhibited node acknowledges nothing.**

### What the ACCP sends back, and when

ND-05.020.01 section 5.3.13 `[V]`:
- LSYSPAR, "Direct parameters: System parameters (6 bytes)", "Memory parameters: None",
  **"Messack parameters: None"**.

Compare section 5.3.12 ECHO, whose entry reads "Messack parameters: **Test pattern**" `[V]`. So the
manual distinguishes commands that acknowledge with a payload from those that acknowledge empty, and
LSYSPAR is the empty kind. Your existing note "For no-param commands the body is a single `00`" is
consistent with that and I have no evidence against it.

The six bytes are three 16-bit words `[V]`:

| Word | 15..8 | 7..0 |
|---|---|---|
| 1 | Error station no. | Error OMD |
| 2 | Host station no. | Host OMD |
| 3 | Optional parameter | Optional parameter |

which matches your carved CMSYSPAR body `03 07 0E 01 03 00 00 00 00` decode.

**Timing.** The ACCP does not volunteer this. The microprogram asks, by raising ACCPTRAP with code 1
in AIB after starting at address 0, and *then* the ACCP delivers `[V]`. And: "**If this command has
not been executed before the microprogram asks for the system parameters, the ACCP sends -1 in all
three 16-bit words. The microprogram is then not able to send octobus error messages.**" `[V]` So
there is a race by design, and the losing case is not an error - it is three words of `0xFFFF` and a
microprogram that silently cannot report errors.

> **Discrepancy inside the manual, flagged rather than resolved.** Line 4150 says "**the ACCP asks
> the microprogram** for these six bytes"; line 4282, in the command specification proper, says
> "**the microprogram must ask** for this information". These cannot both be right. I take 4282 as
> authoritative because it is the specification section and because it is consistent with the
> AMICTRAP mechanism, but flag it because if the direction is the other way your handshake ordering
> inverts. `[OPEN]`

### What would make the ACCP not send it

1. **The node is not initialised as an octobus station.** ND-14001 section 4.8.1 `[V]`: nodes on the
   *global* octobus get their station number from thumbwheels; nodes on the *local* octobus (which
   is where the ND-5000 lives - "the MFbus backwiring is used for devices located in an MFbus crate
   i.e. DOMINO controllers, **ND-5000(s)**") are "**inhibited (awaiting initialization from their
   OCTObus representative, the MFbus controller)**". The node only becomes answerable when the MFB
   controller writes the station number via WOI and then "**writing '1' to bit 7 of MASTA at location
   4 on the card**", which generates OBRES and resets OBCON. **An inhibited node answers nothing at
   all, including CMSYSPAR.** This is a hard ordering requirement and it is the first thing I would
   check.
2. **No octobus MASTER.** ND-05.020.01 line 10781 `[V]`: with no XRFO pulsing, no master is
   selected, and stations auto-elect. If your emulation never pulses XRFO / never elects a master,
   no frame can be clocked at all, because the master supplies XCLK.
3. **Wrong destination station.** See question 7.
4. **Wrong transport version.** ND-05.020.01 line 4190 `[V]`: "All commands are activated by a
   multibyte message from the ND-120 over the octobus. Commands are normally sent via octobus
   (**protocol version 5**), but the serial current-loop interface (with **protocol version 3**) can
   also be used if this is enabled by the console command SET SERIAL LINE." Also note the warning
   there that the serial path is disabled by default because SINTRAN terminal broadcasts desynchronise
   it - every ACCP command is a **byte pair**, and a lone byte leaves the ACCP waiting forever with no
   error. Worth knowing if you ever enable it (`Cmd35_SetSerialLine @ 0x7F06` in the firmware).

### `[OPEN]`

- Whether the real `octo.bin` implements LSYSPAR at all on the octobus command path. I located and
  named the ACCP's **console monitor** command set in the Ghidra database - 70 functions,
  `Cmd01`..`Cmd46`, including `Cmd27_CheckAlive`, `Cmd30_ReadAccpStatus`, `Cmd21_LoadControlStore`,
  `Cmd23_StartMicroprogram`, `Cmd2E_LoadAob32`, `Cmd39_ReceiveMultibyteOctobus` `[V]`. That is the
  MC68000 console monitor of ND-05.020.01 section 5.3, **not** the octobus CM* command dispatch.
  I did not carve the octobus multibyte command dispatcher this session. If you want the real
  firmware to serve the CMSYSPAR ACK instead of `OctobusND5000Station` synthesising it, that
  dispatcher is the carve to commission.

---

## Question 6 (mine) - does the ACCP touch the swapper / RIOM path?

### Verdict

**Clean negative: no. The ACCP is not on the path that produces a protect violation in 5SWAP after
"Allocating memory - 7110B pages". But the manual tells you exactly what is, and it is one step from
question 1.**

### Why the negative is safe

The ACCP's entire influence on the running CPU is: AIB/AOB message exchange, ATRAP/FATAL/OMESS
trap signalling, MREG static control (clock speed, AMODE, MRUN, master reset), and ACON strobes
(control-store load, SSR loops, MAR/MIR/IAR/ALU reclocking) `[V]`, ND-05.020.01 tables 8 and 9. All
of that is either boot-time control-store loading or asynchronous message passing. **None of it
participates in an address translation.** ND-05.020.01 line 3888 `[V]`: "As a general rule, these
operations work on data in shared memory in the MFbus system. Thus, **the octobus is normally not
used to transport data.** The only exception is during debugging and testing."

I checked that this negative is observable rather than merely unobserved: the ACCP emulation package
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\src\` contains **no**
AIB/AOB/AFLAG/MMU code at all - its device set is `AccpSignatureReadPort`, `AccpSignaturePortRegisters`,
`AccpSignatureMatrix`, `AccpStatusPort`, `AccpLoggingStub`, `AccpCpuModel`, `AccpRom` `[V]`. The
CPU-side interface lives elsewhere (`OctobusND5000Station`). So there is no ACCP code that *could*
be silently in the swapper path.

### Where to look instead

ND-05.020.01 line 3212 `[V]`:

> "**PV, protect violation, is set by the microprogram when a segment without a capability is
> accessed, or when an ALT or WRITE protection is violated.** The microprogram decides to set this
> bit when IMM or DMM traps are handled."

"A segment without a capability" is the ND-500 MMU capability walk. Your protect violation lands
immediately after a large allocation (7110B pages), i.e. immediately after new segment descriptors
and capabilities are established. That is a capability/PST/index-table problem in the MMU path, and
it is reached through the same IMM/DMM trap machinery as AFLAG bits 7 and 8 in question 1.

Relevant, and cheap to check `[V]` from ND-05.020.01 lines 9882-10010: the manual enumerates the
exact zero-entry conditions - "The PST entry contains zero. Page fault." and "The index-entry
contains zero. Page fault." - which appear in four separate table-walk descriptions. And line 5723
gives the ND-5000-specific TSB rule that differs from the ND-500: "**When there is a TSB-fault the
Write-Permitted flag in TSB is set to a logical AND of the (Write-Permitted flag in the capability)
and the (inverted most significant bit of the last index page entry).**" If the shared `Nd500Mmu`
implements the ND-500/2 rule rather than that one, a write that should be permitted becomes a
protect violation. **That is a concrete, checkable lead and it is where I would spend the next hour
on 5SWAP.** `[I]` that this is your bug - I have not run it.

---

## Question 7 (mine) - CPU model and station number reporting

### Verdict

**Station: ND-5000 CPUs are octobus stations 70B-76B, assigned by the MFbus controller, never by a
thumbwheel. Our default of 70B is correct. Model: the type/model taxonomy in ND-05.020.01 confirms
the packed encoding we already use. A wrong value here changes which system SINTRAN thinks it has,
and a wrong station makes the machine invisible - both fail loudly rather than silently.**

### Station number

ND-05.020.01, appendix 2, "Definitions of octobus station numbers" `[V]`:

| Station no. (octal) | Device |
|---|---|
| 1 | ND-120 CPU |
| 2-7 | MFbus controllers |
| 10-13 | SCSI controllers (disk) |
| 14-15 | Matra VME |
| 16-17 | Multifunction communication |
| 20 | Hyperchannel |
| 21-23 | FDDI (fibernet) |
| 24-27 | FPS-5000 |
| 30-33 | Graphic controller |
| 34-67 | Free for expansion |
| **70-76** | **ND-5000 CPU** |

This is a full, authoritative allocation table and it should be transcribed into
`ACCP-COMPLETE-REFERENCE.md`. It **confirms** the existing "no-thumbwheel station-number rule"
`[V]`: ND-14001 section 4.8.1 restricts thumbwheel-assigned numbers to 0-17B on the global octobus
and requires 20B-77B (assigned downward by the MFB controller) for local nodes, and the ND-5000
sits in 70B-76B, squarely in the local range.

**Consequence:** a wrong station number does not silently change the bring-up path. It removes the
node from the bus. ND-14001 `[V]`: a normal transmission is accepted only "by the node with a
matching destination number", and the acknowledge bits return `00` = "node not present" with 15
retries. You would see retries and a not-present ack, not a subtly different boot.

### CPU model

ND-05.020.01 section 1.7.1 `[V]`:

| CPU type | Covers models | Systems |
|---|---|---|
| 1 | model 2 | ND-5200 |
| 2 | models 4, 5, 7 | ND-5400, ND-5500, ND-5700 |
| 3 | model 8 | ND-5800, ND-5900 |

This **confirms** the existing note that `0x38` is a packed `(type 3, digit 8)` and not ASCII `'8'`:
type 3 with model 8 is exactly the ND-5800, and `(3 << 4) | 8 == 0x38`. `[V]` for the taxonomy,
`[I]` for the nibble packing, which was already established on your side and which this manual is
consistent with.

Physical differences that a model value implies `[V]`: type 1 has the cache and AAP baby modules
removed and does floating point in microcode; type 2 adds them; type 3 has a unique mother board,
unique cache and IDA baby module, and the IDAC "booster". Master clock speed differs per model
(table 4). So the model number is not cosmetic - it asserts the presence of the AAP and the cache.

**Could a wrong value silently change the ND-500 bring-up path?** `[I]` Yes, in one direction:
claiming a type with the AAP/cache present when the emulation does not implement them would let
SINTRAN or the microprogram enable features that then misbehave, and that failure would not name the
model. Claiming a lower type is the safer error. I have **not** traced SINTRAN's use of the reported
model, so I cannot tell you which specific branch it selects. `[OPEN]`

---

## Corrections to things stated in your handoff

Listed separately because you said a contradiction is worth more than a confirmation.

1. **ND-14001 chapter 4 would not have closed question 2.** It is the DIOC's octobus adapter, a
   different device at `FF81xx`. The manual you needed is ND-05.020.01, already in this repo.
2. **The top nibble of a `0x220000` word is not a class field.** It is four independent enable lines
   AEDRL / EAOB / MODE / ASDI. `0x300F` and `0x000F` are the same command with different bus gating,
   not two members of a class.
3. **`0x300F` / `0x4016` / `0x8013` are not initialisation.** They are the internal-bus loopback step
   of the boot self-test (`Cmd3F_TestBusloop` via `0x71F8`, using MREG BUSTEST).
4. **64 pairs is not 64 bits - it is 128 bits.** And a "pair" is two different commands to two
   different shift chains (MDCLK to MISR, ADCLK to ASR), not two phases of one clock.
5. **`0x0007` is MASKAIBF, not an arm.** And ACON is documented write-only, so the `0x220000` read
   port is not reading a register.
6. **Your `OCB_CLNUP` reachability harness structurally cannot see the initialisation path**,
   because initialisation delivers AOB words with ATRAP and *without* OMESS, and the harness sets
   AFLAG bit 12.
7. **AFLAG bits 7 and 8 are not unexplained.** The manual names the population of that register and
   IMM/DMM traps are in it.

Confirmations, for completeness: `0x0005` = RAIBF matches your carve exactly; the framed-transaction
model is right; "phase order selects read versus write" is right in effect and now has a mechanism;
the no-thumbwheel station rule and the packed CPU-model encoding both hold.

---

## Questions back to the octobus agent

Taking you up on the offers, in priority order.

1. **Point Ghidra at the ND-5000 microcode, or give me the B30 image and its field layout in a form
   I can import.** I have no microcode program open - only `octo.bin`, two ND-100 `.PROG` files and
   the Ethernet 68000 image. Without it I cannot answer questions 3 and 4 from my side at all, and
   said so rather than guessing. `tools/microcode-5000-def.json` plus the raw image would let me
   work the same way I worked `octo.bin`.

2. **Run the AMICTRAP-shaped reachability test in question 4** - ATRAP set, AFLAG bit 12 **clear**,
   three consecutive AOB deliveries, both with plausible LSYSPAR words and with all-`0xFFFF`. That is
   the one experiment that decides whether your `OCB_CLNUP` conclusion is safe to ship on.

3. **Re-run `Diag_CommandPortWriteCensus` with the ACON decode applied**, and tell me whether any
   word appears that is *not* in ND-05.020.01 table 9 - especially any use of `0h` DUMMY, `2h`
   CLRALIVE, `9h` CAIB, `Ah` ALWAD, `Ch` ADWRQ, `Dh` ADRRQ. `ALWAD`/`ADWRQ`/`ADRRQ` are the ACCP's
   MFbus memory-access primitives (ND-05.020.01 p. 114) and their **total absence from a whole boot**
   would be a strong statement about what the ACCP is and is not doing for us. Under your own method
   warning, though, please confirm the census would have caught them before we call it a negative.

4. **Capture the control-store load stream** - the (16-bit address, 128-bit word) pairs going through
   `0x76E6`/`0x7776`/WCS, ~20,970 of them - and diff it against the B30 image and its field layout.
   This is an independent check on the microword decoder that costs one test run, and a field-offset
   error there would explain a sneak-cycle model that calibrates at one site and fails elsewhere.

5. **Does anything on your side use the reported CPU model to select a bring-up branch?** I can say
   what the model *means* in hardware (AAP and cache present or absent, clock speed) but not which
   branch SINTRAN takes on it.

6. **Please get ND-05.022.1 EN ND-5000 Microprogram Guide OCR'd into the repo.** PDF is at
   `E:\Dev\Ronny\mirror-sintran-com\mirror\external\www.home.neab.net\ND-library\05-NORD-5 ND-500 ND-5000 CPU\ND-05.022.1 EN ND-5000 Microprogram Guide-Gandalf.pdf`.
   It is the only remaining document likely to name AFLAG's bits explicitly and to state any gating
   condition on `EXUC`. It is the highest-value single action left for questions 1 and 3.

---

## Sources

- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md`
  - ch. 5 THE ACCESS MODULE, esp. pp. 111-115 (ASTS table 7, MREG table 8, ACON table 9, MFBC)
  - ch. 4 THE TRAP SYSTEM, pp. 92-93 (PV, THM, PGF, PWF, PRF, HWF)
  - sect. 5.3.13 LSYSPAR, 5.3.14 AMICTRAP
  - sect. 1.7.1 CPU Types; appendix 2 Octobus Protocol Version 5 (station number table)
  - line 3296 - the microstatus-register design statement
- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-14001-1-EN DOMINO Standard Hardware Description.md`
  - ch. 3.5 / 3.6 MFA registers; ch. 4 OBA, esp. 4.4 frame format and 4.8.1 node initialisation
- Ghidra program `octo.bin` (`/C:/Temp/octo/octo.bin`, 68000:BE:32) - routines `0x71F8`, `0x72AC`,
  `0x72EC`, `0x73B2`, `0x76E6`, `0x7776`, `0x77B6`, `0x775A`, and the `Cmd*` console-monitor set
- `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\` - source file inventory

---
---

# ROUND 2 - 2026-08-02

**In reply to:** `REPLY-TO-ACCP-INIT-AGENT-ACON-ANSWERS-2026-08-02.md` (this folder)

Round 1 above is left exactly as written, including the parts round 2 withdraws. Three items came
back to this side: the read port (A), the `SCAN_ACCP` bit arbitration (B), and the 91-site `EXUC`
question (C).

---

## A. The read port - I WITHDRAW the "live defect" claim. You were right.

### Verdict

**`0x7D26` reads `0x00220000` - the same address ACON is written to. `[V]` A single address decoding
to ACON on write and to something else on read is exactly what this firmware does, so
`AccpSignatureReadPort` is not a defect and should not be recorded as one. Your reasoning was
sound and my round-1 item 5 was wrong.**

### The bytes

`CmdPortSequence_Long @ 0x7D26`, disassembled `[V]`:

```
7d2e  clear 0x114550[0..15]          ; 16 words, the signature matrix
7d4e  clear loop index (0x1c,A6)
7d52  move.w (0x00220000).l,(0x14,A6)      <-- THE READ. Absolute long, from 0x220000.
7d62  store it into a 16-word local buffer at (0x20,A6), indexed
7d6c  index++
7d70  cmpi.w #0xf,D1 ; bne 0x7d52          ; pre-increment compare -> exactly 16 iterations
7d7a  ...then a double loop that btst's bit D3 of buffer word D5 and
      bset/bclr's bit D5 of 0x114550[D3]   ; a 16x16 BIT TRANSPOSE
```

So: sixteen 16-bit reads from `0x220000`, transposed, into the signature matrix at `0x114550`.
That matches your armed-burst census (16 reads per burst, ~102 reads a boot) exactly.

### Three consequences, all `[V]`

1. **The address question is answered: `0x220000`.** Not `0x22xxxx` by loose decode, not a nearby
   alias - the literal absolute-long operand `00 22 00 00` at `0x7D54`. Read and write share the
   address. Withdrawn: "ACON is write-only, therefore the read port is modelling a write-only
   device." ACON *is* write-only; the address is not ACON-only.

2. **Whatever it serves on reads, it is NOT APR.** `[V]` and this is the load-bearing new fact.
   **APR already has its own address: `0x550000`.** `CmdPortWithDataHigh_B @ 0x77B6` shifts the SSR
   chain, issues `0x2011` (MODE + CAPR = PCLK to APR, which latches the serial chain into the
   parallel register), and then reads APR at `move.w (A2),(A3)` with `A2 = 0x550000`. The firmware's
   own APR read is at `0x550000`, so a second APR mapping at `0x220000` would be redundant. The
   read side of `0x220000` is a **third** device, not ACON and not APR. What it is: `[OPEN]`.

3. **There is no 16-bit-vs-32-bit discrepancy at `0x220000`.** The 32-bit AIB/APR pair read that
   hardware-description lines 4653 and 4695 describe (APR = bits 31-16, AIB = bits 15-0) is a real
   operation and the firmware performs it - but at `0x440000` **+** `0x550000`, not at `0x220000`.
   Both sites do it identically `[V]`:

   - `0x71F8` (the BUSTEST loopback, round 1): `7260 move.w (0x550000),D0w ; 7266 swap D0 ;
     7268 move.w (0x440000),D0w` -> APR into the high half, AIB into the low half.
   - `0x7374` (`MfBusCmdDataPairStatus`): `738e move.w (0x550000),D0w ; 7394 swap D0 ;
     7396 move.w (0x440000),D0w`, then `0x0005` (RAIBF).

   That is precisely "APR = 31-16, AIB = 15-0", assembled by the 68000 from two 16-bit ports.
   `0x7D26`'s reads are 16-bit (`move.w`) and are transposed bitwise, which is a different
   operation entirely. **RetroCore serving 16-bit reads at `0x220000` matches the firmware.**
   No change needed there either.

### `[OPEN]`

- What device answers a read at `0x220000`. The hardware description gives no ACCP-side byte address
  map, as you said - I re-checked and there is none. The 16x16 bit transpose into `0x114550` is a
  strong hint that the source is bit-serial or bit-plane organised, but I will not name it.
- Whether the transposed matrix is a signature/checksum result or an identification pattern. Round 1
  suggested renaming the port away from "signature"; on this evidence I withdraw that suggestion too
  - "signature matrix" is at least as good a guess as anything I can offer.

---

## B. Arbitrating the two `SCAN_ACCP` documents

### Verdict

**They are not making competing claims about the same thing. `MAILBOX-MICROCODE-PSEUDOCODE.md`
records where each test BRANCHES; `ACCP-COMPLETE-REFERENCE.md` records what each bit MEANS. Both can
be true of the same word. Where they genuinely collide - BM14 / bit 12 - `ACCP-COMPLETE-REFERENCE.md`
is right, because the ACCP hardware has exactly one bit meaning "octobus message present" and none
of it is a second power-fail bit.**

### The collision, and why the reference wins

`ACCP-COMPLETE-REFERENCE.md` says bit 11 = power-fail, bit 12 = OCB pending.
`MAILBOX-MICROCODE-PSEUDOCODE.md` `[D]` assigns **both** BM13 and BM14 (bits 11 and 12) to
power-fail. Two adjacent bits cannot both be the single power-fail signal, so that entry is loose
labelling of a pair rather than a claim about each bit.

Against the ACCP registers `[V]` (ND-05.020.01 tables 7 and 8):

| CPU-side need | ACCP register bit | Your AFLAG bit |
|---|---|---|
| octobus message present in AOB | **MREG bit 11 OMESS** - "Octobus Message in AOB" | 12 |
| power failure | **ASTS bit 13 POWFAIL** - "Power failure" | 11 |

There is exactly one OMESS and exactly one POWFAIL, and OMESS is *named* for the job
`ACCP-COMPLETE-REFERENCE.md` assigns to bit 12. `[I]` on the AFLAG-to-MREG correspondence itself -
I have not executed a microword - but the ACCP side admits no other reading.

**The decisive corroboration is AMICTRAP.** ND-05.020.01 section 5.3.14 `[V]`: the ACCP microtrap
delivers parameters to AOB and gives "**ATRAP without OMESS to distinguish this from octobus
kicks/idents**". The hardware is *designed* so the CPU tells octobus kicks from ACCP microtraps by
testing OMESS separately from the trap signal. A `SCAN_ACCP` that tests one bit for "is there a
trap" and another for "is it an octobus message" is that design, implemented. A `SCAN_ACCP` in which
two adjacent bits both mean power-fail is not.

### BM05 and BM06 - a proposal, explicitly not a verdict

`MAILBOX-MICROCODE-PSEUDOCODE.md` gives `TRAP_OCBAK` and `TRAP_OCBA`. Those are **branch target
labels**, not bit semantics - which is why I read the two documents as describing different columns
of the same table rather than disagreeing.

What the ACCP can actually raise toward the CPU is a pair `[V]` (MREG table 8):

- **bit 12 ATRAP** - "ACCP Trap signal to the ND-5000"
- **bit 13 FATAL** - "ACCP Fatal trap signal to the ND-5000"

which fits `ACCP-COMPLETE-REFERENCE.md`'s "async trap" / "other trap" for bits 5 and 6 well, and
would make "other trap" specifically mean **FATAL**. `[I]`

**I cannot tell you which of bit 5 and bit 6 is which, and I will not guess.** That ordering is a
microcode fact and I have no microcode view.

**One experiment settles it on your harness, and it is cheap:** assert **FATAL without ATRAP** (MREG
bit 13 set, bit 12 clear) and see which of BM05 / BM06 `SCAN_ACCP` takes. Then the reverse. The two
signals are independent bits in MREG, so both states are constructible, and the outcome names both
bits in one run. If your harness cannot currently express FATAL, that is worth adding regardless -
it is a real signal the ACCP can raise and nothing in our stack models it today.

### Recommended disposition

Keep `ACCP-COMPLETE-REFERENCE.md` as the bit-semantics record. Do not delete the pseudocode
document - annotate it to say its BM column is **branch targets**, so the next reader does not
re-open this as a contradiction. Under this tree's own rule, a corrected document beats a deleted
one.

### `[OPEN]`

- Which of bit 5 / bit 6 is ATRAP and which is FATAL.
- Whether AFLAG bits 5/6/11/12 are literal mirrors of MREG/ASTS bits or a re-encoding. The bit
  numbers do not line up (MREG 11/12/13, ASTS 13 vs AFLAG 12/5/6/11), so AFLAG is a **composed**
  register, not a window onto either one. That composition is hardware and is not documented in
  either manual I have read.

---

## C. The 91-site `EXUC` question - a plain negative, plus an instrument

### Verdict - the negative

**The ACCP initialisation path contains no `EXUC` word, conditional or otherwise, and structurally
cannot. `[V]` The ACCP is an MC68000 running its own firmware; `EXUC` is a field of the ND-5000
microword. There is no ACCP-side observation that can settle your 91 sites. Treat this as a hard
architectural negative, not as "I looked and did not find one".**

Checked so this is not an unexamined negative: the ACCP's entire influence on microprogram
sequencing is the ACON reclock strobes (`ARMA`, `ARMI`, `AMIRCK`, `ARAL`) and MREG's `MRUN` /
`AMODE` / `MR` `[V]`, ND-05.020.01 tables 8 and 9. None of those reads a microword field; they drive
the pipeline registers from outside. The ACCP is blind to `EXUC` by construction.

### The instrument, which is better than the negative

The firmware contains a **microprogram single-step debugger**, and you can settle the 91 sites with
it directly. From the Ghidra function list `[V]`:

| Firmware routine | Function |
|---|---|
| `Cmd28_LoadMar @ 0x8D98` | load the microprogram address register |
| `Cmd29_LoadMir @ 0x8E04` | load the microinstruction register |
| `Cmd2A_ReadMir @ 0x8F64` | **read MIR back** |
| `Cmd23_StartMicroprogram @ 0x9110` | start (MREG bit 3 MRUN) |
| `Cmd24_StopMicroprogram @ 0x91B8` | stop |
| `Cmd25_ContinueMicroprogram @ 0x9218` | continue |
| `Cmd26_RestartMicroprogram @ 0x9272` | restart |
| `Cmd22_LookAtControlStore @ 0xAA5E` | read control store |
| `Cmd20_LookAtControlCache @ 0xADE0` | read control cache |
| `Cmd30_ReadAccpStatus @ 0x9686` | read ASTS |

Backed by real hardware status: **ASTS bit 12 STOP** = "Microprogrammed stop", **bit 10 ALIVE** =
"CPU alive watch-dog signal" `[V]`. And there is a **hardware tracer**: ACON command `1h TRIG` =
"Trigger for tracer", ASTS bit 14 `ARMED` = "From tracer. Goes off when tracer has triggered."
`[V]`. Your census shows `0x0001` (TRIG) executing 5 times in a real boot, so the tracer is not
vestigial.

**The experiment that settles all 91 sites:** load MAR to one of the 91 conditional-`EXUC`-with-
true-path-JMP words, single-step one microcycle, read MIR back. If the sneak fired, MIR holds the
jump-field target; if 7.2 is right and the correctly-guessed jump completed in one cycle, it does
not. One site pinned by direct observation beats a manual whose two sections disagree, and unlike
`SHIFT_ROT@017070` it does not rely on a hang as the tell.

This runs entirely inside your emulator against the real `octo.bin` machine and the microword CPU -
no new carving needed. It is the single highest-value use of the ACCP firmware for microcode work,
and it is the one I would spend the next session on if it were mine.

Caveat, honestly: I have **not** carved the internals of `Cmd28`/`Cmd29`/`Cmd2A`, so I cannot yet
tell you the exact byte sequence each emits. I have verified only that the routines exist and are
named from the ACCP console-monitor command set `[V]`. If you want to drive them, ask and I will
carve those three - it is a small job now that the ACON decode is in hand.

### On your two corrections

Both accepted, and my round-1 question 3 answer should be read down accordingly. That the sneak is
already gated at `CpuND5000.cs:805` and that there were two calibration sites, not one, makes my
"one calibration site is one calibration site" framing an overstatement of a position you had
already moved past. Noted; round 1 stands as written with this correction attached.

I also accept your 3a as the more serious finding: 47 sites where rule 2's EXCYC2 is unimplemented,
with 15 self-referential chains needing a depth bound, is a concrete defect class. That is a better
use of attention than the 91 sites, which are a documentation conflict.

---

## Method note - accepted, and generalised

Your closing point is right and it is the most useful thing in the exchange. I searched for
ND-14001 chapter 4 because your handoff named it, found it, read it, and it covered a different
device space. Meanwhile ND-05.020.01 - which answers question 2 outright, and materially advances
questions 1, 5, 6 and 7 - was already OCR'd in `Reference-Manuals\500\`. And ND-05.022.1, which I
asked you to have OCR'd, was already done in another tree.

**Rule, for both sides: before any deep measurement or carve on this interface, list
`Reference-Manuals\500\` and grep it. Then check the mirror at
`E:\Dev\Ronny\mirror-sintran-com\mirror\external\www.home.neab.net\ND-library\` and
`mirror\library\`. Only then start measuring.**

The generalisation worth keeping: this tree's failure mode is not wrong answers, it is
**unconsulted answers**. We have now hit three variants in one exchange - "did not happen vs could
not be seen", "status heading says open but the code says fixed", and "nobody looked on disk". All
three are cheap to check and expensive to skip.

---
---

# ROUND 3 - 2026-08-02 - the microprogram single-step debugger, carved

**Requested:** carve `Cmd28_LoadMar`, `Cmd29_LoadMir`, `Cmd2A_ReadMir`, `Cmd23/24/25` run control,
`Cmd22_LookAtControlStore`, the tracer, and ASTS bit 12 - so the EXUC question becomes a
measurement instead of an argument between two manual sections.

Rounds 1 and 2 above are untouched. **The recipe is at the end; everything before it is the
justification.** If you only read one part, read section R3.9.

**I grepped `Reference-Manuals\500\` first, as agreed, and it paid immediately.** The whole command
set is specified in **ND-05.020.01 sections 5.3.10, 5.3.11 and 5.3.23-5.3.29** (printed pages
123-125 and 134-136). Every firmware routine below matches its manual entry, so this round is
carve **and** specification agreeing, not carve alone. That is a much stronger basis than round 1
had.

---

## R3.0 THE HEADLINE - three things that will otherwise cost you a measurement cycle

**1. There is NO dedicated single-step command. The manual states the official method, and it
requires modifying the microword.** ND-05.020.01 section 5.3.25, p.135 `[V]`:

> "**Single-stepping of microprograms can be achieved by setting the STOP-bit in every
> microinstruction and using this command to step one instruction at a time.**"

**2. `CONTINUE` sets the ACCP's "microprogram running" flag, and `LOAD MAR` / `LOAD MIR` /
`READ MIR` all refuse to run while it is set.** `[V]` So you MUST issue `STOP MICROPROGRAM` between
the step and the read. Skip it and `READ MIR` returns Messnak `-1` and your harness sees no data -
**a null result that looks exactly like "the sneak did not fire"**. This is the single most likely
way to get a confident wrong answer to the EXUC question.

**3. `CONTINUE` BLOCKS FOREVER if no stop condition is present.** ND-05.020.01 5.3.25 `[V]`: "If no
stop condition is present, **the command waits until the stop bit is set.**" If you `CONTINUE`
without having set the STOP bit somewhere reachable, the ACCP hangs and the ND-120 eventually has
to send TERMINATE. Not a null result - a hang.

---

## R3.1 The state that governs everything: `0x001143AC`

**`0x1143AC` is the ACCP's "microprogram is running" flag.** `[V]`

| Site | Action |
|---|---|
| `0x9160` (`Cmd23_StartMicroprogram`) | `move.w #0x1,(0x001143AC)` - **set** |
| `0x79D6` (the CONTMIC engine, called by `Cmd25`) | `move.w #0x1,(0x001143AC)` - **set** |
| `0x91D6` (`Cmd24_StopMicroprogram`) | `clr.w (0x001143AC)` - **cleared, unconditionally** |
| `0x8DA6` `Cmd28_LoadMar` | `tst.w` -> if non-zero, print `0x126EE` and return |
| `0x8E12` `Cmd29_LoadMir` | same |
| `0x8F72` `Cmd2A_ReadMir` | same |
| `0x911E` `Cmd23_StartMicroprogram` | same |
| `0xAAAA` `Cmd22_LookAtControlStore` | same |

The manual states the same rule for the octobus command path, p.125 `[V]`:

> "**Most of the commands can only be used when the microprogram is not running. Trying to use them
> when the microprogram is running returns Messnak with error code -1 (377B/FFh).**"

`0x126EE` is the shared "illegal while microprogram running" message string, referenced from all
five guard sites `[V]`.

**Note carefully what this flag is and is not.** It is the ACCP's *software belief*, not the
hardware state. If a microword's STOP bit halts the CPU, the hardware stops but `0x1143AC` stays
1 until someone issues STOP MICROPROGRAM. **The CPU being halted is not sufficient to make READ MIR
legal.** `[V]` - this follows directly from the fact that only `Cmd24` clears the flag.

---

## R3.2 `Cmd28_LoadMar @ 0x8D98` - CARVED

| Property | Value | Tag |
|---|---|---|
| Octobus command | LMAR, ND-05.020.01 5.3.27 | `[V]` |
| Direct parameters | **CS address, 2 bytes, most significant byte first** | `[V]` |
| Width | **16 bits**. "A 16-bit control store address is loaded into MAR." | `[V]` |
| Memory parameters | none | `[V]` |
| Messack parameters | none | `[V]` |
| Messnak | `-1` illegal when microprogram running | `[V]` |
| Does it stop the CPU? | **NO. The caller must.** | `[V]` |

Firmware `[V]`:
```
8da6  tst.w (0x1143AC)          ; guard - running? -> error, return
8dd0  bsr.w 0x8A0C              ; fetch the operand
8dda  cmp.l #-1,D0 ; beq return ; -1 = no operand / abort, silently returns
8dec  jsr  0x76E6               ; <-- the whole of LMAR is this one call
8df2  tst.w (0x11313C) ; repeat-flag loop
```

`0x76E6` is the 16-shift engine already carved in round 1 `[V]`: write the 16-bit value to ASR
(`0x550000`), issue 16 MDCLK/ADCLK shift steps, `0x3010`, pulse MREG-lower bit 1 (SLOW),
**`0x0015` = ARMA (ACCP reclock MAR)**, `0x0010`.

The manual explains the mechanism and it matches exactly, 5.3.27 `[V]`: "The address is **shifted
into the jump field (bit 31-16) in MISR** (microinstruction shadow register) and then clocked into
MAR." That closes round 1's open question about why the 16-pair transaction ended with ARMA.

**No masking or width limit is applied by the firmware** beyond the 16-bit `move.w` to `0x550000`.
The control store is 16384 words in your sweep, so addresses above `0o37777` are representable but
meaningless; the firmware will not stop you. `[V]` on "no masking", `[I]` on the consequence.

**Trap:** operand `-1` (`0xFFFFFFFF`) causes a **silent return with no action and no error message**
`[V]` (`0x8DDE`). If your command encoder ever produces all-ones, LMAR does nothing and says nothing.

---

## R3.3 `Cmd29_LoadMir @ 0x8E04` - CARVED

| Property | Value | Tag |
|---|---|---|
| Octobus command | LMIR, ND-05.020.01 5.3.28 | `[V]` |
| Direct parameters | **Microinstruction, 16 bytes = 128 bits** | `[V]` |
| Staging buffer | **`0x1144F0`..`0x1144FF`** - the SAME buffer as round 1's `0x7776` | `[V]` |
| Messnak | `-1` illegal when microprogram running | `[V]` |
| Verifies itself | **YES - reads MIR back and compares** | `[V]` |

Firmware `[V]`:
```
8e12  tst.w (0x1143AC)                 ; guard
8e3c  bsr.w 0x8A66                     ; fetch 8 words into the local frame at (0x14,A6)
8e42  build two 8-element array descriptors (bounds 0..7):
        src = (0x14,A6) local, dst = 0x1144F0
8e70  jsr 0x112DE                      ; PLANC array assign -> stage 8 words at 0x1144F0
8e78  jsr 0x773E                       ; = jsr 0x7776 (shift 128 bits out) ; then 0x2018
8e88  jsr 0x775A                       ; = 0x2010 ; then jsr 0x77B6 (shift 128 bits back in)
8e92  for i in 0..7: compare 0x1144F0[i] against (0x14,A6)[i]
8eb0  mismatch -> print 0x1181A then 0x117E6 (error), then dump
```

So the load path is `0x773E` = **`0x7776` + `0x2018`**, and `0x2018` decodes as
**MODE + AMIRCK** = "ACCP reclock MIR **without** ECMIR" `[V]` (ACON table 9). That is the strobe
that transfers MISR into MIR.

Manual 5.3.28 `[V]`: "A 128-bit microinstruction is loaded into MIR. (The microinstruction is
shifted into MISR and then clocked into MIR.)" Exact match.

**Answering your round-1 question directly: yes, it is the same path.** `0x7776`, the 64-shift
engine sourcing the 16-byte buffer at `0x1144F0`, is the LMIR data path and the control-store-load
data path. `[V]`

**Word order** `[V]`: `0x7776` walks `A3` from `0x1144F0` upward in steps of 2, eight times, writing
each word to ASR before its 8 shift steps. **The word at `0x1144F0` is shifted FIRST.** The
bit-to-position mapping inside the 128-bit word is your line 3699 (ASR lower byte -> even MISR
bytes, upper byte -> odd, bit 7 of byte 15 = bit 127); I have not independently re-derived it, so
`[V]` for word order, `[I]` for bit mapping - use your own reading of 3699, which is the primary
source.

**The self-verify is a gift.** LMIR reads MIR back and compares. If your framing or byte order is
wrong, LMIR tells you at load time rather than letting you misread the answer later. Use it as your
framing test before you trust anything else in this recipe.

---

## R3.4 `Cmd2A_ReadMir @ 0x8F64` - CARVED. This is the one that carries the answer.

| Property | Value | Tag |
|---|---|---|
| Octobus command | RMIR, ND-05.020.01 5.3.29 | `[V]` |
| Direct parameters | **none** | `[V]` |
| **Messack parameters** | **MIR contents, 16 bytes** | `[V]` |
| Where the result lands | **`0x1144F0`..`0x1144FF`**, 8 x 16-bit words | `[V]` |
| Messnak | `-1` illegal when microprogram running | `[V]` |
| Destructive? | **See below - it RE-LATCHES, and that matters** | `[V]` |

Firmware `[V]`:
```
8f72  tst.w (0x1143AC)      ; guard
8f9c  jsr 0x775A            ; <-- the entire read
8fa2  tst.w (0x113138)      ; repeat-flag loop
8fac  print 0x1181A, then ':' (0x3A), then ' ' (0x20)
8fe6  bsr.w 0x8B5C          ; dump the 8 words
```

`0x775A` `[V]`:
```
7762  move.w #0x2010,(0x220000)   ; MODE + MDCLK
776a  jsr 0x77B6
```
and `0x77B6` `[V]`: for each of 8 words - **8 shift steps with the phase REVERSED**
(`0x000F` ADCLK **then** `0x0010` MDCLK, opposite to the write engine at `0x7776`), then
`0x2011` = **MODE + CAPR (PCLK to APR)**, then `move.w (0x550000),(A3)` - read APR into
`0x1144F0[i]`.

Manual 5.3.29 `[V]`: "A 128-bit microinstruction is read from MIR. (**The contents of MIR are
clocked into MISR and shifted out to ASR.**)" Exact match, and it names the `0x2010` MODE+MDCLK
write at the top of `0x775A` as the MIR-into-MISR clock.

**Destructive / re-latching - the honest answer:**

- It is **not** destructive of MIR in the sense of clearing it. `[I]`
- It **does** clock MIR into MISR (`0x2010`) and then **shift MISR out 64 steps** `[V]`. MISR is
  therefore left holding shifted-out garbage, **not** the microword. If you `READ MIR` and then
  `CONTINUE` expecting the previously loaded microword still to be staged in MISR, it is not.
  `[I]` strong - this follows from the shift being destructive of shift-register contents, which is
  what a shift register is.
- Practical rule: **`READ MIR` last.** Do not interleave it between `LOAD MIR` and `CONTINUE`.
  `[I]`
- It also leaves APR holding the last word shifted out `[V]`.

**Reply framing** `[V]`, ND-05.020.01 p.125: "If a command requires a response with returned data
via octobus (**e.g. READ MIR**), this data is sent **directly after Messack in the same multibyte
message**." So the reply body is `Messack byte` followed by 16 data bytes. Direct multi-byte
parameters are **most significant byte first** (p.124) `[V]`, and the parameter field "is organized
in 16-bit words". Word order on the wire follows the buffer order, i.e. `0x1144F0` first `[V]`.

---

## R3.5 Run control: `Cmd23` / `Cmd24` / `Cmd25` - CARVED, with a NAME CORRECTION

### The engines

| Engine | Command | Firmware |
|---|---|---|
| `0x78CA` | **STARTMIC** | `[V]` |
| `0x795A` | **STOPMIC** | `[V]` |
| `0x79BC` -> `0x79E4` | **CONTMIC** | `[V]` |

**NAME CORRECTION, flagged loudly as instructed.** `ACCP-COMPLETE-REFERENCE.md` records `0x795A` as
"**the octobus-controller re-init routine** - it is the one thing both reset paths call, and
therefore the natural next carve target". **`0x795A` is STOPMIC.** `[V]` It is called by
`Cmd24_StopMicroprogram @ 0x91C6` and its body is exactly the manual's description of STOPMIC. The
reset paths call it because a reset stops the microprogram first, which is a consequence, not its
identity. Third time a name-based assumption has misled this interface, after `0x300F`/`0x4016`/
`0x8013` and `0x0007`.

### `Cmd23_StartMicroprogram @ 0x9110` -> `0x78CA`

Direct parameter: **control store address, 2 bytes** `[V]` (5.3.23). Body `[V]`:
```
78d6  bsr.w 0x76E6                 ; LOAD MAR with the start address
78da  clear MREG-lower bit 1 (SLOW), push to 0x330001
78ec  set MREG-upper shadow bit 2 -> MREG bit 10 = AECS (ACCP Enable Control Store), push
78fc  0x0017  = ARMI    (ACCP reclock MIR with ECMIR)
7904  0x0015  = ARMA    (ACCP reclock MAR)
790c  restore MREG upper
7916  ori.b #0x5C on MREG-lower shadow -> bits 2,3,4,6 = AMODE, MRUN, ORESEN, MR
7926  if CPU model word (0x1131F8) is 0x5400 or 0x5500: clear MREG-lower bits 0 and 1 (FAST, SLOW)
```
Manual 5.3.23 `[V]`: "The microprogram is started at the given CS address **by loading MAR with
CSA**. **The AMODE bit is reset and the MRUN bit is set** in the modus register." Exact match -
remember AMODE, MR and SLOW are **polarity 0** in MREG table 8, so writing 1 de-asserts them.

Messnak codes `[V]`: `-1` illegal when running, **`9` control store not initialized**.

Note the **ND-5400 / ND-5500 special case** at `0x7926` `[V]`: those two models get FAST and SLOW
both cleared. Not documented in 5.3.23. If your emulated CPU model word is `0x5400`/`0x5500` the
clock-speed bits end up in a different state than for other models. `[OPEN]` why.

### `Cmd24_StopMicroprogram @ 0x91B8` -> `0x795A`

No parameters `[V]`. Body `[V]`: from the MREG-lower shadow, clear bit 3 (MRUN) and bit 1 (SLOW)
into a temp and push; then clear bit 2 (**AMODE**, polarity 0, so clearing **asserts** it), save the
shadow, push. Then `Cmd24` does `clr.w (0x1143AC)`.

Manual 5.3.24 `[V]`: "The microprogram is stopped immediately by **resetting MRUN and setting
AMODE**." Exact match.

**Messnak `-2`: "Illegal when kicks are enabled."** `[V]` See R3.7 - this is the second trap.

### `Cmd25_ContinueMicroprogram @ 0x9218` -> `0x79BC`

No parameters `[V]`. Body `[V]`:
```
79c4  if (0x113138) == 0 then bsr 0x78B2
79d0  jsr 0x79E4:
        temp = MREG-lower shadow with bit 3 (MRUN) CLEARED and bit 1 (SLOW) cleared -> push
        shadow |= bit 3 (MRUN) | bit 2 (AMODE de-assert)                              -> push
79d6  move.w #0x1,(0x1143AC)      ; <-- SETS THE RUNNING FLAG
```
Manual 5.3.25 `[V]`: "To handle both situations, **MRUN is first reset and then set.**" Exact match,
down to the ordering.

**`0x79D6` is the trap.** CONTINUE marks the microprogram running even though it is about to stop
again at the next STOP bit. Every subsequent LMAR / LMIR / RMIR is rejected until STOPMIC clears it.

### Legal call order for one microcycle - the answer to your item 4

**There is no dedicated single-step.** `[V]` The order is:

**STOP -> LOAD MAR (-> LOAD MIR) -> CONTINUE -> STOP -> READ MIR**

with the STOP bit set in the microword so CONTINUE terminates. The trailing STOP is not optional -
see R3.0 point 2.

---

## R3.6 `Cmd22_LookAtControlStore @ 0xAA5E` - partially carved

`[V]`: it carries the **same `0x1143AC` guard** at `0xAAAA`, printing the same `0x126EE` message.
So control store cannot be read while the microprogram is running either.

`[V]`: it is an **interactive console browser** (a PLANC `ON ROUTINEERROR` frame, a prompt buffer at
`0x1132FA` of length `0x28`, an index at `0x114390` seeded from `0x1131FC`), not a single-shot
command. I did **not** carve its inner read loop.

**For your purpose you want the octobus command, not this.** ND-05.020.01 p.124 names it `[V]`:
"Some commands have direct parameters and instruct the ACCP to put data in the parameter field as a
response (**e.g. dump control store**)", and 5.3.10 says long-parameter commands like LOAD CONTROL
STORE normally pass **via MFbus memory**, with LOAD PARAMETER POINTER / VERIFY PARAMETER POINTER
setting up the shared area. `[OPEN]`: the exact DUMP CONTROL STORE command byte and its
parameter-area layout - ask and I will carve it, but for confirming you loaded the site you meant,
**LOAD MIR's own read-back self-verify (R3.3) is cheaper and already proven**.

---

## R3.7 The kick interlock - the second precondition that produces silence

ND-05.020.01 p.125 `[V]`:

> "There are also some commands which **can not be executed after the ENABLE KICK command has been
> given**. (For example, READ AIB would destroy a kick being sent from the ND-5000). These cases
> return **Messnak with -2**."

and **STOPMIC's error list includes `-2`: "Illegal when kicks are enabled."** `[V]` (5.3.24).

**Consequence: once kicks are enabled you cannot stop the microprogram, therefore you cannot run
any part of this recipe.** On a machine that has reached SINTRAN's ND-500 monitor, kicks are on.
**Do the EXUC experiment on a machine where ENABLE KICK was never issued** - a cold ACCP with the
control store loaded and the microprogram halted, not a booted system. `[V]` on the rule, `[I]` on
"a booted system has kicks on", though your own kick work makes that near-certain.

---

## R3.8 How you know the CPU is really halted - item 7, and ASTS bit 12 is not the best answer

Two independent indicators, both `[V]` from ASTS table 7:

| ASTS bit | Name | Polarity | Meaning |
|---|---|---|---|
| 12 | **STOP** | 1 | Microprogrammed stop |
| 10 | **ALIVE** | 1 | CPU alive watch-dog signal |

**ALIVE is the better instrument, and the firmware already implements the test.**
ND-05.020.01 5.3.26 ALIVE CHECK `[V]`:

> "Checks if the microprogram is running by polling the ALIVE signal in the status register. **ALIVE
> is generated by a flip-flop clocked by the MIR clock and reset by the ACCP. The microprogram has
> stopped if ALIVE is not set approximately 10us after it has been reset by the ACCP.** If ALIVE is
> true, Messack is returned; otherwise Messnak is returned." Messnak error **7: Not alive**.

That is a *positive* test for motion - the flip-flop is clocked by the MIR clock, so it can only be
set by the microprogram actually advancing. ASTS bit 12 STOP tells you a microprogrammed stop
occurred; ALIVE tells you nothing is executing. Use both: **STOP set AND ALIVE-check returning
Messnak 7** is unambiguous.

Implemented at **`Cmd27_CheckAlive @ 0x9D9C`** `[V]` (function present and named; I did not carve
its body this round). The reset strobe is **ACON `2h CLRALIVE`** = "Resets ALIVE flip-flop" `[V]`.

**Cross-check worth noting:** `CLRALIVE` (`0x0002`) is one of the ACON codes that appears **nowhere**
in your 7-million-access boot census. That is consistent with ALIVE CHECK being a debug-only command
never used in a normal boot, and it is a small independent confirmation that your census is complete
rather than truncated. `[V]`

---

## R3.9 The tracer - item 6. Right data, wrong reader.

**Verdict: the tracer captures exactly the observable your EXUC question needs, but the ACCP cannot
read it out. On real hardware it is not a better instrument. In your emulator it is by far the best
one, because the read restriction does not apply to you.**

What it captures, ND-05.020.01 chapter 13 `[V]`:

- Trace memory is **160 bits wide and 4K deep static RAM**, storing every micro or nanocycle.
- Partition **CTRACE**: "the 16 least significant bits contain **the microprogram address (MAR
  0:15)**". Upper 16 bits carry DCC nano-sequence states, **COBR (conditional break from MIC)**,
  **MRMI (reclock MIR from MIC)**, **MICW (MIC wait)**, the pipeline **WAIT** signal, a spare, and
  **TRIGD**.
- Other partitions: ITRACE (MIB), DTRACE (DB), ATRACE (AOP).

**A 4096-entry per-microcycle record of MAR is precisely the sneak-cycle observable.** If an `EXUC`
word's sneak fires, the MAR sequence contains the jump-field target between the site and its
successor. That settles all 91 sites in one capture, and it settles the 47 EXCYC2 sites too -
including the 15 self-referential chains, where a depth bound shows up directly as a repeat count in
the MAR trace. **This is a better experiment than single-stepping** and it does not require
modifying any microword, which removes the objection in R3.10.

**Why the ACCP cannot do it** `[V]`, p.317: "The memory is **accessible from microprogram (only for
read)** in five 32-bit partitions. The contents can be read consecutively from address zero after
clearing the Trace Address Counter. One of the five read actions, read C-trace, increments the
address counter."

So readout is a **microprogram** operation, not an ACCP one. Running microcode to read the trace of
microcode is circular for a hardware bring-up, which is presumably why ND built it that way for a
logic-analyser workflow. **In an emulator you just read the trace RAM array directly and the
circularity vanishes.**

Arm / trigger control `[V]`, p.319: driven by the microprogram destination `D,SPEC,CTRACE`:
bit 1 = arm tracer, bit 2 = trigger tracer, with combined codes 1-7 (1 = clear address counter and
disarm, 2 = arm, 3 = clear and arm, 4 = trigger and disarm, 5 = trigger, disarm and clear, 6 =
trigger and arm again, 7 = trigger, arm and clear). There is also a mode register with "11: **Stop
microprogram on trigger**" and a **middle-trace** mode located via TRIGD.

The ACCP's part is only the two ends: **ACON `1h TRIG` = "Trigger for tracer"** and **ASTS bit 14
ARMED = "From tracer. Goes off when tracer has triggered."** `[V]`. Your census shows `TRIG`
executing 5 times per boot, so the path is live - but that is the ACCP poking the trigger, not
reading the capture.

`[OPEN]`: the ACCP-side address of the trace RAM, if any. I found none, and the manual says the
reader is the microprogram.

---

## R3.10 One honest objection to the single-step recipe

The manual's method requires **setting the STOP bit in the microinstruction**. The STOP bit is a
field of the 128-bit microword. For the EXUC question you are measuring how the sequencer treats a
specific word - and you would be measuring a **modified** word.

`[OPEN]` whether the STOP bit interacts with `EXUC` or with the conditional-sequence field. If it
does, the experiment measures the modification. Mitigations, in order of preference:

1. **Use the tracer instead** (R3.9). It needs no microword modification at all. In an emulator this
   is strictly better and I would do only this.
2. Set the STOP bit on the **successor** word rather than the site under test, so the site executes
   unmodified and the machine halts one cycle later. Then MIR after the halt still tells you which
   word was reached.
3. Run the same site twice, once with the STOP bit on the site and once on its successor, and check
   the answers agree. If they disagree, the STOP bit is not inert and mitigation 1 is mandatory.

I am flagging this rather than hiding it because it is exactly the shape of error this interface has
made twice: a measurement that produces a clean number about the wrong thing.

---

## R3.11 THE RECIPE - executable without reading any of the above

### Preconditions - verify ALL FOUR before you start

| # | Precondition | How to check | If violated |
|---|---|---|---|
| P1 | **Kicks are NOT enabled.** ENABLE KICK was never issued on this machine. | your own kick state | STOPMIC returns Messnak `-2` and **nothing below works** |
| P2 | **Control store is initialised** (microcode loaded). | you loaded it | STARTMIC returns Messnak `9` |
| P3 | **The ACCP "running" flag `0x1143AC` is 0.** | issue STOP MICROPROGRAM (step 1) | LMAR/LMIR/RMIR return Messnak `-1` |
| P4 | **A STOP bit is set in a microword the CPU will reach.** | you set it | **CONTINUE HANGS FOREVER** |

Command framing, all `[V]` from ND-05.020.01 5.3.11: multibyte octobus message, **DestOMD = 3**,
protocol version 5. **Command byte first in the message body.** Direct parameters follow, organised
in 16-bit words, **most significant byte first**. Reply is `Messack` (single byte, sent as a
multibyte message even with no parameters) or `Messnak` (error code, then ASTS low byte, then ASTS
high byte). If neither arrives, send TERMINATE.

### Procedure

**Step 0 - prove your framing before you trust anything.**
Issue `LOAD MIR` with any recognisable 128-bit pattern. LMIR **reads MIR back and compares
internally** and reports a mismatch. If it reports a mismatch, your byte or word order is wrong -
fix it here, not after you have an answer you like.
*Expected observable:* Messack, no mismatch message.

**Step 1 - STOP MICROPROGRAM.**
`Cmd24`. No parameters.
*Effect:* MRUN reset, AMODE set, and **`0x1143AC` cleared** - this is what makes steps 3-4 and 7
legal.
*Expected observable:* Messack. Messnak `0` ("microprogram is not started") is harmless if it was
already stopped. **Messnak `-2` means P1 is violated - STOP and fix that first.**

**Step 2 - confirm the CPU really is halted.**
Issue `ALIVE CHECK` (`Cmd27`). *Expected observable:* **Messnak error 7 "Not alive"** - that is the
PASS condition here. Also read ASTS and expect **bit 12 STOP set**. Both together is unambiguous.
If ALIVE CHECK returns Messack, the microprogram is still executing and everything below is invalid.

**Step 3 - LOAD MAR with the control-store address under test.**
`Cmd28`. Direct parameter: **2 bytes, 16-bit CS address, most significant byte first.**
*Effect:* the address is shifted into the MISR jump field and clocked into MAR.
*Expected observable:* Messack.
*Trap:* an operand of `0xFFFF FFFF` makes the firmware **return silently with no action and no
error**.

**Step 4 (optional) - LOAD MIR.**
`Cmd29`. Direct parameter: **16 bytes = the 128-bit microword, most significant byte first, word at
staging offset 0 sent first.**
Use this only if you want to override control store - e.g. to place the STOP bit. Prefer setting it
on the **successor** word (see R3.10).
*Expected observable:* Messack, no mismatch message.

**Step 5 - CONTINUE MICROPROGRAM. This is the microcycle.**
`Cmd25`. No parameters.
*Effect:* MRUN is reset then set; the CPU executes from MAR and halts at the next STOP bit.
**Side effect: `0x1143AC` is set to 1.**
*Expected observable:* Messack, returning **only after** the stop condition occurs.
*Trap:* **if no STOP bit is reachable, this never returns.** Have a timeout and send TERMINATE.

**Step 6 - STOP MICROPROGRAM again. DO NOT SKIP THIS.**
`Cmd24`. No parameters.
*Why:* step 5 set `0x1143AC` and only this clears it. **Without step 6, step 7 returns Messnak `-1`
and your harness sees no data - which is indistinguishable from "the sneak did not fire".**
*Expected observable:* Messack.

**Step 7 - READ MIR.**
`Cmd2A`. No parameters.
*Expected observable:* **Messack followed by 16 bytes in the same multibyte message** - the 128-bit
MIR contents, most significant byte first, staging-offset-0 word first.
*Do this LAST.* READ MIR clocks MIR into MISR and shifts MISR out, so MISR no longer holds the
microword afterwards. Do not interleave it between steps 4 and 5.

### Reading the answer

Let **X** be the site under test, **T** its jump-field target (`ABS_ADDR`), **X+1** its sequential
successor.

| MIR after step 7 holds | Conclusion |
|---|---|
| the microword at **T** | **the sneak FIRED** - section 7.3.5 rule 1 wins, and your current model under-fires at all 91 sites |
| the microword at **X+1** | **the sneak did NOT fire** - section 7.2 wins, `CpuND5000.cs:805` is right, and `SHIFT_ROT@017070` generalises |
| all-ones, all-zeros, or a Messnak | **not a result.** Re-check P1-P4 and step 6 |

Cross-check the identification by reading the same control-store addresses back independently rather
than assuming - LOAD MIR's self-verify path gives you a free way to confirm what a given word
actually contains.

### If you can, do this instead

Run the tracer (R3.9). Arm it, execute the site, read CTRACE, and read `MAR(0:15)` per microcycle
straight out of the 4K trace RAM. No microword modification, no `0x1143AC` dance, no kick
interlock, and it answers all 91 sites plus the 47 EXCYC2 sites in one capture. On real hardware the
trace RAM is microprogram-readable only; **in your emulator that restriction does not exist.**

---

## R3.12 Status of the seven requested items

| # | Item | Status |
|---|---|---|
| 1 | `Cmd28_LoadMar` | **CARVED** - firmware + manual 5.3.27 agree |
| 2 | `Cmd29_LoadMir` | **CARVED** - incl. the `0x1144F0` shared path and the self-verify |
| 3 | `Cmd2A_ReadMir` | **CARVED** - incl. reply framing and the re-latch caveat |
| 4 | `Cmd23/24/25` run control | **CARVED** - no dedicated single-step; order is STOP/LMAR/CONT/STOP/RMIR |
| 5 | `Cmd22_LookAtControlStore` | **PARTIAL** - guard carved, inner loop not; it is a console browser. DUMP CONTROL STORE command byte `[OPEN]` |
| 6 | Tracer | **CARVED from the manual** - right data, microprogram-only readout. ACCP-side trace RAM address `[OPEN]` |
| 7 | ASTS bit 12 STOP | **CARVED** - and ALIVE (bit 10) via ALIVE CHECK is the better test |

Remaining `[OPEN]` I would take next, in order: the DUMP CONTROL STORE command byte and parameter
layout; `Cmd27_CheckAlive`'s body; whether the STOP bit interacts with `EXUC` (R3.10); and the
ND-5400/ND-5500 clock-bit special case at `0x7926`.
