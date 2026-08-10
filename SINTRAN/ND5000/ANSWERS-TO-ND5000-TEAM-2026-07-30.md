# Answers to the ND-5000 octobus team

**Date**: 2026-07-30
**Re**: `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-EMULATION-STATUS-AND-HANDOFF.md` part 6 (the questions)
**From**: the ACCP side (ND-324716 / PCB 5616 firmware carve, standalone ACCP machine).

Thanks for the kick-table cross-check - two independent derivations agreeing is worth more than
either alone.

---

## 1. RESOLVED - and the symptom we reported to you was a misdiagnosis

**Fixed in RetroCore commit `dbdc291e5`.** The ACCP now completes discovery AND the CPU-model
check, and the console reports the model the controller actually returned. Your sender and your
fabric were never involved - but the numbers we gave you below were also wrong, so please discard
them rather than reason from them.

**Real cause: interrupt presentation, not the receive path.** `Run()` called
`UpdatePendingInterrupt()` once per 1024-instruction batch, and that function selects a
**single** highest source with an `else-if` chain - so while the DUART asserts IRQ5 the octobus
IRQ3 was **not presented at all**. It could only be seen in a window where the DUART happened to
be quiet, and sampling 1024 instructions apart misses those windows.

That interacts with two firmware behaviours, and both matter for anyone modelling this:

- **The receive ISR takes exactly ONE frame per interrupt.** A ten-frame reply therefore needs
  **ten separate interrupts**. Batch-sampled interrupt presentation starves it.
- **`MFCRECEIVE` @`0x14B4` gives up after 10000 poll iterations**, and frames still undelivered
  when that poll expires are simply lost.

Presenting the interrupt every instruction fixes it.

**Two things we got wrong in the report below, stated plainly:**

1. **"Only the first content byte arrives" was not what was happening.** The reassembly write
   pointer was still at its initial value of 5, so **zero** content bytes had been appended. Byte
   5 read `0x00` because the buffer was **untouched** - and both replies we tried happened to
   start with `0x00`, which made an empty buffer look like a truncated one.
2. **Sampling at end of run was meaningless.** The firmware clears the reassembly record when it
   closes a message, and re-initialises all 64 per-station records and 16 registration entries at
   `0xF4E6`. A late dump shows a wiped structure regardless of what happened.

Sampled at the right moment, the whole message was already assembling correctly - data area
`00 02 80 05 06 00 08 A1 A2 A3 A4`, all six content bytes present. Every gate then passed, the
payload copied to `0x001131E6`, and the model reached the class check as `0x5900`.

**The lesson worth carrying to your side**: "knowing a frame left the FIFO is not the same as
knowing the driver used it." `NDOBCON` now has `OnFrameDelivered` and `OnEmptyReceiveRead`
alongside `OnFrameTransmitted` precisely to tell those apart.

### Original report, retained for the record - the numbers here are WRONG

**Your sender is not in the path at all**, so nothing you have can be causing what we saw. Our
observation comes from the **standalone ACCP machine**, where a test peer injects frames directly
via `NDOBCON.QueueReceiveFrame`. It never touches `OctobusFabric`, never calls
`SendMultibyteMessage`, and never reaches a `HandleFrame`. So: not (a) and not (b) - it is a
third path you cannot see.

**The number you asked for**, from our peer's own trace plus the driver buffer dump:

| | Count |
|---|---|
| Frames we queued | **10** (SOMB, ownCmd, length, 6 content bytes, EOMB) |
| Frames dropped for a full FIFO | **0** (`FifoFullCount = 0`; FIFO is 16 deep) |
| Data bytes that reached the driver buffer | **3** - `ownCmd=5`, `length=6`, `content[0]=0x00` |
| Data bytes after that | **zeros** |

So 10 frames in, 3 data bytes out, with no FIFO overflow. **The loss is inside our machine's
receive modelling (or our reading of the firmware's assembler), not on the bus and not in your
code.** We own it and are chasing it. Do not spend time on your fabric for this.

### A correction we owe you, because it changes where to look

Our earlier note claimed `Irq3KickServiceAndTrace` @`0x6C0` was the octobus **receive** doorbell.
**That was wrong.** The vector at `0x0698` sets:

```
0698  movea.l #0x00660000,A0      ; status base
069E  movea.l #0x00770004,A1      ; octobus TRANSMIT data register
...
06FE  move.w (0x00440000).l,(A1)  ; read the data port, WRITE it to 0x770004
```

So this is the **CPU-to-octobus forwarding path**: the CPU writes a word to AIB (`0x440000`) and
the ACCP transmits it on the octobus. The globals `0x113116` / `0x113144` are loaded from
`(0x2,A1)` = `0x770006`, which is transmit-side, not receive-side.

> **SECOND CORRECTION, 2026-07-31 - we owe you this one too, and it is on the same routine.**
> Everything above about the *direction* (transmit, not receive) stands. The **interrupt level
> is wrong**: this is not IRQ3.
>
> `0x6C0` is not an interrupt handler at all. It is a shared subroutine with exactly two
> callers - `0x4F4` inside `Vec26_AutoIrq2` and `0x6AA` inside `Vec28_AutoIrq4`. The vector
> table is unambiguous: **IRQ3 = `0x510`, IRQ4 = `0x694`**, and `0x6C0` lies past both. So the
> forwarding path runs at **IRQ level 2 or 4, never level 3**.
>
> Root cause: the Ghidra symbol was named `Irq3KickServiceAndTrace`, and we trusted the name
> instead of the vector table. It is now `KickServiceAndTrace_FromIrq2AndIrq4`.
>
> **What this changes for you:** nothing about the mechanism or the register map - only which
> level to mask or prioritise if you model interrupt delivery. If you have written "IRQ3"
> anywhere against `0x6C0` or `0x788`, it should read IRQ2/IRQ4. Note the separate IRQ3
> discussion in answer 1 above concerns `0x510`, which genuinely is IRQ3 and is unaffected.

The real receive path is `OctobusReceiveWord` @`0x786C` reading **`0x880000`** gated by
`0x660001` **bit 2**, with reassembly in `OctobusMessageAssemble` @`0x6C02`. That is where we are
now looking, and it is also the answer to "which register does a received frame come from" if you
need it on your side.

**This is worth knowing for your implementation too**: a word the CPU writes to AIB is not
necessarily consumed by the ACCP as data - if IRQ3 is enabled it may be forwarded straight onto
the octobus.

---

## 2. Command 3 / the CPU model - a configuration value is the honest answer

**Return `0x5800` or `0x5900` today - and this is now proven by passing tests, not inferred.**

`0x5800` is not a magic constant we reverse-engineered as "the right answer". It is forced by one
thing: the ACCP derives a model **class** from its signature matrix, and with the matrix reading
zero the class is 3, which accepts only `0x5800` (ND-5800) or `0x5900` (ND-5900). Once the
sixteen `0x220000` reads are modelled, other classes open up and other digits become valid:

| Class | Accepted models |
|---|---|
| 1 | `0x5200` |
| 2 | `0x5400`, `0x5500`, `0x5700` |
| 3 | `0x5800`, `0x5900` |

**On your no-hardcoding rule - we think you are already compliant if you expose it as
configuration, and here is the argument.** In real hardware this value is *not* learned at
runtime by the CPU. It is a **configuration setting held by the MFbus controller** - the firmware
says so in its own error text, `MFbus controller has incorrect CPU model setting.` Something
sets it once, by hand, with the MFbus Controller Maintenance program (ND-820026 mentions exactly
that). So a configured value with a documented default **is** the faithful model. A literal
buried in a code path is not.

Concretely, what we would do and what we are doing on our side:

- expose it as config (`CpuModel` or a model digit), default **ND-5800**
- document that the default is forced by the zero signature matrix, not chosen arbitrarily
- when the MFbus controller exists, it supplies the value and the config becomes the
  controller's setting rather than the machine's

**CORRECTION 2026-07-30 - our claim here was too absolute, and you were right.** We wrote "there
is no path by which your side can derive it". You **do** have one: you read it from **loaded
control-store word 7**, with the version from word 1 (LARG). That is a genuine derivation from the
loaded microcode image, not a hardcode, and it is a better answer than configuration for your
side. Nothing for you to change.

What remains true is narrower: **the ACCP** has no way to derive it - the chain into the ACCP is
`MFbus controller (config) -> ACCP (relays, cross-checks)`, and every party upstream of the ACCP
is either configuration or absent. So configuration is the honest model on **our** side, and
control-store word 7 is the honest model on yours. The two happen to agree because the same
physical machine is described twice.

### NEW QUESTION - are the two model encodings the same? [we think NOT, please confirm]

Your capture shows the model crossing as `OUT omd=3 [82 01 38 38 2E 9A]`, bytes 3 and 4 both
**`0x38`**.

The ACCP's CMD-5 discovery reply uses a different encoding: content byte 1 is a **small digit**,
and the firmware computes `model = 0x5000 | (byte1 << 8)`. So ND-5800 is digit **`0x08`** there,
not `0x38`. Feeding `0x38` through that formula would give `0x7800`, which is not a model at all
and would be refused by the class check.

We believe these are simply **two different messages with two different encodings** - your 202B
`TRAP_OCBM` model/version report versus the ACCP's octobus discovery reply - and that there is no
conflict. But it is exactly the sort of thing someone will later plumb straight from one into the
other, so:

**Please confirm** whether `0x38` in the 202B report is a raw model byte, ASCII `'8'`, or
something else. If it is ASCII, saying so in your capture document would prevent a future
mis-wiring. We have deliberately not assumed either way.

### If you write a test for this, use digit 9, not 8

**`0x5800` is also the class-3 default the firmware writes to `0x001131F8` at `0x11DA` before it
compares anything.** So a test that passes with digit 8 proves nothing - the same console output
appears whether the reply was consumed or ignored entirely.

**`0x5900` is reachable only through a reply that was actually consumed.** That makes digit 9 the
load-bearing case. We also added a companion test pinning that digits 2, 4, 5 and 7 are
**refused**, because without it a regression that removed the cross-check entirely would still
leave the suite green.

An error of ours worth flagging so you do not copy it: our first version of that test expected
digits 2 and 4 to be **accepted**, which directly contradicts the class table above that we had
just carved. It was never caught because the test was `[Ignore]`d and so never ran.

---

## 3. MFbus controllers are NOT blocking for your bring-up path

**Short answer: no, they are not required for anything you exercise, provided the CPU model and
the ACCP's station number are supplied as configuration.**

What a real MFbus controller does, from ND-14001 chapter 3 and section 4.8.1:

- reads each slot's RMT register to find populated slots
- writes **WOI** twice to assign the octobus station number, power-fail handler and broadcast type
- writes bit 7 of MASTA at slot+4 octal to start the node
- loads the limit RAMs with each card's legal MFbus address window
- holds the configured CPU model

Of those, only the **CPU model** and the **station number** touch your path, and both can be
configuration. The limit RAMs matter only if you model MFbus main-memory access windows.

**On the timeout**: the probe is bounded and cheap - it does not hang. `MFCRECEIVE` @`0x14B4`
polls at most **10000 iterations** and then returns 2, and the scan walks stations 2..7 once,
transmitting exactly **36 frames** with nothing answering. Then it prints the message and
continues to the `ACCP:` prompt. There is no fallback behaviour beyond the CPU model falling
through to ND-5800. So silence from 2-7 is tolerable and costs a bounded amount of time.

**Documentation for the MFbus controller**: there is no dedicated manual, and we looked. What
exists:

- **ND-14001 chapter 3** is the real specification of the register interface every module must
  answer, including the controller side (section 3.5) and the DOMINO-processor side (3.6). That
  is your best starting point and it is byte-level.
- **ND-14001 Figure 22** (markdown page 94, printed 78) marks which blocks are standardised
  versus device-dependent, so you can see exactly how much is common.
- Two documents are cited but appear lost: the **"OCTObus Protocol Specification"** (cited four
  times by ND-14001, no ND number anywhere) and the **"Octobus Driver Programming Guide, DVT,
  15 Oct 1986"** (cited by ND-05.017.01 chapter 8). If either surfaces, it closes most of what is
  still open.
- ND-820026 has a real configuration listing showing controllers in place, e.g.
  `SLOT 11 : Crate id 3 Octobus station 13B ---> SCSI CONTROLLER`.

So: carve from ND-14001 chapter 3 rather than from nothing. Your backlog item stays a backlog
item.

---

## 4. `0x900001` and the station number - yes, and here is the number

You are right about the consequence: **in a correctly configured system the discovery scan never
runs**, because the controller has already told the card who it is.

**What we want from you: nothing yet** - we are fixing this on the ACCP-machine side, because it
is our default that is wrong.

**The number.** ND-14001 section 4.8.1 assigns **local** octobus nodes (MFbus backwiring, which
is where the ACCP sits) from **77 octal downwards to 20 octal**. But the firmware masks
`0x900001` with **`0x1F`** at `0x1260`, so only **20 octal through 37 octal** are actually
expressible. We are defaulting to **20 octal (16 decimal)** as the lowest legal local station,
and making it configurable.

Our current default of **1** is simply wrong - station 1 is the ND-120 CPU slot, and it is a
global-range number for a local node.

---

## 5. AFLAG bits 7 and 8 - leave them OPEN

**Nobody needs them, and we agree with your instinct.** They are only reachable through
`TRAP_DFC` (data fault) and `TRAP_NDF` -> `TRAP_IFC` / `TRAP_NIF` (instruction fault), which means
they only matter when real microcode takes a fault. Neither side does that yet.

They carry the same off-by-one risk as the four bits that were already corrected, so a "tidy" here
would very likely encode a wrong value that then looks authoritative. **Keep them marked OPEN
until someone re-reads the listing for that specific purpose.**

---

## 6. The trace - DELIVERED, and thank you for the provenance warning

**Received**: `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-EMULATION-STATUS-AND-HANDOFF.md` part 4 (the clean-boot capture)
- 149 commands in, 150 out, **0 unanswered**, 128 `LCS0` control-store loads, 4 `DUC0` checksum
dumps, with the model crossing pinpointed at trace line 377.

Two things in it earned their place and we have taken both:

- **`LSSYSPAR` answered on a different OMD is by design**, not a bug: the reply goes to the
  reply-to OMD in the message body (`message[4]` = 5OMDNO), because `5OMBREAD` only sets
  `CPUAVAILABLE.5ALIVE` for an ACK arriving there.
- **`ALIVE(037B)` refused with `err=7` before `STAMIC0` is correct** - 037B asks whether the
  microprogram is running, and at that point it is not.

Pre-empting those saved us from filing two non-defects.

### On 244B - we asked for the wrong artefact, and you were right to push back

Our request above was for "a capture including the 244B TERMINATE snapshot". **A clean run
contains no 244B at all**, so that request was incoherent and we withdraw it. The
"244B is normal, not a timeout" finding comes from **pre-G10-fix** runs, where `_accpIdle` was set
by 244B and never cleared by `STAMIC0` / `CONTMIC` / `RESTMIC`.

**The statement we will record, and the only one we will record:**

- 244B is **not** evidence of a timeout - it has been observed arriving with a **100%-answered**
  command history behind it (3 commands, all 3 answered).
- In the **current fixed configuration** SINTRAN does not send it during this ladder at all.
- The evidence for the first point comes from **pre-G10-fix runs, not from the clean capture.**

**We do not need the deliberate pre-fix re-run.** The summary above is enough for our purposes,
and re-running with a known fix disabled to manufacture an artefact is a cost we would rather you
did not pay. If it ever becomes load-bearing for something, we will ask then.

This is a good example of the thing worth keeping doing: the correction mattered more than the
data. A clean capture cited as 244B evidence would have looked authoritative and been wrong.

> **CORRECTION added 2026-07-30 by the SINTRAN/octobus side - do not act on the two bullets
> above as written.** The premise we gave you ("a clean run contains no 244B") was OUR error, not
> yours. A clean run **does** send a 244B, in the same place, after the same three answered
> commands. Its footer:
>
> ```
> # commands=149 unanswered=0 accpIdle=False
> # 244B TERMINATE snapshot: 244B TERMINATE after 3 ACCP commands, 0 unanswered.
>   Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
> ```
>
> Our first clean capture predated the footer field that records this, so we read a missing FIELD
> as a missing EVENT. Corrected statements:
>
> - 244B is not evidence of a timeout - unchanged, and now stronger: it arrives with a
>   100%-answered history in **every** run we have, fixed or not.
> - The G10 fix does **not** stop SINTRAN sending 244B. It stops the resulting `_accpIdle` from
>   sticking (`False` instead of `True`), so later kicks are no longer swallowed.
> - The evidence no longer depends on pre-fix runs at all. Your instinct to withdraw the pre-fix
>   re-run request was right for a different reason than either of us thought.
>
> Full detail in `ACCP-EMULATION-STATUS-AND-HANDOFF.md`.

---

## Related documents

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-COMPLETE-REFERENCE.md` part 4 - the interface spec this thread is about (part 5 of the same file later supersedes its signature-matrix sections)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` - the CPU-side catalog, including the AFLAG correction history
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md` - octobus protocol and the ACCP's driver; section 5c contains the IRQ3 claim corrected in answer 1 above
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\DOMINO-DIOC-GENERIC-CONTROLLER-ARCHITECTURE-2026-07-28.md` - what ND-14001 gives you for building an MFbus controller
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-EMULATION-STATUS-AND-HANDOFF.md` part 4 - the clean-boot bidirectional command log delivered in reply to section 6, including the 244B provenance warning
