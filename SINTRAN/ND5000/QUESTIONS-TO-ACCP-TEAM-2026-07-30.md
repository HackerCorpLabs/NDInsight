# Questions to the ACCP / ND-5000-octobus team

**From**: the SINTRAN-over-octobus side (ND-100 card, fabric, `OctobusND5000Station`, the C#
servicer that stands in for the microcode).
**Re**: `ACCP-ND5000-CPU-INTERFACE-SPEC-2026-07-30.md`.

Context on our side, so you know what is already true and do not re-derive it:

- The ND-500 monitor over octobus completes its whole administrative ladder, the swapper runs and
  allocates 7110B pages, and `stop-system` halts the ND-100 cleanly.
- Kicks now work end to end. Our kick table matches yours exactly (0 NOTREC; 1,2 ACTIVATE;
  3 OCB_KICK03; 4,5 OCB_KICK05; 6 OCB_KICK06; 7-63 NOTREC 204). Kick 3 and kick 6 were
  implemented against the executed B30 microcode, not the carve summary.
- Activation is the `X5ACT := 0` write (ACT51), never a kick. In a whole SINTRAN boot exactly ONE
  kick is sent: `CLRKICK` at `stop-system`.
- We measured 0 of 149 ACCP commands unanswered in a full run.

---

## 1. Your open item 6 - which side is the multibyte truncation on?

You report: *"only the FIRST content byte of an octobus multibyte reply reaches the ACCP driver's
receive buffer, so the model digit cannot currently get through"*, with the dump at `0x00112D54`
showing `byte5=0x00` content[0] then zeros.

**Our SENDER emits every payload byte.** `NDBusOctobus.SendMultibyteMessage`:

```
SOMB (C=1,M=1,S=1, destOmd)
data frame: sourceOmd
data frame: payload.Length
data frame: payload[i]      for EVERY i
EOMB (C=1,M=1,S=0, destOmd)
```

So the loss is on the receive path, not in what we put on the bus.

**Question:** did you observe the truncation (a) at the ACCP's octobus receive FIFO / driver, or
(b) at the fabric delivery into your station's `HandleFrame`? If (b), tell us the destination
station and OMD and we will chase it on the fabric side - our `OctobusFabric.SendFrame` delivers
one frame per call and does not batch, so a drop there would be ours.

Useful detail if you have it: how many DATA frames does your side actually see between SOMB and
EOMB? That single number separates "we sent one" from "you kept one".

## 2. Command 3 / the CPU model - what should an emulated ACCP return, and where from?

You say an emulated ACCP **must** answer AIB command 3 or the CPU never announces itself, and that
digit **8** (ND-5800) is the only value that works while the signature matrix reads zero.

Our station already has a CPU type/model concept for micro command 3 (chapter 5.3.7).

**Questions:**
- Is `0x5800` what we should return today, or is that only correct until the sixteen `0x220000`
  reads are modelled?
- We have a standing rule here: never hardcode a value the real hardware learns at runtime. Since
  the model is *configured in the MFbus controller*, is there any path by which our side can
  derive it, or is a constant genuinely the honest answer until the MFbus controller exists?

## 3. MFbus controllers at stations 2-7 - blocking or not?

Our standalone ACCP prints `MFbus controller not found at Octobus stations 2-7`. We have no
documentation for these and expect to have to carve them.

**Questions:**
- For the ND-5000 bring-up path we exercise (SINTRAN -> ACCP -> microcode -> swapper), is a
  responding MFbus controller required for anything **other** than supplying the CPU model?
- Does the monitor tolerate silence from 2-7, or does the probe have a timeout that costs real
  time or triggers a fallback?
- Do you have ANY document, part number or trace for the MFbus controller? We could not locate one
  and would rather carve from a hint than from nothing.

## 4. Your open item 7 - `0x900001` returning station 1

You suspect it is the WOI/STANO value the MFbus controller writes during crate configuration, and
that a correct model writes it before the ACCP boots.

**Question:** if that is right, the discovery scan never runs at all in a correct system. Does that
change what you want from us - i.e. should our fabric present a station number in the 20-77 octal
range for the ACCP's local node, and if so which one?

## 5. AFLAG bits 7 and 8

You flag these as never re-verified after the off-by-one correction.

**Question:** do you need them for anything current? From our side they only matter if we run the
real microcode through data/instruction faults, which we do not do yet. If nobody needs them, we
would rather leave them marked OPEN than have someone "tidy" them into a guess.

## 6. One thing we can give you back

If you want the reverse direction verified, our harness can now record every ACCP command with
whether it was answered (`AccpCommandLog`, `AccpUnansweredCount`), plus a snapshot at the moment
emergency 244B TERMINATE arrives. That is how we established 244B is a NORMAL bring-up step and not
a timeout. Say the word and we will hand over the trace format or run a capture for you.
