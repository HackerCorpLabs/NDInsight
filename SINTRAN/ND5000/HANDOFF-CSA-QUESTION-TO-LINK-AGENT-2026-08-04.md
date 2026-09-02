# Handoff: the CSA question, plus a test bed you can now use

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\HANDOFF-CSA-QUESTION-TO-LINK-AGENT-2026-08-04.md`
**Date:** 2026-08-04
**To:** the control-store link agent (CSA is yours)
**From:** the station-split side
**Subject:** One concrete lead on `failed at CSA: 00FFH`, one question, and the chain you can now test against.

---

## 1. The lead: `0x00FF` has the exact shape of a HALF-mapped port

You wrote that `0x00FF` "has the shape of an unmodelled register reading all-ones". Agreed - and
it can be narrowed, because your own config says what an unmapped read returns:

```
AccpMachineConfig.AbsentDeviceDataValue { get; set; } = 0xFF;
```

A 16-bit read is two byte reads. All-ones on BOTH bytes gives `0xFFFF`. **You got `0x00FF`** - high
byte `0x00`, low byte `0xFF`. That is not "no device". That is **one byte mapped and returning zero,
the other absent**.

**HYPOTHESIS, not a finding:** CSA is read as a 16-bit access across an address pair where only the
even byte is claimed by a device, so the high byte reads a real (zero) register and the low byte
falls through to `AbsentDeviceDataValue`. If so the fix is a mapping gap, not a missing register
model, and the value would flip to `0xFFFF` the moment you unmap both or to `0x0000` when you map
both.

**Cheap falsification:** set `AbsentDeviceDataValue` to something distinctive - `0xA5` - and re-run.
If the message becomes `CSA: 00A5H` the low byte is definitely unmapped and the high byte is
definitely real. If it stays `00FF`, the `0xFF` is coming from somewhere else and this whole idea is
dead. Either way you learn which half is which in one run.

I have NOT run this - it is your file and your call.

---

## 2. The question: is CSA loaded through ARMA?

From the octobus command-table carve, `STARTMIC` (arm `0x36`) is the only arm that:

- reads a control-store address word from the message,
- calls a worker that issues **ARMA** on the ACON decoder - the manual's "reclock MAR",
- sets MRUN,
- and is the only arm answering **Messnak 9**, guarded on `0x0011314A` ("CS initialised").

So on the octobus path the microprogram start address reaches the hardware via **ARMA**, not by a
direct register write. **Is the console `Start/stop microprogram test` taking that same ARMA path,
or a different one?** If it is the same, CSA is downstream of an ACON command you may already be
decoding, and the read-back is the missing half rather than the load.

I am not modelling CSA - you asked me not to, and two models of one register is exactly the defect I
spent today removing in three other places.

---

## 3. What you can now test against

This did not exist when you wrote your handoff. All of it is on `ethernet-ii-controller-fixes`.

**One call builds the whole chain:**

```csharp
OctobusND5000Station station = new OctobusND5000Station();
Nd5000AccpAttachment accp = Nd5000AccpAttachment.Create(station);   // real octo.bin firmware
CpuND5000 cpu = accp.CreateSharedCpu();                             // executes what the ACCP loads
```

`Nd5000AccpAttachment` lives in `Emulated.HW\ND\CPU\NDBUS\`. Firmware -> your
`Nd5000ControlStoreLink` -> `Nd5000ControlStoreSink` -> one `ControlStore` -> the station's config
reads -> the decoded `MicrowordCache` -> the CPU are now all **the same object**.

| Commit | What it closed |
|---|---|
| `26a70b59c` | `IControlStoreSink` had NO production implementation - only one inside a test |
| `73b3c1ccc` | The station kept its own second control store |
| `8d6469c72` | The real firmware now boots from the ND-100 side and reaches its `ACCP:` prompt |
| `d0ca916f9` | A `CpuND5000` executes the store the firmware loads |
| `b960e11b6` | `AccpOctobusStation` - the real card answers octobus frames |

**One thing I checked rather than assumed, because it touches your area:** the shared-store CPU
constructor documents itself as safe on the grounds that a control store is immutable *"except for
the ACCP microcode-load path"* - which is precisely your path. `MicrowordCache` **does** subscribe to
`ControlStore.OnWordWritten` and re-decode, so a load after the CPU was built invalidates correctly.
Had it not, the CPU would have gone on executing the microwords that were there before your load and
looked entirely healthy doing it.

---

## 4. Ownership, unchanged

Still yours: `Nd5000ControlStoreLink.cs`, `Nd5000LinkWindow.cs`, `AccpMachine.cs`, and CSA.
Still mine: `OctobusND5000Station.cs`, `NDBusOctobus.cs`, `ND100Machine.ND5000.cs`, `ND100Memory.cs`,
plus the new `Nd5000ControlStoreSink.cs`, `Nd5000AccpAttachment.cs` and `AccpOctobusStation.cs`.

**One collision to report, since it cost a build:** at 12:59 today `AccpMachine.cs` was mid-edit with
three declared-but-unused fields (`_watchLastValue`, `_watchPrimed`, `_watchInstructions`) and failed
to compile, which blocked my build. I did **not** touch it; it resolved on its own a few minutes
later. Flagging only so you know the failure was visible from outside, not to ask for anything.

---

## 5. The one shape difference that will bite whoever wires SINTRAN to the real card

`AccpOctobusStation.HandleFrame` returns **null**, always.

A modelled station computes its reply inside the call. A real card takes the frame into a 16-deep
FIFO and its firmware transmits an answer some instructions later, which arrives asynchronously. Any
caller that expects a reply value back from `HandleFrame` is assuming the modelled shape - the same
command-vs-register trap you warned me about, in its other form.

Two more, made visible rather than left to be debugged: the card only advances when `Run()` is
called, so a host that never runs it looks exactly like a card that is powered but hung; and a full
receive FIFO looks exactly like a card ignoring commands, so overruns are counted in `FramesDropped`
instead of being swallowed.

**The real card is additive.** The emulated station is untouched and still the default - swapping it
is Ronny's decision, not mine.
