# HANDOFF - implement the ACCP machine in RetroCore

**Goal**: an emulated Samson ACCP card (ND-324716 / PCB 5616) in RetroCore that boots the real
firmware, prints its banner on an emulated console, and becomes a unit-test harness that uses the
firmware's own selftest output as the oracle.

**Companion document - READ IT FIRST**:
`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-324716-FIRMWARE-RE-2026-07-27.md`
Every hardware fact below is derived there, with the evidence. This file is only the build plan.

**Firmware image (of record)**:
`E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
131072 bytes. SHA256 `0EA81716AD81984B64675E9A8CCEB6C1909AB299BE0048857C58F85C3479C5F1`.
Load it from this path; do not copy it into the RetroCore tree.

**Repo**: `E:\Dev\Repos\Ronny\RetroCore`

---

## 0a. UPDATE 2026-07-28 - the firmware is now FULLY reverse engineered

This handoff was written while large parts of the image were still undisassembled. That is no
longer true: **every one of the 279 functions is named, all 43 console commands are decoded
with their handler addresses, and the octobus registers are proven.** Build against the table
below rather than anything softer earlier in this file.

### The device set you must model - and it is smaller than expected

| Address | Dir | Width | Role | Confidence |
|---|---|---|---|---|
| `0x110000-0x117FFF` | rw | - | **SRAM 32 KB**. Stack 0x110000-0x111FFF (SP = 0x112000, grows down); globals from 0x112000. A6 = 0x110000 is the PLANC global base | PROVEN |
| `0xDD0000 + 2N + 1` | rw | byte | **SCN2681 DUART**, register N on ODD bytes. Channel A = console | PROVEN |
| `0x770004` | w | word | **OCTObus transmit data** | PROVEN |
| `0x770007` | r | byte | bit 3 = **transmit ready** | PROVEN |
| `0x880000` | r | word | **OCTObus receive data (FIFO)** | PROVEN |
| `0x660001` | r | byte | bit 1 = AOB busy, **bit 2 = octobus RX available**, bit 4 = MF-bus complete | PROVEN |
| `0x660000` | r | byte | bit 0 = control-store op OK; bits 3, 5 also tested | carved |
| `0x220000` | w | word | **general command/function port** - the code selects the target of the data pair: `0x300F`/`0x400A`/`0x400C`/`0x000F` MF-bus, `0x0005` AOB, `0x0018` control store | carved |
| `0x440000` / `0x550000` | rw | word | 32-bit data pair, low / high | carved |
| `0x330000` / `0x330001` | w | byte | **write-only latches**. 0x330000 bit 6 = write strobe, bit 2 = control-store gate. **Never read back** - the firmware keeps RAM shadows at `0x1144EE`/`0x1144EF`, so your read value for these two is irrelevant | carved |
| **`0x900001`** | r | byte | **THE ACCP'S OWN STATION NUMBER**, low 5 bits (`and.b #0x1F`). Read at 0x122E before the MFbus scan. **Must not be 0** - station 0 is illegal on the OCTObus | **PROVEN 2026-07-28** |
| `0xAA0000`, `0xBB0000`, `0x900007` | - | - | real but role unknown | unknown |

**`0x900007` breaks the replicated-nibble rule.** Do not build an address decoder that assumes
`0xNN0000` with NN a repeated nibble - that is a strong tendency, not a law.

### Two behaviours you MUST reproduce exactly

1. **Neither octobus ready-poll has a timeout.** `OctobusTransmitWord` @0x7890 and
   `OctobusReceiveWord` @0x786C are unbounded `beq.b -10` spins. If your model never raises
   `0x770007` bit 3 or `0x660001` bit 2, **the ACCP hangs** - exactly as the real card would.
   The `"K I C K   T I M E O U T"` message comes from a caller, not from these.
2. **The OBCON driver is software, not registers.** `ObconRequestDispatch` @0xF686 has 17
   function codes, and none of the four handlers examined touches hardware at all. Model the
   two raw primitives plus the IRQ3 (0x0510) and IRQ7 (0x0826) paths; the dispatcher then runs
   as ordinary code on top. You do not need to emulate a 17-function driver.

### Free test oracles the firmware hands you

- **The RAM walk-test at reset** (0x0BD6) validates CPU + ROM + RAM with no chip present.
  Assert `g_ramTestErrors_firstHalf` (0x11312A, **32-bit**), `_secondHalf` (0x11312E, 32-bit)
  and `g_ramTestDone` (0x113132, **16-bit**) - note the widths.
- **`TRACE-COMMUNICATION-DATA Y`** (command 0x3C) sets `g_traceOctobusKicks` (0x1143B4), after
  which the IRQ3 handler prints every kick with `" from SAMSON"` / `" to SAMSON"`. **The
  firmware narrates its own octobus traffic** - the cheapest possible cross-check.
- **The selftest suite** prints per-test pass/fail text; each test is a named function
  (`Selftest_*`) so you can aim at one at a time.
- **`READ-ACCP-STATUS`, `CHECK-ALIVE`, `SHOW-REGISTERS`** are cheap liveness commands.

### Where the details live

- Command set and dispatch: `ACCP-CONSOLE-COMMAND-SET-AND-DISPATCH-2026-07-27.md`
- Full hardware sweep: `ACCP-HARDWARE-ADDRESS-MAP-2026-07-27.md`
- OCTObus protocol + driver API: `OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`
- Everything else: `ACCP-324716-FIRMWARE-RE-2026-07-27.md`

---

## 0. Decisions already made - do not relitigate

| Decision | Choice | Why |
|---|---|---|
| DUART chip home | `Nuget\HackerCorpLabs.Emulation.Chips.Motorola\src\DUART\SCN2681\` | MAME shares one core (`duart_base_device`) between `scn2681_device` and `mc68681_device`; putting it anywhere else splits or duplicates that core. Package id stays `HackerCorpLabs.Emulation.Chips.Motorola`. |
| Namespace | `HackerCorpLabs.Emulation.Chips.Motorola.DUART.SCN2681` | Namespaces mirror folders in this repo - see `src\ACIA\MC6850\MC6850ACIA.cs` -> `...Chips.Motorola.ACIA.MC6850`. |
| Serial fidelity | **Byte-level**, like the existing `MC6850ACIA` | Do NOT port MAME's `device_serial_interface` bit shifting. Characters move as bytes with a per-character delay. Sufficient for the console and for every test below. |
| Build order | Machine + memory map FIRST, DUART second | The RAM walk-test at reset validates CPU + ROM + RAM with no chip present. |

Record a note in `Chips.Motorola\TODO.md`: the SCN2681 is a Signetics/Philips part parked in the
Motorola package so it can share the DUART core with the MC68681; the split is reversible.

---

## 1. What already exists in RetroCore (verified 2026-07-27)

| Need | Where |
|---|---|
| MC68000 CPU | `Nuget\HackerCorpLabs.Emulation.CPU.MC68K`, class `MC68KCpu(bus, CpuLevel.M68000)` |
| Smallest machine template | `Nuget\HackerCorpLabs.Emulation.Machines.Generic68K\src\Generic68KMachine.cs` - copy its shape |
| Full machine example (ROM + many devices) | `Nuget\HackerCorpLabs.Emulation.Machines.MacIIci\src\MacIIciMachine.cs` |
| Package scaffold | `Nuget\_template\` (src, tests, benchmarks, Directory.Build.props, global.json, nuget.config, .sln, .github\workflows\docs.yml) |
| Serial-chip pattern | `Chips.Motorola\src\ACIA\MC6850\MC6850ACIA.cs` - `class MC6850ACIA : ChipBase, IMemoryDevice` |
| Bus builder | `Nuget\HackerCorpLabs.Emulation.SystemBus\src\MemoryBuilder.cs` |
| MAME source | `E:\Dev\Emulators\mame\src\devices\machine\mc68681.cpp` (47 KB) + `.h`; `scn2681_device` at `mc68681.h:199` |

~~`MemoryBuilder` public surface is exactly: `Ram(start,end,name,useFastPath)`,
`Ram(start,end,byte[],name)`, `AlwaysVisibleIo(device,start,end)`, `Device(IMemoryDevice,name)`,
`Mirror(mirrorMask)`.~~ **WRONG - corrected 2026-07-27.** That list came from a case-sensitive
search that missed the all-caps methods. The real surface also includes `ROM(start,end,name)`,
`ROM(name)`, `Banking`, `BankedRegion`, `RelocatableRegion`, `GatedIoRegion`, `Space`, `Chip<T>`,
`IO()` and `Build()`. See §1.2.

### 1.1 The odd-byte DUART decode is already solved

`GappedAddressDecoder(inner, start, end, shift, mask)` does exactly what the ACCP needs. The
SCN2681 sits at register N = `0xDD0000 + 2N + 1`, so `shift: 1, mask: 0xF`. **Write no custom
wrapper.**

**Arithmetic verified 2026-07-27** against `Machines.MacIIci\src\GappedAddressDecoder.cs:66`, which
computes `reg = ((address - _start) >> _shift) & _mask`. For the ACCP: `address - 0xDD0000 = 2N+1`,
`>> 1 = N`, `& 0xF = N`. Correct for all 16 registers, with `end = 0xDD001F` (register 15 lives at
`0xDD0000 + 31`).

Two caveats, neither a blocker:

- The decoder is **byte-only** (it implements just `ReadByte`/`WriteByte`). That matches the
  firmware, which touches the DUART exclusively with `move.b`. If the bus splits a word access into
  two byte calls, an even+odd pair would hit register N twice rather than N and N+1 - harmless here
  because no word access to `0xDD....` exists in the firmware, but worth a comment in the machine.
- Even addresses (`0xDD0000 + 2N`) alias onto the same register N rather than reading as
  unmapped/bus-error, because the shift discards bit 0. Real hardware wires only the odd byte lane.
  Again harmless for this firmware; do not "fix" it without evidence.

**BLOCKER-ISH / ask Ronny before coding**: `GappedAddressDecoder` currently exists TWICE -
`Machines.MacIIci\src\GappedAddressDecoder.cs` and an inline copy in
`Machines.MacClassic\src\MacClassicMachine.cs:1251`, both `internal sealed`. Per the project's
no-code-duplication rule, **promote one copy into `HackerCorpLabs.Emulation.SystemBus` and repoint
MacIIci + MacClassic at it**, rather than creating a third copy.

**RESOLVED 2026-07-28 - DONE.** Ronny ruled: move it into `HackerCorpLabs.Emulation.SystemBus`.
Implemented:

- New `public sealed class GappedAddressDecoder` at
  `Nuget\HackerCorpLabs.Emulation.SystemBus\src\GappedAddressDecoder.cs`
  (namespace `HackerCorpLabs.Emulation.SystemBus`).
- Deleted `Nuget\HackerCorpLabs.Emulation.Machines.MacIIci\src\GappedAddressDecoder.cs`.
- Removed the inline copy from `MacClassicMachine.cs` (a plain comment records the move).
- Both machines already had `using HackerCorpLabs.Emulation.SystemBus;`, so no call site changed.

So the ACCP just uses it: `new GappedAddressDecoder(duart, 0xDD0000, 0xDD001F, shift: 1, mask: 0xF)`.

**PERFORMANCE TRAP found while validating this - read before touching that class.**
The original copies re-read `_inner.StartAddress` - an **interface property call** - on *every*
`ReadByte`/`WriteByte`. That is a per-bus-access interface dispatch on a path the Mac ROM hits
hundreds of millions of times during boot.

Consolidating without addressing it made the MacIIci ADB boot test ~18% slower
(2m02s -> 2m24s standalone). That test carries `[Timeout(300_000)]`, and **NUnit's timeout kills the
entire test host**, so under full-suite load the run did not fail one test - it aborted the whole
suite after 39 tests. Baseline was clean 2/2; the regression reproduced 3/3.

Fix: cache the inner base address in a `readonly uint _innerBase` field in the constructor. That
restored parity (2m06s standalone) and the full suite is green again. The cache is valid only
because every device wrapped by this type has a construction-fixed `StartAddress`; a movable-base
device must not be wrapped by it. `[MethodImpl(AggressiveInlining)]` on the two accessors helped
slightly but was **not** sufficient on its own - the interface property call was the real cost.

Validation (all after the fix):

| Suite | Result |
|---|---|
| `HackerCorpLabs.Emulation.SystemBus.Tests` | 61/61 pass |
| `HackerCorpLabs.Emulation.Machines.MacClassic.Tests` | 39/39 pass |
| `HackerCorpLabs.Emulation.Machines.MacIIci.Tests` | 56/56 pass, twice |

Lesson worth carrying: a long-running emulation test with an NUnit `[Timeout]` turns a modest
performance regression into a whole-suite abort with no useful error message. When a test host
"crashes" after a refactor, measure the hot path before assuming a correctness bug.

### 1.2 RESOLVED 2026-07-27 - there is no ROM problem

**This section previously claimed "`MemoryBuilder` has no `Rom(...)` method" and sent the
implementer chasing `RomManager` and the MacIIci post-Build overlay. That premise was wrong.**

Root cause of the error: the method is spelled **`ROM`**, all caps, not `Rom`. A case-sensitive
search for `Rom(` misses it, and the "public surface is exactly Ram / AlwaysVisibleIo / Device /
Mirror" list above was built from that bad search. That list is incomplete - `MemoryBuilder` also
exposes `ROM`, `Banking`, `BankedRegion`, `RelocatableRegion`, `GatedIoRegion`, `Space`, `Chip`,
`IO`, `Build`.

`MemoryBuilder.ROM(start, end, name)` (`SystemBus\src\MemoryBuilder.cs:53`) returns a `ROMBuilder`
(`SystemBus\src\ROMBuilder.cs`), which you finish with `.LoadFile(path)` or `.LoadArray(bytes)`.
Both funnel into `TrackFastMemory(..., isReadOnly: true, ...)` - candidate 1 in the old list, just
reached through the fluent API instead of by hand.

So the ACCP ROM declaration is simply:

```csharp
// 128 KB EPROM at 0x000000. LoadFile logs a warning and 0xFF-fills if the
// path is missing, so a wrong path shows up as a bus of 0xFF rather than a
// silent zero-fill - which the 68000 would take as SSP/PC = 0.
builder.Memory
    .ROM(0x000000, 0x01FFFF, "ACCP EPROM")
    .LoadFile(@"E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin")
    .Ram(0x110000, 0x117FFF, "ACCP SRAM");
```

**No overlay is needed.** `TrackFastMemory` regions are installed into the `SystemBus` during
`MachineBuilder.Build()`, i.e. before the machine's first `Reset()`, so the 68000's SSP/PC fetch
from `0x000000`/`0x000004` sees real ROM. The MacIIci overlay dance exists because that machine
must *toggle* ROM-at-zero off later; the ACCP never does.

Two gotchas worth knowing:

- `LoadFile` **does not throw** on a missing file - it logs a warning and installs a 0xFF-filled
  region. `0xFFFFFFFF` as SSP and PC will fault in a confusing way, so the Phase 2 test should
  assert the reset vectors read back correctly before it asserts anything else.
- If the file size does not match the region, `LoadFile` resizes with 0xFF padding and warns. For
  `octo.bin` the sizes match exactly (131072 = 0x20000), so neither path should trigger.

---

## 2. The ACCP memory map - implement exactly this

All values proven from the firmware; see the companion doc for the evidence.

**The chip-select decode is nibble-replicated**: every peripheral sits at `0xNN0000` with NN a
repeated nibble (0x11 SRAM, 0x22/0x33/0x44/0x55/0x66/0x88/0xBB peripherals, 0xDD DUART). 0x44 and
0x55 were only found once 0x70CC was hand-disassembled - **assume further selects exist until
proven otherwise**, and make the stub installer cover the whole `0xNN0000` family rather than a
hand-listed set, so an unmodelled select shows up in the log instead of as a bus error.

| Range | Contents | Status |
|---|---|---|
| `0x000000-0x01FFFF` | ROM, 128 KB, `octo.bin` | Base address is an INFERENCE (standard 68000 arrangement, vectors point at low offsets). No schematic seen. |
| `0x110000-0x117FFF` | SRAM, 32 KB (4x 8192x8), two 16 KB halves | **PROVEN** - the reset routine walk-tests both halves and zeroes them |
| `0x00DD0000` + odd bytes | SCN2681 DUART, register N at `+2N+1` | **PROVEN** from SRA/THRA/SRB/THRB |
| `0x220000` | word - MF-bus COMMAND / PARAMETER port. High nibble = function, low byte = value. Seen `0x300F` open, `0x400A`/`0x400C` sub-function, `0x000F` strobe. | **CARVED** at 0x70CC (MF-bus, not octobus - its timeout branch loads `"$MF-bus memory timeout$"`). Function-code *meanings* are INFERENCE; read ND-14001 ch. 4 before naming them. |
| `0x440000` | word - MF-bus DATA, LOW half of a 32-bit value | **CARVED** |
| `0x550000` | word - MF-bus DATA, HIGH half (`swap D0` between the two writes) | **CARVED** |
| `0x660001` | byte - MF-bus STATUS, **bit 4 = transaction complete**, polled with a software countdown | **CARVED** |
| `0x330000` | byte - command port. Seen `0xF0` (master clear) and `0xD8` (send). | **CARVED** in the IRQ3 handler |
| `0x330001` | byte - **write-only control latch**, RAM shadow at `0x001144EF`; bit 1 pulsed | **CARVED** - never read back by the firmware, so a stub returning anything is fine, but the shadow must be modelled if you ever want to know the latch state |
| `0x880000` | word - **message / kick read port**. IRQ3 reads one word as the interrupt cause; IRQ7 drains it in a loop while `0x660001` bit 2 is set. | **CARVED** - it is both a cause register and FIFO-like, depending on the path |
| `0x770004` / `0x770007` | word data-in (from `0x440000`) + status byte, bits 3/4 handshake, retry count 10 | **NEWLY FOUND 2026-07-27** - was in no previous list |
| `0x660000` | byte status. **bit 0 = control-store operation OK** (0x744E). bit 3 tested (0x06A4), bit 5 tested (0x082E); whole byte snapshotted to `0x1143BA` | **bit 0 CARVED**, rest open |
| `0x900007` | byte, snapshotted to `0x1143B8` at interrupt time | **CONFIRMED REAL** - `0x90` is not a repeated nibble, so the nibble rule is a tendency, not a law |
| `0xBB0000` | word, written `0` in the IRQ7 path right before the restart | UNIDENTIFIED |

`0x660001` is now known to carry **three** unrelated bits: bit 1 = AOB busy, bit 2 = message
available at `0x880000`, bit 4 = MF-bus transaction complete. A stub must be able to drive them
independently.

**`0x220000` is a general command port, not "the MF-bus port".** The IRQ3 AOB path writes
`move.w #0x0005,(0x00220000)` and uses the same `0x440000` data port. The function code selects the
target: `0x300F/0x400A/0x400C/0x000F` = MF-bus memory, `0x0005` = AOB. Likewise `0x660001` is a
shared status byte - **bit 1 = AOB busy, bit 4 = MF-bus complete**. Model these as one command/status
block with per-function meaning, not as two separate devices.

**`0x220000` is BOTH a command port AND a shift clock - this is the key fact for Phase 6.**
A "bit-banged serial port" reading was floated, retracted, and then resolved properly (companion
§2.4h -> §2.4j -> §2.4n). Final position, all verified:

- **As a command port**: `0x71F8` writes four discrete command words and reads 32-bit results back
  from `0x440000`/`0x550000`. Definitely a parallel register interface.
- **As a clock**: three routines (`0x76E6`, `0x7776`, `0x77B6`) emit the pair `0x0010` / `0x000F`
  in tight loops to shift data. **Write vs read is distinguished purely by the phase order** of that
  pair - `0x0010` then `0x000F` shifts out, `0x000F` then `0x0010` shifts in.

**An emulator must tell "command word" from "clock edge" by context.** That makes this the value
table to work from: `0x0001`, `0x0005`, `0x0007`, `0x000F`*, `0x0010`*, `0x0015`, `0x0017`,
`0x2010`, `0x2011`, `0x2018`, `0x300F`, `0x3010`, `0x400A`, `0x400C`, `0x4016`, `0x8013`
(* = clock constants, not commands).

**`0x001144F0` is a 16-byte (128-bit) microword buffer** - exactly the ND-5000 microword width.
`0x7776` shifts it out to the control store; `0x77B6` shifts it back in. **`0x775A` - the shared
exit path of both control-store paths - is a read-back VERIFY**: it issues `0x2010` and shifts the
128 bits back into that buffer after every access. With stubs returning zeros the verify always
mismatches, which is the correct "no ND-5000 present" behaviour, and `0x001144F0` is a good place
for a test to inspect.

**`0x440000` / `0x550000` are a bidirectional 32-bit data pair** (low / high, `swap D0` between).
`0x71F8` writes both, issues four commands, then reads both back - so they are real readable
registers. A stub must return what was written, or the firmware's read-back path yields garbage.

**Two transaction gates and one strobe** - these are the bits an emulator has to honour:

| Bit | Behaviour |
|---|---|
| `0x330001` bit 6 | cleared for the duration of a 32-bit transaction, restored after |
| `0x330000` bit 0 | set for the duration of a 32-bit transaction, cleared after |
| `0x330000` bit 6 | **write strobe** for the AOB single-word path: set, whole byte written, then cleared *in the shadow only* - there is no explicit falling-edge write. Treat the write-with-bit-6-set as the commit edge for the word in `0x440000`. |

The AOB write path (`0x72A0`) spins on `0x660001` bit 1 until the AOB is free (unless `0x113138` is
non-zero), then runs the strobe at `SR = 0x2700` - masked against IRQ3/7. A stub that never clears
`0x660001` bit 1 will hang the firmware here rather than time out.

**Neither `0x33` byte is ever read back.** Both are write-only with RAM shadows -
**`0x001144EE` shadows `0x330000`**, `0x001144EF` shadows `0x330001`. So a stub's *read* value for
those two addresses is irrelevant, and a test that wants the latch state should read the shadows in
RAM, not the device.

**The card can be reset remotely over the octobus.** A kick matching mask `0xC0FF` = `0xC0FF` with
bits 13-8 equal to the guard word at `0x001143A0` makes the firmware pulse `0x330001`, write `0xF0`
to `0x330000`, spin 10000 iterations, and `jmp 0x00000C72` - re-entering init *after* the RAM test.
A stub that returns arbitrary values on `0x880000` can trip this by accident and silently restart
the machine mid-test. **Have the Phase 2 stub return 0 for `0x880000`, and assert the machine never
re-enters `0x0C72`.**

There are in fact **three** paths that end in `jmp 0x00000C72` (restart-without-RAM-test): the IRQ3
remote master clear, the IRQ7/NMI path (drain `0x880000`, clear `0xBB0000`, restart), and the
`0x660000` bit-5 branch at `0x082E`. A stub that returns `0xFF` for status bytes will hit these
immediately. **Default every stub read to `0`, not `0xFF`** - and make the "never re-enters
`0x0C72`" assertion a shared precondition of all Phase 2 tests, not just one.

Related: `0x00113146` and `0x00113136` are set to `1` on error paths (0x0780, 0x07DC) - candidate
extra oracles once their meaning is known.

**Correction to an earlier version of this document**: `0x220000` was listed as "written `1` on
entry to BOTH the IRQ6 and IRQ7 handlers", implying an interrupt-acknowledge register. That write
exists, but the select is the MF-bus command port - do not model it as an interrupt register.

A 32-bit MF-bus datum must move as a low/high word pair, which is exactly what the banner line
`Only 32-bit Word accesses available from ACCP to MF-bus!` is telling the operator. The canonical
sequence (from 0x70D0) is: three command writes (`0x300F`, `0x400A`, `0x000F`), data low to
`0x440000`, `swap D0`, data high to `0x550000`, three more command writes (`0x300F`, `0x400C`,
`0x000F`), then `btst #4,(0x00660001)`. A stub that never sets bit 4 of `0x660001` will make the
firmware print the timeout - which is a perfectly good Phase 2 assertion, and later the hook for a
real MF-bus model.

The remaining UNIDENTIFIED ranges are the **NDOBCON / OCTC octobus controller LSI** and the BADAP.
Do NOT guess their semantics. Install a **logging stub device** for each: record
(address, size, direction, value, CPU PC) and return a configurable fixed value. The stubs are the
instrument that identifies the chip - see section 5.

Reset values that fall out of ROM automatically: SSP `0x00113FFC`, PC `0x00000BD6`.

---

## 3. Package layout to create

**RULING FROM RONNY (2026-07-27) - not negotiable**: the whole ACCP machine is a **NuGet machine
package**. Put **nothing** in the legacy `Emulated.*` namespaces or projects, and wire the machine
up with **`MachineBuilder`** (the `Machines.Generic68K` / `Machines.MacIIci` shape), not by hand.

```
Nuget\HackerCorpLabs.Emulation.Machines.Accp\
  Directory.Build.props   LICENSE   README.md   TODO.md
  .github\   docfx\
  src\  HackerCorpLabs.Emulation.Machines.Accp.csproj
        AccpMachine.cs
        AccpMachineConfig.cs
        Devices\AccpLoggingStub.cs        <- the UNIDENTIFIED-range recorder
  tests\ HackerCorpLabs.Emulation.Machines.Accp.Tests.csproj
         AccpBootTests.cs
```

Copy the `.csproj` shape from
`Machines.Generic68K\src\HackerCorpLabs.Emulation.Machines.Generic68K.csproj` - it already lists the
right project references (Abstractions, Common, SystemBus, CPU.Base, CPU.MC68K, Machines.Base,
Debugger.Abstractions). Add `Chips.Motorola` once the DUART lands.

Machine attribute:

```csharp
[Machine(
    FolderName  = "Accp",
    WindowTitle = "Norsk Data Samson ACCess Processor (ND-324716)",
    Description = "ND-5000 access processor and octobus controller. MC68000, 128 KB EPROM, 32 KB SRAM, SCN2681 DUART console.",
    Id          = "accp",
    DisplayName = "ND ACCP (Samson)",
    Family      = "ND-5000",
    Vendor      = "Norsk Data",
    Tags        = new[] { "nd", "norsk-data", "nd5000", "samson", "accp", "octobus", "mc68000" })]
```

Also add the machine to `Nuget\Tools\Sdl2CliDemo\Sdl2CliDemo.csproj` so `machine.start accp` works
(see skill `cli-attach-machine`).

---

## 4. Phased build - each phase ends with a green build AND green tests

Project rules that apply throughout: **no LINQ, no `foreach`, no FluentAssertions**, prefer `Span` /
`ArrayPool`, keep and add as many comments as possible, run `dotnet format` if whitespace complains,
and **never report success without actually running the tests**.

### 2026-07-28 - THE CARD IS INTERACTIVE ✅ `ACCP:` PROMPT REACHED

The firmware boots, runs its full selftest suite, prints `ACCP:` and **answers typed commands**:

```
ACCP: HELP
Command:
```

**28/28 ACCP tests, 15/15 NDOBCON, 179/179 Chips.Motorola.**

Three things had to be right, and two of them were wrong first:

1. **`NDOBCON`** (`Nuget\HackerCorpLabs.Emulation.Chips.NorskData\src\OBCON\NDOBCON.cs`) — the
   OCTObus adapter gate array. Transmit is FAKED (frames counted, recorded, dropped; transmitter
   always ready) because the firmware's TX poll has no timeout. Requests complete after a
   configurable delay via `Tick()` + `AutoReplyDelayTicks`; `ReplyBuilder` is the seam for a real
   peer. Without the auto-reply the boot stalls in a software poll at 0x6C42 waiting on two OBCON
   request-block status words and never reaches the prompt.

2. **THE DUART INTERRUPT IS ON IRQ5, AND WIRING IT IS NOT OPTIONAL.** Proven: vector 29 at ROM
   `0x074` -> `0x796` -> `0x1E0C`, which writes 0 to `0xDD000B` (IMR), reads it back (register 5 =
   ISR) and tests bits 1/5/0/4 = RxRDY A/B, TxRDY A/B. **The firmware NEVER polls the receiver** —
   it reads characters only from that handler. With the interrupt unconnected the banner and prompt
   print perfectly and every keystroke is silently discarded. That failure mode looks exactly like a
   working machine, so `Prompt_EchoesAndAnswersTypedCommand` exists as its gate.

3. **A live status port**, not a static stub, for `0x660000`/`0x660001` — the receive-available bit
   has to track the real FIFO or an auto-reply is never collected.

**Method warning worth carrying**: "run until the console stops growing, then type" is a BAD way to
detect the prompt. The selftest suite has natural gaps, so it fires mid-suite, pokes a busy machine
and reports it dead. That produced two confidently wrong "the console does not respond" findings
before it was caught. Wait for the `ACCP:` marker instead.

### Phases 1 and 3 - COMPLETE 2026-07-28 ✅ THE CARD BOOTS

**Phase 1 — SCN2681 DUART.** `Nuget\HackerCorpLabs.Emulation.Chips.Motorola\src\DUART\SCN2681\`
(`SCN2681Duart.cs`, `SCN2681Registers.cs`), byte-level serial, semantics cross-checked against
MAME `mc68681.cpp`. **29 new tests; 179/179 pass in the package.** The centrepiece replays the
firmware's real `DuartInit` @0x162E register for register and asserts 9600 7E2 on channel A,
9600 8N1 on channel B, IMR 0x22, counter preload 0x9000 and the counter stopped.

**Phase 3 — console.** DUART attached at `0xDD0000` through
`GappedAddressDecoder(shift: 1, mask: 0xF)`. **20/20 ACCP tests pass** and the real firmware
prints:

```
******   S A M S O N   A C C E S S   P R O C E S S O R   ******
ACCP local ram test OK
BUS test  failed          Result: 00000000H   Expected: 1C587698H
MIR test a failed         Expected: 7698H B027H 0AAAH 2C91H 0D8CH F58BH AFBEH 6195H
Control Store  sample test ab failed
Start/stop microprogram test abc failed at CSA: 00FFH
A,MARG D,AIB test  failed Result: 00000000H   Expected: FFFFFFFFH
Loading control store with selftests...
ALU verify test  failed   Result: 00000000H   Expected: 87654322H
```

Every selftest failing is the CORRECT result — they target the ND-5000, which is not modelled.
What matters is that the firmware runs the whole suite and reports it.

**THE KEY IMPLEMENTATION FACT — there are FOUR unbounded ready-polls, not two.** Section 0a
lists two; running it found two more. Each stops the boot at a different point, and the symptom
is always "the console just stops", never an error. See the octobus doc §5a for the table and
the measured progression (513 -> 608 -> 1769 chars of console output as each is released).

Bits that must be left LOW are as important as the ones raised: `0x660001` bit 1 (AOB busy),
`0x660000` bit 0 (**control-store OK — raising it fabricates a selftest pass**) and `0x660000`
bit 5 (restart path). And `0x660001` bit 2 held high gets the boot *less* far, not further.

Implemented as `AccpMachineConfig.Hold*` flags, all defaulting true except the receive one, with
`AccpMachine.ApplyHandshakeBits` composing the shared `0x660001` byte rather than letting the
last writer win.

**Remaining Phase 3 items need a command prompt** (`HELP` -> 43 commands,
`LOOK-AT-LOCAL-MEMORY`, `MAIN-FORMAT`). The firmware is still inside its selftest suite at the
end of the run; reaching the prompt needs either a longer budget or enough of an ND-5000 model
to let the tests pass. That is Phase 5/6 work.

### Phase 2 - COMPLETE 2026-07-28 ✅

Built and validated. **10/10 tests pass** in
`Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpBootTests.cs`.

Files created:

```
Nuget\HackerCorpLabs.Emulation.Machines.Accp\
  Directory.Build.props
  src\HackerCorpLabs.Emulation.Machines.Accp.csproj
  src\AccpMachine.cs
  src\AccpMachineConfig.cs
  src\Devices\AccpLoggingStub.cs
  tests\HackerCorpLabs.Emulation.Machines.Accp.Tests.csproj
  tests\AccpBootTests.cs
```

Verified at instruction level on the real `octo.bin`:

- Reset vectors read back SSP `0x00113FFC` / PC `0x00000BD6`, and the CPU actually takes them.
- The RAM walk-test completes with **both error counts zero** and the flag set - i.e. the MC68K
  core, the ROM mapping at `0x000000`, the reset-vector fetch and the 32 KB SRAM at `0x110000` are
  all correct.
- No 68000 exception fires (`0x00113112` stays 0).
- The card does not take any of the three restart paths.
- SRAM round-trips through the bus; ROM reads the image and correctly ignores writes.

**The boot runs far past init unaided** - with only stubs present the firmware reaches the
control-store shift loop at `0x77A6`. First census, to ~8M instructions:

| Select | Accesses |
|---|---|
| CMD-22 | 5,781,124 |
| DATA-HI-55 | 313,442 |
| LATCH-33 | 34,844 |
| XFER-77 | 6 |
| DATA-LO-44 | 4 |
| MISC-90 | 2 |
| STATUS-66 | 1 |
| MSG-88, CTL-BB | 0 |

The `CMD-22` / `DATA-HI-55` traffic is the microword shift engine (§2.4n) spinning - expected with
no ND-5000 present. Note `MISC-90` is non-zero, which independently confirms `0x900007` is real
(§2.5).

### Phase 2 original plan (kept for reference) - machine + memory map, no DUART

Build `AccpMachine` with ROM, SRAM, and a logging stub on every non-SRAM, non-DUART `0xNN0000`
select (0x22, 0x33, 0x44, 0x55, 0x66, **0x77**, 0x88, 0xBB, plus whatever `0x900007` really is -
and be generous, see section 2: the list has grown twice already, first 0x44/0x55, then 0x77).
No chip.

**Test 1 - the RAM walk-test is the oracle.** Boot and run until the reset routine finishes, then
assert:

| Address | Width | Expected | Meaning |
|---|---|---|---|
| `0x0011312A` | 32-bit (`move.l`) | `0` | first-half RAM error count (D2) |
| `0x0011312E` | 32-bit (`move.l`) | `0` | second-half RAM error count (D3) |
| `0x00113132` | 16-bit (`move.w`) | `1` | RAM test completed flag |

All three re-verified by disassembly on 2026-07-27 (`0x0C5A` / `0x0C60` / `0x0C66`) - the widths
were previously unstated and are not uniform.

> **CRITICAL CORRECTION 2026-07-28, found by running it: `0x00113132` IS TRANSIENT.**
> The firmware sets it to 1 at `0x0C66` and something later in init clears it again. Measured on
> the real image: set at single-step **139,274**, and back to `0x0000` well before the boot settles
> (by which time the CPU is deep in the control-store shift loop at `0x77A6`).
>
> So the obvious implementation - "run the machine for a while, then assert the flag is 1" - reports
> **"the firmware never completed its RAM test" on a perfectly healthy boot**. That is exactly what
> the first version of the test did, and it cost a debugging cycle.
>
> **Read the verdict at the moment it is published**: single-step until `PC == 0x00000C72` (the
> instruction right after the three stores), then assert. That address is reached after ~139,276
> steps and is deterministic. The two error counts happen to survive, but read them at the same
> stop point anyway.
>
> This also gives the restart test its teeth for free: arriving at `0x0C72` *with the flag still
> clear* means the firmware got there via one of the three restart paths rather than by finishing
> the walk test.

Prerequisite assertion for this test: check the reset vectors read back as SSP = `0x00113FFC` and
PC = `0x00000BD6` *before* running. Both are confirmed against `octo.bin` bytes 0-7
(`00 11 3F FC 00 00 0B D6`), and `LoadFile` fails soft on a bad path, so this catches a mis-pathed
ROM immediately instead of as a baffling fault.

Note the reset SSP is **never used** - the walk test makes no subroutine call, and the zero-fill at
`0x0C4A` wipes the whole 32 KB including `0x113FFC`. The real stack is set to `0x112000` at
`0x0C7A`, after the test. Do not "fix" a test failure by preserving the boot stack.

**Better still, breakpoint `0x00000ECE`.** That is the firmware's own `tst.l (0x0011312A)` /
`bne 0x0EFC` - the branch that decides between the `ACCP local ram test OK` message and the failure
report. Asserting on which way that branch goes tests the firmware's verdict directly, without
depending on console plumbing that does not exist until Phase 3. It also independently confirms the
32-bit width of the error count, since a different routine reads it with `tst.l`.

That single test validates the MC68K core, the ROM mapping, the reset-vector fetch and the SRAM
mapping at once, with no peripheral in the way.

**Test 2 - no unexpected exception.** Assert the fault code word at `0x00113112` is untouched. Any
68000 fault writes a code there (`0x20` unused TRAP, `0x2A` TRAP #10, `0x4D` reserved vector) plus
SR/SP/PC/A6 at `0x113118`/`0x11311E`/`0x113122`/`0x113126`. If a test fails, **read that block
first** - it says exactly which exception fired and where.

**Test 3 - stub access census.** Assert the recorded stub hits match a golden list. This is how you
notice the firmware reaching for hardware you have not modelled.

### Phase 1 (second) - the SCN2681

Port the SCN2681 variant only from `mc68681.cpp`. Skip `sc28c94_device`, `mc68340_duart_device`,
`xr68c681_device`, `mcf5206e_uart_device` - most of that 47 KB is other parts.

Registers 0-0x0F: MR1/MR2 A+B (with the MR pointer auto-advance), SRA/SRB, CSRA/CSRB, CRA/CRB,
RHR/THR A+B, ACR, ISR/IMR, CTUR/CTLR, IPCR, OPCR, SET-OPR / CLR-OPR, plus the 16-bit counter/timer
and the interrupt output. Receiver FIFO is 3 deep.

Decorate per skill `retrocore-chip-cli-decoration`: `[Chip]`, `[ChipRegisterMap]` + register enum
with `[Description]`, `[ChipRegisterBits]` + `[Flags]` bit enums (use `1<<n` style, on-wire hex in
the XML summary), correct `RegisterAccess` semantics, and a `PeekPort` override so debug reads do
not consume RHR or clear status.

Tests: register read/write, RxRDY / TxRDY / TxEMT transitions, FIFO depth 3 and overrun, break
detect, counter/timer modes, ISR/IMR masking, and the reset commands in CRA/CRB.

**The firmware's own power-on programming is the best conformance test you will get.** `DuartInit`
@ `0x162E` was fully carved on 2026-07-27 (companion doc §2.2b) and touches 11 distinct registers
in a fixed order. Write one test that runs the machine to the end of `DuartInit` and asserts the
resulting chip state:

| Property | Expected after `DuartInit` |
|---|---|
| Channel A line settings | **9600 baud, 7 data bits, even parity, 2 stop bits** (MR1A=`0x02`, MR2A=`0x0F`, CSRA=`0xBB`, ACR=`0xE0` -> baud set 2, index 0xB) |
| Channel B line settings | **9600 baud, 8 data bits, no parity, 1 stop bit** (MR1B=`0x13`, MR2B=`0x07`, CSRB=`0xBB`) |
| Both channels | RX enabled and TX enabled (`CRA`/`CRB` = `0x05`) |
| IMR | `0x22` - RxRDY-A + RxRDY-B **only** |
| CTUR/CTLR | `0x90` / `0x00` (preload `0x9000`) |
| Counter | stopped (init *reads* `0xDD001F` = stop-counter command) |

Baud indices verified against MAME `mc68681.cpp:84` (`baud_rate_ACR_1[0x0B] == 9600`); register
indices against the write-path `case` labels at `mc68681.cpp:953-1025`.

**Two behaviours the model must get right or the console will silently misbehave:**

1. **MR pointer auto-advance is load-bearing.** Init writes MR1A then MR2A through the *same*
   address `0xDD0001`, and MR1B/MR2B through `0xDD0011`, relying on the pointer advancing after the
   first write and on `CRA/CRB` command 1 resetting it. Get this wrong and you get the wrong
   character length and parity - which produces plausible-looking garbage on the console instead of
   an obvious failure. Test it explicitly.
2. **TX must not raise interrupts.** IMR = `0x22` masks `INT_TXRDYA` (0x01) and `INT_TXRDYB`
   (0x10); the firmware polls SR bit 2 instead (`DuartTxServiceBothChannels` @ `0x1D4C`). A model
   that asserts IRQ on TxRDY will interrupt firmware that is not expecting it.

**Channel B is initialised AND enabled at power-on**, at 8N1 - a data setting, not a terminal
setting. That is now strong (still not conclusive) support for the ND-100-serial-link reading in
Phase 3; it also means leaving channel B unconnected must not fault.

### Phase 3 - console

Attach the DUART via `GappedAddressDecoder(duart, 0xDD0000, 0xDD001F, shift: 1, mask: 0xF)`.
Channel A to the CLI console (`IInputMachine`, skill `retrocore-keyboard-input`).
Leave channel B unconnected - it is LIKELY the ND-100 serial link (command
`SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` and the string
`Illegal kick ... received over serial line`) but that is NOT proven.

**Tests, using the firmware's own output:**

1. Banner: console receives `****** S A M S O N   A C C E S S   P R O C E S S O R ******`.
2. `ACCP local ram test OK` appears.
3. Type `HELP` and assert all **43** commands come back (list is in the companion doc, section 5).
   This one test exercises RX, the command scanner, the string engine and TX together.
4. `LOOK-AT-LOCAL-MEMORY <addr>` returns bytes that match `octo.bin` at that offset.
5. `MAIN-FORMAT` accepts `HEXADECIMAL` / `DECIMAL` / `OCTAL` and the output radix changes.

Note for the console driver: `$` (0x24) is the ND newline marker inside firmware strings; the
firmware turns it into CR LF itself (`ConsPutCrLf` at `0x1D32`), so the host sees ordinary CR LF.

### Phase 5 - carve OBCON, then Phase 6 - implement it

Covered in the companion doc, section 8. In short: read `ND-14001-1-EN DOMINO Standard Hardware
Description` chapter 4 (OBCON / OCTObus Adapter) and `ND-05.017.01` section 3.4 FIRST - a documented
register map beats a carved one - then drive `SEND-OCTOBUS`, `RECEIVE-OCTOBUS`,
`SEND-KICK-OCTOBUS`, `READ-AIB16/32`, `LOAD-AOB16/32`, `READ-ACCP-STATUS` and `TEST-BUSLOOP` against
the logging stubs and read the address map out of the trace.

**Head start, 2026-07-27**: the IRQ3 handler (`0x0510`) is already carved - see companion doc §2.4b.
It is the KICK / AOB path, named by the firmware's own strings (`K I C K   T I M E O U T :`,
`AOB not read by microprogram within timeout.`, `AOB full, previous message not read.`). "microprogram"
= the ND-5000 microcode, so **AOB = ACCP -> SAMSON**, **AIB = SAMSON -> ACCP**, which lines up with
the `LOAD-AOB` / `READ-AIB` command names.

**Do this before anything else in Phase 5**: `0x001143B4` and `0x001143B6` are trace-enable flags.
While non-zero, the firmware prints each kick value followed by ` from SAMSON` / ` to SAMSON` on the
console. Find the command that sets them - then the firmware narrates its own octobus traffic, and
you get a console-visible oracle for every stub interaction essentially for free. That is a far
better instrument than reading the stub log.

**Both flags are explicitly cleared at boot** (`clr.w` at 0x0E9A and 0x0EA8), so tracing is off by
default and there *is* a command that turns it on. **Do not go looking for it in the current Ghidra
database** - see the next paragraph.

**Prerequisite for any further firmware carving - run `PlancFixFlow` first.** Large parts of this
image (including `0x5D00-0x6882`, where the trace-flag writes live) show as undefined bytes. The
bytes are fine; Ghidra stops disassembling at every PLANC error slot (`4E D5` = `jmp (A5)`) because
it reads as a flow terminator, and PLANC puts one after *every* call. `PlancAnnotate` has been run
on `octo.bin` but `PlancFixFlow` has not. Run the ND.PLANC script set in the patched Ghidra install
(`C:\Utils\ghidraRun.bat`) before spending time here - it will likely surface more code, and
possibly more chip selects, than any amount of manual work. See companion doc §2.4f.

Also useful: the AOB busy-wait timeout counter is loaded from `0x001131DC`, so the timeout is a
tunable RAM word rather than a hard-coded constant - handy for keeping tests fast.

**`0x330001` latch model (carved 2026-07-27, companion §2.4e).** The latch is written in a
**two-phase** pattern: first an intermediate value with bits 1 and 3 forced low, then the real
value. A stub or model must accept that intermediate write without treating it as a state change.
`0x795A` clears latch bits 2+3 (disable), `0x79E4` sets them (enable) - they are a matched pair, and
`0x795A` is *not* the big "controller re-init" routine it looked like from the call sites.

**`0x001131F8` is a variant/identity word that changes behaviour**: `0x795A` clears latch bit 0 only
when it holds `0x5400` or `0x5500`, and the firmware prints it to the console at 0x10D4. If the
emulator ever needs to pick a value here, that choice is observable both on the console and in the
latch - so it must be a deliberate, documented config value, not a default of zero. What `0x5400` /
`0x5500` actually mean is NOT established.

**Naming caution**: Ronny calls the part NDOBCON. The existing repo analysis
(`SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`, line 179)
says the ND-5000-side equivalent of OBCON is the **OCTC** gate array on the ACCP, "the same OBCON
family chip". Settle which name and which datasheet applies to THIS card before assuming ND-14001
chapter 4 describes it verbatim.

---

## 4z. WHAT AN MFbus CONTROLLER MODEL MUST DO [for the octobus machine work, 2026-07-30]

A working MFbus-controller peer now exists as a test double and the exchange is fully
decoded. Anyone building the octobus side needs the following, because the ACCP will not
come up clean without it.

### The CPU model is a CONFIGURATION value held by the MFbus controller

Not a memory type, and not something the CPU reports about itself. The MFbus controller
stores "which ND-5000 model is this system", and the ACCP **cross-checks it against its own
hardware probe**. The firmware's complaint says so directly: `MFbus controller has incorrect
CPU model setting.` A second string, `Not allowed for this CPU model: ND-`, shows the model
gates other behaviour too.

Reported model, computed at 0x12F4 from reply content byte 1:

```
model = 0x5000 | (contentByte1 << 8)
```

so 2 -> ND-5200, 4 -> ND-5400, 5 -> ND-5500, 7 -> ND-5700, 8 -> ND-5800, 9 -> ND-5900.

The ACCP then derives a **class** by probing `0x00114550` for `0x7F55` at `+6`, `+0x0C`, `+4`
and requires the reported model to be in that class's set:

| Class | `0x1131F6` | Accepted models |
|---|---|---|
| 1 | 1 | `0x5200` |
| 2 | 2 | `0x5400`, `0x5500`, `0x5700` |
| 3 | 3 | `0x5800`, `0x5900` |

`0x1131FA` is the accept bit. If it stays zero, `0x120C` sets bit 15 of `0x1131E2`, and the
error is printed. **A peer cannot claim an arbitrary model.**

### What fills `0x00114550` - CARVED, implementable [2026-07-30]

`0x00114550` is **not** shared MPM memory, **not** a thumbwheel, and must **not** be written
directly by an emulator. It is a 16-word buffer in the ACCP's own local SRAM
(`0x110000`-`0x117FFF`), and the firmware **clears and rebuilds it** from the ND-5000 datapath
every time. Seeding it would be overwritten.

The builder is the routine entered at **`0x7D26`** (`link.w A6,-0x4c`). Three phases:

**Phase 1 - clear (0x7D2E..0x7D4C)**

```
for (off = 0; off <= 0x1E; off += 2)
    word16[0x114550 + off] = 0
```

**Phase 2 - sixteen sequential reads of the command port (0x7D4E..0x7D74)**

```
for (w = 0; w < 16; w++)
    read[w] = read_word16(0x00220000)     // sixteen separate reads, same address
```

**Phase 3 - 16x16 bit transpose (0x7D76..0x7DCE)**

```
for (bit = 0; bit < 16; bit++)
    for (w = 0; w < 16; w++)
        matrix[bit] bit w  =  read[w] bit bit
```

Verified from `0x7D7E` (`D2 = read[w]`, `btst bit,D2`) and `0x7DA2`
(`A0 = 0x114550 + bit*2`, `bclr/bset w`). So **`matrix[b]` is the b-th bit of every read,
gathered into one word**, and matrix word index `s` is byte offset `s*2`.

### Consequence: exactly what the reads must contain

Both consumers index the SAME matrix, so their requirements are computable rather than
guessable.

**CPU-model class** (`0x110A`) tests `matrix[3]`, `matrix[6]`, `matrix[2]` - byte offsets
`+6`, `+0x0C`, `+4` - against `0x7F55`:

```
if (matrix[3] != 0x7F55)                       -> class 3, accepts ND-5800 / ND-5900
else if (matrix[6] == 0x7F55)                  -> neither class established, always rejects
else if (matrix[2] == 0x7F55)                  -> neither class established, always rejects
else                                           -> class 2, accepts ND-5400 / ND-5500 / ND-5700
   (the full-match path at 0x1150 gives class 1, accepts ND-5200 only)
```

Because `matrix[s] bit w = read[w] bit s`, requiring `matrix[s] == 0x7F55` means:

```
read[w] bit s must be 1 for w in {0,2,4,6,8,9,10,11,12,13,14}
read[w] bit s must be 0 for w in {1,3,5,7,15}
```

(`0x7F55` = `0111 1111 0101 0101`.)

**So for the emulator's current all-zero reads, `matrix[3] != 0x7F55` holds and class 3 is
chosen - which is why only model digits 8 and 9 can ever be accepted today.** That is correct
behaviour, not a bug. To reach class 2 or class 1, bit 3 (and then bits 6 and 2) of the
sixteen reads must follow the pattern above.

**ECO levels** (`0x9F78`, reached from `Cmd1F_ReadEcoLevels` @0x9F12) read the same matrix:

```
eco(s) = (matrix[s] >> 11) & 0x0F          // 0x9FBA: asr #11, and #0x0F
```

and `0x9FC6` treats **`0x0F` as "absent"** - it prints `00` instead of a level. Expanded
through the transpose:

```
eco(s) = (read[11]>>s & 1)
       | (read[12]>>s & 1) << 1
       | (read[13]>>s & 1) << 2
       | (read[14]>>s & 1) << 3
```

`Cmd1F_ReadEcoLevels` walks ten selectors in this order: **0, 1, 2, 4, 5, 8, 6, 0x0C, 0x0D, 3**,
labelling each from a 12-byte descriptor table at `0x00012D5C`.

**[INFERENCE - consistent, not proven]** `0x7F55` looks like an **"absent / invalid" sentinel**:
its bits 11-14 are all ones, which is exactly the `eco == 0x0F` "absent" case, and
`Selftest_ProbeCacheAndAap_B` (`0xF28E`, `0xF2DE`) uses the same `0x7F55` test to decide
whether to print a message instead of running a test. Reading it as "slot empty" makes the
model-class chain read naturally: word 3 not empty means class 3, and so on.

**[OPEN]** Phase 3 is followed at `0x7DD0` by field extraction that copies matrix bit 10 into
bit 11, then splits each word into `& 0x7800`, `(w << 3) & 0x700`, `(w >> 3) & 0x60` and
`w & 0x1F` before calling `0x7CA2`. That repacking is only partly decoded and is not needed to
satisfy the model check.

### The one-line instruction for an implementer

Model what **`0x00220000` returns on sixteen successive word reads** during the routine at
`0x7D26`. Do not touch `0x114550`.

### Straps and thumbwheels - the complete list [SWEPT 2026-07-30]

**The 5616 has no thumbwheel switches.** The board documentation records `Switches: None` and
`LEDs: None`, with only five ECO-level straps. ND-14001 section 4.8.1 explains why: thumbwheels
are for **global** OCTObus nodes, while **local** nodes - which the ACCP is, sitting on the
MFbus backwiring - are initialised by the MFbus controller writing their on-board WOI register.

A full sweep of the `0x90xxxx` select finds only **two** addresses touched by code:

| Address | Read at | Use |
|---|---|---|
| `0x00900001` | `0x0B4A` in `BootInitAndErrorRouting`, `0x1230` in `MfBusControllerConfigCheck` | **Station / configuration register.** At `0x1230` masked with `0x1F` (5 bits) to give the ACCP's own OCTObus station number for the discovery payload. At `0x0B4A` the whole byte is shifted left 8, OR'd with the byte at `0x001143B8`, and passed to `0x72A0`. |
| `0x00900007` | `0x07D4` in `Vec30_AutoIrq6`, `0x7C04` in `CmdPortWithLatchGate` | **Not configuration.** Interrupt and latch gating. |

**`0x900001` is almost certainly the WOI/STANO register the MFbus controller writes, not a
strap.** The 5-bit mask matches WOI's STANO field width exactly, and the board has no switches
to read. **[INFERENCE, and it matters]** if that is right, a correct emulator has the MFbus
controller write this register during crate configuration, *before* the ACCP boots - and the
discovery scan would then never need to run at all. Today the emulator returns **1**, which is
the ND-120 CPU slot and not a legal local-node number (local nodes are 20-77 octal).

**ECO levels are not straps read by the firmware.** `Cmd1F_ReadEcoLevels` @`0x9F12` reads them
out of the `0x114550` matrix, not from any port - see the eco(s) formula above. So there is no
strap address to model for them either.

**False positives to ignore**: a byte search for `009000??` also hits `0x1704` in `DuartInit`
(which is `move.b #0x90,(0x00DD000D)` - a DUART write, not a `0x90` select) and a dozen offsets
above `0x14000`, which are in the string and descriptor region.

### Reuse what exists - do not reimplement the protocol

`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Chips.NorskData\src\OBCON\ObconMessage.cs`
already has `ObconFrame` (compose/decode), `ObconMultibyte` (`Encode` / `TryDecode`),
`ObconInformationBits`, `ObconMessageType` and `ObconReceiveBuffer` (buffer offsets,
registration stride, the CMD constants). On the ND-100 side there is also
`OctobusFabric` / `IOctobusStation` / `OctobusStationBase` / `OctobusMultibyteCollector`, with
`OctobusStationType.MFbus = 2` already defined and two worked examples
(`OctobusND5000Station`, `OctobusScsiDiocStation`).

### Checklist for the peer

- Answer on **CMD 5**, and be **registered** for it - an unconnected CMD is never delivered.
- Reply content byte **0 = 0x00** (status good). `0xFF` instead makes byte 1 an error code:
  1 = keep scanning, 2/3/4 = print a specific complaint.
- Reply content byte **1 = the model digit**, consistent with the signature table.
- Sit in the station range **2-7**; the ACCP scans exactly those.
- **The direction rewrite is the classic trap.** On transmit bits 13-8 are a DESTINATION; on
  receive they are a SOURCE. A reply must be composed with the PEER's station in that field.
  Composing it with the ACCP's station looks correct in a log and then silently fails
  MFCRECEIVE's source test.
- Disable the OBCON loopback (`AutoReplyEnabled = false`). The default echo ends the scan
  while carrying no real information, and it masks a real peer.

### Two defects this surfaced

1. ~~**[BLOCKER] Only the first content byte of a reply reaches the driver's receive buffer.**~~
   **SOLVED 2026-07-30, RetroCore commit `dbdc291e5` - and the diagnosis below was wrong.** The
   real cause was **interrupt presentation**: `Run()` sampled `UpdatePendingInterrupt()` once per
   1024 instructions, and that function picks one highest source with `else-if`, so octobus IRQ3
   was never presented while the DUART asserted IRQ5. The receive ISR takes **one frame per
   interrupt**, so a ten-frame reply needs ten interrupts, and MFCRECEIVE abandons after 10000
   polls. The buffer was never truncated - the write pointer was still at its initial 5, so ZERO
   content bytes had been appended, and byte 5 read `0x00` because the buffer was untouched.
   **Also: never sample that buffer at end of run** - the firmware clears the reassembly record on
   close and re-initialises all 64 per-station records and 16 registration entries at `0xF4E6`.
   Superseded original text follows.

   **[SUPERSEDED] Only the first content byte of a reply reaches the driver's receive buffer.**
   Dumped live from the buffer at registration entry 5 (`0x00112D40`, data area `0x00112D54`)
   after a six-byte reply: `byte1=0x02` source, `byte3=0x05` own CMD, `byte4=0x06` length all
   correct, `byte5=0x00` content[0] - and then **zeros**. Content bytes 1 upward never arrive,
   so the model digit cannot get through. This is a receive-path bug, and it is why the model
   cross-check cannot yet be satisfied.
2. **`0x900001` returns station 1.** Better than the old zero, but station 1 is the ND-120 CPU
   slot. The ACCP sits on the local octobus and should be 20-77 octal. Worth confirming
   whether that value is deliberate.

### Harness to build against

`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpMfBusControllerPeer.cs`
and `...\tests\AccpMfBusDiscoveryTests.cs`. Six tests pass; the three model-digit cases are
`[Ignore]`d with defect 1 as the stated reason. `Diag_CpuModelCrossCheckState` dumps the
signature table, `0x1131F6`/`F8`/`FA`, the registration entry and the buffer - use it rather
than reasoning from console output, which cannot distinguish "wrong digit" from "no class
established".

Full carve: section 1c of
`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`.

---

## 5. Explicitly out of scope for now

- Connecting the ACCP machine to the ND-100 side or to `NDBusOctobus`. Get the card alive standing
  alone first, then bridge.
- The control-store / microprogram commands. `LOAD-CONTROL-STORE` and friends talk to an ND-5000
  CPU that is not present; expect them to time out, and that is correct behaviour for now.

  **Update 2026-07-27 - the register-level path is now identified, and it fails cleanly.** The
  region `0x71F8-0x7C14` is the control-store loader (companion doc §2.4l); `0x741E` prints
  `CONTROL STORE ERROR in buffered CI-bits 35 or 40.` and returns -1 when
  **`0x660000` bit 0** (operation OK) is clear. So with the Phase 2 stubs returning 0, every
  control-store operation reports a clean error rather than hanging - exactly the "expect them to
  time out" behaviour assumed above, now confirmed at instruction level rather than hoped for.

  Two levers worth knowing: **`0x001131E2`** is the sticky error latch (set to -1), and bits 10..8
  of **`0x00114560`** are a message-level threshold - the error text is only printed when that
  field is >= 1, so a test can suppress or force the diagnostic.

  **There are TWO control-store paths, not one** (companion §2.4m). `0x741E` and `0x764E` are
  near-identical - same `0x0018` command, same `0x660000` bit-0 success test, same error latch and
  string - and differ only in which `0x330000` gate bit they set (**bit 2** vs **bit 1**) and which
  status word they consult (**`0x00114560`** vs **`0x0011455C`**, 4 bytes apart). That lines up with
  the error text naming two positions, "buffered CI-bits 35 **or** 40": two buffered
  control-instruction bit groups, selected by the gate bit.

  Gotcha for tests: the two paths do **not** report identically. `0x741E` complains when the level
  field (bits 10..8) is >= 1; `0x764E` additionally requires `(status and 0x1F) > 3` when the level
  is exactly 1. Same stub state can therefore produce a diagnostic from one path and silence from
  the other - do not assert they behave the same.
- Bit-level serial. Byte-level was chosen deliberately.

## 6. Related

- `SINTRAN\ND5000\ACCP-324716-FIRMWARE-RE-2026-07-27.md` - all firmware facts
- `Installation\Communication\OctobusAccp\` - the image, the EPROM dumps, the interleave README
- `SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
- `SINTRAN\ND5000\OCTOBUS-TEST-PROTOCOL-RE.md` - the OMD-0 protocol this card answers
- Skills: `retrocore-machine-integration`, `retrocore-chip-cli-decoration`, `retrocore-csharp`,
  `retrocore-cpu-test`, `cli-attach-machine`, `retrocore-keyboard-input`, `octobus-nd5000`
