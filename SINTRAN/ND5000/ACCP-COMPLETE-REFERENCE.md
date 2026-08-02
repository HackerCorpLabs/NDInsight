# ACCP (ND-324716 / PCB 5616) - complete reference

The Samson ACCess Processor: its 68000 firmware, hardware register map, console command
set, the CPU-side interface seam, and the CPU model class derivation. Everything here is
carved from the EPROM image `octo.bin` unless explicitly marked otherwise.

**This file replaces six separate documents** (listed below), merged 2026-07-31. The part
bodies are the original text; nothing was summarised, condensed or dropped, and the merge
was verified line by line.

Companion file: `ACCP-EMULATION-STATUS-AND-HANDOFF.md` (implementation plan, status,
defect reports, captures).

Related docs NOT merged here, because they cover other layers:
`OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md` (octobus protocol),
`ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` (microcode side),
`CARVE-ANSWER-OCTOBUS-ACCP-SELFTEST-CSLOAD-2026-07-18.md`.

**Exactly two parts are NOT byte-identical to their last committed version.** Part 4 and
part 1 of the companion file were edited on 2026-07-31, after their last commit and before
the merge, to record the phase-4 carve. If you diff this file against git you will find
those additions; they are intended. Every other part is byte-identical modulo one heading
level.

---

## READ THIS FIRST - which part wins when two disagree

These documents were written on different days as the carve progressed, and several later
findings **overturn** earlier ones. As separate dated files the date settled it. Inside one
body that signal is gone, and in some places the superseded claim sits LATER in reading
order, so it looks newer. It is not.

An earlier version of this header said "the later date wins". That rule does not work:
parts 1 and 2 are both dated 2026-07-27. Use this table instead.

| Topic | SUPERSEDED - do not build on | AUTHORITATIVE |
|---|---|---|
| What `0x220000` is | Part 2's map table calls it "MF-bus / BADAP command port, PROVEN"; part 1 heading 2.4h calls it a bit-banged serial port | **Part 1 sections 2.4b + 2.4j + 2.4n**: a GENERAL command/function port that ALSO acts as a shift clock. Both models are true depending on phase |
| What `0x440000` / `0x550000` are | Part 2: "MF-bus data LOW/HIGH half, PROVEN" | **Part 1 sections 2.4j + 2.4n**: a general bidirectional 32-bit pair, also the control-store microword staging port |
| What `0x330000` is | Part 2: "octobus (OBCON) control latch" | **Part 1 sections 2.4l + 2.4k**: bit 2 gates a control-store operation, bit 6 is the AOB write strobe. Part 2's own section 0 already says the "octobus latch" name is wrong |
| `0x660001` | Part 2: "status byte, bit 4 = transaction complete, PROVEN" | **Part 1 section 2.4b**: individual bits belong to DIFFERENT functions - bit 1 AOB busy, bit 2 message available, bit 4 MF-bus complete. Not a single-purpose register |
| SCN2681 register 15 | Part 2 section 3: "OPCR, Set Output Port", no read function | **Part 1 section 2.2b step 3**: a READ of register 15 is the Stop-counter command, cross-checked against MAME `mc68681.cpp`. Part 2's "no gaps, nothing left over" proof claim does not hold for this row |
| DUART channel B | Part 2 section 3: "purpose still unknown, not yet proven from code" | **Part 1 section 2.2b**: fully initialised and ENABLED at power-on, 9600 8N1 - a data setting, not a terminal setting |
| Signature matrix / CPU model class | **Part 4 sections 6-8**, wherever it maps `read[w]` straight onto `matrix[s]` - including the `0x7F55` bit lists and the ECO formula | **Part 5**: the builder has FOUR phases; phase 4 at `0x7DD0` rewrites every word before the class chain reads it |
| Peripheral select list | Part 1 section 2.3 (omits `0x77` and `0xAA`) | **Part 2 section 1** - and note `0xAA0000` appears ONCE in this whole file, at part 2, and is missing from part 1's "still open" list |
| Handler count | Part 3: "all 43 handlers" | **Part 1 section 5: 42.** Both parts agree HELP (`0x0C`) is inline with no `jsr`, so 42 is the defensible number |

**Cross-reference warning.** Each part kept its own numbering, so this file contains six
`### 1.`, six `### 2.` and so on. A bare "see section 5" means section 5 **of the part you
are reading**. The `2.4x` lettered scheme in part 1 is unique across the file and is safe.

**"Phase N" is ambiguous across this file.** Part 5 uses phases 1-4 for the *signature
matrix builder* at `0x7D26`. Part 1 lines mentioning "Phase 2" / "Phase 6" mean phases of
the *disassembly plan*, which now lives in part 1 of the companion file. They are unrelated.

---

## Contents

1. **Part 1** - the firmware write-up of record: identification, memory map, 68000 vector
   table, PLANC conventions, the console command table, the embedded selftest microcode,
   and the `2.4x` register carves. Originally `ACCP-324716-FIRMWARE-RE-2026-07-27.md`.
2. **Part 2** - full-image sweep of every peripheral address, with false positives called
   out. **See the arbitration table above before trusting its "PROVEN" column.**
   Originally `ACCP-HARDWARE-ADDRESS-MAP-2026-07-27.md`.
3. **Part 3** - all 43 console commands with codes, parameter syntax and handler addresses;
   the dispatch is a linear compare chain, not a jump table.
   Originally `ACCP-CONSOLE-COMMAND-SET-AND-DISPATCH-2026-07-27.md`.
4. **Part 4** - the ACCP <-> ND-5000 CPU interface seam: AOB/AIB/AFLAG/AOBASR, both
   handshakes, the AIB command channel, kick and trap classes, the CPU model chain.
   Originally `ACCP-ND5000-CPU-INTERFACE-SPEC-2026-07-30.md`.
5. **Part 5** - the CPU model class derivation, SOLVED and live-verified. The four-phase
   matrix builder, the Gray decoder at `0x7CA2`, and the `0x220000` armed read port.
   **Read this before touching any CPU-model logic.**
6. **Part 6** - the ACCP is NOT running PIOC-OS. Same PLANC-MC compiler, no kernel, no
   `trap #2`. Originally `PIOC-OS-VS-ACCP-FIRMWARE-COMPARISON-2026-07-27.md`.

Evidence for parts 4 and 5 (the defect report and the clean-boot capture) lives in parts 3
and 4 of `ACCP-EMULATION-STATUS-AND-HANDOFF.md`. Working the CPU-model problem needs both
files open.


---

# Part 1 - originally `ACCP-324716-FIRMWARE-RE-2026-07-27.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## ACCP (ND-324716 / PCB 5616) 68000 firmware - reverse-engineering notes

**Image**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
- 131072 bytes (0x20000), the two AM27C512 EPROMs `51200J.bin` + `51201J.bin` interleaved
even/odd (even = 51200J = D8..D15, odd = 51201J = D0..D7; see that folder's `README.md`).
SHA256 `0EA81716AD81984B64675E9A8CCEB6C1909AB299BE0048857C58F85C3479C5F1`.
A byte-identical working copy was analysed as `C:\Temp\octo\octo.bin` (same SHA256, verified
2026-07-27) - the repo path above is the one of record.
**Ghidra program**: `octo.bin`, language `68000:BE:32:default`, image base 0x00000000.
**Date of this pass**: 2026-07-27.

The card is the **Samson ACCess Processor** - the ND-5000's access processor **and** its
octobus controller. Hardware reference: http://sintran.com/hardware/nd-5000/nd-324716.html
It supersedes ND-324702. An RS-232 console on the card-crate plug board reaches a debug
command prompt; the DUART is an SCN2681. The card carries 4x 8192x8 SRAM (32 KB total).

**This image is the octobus controller's operating software** - the thing previously recorded
as absent from the repo and from F: (see `SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
section 4.3, and the memory note `domino-bdio-in-kernel`).

Everything below is read out of the image. Where something is inference rather than proof it
says so.

---

### 1. Identification - what the strings say [VERIFIED]

Console text confirms the card without any outside assumption:

```
0x11729  ******   S A M S O N   A C C E S S   P R O C E S S O R   ******
0x11778  ACCP local ram test OK
0x11D49  Communication ACCP-ND100 started. Version:
0x11DB1  Only 32-bit Word accesses available from ACCP to MF-bus!
0x11E95  Illegal ACCP command received from microprogram:
0x1230A  K I C K   T I M E O U T :
0x12331  AOB not read by microprogram within timeout.
0x1236B  AOB full, previous message not read. Message lost!
0x1240C  Octobus receive fifo:
0x12481  Not prepared for message from station
0x12554  in DOREC_MULTI_OCTO
0x12574  in DOSEND_MULTI_OCTO
0x128FA  error. BADAP status:
0x12A08  6 8 0 0 0   T R A P :
```

Strings are ND/PLANC style: **`$` (0x24) is the newline marker**, embedded in the text.

---

### 2. Memory map

#### 2.1 SRAM = 0x00110000-0x00117FFF (32 KB) [PROVEN]

Not an inference. The reset routine at **0x0BD6** walk-tests it before anything else:

```
0BD6  move.l  #0x110000,D0
...   write D0 to (D0), read back, compare; then write NOT D0, read back, compare
0C0C  cmpi.l  #0x114000,D0        ; first half, error count -> D2
...
0C40  cmpi.l  #0x118000,D0        ; second half, error count -> D3
0C4A  movea.l #0x110000,A0        ; then zero 0x110000..0x117FFF
0C5A  move.l  D2,(0x0011312A).l   ; store the two error counts
0C60  move.l  D3,(0x0011312E).l
0C66  move.w  #1,(0x00113132).l
```

Two 16 KB halves, counted separately - consistent with 4 chips of 8192x8 arranged as two
16-bit-wide banks. Initial supervisor SP (vector 0) = **0x00113FFC**; early boot SP = 0x112000.

The address space is **contiguous**: the second loop starts from wherever the first ended
(`0x114000`) and runs to `0x118000` - the code never reloads a base for the second half. So the two
"halves" are `0x110000-0x113FFF` and `0x114000-0x117FFF`, and a single flat 32 KB `Ram(...)` region
models both correctly.

**Details that matter when this is used as the emulator's boot oracle:**

- **The walk test uses no stack.** From 0x0BD6 to 0x0C72 there is not one `JSR` or `PUSH`. That is
  necessary, because the zero-fill at 0x0C4A wipes `0x110000-0x117FFF`, which includes the reset
  SSP at `0x00113FFC`. The reset SSP is a *placeholder that is never actually used*.
- **The result stores happen after the zero-fill** (0x0C4A-0x0C58 wipes, then 0x0C5A-0x0C66
  stores), so the three oracle values survive. Getting this order wrong in a re-implementation
  would zero the very evidence the test reads.
- **Widths**: `0x0011312A` and `0x0011312E` are written with `move.l` (**32-bit**), `0x00113132`
  with `move.w` (**16-bit**). Assert them at those widths.
- **SP is relocated immediately afterwards**: at 0x0C7A, `lea (0x2000,A6),SP` with A6 = `0x110000`
  sets the real stack to **`0x112000`**, growing down. A6 = `0x110000` is the PLANC global base, so
  every `0x11xxxx` global is really `A6 + offset` (e.g. `0x113132` = `A6+0x3132`). Layout that
  falls out: `0x110000-0x111FFF` = stack (8 KB), `0x112000` upward = globals.
- `0x0C14` and `0x0C6E` are `jmp` to the very next instruction - prefetch-flush idioms, no
  behavioural effect. A correct 68000 core needs no special handling; they are noted only so nobody
  mistakes them for damaged disassembly.
- SR is set to `0x2700` at 0x0C8E (supervisor, all interrupts masked) **before** the first `JSR`,
  so nothing can interrupt the early init.

#### 2.2 SCN2681 DUART = 0x00DD0000 [PROVEN]

The 68000 has no separate I/O space, so every peripheral is memory-mapped. The DUART sits at
**0x00DD0000 with its registers on ODD byte addresses** - an 8-bit part on the low data lane,
so register N is at `0xDD0000 + 2*N + 1`.

Proven in `DuartTxServiceBothChannels` @ **0x1D4C**:

| Address in code | SCN2681 register | Use |
|---|---|---|
| `btst.b #2,(0x00DD0003)` | 1 = SRA | channel A status, bit 2 = TxRDY |
| `move.b ch,(0x00DD0007)` | 3 = THRA | channel A transmit holding |
| `btst.b #2,(0x00DD0013)` | 9 = SRB | channel B status, bit 2 = TxRDY |
| `move.b ch,(0x00DD0017)` | 11 = THRB | channel B transmit holding |

SRA=1, THRA=3, SRB=9, THRB=0x0B is the SCN2681 map exactly.

Channel A = the ACCP console. **Channel B is very likely the ND-100 link** - the command
`SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` exists, and there is a
string `Illegal kick ... received over serial line`. Marked LIKELY, not proven, because I have
not traced which ring feeds which channel end to end.

Software TX rings: channel A struct at **0x0011307E**, channel B at **0x00112EC2**
(`+0x10` = limit, `+0x12` = count; dequeue helper at 0x192E).
Busy-flag byte **0x001131D8**: bit 0 = channel A transmitting, bit 4 = channel B.
**Refinement [VERIFIED 2026-07-27]**: `0x001131D8` is the software **shadow of the DUART IMR**.
`DuartInit` writes `0x22` to IMR *and* the identical `0x22` to `0x001131D8` back to back
(0x16F4 / 0x16FC). Its bits are therefore IMR bits, and the earlier description is right for the
reason that IMR bit 0 = TxRDY-A and bit 4 = TxRDY-B: the firmware enables the TX interrupt exactly
while a channel has something to send. Model it as "IMR shadow", not as an ad-hoc busy flag.

#### 2.2b DuartInit @ 0x162E - the complete power-on DUART programming [VERIFIED 2026-07-27]

Hand-disassembled at 0x162E; every register index below is `(addr - 0xDD0000 - 1) / 2` and every
index was cross-checked against MAME `mc68681.cpp` write-path case labels (lines 953-1025).
Eleven distinct registers all landing on sensible functions is independent confirmation that both
the `0xDD0000` base and the `+2N+1` odd-byte decode are correct.

| Order | Address | Reg | Name | Value | Meaning |
|---|---|---|---|---|---|
| 1 | `0xDD0005` | 2 | CRA | `10,20,30,40,50,7A` | reset MR pointer, reset RX, reset TX, reset error status, reset break-change IRQ, then stop-break + disable RX/TX |
| 2 | `0xDD0015` | 10 | CRB | `10,20,30,40,50,7A` | identical reset ladder for channel B |
| 3 | `0xDD001F` | 15 | *read* | - | **Stop counter command** (read of reg 0x0F). Reading it is the side effect; the byte lands in a scratch local. |
| 4 | `0xDD0009` | 4 | ACR | `0xE0` | bit 7 = 1 -> **baud-rate set 2**; bits 6-4 = 110 = counter/timer mode |
| 5 | `0xDD0001` | 0 | MR1A, then MR2A | `0x02`, `0x0F` | MR pointer auto-advance. MR1A = 7 data bits, parity enabled, even. MR2A = 2 stop bits, normal channel mode. |
| 6 | `0xDD0003` | 1 | CSRA | `0xBB` | RX and TX clock = index 0xB. With ACR bit 7 = 1, MAME `baud_rate_ACR_1[0xB]` = **9600 baud**. |
| 7 | `0xDD0005` | 2 | CRA | `0x05` | enable RX + enable TX, channel A |
| 8 | `0xDD0015` | 10 | CRB | `0x1A` | reset MR pointer + disable RX/TX (so MR1B/MR2B can be written) |
| 9 | `0xDD0011` | 8 | MR1B, then MR2B | `0x13`, `0x07` | MR1B = 8 data bits, **no parity**. MR2B = 1 stop bit. |
| 10 | `0xDD0013` | 9 | CSRB | `0xBB` | **9600 baud** channel B |
| 11 | `0xDD0015` | 10 | CRB | `0x05` | enable RX + enable TX, channel B |
| 12 | `0xDD000B` | 5 | IMR | `0x22` | `INT_RXRDY_FFULLA` (0x02) + `INT_RXRDY_FFULLB` (0x20) |
| 13 | `0x1131D8` | - | *RAM* | `0x22` | IMR shadow, see above |
| 14 | `0xDD000D` | 6 | CTUR | `0x90` | counter/timer preload, high byte |
| 15 | `0xDD000F` | 7 | CTLR | `0x00` | low byte -> preload `0x9000` = 36864 |

**Line settings, now proven, and they differ per channel** - this is the strongest evidence yet for
what each channel is for:

- **Channel A = 9600 7E2.** A classic ND *terminal* setting. This is the operator console.
- **Channel B = 9600 8N1.** An 8-bit *data* setting, not a terminal setting. This materially
  strengthens (does not yet prove) the ND-100-serial-link reading of `SET-SERIAL-LINE`.
- Channel B is fully initialised **and enabled** (`CRB = 0x05`) at power-on, so the firmware expects
  it to exist even when nothing is attached.

**Two facts an emulator must honour:**

1. **TX is polled, RX is interrupt-driven.** IMR = `0x22` enables only the two RxRDY sources;
   `INT_TXRDYA` (0x01) and `INT_TXRDYB` (0x10) are masked at init. The TX path polls SR bit 2
   (`btst.b #2` in `DuartTxServiceBothChannels` @ 0x1D4C). A DUART model that asserts an interrupt
   on TxRDY at reset will fire an interrupt the firmware is not expecting.
2. **MR pointer auto-advance is mandatory.** Steps 5 and 9 write MR1 and MR2 through the *same*
   address, relying on the pointer advancing after the first write, and on `CRA/CRB` command 1
   resetting it. A model that ignores the MR pointer silently gets the wrong character length and
   parity, and the console then emits plausible-looking garbage rather than failing loudly.

Console output chain: `ConsPrintString` 0x1A0A -> `ConsPutCharQueued` 0x1BF6, with
`ConsPutCrLf` 0x1D32 for the `$` marker.

#### 2.3 The chip-select decode is nibble-replicated [VERIFIED pattern]

Peripheral addresses are all of the form `0xNN0000` where NN is a repeated nibble:
**0x11 = SRAM, 0x22, 0x33, 0x44, 0x55, 0x66, 0x88, 0xBB = peripheral selects, 0xDD = DUART.**
0x44 and 0x55 were missed on the first pass and only appeared once the code at 0x70CC was
disassembled by hand. Treat one NN as one chip select; assume more exist until proven otherwise.

#### 2.4 MF-bus / BADAP register model [CARVED 2026-07-27]

The routine at **0x70CC** pins four of the selects. It is the MF-bus path, not the octobus -
proven by its timeout branch at 0x7124, which loads the string descriptor at 0x1216A =
`{origo 0x12176, 0, 0x16}` = `"$MF-bus memory timeout$"`.

| Address | Width | Role |
|---|---|---|
| `0x00220000` | word | COMMAND / PARAMETER port. High nibble selects a function, low byte carries the value. Seen: `0x300F` open/select, `0x400A` and `0x400C` sub-functions, `0x000F` strobe/execute closing each group of three writes. |
| `0x00440000` | word | DATA, LOW half of a 32-bit value |
| `0x00550000` | word | DATA, HIGH half (the code does `swap D0` between the two writes) |
| `0x00660001` | byte | STATUS, **bit 4 = transaction complete**. Polled in a software countdown loop; exhausting it prints the timeout. |
| `0x00113138` | word | software "skip the wait" flag - non-zero suppresses the poll |

This is exactly why the firmware prints `Only 32-bit Word accesses available from ACCP to MF-bus!`
(0x11DB1): a 32-bit datum must be moved as a low/high word pair.

Typical sequence (from 0x70D0):

```
move.w #0x300F,(0x00220000)   ; open
move.w #0x400A,(0x00220000)   ; sub-function
move.w #0x000F,(0x00220000)   ; strobe
move.w D0,(0x00440000)        ; data low
swap   D0
move.w D0,(0x00550000)        ; data high
move.w #0x300F,(0x00220000)   ; open
move.w #0x400C,(0x00220000)   ; sub-function
move.w #0x000F,(0x00220000)   ; strobe
btst   #4,(0x00660001)        ; wait for complete
```

[INFERENCE, not proven]: the exact meaning of the high-nibble function codes 0x3/0x4/0x0 and of
sub-functions 0x0A/0x0C. Read ND-14001 chapter 4 before naming them.

#### 2.4b The IRQ3 KICK handler @ 0x0510 - the AOB/AIB path [CARVED 2026-07-27]

Carved from `Vec27_AutoIrq3` @ 0x0510 and its subroutines 0x05C0 / 0x0694. This is the octobus
message path, and it is named by the firmware's own error strings:

| String descriptor | Text |
|---|---|
| `0x122FE` | `$K I C K   T I M E O U T : ` |
| `0x12326` | `AOB not read by microprogram within timeout.$` |
| `0x12360` | `AOB full, previous message not read. Message lost!$` |
| `0x123A0` | ` from SAMSON$` |
| `0x123BA` | ` to SAMSON$` |

"microprogram" = the ND-5000's microcode, so **AOB is the ACCP -> SAMSON buffer** and AIB the
reverse - matching the console commands `LOAD-AOB16/32` and `READ-AIB16/32`.

**IMPORTANT CORRECTION to §2.4.** That section named `0x220000` / `0x440000` / `0x550000` /
`0x660001` "the MF-bus register model". That was over-generalised from a single routine (0x70CC),
whose *timeout string* happens to mention the MF-bus. The IRQ3 path uses **the same ports** for the
AOB: it writes a word to `0x440000` (0x0616, 0x06FE) and issues `move.w #0x0005,(0x00220000)`
(0x072A). So the correct model is:

> `0x220000` is a **general command/function port**. The function code selects *which* target the
> `0x440000` / `0x550000` data pair is talking to. `0x300F/0x400A/0x400C/0x000F` drive MF-bus
> memory; `0x0005` drives the AOB path. `0x660001` is a shared status byte whose *individual bits*
> belong to different functions.

`0x660001` bit assignments now known: **bit 1 = AOB busy / previous message not yet read**
(polled at 0x05CA, 0x05DC, 0x0646, 0x065A with a software countdown loaded from `0x001131DC`);
**bit 4 = MF-bus transaction complete** (§2.4). Do not treat the byte as a single-purpose register.

Other registers pinned by this handler:

| Address | Width | Role |
|---|---|---|
| `0x00880000` | word | **Message / kick read port.** Read once at IRQ3 entry (0x0514) and bit-classified as the interrupt cause. Also *drained in a loop* by the IRQ7 path (0x0882-0x088A) while `0x660001` bit 2 stays set - so it is genuinely FIFO-like as well. See the note below. |
| `0x00330000` | byte | Command byte port. Seen `0xF0` (0x056C, master-clear path) and `0xD8` (0x061C, send). |
| `0x00330001` | byte | **Write-only control latch with a RAM shadow at `0x001144EF`.** The firmware never reads it back: it does `bclr`/`bset` on the shadow then copies the shadow to the latch (0x055A-0x057C). Bit 1 is pulsed low-then-high. |
| `0x00660000` | byte | status, bit 3 tested (0x06A4) |
| `0x00660001` | byte | bit 0 tested (0x06EC) in addition to bits 1 and 4 above |
| **`0x00770004`** | word | **NEW SELECT - not in any previous list.** `movea.l #0x770004,A1` @ 0x069E. Data is moved *into* it from `0x440000` (0x06FE). `(3,A1)` = `0x00770007` is a status byte, bits 3 and 4 polled as a handshake with a retry count of 10. |

The IRQ3 classification is a chain of `eori.w`/`andi.w`/`bne` triples - i.e. "does `D0 & mask`
equal `pattern`":

| Test | Mask | Pattern | Action |
|---|---|---|---|
| 0x051C | `0x80C0` | `0x8040` | -> 0x05C0 (AOB service) |
| 0x0530 | `0x80A0` | `0x8000` | -> 0x05C0 (AOB service) |
| 0x0542 | `0xC0FF` | `0xC0FF` | plus bits 13-8 must equal the word at `0x001143A0` -> **remote master clear** |
| 0x059C | `0x8080` | `0x8080` | plain `rte`; anything else -> `jsr 0x10832` |

**Remote master clear [notable]**: on the third match the firmware pulses `0x330001` bit 1 via the
shadow, writes `0xF0` to `0x330000`, busy-waits 10000 iterations, calls `0x795A`, then
`jmp 0x00000C72` - which is the init entry *just after* the RAM walk-test. So the ND-100/ND-5000
side can **remotely restart the ACCP firmware** over the octobus, and that restart deliberately
skips the RAM test. The guard word at `0x001143A0` (bits 13-8) is what keeps a stray kick from
resetting the card.

**Free oracle for the emulator**: `0x001143B4` and `0x001143B6` are trace-enable flags. When
non-zero the handler prints the kick value followed by ` from SAMSON` / ` to SAMSON` (0x062E-0x0642,
0x070C-0x0726). If a console command sets them, the firmware will *narrate its own octobus traffic*
- the cheapest possible cross-check for a stub or a real OBCON model. Finding which command sets
them is worth doing before Phase 6.

#### 2.5 Still unidentified [OPEN]

**Superseded entries** - `0x00330000`, `0x00330001`, `0x00880000` and `0x00660001` are now carved;
see §2.4b and §2.4c.

**Correction to a correction**: §2.4b originally called the "FIFO read in a drain loop" reading of
`0x00880000` *wrong*, on the grounds that IRQ3 reads it exactly once. That was itself too strong.
IRQ7 **does** drain it in a loop (0x0882: `btst #2,(A0)` / `move.w (A1),D0` / branch back, with
A0 = `0x660001`, A1 = `0x880000`). Both readings are right: `0x880000` is the message read port,
`0x660001` bit 2 is its data-available flag, IRQ3 takes one message, IRQ7 empties it.

**`0x00900007` is REAL - the nibble-replication rule has at least one exception.** This section
previously flagged it as probably misread because `0x90` is not a repeated nibble. It is not
misread: at **0x07D2**, `move.b (0x00900007).l,(0x001143B8).l` - a genuine absolute-long byte read
whose value is latched into a RAM variable. Only two references exist in the whole image (0x07D4
and 0x7C04). So §2.3's "every peripheral is at `0xNN0000` with NN a repeated nibble" is a strong
*tendency*, not a law. Do not use it to reject an address.

Genuinely still open:

- `0x00660000` - byte status. Bit 3 tested at 0x06A4, **bit 5** tested at 0x082E (set -> take the
  drain-and-restart path). Whole-byte value snapshotted to `0x001143BA` at 0x07B2.
- `0x00BB0000` - word, written `0` in the IRQ7 path immediately before the firmware restarts.
- `0x00900007` - byte, snapshotted to `0x001143B8` at interrupt time alongside `0x660000`. The
  pairing suggests a second status/ID byte captured for diagnostics.
- `0x00770004` / `0x00770007` - newly found (§2.4b), role only partly understood: data arrives from
  `0x440000`, and bits 3/4 of `0x770007` are a handshake with a retry count of 10.

The count of selects has now grown twice (0x44/0x55, then 0x77) after each "complete" list. Assume
the list is still incomplete.

**Where the octobus driver lives**: references to 0x33/0x66/0x88 cluster densely in
**0x6A74-0x7C14**, alongside the multibyte routines named in the strings (`DOSEND_MULTI_OCTO`,
`DOREC_MULTI_OCTO`). That region is the NDOBCON/OCTC driver and is the next carve target.

#### 2.4c The IRQ7 / NMI path and the `0x330001` latch bits [CARVED 2026-07-27]

`0x00330001` is a write-only control latch shadowed at `0x001144EF`. Individual bits are
manipulated on the shadow (or on a D0 copy of it) and the whole byte is written out. Bits seen:

| Bit | Where | Operation |
|---|---|---|
| 1 | 0x055A / 0x0574, 0x0838 / 0x0852 | cleared, then set - a **pulse**, part of master clear |
| 2 | 0x07E8 | cleared |
| 3 | 0x07C2 | cleared |
| 6 | 0x07E4 then 0x07F2 | cleared, then set - another pulse |

**Master-clear sequence** (identical at 0x0838-0x086E in the IRQ7 path and 0x055A-0x0590 in IRQ3):
pulse latch bit 1 low, write `0xF0` to `0x330000`, pulse bit 1 high, busy-wait `0x2710` (10000)
iterations, `jsr 0x795A`, then `jmp 0x00000C72`. ~~**`0x795A` is the octobus-controller re-init
routine** - it is the one thing both reset paths call, and therefore the natural next carve target
for the remaining register semantics.~~

> **CORRECTION 2026-08-02 - `0x795A` is STOPMIC. Do not re-adopt the re-init reading.**
>
> The struck-through claim above was wrong twice over, and it survived because the correction was
> written somewhere else in this same file instead of here:
>
> 1. **2026-07-27** (section 2.4e below): carved as a **latch DISABLE**, not a re-init. Section 2.4c
>    was never updated, so this file has contradicted itself for six days.
> 2. **2026-08-02** (ACCP-init agent, `[V]`): it is **STOPMIC**. Called by
>    `Cmd24_StopMicroprogram @ 0x91C6`, and its body matches manual 5.3.24 verbatim - from the
>    MREG-lower shadow clear bit 3 (MRUN) and bit 1 (SLOW), then clear bit 2 (**AMODE**, polarity 0,
>    so clearing *asserts* it). `Cmd24` then does `clr.w (0x1143AC)`.
>
> The two reset paths call it because **a reset stops the microprogram first**. That is a
> consequence of what it does, not its identity - which is exactly how the name-based guess got in.
>
> **This is the third time a name-based assumption has misled work on this interface**, after
> `0x300F`/`0x4016`/`0x8013` (assumed initialisation, actually the boot self-test bus loopback) and
> `0x0007` (assumed a read-arm, actually MASKAIBF). See section 2.4g-census.

The IRQ7 path proper (0x0876): drain `0x880000` while `0x660001` bit 2 is set, write `0` to
`0x00BB0000`, then `jmp 0x00000C72` to restart the firmware.

**`0x220000` command opcodes observed so far** (adding to §2.4's `0x300F` / `0x400A` / `0x400C` /
`0x000F`): **`0x0001`** (0x07A8, 0x0826), **`0x0005`** (0x072A, AOB path), **`0x0007`** (0x0788,
on the AOB-timeout path). The low byte is clearly the command; the high nibble (0/3/4) selects
something else. What each means is still [INFERENCE] - ND-14001 chapter 4 remains the thing to read.

#### 2.4g-census RUNTIME CENSUS of `0x220000` - complete, zero loss [V, 2026-08-01]

Static carving says what **exists**; this says what **runs**. Generated by
`Diag_CommandPortWriteCensus` in
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpCommandPortCensusTests.cs`
- regenerable, and re-runnable under other configurations on request.

**READ THE METHOD WARNING FIRST - it nearly produced a confident wrong answer.**
The stub log is an **8192-entry ring** and one boot pushes **6,965,052** accesses through this
window - about **0.65 accesses per instruction**. A first run using 100,000-instruction slices
**lost 6,128,148 of them (88%)** and still produced an entirely plausible result: 17 distinct words,
sensible counts, no error anywhere. **4,000-instruction slices bring loss to zero.** The test now
compares drained sequence numbers against the stub's monotonic counter and prints the loss every
run, so an incomplete census cannot be mistaken for a complete one.
**If you ever sample this port yourself, do NOT trust a single end-of-run snapshot.**

**CORRECTION to section 2.4h below: this port is almost entirely WRITES.**
**6,964,950 writes against about 102 reads** in a whole boot. The "bit-banged serial port... polled"
framing overstated the read side. The **armed/disarmed** read model still holds - 16 reads per armed
burst is exactly the right order of magnitude for ~102 reads a boot.

**All 17 distinct command words, with execution counts:**

| Word | Count | Word | Count | Word | Count |
|---|---|---|---|---|---|
| `0x0010` | 1,720,195 | `0x000F` | 1,678,257 | | *the clock pair* |
| `0x3010` | 41,939 | `0x0015` | 20,979 | `0x0006` | 20,964 |
| `0x2011` | 80 | `0x0005` | 20 | `0x2010` | 10 |
| `0x0018` | 8 | `0x0001` | 5 | `0x0007` | 4 |
| `0x0017` | 4 | `0x2018` | 4 | `0x001A` | 3 |
| `0x300F` | 1 | `0x4016` | 1 | `0x8013` | 1 |

**Every code we carved statically appears** - `0x300F`, `0x2011`, `0x000F`, `0x0010`, `0x0001`,
`0x0005`, `0x0007` - which cross-validates the static sweep of 52 immediate-write sites.

**`0x300F`, `0x4016` and `0x8013` execute exactly ONCE per boot.** Any model built by watching a
single routine will miss them completely. This is the concrete justification for the standing rule
in this file that five observed sites is not a survey.

**Transactions are FRAMED** - an opener, a run of clock pairs, a closer:

| Shape | Count |
|---|---|
| opener `0x0007` or `0x8013`, **64** clock pairs, closer `0x2018` | 20,968 |
| **16** clock pairs | 20,974 |
| **80** clock pairs | 1 |

The 64-pair and 16-pair transactions occur in **near-equal numbers** (20,968 vs 20,974), which is the
shape of a **paired transfer** - either address-then-data, or two halves of something wider.
**Deliberately not guessed**, and specifically NOT asserted that 64 pairs means 64 bits, because
phase 4 of the signature builder rewrites and Gray-decodes what the shift engine produces, so the
bit count on the wire cannot be read off the pair count.

**80 bursts have INVERTED clock phase** relative to the rest. The segmenter matches high-then-low
only, so it splits those into spurious 7-pair fragments rather than measuring them. Their count is
the same order of magnitude as the read accesses, which is **consistent with** our static reading
that phase order selects read versus write - **but that is an observation, not a carve**, and the
fragment width is an artifact of the tooling.

**Still unknown: what any individual code MEANS.** The census says which execute, how often, and in
what frames. ND-14001 chapter 4 remains the thing to read.

#### 2.4h `0x220000` looks like a BIT-BANGED SERIAL PORT, not a register file [CARVED 2026-07-27]

> **QUALIFIED BY THE RUNTIME CENSUS (2.4g-census above), 2026-08-01.** The shift-clock reading is
> right, but the traffic is **6,964,950 writes to about 102 reads** in a boot - so this is a
> write-driven shift port, not a polled one. Where earlier text here implies the firmware polls it
> for input, read the census instead.

`0x76E6` changes the picture of `0x220000` substantially. Its body:

```
76F2  movea.l #0x220000,A0
76F8  move.w  #0x10,D0          ; loop counter = 16
76FC  move.w  #0x10,D4
7700  move.w  #0x0F,D5
7704  move.w  (0x14,A6),(0x00550000)   ; the 16-bit parameter -> data-high port
770C  move.w  D4,(A0)           ; \  write 0x0010
770E  move.w  D5,(A0)           ;  }  then 0x000F
7710  subq.w  #1,D0             ;  }
7712  bne     0x770C            ; /  ... 16 times
7714  move.w  #0x3010,(A0)
7718  <pulse latch bit 1 low>
7728  move.w  #0x0015,(A0)
772C  <restore latch>
7736  move.w  D4,(A0)           ; 0x0010
```

**Sixteen iterations of an alternating two-value write, immediately after loading a 16-bit word into
`0x550000`.** The iteration count equals the word width, and the two constants differ in exactly the
low nibble vs bit 4 (`0x000F` / `0x0010`).

[INFERENCE - strong but not proven] This is a **clocked serial shift**: `0x550000` presents the
word, and the `0x0010` / `0x000F` pair toggles a clock line 16 times to shift it into the gate
array. What is *verified* is only the shape - the constants, the count of 16, and the ordering. Do
not name the individual bits until ND-14001 chapter 4 confirms them.

> **RETRACTED as a general claim, same day (see §2.4j).** The paragraph that stood here argued that
> because of this loop, `0x220000` is "not a parallel register file" and the §2.4 command-port model
> is wrong. **`0x71F8` disproves that** - it is a clean parallel transaction with four discrete
> command words and no loop at all. The 16x loop at `0x76E6` is a special case, not the general
> mechanism.
>
> Worth recording *why* this went wrong: it is the identical error this document already criticises
> in §2.4b - **generalising a whole port model from a single routine**. One routine showed a loop,
> so the port "became" serial; earlier, one routine mentioned MF-bus, so the ports "became" the
> MF-bus. Two routines is still not a survey. Check a second, structurally different caller before
> promoting any mechanism to a model.

**Full list of `0x220000` values observed so far**, across all routines carved to date:

| Value | Seen at |
|---|---|
| `0x0001` | 0x07A8, 0x0826 |
| `0x0005` | 0x072A (AOB path) |
| `0x0007` | 0x0788 (AOB timeout path) |
| `0x000F` | 0x70D0 family, and the 16x loop at 0x770E |
| `0x0010` | 0x770C, 0x7736 |
| `0x0015` | 0x7728, 0x7904 |
| `0x0017` | 0x78FC |
| `0x2018` | 0x774C |
| `0x300F` | 0x70D0 family |
| `0x3010` | 0x7714 |
| `0x400A`, `0x400C` | 0x70D0 family |

#### 2.4l MAJOR: `0x71F8-0x7C14` is the CONTROL STORE loader [IDENTIFIED 2026-07-27]

`0x741E` prints the string at `0x11898`, and the firmware names the subsystem itself:

```
$$C O N T R O L  S T O R E  E R R O R in buffered CI-bits 35 or 40.$
```

So the dense `0x33`/`0x66`/`0x88` cluster at `0x71F8-0x7C14` is **not only the octobus driver** - it
is (at least in large part) the **ND-5000 control-store / microcode loader**, the register-level
implementation behind the `LOAD-CONTROL-STORE` console command. "CI-bits 35 or 40" are bit positions
within the ND-5000's 128-bit microword, which is consistent with the ND-5000 being the target.

`0x741E` carved:

```
741E  <word parameter in D0>
742E  jsr     0x76E6                  ; address/setup phase (the 16x loop routine)
7434  bset    #2,(0x001144EE) ; -> 0x330000        ; gate on
7446  move.w  #0x0018,(0x00220000)                 ; the control-store command
744E  btst    #0,(0x00660000)         ; success?
7456  bne     0x7484                  ; yes -> done
7458  move.w  (0x00114560),D0 ; asr #8 ; and #7    ; extract bits 10..8
7464  cmpi.w  #1,D0 ; blt 0x7484                   ; below 1 -> stay quiet
746A  move.w  #-1,(0x001131E2)        ; latch the error
7472  move.w  #-1,(0x16,A6)           ; return -1
7478  lea     (0x11898).l,A0 ; jsr 0x1A78          ; print CONTROL STORE ERROR
7484  bclr    #2,(0x001144EE) ; -> 0x330000        ; gate off
7496  jsr     0x775A
749C  return (0x16,A6)                ; 0 = OK, -1 = error
```

New facts from this:

- **`0x330000` bit 2** gates a control-store operation (set before, cleared after).
- **`0x220000` command `0x0018`** performs it.
- **`0x660000` bit 0 = control-store operation OK.** This is the first identified bit of
  `0x660000`. A stub returning 0 here makes every control-store write report an error - which is
  *correct* behaviour with no ND-5000 present, and matches what the handoff already predicted.
- **`0x001131E2` is the sticky control-store error latch** (set to -1).

  > **ENRICHED 2026-08-02 (carve agent, `[V]`): this is THE selftest status word, and it is
  > ORDER-SENSITIVE.** It is not only a control-store latch - it is the single word that *both*
  > the boot console selftest summary (`0xF1A4`) and the octobus `RTEST` reply (`0x6632`) read.
  > One address, two readers.
  >
  > **`CMSYSPAR` (`0x0E`) and `CPURES` (`0x39`) CLEAR it.** So an `RTEST` issued after either one
  > returns `00 00 00` while the console still prints `Selftest failed ... 077FH`. Sent as the
  > FIRST command with nothing before it, `RTEST` returns `00 07 7F` - exact agreement.
  >
  > **Poisoned prior, do not re-derive:** the framing *"RTEST may read a different word, or the
  > ND-5000's status rather than the ACCP's"* is **withdrawn**. Both paths provably read one
  > address. The question was never *which word* but *when*.
  >
  > `RTEST` also clears `0x001144EC` and `0x001144EA` before replying - an undocumented side
  > effect on a status word, which is what made the first measurement lie.
  >
  > Writers already carved in this file: `0x746A` sets it to -1 (control-store error); `0x120C`
  > sets bit 15 (model not validated). Those are consistent - it is a status word with several
  > contributors, cleared by specific commands.
- **`0x00114560` is a new RAM variable**, outside the `0x1131xx` / `0x1143xx` / `0x1144xx` clusters
  seen so far. Bits 10..8 act as a **verbosity / message-level threshold** - the error is only
  printed when that field is >= 1. Worth knowing: a test can suppress or force this diagnostic.
- The caller at `0x74A6` invokes `0x741E` with **`parameter + 0x3FF0`**, so the parameter is an
  **address** in a space of roughly 0x4000 (16K) units - the expected order of magnitude for an
  ND-5000 control store.

**This partially rehabilitates the retracted serial-shift idea, in a narrower form.** `0x76E6` takes
the same word, writes it to `0x550000`, and clocks 16 times. If that word is a control-store
*address*, then 16 clocks = 16 address bits, and `0x76E6` is the address-shift phase of a
control-store access. [INFERENCE - the "address" role is now well supported by the `+0x3FF0` call
site; the *serial* mechanism is still not proven.] It remains false that `0x220000` is serial in
general - `0x71F8` settles that - but `0x76E6` specifically may well be.

#### 2.4n THE MICROWORD SHIFT ENGINE - `0x7776` / `0x77B6` [CARVED 2026-07-27]

This is the routine pair that settles the `0x220000` question properly, and it identifies the
control-store data buffer.

**`0x7776` - shift OUT (write a microword to the control store):**

```
777E  movea.l #0x220000,A1      ; clock/command port
7784  movea.l #0x550000,A2      ; data staging port
778A  lea     (0x001144F0),A3   ; RAM buffer
7790  lea     (0x10,A3),A4      ; buffer end = A3 + 16 bytes
779C  move.w  #8,D3             ;   per-word clock count
77A0  move.w  (A3),(A2)         ;   word -> 0x550000
77A2  move.w  D4,(A1)           ;     0x0010   \  8 times
77A4  move.w  D5,(A1)           ;     0x000F   /
77A8  bne     77A2
77AA  addq.l  #2,A3 ; cmpa A4 ; bne 779C        ; next word
```

**`0x77B6` - shift IN (read a microword back):** identical structure, with two differences that are
the giveaway - the clock pair is emitted in the **opposite order** (`0x000F` then `0x0010`), and a
command word **`0x2011`** is issued before each word is read back from `0x550000` into the buffer.

Three things fall out, all verified:

1. **`0x001144F0` is a 16-byte control-store data buffer = 8 words = 128 bits.** That is exactly the
   **ND-5000 microword width**. Independent corroboration that this subsystem targets the ND-5000
   control store (§2.4l).
2. **`0x220000` really does act as a clock port during shift sequences** - three separate routines
   now do it (`0x76E6`, `0x7776`, `0x77B6`), and the write/read direction is distinguished purely by
   the **phase order** of the `0x0010` / `0x000F` pair. That is a textbook clocked-shift idiom.
3. **Both models are true, and that was the error worth recording.** `0x220000` accepts discrete
   parallel command words (`0x71F8`, §2.4j) *and* serves as the clock line inside shift sequences.
   The retracted claim in §2.4h was wrong only in saying it was **not** a parallel register file -
   it is both, depending on phase. An emulator must distinguish "command word" from "clock edge" by
   context, which makes the value table in §2.4h the key artefact for Phase 6.

Control-store command words now attributable to this engine: **`0x2010`** (issued by `0x775A`
before a readback), **`0x2011`** (per-word during shift-in), **`0x2018`** (the operation itself,
`0x774C`), plus the clock constants `0x0010` / `0x000F`.

**`0x775A`, the shared exit path of both control-store paths (§2.4m), is a VERIFY step**: it writes
`0x2010` to `0x220000` and calls `0x77B6` - i.e. after every control-store access it **shifts the
128-bit microword back in** to `0x001144F0`. So the firmware read-back-verifies each write. An
emulator whose stub returns zeros for `0x550000` will make every verify mismatch - which is the
correct "no ND-5000 present" outcome, but means the buffer at `0x001144F0` is a useful place for a
test to look.

#### 2.4m Two parallel control-store paths - `0x741E` and `0x764E` [CARVED 2026-07-27]

`0x764E` is a near-twin of `0x741E` (§2.4l). Same command word `0x0018` to `0x220000`, same
`btst #0,(0x00660000)` success test, same error latch `0x001131E2 = -1`, same error string
`0x11898`, same cleanup call to `0x775A`. **Only two things differ:**

| | `0x741E` | `0x764E` |
|---|---|---|
| `0x330000` gate bit | **bit 2** | **bit 1** |
| status/level word | **`0x00114560`** | **`0x0011455C`** |

Those two RAM words are **4 bytes apart**, which reads as an indexed pair rather than two unrelated
variables. Combined with the error text naming **two** bit positions - `CONTROL STORE ERROR in
buffered CI-bits 35 or 40` - the natural structure is:

> **Two buffered control-instruction (CI) bit groups, one per path.** `0x330000` bits 1 and 2 select
> which group the `0x0018` command acts on, and each group has its own status word
> (`0x0011455C`, `0x00114560`).

[INFERENCE for the mapping of *which* bit selects CI-bit 35 vs 40 - the pairing itself is verified
by the code, the specific assignment is not.] This is exactly the kind of claim ND-14001 chapter 4
should settle.

**The status-word encoding is now partly decoded** (same layout in both):

- **bits 10..8** = a level / severity field. Extracted with `asr #8` then `and #7`.
- **bits 4..0** = a sub-field, only consulted by `0x764E`: when the level field equals 1, it
  additionally requires `(word and 0x1F) > 3` before reporting an error.
- `0x741E` uses the simpler rule: report only when level >= 1.

So `0x764E` is *more* selective about when it complains than `0x741E`, despite being otherwise
identical. A test that expects both paths to produce the same diagnostic for the same stub state
will be wrong.

`0x775A` is the shared post-operation routine called by both - a good next carve target, since it
is on the exit path of every control-store access.

#### 2.4j `0x71F8` - the canonical 32-bit read transaction [CARVED 2026-07-27]

This is the routine that settles the port model. It takes a 32-bit value in D0 and **returns a
32-bit value in D0**:

```
7204  move.w  D0,(0x00440000)      ; low half out
720A  swap    D0
720C  move.w  D0,(0x00550000)      ; high half out
7212  bclr    #6,(0x001144EF) ; write shadow -> 0x330001
7224  bset    #0,(0x001144EE) ; write shadow -> 0x330000
7236  rewrite 0x330001
7240  move.w  #0x300F,(0x00220000)     \
7248  move.w  #0x4016,(0x00220000)      |  four DISCRETE command words,
7250  move.w  #0x000F,(0x00220000)      |  no loop
7258  move.w  #0x8013,(0x00220000)     /
7260  move.w  (0x00550000),D0      ; high half back
7266  swap    D0
7268  move.w  (0x00440000),D0      ; low half back
726E  bset    #6,(0x001144EF) ; restore
7276  bclr    #0,(0x001144EE) ; restore
```

So the model in §2.4 was right after all: **`0x440000` / `0x550000` are a bidirectional 32-bit data
pair (low / high), and `0x220000` takes discrete command words.** This routine both writes and reads
the same two ports, which is direct evidence they are readable registers, not a shift-out staging
area.

New command words: **`0x4016`** and **`0x8013`** (the first observed with bit 15 set).

Latch bits gain meaning here: **`0x330001` bit 6** is cleared for the duration of the transaction
and restored after, and **`0x330000` bit 0** is set for the duration and cleared after. Both are
"transaction in progress" gates rather than data.

#### 2.4k `0x72A0` - the AOB single-word write, and the `0x330000` strobe [CARVED 2026-07-27]

```
72AC  tst.w   (0x00113138)          ; "skip the wait" flag
72B2  bne     72BE
72B4  btst    #1,(0x00660001)       ; AOB busy?
72BC  bne     72AC                  ; spin until clear
72BE  move    SR,-(SP) ; ori #0x2700,SR    ; mask all interrupts
72C4  move.w  D0,(0x00440000)       ; the word
72CA  bset    #6,(0x001144EE) ; write shadow -> 0x330000   <- STROBE HIGH
72DC  bclr    #6,(0x001144EE)                              <- shadow only, NOT written out
72E4  move    (SP)+,SR
```

**`0x330000` bit 6 is a write strobe.** The shadow is set, the whole byte written to the latch, then
the bit is cleared *in the shadow only* - so the next unrelated write to `0x330000` naturally
presents it low again. An emulator must treat the write-with-bit-6-set as the edge that commits the
word in `0x440000`; there is no explicit falling-edge write.

Note the interrupt masking: the strobe sequence runs at SR = 0x2700, so it is atomic against IRQ3/7.
And `0x00113138` reappears as the "skip the busy-wait" flag, matching §2.4.

#### 2.4i BOTH `0x33` bytes are write-only with RAM shadows [CARVED 2026-07-27]

§2.4c established `0x001144EF` as the shadow of `0x00330001`. There is a **second shadow**:

> **`0x001144EE` is the RAM shadow of `0x00330000`** - 28 references across the image
> (0x0FF0, 0x4D36/0x4D3C, 0x6894/0x68A4, and throughout 0x71F8-0x7C14).

Seen at 0x78EC: `move.b (0x001144EE),D2 ; bset #2,D2 ; move.b D2,(0x00330000)`, and restored at
0x790C. So the firmware never reads either `0x33` byte back from hardware - it maintains both in RAM
and writes the whole byte out. **Consequence for the emulator: reads of `0x330000`/`0x330001` are
never performed, so a stub's read value for those two addresses is irrelevant; but if you want to
know the latch state for a test, read the shadows at `0x001144EE`/`0x001144EF`, not the device.**

`0x330001` **bit 4** also exists, from `ori.b #0x5C,D0` at 0x7916 (bits 2, 3, 4, 6 set together) -
adding to the bit map in §2.4e.

**`0x78CA` and `0x795A` are a matched pair with a subtly different variant path.** Both check
`0x1131F8` against `0x5400` / `0x5500`, but:

| Routine | Variant path does |
|---|---|
| `0x78CA` (takes a word parameter in D0, calls 0x76E6 first) | `bclr #0` **and** `bclr #1` |
| `0x795A` | `bclr #0` **and** `bset #1` |

Same guard, opposite action on bit 1. Do not assume the two share an implementation.

#### 2.4o RESOLVED - `0x5400` / `0x5500` are CPU MODEL CODES, read as hex [CARVED 2026-07-28]

§2.4e recorded `0x001131F8` as a "variant / identity word" compared against `0x5400` and `0x5500`,
and said plainly: *"What `0x5400` / `0x5500` denote is NOT established - do not guess."* It is now
established, and the answer is mundane.

**They are the ND model number written in hex.** `0x5400` = ND-5400, `0x5500` = ND-5500,
`0x5800` = ND-5800, `0x5900` = ND-5900.

Proven by the print site at **0xA522**:

```
A522  lea     (0x12C86).l,A0    ; descriptor -> "$CPU model: ND-"
A534  jsr     0x1A0A            ; print the string
A53C  move.w  (0x001131F8),D0   ; the identity word
A542  jsr     0x1AE6            ; format it in the current radix and print
A548  jsr     0x1D32            ; CrLf
```

So the console line `CPU model: ND-5800` is literally that word rendered as hex digits. There is no
`ND-5` string anywhere in the image - a byte search finds none - because the number is formatted at
run time.

##### `ND-5800` is the firmware's hard-coded DEFAULT, not a hardware read [IMPORTANT]

At **0x11DA**:

```
11D2  move.b  #3,(0x001131F6)          ; model class byte
11DA  move.w  #0x5800,(0x001131F8)     ; DEFAULT = ND-5800, before asking anything
11E2  cmpi.w  #0x5800,D0               ; does the controller report 5800...
11E8  cmpi.w  #0x5900,D0               ; ...or 5900?
11EE  move.w  #1,(0x001131FA)          ;   yes -> mark the model VALID
11F6  move.w  D0,(0x001131F8)          ;   ...and adopt the reported value
11FC  (no match) -> 0x1131FA left at 0
1204  tst.w   (0x001131FA) / beq
120C  ori.w   #0x8000,(0x001131E2)     ; set the "model not validated" error bit
```

**A card with nothing attached prints `CPU model: ND-5800` every time**, because that is what the
firmware wrote into the word itself. A zero word would print `ND-0`.

##### The two MFbus console lines are CONSISTENT, not contradictory

An earlier review flagged this pair as self-contradictory - *"not found at Octobus stations 2-7"*
followed by *"has incorrect CPU model setting"*, on the grounds that the second implies one WAS
found. It does not:

1. The scan finds no MFbus controller on stations 2-7 (those stations are the MPM5 shared-memory
   window shared with the octobus controllers - so the scan is looking for shared memory).
2. Nothing answers, so `D0` matches neither `0x5800` nor `0x5900`.
3. The valid flag at `0x001131FA` stays 0.
4. `0x001131E2` bit 15 is set, and the firmware prints that the setting is incorrect.

Both lines are the correct output for a bare card. Neither is an artifact.

##### Correction to a same-day review

A review suggested `ND-5800` might be "simply what an all-zeros read decodes to", and that reading
was repeated here as near-certain before it was checked. **The condition was false** - the value is
written as a literal by the firmware. Recorded because the wrong version may already have been read.

Modelled in RetroCore as the `AccpCpuModel` enum
(`Nuget\HackerCorpLabs.Emulation.Machines.Accp\src\AccpCpuModel.cs`), with regression tests
asserting the printed line, the identity word, the valid flag and the error bit.

#### 2.4f METHOD NOTE - why large parts of this image look like undefined bytes [ROOT-CAUSED 2026-07-27]

Anyone continuing this carve will hit blocks of `<undefined> 0xNN` in the middle of obviously-real
code (the whole `0x5D00-0x6882` command-handler region, for example). **The bytes are fine. Ghidra's
flow analysis is what stops.**

`octo.bin` is PLANC-MC, so it uses the **skip return**: a routine that returns normally returns to
`RETLINK + 2`, and the two bytes right after every call are an error slot, usually
`4E D5` = `jmp (A5)`. Ghidra sees an unconditional indirect jump, treats it as a flow terminator,
and stops disassembling. Every single PLANC call site truncates the listing. That is why the image
looks half-disassembled.

This matches the `ghidra-planc` skill's own diagnosis verbatim: *"the dominant failure mode is NOT
bad bytes, it is Ghidra treating `jmp (A5)` as an unconditional terminator and stopping. Restart
disassembly at each error slot + 2."*

**The image already carries `[planc-auto]` comments, so `PlancAnnotate` has been run on it - but
`PlancFixFlow` evidently has not.** Before the next serious carving session, run the ND.PLANC
scripts against `octo.bin` in the patched Ghidra install (`C:\Utils\ghidraRun.bat`):

1. `PlancFixFlow` (loops until stable - this is the one that matters here)
2. `PlancAnnotate`
3. `PlancSetupTypes`
4. `PlancApplyConvention`
5. `PlancFrameTypes`

Doing that first will probably surface more code - and possibly more chip selects - than any amount
of manual address-by-address work. Manual workaround in the meantime: force disassembly at
`<error slot> + 2` (each `4E D5` is 2 bytes).

Confirmed for this image: A5 is loaded with `0x000115AE` at 0x0C88, and the epilogue form
`movea.l (SP)+,A6 ; movea.l (SP)+,A2 ; jmp (0x2,A2)` appears at 0x1714-0x1718 - skip distance 2,
exactly as the skill describes.

#### 2.4g Boot self-test reporting path @ 0x0E92 [CARVED 2026-07-27]

Immediately after `DuartInit`, the firmware initialises its diagnostic flags and then reports the
RAM result:

```
0E92  move.w  #1,(0x0011314A)
0E9A  clr.w   (0x001143B6)      ; trace flag OFF
0EA0  move.w  #1,(0x001143AA)
0EA8  clr.w   (0x001143B4)      ; trace flag OFF
0EAE  clr.w   (0x00113148)
0EB4  lea     (0x1171E).l,A0    ; print a string
0EC6  jsr     0x1A0A
0ECE  tst.l   (0x0011312A)      ; <-- the RAM first-half error count
0ED4  bne     0x0EFC            ; non-zero -> the error report path
```

Three things worth having:

1. **Both trace flags (`0x1143B4`, `0x1143B6`) are explicitly cleared at boot**, so the octobus
   narration described in §2.4b is OFF by default and must be switched on by a console command. The
   command that sets them has not been found yet - the candidate write sites are in `0x5D00-0x6882`,
   which is exactly the region blocked by the flow problem in §2.4f. **Run `PlancFixFlow` before
   hunting further**; searching the current listing is wasted effort.
2. **`0x0011312A` is tested with `tst.l`** - independent confirmation that the first-half error
   count is a 32-bit quantity (§2.1), from a completely different routine than the one that writes it.
3. `0x0ECE` is the branch that chooses between the `ACCP local ram test OK` message and the failure
   report. That makes **`0x0ECE` the exact instruction a Phase 2 test should aim at** if it wants to
   assert on the decision rather than on the console text.

#### 2.4e `0x795A` carved - it is a latch DISABLE, not a re-init [CARVED 2026-07-27; NAMED **STOPMIC** 2026-08-02]

> **Name settled 2026-08-02:** `0x795A` is **STOPMIC** (manual 5.3.24), called by
> `Cmd24_StopMicroprogram @ 0x91C6`. The latch-disable behaviour carved below is what stopping the
> microprogram *does*; it is not a separate routine. Section 2.4c carried the old "re-init" reading
> until 2026-08-02 - see the correction banner there.

`0x795A` was expected (§2.4c) to be a big octobus-controller re-initialisation routine, because both
master-clear paths call it. **It is not.** It is a short routine that manipulates the `0x330001`
control latch, and it has an obvious counterpart:

| Routine | Bits 2 and 3 of the latch | Reading |
|---|---|---|
| `0x795A` | **cleared** | DISABLE |
| `0x79E4` | **set** | ENABLE |
| `0x79BC` | calls `0x78B2` (if `0x113138` == 0) then `0x79E4`, then sets `0x1143AC` = 1 | enable wrapper |

Both routines use the same **two-phase write**, and this is the important structural detail:

```
D0 = shadow(0x1144EF)
D1 = D0 with bits 3 and 1 cleared
move.b D1,(0x00330001)        ; phase 1 - strobe bits low
D0 = D0 with bits 2,3 set (0x79E4) or cleared (0x795A)
move.b D0,(0x001144EF)        ; update shadow
move.b D0,(0x00330001)        ; phase 2 - real value
```

So **bits 1 and 3 must be driven low before the latch is reconfigured**. Any model of this latch
has to accept the intermediate write without treating it as a real state change.

Consolidated `0x330001` bit meanings (all [CARVED] except the reading in the last column):

| Bit | Behaviour | Reading |
|---|---|---|
| 0 | cleared only when `0x1131F8` is `0x5400` or `0x5500` (0x799C) | variant-conditional, unknown |
| 1 | pulsed low/high in master clear; forced low during reconfiguration | strobe |
| 2 | cleared by `0x795A`, set by `0x79E4` | enable (paired with bit 3) |
| 3 | cleared by `0x795A`, set by `0x79E4`; forced low during reconfiguration | enable (paired with bit 2) |
| 6 | pulsed low/high at 0x07E4 / 0x07F2 | strobe |

**`0x001131F8` is a variant / identity word** [PARTLY OPEN]. `0x795A` compares it against `0x5400`
and `0x5500` and only then clears latch bit 0. The same word is *printed to the console* at 0x10D4
(`move.w (0x001131F8),D0` then `jsr 0x1AE6`, the number formatter). So it is operator-visible and it
changes hardware behaviour. What `0x5400` / `0x5500` denote is **not** established - do not guess;
they are plausibly board or gate-array revisions, but nothing in the image proves that yet. Other
references: 0x10D6, 0x115C, 0x1190, 0x11B0, 0x11DE, 0x11F8, 0x66FC, 0x792A/0x7934, 0x7B30/0x7B3A.

#### 2.4d Exception frame block - confirmed byte-for-byte [VERIFIED 2026-07-27]

Disassembled at 0x0898-0x08DE; matches the previously documented layout exactly:

| Address | Width | Contents |
|---|---|---|
| `0x00113112` | word | fault code: `0x20` unused TRAP (0x0898), `0x2A` TRAP #10 (0x08AC), `0x4D` reserved vector (0x08B8) |
| `0x00113118` | word | SR from the exception frame |
| `0x0011311E` | long | SP |
| `0x00113122` | long | PC from the exception frame |
| `0x00113126` | long | A6 |

The TRAP #10 handler first does `addi.l #-2,(2,SP)` - it rewinds the stacked PC by 2 so the reported
PC points at the trapping instruction rather than past it. An emulator-side test that decodes this
block must not "helpfully" re-adjust.

The fault path installs its **own** frame base and stack: `A6 = 0x112800`, `SP = 0x112C00`
(0x08EC-0x08F2), separate from the normal `A6 = 0x110000` / `SP = 0x112000`. So a fault can still be
reported when the main stack is what got corrupted - and it means a test asserting "SP is unchanged"
after a fault would be wrong.

---

### 3. 68000 exception vector table (0x000-0x0FF) [VERIFIED]

Fully populated in the image and now typed in Ghidra (0x000 as `undefined4`, 0x004-0x0BF as
`pointer`, all handlers made into named functions).

| Vector | Addr | Target | Name given |
|---|---|---|---|
| 0 | 0x00 | 0x00113FFC | initial SSP |
| 1 | 0x04 | **0x0BD6** | `Vec_Reset_Entry` |
| 2 | 0x08 | 0x400 | `Vec02_BusError` |
| 3 | 0x0C | 0x40C | `Vec03_AddressError` |
| 4 | 0x10 | 0x418 | `Vec04_IllegalInstruction` |
| 5 | 0x14 | 0x436 | `Vec05_DivideByZero` |
| 6 | 0x18 | 0x442 | `Vec06_ChkInstruction` |
| 7 | 0x1C | 0x44E | `Vec07_TrapV` |
| 8 | 0x20 | 0x45A | `Vec08_PrivilegeViolation` |
| 9 | 0x24 | 0x466 | `Vec09_Trace` |
| 10 | 0x28 | 0x472 | `Vec10_LineA_Emulator` |
| 11 | 0x2C | 0x490 | `Vec11_LineF_Emulator` |
| 12-23 | 0x30-0x5C | 0x8B8 | `Vec12_23_ReservedStub` (fault code 0x4D) |
| 24 | 0x60 | 0x4AE | `Vec24_SpuriousInterrupt` |
| 25 | 0x64 | 0x4BA | `Vec25_AutoIrq1` |
| 26 | 0x68 | 0x4C6 | `Vec26_AutoIrq2` |
| 27 | 0x6C | 0x510 | `Vec27_AutoIrq3` |
| 28 | 0x70 | 0x694 | `Vec28_AutoIrq4` |
| 29 | 0x74 | 0x796 | `Vec29_AutoIrq5` |
| 30 | 0x78 | 0x7A8 | `Vec30_AutoIrq6` |
| 31 | 0x7C | 0x826 | `Vec31_AutoIrq7_NMI` |
| 32-47 | 0x80-0xBC | 0x898 | `Vec_TrapUnusedStub` (fault code 0x20) |
| **42** | **0xA8** | **0x8A4** | `Vec42_Trap10_Handler` (fault code 0x2A) |
| 48-63 | 0xC0-0xFF | 0 | unused |

**TRAP #10 is the only distinguished TRAP.** (The ENCOS PIOC-OS image uses TRAP #2 - do not
carry that assumption across.)

Vector 30's target at 0x7A8 was raw bytes and Ghidra had mis-split an instruction at 0x7FC;
both were cleared and re-disassembled by hand.

#### 3.1 `FaultRecordAndPanic` @ 0x08C4 [VERIFIED]

Every exception stub loads a fault code into `0x00113112` and branches here, which records:

| Address | Content |
|---|---|
| 0x00113112 | fault code (0x20 unused TRAP, 0x2A TRAP #10, 0x4D reserved vector, ...) |
| 0x00113118 | saved SR (word at SP) |
| 0x0011311E | saved SP |
| 0x00113122 | saved PC (long at SP+2) |
| 0x00113126 | saved A6 (PLANC frame pointer) |

then `movem.l` of D0-D7/A0-A6 and a call to 0x8EA. This is the DOMINOS-style fatal-event
record, and it is what prints `6 8 0 0 0   T R A P :`.

---

### 4. PLANC conventions in THIS image - they differ from ENCOS [IMPORTANT]

The image is PLANC-MC compiled (`jmp (A5)` error unwinds, A6 bump-allocated frames, the
`movea.l (SP)+,A6 ; movea.l (SP)+,A2 ; jmp (2,A2)` skip-return epilogue). The
`tools/ghidra-planc` scripts apply. **But three details are NOT the ENCOS ones:**

1. Callers stage arguments through **`(A6)`** - the next-free cursor - not through `(0x4,A6)`.
2. The first parameter lands at **+0x14**, not +0x12.
3. Array descriptors are the **12-byte** form `{long origo, long lowerLimit, long upperLimit}`,
   i.e. 4-byte words (PLANC-MC version F or later), not the 8-byte word-limit form.

Canonical call site, at 0x0EDE:

```
0EDE  lea      (0x0001176C).l,A0     ; the descriptor
0EE4  movea.l  (A6),A2               ; A2 := callee frame  (NOT (0x4,A6))
0EE6  lea      (0x14,A2),A1          ; first parameter slot
0EEA  move.l   (A0)+,(A1)+           ; copy 12-byte descriptor
0EEC  move.l   (A0)+,(A1)+
0EEE  move.l   (A0)+,(A1)+
0EF0  jsr      ConsPrintString
      jmp      (A5)                  ; 2-byte PLANC error slot
```

Descriptors sit immediately before their text, e.g. `0x1176C = {0x11778, 0, 0x16}` for the
23-character string `"$ACCP local ram test OK"`.

---

### 5. The ACCP console command table @ 0x130FE [VERIFIED]

> **Now superseded in detail by
> part 3 of this file**, which adds what this section does
> not have: every command's **code**, its **full parameter syntax**, and its **handler
> address**. The dispatch is a **linear compare chain at 0x227E-0x2746** on the code word held
> in RAM at `0x00113334` - not a jump table, which is why no table was ever found. Entry
> points: `AccpMainInitAndRunConsole` 0x205C, `ConsoleCommandLoop` 0x21A6,
> `ConsoleReadCommandLine` 0x274E, `MatchCommandNamePrefix` 0x2D36. All 42 handlers are now
> named `Cmd<code>_<Name>` in the database; `HELP` (0x0C) is inline at 0x22D2 and has no jsr.
> Runtime error messages: table `0x12E9C`, indexed `(errcode - 1000) * 12`, text at 0x12F5C.

**43 entries, 14 bytes each**, laid out as:

```
+0x00  word  command code
+0x02  long  string virtual origo
+0x06  long  lower limit (always 0)
+0x0A  long  upper limit (length - 1)
```

The fit is exact: `(0x13358 - 0x130FE) / 14 = 43`, the table ends precisely where the text
begins at 0x13358, and the text contains exactly 43 command strings. That is the proof the
record layout is right.

The command set, in table order (angle brackets are the prompt text for each parameter):

| # | Command |
|---|---|
| 1 | `CHECK-ALIVE` |
| 2 | `VALUE <Convert number>` |
| 3 | `CONTINUE-MICROPROGRAM` |
| 4 | `DUMP-LOCAL-MEMORY <Address> <Wordsize /halfword/>` |
| 5 | `HELP <Command>` |
| 6 | `LOAD-AOB16 <Data (16)>` |
| 7 | `LOAD-AOB32 <Data (32)>` |
| 8 | `LOAD-CONTROL-DECODER <Data (16)>` |
| 9 | `LOAD-CONTROL-STORE <CS address> <127-112> <111-096> <095-080> <079-064> <063-048> <047-032> <031-016> <015-000>` |
| 10 | `LOAD-MAR <CS address>` |
| 11 | `LOAD-MIR <127-112> <111-096> <095-080> <079-064> <063-048> <047-032> <031-016> <015-000>` |
| 12 | `LOAD-MODE-REGISTER < Upper byte> < Lower byte>` |
| 13 | `LOOK-AT-CONTROL-CACHE <CC address>` |
| 14 | `LOOK-AT-CONTROL-STORE <CS address>` |
| 15 | `LOOK-AT-LOCAL-MEMORY <Address>` |
| 16 | `LOOK-AT-MEMORY <Address>` |
| 17 | `LOOP-ON-NEXT-COMMAND <Supress output text ?>` |
| 18 | `MAIN-FORMAT <BASE (HEX,OCT,DEC)>` |
| 19 | `READ-ACCP-STATUS` |
| 20 | `READ-AIB16` |
| 21 | `READ-AIB32` |
| 22 | `READ-ECO-LEVELS` |
| 23 | `READ-MIR` |
| 24 | `RECEIVE-MULTIBYTE-OCTOBUS` |
| 25 | `RECEIVE-OCTOBUS` |
| 26 | `RESET-CPU` |
| 27 | `RESTART-MICROPROGRAM <CS address> <Interval>` |
| 28 | `RUN-LONG-SELFTEST <Loop selftest? (y/n)>` |
| 29 | `RUN-SHORT-SELFTEST <Loop selftest? (y/n)>` |
| 30 | `SHOW-REGISTERS` |
| 31 | `SEND-KICK-OCTOBUS <DESTINATION><Kick value (process)>` |
| 32 | `SEND-MULTIBYTE-OCTOBUS <Destination><Subprocess><Message>` |
| 33 | `SEND-OCTOBUS <Data (16)>` |
| 34 | `SET-CLOCK-SPEED <Clock speed (Slow,Normal,Fast)>` |
| 35 | `SET-INTERRUPT-MASK <Interrupt mask>` |
| 36 | `SET-KICK-TIMEOUT <Kick timeout (ms)>` |
| 37 | `SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` |
| 38 | `START-MICROPROGRAM <CS address>` |
| 39 | `STOP-MICROPROGRAM` |
| 40 | `TEST-BUFFERS <ASR/AOB>` |
| 41 | `TEST-BUSLOOP <Test-pattern>` |
| 42 | `TEST-MEMORY <From address> <To address>` |
| 43 | `TRACE-COMMUNICATION-DATA <Trace Octobus communication to consol? (y/n)>` |

Smaller answer tables follow the same `{code, descriptor}` record shape:

| Address | Contents |
|---|---|
| 0x138F0 | `YES`, `NO` |
| 0x13910 | `HEXADECIMAL`, `DECIMAL`, `OCTAL`, `BINARY`, `ASCII`, `NONE` |
| 0x13988 | `FAST`, `NORMAL`, `SLOW` |
| 0x139C4 | `WORD`, `HALFWORD`, `BYTE` |

Error text for the command scanner lives at 0x12F80-0x13065: `Illegal format`, `Ambiguous`,
`Illegal answer`, `Ambiguous answer`, `No such channel`, `Odd address illegal`,
`Too long line`, `Illegal character in number`, `Illegal format on numeric input`,
`Deposit not permitted`, `Data did not store`, `Max address exceeded`.

#### 5.1 What the command set proves about the microword

`LOAD-CONTROL-STORE` and `LOAD-MIR` both take **eight 16-bit fields labelled 127-112 down to
015-000**. That is a **128-bit microword**, stated by the firmware itself - matching the
ND-5800 figure already recorded in `nd500-microcode-files`, and settling it for this card.

---

### 6. Embedded selftest microcode @ 0x13C30 [LAYOUT NOW PROVEN - see 6.1]

#### 6.1 SOLVED 2026-07-27 - the loader has been read, and section 6 below is WRONG

`LoadSelftestMicrocodeIntoControlStore` @ **0xB16E** is the only consumer of the blob (six
`lea (0x13C18).l` sites, all inside it). It was invisible to earlier searches because nothing
references the blob address `0x13C30` directly - the code references the **descriptor**.

**The descriptor is at 0x13C18, not 0x13C1C** (section 6 below is off by four bytes), and it
is a PLANC **TWO-DIMENSIONAL** array descriptor, **12 bytes per dimension**:

| Offset | Value | Meaning |
|---|---|---|
| +0x00 | `0x00013C30` | origo |
| +0x04 | 0 | dim 1 lower |
| +0x08 | `0x0BFF` | dim 1 upper - **3072 microwords** |
| +0x0C | 8 | dim 2 stride |
| +0x10 | 0 | dim 2 lower |
| +0x14 | 7 | dim 2 upper - **8 elements each** |

The loop is `for i := 0 until 0xBFF`; the element address is `i` -> `asl.l #1` -> `i*2` ->
`PlancIntegerMultiply_IMU(8, i*2)` -> **`i*16` bytes**, then `A3 = *(0x13C18) + i*16` and the
inner descriptor `{A3, 0, 7}` is handed to the callee. Staging buffer `0x001144F0`.

**8 elements spanning 16 bytes == eight 16-bit fields == a 128-BIT MICROWORD**, matching
`LOAD-CONTROL-STORE <CS address> <127-112> ... <015-000>` field for field. word[0] = bits
127-112, word[7] = bits 015-000.

**The "sequence number" reading below is WRONG.** Bytes +0x0C..+0x0D are not a separate field
at all - they are **word index 6, i.e. microword bits 031-016**, which merely happens to
increment from record to record. All 16 bytes are microword; nothing is missing and nothing
needs reassembling. In the samples checked, word[7] (bits 015-000) is always zero.

Side benefit: this **proves 12-bytes-per-dimension PLANC array descriptors on MC68000**, which
ND-20034 states for the ND-500 and which the `ghidra-planc` skill records as unverified on
68000.

#### 6.2 Superseded original text

A PLANC array descriptor at **0x13C1C** reads `{origo = 0x13C30, lower = 0, upper = 0x0BFF}` -
**3072 elements**. The elements are 16 bytes each, so the blob occupies
**0x13C30 - 0x1FC2F (0xC000 bytes)**; everything above 0x1FC30 in the image is zero.

This is what the string `Loading control store with selftests...` (0x119A5) loads.

Record shape, from the data:

- bytes **+0x0C..+0x0D** hold a **strictly increasing 1-based sequence number**: entry 0 -> 1,
  entry 1 -> 2, ... verified at three widely separated points (0x13C30 -> 1, 0x1A000 -> 0x63E,
  0x1C01C -> 0x83F, each matching `(offset - 0x13C30)/16 + 1` exactly, with no gaps or
  repeats across 513 consecutive records checked).
- The other 14 bytes are the microword payload. **[INFERENCE]** I have not proven the exact
  bit-to-field mapping, and 14 bytes is 112 bits against a 128-bit microword, so either the
  sequence number field is itself part of the word (a next-address field) or the loader
  reassembles the missing 16 bits from elsewhere. A next-address field is very unlikely to be
  strictly sequential across 513 words of real microcode, which is why the sequence-number
  reading is preferred - but this is not settled. **Do not build an emulator table on the
  payload layout until the loader routine has been read.**

Value to the ND-5000 work: this is a real block of control-store content with a known load
path, and the only ND-5000 microcode binary in the repo other than the ND5800 control store.

---

### 7. What was done in the Ghidra database

#### 7.0 STATE AS OF 2026-07-28 - the image is fully disassembled and fully named

The `noreturn`-flag blocker was cleared and the ND.PLANC scripts run. Section 7.1 below is
superseded.

| Measure | Before | Now |
|---|---|---|
| Functions | 187 | **279** |
| Still `FUN_xxxxxxxx` | 159 | **0** |
| Error slots annotated | 268 | **1062** |
| `jmp (A5)` unwinds | 170 | **832** |
| `__planc` applied | 0 | **117** (leaf/NATIVE routines correctly excluded) |

**How the GUI blocker was beaten.** Four of the six ND.PLANC scripts would not appear in the
Script Manager - cause never established; the files are present, tagged correctly, compile
clean against the 12.0.4 jars, and the directory is registered and enabled. They were run
**headless** instead:

```
analyzeHeadless.bat E:\Dev\Repos\Ronny\RetroGhidra\ETH_II ND_ETH_II ^
  -process octo.bin -noanalysis -scriptPath <dir> -postScript <Script>.java
```

The scripts use `askYesNo` / `askChoice`, which throws in headless mode. Fix: copy the script,
replace the ask with a constant, rename the class to match the new filename. Ghidra must be
closed (project lock). Copies were kept out of `ghidra_scripts` so the originals stay clean.

Two runs were needed, and the ORDER MATTERED: `PlancFixFlow` had already been run once at
17:33 on 2026-07-27 *before* the `noreturn` flag was cleared, and could only reach what was
visible then (368 fallthroughs, 333 sites). Re-run after clearing the flag it did **721
fallthroughs, 693 sites** - nearly double.

#### 7.1 Naming and annotation completed

- **All 279 functions named.** Three are ND's own, recovered verbatim from the error strings
  that name them: **`ND100TRANSMIT`** @0x6AA6, **`Areceive`** @0x10832, **`MFCRECEIVE`**
  @0x14B4.
- The **selftest suite named itself** - each test loads its own title string:
  `Selftest_BusTest`, `_MirTest`, `_AluVerify`, `_Registers`, `_Tsb`,
  `_ControlStoreSample`/`_Memory`, `_ControlCacheSample`/`_Memory`,
  `_InstructionAndDataCache`, `_MargSargLargAib`, `_StartStopMicroprogram`,
  `_Aap_NotImplemented`.
- **All 43 console command handlers** named `Cmd<code>_<Name>`.
- **All 17 OBCON handlers** named `ObconFn<code>_*`.
- **26 hardware registers** labelled `HW_*`, **32 RAM globals** labelled `g_*`, **14 tables
  and message descriptors** labelled `tbl_*` / `txt_*` / `desc_*`.
- **A master plate comment at address 0** carrying the memory map, the hardware table, the
  software structure, the PLANC convention differences and the script warnings.

**Roughly 85 routines print nothing and touch no hardware.** Rather than invent plausible
names they were given **region + address** - `Obcon_Helper_10C38`, `NumberFormat_Helper_412E`,
`ControlStore_Helper_74A6`, `PlancLeafRuntime_112DE`. The region is evidence-backed (from
neighbours and callers); the address keeps it honest. Treat those names as "located, not
understood".

#### 7.2 NO SYMBOL TABLE - settled definitively

Proven two ways on 2026-07-28: the tail **0x1FC30-0x1FFFF is 976 bytes of pure zeros**, and
there is **no printable run of 6 or more characters anywhere above the microcode blob**.
Unlike ENCOS (241 linker symbols at file offset 0x663E0) this image carries none. Open
question 5 is closed.

#### 7.3 Superseded original text

- Vector table typed and every handler created and named (section 3).
- `FaultRecordAndPanic` @0x8C4 named, with a plate comment listing the fault-record fields.
- `ConsPrintString` @0x1A0A, `ConsPutCharQueued` @0x1BF6, `ConsPutCrLf` @0x1D32,
  `DuartTxServiceBothChannels` @0x1D4C named, the last two carrying the DUART map as a plate
  comment.
- 0x7A8 and 0x7FC re-disassembled after clearing a bad split; 0x1C38 disassembled.

**Not done, and it needs the Ghidra GUI:** the five `tools/ghidra-planc` scripts
(`PlancFixFlow`, `PlancAnnotate`, `PlancSetupTypes`, `PlancApplyConvention`, `PlancFrameTypes`)
have not been run on this program. They cannot be driven over MCP. Large parts of the image are
still undisassembled bytes. **Run them before the next analysis pass** - remembering the
convention differences in section 4, which mean `PlancFrameTypes` field offsets from the ENCOS
work should not be trusted here without rechecking.

---

### 8. Open questions

**Three of the original five are now ANSWERED. Kept here with their answers so nobody
re-opens them.**

1. **[OPEN]** The hardware addresses in section 2.3 - which one is the octobus AIB, AOB,
   control/status, and which is the BADAP. Partly advanced: `0x880000` = message/kick read
   port with `0x660001` bit 2 as data-available (2.4b), `0x440000` = AOB data with the
   `0x330000` bit-6 strobe (2.4k), `0x220000` = general command/function port. The BADAP is
   still unassigned. ND-14001 chapter 4 is now transcribed - see
   `OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`.
2. **[OPEN, still likely]** Whether DUART channel B is the ND-100 serial link. Evidence
   unchanged: the `SET-SERIAL-LINE` command and `Illegal kick ... received over serial line`.
   Nobody has yet traced which TX ring feeds which channel end to end, which is what would
   settle it.
3. **[ANSWERED - see 6.1]** The microword payload layout. The loader
   `LoadSelftestMicrocodeIntoControlStore` @0xB16E has been read: the descriptor at **0x13C18**
   is two-dimensional, `{3072} x {8}`, 12 bytes per dimension, so each 16-byte record is
   **eight 16-bit fields = one 128-bit microword**. The "sequence number" reading was wrong.
4. **[ANSWERED]** TRAP #10 is **not** a kernel or monitor entry - this image has no kernel.
   The handler at 0x08A4 does `addi.l #-2,(2,SP)` to back the saved PC onto the trapping
   instruction, stores fault code `0x2A` at `0x00113112`, and falls into
   `FaultRecordAndPanic`. It is the PLANC runtime's fatal-error trap, the counterpart of the
   `6 8 0 0 0   T R A P :` report. There is **no `trap #2` anywhere in the image** - all 11
   `4E 42` byte matches are inside the string region or at odd addresses. See
   part 6 of this file.
5. **[ANSWERED - NO]** There is **no embedded ND linker symbol table**. Searched: the
   candidate record patterns all land above 0x13C30, i.e. inside the selftest microcode blob,
   where they are coincidence. Unlike ENCOS (241 symbols at 0x663E0), every name in this image
   has to be invented - except the five the error strings give outright: `MFCRECEIVE`,
   `ND100TRANSMIT`, `Areceive`, `DOREC_MULTI_OCTO`, `DOSEND_MULTI_OCTO`.

#### 8.1 Current next targets

- Run the ND.PLANC scripts (section 7) - still the single highest-value action, and still
  blocked on the GUI.
- `0x795A` / `0x78CA` (2.4e) - the matched pair with opposite bit-1 behaviour.
- Trace a TX ring to DUART channel B to settle question 2.
- ~~Find which console command sets the trace flags~~ **`0x001143B4` SOLVED**: it is
  `TRACE-COMMUNICATION-DATA` (command 0x3C), handler `Cmd3C_TraceCommunicationData` @0x9D62 -
  `move.w #1,(0x001143B4)` at 0x9D84 for "y", `clr.w` at 0x9D8E for "n". Those are the **only
  two writes** in the image; every other reference is a read. So the oracle in 2.4b is
  reachable from the console: turn tracing on and the firmware narrates its own octobus
  traffic as ` from SAMSON` / ` to SAMSON`.
  **`0x001143B6` is a SECOND, different flag** - set at 0x650A and cleared at 0x6538, inside
  0x5D00-0x6882, by a routine that is not one of the 43 command handlers. Owner still unknown;
  do not treat the two flags as interchangeable.

  > **USE THIS FIRST on any octobus protocol question** (RetroCore side, 2026-07-31).
  > `TRACE-COMMUNICATION-DATA YES` sets `0x001143B4` (the `move.w` at `0x9D84`) and **eleven sites
  > read it, covering BOTH directions**: receive at `0x1095A` / `0x10A3A`, transmit at
  > `OctoTxTracePrint_fromACCP_A` `0x11078` and `_B` `0x110C0`. With it on, the card **narrates its
  > own octobus frames to the console**. That is the cheapest possible first move on a protocol
  > question - it turns a static-analysis argument into a reading.
  >
  > Note this flag is **only reachable from the CONSOLE**, not over CMD 3: sending `0x3C` over the
  > octobus replies `FF 01` and leaves the flag untouched (see the shared-enum refutation).
  >
  > **SCOPE, and a correction to my own advice (2026-08-01).** I suggested tracing as a way at the
  > `0x220000` function codes. **It will not work, and that suggestion is withdrawn.** All eleven
  > readers of the flag are `0x628`, `0x706`, `0xEAA`, `0x1EFA`, `0x592E`, `0x9D88`, `0x9D90`,
  > `0x1095A`, `0x10A3A`, `0x11078`, `0x110C0` - **none is inside the control-store / signature
  > region `0x71F8`-`0x7C14`**, so `0x220000` traffic is completely silent to the tracer.
  >
  > **What the trace DOES cover is wider than "octobus" though - three paths, not one:**
  >  - octobus inbound - `" to ACCP$"`, descriptor at `0x123EA`.
  >  - octobus outbound - `" from ACCP"`.
  >  - **AOB to the CPU** - `" to SAMSON$"`, descriptor at `0x123BA`, printed at `0x0636`
  >    immediately after the `btst.b #1,(0x00660001)` AOB-busy check.
  >
  > **Plus a kick-timeout diagnostic**, emitted when the `0x001131DC` countdown expires with
  > `0x660001` bit 1 still set:
  > `"$K I C K   T I M E O U T : "` at `0x122FE`, then
  > `"AOB not read by microprogram within timeout."` at `0x12326`.
  >
  > ~~**That timeout message is directly useful to the kick work**: a mis-framed kick word - one that
  > is not `0o1005nn`, so `OCB_MES_K` never fast-paths it - should surface from the ACCP end as
  > exactly this message.~~
  >
  > **TESTED AND REFUTED, SAME DAY - a mis-framed kick does NOT produce the timeout.** Driving a
  > bare `0x0003` and a framed `0x8143` through the injection harness and reading AOBF afterwards:
  >
  > ```
  > kickWord  dispatched  AOBF after
  >   8143          True           0     framed - collected and dispatched
  >   0003         False           0     bare   - collected and DROPPED
  > ```
  >
  > **The microprogram reads the word either way.** AOBF clears for the bare kick too; it is only
  > the DISPATCH that fails, because `OCB_MES_K` does not fast-path it and `OCB_DEC_K`'s index
  > lands nowhere useful.
  >
  > **So the ACCP never times out on a mis-framed kick** - from its side the word was collected
  > normally. And the CPU never acts on it. **Neither end reports anything: a mis-framed kick is
  > silently swallowed.** That is worse than the prediction, and it means there is no diagnostic
  > anywhere for this failure - which is exactly why our own station now logs unrecognised kick
  > numbers unconditionally (gap G6).
  >
  > Guarded by `MailboxClrKickTests.MisframedKick_IsLeftUncollectedInAob_MatchingTheAccpTimeoutCondition`
  > - the name records the hypothesis, the assertions record what actually happens.
- **0x5D00-0x6882 is NOT the command-handler region.** The dispatch map (see
  part 3 of this file) puts every handler at 0x333A, 0x353E,
  0x3A12, 0x400A, 0x4076, or in 0x7EAE-0xADE0. Nothing dispatches into 0x5D00-0x6882, so
  section 2.4f's passing description of it as "the whole command-handler region" is wrong.
  **IDENTIFIED**: the only meaningful string descriptor loaded anywhere in that region is at
  0x6770, and it resolves to `"$Undefined ACCP command received:$"` (0x12B44). So
  **0x5D00-0x6882 handles ACCP COMMANDS ARRIVING FROM THE ND-5000 MICROPROGRAM** - a second,
  separate command interface, the one the string `"Illegal ACCP command received from
  microprogram: "` (0x11E95) also belongs to. The only other descriptors there are `"Octal"`
  and `"Hexadecimal"`.
  **[INFERENCE, not proven]** This would explain the sparse console command codes: a single
  command-code enum shared between the console and the microprogram interface puts the
  missing codes (04, 05, 08, 0B, 0D-1E, 43-45) on the microprogram side. A
  `cmpi.w #imm,(abs).l` scan found **no** compare chain in this region, so its dispatch uses
  another form and has not been located - which is what would settle it.
  Likewise the second trace flag `0x001143B6` is set at 0x650A and cleared at 0x6538, i.e.
  inside this region, so it is plausibly the ACCP-command counterpart of `0x001143B4`.
  Unproven.

---

### Related

- `SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
- `SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md`
- `SINTRAN\ND5000\ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md`
- `SINTRAN\ND5000\OCTOBUS-TEST-PROTOCOL-RE.md` (the OMD-0 protocol this card answers)
- `tools\ghidra-planc\README.md` and skill `ghidra-planc`

---

# Part 2 - originally `ACCP-HARDWARE-ADDRESS-MAP-2026-07-27.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## ACCP hardware address map - full sweep

**Date**: 2026-07-27
**Image**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
**Method**: exhaustive byte sweep of the code region `0x000000-0x0114FF` for every longword
of the form `0x00NN00xx` where NN is a **replicated nibble**, then manual confirmation of
each hit against real disassembly.

**Read the confidence column before using anything here.** The sweep produces candidates.
Several turned out to be false positives - byte sequences that only look like an absolute
address - and they are called out rather than quietly dropped.

---

### 0. CORRECTIONS - read before section 2

Written the same day, after re-reading part 1 of this file, which is the
write-up of record and already contained more than this sweep did.

- **Select 0x77 is not new.** Section 2 calls `0x770000`/`0x770004`/`0x770006`/`0x770007`
  "CONFIRMED, NEW". Only the sweep's coverage is new - **RE section 2.4b already had
  `0x770004`**, from `movea.l #0x770004,A1` at 0x069E, plus the fact that data arrives into it
  from `0x440000` and that `0x770007` bits 3 **and 4** form a handshake **with a retry count
  of 10**.
- **`0x220000` is a general command/function port, not "the MF-bus command port".** The
  function code selects which target the `0x440000`/`0x550000` pair addresses. Known codes:
  `0x300F`/`0x400A`/`0x400C`/`0x000F` MF-bus memory, `0x0005` AOB, `0x0018` control store,
  plus `0x0001`, `0x0007`, `0x0010`, `0x0015`, `0x0017`, `0x2018`, `0x3010`, `0x4016`,
  `0x8013` observed.
- **`0x330000`/`0x330001` are write-only latches with RAM shadows** at `0x001144EE` /
  `0x001144EF`; the firmware never reads them back. Calling 0x33 an "octobus control latch"
  is wrong. `0x330000` bit 6 = write strobe, bit 2 = control-store gate. Reads of these two
  addresses never happen, so an emulator's read value for them is irrelevant - read the
  shadows instead.
- **`0x660000`/`0x660001` is a shared status byte pair whose bits belong to different
  functions**: `0x660001` bit 1 = AOB busy, bit 2 = message available, bit 4 = MF-bus
  complete; `0x660000` bit 0 = control-store operation OK, bit 3 and bit 5 also tested.
- **`0x00900007` is REAL and this sweep missed it**, because `0x90` is not a replicated
  nibble and the scan filtered on that. `move.b (0x00900007).l,(0x001143B8).l` at 0x07D2.
  **So the nibble-replication rule in section 1 is a strong tendency, not a law, and this
  sweep is therefore NOT exhaustive** - any peripheral at a non-replicated select was
  invisible to it.

---

### 1. The decode pattern

Every peripheral on this card sits at `0xNN0000` with **NN a repeated nibble**. That is a
cheap address decoder: one comparator per nibble pair. Selects seen: 11, 22, 33, 44, 55, 66,
77, 88, AA, BB, DD.

Registers are then at `0xNN0000 + offset`. For the DUART the offset is odd (an 8-bit part on
the low data lane); for the others it is mostly zero or a small even offset.

---

### 2. The map

| Address | Hits | Confidence | What it is |
|---|---|---|---|
| `0x110000-0x117FFF` | 3 | **PROVEN** | **SRAM**, 32 KB, walk-tested by the reset routine at 0x0BD6 as two 16 KB halves |
| `0x220000` | 58 | **PROVEN** | **MF-bus / BADAP command port.** Carved at 0x70CC: `0x300F` open, `0x400A`/`0x400C` sub-function, `0x000F` strobe. The most-touched register in the image |
| `0x220011` | 2 | candidate | |
| `0x220050`, `0x220056` | 1 each | candidate | |
| `0x2200DD` | 1 | likely | `move.b #imm,(0x2200DD).l` - the encoding is unambiguous |
| `0x330000` | 18 | **likely** | **octobus (OBCON) control latch.** Some hits are false positives (`44 EE 00 33` is `move (0x33,A6),CCR`, not an address) - the real ones are `move.b Dn,(0x330000).l` |
| `0x330001` | 33 | **likely** | second byte of the same latch; same false-positive caveat |
| `0x440000` | 16 | **PROVEN** | **MF-bus data, LOW half** |
| `0x550000` | 13 | **PROVEN** | **MF-bus data, HIGH half** (`swap D0` between the two writes) |
| `0x660000` | 12 | **PROVEN** | **status**, read as a word |
| `0x660001` | 14 | **PROVEN** | **status byte, bit 4 = transaction complete** - polled with a countdown; timeout prints `"$MF-bus memory timeout$"` |
| `0x770000` | 5 | **CONFIRMED, NEW** | loaded with `lea (0x770000).l,A1` at five sites in 0x11030-0x11230 - a **window/buffer base**, then accessed indexed |
| `0x770004` | 3 | **CONFIRMED, NEW** | `movea.l #0x770004,A1` (0x4EA, 0x69A) and `move.w D0,(0x770004)` (0x78A2) - a data register |
| `0x770006` | 1 | **CONFIRMED, NEW** | `move.w #4,(0x770006)` at 0xDAC - a command/mode write |
| `0x770007` | 1 | **CONFIRMED, NEW** | `btst #3,(0x770007)` at 0x789A followed by `beq.b -10` - a **ready/busy poll loop** |
| `0x880000` | 5 | **CONFIRMED** | read as a word (`move.w (0x880000).l,D0` at 0x510, inside `Vec27_AutoIrq3`) - FIFO-like drain |
| `0xAA0000` | 3 | **CONFIRMED, NEW** | `move.w (0x20,A6),(0xAA0000).l` at 0x7AE6, 0x7AFA, 0x7B0E - three identical writes of a routine parameter |
| `0xBB0000` | 1 | **CONFIRMED** | `move.w #0,(0xBB0000).l` at 0x88A - a clear/reset |
| `0xBB00DD` | 2 | **CONFIRMED** | `move.b #imm,(0xBB00DD).l` at 0x16B8 and 0x16E0, in the console/DUART code |
| `0xDD0001`..`0xDD001F` | 40 | **PROVEN** | **SCN2681 DUART** - see section 3 |
| ~~`0xFF0011`~~ | 1 | **FALSE POSITIVE** | `0C 39 00 FF 00 11 xx xx` is `cmpi.b #0xFF,(0x0011xxxx).l` - an access to **SRAM**, not to 0xFF0011 |
| ~~`0xFF0020`~~ | 1 | **FALSE POSITIVE** | `33 7C 00 FF 00 20` is `move.w #0x00FF,(0x20,A1)` - an immediate and a displacement, not an address |

**Selects 0x77 and 0xAA were not in any previous list.** Neither was 0x44 or 0x55 before
0x70CC was hand-disassembled. Assume the list is still incomplete until every routine is
carved.

---

### 3. The SCN2681 DUART at 0x00DD0000

Register **N** is at **`0xDD0000 + 2N + 1`**. Every offset the sweep found maps onto a real
SCN2681 register, with no gaps and nothing left over - which is what makes this a proof
rather than a guess.

| Offset | Reg | SCN2681 register (read / write) | Hits |
|---|---|---|---|
| 0x01 | 0 | MR1A / MR2A | 2 |
| 0x03 | 1 | SRA / CSRA | 4 |
| 0x05 | 2 | — / CRA | 7 |
| 0x07 | 3 | RHRA / **THRA** | 2 |
| 0x09 | 4 | IPCR / ACR | 1 |
| 0x0B | 5 | ISR / IMR | 6 |
| 0x0D | 6 | CTU | 1 |
| 0x0F | 7 | CTL | 1 |
| 0x11 | 8 | MR1B / MR2B | 2 |
| 0x13 | 9 | SRB / CSRB | 3 |
| 0x15 | 10 | — / CRB | 8 |
| 0x17 | 11 | RHRB / **THRB** | 2 |
| 0x1F | 15 | — / OPCR, Set Output Port | 1 |

Channel A is the ACCP console. Channel B is a second line whose purpose is still unknown -
though `SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` (command 0x35)
strongly suggests **channel B carries ACCP-to-ND100 communication as an alternative to the
octobus**. Not yet proven from code.

TX rings: A at 0x0011307E, B at 0x00112EC2 (+0x10 limit, +0x12 count). Busy flags in the
byte at 0x001131D8, bit 0 = A, bit 4 = B.

---

### 4. What is still unknown

- **The MF-bus function codes.** `0x3`, `0x4`, `0x0` and sub-functions `0x0A`, `0x0C` are
  transcribed but their meanings are not established.
- **Which select is the octobus (OBCON).** 0x33 is the strongest candidate on access density
  and on sitting inside the 0x6A74-0x7C14 driver region, but 0x77, 0x88 and 0xAA are all
  unassigned and any could be part of it. **Read ND-14001-1-EN chapter 4 before naming
  these** - it documents OBCON, the frame format both directions, the acknowledge bits and
  the INT7 OCTObus Message Reset Register.
- **0xBB00DD and 0x2200DD** - a 0xDD offset under two different selects is odd enough to be
  worth explaining rather than assuming.

---

### Provenance

The sweep is mechanical and complete over 0x000000-0x0114FF. Every entry marked CONFIRMED or
NEW was checked by reading the actual instruction bytes at the listed address. Entries marked
"candidate" have not been; entries marked FALSE POSITIVE were rejected after checking. The
PROVEN entries were established earlier by disassembly (reset routine, 0x70CC, 0x1D4C) and
are restated here.

---

# Part 3 - originally `ACCP-CONSOLE-COMMAND-SET-AND-DISPATCH-2026-07-27.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## ACCP console command set and dispatch map

**Date**: 2026-07-27
**Image**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
**Status**: COMPLETE and read directly from the image - all 43 commands, their codes, their
full parameter syntax, and their handler addresses. Nothing here is inferred.

This is the ACCP's entire control surface. It is what a person sees on the RS-232 console,
and it is what a RetroCore ACCP machine has to reproduce.

---

### 1. How a command gets from the terminal to its handler

```
AccpMainInitAndRunConsole @0x205C
  builds PlancArrayDescriptor {origo 0x000130FE, lower 0, upper 0x2A}   <- the command table
  builds PlancArrayDescriptor over RAM 0x00113232                       <- the parse result
  jsr ConsoleCommandLoop @0x21A6
        |
        +-- ConsoleReadCommandLine @0x274E     read a line from the console
        +-- MatchCommandNamePrefix @0x2D36     match it against the 43 names
        |     returns a pointer to the matched TABLE RECORD in A0
        +-- move.w (A2),(0x00113334)           the record's CODE word -> global 0x00113334
        +-- a LINEAR COMPARE CHAIN, 0x227E-0x2746, dispatching on 0x00113334
```

**The dispatch is a chain of `cmpi.w` / `bne.b` / `jsr`, not a jump table.** 43 compares in
sequence. That matters for two reasons: there is no table to type as pointers, and the
command codes are sparse (0x03..0x46 with holes), which a jump table could not have been.

> **THERE ARE TWO SEPARATE COMPARE CHAINS ON THIS CARD. Do not conflate them.** They have
> different lengths, different address ranges and different command-code spaces, so a count or an
> arm address from one is meaningless in the other.
>
> | Chain | Range | Selector | Arms | Codes |
> |---|---|---|---|---|
> | **Console** command (this section) | `0x227E-0x2746` | `0x00113334` | **43** | `0x03..0x46`, sparse |
> | **Octobus** ACCP command | `0x4D50-0x66B6` | `D0` | **46** | `0x0E..0x3E`, sparse |
>
> The octobus chain's 46 arms are enumerated in
> `CARVE-ANSWER-OCTOBUS-ACCP-COMMAND-DISPATCH-AND-RTEST-2026-08-02.md`, confirmed `[V]` by three
> independent methods (chain walk; external naming from `N500-SYMBOLS.SYMB` + ND-05.020.01 5.3, where
> all 13 named commands land inside code runs and none in a gap; and a whole-image byte search for
> `0C 00 00 ?? 66` yielding exactly 46). That third method **includes `0x4D50`, which an earlier scan
> missed**, and **excludes `0x63DC`, a `beq` false positive**.

#### The 46 octobus dispatcher arms, arm address to command code [V, read from octo.bin 2026-08-02]

Every arm's `cmpi.b` immediate read directly out of
`Installation\Communication\OctobusAccp\eprom\octo.bin` (base 0, so file offset = address; the code
byte is at `arm+3`). **All 46 matched the `0C 00 00 <imm>` + `66` shape - zero mismatches** - and all
13 previously-named commands landed on the code the carve already had, which is the cross-check that
makes the other 33 trustworthy.

| Code | Octal | Arm | Name | Code | Octal | Arm | Name |
|---|---|---|---|---|---|---|---|
| `0x0D` | 015B | `583A` | `[OPEN]` | `0x27` | 047B | `5ECE` | **LAOB32D** |
| `0x0E` | 016B | `57E8` | **CMSYSPAR** | `0x28` | 050B | `5FD6` | `[OPEN]` |
| `0x0F` | 017B | `5736` | `[OPEN]` | `0x29` | 051B | `6016` | **LMODE** |
| `0x10` | 020B | `6562` | `[OPEN]` | `0x2A` | 052B | `608C` | **LOCSM** |
| `0x11` | 021B | `5980` | **LPARP** | `0x2B` | 053B | `60F6` | `[OPEN]` |
| `0x12` | 022B | `59B6` | **VPARP** | `0x2C` | 054B | `6178` | `[OPEN]` |
| `0x13` | 023B | `4D50` | `[OPEN]` | `0x2D` | 055B | `6326` | `[OPEN]` |
| `0x14` | 024B | `4EDC` | `[OPEN]` | `0x30` | 060B | `6616` | **RTEST** |
| `0x15` | 025B | `4FC0` | `[OPEN]` | `0x31` | 061B | `6504` | **ENKICK** |
| `0x16` | 026B | `519C` | `[OPEN]` | `0x32` | 062B | `6534` | **DISKICK** |
| `0x17` | 027B | `56BC` | `[OPEN]` | `0x33` | 063B | `5C44` | `[OPEN]` |
| `0x18` | 030B | `58A4` | **AMICTRAP** | `0x34` | 064B | `5F38` | `[OPEN]` |
| `0x1B` | 033B | `65B6` | **STARTMIC** | `0x35` | 065B | `5DC0` | `[OPEN]` |
| `0x1C` | 034B | `562E` | **STOPMIC** | `0x36` | 066B | `558A` | `[OPEN]` |
| `0x1D` | 035B | `568C` | **CONTMIC** | `0x37` | 067B | `6390` | `[OPEN]` |
| `0x1E` | 036B | `6438` | **RESTMIC** | `0x38` | 070B | `63B8` | `[OPEN]` |
| `0x1F` | 037B | `56EA` | **ALIVE** | `0x39` | 071B | `6408` | **CPURES** |
| `0x20` | 040B | `5A46` | `[OPEN]` | `0x3A` | 072B | `61F4` | `[OPEN]` |
| `0x21` | 041B | `5AB0` | `[OPEN]` | `0x3B` | 073B | `547E` | `[OPEN]` |
| `0x22` | 042B | `5B38` | `[OPEN]` | `0x3C` | 074B | `52C6` | `[OPEN]` |
| `0x23` | 043B | `5BC8` | `[OPEN]` | `0x3D` | 075B | `6644` | `[OPEN]` |
| `0x24` | 044B | `5CC0` | `[OPEN]` | `0x3E` | 076B | `66B6` | **READ CPU MODEL** |
| `0x25` | 045B | `5D56` | **RAIB32D** | | | | |
| `0x26` | 046B | `5E64` | **LAOB16** | | | | |

**Codes run `0x0D`-`0x3E` with exactly four holes: `0x19`, `0x1A`, `0x2E`, `0x2F`.** Arm order in the
image is NOT code order - `0x4D50` serves `0x13` while `0x6562` serves `0x10` - so never infer a
code from an arm's position.

**Naming the remaining 33 is still `[OPEN]`, and positional mapping does NOT work.** The manual
documents 46 commands in sections 5.3.12 (ECHO) through 5.3.57 (READ CPU MODEL) - the same count,
temptingly - but it prints no numeric code in any section, and the mapping fails a check: `LSYSPAR`
is 5.3.13 and `CMSYSPAR` is `0x0E`, while `LPARP` is the very next section, 5.3.15, yet is `0x11`.
That leaves two code slots (`0x0F`, `0x10`) for one intervening section. **Do not name these by
counting sections.** Read each handler body instead.

#### The octobus command layer: helpers, globals and the shared reply shapes [V 2026-08-02]

Carved while naming the arms. These are what every arm is built from, so knowing them makes each
arm readable at a glance. Renamed in the Ghidra database from the placeholder
`MicroprogCmd_Helper_*` names:

| Address | Name given | What it does |
|---|---|---|
| `0x6986` | `Reply_EmitByte` | Append one byte to the reply. 60+ callers - useless for identifying an arm |
| `0x69D0` | `Reply_EmitWord` | Append a 16-bit word to the reply |
| `0x6F9C` | `MsgBody_NextParamByte` | Read the next parameter BYTE from the received body |
| `0x6FFA` | `MsgBody_NextParamWord` | Read the next parameter WORD |
| `0x6A64` | `StatusHiRead` (already named) | Read ASTS and emit it as two bytes |

**The message-body reader, which also explains a symptom we already had.**
`MsgBody_NextParamByte` indexes the body buffer at **`0x001143BC`** using read cursor
**`0x001144EA`**, bounded by body length **`0x001144EC`**. Its overrun path is **`0x6FE4`** - which is
exactly the "`Communication error at address 6FE4H`" the card prints for an OBCON message with an
empty body. The two `clr.w` of `0x1144EC`/`0x1144EA` at the top of most arms are **resetting that
reader**, not clearing status.

**The Messnak tail is one shared shape**, visible in every guarded arm and matching the documented
format (byte 0 error code, bytes 1-2 ASTS lower/upper):

```
Reply_EmitByte(0xFF)        ; nak marker
Reply_EmitByte(errcode)     ; e.g. 0xFF = -1, 0x01 = 1
StatusHiRead()              ; the two ASTS bytes
```

**Three guard globals `[V]` - these classify an arm before its body is read:**

- **`0x001143AC`** - microprogram-running flag. Non-zero -> Messnak **-1** ("illegal when microprogram
  is running"). This is the same flag the carve agent found `Cmd31_LoadModeRegister` conspicuously
  lacking.
- **`0x001143B2`** - parameter-pointer-set flag. Zero -> Messnak **1** ("no parameter pointer is
  given"). **An arm that tests this is a memory-parameter command** and needs `LPARP` first.
  Written by arm `0x11` = `LPARP` at `0x5994`, which confirms both ends of the mechanism. Read by
  arms `0x13`, `0x15` and `0x34`.
- **`0x001143B6`** - kicks-enabled flag. Non-zero -> Messnak **-2** ("illegal when kicks are
  enabled"). ND-05.020.01 5.3.11 names READ AIB as its example, "READ AIB would destroy a kick being
  sent from the ND-5000", so **an arm with this guard is very likely an AIB reader**.

**Method warning that cost a sweep:** xrefs in this range **undercount**, because parts of the
dispatcher are still undefined bytes rather than code. `0x4D90` tests `0x001143B2` but did not appear
in the xref list until it was force-disassembled. **Disassemble the gaps first, then trust xrefs** -
otherwise a clean-looking empty result is just unanalysed data. Same family as the `bset #5` trap.

#### Commands named this round

**`0x18` (030B) = AMICTRAP** `[V]` - ACCP MICROTRAP, 5.3.14. The arm collects the body bytes, pairs
them big-endian into 16-bit words, waits on `0x00660001` bit 1, and writes each to AOB at
`0x00440000`. It then falls into **`0x5958`, the site that writes `0xD0` to MREG-upper** =
OBACT+AOBF+ATRAP - **ATRAP without OMESS**, which is exactly what 5.3.14 specifies "to distinguish
this from octobus kicks/idents". The kick shape is `0xD8`, the same word plus OMESS, at `0x061C`.
That literal is what pins the identity, and it ties the carve agent's MREG literal table directly to
a command.

**`0x34` (064B) = RAIB32M?** `[I]` - guarded by BOTH the parameter-pointer flag and the kicks-enabled
flag, which per 5.3.11 points at a memory-parameter AIB reader, i.e. Read AIB32 Via Memory (5.3.34).
**Not promoted to `[V]`** - the body past the guards has not been read.

> **Reading the body WEAKENED this guess, which is why it was not promoted.** Arm `0x34` first calls
> **`0x795A` = STOPMIC**, then reads the parameter pointer and runs a bulk loop gated by
> `0x00113138`. Stopping the microprogram first fits a control-store operation better than an AIB
> read. **Treat `0x34` as unresolved, not as RAIB32M.**

**`0x3E` (076B) = READ CPU MODEL** `[V]`, 5.3.57. The arm reads the CPU class **byte** at
`0x001131F6` and the accept bit at `0x001131FA`, then replies with the packed model byte. This is
the arm behind the CMD-3 reply `00 39` already documented above, so the command table and the
dispatcher now agree from both ends.

**A fourth guard, and it classifies the control-store family** `[V]`: arms `0x3B` (073B) and `0x3C`
(074B) both begin

```
cmpi.w #0x7F55,(0x0011455C)   ; must MATCH, else...
-> Messnak 5                  ; "control store / control cache HW error"
```

So **`0x0011455C` holds a health/ready word whose good value is `0x7F55`**, and **an arm testing it is
a control-store or control-cache command**. That narrows `0x3B` and `0x3C` to the DCSD / DUCS / DCCD /
DUCC family (5.3.19-5.3.22) - `[I]`, not resolved between them yet.

#### Guard sweep: what each guard says about which arms [V 2026-08-02]

Reading the first few bytes of an arm now places it in a manual section group before any of its body
is read. Sweeping all four guards across the dispatcher gives this:

| Guard read by arm | Arms | What it means |
|---|---|---|
| `0x001143B6` kicks enabled -> Messnak -2 | `0x25`, `0x26`, `0x27`, **`0x34`**, **`0x35`** | AIB/AOB access - would disturb a kick in flight |
| `0x001143AE`/`B2` parameter pointer -> Messnak 1 | `0x13`, `0x15`, **`0x34`**, **`0x35`** | memory-parameter command, needs `LPARP` first |
| `0x0011455C` != `0x7F55` -> Messnak 5 | `0x3B`, `0x3C` | control store / control cache family |

**Two flags are confirmed from both ends `[V]`:**

- `ENKICK` (arm `0x31`) and `DISKICK` (arm `0x32`) are the **writers** of `0x001143B6`, which is
  exactly what that flag should be. Readers and writers agree.
- `LPARP` (arm `0x11`) is the **writer** of both `0x001143AE` (the pointer) and `0x001143B2` (the
  "pointer given" flag).

**The overlap is the useful part.** Exactly two arms carry BOTH the kicks guard and the
memory-parameter guard: **`0x34` and `0x35`**. The manual has exactly two AIB/AOB commands that take
their parameters via memory - **RAIB32M** (5.3.34) and **LAOB32M** (5.3.37). So those two names
belong to those two arms, `[I]` on which is which.

The three arms with the kicks guard but NO memory-parameter guard - **`0x25`, `0x26`, `0x27`** - are
then the direct-parameter AIB/AOB commands, from RAIB16 (5.3.32), RAIB32D (5.3.33), LAOB16 (5.3.35)
and LAOB32D (5.3.36). Four candidate names for three arms, so **one of those four is not
kick-guarded and sits elsewhere** - do not assign by elimination until that is resolved.

**Caution carried forward:** `0x18` = AMICTRAP writes AOB and is NOT kick-guarded, which shows the
guard is not simply "touches AOB". Do not treat the guard as a mechanical rule for the whole family.

**Direction is what actually names these, not the guard.** The workers already carry their direction
in their names, so one decompile settles an arm:

| Worker | Direction |
|---|---|
| `0x72A0` = `AobSingleWordWrite` | writes `_HW_DATA_LOW`, strobes latch bit `0x40` - **into AOB** |
| `0x70AA` = `MfBusMemoryTransaction_VariantA` | writes `_HW_DATA_*`, command `0x0F`, waits status - **out to MFbus** |
| `0x7138` = `MfBusMemoryTransaction_VariantB` | same but **returns** `_HW_DATA_*` - **in from MFbus** |

**`0x26` (046B) = LAOB16** `[V]`, 5.3.35. Reads one 16-bit direct parameter, then loops
`AobSingleWordWrite` until `0x00113138` clears. Kick-guarded, no memory parameter, and the worker
writes - so it is the direct 16-bit load into AOB.

~~**`0x35` (065B) = LAOB32M?**~~ **CORRECTED 2026-08-02 - that guess was BACKWARDS.**

> Reading arm `0x34`'s body once it was disassembled settles the pair the other way round:
>
> ```
> 0x5F9E  jsr 0x795A                       ; the STOPMIC / latch-disable routine
> 0x5FA4  move.l (0x001143AE),D0           ; the parameter pointer
> 0x5FAA  jsr 0x7138                       ; VariantB = read IN from MFbus memory
> 0x5FB4  loop: jsr 0x7320                 ; write the data pair OUT
> ```
>
> Fetching a longword **from** memory and writing it **to** the data pair is **Load AOB32 Via
> Memory**. So **`0x34` (064B) = LAOB32M** `[I` strong`]`, 5.3.37 - and `0x35` is then RAIB32M by
> pairing, which is **still not written into the table** for the same reason as before.
>
> **The STOPMIC call is no longer an anomaly, and it explains itself.** `0x795A` is the latch-disable
> routine (section 2.4e); `Cmd24_StopMicroprogram` calls it because stopping the microprogram *is*
> disabling that gate. An arm about to drive AOB has to close the same gate first. The name "STOPMIC"
> came from its call site, so seeing it inside a load command looked wrong - **the same
> name-from-caller trap, one level further down.**
>
> The earlier guess was tagged `[I]` and never entered the command table, which is the only reason
> this correction is cheap.

**`0x25` (045B) = RAIB32D** `[V]`, 5.3.33. Takes **no** parameters. Its body calls
`MfBusCmdDataPairStatus` (`0x7374`) and keeps the result as a long. **Independently confirmed from
the console side**: the same worker is called at `0x8882`, inside `Cmd2C_ReadAib32`. The two
dispatchers share the AIB32 read routine, so the octobus arm that calls it is the octobus AIB32 read. **What names it is an ACON
code**: that worker reads the data pair and then issues **command `5` = RAIBF**, "reset AIBF flag and
clear MASKAIBF flip-flop" (table 9). Reading the pair and clearing AIBF *is* a read of AIB. The guard
only narrowed it; the ACON code proved it.

**`0x27` (047B) = LAOB32D** `[V]`, 5.3.36. Reads a **32-bit** direct parameter via
`MsgBody_NextParamLong` (`0x7036`, four bytes assembled big-endian - renamed from
`MicroprogCmd_Helper_7036`), then feeds it to the data-pair **write** worker `0x7320`.

**The prediction held, and then went further than expected.** Four candidate names, three
kick-guarded arms - so one had to sit elsewhere. **RAIB16 (5.3.32) appears to have NO octobus arm at
all.** `[V` for the evidence, `I` for the conclusion`]`

`0x72EC` - renamed **`AibRead16_AndClearAibf`** - is unmistakably the 16-bit AIB read: it returns
**`_HW_DATA_LOW` only** and then issues **ACON command `5` = RAIBF**. It has exactly **two** callers:
`0x0AEA`, and `0x89B2` inside the console `Cmd2B_ReadAib16`. **No dispatcher arm calls it.**

**This breaks the "46 arms, 46 manual sections" coincidence.** The counts matching (5.3.12 ECHO
through 5.3.57 READ CPU MODEL is exactly 46) made a positional mapping very tempting, and this file
already recorded that it fails a spot check. Now there is a structural reason: **at least one
documented command has no octobus arm**, so the two sets are not the same set and never were. Any
attempt to finish the naming by counting sections, or by elimination against the manual's list, will
produce wrong names - not just possibly, but certainly.

#### Parameter shapes per arm [V 2026-08-02] - raw material for the remaining names

The first few instructions of an arm say exactly what parameters it takes, because the readers are
now named. This is measured, not inferred, and it is the input a namer needs:

| Arm | Code | Parameters taken | Guards |
|---|---|---|---|
| `4EDC` | `0x14` | **bulk WORD list** into `0x001144F0[]`, with a running **checksum** in `(0x50,A6)` | - |
| `5A46` | `0x20` | one WORD | running |
| `5AB0` | `0x21` | **bulk WORD list** into `0x001144F0[]`, **no checksum** | - |
| `5B38` | `0x22` | none | running |
| `5BC8` | `0x23` | one LONG (`MsgBody_NextParamLong`) | running |
| `5C44` | `0x33` | one LONG | running |
| `5CC0` | `0x24` | none | running |
| `519C` | `0x16` | one WORD | running |
| `558A` | `0x36` | one WORD | running |
| `5E64` | `0x26` | one WORD | kicks |
| `5ECE` | `0x27` | one LONG | kicks |
| `5D56` | `0x25` | none | kicks |

**The checksum is a real discriminator.** Messnak code **4 is "checksum error"**, and only the
control-store load path in the manual carries a checksum. `0x14` accumulates one; `0x21` builds the
same kind of word array **without** one. So they are two different bulk-word-load commands, and the
one with the checksum is the control-store load.

**Names deliberately NOT assigned here.** The obvious reading is `0x14` = LOCSD and `0x21` = LMIR,
and it may well be right - but this session has already walked back two inferences that looked at
least as good (the RAIB32M/LAOB32M pair was backwards, and arm `0x34` looked like an AIB read
because of a call whose name came from its caller). **Shape narrows; only a worker or a hardware
code proves.** These stay `[OPEN]` until one of those turns up.

- **`0x001143AE`** is **the parameter pointer itself** - a longword MFbus address. `LPARP` (arm
  `0x11`) writes it and sets the `0x001143B2` "pointer given" flag; every memory-parameter arm reads
  it back from here.
- **`0x7138` = `MfBusMemoryTransaction_VariantB`** (already named) reads a 32-bit word from MFbus
  memory. Arm `0x13` uses it to pull a `{address, count}` descriptor out of the parameter block,
  taking the low half to one frame cell and the high half as a count.
- **`0x7320` = `MfBusDataPairWithLatchGate_33`** (already named) is the data-pair write with the
  latch gate.

**A cross-check worth noting, and a possible refinement to the `0x220000` census.**
`MfBusMemoryTransaction_VariantB` drives the command port with ACON `0x000F` (ADCLK) and then
`0x8013`. The census recorded `0x8013` running **exactly once per boot**, and the carve agent read
those once-per-boot codes as the self-test bus loopback. Both can be true - they are different call
sites - but it means **`0x8013` has at least two producers**, and the census's single occurrence only
tells us which one ran during that particular boot. A boot that exercised a memory-parameter command
would count more. `[OPEN]` - worth one rerun to settle.

**Trap avoided while doing this:** arm `0x13` calls `StatusHiRead`, which looks like it makes the
command RASTS (5.3.38, Read ASTS). **It does not** - the call is part of the Messnak tail above, which
every guarded arm shares. A shared error path is not an identity.

**The method that DOES work - cross-reference from the implementation, not the arm.** Instead of
reading 33 handler bodies, take a worker routine whose identity is already known and ask who calls
it. The callers that fall between two arm addresses belong to that arm.

Worked example, `[V]` 2026-08-02: `0x77FE` is the mode-register write. It has exactly **two**
callers - `0x951E` (the console `Cmd31_LoadModeRegister`) and `0x606E`. `0x606E` lies between arms
`0x6016` and `0x608C`, so it belongs to the `0x6016` arm. **Octobus command `0x29` (051B) is
therefore LMODE**, which independently agrees with the carve agent's note that "octobus LMODE reaches
the same code but is Messnak -1 while running".

Two cautions learned doing it:

- **Arm ownership is by address range, not by proximity.** Sort the 46 arm addresses and find the
  interval the caller falls in. Arms are not in code order, so eyeballing the nearest arm is wrong.
- **Not every worker is single-caller.** `0x6986` (emit ack byte) has 60+ callers and identifies
  nothing. Pick workers that touch a distinctive hardware address.

Suggestive but NOT yet verified `[I]`: `0x5924` writes `0x440000` (the AOB register) and lies in the
`0x58A4` arm, making command `0x18` (030B) a load-AOB command - the manual has LAOB16 at 5.3.35 and
LAOB32D at 5.3.36. **Which of the two is unresolved; do not write either name down yet.**

**Also rejected while doing this:** `5P-P2-MON60.NPL` has `SYMBOL STOPMIC= 34`, and 34B is `0x1C`,
which is exactly the ACCP `STOPMIC` code. That is a **collision, not a source** - those symbols are
MON 60 subfunction numbers, a different namespace (its `RCNTS`/`WCNTS` sit at 23B/24B where the ACCP
has a hole pattern that does not line up). A name-shaped match in the wrong namespace is the same
trap that produced `TRAP_OCBAK`.
>
> Both chains being linear compares is not a coincidence: it is the ordinary PLANC `CASE` shape
> (see the `ghidra-planc` skill). **Never go looking for a jump table on this card.**

`0x00113334` holds the current command code. Watch that one word and you know what the
console is executing.

`HELP` (code 0x0C) is the only command with **no `jsr`** - its body is inline in the chain at
0x22D2, which is why its `bne` displacement is 34 instead of the usual 18.

---

### 2. The 43 commands

Read from the table at `0x000130FE`: 43 records of 14 bytes,
`{word code, long origo, long lower, long upper}` - a code plus a 12-byte PLANC array
descriptor over the command's help text. Table ends at 0x13358, exactly where the text
begins.

Sorted by command code.

| Code | Command and its parameter syntax | Handler |
|---|---|---|
| 0x03 | `DUMP-LOCAL-MEMORY <Address> <Wordsize /halfword/>` | 0x03A12 |
| 0x06 | `SHOW-REGISTERS` | 0x0A3B2 |
| 0x07 | `LOOK-AT-LOCAL-MEMORY <Address>` | 0x0353E |
| 0x09 | `VALUE <Convert number>` | 0x0400A |
| 0x0A | `MAIN-FORMAT <BASE (HEX,OCT,DEC)>` | 0x04076 |
| 0x0C | `HELP <Command>` | **inline @0x22D2** |
| 0x1F | `READ-ECO-LEVELS` | 0x09F12 |
| 0x20 | `LOOK-AT-CONTROL-CACHE <CC address>` | 0x0ADE0 |
| 0x21 | `LOAD-CONTROL-STORE <CS address> <127-112> <111-096> <095-080> <079-064> <063-048> <047-032> <031-016> <015-000>` | 0x08C44 |
| 0x22 | `LOOK-AT-CONTROL-STORE <CS address>` | 0x0AA5E |
| 0x23 | `START-MICROPROGRAM <CS address>` | 0x09110 |
| 0x24 | `STOP-MICROPROGRAM` | 0x091B8 |
| 0x25 | `CONTINUE-MICROPROGRAM` | 0x09218 |
| 0x26 | `RESTART-MICROPROGRAM <CS address> <Interval>` | 0x09272 |
| 0x27 | `CHECK-ALIVE` | 0x09D9C |
| 0x28 | `LOAD-MAR <CS address>` | 0x08D98 |
| 0x29 | `LOAD-MIR <127-112> <111-096> <095-080> <079-064> <063-048> <047-032> <031-016> <015-000>` | 0x08E04 |
| 0x2A | `READ-MIR` | 0x08F64 |
| 0x2B | `READ-AIB16` | 0x0898A |
| 0x2C | `READ-AIB32` | 0x0885A |
| 0x2D | `LOAD-AOB16 <Data (16)>` | 0x088E2 |
| 0x2E | `LOAD-AOB32 <Data (32)>` | 0x087B8 |
| 0x2F | `RUN-SHORT-SELFTEST <Loop selftest? (y/n)>` | 0x07FBC |
| 0x30 | `READ-ACCP-STATUS` | 0x09686 |
| 0x31 | `LOAD-MODE-REGISTER < Upper byte> < Lower byte>` | 0x0945E |
| 0x32 | `LOAD-CONTROL-DECODER <Data (16)>` | 0x095E4 |
| 0x33 | `LOOK-AT-MEMORY <Address>` | 0x0A556 |
| 0x34 | `SET-INTERRUPT-MASK <Interrupt mask>` | 0x0333A |
| 0x35 | `SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` | 0x07F06 |
| 0x36 | `SET-KICK-TIMEOUT <Kick timeout (ms)>` | 0x07EAE |
| 0x37 | `RECEIVE-OCTOBUS` | 0x09748 |
| 0x38 | `SEND-OCTOBUS <Data (16)>` | 0x097CA |
| 0x39 | `RECEIVE-MULTIBYTE-OCTOBUS` | 0x09B98 |
| 0x3A | `SEND-MULTIBYTE-OCTOBUS <Destination><Subprocess><Message>` | 0x0986C |
| 0x3B | `SEND-KICK-OCTOBUS <DESTINATION><Kick value (process)>` | 0x09A4E |
| 0x3C | `TRACE-COMMUNICATION-DATA <Trace Octobus communication to consol? (y/n)>` | 0x09D62 |
| 0x3D | `RUN-LONG-SELFTEST <Loop selftest? (y/n)>` | 0x08072 |
| 0x3E | `TEST-BUFFERS <ASR/AOB>` | 0x0855C |
| 0x3F | `TEST-BUSLOOP <Test-pattern>` | 0x0868A |
| 0x40 | `TEST-MEMORY <From address> <To address>` | 0x08128 |
| 0x41 | `LOOP-ON-NEXT-COMMAND <Supress output text ?>` | 0x07F40 |
| 0x42 | `SET-CLOCK-SPEED <Clock speed (Slow,Normal,Fast)>` | 0x09004 |
| 0x46 | `RESET-CPU` | 0x09708 |

All 43 handlers now exist as named functions in the Ghidra database, as
`Cmd<code>_<Name>`.

#### Codes are sparse - a shared enum, not an index

Used: 03, 06, 07, 09, 0A, 0C, 1F, 20-2F, 30-3F, 40, 41, 42, 46.
Absent: 04, 05, 08, 0B, 0D-1E, 43, 44, 45.

The holes are real. This looks like a **global ND command-code enum** that the console
shares with something else - most likely the ACCP-ND100 command set, given the string
`"Illegal ACCP command received from microprogram:"`. ~~**UNVERIFIED** - the holes have not
been traced to a second consumer.~~

> **REFUTED for the octobus path, 2026-07-31** (RetroCore side, verified by EXECUTING the
> firmware against a peer, not by reading it).
>
> There is **no shared enum**. The ACCP's **CMD-3 (octobus) command dispatcher is a separate
> compare chain** - head `0x4D50`, following each `bne` target, default arm `0x6746` - carrying
> **46 commands** in three contiguous runs: **`0x0D-0x18`, `0x1B-0x2D`, `0x30-0x3E`**.
>
> That set covers the console table's largest hole (`0x0D-0x1E`), which makes a shared enum look
> plausible - and that is exactly the trap. **Decisive probe:** console `0x3C` is
> TRACE-COMMUNICATION-DATA, whose only observable effect is the flag at `0x001143B4`. Sent over
> CMD 3 to a booted card it replies `FF 01` - a *defined* command returning error code 01, not the
> undefined-command complaint - **and the trace flag does not move**. **Confirming case:** console
> `0x3E` is TEST-BUFFERS, but CMD-3 `0x3E` returns the packed CPU model.
>
> **Two different enums that overlap numerically.** The console-table holes remain unexplained -
> the second consumer is still not identified, it is simply *not* the octobus command set.

---

### 3. What the command set tells us about the hardware

Reading the commands as a specification of the machine:

- **The microword is 128 bits.** `LOAD-CONTROL-STORE` and `LOAD-MIR` both take eight 16-bit
  fields spelled out from `<127-112>` down to `<015-000>`. The firmware states its own
  microword width; no inference needed. (Settles it for this card - cf. the ND-5800 vs
  classic-500 question.)
- **The ACCP can single-step and restart the ND-5000 microengine**: START / STOP / CONTINUE /
  RESTART-MICROPROGRAM, LOAD-MAR (microaddress register), LOAD-MIR / READ-MIR
  (microinstruction register), LOOK-AT-CONTROL-STORE, LOOK-AT-CONTROL-CACHE.
  `RESTART-MICROPROGRAM <CS address> <Interval>` even takes a repeat interval - the
  `"restarted every N microseconds"` string.
- **AIB / AOB are the ACCP-to-microprogram mailboxes**, in both 16- and 32-bit forms:
  READ-AIB16/32 (in) and LOAD-AOB16/32 (out). The error strings
  `"AOB not read by microprogram within timeout"` and `"AOB full, previous message not read.
  Message lost!"` describe exactly this pair.
- **The octobus has five console entry points**: SEND / RECEIVE-OCTOBUS (16-bit single word),
  SEND / RECEIVE-MULTIBYTE-OCTOBUS (with `<Destination><Subprocess><Message>`), and
  SEND-KICK-OCTOBUS (`<DESTINATION><Kick value (process)>`). **"Subprocess" and "process" in
  those parameter lists are the addressing above station number** - a kick is aimed at a
  process, a multibyte message at a destination plus subprocess.
- **Three commands were not in any earlier list**: `LOAD-CONTROL-DECODER <Data (16)>`,
  `LOAD-MODE-REGISTER <Upper byte> <Lower byte>`, `READ-ECO-LEVELS`, plus
  `SET-CLOCK-SPEED <Slow,Normal,Fast>` and `TEST-BUSLOOP`. SET-CLOCK-SPEED is notable - the
  ACCP can change the ND-5000's clock rate.

---

### 4. Related structures found alongside

| Address | What |
|---|---|
| 0x000130FE | the command table itself, 43 x 14 bytes |
| 0x00013358 | the command name / help text blob |
| 0x00012E9C | `tbl_runtimeErrorMessages` - 12-byte descriptors, indexed `(errcode - 1000) * 12` |
| 0x00012F5C | the error text: `"No such command$Ambiguous command$Illegal format$..."` |
| 0x00113334 | RAM: the current command code |
| 0x00113232 | RAM: the parsed-command array the loop passes to the matcher |
| 0x00113324 | RAM: tested right after the read; non-zero continues, zero exits the loop |

---

### Provenance

The table was decoded by reading the 43 records straight out of the ROM file and resolving
each descriptor to its text. The dispatch map was extracted by walking the compare chain
0x227E-0x2746 in the raw image and reading the `jsr` target after each `cmpi.w` / `bne.b`.
Both were cross-checked against the Ghidra listing. Section 3's readings of what the
commands imply are labelled where they are interpretation rather than transcription; the
sparse-code explanation in section 2 is explicitly UNVERIFIED.

---

# Part 4 - originally `ACCP-ND5000-CPU-INTERFACE-SPEC-2026-07-30.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## The ACCP <-> ND-5000 CPU interface - implementation spec

**Date**: 2026-07-30
**Audience**: anyone implementing an octobus controller or an ACCP interface inside a simulated
ND-5000 CPU.
**Status**: the four interface registers and both handshakes are **carved from both sides** and
agree. Open items are listed in section 9 and marked in place.

**Two independent sources, cross-checked against each other:**

- **CPU side**: `MICRO-5800-B30` microcode listing. Catalogued in
  `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md`.
  Source binaries `E:\Dev\Ronny\ND5000UC\docs\MC\MICRO-5800-B30.DATA` (+ `.LABE`),
  disassembly `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`. Microcode addresses are
  **octal**.
- **ACCP side**: the ND-324716 / PCB 5616 EPROM image
  `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`,
  SHA256 `0EA81716AD81984B64675E9A8CCEB6C1909AB299BE0048857C58F85C3479C5F1`.
  68000 addresses are **hex**.

Marks used below: **[V]** read directly from one of the two images. **[X]** confirmed
independently from BOTH sides. **[I]** inference, stated as such. **[OPEN]** not known.

---

### 1. Orientation - the names are from the CPU's point of view

This trips people up, so it comes first.

- **AOB = "ACCP Output Buffer".** The **ACCP writes** it; the **CPU reads** it.
- **AIB = "ACCP Input Buffer".** The **CPU writes** it; the **ACCP reads** it.

So on the ACCP side "write AOB" is an outbound data path toward the CPU, and "read AIB" is
inbound from the CPU. The ACCP's own console commands are named the same way and confirm it
**[X]**: `LOAD-AOB16` / `LOAD-AOB32` write toward the CPU, `READ-AIB16` / `READ-AIB32` read
what the CPU left.

---

### 2. The CPU side - four special operands, 23 microwords total [V]

| Operand | Encoding | Dir | Meaning |
|---|---|---|---|
| `A,SPEC,AOB` | A-source `0141` | CPU reads | data from the ACCP |
| `D,SPEC,AIB` | dest `041` | CPU writes | data to the ACCP |
| `A,SPEC,AFLAG` | A-source `0151` | CPU reads | status / flags. **Always SLOW2 (160 ns) cycles** |
| `A,SPEC,AOBASR` | A-source `0152` | CPU reads | AOB-side / ASR register. **Read only at boot, immediately before AOB** |

#### AFLAG bit map [V, but see the warning]

BM naming in the listings is **octal**: BM05 = bit 5, BM11 = bit 9, BM12 = bit 10,
BM13 = bit 11, BM14 = bit 12.

| Bit | Meaning | Confidence |
|---|---|---|
| 5 | async-trap word pending (-> `TRAP_OCBA` / `TRAP_ATRP`; also the `ACCP_RDYW` stash trigger) | [V] corrected |
| 6 | "other trap" (-> `TRAP_OTRP`, NOTREC 210) | [V] corrected |
| 7 | data-fault indication (-> `TRAP_DFC`) | **[OPEN] not re-verified** |
| 8 | instruction-fault indication (-> `TRAP_IFC` / `TRAP_NIF`) | **[OPEN] not re-verified** |
| 9 | **AOB has data** | [V] loop-polarity proven |
| 10 | **AIB busy** | [V] loop-polarity proven |
| 11 | power-fail warning (-> `TRAP_PWF`) | [V] corrected |
| 12 | OCB kick / message pending (-> `TRAP_OCBAK` / `TRAP_OMESS`) | [V] corrected |

> **WARNING - read this before modelling AFLAG.** The four dispatch bits (5, 6, 11, 12) were
> each **wrong by one position** in the first version of the catalog. In this microcode a
> `C,SEQ` branch condition comes from the **previous** microword's ALU result, so a naive
> reading shifts every tested bit. The table above is the corrected one, consistent across both
> the `SCAN_ACCP` and `ATRAP_CHK` chains. **Bits 7 and 8 were never re-checked and carry the
> same risk.** Do not trust them without re-reading the listing.

> **BITS 7 AND 8 CANNOT BE ANSWERED FROM `octo.bin` - DEAD END, do not spend time on it.**
> Established 2026-08-01 from the ACCP firmware side, in answer to a direct request.
>
> **AFLAG is a CPU-side hardware status register** (A-source `0151`). **The ACCP firmware never
> composes it.** What the 68000 actually touches is the write-only latches at
> `0x330000`/`0x330001` (shadowed in RAM at `0x001144EE`/`0x001144EF`) and reads of `0x660001`.
> The carved bits there are `0x330001` bits 1, 2, 3, 4, 6 and `0x660001` bits 0, 1, 2 - **none of
> which is a data-fault or instruction-fault indication.**
>
> So the 68000 disassembly is the **wrong artifact** for this question. The answer lives in the
> **microcode listing or the hardware documentation**, not in the card's firmware.
>
> The only route offered from that side is experimental rather than static: with tracing armed,
> correlate what the ACCP pushes on the AOB path against the CPU-side AFLAG reads on a live run.
>
> **Consequence for the emulator:** `AccessModule.ReadAflag` models bits 5, 6, 9, 10, 11, 12 and
> deliberately leaves 7 and 8 out. That stays the correct choice - there is now a positive reason
> they cannot be pinned, not merely an absence of evidence.

#### The four primitives [V]

```
ACCP_READ   (016371)   wait AFLAG bit 9 set;      Q/SC13 := AOB
ACCP_WAITI  (016375)   wait AFLAG bit 9 set;      return AFLAG (does NOT consume AOB)
ACCP_WRITE  (016402)   wait AFLAG bit 10 clear;   AIB := SC12
ACCP_WAITO  (016406)   wait AFLAG bit 10 clear;   return
```

The write argument is always register **SC12**.

`ACCP_XWRITE` (016401) is **not** an AIB write - it does `RF2D := SC12`, appending to a
register-file / memory message buffer. See section 7.

---

### 3. The ACCP side - the same interface, in 68000 address space [V]

| Address | Access | Role |
|---|---|---|
| `0x00440000` | **write** | AOB **low** word (toward the CPU) |
| `0x00440000` | **read** | AIB **low** word (from the CPU) |
| `0x00550000` | **write** | AOB **high** word |
| `0x00550000` | **read** | AIB **high** word |
| `0x00660001` bit 0 | read | **AIB data available** - must be SET before reading `0x440000` |
| `0x00660001` bit 1 | read | **AOB busy** - must be CLEAR before writing `0x440000` |
| `0x00330000` | write | **strobe / control register.** Bit 6 strobes an AOB write. RAM shadow at `0x001144EE` |
| `0x00220000` | write | command port. **Writing `0x0005` acknowledges an AIB read** |
| `0x00220000` | read | status / identification words - see section 8 |

`0x00440000` and `0x00550000` are **bidirectional**: a write is AOB, a read is AIB. That is
the cleanest reading of the two routines below and it matches the console command naming
**[X]**.

**This resolves `0x00330000`**, which earlier documents listed as an unidentified heavily
accessed address. It is the AOB write strobe, driven through a RAM shadow byte so the other
bits are preserved.

#### `0x001131 38` - a bypass flag that will confuse you

Both handshakes begin with `tst.w (0x00113138)`, and a **non-zero value skips the wait
entirely**. It exists in the firmware, not just in an emulator. If a guest sets it, neither
gate is honoured.

---

### 4. The two handshakes, exactly [V]

#### ACCP writes AOB, 16-bit - routine `0x72A0`

```
if (word16[0x00113138] == 0)
    while ((byte[0x00660001] & 0x02) != 0)   /* AOB busy - spin until clear */
        ;

save SR; SR |= 0x2700                        /* interrupts off - the strobe is atomic */
write_word(0x00440000, value)
byte[0x001144EE] |=  0x40                    /* set strobe bit 6 in the shadow */
write_byte(0x00330000, byte[0x001144EE])     /* strobe */
byte[0x001144EE] &= ~0x40                    /* clear it again in the shadow only */
restore SR
```

Note the shadow is left with bit 6 **clear**, and `0x330000` is written **once** with the bit
set. So the strobe is a single write of a value whose bit 6 is 1; the hardware presumably
edge-triggers on it. **[I]** on "edge-triggered"; the single write is **[V]**.

#### ACCP writes AOB, 32-bit - routine `0x7320`

Identical, except both halves go out before the strobe:

```
    ... same busy wait and SR save ...
write_word(0x00440000, value & 0xFFFF)       /* low */
write_word(0x00550000, value >> 16)          /* high */
    ... same single strobe through 0x330000 ...
```

**Order matters: low first, then high, then one strobe.**

#### ACCP reads AIB, 16-bit - routine `0x72EC` (`MfBusCmdAndWaitStatus_22_44_66`)

```
if (word16[0x00113138] == 0)
    while ((byte[0x00660001] & 0x01) == 0)   /* wait for data available */
        ;

value = read_word(0x00440000)
write_word(0x00220000, 0x0005)               /* acknowledge / advance */
return value
```

#### ACCP reads AIB, 32-bit - routine `0x7374` (`MfBusCmdDataPairStatus`)

```
    ... same wait on 0x660001 bit 0 ...
D0  = read_word(0x00550000)                  /* HIGH half first */
swap D0
D0 |= read_word(0x00440000)                  /* then LOW */
write_word(0x00220000, 0x0005)
return D0
```

**Read order is high-then-low, the opposite of the write order.** That asymmetry is real and
byte-verified; do not "tidy" it.

#### Correspondence between the two sides

| CPU side (AFLAG) | ACCP side | Confidence |
|---|---|---|
| bit 9 = AOB has data | the flag the ACCP sets by strobing `0x330000` bit 6 | [I] direction certain, wiring not proven |
| bit 10 = AIB busy | `0x660001` bit 1 seen from the other end (ACCP calls it "AOB busy") | [I] consistent, not proven |
| - | `0x660001` bit 0 = AIB data available to the ACCP | [V] |

**[OPEN]** `AOBASR` has no identified ACCP-side counterpart yet. It is read once at boot,
immediately before AOB, in `LOOK_HARD_1`. The ACCP's `TEST-BUFFERS <ASR/AOB>` command names an
ASR, so the register exists on that side too - but the address is not established. The
`0x220000` read path in section 8 is a candidate and nothing more.

---

### 5. The command channel - CPU asks, ACCP answers [V]

`ACCP_RDYW` (017073) is the request/response primitive:

```
ACCP_WAITO                     /* wait AIB not busy */
set MOD bit 27
SC12 := SC11 ; ACCP_WRITE      /* send the command number */
ACCP_WAITO
clear MOD bit 27
ACCP_WAITI                     /* wait for the answer */
ADR_ATRAP
if (returned AFLAG has bit 5 set)      /* an async message arrived mid-exchange */
    stash AFLAG -> RF2D, AOB -> RF2    /* queue it for ATRAP_CHK */
else
    zero both
```

The three commands:

| Cmd | Microcode routine | What comes back on AOB |
|---|---|---|
| 1 | `SYS_READ` (017111) | 3 words of system parameters (LSYSPAR) |
| 2 | `ASTS_BADAP` (017121) | ASTS + BADAP status words |
| 3 | `CPU_READ` (017130) | **the CPU model word** |

**Command 3 is the one that matters for bring-up.** `CPU_READ` decodes the returned word
through `CPU_MODEL00-17`, stores `CPUSAVE` / `VERSIONxx`, sets `CPU_AVAIL` or `CPU_UNAVA`, and
then emits `CPU_MESSAGE` (017301) - the "CPU available, model X version Y" report that makes
SINTRAN's `5OMBREAD` set `5ALIVE`.

An **emulated ACCP must answer command 3 or the CPU never announces itself.**

---

### 6. Message classes on this interface [V]

#### ACCP -> CPU, via AOB

- **Word streams**: **bit 15 set marks the final word** of a multi-word message. The drain
  loop is `OCB_MES_M` (016533): `ACCP_READ; repeat while (word & 0100000) == 0`.
- **Kicks**: single words, kick number in **bits 0-5**. Dispatched by `OCB_DECODE` (016417):
  bit 7 clear -> NOTREC 205; bit 6 -> NOTREC 206; bit 5 set -> the 64-entry kick table
  `OCB_DEC_K` (016430).
  - kick 0 -> NOTREC, **1 and 2 -> ACTIVATE**, **3 -> OCB_KICK03** (cache-clear / CLRKICK),
    **4 and 5 -> OCB_KICK05** (stop + clean queue), **6 -> OCB_KICK06** (forced de-schedule),
    7-63 -> NOTREC 204.
  - **Fast path** (016424): the word is XOR-compared against constant `0100501` first; an exact
    match jumps straight to `ACTIVATE`, bypassing the table.
- **Async-trap words**: subcode 0-7, dispatched by `TRAP_ATRPV` (016623). 1 = redefine system
  parameters, 2 = debug stop, 3 = debug start, others NOTREC.

Routing note **[V]**: `TRAP_OMESS1` (016413) branches on whether the **PROC0 cell (srf 2013)
is zero**. Zero means the microcode handles the word itself (`OCB_DECODE`); non-zero means it
is delivered to the software process-0 handler (`OCTO_SOFT`). It is **not** a comparison of the
word against an id - an earlier version of the catalog had that wrong.

#### CPU -> ACCP, via AIB

- Command numbers **1, 2, 3** (answers arrive on AOB, no async trap).
- Kick words: `0100001 | level` ("give interrupt"), `0100101 | dest`, `0100102 | cpu`.
  Bit 15 set marks a single-word message.
- **Boot acknowledge: `AIB := 0`** - written by `LOOK_SRF_1` when the SRF load is complete.

---

### 7. Big messages do NOT use this interface [V]

Multi-word OCB messages and the whole NK nucleus travel through **shared memory**, base
constant `START_MESS = 020000` physical. `ACCP_XWRITE` appends into that region via `RF2D`; it
never touches AIB.

The ACCP is expected to **fetch and deposit** those messages in shared memory itself. So an
implementation that models only AIB/AOB will handle commands, kicks and traps correctly and
will silently do nothing for real message traffic.

`TRAP_OCBM` (016727) builds the outbound form: header word `SC3|0100060`, route field
`SC3 & 037400 IX/8`, per-type payload via the 16-entry `TRAPOCB00` table, terminator
`SC3|0100040` (bit 15 = last). The header's `SC3` is `srf[2006]` = LSYSPAR word 1 =
`5OMDNO<<8`, so out-of-band reports are addressed to SINTRAN's receive OMD. Message codes
seen: 202B CPU available, 203B CPU unavailable, 204B-210B error / not-recognised.

---

### 8. Where the CPU model actually comes from - the full chain [X]

This is the part most likely to be got wrong, because three parties are involved.

```
MFbus controller            holds a CONFIGURED "which ND-5000 is this system" value
   |  octobus, CMD 5, multibyte
   v
ACCP                        model = 0x5000 | (reply content byte 1 << 8)
   |                        then CROSS-CHECKS it against its own hardware class
   |  AIB command 3 / answer on AOB
   v
ND-5000 microcode           CPU_READ -> CPU_MODEL00-17 -> CPU_MESSAGE
   |
   v
SINTRAN                     5OMBREAD sets 5ALIVE
```

So it is the **CPU's** model, **configured in the MFbus controller**, relayed by the ACCP, and
consumed by the microcode. It is not a memory type and not self-reported by the CPU.

#### THE MODEL IS ENCODED TWO DIFFERENT WAYS - do not plumb one into the other [X, 2026-07-30]

There are **two encodings of the CPU model** in this system and they are not interchangeable.
Confusing them is the single most likely mis-wiring on this interface.

**Authoritative source for the packed form**, verified against the manual text, not a summary:
`E:\Dev\Ronny\NDInsight\SINTRAN\Release-Documentation\ND-60230-5-EN SINTRAN III - Release Information - K-version.md`,
**Function 156a WRSYSINFO**, the second `INTEGER4`:

```
bits  0-15   Microprogram version
bits 16-19   CPU model:  2=ND-5200  4=ND-5400  5=ND-5500  6=ND-5600  7=ND-5700  8=ND-5800
bits 20-21   CPU type:   1=ND-5200  2=ND-5400/5500/5600/5700  3=ND-5800
```

So the whole `INTEGER4` is `(cpuType << 20) | (model << 16) | version`.

| Encoding | Where | ND-5800 looks like |
|---|---|---|
| **Packed byte** `(type << 4) \| digit` | the ND-5000 side's 202B model/version report, `(INTEGER4 >> 16) & 0x3F` | `0x38` |
| **Bare digit** | the ACCP's octobus CMD-5 discovery reply, content byte 1 | `0x08` |

Conversion:

```
bareDigit  = packedByte & 0x0F
packedByte = (cpuType << 4) | bareDigit
```

The ACCP firmware computes `model = 0x5000 | (bareDigit << 8)`. **Feeding the packed `0x38`
through that gives `0x7800`, which is not a model and is refused by the class check.**

> **THE CARD ITSELF CONVERTS BETWEEN THE TWO ENCODINGS - CMD-3 command `0x3E`.**
> Added 2026-07-31 (RetroCore side, verified by execution against a peer).
>
> Handler at **`0x66BE`** (the fall-through of the `0x3E` arm at `0x66B6`) builds:
>
> ```
> packedByte = (class << 4) | ((identityWord >> 8) & 0x0F)
> ```
>
> from the **class byte at `0x001131F6`** and the **identity word at `0x001131F8`**. Confirmed by
> running it: with a peer answering model digit **9**, the card replies content **`00 39`** -
> class 3 + digit 9 = ND-5900 - which is **exactly the packed WRSYSINFO byte**.
>
> **So the bare-digit form (MFbus discovery reply) and the packed form (SINTRAN / WRSYSINFO) are
> NOT rival conventions needing a decision.** `0x3E` is the converter, and it lives on the card.
> Anything that needs the packed byte should ask the card for it rather than composing one.
>
> This also independently corroborates the byte-vs-word warning below: the class at `0x001131F6`
> is a **BYTE** (`move.b` at `0x66EE`), so a word read yields `0x0300` for class 3.

#### CMD-3 reply convention [V, decoded from the peer side 2026-07-31]

Byte 0 is a status byte, byte 1 an error code, and error replies carry a constant `10 11` trailer:

| Command | Reply | Meaning |
|---|---|---|
| `0x3E` | `00 39` | OK, packed CPU model (class 3, digit 9) |
| `0x30` | `00 07 7F` | OK, the `0x077F` selftest status |
| `0x1F` | `FF 07 10 11` | error 07 |
| undefined | `FF 06 10 11` | error 06 = undefined command |

**`FF 01` therefore means a DEFINED command that returned error 01** - not "unknown command". That
distinction is what made the shared-enum refutation above decisive.

> **CONVENTION SETTLED 2026-08-02 (carve agent, `[V]` - both halves were previously guesses).**
> ack = a single `0x00`. nak = `FF <Messnak code> 10 11`.
>
> The confirming case is `ALIVE` (`0x1F`): the card answers `FF 07 10 11`, and ND-05.020.01 5.3.26
> documents exactly one nak for ALIVE - **`7 = NOT alive (stopped)`** - which is correct for a card
> with no microprogram running. That single agreement pins `0xFF` as the nak marker and byte 1 as
> the Messnak code. The table above was right; it is now evidenced rather than inferred.
>
> **The `0x30` row is ORDER-SENSITIVE and the table cannot show it.** `00 07 7F` is the reply when
> `RTEST` is sent **first, with nothing before it**. `CMSYSPAR` (`0x0E`) or `CPURES` (`0x39`) clear
> `0x001131E2` first, after which the same command returns `00 00 00` - and the console still prints
> `Selftest failed ... 077FH`. Two of the card's own outputs disagreeing, with the card entirely
> consistent and the measurement at fault. See the `0x001131E2` banner in section 2.
>
> **Malformed input is handled, not fatal `[V]`:** an OBCON message with an empty body gives
> `Communication error at address 6FE4H` -> `ACCP Software Reset performed` -> a clean reboot.

> **The ASCII collision is systematic, not a coincidence.** CPU type 3 puts `0b11` in exactly the
> bit positions where ASCII's `0x30` marker sits, so **every type-3 model reads as a plausible
> ASCII digit**: ND-5800 -> `0x38` = `'8'`, ND-5900 -> `0x39` = `'9'`. A type-2 machine reports
> `0x24` / `0x25` / `0x27` and the ASCII reading collapses immediately. The illusion survives only
> because every machine either side has looked at so far is type 3. **Do not treat this byte as
> text.**

**The ACCP's "class" is ND's "CPU type" field.** Same concept, and the tables line up:

| ACCP class (`0x1131F6`) | WRSYSINFO CPU type | Models |
|---|---|---|
| 1 | 1 | ND-5200 |
| 2 | 2 | ND-5400 / 5500 / 5700 (**and 5600 per the manual - see below**) |
| 3 | 3 | ND-5800 (**and 5900 per the firmware - see below**) |

#### Two divergences from WRSYSINFO, both now settled by the ROM [V]

1. **ND-5600 is in the manual's type 2 but the ACCP firmware does not accept it.** The class-2
   comparison chain is exactly three tests, byte-verified:

   ```
   1194  cmpi.w #0x5400,D0w ; beq accept
   119A  cmpi.w #0x5500,D0w ; beq accept
   11A0  cmpi.w #0x5700,D0w ; bne reject
   ```

   There is **no `0x5600` comparison**. So this is a real divergence in the firmware, not a gap in
   our carve. An ND-5600 reporting `0x5600` would be **refused** by this EPROM revision
   (`51200J` / `51201J`). Whether that is a firmware-revision matter or the manual being
   aspirational is **[OPEN]**.

2. **ND-5900 is accepted by the firmware but absent from WRSYSINFO.** `0x11E8` compares against
   `0x5900` and accepts it in class 3. WRSYSINFO's model list stops at `8 = ND-5800` and its type
   list likewise. The K-version document predates the ND-5900, so **defer to the carve** here.

#### A worse naming trap in the same manual page [V]

WRSYSINFO's **first** `INTEGER4` also has a field called **"CPU type"**, with overlapping values
and a completely different meaning:

```
bits 0-7   CPU type:  1 = ND-560 series   2 = ND-570 series   3 = ND-5000 series
```

So **"CPU type = 3" means "ND-5000 series" in the first INTEGER4 and "ND-5800" in the second.**
Two fields, same name, same value, different meanings, on the same manual page. Always say which
`INTEGER4` you mean.

**[OPEN]** the same page says WRSYSINFO returns `INTEGER ARRAY(0:8)` = **ECO level for 9 PCBs**,
while the ACCP's `Cmd1F_ReadEcoLevels` walks **ten** selectors. Not necessarily contradictory - the
card may read more than SINTRAN reports - but the counts differ and nobody has reconciled them.

#### The ACCP's cross-check - why a wrong digit is rejected [V]

The ACCP does not simply forward what the controller said. Routine `0x110A`:

```
clr.w  (0x001131FA)                     /* "model valid" = 0 */
jsr    0x121C                           /* octobus discovery -> D0 = reported model */
lea    (0x00114550),A0                  /* the signature matrix - see below */
cmpi.w #0x7F55,(A0,6) ; then +0x0C, then +4
```

| Class | `0x1131F6` | Default `0x1131F8` | Accepted reported models |
|---|---|---|---|
| 1 | 1 | `0x5200` | `0x5200` only |
| 2 | 2 | `0x5400` | `0x5400`, `0x5500`, `0x5700` |
| 3 | 3 | `0x5800` | `0x5800`, `0x5900` |

`0x1131FA` is the accept bit. If it stays zero, `0x120C` sets bit 15 of `0x1131E2` and the
console prints `MFbus controller has incorrect CPU model setting.`

#### The signature matrix at `0x00114550` [V]

**It is ACCP local SRAM (`0x110000`-`0x117FFF`), not shared memory and not a strap.** The
firmware clears and rebuilds it, so seeding it from outside is pointless. Builder at `0x7D26`:

```
/* Phase 1 - clear */
for (off = 0; off <= 0x1E; off += 2) word16[0x114550 + off] = 0

/* Phase 2 - sixteen sequential reads of the SAME address */
for (w = 0; w < 16; w++) read[w] = read_word16(0x00220000)

/* Phase 3 - 16x16 bit transpose */
for (bit = 0; bit < 16; bit++)
    for (w = 0; w < 16; w++)
        matrix[bit] bit w = read[w] bit bit

/* Phase 4 - a SECOND pass that REWRITES the matrix in place. NOT yet carved. */
```

**CORRECTION 2026-07-30 - phase 4 exists, and an earlier version of this document was wrong
to describe the matrix as the plain transpose.** The transpose loop ends at `0x7DCE`. The
builder then runs a further pass from `0x7DD0` which re-reads `0x114550` and writes it back:

 - it copies bit 10 of one word into bit 11 of another (`0x7DF4` `btst #0xA`, `0x7E0E` `bclr #0xB` / `0x7E16` `bset #0xB`).
 - it splits each word into fields and recombines them: `and #0x7800`, `asl #3` then `and #0x700`, `asr #3` then `and #0x60`, and `and #0x1F`.
 - it passes the combined low field through the routine at `0x7CA2`.

The class chain at `0x110A` and the ECO reader both read the **post-phase-4** matrix. Any
formula in this document that maps `read[w]` straight to `matrix[s]` therefore describes
phase 3 only and does NOT predict what the firmware finally sees.

**Measured, not assumed.** Feeding a sequence computed to make phase 3 produce
`matrix[3] = 0x7F55` yields `matrix[3] = 0x7A59` after phase 4, with all other words zero
(bits 2, 3, 8 and 10 differ). Hand-applying only the field moves listed above predicts
`0x3A75`, so the helper at `0x7CA2` accounts for the rest. **UNVERIFIED:** whether phase 4 is
a pure per-word bit permutation. It is per-word and bit-preserving in the one case measured,
which is one data point, not a proof.

**Phase 4 is fully carved and inverted (2026-07-31).** Per matrix word:

```
m = matrix[s]
m bit11 := m bit10                      /* 0x7DF4 btst 10, 0x7E0E bclr 11, 0x7E16 bset 11 */
hi   = m & 0x7800                       /* bits 11-14 kept                                */
mid  = (m << 3) & 0x700                 /* bits 8-10 := bits 5-7                          */
low  = ((m >> 3) & 0x60) | (m & 0x1F)   /* bits 5-6 := bits 8-9; bits 0-4 kept            */
low  = gray_to_bin7(low)                /* the helper at 0x7CA2                           */
matrix[s] = hi | mid | low              /* bits 7 and 15 are DROPPED                      */
```

`0x7CA2` is a **7-bit Gray-to-binary decoder**: `out[6] = in[6]`, then
`out[i] = in[i] XOR out[i+1]` for i = 5 down to 0.

Because bits 7 and 15 are discarded, phase 4 is **not invertible in general**; pick the
representative with those bits zero. To make phase 4 output `0x7F55`, phase 3 must produce
**`0x77FF`**.

So the full recipe to choose a class is: desired final matrix -> invert phase 4 per word ->
invert the transpose -> serve those sixteen words at `0x220000`. **Live-verified 2026-07-31**
(RetroCore, 70/70 tests): a class-2 sequence yields `matrix[3]=0x7F55`, `0x1131F6=0x02` (class
2), `0x1131F8=0x5500` (ND-5500), `0x1131FA=0x0001` (accepted). `0x5500` is the right probe
because, unlike `0x5800`, it is never written as a default - the firmware can only print it if
the reply was genuinely consumed.

Matrix word index `s` is byte offset `s*2`. Because `matrix[s] bit w = read[w] bit s`,
requiring `matrix[s] == 0x7F55` means:

> **[SUPERSEDED 2026-07-31 - THE BIT LIST BELOW IS WRONG. DO NOT IMPLEMENT IT.]**
> It describes phase 3 only. The builder runs a fourth phase at `0x7DD0` that rewrites every
> word before the class chain reads it, so reads built to make **phase 3** produce `0x7F55`
> yield `0x7A59` in the final matrix and the model is REFUSED. This exact failure was hit and
> measured. To get `0x7F55` out of phase 4, phase 3 must produce **`0x77FF`** - see part 5
> section 3. Kept here because the derivation of the transpose itself is still correct and
> instructive.

```
read[w] bit s = 1  for w in {0,2,4,6,8,9,10,11,12,13,14}
read[w] bit s = 0  for w in {1,3,5,7,15}
```

(`0x7F55` = `0111 1111 0101 0101`.)

The **same matrix** carries ECO levels, read by `Cmd1F_ReadEcoLevels` (`0x9F12` -> `0x9F78`):

```
eco(s) = (matrix[s] >> 11) & 0x0F        /* 0x0F means "absent", printed as 00 */
```

which through the transpose is

> **[CORRECTED 2026-07-31 - the line below has an off-by-one and the corrected form follows.]**
> The `eco(s) = (matrix[s] >> 11) & 0x0F` line above is FINE: it reads the final matrix. The
> translation below is not, because it assumes the final matrix is the plain transpose. Phase 4
> (`0x7DD0`, see part 5) **overwrites bit 11 with bit 10** and then keeps bits 11-14. So the
> final word's bit 11 carries phase-3 bit **10**, not bit 11:
>
> ```
> eco(s) = (read[10]>>s & 1) | (read[12]>>s & 1)<<1 | (read[13]>>s & 1)<<2 | (read[14]>>s & 1)<<3
> ```
>
> i.e. `read[11]` must be `read[10]`; bits 12-14 pass through phase 4 untouched and are correct
> as written.
>
> **VERIFIED 2026-07-31 against the real ROM** - no longer a derivation. An arbitrary
> sixteen-word sequence was fed to the firmware, the matrix was read back out of machine memory
> at `0x114550`, and the ECO field the firmware would print was compared against both candidate
> formulas:
>
> ```
> s= 0 matrix=0x5D2C eco=B fromRead10=B fromRead11=A
> s= 1 matrix=0x2E23 eco=5 fromRead10=5 fromRead11=4
> s= 2 matrix=0x6524 eco=C fromRead10=C fromRead11=D
> s= 3 matrix=0x5623 eco=A fromRead10=A fromRead11=B
> ```
>
> `read[10]` matches on every selector; `read[11]` is wrong on every one. Pinned by
> `EcoField_ComesFromReadWord10_NotReadWord11`, which also asserts the `read[11]` form is
> observably WRONG so the test cannot pass vacuously.
>
> The extraction itself is confirmed byte-for-byte in `Cmd1F_ReadEcoLevels`: `0x9FB0`
> `lea (0x114550).l,A0`, `0x9FB6` load `matrix[s]`, `0x9FBA` `asr #11`, `0x9FC0` `and #0x0F`,
> `0x9FC6` compare against `0x0F` and print `00` for "absent".

```
eco(s) = (read[11]>>s & 1) | (read[12]>>s & 1)<<1 | (read[13]>>s & 1)<<2 | (read[14]>>s & 1)<<3
```

Ten selectors are walked, in this order: **0, 1, 2, 4, 5, 8, 6, 0x0C, 0x0D, 3**, labelled from
a 12-byte descriptor table at `0x00012D5C`.

**[I]** `0x7F55` reads as an **"absent / invalid slot" sentinel**: its bits 11-14 are all ones,
which is exactly the `eco == 0x0F` "absent" case, and `Selftest_ProbeCacheAndAap_B` (`0xF28E`,
`0xF2DE`) uses the same test to skip a test rather than run it.

**Practical consequence.** With all-zero reads, `matrix[3] != 0x7F55`, so **class 3 is chosen
and only model digits 8 or 9 are ever accepted.** That is correct firmware behaviour. To reach
class 2 or 1, bit 3 (then bits 6 and 2) of those sixteen reads must follow the pattern above.

> **[SUPERSEDED 2026-07-31 - the last sentence sends you to the wrong pattern.]** "The pattern
> above" is the phase-3-only bit list, which does not work. The first two sentences stand: an
> unmodelled port really does give class 3 and digits 8/9 only. For how to actually reach class
> 1 or 2, use **part 5 section 3** - invert phase 4 per word, THEN invert the transpose. That
> path is live-verified: class 2 established, ND-5500 accepted.

**[OPEN]** what the hardware actually presents on those sixteen reads. `LOOK_HARD_1` (017472)
shows the ACCP is the *source* of a hardware-configuration word toward the CPU, so the matrix
is read from the datapath or backplane rather than from the CPU - direction clear, source not
proven.

---

### 9. Open items - do not paper over these

1. **AFLAG bits 7 and 8** were never re-verified after the off-by-one correction. Same risk.
2. **`AOBASR` has no established ACCP-side address.**
3. **What drives the sixteen `0x220000` reads** (section 8).
4. **Request content byte `0x03`** in the octobus discovery message. The only undecoded field
   in the outgoing message; neither ND-14001 nor ND-05.017.01 nor the ACCP ROM explains it.
5. **`0x00220000` write codes** other than `0x0005`: `0x300F`, `0x400A`, `0x400C`, `0x000F`
   appear in the MFbus memory transaction, and the meaning of the fields is not decoded.
6. **A live emulator defect**: only the FIRST content byte of an octobus multibyte reply
   reaches the ACCP driver's receive buffer, so the model digit cannot currently get through.
   Buffer dump at data area `0x00112D54` after a six-byte reply: `byte1=0x02` source,
   `byte3=0x05` own CMD, `byte4=0x06` length all correct, `byte5=0x00` content[0], then zeros.
7. **`0x900001` returns station 1** in the emulator, which is the ND-120 CPU slot; a local
   octobus node should be 20-77 octal. **[I]** the register is probably the WOI/STANO value the
   MFbus controller writes during crate configuration - the 5-bit mask matches WOI's STANO
   field and the board has no switches to read. If so, a correct model writes it *before* the
   ACCP boots, and the discovery scan would never run.

---

### 10. Minimum viable implementation order

1. `0x660001` bits 0 and 1 as real gates, plus the `0x440000` / `0x550000` data pair and the
   `0x330000` bit-6 strobe. Without these nothing else can be exercised.
2. Answer **AIB command 3** with a CPU model word. Digit **8** (ND-5800) is the only value that
   works while the signature matrix reads zero.
3. Kicks 1 and 2 -> ACTIVATE, since those are what actually start work.
4. The `0x220000` sixteen-read sequence, if you want a model class other than 3, or meaningful
   ECO levels.
5. Shared-memory message fetch/deposit at base `020000` - required before any real message
   traffic works.

---

### 11. Related documents

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` - the CPU-side catalog, with the re-verification sweep and its corrections
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md` - the octobus protocol and the ACCP's octobus driver
- part 1 of this file - the ACCP firmware write-up of record
- part 2 of this file - the full ACCP peripheral address sweep
- part 2 of `ACCP-EMULATION-STATUS-AND-HANDOFF.md` - section 4z, the MFbus-controller peer requirements
- part 3 of this file - all 43 console commands, including the AOB/AIB/ASR ones used as evidence here

### Provenance

Sections 2, 5, 6 and 7 are from the microcode listing. Sections 3, 4 and 8 are from the ACCP
EPROM. The correspondence table in section 4 and the chain in section 8 are where the two meet;
each row there carries its own confidence mark. The register-direction convention in section 1
is confirmed from both sides independently - the microcode's operand directions and the ACCP's
own console command names agree without having been made to.

---

# Part 5 - originally `ACCP-SIGNATURE-MATRIX-AND-CPU-CLASS-CARVE-2026-07-31.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## ACCP signature matrix and CPU model class - full carve

**Date:** 2026-07-31
**Firmware:** `octo.bin`, ND-324716 Samson ACCP EPROM set (128 KB, SHA256
`0EA81716AD81984B64675E9A8CCEB6C1909AB299BE0048857C58F85C3479C5F1`)
**Status:** SOLVED and live-verified. RetroCore ACCP suite 70/70 green.

---

### 1. What this answers

The ACCP asks the MFbus controller what CPU model the system is, then **refuses to believe it**
unless the model falls inside a *class* the ACCP derives from its own hardware. Until now the
emulated machine could only ever produce **class 3**, so only ND-5800 and ND-5900 were
acceptable and every other model printed:

```
MFbus controller has incorrect CPU model setting.
```

This document carves the whole derivation and shows how to select any class.

---

### 2. The class chain at `0x110A` [V byte-cited]

The chain probes the matrix at `0x00114550`. **The index registers are BYTE offsets**, which is
easy to misread as word indices:

| Instruction | Register | Byte offset | Matrix WORD |
|---|---|---|---|
| `0x112A moveq #6,D1` | D1 | +6 | word **3** |
| `0x113C moveq #0xC,D2` | D2 | +0x0C | word **6** |
| `0x1146 moveq #4,D3` | D3 | +4 | word **2** |

Each is compared against **`0x7F55`**, the "absent / invalid slot" sentinel.

| word3 | word6 | word2 | Result |
|---|---|---|---|
| `== 0x7F55` | `== 0x7F55` | `== 0x7F55` | **class 1**, accepts `0x5200` |
| `== 0x7F55` | `!= 0x7F55` | `!= 0x7F55` | **class 2**, accepts `0x5400` / `0x5500` / `0x5700` |
| `!= 0x7F55` | (see below) | | **class 3**, accepts `0x5800` / `0x5900` |
| any other mix | | | `0x1131F6` cleared - **every model refused** |

Stores: class byte at `0x001131F6` (**`move.b`** at `0x1150` / `0x1184` - it is a BYTE, so a word
read there returns `0x0Cxx`), model word at `0x001131F8`, accept flag at `0x001131FA`.

**Two settled divergences from ND-60230-5-EN Function 156a WRSYSINFO** (both re-confirmed here):
class 2 makes exactly three comparisons - `0x1194` `0x5400`, `0x119A` `0x5500`, `0x11A0` `0x5700`
- so there is **no `0x5600` test** and an ND-5600 is refused by this EPROM; conversely the
firmware **does** accept `0x5900` at `0x11E8`, which WRSYSINFO predates.

---

### 3. The matrix builder at `0x7D26` has FOUR phases, not three

This is the single most important correction in this document. Earlier write-ups described the
matrix as the plain transpose of sixteen reads. **That describes phase 3 only and does not
predict what the class chain sees.**

| Phase | Range | What it does |
|---|---|---|
| 1 | `0x7D2E`..`0x7D4C` | zero all sixteen words |
| 2 | `0x7D52`..`0x7D74` | read `0x00220000` sixteen times into a local buffer |
| 3 | `0x7D7E`..`0x7DCE` | 16x16 bit transpose |
| **4** | **`0x7DD0`..`0x7EA4`** | **rewrite every word IN PLACE** |

#### Phase 3 - the transpose

```
for (bit = 0; bit < 16; bit++)
    for (w = 0; w < 16; w++)
        matrix[bit] bit w = read[w] bit bit
```

So `matrix[s]` gathers the s-th bit of every read. This transpose is its own inverse over the
square, which is why it looked like the whole story.

#### Phase 4 - the rewrite [V byte-cited]

Runs once per matrix word, reading and writing `0x114550` directly:

```
m = matrix[s]
m bit11 := m bit10                      /* 0x7DF4 btst #10, 0x7E0E bclr #11, 0x7E16 bset #11 */
hi   = m & 0x7800                       /* 0x7E28  bits 11-14 kept                           */
mid  = (m << 3) & 0x700                 /* 0x7E3C  bits 8-10 := bits 5-7                     */
low  = ((m >> 3) & 0x60) | (m & 0x1F)   /* 0x7E52 / 0x7E6A  bits 5-6 := bits 8-9; 0-4 kept   */
low  = gray_to_bin7(low)                /* 0x7E74  bsr 0x7CA2                                */
matrix[s] = hi | mid | low              /* 0x7E94                                            */
```

**Bits 7 and 15 appear in no output field, so they are DROPPED.** Phase 4 is therefore *not*
invertible in general.

#### The helper at `0x7CA2` is a 7-bit Gray decoder [V]

```
out[6] = in[6]                          /* 0x7CB4 btst #6                     */
for (i = 5; i >= 0; i--)
    out[i] = in[i] XOR out[i+1]         /* 0x7CFC eor, loop guard 0x7D16      */
```

Standard Gray-to-binary, over 7 bits rather than 16. Its inverse is
`gray = bin XOR (bin >> 1)`.

#### The anchor - measured, not merely read off the listing

A sequence built so that **phase 3** yields `0x7F55` in word 3 produces, on the real ROM:

```
matrix word 3 = 0x7A59
```

Hand-running `0x7F55` through the model above gives **`0x7A59` exactly**. That is what turns
this from a plausible reading into a confirmed one, and it is pinned by
`Phase4Forward_MatchesTheValueTheFirmwareProduced`.

Working: `0x7F55` -> `hi=0x7800`, `mid=0x200` (bits 5-7 = `0,1,0`),
`low=0x60|0x15=0x75` -> `gray_to_bin7(0x75)=0x59` -> `0x7800|0x200|0x59 = 0x7A59`.

#### Inverting it

To make phase 4 **output** `0x7F55`, phase 3 must **produce `0x77FF`**:

```
low  = bin_to_gray7(0x7F55 & 0x7F) = bin_to_gray7(0x55) = 0x7F
m bits 0-4  = low & 0x1F         = 0x1F
m bits 8,9  = (low >> 5) & 3     = 3
m bits 5-7  = (0x7F55 >> 8) & 7  = 7
m bit 10    = (0x7F55 >> 11) & 1 = 1
m bits 12-14= 0x7F55 & 0x7000    = 0x7000
                                 -> 0x77FF
```

Verified forwards: `0x77FF` -> bit11:=bit10 -> `0x7FFF` -> `hi=0x7800`, `mid=0x700`,
`low=0x7F` -> `gray_to_bin7(0x7F)=0x55` -> **`0x7F55`**.

**Full recipe:** desired final matrix -> invert phase 4 per word -> invert the transpose ->
serve those sixteen words at `0x220000`.

Note `0` maps to `0` through phase 4, which is what lets the untouched matrix words stay "not
the sentinel" for classes 2 and 3 without any extra work.

---

### 4. The `0x220000` read port - model it as ARMED, never as a counter

#### Exactly two readers exist [V byte-searched]

A search for every absolute-long reference to `0x00220000` (60 hits) filtered to source-operand
opcodes gives **two** read sites:

| Address | Form | Role |
|---|---|---|
| `0x7D52` | `3d 79 ...` `move.w (0x220000).l,(0x14,A6)` | the sixteen-read burst |
| `0x7BD2` | `30 39 ...` `move.w (0x220000).l,D0w` | **a dummy read - `D0` is never used again** |

`0x7BD2` sits in `CmdPortWithLatchGate`; the following instructions load `D2`, `D3`, `D1` and the
routine then calls the builder. Its result is discarded, so the port may serve it anything - but
it must **not** consume a sequence entry.

*(A byte search for `??39 00220000` alone misses the burst, which encodes as `3d 79`. Search both
forms, or the site count comes out wrong.)*

#### The arming signal [V]

```
0x7C18  move.w #0x0005,(0x00220000).l
0x7C20  move.w #0x0007,(0x00220000).l     <-- ARM
0x7C28  jsr    0x00007D26                 <-- the builder
```

Between the arm and the first burst read there is only the builder prologue and its clear-loop,
neither of which touches the port. So:

> **A write of `0x0007` arms the sequence at entry 0. Sixteen reads consume it, then it
> disarms. Reads while disarmed return a constant and consume nothing. Writes arriving while
> armed are IGNORED.**

The last clause matters: the kick-service routine at `0x6C0` writes the very same `0x0007` at
`0x788`, so an interrupt landing mid-burst would otherwise restart the sequence and corrupt the
matrix.

> **CORRECTION 2026-07-31 - which interrupt level.** An earlier version of this said "the IRQ3
> handler at `0x788`". **Wrong.** The routine at `0x6C0` is not an interrupt handler at all; it
> is a shared subroutine with exactly two callers - `0x4F4`, inside `Vec26_AutoIrq2`, and
> `0x6AA`, inside `Vec28_AutoIrq4`. The vector table settles it: **IRQ3 is `0x510`** and
> **IRQ4 is `0x694`**, and `0x6C0` lies past both. The write therefore arrives from **IRQ level
> 2 or 4, never level 3**.
>
> The error came from trusting the Ghidra symbol `Irq3KickServiceAndTrace`, which was itself a
> misnomer; the function has since been renamed `KickServiceAndTrace_FromIrq2AndIrq4` and given
> a plate comment recording the callers. **The armed-port model is unaffected** - an interrupt
> really can write mid-burst - but anyone masking an interrupt to test this must mask level 2
> and level 4, not level 3.

#### Two designs that FAIL - do not retry them

| Design | Failure |
|---|---|
| reset the index on **any** write | the frequent `0x0005` writes re-phase the burst; matrix word 3 came out `0x7A59`-like garbage |
| reset only on a write **outside** a burst | the dummy read at `0x7BD2` pre-advances the index to 1, so **both** arming writes look mid-burst and are ignored; the burst runs one entry late. Matrix word 3 = `0x3D4C` |

Both failures come from treating a shared counter as if it tracked the burst. **The arm signal is
what tracks it.**

#### A constant cannot substitute

With every read equal, the transpose can only produce matrix words of `0x0000` or `0xFFFF` -
never `0x7F55`. An ordered sequence is genuinely required.

#### Health check

With the armed model, one boot gives **48 consumed reads = exactly 3 complete bursts**, 4 arms,
3 disarmed dummy reads, 0 mid-burst writes. **Consumed reads must always be a multiple of 16**;
anything else means a burst was truncated.

---

### 5. Live verification (RetroCore, 2026-07-31)

Class-2 sequence + MFbus peer reply with model digit 5:

```
matrix word 3   = 0x7F55      (was 0x7A59)
0x1131F6 class  = 0x02        (was 0x03)
0x1131F8 model  = 0x5500      (was 0x5800)
0x1131FA valid  = 0x0001      accepted
console         : CPU model: ND-5500
```

**Why `0x5500` is the right probe:** `0x5800` is also the class-3 default written at `0x11DA`
*before* any comparison, so digit 8 passes even if the reply was ignored entirely. `0x5500` is
reachable only through a consumed reply **and** a correctly selected class. For the same reason,
never test with digit 8.

---

### 6. Implementation

RetroCore, `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\`:

| File | Role |
|---|---|
| `src\Devices\AccpSignatureMatrix.cs` | `ReadsForMatrix` (transpose inverse), `Phase4Forward`, `Phase4Inverse`, `GrayToBinary7`, `BinaryToGray7`, `ReadsForCpuClass` (inverts BOTH stages) |
| `src\Devices\AccpSignatureReadPort.cs` | the armed/disarmed `0x220000` port, with `BurstsArmed` / `IdleReadsServed` / `MidBurstWritesIgnored` diagnostics |
| `src\AccpMachineConfig.cs` | `SignatureMatrixReads` (default null = unmodelled port = class 3) |
| `tests\AccpSignatureMatrixTests.cs` | the anchor test, a full round-trip sweep, port-burst integrity, and the two live class tests |

`SignatureMatrixReads` defaults to **null**, which preserves the historical behaviour: an
unmodelled port reads a constant, the matrix is all zeros, class 3 is derived and ND-5800 is
accepted. That is *correct firmware behaviour for a machine with no datapath*, not a defect, so
it stays the default.

---

### 7. Corrections to earlier documents

Recorded because both were stated confidently and both were wrong:

1. **"The matrix is the transpose of the sixteen reads."** Incomplete - phase 4 rewrites it
   afterwards. Any formula mapping `read[w]` straight onto `matrix[s]` describes phase 3 only.
2. **"51 reads per boot from multiple sites, so a positional sequence cannot work; the design
   needs replacing."** Wrong on the premise. There are exactly **two** readers. The 51 figure was
   an artefact of a broken alignment rule counting reads it should not have consumed. The design
   needed a different rule, not replacing.

---

### 8. Still open

- ~~The routine that *calls* `CmdPortWithLatchGate` runs the builder three times per boot~~ -
  **CLOSED 2026-07-31. It is not a loop: `CmdPortWithLatchGate` @ `0x7BAC` has exactly THREE
  callers** - `0x7A1C` (`ControlStore_Helper_7A1C`), `0xEF58` (`Selftest_Helper_EF58`) and
  `0xEFC4` (`Selftest_AnnounceLoadingControlStore`) - and each calls the builder once. 3 x 16
  reads = the 48 measured. No caller expects a different sequence; they all read the same port.
  The routine is also **misnamed**: the signature read is only its preamble. Its body zeroes the
  16-byte control-store buffer at `0x1144F0`, plants microword fields `0x0007` / `0xB000`, shifts
  the 128-bit word out via `0x773E`, issues command `0x001A`, and clears the sticky control-store
  error latch `0x1131E2`. See the plate comment at `0x7BAC`.
- `0x220000` write codes - **structure carved 2026-07-31, meanings still UNPROVEN.** All 52
  immediate-write sites are enumerated (`33 fc ?? ?? 00 22 00 00`). There is a clear grammar:
   - a bracketed triplet `0x300F` -> `0x40xx` -> `0x000F` for MFbus memory transactions, where the selector sits in the middle word. `0x400A` is issued BEFORE the data-pair write and `0x400C` / `0x400D` AFTER it, so the triplet brackets a transfer rather than being one command.
   - bare commands without the bracket at `0x71B4`: `0x4009` after the LOW half is written to `0x440000`, then `0x2011` after the HIGH half is written to `0x550000`, then both are read back.
   - the alternating `0x0010` / `0x000F` clock pair in the three shift routines, where write-vs-read is distinguished only by the PHASE ORDER of the pair.
  Three codes not previously recorded anywhere: **`0x4009`, `0x400D`, `0x001A`**.
  **The function-code meanings are deliberately NOT named.** Five `0x40xx` sites is not a survey,
  and this document already records two occasions where a whole port model was wrongly
  generalised from a single routine (sections 2.4b and 2.4h). Naming them needs ND-14001 chapter
  4 for this device.
- **[INFERENCE, not proven]** `0x0007` may be a general **shift-register rewind** rather than
  "arm the signature sequence" specifically. That would explain why the identical value appears
  on the AOB-timeout path at `0x788`, and it predicts exactly the emulator behaviour already
  required. Nothing in the image proves it.
- Whether phase 4 is a **pure per-word bit permutation** is UNVERIFIED. It is per-word in every
  case observed, and the model round-trips across all producible values, but that is a property
  of the model - not proof about the silicon.
- What real hardware actually presents at `0x220000` - i.e. whether the ND-5000 datapath drives
  a shift register, a wired signature, or something else - is unknown. We model the *sequence the
  firmware consumes*, not the mechanism that produces it.

---

### 9. Related

- part 4 of this file - the shareable CPU-side interface spec
  (carries a correction block pointing here)
- part 1 of this file - the firmware reverse-engineering base
- part 2 of this file - the `0x220000` / `0x330000` / `0x440000` selects
- `OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md` - the discovery exchange that supplies
  the model digit
- Memory `accp-cpu-model-crosscheck.md`

---

# Part 6 - originally `PIOC-OS-VS-ACCP-FIRMWARE-COMPARISON-2026-07-27.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## PIOC-OS (ENCOS Ethernet II) vs the ACCP octobus firmware

**Date**: 2026-07-27
**Question asked**: is the ACCP's firmware "very similar" to the PIOC-OS that runs the
Ethernet controller?

**Short answer: same compiler and same house style, but NOT the same operating system.**
PIOC-OS is a priority-preemptive multitasking kernel with a trap-based system-call API.
The ACCP firmware has no kernel at all - it is a single-threaded monitor program with
interrupt handlers. The reuse between them is the PLANC-MC runtime, not an RTOS.

Sources:
- PIOC-OS: `..\..\Installation\Communication\Ethernet\x\stripped\docs\PIOC-OS\` (10 documents)
- ACCP: part 1 of this file and the live Ghidra database `octo.bin`
- Both images are open in Ghidra, so every claim below was checked against both, not
  recalled.

---

### 1. What PIOC-OS is

PIOC-OS runs on the ENCOS Ethernet II controller card (`encos-ser-all-banks-68k.bin`,
MC68000, PLANC-MC, ND 1986). It is a real RTOS:

| Property | Value |
|---|---|
| Structure | **nine linked modules** with a circular directory of 32-byte records at 0x05C8, each carrying an 8-char name and an ASCII build date |
| Scheduling | **priority-preemptive**, 16 ready queues at 0x0B06, highest priority wins |
| Processes | **three**: `FREE` (priority 5, idle), `PRO1` (priority 1, the application), `RTC ` (priority 14, the tick) |
| Objects | 30-slot object table at 0x0A8A, heap-allocated descriptors |
| System calls | **`trap #2`, 27 services**, dispatch table at 0x0C6A, D0 = function code, A0 = argument block |
| Context switch | deferred preemption by **return-address hijack** - the event poster rewrites the interrupt frame's saved PC to the scheduler address so `rte` lands in the scheduler |
| Time | MFP timer at 2457600 Hz, ISR on vector 69, 32 timer elements |
| IPC | three layers - per-process event bit-sets, named ports (`PORT*`/`PONA*`), and sub-process work queues |
| Memory | general heap, a never-reclaimed PLANC frame arena, and about nine fixed pools |
| Host link | ND-100 doorbell on vector 78 in, SCIP at 0x00EF0080 out, monitor SUBFN dispatch at 0x0512 |

The single mechanism to understand is the deferred preemption: nothing switches inside the
code that makes a process runnable; it sets a flag and rewrites the exception frame, guarded
by a **double latch** (0x0660 and 0x0662) so a second event post cannot destroy the saved PC.

---

### 2. What the ACCP firmware is

`octo.bin`, the Samson ACCP (ND-324716), December 5, 1988. Checked in Ghidra today:

- **No `trap #2` anywhere.** Searching the whole 128 KB for `4E 42` returns 11 hits and every
  one of them falls inside the string region or at an odd address. There is no kernel entry
  point and no service dispatch table.
- **The one distinguished TRAP is #10, and it is a fault path, not an API.** The handler at
  0x08A4 does `addi.l #-2,(2,SP)` to back the saved PC onto the trapping instruction, stores
  fault code 0x2A at 0x00113112, and falls straight into `FaultRecordAndPanic` (0x08C4) -
  the same place every processor fault stub goes. Its job is to print
  `"6 8 0 0 0   T R A P : "` and the register dump, not to serve a request.
- **No module directory.** ENCOS has nine records with build dates. ACCP has exactly one
  version record - a numeric date string `"88.12. 5"` followed by an array descriptor
  `{origo 0x13BFC, lower 0, upper 0x0F}` over `"December 5, 1988"`, at 0x13BF4. One module,
  one link.
- **No process table, no ready queues, no object handles.** Instead the strings name a
  **single main loop**: `"Error exit from idle loop"`. Work arrives by interrupt (autovectors
  IRQ1-7 at 0x4BA/0x4C6/0x510/0x694/0x796/0x7A8/0x826) and by console command.
- **Its "API" is a human command line** - the 43-entry console command table at 0x130FE
  (LOAD-CONTROL-STORE, START-MICROPROGRAM, SEND-OCTOBUS, RESET-CPU, READ-ACCP-STATUS,
  RUN-SHORT/LONG-SELFTEST, ...), reached over the SCN2681 console at 0x00DD0000.

---

### 3. What the two genuinely share

This is where the "very similar" intuition is right, and it is worth being precise about it,
because it is the part that transfers.

| Shared | Detail |
|---|---|
| **Compiler** | PLANC-MC on MC68000, both ND-built |
| **Skip return** | normal return goes to RETLINK+2; the 2 bytes after every call are the error slot |
| **`jmp (A5)` error unwind** | A5 permanently holds the runtime error vector. ENCOS: `#XRET` 0x135A8 / `#ERET` 0x13596. **ACCP: 0x115AE**, loaded at 0x0900 |
| **A6 bump-allocated frames** | frames live in an arena and are never popped; `move.l A2,(A6)` publishes the next-free cursor. ACCP's TRAP handler does this inline at 0x08EC-0x08FA with arena base 0x00112800 |
| **Array descriptors** | `{origo, lower, upper}` passed by copying three longwords into the callee frame |
| **`$` = newline** | 0x24 terminates/breaks every string in both images |
| **Fault-record-and-panic** | both funnel every processor fault through one recorder that saves SR/PC/SP/A6 and the register file, then prints |
| **Leaf runtime** | the same `#IMU`/`#IDV`/`#APPD`/`#REMV` style helpers, register-argument and plain `rts` |

So the **`ghidra-planc` skill and its five scripts apply to both images.** That is the real
carry-over.

---

### 4. Where they differ in ways that will bite

Do not copy PIOC-OS offsets into ACCP work.

| | PIOC-OS (ENCOS, 1986) | ACCP (1988) |
|---|---|---|
| First "further" parameter | **+0x12** | **+0x14** |
| ERRCODE | 16-bit at +0x10 | (consistent with +0x14 parameter start) |
| Array descriptor | **8 bytes** `{long origo, word lower, word upper}` | **12 bytes** `{long origo, long lower, long upper}` |
| Argument staging | through `(0x4,A6)`, the outgoing-frame pointer | through **`(A6)`** directly |
| Symbol table | **ND linker symbols present**, 241 names at file offset 0x663E0 | **none** - candidates all fall inside the microcode blob |
| Kernel | trap #2, 27 services | none |

The descriptor-width difference is the PLANC-MC **version-F word-size boundary** (word went
from 2 bytes to 4). ACCP is on the later side of it, ENCOS on the earlier. Two years apart,
and it silently doubles every descriptor.

---

### 5. What this means practically

1. **Do not go looking for a PIOC-OS in `octo.bin`.** It is not there. Time spent hunting an
   object table or a trap dispatcher is time wasted; the carving targets are the octobus and
   MF-bus drivers and the command table.
2. **Do reuse the PLANC tooling and reading discipline** - skip returns, error slots, the
   arena, descriptor-chasing to resolve strings. That technique is exactly how the MF-bus
   routine at 0x70CC was identified (its timeout descriptor resolves to
   `"$MF-bus memory timeout$"`).
3. **The ACCP is the simpler machine to emulate.** No scheduler, no descriptor size that is
   only computable at boot, no host-supplied board-config record gating the heap. A single
   main loop plus interrupt handlers plus the DUART is the whole model - which is what the
   RetroCore handoff assumes.

---

### 6. UNVERIFIED / open

- Whether the ACCP firmware has **any** multitasking at all, cooperative or otherwise, has
  been established only negatively (no trap kernel, no process table, an idle-loop string).
  The main loop itself has not yet been transcribed. That is not the same as proving it is
  strictly single-threaded.
- Whether ND ever shipped a PIOC-OS-based ACCP in an earlier revision (ND-324702) is unknown.
  Only the 324716 image has been read.

### Provenance

Section 1 is a condensation of the ten PIOC-OS documents. Section 2's negative findings were
run against `octo.bin` in Ghidra on 2026-07-27 - the `4E 42` byte search, the TRAP #10
handler disassembly, the string enumeration (154 strings) and the version record hexdump.
Section 3 and 4's shared/divergent items come from both databases.
