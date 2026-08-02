# The ACCP octobus command table - all 46 dispatcher arms

**Date:** 2026-08-02
**Image:** `Installation\Communication\OctobusAccp\eprom\octo.bin` (SHA256 `0EA81716AD81984B...`, 131072 bytes)
**Ghidra program:** Raw Binary, `68000:BE:32`, base 0 (loaded from `C:\Temp\octo\octo.bin`, byte-identical)

Evidence tags: **[V]** proved from firmware behaviour or a hardware code - **[I]** inferred, not
proved - **[OPEN]** behaviour known, name not established - **[inh]** inherited from an earlier carve,
provenance not re-checked here.

**Arm addresses are `[V]` for all 46** - every `cmpi.b` immediate was read directly out of the image
and all 46 matched the `0C 00 00 <imm>` + `66` shape with zero mismatches.

---

## The table

| Code | Octal | Arm | `CM*` | Command | Basis |
|---|---|---|---|---|---|
| `0x0D` | 015B | `583A` | - | RECO, Read ECO Levels | `[I]` returns 3 words from `1143A0/A2/A4` |
| `0x0E` | 016B | `57E8` | `CMSYS` | CMSYSPAR / LSYSPAR | `[inh]` + clears `1131E2` |
| `0x0F` | 017B | `5736` | `CMTEC` | **ECHO** | `[V]` collects n bytes, sends the same n back; **no guards** |
| `0x10` | 020B | `6562` | `CMREA` | - | `[OPEN]` returns 16 words from `114550` |
| `0x11` | 021B | `5980` | `CMLPA` | **LPARP** | `[V]` writes the parameter pointer + flag |
| `0x12` | 022B | `59B6` | `CMVER` | VPARP | `[inh]` |
| `0x13` | 023B | `4D50` | `CMWWC` | LOCSM | `[I]` memory -> `1144F0` array + checksum, shares `CmdPortWrite_A` with console load |
| `0x14` | 024B | `4EDC` | `CMDWW` | LOCSD | `[I]` 8 words = 128 bits + checksum |
| `0x15` | 025B | `4FC0` | `CMADR` | - | `[OPEN]` memory param; loops `ControlStoreWriteWithVerify` |
| `0x16` | 026B | `519C` | `CMDRW` | - | `[OPEN]` 14-bit CS address, Messnak 3 |
| `0x17` | 027B | `56BC` | - | - | `[OPEN]` latch enable + sets running |
| `0x18` | 030B | `58A4` | - | **AMICTRAP** | `[V]` MREG `0xD0` = ATRAP without OMESS |
| `0x1B` | 033B | `65B6` | `CMRUN` | STARTMIC | `[inh]` |
| `0x1C` | 034B | `562E` | `CMSTO` | STOPMIC | `[inh]` |
| `0x1D` | 035B | `568C` | `CMCON` | CONTMIC | `[inh]` |
| `0x1E` | 036B | `6438` | `CMRES` | RESTMIC | `[inh]` |
| `0x1F` | 037B | `56EA` | `CMALI` | **ALIVE** | `[V]` answers nak 7 "not alive" |
| `0x20` | 040B | `5A46` | `CMLMA` | LMAR | `[I]` one word |
| `0x21` | 041B | `5AB0` | `CMLMI` | **LMIR** | `[V]` shares `0x773E` with console `Cmd29_LoadMir` |
| `0x22` | 042B | `5B38` | `CMRMI` | RMIR | `[I]` no params |
| `0x23` | 043B | `5BC8` | `CMBUS` | TBUS | `[I]` one long |
| `0x24` | 044B | `5CC0` | `CMATE` | **RAIB16** | `[V]` calls `AibRead16_AndClearAibf` |
| `0x25` | 045B | `5D56` | - | **RAIB32D** | `[V]` 32-bit pair read then ACON 5 = RAIBF |
| `0x26` | 046B | `5E64` | - | **LAOB16** | `[V]` word param -> `AobSingleWordWrite` |
| `0x27` | 047B | `5ECE` | - | **LAOB32D** | `[V]` long param -> data-pair write |
| `0x28` | 050B | `5FD6` | `CMRAS` | **RASTS** | `[V]` no params, returns one status word |
| `0x29` | 051B | `6016` | `CMLDM` | **LMODE** | `[V]` only non-console caller of `0x77FE` |
| `0x2A` | 052B | `608C` | `CMTMA` | **LCON** | `[V]` one word -> ACON port; inherited "LOCSM" **refuted** |
| `0x2B` | 053B | `60F6` | `CMWMP` | **WMPM** | `[V]` two longs = address + data |
| `0x2C` | 054B | `6178` | `CMRMP` | **RMPM** | `[V]` one long = address |
| `0x2D` | 055B | `6326` | `CMSET` | SETTRAC | `[I]` three words |
| `0x30` | 060B | `6616` | `CMRSE` | **RTEST** | `[V]` reads `1131E2`, clears `1144EC/EA` |
| `0x31` | 061B | `6504` | `CMENK` | **ENKICK** | `[V]` writes the kicks flag |
| `0x32` | 062B | `6534` | `CMDIS` | **DISKICK** | `[V]` clears the kicks flag |
| `0x33` | 063B | `5C44` | `CMBUF` | TBUF | `[I]` one long |
| `0x34` | 064B | `5F38` | - | LAOB32M | `[I]` memory read -> data-pair write |
| `0x35` | 065B | `5DC0` | - | RAIB32M | `[I]` remaining slot in a closed family of six |
| `0x36` | 066B | `558A` | `CMMIC` | - | `[OPEN]` ACON 7 = MASKAIBF then starts |
| `0x37` | 067B | `6390` | `CMLOO` | **LOOP** | `[V]` sets `113138`, the loop flag itself |
| `0x38` | 070B | `63B8` | `CMSPE` | Set Clock Speed | `[I]` one byte -> `0x7B20` |
| `0x39` | 071B | `6408` | `CMCPU` | **CPURES** | `[V]` |
| `0x3A` | 072B | `61F4` | `CMTES` | TESTMPPM | `[I]` **two** longs; completes the multiport trio |
| `0x3B` | 073B | `547E` | `CMCCD` | **DCCD** | `[V]` CS/cache guard + symbol agree |
| `0x3C` | 074B | `52C6` | - | - | `[OPEN]` CS/cache guard + running guard |
| `0x3D` | 075B | `6644` | `CMRPR` | Read PROM Version | `[I]` symbol only |
| `0x3E` | 076B | `66B6` | - | **READ CPU MODEL** | `[V]` reads class byte `1131F6` |

**Count: 19 `[V]`, 11 `[I]`, 10 `[OPEN]`, 6 `[inh]`.** Codes run `0x0D`-`0x3E` with exactly four
holes - `0x19`, `0x1A`, `0x2E`, `0x2F`.

---

## How each name was earned

**Only two kinds of evidence ever produced a name that survived:**

1. **A hardware code.** MREG literal `0xD0` named AMICTRAP. ACON command `5` (RAIBF) named RAIB32D.
   ACON `7` (MASKAIBF) characterised `0x36`.
2. **A worker with one or two callers.** `0x77FE` named LMODE. `0x773E` named LMIR.
   `AibRead16_AndClearAibf` named RAIB16.

**Everything else misled at least once:** manual section order, position in the image, a caller's
name, elimination against the manual list, and a shared worker with many callers.

**And a fifth: a WORKER'S OWN NAME.** `ControlStoreWriteWithVerify` (`0x741E`) says "write", but its
ACON command is `0x18` = AMIRCK, a MIR **reclock**; the write-control-store command is `0x06` = WCS,
which it never issues. A name assigned by an earlier carve is exactly as unreliable as a name taken
from a call site. **Check the hardware code the worker actually issues.**

---

## The machinery every arm is built from

| Address | Name | Role |
|---|---|---|
| `0x6986` | `Reply_EmitByte` | append a byte to the reply (60+ callers - identifies nothing) |
| `0x69D0` | `Reply_EmitWord` | append a word |
| `0x6F9C` | `MsgBody_NextParamByte` | next parameter byte; overrun -> `0x6FE4` |
| `0x6FFA` | `MsgBody_NextParamWord` | next parameter word |
| `0x7036` | `MsgBody_NextParamLong` | four bytes, big-endian |
| `0x6A64` | `StatusHiRead` | emit the two ASTS bytes (the Messnak tail) |
| `0x72EC` | `AibRead16_AndClearAibf` | 16-bit AIB read + ACON 5 |
| `0x7374` | `MfBusCmdDataPairStatus` | 32-bit pair read + ACON 5 |
| `0x72A0` | `AobSingleWordWrite` | write one word into AOB |
| `0x7320` | `MfBusDataPairWithLatchGate_33` | write the 32-bit pair |
| `0x7138` | `MfBusMemoryTransaction_VariantB` | read IN from MFbus memory |
| `0x70AA` | `MfBusMemoryTransaction_VariantA` | write OUT to MFbus memory |

**Globals:** `1143BC` message body, `1144EA` read cursor, `1144EC` body length, `1143AE` parameter
pointer, `1143B2` pointer-given flag, `1143AC` microprogram running, `1143B6` kicks enabled,
`11455C` control-store health (good value `0x7F55`), `11314A` CS initialised, `113138` the loop flag.

**Reply shapes:** ack = `0x00`. nak = `0xFF`, error code, then two ASTS bytes.
**The documented error list 0-9 is incomplete** - arm `0x0D` emits **13**.

---

## The control-store load family, and a flip caught in time

**`0x741E` = `ControlStoreWriteWithVerify`** - it **writes** a control-store word using **ACON command
`0x18` = AMIRCK** ("ACCP reclock MIR without ECMIR"), checks `HW_STATUS_HI` bit 0, and sets the
control-store error latch `0x001131E2` on failure. **Fourth ACON code to explain firmware behaviour.**

**This flipped a reading I was one step from writing down.** Arm `0x15` loops that worker over an
incrementing address, and from the arm alone the natural guess was a *dump* - control store out to
memory. The worker's name and its ACON code say the opposite: it **writes**. Decompiling the worker
before recording the arm is what caught it.

**`0x13` (023B) = LOCSM, Load Control Store Via Memory (5.3.18)** `[I` strong`]`:

- memory-parameter guard, so it needs `LPARP` first;
- pulls a `{address, count}` descriptor out of the parameter block and then reads successive words
  **in from MFbus memory** with `MfBusMemoryTransaction_VariantB`;
- builds the same `0x001144F0` word array as `0x14`, accumulating the same **checksum**;
- and calls `CmdPortWrite_A` - **the worker the console `Cmd21_LoadControlStore` also calls.**

So `0x13` is the memory-sourced sibling of `0x14` = LOCSD. **That gives LOCSM a home at last**, and
it is not `0x2A` - see below.

**`0x15` (025B)** drives `0x741E` with an incrementing address counter, memory-parameter guarded.
**`0x16` (026B)** drives the same worker with a single 14-bit address and answers Messnak **5** when
it returns non-zero. Both `[OPEN]`.

> ### CAUTION: `ControlStoreWriteWithVerify` is a NAME, not evidence - and the ACON code disagrees
>
> That function name came from an earlier carve. Its actual ACON command is **`0x18` = AMIRCK**,
> *"ACCP reclock MIR without ECMIR"* - a **clock** operation on the microinstruction register.
> **The command that writes the control store is `0x06` = WCS**, and `0x741E` does not issue it.
>
> Reclocking MIR happens on the way **in or out** of a control-store word, so this worker does not
> settle direction at all. **I was one step from naming `0x15` and `0x16` on the strength of that
> function name** - the same caller-name trap that produced `TRAP_OCBAK` and the `0x795A` "re-init"
> reading, arriving for a fourth time in a different costume.
>
> **Consequence:** four arms (`0x13`, `0x14`, `0x15`, `0x16`) touch the control-store path but the
> manual has only two LOAD commands (LOCSD, LOCSM). So at least two of them are **dumps** -
> DCSD (5.3.19) and DUCS (5.3.20) - and the direction cannot be read off `0x741E`. Whoever picks
> this up should find who issues **ACON `0x06` (WCS)** and work outward from there; the census
> counted `0x0006` **20,964** times per boot, so the real control-store load path is heavily used
> and easy to spot.
>
> `0x13` = LOCSM is **unaffected** - that inference rests on the shared `CmdPortWrite_A` with the
> console `Cmd21_LoadControlStore` and on the checksum, not on `0x741E`.

## `0x2A` = LCON - and why the inherited "LOCSM" was wrong

**`0x2A` (052B) = LCON, Load CON (5.3.40)** `[V]`. The arm reads **one 16-bit word** and loops it
through `CmdPortWriteTiny`, which writes straight to the **ACON decoder** at `0x220000`. The manual:
*"The ACON decoder is loaded (16 bits). This is used to generate strobe pulses according to the bit
pattern loaded. **Nothing is stored.**"* One word in, written to ACON, nothing retained.

Corroborated by the console twin sharing that worker - `Cmd32_LoadControlDecoder`.

**The earlier carve recorded `0x2A` as LOCSM (Load Control Store Via Memory), and that is wrong on
three counts:** SINTRAN's symbol is `CMTMA`, not a load-control-store name; the body loads the
control **decoder**, not the control **store**; and LOCSM takes memory parameters while this arm
takes one direct word and carries no parameter-pointer guard. **LOCSM has no confirmed arm.**

> **A near-miss worth keeping.** Two rounds ago `CmdPortWriteTiny` briefly looked like proof that
> `0x3A` was LCON, because the console `Cmd32_LoadControlDecoder` calls it. I rejected that on the
> grounds that a worker with three callers proves nothing. **The right answer was a different arm
> sharing the same worker** - `0x2A`, which matches on parameter shape as well. The rule held: the
> worker narrowed the field, the 16-bit-word shape picked the winner.

**`0x3C` (074B) has THREE guards** - control-store health, running, **and** the parameter pointer -
so it is a memory-parameter control-store/cache command: **DUCS** (5.3.20) or **DUCC** (5.3.22),
`[OPEN]` between them. With `0x3B` = DCCD (cache, directly), the family's fourth member DCSD is still
unplaced.

## ECHO - the cleanest identification in the set

**`0x0F` (017B) = ECHO, Echo Test (5.3.12)** `[V]`, confirmed three independent ways:

1. **Behaviour.** Reads a byte count, collects that many bytes, acks, then emits **the same bytes
   back**. The manual's wording is "returns the test pattern".
2. **The absence of guards.** 5.3.11 says *"Some commands never return Messnak, like ECHO TEST"* -
   and this is the **only arm in all 46 with no guard at all**. An absence that the manual predicts
   is real evidence, unlike an absence a tool failed to show.
3. **The symbol.** `CMTEC` reads as "test communication", matching 5.3.12's stated purpose, *"to
   assure that the basic communication between the ND-120 and the ACCP works"*.

**Detail the manual does not state: the count is masked to `0x1F`, so at most 31 test bytes.**

## The multiport trio

`WMPM` and `RMPM` were named from parameter **count** - two longs (address + data) versus one long
(address). `0x3A` takes **two longs** and its symbol is `CMTES`, "test". The manual's third multiport
command is **TESTMPPM, Test Multiport (5.3.43)**, and the other two test commands are already spoken
for by `CMBUS` = TBUS (`0x23`) and `CMBUF` = TBUF (`0x33`). So `0x3A` = TESTMPPM, `[I]`.

**Why only `[I]`:** the argument is elimination plus shape, and elimination against the manual's list
has misfired twice in this effort already. It needs a worker or a hardware code like the others.

## What is still open, and the honest reason

- **Ten arms have behaviour recorded but no name.** In every case the distinctive behaviour does not
  match an unclaimed manual command, and the plausible names are already taken. Forcing them would
  repeat the mistakes listed above.
- **`0x2A` = `CMTMA`** contradicts the inherited `LOCSM`. Until that is resolved, `LOCSM` has no home
  and arm `0x13` cannot be named.
- **The Ghidra database still has undefined pockets inside the arms**, which makes every xref in
  `0x4D50`-`0x66B6` an undercount. This produced two wrong conclusions in one session. **Run a range
  disassembly over that span, then re-run the guard sweeps** - they are the cheapest classifier
  available and only as good as the disassembly beneath them.
