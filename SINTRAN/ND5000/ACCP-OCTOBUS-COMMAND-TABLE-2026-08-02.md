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
| `0x0D` | 015B | `583A` | - | read back system parameters | `[V]` behaviour; **not RECO** - returns the cells `0x0E` writes |
| `0x0E` | 016B | `57E8` | `CMSYS` | **LSYSPAR** (CMSYSPAR) | `[V]` 3 words = 6 bytes -> `1143A0/A2/A4`, sets `1143A6` |
| `0x0F` | 017B | `5736` | `CMTEC` | **ECHO** | `[V]` collects n bytes, sends the same n back; **no guards** |
| `0x10` | 020B | `6562` | `CMREA` | - | `[OPEN]` returns 16 words from `114550` |
| `0x11` | 021B | `5980` | `CMLPA` | **LPARP** | `[V]` writes the parameter pointer + flag |
| `0x12` | 022B | `59B6` | `CMVER` | **VPARP** | `[V]` no params; running guard + param-pointer guard -> Messnak 1 |
| `0x13` | 023B | `4D50` | `CMWWC` | **LOCSM** | `[V]` issues **WCS**; memory params + checksum |
| `0x14` | 024B | `4EDC` | `CMDWW` | **LOCSD** | `[V]` issues **WCS**; 8 words = 128 bits + checksum |
| `0x15` | 025B | `4FC0` | `CMADR` | **DUCS** | `[V]` store latch bit 2 + memory param |
| `0x16` | 026B | `519C` | `CMDRW` | **DCSD** | `[V]` store latch bit 2 + direct 14-bit address |
| `0x17` | 027B | `56BC` | - | - | `[OPEN]` latch enable + sets running |
| `0x18` | 030B | `58A4` | - | **AMICTRAP** | `[V]` MREG `0xD0` = ATRAP without OMESS |
| `0x1B` | 033B | `65B6` | `CMRUN` | **RUNTST** | `[V]` no params; runs `0xF22C`; returns `1131E2`. **NOT StartMic** |
| `0x1C` | 034B | `562E` | `CMSTO` | **STOPMIC** | `[V]` no params; **inverse** guard -> Messnak 0 |
| `0x1D` | 035B | `568C` | `CMCON` | **CONTMIC** | `[V]` no params; calls the `0x79BC` enable wrapper |
| `0x1E` | 036B | `6438` | `CMRES` | **RESTMIC** | `[V]` **two** words = CS address + interval, exactly 5.3.46 |
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
| `0x36` | 066B | `558A` | `CMMIC` | **STARTMIC** | `[V]` CS address word, ARMA reclocks MAR, sets MRUN, Messnak 9 |
| `0x37` | 067B | `6390` | `CMLOO` | **LOOP** | `[V]` sets `113138`, the loop flag itself |
| `0x38` | 070B | `63B8` | `CMSPE` | Set Clock Speed | `[I]` one byte -> `0x7B20` |
| `0x39` | 071B | `6408` | `CMCPU` | **CPURES** | `[V]` |
| `0x3A` | 072B | `61F4` | `CMTES` | TESTMPPM | `[I]` **two** longs; completes the multiport trio |
| `0x3B` | 073B | `547E` | `CMCCD` | **DCCD** | `[V]` CS/cache guard + symbol agree |
| `0x3C` | 074B | `52C6` | - | **DUCC** | `[V]` cache latch bit 1 + memory param |
| `0x3D` | 075B | `6644` | `CMRPR` | Read PROM Version | `[I]` symbol only |
| `0x3E` | 076B | `66B6` | - | **READ CPU MODEL** | `[V]` reads class byte `1131F6` |

**Final count: 34 `[V]`, 10 `[I]`, 2 `[OPEN]`.** Codes run `0x0D`-`0x3E` with exactly four holes -
`0x19`, `0x1A`, `0x2E`, `0x2F`. No name is left tagged `[inh]`: all six inherited names were audited
against the manual's parameter lists, **five confirmed and two disproved** (`STARTMIC` at `0x1B` was
really RUNTST; `LOCSM` at `0x2A` was really LCON).

The two `[OPEN]` arms are not unfinished work - **`0x10` and `0x17` implement commands ND-05.020.01
does not document**, which is established rather than assumed: every manual command is now accounted
for, and TERM and ARES are hardware-decoded with no arm at all.

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

## Auditing the inherited names - two confirmed, and one of MY OWN readings overturned [2026-08-03]

**`0x0E` = LSYSPAR / CMSYSPAR - CONFIRMED `[V]`.** 5.3.13 says *"Direct parameters: System parameters
(6 bytes)"* and shows them as three 16-bit words. The arm reads **exactly three words** and stores
them to `0x001143A0`, `0x001143A2`, `0x001143A4`, then sets `0x001143A6 := 1` as a
"parameters given" flag.

**`0x1C` = STOPMIC - CONFIRMED `[V]`, and its guard runs BACKWARDS from every other arm.** 5.3.24
gives Messnak **0** = *"Microprogram is not started"*. The arm tests `0x001143AC` and naks when it is
**clear** - the inverse of the usual "illegal while running" test. That is correct for a stop
command, and it is a nice independent check: a mis-assigned name would almost certainly have the
guard the normal way round.

### RETRACTED: `0x0D` is NOT RECO

Two rounds ago `0x0D` was recorded `[I]` as RECO, Read ECO Levels, because it returns three words and
5.3.52 describes two bytes per module.

**It returns the three words `0x001143A0/A2/A4` - the exact cells `0x0E` = CMSYSPAR writes** - and it
gates on `0x001143A6`, the flag CMSYSPAR sets. So it is the **read-back of the system parameters**,
not a report of board revisions. The writer and the reader name each other; the module-count
coincidence was just a coincidence.

**Method note:** this was caught by reading the *writer* of a memory cell after reading its reader.
Neither arm alone said what the cells were. **When an arm returns some globals, find who writes
them.**

`0x0D` therefore joins `0x10` and `0x17` as a command with no manual section, and **RECO has no arm
identified.**

### `0x12` = VPARP - CONFIRMED `[V]`, and the doubt was the xref undercount AGAIN

5.3.16 gives VPARP no parameters and Messnak **-1** and **1**. The arm has exactly that: no parameter
read, the running guard, then `tst.w 0x001143B2` answering Messnak **1**, then it reads the pointer
at `0x001143AE`.

**The reason it looked wrong was that `0x59F4` was still undefined bytes**, so its test of
`0x001143B2` never reached the xref list - the same undercount that produced the retracted RAIB16
claim and that this file has now warned about three times.

**That is the FIFTH time this single database defect has misled this work.** The one thing that went
right here is that it was written up as *"either the arm reaches the flag another way, or the
inherited name is wrong"* rather than as a finding. **When a sweep contradicts a documented
behaviour in this range, suspect the disassembly before suspecting the claim.**

## TERM and ARES have NO arm - and that settles the sets question properly [V 2026-08-03]

5.3.50 TERM and 5.3.51 ARES both say the same three things:

- the command *"has a special code with the **emergency bit set (bit 7)** and is **detected by the
  hardware**"*;
- *"(The octobus driver is not activated.)"*;
- *"**There is no response to this command.**"*

**They bypass the dispatcher entirely.** A command the hardware decodes and the driver never sees
cannot have a `cmpi.b` arm, and does not need one. So the last two open arms were never going to be
TERM and ARES, and the tidy "three arms, three commands" symmetry from two rounds ago was **false**.

**This also repairs, on correct grounds, a claim withdrawn earlier.** The sets genuinely are not the
same: **TERM and ARES have manual sections but no arms.** The earlier version of this claim rested on
RAIB16 supposedly having no arm, which was an xref undercount and was retracted. This one rests on
the manual saying outright that the driver is not involved.

**Consequence for `0x10` and `0x17`:** with TERM and ARES excluded, there is no unclaimed manual
command left for them. They are commands the firmware implements that **ND-05.020.01 does not
document**, alongside ACON `0x08` and Messnak 13. Their behaviour is recorded; there is no name to
find.

**RECO strengthened by the same page.** 5.3.52: *"the print status of each module consists of two
bytes"* - and arm `0x0D` returns **three words = three modules x two bytes**. That upgrades the `0x0D`
reading from a guess about shape to a match on structure.

## The `CM*` ALIASES - and a mistake in how this file first dumped them [2026-08-03]

**The arm code and the `CM*` code are the same number space.** Evidence, from the ACCP-init agent:
**39 codes carry a `CM*` symbol, 7 do not, 39 + 7 = 46**, and **all four arm holes (`0x19`, `0x1A`,
`0x2E`, `0x2F`) have no `CM*` symbol either - four for four.** So any inferred row can be checked by
looking up `0o<code>` in `N500-SYMBOLS.SYMB` without touching Ghidra.

**Correction to this file's own method.** The first dump here de-duplicated **by value**, which
silently discarded every alias. **Several codes carry two or three names**, and the discarded ones
were the informative ones:

| Code | Aliases | What the alias settles |
|---|---|---|
| `0x24` | `CMATE`, **`CMR16`** | read 16 -> **RAIB16** |
| `0x25` | **`CMR32`** | read 32 -> **RAIB32D** |
| `0x26` | **`CML16`** | load 16 -> **LAOB16** |
| `0x27` | **`CML32`** | load 32 -> **LAOB32D** |
| `0x2A` | `CMTMA`, **`CMLDC`** | **load decoder** -> **LCON** |
| `0x21` | `CMLMI`, `CMMAC`, `CMTMO` | three names on one code |
| `0x15` | `CMADR`, `CMAD1`, `CMRWC` | |
| `0x16` | `CMDRW`, `CMAD2` | |
| `0x22` | `CMRMI`, `CMACO` | |
| `0x23` | `CMBUS`, `CMAST` | |

**Four of the AIB/AOB names were proved from hardware before these aliases were seen** - `CMR16` from
`AibRead16_AndClearAibf`, `CMR32` from the pair-read plus ACON 5, `CML16`/`CML32` from the write
workers. **The symbol table and the firmware agree on all four independently.** `CMLDC` does the same
for the LCON correction, which is the more useful one since it also refutes the inherited `LOCSM`.

**A caution the agent added and this file endorses:** a `CM*` name is still a **name**. Better than
section order or position, weaker than a hardware code or a one-or-two-caller worker. `0x2A` carrying
both `CMTMA` and `CMLDC` shows why - one of those two would have sent a reader nowhere.

**Where it leaves the ten inferred rows:** eight now have symbol support (`CMLMA` LMAR, `CMRMI` RMIR,
`CMBUS` TBUS, `CMSET` SETTRAC, `CMBUF` TBUF, `CMSPE` Set Clock Speed, `CMTES` TESTMPPM, `CMRPR` Read
PROM Version). **`0x34` and `0x35` have no `CM*` symbol at all** - consistent with LAOB32M/RAIB32M
being commands SINTRAN never sends, which is exactly why they had to be argued from the closed
six-arm family instead.

## An INHERITED name was wrong: `0x1B` is RUNTST, not STARTMIC [V 2026-08-03]

**Reading the manual's parameter lists settled two arms at once.** 5.3.23 STARTMIC takes *"Control
store address (2 bytes)"*, loads MAR with it, sets MRUN, and can answer Messnak **9**. 5.3.53 RUNTST
takes *"None"*, returns *"Self-test status (2 bytes)"*, and answers only **-1**.

| Arm | What it actually does | Command |
|---|---|---|
| `0x1B` (033B, `CMRUN`) | **no parameters**; only the running guard; calls `0xF22C`; replies with **`0x001131E2`, the self-test status word** | **RUNTST** (5.3.53) |
| `0x36` (066B, `CMMIC`) | reads **one word**; worker issues **ARMA** = "reclock MAR"; sets `1143AC` (MRUN); answers Messnak **9** - the only arm that does | **STARTMIC** (5.3.23) |

**`0x1B` had been carried as STARTMIC since the earlier carve.** It is not: STARTMIC needs an address
and this arm reads nothing. `CMRUN` reads as "run test", which fits RUNTST.

It also pairs cleanly with `0x30` = RTEST, which reads the **same** `0x001131E2` **without** running
the test - exactly the 5.3.53 / 5.3.54 relationship.

**This is what the provenance question was about.** Six names arrived from an earlier carve tagged
`[inh]`; one is now proved wrong and one (`LOCSM` at `0x2A`) was refuted earlier. **The manual's
parameter lists turned out to be the cheapest test available** - "does this arm read a parameter at
all?" is one glance and it falsifies a name outright.

## ACON command `0x08` is UNDOCUMENTED, and it is what enables kicks [V 2026-08-03]

Scanning the whole image for `move.w #imm,(0x00220000)` gives every static ACON write - 52 sites.
All decode against ND-05.020.01 table 9 **except one**:

| Site | Command | In table 9? |
|---|---|---|
| `0x6512` - inside arm `0x31` = **ENKICK** | **`0x08`** | **NO** |
| `0x6540` - inside arm `0x32` = **DISKICK** | `0x07` = MASKAIBF | yes |
| `0x6888`, `0x100EC` | `0x08` | **NO** |

**Table 9 lists 0,1,2,5,6,7,9,A,C,D,F,10,11,13,14,15,16,17,18,1A - there is no `0x08`.**

**What it must be.** DISKICK issues **MASKAIBF** ("mask AIB-flag interrupt"). ENKICK issues `0x08`.
A kick arrives as an AIB-flag interrupt, so enabling kicks means **unmasking** it. `0x08` is
therefore the **unmask counterpart of MASKAIBF**, missing from the published table.

This also **confirms ENKICK and DISKICK from hardware**, not just from their SINTRAN symbols - they
are a matched mask/unmask pair on the same interrupt.

**Second correction to ND-05.020.01 from this work**, alongside the incomplete Messnak error list
(arm `0x0D` emits 13, the manual documents 0-9).

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
> `0x13` = LOCSM is **unaffected** - that inference rests on the shared worker with the console
> `Cmd21_LoadControlStore` and on the checksum, not on `0x741E`.

### RESOLVED the same round - the WCS lead worked

Searching the whole image for a write of ACON `0x06` to `0x220000` finds **exactly two sites**:
`0x73D2` and `0x7408`. They sit inside the two functions formerly called `CmdPortWrite_A` and
`CmdPortWrite_B`, **renamed `ControlStoreWrite_WCS_A` / `_B`**. Those are the control-store write
path, and nothing else in the image issues WCS.

**That promotes both loads to `[V]`:**

| Arm | Command | Now proved by |
|---|---|---|
| `0x14` (024B) | **LOCSD** (5.3.17) | issues **WCS**; 8 words = 128 bits + checksum; direct parameters |
| `0x13` (023B) | **LOCSM** (5.3.18) | issues **WCS**; same array + checksum; **memory** parameters |

**And it settles what `0x15` and `0x16` are not.** Neither issues WCS - they go through `0x741E`,
whose command is AMIRCK, a MIR reclock. **So they are not control-store loads**, which leaves the
dump commands DCSD (5.3.19) and DUCS (5.3.20) as the live candidates, `[OPEN]` between them.

**The technique, worth reusing:** when a worker's name is untrustworthy, search the image for the
**hardware command byte** the real operation must issue and see who issues it. Two hits out of a
131072-byte image settled a question four rounds of reading arms had not.

### And the same map settles `0x15` / `0x16` as DUMPS

`0x741E` issues, in order: **AMIRCK** (`0x18`, reclock MIR **from** the control store) and then, via
`CmdPortWriteShort_B`, **MDCLK** (`0x10`, clock the MISR serial chain). It checks `HW_STATUS_HI`
bit 0 and returns a status flag, and it **never issues WCS**.

Reclock-then-shift is the **read** direction: pull a control-store word into MIR, then shift the
shadow chain out. A load runs the opposite way - shift in, then WCS - which is exactly what
`ControlStoreWrite_WCS_A`/`_B` do for `0x13` and `0x14`.

| Arm | Parameter | Command | Basis |
|---|---|---|---|
| `0x16` (026B) | one **14-bit CS address** (Messnak 3 if > `0x3FFF`) | **DCSD**, Dump Control Store Directly (5.3.19) | `[I]` |
| `0x15` (025B) | **memory** parameter, address counter | **DUCS**, Dump Control Store Via Memory (5.3.20) | `[I]` |

### The four dump commands form a 2x2, and a LATCH BIT is the discriminator [V 2026-08-03]

There are **two** copies of the reclock-and-shift read primitive, identical except for which latch
bit they gate:

| Worker | Gates latch bit | Array | Arms that call it |
|---|---|---|---|
| `0x741E` | **2** (`0x04`) | control **STORE** | `0x15`, `0x16` |
| `0x764E` | **1** (`0x02`) | control **CACHE** | `0x3B`, `0x3C` |

**`0x3B` is `CMCCD` = DCCD, Dump Control *Cache* Directly - and it calls `0x764E`.** That pins latch
bit 1 to the cache, and therefore bit 2 to the store. The second axis is already carved: the
**parameter-pointer guard** marks the memory-parameter variants.

Two hardware facts, two axes, four commands - **no elimination required**:

| Arm | Array (latch bit) | Parameters | Command |
|---|---|---|---|
| `0x16` | store (bit 2) | direct, 14-bit address | **DCSD** (5.3.19) |
| `0x15` | store (bit 2) | **memory** | **DUCS** (5.3.20) |
| `0x3B` | cache (bit 1) | direct | **DCCD** (5.3.21) |
| `0x3C` | cache (bit 1) | **memory** | **DUCC** (5.3.22) |

All four promoted to `[V]`. This is the cleanest structural result in the phase: every cell is fixed
by a hardware bit or a guard, and the manual's four dump commands land in the four cells exactly.

**Worth contrasting with how this looked two rounds ago**, when the same four arms were going to be
assigned by counting what was left over. The 2x2 says the same thing about `0x15`/`0x16`, but for a
reason that would survive one of them turning out to be something else entirely.

**And the worker's name is now doubly wrong.** `ControlStoreWriteWithVerify` neither writes (no WCS)
nor is limited to verifying. Left renamed in place rather than re-guessed, with this note as the
correction.

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

- **Two arms have behaviour recorded but no name: `0x10` and `0x17`.** With TERM and ARES shown to
  bypass the dispatcher, **no unclaimed manual command remains**, so these two are almost certainly
  undocumented - like ACON `0x08` and Messnak 13. `0x10` returns the sixteen-word signature block
  from `0x00114550`; `0x17` enables the latch and sets the microprogram-running flag. Behaviour
  recorded, no name to find.
- **Four names still carry `[inh]`** - `CMSYSPAR` (`0x0E`), `VPARP` (`0x12`), `STOPMIC` (`0x1C`),
  `CONTMIC` (`0x1D`), `RESTMIC` (`0x1E`). Two of the original six inherited names have already been
  disproved (`LOCSM` at `0x2A`, `STARTMIC` at `0x1B`), so **these deserve the same parameter-list
  check** - it is one glance per arm and it falsified both of the others.
- **The Ghidra database still has undefined pockets inside the arms**, which makes every xref in
  `0x4D50`-`0x66B6` an undercount. This produced two wrong conclusions in one session. **Run a range
  disassembly over that span, then re-run the guard sweeps** - they are the cheapest classifier
  available and only as good as the disassembly beneath them.
