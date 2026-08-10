# Handoff: finish the ACCP octobus command table (12 rows)

**Date:** 2026-08-03
**To:** the central octobus controller agent
**From:** the ACCP firmware carve
**Subject:** 34 of 46 dispatcher arms are proved. 12 are not. Here is exactly what is left.

---

## Start here

**The table:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-OCTOBUS-COMMAND-TABLE-2026-08-02.md`
Every one of the 46 arms with its code, octal, arm address, SINTRAN `CM*` symbol, command name and
**the evidence for that name, per row**. Read the evidence column before trusting any row.

**The image:** `Installation\Communication\OctobusAccp\eprom\octo.bin`, SHA256 `0EA81716AD81984B...`,
131072 bytes. Ghidra has it as Raw Binary, `68000:BE:32`, base 0.

**The tests that hold it:**
`RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpOctobusCommandTableTests.cs`
re-extracts the dispatcher from the shipped ROM and compares it to the table. If you change the
table, these must still pass. That whole ACCP suite is 106/106 green, so a failure is real signal.

---

## The 12 rows to close

### Group A - two arms, strong argument, never read end to end

| Arm | Code | Claim | Why it is only `[I]` |
|---|---|---|---|
| `0x5F38` | `0x34` | LAOB32M (5.3.37) | Reads MFbus memory then writes the data pair |
| `0x5DC0` | `0x35` | RAIB32M (5.3.34) | The remaining slot in a closed six-arm family |

**The family argument, so you can judge it yourself:** exactly six arms carry the kicks guard
(`0x001143B6` -> Messnak -2), and the manual has exactly six AIB/AOB commands. Four are proved from
hardware - `0x24` RAIB16, `0x25` RAIB32D, `0x26` LAOB16, `0x27` LAOB32D. These two are what remains.

**What would finish it:** read either arm from its guard to its reply and confirm the data direction.
`0x34` should end up writing AOB; `0x35` should end up writing MFbus memory. **Neither has a `CM*`
symbol** - consistent with SINTRAN never sending the via-memory pair, so the symbol table cannot help
here.

### Group B - eight arms with a `CM*` symbol and a matching shape, but no hardware proof

| Arm | Code | `CM*` | Claim | Measured |
|---|---|---|---|---|
| `0x5A46` | `0x20` | `CMLMA` | LMAR (5.3.27) | one word |
| `0x5B38` | `0x22` | `CMRMI` | RMIR (5.3.29) | no parameters |
| `0x5BC8` | `0x23` | `CMBUS` | TBUS (5.3.31) | one long |
| `0x6326` | `0x2D` | `CMSET` | SETTRAC (5.3.44) | three words |
| `0x5C44` | `0x33` | `CMBUF` | TBUF (5.3.30) | one long |
| `0x63B8` | `0x38` | `CMSPE` | Set Clock Speed (5.3.55) | one byte -> `0x7B20` |
| `0x61F4` | `0x3A` | `CMTES` | TESTMPPM (5.3.43) | two longs |
| `0x6644` | `0x3D` | `CMRPR` | Read PROM Version (5.3.56) | no parameters |

**Each needs one thing: a hardware code or a one-or-two-caller worker.** Symbol plus shape is not
proof - see the traps section.

### Group C - two arms with no manual command at all

| Arm | Code | Behaviour |
|---|---|---|
| `0x6562` | `0x10` | Returns **sixteen words** from `0x00114550` - the CPU-class probe address, i.e. the signature block |
| `0x56BC` | `0x17` | Calls `0x79E4` (latch **enable**) then writes `0x001143AC := 1` (MRUN). A fourth start-the-microprogram variant |

**These are believed undocumented, and that is established, not assumed:** every manual command is
accounted for elsewhere in the table, and **TERM (5.3.50) and ARES (5.3.51) have no arm at all** -
both carry the emergency bit, are decoded by hardware, and the manual says outright *"the octobus
driver is not activated"*. So the 46 arms and the 46 manual sections are different sets.

**What would close them:** a reference outside ND-05.020.01, or accepting them as undocumented and
recording the behaviour. Do not force a manual name onto them.

---

## What actually works, and what does not

**Only two kinds of evidence produced a name that survived a full session of testing:**

1. **A hardware code.** MREG literal `0xD0` (ATRAP without OMESS) named AMICTRAP. ACON `5` = RAIBF
   named RAIB32D. ACON `0x06` = WCS named LOCSD and LOCSM. A latch bit told store from cache.
2. **A worker with ONE OR TWO callers.** `0x77FE` -> LMODE. `0x773E` -> LMIR.
   `AibRead16_AndClearAibf` -> RAIB16. **Count the callers first**: `Reply_EmitByte` has 60+ and
   `CmdPortWriteTiny` has three unrelated ones - neither identifies anything.

**Six things misled this carve at least once each. All six are recorded because all six looked
convincing at the time:**

- manual **section order** - 5.3.12-5.3.57 is 46 sections and there are 46 arms, but the mapping
  fails a spot check and the sets are not even the same;
- **position** in the image - arms are not in code order;
- a **caller's** name - `0x795A` is the latch-disable; "STOPMIC" came from its call site;
- a **worker's own** name - `ControlStoreWriteWithVerify` issues AMIRCK, a MIR reclock, and never
  issues WCS at all;
- **elimination** against the manual's list - it produced a tidy three-for-three that was entirely
  wrong, because TERM and ARES were never arms;
- a **shared worker with many callers**.

---

## The trap that will cost you a day if nobody tells you

**Cross-references in `0x4D50`-`0x66B6` silently UNDERCOUNT when the region has undefined bytes.**

This produced a published-and-retracted claim ("RAIB16 has no octobus arm" - it is `0x24`) and four
other false leads. **Five times in total**, including twice after the warning had been written down
by the person who then ignored it.

Ronny has since range-disassembled `0x4D50`-`0x66B6` and every conclusion was re-validated against
the clean database: arm count still 46, kicks guard still exactly 6, nothing overturned. **So the
database you are picking up is clean** - but if you extend into a region that is not, an empty xref
result is not a result.

---

## Two things that will bite an emulator, from the same carve

- **ACON command `0x08` is not in ND-05.020.01 table 9**, and the card issues it on **every ENABLE
  KICK** (`0x6512`, inside the `0x31` arm). Its counterpart `0x32` DISKICK issues `0x07` = MASKAIBF.
  A kick arrives as an AIB-flag interrupt, so `0x08` is the unmask. **A decoder built from table 9
  alone rejects a command the real card uses.** There is a test for this.
- **The Messnak error list 0-9 is incomplete.** Arm `0x0D` emits **13**.

---

## Reference data you will want

**SINTRAN's command codes are `CM*` constants in octal** in
`SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB`. **ND-05.020.01 prints no numeric codes anywhere**, so any
document citing it as the source of a code-to-name mapping got the numbers elsewhere.

**Dump that table KEEPING ALIASES.** Several codes carry two or three names and de-duplicating by
value throws away the informative one. `CMR16`/`CMR32`/`CML16`/`CML32` at `0x24`-`0x27` confirm the
AIB/AOB four; **`CMLDC` at `0x2A` ("load decoder") confirms LCON** - and `CMTMA`, the other alias on
that same code, is what produced an earlier carve's wrong `LOCSM`.

**Guard globals, which classify an arm before its body is read:**

| Global | Meaning |
|---|---|
| `0x001143AC` | microprogram running -> Messnak -1 (inverse in STOPMIC -> Messnak 0) |
| `0x001143B2` | parameter pointer given -> Messnak 1 when absent = memory-parameter command |
| `0x001143AE` | the parameter pointer itself, a longword MFbus address |
| `0x001143B6` | kicks enabled -> Messnak -2 = AIB/AOB access |
| `0x0011455C` | control store/cache health, good value `0x7F55` -> Messnak 5 |
| `0x0011314A` | CS initialised -> Messnak 9. **Only STARTMIC (`0x36`) uses it** |
| `0x00113138` | the LOOP flag - `0x37` sets it, every other arm tests it at the bottom of its loop |

**Message plumbing:** body buffer `0x001143BC`, read cursor `0x001144EA`, length `0x001144EC`.
Overrun lands at `0x6FE4`, which is the "Communication error at address 6FE4H" the card prints for an
empty OBCON body.

**Reply shapes:** ack = `0x00`. nak = `0xFF`, error code, then the two ASTS bytes from `StatusHiRead`.
