# The ACCP <-> ND-5000 CPU interface - implementation spec

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

## 1. Orientation - the names are from the CPU's point of view

This trips people up, so it comes first.

- **AOB = "ACCP Output Buffer".** The **ACCP writes** it; the **CPU reads** it.
- **AIB = "ACCP Input Buffer".** The **CPU writes** it; the **ACCP reads** it.

So on the ACCP side "write AOB" is an outbound data path toward the CPU, and "read AIB" is
inbound from the CPU. The ACCP's own console commands are named the same way and confirm it
**[X]**: `LOAD-AOB16` / `LOAD-AOB32` write toward the CPU, `READ-AIB16` / `READ-AIB32` read
what the CPU left.

---

## 2. The CPU side - four special operands, 23 microwords total [V]

| Operand | Encoding | Dir | Meaning |
|---|---|---|---|
| `A,SPEC,AOB` | A-source `0141` | CPU reads | data from the ACCP |
| `D,SPEC,AIB` | dest `041` | CPU writes | data to the ACCP |
| `A,SPEC,AFLAG` | A-source `0151` | CPU reads | status / flags. **Always SLOW2 (160 ns) cycles** |
| `A,SPEC,AOBASR` | A-source `0152` | CPU reads | AOB-side / ASR register. **Read only at boot, immediately before AOB** |

### AFLAG bit map [V, but see the warning]

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

### The four primitives [V]

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

## 3. The ACCP side - the same interface, in 68000 address space [V]

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

### `0x001131 38` - a bypass flag that will confuse you

Both handshakes begin with `tst.w (0x00113138)`, and a **non-zero value skips the wait
entirely**. It exists in the firmware, not just in an emulator. If a guest sets it, neither
gate is honoured.

---

## 4. The two handshakes, exactly [V]

### ACCP writes AOB, 16-bit - routine `0x72A0`

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

### ACCP writes AOB, 32-bit - routine `0x7320`

Identical, except both halves go out before the strobe:

```
    ... same busy wait and SR save ...
write_word(0x00440000, value & 0xFFFF)       /* low */
write_word(0x00550000, value >> 16)          /* high */
    ... same single strobe through 0x330000 ...
```

**Order matters: low first, then high, then one strobe.**

### ACCP reads AIB, 16-bit - routine `0x72EC` (`MfBusCmdAndWaitStatus_22_44_66`)

```
if (word16[0x00113138] == 0)
    while ((byte[0x00660001] & 0x01) == 0)   /* wait for data available */
        ;

value = read_word(0x00440000)
write_word(0x00220000, 0x0005)               /* acknowledge / advance */
return value
```

### ACCP reads AIB, 32-bit - routine `0x7374` (`MfBusCmdDataPairStatus`)

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

### Correspondence between the two sides

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

## 5. The command channel - CPU asks, ACCP answers [V]

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

## 6. Message classes on this interface [V]

### ACCP -> CPU, via AOB

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

### CPU -> ACCP, via AIB

- Command numbers **1, 2, 3** (answers arrive on AOB, no async trap).
- Kick words: `0100001 | level` ("give interrupt"), `0100101 | dest`, `0100102 | cpu`.
  Bit 15 set marks a single-word message.
- **Boot acknowledge: `AIB := 0`** - written by `LOOK_SRF_1` when the SRF load is complete.

---

## 7. Big messages do NOT use this interface [V]

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

## 8. Where the CPU model actually comes from - the full chain [X]

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

### THE MODEL IS ENCODED TWO DIFFERENT WAYS - do not plumb one into the other [X, 2026-07-30]

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

### Two divergences from WRSYSINFO, both now settled by the ROM [V]

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

### A worse naming trap in the same manual page [V]

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

### The ACCP's cross-check - why a wrong digit is rejected [V]

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

### The signature matrix at `0x00114550` [V]

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
```

Matrix word index `s` is byte offset `s*2`. Because `matrix[s] bit w = read[w] bit s`,
requiring `matrix[s] == 0x7F55` means:

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

**[OPEN]** what the hardware actually presents on those sixteen reads. `LOOK_HARD_1` (017472)
shows the ACCP is the *source* of a hardware-configuration word toward the CPU, so the matrix
is read from the datapath or backplane rather than from the CPU - direction clear, source not
proven.

---

## 9. Open items - do not paper over these

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

## 10. Minimum viable implementation order

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

## 11. Related documents

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` - the CPU-side catalog, with the re-verification sweep and its corrections
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md` - the octobus protocol and the ACCP's octobus driver
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-324716-FIRMWARE-RE-2026-07-27.md` - the ACCP firmware write-up of record
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-HARDWARE-ADDRESS-MAP-2026-07-27.md` - the full ACCP peripheral address sweep
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-RETROCORE-MACHINE-IMPLEMENTATION-HANDOFF-2026-07-27.md` - section 4z, the MFbus-controller peer requirements
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-CONSOLE-COMMAND-SET-AND-DISPATCH-2026-07-27.md` - all 43 console commands, including the AOB/AIB/ASR ones used as evidence here

## Provenance

Sections 2, 5, 6 and 7 are from the microcode listing. Sections 3, 4 and 8 are from the ACCP
EPROM. The correspondence table in section 4 and the chain in section 8 are where the two meet;
each row there carries its own confidence mark. The register-direction convention in section 1
is confirmed from both sides independently - the microcode's operand directions and the ACCP's
own console command names agree without having been made to.
