# Microcode Answer - Trap Report Fields Back to SINTRAN (C5 / Q-TRP-01 / plan 3.2)

**Date:** 2026-07-20
**Track:** ND-5000 microcode-CPU (B30 image)
**Question:** What EXACTLY does the microcode place in a trap report message back to the
ND-100 (SINTRAN) when the ND-500 faults? Which word is the access type, which is the failing
logical address, how is the segment encoded, and how does that map onto SINTRAN's printed
`... segment ... access / Logical address N <octal>B`?

**Image identity (stated explicitly):** all microcode citations are from the **ND-5800 B30**
(octobus generation) image - decoder `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`,
raw `E:\Dev\Ronny\ND5000UC\docs\MC\MICRO-5800-B30.DATA` / `.LABE`. This is NOT the classic
144-bit ND-500; on this image the classic 21B/22B mailbox path is disabled and traps flow
through the octobus/OCB path.

---

## TL;DR verdicts

| # | Point | Verdict |
|---|-------|---------|
| 1 | Access-type word + encoding | **PARTLY CONFIRMED.** Access type is NOT one tidy "fetch/read/write" enum word. It is carried by (a) the **trap number** at `link.16` (TRAPN) and (b) the **MMS/DMM/IMM hardware status word** copied verbatim into the trap-number-dependent parameter area `link.17..22`. Program-vs-data = which fault class fired (IMM=`TRAP_IFC` vs DMM=`TRAP_DFC`); read-vs-write = a bit in that status word. |
| 2 | Failing-logical-address word | **CONFIRMED it is the full 32-bit VA.** The microcode copies the collected `LA` (logical address) as one 32-bit value; SINTRAN splits it for printing. Verified by arithmetic against the live example (see point 4). |
| 3 | Segment encoding | **CONFIRMED: NOT a separate field.** The segment is simply the top 5 bits of that same 32-bit LA. ND-500 Reference Manual: "The 5 upper bits of an address are the segment number and the 27 lower bits are the address within the segment." |
| 4 | Field layout + mapping to SINTRAN print | **CONFIRMED at the link.NN level** (vendor manual section 13.16); the per-word offsets inside the "varies" area `link.17..22` are **[D]** (trap-number dependent, not enumerated by the vendor). |
| 5 | MMS_SIX0 top-two-bits caveat | **CONFIRMED and CRITICAL.** `MMS_SIX0` masks `0xC0000000` (octal LARG `30000000000`), i.e. the TOP two bits of the hardware exception-status word, to classify the fault. Hardware bit order != any software struct order. This is the most likely root cause of the emulator's wrong access type. |

---

## Point 1 - Which word holds ACCESS TYPE, and how encoded

**Verdict: PARTLY CONFIRMED (access type is composite, not a single enum word).**

The microcode does not emit a single "access = fetch/read/write, data/program" word. Access
type is reconstructed by SINTRAN from two things the microcode DOES place:

1. **The trap number** - `link.16` = `TRAPN` (SINTRAN symbol `TRAPN=000016`, see
   `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB:744`).
   Written by the microcode at `TRAP_GEN4C` (`013574`):
   - `MICRO-5800-B30.md:6026` -> `| 013574 | TRAP_GEN4C | ALU,A TYP,HW A,SC14 B,X1 T,JMP ... WR,POF ADDR=013575 |`
   - Pseudocode `MAILBOX-MICROCODE-PSEUDOCODE.md:870`: `mem_hw[msg + TRAPN] = trapn; // 013574 (TRAP_GEN4C): [V value, X offset 16]`, and `:867` `uint16 trapn = 0o46; // 013563: page-fault trap number [V]`.

2. **The hardware MMS/DMM/IMM status word**, copied verbatim into the trap-parameter area.
   The COLLECTION step is `TRAP_SAM`/`TRAP_DFC`/`TRAP_IHWF`, documented at
   `MAILBOX-MICROCODE-PSEUDOCODE.md:792-794`:
   > "`TRAP_DFC`/`TRAP_DHWF`/`TRAP_IHWF` collect DMM/IMM status, LA, PHYS, CAP, WR into the
   > record for fault reporting."

**Program-vs-data** = WHICH fault class fired:
- instruction fetch / program = `TRAP_IFC` (IMM path), pseudocode `:783-784`, `:792`.
- data read/write = `TRAP_DFC` (DMM path).

**Read-vs-write** = the `WR` bit inside the collected status word (the "WR" in the
`{... CAP, WR}` list above). The ND-500 Reference Manual confirms the three-way access
distinction exists in hardware:
- `E:\Dev\Ronny\ND500-DOCS\ND-05.009.4 EN ND-500 Reference Manual.md:2147`:
  "These two registers are compared to the logical program and data address for each memory
  reference ... (Memory reference type may be fetch, read, or write access.)"

So: the emulator must not invent a scalar access-type field. It must (a) pick the correct
**trap number** for the fault class and (b) fill the **status word** with the correct
hardware bit layout (see point 5).

---

## Point 2 - Which word holds the FAILING LOGICAL ADDRESS; full 32-bit VA or split

**Verdict: CONFIRMED - it is the full 32-bit logical VA (one value, two ND-100 words).**

- The collected record carries `LA` (the logical address) as a 32-bit quantity
  (`MAILBOX-MICROCODE-PSEUDOCODE.md:794`, list `{... LA, PHYS, CAP, WR}`).
- `TRAP_GEN3`/`3B`/`3C` (`MICRO-5800-B30.md:5994`, `6005`, `6009`) copy the SRF trap record and
  its parameters "into the message data part as words + halfwords" -
  `MAILBOX-MICROCODE-PSEUDOCODE.md:861-863`.
- It is NOT pre-split into segment/offset by the microcode. The proof is the emulator's own
  live example decoding cleanly as a single 32-bit VA (point 4). SINTRAN performs the split
  when it prints.

The LA lands somewhere in the trap-number-dependent parameter area `link.17..22`
(vendor manual, point 4). The EXACT word within that area differs per trap number - this is
the direct explanation of the second emulator symptom (`Logical address 0 0B`): the emulator
wrote the LA at the offset used by one trap class, but the actual trap fired was a different
class whose LA sits at a different offset, so SINTRAN read a still-zero word. [D on exact
per-trap offset - needs the SINTRAN `DECOERRMESS` carve, Q-TRP-02/03, or a live single-step
of `TRAP_GEN3`.]

---

## Point 3 - How the SEGMENT is encoded

**Verdict: CONFIRMED - the segment is NOT a separate field; it is the top 5 bits of the LA.**

ND-500 Reference Manual `E:\Dev\Ronny\ND500-DOCS\ND-05.009.4 EN ND-500 Reference Manual.md:1313`:
> "A logical address domain is divided into 32 segments. The 5 upper bits of an address are
> the segment number and the 27 lower bits are the address within the segment."

(Also `:1289` "5 bits of logical address to be converted: The logical segment number", and
`:843` each domain is a full 32-bit program area + 32-bit data area.)

Therefore, given a 32-bit failing VA:
- `segment  = (VA >> 27) & 0x1F`    (5 bits)
- `offset   =  VA & 0x07FFFFFF`      (27 bits, printed as octal)

SINTRAN's printed "`Logical address <seg> <offset>B`" is exactly this split of the single
32-bit LA word. The emulator does not need to encode a separate segment field - it only needs
to put the correct 32-bit VA in the LA word and the split is SINTRAN's.

---

## Point 4 - Overall field layout and mapping to the SINTRAN print

**Verdict: CONFIRMED at the link.NN (ND-100 word) level; per-word inside link.17..22 is [D].**

### 4a. The message block (vendor manual section 13.1, `ND-05.012.01 ... Micro Program Guide.md:1100-1111`)

Header (6 ND-100 words) + data part:

| link.NN | Field |
|---------|-------|
| link.00/01 | Next link (2 words; -1 = end) |
| link.02 | Status (0 free, 1 to-500, 2 in-process, 3 answer-to-100, 4 error-return) |
| link.03 | Sender (RT description) |
| link.04 | Receiver (ND-500 process number) |
| link.05 | Size of data part |
| link.06 | Function value (data part starts here) |

### 4b. The TRAP data part (vendor manual section 13.16, `...Micro Program Guide.md:1371-1388`)

| link.NN | SINTRAN symbol | Content | Microcode citation |
|---------|----------------|---------|--------------------|
| link.06 | (function) | `23` (Start / Monitor call / Trap) | function dispatch |
| link.11 | `STOPR` | `2` = TRAPCODE (marks "this is a trap", not a monitor call) | `013513` writes BM01 (=2 halfword); pseudocode `:855`, `:868` |
| link.12/13 | (trapping P) | Trapping P register (32-bit) - address of faulting instruction | `TRAP_GEN4B` `013572-73` (`MICRO-5800-B30.md:6024-6025`), pseudocode `:869` |
| link.14/15 | | Restart P (32-bit) | pseudocode `:871` |
| link.16 | `TRAPN` | **Trap number** | `TRAP_GEN4C` `013574` (`:6026`), pseudocode `:870` |
| link.17..22 | (varies) | **"Varies depending on trap number"** - the fault parameters: the **32-bit failing LA**, the **MMS/DMM/IMM status word** (holds read/write + fault class), PHYS, CAP | `TRAP_GEN3/3B/3C` `013534-6013` (`:5994-6013`), pseudocode `:861-863` |

The vendor manual explicitly leaves `link.17..22` as **"Varies depending on trap number"** and
does NOT enumerate the sub-fields (`...Micro Program Guide.md:1384`). That undecoded area is
precisely where access-type detail and the failing LA live, and it is where the emulator's
fields are wrong.

### 4c. Mapping onto the live example (this is the load-bearing verification)

Emulator recorded an **instruction fetch at VA `0x080081A5`**. SINTRAN printed
`DATA segment READ access / Logical address 1 100645B`.

Decode `0x080081A5` with the section-3 rule:
- segment = `(0x080081A5 >> 27) & 0x1F` = `1`            -> matches printed segment `1`. **CORRECT.**
- offset  = `0x080081A5 & 0x07FFFFFF`  = `0x0081A5` = `100645` octal -> matches `100645B`. **CORRECT.**

So the **failing LA and segment were RIGHT** in this case - the emulator put a valid 32-bit VA
in the LA word and SINTRAN split it correctly. The ONLY wrong thing in case 1 was the
**access type**: SINTRAN printed "DATA ... READ" for what was really a PROGRAM (instruction)
FETCH. That is a status-word / trap-number problem (points 1 and 5), not an address problem.

Second symptom - segment-1 data WRITE printed `Logical address 0 0B` - is the complementary
failure: for THAT trap class the LA word the emulator filled is not the word SINTRAN reads
(the "varies" area layout is per-trap-number), so SINTRAN read a zero word.

**Net:** the emulator's LA VALUE and segment MATH are correct; what is wrong is (i) the
access-type/status-word encoding and (ii) placing the LA at a single fixed offset instead of
the trap-number-specific offset in `link.17..22`.

> Note: an earlier note in
> `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START-2026-07-20.md:37-38`
> concluded "our trap diagnosis was right and SINTRAN's console print is wrong." Half true:
> the fetch-vs-read classification disagreement is real, but SINTRAN's ADDRESS print (seg 1 /
> 100645B) is exactly correct for `0x080081A5`, so SINTRAN is decoding the LA faithfully. The
> disagreement is confined to the access-type field, which the emulator, not SINTRAN, is
> filling wrongly.

---

## Point 5 - The MMS_SIX0 top-two-bits caveat (verify against the status word)

**Verdict: CONFIRMED and this is the probable root cause of the wrong access type.**

`MMS_SIX0` (`MICRO-5800-B30.md:5682`):
```
| 013044 | MMS_SIX0 | ALU,AND A,LARG LARG=30000000000 B,SC14 D,SC5 ... ADDR=140000 |
| 013045 |          | ALU,XOR A,LARG LARG=30000000000 B,SC5 ... |
| 013046 |          | ALU,XOR A,BM00 B,X1 C,SEQ ... -> PF_NORM |
```
Octal `LARG=30000000000` = `0xC0000000` = the **TOP TWO BITS** (bit 30 and bit 31) of the
32-bit word. `MMS_SIX0` ANDs the MMS status word (in SC14) with `0xC0000000`, then XORs to
test whether both top bits are set, to CLASSIFY the fault (route to `PF_NORM`, `PROTVIOL`,
`MMS_ERROR`, `MMS_PST0`, ...). Companion masks in the same block:
- `PF_NORM` (`:5687`): `LARG=03700000000` = `0x1F000000` = bits 24-28 (the page-fault info sub-field).
- `:5688`: `LARG=00600000000` = `0x06000000` = bits 25-26 (sub-type test).

**Consequence for the emulator:** the access/fault-class discriminator lives in the HARDWARE
exception-status word's TOP bits (`0xC0000000`) and adjacent high bits (`0x1F000000`), in
hardware bit order. The microcode copies that status word essentially verbatim into the
`link.17..22` area, and SINTRAN's `DECOERRMESS` decodes access type from those same hardware
bit positions. If the emulator constructs its "status word" from a C# software struct whose
bit order does not match (e.g. read/write or data/program placed in a different bit than the
hardware's top two / high five bits), SINTRAN will mis-decode - which is exactly the observed
"instruction fetch shown as DATA READ". Field order in the software struct MUST match the
hardware exception-status word bit layout: top 2 bits = fault class, bits 24-28 = fault info.

---

## Reconstructed trap-message field layout (for the emulator)

```
Data part of the trap answer (function link.06 = 23, STOPR link.11 = 2):

  link.06  = 0o23    function value (Start/Moncall/Trap)
  link.11  = 0o2     STOPR = TRAPCODE  -> "this is a trap"
  link.12  } 32-bit  Trapping P (address of faulting instruction)
  link.13  }
  link.14  } 32-bit  Restart P
  link.15  }
  link.16  = trapno  TRAPN (page fault = 0o46; others per trap-number table)
  link.17  \
  link.20   \  "varies depending on trap number":
  link.21   /     - 32-bit failing LOGICAL ADDRESS (full VA; SINTRAN splits
  link.22  /        seg = VA>>27 & 0x1F, offset = VA & 0x07FFFFFF)
                   - MMS/DMM/IMM STATUS WORD (hardware bit order:
                       top 2 bits 0xC0000000 = fault class,
                       bits 24-28 0x1F000000 = fault info,
                       a WR bit = write vs read)
                   - PHYS, CAP as applicable

Access type is composite: (program vs data) = trap number / IMM-vs-DMM class;
(read vs write vs fetch) = status-word bit + trap number. NOT a standalone enum word.
Segment is NOT separate: it is the top 5 bits of the LA word.
```

---

## What remains UNKNOWN / [D]

1. **Exact byte/word offset of the LA within `link.17..22` for each trap number.** The vendor
   manual says only "varies depending on trap number" (`...Micro Program Guide.md:1384`) and
   the pseudocode marks the `TRAP_GEN3` offsets `[D]` ("ADACT stepping not rendered ... exact
   offsets D", `MAILBOX-MICROCODE-PSEUDOCODE.md:768-769`, `:863`). This is the field most
   likely responsible for the `Logical address 0 0B` symptom and needs either the SINTRAN
   `DECOERRMESS` carve (Q-TRP-02/03) or a live single-step of `TRAP_GEN3` (`013534`).

2. **Exact bit assignments inside the MMS status word** that SINTRAN maps to the words
   "READ"/"WRITE"/"FETCH" and "DATA segment"/"program". Verified: top 2 bits = class,
   bits 24-28 = info, there is a WR bit. NOT yet byte-verified: the precise bit number of WR
   and the exact class encoding SINTRAN keys on. Source of truth = SINTRAN `DECOERRMESS`
   (carve) cross-checked against the hardware status word the emulator produces.

3. **Whether `link.17..22` for a data-access fault carries the LA in one 32-bit slot or the
   status word first.** The manual does not order them; the microcode order is `[D]`.

4. **Trap-number table validity for MMU faults.** The reference-manual trap bits (protect
   violation = bit 36, `ND500_TRAP_SYSTEM_COMPREHENSIVE.md:127`) are from the reference manual;
   the mapping trap-bit -> `TRAPN` value placed at `link.16` for each of {instruction-fetch
   protect, data-read protect, data-write protect, page fault} is not byte-verified here.

---

## Sources

- `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` (B30 decoder) - `MMS_SIX0` `:5682-5686`,
  `PF_NORM` `:5687-5691`, `TRAP_GEN1..4` `:5967-6035`, `TRAP_SAM`/`TRAP_FIND` `:5515-5752`.
- `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` - trap section 3.9
  `:763-919` (record `:788`, `:792-794`; STOPR/TRAPN `:855`,`:867-870`; offset caveat `:768`).
- `E:\Dev\Ronny\ND500UC\manuals\ND-05.012.01 ND-500 Micro Program Guide.md` - message block
  section 13.1 `:1100-1111`, TRAP section 13.16 `:1371-1388`.
- `E:\Dev\Ronny\ND500-DOCS\ND-05.009.4 EN ND-500 Reference Manual.md` - VA split `:1313`,
  `:1289`; access types `:2147`; page-fault-on-PST-0 `:1616`.
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB` - `TRAPN=000016` `:744`,
  `TRAPI=000012` `:655`.
- Context: `...\OPEN-QUESTIONS-REGISTER-2026-07-20.md:127-133,384`;
  `...\QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START-2026-07-20.md:226-234`.
