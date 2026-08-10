# The parity bit in SINTRAN text files - measured

Full path: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\SINTRAN-FILE-PARITY-BIT-MEASURED-2026-08-09.md`

**Date:** 2026-08-09
**Source:** `DOC/captures/FA-READ-WRITE-2026-08-04/capture-read.txt` - a real file read off a
live SINTRAN over FA. 587 contiguous bytes of text content.
**Why:** task #33 asks for "set and remove the parity bit automatically for specified file
endings like SYMB, LIST". Before writing a per-extension table, find out what the bit is.

---

## What the manuals say

Not much, and nothing about files. The ND-100 and ND-110 manuals both say bit 7 of a byte is
"the parity bit" and is "always zero in ASCII code". SINTRAN programs mask it off when reading
characters - `A BZERO 7 % CLEAR PARITY BIT` in the SINTRAN III User's Guide, and
`BSET ZRO 70 DA % MASK OUT PARITY BIT`.

**There is no SINTRAN command to set or clear parity on a file.** Searched
`Reference-Manuals/`, `SINTRAN/`, `Operations/` and `Developer/`. What turns up is terminal I/O
parity settings and the ND-500 `SSPAR` string instruction, neither of which is a file
operation. So if a sync daemon is to do this, it does it itself, in the bytes it carries.

## What a real file actually contains

### Bit 7 IS used, and it is content-determined

139 of 587 bytes have bit 7 set. Masking it off turns the content into clean readable text:

```
;@(ND-PATCH-SIN-:SYST)NEW-SYST WRITE-MESSAGE ASK
MultiFunction Program MAY 27, 1988
$Checking SYSTEM generation information - wait...
```

The string `@(ND-PATCH-SIN-:SYST)NEW-SYST` appears three times in the run, at offsets 3, 197
and 422, and carries an **identical** bit-7 pattern each time:

```
@(ND-PATCH-SIN-:SYST)NEW-SYST
^......^^...^......^^.^^....^
```

So the bit is a function of the characters, not of position or noise.

### The rule is EVEN parity

Bit 7 is set exactly when the low seven bits hold an ODD number of ones, making the total
count even. Every marked character in that string has an odd population count - `@`(1),
`T`(3), `C`(3), `I`(3), `)`(3), `E`(3), `W`(5) - and every unmarked one is even - `(`(2),
`N`(4), `D`(2), `-`(4), `S`(4).

### But the SAME FILE also holds plain, unparitied text

This is the part that matters, and it kills the per-extension idea.

```
run length 587, even-parity violations: 120
every violation has bit 7 CLEAR - never a spurious set bit
first 53 bytes:  100% even parity
remaining 534:    78% conformance, bit 7 still set on 117 of them
```

The exceptions are never a wrongly-set bit. They are characters that even parity *would* have
marked, left plain. And they are not one clean region - the tail is a MIXTURE, 78% conforming.

Reading the text, the split lands where the SINTRAN command lines end and the message text
begins:

```
)NEW-SYST WRITE-MESSAGE ASK<<><>MultiFunction Program MAY 27, 1988<>$C
..............................X..X...XXX....XX...X.XXX...XXXXXX.XXX...
```

**One file, written by more than one tool, carrying both conventions.**

## What this means for the sync daemon

 1. **A per-extension parity table cannot be right.** The mixture is INSIDE a single file, so
    no rule keyed on `:SYMB` or `:LIST` can describe it. Do not build that table.

 2. **Stripping bit 7 on the way in is safe and correct.** Masking always recovers the text,
    on both the parity-marked and the plain parts. That is what SINTRAN's own programs do
    (`BZERO 7`). So the return leg should mask, and VS Code sees clean ASCII.

 3. **Setting parity on the way out is NOT obviously needed - INFERRED, not verified.** The
    captured file is a real ND patch script that presumably runs, and 117 of its bytes carry
    parity while others that would need it do not. So SINTRAN evidently tolerates text without
    parity. That is an inference from one file: it has not been tested by writing an
    unparitied file to a machine and compiling it. **Test that before relying on it** - it is
    exactly the sort of "it looked fine in the capture" reasoning that has cost this project
    twice already.

 4. Default: **mask inbound, send outbound unchanged**, with setting parity available as an
    option nobody turns on until point 3 has been tested live.

## Still unknown

 - Whether any SINTRAN tool REQUIRES parity on its input. Point 3 is the open question.
 - Whether the assembler, PLANC and NPL differ from each other on this.
 - Whether the file's two regions correspond to two writers, or to some other split. Only one
   file has been examined.

## Related

 - `DOC/PLAN-SYNC-DAEMON-2026-08-09.md` - task #33.
 - `DOC/captures/FA-READ-WRITE-2026-08-04/` - the capture this came from.
