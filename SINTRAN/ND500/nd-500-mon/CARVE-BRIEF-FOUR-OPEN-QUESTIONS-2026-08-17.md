# Carve brief: four open questions, all needing Ghidra

**Date:** 2026-08-17
**For:** a Windows session with Ghidra. Everything else on the DESC/`:LINK` thread is done;
these four are the whole remaining list, and none of them can be answered from WSL - WSL2
cannot reach the Windows Ghidra (loopback unreachable, nothing listening on the LAN address,
no `ghidra` MCP entry in any WSL-side config).

Each item below says what is already known, what the exact question is, and which binary and
address to start from. They are independent - do them in any order.

---

## 1. Domain-entry field offsets past DNAME

**Known.** The segment entry is fully resolved: ten field offsets from `MON-DEBUG:PROG` J04's
own print routine, evidence per field in
`CARVE-ANSWER-DESC-FIELD-OFFSETS-2026-08-11.md`. The domain entry is not - only word 0
(SEGLINK) and byte 4 (DNAME) are byte-exact. Everything after DNAME (`childDomains`, `mother`,
`childIndex`, `procPrior`, `flag`, `STADR`, `ENABLEINT`, `THA`, `SYSENABL`, `PBITMAP`,
`DBITMAP`) rests on the manual's field order alone and is deliberately omitted from
`desc-format.json`'s verified set and from `pcc-nd500`'s `desc.h`.

**Question.** Which byte offset is each domain-entry field at?

**Where to start.** The same method that settled the segment entry works here. The domain-entry
buffer is bank-1 word `037651B`. The monitor prints domain-entry doubles at words
`0, 17B, 21B, 23B, 25B, 30B, 32B`, with label strings in the bank-2 run at bytes
`0x80C6`-`0x8140` (`$Domain  : `, `  Start address:`, `$Owner:`, `  Childindex:`). Pair each
label print with the single field load that follows it, exactly as was done for `$PLB:` at
`014575B`. The domain-entry reader itself is `013454B`.

**Check your answer against** the 13 real DESC files now committed in
`SINTRAN/File-Formats/samples/`. Whatever offsets you derive must be consistent across all of
them; `SCRATCH-DOMAIN` is present on every floppy and always has index 0.

## 2. Segment-entry bytes 74-84: the manual contradicts the monitor

**Known.** The manual says these bytes are the `COMSEGSIZE` and `N100SEGNO` arrays. The monitor
instead prints **two byte strings** starting at byte 74 and byte 80, using a character count it
reads from word `37B` (byte 62), with `LBYT` loops at `015256B`-`015301B` and
`015311B`-`015335B`. Both readings cannot be right. This is recorded as unadjudicated in
`DESCRIPTION-FILE-FORMAT.md`, `desc-format.json` and `desc.h`, and must stay that way until
settled.

**Question.** What actually lives at bytes 74-84 - two counted byte strings, or two small
arrays?

**Where to start.** Read what word `37B` is used for elsewhere. If it is a character count, the
manual's `COMSEGSIZE` reading is wrong; if the monitor is printing array elements as characters
for display purposes, the manual may be right and the monitor merely sloppy. The samples give
you real bytes to test either reading against.

## 3. The `:LINK` file format

**Known.** `LINK-FILE-FORMAT.md` is measurements only, from 11 real files now in
`SINTRAN/File-Formats/samples/`. One strong structural finding: **every non-empty `:LINK` is
exactly 32k + 1 bytes**, 11 of 11, across a thirtyfold size range and seven years, with the last
byte `0x00` and no long trailing zero run. And one finding that blocks any quick answer: the
contents are **not one uniform layout**. The 1982 `FORTRAN-500.LINK` is 392 fixed 32-byte
records each opening `ff ff ff ff` with a 7-8 character symbol name at +16 and an apparent
length byte at +4; `COBOL-85-K01.LINK` has that marker once then a long zero run;
`SL202-FO-L27.LINK` has no marker at all and opens with length-prefixed source-file
specifications. The linker and the symbolic debugger both ship with a **zero-byte** `:LINK`.

**Question.** What reads a `:LINK` file, and what is the record layout?

**Where to start.** Whichever routine in NLL or `MON-DEBUG:PROG` opens a file of type `LINK`
and indexes into the buffer. The DESC precedent is the method to copy: find the reader, find the
print or use site, and let the code name the fields. Note the open questions listed in section 6
of `LINK-FILE-FORMAT.md`, particularly what produces the `32k + 1` length.

## 4. Does write-open clear a file's byte count? (OPENF)

**Known.** This one is not about DESC - it decides whether a live change to the emulator is
correct. `nd500x`'s ndmonlib mapped access code 0 (sequential write) to fopen mode `"wb"`, which
truncates an existing file to zero **at open**, before the guest writes anything. A program that
opened its output and then aborted destroyed it, which is how a valid 2,316,049-byte `:DOM` came
to be found at 0 bytes - the ND linker opens an existing domain with an unquoted name and lands
exactly there. As of 2026-08-17 the emulator uses `"r+b"` and does not truncate, on the grounds
that destroying data on an unverified assumption is the worse failure.

**Question.** On real SINTRAN, does opening an existing file for sequential write clear the
object entry's byte count?

**Where to start.** The `OPENF` worker, reached as `MCTAB[50B]` = `103034B` = `OPFIL` ->
`FCON@067002B` -> `FFILE@065144B` in the L-VSX-500 carve. The resolver half is already done and
is the reason this is answerable: `GCFIL@064670B` dispatches `CROBJ@063726B` for a quoted name
and `GFILI@057173B` for an unquoted one, and `GFILI` is lookup-only - it has no create call and
does nothing to the file. So if truncation happens anywhere it is in `OPENF` itself. Look for
any store that clears the byte count on the unquoted write-open path.

**If the answer is "yes, it truncates":** the emulator's `"r+b"` is wrong and the comment in
`external/ndmonlib/src/support/mon_file_table.c` at `ACCESS_SEQ_WRITE` says so explicitly - it
records the open question rather than claiming to be right. Reverting is a one-line change, but
the `.DOM` data-loss problem then needs a different answer.

---

## Traps that already cost time on this binary

All four held on the last pass; assume they still do.

- The shipped `nd-500-mon-j04.prog.asm` **disassembles pointer words as instructions**. Literal
  pools at `013544B`-`013551B`, `014714B`-`014735B`, `016264B`-`016277B`, `016462B`-`016506B`.
- P-relative effective address = **address of the instruction** + displacement, not
  next-instruction-relative.
- `JPL` is opcode `0o134`, not `0o130`. The wrong value finds no calls and reads like a finding.
- `LDF`/`STF` move 3-word PLANC descriptors (pointer + length), not floats.
- Bank 1 (code) and bank 2 (data) **both base at word 0**. Import them as two Ghidra programs; a
  bank-1 pointer word holding `040734B` refers to the bank-2 string at that address.

## What is already settled - do not re-derive

- The DESC segment-entry layout, the size rule `PLB+PSIZE+1 = .pseg` / `DLB+DSIZE+1 = .dseg`
  (48 checks across 13 floppies, no mismatches), the page geometry, and the segment linked list.
- CONVERT-DOMAIN is **not** a witness for the size fields - it uses MON 62B GetBytesInFile.
- The DESC read is MON 74 SETBT plus a MON 1 INBT loop, **not** RFILE.
- `DLINKDATE`, `ABSFIXAD`, `LOWLOGFIX`, `PLOLOGFIX`, `PUPLOGFIX`: offsets proven, meanings not.
  All five read zero in all 26 real segment entries, so the names are the monitor's labels only.
  Do not spend time "confirming" values that are uniformly zero - find a sample that sets one.

**Entry point for the whole thread:**
`SINTRAN/File-Formats/HANDOFF-DESC-AND-NRF-STATE-2026-08-11.md`
