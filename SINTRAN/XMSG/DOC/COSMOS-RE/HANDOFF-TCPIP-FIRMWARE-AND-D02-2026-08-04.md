# Handoff - ND TCP/IP (211185) firmware RE and D02 segment recovery

**Date**: 2026-08-04
**Status of record for**: the Ethernet II TCP/IP card firmware work and the damaged-D02 recovery.
Companion docs: `TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md`,
`TCPIP-D02-SEGMENT-RECOVERY-2026-07-30.md`, `TCPIP-B05-FIRMWARE-RE-2026-07-30.md`,
`WRITING-A-TCPIP-STACK-ON-SINTRAN.md`.

---

## 1. Where we got to

### DONE - the media is found and proven

ND's 211185 TCP/IP product, revision **B05 (1988)**, was recovered from the Tingo MFM disc dump
(`\\Nas9t\data\NorskData\FloppyImages\Tingo\Tingo-HDD\raw\tingo_raw_debug.img`, user `TCP-IP`).
This was the top item on the artifact hunt list.

All four BPUN banks pass the documented checksum rule ("arithmetic sum of all the words in the
Data field, modulo 2^16, big endian"). 8 of 8 stored checksums match.

**SCOPE LIMIT** (do not overstate this): only the BPUN files carry a checksum. The `.prog`,
`.brf`, `.mode` and `.symb` files on the same disc have **no checksum**, so their integrity is
unproven. "The media is intact" is true of the BPUNs only.

### DONE - the 512 KB Ghidra image

`E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\tcp-ser-all-banks-b05-68k.bin`
sha256 `6d2a76f061859612f0134fda7fead17d1472c646a5c5f5438ab7667667fd04a4`, 524288 bytes.
Built as `bpun[0x44 : 0x44+0x20000]` per bank, concatenated bank 0 -> 1 -> 2 -> 3.
Load in Ghidra as **68000, big endian, base 0x00000000, length 0x80000**.
Provenance and the record layout are in the README beside it.

**Measured layout** (this replaces the earlier bank-as-subsystem guess, which was WRONG - all the
code is one contiguous span and the subsystems are interleaved):

```
0x00000-0x003FF   68000 vector table
0x00400-0x0465F   low DRAM / BSS, plus a 2-byte stub run at 0x1F24+
0x04660-0x23F38   CODE - one contiguous span, all subsystems interleaved
0x24000 onward    tables and strings (dispatch 0x24A86, port names 0x24A5C)
0x7C3A0 onward    the embedded ND symbol table (463 records)
```

There is **no CODE/DRAM overlap**. An earlier warning to that effect was arithmetic I got wrong:
`BUFFER_END` (DRAM, 0x1A00) is simply below `END_PIOCOS` (CODE, 0x4660). One flat space.

### DONE - the flow fix

`PlancFixFlow.java` ran headless against the image: **16 rounds to stable, 653 fallthroughs
corrected, 464 epilogues marked RETURN, 589 sites disassembled**. The 9 sites it declined are
inline `ON ROUTINEERROR` handlers - that refusal is correct, leave it. The 3 "won't disassemble"
sites are code, not data.

The script now serves GUI and headless from one file (argument `apply` in headless, dialog in GUI),
so the old duplicate `*Headless.java` copies are obsolete.

### DONE - findings so far

- ~~`set DIX mode` = AIP request-block type **12**, handler at **0x7096**~~ **CORRECTED
  2026-08-08: opcode 12 @0x7096 is DEFINE/ADD MULTICAST ADDRESS, not "set DIX mode".** The 6-byte
  argument at `RB+0x08` is a full MAC address; the "bit 0 of byte 0" that is tested is the Ethernet
  I/G (group) bit (a non-multicast address is rejected -18), and the other 5 bytes ARE used. The
  `STOPMA`/`STARTMA` pair reprograms the LANCE multicast filter, which is what made the original
  reader guess "DIX mode". Sibling opcode 14 @0x7162 = REMOVE MULTICAST. The real DIX-vs-802.3 gate
  is the 0x1888A mode word, a separate mechanism. See
  `SINTRAN\XMSG\DOC\NDIX-XMSG-VS-SINTRAN-ETHII-CROSSCHECK-FINDINGS-2026-08-08.md` section 4b.
- The host seam is **`*TCP`** - a 4-character PIOC-OS port name at `0x7B90A`, copied by
  `TCPNETINIT` into `TCPPORTNAM`.
- ND shipped **TCP on the ND-100, IP on the card**, DIX 2.0 only (quoted from ND-860284-1 sec 1.3).
- PIOCOS - the card's RTOS - is byte-identical between the COSMOS/ENCOS load and the TCP/IP load.

### NOT DONE

- ~20 functions are pinned with one-byte bodies (the stale-body trap - see phase 1).
- The ND-100 side of the `*TCP` seam is undetermined: does the ND-100 **register** the port or only
  **look it up**? `PO100PORTS` / `POMS100POO` are the next targets.
- ~~Five of the six `set DIX mode` argument bytes are undecoded.~~ RESOLVED 2026-08-08: opcode 12
  is define-multicast; all 6 argument bytes are the MAC address (see the corrected finding above).
- AIP request blocks attach / start / stop are only partly decoded (ops 22/24/26).

---

## 2. D02 (1992) - the damaged copy

Found on `D:\ND\HDD\extract-ronny.img`. Damaged.

**Bank 3 RECOVERED and PROVEN.** Pulled from `SEGFIL0` and verified against the damaged file's own
stored checksum `0x99ee`. 3008 non-zero bytes against 2082 in the damaged original.

**Banks 0/1/2 not recovered.** Bank 0 was located at page 5015 but the checksum did not match.
Banks 1 and 2 have their checksum words zeroed.

### The method I used was the fallback, not the method

I located the segment copies by **content-scanning `SEGFIL0`**. The `sintran-carving` skill
explicitly forbids this: *"Never locate code by scanning for a constant."* The documented method is

```
segment bytes = SEGFIL0[ MADR*2048 : MADR*2048 + SEGLE*2048 ]
```

with `MADR` and `SEGLE` taken from the **Segment Table** (bank 3, offset `0o124000`),
entry = 8 words, `word[2]=LOGAD  word[3]=SEGLE  word[4]=MADR  word[5]=FLAG` (top 3 bits = SEGFIL#).

`EXTRACTING-SEGMENTS.md` lines 20-22 state the blocker plainly: those values live **only in the
in-memory Segment Table** - *"there is no fixed disk header to parse offline."* So content-scanning
was the only thing available to me statically, but it is why bank 0 came out ambiguous. Bank 3 only
landed because the checksum could verify a guess.

**Correct route for bank 0**: boot the image under RetroCore/nd100x, dump the Segment Table over
DAP, read `SEGLE`/`MADR` for segment **146 octal** (`TCPS0B0`), cut exactly, and check against
bank 0's stored checksum `0x2090`.

**Bank 1 is permanently unverifiable** - its checksum word is zeroed, so no candidate can ever be
proven. Do not produce a reconstruction for it; an unfalsifiable artifact is worse than a gap.

---

## 3. The reusable lesson

The segment file holds a **second copy of every loaded BPUN**, and the BPUN's own stored checksum
survives independently of the file-system damage. That gives a general recovery path for any
damaged pack:

1. Read the mode-run `:LIST` output to get the **segment number** (do not search for it).
2. Get `MADR`/`SEGLE` for that segment from the Segment Table.
3. Cut the segment out of `SEGFIL0`.
4. Verify against the damaged file's stored checksum.

`fsck` validating cleanly proves nothing here - it validates pointers, not contents.

This belongs in `EXTRACTING-SEGMENTS.md` and is not yet written there. See phase 4.

---

## 4. Plan - phases and todos

### Phase 1 - finish the Ghidra symbol pass (DONE 2026-08-08)

- [x] **Ronny**: close the program in Ghidra (headless cannot take the project lock while it is open)
- [x] Run `PlancApplyNdSymbols.java` headless as a **dry run** first
- [x] Read the dry-run failure list before applying anything
- [x] Re-run with the `apply` argument
- [ ] **Ronny**: reopen Ghidra so the RE agent can resume

**Result (2026-08-08)**: 436 defined records parsed (317 CODE + 119 DRAM), **0 failures**.
143 stale bodies removed and recreated, 94 entry points newly disassembled, 119 DRAM labels
placed. Function count 230 -> 404. Verified persisted with a fresh `-readOnly` dry run.

**Correction found on the way**: the script's default table base `0x7C3A4` was WRONG - at that
base the name-length byte reads 0 and the parser rejects every record (first dry run parsed 0).
The real base is **`0x7C3A0`**, pinned unambiguously by the +0 pointer field incrementing by
0x20 across records. 463 slots, of which 27 are `kind=0xFF` undefined/marker records (`NIL`,
`NONE_x`) and are correctly skipped - so "463 symbols" in this doc means 436 placeable ones.
The script default and its header comment are fixed
(`C:\Users\ronny\ghidra_scripts\PlancApplyNdSymbols.java`).

Ghidra project: `E:\Dev\Repos\Ronny\RetroGhidra\ETH_II\ND_ETH_II.gpr`, program
`tcp-ser-all-banks-b05-68k.bin`.

```
C:\Utils\Ghidra\ghidra_12.0.4_PUBLIC\support\analyzeHeadless.bat <project> <name> ^
  -process <prog> -noanalysis -postScript PlancApplyNdSymbols.java
```
Append `apply` after the script name for the real run.

Optional one-time GUI fix: Script Manager -> Manage Script Directories -> add
`C:\Users\ronny\ghidra_scripts`, so all 7 PLANC scripts appear again. They vanished because a
previous session deleted the `osgi` folder, which de-registers every script directory.
**Never delete the `osgi` folder** - delete only `osgi\felixcache` and `osgi\compiled-bundles`.

### Phase 2 - finish the firmware RE (needs phase 1)

- [x] `PO100PORTS` / `POMS100POO` - **SETTLED 2026-08-08: the CARD registers `*TCP`, the ND-100
      only looks it up.** `TCPNETINIT` -> `SKPOPENPOR` (net flag) -> `PORTOPEN` -> `PORTCREATE`
      (port type 2 -> `PO100PORTS`, the ND-100-facing pool) -> `PONAREGIST`. Port type 0/1 goes to
      `POPIOCPORT` (on-card only). Plate comments set on `TCPNETINIT`, `PO100PORTS` @0x2692C.
- [x] The MA command-port dispatch enumerated - `MACMDPORTH` @0x6D2E, table
      `tbl_maCommandDispatch` @0x24A86 (27 entries, only EVEN opcodes populated; odd -> error
      0x727C). Opcodes 0,2,4,6,8,10,12,14,22,24,26. **Opcode 12 @0x7096 = DEFINE/ADD MULTICAST
      ADDRESS** (verified, corrects the old "set DIX mode" label - see the strike-through above),
      opcode 14 @0x7162 = REMOVE MULTICAST. This is the on-card AIP(IP)->MA seam, a 6-bit opcode
      space, NOT the XMSG EXMTY 128..132 family and NOT the `*ENUM` XMSG server (which is a separate
      product, absent from both images - explains the no-ENUM-string scan).
- [ ] Decode the remaining AIP request blocks: attach, start, stop = opcodes 22/24/26 @0x71EA /
      0x6FF2 / 0x7050 (partly decoded).
- [ ] Fold the results into `WRITING-A-TCPIP-STACK-ON-SINTRAN.md` part 4.

Full cross-check against the NDIX (ND-500 Unix, 1988) source is in
`SINTRAN\XMSG\DOC\NDIX-XMSG-VS-SINTRAN-ETHII-CROSSCHECK-FINDINGS-2026-08-08.md` (sections 4a, 4b);
the RetroCore decoder `MON_200_XMSG.cs` now carries the func 45/48 version-gate and `XFRMR`
annotations.

### Phase 3 - D02 bank 0 (independent, needs only the emulator)

- [ ] Boot `extract-ronny.img` under RetroCore/nd100x
- [ ] Dump the Segment Table (bank 3, offset `0o124000`) over DAP
- [ ] Read `SEGLE` = word[3], `MADR` = word[4] for segment 146 octal (`TCPS0B0`)
- [ ] Cut `SEGFIL0[MADR*2048 : +SEGLE*2048]`, check against stored checksum `0x2090`
- [ ] If it passes, rebuild the 512 KB D02 image. If not, say so and stop.

Bank 1 is out of scope - see section 2.

### Phase 4 - write-up and decisions

- [ ] Add the section-3 technique to `EXTRACTING-SEGMENTS.md`
- [ ] Update `TCPIP-D02-SEGMENT-RECOVERY-2026-07-30.md` with whatever phase 3 finds
- [ ] **Ronny decides**: commit or not, and whether the ~1 MB of firmware `.bin` files belong in
      the repo at all

---

## 5. Traps for whoever picks this up

1. **The stale-body trap.** Ghidra does not recompute an existing function's body when control flow
   changes underneath it, and `createFunction()` on an existing entry point silently returns
   "already exists". Renaming such a function pins the broken body. Order must be:
   disassemble -> remove -> create -> name. Run `PlancFixFlow` **before** `PlancApplyNdSymbols`.
2. **`createLabel` ADDS**, it does not replace. Re-running a script that uses it stacks duplicates.
   `PlancApplyNdSymbols` uses `setName` on the function instead, so it is idempotent.
3. **The 5-character symbol truncation trap.** The 463 embedded ND symbols are full-length (up to
   10 chars, e.g. `RCVCOMPLET`, `XMTRINGAPP`). But `SYMBOL-1-LIST` / `SYMBOL-2-LIST` **are**
   5-char truncated - cross-referencing against them on a full name will miss and mean nothing.
4. **`ERRCODE` width sets the first-parameter offset.** `move.w D0w,(0x10,A6)` -> first param at
   `0x12`; `move.l D0,(0x10,A6)` -> `0x14`. Both are correct, decided per routine. Version F
   changed the word size. This is not a contradiction in the docs.
5. **`jmp (A5)` is the runtime ERROR vector** (`#XRET`/`#ERET`), not a continuation address. A5
   permanently holds it.
6. **`4E EA 00 02` IS the return.** The *caller* resumes at RETLINK+2 - that is the PLANC-MC skip
   return, not a mid-function jump.
7. **`ndtool -x` flattens all users into one directory** and silently skipped 35 files on the
   Tingo extract. Extraction itself is byte-perfect (proved with a control) - the flattening is the
   bug. This is separate from the `--put` structure note.
8. **Read the mode-run `:LIST` output for segment numbers.** Do not search the image for them.
