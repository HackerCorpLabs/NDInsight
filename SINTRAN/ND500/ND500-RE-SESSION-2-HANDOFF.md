# ND-500 Reverse Engineering - Session 2 Handoff (Open Items)

**Date:** 2026-07-08
**Purpose:** hand the REMAINING open items to the next RE session (LLM + Ghidra),
after session 1 closed Q2/Q3/Q6 and partially advanced Q5. Read this file FIRST,
then the two findings files below, then the original task spec only for the
rules you need.

**Prior documents (read in this order):**

1. [ND500-MON-RE-FINDINGS.md](ND500-MON-RE-FINDINGS.md) - session 1 results
   (Q6 no-IOX, Q3 MON 60B interface, Q2 constants), INCLUDING the reviewer's
   REVIEW OUTCOME note at the top. Reviewed and committed.
2. [ND500-PLACE-LIBRARY-C9-FINDINGS.md](ND500-PLACE-LIBRARY-C9-FINDINGS.md) -
   session 1 partial Q5 result (capability bit-name inventory, 27-bit logical
   address, bit-11 index boundary). STATUS: PARTIAL - C9 is NOT resolved.
3. [ND500-L-RELEASE-RE-TASK-HANDOFF.md](ND500-L-RELEASE-RE-TASK-HANDOFF.md) -
   the original task spec. Section 6 (poisoned priors, method rules,
   contradiction protocol) still applies VERBATIM to this session.
4. [ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md) and
   [ND500-EVIDENCE-AND-CONTRADICTIONS.md](ND500-EVIDENCE-AND-CONTRADICTIONS.md) -
   the spec and dossier your findings will be patched into.

---

## 1. Rules (unchanged, plus two new lessons)

All of handoff section 6 still applies: cite every claim to a disassembly
address / file:line / manual section, mark UNVERIFIED otherwise, two independent
sources before a dossier upgrade, contradiction protocol (report, do not
silently overwrite), ASCII only, octal marked with B suffix.

Two lessons learned in session 1 - do not repeat them:

1. **Truncated symbol names.** N500-SYMBOLS.SYMB tables truncate names to about
   5 characters (MSGN5 = MSGN500). A "symbol not found" under its full name is
   NOT evidence of absence - search the 5-char prefix.
2. **Octal/decimal base trap.** Session 1 matched binary word 0x080F (2063
   decimal) against symbol PFECSLOAD=2063 OCTAL (=0x0433) - wrong. When mapping
   a binary constant to an NPL SYMBOL, convert the SYMBOL's octal value to hex
   and compare hex-to-hex. Never compare a decimal rendering to an octal one.

---

## 2. Open items, in priority order

### Item A - identify the second MON 60B retry status (ram:ccc5 = 0x080F)

- Binary: F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\500\ND-500-MON-J04.PROG (reload in
  Ghidra; session 1 had it open, session end had PLACE-BIG-2B loaded instead).
- Established: wrapper at ram:cca4 retries MON 60B while returned A equals
  ram:ccc4 = 0x041A (= 2032B = ECSLOAD, 5P-P2-MON60.NPL:66) OR ram:ccc5 =
  0x080F. 0x080F = 4017B = 2063 decimal. NO 4017B symbol exists in the MON-60
  status table (5P-P2-MON60.NPL:40-114).
- Tasks:
  1. Re-read the word at ram:ccc5 to triple-confirm 0x080F (hexdump at
     ram:ccc1 showed "... fe dd 04 1a 08 0f b4 78 ...").
  2. Grep the whole NPL tree and ALL symbol tables (K03/L07/M06) for value
     4017B / 0x080F / decimal 2063-as-octal-4017, remembering rule 1 above.
  3. Check whether 0x080F could be a MON-60 status with a bit OR-ed in
     (e.g. 2000B block code plus flag), or a status from a DIFFERENT table
     (message status, XMSG, file system error 17B?).
  4. If unresolvable statically, mark UNVERIFIED and propose the dynamic check
     (break on the MON 60B return in the emulator).

### Item B - finish Q5 / contradiction C9 (capability word bit positions)

- Binary: F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\500\PLACE-BIG-2B-C01.BRF
  (probably still loaded).
- Established (see the C9 findings file): bit-name table at ram:20e3-2199
  (Words A-E), W/P/S names confirmed, logical address = 27 bits with index
  boundaries at bit 11 (level 0) and bit 21 (level 1) - suggestive of the
  11-bit physical-segment field (C9 Claim A) but NOT conclusive.
- Tasks (from the findings file section 5):
  1. Locate the capability-word decoder loop that walks the ram:20e3 table
     (reached via computed base - scan for loads of addresses 0x2100-0x2140,
     or trace the LOOK-AT-PHYSICAL-SEGMENT / domain-info-dump handlers) and
     read its shift/mask to assign NUMERIC bit positions to W/P/S.
  2. Cross-check against Reference-Manuals ND-05.009.4 (ND-500 Reference
     Manual) MMS chapters - the authoritative source dossier C9 lacked.
  3. Only with BOTH (or one decisive) upgrade dossier C9; otherwise report
     progress and leave C9 UNRESOLVED.

### Item C - MON 60B parameter-block layout + high subfunction names

- Known: block pointer passed in A; built at B-0x6f..B-0x7b on the caller
  stack; offset 6 = returned status. Stub array ram:ccc8-ce6d maps user entry
  points to subfunction codes 0-177B plus 201B-214B (LDA-form encoding).
- Tasks:
  1. Trace 2-3 representative stubs' CALLERS backwards to see what they store
     into the block before calling the wrapper -> field offsets 0-5.
  2. Name the 201B-214B subfunctions from 5P-P2-MON60.NPL lines 165-285
     (session 1 decoded only the SAA-form codes in detail).
  3. Deliverable: a parameter-block offset table per subfunction class, each
     row cited to a disassembly address.

### Item D - Q1: ND-211305 floppy inventory

- The L-release info manual says ND-500 support ships on floppy ND-211305
  (ND-500-MON-J:PROG, SWAPPER-K:PSEG/DSEG, Place Library, MIC-5xxx microcode).
- Lead: directory listings exist at F:\ND\SINTRAN-L - 2026\FILE-INFO\
  (211024E02-XX-01D.TXT, BIGDISK0-L.TXT, VSXL1/2/3.TXT). VSXL3.TXT is floppy
  250305L07-XX-03D (SYSTEM files incl. N500-SYMBOLS:SYMB, XMSG-COMMAND:PROG) -
  NOT 211305, but the same-format listings may cover it, and BIGDISK0-L.TXT
  (the installed system disk) shows what 211305's contents look like AFTER
  installation.
- Tasks: enumerate which of the expected 211305 files exist under F:\ND (any
  release tree), record exact filenames/sizes/dates, and flag anything the
  manual lists that is absent.

### Item E - Q4: SWAPPER-K:PSEG/DSEG

- Not yet opened in any session. Goal (from the original handoff): confirm the
  swapper is placed INTO the ND-500 as a domain by the ND-100 (matches spec
  section 12: 5SWAP the RT program is ND-100 side; the PSEG/DSEG is the ND-500
  resident swapper domain), and extract its MICFU/message usage if visible.
- Note: PSEG/DSEG are ND-500 (32-bit) images - the ND-100 Ghidra loader does
  NOT apply. Expect big-endian 32-bit ND-500 code; disassembly support may be
  limited, so string/structure analysis may be the realistic ceiling. Say so
  honestly if that is the case.

### Item F (optional, third source) - live mailbox dump

- Session 1's constants (findings section 3) are symbol-table verified with the
  PROG binary as informal corroboration. The original handoff's dynamic
  shortcut still stands: boot SINTRAN-L in the emulator with the 3022 stub,
  break after XMSINIT, dump the mailbox bank, and read the live message
  headers (N5STA values, MAILINK head = DUMMESS address). This would also
  settle Item A dynamically.

---

## 3. Deliverables

Per item: a findings section in a new file
`ND500-RE-SESSION-2-FINDINGS.md` (same folder), same format as session 1's
findings files: every claim cited, UNVERIFIED tags, a patch-list section
proposing dossier/spec edits (do NOT edit the dossier or spec yourself), and an
explicit "NOT determined" list. The reviewer will verify and apply.
