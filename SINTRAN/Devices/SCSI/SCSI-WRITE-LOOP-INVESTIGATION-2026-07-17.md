# SCSI Write Loop Investigation - Status 2026-07-17

## VERIFIED FIXED (17:13 emulated time, 2026-07-17 real time)

With the fix in place the SCSI boot runs clean: full SINTRAN III banner
("SINTRAN III - VSX/500 K" - note: K generation, confirming the carver's
different-generation finding), "SINTRAN III RUNNING", swap pages allocated, XMSG/XROUT
started, and the system proceeds into normal terminal login handling (User Environment
"not started" notice = the UE menu product is not configured on this disk - a normal
message, not an error of ours). No "STACK OVERFLOW AT" output, no write loop.

Final fix form (improved by Ronny): StackFramePageTable is a CONST
PageTable.AlternativePageTable - matching the ND-110 RASK microcode, which issues every
frame access as COMM,RDRQ/WRRQ,APT (ENTR 001162, ENTRC 001174, ENTRB 001206,
ELEAV 001213, LEAV 001224); the PTM gate lives once in mapVirtualToPhysical
("alternative table when PTM set, standard otherwise"). Inline parameters stay on the
standard table (microcode reads them COMM,RDRQ,PT).

## ROOT CAUSE (15:45, FIXED, pending live verification)

**Emulator bug: the ND-100 stack instructions (INIT/ENTR/LEAVE/ELEAV) performed all
stack-FRAME accesses through the standard (instruction) page table. Under PTM=1 -
split I/D space, which is how SINTRAN runs background programs - the frame is DATA in
the ALTERNATIVE page table's view.**

Proven live: the background program's entry ENTR (virtual 0x2F3F, B=0xB5AF, demand=1)
reads SMAX from [B-175B]. Through dspace/APT that word is 0xB706 - no overflow, real
hardware proceeds. Through ispace/PT the same virtual address holds 0x5986 (a code
page) - the emulator computed 0xB536 > 0x5986 and took the error return (the only ENTR
in an 18845-instruction trace ring, error path trace-verified).

Causal chain, every link live-verified during this investigation:
spurious ENTR overflow -> SINTRAN prints "STACK OVERFLOW AT 002F41" (octal digit
printer at 0x9870-0x987C) -> session abort -> BLOGO logout (background command
processor) -> RELEASE-USER decrements user SYSTEM's enter count (no zero guard, by
design) -> user file block 61438 written to disk -> program restart -> ENTR fails
again -> forever, ~4 writes/second.

Fix: `StackFramePageTable` (PTM ? Alternative : Standard) for every frame access in
all four instructions; inline parameters remain on the standard table (instruction
stream). File: RetroCore `Emulated.HW\ND\CPU\ND100\Instructions.StackHandlingInstructions.cs`.
Compiles clean. Credit: Ronny's hunch "maybe its using STACK functions like ENTR and
LEAVE and I have bugs" - exactly right.

Verification checklist after rebuild+restart+SCSI boot: no "STACK OVERFLOW AT" spam on
the console; no repeating SC_WRITE_6 to block 61438; the stale enter counts drain
normally; the system reaches an idle prompt.

## 0. BREAKTHROUGH UPDATE (10:30, post-restart session with fixed DAP)

The decrement chain is now pinpointed instruction-exact (all VERIFIED live via a raw-DAP
physical write watchpoint - the MCP tool cannot arm data breakpoints, the server can;
scripts in the session scratchpad: dap_scan.py, dap_watch2.py):

- The live working copy of the SYSTEM user entry (the STAGING buffer) is at
  **physical 0x0670A (bank 0)**, virtual 0xF0A in the FILSYS context.
- **The decrementer is FILSYS release-user code at virtual 0x8B3D-0x8B44**
  (overlay page 1055xx octal, MRUSE/RLUSE territory; the page is context-swapped,
  disassemble only while stopped there):
  `LDA $15,X ; AAA -1 ; AND 0xFF -> D ; LDA $15,X ; AND 0xFF00 ; RADD SD DA ; STA $15,X`
  i.e. count := (word0 - 1) & 0xFF - **no zero guard at this level**; releasing at
  count 0 produces 0xFF by construction. The wrap-through-zero observed on disk
  (0x10 -> 00 -> FF -> FD) is therefore expected IF release keeps being called.
- The decrement runs only on the skip-return of a check call at 0x8B3B
  (args T=B.7, A=B.6, frame B=0xB51E); the write-back is queued via 0x8B46.
- The staging entry is copied to the DMA buffer (phys 3:F400) by the resident
  kernel's chunked MOVEW loop at 0x1D2-0x1F4 (PTM-flipping, 32-word chunks).
- Loop rate this boot: ~4 writes/second continuous.

REMAINING QUESTION (the true root cause): who calls release-user once per cycle
forever - i.e. which caller/guard keeps deciding "still entered, release again"
instead of stopping. Next: capture L and the B-frame at the release routine's ENTRY
and decode the 0x8B3B check's verdict source.

Frame capture at the decrement (VERIFIED live): B=0xB51E work frame; saved X (entry
ptr 0x0EF5) in frame; the check call at 0x8B3B receives A=B.6=0 and T=B.7=0 and
returns SKIP (= proceed to release) every cycle. The routine sits behind a JXN/JPC
dispatch table at 0x8B00-0x8B09 = a FILSYS operation executor; the release body
starts at 0x8B26 (COPY SL DA save-link preamble). The 0x8Bxx page is overlay-swapped;
all of this is only visible while stopped inside it.

## 0b. DYNAMIC ANALYSIS COMPLETE (13:40) - the loop's engine, proven

The FILE SYSTEM RT program (RT description FSYRT=043262) re-runs its release path on
every activation because its state flag says "work in progress" forever:

- Flag: frame offset B-0x43, virtual 0xB03D in FS context, **physical 0x60C3D** (bank 6).
  Observed values: 1 = idle/check request word; -1 = work in progress.
- Main loop (runtime 0x649B): flag==-1 -> re-run release WITHOUT any request; the loop
  itself re-asserts -1 (STA -$43,B at 0x64B7) every pass.
- **PROOF over 225 complete cycles (900 watched stops): the only writer of the flag is
  the loop's own -1; no instruction ever wrote a "done" value. The request word (phys
  0x9E6B) never received a command either (separate 900-stop watch).**
- Remaining root-cause work is STATIC: find SINTRAN's "done" writer for this flag and
  its precondition - handed to the carver in
  [QUESTION-FOR-CARVER-FS-RT-FLAG.md](QUESTION-FOR-CARVER-FS-RT-FLAG.md).
- Symbol hints (INFERRED, relocation unresolved): main loop near FILSYS CHNUS (d=21),
  elements near SEPUS/SEPFS, RT descriptions WSRTF/WARTF/DELRT/DSRTF/FSYRT/RRTEL.

Toolbox note for continuation: raw-DAP scripts (scratchpad: dap_scan.py signature
scanner, dap_watch2.py physical write watch with noise skip, dap_caller.py break at
0x8B3D + frame dump). The MCP debug tools CANNOT arm data breakpoints (client-side
symbol-name requirement) and instruction breakpoints ACCUMULATE server-side
(SetDapInstructionBreakpoints never clears - DAP contract violation, fix pending);
the raw scripts work around both.

Boot of the SCSI disk (tor-disk.img, SINTRAN L-VSX-500) on RetroCore loops forever
re-issuing the identical `SC_WRITE_6` to LBA 61438 (2 blocks). This document records the
live-DAP + carve findings of 2026-07-17. Root cause NOT yet final; every claim below is
tagged VERIFIED (live DAP / file bytes / carved bytes) or INFERRED.

Companion pseudo-code for the disk layer:
[SCSI-DISKLAYER-CONTROL-RECORD-PSEUDOCODE.md](SCSI-DISKLAYER-CONTROL-RECORD-PSEUDOCODE.md)

## 1. What the loop writes (VERIFIED, file bytes + ND-60.052 manual)

Blocks 61438-61439 hold the directory's **USER FILE**: 32-word (64-byte) USER ENTRIES
per the NORD File System manual (ND-60.052.04 section 3.1.3, found at
`\\Nas9t\data\NorskData\FileSystem\ND-60.052.04_NORD_File_System_April_1977_ocr.pdf`):

```
User entry word 0 = user-entry flag (bit 15) + ENTER COUNT
words 1-8  = user name (16 chars)     "SYSTEM'" / "RT'" ...
...          password, date created (2w), LAST DATE ENTERED (2w),
             pages reserved (2w), pages used (2w), user index, friend table
```

- **Word 0's low byte is the ENTER COUNT, and the loop DECREMENTS it by one per
  write**: pristine image = 0x8103 (SYSTEM entered 3 deep when the 2016 image was
  saved), observed 0x81DA then 0x81D9 across a single stepped write. It has been
  driven BELOW ZERO and wrapped (3 -> 0? -> 0xFF -> ... -> 0xD9). The exit test
  (presumably count == 0) never fired - or fired and was retriggered.
- The 4-byte change at entry offset +0x18 = the "last date entered" field being
  stamped - consistent with an enter/release cycle.
- The pristine image also has enter count 01 for user RT (file byte 0x3BFF841) and
  for a user in block 61439 (+0x2C1) where the local image now has 00: those two
  users' stale enters were "released" successfully (1 -> 0, one write each) - **the
  cleanup mechanism works for counts that reach 0 exactly; SYSTEM's count overshot**.
  Why SYSTEM's decrement chain skipped 0 (or restarted) is the open core question.
- Total damage after hours of looping: 380 bytes in 132MB (block 0: one byte at 0x3CF;
  sparse single low-bytes at 0x40 stride in blocks 50006/52102/54140-41 = object-entry
  fields; 8 bytes in 61438-39). The image is NOT being trashed.
- NOT the invariant: XOR of the 512 BE words of block 61438 is nonzero even on the
  pristine working image (0xF1); 16-bit additive sum also differs pristine vs local.
  Whatever FINEX's checksum covers, it is not the raw on-disk block alone.

## 2. The write succeeds at every level (VERIFIED, live DAP)

Traced one full cycle with breakpoints (plain addresses - see section 5 for the @PIL trap):

1. SCSID entry (runtime 0xAFA3, carved 062217): D=0xEFFE (LBA), X=0x16EC, B=0x1704,
   L=0xA99A - same call site every cycle.
2. NCR5386 completes cleanly (chip trace: bus-service -> func-complete -> disconnect).
3. Disk-layer status check (0xA99C, carved 057613) reads A=0: the 6/13
   (UNIT-ATTN/ABORTED) retry does NOT fire.
4. FINEX entry is 0xA9C0 (carved 057655). Its HEAD tests bit 6 of the flags word at
   [mem[datafield+4]=0x1C4C + 0xA] (live value 0x9204, bit 6 CLEAR) and takes an early
   exit to 0xA9FA = RCLR DT = T:=0 SUCCESS. The XOR-checksum verdict block
   (0xA9E2 NOCRC / 0xA9E4 OK, carved 057717/057721) NEVER RUNS for these writes.

So the reissue is driven from ABOVE the disk layer, against an operation that reports
success every time.

## 3. The re-driver: boot init overlay scan (VERIFIED location, INFERRED semantics)

The loop's top is in carved segment 065-S3SIPIT / 066-S3IIPIT (the init PIT overlay),
runtime load base 0x9426 this boot. Anchor VERIFIED byte-exact: fingerprint
`SHA ZIN SHR 6 / SAT 11 / SKP IF DA EQL ST` at segment word 02115 = runtime 0x9873.
(Two other fingerprint sites do not line up 1:1 - relocation of literal pools differs
between carve and live; re-anchor per block.)

Structure (carve 114146-114360, live 0x984x-0x98Fx):

- Per-unit blocks iterate descriptor pointers from a low-memory array (dspace
  0x0904-0x090B; two slots both = 0xD690). Descriptors live in PHYSICAL bank 2
  (bank number in dspace 0x08D0 = 2), accessed with LDATX - 8-word entries
  (helper computes (B - base) >> 3 as the index, carve 116003-116006).
- Each block reads a descriptor word, extracts an error field with A >> 6 and compares
  it to 9 = 11B. 11B is NOT a file-system error code (ND-60.128 table: "not used");
  it matches the SCSI disk layer's internal NOCRC return. INFERRED: the field stores
  the disk-layer verdict for that unit.
- The scan includes a WAIT (carve 114356) - it sleeps and rescans on interrupts.
- Live observation: the scan's registers are BIT-IDENTICAL every pass
  (A=B=0xD690, D=0xE400, T=9, X=0x79F7, L=0x985D) and ~5-6 scans pass per disk write.
  The state it polls never advances - the machine alternates write-burst and pure-spin
  phases forever.

Open question (THE question): which state word is supposed to advance after the
successful user-file write, and which code should advance it.

Additional live observation (07:0x): the disk-start routine at 0x38E3 has TWO callers -
the monitor trampoline (L=0x10D9, paging-switch gate at 0x10D8) and a DIRECT caller at
0xEB47 (L=0xEB48; its code page is unmapped in every inspectable page table, so it runs
under a PT only live in its own context). A breakpoint at the 0xEB48 return address
NEVER fired across two complete write cycles. INFERRED: the requester does not resume
after its disk call - it is re-run from the top (RT program restart), and each run
performs one more release (enter count -1). If true, the root cause is whatever makes
the requester terminate-and-restart instead of resuming - e.g. a missed wakeup/RTRES
condition after the transfer completes.

## 4. Emulator findings

- All ND-100 instructions in the build/FINEX/scan paths were validated against
  `nd100-markdown` docs + opcode arithmetic: RADD/COPY ADC/REXO/LDDTX/LDATX/MPY/MIN/
  SKP EQL/UEQ/LST all MATCH. Two divergences found, both provably OUTSIDE this path:
  SKP LSS is overflow-corrected in C# but raw-sign per spec; RADD with ADC+AD1 both
  set adds 0 in C# (spec sources disagree with each other).
- The earlier interrupt-ack fix (RITRG clears NCRIT, commit 568882fd9) is in and works.
- Prior session's "6/13 disk-layer retry is the mechanism" claim: REFUTED live.

## 5. Debugger traps found this session (both cost hours)

1. Instruction breakpoints with an `@11` PIL suffix NEVER FIRED for the disk-layer /
   init code. Plain addresses fire. Do not use @PIL filters until their match rule
   is verified.
2. **DAP `phys:` reads were silently truncated to 16 bits** (`& 0xFFFF` in
   `ND100Machine.cs` DapReadMemorySpace/DapWriteMemorySpace/DapDisassembleSpace):
   `phys:0x3F400` actually read 0xF400. Every bank>0 physical read of this session was
   wrong and has been discarded. FIXED 2026-07-17 (space-dependent mask, 0xFFFFFF for
   phys:), compiles clean; requires RetroCore rebuild+restart to take effect.
   dspace:/ispace: reads and all disassembly were correct throughout.

## 6. Next steps

1. After RetroCore restart with the DAP fix: verify `phys:0x3F400` shows the user-file
   block during a write (DMA MAR base VERIFIED from trace: data 0x3F400, CDB 0x45480).
2. Read the TRUE unit descriptors at phys bank 2 offset 0xD690; identify the word and
   field the scan polls (the `>>6 == 11B` source) and the flags word 0x9204's bit 6.
3. Find the ENTER-COUNT decrementer live: locate the in-core user-file buffer (fixed
   phys reads), break where word 0 is rewritten, and read the exit test around it.
   The failing exit condition of that release loop is the root cause. Candidate
   mechanisms to distinguish:
   - decrement loop "WHILE count <> 0" that somehow skipped 0 for SYSTEM (worked for
     the two count=1 users), e.g. a double-decrement or a wrong initial read;
   - enter/release cycle with net -1 per iteration driven by a higher-level retry;
   - emulator bug in whatever instruction performs the read-modify-write or the test
     (LBYT/SBYT audited CLEAN against spec; plain-word ops audited CLEAN earlier).
4. ND-60.052.04 (NORD File System) covers the on-disk layout; the SINTRAN III-era
   System Documentation (ND-60.122) may describe the boot-time release logic - check
   the NAS Documentation folder if needed.
