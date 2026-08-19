# CARVE ANSWER - N5SWAP fn 5 (MSWIN, "swap-in") driven into the real swapper (2026-08-17)

Milestone 14 of the CpuND500UC microword engine: fn 5 - the OTHER
gate-bypass arm of the swapper's 29-way dispatch - issued as a real work
order and executed DEEP into its worker on the real CONT-STORE-10611
microcode, plus the instruction batch this took (ADD3/SUB3/MUL3, MULAD,
F/D DIV3, LOOPI, SHL, W GETBI, SFILL, BMOVE, l=:, ced=:, tutti) and the
resolution of the CPGU/CWIP exit anomaly (TICK-MODEL item 78 -> item 83).

Evidence tags: **PROVEN** = read from the cited bytes/listing AND (where
stated) executed on the microword engine. **INFERRED** = deduced, not
fact. **OPEN** = unresolved.

Sources:
- Microcode listing: `E:\Dev\Repos\Ronny\ND110Compile\ND110Compile\uCode\CONT-STORE-10611.LISTING.TXT`
- Swapper disassembly: `SINTRAN/ND500/swapper/swapper-k01-pseg.asm` (+ raw `.PSEG`/`.DSEG`;
  NOTE: the disassembler DESYNCS after every `loopi` - it swallows the
  displacement byte into a long literal; the boundaries in this doc were
  re-verified from raw PSEG bytes where it matters, e.g. 1000077663)
- String-dispatch carve: `STRING-DISPATCH-BLOCK-CARVE-RELAY-2026-08-17.md`
- Protocol context: `CARVE-ANSWER-N5SWAP-FUNCTION-VOCABULARY-2026-08-17.md`,
  `CARVE-ANSWER-N5SWAP-FN3-SLOT-TABLE-INIT-2026-08-17.md`,
  `N5SWAP-SWMSG-FIELD-DOSSIER-RELAY-2026-08-17.md` (fn 5 = MSWIN, L07:5680)
- Execution proof: `SwapperN5Swap_Fn22B_SetConfig_ThroughJumpgTable` (the
  four-function drive) + the new oracles in
  `RetroCore/Nuget/HackerCorpLabs.Emulation.CPU.ND500UC/tests/`,
  191/191 green 2026-08-17. Engine model: package `docs/TICK-MODEL.md`
  milestone-14 section (INFERRED items 83-88).

---

## 1. The CPGU/CWIP exit anomaly RESOLVED (PROVEN words, INFERRED semantics - item 83)

The listing keeps TWIN dispatch terminators apart on bit 142 (SPAREBIT):

- 002325B `SPAREBIT JMPMAP PRF,EOP` - the OUT-OF-LINE exit. Users:
  the page-op exit 010613B (`A,P D,DP PRF,CLEAR` -> 002327B `PRF,START`
  at DP = P), the float exits 002253B/002255B/002270B/002272B, the
  trap-path twin 002664B-002666B, and LOOPI's not-taken arm (002323B
  POPRET -> 002255B). The re-decode presents an address INSIDE the
  just-completed instruction; the EOP steps OVER it to the completed
  instruction's byte end.
- 002221B `JMPMAP PRF,EOP` (NO sparebit) - the in-place dispatcher
  (RET/ifkret 002546B; the 0-operand system ops compute DP := P + 2
  themselves, 007530B).

This RETIRES the milestone-12 "PRF,CLEAR resyncs P" model (the two are
mutually exclusive on ZPGU, and only the SPAREBIT one lets operand-less
CPGU/CWIP - and the F/D float exits and LOOPI - terminate). Pinned by
execution: CPGU;CWIP back to back complete with P advancing, and the
BRANCH_loopi golden file went 30/0.

## 2. The fn-5 loop-head bypass and SWMSG intake (PROVEN, executed)

- 1000101504 `w comp2 [0x240B8],$5; if = go $25` - fn 5 skips ONLY the
  slot-table gate compare (like fn 3's arm at 1000101515) and reaches the
  29-way jumpg. Executed: fn echo [0x240B8] = 5, stub 1000101554, worker
  1000077534.
- RIOM count for fn 5 = **70 halfwords** - exactly enough to include the
  message's TRAILING POINTER (hw 68-69, section 4).

## 3. The worker 1000077534's first-time path (PROVEN, executed)

- 1000077551 `w test [0o246374]; if = go $104`: the init-done flag
  [0x14CFC] ships 0 in the DSEG -> block A (1000077561-1000077662: the
  re-init flush with hconv/47365 calls and a loopi'd release loop ending
  in `ret`) is SKIPPED; the first-time spine resumes at 1000077663.
- 1000077663 `w bmove [0x08030F8C],[0x0801285C],$3` + 1000077700
  `w move` - config copies (raw-byte verified; the disassembler is
  desynced here).
- Sub 1000110007 (whole, executed): `w sub3 r.10,r.4,b.34` (the
  176156B byte anchor), `w2 mulad $4,r.0` (251B), a `w4 sfill` whose
  descriptor spans the STATS BLOCK - cells [0o461114]..[0o507454] - so
  the fn-5 init ZEROES the per-code stats INCLUDING the total-message
  counter [0x26280], then three 3-word `w bmove`s.
- The jumpg helper 1000077515: `entd; l=: b.100; ced=: [0o246454];
  clrk; jumpg b.100` - executed; `ced=:` stores the CURRENT EXECUTING
  DOMAIN = the AM#15 main-domain register (0 here; the pairing is
  CONFIRMED by 76/76 MOVE_ced=_ golden vectors in the engine sweep).
- Sub 1000065427 + callee 1000003444: stamps a descriptor into **data
  segment 15B** (VA 0x68000000+; `by rladdr $15000000000+` with slot =
  [0o246454-family]*0x400; putbf/putbi at r3.(0)). Executed against a
  zero-mapped segment (staging, section 6).
- The slot-table RESET: the fn-3 gate cell [0x128E4] goes 0x40 -> 0
  (executed observable) - fn 5 re-initializes the slot table fn 3 built.
- Sub 1000103415 (the DESCRIPTOR-BLOCK UNPACK, executed): takes the
  SWMSG hw-68/69 word = a **PHYSICAL address** (the spine reads it via
  `r := b.24; w1 := r.210` = intake+136) and, under dmof (data mapping
  OFF - AL#17 bit 23B clear via 010547B, so the dereferences are
  PHYSICAL), unpacks: [0o461020] := h[block+2] (walk bound),
  [0o461024] := block+36 (list-head cell), [0o461030] := block+40,
  [0o461034] := block+0.
- Sub 1000077433: reads the OPTIONS word (SWMSG hw 8) via [W1+0x10],
  `h shl r2,$72`, and with options = 0 SKIPS its 1000003057 announce
  (executed).
- Sub 1000066175: derives the mode flags [0o436530..0o436570] from the
  options word (`by2 := r1.(21) and $7`, `w getbi r1.(20),$23/$25`),
  then calls 1000107011 (executed).
- Sub 1000107011 (the MAILBOX LIST WALK, executed): under dmof, walks
  the node list at [[block+36]] - per node: done-flag h[node+8]
  (compared to 1), next = [node+4] - bounded by [0o461020]. Its TWO
  1000003057 announce sites are ERROR arms (codes 0o200 = bound
  exceeded, 0o201 = no node seen), both SKIPPED with a healthy
  one-node list.
- The 255-entry TABLE-B stamp loop (executed): 1000077731-1000077760
  `h set1 b.30; h wconv; w1*$400; by rladdr $4000004400+;
  h2 =: r.330; loopi b.30,$376` - stamps a halfword into every entry
  of data segment 4 (base 0x900, stride 0x400).

## 4. The fn-5 SWMSG (carved + executed geometry)

| halfword | consumer | meaning |
|---|---|---|
| 8 (+0o20) | 1000077433 / 1000066175 | OPTIONS word (low 3 bits + getbi bits 0o23/0o25 -> mode flags; 0 = the minimal path, no MON arms) |
| 51 (+0o146) | spine 1000100126 (AFTER the deferred wall) | segment count -> [0o224124] |
| 53/54/57 | shared copier 1000064476 (record base intake+0o150; AFTER the wall) | the three config cells [0x14D1C]/[0x14D20]/[0x1284C] |
| 68-69 (+0o210) | spine -> 1000103415 | **PHYSICAL address of the descriptor block** (bound at +2, mailbox list head cell at +36) - why the RIOM count is exactly 70 |

## 5. The 1000003057 announce = SWPFATAL, not "sub-fn 2" (PROVEN bytes - CORRECTION)

The generic announce worker 1000003057's MON 377B (at 1000003167, argc
2) carries arg-1 = the CELL [0o225040] = [0x12A20], which **ships
0o2047 = SWPFA (SWPFATAL)** in the DSEG - the deep-analysis dossier's
own site table lists it as the 0o2047 argc-2 site. The
`swapper-k01-handlers.md` reading "MON 377B sub-function 2" is WRONG;
the hot flags [0o436554]/[0o436560] gate the CACHE-FLUSH block
(1000003123-1000003163), not the MON. The announce loop (1000003207
`ifkret`, 1000003210 `go -0o21`) re-issues until K = 1, and ifkret
PROPAGATES K up every caller (`call X; ifkret` chains) - so a K = 1
reply unwinds the worker into 1000101241 (which writes W1 into the fn
cell [0x240B0], clrk, returns to the stub). The codes the fn-5 error
arms pass (0o110, 0o200, 0o201, 0o66 for the older handlers) are
ERROR/EVENT numbers in PAR2. **OPEN**: the ND-100 side of this protocol
(what SINTRAN does with SWPFU = 0o2047 beyond ESWPFATAL/XRSTARTALL, and
whether any reply flavor lets the announce loop terminate NON-fatally).

## 6. Harness staging (INFERRED - TICK-MODEL item 87) and the deferred wall

Staged (all documented in `SwapperAttemptTests`):
- Shared data segments as writable zeros: segment 4 (Table B, 132
  pages), segment 5 (offset window 0x10000-0x20000 - sub 1000066411's
  `$5000204000+` putbf stamps), segment 15B (96 pages).
- The SWMSG hw-68/69 descriptor block + ONE healthy done-flagged
  mailbox node in physical 0xF8000 (bound 8, head -> node, node.flag=1,
  node.next=0).
- [0o224014] = [0x1280C] (a slot pointer 1000066411 page-numbers; ships
  the stale live value 0x080B8000) -> the item-82 zero scratch.

THE WALL (deferred): past the mailbox walk, sub 1000071273's
per-segment Table-A rebuild dives into SINTRAN-maintained descriptor
state (frame chain: spine -> 1000071273 -> 67xxx -> the 2121-region
paging sub -> a fatal announce whose own argument pointers are null) -
page fault 46B during the announce's argument walk. Reaching cwip
(1000100120), the copier re-run and the final `set1 [0x14CFC]` needs
the uncarved SINTRAN Table-A/descriptor init. The attempt pins (test
asserts): dispatch through the bypass arm, the execution footprint
(SUB3/MULAD/SFILL/BMOVE/LOOPI>=254/SHL/GETBI entries), counter zeroed,
gate reset, the unpack cells, and the recorded trap park (N5STA=3,
STOPR=2, TRAPN=46B) with a clean return to IDLE.

## 7. What this closes and what stays open

Closed (PROVEN, executed): the fn-5 bypass arm; the worker's first-time
spine through nine subsystems; the SWMSG hw-8/68-69 geometry; the
descriptor-block unpack and mailbox-walk protocol shapes; the
1000003057 = SWPFATAL correction; the SPAREBIT exit (item 83); the
instruction batch (section 3 families) with oracles.

Open: the Table-A/descriptor SINTRAN init (blocks fn-5 completion); the
ND-100 reply protocol for the SWPFATAL announces; SWMSG hw 51/53/54/57
semantics past the wall (carved statically, unexecuted in fn 5); D
MULAD / F-D LOOPI / LOOPD / SHA / SOLO entries (unmapped, unexecuted).

---

# ADDENDUM 2026-08-17 (milestone 15): the ND-100 reply protocol RESOLVED

The section-5 OPEN ("what SINTRAN does with SWPFU = 0o2047, and whether
any reply flavor lets the announce loop terminate non-fatally") is
CLOSED, carved from the NPL nucleus (MP-P2-N500.NPL, CC-P2-N500.NPL,
XC-P2-N500.NPL, RP-P2-N500.NPL) plus the 24B microcode path:

- **KFLIP bit 0 is the ONLY source of the resumed process's K flag**
  (PROVEN both sides: the 3MONCO resume microwords 010212-010306 mask
  S1 bit 8 out and OR it back from KFLIP bit 0; OKMONICO sets T:=0,
  EMONICO sets T:=1 before the shared MONICO tail stores KFLIP :=
  T, CC-P2-N500.NPL:359-366).
- **Legal announces get replied**: new work via 5ACTSWAPPER (KFLIP=0,
  NUMPA=6 - writes the fn code into [0x240B0] and the requester's
  physical address into [0x240B4], MP:2884); transfer completions via
  OKMONICO (KFLIP=0) or EMONICO with code 0o1055 SWDERR (KFLIP=1 -> K=1
  -> the swapper's ifkret chain unwinds = how it learns a disc transfer
  failed).
- **A SWPFA (0o2047) announce gets NO reply at all** (PROVEN,
  MP:1192-1193): SWPFU > SWFMAX(6) -> ESWPFATAL -> XRSTARTALL, which
  errors every queued ND-500 message, restarts the ND-100 owner RT
  programs, sets FERROR := 0o2047 (N500TMR then stops watchdog
  servicing, RP:127653) and never re-activates the swapper process.
  The ND-500 side stays parked at the MON 377B stop forever.
- **Harness rule**: a mid-worker SWPFA announce is a terminal park -
  never auto-resume. (A KFLIP=1 resume exercises the unwind
  mechanically but is NOT SINTRAN behavior for SWPFA; KFLIP=0 makes
  the announce loop re-issue forever.)

# ADDENDUM 2026-08-17 (milestone 15): fn 5 driven TO COMPLETION

The section-6 wall is CLOSED. The completion carve (spine 1000077767 ->
1000100644, sub 1000071273's full body, the 1422/1755/1165 PTE
machinery, 1000066411's two-level table constructor, 70054/75410/76133/
65112/65622/72767) lives in
`FN5-COMPLETION-STAGING-CARVE-RELAY-2026-08-17.md` (same folder), and
the executable staging contract is in the engine harness
(`RetroCore/Nuget/HackerCorpLabs.Emulation.CPU.ND500UC/tests/
SwapperAttemptTests.cs`, the fn == 5 branch) with TICK-MODEL item 89.
Key corrections to THIS doc from execution:

- The per-segment rebuild loop bound of 1000071273 is **SWMSG hw 44**
  (intake+0o130), NOT hw 51; hw 51 is stored to [0x12854] by the spine
  AFTER cwip. A ZERO hw 44 is not a live shape: [0x128D4] := hw44 - 1
  goes 0xFFFF and 1000076133's scan compare (`w comp2 b.110,r1;
  if >> go`) is UNSIGNED - the microcode loops 65535 times and walks
  off the DSEG.
- The milestone-14 "trap 46B in the announce argument walk" reading is
  refined: the wall was 1000066411's very first descriptor write
  (1755 state-0/page!=0 -> announce 0o20 at 1000002121) with the
  announce faulting on unmapped DSEG pages; with real slot
  descriptors staged the announce never fires.
- There is **NO completion MON on the success path**: the worker ends
  `set1 [0x14CFC]; ret` and the stub returns the swapper to its
  main-loop LNEWSWAP announce.

## Completion post-script (same day, after the run went green)

The completion is EXECUTED: 195/195 package tests green with fn 5
running `set1 [0x14CFC]; ret` and returning to the main-loop announce.
Execution corrected three more static readings (details in the relay
doc and the engine's TICK-MODEL milestone-15 section):

- The record TYPE-REMAP table at DSEG 0x24014 rewrites every raw type
  to 0 or 3 - the 4..7 per-code arm of 71273's loop #2 is UNREACHABLE
  with the shipped table. Raw type 4 (-> 3) is the arm whose tail
  writes the [0o224370] free-chain head; raw type 0 routes the head
  elsewhere.
- The 71273 record tail BUILDS the free chain out of seg-6 entry
  indexes 1..count-1 and links its last entry to a frame-LOCAL zero,
  ORPHANING anything pre-staged at [0o224370] - the free chain IS the
  swap-in record's own page set. 72767's phase-2 bound b.64 divides
  the Table-A HEADER capacity word ([0o224014]+8), not a seg-6 cell.
- Three ENGINE defects had to fall before the heap survived the four
  76133 passes: the SHC register is 6-bit two's complement (else
  `shl $66/$72` and `sha $76` read +54/+58/+62 and the 1422/1755/1165
  walk collapses every directory index to 0), EX,SHA (EXFUNC 10B)
  existed only half-wired, and EX,SHR rotates LEFT for positive
  counts. The swapper's own shipped code was right all along.
