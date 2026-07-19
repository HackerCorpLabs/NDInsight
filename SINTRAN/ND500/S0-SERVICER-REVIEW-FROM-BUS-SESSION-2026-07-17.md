# S0 servicer extraction review - from the bus-interface session

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\S0-SERVICER-REVIEW-FROM-BUS-SESSION-2026-07-17.md`
**Reviewing:** RetroCore commit `82e83a148` (frozen contract + S0 extraction).
**Reviewer basis:** the live traces (3 runs), the 030-S3SM5/nd-500-mon carves, the B30
microcode pseudocode, and the swapper startup analysis (catalog sections 7c/7d).

## Verdict

**S0 extraction verified faithful.** Checked line-by-line against the pre-extraction engine
(f79937857): N5STA lifecycle + power-fail-bit preservation, MICFU cases (13B/14B 32-bit
addressing, 12B no-op, 22B accept, known-set accept, unknown->5ERANSWER), event ordering,
log formats ("@word" = msgBase>>1 == old mar&0xFFFFFF), `mar==0` bare-activate guard kept in
the host, MAR word->byte <<1 conversion at the host boundary, `DEBUG_DETAIL` hardcoded
before AND after (no change). `IServicerHost.Nd500AddressBase == SharedMemoryStart`,
`AnswerWritten` no-op on classic, completion stays with ExecuteND500Operation - all as
documented. Full ND-500 suite re-run in progress (architect reported 1751/0; will confirm).

## Findings (ranked)

### F1 - GENERATION POLICY HAZARD (design-level, hits the live oracle in S1)
`Nd500Generation` doc says semantics follow the LOADED MICROCODE, and the live machine runs
a **5800-revision image behind the classic 3022**. But on a true 5800 table, **MICFU 21B
(3WREG) and 20B (3RREG) are MSG_ILLEG** - and the live SINTRAN L LOAD-SWAPPER path REQUIRES
21B accepted (it is the current blocker; catalog 7c). If S1's generation table strictly
follows the loaded image, the live oracle regresses the moment anything switches the
servicer to Samson5800 (e.g. "detect image revision"). Recommendation: the effective
authority for accept/reject is the **SINTRAN SENDER**, not the image; keep the 3022 host's
servicer on Classic regardless of loaded image, and never auto-derive Generation from CS
content. The B30 "21B=ILLEG" belongs in the Samson5800 column only.

### F2 - CONTRACT GAP for the live blocker: 21B 3WREG needs a servicer->CPU deposit path
21B = REGISTER WRITE (process-0 initial register context incl. P; layout: first-reg@7,
count@10B, 32-bit ND-100 src addr@11B-12B; answer = status only - catalog 7c, Micro Program
Guide 13.13). Neither sink covers this direction (both are CPU->servicer). Needed: an
IND500Cpu extension (e.g. WriteRegisterBlock(firstReg, values)) or a servicer-side context
stash applied at CPU attach. Twin 20B 3RREG needs the reverse (register values -> ND-100
memory). Without 21B the live oracle cannot pass LOAD-SWAPPER, so this belongs in S1/M1,
not later. (Register-image width in ND-100 memory - 2 words/reg hi-first - is INFERRED;
next live run should log offsets 10B-12B.)

### F3 - CONTRACT GAP for S1's 3RMICV: no version source on IServicerHost
The 3RMICV two-halfword answer (version + CPU param, landing at HW 7 + 10B) must source the
version FROM THE LOADED CS IMAGE (acceptance oracle: classic images self-report 10509/10609
via READ MICRO PROGRAM VERSION; the 5800 image = 027232B). csStore lives in the 3022 host;
the servicer has no way to read it. Suggest `ushort MicrocodeVersion { get; }` (or a
version+cpuParam pair) on IServicerHost, host-updated at CS load/restart. Note the classic
144-bit image's version LOCATION inside the image is still UNKNOWN - extract empirically
when Bo Goran's floppy arrives (search 10509=0x290D / 10609=0x2971).

### F4 - DOC BUG in Nd500MicrocodeServicer.MessageProcessed XML comment
Comment says the event fires "after the answer status is written"; the code fires it BEFORE
(invoke precedes the N5STA write). The CODE order is the correct one - result data must be
in place before N5STA:=3 releases SINTRAN (matters for M1's answer payloads). Fix the
comment, keep the code.

### F5 - Threading note (not an S0 defect)
The classic host still calls servicer.ProcessMessage synchronously on the ND-100 IOX thread
(ExecuteND500Operation). Fine under the contract's inline mode; when the CPU thread arrives,
the classic activate must be enqueued through the SPSC ring too, or the servicer runs on two
threads. Flagging ExecuteND500Operation as the spot that changes.

### F6 - In-flight work / naming migration: clean
Nothing in the bus session assumed the nested enum location or the private engine body -
everything was committed at f79937857 and the three migrated test files compile against the
new names. Bus-session docs (skill + NDInsight) referenced "the engine in NDBusND500IF" -
being updated to point at the Servicer namespace (doc-only, no code).

## Live-oracle status
Pending a fresh EXE build + `status` run on the live machine (needs Ronny). Expected
decoded-trace equivalence: identical MAILBOX RECV/ANSW lines incl. the 21B 5ERANSWER (still
unimplemented by design in S0). Will report after the run.
