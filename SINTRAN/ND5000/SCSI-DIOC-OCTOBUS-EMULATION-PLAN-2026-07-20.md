# SCSI DIOC on the Octobus - Emulation Plan (2026-07-20)

> STATUS UPDATE 2026-07-23: S0 (carves), S1 (station), S3 (BDIO record + engine)
> are BUILT + verified in RetroCore, plus the S4-2 record-scanner tool and an
> S2 NucleusClient scaffold and machine wiring (AttachScsiDioc). The address
> model is fully carved (DMYAD window-relative, DSTBL = 2KB-page verbatim).
> Remaining tail (S2 NUCLEUS record auto-discovery, S4 live mount, Compare
> status) is blocked on the live machine. Full state + how to close the tail:
> **[SCSI-DIOC-RETROCORE-IMPLEMENTATION-HANDOFF-2026-07-23.md](SCSI-DIOC-RETROCORE-IMPLEMENTATION-HANDOFF-2026-07-23.md)**.

Goal: emulate a DOMINO SCSI controller (SCSI DIOC, module type 21B) as an
octobus station in RetroCore so that SINTRAN L07 performs REAL BDIO disk
I/O (read / write / compare) against the existing RetroCore SCSIHDD disk
emulation, coexisting on the same fabric with the ND-5000 CPU station(s).

Decisions recorded (Ronny, 2026-07-20):
- SEQUENCING = CARVE-FIRST: the two blocking carves (CONKI kick number,
  DOMDF initializer) run BEFORE the completion-path code; station bring-up
  code proceeds in parallel.
- END GOAL = FULL DISK I/O: a mountable DOMINO disk under the boot
  harness, not just discovery level.

Evidence marking as in the rest of this folder: [V] byte/live/manual
verified, [NPL-V] NPL source (different revision - logic only), [I]
inference, [ASSUMPTION] explicitly assumed, [OPEN] open.

Parent documents:
- Master architecture + phases:
  [OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md](OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md)
  (this plan EXPANDS its phase D for the SCSI case; phase-D task IDs D2,
  D3, D7, D8, D13 are referenced below).
- Review findings cited as DEV-n / SIN-Fn / HW-An:
  [OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md](OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md).

---

## 1. Ground truth to REUSE - already carved, do NOT re-derive

Everything in this table exists, is byte-verified, and is the input to
this plan. Carve docs live in the carver tree:
`tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\`
(repo-relative; folder README is the index).

| Fact | Where carved |
|---|---|
| BDIO request records: fn 166B read (size 74B) / 167B write (70B) / 213B compare (70B), built in DOMDF=041064 (body at DSVER=041104B), sent NKWRI 043411 / NKSEN 042171 [V] | BDIO-DOMINO-DRIVER-CARVE.md section 2 |
| REBDIO completion READ contract: DSSTS (0,0)=OK; statuses 104031/104651/104622 -> HSTAT -5; DSQCN==(-1,-1)+DXPOO==PDF.DIPOO -> mirror-pool timer; else HSTAT -4 + SINEC 1662 [V] | BDIO-DOMINO-DRIVER-CARVE.md section 4 |
| DCNVA: DOMINO byte addr = ((word_addr - (ADRZERO<<10dec)) << 1) OR bit31; range-check error -3 "outside multiport"; self-modifying one-shot bias cache [V] | BDIO-DOMINO-DRIVER-CARVE.md section 5 |
| HSTAT error ladder -1/-2/-3/-4/-5/0 incl. -2 SINEC 1661, -4 SINEC 1662 + BDTMU retry [V] | BDIO-DOMINO-DRIVER-CARVE.md |
| NUCLEUS kernel byte layout: master block (+2/+7/+20/+25/+74/76), 40B-word descriptors (port +10/+12/+14/+16/+20/+21/+22/+30; message +10/+12/+14/+21; buffer +23/+25/+26), port +20 = KICK DEST octobus station, kick table 14B-word entries KHEAD/KTAIL/KLOCK, TSET lock value 070000B [V] | NUCLEUS-PRIMITIVES-CARVE.md section 4 |
| NKSEND -> SKICK path emits kick number 1 (NUCKI), only when the queue WAS empty [V]; MON 347B = NUCLEUS SERVE 047072 (MCTAB) [V] | NUCLEUS-PRIMITIVES-CARVE.md sections 5.2, 7 |
| DKICK 044747 receive drain; registered via CONKI(A=14B, T=1) - CONKI body UNCARVED, so the remote->host kick number is [OPEN] | NUCLEUS-PRIMITIVES-CARVE.md sections 5.3, 5.8 |
| Disk-length rule: sector = DSTBL double; length = DNRPG pages x 2000B words (5MTRANS Domino path) [V] | OCTOBUS-DRIVER-ROUTINES-CARVE.md section 11.1 |
| SINTRAN sends NOTHING to stations 10B-13B at bring-up; MFPREPARE goes only to 2..6; emergencies only to SAMSON stations [V/NPL-V] | SIN-F8; SINTRAN-OCTOBUS-MESSAGE-CATALOG.md sections 6.1-6.3 |
| Overlay for the whole driver/NUCLEUS region: 017-S3SMPIT = 026-S3IMPIT (byte-identical), load base 032000B [V] | domino-nucleus-io README |
| NUCLEUS server segments 104/105-NKSE + 106/107-NKNA recon (carved .bin, interiors not disassembled) | NUCLEUS-SEGMENTS-RECON.md |
| OMD-0 test protocol responder byte contract (module type 1 = Domino) [V] | OCTOBUS-TEST-PROTOCOL-RE.md |
| SCSI status-code vocabulary 104601B-104677B, 105301B-105377B (105314B heartbeat loss, 105312B/105313B init status); PROMAN observable contract (reset states, SetBxP NAK, ERS 105042B-105045B) [V manuals] | ND-814009; ND-820026.1 (see master plan sections 4.4, HW-A14/A18) |

Poisoned priors already killed - do not resurrect: "DOMINO is all
userland" (in-kernel BDIO exists); "kick 5 on the NUCLEUS path" (host
sends kick 1; remote->host [OPEN], SIN-F1); the 22B-StartProcessZero
swapper hypothesis (22B = watchdog).

---

## 2. Architecture in RetroCore

New components (names per the master plan section 7; the phase A
extraction provides the base types):

```
OctobusFabric
  +-- NDBusOctobus (ND-100 card, station 1)          [exists]
  +-- OctobusND5000Station (SAMSON CPU, 70B+)        [exists]
  +-- ScsiDiocStation (NEW, default station 13B, module 21B)
        +-- IStationIdentity        (module 21B "SCSI", OMD-0 responder)
        +-- OmdDispatcher           (OMD-0 endpoint; admission = default)
        +-- NucleusClient (NEW)     (NUCLEUS kernel structures over
        |                            ISharedMemoryWindow; DIOC-side port,
        |                            message pop/push, INQUEUE, TSET via
        |                            the SHARED Interlocked semaphore
        |                            primitive - DEV-3/E1)
        +-- BdioEngine (NEW)        (fn 166B/167B/213B decode ->
        |                            SCSIHDD sector I/O -> DSSTS
        |                            write-back per REBDIO contract)
        +-- DiocWatchdog (NEW,late) (heartbeat / 105314B model)
```

Data flow (the two-bus model, master plan section 1):
1. SINTRAN STRBDIO builds the fn record in DOMDF, NKWRI/NKSEN queues it
   to the DIOC's port in shared MPM, SKICK sends kick 1 to the DIOC
   station when the queue was empty [V].
2. ScsiDiocStation.OnKick -> NucleusClient locks its port (shared
   semaphore), pops the message, reads buffer +26.. as the BDIO record
   (DSVER-relative layout [V]).
3. BdioEngine decodes fn/sector/length/memory address; DMYAD bit31
   interpretation mirrored from ND-500 Port-B [ASSUMPTION - no DIOC-side
   verification exists, SIN-F5g]; transfers pages against SCSIHDD
   (REUSE the existing SCSI disk emulation - do NOT reinvent the SCSI
   bus layer).
4. Completion: write DSSTS exactly per REBDIO's read contract [V],
   append the message to HOMEPORT queue, set INQUEUE, kick station 1
   with the CONKI-carved kick number (S0-1 below).
5. SINTRAN DKICK drains, REBDIO decodes, WT12 wakes the waiting level-12
   caller.

NOT emulated (out of scope, recorded so nobody re-assumes): the 68020,
DOMINOS, PMA-SCSI-BDIO firmware bytes, the PROMAN boot BYTE protocol
(stub honors only the documented observable contract - master plan D12),
MFbus crate configuration (master plan D11, separate task), SCSI tape
(ND-814009: MAGTP unusable toward DOMINO SCSI tape [V]; tape = later).

---

## 3. Phase S0 - the carves (BLOCKING, golden-path discipline)

All carve tasks follow the sintran-carving skill: carved bytes are ground
truth; NPL is logic-only; every published offset dd-reproduced; overlay
chosen by sibling-symbol coherence (the driver/NUCLEUS region is proven
017-S3SMPIT = 026-S3IMPIT base 032000B - revalidate coherence if a carve
leads OUTSIDE that region); every claim tagged [V]/[I]/[OPEN] with its
symbol table named; 317B-ExecuteCommand is the dispatch-model exemplar.
Output location: new files in
`tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\`
(extend the existing carve folder; add to its README index). Status docs
(`SINTRAN\CARVING-HANDOFF.md` section 1.9 and
`SINTRAN\ND500\ND500-STATUS-AND-INDEX.md` section 0e) are updated IN THE
SAME change - via STATUS-PATCH-*.md files if agents run in parallel, as
in the 2026-07-19 round. Segments are ALREADY carved - these tasks are
disassembly/analysis of existing .bin files, no pipeline run needed.

- [x] S0-1 [carve, BLOCKING] DONE 2026-07-20 [V]. ANSWER: **incoming
      octobus KICK 1 dispatches to DKICK @044747** - NKINI calls
      CONKI(T=1 kick number, A=14B = PIL level 12 dispatch code, X=0
      ring, B=125144 datafield); receive chain 035555 -> 036047
      KICKENT[frame & 17B] -> code-14B arm 036233 fires level 12 with
      P := mem[125143] = DKICK. Send and receive sides now match end to
      end (both kick 1). Receiver masks with 17B (kicks 20B-37B alias).
      Doc: CONKI-KICKENT-CARVE.md + a-conki-040765.txt. The S2-2 fence
      can be replaced by the constant 1 with the [V] reference.
- [x] S0-2 [carve, BLOCKING] DONE 2026-07-20 [V]. ANSWER: the
      initializer is the FILSYS DOMINO pool/port module in 006-S3FS
      (base 026000B). QUINI @134206 (lazy) creates the local port via
      MON 347 fn 1 -> DOMDF.DLPRT, writes DSVER:=1, DOMDF+21:=30B, and
      creates per-queue-element NUCLEUS messages (DMSID at elem+13).
      PDF.DRPRT = result of DOPPR @136352 = MON 347 fn 3
      open-port-by-NAME (GPOOL/RGPOO/RCPOO paths). DISPROVEN: the
      "DSVER+32..67 static header" carries NO config (generated zero
      tail + ADOML + NKMBU start; don't-care for the DIOC). UNIT
      BINDING = the per-pool NAMED port (DRPRT) + DXPOO/OPAIN message
      fields, resolved DIOC-side. Poisoned prior killed: BDTMU/BDTMV
      bodies are in RPIT (016-S3SRPIT), not MPIT. Remaining gap moved
      to S0-3: exact MON 347 request/answer layouts + connect-answer
      fields (DIPOO/OPAIX/ARESZ). Doc: DOMDF-INITIALIZER-CARVE.md.
- [x] S0-3 [carve] DONE 2026-07-20 [V]. Segment 105 is a
      PLANC-compiled server (runtime lib 112xxx, frame stubs
      112541/112576/112570). doNuc dispatcher @037033: fns 1..14B,
      fn->worker table dd-verified. fn 10B @047432 = descriptor
      CREATE/provision - writes port +20 KICKDEST (remote station) and
      +30 OWNID (port-number block); 7-item coherence check vs the
      kernel layout PASSED. ACONV 056332 (ID = number<<6 + base),
      walker 057631, allocators 063371/063464, NCALL wrappers
      072263/073207/073266 (request skeleton confirmed). Remaining
      [OPEN] (named in the doc): DRPRT/DLPRT sub-offset pin
      (SYMBOL-2-LIST pinning or S4-2 live capture), freelist head
      (runtime global), full NCALL per-word map (live round-trip),
      fn 11B-14B bodies. Doc: NKSE-SERVER-INTERIOR-CARVE.md + 6
      a-nkse-105-*.txt listings.
- [x] S0-4 [carve] DONE 2026-07-20 [V]. ANSWER: **SAFE** - PROMAN never
      runs at boot on this image (live @LIST-RT-PROGRAMS: PROMAN 14615B
      PASSIVE P-REG=0B; pack has no PMA-CONFIG / PMA-* images; segments
      120/121 string-proven = PROMAN, 124/125 = BOPCOM). Station 13B in
      the default harness config receives NO boot-protocol traffic -
      S1-2's zero-unsolicited-frames assertion stands as written. Flips
      only if PROMAN is deliberately started + crate
      interrogation/PMA-CONFIG + PMA images installed. NEW [OPEN]: a
      command-processor server-start table (003-S3CP @0xbb60, pairs
      NKSERV,2/NKNAME,0/PROMAN,2/EVMESG,0/BOPCOM,2/MTSERV,2) - consumer
      and gate unknown, did not fire this boot. Doc:
      PROMAN-AUTORUN-RECON.md.
- [ ] S0-5 [doc] Fold each carve result into the status docs in the same
      change (skill rule): new [V] facts -> CARVING-HANDOFF 1.9 +
      ND500-STATUS-AND-INDEX 0e; disproven claims deleted and recorded
      as poisoned priors; domino-nucleus-io README file table extended.

Deferrable, recorded (SIN-F6): ENKIC overlay (not on the STRBDIO path),
OMBREAD counters (OMD multibyte only - BDIO does not use it), WT12/NFUNC
wake internals (SINTRAN executes them itself).

## 4. Phase S1 - station bring-up (parallel with S0)

Depends on master-plan phase A objects (OctobusDeviceStation,
OctobusTestProtocolEndpoint, IStationIdentity). If phase A has not run
yet, S1 may start on OctobusSimpleStation and migrate - but do NOT
duplicate the OMD-0 responder.

- [ ] S1-1 [code] ScsiDiocStation skeleton: OctobusDeviceStation with
      IStationIdentity { ModuleTypeCode = 1 (Domino), module number 21B,
      DominoProcessorType/OpcomVersion placeholders (master plan Q6) },
      default station 13B (non-overlapping with ETH3 12B, SIN-F10),
      registered via machine config ONLY (never the card constructor -
      DEV-1/B4). Gate: dotnet build clean + TPE OCTOBUS tests 4/5/6
      answer with module 21B.
- [ ] S1-2 [test] Boot-harness assertion: with station 13B configured,
      SINTRAN bring-up sends ZERO unsolicited frames to it and ND-5000
      bring-up is unchanged (SIN-F8; C6d). If S0-4 finds PROMAN traffic,
      this test's expectation changes - update BOTH together.
- [ ] S1-3 [code] Default silence: the station answers OMD-0 test
      protocol only; no OMD-4 responses ever (it is not an MF
      controller); kicks are accepted (queued to NucleusClient once S2
      lands) but never NAKed.

## 5. Phase S2 - NUCLEUS client

Depends on: ISharedMemoryWindow (A10) + the SHARED Interlocked semaphore
primitive (E1/DEV-3 - one object per MPM window; do NOT build a private
lock). Layouts implement ONLY the byte-verified facts from
NUCLEUS-PRIMITIVES-CARVE.md section 4; every behavior mirrored from the
host carve by symmetry gets an [I-symmetry] code comment (SIN-F5h).

- [ ] S2-1 [code] NucleusClient: read master block, locate own port
      descriptor (40B words), TSET lock (070000B) via the shared
      primitive, pop MESS HEAD honoring INQUEUE, resolve buffer +26..
      record pointer. Gate: unit test against a synthetic NUCLEUS area
      built from the carved layout.
- [ ] S2-2 [code] Reply path: append message to HOMEPORT queue, set
      INQUEUE, emit KICK 1 to station 1 (S0-1 result [V],
      CONKI-KICKENT-CARVE.md - incoming kick 1 fires level 12 -> DKICK;
      cite the carve doc in the code comment). Gate: unit test verifies
      queue words + kick frame.
- [ ] S2-3 [code] Debug oracle: NucleusClient dump follows the
      ND-820026 masterblock display semantics ("only descriptor array
      and kick table have meaning", HW-A17) for cross-checking against
      the carve.

## 6. Phase S3 - BDIO happy path

- [ ] S3-1 [code] BdioEngine decode: fn 166B/167B/213B; sector = DSTBL
      double, length = DNRPG x 2000B words [V]; memory = DMYAD with
      bit31 [ASSUMPTION - comment it]; request words rel +32..67 are
      DON'T CARE (S0-2 [V] - generated tail, no config). UNIT BINDING
      (S0-2 [V]): the DIOC registers one NUCLEUS port per pool NAME;
      SINTRAN's DOPPR open-by-name resolves it into PDF.DRPRT; requests
      arrive on that port and carry DXPOO/OPAIN - map port+indices ->
      SCSIHDD unit in config.
- [ ] S3-2 [code] SCSIHDD mapping: reuse the existing RetroCore SCSI
      disk emulation image path (same images the 3201/SCSI-100 path
      uses); word/byte order per the MPM rules (ND-100 word addressing;
      DIOC DMA is big-endian byte stream [I]).
- [ ] S3-3 [code] Completion write-back per REBDIO [V]: success DSSTS
      (0,0); error -> status low word (carved trio -> -5, else -4);
      DSQCN untouched (mirror-pool case explicitly out of scope until a
      real status semantics carve). Kick per S2-2.
- [ ] S3-4 [test] The HSTAT ladder suite: synthetic DOMDF/PDF/QUDF +
      NUCLEUS area, drive STRBDIO's exact NKWRI/NKSEN register contract
      (BDIO section 2 [V]), assert -1/-2/-3/-4/-5/0 outcomes incl. the
      DCNVA error branch (-3) and bias-cache behavior (SIN-F12). All
      NUnit, in the existing ControllerOctobus fixtures - no standalone
      programs.

## 7. Phase S4 - SINTRAN integration (full disk I/O)

- [ ] S4-1 [config] Generate a DOMINO disk in the harness SINTRAN config
      (5DSKC=1 path [NPL-V]). Provisioning per S0-2 [V]: SINTRAN itself
      initializes DOMDF lazily (QUINI at first pool access) - the
      emulator does NOT pre-provision DOMDF; instead the DIOC/NUCLEUS
      side must ANSWER: create-port (fn 1 -> DLPRT), create-message
      (fn 6 -> DMSIDs), open-port-by-name (fn 3 -> DRPRT), and the
      connect exchange supplying DIPOO/OPAIX/ARESZ (layouts from S0-3
      or S4-2 live capture).
- [ ] S4-2 [live-trace] First real round trip (master plan D13): boot
      harness, capture STRBDIO -> kick -> completion; diff EVERY DOMDF
      word against the emulated DIOC's expectation - closes DSVER+32..67
      without firmware. Use the dap-debugger trace-ring method from the
      SCSI-mount effort if wire capture is not enough.
- [ ] S4-3 [test] Mount + file I/O acceptance: ENTER-DIRECTORY on the
      DOMINO disk, file read/write, dismount - green in the boot
      harness. Gate for declaring FULL DISK I/O reached.
- [ ] S4-4 [test] Coexistence: same fabric runs ND-5000 CPU station +
      ScsiDiocStation; full octobus + ND-500 suites unchanged.

## 8. Phase S5 - robustness (after S4 green)

- [ ] S5-1 [code] DiocWatchdog: heartbeat model surfacing 105314B
      "Missing DOMINO heartbeat" + 105312B/105313B init statuses
      (HW-A18) - error VOCABULARY from ND-814009, but tests restricted
      to carved discriminations (SIN-F5d).
- [ ] S5-2 [code] Retry semantics: BDTMU retry on HSTAT -4 [V] exercised
      by a fault-injection test.
- [ ] S5-3 [OPEN, deferred] Mirror-pool DSQCN case; multi-outstanding
      requests (INQUEUE depth behavior [I-symmetry] until observed
      live); SCSI tape (taOpen/OpId async pattern, ND-814009).

---

## 9. C# rules for all code tasks

No LINQ; no foreach where for works; Span/ArrayPool, zero-alloc receive
path (frames are hot); fixed arrays not Dictionary for OMD/kick tables;
unit tests only (existing NUnit fixtures); dotnet build verified before
any "done"; comments carry the evidence tags ([V]/[I-symmetry]/
[ASSUMPTION]) with the carve-doc reference so the next reader can audit.

## 10. Doc maintenance

This plan is a working document: tick the checkboxes as tasks land, and
record every S0 carve result in the status docs in the same change (S0-5
is the standing reminder). Cross-linked from: the ND5000 README, the
master plan phase D, and CARVING-HANDOFF section 1.9.
