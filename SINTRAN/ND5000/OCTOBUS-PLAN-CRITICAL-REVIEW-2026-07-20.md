# Octobus Plan - Critical Review (2026-07-20)

Three independent expert reviews of
[OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md](OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md),
each attacking shortcuts and unvalidated assumptions from one angle:

- DEV = RetroCore C# developer review (phases A/B/E vs the actual code and
  test suites at Emulated.HW\ND\CPU\NDBUS\ and Emulated.Tests.ND100\).
- SIN = SINTRAN kernel architect review (phases C/D vs the byte-verified
  carves in tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\
  and the message catalog).
- HW = ND-5000/DOMINO hardware architect review (sections 1-5 vs the
  manuals in Reference-Manuals\500).

Every finding was verified against code/carve/manual by the reviewer; items
the reviewers could NOT verify are marked [UNVERIFIED]. The plan document
has been corrected per these findings (same date); this file preserves the
full evidence trail. Finding IDs (DEV-n, SIN-Fn, HW-An) are referenced from
the plan's section 8 TODO lists.

---

## 1. DEV findings (phases A/B/E vs RetroCore code)

Test-suite inventory VERIFIED: OctobusControllerTests, OctobusND5000Tests,
OctobusMailboxO1Tests, OctobusTpeConfigReproTests, OctobusPhase3ExecBringupTests,
OctobusPhase3MonBringupTests, OctobusPhase3RestartTests, OctobusPhase3TrapTests,
OctobusPhase3ThreadedTests (Emulated.Tests.ND100\ControllerOctobus\) plus
OctobusTpeBootHarnessTests, OctobusMachineBringupTests,
Nd100SintranNd5000OctobusBootHarnessTests (Emulated.Tests\ND100\). All names
in the plan are real. Note: OctobusTpeBootHarnessTests.cs is UTF-16 encoded
(ripgrep sees binary; use Select-String).

DEV-1 BLOCKER. Removing the hardcoded SCSI placeholder (NDBusOctobus.cs:1819,
constructor `RegisterStation(new OctobusSimpleStation(10, ...))`) breaks
register-level unit tests that assert default registration:
OctobusControllerTests.cs:361/:473/:508, OctobusTpeConfigReproTests.cs:60-75.
The plan's "move to machine config" mitigation only covers the machine-booted
harness (OctobusTpeBootHarnessTests.cs:231/:268). Phase B is therefore NOT
zero-behavior-change; the test edits must be sequenced explicitly.

DEV-2 BLOCKER. Phase E "second CPU is just a second station" is contradicted
three ways in current code:
  a. NDBusOctobus._nd5000Station is a single field (:1504); AttachCpu
     UNREGISTERS the previous station and replaces it (:1873-1880). A second
     AttachCpu today kills CPU 1's station. (ND100Machine.ND5000.cs:38-43
     records this limitation; the octobus plan did not.)
  b. The MPM hooks fan out to the single station only: TryOverrideMpmRead
     (:1612-1615, called from ND100Memory.cs:404/431), NoteMpmRead (:1624),
     NotifyMpmZeroWrite (:1665-1668, ND100Memory.cs:522/542). A second CPU
     registered on the fabric never sees its X5ACT activation writes and its
     CS-load read-back interception is dead.
  c. X5ACT self-discovery (OctobusND5000Station.OnMpmActivationWrite,
     :699-751) latches the FIRST plausible -1 -> 0 halfword; with 4 CPUs
     (ext blocks differ by (M-N)*256) a station mis-latches another CPU's
     write. Discovery must become CPUNO-aware against a shared header.

DEV-3 BLOCKER. X5SEM atomicity: IServicerHost.TryTakeSemaphore is documented
(IServicerHost.cs:68-79) to REQUIRE Interlocked on the shared backing array,
explicitly forbidding read-check-write via ReadNd100Word/WriteNd100Word. The
station implements exactly the forbidden pattern under a per-station
lock(_mailboxLock) (OctobusND5000Station.cs:859-876; known audit item
F-oct-2 cited at :576-581). With 4 stations, four different lock objects
guard the SAME shared word, plus the emulated ND-100 TSET path: mutual
exclusion gone. [UNVERIFIED] the multi-CPU plan's claim that an "Interlocked
CAS path is already designed" - no such implementation found. The
ISharedMemoryWindow semaphore helper must be ONE shared object per MPM
window, Interlocked against the backing array.

DEV-4 RISK. "MultibyteAssembler one per OMD" is impossible at the wire
level: data frames (C=0) carry NO OMD field; only SOMB/EOMB do
(NDBusOctobus.cs:229-251). Assembly is per-station, keyed by the OMD
captured at SOMB. Hidden couplings a naive extraction breaks:
  - _accpMessageSource is consumed by SendAccpMessack/SendVparpEcho/
    SendAccpMessnak long after collection (OctobusND5000Station.cs:1544,
    1603, 1713) - reply-address state leaks into the ACCP executor; the
    delivered tuple must be (bytes, source, omd).
  - _lastAccpMessage (public LastAccpMessage, asserted by
    Multibyte_Omd3_ConsumedByAccp) is set at EOMB for BOTH OMD 0 and 3
    (:1041); splitting OMD 0 out changes what it observes unless preserved.
  - The _accpIdle admission gate is frame-class- AND OMD-specific (:943-956):
    while terminated, OMD-0/3 multibyte passes, kicks/idents/other drop
    (locked by Emergency_TerminateAndContinue_ControlIdleLoop,
    OctobusND5000Tests.cs:314). A generic dispatcher needs a per-station
    admission-policy hook.

DEV-5 RISK. AccpAccessModule / Nd5000MailboxHost split has cross-cutting
state the plan assigned to the wrong half:
  - LSysparWord1 is captured on the ACCP path (CMSYSPAR parse, :1428) but
    consumed by the mailbox host's GIVEINT (AnswerWritten, :886-896).
  - The CS-load machinery (_accpParameterPointer, _controlStore, _ducs*,
    TryOverrideMpmRead, :201-339, 1617-1694) is ACCP-side but reads/writes
    MPM - the ACCP module needs ISharedMemoryWindow too (section 7.3 had
    assigned it only to the mailbox host).
  - NoteMpmRead uses static diagnostics CpuND100.DiagCurrentPC/PIL
    (:245-246) and the shared _accpExchangeLog spans both halves.
  The station stays a facade forwarding to both components.

DEV-6 RISK. Moving latency/busy-retry into the fabric is an INTERFACE
REDESIGN, not a relocation: busy-retry is driven by the receiver's FIFO
occupancy (_busyRetryQueue gated on _receiveFifo.Count, NDBusOctobus.cs:
3040-3047, pumped from Read() :2199) and the fabric has no way to see FIFO
state (HandleFrame returns frames or null, no busy channel,
OctobusFabric.cs:66); only NDBusOctobus has Clock() (:3005); the dest-0 /
own-station loopback bypass (:2580-2592) must NOT be delayed. Plus an
EXISTING cross-thread race: _inboundDelay is a plain Queue<T> enqueued on
the CPU run thread (servicer AnswerWritten -> fabric -> adapter ->
OnFrameFromOctobus, :2989-2995) and dequeued on the ND-100 device thread in
Clock() (:3009-3022) with NO lock (IServicerHost.cs:20-25 warns about the
servicer thread).

DEV-7 RISK. Test coverage does NOT lock all behavior phase A claims is
locked. Verified-uncovered: ControlWordBits.ContinueACCP path incl.
_mudomDetected side effect (NDBusOctobus.cs:2453-2461; zero test matches);
OMD-0 commands 0x0016 "Get Domino Information" / 0x0018 "Get test version"
reply layouts (:487-505); echo-multi wordCount>121 rejection (:438-439);
malformed OMD-0 length paths (:330-349; only WrongMagic_NoReply exists);
OctobusSimpleStation OMD!=0 "acked and dropped" (:667-693);
_inputDataRegister echo-on-empty-FIFO fallbacks (:2214-2217, 2507-2517).
Lock-tests must be written BEFORE extraction.

DEV-8 RISK. ContinueACCP rerouting via a 242B emergency frame is not
equivalent as-is: the register path also sets _mudomDetected = true
unconditionally, and the ControlWordBits write carries no destination
station (the card must still consult the station's number). Define intended
behavior, lock it with a test (DEV-7), then move.

DEV-9 RISK. MPM geometry has three duplicated constant sites:
InitializeSharedMemory(0x420000, 8MB) in the constructor (:1789-1790),
ND5000_DEFAULT_MPM_START/SIZE in ND100Machine.ND5000.cs:65-66, and
PARAM_REGION_BASE hardcoding NDBusND500IF.DEFAULT_SHARED_MEMORY_START
(:1572). Parameterizing without unifying invites drift; the window must
stay default-ON (SINTRAN's LOCAL-vs-MPM5 KMPM5 probe, :1782-1788,
2877-2883).

DEV-10 RISK. Per-CPU identity is hardcoded: _servicer.CpuParameter = 0x03E1
(model-8 5800) in the station constructor (OctobusND5000Station.cs:452);
_cpuNumber defaults to 1 until ConfigureMailbox (:564). Phase E needs the
Nd5000CpuConfig surface (multi-CPU plan phase 1) FIRST.

DEV-11 NIT. Plan section 6 line counts stale (NDBusOctobus 3099 not 3050;
OctobusND5000Station 1805 not 1449) - re-verify the coupling list at HEAD
before phase A.

DEV-12 NIT. C# rule temptations: MultibyteAssembler must reuse a growable
buffer / ArrayPool (current code allocates List<byte> + byte[] per message,
NDBusOctobus.cs:687-690); OmdDispatcher = fixed IOmdEndpoint[16] array, not
Dictionary; no LINQ anywhere incl. when touching OctobusFabric's broadcast
collection (:226-260); all new verification stays in the existing NUnit
fixtures.

DEV-13 [UNVERIFIED]: whether OctobusMachineBringupTests /
Nd100SintranNd5000OctobusBootHarnessTests also depend on constructor-registered
station 10; whether the emulated ND-100 TSET instruction path synchronizes
with anything on the MPM window (F-oct-2 implies not).

---

## 2. SIN findings (phases C/D vs the carves)

SIN-F1 BLOCKER. "Kick-5 wiring" contradicts the plan's own carve: the
byte-verified host NKSEND -> NKICK path emits SKICK with kick number 1
(NUCKI=000001; 044600 SAA 1 -> 037254 SKICK [V], NUCLEUS-PRIMITIVES-CARVE.md
section 5.2). Kick 5 = "NUCLEUS" is only the manual/NPL kick-NAME table.
Worse, the kick number the DIOC must send TO the ND-100 to wake DKICK is
[OPEN]: DKICK is registered via CONKI(A=14B, T=1) whose body is uncarved
(NUC 5.8). Wrong kick number = KICKENT dispatch never reaches DKICK = BDIO
completion silently dead. The section 5 matrix [V] on "kick 5" was an
overclaim (now fixed in the plan).

SIN-F2 BLOCKER. Phase C's MF error-record test was wrong on the wire:
OMD 4 (MFOMDNO) is the MF controller's RECEIVE OMD - direction ND-100 -> MF
(MFPREPARE and the host's MFACK reply). MF-to-host error records go to
STATION 1 on OMD = 5OMDNO, the runtime-allocated ND-100 receive OMD
announced in the MFPREPARE body "0E 01 <5OMDNO>"
(OCTOBUS-DRIVER-ROUTINES-CARVE.md sections 9/10 [V]). 5OMDNO is NOT a
constant (LMDF word 0, allocated by CONOMD). An emulated MF station MUST
parse MFPREPARE and capture body byte 2.

SIN-F3 BLOCKER (test-oracle). The L07 MFACK addressing is byte-anomalous:
carved code at 147207-147220 reads LMFIELD word 3 - which at that point
holds the RECEIVED BYTE COUNT (OMBREAD wrote it there), not the source
station - range-tests it 2..6 and uses it as MOCTSTATION for the MFACK
(ODR divergence 12.2, [V bytes / OPEN reconciliation]). On this image the
ack destination appears to equal the record length and only fires for
lengths 2..6. Do not code a phase C oracle from the catalog's NPL prose
until reconciled.

SIN-F4 RISK. MFPREPARE requires far less back than the plan implied: carved
5OMBREAD (147115-147120 [V]) simply DISCARDS an MF-source ETYPE==MFACK(0)
message ("ack for our message") - no aliveness flag, no retry ladder found
for MF stations (contrast SAMSON 5ALIVE). Absent and present-but-silent MF
stations are indistinguishable to SINTRAN at 5PIT; a station that answers
OMD 0 but never OMD 4 bothers SINTRAN not at all (SINTRAN never speaks
OMD 0 - that is TPE only). Phase C.2's assertable surface is essentially
"boot does not regress". NIT within: the ack-recognition compare is
FULL-WORD (A vs 0) and OMBREAD copies (len+1)/2 words; whether SOCTO
zero-fills the odd byte is uncarved - an emulated MF ack should send a
2-byte 00 00 body. [I]

SIN-F5 BLOCKER. Phase D "carves are done" oversold: the complete
NOT-byte-verified list a Scsi DIOC must touch:
  a. Port provisioning: who creates the DIOC port descriptor, writes
     PDF.DRPRT / DOMDF.DLPRT, and how DDS-DEVICES:CNFG binds
     station+SCSI-unit+LUN - all [OPEN]; allocation/freelists live in
     segment 105 interior (RECON target 2).
  b. DOMDF initialization: DSVER, DOMDF+21, static header words
     DSVER+32..67/+73 - "initializer not located" (BDIO section 11 [OPEN]);
     most plausible carrier of the SCSI unit/LUN binding [I]. Which SCSIHDD
     a request targets is currently unknowable from carved data.
  c. DMSID/HOMEPORT provenance: queue-DF word 13 is a pre-existing message
     descriptor number; allocator uncarved; the reply route "DIOC nkSends
     with sendref=0 -> HOMEPORT == DLPRT" is [I], not [V].
  d. Completion record: PARTIALLY carved - REBDIO's READ side is fully
     byte-verified (BDIO section 4): DSSTS (0,0)=OK; benign statuses
     104031/104651/104622 -> HSTAT -5; DSQCN==(-1,-1) with DXPOO==PDF.DIPOO
     -> mirror-pool timer; anything else -> HSTAT -4 + SINEC 1662. So the
     MINIMAL write-back bytes are known; the status-code SPACE semantics
     are not. Happy-path emulation possible; faithful error injection not.
  e. Kick number + kick-entry binding to DKICK (SIN-F1).
  f. Owner-ID convention for a controller-side actor (CLUST is
     ND-500-specific) - probably moot via NKREC host-side owner transfer [I].
  g. DCNVA bit31 on the DIOC side: host encode rule is [V]; how a 68020
     DIOC's BADAP DMA interprets bit31 is NOWHERE verified - [ASSUMPTION]
     if mirrored from ND-500 Port-B semantics; self-consistent inside the
     emulator but must be flagged.
  h. Controller-side NUCLEUS library behavior (TSET value, INQUEUE honor)
     mirrored from host carve by symmetry [I]; acceptable only because both
     ends are emulated - say so in code comments.

SIN-F6 RISK. Open-items triage for phase D:
  - NKSE segment-105 interior: BLOCKING for real provisioning; DEFERRABLE
    only if phase D explicitly scopes to "pre-provisioned datafields,
    happy path" and documents the fake.
  - ENKIC=047526 overlay: DEFERRABLE (callers 007-S3DMAC/130-CFT/135-XFTRAD/
    134-SNA3270, none on the STRBDIO path which uses NKWRI/NKSEN [V]).
  - OMBREAD entry[-10..-12] counters + JXN tail anomaly: DEFERRABLE for D,
    RELEVANT to phase C the moment a station sends more than one error
    record before 5OMBREAD drains (multi-pending semantics unresolved).
  - WT12/NFUNC wake internals: DEFERRABLE (SINTRAN executes them itself;
    the emulator only delivers the kick frame - loops back to SIN-F1).
  - PROMAN/OPCOM boot protocol + PMA firmware: DEFERRABLE for the
    SINTRAN-facing contract, but see SIN-F9.

SIN-F7 RISK. Phase C is honestly independent of phase D (MF error path is
pure octobus multibyte, never NUCLEUS), but both depend on CONOMD's runtime
OMD allocation (body at 040062, instruction-level RE OPEN - catalog
section 8). A harness that hardcodes an assumed 5OMDNO tests itself.

SIN-F8 RISK. Boot-order reality (catalog sections 6.1-6.3): cold SINTR
probes the octobus CARD by IOXT (100406 RFT) then sends emergencies
241B/242B to SAMSON stations ONLY; 5PIT runs CON5OMD once (if CONOMD fails
-> WT12 and MFPREPARE NEVER RUNS, ODR 147273); MFPREPARE goes to EVERY
station 2..6 unconditionally (absent -> Ack=00 timeout); CON5IDENT per
SAMSON. SCSI stations 10B-13B receive NOTHING at bring-up. Consequences:
default station behavior must be "do not answer OMD 4 unless configured as
an MF controller"; the boot harness should assert ZERO unsolicited frames
to 10B-13B.

SIN-F9 RISK [UNVERIFIED]. Whether this L image auto-runs PROMAN (segments
120/121, carved-but-never-disassembled) when a DOMINO device is generated.
If yes, configuring stations 10B-13B could expose them to
EchoTest/IdentY/SetBxP traffic whose byte protocol is [OPEN]. Check
(segment recon or live wire watch) before adding DIOC stations to the
default harness config.

SIN-F10 NIT. Phase C station examples overlapped (SCSI 10B-13B vs ETH3
12B); live examples were SCSI at 13B + ETH3 at 12B - pick non-overlapping
defaults.

SIN-F11 NIT. Cite the byte carve (ODR) as primary for the MF ack path, the
catalog (NPL-derived, with its own L07 [UNCERTAIN] corrections) as
secondary.

SIN-F12 NIT. DCNVA emulation/tests must model the error branch too
(negative bias subtraction -> error -3 "outside multiport") and the
self-modifying one-shot bias cache, not just the formula.

---

## 3. HW findings (sections 1-5 vs the manuals)

Summary: the protocol core (2.1, 2.2, 3.1, 3.2, 3.4) is solidly
manual-backed. No blocker invalidates the emulator's byte-verified values.
The weak spots: the 361B analysis, the ND-500-II exclusion, the compressed
crate-config story, and ignoring the ND-820026.1 edition + ND-05.017.01
chapter 8 (the richest de-risk material for phases C/D).

HW-A1 NIT. Station map confirmed in three manual instances (ND-05.020.01
p.329; ND-05.017.01 ch 3.1 p.53 and ch 8 p.237).

HW-A2 RISK. "Station 0 and 77B are illegal" overstated: ND-14001 4.8.1 puts
global devices at 0-17B (0 nominally in range) and the MFB controller
PROBES station 77B downward during crate configuration. A fabric that
hard-rejects dest 77B breaks faithful crate config. Correct statement:
"0 and 77B are not valid ASSIGNED stations; 77B is the crate-config probe
address".

HW-A3 RISK. Fixed 70B-73B vs ND-14001 dynamic assignment reconciled by
ND-05.017.01 ch 8 p.237: the MFbus T&M program writes OCTOBUS STATION NO
(70), POWER FAIL DESTINATION (default 1), REC. BROADCAST TYPE (default 0)
into NON-VOLATILE MEMORY per slot. Stations are pre-provisioned in NVRAM,
not discovered each boot - and those two defaults are exactly what an
emulated BADAP/WOI should power up with.

HW-A4 (confirm). Module-type Table 1 verbatim in ND-820026-1c p.30;
hardwired-per-card verified (ND-14001 ch 3 p.107); live examples verified
(ND-820026.1: SLOT 11 station 13B SCSI, station 12B ETH3). Extra fact:
modules 6B-76B not in Table 1 present a numeric hardware-id ("006B" etc.).

HW-A5 NIT. The TPE type classification 1=Domino/2=MFbus/3=ACCP is
carve-derived (OCTOBUS-TEST-PROTOCOL-RE.md), NOT in ND-05.017.01 8.2.7 -
keep provenance as carve.

HW-A6 NIT. DIOC anatomy mostly confirmed (68020, 8 MB DRAM max, EPROM/EEROM,
MFP 68901, FF8000-FF810E map, BADAP registers, OCTC on ACCP). Two
unsupported details: "16x16 receive FIFO" is documented only on the ND-5000
CPU side (ND-05.017.01 3.4); the DIOC OBCON FIFO depth is undocumented
(TPE OCTOBUS test 3 measures it) [I-infer same chip family]. "PCB 3109/3096
part 324118" appears in NO manual in Reference-Manuals\500 - source it or
mark [UNCERTAIN].

HW-A7 (confirm). Frame decode confirmed; ND-05.017.01 3.3.1 (p.57) is the
CLEAN decode table; ND-05.020.01 figure 64's OCR is column-garbled - cite
017.01 as primary.

HW-A8 NIT. Broadcast-multibyte legality is WEAK (only the garbled figure's
B=X column; section 2.9 silent; 017.01 table shows B=0 on every row).
Downgraded to [UNCERTAIN] unless a carve shows it used.

HW-A9 NIT. Wire-frame extras the plan dropped: broadcast ack 11 = ambiguous
with 0 retries (ND-14001 Figure 32 p.128); retry counts are DEFAULTS,
programmable via the Transmitter Control Register; retransmission flagged
in the Transmit Status Register; priority counter resets after success,
lowest station wins ties (ND-14001 4.2/4.4). Priority width: 020.01 figure
4 bits vs ND-14001 text "5-bit" - the plan's hedge stands.

HW-A10 NIT. Multibyte: byte count is ONE data frame, so max body = 255
bytes - a hard limit the plan never stated; MultibyteAssembler should
enforce it. Tension to record: ND-05.020.01 5.3.48 says in test/init mode
the ACCP ignores everything except multibyte to OMD 3, yet the OMD-0 test
responder demonstrably answers on live hardware - note it, do not silently
assume.

HW-A11 (confirm). Kick table 1-6 confirmed (ND-05.020.01 2.7 p.336);
kick 5 as the NUCLEUS doorbell named in ND-820026 ("KICK DEST. Index in
kicktable(=OCTOBUS station no)"). NOTE: this is the manual NAME table; the
byte-verified host-side send is kick 1 (SIN-F1) - both true, different
directions/paths, keep both statements separate.

HW-A12 RISK (analysis replaced). The 361B discrepancy is NOT a possible OCR
error: 361 octal = 241 DECIMAL (362B=242, 363B=243, 364B=244), so
ND-05.017.01 Appendix A self-consistently claims wire byte 0xF1-0xF4 while
NPL/ND-14001 claim 0xA1-family (241B octal), base anchored by POWERDOWN
377B = all ones (ND-14001 Figures 33/35 p.129-130). ND-05.020.01 Figure 66's
R-bit table cannot arbitrate (0xFE/0xFF already violate it). SINTRAN's send
side is byte-verified 0xA1-family and real systems worked, so the ACCP must
accept them [I-infer, strong]. Emulator values stand; residual question =
what the AOCP hardware decoder matches (one live-trace byte closes it).
Bad citation fixed: ND-05.020.01 5.3.9 is "ACCP TIMEOUT"; the emergency
commands are 5.3.50/5.3.51 (behavioral only, no byte values); the code
points come from ND-14001 Figure 35 + NPL.

HW-A13 NIT. Power messages confirmed (source 1-17B = power fail, 20B-76B =
fatal controller failure, ND-14001 p.129). Manual inconsistency to log:
Figure 34 draws power-UP info as ALL ZEROS while Figure 35 says 376B; they
cannot both be right (all-zeros with C=1 would decode as an ident; 0xFE
fits the decoded family) - likely a drawing error.

HW-A14 RISK. PROMAN boot: byte formats absent (plan right), but the manuals
document MORE observables than the plan exploited (all in ND-820026.1):
reset state machine incl. aborted/WAITCONT states (2.6 flowchart) and
selftest numbers/names (Appendix B); SetBxP has ACK/NAK with error codes
and timeout-0 convention (ERS 105027B "Unable to set mailbox ... Opcom
NAK-error code ('0' means timeout)"); boot event ladder ERS 105042B-105045B
+ DOMINOS fatal events 6000B-6007B; OPCOM LED flash patterns incl. "error
connecting to OMD" classes; DOMINO Monitor OPCOM paths ASYL/SERVER/MAILBOX,
USE-MAILBOX "MF page number for mailbox: 400B". These define the observable
contract a stub DIOC must honor.

HW-A15 RISK. Crate configuration was compressed to inaccuracy. ND-14001
4.8/4.8.1 actual sequence: phase I global auto-init + master selection;
phase II configurator broadcasts "Identify yourself"; the MFB controller
with the HIGHEST station number configures its crate first; per slot: read
RMT (0 = empty), probe station 77B downward for free numbers, assign
station/PF/BT via TWO consecutive WOI writes to slot address + 10B, start
the node by writing 1 to MASTA bit 7 at slot+4B which GENERATES the OBRES
reset (after which the node answers - not "releases reset"); send "Finish";
next controller proceeds. Slot addressing: slot*2 -> upper address digits
(worked example p.136). Fully specified at register/sequence level - needs
no firmware find.

HW-A16 RISK. "Classic ND-500-II is NOT an octobus device" contradicted in
part: ND-14001 p.123 "The cable is used to connect MFbus banks, ND-100 and
ND-500 model II to the OCTObus". Correct statement: the ND-500-II CPU
interface is 3022/5015 (not octobus), but a model II ATTACHES to the
octobus via cable in DOMINO configurations; its NUCLEUS fast calls run in
the ND-100 at level 12 (ND-820026.1).

HW-A17 (confirm). NUCLEUS model confirmed (kick table indexed by station,
master block/descriptors/hash, microcoded nkMove/nkSend/nkReceive/nkGetInfo
on ND-5000, everything else in the ND-100). Masterblock display semantics
("only descriptor array and kick table have meaning") = debug oracle for
NucleusKernel.

HW-A18 (confirm). ND-814009 has NO byte-level NUCLEUS message format - the
host-side carve is the only wire-contract source (plan right). But it DOES
have two complete status-code tables: tape 104701B-104777B; disk/DOMINO
104601B-104677B and 105301B-105377B incl. 105314B "Missing DOMINO
heartbeat, controller aborted" and 105312B/105313B DOMINO init status -
the error vocabulary for the future SCSI DIOC.

HW-A19 NIT. Two-bus quote "the octobus is normally not used to transport
data" is verbatim in ND-05.020.01 (twice, ch 5); ND-05.017.01 A.4 says
"Octobus and memory are used for communication and synchronization" -
citation tightened.

HW-A20 RISK (process). The plan cited only the ND-820026-1c edition; the
folder also holds ND-820026.1 EN (larger), whose appendices (ERS catalogs,
selftest Appendix B, LED patterns) supplied most of HW-A14. Added as a
first-class source.

---

## 4. Disposition

All corrections were applied to the plan doc on 2026-07-20:
sections 1, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 3.4, 3.5, 4.1, 4.3, 4.4, 5, 6,
7.1-7.4, 9 amended; section 8 rewritten with per-phase TODO task lists
referencing the finding IDs above.
