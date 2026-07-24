# ENNS0 annotated disassembly: WHY it never registers *XM-ENNS0 (2026-07-23)

Read-only static disassembly of the ND-100-side ENNS0 supervisor
`E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\encos-err-i-b01.brf`
(PLANC/BRF, MAIN = ENNS0 @ octal 031655), cross-checked against the known-good
live controller trace supplied with the task.

Legend: **[V] VERIFIED** = decoded from the linked image bytes / read from source.
**[I] INFERRED** = reasoned, not byte-proven. Nothing on the wire is guessed.

Tooling reused (NOT rebuilt): scratchpad `brf_link.py` (BRF linker), `nd100dis.py`
(ND-100 disassembler), `mon200.py`/`census2.py` (MON census), `symdump.py`.

---

## TL;DR verdict (Task 5 answer, up front)

**Ranking: (c) is correct. The name-registration code is NOT PRESENT in ENNS0 at
all.** It is neither skipped by a bad status nor gated by a version check - it does
not exist in this module.

**Single decisive fact [V]:** the ENTIRE ENNS0 image contains exactly **two** MON 200B
(XMSG) instructions, and they are:

```
030227  171000   SAT  0        ; T := 0  = XFDUM  (function 0: dummy / get-config)
030230  153200   MON  200      ; XMSG XFDUM  - probe: is the message system up?
030231  010607   STT  -121,B   ; save returned status T -> local(-121)
030232  171001   SAT  1        ; T := 1  = XFDCT  (function 1: disconnect from msg system)
030233  153200   MON  200      ; XMSG XFDCT  - disconnect the probe connection
030234  044607   LDA  -121,B   ; A := saved XFDUM status -> test it below
```

Per `XMSG-API.md` section 6.1, the MON 200B **T register = XMSG function code**:
`0 = XFDUM` (dummy / get config), `1 = XFDCT` (disconnect from msg system). So ENNS0's
complete XMSG footprint is a **liveness probe**: "call XFDUM to confirm XMSG is running,
then XFDCT to disconnect." [V]

Name registration is a completely different sequence. Per `XMSG-API.md` section 6.4 the
XROUT service codes (`XSNAM = 66`, `XSCRS = 80`) are **byte 1 of a message body**, not a
MON operand. To register `*XM-ENNS0` a program must issue `XFGET`(T=2, get message space)
-> `XFWRI`(T=7, write the XSCRS/XSNAM+name bytes) -> `XFSND`(T=12, send to XROUT port 0).
**None of XFGET / XFWRI / XFSND / XFPRV / XFOPN exists anywhere in ENNS0** - there are only
the two MON 200B shown above (T=0 and T=1). [V, by exhaustive census]

Therefore START-NETWORK-SERVER's "Unknown name (XRUNN=2)" is correct: `*XM-ENNS0` is never
created because the program that would create it (the XMSG-COMMAND `Define-Network-*` /
`Start-Network-Server` handler in `xmsg-command-l03.prog`) was never run by the harness,
and ENNS0 itself has no such code.

Rankings for the other candidates:
- **(a) controller-reply mismatch on SUBFUNCTION=5: RULED OUT this run.** [V] The card
  reached READY (PRKEY posted, LANCE up, banner printed, ENNS0 returned with no error). A
  controller-comm failure *would* also skip name registration (POSU aborts) - but it did
  not fire this run, and even if the card had failed, the missing name is independently
  guaranteed by (c): the registration code is absent regardless.
- **(b) XMSG-version / getMagic gate: RULED OUT.** [V] ENNS0 issues no `XFSND` to port 0,
  so it cannot build an `XSGMG`(71) get-magic request. The `" subfunction getMagic"` /
  `" XROUT   ID  "` strings at 021063-021072 are a **trace/dump-viewer label table**, not a
  request builder (confirms prior finding `enns0-getmagic-trace-vs-builder`). The
  `$INCOMPATIBLE TRACE - VERSION` / `$INCOMPATIBLE STATISTICS - VERSION` strings (023200,
  025227) belong to the COSMOS trace/statistics **dump utility** (compares a dump-file
  version), not a live XMSG-version gate.

---

## 1. Annotated STARTUP / POSU disassembly (Task 1)

MAIN = ENNS0 @ 031655; the linear POSU startup body it drives is at **030200-030710+**.
Each PLANC stage follows the shape:
`<do MON/op> ; SKP IF DB UEQ SP ; JPL I *nn (error-handler pointer) ; ... ; STF *147
(-> next-stage pointer)`. On a bad status a stage forks to its error handler (stores a
message index into the POSU error context) and the `STF *147` chain skips the remaining
stages. **PLANC caveat:** the `JPL I *nn` / `STF *147` targets are pointer words
interleaved with code; `nd100dis.py` cannot always separate a pointer word from an
instruction, so the *exact* "status N -> message M -> skip" edge is [I]; the MON opcodes,
the loop structure, and the two MON 200B decodes are [V].

### 1a. Stage map (VERIFIED MON opcodes, in execution order)

```
030222  153135   MON 135      ; [V] get RT/segment info (setup)
030227  171000   SAT 0        ; ---- XMSG STAGE (the only XMSG in ENNS0) ----
030230  153200   MON 200      ; [V] XFDUM (T=0) : probe message system / get config
030231  010607   STT -121,B   ; save XFDUM status
030232  171001   SAT 1
030233  153200   MON 200      ; [V] XFDCT (T=1) : disconnect the probe
030234  044607   LDA -121,B   ; reload XFDUM status
030236  142065   SKP IF DA LST SB ; status ok? (A <= B)
030237  124015   JMP =030254  ;   yes -> continue
030240..030253               ;   no  -> build "XMSG unavailable" error msg (idx 0/1),
030253/030255 JPL I ..        ;         fork to error handler, then STF-chain skips rest
030260  030147   STF *147 =030427   ; next-stage pointer

030375  153322   MON 322      ; [V] (RT/COMMON connect-type call) ; status -> -118,B
030427  153124   MON 124      ; [V] status -> -114,B ; JAZ error fork @030432
030472  153125   MON 125      ; [V] status check @030475 ; fork on bad status

030507-030556 :   controller-slot / RES_SLO staging (builds param blocks, STF *147 chain)
030562-030632 :   SEGLOAD staging  (LDD param, JPL wrappers, STF *147 -> 031001)
030636-030656 :   START_P staging  (JAF status test @030640/030645)

030657  045120   LDA I *120 =030777   ; ---- RETRY / READINESS POLL LOOP ----
030662  170404   SAA 4               ;   retry count / function arg = 4
030664/030665    JPL I wrappers       ;   call PIOCM (SEGLOAD/START_P/status)
030674  142065   SKP IF DA LST SB     ;   status within limit?
030675  124004   JMP =030701          ;     ok -> exit loop, advance
030700  124357   JMP *-17 =030657     ;     retry -> loop back  [V backward branch]
030710  135073   JPL I *73 =031003    ;   post-loop stage
```

[V] The MON census over the whole linked image: **IOX=0, IOXT=0, MON 200 x2
(030230/030233), MON 255 x10**. ENNS0 never touches the Ethernet STATUS/CONTROL register
directly; all controller I/O is via the ten MON 255B (PIOCM) wrappers; all XMSG is the two
MON 200B above.

### 1b. The 10 PIOCM (MON 255B) wrappers @ 032703-033147 - VERIFIED

Each wrapper: load X = PIOC-LDN param ptr, `SAT <fn>`, `MON 255`, save status in T.
The function selects the kernel `PISTA/PILOA/...` dispatch (RP-P2-PIOC.NPL).

```
032703 READPIO  SAT -1  (EXEL execute-instruction block / read)
032734 (2nd read block) SAT -1
033003 INT2GET  (builds block, no MON)
033023 RES_SLO  SAT 0   PIRES  reserve slot
033035 REL_SLO  SAT 5   PIUNL  release
033047 SEND_KI  SAT 7   PISTO
033063 REC_KIC  SAT 3   PIWKI  wait-for-info
033077 SEGLOAD  SAT 4   PILOA  load firmware segment
033113 UNLOAD   SAT 5   PIUNL  unload segment
033124 START_P  SAT 6   PISTA  START PIOC       <-- controller start
033137 STOP_PI  SAT 7   PISTO  stop PIOC
```

START_P body (VERIFIED bytes):
```
033124 146547 START_P: SKP IF DB UEQ SP     ; guard
033125 135012          JPL I *12 =033137     ; (skip if guarded)
033127 054606          LDX -122,B            ; X = caller PIOC LDN
033130 044607          LDA -121,B            ; A = start-address arg
033131 171006          SAT 6                 ; T = 6 -> kernel PISTA
033132 153255          MON 255               ; PIOCM START
033133 004611          STA -119,B            ; save returned A
033134 010613          STT -117,B            ; save returned status T
```

The controller readiness handshake itself (poll PIOC DRAM word 1002B for
PRKEY=052163B, 3 s timeout, then write MPIOC=5 + TRIG=1 + ring doorbell 11B) is done by
the **kernel** PISTA on ENNS0's behalf (`RP-P2-PIOC.NPL` @ 114703-115103), not by ENNS0.
[V, prior finding `ENNS0-PIOCM-START-FINDINGS`].

---

## 2. Is the name-registration code present? (Task 2) - VERIFIED: NO

- [V] Exhaustive census: only two MON 200B in ENNS0, both decoded above as `XFDUM`(0) +
  `XFDCT`(1). A name registration needs `XFGET`(2)/`XFWRI`(7)/`XFSND`(12) to port 0 with a
  service byte `XSCRS`(80)/`XSNAM`(66). Since there are literally no other MON 200B
  instructions, those functions **cannot** be issued by ENNS0. This is decidable
  statically here precisely because there are only two XMSG sites (normally the XF function
  is a data parameter and you cannot tell - but with a complete count of two, and both
  being 0 and 1, the set of functions ENNS0 can ever issue is {XFDUM, XFDCT}).
- [V] Consistent with the live MON 200B trace (prior root-cause doc): during `@rt enns0`
  only driver-port / probe traffic was seen and **no XSNAM/XSCRS to port 0**; only
  `*XM-FIDO` self-registered (that is XMFIDO's job, a different program).
- **Correction to an earlier loose attribution [V]:** the runtime `XFPRV/XFOPN/XFWDF/XFDBK`
  observed in that window are **not** emitted by this ENNS0 (encos-err) image - it contains
  none of those opcodes. They come from SINTRAN's RT-program XMSG driver-port setup / other
  system code, not from ENNS0's own instructions. ENNS0's own XMSG output is just
  XFDUM+XFDCT.

Because the code is absent, there is no "condition/branch to satisfy" and no "earlier bad
status that skips it" - Task 2's if-yes sub-questions do not apply. (The POSU staged error
forks DO skip *later stages* on bad status, but none of those later stages is a name
registration; they are MON 322/124/125 and the SEGLOAD->START_P controller bring-up.)

---

## 3. The SUBFUNCTION=5 exchange, ND-100 side (Task 3) - controller, not XMSG

[V]+[I] SUBFUNCTION=5 belongs to the **PIOC/firmware controller** path (MON 255B /
doorbell), entirely separate from XMSG naming:
- ENNS0 has zero direct I/O; it calls `START_P` (T=6). The kernel `PISTA` does the
  `PWCR=60B` HALT+RESET, `PWCR=0` INITIATE, then polls PIOC word 1002B for PRKEY, then
  writes the request word `MPIOC=5` (this "=5" is the REQUEST value the firmware reads as
  its function/subfunction) and rings doorbell 11B. [V, RP-P2-PIOC.NPL]
- The live trace's `REQUEST=1 SUBFUNCTION=5` OPCOM doorbell + monitor-postbox reply
  `MON_CODE=1 PARAM=0x001E(30)` is the firmware acknowledging that controller-start
  handshake. This run it **succeeded** (card READY, banner printed, ENNS0 returned OK), so
  it did NOT send ENNS0 to an error/abort branch. [V, from the supplied known-good trace]
- ENNS0 does not itself read 0x40C/0x40E; the kernel PISTA consumes the readiness cell and
  returns a status word in T to `START_P`, which ENNS0 tests in the POSU START stage
  (JAF/SKP status forks at 030636-030656). This run that status was OK. [I from trace + [V]
  code structure]

So the SUBFUNCTION=5 exchange is a red herring for the missing name: it concerns the
controller, it passed, and even had it failed it would abort POSU *before* reaching...
nothing, because there is no name registration downstream anyway.

---

## 4. Version / compatibility gate? (Task 4) - VERIFIED: none in ENNS0

- [V] No `XFSND`-to-port-0 exists, so ENNS0 cannot issue `XSGMG`(71) getMagic, cannot read
  an XMSG magic/version cell over XROUT, and cannot branch on it. The only XMSG calls are
  XFDUM (get-config) + XFDCT. XFDUM returns config, but its status is only tested as
  "message system present?" (030236 SKP), not as a version comparison. [V]
- [V] The `" XROUT   ID  "`, `" subfunction getMagic"`, `"**SUBFUNCTION UNKNOWN**"`,
  `" MONTR   ID  "` strings @ 021063-021072 are a **format/label table** for the COSMOS
  trace-log viewer (matches prior finding). `$INCOMPATIBLE TRACE - VERSION` (023200) and
  `$INCOMPATIBLE STATISTICS - VERSION` (025227) are the trace/statistics **dump utility**
  version checks (dump-file format), not a runtime XMSG (M00 vs B01) gate. B01-vs-M00 is
  never compared in code here.

---

## 5. CONCLUSION (Task 5)

**Most-likely reason, ranked answer = (c): the name-registration code is not present in
ENNS0.** Evidence, exact octal:

- [V] The complete XMSG footprint of ENNS0 is two instructions:
  `030230 153200 MON 200` with T=0 (**XFDUM**, set at `030227 171000 SAT 0`) and
  `030233 153200 MON 200` with T=1 (**XFDCT**, set at `030232 171001 SAT 1`). That is a
  message-system liveness probe + disconnect, nothing more.
- [V] MON census of the whole linked image: MON 200 x2 (only the two above), MON 255 x10,
  IOX/IOXT = 0. No `XFGET`/`XFWRI`/`XFSND` (T=2/7/12) exist, so no `XSCRS`(80)/`XSNAM`(66)
  message to XROUT port 0 can be built. Name creation lives in the separate XMSG-COMMAND
  `Define-Network-*` / `Start-Network-Server` handler (`xmsg-command-l03.prog`), which the
  bring-up harness never executed.
- [V] (a) ruled out this run: controller SUBFUNCTION=5 handshake succeeded (card READY).
- [V] (b) ruled out: no XFSND-to-port-0 => no getMagic/version request; the version strings
  are trace/dump-viewer labels.

Net: START-NETWORK-SERVER correctly returns XRUNN(2) "Unknown name" because `*XM-ENNS0`
was never registered, and it was never registered because **neither ENNS0 nor the harness
run ever executed the XSCRS/XSNAM create-service sequence** - that sequence is not in this
module. The fix is a procedure/config gap (run the COSMOS `Define-Network-*` /
`Start-Network-Server` XMSG-COMMAND that issues XSCRS `*XM-ENNS0`), not an ENNS0 or
emulator MON-call change.

---

## PLANC data-in-code ambiguities (explicit, per policy)

Byte-exact decode is blocked where PLANC interleaves pointer/literal words with code; the
disassembler renders these as spurious `STZ/BITOP/.WORD`:
- `031000`, `030777`, `030557`, `030627`, `030631` and the `*147`/`*nn` targets of the
  `STF`/`JPL I` staging are POINTER words, not instructions (e.g. `030700 JMP *-17 =030657`
  loops on a real branch, but the `JPL I *120 =030777` at 030657 dereferences a pointer
  cell at 030777). [ambiguous addresses: 030777, 031000, 031001, 030557, 030627]
- The exact "status code -> message index -> which later stage is skipped" edge is [I];
  the MON opcode identities, the two MON 200B function decodes (T=0/T=1), the ten MON 255B
  functions, START_P T=6, and the retry loop 030657-030700 are all [V].

None of these ambiguities affect the conclusion: the MON 200B count is a hard,
opcode-exact census (2), and both are XFDUM/XFDCT.

---

## Source / evidence paths
- Target: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\encos-err-i-b01.brf`
  (MAIN=ENNS0 @031655; POSU 030200-030710; PIOCM wrappers 032703-033147; START_P @033124).
- XMSG convention: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\XMSG-API.md` sec 6.1 (T=function
  code; 0=XFDUM,1=XFDCT,2=XFGET,7=XFWRI,10=XFOPN,12=XFSND) + sec 6.4 (XSNAM=66,XSCRS=80).
- Kernel PIOCM driver (PISTA/SUBFUNCTION path): `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL`.
- Prior findings (scratchpad): ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md,
  ENNS0-POSU-XROUT-ERROR-GATE-2026-07-23.md, ENNS0-PIOCM-START-FINDINGS-2026-07-23.md,
  ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md, XMSG-XROUT-CARVE-LOCATE-AND-RECIPE-2026-07-23.md.
- Tooling (scratchpad): brf_link.py, nd100dis.py, mon200.py, census2.py, symdump.py.
- This file: `C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Ronny-NDInsight\b17a7474-33c0-4d7f-b9cb-f921c3ad419b\scratchpad\ENNS0-ANNOTATED-DISASM-WHY-NO-NAME-REG-2026-07-23.md`
