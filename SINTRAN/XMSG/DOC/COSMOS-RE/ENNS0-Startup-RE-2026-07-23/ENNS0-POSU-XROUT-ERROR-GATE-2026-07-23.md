# ENNS0 POSU startup: where "Error ... XROUT" comes from and what gates it (2026-07-23)

Read-only static analysis of the ND-100-side ENNS0 supervisor
(`encos-err-i-b01.brf`) plus the sibling message/error files and the kernel
PIOCM driver. Every claim tagged **[V]=VERIFIED** (decoded bytes / read source)
or **[I]=INFERRED** (reasoned, not byte-proven). Nothing on the wire is guessed.

Tooling: scratchpad `brf_link.py` (BRF linker), `nd100dis.py` (ND-100 disasm),
`dumpstrings.py`, `census2.py`, `dumpwords.py`.

---

## TL;DR verdict (Task 5 up front)

The user's mechanism is **structurally CORRECT**: ENNS0's POSU startup drives the
Ethernet controller through the kernel PIOCM driver **BEFORE** it registers the
network-server name, and if that controller step returns a bad status, POSU
aborts to its error path and **never reaches the name registration**. So a
controller-communication failure does gate the "startup error / never-registered
name" outcome.

But two important corrections to the hypothesis:

1. **The gating controller step is NOT an INT12 / IDENT interrupt wait.** [V] It is
   the kernel `PISTA` (MON 255B fn 6) **memory-mapped PRKEY readiness poll** on PIOC
   DRAM word 1002B (value must become `PRKEY=052163B`), 3-second timeout. The
   INT12/IDENT/RFT/RIE interrupt path is used for *later* kick/mailbox traffic
   (`PIKIC`/`PIWKI`), not the START readiness gate. So the emulator's
   "level-12 IDENT clears `interruptEnabled` (RIE)" bug is **not** the START gate.
   The relevant emulator fix point is `PWCR=60B` (HALT+RESET) not re-running the
   68K reset vector, so PRKEY is never reposted (see
   `ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md`).

2. **The literal banner "Error in communicating with XROUT" does not exist as a
   verbatim string in ANY of these files.** [V] It is a paraphrase/composite of the
   real catalog strings (see section 1). The real POSU top-level is
   `"Error during startup."` and the interface-status hints
   (`"No answer from interface."`, `"Interface not started."`,
   `"Check if RTCOMMON is in interface memory."`).

**Which gate is "current" depends on run-state:** the latest run
(`ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md`, 13:47) reports the card
reaching READY yet ENNS0 STILL issuing only `XFOPN/XFWDF/XFDBK` and no XSCRS -> a
**second, downstream** name-registration gate also exists. Earlier runs (PRKEY
never posted) failed at the controller-comm gate. Both are real.

---

## 1. Where the messages actually live (Task 1)

### 1a. `encos-err-i-b01.brf` POSU / PIOC message catalog [V]
`dumpstrings.py` over the linked image. The catalog begins at **014642** with the
POSU banner root and runs a packed sequence of PIOCM (MON 255B) status texts:

```
014642  ENNS0-POSU ... ENCOSS0            (banner root)
014675  Undefined MON PIOC error.
014712  No answer from interface.
014727  Insufficient priviledge. / Interface not started. / Illegal function code.
        / Slot already occupied. / Illegal slot number. / Slot is not reserved.
015031  Mailbox is not empty.
015044  Mailbox is empty.
015055  Illegal LDN. Order new SINTRAN with PIOC/Ethernet.
        / Interface not initiated. / Complete interface memory not fixed. Change size.
015153  Check if RTCOMMON is in interface memory.
015200  Illegal segment. / Check list-file from loading.
015227  Segment not loaded.
015241  Attempt to fix demand segment. / ...
```
[V] These are consecutive strings in one catalog; their content shows they are
**PIOCM (MON 255B) return-status messages** - i.e. the text printed when the
kernel interface driver returns a non-OK status to ENNS0.

[V] `POSUERR` (ENTR @ **001756**) is a **data label** (a zeroed COMMON/BSS cell,
disassembly = all STZ/zero words), i.e. the POSU error-code variable, not a
routine. The formatting is done by the `UEIxMSG/UEERxMSG` UE-library routines
(symbols `UEIDMSG/UEIFMSG/UEIOMSG/...` @ 033323+).

### 1b. The UE catalog `ue-ermsg-en-b03.err` [V]
`strings | grep`:
```
@A Error during startup.          <- the POSU top-level line
@A Unexpected status from XMSG.
@A Unexpected status from PIOCOS. Contact ND.
Error in communication with XMSG. @A
Unknown msg form XROUTE.
XROUT or File Server
+The User Environment system is not running.
?Data communication (XMSG) error in the User Environment system.
```
[V] **No string "Error in communicating with XROUT" exists.** The closest real
strings are `"Error in communication with XMSG."`, `"Unknown msg form XROUTE."`,
`"XROUT or File Server"`. The banner as quoted is a paraphrase; treat the actual
runtime line as one of these + `"Error during startup."`.

### 1c. The `getMagic` / `XROUT ID` strings are a TRACE-LOG table, not the builder
[V] At 021063 (`" XROUT   ID  "`, `" subfunction getMagic"`, `"**SUBFUNCTION
UNKNOWN**"`, `" MONTR   ID  "`) - this is a message/format **label table** for the
COSMOS server trace viewer (consistent with the prior finding
`enns0-getmagic-trace-vs-builder`, 2026-07-07). It is NOT the code that builds an
XROUT request. Do not treat "getMagic" as the failing call.

---

## 2. The POSU startup sequence and where the error path forks (Task 2)

[V] `census2.py`: the whole linked ENNS0 image has **IOX=0, IOXT=0** and exactly
**MON 200 x2** and **MON 255 x10**. So:
- ENNS0 never touches the controller status/control register directly.
- All XMSG goes through **two** MON 200 sites (030230 `SAT 0;MON 200`, 030233
  `SAT 1;MON 200`) = a **shared XMSG wrapper**; the XMSG subfunction (XFOPN, XFWDF,
  XSCRS, XSNAM, ...) is a **runtime parameter**, not encoded in the MON operand.
  => You **cannot** tell statically whether XSCRS is issued; the "no XSCRS"
  conclusion is from the RUNTIME MON-200 trace, not the code.
- All controller I/O goes through the **10 MON 255B PIOCM wrappers** (`READPIO,
  SEGLOAD(T=4), UNLOAD, START_P(T=6), STOP_PI(T=7), RES_SLO, REL_SLO, SEND_KI(T=7),
  REC_KIC(T=3), INT2GET`) @ 032703-033147 (decoded in
  `ENNS0-PIOCM-START-FINDINGS-2026-07-23.md`).

[V] The POSU startup body is a linear **staged** sequence at **030200-030710+**.
Decoded MON calls in order: `MON 135` (030222), `MON 200`x2 (030230/030233,
XMSG open/setup), `MON 322` (030375), `MON 124` (030427), `MON 125` (030472),
then a **retry/poll loop @ 030657-030700** (`JMP *-17` backward, retry count staged
via `SAA 4`), then continues toward the `SEGLOAD -> START_P` controller-start
substeps and finally the XSCRS name registration.

[V] Each stage follows the PLANC pattern
`<do op> ; SKP IF DB UEQ SP ; JPL I *nn (error handler) ; ... ; STF *147 (next-stage)`.
On a bad status a stage branches to an error handler that stores into the POSU
error context and **skips the remaining stages** - including the later XSCRS name
registration. [I] Therefore any earlier stage failing (XMSG setup, MON 322/124/125,
or the controller SEGLOAD/START) aborts POSU before the server name is created.

**[LIMITATION - PLANC indirect calls]** Byte-exact tracing of *which* stage forks
to *which* message index, and the exact branch that skips XSCRS, is **not reliably
decodable** with the minimal disassembler: the staging uses `JPL I *nn` through
literal/pointer cells interleaved with code (e.g. 030777->030416, 031000->030277),
and the disassembler cannot separate those pointer words from instructions. So the
stage ordering above is [V] from the MON opcodes and the loop structure, but the
precise "status code N -> message index M -> skip XSCRS" edge is [I].

---

## 3. Is there a controller-interrupt / kick wait that times out? (Task 3)

[V] **The START readiness gate is a memory-mapped poll, not an interrupt wait.**
From the kernel driver `RP-P2-PIOC.NPL` `PISTA` (MON 255B fn 6), byte-decoded in
`ENNS0-PIOCM-START-FINDINGS-2026-07-23.md`:
```
114715  PWCR := 60B   (IOXT HDEV+3)   ; HALT AND RESET the controller
114721  PWCR := 0     (IOXT)          ; INITIATE   (the one doorbell)
114724  -3 =: TMR                     ; arm 3-SECOND timeout
        DO  spin ; X:=1002; CALL PREAD  ; poll PIOC DRAM word 1002B
        WHILE A = 0  OD
114737  IF A >< PRKEY THEN error; GO PIRET FI   ; must == 052163B
        ... only THEN write MPIOC/TRIG and ring start doorbell 11B ...
```
[V] So the "wait for controller" is `PISTA` busy-polling word 1002B for
`PRKEY=052163B`, **3-second** timeout. It is done by the **kernel on ENNS0's
behalf** (ENNS0 just calls `START_P`, T=6, and reads back status). It does **not**
wait on INT12 / STATUS bit 2 / IDENT for the START.

[I] The INT12/IDENT/RFT/RIE interrupt feedback is the notification path for the
*later* `PIKIC`/`PIWKI` kick/mailbox traffic, not the START gate. So chasing the
emulator "IDENT clears RIE / STATUS bit 2 never set" bug **for the startup hang**
is a red herring; the startup gate is the PRKEY memory cell.

[V] **"wait 10 sec"** is **not present** in `encos-err-i`. The only verified
controller-wait timeout is PISTA's **3 seconds**. There is an ENNS0-side retry loop
@ 030657-030700, but its exact iteration count/duration could not be byte-resolved
(PLANC indirect, see section 2 limitation). Do not cite a 10-second ENNS0 wait as
fact.

**How a controller-comm failure surfaces to the user:** [I, well-grounded] PISTA
returns a non-OK status (e.g. "interface not started / not initiated / no answer")
-> `START_P` returns that status in T -> the POSU stage that called it forks to the
error path -> POSU prints `"Error during startup."` + the matching PIOC-status
text from the section-1a catalog (one of `"No answer from interface."`,
`"Interface not started."`, `"Interface not initiated."`,
`"Check if RTCOMMON is in interface memory."`) and skips XSCRS. That is exactly the
"POSU error / RTCOMMON / can't talk to server" banner shape the user reported.

---

## 4. The RTCOMMON branch (Task 4)

[V] `"Check if RTCOMMON is in interface memory."` (015153) is **not** an active
runtime check ENNS0 performs. It is a **diagnostic hint string** sitting in the
PIOCM status-message catalog, immediately after
`"Complete interface memory not fixed. Change size."` and
`"Interface not initiated."`. [I] It is the operator hint printed when the kernel
interface driver returns an interface-memory-fault status (the interface DRAM the
banks/RTCOMMON live in is not fixed/initialised). So the RTCOMMON line is a
*consequence* of a PIOCM interface-memory status, not a separate ENNS0 gate that
aborts on RTCOMMON's presence.

---

## 5. Conclusion (Task 5)

- **(a) YES - a controller-communication step gates POSU before name registration.**
  [V] mechanism: ENNS0 has zero direct I/O; it drives the controller only through
  the kernel PIOCM wrappers (MON 255B), and the POSU sequence runs the controller
  start (`SEGLOAD`->`START_P`) as a staged step whose bad status forks to the POSU
  error path and skips the later XSCRS name registration.
- The **specific** gating feedback is the **`PISTA` PRKEY readiness handshake**
  (memory-mapped poll of PIOC word 1002B for `052163B`, 3 s), driven by the
  `PWCR=60B` HALT+RESET / `PWCR=0` INITIATE doorbell - **NOT** the INT12/IDENT/RIE
  interrupt latch. [V]
- Therefore the emulator fix that unblocks the controller-comm gate is
  **`PWCR=60B` must trigger a real 68K reset-vector restart** (so the firmware
  reposts PRKEY), per `ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md` - **not** the
  IDENT/RIE change (that regressed TPE and is unrelated to the START gate).
- **(b)** the RTCOMMON line is a status hint, not an active abort check.
- **(c)** additionally, per the latest run the card reached READY yet ENNS0 still
  did not issue XSCRS -> a **second downstream gate** (name registration / missing
  COSMOS `Define-Network-*`/`DEFINE-LOCAL-SYSTEM` config, or an ENNS0 self-register
  path that defers to configured state) also exists and must be pinned separately.

**Exact evidence anchors (octal):**
- POSU staged sequence with the error forks: **030200-030710** (`encos-err-i`,
  MON 135/200/200/322/124/125 + retry loop 030657-030700). [V structure]
- `START_P` wrapper (T=6 -> PISTA): **033124-033136**. [V]
- PIOCM status message catalog incl. RTCOMMON: **014642-015250**. [V]
- Kernel PISTA PRKEY poll: `RP-P2-PIOC.NPL` @ 114703-115103, TMR=-3, PRKEY=052163B.
  [V]

---

## Concrete next lead (to make the [I] edges [V])

DAP-trace the live run (breakpoint the shared MON 200 wrapper @ 030230/030233 and
`START_P` @ 033124), and after each POSU stage log the returned **T status** and
watch whether execution reaches an XSCRS-parameter MON 200 or diverts into the POSU
error store (`POSUERR` @ 001756). Specifically: does `START_P`/PISTA return OK
(PRKEY read) in the current build, or time out? That single trace disambiguates
"controller-comm gate still failing" vs "controller OK, downstream name gate" with
certainty - the static PLANC indirect staging cannot.

Findings file:
`C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Ronny-NDInsight\b17a7474-33c0-4d7f-b9cb-f921c3ad419b\scratchpad\ENNS0-POSU-XROUT-ERROR-GATE-2026-07-23.md`
