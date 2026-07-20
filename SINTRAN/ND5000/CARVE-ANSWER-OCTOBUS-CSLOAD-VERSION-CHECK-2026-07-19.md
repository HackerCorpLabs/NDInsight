# Carve answer: the CS-load "VPARP(0x800)" check and "Wrong microprogram" (2026-07-19)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-OCTOBUS-CSLOAD-VERSION-CHECK-2026-07-19.md`
**Answers the octobus CS-load blocker:** `> Loading Control Store / Error when loading Control Store. / Microprogram error: Wrong microprogram`, with the decoded loop `STOPMIC(034B) -> LPARP(021B,ptr=0x00000800) -> VPARP(022B) -> CPURES(071B) -> repeat`.
**Builds on (do not re-derive):**
- `CARVE-ANSWER-OCTOBUS-WRONG-MICROPROGRAM-2026-07-19.md` (OCB fault 202/203 = the string source)
- `CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md` (VPARP = memory echo; state model)
- `CARVE-ANSWER-OCTOBUS-ACCP-SELFTEST-CSLOAD-2026-07-18.md`

**Evidence grade:** **[V]** byte/manual/symbol-cited · **[I]** inferred · **[OPEN]** not carved.

---

## VERDICT

**`0x800` is NOT a microprogram-version cell, and VPARP is NOT a version read. The premise
"the monitor reads the micro VERSION via VPARP at window offset 0x800" is REFUTED by the manual.**

`VPARP` (022B / 0x12) is a **pure parameter-pointer self-consistency echo**: SINTRAN writes an
arbitrary 32-bit word into the parameter area, then asks the ACCP to read *that same cell* back,
and checks the two are equal. It has **no version semantics and no "wrong microprogram" outcome** —
its only two error codes are `-1` (micro running) and `1` (no pointer given). [V, manual §5.3.16]

Two independent facts follow:

1. **The value VPARP must return is not a constant and not a version.** It is *exactly the 32-bit
   word SINTRAN just wrote at `base+ptr`*, returned **most-significant-byte-first**. Echoing raw MPM
   memory is the correct strategy; the reason our echo of `0x65969B49` is rejected is that
   `window_base+0x800` in the emulator is **not the cell SINTRAN wrote its test word to** (stale
   control-store data is sitting there) — a pointer-address / byte-order / write-capture mismatch,
   **not** a wrong version. [V premise; [I] on which of the three mismatch causes]

2. **"Microprogram error: Wrong microprogram" is a different mechanism entirely** — it is the ND-5000
   OCB **FaultType 202B/203B** (reporter = MicroProgram) that the emulated ND-5000 side *sends inbound*
   to the ND-100, decoded and printed by SINTRAN's `5OMBREAD` -> `9FLER`. SINTRAN performs **no**
   version compare; it prints what the ND-5000 sent. A fatal 203B also calls `XRSTARTALL`, which
   restarts the whole ND-500 init — **that is what makes the STOPMIC/LPARP/VPARP/CPURES prologue LOOP.**
   [V, MP-P2-N500.NPL:3459-3519 + prior WRONG-MICROPROGRAM carve]

So there are **two potentially-separate bugs stacked**: (a) VPARP echo returning the wrong cell, and
(b) the emulated ND-5000 emitting a 202/203 OCB fault. Fixing (a) alone will not silence "Wrong
microprogram" if (b) is firing; the loop is driven by (b)'s `XRSTARTALL`.

---

## Q1 — Where "Wrong microprogram" is decided, and the comparison

**Not in J04, not in a VPARP compare, not in any version compare on the ND-100 side.** [V]

- The literal string is **absent from the J04 message pool** (`nd-500-mon-j04.prog.md:1462-1477`
  lists the CS-load siblings `$CONTROL STORE NOT SUCCESSFULLY LOADED`, `$ILLEGAL MICRO INSTRUCTION
  CODE` — "Wrong microprogram" is not among them). [V]
- It exists only as an **OCB FaultType** the ND-5000 microcode emits: `202B` (NotFatal) / `203B`
  (Fatal), body `{Current CPUModel, My CPUModel, MicroprogramVersion}`, ErrorReporter `1 =
  MicroProgram`. [V, prior WRONG-MICROPROGRAM carve: microcode `CPU_READ @017130`, manual
  ND-05.017.01 EN:10930-11007]
- **The decode+print path, byte-cited** (`MP-P2-N500.NPL`, routine `5OMBREAD @146556`):
  - `146575  X.ETYPE=:D SHZ -10=:CSTS` — FaultType = `ETYPE >> 8` (202/203).
  - `146575  A:=D/\377=:CMICP` — ErrorReporter = `ETYPE & 0377` (`1` = microprogram).
  - `146643  IF CMICP=1 AND CSTS=200 OR A=201 THEN ...` — microprogram-reporter branch.
  - `146627  ... T:="MPFATAL"; LBYT; IF A><0 THEN CSTS\/N5SECCODE; CALL XRSTARTALL` — a **fatal**
    fault (`MPFATAL` table `@146550`) triggers **`XRSTARTALL`** = restart everything.
  - `147004  CALL 9FLER` — hands the record to the error printer, which renders "Microprogram
    error:" (reporter) + "Wrong microprogram" (fault 202/203).
- **The station gate** `146565 IF MOCTSTATION >= FN5DEST AND <= LN5DEST` confirms it only fires for a
  message *sourced from an ND-5000 station (70B-73B)* — i.e. it is INBOUND from the emulated ND-5000,
  never something SINTRAN computes. [V]

**Comparison that precedes the string:** it happens **inside the ND-5000 microcode** (`CPU_READ`):
`TYP,HW` (ACCP/backplane-reported model, "Current CPUModel") vs loaded-CS **word 7** `CPUMODEL` ("My
CPUModel"), AND word-1 version vs a per-model `[min..max]` range. Mismatch -> 202B/203B. [V, prior
carve]. **There is no ND-100-side comparison.**

---

## Q2 — What VPARP reads and what the monitor compares

**VPARP returns the first 32-bit word of the parameter area, i.e. `memory[parameter_pointer]`, and
the monitor compares it to the 32-bit word it wrote there itself.** It is a self-consistency test,
not a magic-constant test. [V, manual]

Manual, byte-cited:
- **§5.3.16 (line 4352-4364):** "This command is used to verify that the ND-120 and the ACCP agree on
  where the parameter area is. **Before the command is given, the ND-120 writes a 32-bit word in the
  parameter area. The ACCP reads and returns the word from its parameter area, and the ND-120 should
  then check if they are equal.** Messack parameters: Test pattern (4 byte). Messnak: `-1` illegal
  when microprogram is running; `1` no parameter pointer is given." [V]
- **§5.3.10 (line 4170-4172):** LPARP "is used during initialization to specify the location of the
  parameter area in shared memory. The ND-120 can check that the ACCP agrees ... by writing a 32-bit
  word in the parameter area and sending ... VERIFY PARAMETER POINTER. **The ACCP then reads its
  parameter area and returns the first 32-bit word found there.**" [V]
- **There is NO "read microprogram version" ACCP command in the entire §5.3 catalog** (index lines
  225-267: LSYSPAR, LPARP, VPARP, LOCSD/LOCSM, DCSD/DUCS, START/STOP/CONT/RESTMIC, ALIVE, LMAR/LMIR/
  RMIR, ... CPURES — no version read). Version is read a different way (Q3). [V]

**Is `0x800` the version cell? NO.** [V/I]
- Byte-wise the CS-image **version** is **word 1** (`LARG` operand) and **CPUMODEL** is **word 7** of
  the 128-bit-word image — both in the *header* (`docs\MC\README.md:54-98`, prior carve). Offset
  `0x800` = 2048 bytes = the **128th** 16-bit word into the staged block — nowhere near the header.
  So `0x420800` holding CS microword data (`0x65969B49`) is consistent with `0x800` being **inside the
  LOCSM staging buffer**, not a version field. [I strong]
- Per §5.3.10/§5.3.18, the parameter pointer is exactly **where the LOCSM block is built** (word 0 =
  µI word count, word 1 = CS address, then N x 128-bit µwords, then checksum addend). VPARP verifies
  *that pointer's addressing* before LOCSM runs. `0x65969B49` is a µword byte-quad, i.e. the buffer
  already holds CS content — so VPARP's fresh test word either was never (re)written at the cell the
  emulator reads, or landed at a different byte address. [I]

**Exact value our VPARP must return to pass:** the **same 4 bytes SINTRAN most-recently wrote at
`base+ptr`, MS-byte-first** (§5.3.11:4194 "Direct parameters with several bytes always have the most
significant byte first"). **No fixed constant, no version.** [V]

---

## Q3 — CS-image-sourced vs micro-reported vs ACCP-reported

**None of the three for the VPARP echo** — the VPARP reply is a self-consistency echo of SINTRAN's own
scratch write (ACCP-read-back of ND-100-written memory). [V]

For completeness, the actual **microprogram version** (the `027232B` value) travels on a **separate
path**, never over an ACCP command:
- **Micro-reported via the mailbox:** `3RMICV` (MICFU = 1) returns `version = 027232B` (CS word-1
  constant) + `CPUPAR = 001741B` over shared memory. [V, prior carves / `MSG_VERSRD @015330`]
- **Surfaced to the operator via MON60**, not VPARP: `FRMICV` (MON60 func **057B**, READ MICRO PROGRAM
  VERSION, `5P-P2-MON60.NPL:2112`) and `FGCPUTYPE` (func **170B**, `:2216`) are **pass-through copies**
  (`CALL STDS0 ... GO FAR RET5`) of the value the ND-500 System Monitor already holds — **they do no
  comparison.** [V]
- The **CS image is the ultimate source** of both the version (word 1) and the model (word 7); the
  microcode reads its own loaded store and either reports it (3RMICV) or faults on it (CPU_READ). [V]

So for the emulator: **do NOT synthesize a version on the VPARP reply.** Drive the version only on
`3RMICV`, from the CS image (word 1 = `027232B` / `0x2E9A`, CPUPAR `001741B` for model 8). The VPARP
reply must be a faithful memory echo. [V/I]

---

## Q4 — LPARP/VPARP parameter model at CS-load

- **LPARP (021B / 0x11), §5.3.15 (line 4341-4348) [V]:** direct params = **Parameter pointer, 4 bytes,
  MS byte first**; "The address of the parameter area in the MFbus memory is given"; Messack params:
  none. The emulator **stores** the 4 bytes as `_parameterPointer`.
- **VPARP (022B / 0x12) [V]:** no params in; Messack = **4-byte test pattern** = `memory[ptr]`,
  MS-byte-first.
- **Pointer UNITS — [OPEN], and this is the prime suspect.** The manual says "address of the parameter
  area in the MFbus memory" but does not pin the scaling. Two candidate reads of `0x00000800`:
  - **byte offset** into the 5MPM window -> `0x420000 + 0x800 = 0x420800` (what the emulator does now),
  - **16-bit-word index** (both ND-120 and ACCP are 16-bit, §5.3.11:4194 "organized in 16-bit words")
    -> byte offset `0x800*2 = 0x1000` -> `0x421000`,
  - or MAR-style `(addr & 0xFFFFFF)*2` scaling (the nd-500-bus-interface convention).
  The earlier trace's VPARP at `ptr=0x00018000` reportedly **passed** while `ptr=0x0800` fails; a plain
  byte-offset echo would make BOTH pass or BOTH fail for the same reason, so the differing outcome is a
  signal that either the scaling differs across the two call sites or the `0x800` cell is being clobbered
  by the CS staging DMA before VPARP reads it. **Resolve by matching the emulator's `base+ptr` byte
  address to the exact address SINTRAN writes its VPARP test word to** (capture the write, echo that
  cell). [OPEN — the octobus CS-load command *sender* is not in the carved L07 NPL; see below]

**Why "what SINTRAN compares against" cannot be byte-cited here [OPEN]:** the L07 NPL tree contains the
octobus **receive** side (`5OMBREAD @146556`, alive/reset senders `@147133/147544`) but **no LPARP /
VPARP / LOCSM / STARTMIC command SENDER** anywhere (`grep \bLPARP\b|\bVPARP\b|\bLOCSM\b` over
`NPL-SOURCE\NPL` = zero hits). The CS-load ACCP sequencer that issues the observed frames lives in a
module not in this carve (a separate ND-5000 microcode-loader / downloader). So the *fact* that VPARP is
a self-consistency echo is [V] from the manual, but the *exact SINTRAN compare* on the return is
[OPEN-not-carved]. It is a self-consistency equality per the manual, not a constant.

---

## Bottom line for the emulator (implement in one edit)

**Do NOT return a version at ptr=0x800.** Two things, in priority order:

1. **VPARP echo correctness.** Make the VPARP Messack return the **exact 4 bytes SINTRAN wrote at the
   parameter cell, MS-byte-first**, reading the *same byte address* SINTRAN wrote — i.e. resolve the
   `LPARP` pointer with the correct scaling (test byte-offset `0x420800` vs word-index `0x421000`) and
   make sure the value is captured *at VPARP time* (after SINTRAN's test-word write, before any CS-stage
   DMA overwrites it). If `_parameterPointer` was never set, return **Messnak 1** (`[0xFF,0x01,0x00]`,
   high byte MFNACK). This is a faithful-echo fix, never a synthesized constant. [V manual]

2. **Suppress the OCB 202/203 "Wrong microprogram" fault** — this is what actually prints the string and
   loops the prologue (`XRSTARTALL`). The emulated ND-5000 servicer/microcode must model `CPU_READ` as
   **passing** for the configured model: the model byte it reports as "Current CPUModel" MUST equal the
   loaded CS-image **word 7** ("My CPUModel"; 5800 = `0x38`/`0o70`), and the CS word-1 version
   (`027232B`) must sit in that model's range (`027174..027337` for model 8 — it does). Never emit an
   inbound multibyte from station 70B whose `ETYPE` high byte is `202B`/`203B` (0x82/0x83) with low byte
   `1`, or `5OMBREAD` will decode and print "Wrong microprogram" and restart init. [V/I]

**Decisive diagnostic to tell which bug is live:** watch the inbound frames (`SnapshotInboundFrames`).
If a station-70B multibyte with body FaultType `202/203`, reporter `1` is present, bug (2) is firing and
is the true source of the string — fix it first. If VPARP is simply being NAK'd/mismatched with no such
inbound fault, bug (1) is the blocker.

---

## Honest [OPEN] items

- **The octobus CS-load command SENDER is not carved.** No LPARP/VPARP/LOCSM sender in the L07 NPL; the
  exact SINTRAN compare on the VPARP return, and the exact pointer scaling, are therefore [OPEN]. The
  self-consistency-echo *semantics* are [V] from the manual. [OPEN]
- **Pointer units (byte vs 16-bit-word vs MAR-scaled).** [OPEN] — resolve empirically against SINTRAN's
  own test-word write address.
- **Why 0x18000 passed but 0x800 fails.** Consistent with `0x800` being inside the CS staging buffer
  (clobbered) or a scaling difference; not proven. [OPEN]
- **Whether both bugs are live, or only the OCB fault.** The loop's `XRSTARTALL` origin (`5OMBREAD`
  fatal branch) makes bug (2) the more likely driver; the requester's "VPARP echo rejected" reading is a
  live-harness interpretation, not carved. Use the diagnostic above. [OPEN]
- **`202B` (NotFatal) vs `203B` (Fatal) selection** in the microcode is [I] (203B = model mismatch is
  firm; only 203B/fatal reaches `XRSTARTALL` via `MPFATAL`). [I]

---
*Sources — MANUAL (authoritative for VPARP semantics, no CARVE contradicts it):
`Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md` §5.3.10 (l.4168-4176), §5.3.11
(l.4186-4247, esp. l.4194 MS-byte-first, l.4226 Messack shape), §5.3.15 (l.4341-4348 LPARP), §5.3.16
(l.4352-4364 VPARP), §5.3.18 (l.4390-4416 LOCSM), command index (l.225-267 — no version-read command).
CARVE: `MP-P2-N500.NPL` `5OMBREAD @146556` (l.3459-3519: ETYPE->CSTS/CMICP, MPFATAL/XRSTARTALL, 9FLER);
`5P-P2-MON60.NPL` `FRMICV @035441` (l.2112, func 57 pass-through), `FGCPUTYPE @035670` (l.2216, func 170);
`nd-500-mon-j04.prog.md:1462-1477` (J04 pool, string absent); `057B-MPVER/README.md`. CROSS-REF:
`CARVE-ANSWER-OCTOBUS-WRONG-MICROPROGRAM-2026-07-19.md` (OCB 202/203 = CPU_READ),
`CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md` (VPARP echo, state model).*
