# Carve answer: ACCP control-store-load → microprogram-start protocol over the octobus (2026-07-18)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md`
**Answers:** `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-OCTOBUS-CSLOAD-PARAMPTR-MICROSTART-2026-07-18.md`
**Builds on (do not re-derive):**
- `CARVE-ANSWER-OCTOBUS-CPU-PRESENCE-2026-07-18.md` (5ALIVE / CMSYSPAR ack)
- `CARVE-ANSWER-OCTOBUS-ACCP-SELFTEST-CSLOAD-2026-07-18.md` (RTEST poll, terminate, CS-load family)

**Evidence grade:** **[V]** byte/manual/symbol-cited · **[I]** inferred · **[OPEN]** not carved.

---

## VERDICT

The two new failures are **not** CS-load bugs — they are two ACCP commands that **read back state**, which the
blind canned Messack answers *wrongly*:

1. **"Verification of ACCP parameter pointer failed"** — command **VPARP (022B / 0x12, §5.3.16)** must return,
   in its Messack, the **32-bit test word that SINTRAN just wrote into the MFbus parameter area** at the pointer
   given by the preceding **LPARP (021B / 0x11, §5.3.15)**. A canned zero-length/zero Messack returns the wrong
   word → SINTRAN's compare fails. **The emulator must read the word back out of MPM shared memory and echo it.**
   This is the ONE place on the whole load path where a canned answer cannot work. [V]

2. **"ACCP was terminated; Microprogram is running"** — command **CMALI / ALIVE CHECK (037B / 0x1F, §5.3.26)**
   encodes running-state by **Messack = microprogram IS running, Messnak error 7 = NOT running**. After the
   `244B` TERMINATE, SINTRAN expects the microprogram to be **stopped**, i.e. it expects **Messnak-7**. Our canned
   **Messack** told it "still running" → the timeout message. **This reverses the advice in the prior selftest
   carve** (which said "Messack is the clean answer to ALIVE"): that is correct *only* while the microprogram is
   actually running. **The emulator must track a `microprogram-running` flag and answer CMALI from it.** [V]

Downstream ("Loading Control Store / Error when loading Control Store") is a *consequence*: because VPARP fails,
the parameter pointer is never accepted, so **LOCSM (load control store via memory) never runs**, and the
monitor reports the CS as not loaded. Fix VPARP + CMALI and the load/start sequence can proceed. [V/I]

---

## 1. Frame decode of the live trace (`sintran-octobus-frames-octobus-fullflow.txt`)

Wire envelope (byte-verified, OCTOBUS-ND100-ND5000-REFERENCE.md §3 + SINTRAN-OCTOBUS-MESSAGE-CATALOG.md §1):

```
SOMB  = C-frame, info = 0x30 | OMD      (0x8133 → OMD 3, the ACCP command OMD)
srcOMD= data byte  (reply-to OMD; 0x30-family replies go here)
count = data byte  (body length in bytes)
body  = count data bytes (byte[0] = command code, then params, MS byte first)
EOMB  = C-frame, info = 0x20 | OMD      (0x8123 → OMD 3)
data byte on the wire = 0x01xx (C=0 data frame, low byte = value)
emergency = C+E frame, info = 2xxB     (0x81A1=241B, 0x81A2=242B, 0x81A4=244B)
```

**Decoded command stream (in order):**

| # | cpos | Frame(s) | Meaning |
|---|------|----------|---------|
| 1 | 0 | `81A1` | Emergency **241B CMMACLE** — master clear (ACCP+CPU, starts selftest) |
| 2 | 0 | `81A2` | Emergency **242B CMACONT** — continue ACCP |
| 3 | 546 | SOMB, `03 07 0E 01 03 00 00 00 00`, EOMB | **CMSYSPAR 016B** (LSYSPAR), srcOMD=3, N100IDENT=1, 5OMDNO=3 — the alive handshake (CON5IDENT) |
| 4 | 546 | SOMB, `04 01 30`, EOMB | **RTEST 060B / CMRSE**, srcOMD=4 — read self-test status (poll) |
| 5 | 546 | SOMB, `04 07 0E 01 03 00 00 00 00`, EOMB | **CMSYSPAR 016B** again, srcOMD=4 |
| 6 | 546 | `81A4` | Emergency **244B CMATE** — TERMINATE ACCP |
| 7 | 546 | SOMB, `04 01 1F`, EOMB | **CMALI 037B / ALIVE CHECK**, srcOMD=4 — "did terminate stop the microprogram?" |
| 8 | 714 | SOMB, `04 01 32`, EOMB | **DISKICK 062B / CMDIS** — disable kicks (so ACCP cmds work) |
| 9 | 714 | SOMB, `04 01 1C`, EOMB | **STOPMIC 034B / CMSTO** — stop microprogram |
| 10 | 714 | SOMB, `04 01 39`, EOMB | **CPURES 071B / CMCPU(RES)** — reset the ND-5000 CPU |
| 11 | 714 | SOMB, `04 01 32`, EOMB | DISKICK 062B again |
| 12 | 714 | SOMB, `04 01 1C`, EOMB | STOPMIC 034B again |
| 13 | 714 | SOMB, `04 05 11 00 01 80 00`, EOMB | **LPARP 021B / CMLPA** — Load Parameter Pointer = **0x00018000** (4 bytes, MS first) |
| 14 | 714 | SOMB, `04 01 12`, EOMB | **VPARP 022B / CMVER** — Verify Parameter Pointer ← **fails** |
| 15 | 873 | (steps 8-14 repeat) | SINTRAN retries the whole init prologue because VPARP failed |

So the observed "0x30,0x1F,0x39,0x1C,0x12,0x11,0x32,0x05,0x07,0x0E" from the request are: `0E`=CMSYSPAR command,
`05`/`07`=body-length bytes, and the real distinct **command codes** are
`30,1F,32,1C,39,11,12` = **060B, 037B, 062B, 034B, 071B, 021B, 022B**. [V]

---

## 2. Full ACCP command table for the load/start path

Codes from `N500-SYMBOLS.SYMB.TXT` (CM* symbols, 5-char truncated) cross-checked with ND-05.020.01 §5.3.
The "Description" column in `OCTOBUS-PROTOCOL-REFERENCE.md §3` is a set of unverified guesses — the manual
command name governs.

| Cmd (§) | Octal / hex | Symbol | Dir params | Reply (Messack) | Messnak codes | Grade |
|---|---|---|---|---|---|---|
| **CMSYSPAR / LSYSPAR** (§5.3.13) | 016B / 0x0E | CMSYS | 6 bytes sys-params (err St/OMD, host St/OMD, opt) | ack, none | — | [V] |
| **LPARP** Load Parameter Pointer (§5.3.15) | 021B / 0x11 | CMLPA | **4-byte MFbus pointer** | ack, none | — | [V] |
| **VPARP** Verify Parameter Pointer (§5.3.16) | 022B / 0x12 | CMVER | none (SINTRAN pre-writes a 32-bit word at pointer) | **ack + 4-byte test pattern (word read back from param area)** | −1 running, 1 no-pointer | [V] |
| **LOCSD** Load CS Directly (§5.3.17) | [OPEN] | — | CS addr(2) + µinstr(16) + cksum addend(2) | ack, none | −1 running, 4 checksum | [V spec] |
| **LOCSM** Load CS Via Memory (§5.3.18) | 052B / 0x2A (CMLDC/CMLDM cluster) [I] | CMLDC/CMLDM | none; block in MFbus mem (N, CS addr, N×128b, addend) | ack, none | −1 running, 1 no-pointer, 4 checksum | [I code] |
| **STARTMIC** Start Microprogram (§5.3.23) | 033B / 0x1B | CMRUN | CS start address (2 bytes) | ack, none → **micro now RUNNING** | −1 running, 9 CS-not-init | [I strong] |
| **STOPMIC** Stop Microprogram (§5.3.24) | 034B / 0x1C | CMSTO | none | ack → micro NOT running | −2 kicks-on, 0 not-started | [V code] |
| **CONTMIC** Continue Microprogram (§5.3.25) | 035B / 0x1D | CMCON | none | ack → micro RUNNING | 0 not-started | [I strong] |
| **ALIVE / CMALI** (§5.3.26) | 037B / 0x1F | CMALI | none | **ack = RUNNING** | **7 = NOT alive (stopped)** | [V] |
| **ENKICK** Enable Kicks (§5.3.47) | 061B / 0x31 | CMENK | none | ack | — | [V code] |
| **DISKICK** Disable Kicks (§5.3.48) | 062B / 0x32 | CMDIS | none | ack | — | [I strong] |
| **CPURES** Reset CPU (§5.3.49) | 071B / 0x39 | CMCPU | none | ack | — | [V code, XRS5CPU] |
| **RESTMIC** Restart Microprogram (§5.3.46) | 036B / 0x1E | CMRES | [OPEN] | ack → micro RUNNING | — | [I] |
| **TERMINATE** (§5.3.50) | emergency **244B** | CMATE=044B | — (hardware-decoded) | **no reply** → micro STOPPED, ACCP idle loop | — | [V] |
| **RTEST** Read Self-test (§5.3.54) | 060B / 0x30 | CMRSE | none | ack + 2-byte selftest status (0=OK) | — | [V] |
| Master clear | emergency **241B** | CMMAC=041B | — | no reply | — | [V] |
| Continue ACCP | emergency **242B** | CMACO=042B | — | no reply | — | [V] |

**Messnak global error codes (§5.3.11:4240-4256) [V]:**
`−2` illegal-when-kicks-enabled · `−1` illegal-when-micro-running · `0` micro-not-started · `1` no-parameter-pointer ·
`2` illegal-word-count · `3` illegal-address · `4` checksum-error · `5` CS/CC HW-error · `6` undefined-command ·
`7` **not-alive** · `8` memory-error · `9` CS-not-initialized.

**Messnak wire body (§5.3.11:4228-4232) [V]:** `[byte0 = error code][byte1 = ASTS low][byte2 = ASTS high]`.
**Messack wire body (§5.3.11:4218-4226) [V]:** single ack byte, then any return params, in the SAME multibyte
message ("Messack is sent as a multibyte message even if there are no return parameters").

---

## 3. Ordered protocol sequence: selftest → running swapper (with SINTRAN's check at each step)

```
STAGE A  PRESENCE / SELFTEST  (solved in the two prior carves)
  241B master-clear ─┐  242B continue-ACCP ─┘   (boot / @nd-500 entry)     no reply
  CMSYSPAR 016B (alive)   → ACCP must ACK → 5OMBREAD sets 5ALIVE            [CPU-presence carve]
  RTEST  060B (poll ~1 min) → Messack + status 0x0000                       [selftest carve]

STAGE B  TERMINATE / STATE PROBE                                            ← NEW failure #2
  244B TERMINATE          (hardware-decoded, no reply)  → micro-running := FALSE
  CMALI 037B ALIVE CHECK  → SINTRAN expects micro STOPPED
       correct answer = Messnak 7 (Not alive).  Messack here = "Microprogram is running" timeout.

STAGE C  ACCP INIT PROLOGUE (get to a clean, stopped, kicks-off state)
  DISKICK 062B  → Messack           (enable ACCP hw commands: while kicks on they are refused)
  STOPMIC 034B  → Messack           (micro-running := FALSE)
  CPURES  071B  → Messack           (reset ND-5000 CPU)
  DISKICK 062B  → Messack           (repeated; monitor re-asserts stopped/kicks-off)
  STOPMIC 034B  → Messack

STAGE D  PARAMETER POINTER HANDSHAKE                                        ← NEW failure #1
  (SINTRAN writes a 32-bit test word into MFbus memory at the pointer)
  LPARP 021B ptr=0x00018000 → Messack    (STORE the 4-byte pointer)
  VPARP 022B                → Messack + the 32-bit word READ BACK from param area
       SINTRAN compares echoed word to what it wrote. Mismatch = "Verification ... failed".

STAGE E  CONTROL-STORE LOAD  (only reached once VPARP passes)               [selftest carve §Q2]
  LOCSM (block in MFbus mem: N words, CS addr, N×128-bit µwords, checksum addend) → Messack
       (or per-word LOCSD 052B/0x2A). Messnak 4/5/−1 or missing ack → EILOCS 2103B
       "Error in loading Control Store" (5P-P2-MON60.NPL:107).

STAGE F  MICROPROGRAM START
  STARTMIC 033B, CS start addr → Messack  → micro-running := TRUE
  (CMALI 037B now correctly returns Messack "running")

STAGE G  START SWAPPER  (nd-500-mon: LOAD-SWAPPER SWLOD 7B, START-SWAPPER STSWP 54B)
  Activate process 0 (the swapper): mailbox message + octobus kick N100KICK(1).
  Answered by the activate/answer engine (Nd500MicrocodeServicer), NOT the ACCP command library.
  [OPEN in detail — this is the 3WREG/LOAD-SWAPPER path noted in the nd-500-bus-interface skill.]
```

**Where the 244B terminate fits (request Q5):** it is Stage B — a *pre-load* "stop whatever is running and
prove it stopped" probe, BEFORE the CS load. "ACCP was terminated; Microprogram is running" means exactly what
the words say: SINTRAN terminated the ACCP and then the ALIVE CHECK *unexpectedly reported the microprogram
still running* — because our CMALI lied (canned Messack). [V]

---

## 4. LPARP / VPARP and CMALI-state — byte-cited detail

### LPARP (§5.3.15, manual line 4341-4348) [V]
- Direct params: **Parameter pointer, 4 bytes**, MS byte first (§5.3.11:4194).
- "The address of the parameter area in the MFbus memory is given." Messack params: none.
- Trace value: `00 01 80 00` = **0x00018000**. The emulator **stores** this; it is the base of the parameter
  area in shared MFbus/MPM memory.

### VPARP (§5.3.16, manual line 4352-4364) [V]
- "Before the command is given, the ND-120 writes a 32-bit word in the parameter area. **The ACCP reads and
  returns the word from its parameter area**, and the ND-120 should then check if they are equal."
- **Messack params: Test pattern (4 bytes).** Messnak: −1 (running), 1 (no parameter pointer given).
- Therefore the ACCP's job is a pure **echo of MFbus memory[pointer]**. The emulated station already holds the
  MPM RAM (`_mpmRam`/`_mpmSize`, attached via `AttachSharedMemory` /`SetMpmMemory`), so it CAN read the 4 bytes
  at the LPARP pointer and return them. This is the required fix — there is no canned value that works, because
  SINTRAN chooses the test word.
- **[OPEN]:** the exact mapping of the 32-bit pointer 0x00018000 into the emulator's MPM window (word vs byte
  address; whether the low 24 bits index MPM the same way MAR does — `(addr & 0xFFFFFF)*2` byte base per the
  nd-500-bus-interface skill). Match it to however SINTRAN's MON60 writes the test word; if the two sides read
  the same shared cell, the echo verifies regardless of the exact scaling.
- **[OPEN]:** what SINTRAN compares against. The manual says "check if they are equal" — i.e. it compares the
  returned 4 bytes to the 4 bytes it wrote. It is a self-consistency check, not a magic constant.

### CMALI / ALIVE CHECK (§5.3.26, manual line 4592-4601) [V]
- "Checks if the microprogram is running by polling the ALIVE signal ... If ALIVE is true, **Messack** is
  returned; otherwise **Messnak** is returned." Messnak error code: **7 Not alive.**
- ALIVE = a flip-flop clocked by the MIR clock — i.e. **it is set iff the microprogram is executing.**
- State machine the emulator must model:
  - reset / master-clear (241B) / TERMINATE (244B) / STOPMIC (034B) → **running = FALSE**
  - STARTMIC (033B) / CONTMIC (035B) / RESTMIC (036B) → **running = TRUE**
  - CMALI (037B): `running ? Messack : Messnak(7)`

---

## 5. Bottom line for the emulator (`OctobusND5000Station`)

**State the station must now track (in addition to the CPU-presence/selftest state already added):**

| State var | Set by | Cleared by | Read by |
|---|---|---|---|
| `_parameterPointer` (32-bit) | **LPARP 021B** (store the 4 direct-param bytes, MS first) | master-clear | VPARP |
| `_microRunning` (bool) | **STARTMIC 033B**, CONTMIC 035B, RESTMIC 036B → true | **TERMINATE 244B**, **STOPMIC 034B**, master-clear 241B, reset → false | **CMALI 037B** |
| `_kicksEnabled` (already exists) | ENKICK 061B | DISKICK 062B | (gates hw commands) |

**Per-command action (reply on the command's `srcOMD`, from station 70B, as a multibyte message):**

- **CMSYSPAR 016B** — (already) ACK so 5OMBREAD sets 5ALIVE. Reply on srcOMD (=3 for this frame).
- **RTEST 060B** — Messack + 2 status bytes `00 00` (selftest OK). srcOMD=4.
- **DISKICK 062B / ENKICK 061B** — Messack (set `_kicksEnabled`). srcOMD=4.
- **STOPMIC 034B** — Messack; `_microRunning = false`.
- **CPURES 071B** — Messack (reset CPU state; may clear `_microRunning`).
- **LPARP 021B** — Messack; **store** the 4 param bytes as `_parameterPointer` (0x00018000 in the trace).
- **VPARP 022B** — **Messack + 4 bytes = the 32-bit word read from MPM memory at `_parameterPointer`.**
  This is the fix for "Verification of ACCP parameter pointer failed." If `_parameterPointer` was never set,
  return **Messnak 1** (no parameter pointer).
- **CMALI 037B** — `_microRunning ? Messack : Messnak(7)`. **Answering Messnak-7 after the 244B terminate is
  the fix for "ACCP was terminated; Microprogram is running."**
- **244B TERMINATE (emergency)** — no reply; reset AIB/AOB buffers; `_microRunning = false`; ACCP → idle.
- **LOCSM / LOCSD (CS load)** — Messack every command (discard payload; do NOT verify checksum on this path).
  Never leave one unanswered and never Messnak, or you get EILOCS 2103B. After the load, keep answering.
- **STARTMIC 033B** — Messack; `_microRunning = true`. (Optionally record the CS start address.)

**Reply-shape reminders (from the manual, [V]):**
- Messack = a multibyte whose body is `[ack byte][return params...]`; ack byte = 0 (MFACK), consistent with the
  working CMSYSPAR ack. For no-param commands the body is a single `00`.
- Messnak = a multibyte whose body is `[error code][ASTS low][ASTS high]` (3 bytes). For CMALI-not-running use
  error code `07`; ASTS bytes may be 0.
- **[OPEN]:** the exact discriminator J04's own OMD-4 receiver uses to tell Messack from Messnak (byte count 1
  vs 3, or the leading byte) is not carved — J04's octobus receive path is not disassembled. The manual wire
  shapes above are authoritative; if a 3-byte `07 00 00` Messnak does not flip J04 to "not running", fall back
  to encoding the nak via the reply frame's status high byte = MFNACK(377B), mirroring 5OMBREAD's CSTS test.

**Canned-answer verdict, updated:** the selftest/terminate/CS-load path passes with canned Messacks **except**
the two state-reading commands: **VPARP must echo memory, and CMALI must reflect the running flag.** Those two
are mandatory; everything else on Stages A–E can stay a canned Messack.

---

## 6. Honest [OPEN] items

- **LOCSM/LOCSD octal codes** — not pinned to a specific CM* symbol by bytes. LOCSM is inferred to be in the
  CMLDC=052B / CMLDM=051B cluster; the trace stops at VPARP so no CS-load command was actually observed on the
  wire yet. Answer any 0x2A/0x29 (and unrecognised load-family) command with Messack until the real code shows.
- **STARTMIC=033B / CONTMIC=035B / RESTMIC=036B** — mapped by the CMRUN/CMSTO/CMCON/CMRES 033-036 cluster +
  manual §-order + "Run/Stop/Continue" semantics; STOPMIC=034B and CPURES=071B and DISKICK=062B are the firmest.
  Not yet seen post-VPARP on the wire (the run stalls before Stage F).
- **VPARP pointer→MPM address scaling** — see §4; principle firm (echo shared memory), exact scaling to verify.
- **J04 error-string byte addresses** — "Verification of ACCP parameter pointer failed",
  "ACCP was terminated; Microprogram is running", "Error when loading Control Store" live in the J04-binary
  message pool (not dumped). The SINTRAN-side status they map to is `EILOCS 2103B` (5P-P2-MON60.NPL:107) /
  `ECSLOAD 2032B` (line 66) for the CS-load messages; the parameter-pointer and alive strings are J04-local. [OPEN addr]
- **Start-swapper (Stage G)** — the activate/kick/mailbox handshake is the Nd500MicrocodeServicer's job, tracked
  separately (LOAD-SWAPPER 3WREG blocker); out of scope for the ACCP command library answered here.

---
*Sources: ND-05.020.01 EN §5.3.9-5.3.57 (`manual\ND-05.020.01 EN ND-5000 Hardware Description.md:4168-5120`);
`N500-SYMBOLS.SYMB.TXT` (CM* codes); `sintran-octobus-frames-octobus-fullflow.txt` (live decode);
`OCTOBUS-ND100-ND5000-REFERENCE.md` §3/§5; `SINTRAN-OCTOBUS-MESSAGE-CATALOG.md` §1; `5P-P2-MON60.NPL`;
`nd-500-mon-j04.prog.md`; prior carves (CPU-presence, selftest-CSload, 2026-07-18).*
