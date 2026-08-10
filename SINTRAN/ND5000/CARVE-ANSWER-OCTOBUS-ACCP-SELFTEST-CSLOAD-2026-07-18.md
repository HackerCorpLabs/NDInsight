# Carve answer: ACCP selftest + Control-Store load + terminate over the octobus (2026-07-18)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-OCTOBUS-ACCP-SELFTEST-CSLOAD-2026-07-18.md`
**Answers:** `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-OCTOBUS-ACCP-SELFTEST-CSLOAD-2026-07-18.md`
**Evidence grade:** **[V]** verified from bytes/manual/symbols, **[I]** inferred, **[OPEN]** not carved.

---

> ## CORRECTION 2026-08-04 - the CS-load half of this answer is INCOMPLETE
>
> This doc describes the **octobus command layer** (LOCSD/LOCSM, Messack) correctly. What it does not
> describe is the **register-level protocol the ACCP actually uses to shift a microword into the
> ND-5000**, which is now implemented and verified against the real `octo.bin`. Five points had to be
> measured on the running firmware:
>
> 1. **The gate is bit 1 OR bit 2** of the `0x330000` latch. Boot uses bit 1 (`0x764E`) exclusively;
>    bit 2 (`0x741E`) is only the console command path.
> 2. **The address is the NINTH gated word**, after the eight 16-bit halves - not a pre-gate write.
> 3. **The completed word must be LATCHED** - the card closes and reopens the gate between the shift
>    and the perform. This doc's own phrase "BUFFERED CI-bit groups" turns out to be literal.
> 4. **Multiple performs happen inside one gate window** - staging must reset after each.
> 5. **`0x440000` must be HELD and echoed** (32-bit round trip), not ignored.
>
> Also: **booting the card never exercises the addressed control-store path.** Boot traffic is all
> command `0x2018` at address 0; only the typed `LOAD-CONTROL-STORE` reaches `0x0018`.
>
> See `ACCP-EMULATION-STATUS-AND-HANDOFF.md` (headline section) and
> `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\src\Devices\Nd5000ControlStoreLink.cs`.

## VERDICT

All three handshakes are **ACCP-firmware (68000 EPROM) command-library exchanges over OMD 3**, not
microcode. The `060B` poll is the ACCP **RTEST / Read Self-test Status** command (symbol `CMRSE=000060`);
its answer is a **Messack multibyte carrying a 2-byte self-test status = 0 (OK)**, sent back on the
source's own OMD (`srcOMD`, =4 here). Control-store load is the ACCP **LOCSD/LOCSM** command family, each
message **Messack**-ed; a missing/negative ack (Messnak) is what makes J04 print "Error when loading
Control Store". The `244B` terminate is the emergency **`EBIT|CMATE(044B)`**; SINTRAN follows it with an
**ALIVE CHECK (`CMALI=037B`) multibyte** and considers the ACCP terminated **only when it receives ANY
reply (Messack or Messnak) to that ALIVE CHECK** — no reply = "Impossible to terminate ACCP after timeout".
The selftest IS a genuine ACCP EPROM self-test, so a **canned "selftest OK" answer is sufficient**.

---

## Q1 — Command `060B` on OMD 3 (the selftest poll)

**What it is [V/I].**
- The wire byte `0x30 = 060B` matches exactly one SINTRAN octobus command symbol: **`CMRSE = 000060`**
  (octal 060) — `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\M06\N500-SYMBOLS.SYMB.TXT:4651`
  (`CMRSE=000060`). [V that the byte = CMRSE]
- By the ACCP command library it is the **Read Self-test Status (RTEST)** command, ND-05.020.01
  **§5.3.54** (`E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ND-5000 Hardware Description.md:5066-5076`):
  *"Get the ACCP self-test status. After the ND-5000 receives a Master Clear, or after power-up, a
  self-test program is loaded from ACCP PROMs and executed under full ACCP control ... the ND-120 must
  issue this command every 5 seconds after a master clear or power failure in order to find out when it
  can start booting the ND-5000. ... Messack parameters: Self-test status (2 bytes) zero if OK."* [V]
- **Correction:** the gloss "Reset special" for `CMRSE` in
  `E:\Dev\Ronny\NDInsight\SINTRAN\Devices\Octobus\OCTOBUS-PROTOCOL-REFERENCE.md:175` is an unverified
  guess. The polled-until-complete behaviour + "self-test 2-byte status" identifies it as **RTEST**
  (`CMRSE` = "read self-test"), not a reset. [I]

**Why a repeated poll, not a one-shot.** The same message body is retried ~67× over the "~1 minute"
wait because RTEST is a **status poll** — SINTRAN loops it until the ACCP reports the self-test finished.
The short self-test started by master clear is ~20 s (§5.3.54:5073), which is why the monitor prints
"approx. 1 minute". This is not RUNTST (§5.3.53, the 3-minute full test) — RUNTST would be sent once. [I,
from RTEST semantics + the observed repeated identical frame]

**What the ACCP replies, and on which OMD [V].**
- Reply = **Messack**, a multibyte message on OMD 3's reply convention: to (source station 1, OMD =
  the `Source OMD` field of the command). ND-05.020.01 **§5.3.11 Fig. 28**
  (`...:4196-4208`) defines the command body as `[DestOMD=3][Source OMD][body length][command byte...]`
  — the wire `srcOMD=4` IS that Source-OMD field, so **the ACCP answers on OMD 4** (station 1). [V]
- Messack framing: §5.3.11 (`...:4218-4226`): *"Messack is only a single byte ... this data is sent
  directly after Messack in the same multibyte message. (Messack is sent as a multibyte message even if
  there are no return parameters.)"* For RTEST the return data = **2 bytes of self-test status, zero =
  OK**. The single Messack byte is the ACK status `CMACK=0` (SINTRAN reads it as the ETYPE/status high
  byte = `MFACK(0)`; cf. the CPU-presence answer's reply shape,
  `SINTRAN-OCTOBUS-MESSAGE-CATALOG.md:274-281`). [V for framing; I that the leading status byte = 0]

**What J04 waits for vs the 1-minute timeout [I/OPEN].** J04 polls RTEST and treats **Messack with
status word = 0** as "selftest complete"; a nonzero status = a failed self-test bit (§5.3.53:5035-5054
bit map). The 1-minute wall-clock with no Messack at all yields the "ND-5000 timeout: No answer from
ACCP" print. **[OPEN]** The exact byte address of the J04 poll loop and the "No answer from ACCP" /
"Waiting approx. 1 minute" strings is NOT carved: they are not in the M06 NPL
(`PH-P2-RESTART.NPL`, `MP-P2-N500.NPL`, `5P-P2-MON60.NPL` — greps negative) nor in the extracted J04
message pools (`nd-500-mon-j04.prog.md:1462-1477`, which do hold the sibling
`$CONTROL STORE NOT SUCCESSFULLY LOADED`). The "ND-5000 timeout:" family is a J04-local pool not yet
dumped to bytes. The MON60 subfunctions that would carry it, `STSELFTST=155` (START SAMSON SELFTEST) and
`MONACCP=157` (ACCP-INTERFACE), are **commented out** in this M06 revision
(`5P-P2-MON60.NPL:270,272`), consistent with the octobus/ACCP selftest being driven from the J04 binary
or an L-specific resident path we have not located. Do not claim a byte address for it.

---

## Q2 — Control-store load over the octobus

**Command family [V].** The ND-5000 has no microcode ROM; the writable control store (16384 × 128-bit
words) is loaded by the ACCP command library, NOT the classic 3022 WA/BREAK/CSCNT path
(nd-500-bus-interface skill, "Control store" section). The two commands are:
- **LOCSD — Load Control Store Directly**, §5.3.17 (`...:4368-4382`): direct params = **CS address
  (2 bytes) + microinstruction (16 bytes) + checksum addend (2 bytes)** = one 128-bit microword per
  octobus multibyte message. On each word the ACCP does a 16-bit checksum (all byte-pairs incl. addend);
  **zero → Messack**, else **Messnak 4 (checksum error)**. [V]
- **LOCSM — Load Control Store Via Memory**, §5.3.18 (`...:4390-4416`): direct params = none; the whole
  block (`μI word count N`, CS address, N×128-bit words, checksum addend) is placed in **MFbus shared
  memory** and only the command rides the octobus. `Messack` on success; Messnak **-1** (illegal while
  micro running), **1** (no parameter pointer), **4** (checksum). [V]

**Chunking [I].** Large loads normally use **LOCSM** (§5.3.10:4174 — "commands with long parameters (as
LOAD CONTROL STORE) are normally passed via memory since this takes the shortest time and least octobus
traffic"), so the octobus carries only `LPARP` (Load Parameter Pointer, §5.3.15) + one/few LOCSM
commands, each Messack-ed — that is a *small* number of OMD-3 frames, consistent with the observed burst
being tens (not 16384) of messages. Whether SINTRAN-L uses LOCSD or LOCSM on this box is **[OPEN]** (the
observed count ~67 does not by itself decide it; the request notes the ~67 may be conflated with the
selftest retry count). Either way each command expects a Messack. [I]

**SINTRAN side [V].** J04 `LOAD-CONTROL-STORE` (`nd-500-mon-j04.prog.md:1365`) → **MON 60 subfunction
`LDCS 37B`** = SINTRAN `CSLOAD=037` (`5P-P2-MON60.NPL:196`, dispatch slot `ICSLOAD` at
`5P-P2-MON60.NPL:1323,1380`); `ICSLOAD` copies the CS **file name** into the MON60 buffer
(`...:1380-1382`) and the resident driver streams the file to the ACCP. The status codes that gate it:
- `ECSLOAD = 2032B` "CONTROL STORE MUST BE LOADED" (`5P-P2-MON60.NPL:66`) — the transient "load it and
  retry" status; and
- **`EILOCS = 2103B` "ERROR IN LOADING CONTROL STORE"** (`5P-P2-MON60.NPL:107`). [V]

**What makes J04 print "Error when loading Control Store" [I].** A CS-load command that the ACCP
**Messnak**s (checksum error 4, CS/HW error 5, or "-1 illegal while micro running") — or that gets **no
Messack at all** — surfaces as `EILOCS 2103B` / J04's `$CONTROL STORE NOT SUCCESSFULLY LOADED`
(`nd-500-mon-j04.prog.md:1467`). In the captured run the emulator answers *none* of the OMD-3 CS-load
messages, so SINTRAN sees no Messack → error. The exact J04 string "Error when loading Control Store" is
in the un-dumped J04 pool — **[OPEN]** for a byte address, but it maps to `EILOCS 2103B`. [I]

---

## Q3 — ACCP terminate (`244B` emergency)

**What `244B` is [V].** Octobus emergency frame `0x81A4`: `EBIT(200B) | CMATE(044B)`. `CMATE = 000044`
(`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\M06\N500-SYMBOLS.SYMB.TXT:4653`). The
`OCTOBUS-ND100-ND5000-REFERENCE.md:147` emergency table already pins **244B (0xA4) = 044B = "Terminate
ACCP → level-7 interrupt → firmware idle loop"** (its "Autotest enable" gloss for CMATE in
`OCTOBUS-PROTOCOL-REFERENCE.md:176` is a wrong guess — the manual says TERMINATE). [V]

**What the ACCP does [V].** ND-05.020.01 **§5.3.9 (ACCP TIMEOUT)**
(`...:4152-4156`): *"To terminate the ACCP, an octobus emergency message with code 244B must be sent.
This gives an interrupt to the ACCP at the highest level (7) and causes the ACCP program to enter the
idle loop."* Also §5.3.11 (`...:4234`): the emergency is **decoded by hardware** and *"causes the ACCP
to reset its buffers and start at the top of the communication loop."* [V]

**What SINTRAN expects back to consider it terminated [V].** §5.3.9 (`...:4156`): *"To test if this was
successful, an **Alive Check** multibyte message command is sent after the terminate. This returns
information on whether or not the microprogram is running. If there is no response to the Alive Check,
another timeout occurs. This is an impossible to terminate ND-5000 situation, and only master clear can
solve the problem (octobus emergency message 241B)."*
- ALIVE CHECK = **`CMALI = 000037`** (`N500-SYMBOLS.SYMB.TXT:4897`), ACCP §5.3.26 (`...:4592-4600`):
  **Messack if ALIVE true, else Messnak error 7 (Not alive)**. [V]
- So termination is judged by **receiving a reply frame** (Messack OR Messnak-7) to the ALIVE CHECK — a
  *polled reply*, not a status register. The emergency `244B` itself is fire-and-forget (hardware
  decoded, no reply). In the capture the emulator ignores the emergency and (crucially) never answers the
  following ALIVE CHECK → "Impossible to terminate ACCP after timeout"; the 6× repeat is the retry-then-
  master-clear ladder. [V mechanism; the wire trace showed the 6× 244B but did not surface the interleaved
  ALIVE CHECK — its presence is [I] from §5.3.9]

---

## Q4 — Is the "~1 minute selftest" real ACCP firmware, or just a SINTRAN wait?

**It is a real ACCP-firmware (MC68000 EPROM) self-test [V].** §5.3.54 (`...:5071`): after Master Clear or
power-up *"a self-test program is loaded from ACCP PROMs and executed under full ACCP control"* — the
short version ~20 s; the full RUNTST (§5.3.53) ~3 min, testing BUS/MIR/CS/ALU/REG/caches/AAP
(`...:5035-5054`). The ACCP baby-card is a 68000 + 128 KB EPROM (ND-05.020.01 §5, §7.1 in
`OCTOBUS-ND100-ND5000-REFERENCE.md:253`); there is **no "ACCP microcode"** — it runs 68000 machine code.
[V]

**But SINTRAN only observes it via one octobus message — the RTEST poll [V].** The self-test runs
entirely inside the ACCP; the ND-120 learns of completion solely by polling RTEST for a Messack whose
2-byte status = 0. Therefore the **emulator can answer with a canned "selftest OK"** (Messack + status
`0x0000`) — it does NOT need to model the 68000 self-test. This is a green light for the canned answer.
[V/I]

---

## Bottom line for the emulator (`OctobusND5000Station`)

Reply convention for every OMD-3 command: send a **multibyte message back to (station 1, OMD =
`srcOMD` of the received command)** — NOT a fixed OMD. Messack = leading status byte `0` (CMACK/`MFACK`)
+ any return bytes; Messnak = `[error_code, ASTS_low, ASTS_high]` with the status high byte read by
SINTRAN as `MFNACK(377B)`. (ND-05.020.01 §5.3.11:4218-4232; reply-OMD = §5.3.11 Fig.28 Source-OMD field.)

**1. Selftest poll — `060B` (CMRSE / RTEST) on OMD 3, `srcOMD=4`, no params:**
- Answer a **Messack multibyte** on OMD 4 (station 1): body = `[Messack=0][selftest status = 0x00 0x00]`
  (2 status bytes, zero = OK). One good answer ends the ~1-minute poll → clears "No answer from ACCP".
- Answering every retry identically is fine; SINTRAN stops as soon as it sees one Messack-status-0.

**2. Control-store load — OMD-3 LOCSD (`§5.3.17`) and/or LOCSM (`§5.3.18`):**
- For **every** CS-load command message received on OMD 3, reply **Messack** (body `[0]`, no params) on
  the command's `srcOMD`. Discard the payload/MFbus block — do not verify the checksum.
- Do NOT emit any Messnak (codes 1/4/5/-1) during the load, and do not leave a command un-answered — a
  Messnak or a missing ack is exactly what produces `EILOCS 2103B` → "Error when loading Control Store".
- If SINTRAN sends `LPARP`/`VPARP` (§5.3.15/16) or `STARTMIC`/`RESTMIC` (§5.3.23/46) around the load,
  Messack those too. After the load, `READ CPU MODEL`/`RMVER` answers should reflect the loaded image
  (word 7 model / word 1 version) per the nd-500-bus-interface skill — but that is downstream of passing
  this handshake.

**3. Terminate — emergency `244B` (`EBIT|CMATE 044B`), sent up to 6×:**
- On the `244B` emergency: reset the station's AIB/AOB buffers and return the ACCP model to its idle/top-
  of-loop state (no reply frame — it is hardware-decoded).
- Then **answer the ALIVE CHECK** that follows: SINTRAN sends OMD-3 command `CMALI=037B`; reply with a
  multibyte on its `srcOMD` — a **Messack (`[0]`)** ("alive/idle, terminate OK") is the clean answer;
  even a **Messnak-7 ("Not alive")** counts as "terminated" because it is *a reply*. The failure mode is
  giving NO reply → "Impossible to terminate ACCP after timeout". Answer the ALIVE CHECK and the monitor
  proceeds.

**Canned-answer verdict:** all three handshakes pass with canned Messacks — no 68000 self-test emulation
and no real control-store checksum/verify is required on the octobus path (unlike the classic 3022 path,
which DOES verify readback). The only hard requirement is: **never consume an OMD-3 command without
sending a reply frame on its `srcOMD`.**

### Honest [OPEN] items
- Byte addresses of the J04 selftest-poll loop and the literal strings "No answer from ACCP",
  "Waiting approx. 1 minute", "Impossible to terminate ACCP after timeout", "Error when loading Control
  Store" — not located (J04-binary pool not dumped; M06 NPL has `STSELFTST`/`MONACCP` commented out).
- Whether SINTRAN-L uses LOCSD (per-word) or LOCSM (via-memory) for the CS load — undecided from the
  wire; the emulator's "Messack every OMD-3 CS command" strategy is correct for both.
- The exact leading Messack byte value (assumed `0`/CMACK, consistent with the solved CPU-presence
  ack) and whether the selftest reply is consumed by the same or a different SINTRAN receiver than
  5OMBREAD — [I], because the poll loop is not carved.

---
*Sources: ND-05.020.01 EN §5.3.9-5.3.57; N500-SYMBOLS.SYMB.TXT (CMRSE/CMATE/CMALI); 5P-P2-MON60.NPL;
nd-500-mon-j04.prog.md; OCTOBUS-ND100-ND5000-REFERENCE.md; SINTRAN-OCTOBUS-MESSAGE-CATALOG.md.*
