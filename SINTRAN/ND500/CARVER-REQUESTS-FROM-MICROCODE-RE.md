# Carver requests — byte-evidence needed to close the microcode RE open items

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVER-REQUESTS-FROM-MICROCODE-RE.md`
**From:** the ND-5800 microcode reverse engineering (2026-07-16).
**Deliverable:** append findings to `ND500-MAILBOX-MESSAGE-CATALOG.md` with the usual
evidence grades (BYTES / SYMBOL / NPL / MANUAL / INFERRED) and byte addresses. If a request
cannot be proven, say so explicitly — do not fill gaps with plausible values.

## Context (read first, 5 min)

`E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` — the B30 microcode
mailbox servicer is fully decoded: fetch/dispatch (sec 3), MON-call exit (sec 3.8), trap
stops (sec 3.9). The microcode side proves ORDER and VALUES of reads/writes but NOT absolute
message offsets (the disassembly doesn't render address stepping). The SINTRAN side is the
offset authority. These requests are exactly the offsets/consumers the microcode side cannot
pin.

## Requests, highest value first

### R1. MON-call parameter block offsets (catalog UNKNOWN #2 — blocks the emulator most)

The microcode (CALL_MON8 @003772) writes, per CALLG argument, an **(address, value) word
pair** into the message data part, then STOPR/NUMPA/MCNO. Question: **at which message word
offsets does MCHANDLE / each GOSW handler (500B-523B, esp. 504 NOUTS, 511 DVIO, 512/513
A5XMSG/B5XMSG) read the parameters?**
- Verify or refute the NPL-derived `5AP1..5AP4 / 5DP1..5DP4 = 100B..107B` rows
  (`ND500-MONITOR-CALL-PARAMETER-PASSING.md` sec 2.1) with bytes from 026-S3IMPIT
  (MCHANDLE @137206, GOSW table @137625B, NOUTS body @141027).
- Is the pair order (address, value) or (value, address) as consumed? One pair per param or
  addresses first then values?
- For 504/511/512 the microcode ALSO inline-copies the user buffer into the message — where
  does the ND-100 handler expect that buffer (offset, max length)?

### R2. Saved-P location

CALL_MON9 @004006 and TRAP_GEN4B @013572-73 write the stopped process's P (a 32-bit word)
into the message right before/around STOPR. **Where does SINTRAN read the stopped P from** —
a message offset (which?), the context block, or the process descriptor via XADPR(144)?
Look in MCHANDLE/TRAPDECODER/5RRTWT and whatever answers `N500-STATUS`-type queries.

### R3. Trap stop consumption — TRAPDECODER

Carve TRAPDECODER (reached from DECOMESS when STOPR=TRAPCODE(2)):
- Which fields does it read, in order? (TRAPN@16 expected; the microcode also writes ~4
  status/trap-record words + fault parameters — TRAP_GEN2/3/4 @013520-013605.)
- The trap-number table/dispatch inside TRAPDECODER — I need names/semantics for trap
  numbers, specifically **0o44, 0o46 (page fault?), 0o51** (special-cased in the microcode's
  TRAP_ENT @013734-741) and the full legal range.

### R4. Does SINTRAN (this L-VSX-500 version) ever SEND MICFU 05 (3SWMESS) or 27B (3FITRNSF)?

The B30 microcode dispatches both to MSG_ILLEG. Find every writer of MICFU(6) in the carve
(known: MONICO writes 24B; 5ACTSWAPPER writes 24B; RP watchdog writes 1). If 05/27B are
never sent on the 5000 path, the discrepancy is resolved (dropped codes); if they ARE sent,
we have a version problem. Also: any senders of the extra B30 codes
10/11/12/13/14/22/30/31/34/35/42/44/45/46/47/50-52/70-76/77B? (44B=3RPREG and 1=3RMICV are
known senders; the rest unknown.)

### R5. 3RMICV watchdog answer — how many halfwords are read back?

Microcode writes TWO answer halfwords (version 027232B + a CPU-parameter halfword from its
SRF cell 0o2015). Carve the watchdog-answer reader (CHN5STATUS ANSWER+WATCHDOG branch @
~135205 region): which message offsets does it read? Does anything consume the second
halfword, and what does SINTRAN call it?

### R6. Restart write-back mask NUMPA(12)

Byte-verify the bit->slot mapping when NUMPA is a write-back mask (catalog: "bit k =>
copy 5AP(k+1)/5DP(k+1); DVIO sets 100000B"): which handler applies it, and to which message
offsets do the write-back values go (ties into R1)?

### R7. The ND-5000 mailbox head cell + system parameters init

On the 5800 the microcode polls `mem[ srf[#CPUDF] ]` for the queue head and reads "SYSPAR"
ident bits (used to compose GIVEINT interrupt words: (ident & 037400B) | 100001B/100401B).
- Which ND-100/MPM address does SINTRAN load the queue head into for the 5000 (the MAILINK
  equivalent for ITOFIFOQ)? XMSINIT / X500DF area.
- Which init message/mechanism hands the ND-500 its "system parameter" values (the ident
  bits, mailbox pointers) — is it MSG_STARTP0 (22B, which reads an ADR_SYSPAR-relative
  block), a CS-load side table, or MPM fixed addresses? Any sender of MICFU 22B?

### R8. OCB message receivers for microcode-built messages (5000 only)

The microcode emits out-of-band OCB multibyte messages: type **201B** (system trap),
**203B** (CPU unavailable), **204B/205B/206B/210B** (protocol errors) — built in TRAP_OCBM
@016727 via ACCP_XWRITE. Carve the ND-100 receive side (OMBREAD @037660 in 026-S3IMPIT and
whatever dispatches received multibyte bodies): per type, what payload layout does the
receiver expect? This closes the TRAP_OCB00..20 payload maps from the consuming end.
(Manual cross-ref: ND-05.020.01 ~line 4105-4174 documents the HARDWARE FAULT multibyte
format — the receiver should match.)

### R9. Confirm the trap answer N5STA=4 case

TRAP_END @013610 answers N5STA := 3 normally but := 4 (5ERANSWER) when the run-state flag
says no process was running. Does DECOERRMESS (the 5ERANSWER branch) handle trap-shaped
messages (STOPR=2) specially, or is this only ever an error case? Determines whether the
emulator must reproduce the conditional.

## Not needed from the carver

- MICFU dispatch table, N5STA lifecycle, MON exit mechanism, trap triage — already
  microcode-verified.
- Segment-31/CALLG encoding — manual-verified; MCNO = low halfword of the CALLG target.
- Octobus frame/envelope formats — manual + live-verified.
