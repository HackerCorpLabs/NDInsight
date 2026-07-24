# ENNS0 START-NETWORK-SERVER: PIOCM (MON 255B) start path - RESOLVED

Date: 2026-07-23. Builds on ENNS0-POLL-FINDINGS.md.
Split: **[V]=VERIFIED** (decoded bytes / read source) vs **[I]=INFERRED**.

## THE ONE-LINE ANSWER
The "start server" handshake gate is **NOT** the controller STATUS register / INT12.
It is a **memory-mapped PIOC-DRAM readiness cell: logical word 1002B, value must become
PRKEY=052163B**. The SINTRAN kernel driver `PISTA` busy-polls that cell (3-second timeout)
and only writes the REQUEST word (`MPIOC=5` at mailbox+`NPFUN`) **after** it reads PRKEY.
The firmware never wrote PRKEY, so the kernel timed out before ever writing REQUEST -> the
68K saw REQUEST=0.

---

## Task 4 FIRST (it explains everything): the kernel MON 255B driver WAS in the repo

**Found: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL`** (589 lines) -
the SINTRAN III `PIOCM` monitor-call (MON 255B) driver. This is the code that ACTUALLY
does the IOXT to the Ethernet controller; ENNS0 only calls it.

[V] Calling convention (RP-P2-PIOC.NPL lines 21-35, verbatim header):
```
T = FUNCTION     X = PIOC LDN     A = PARAM POINTER
IF T = -1  -> A points to an instruction-block to execute (EXEL special fn)
ELSE       -> parameters in registers
RETURN: STATUS IN T
```
[V] Function dispatch table (line 37):
`PIENT := (PIRES, PIREL, PIKIC, PIWKI, PILOA, PIUNL, PISTA, PISTO, PIDIS)`
indexed by T. So **T=6 -> PISTA = "START PIOC"** is the server-start call.

This exactly matches the ENNS0 wrappers decoded below (T register selects the action).

### Device registers used by the kernel driver (symbol table L07)
[V] `HDEV`   = status register (IOXT read; `SHA ZIN SHR 10` extracts bank no.)
[V] `HDEV+3` = control register `PWCR` (IOXT write): 60B=halt/reset, 0=initiate,
    11B=start-command doorbell, `PWCR BONE BNDC`=kick.
These ARE the "$1001 STATUS" / "CONTROL register" / "doorbell" the live tracer saw.

### PIOC-memory access is memory-mapped, not IOX (lines 156-165)
[V] `PREAD: A:=K1024; X+A; T:=PIOCA; *LDATX`  (K1024=2000B, PIOCA=13B datafield cell)
    `PWRIT: T:=PIOCA; *STATX`
    -> the kernel maps the PIOC's DRAM into an ND-100 bank and reads/writes it with
    LDATX/STATX at offset (X + 2000B). Logical PIOC word `N` lives at bank window `N+2000B`.

---

## Task 1: the PIOCM wrappers in ENNS0 (encos-err-i-b01.brf) - DECODED

[V] All 10 wrappers disassembled at 032703-033147. Each loads X=param-block ptr, sets
T=function via `SAT`, executes `MON 255`, returns status via `STT`. Confirmed T codes:

| Wrapper  | addr(oct) | T (SAT) | -> PIENT entry | meaning              |
|----------|-----------|---------|----------------|----------------------|
| READPIO  | 032703    | -1      | EXEL           | execute-instr / read |
| (unnamed)| 032734    | -1      | EXEL           | 2nd read block       |
| INT2GET  | 033003    | (none)  | -              | builds block, no MON |
| RES_SLO  | 033023    | 0       | PIRES          | reserve slot         |
| REL_SLO  | 033035    | 5       | PIUNL          | unload/release       |
| SEND_KI  | 033047    | 7       | PISTO          | (stop/send)          |
| REC_KIC  | 033063    | 3       | PIWKI          | wait-for-info        |
| SEGLOAD  | 033077    | 4       | PILOA          | LOAD firmware seg    |
| UNLOAD   | 033113    | 5       | PIUNL          | unload seg           |
| **START_P** | **033124** | **6** | **PISTA**   | **START PIOC**       |
| STOP_PI  | 033137    | 7       | PISTO          | stop PIOC            |

[V] START_P body (033124):
```
033127 054606  LDX -122,B      ; X = caller's PIOC LDN
033130 044607  LDA -121,B      ; A = start address arg
033131 171006  SAT 6           ; T = 6  -> PISTA
033132 153255  MON 255
033133 004611  STA -119,B      ; save returned A
033134 010613  STT -117,B      ; save returned status T
```
[V] Confirmed (prior finding): ENNS0 has **zero IOX/IOXT**. It builds NO controller
postbox words itself. All postbox writes are done by PISTA in the kernel.

[V] **No wrapper is called from anywhere inside encos-err-i** (scanned the whole 174-unit
linked image for pointer/JPL references to the wrapper entry addresses - the only hit,
017427->REC_KIC, is an ASCII data word inside a message table, not a call). => the actual
START-NETWORK-SERVER caller that supplies the start-address/LDN lives in
`encos-in-b01.prog` (the main INcos program image), NOT in this error module. This BRF only
*provides* the wrapper library + the error/message subsystem. (See "Blocked / next lead".)

---

## Task 1 core: the parameter block PISTA builds (the real 0x406/0x408 answer)

[V] `PISTA` (RP-P2-PIOC.NPL lines 337-414). Sequence:
```
114703  T:=HDEV;  *IOXT              ; read status -> PIOC bank number  (=PIOCA)
114712  X:=1002; A:=0; CALL PWRIT    ; ZERO the readiness/INITFLAG cell (PIOC word 1002B)
114715  T:=HDEV+3; A:=60;  *IOXT     ; HALT AND RESET  (write PWCR=60B)
114721  A:=0=:PWCR;        *IOXT     ; INITIATE        (write PWCR=0)   <-- the one doorbell
114724  %WAIT FOR PIOC TO GET READY
        -3 =: TMR                    ; arm 3-second timeout
        DO  A:=-500=:DELAY;*MIN;JMP*-1 ; inner spin
            X:=1002; CALL PREAD      ; read PIOC word 1002B
          WHILE A = 0  OD            ; loop while still zero
        0 =: TMR
114737  IF A >< PRKEY THEN PPROM; GO PIRET FI   ; must equal PRKEY=052163B, else error
        ... reads PIOC datafield pointer (word 1001B) and lays out all mailbox pointers ...
114767  ...  MASTA = "NORD -> PIOC mailbox" (from datafield word NPOPC)
        %  SO WE CAN GIVE THE START COMMAND
115067  X:=MASTA; *AAX NPFUN; A:=MPIOC; CALL PWRIT   ; REQUEST  = MPIOC(=5) at mailbox+1
115073  X:=MASTA; *AAX NPTIG; A:=TRIG;  CALL PWRIT   ; TRIGGER  = TRIG(=1)  at mailbox+0
115077  A:=11; T:=HDEV+3; *IOXT                      ; ring control-reg doorbell (start)
115103  3 =: PISTT                                   ; mark STARTED
```

[V] Constants (symbol tables, all versions K03/L07/M06 identical):
```
PRKEY = 052163B (=0x5473)   readiness key firmware must post to word 1002B
MPIOC = 000005              the REQUEST/function value written to the mailbox   <-- non-zero!
TRIG  = 000001              the trigger value
NPFUN = 000001              mailbox offset of the FUNCTION/REQUEST word
NPTIG = 000000              mailbox offset of the TRIGGER word
NPOPC = 000001              datafield offset giving MASTA (NORD->PIOC mailbox base)
K1024 = 002000B  PIOCA=000013B  MASTA=000014B  HDEV=177775
```

So **(a)** the non-zero REQUEST the hunt asked for is `MPIOC = 5`, written by the KERNEL
(PISTA line 115067), never by ENNS0. There is no "SUBFUNCTION=0" written separately in this
path - the request is a single word (MPIOC) plus a trigger; a SUBFUNCTION field, if the
firmware reads one, would be an adjacent mailbox cell left at its reset value 0.

**(b)** the "monitor-sync ack" the firmware waits on = the **readiness cell word 1002B**.
The firmware is supposed to post PRKEY there after reset/initiate; that is what makes PISTA
proceed (its equivalent of "return D0==1 / set STARTED"). The D0==1 the 68K wants and the
STARTED_FLAG are the firmware-side mirror of "kernel saw PRKEY, then wrote MPIOC+TRIG+rang
doorbell 11B".

---

## Task 2: what the start path polls for completion
[V] It polls a **PIOC-DRAM cell (logical word 1002B, physical bank offset 1002B+2000B=3002B),
via LDATX (PREAD), waiting for value PRKEY=052163B**, with a 3-second timeout (TMR=-3).
It does **NOT** poll STATUS bit 2 / wait on INT12 for the START handshake.

[V] Reconcile with live "$1001, STATUS bit2 never set, 142x, timeout":
- The `IOXT` reads of `HDEV` (status) at 114703 return the status word (bank in high bits) -
  that is the `$1001`-shaped value (bank 16 | int-enabled); it is read to extract the bank,
  not polled for bit 2.
- The 142x status spin + timeout the tracer saw corresponds to the PISTA readiness loop
  reaching its 3-second `TMR` timeout because word 1002B stayed 0 (PRKEY never posted).
- [I] STATUS bit 2 / INT12 is the notification path for later kick/mailbox traffic
  (PIKIC/PIWKI ring `HDEV+3` and the firmware INT12s back), NOT the START gate. Chasing
  "STATUS bit 2 never set" for the start hang is a red herring; the start gate is the
  memory-mapped INITFLAG cell.

---

## Task 3: why the firmware saw REQUEST = 0x0000
[V]+[I] Because in the real protocol the REQUEST word (MPIOC=5 at MASTA+NPFUN) is written by
the KERNEL's PISTA **only after** the readiness poll succeeds (word 1002B == PRKEY). The
single doorbell the ND-100 rang was the **INITIATE** `IOXT` (PWCR=0, line 114721) that
precedes the readiness wait - at that instant REQUEST is *correctly* still 0, because the
start-command doorbell (PWCR=11B, line 115077) and the MPIOC write only happen later. The
firmware read REQUEST=0 on the initiate doorbell and (in the emulator) treated it as an
invalid request (D0=-5) instead of doing its post-reset job of writing PRKEY into word 1002B.

So: ENNS0 is NOT writing the request to the wrong offset, and is not writing 0. **The request
is supposed to come from the kernel PIOCM driver (PISTA), and it never got that far** because
the firmware skipped the readiness handshake. Root cause is on the 68K firmware / emulator
side: after RESET+INITIATE the firmware must (1) publish the PIOC datafield pointer at word
1001B and (2) write PRKEY (052163B) into word 1002B; only then does the kernel write MPIOC
and ring the real start doorbell. Evidence: the exact PISTA code + constants above; the live
"one doorbell, REQUEST=0, then 3s timeout" trace matches initiate-then-readiness-timeout
bit-for-bit.

[I] Mapping to the firmware's DRAM 0x406/0x408/0x4C0: MASTA is derived at runtime from PIOC
datafield word NPOPC, so the firmware's REQUEST@0x406 == MASTA+NPFUN and its readiness
STARTED_FLAG@0x4C0 is the firmware-side of the PRKEY/word-1002 exchange. The absolute
0x406/0x408/0x4C0 addresses are the 68K's fixed DRAM layout; I have NOT byte-matched them to
the ND-100 mailbox offsets (that needs the 68K firmware side), so treat the specific-offset
correspondence as inferred, not byte-verified.

---

## Blocked / concrete next lead
- The ENNS0 command handler that CALLS `START_P` (supplying LDN + start address, and possibly
  pre-seeding SUBFUNCTION) is in **`encos-in-b01.prog`**, a linked `.prog` image the BRF
  linker can't load. Next step: teach a loader for the `.prog`/BPUN dump format (small header
  + raw 16-bit words), then grep it for `153255` (MON 255) and back-trace the `T=6` caller to
  see the exact param block and any SUBFUNCTION word ENNS0 stages. But note: per PISTA, the
  START request word itself is kernel-supplied (MPIOC), so this mainly confirms the LDN/start
  address, not the 0x406 value.
- Firmware side: verify the 68K post-reset path writes datafield-ptr@word1001B and
  PRKEY@word1002B. That is the actual fix point for the hang.

## Source paths
- Kernel driver: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL`
- Symbols: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
  (+ N500/RTLO/FILSYS lists; K03/M06 identical for these constants)
- ENNS0 wrappers: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\encos-err-i-b01.brf` @032703-033147
- Tooling: scratchpad `brf_link.py`, `nd100dis.py`, `find_callers.py`
