# ND-500 Bus Interface: Evidence and Contradiction Verdicts

**Status:** Phase 2 evidence dossier for the ND-500 bus interface documentation overhaul
**Date:** 2026-07-08
**Role:** This document is the citation trail behind
[ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md). Every load-bearing
claim in the master reference is anchored here, and every known contradiction between the
older documents is resolved (or explicitly declared UNVERIFIED) here.

**Conventions:**

- All numbers are OCTAL unless marked otherwise (SINTRAN/NPL convention).
- "NPL:file:line" cites `../NPL-SOURCE/NPL/<file>` at the given line of the repo copy.
- "SYM:M06:line" cites `../NPL-SOURCE/SYMBOLS/M06/N500-SYMBOLS.SYMB.TXT`.
- Evidence classes:
  - VERIFIED = quoted directly from NPL source, symbol table, or manual.
  - DERIVED = follows from verified evidence by stated reasoning.
  - UNVERIFIED = no decisive evidence found in this repository; do not treat as fact.
- Verification method: every citation below was re-opened and re-read on 2026-07-08
  (not inherited from earlier AI analysis). Where earlier analysis was found wrong,
  the correction is recorded in section 6.

---

## 1. Sources examined

| Source | Path (relative to this folder) | Class |
|--------|-------------------------------|-------|
| SINTRAN NPL driver sources | `../NPL-SOURCE/NPL/` (MP-P2-N500.NPL, CC-P2-N500.NPL, RP-P2-N500.NPL, XC-P2-N500.NPL, PH-P2-OPPSTART.NPL, PH-P2-RESTART.NPL, MP-P2-PERF-SAMP.NPL, 5P-P2-MON60.NPL) | Primary |
| Symbol tables K03/L07/M06 | `../NPL-SOURCE/SYMBOLS/` (N500-SYMBOLS, N5000-SYMBOLS, SYMBOL-1-LIST) | Primary |
| Full compile listing | `../NPL-SOURCE/s3vs-4.symb` | Primary |
| Reference manuals | `../../Reference-Manuals/500/` and `../../Reference-Manuals/` (see section 4) | Primary (hardware side) |
| Existing analysis docs | this folder + `../OS/` + `../Emulator/` | Secondary - validated against primaries |

---

## 2. Verified ground truth (NPL + symbol tables)

### 2.1 IOX register offsets of the 3022 interface

Symbol table N500-SYMBOLS.SYMB.TXT, verified IDENTICAL across K03, L07 and M06
(SYM:M06 lines 521-522, 1224-1231, 3659, 4661, 7164, 7229-7235; L07 lines 507-508,
1189-1196, 3600, 4598, 7065, 7137-7143; K03 lines 292-293, 733-739, 2347, 3127,
4472, 4536-4542):

| Offset (octal) | Read symbol | Write symbol | Meaning |
|---|---|---|---|
| 0  | RMAR5 | -     | Read Memory Address Register |
| 1  | -     | LMAR5 | Load Memory Address Register |
| 2  | RSTA5 | -     | Read STATUS |
| 3  | -     | LSTA5 | Load STATUS |
| 4  | RCON5 | -     | Read CONTROL |
| 5  | -     | LCON5 | Load CONTROL |
| 6  | -     | MCLR5 | Master Clear (SYM:M06:7164 `MCLR5=000006`) |
| 7  | -     | TERM5 | Terminate |
| 10 | RTAG5 / RUPP5 | - | Read TAG-IN / Read Upper Limit (same offset, two names) |
| 11 | -     | LTAG5 / LUPP5 | Load TAG-OUT / Load Upper Limit (same offset, two names) |
| 12 | RLOW5 | -     | Read Lower Limit |
| 13 | -     | LLOW5 | Load Lower Limit |
| 14 | -     | SLOC5 | Set lock ("start-lock") |
| 15 | -     | CLKD5 | Clock data (name only; function UNVERIFIED in NPL) |
| 16 | -     | UNLC5 | Unlock |
| 17 | -     | RETG5 | Return gate |
| 20 | 5MODE | -     | Mode register (name only; function UNVERIFIED in NPL) |

VERIFIED. Note especially: **MCLR5=000006 EXISTS in all three symbol table versions**
(SYM:M06:7164, L07:7065, K03:4472). An earlier analysis pass claimed there was no symbol
at offset 6; that claim was wrong (see section 6.1).

The IOX device base is not a constant. Every access is `T:=HDEV+<offset>; *IOXT` where
HDEV is the standard SINTRAN datafield slot at B-relative displacement 177775 (-3)
(SYM:M06:7003 `HDEV=177775`, SYM:M06:7002 `XHDEV=177774`). Which device datafield B points
to determines which interface (thumbwheel) is addressed. There is no fixed "typical"
device number in the sources (see C10 in section 5).

### 2.2 Registers SINTRAN actually uses vs never uses (negative claims)

A fresh whole-tree grep of `../NPL-SOURCE/NPL/` for every register symbol
(RMAR5|LMAR5|RSTA5|LSTA5|RCON5|LCON5|TERM5|RTAG5|LTAG5|RUPP5|LUPP5|RLOW5|LLOW5|SLOC5|
CLKD5|UNLC5|RETG5|5MODE|MCLR5) performed 2026-07-08 returned uses ONLY at:
CC-P2-N500.NPL:214-216, MP-P2-N500.NPL:266, 2933-2937, 3063, 3084-3092,
MP-P2-PERF-SAMP.NPL:373, PH-P2-RESTART.NPL:117-135, PH-P2-OPPSTART.NPL:3913,
RP-P2-N500.NPL:94, XC-P2-N500.NPL:50-59.

From these lines:

**Used by SINTRAN:**

| Register | Where used |
|---|---|
| RSTA5 (read STATUS) | ISR/CLE5STATUS (XC:50,59; MP:266,3063; MP-PERF-SAMP:373; RP:94), terminate (MP:2933,2937), power-fail (PH-RESTART:117,121,130), detection (PH-OPPSTART:3913) |
| LSTA5 (write STATUS) | CLE5STATUS (XC:57), activate (MP:3090), power-fail (PH-RESTART:131) |
| LCON5 (write CONTROL) | micro-stop (CC:215), activate (MP:3086,3089,3091), CLE5STATUS (XC:55,58), power-fail (PH-RESTART:129,132,133) |
| LMAR5 (write MAR) | activate/reactivate ACT50 (MP:3084-3085) |
| TERM5 (terminate) | terminate (MP:2936), power-fail (PH-RESTART:119,135) |
| SLOC5 (set lock) | activate (MP:3092), power-fail (PH-RESTART:134) |
| UNLC5 (unlock) | micro-stop 5MCST (CC:214) |
| RETG5 (return gate) | micro-stop 5MCST (CC:216, value 2) |

**NEVER used anywhere in the NPL tree (defined in symbol tables only):**
RMAR5 (read MAR), RCON5 (read CONTROL), MCLR5 (offset-6 master clear),
RTAG5/LTAG5 (TAG in/out), RUPP5/LUPP5 (upper limit), RLOW5/LLOW5 (lower limit),
CLKD5, 5MODE.

VERIFIED. Consequences:

1. SINTRAN performs NO register-level TAG handshake with the ND-500. Any documentation
   or emulator code built on TAG-IN/TAG-OUT message codes (e.g. "code 8 =
   MonitorCallRequest") describes an invented protocol, not SINTRAN behavior.
2. SINTRAN never reads back MAR or CONTROL.
3. The MCLR5 register exists (hardware symbol) but SINTRAN stops the machine with the
   5MCST sequence (UNLC5, LCON5:=40, RETG5:=2) instead - see 2.5.3.
4. The limit registers and 5MODE are never programmed by SINTRAN; if they matter at all
   it is for test programs/microcode (see manual evidence, section 4).

### 2.3 STATUS register (RSTA5) bits

Authoritative comment block, NPL:XC-P2-N500.NPL:36-45 (subroutine CLE5STATUS header),
quoted:

```
% ENTRY:     A=Mask to clear status with:
%              177377 Clear latched "power has been off" Status (bit 5POWOF=10)
%              177177 Also clear "power fault executed by Microprog" (5PFAIL=7)
%
% EXIT:      A=ND-500 Status after clearing
%              BIT  5PAGF=4 (000020) = Inclusive "or" of errors
%              BIT 5DMAER=6 (000100) = Communication error
%              BIT 5PFAIL=7 (000200) = Power fault executed by Microprog
%              BIT 5POWOF=8 (000400) = Latched power fault
%              BIT 5CLOST=9 (001000) = Microclock stopped
```

(The "=10" for 5POWOF in the mask comment is the OCTAL bit number of decimal bit 8;
the EXIT block gives decimal bit numbers with octal masks.)

Symbol table values (SYM:M06:1312, 1038, 287): `5PAGF=000004`, `5ILOC=000005`,
`5CLOS=000011` - bit NUMBERS (octal), i.e. 5ILOC = bit 5, 5CLOS = bit 9 decimal.

| Bit (dec) | Mask (oct) | Name | Meaning | Evidence |
|---|---|---|---|---|
| 4 | 000020 | 5PAGF  | Inclusive OR of errors | XC:41, SYM:M06:1312 |
| 5 | 000040 | 5ILOC (code also spells 5ILOCK) | Interface locked = ND-500 running | SYM:M06:1038; tested MP:2935,2944,3066,3082; PH-RESTART:118,122; MP:267; RP:94 |
| 6 | 000100 | 5DMAER (code also spells 5DMAERR) | Communication (DMA) error | XC:42; tested MP:673 |
| 7 | 000200 | 5PFAIL | Power fault executed by microprogram | XC:43; tested MP:670, XC:51-52 |
| 8 | 000400 | 5POWOF | Latched power fault | XC:44; XC:51,61; PH-RESTART:131; MP:267 |
| 9 | 001000 | 5CLOST (SYM: 5CLOS) | Microclock stopped (ND-500 halted) | XC:45, SYM:M06:287; tested MP:3065 |

The ISR error test masks status with 720 (octal) = 5PAGF+5DMAER+5PFAIL+5POWOF
(MP:669 `IF A/\720><0`). CLE5STATUS is called with clear-masks 177377 or 177177
(MP:668; XC:37-38). VERIFIED.

**Stop reasons - two locations, both real.** The hardware STATUS register DOES define
bits 10-14 as "ND-500 stop reason" (TMP section 3.2 - see section 4.5 below). The
SINTRAN driver, however, does not dispatch on those STATUS bits: it dispatches on the
MESSAGE field STOPR (offset 11 in the message, SYM:M06:3241) in DECOMESS (MP:809-814):
MOCALL -> MCHANDLE, 5FMOCALL -> MCHANDLE, TRAPCODE -> TRAPDECODER, else restart ND-100
process (5RRTWT). VERIFIED both ways. An emulator must model the hardware field; the
driver-visible protocol uses the message field.

### 2.4 CONTROL register (LCON5) values written by SINTRAN

Every LCON5 write in the tree, with source comments:

| Value (oct) | Context | Source comment | Evidence |
|---|---|---|---|
| 40  | 5MCST micro-stop | "DISABLE TAG-IN DECODING" | CC:215 |
| 10  | activate; power-fail; CLE5STATUS | "Enable for interrupt" (MP:3089 label comment "Enabale for interrupt" at 3088) | MP:3089; PH-RESTART:129; XC:55 |
| 0   | activate epilogue in CLE5STATUS / power-fail | (written as `"0"`) | XC:58; PH-RESTART:132 |
| 1   | activate | third step of enable sequence | MP:3091 |
| 5   | reactivate with message (ACT50) | "Reactivate it since an interrupt may have been lost" (MP:3083) | MP:3086 |
| 400 | power-fail stop | power-fail path | PH-RESTART:133 |

VERIFIED. Bit-level interpretation of these values (which bits mean what) is NOT
derivable from NPL alone; NEC-01/manual evidence in section 4 Q5 covers the bit map.
DERIVED (from values used): bit0 (1) and bit2 (4) together = 5 appear in the
reactivate; 10 (bit3) is written before enabling; 40 (bit5) disables TAG-IN decoding
per the CC:215 comment; 400 (bit8) is used only in the power-fail stop.

### 2.5 Key driver sequences (quoted)

#### 2.5.1 Activation - XACT500 (NPL:MP-P2-N500.NPL:3052-3099)

```
XACT500:
        GO XACTRDY                                    % Continue in XACTRDY if nd5000   (patch point *NNJ14)
        ...
        T:=5MBBANK; X:=MAILINK; *AAX X5CPU; LDATX
        IF A=MPACTIVE AND C5STAT NBIT BHPFAIL THEN    % If not in power-fail
           T:=HDEV+RSTA5; *IOXT                       % Read interface status
           A=:500STATUS
           IF A NBIT 5CLOST THEN                      % If nd-500 not stopped (clock stopped)
              IF A BIT 5ILOCK THEN                    % If nd-500 not terminated
                 CALL XTER500; 0/\0
              FI
              % Activate ND-500 with the first message waitng for ND-500 CPU.
              % Status=msgn500 and status=waiting means waiting for ND-500 CPU.
              X:=MAILINK
              DO                                      % Search ex-queueu to see
                 T:=5MBBANK; *LINK@3 LDDTX            % Next message
              WHILE D><-1                             % If any processes is waiting for nd-500 cpu.
                 IF X:=D><DUMMESS THEN
                    CALL RN5STATUS                    % Message.Status
                    IF A=MSGN500 OR A=WAITING GO ACT50% Msgn500 or Waiting?
                 FI
              OD
              % No processes waiting for nd-500 cpu.
              T:=5MBBANK; X:=MAILINK; *LINK2@3 LDXTX
              IF X><-1 AND 500STATUS NBIT 5ILOCK THEN % Has ND-500 been terminated?
                 % Reactivate it since an interrupt may have been lost
ACT50:           5MBBANK; T:=HDEV+LMAR5; *IOXT
                 A:=X; *IOXT
                 A:=5; T+"LCON5-LMAR5"; *IOXT
              ELSE
                 % Enabale for interrupt
                 A:=10; T:=HDEV+LCON5;   *IOXT
                 A:=0;  T+"LSTA5-LCON5"; *IOXT
                 A:=1;  T+"LCON5-LSTA5"; *IOXT
                        T+"SLOC5-LCON5"; *IOXT
                 TTMR=:TMR
              FI
           FI
        FI
OUT:    0=:5CPUSTOPPED
```

VERIFIED. Two distinct hardware paths:

- **ACT50 message path:** LMAR5 twice (first `5MBBANK`, then the message address in X),
  then LCON5:=5. The MAR is loaded bank-first, address-second, by two consecutive IOX
  writes to the same offset.
- **Enable path:** LCON5:=10, LSTA5:=0 (clear status), LCON5:=1, then a write to SLOC5
  (A still holds 1), then arm the software timeout TTMR=:TMR.

#### 2.5.2 Terminate - XTER500 (NPL:MP-P2-N500.NPL:2911-2962)

Header comment: "Routine to terminate (stop) nd-500". Sequence (MP:2933-2946):
read RSTA5; if 5ILOCK set: write TERM5, then re-read RSTA5 in a bounded spin
("Wait for nd-500 to unlock i/f") until 5ILOCK clears; if still locked after the
loop: `CALL X5MCST  % Time out; master clear it`, then error exit ESPTIMOUT.
If already stopped (`5CPUSTOPPED><0`) exits immediately (MP:2928). On success sets
5CPUSTOPPED (MP:2960). VERIFIED.

Note the source comment at MP:2945 calls the 5MCST sequence "master clear" even though
it does not touch the MCLR5 register - see 2.5.3.

#### 2.5.3 Micro-stop / "master clear" - 5MCST (NPL:CC-P2-N500.NPL:207-217)

```
% Subroutine to stop N500 (Micro stop)
SUBR 5MCST,X5MCST
X5MCST: T:=HDEV
5MCST: T+UNLC5; *IOXT                   % UNLOCK
       A:=40; T+"LCON5-UNLC5"; *IOXT    % DISABLE TAG-IN DECODING
       A:=2;  T+"RETG5-LCON5"; *IOXT
       EXIT
```

VERIFIED. The routine header says "stop N500 (Micro stop)"; the caller comment
(MP:2945) says "master clear it". Sequence: UNLC5 (no data), LCON5:=40, RETG5:=2.
The MCLR5 register (offset 6) is NOT used. Called from XTER500 timeout (MP:2945).

#### 2.5.4 Power-fail stop - 5PF path (NPL:PH-P2-RESTART.NPL:100-139)

For each ND-500 CPU datafield: if CPU type (CPUAVAILABLE/\5CPUTYPE) is SAMSON, skip the
register path (PH-RESTART:112-115). Otherwise (PH-RESTART:116-135):

```
*AAX HDEV-C5PWF; LDATX
A+RSTA5=:T; *IOXT
IF A BIT 5ILOC THEN
   T+"TERM5-RSTA5"; *IOXT
   FOR X:=-10 DO; OD
   T+"RSTA5-TERM5"; *IOXT
   IF A BIT 5ILOC THEN
         GO NCPUPF      % NEXT CPU IF THIS ONE DID NOT STOP
   FI
FI;
...
A+LCON5=:T; A:=10; *IOXT
T+"RSTA5-LCON5"; *IOXT
A BONE 5POWOF; T+"LSTA5-RSTA5"; *IOXT
A:="0"; T+"LCON5-LSTA5"; *IOXT
A:=400; *IOXT
T+"SLOC5-LCON5"; *IOXT
T+"TERM5-SLOC5"; *IOXT
```

VERIFIED: read status; terminate if locked (short wait, re-check, give up if still
locked); then LCON5:=10, read RSTA5, write status back with 5POWOF bit set (LSTA5),
LCON5:=0, LCON5:=400, SLOC5, TERM5.

#### 2.5.5 Detection - CH5CPUPRESENT (NPL:PH-P2-OPPSTART.NPL:3895-3945)

```
% Subroutine called from the start-up (SINTR) sequence to check if
% the generated ND-500 cpu's (SAMSON) exists or not
% EXIT:   No ND-500 cpu is presnet
% EXIT+1: ND-500 cpu is present
...
T:=HDEV+RSTA5; *TRA IIC
A:=200; *TRR IIE; IOXT; TRA IIC
IF A=0 THEN                                % If not IOX-error then
   CPUAVAILABLE/\140000\/OLD500
   A BONE 5ALIVE
   MIN COLD
ELSE
   ...
1CH5CPU:     *TRA IIC                                % Read octobus if. status
             A:=200; *TRR IIE
             T:=100406; *IOXT; TRA IIC
             IF A=0 THEN                             % Octobus present? - (assumes Samson)
                DO                                   % Wait for data ready
                   *IOXT
                WHILE A NBIT 3
                OD
                ASTATION\/COMD=:5STATION
                A SH 10 BONE CBIT BONE EBIT=:X       % To start Samson selftest:
                T:=100405; A\/CMMACLE; *IOXT         % - send "masterclear Samson system" frame
                A:=X\/CMACONT; *IOXT                 % - send "continue accp" frame
                ...
                CPUAVAILABLE/\140000\/SAMSON
```

VERIFIED. Polarity is unambiguous: arm the internal-interrupt enable for IOX errors
(A:=200 -> TRR IIE), attempt `IOX read of RSTA5`, read IIC; **A=0 (no IOX error
recorded) means the 3022 IS present** ("If not IOX-error then" -> flag OLD500 and
5ALIVE). If the IOX faults, probe the Octobus (IOX 100406 status read, wait for bit 3
data-ready, then IOX 100405 frames CMMACLE "masterclear Samson system" and CMACONT
"continue accp") to detect a SAMSON/ND-5000.

Also VERIFIED here: `CPUAVAILABLE/\140000\/OLD500` masks CPUAVAILABLE with 140000
(KEEPING only the top two bits) and ORs in the type value - so the CPU-type values
OLD500/SAMSON live in the LOW bits of CPUAVAILABLE, and 140000 preserves high flag
bits. 5CPUTYPE is used elsewhere as a low-bit mask: `CPUAVAILABLE/\5CPUTYPE><SAMSON`
"DMA interface?" (MP:265), `A=:B/\5CPUTYPE = SAMSON` (PH-RESTART:112). See C8 in
section 5.

#### 2.5.6 Level-12 ISR - 5STDRIV / N500 / XN500 (NPL:MP-P2-N500.NPL:650-721)

Header (MP:654): "L e v e l  1 2  -  ND5000 communication driver kernel".
`SUBR 5STDRIV,N500,XN500,NXTMSG,CALLID12` (MP:656).

N500 path (old 500, MP:661-694): if power-fail pending, exit to CALLID12; call
CLE5STATUS with mask 177377 ("READ STATUS AND MASK IT", MP:668); if `A/\720><0`:
5PFAIL -> set BHPFAIL, restart all processes with "power down" (KPOWDOWN);
5DMAERR -> N5DMAERR; else N5IERR; all -> N500ERR -> XRSTARTALL (restart all).
Otherwise scan the execution queue from MAILINK following LINK fields
(`T:=5MBBANK; *LINK@3 LDDTX`) until -1; for each non-DUMMESS message call CHN5STATUS;
then `CC5CPU=:B; CALL XACT500` and wait (CALLID12: CALL WT12).

XN500 path (ND-5000, MP:700-720): drain a FIFO in shared memory - compare fetch
pointer X5HEN against fill pointer X5FYL, wrap at X5MXF, element address computed from
X5FIF ("HENTE*FIFO ELEMENT SIZE+FIFO START", MP:709), check message flag 5MSFL bit
5IEXQUEUE, dispatch via CHN5STATUS.

CHN5STATUS dispatch on the message status word (MP:730-758, read via RN5STATUS):

| Status | Action | Line |
|---|---|---|
| ANSWER, X=HIMESS | HISTSAMPLE (histogram) | MP:736-737 |
| ANSWER, X=WATCHDOG | SLOCK; IFM500XQ; SUNLOCK; reset TMRXQ/TMR (timer message) | MP:740-744 |
| ANSWER, other | DECOMESS (decode answer) | MP:746 |
| 5ERANSWER | DECOERRMESS (decode error answer) | MP:749-750 |
| > 100 | 5RRTWT (restart ND-100 process) | MP:751-752 |
| MSGN500 or WAITING | XTER500 (terminate ND-500) | MP:753-755 |

VERIFIED. Note: earlier analysis claimed MSGN500/WAITING meant "still waiting - no
action"; the code actually calls XTER500 on that case in CHN5STATUS (see 6.3). The
"waiting" SEMANTICS appear in XACT500/XACTRDY (a message with status MSGN500 or
WAITING is one waiting for the ND-500 CPU - MP:3069-3070 comment, MP:2984), while
CHN5STATUS seeing such a status on the answer path triggers a terminate.

The interrupt LEVEL is 12: header comment (MP:654), wait call WT12 (MP:693), ident
tables ID12T/ITB12 whose physical addresses are computed at startup
(PH-OPPSTART:3176-3177). The numeric IDENT CODE of the 3022 is NOT found as a named
constant in this source set: UNVERIFIED (see section 3).

#### 2.5.7 DECOMESS stop-reason dispatch (NPL:MP-P2-N500.NPL:790-819)

```
DECOMESS:
       T:=5MBBANK; *AAX SPFLA; LDATX; AAX -SPFLA
       IF A><0 THEN A=:P FI                     % GOTO ROUTINE ADDR FOUND IN SPFLAG
       *MICFU@3 LDATX                           % MIC.FUNC
       IF A=3MONCO OR A=3TRACO OR A=3START OR A=3WMONCO THEN
          T:=5MBBANK; *AAX STOPR; LDATX; AAX -STOPR
          IF A=MOCALL THEN CALL MCHANDLE        % STOP-REASON IS MON.CALL
          ELSE IF A=5FMOCALL THEN CALL MCHANDLE % STOP-REASON IS FILE-TRANSFER MONCALL
          ELSE IF A=TRAPCODE THEN CALL TRAPDECODER % STOP-REASON IS TRAP
          ELSE CALL 5RRTWT                      % RESTART ND-100 PROCESS
          FI FI FI
       ELSE
          CALL 5RRTWT
       FI
       GO NXTMSG
```

VERIFIED. Monitor calls and traps are signalled through the MESSAGE (MICFU + STOPR
fields), not through interface registers.

### 2.6 Shared message memory (5MPM side)

#### 2.6.1 Message bank - 5MBBANK (NPL:RP-P2-N500.NPL:732-745, XMSINIT)

```
XMSINIT: ...
       5FPMAILBOX=:D:=0; AD SH 12; A=:5MBBANK   % MEMORY BANK FOR MESSAGES
       A=:T; 5NPMAILBOX SH 12 -1+D; D=:X
       DO WHILE X><A ... *STZTX ... OD          % (clear the mailbox area)
```

VERIFIED: 5MBBANK is derived from 5FPMAILBOX (first physical mailbox page) by a 12-bit
(octal) shift; XMSINIT zeroes the whole mailbox area and then builds the shared-memory
extension datafields, per-process message buffers, histogram message and watchdog
message. XMSINIT header comment (RP:725-728): "Clear the memory area used for nd-500
messages (maiboxes), and setup the message addr in all used nd-500 process
descriptions." All message-memory access in the driver uses `T:=5MBBANK` +
LDATX/STATX/LDDTX/STDTX/STZTX physical windowing instructions.

#### 2.6.2 Message field offsets (symbol table M06, N500-SYMBOLS.SYMB.TXT)

| Symbol | Offset (oct) | Meaning (from usage) | SYM line |
|---|---|---|---|
| LINK / LINK1 | 0 | queue link (double: LINK@3 reads a two-word link) | 5405, 7197 |
| LINK2 | 1 | second link word | 7202 |
| N5STA | 2 | message status word (read by RN5STATUS / written by WN5STATUS; XACTRDY reads `*N5STA@3 LDATX`, MP:2983) | 5827 |
| SENDE | 3 | sender (watchdog init writes -1, RP:821) | 4659 |
| X5CPU | 4 | CPU field (checked = MPACTIVE, MP:3061-3062) | 6979 |
| X5ACT | 5 | activation field (STZTX in ACT51, MP:3027) | 6936 |
| MICFU | 6 | micro-function / command code | 5333 |
| N500A | 7 | ND-500 (logical) address | 5826 |
| N100A | 11 | ND-100 (physical) address | 5824 |
| STOPR | 11 | stop reason (same offset as N100A - overlaid by direction) | 3241 |
| ACPRO | 11 | actual process (another overlay of offset 11) | 2951 |
| NRBYT | 13 | byte count | 6131 |
| PLINK | 147 | process link | 2586 |
| 5MSFL | 177777 (-1) | message flag word (bit 5IEXQUEUE tested MP:715; bit 5CPUBOUND MP:2995; escape-disable via 5IBRK MP:2871-2873) | 1503 |

VERIFIED (offsets); the direction-dependent overlay of offset 11 (N100A vs STOPR vs
ACPRO) is DERIVED from the three symbols sharing the value and their distinct usage
contexts.

#### 2.6.3 Per-CPU shared-memory extension (X500DF) fields

| Symbol | Offset (oct) | Meaning | SYM line / usage |
|---|---|---|---|
| X5HEN | 3 | FIFO fetch ("hente") pointer | 6696; MP:704 |
| X5FYL | 4 | FIFO fill ("fylle") pointer | 6608; MP:705 comparison "WHILE HENTE><FYLLE" |
| X5MXF | 5 | FIFO max (wrap) | 6500; MP:703,706 |
| X5FIF | 6 | FIFO base | 6499; MP:709 |
| X5RES | 47 | semaphore owner (-1 = held by ND-100) | 6885; CC:728-729,755,767-769 |
| X5SEMA | (field, offset value not in N500-SYMBOLS extract) | test-and-set word | CC:716,722,766 |

VERIFIED except X5SEMA numeric offset (UNVERIFIED - referenced as `"X5SEMA"` by name).

#### 2.6.4 Semaphore protocol - SLOCK/SUNLOCK (NPL:CC-P2-N500.NPL:702-772)

- On "old" 500 both SLOCK and SUNLOCK exit immediately (patch points *NNJ05/*NNJ06,
  CC:711-712, 763-764: "Direct exit+1 if old 500"). The semaphore is an ND-5000-era
  mechanism.
- Test-and-set instruction: `TSET(0); *140123` "Logical test-and-set instr. for not
  Rask cpu" and `TSETP(0); *140516` "Physical test-and-set instr. for Rask cpu"
  (CC:707-708). RASK CPUs use TSETP directly on the physical X5SEMA address; non-RASK
  CPUs map the semaphore page through window WNDN5 first (CC:719-723).
- Lock owner convention: after winning the test-and-set, ND-100 writes -1 to X5RES
  ("Set N100 as current reserving CPU (N100=-1)", CC:753-755). SUNLOCK releases only
  if X5RES = -1 (CC:768-769).
- Timeout: two-level spin with documented timing ("Total wait time: appox. 100
  millisec. per loop", "Wait time per Test-and-set: appox. 100 microsec.",
  CC:733-737); on failure N5LTIMOUT error (CC:751).

VERIFIED.

#### 2.6.5 MICFU command codes (semantics verified, values UNVERIFIED)

Codes written to MICFU by SINTRAN, with locations:

| Code symbol | Context | Evidence |
|---|---|---|
| 3MONCO | restart after monitor call (also written by 5ACTSWAPPER before MCCO, MP:2887) | MP:808, MP:2887 |
| 3START | start process | MP:808 (dispatch), MP:2991 (XACTRDY set) |
| 3TRACO | trap continue | MP:808 |
| 3WMONCO | wait monitor call | MP:808 |
| 3FITRNSF | file transfer | MP:2991 |
| 3RPREG | read P register (histogram message) | RP:803, RP:811 |
| 3RMICV | read microprogram version (watchdog message) | RP:282, RP:384, RP:822 |
| 3SWMESS / SWFUN | message to swapper / swapper function | MP:2876-2877 |
| 3RMED / 3WMEP | (error-answer decode context) | MP:839 |

The NUMERIC VALUES of these codes (and of the status codes ANSWER, 5ERANSWER, MSGN500,
WAITING, SWPWAIT, PSWWAIT, SWPPING, DUMMESS) are NOT defined anywhere in this
repository's NPL sources, symbol tables, or the s3vs-4.symb listing (grep performed
2026-07-08; only `SYMBOL LSWPWAIT=4` at s3vs-4.symb:58518 exists). They come from a
definitions module not present here. UNVERIFIED values / VERIFIED semantics.
(Manual evidence may supply values - see section 4.)

### 2.7 Swapper (interface-relevant summary)

- 5SWRT is an ND-100 RT-program: `SUBR 5SWRT` starts with `*2BANK; IOF`, reserves
  ND-500 process #0 (S500S), performs the disk transfer with `"ABSLI"; *MON 131`
  (NPL:RP-P2-N500.NPL:16-58, quoted lines 17, 19, 37). It waits on message status
  PSW1WAIT and reports SWDERR on transfer error (RP:32, 48). VERIFIED - the swapper
  disk work runs ON THE ND-100.
- 5ACTSWAPPER (NPL:MP-P2-N500.NPL:2851-2908) hands a message to ND-500 process #0:
  under SLOCK, if the swapper message SWMSG has status PSWWAIT (free), it fills
  HSWPI/SWPFU/SWPST/NUMPA/FUNCV fields, sets MICFU:=3MONCO and calls MCCO then
  XACTRDY; otherwise inserts the request into the swap-wait FIFO (X5SWF fill pointer,
  X5SWB base). VERIFIED.

### 2.8 HDEV correction

HDEV (177775) and XHDEV (177774) are standard B-relative datafield slots for a device
number (SYM:M06:7002-7003). The often-cited line `X.SWHDEV=:HDEV`
(NPL:PH-P2-OPPSTART.NPL:274) belongs to DPRE, the DISK driver preparation routine
("DF. FOR DISK DRIVER", PH-OPPSTART:268) - it is NOT the ND-500 interface setup.
For the 3022, HDEV is simply read out of whichever ND-500 CPU datafield B currently
points at (e.g. MP:266,3063 run with B = CPU datafield; the multi-CPU loops step B
with `B+5CPUDFSZ`, MP:254,297, PH-OPPSTART:3941). The interface base device number is
therefore per-CPU configuration data; no constant exists in these sources.

---

## 3. Open items NPL cannot answer - status after manual mining

Items 1, 2, 3, 5 and 8 were RESOLVED by the hardware manuals (section 4); items 4, 6,
7 remain open.

1. **Ident code(s) of the 3022.** RESOLVED: ident 16/116/36/114/76 octal for
   thumbwheel 0-4 (ND-06.015.02, section 4.1). Interrupt level 12 is confirmed at
   both the hardware level (ND hardware catalogue entry for the 3022) and the
   software level (4.7).
2. **Function of MCLR5 (offset 6).** RESOLVED: hardware master-clear command strobe;
   restarts the microprogram at control-store address 0 (4.2). SINTRAN never issues
   it (2.2).
3. **Function of CLKD5, 5MODE, RTAG5/LTAG5, RUPP5/LUPP5, RLOW5/LLOW5.** RESOLVED for
   TAG (4.5: microcode-level control channel), limits (4.6: DMA bounds registers,
   test-mode loadable), CLKD (4.4/4.5: clock DATA in locked mode). 5MODE (offset 20)
   remains UNVERIFIED - it is outside the 60-77 register range documented by TMP
   section 3.14 and was not found in the manuals; possibly a later-generation or
   generation-specific register.
4. **Numeric values of message status codes and MICFU codes.** STILL OPEN as verified
   numbers. Manual hint (4.9): status 0=free, 1=message to ND-500, 2=in process,
   3=answer, 4=error return; plausible mapping MSGN500~1, ANSWER~3, 5ERANSWER~4.
   Treat as UNVERIFIED values.
5. **Microcode load procedure.** RESOLVED (4.8): WA/BREAK/CSCNT via TAG-IN strobes,
   144-bit words in 9 parts, LOAD-CONTROL-STORE at the operator level. SINTRAN NPL
   does not contain the loader loop because the Loader Monitor performs it.
6. **Numeric values of OLD500, SAMSON, 5CPUTYPE, 5ALIVE, 5NOTPRESENT.** STILL OPEN
   (see C8). Usage pattern verified: type in low bits, flags in high bits.
7. **X5SEMA numeric offset** within X500DF. STILL OPEN.
8. **CONTROL/STATUS bit meanings.** RESOLVED: full hardware bit maps in 4.3,
   including STATUS bits 10-14 = ND-500 stop reason (values of the stop-reason codes
   remain UNVERIFIED - C12).

---

## 4. Hardware-manual evidence

Manuals mined 2026-07-08. Key identification: **"TMP" = ND-30.013.02 Test Micro Program
Descriptions for ND-500** (`../../Reference-Manuals/500/ND-30.013.02 Test Micro Program
Descriptions for ND-500.md`), whose section 3 ("A short list of registers, IOX
instructions etc.") is a register-level specification of BOTH cards (3022 ND-100 side,
5015 ND-500 side). The SAME section-3 text is reproduced in
`../../Reference-Manuals/500/NEC-01 - ND-500 course.md` (with heavier OCR damage) -
NEC-01 and TMP share one source text, which is the origin of the "four-mode" IOX table
found in older docs and in the C# emulator header. TMP is the cleaner copy; where OCR
diverges, TMP is cited. Both are OCR conversions: register NAMES may carry OCR noise;
positions/codes are consistent across both copies.

### 4.1 IOX device numbers, thumbwheel, ident codes

ND-06.015.02 ND-100 Functional Description, section D.13.1 "Device Number Setting
(Thumbwheel)":

| Thumbwheel | Device number (oct) | Register range (oct) | Ident (oct) |
|---|---|---|---|
| 0 | 60 | 60-77 | 16 |
| 1 | 1060 | 1060-1077 | 116 |
| 2 | 660 | 660-677 | 36 |
| 3 | 760 | 760-777 | 114 |
| 4 | 560 | 560-577 | 76 |

"Thumbwheel settings 5-15 are not valid." Register offset = device number - base.
ND-06.016.01 (device-address table) labels 60-77 as the old "NORD-50/1 Regs." slot,
which the ND-500 interface inherited. VERIFIED (manual).

This resolves open item 3.1 (ident codes) and C10 (there is no one "typical" HDEV; the
five valid bases are 60/1060/660/760/560).

### 4.2 Master Clear at offset 6 (MCLR)

TMP section 3.14 lists `MCLR 066 ND-500 Master Clear` in both the "Locked and Not in
Test Mode" and "Unlocked and not in test mode" tables. TMP section 6.3.1 (TST01): "Will
give master clear (IOX MCLR) continually... This routine may be used to start a micro
program in address 0 again and again, if the stop bit is off."

MCLR is a COMMAND STROBE: executing the IOX performs the action (restart microprogram
at control-store address 0); no data word matters in non-test mode. In unlocked+test
mode the same offset is re-decoded to "Load DATA register" (section 4.4). This closes
the offset-6 question: MCLR5=000006 is real hardware; SINTRAN simply never uses it
(section 2.2), stopping the machine with 5MCST (UNLC5/LCON5:=40/RETG5:=2) instead.

### 4.3 CONTROL and STATUS register bit maps (hardware)

TMP section 3.1, CONTROL word register on 3022:

| Bit | Meaning |
|---|---|
| 0 | Enable interrupt from ND-500 |
| 1 | Not used |
| 2 | Activate ND-500 operation (and lock the communication) |
| 3 | Test mode |
| 4 | ND-500 programmed clear |
| 5 | Disable TAG-IN decoding when locked |
| 6 | DMA error |
| 7 | Command chaining |
| 8-14 | ND-500 operation |
| 15 | Not used |

TST25 adds: "Verify 3022 CONTROL register (Bit 2=0. Bit 4=1 clears bit 6)" - the
programmed clear's side effect clears the DMA-error status.

TMP section 3.2, STATUS register on 3022:

| Bit | Meaning |
|---|---|
| 0 | Interrupt enabled |
| 1 | Not used |
| 2 | ND-500 busy |
| 3 | ND-500 finished |
| 4 | Error |
| 5 | Interface locked |
| 6 | DMA error |
| 7 | ND-500 power fault (set by micro program). The stop bit is set |
| 8 | ND-500 power is/has been off |
| 9 | ND-500 micro clock has stopped |
| 10-14 | ND-500 stop reason |
| 15 | CONTROL register bit 15 |

TST26: "Verify STATUS register (not bits 0, 5, 011 and 017)" - bits 0, 5, 9, 15 are
not settable via IOX LSTA. VERIFIED (manual). The hardware bit map agrees exactly with
the NPL-derived bits in section 2.3 (bits 4-9) and extends them (0,2,3,10-14,15).

**This decodes the NPL CONTROL values of section 2.4:**

| Value (oct) | Bits | Hardware meaning |
|---|---|---|
| 1 | bit0 | enable interrupt from ND-500 |
| 5 | bits0+2 | enable interrupt + activate (lock) - the reactivate-with-message write |
| 10 | bit3 | TEST MODE - written before LSTA5 so that the status write is legal (see 4.4) |
| 40 | bit5 | disable TAG-IN decoding when locked (matches CC:215 comment) |
| 44 | bits2+5 | activate + disable-TAG (TMP section 3.15.1 idiom `SAA 044; IOX LCON`) |
| 400 | bit8 | low bit of "ND-500 operation" field (power-fail path) |

DERIVED: the NPL comment "Enable for interrupt" on `A:=10` (MP:3089) describes the
GOAL of the whole 4-write sequence, not the value 10 - the value 10 is test mode,
required to make the following LSTA5 write decode (four-mode table, 4.4). The
sequence is: enter test mode (10), clear status (LSTA5:=0), leave test mode + enable
interrupt (1), set lock (SLOC5).

### 4.4 The four-mode decode (origin of the C# header table)

TMP section 3.14: "The ND-500 communication can be locked or unlocked, in test mode or
not in test mode. These states are set by IOX LCON (load CONTROL register). IOX
instructions have different meanings, depending on the state."

1. **Locked + not test:** RSTA 062, MCLR 066, TERM 067, RTAG 070, WTAG 071, WDAT 073,
   SLOC 074, CLKD 075, UNLC 076, RETG 077. (RMAR/LMAR/LCON/LSTA/RCON NOT available.)
2. **Locked + test:** ONLY RSTA 062 and RCON 064.
3. **Unlocked + not test:** RMAR 060, LMAR 061, RSTA 062, LCON 065, MCLR 066,
   TERM 067, RTAG 070, WTAG 071, WDAT 073, SLOC 074, UNLC 076, RETG 077.
4. **Unlocked + test:** offsets re-decode to diagnostic functions: RMAR 060 "(do it
   twice)", LMAR 061 "(do it twice)", RSTA 062, LSTA 063 (load STATUS), RCON 064,
   LCON 065, 066 -> Load DATA register, 067 -> Load DATA register, 070/071 -> Load
   upper limit register, RLOW 072 (read lower limit), 073 -> Load lower limit.
   "ND-100 bits 0-15 go to limit register bits 8-23."

VERIFIED (manual). Notes: LSTA (write STATUS) exists ONLY in unlocked+test mode -
which is exactly why every SINTRAN status-write sequence first writes LCON5:=10
(sections 2.5.1, 2.5.3-2.5.5). Mode-select bits: CONTROL bit2 = activate+lock,
bit3 = test mode, bit5 = disable TAG-IN decode (4.3).

### 4.5 TAG-IN / TAG-OUT (hardware function)

The TAG registers are the microcode-level control channel between the cards - real
hardware, used by the ND-500 microcode and by test programs; NOT used by the SINTRAN
driver (section 2.2), and NOT a message-passing protocol.

TMP section 3.12, TAG-IN register on 5015 (written by ND-100 via WTAG 071): "Bits 0-3
in the TAG-IN register on 5015 give 16 code values. Bit 4 is not used, and bit 5
(octal 040) is used to return TAG-IN bits (0-4)." Decoded codes (names OCR-corrected;
positions reliable): 0 not used, 1 DICLK1 (clock DATA-IN-1), 2 DICLK2 (clock
DATA-IN-2), 3 DUCLK (clock DATA-OUT), 4 WACLK (clock write-addr), 5 BRKCLK (clock
BREAK), 6 TGCLK (clock TAG-OUT), 7 CNTCLK (clock CSCNT), 8 DIEN (enable DATA-IN to
CDB), 9 DUEN (enable DATA-OUT least significant), 10 WAR (read write-addr), 11 BRKR
(read BREAK), 12 CNTR (read CSCNT), 13 RESBRK (reset break), 14 DUNL (unlock),
15 EOUTEN (enable data line driver).

TMP section 3.13, TAG-OUT register on 5015 (driven by ND-500 microcode toward the
3022): "Bits 0-2 ... give 8 code values. Bit 3 means ND-100 if it is 0... Bit 7 is
the MOST bit" (selects most/least half of 32-bit registers): 0 read MAR, 1 write MAR,
2 read STATUS, 3 write STATUS, 4 read CONTROL, 5 reset activate, 6 read DATA register
(and ND-100 memory), 7 write DATA register (and then into ND-100 memory).

IOX RETG (077, "return tag"): A-bit0 = reverse tag bus, A-bit1 = stop bit (TMP
sections 6.3.2/6.3.4 - TST02 flips the stop bit via RETG bit1, TST04 flips the
reverse-tag-bus via RETG bit0). This explains 5MCST's `A:=2; RETG5` = SET THE STOP
BIT (section 2.5.3) - "Micro stop", exactly as the 5MCST header comment says.

VERIFIED (manual). **There are NO TAG codes 8/9/16 = MonitorCall/PageFault/
OperationComplete anywhere in the hardware documentation.** TAG-IN code 8 is DIEN;
codes are 4 bits (16 impossible as a code value; the emulator's "code 16" cannot
exist on the wire).

### 4.6 MAR, DATA/DATAX, and the DMA limit registers

TMP section 3.3: MAR on 3022 "is a 24-bit register, pointing to the ND-100 memory. It
is used in DMA transfers. It must be loaded from the 16-bit A-register in two
operations. The most significant part is loaded first. It must also be read in two
operations. The least significant part will be read first."

This matches ACT50 exactly (section 2.5.1): first LMAR5 write = 5MBBANK (most
significant/bank part), second = message address (least significant part).

DMA staging: DATA register (ND-500 -> ND-100) and DATAX register (ND-100 -> ND-500)
(TMP sections 3.4/3.5); transfers initiated by ND-500 TAG-OUT codes 6/7; STATUS bits
2/3 busy/finished; bit 6 DMA error. ND-60.136.04A: "The ND-100 memory is accessed by
DMA, and does not interrupt the ND-100 program execution."

TMP section 3.10, limit registers: "These are 16-bit registers, and represent bits
8-23 of a DMA address. They are compared with bits 8-23 of the MAR register to ensure
that ND-500 keeps within limits." Loadable only in unlocked+test mode (4.4). Purpose:
hardware bounds guard on ND-500-initiated DMA into ND-100 memory. VERIFIED (manual).

### 4.7 Interrupt level

The mined manuals do NOT state the interrupt level; TMP explicitly avoids interrupts
("They do not use the interrupt system"). Software evidence: ND-60.133.02A SINTRAN III
Real Time Guide - "HDLC output and the ND-500 driver are handled by level 12" - plus
the NPL evidence (section 2.5.6). RESOLVED 2026-07-08 at the hardware level too: the
ND hardware catalogue entry for the PCB 3022 ("N-100 N-500 Interface"; supplied by
project owner) states the card is connected to hardware interrupt level 12, with
switch J12 selecting the device number and connectors A = ND-500 interface, B = not
present, C = standard ND-100 system bus. VERDICT: level 12 confirmed at BOTH levels;
ident 16/116/36/114/76 per thumbwheel confirmed (4.1).

### 4.8 Microcode (control-store) loading

Resolves open item 3.5. A control-store word is 144 bits = 9 x 16-bit parts
(ND-60.136.04A section 8.10.6.3 "Every microprogram word (144 bits, 18 bytes)"; TMP
section 3.16). NEC-01 p.41: "The NORD-100 controls the XD bus when writing into the
writable part of the control store... Sixteen bits are transferred... at a time. The
CONTROL STORE CONTROL register bits 2-5 (decoded as CS8-0) control which part... After
9 accesses a complete NORD-500 control store word is written... Control store control
register bit 0 equals 1 means: Control store load. While bit 1 equals 1 means: control
store read."

Procedure per word-part (TMP section 3.16.1): control-store address -> WA register
(TAG-IN WACLK), data word -> BREAK register (BRKCLK), part-select control word ->
CSCNT (CNTCLK). Read-back via DATA-OUT-1 (TMP section 3.16.2). Operator layer:
ND-60.136.04A LOAD-CONTROL-STORE (default file CONTROL-STORE:DATA, default size
20000B words), COMPARE-CONTROL-STORE, MICRO-START, and automatic microcode reload on
warm start. VERIFIED (manual).

### 4.9 The message-block protocol (Micro Program Guide)

ND-05.012.01 ND-500 Micro Program Guide section 13: the message block "resides in
RESIDENT of SINTRAN III"; 6-word ND-100 header (Next link x2, Status, Sender,
Receiver, Size) followed by a data part (function value + parameters). Message STATUS
VALUES: 0 = Block free, 1 = Message to ND-500, 2 = Message in process, 3 = Answer to
ND-100, 4 = Error return from ND-500. "Nothing but an activate or a terminate from
the ND-100 can cause the micro program to leave the IDLE loop."

DERIVED correlation with the symbol-table offsets (2.6.2): link words = LINK/LINK1(0),
LINK2(1); Status = N5STA(2); Sender = SENDE(3); Receiver ~ X5CPU(4); Size ~ X5ACT(5);
function value = MICFU(6). The header layout matches the verified offsets one-to-one.
PLAUSIBLE value mapping (NOT verified as equalities): MSGN500 ~ 1 ("Message to
ND-500"), ANSWER ~ 3 ("Answer to ND-100"), 5ERANSWER ~ 4 ("Error return"). WAITING
and DUMMESS have no manual counterpart found. Carry as UNVERIFIED values with this
manual hint.

### 4.10 5MPM (ND-10.004.01 MPM 5 Technical Description)

- Twin 16-bit port module (PCB 5152 or 5155): "2 x 16 bits wide and can be connected
  to both 16-bit and 32-bit width data channels... up to 29 address bits (addressing
  32-bit words)." ND-500 receives 32(+4) bits per access; ND-100 16(+2). Same
  physical RAM; widths reconciled by interleave.
- Channel vs bank addressing: "The address seen from the source is called the channel
  address. The bank, however, has its own local address range." BASE register
  (section 1.4.4): "the value which is placed in the base register by the program, is
  2's complement of the base subtracted from lower limit" (worked example: lower
  limit 000020, base 000002 -> register value 377762). Base increment 128 KB
  (64KW as words).
- Port address windows set by lower/upper limit on the port (test-and-maintenance
  program on the controller module); resolution 128 KB for 32-bit / 64 KB for 16-bit
  channels.
- Port control register (8 bits): bits 0-1 interleave, 2-3 bank number, 4-5 speed-up,
  bit 6 channel width, bit 7 wait. WARNING: the manual contradicts itself on bit-6
  polarity (one place says 1=32-bit channel, another says 0=32-bit); UNRESOLVED in
  the source - flag to the emulator.
- VERIFIED (manual): 5MPM is separate multiport hardware with per-port BASE/window
  translation - NOT pages allocated out of ND-100 RAM. Note for C2: on DMA-3022
  systems the message block lives in SINTRAN RESIDENT memory reachable through the
  24-bit MAR (4.6, 4.9); multiport memory makes the same physical RAM visible to both
  machines at different channel addresses.

### 4.11 ND-5000/SAMSON (ND-05.020.01, ND-05.009.4)

ND-05.020.01 section 5.1: the ND-5000 communicates via the Octobus through an access
module with an MC68000 "access processor (ACCP)"; the Octobus "is used to pass
interrupts between CPUs and I/O processors and controllers in the shared-memory
environment around the multifunction bus (MFbus)". ND-05.009.4 section 1.3: I/O
processor <-> CPU communication is "a mailbox and DMA transfer system. The mailbox
contains 3 registers: Control register... Status register... Address register" -
the conceptual successor of the 3022 CONTROL/STATUS/MAR triple. Octobus protocol:
ND-05.020.01 Appendix 2 (Octobus Protocol Version 5).

NOT FOUND in any manual: the IOX device numbers 100405/100406 (used by
CH5CPUPRESENT, 2.5.5) and the symbol MAILINK. These are SINTRAN-side constructs;
carry as software-verified (NPL) / hardware-undocumented.

### 4.12 5015-side registers (for completeness)

TMP sections 3.6-3.13: DATA-IN (32-bit, two halves), DATA-OUT (32-bit, two halves),
BREAK (16-bit, control-store data), WA (control-store write address), CSCNT
(control-store control register: bit0 CSLOAD, bit1 CSREAD, bits2-5 WE0-3 part
select, bit6 BRKEN, bit7 STADREN, bit8 TSTPTTY, bit9 TSTTIQ "Returns TAG-OUT instead
of TAG-IN", bit10 CSPTY, bit11 AFIN, bit12 PFIN, bit13 BALM), TAG-IN, TAG-OUT with
MOST bit. Card naming confirmed: 5015 = CONTROL II. The companion card PCB 5012
"N-500 Control 1" (part number 322512) is CPU controller #1 in the ND-500/1 CPU -
OR logic, ALU functions and loop control, typical position #14 in the ND-500/1
crate (ND hardware catalogue entry for part 322512; supplied by project owner
2026-07-08). The 5012 is internal to the ND-500 CPU, not part of the ND-100
communication path.

### 4.12b Physical cable between the machines (NEC-01)

NEC-01 section "INTERNAL - EXTERNAL CABLE ND 500 - ND 100 I/O" (drawing 3-9387 B,
21.08.80; NEC-01 file lines 3282-3358) gives the complete 64-wire pinout of the
ND-100 <-> ND-500 connection: a 64-wire FLAT CABLE of differential pairs (each
signal has a 0/1 polarity wire pair), routed europlug (ND-500 rack) <-> 2x37-pin
D-connector plug panels <-> europlug (ND-100 backwiring, i.e. 3022 connector A).
Signals: DBU 0-15 (16 data pairs), TIN 0-4 (5 TAG-IN pairs), UNLOCK, PWR.FAIL,
MSTR.CH, RETAG, DATA IN, SPARE, ACTIVATE, STOP, DATA OUT, grounds. "EXTERNAL CABLE
TYPE 1 / TYPE 2: 64 wire flat cable." VERIFIED (manual). Corroborates: the DBU bus
is 16 bits wide (32-bit transfers cross in halves, MOST bit), and master clear /
activate / stop / unlock / return-tag are DISCRETE signal lines - consistent with
their strobe semantics (4.2, 4.5).

Connector positions: the ND-100 side pins are labeled Aa n / Ac n = europlug
position A, rows a and c - independently confirmed by the ND hardware catalogue
("3022 connector A = ND-500 interface"). The ND-500 rack side pins are labeled
Ca n / Cc n = europlug position C, rows a and c.

5015 catalogue entry (part 322515, supplied by project owner 2026-07-08): all four
5015 connectors (A, B, C, D) are listed as "Standard ND-500 bus" - the card has NO
dedicated interface edge connector. DERIVED conclusion: the 64-wire flat cable
terminates on the ND-500 RACK BACKWIRING europlug at position C (rows a and c) of
the 5015's crate slot; the DBU/TIN/control signals reach the 5015 through
slot-specific backwiring on the crate backplane, not through a front/edge cable
connector. Consistent with the drawing's "INTERNAL CABLE: ND-500" note (plug panel
to backwiring) and with the 5015 inventory listing "Drivers/receivers for data
bus/tag bus + control signals" (NEC-01 p.57).

### 4.13 Reference sequences for emulator validation (TMP section 3.15)

TMP section 3.15.1 "Master clear, set stop bit, reset tag bits" (MAC code, quoted):

```
IOX UNLC          % unlock
SAA 040; IOX LCON % disable TAG-IN decoding
SAA 2;   IOX RETG % set stop bit
IOX MCLR          % master clear
SAA 0;   IOX WTAG % write TAG-OUT on 3022
SAA 044; IOX LCON % activate (bits 2+5)
IOX UNLC          % unlock
SAA 040; IOX LCON % reset activate
```

Note the first three lines are exactly SINTRAN's 5MCST (2.5.3) - the test-program
version continues with the actual MCLR strobe and TAG reset that SINTRAN omits.
Sections 3.15.2-3.15.4 give write-tag / write-data / read-data reference
subroutines usable as emulator test vectors.

---

## 5. Contradiction verdicts (C1-C11)

Each row: the competing claims (with the documents that made them), the verdict, and
the deciding evidence.

### C1 - Detection polarity (A=0 after trapped IOX read of RSTA5)

- Claim A (ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md): A=0 = no IOX error = card
  PRESENT.
- Claim B (old/ND500-BOOT-DETECTION-MECHANISM.md, inherited by
  ND500-INITIALIZATION-AND-EXECUTION-GUIDE.md section 2): A=0 = trap occurred = card
  ABSENT.
- **VERDICT: Claim A is correct.** Deciding evidence: NPL:PH-P2-OPPSTART.NPL:3913-3917
  quoted in 2.5.5 - `IF A=0 THEN % If not IOX-error then ... /OLD500 ... BONE 5ALIVE`.
  Claim B's DETECTND500 routine does not exist in the sources. CONFIRMED.

### C2 - Where is 5MPM / where do the messages live?

- Claim A (WHERE-IS-5MPM-LOCATED.md, OS/MPM5-KEY-FINDINGS.md): 5MPM is separate
  multiport memory hardware with BASE-register channel translation.
- Claim B (old/ND500-BOOT-DETECTION-MECHANISM.md, init guide, Emulator quick-ref):
  SINTRAN allocates 5MPM pages out of ND-100 RAM at a fixed address (0x040000).
- **VERDICT: Claim A is correct for the MPM hardware; Claim B confuses two things.**
  Deciding evidence: ND-10.004.01 (section 4.10) - separate twin-port module, BASE =
  2's-complement channel translation, per-port windows. HOWEVER the message block on
  DMA-3022 systems "resides in RESIDENT of SINTRAN III" (ND-05.012.01, section 4.9)
  and is reached through the 24-bit MAR "pointing to the ND-100 memory" (TMP section
  3.3, section 4.6); the mailbox area is addressed by SINTRAN via the physical bank
  variable 5MBBANK derived from 5FPMAILBOX (2.6.1). So: messages live in
  ND-100-addressable physical memory (which may physically be multiport RAM visible
  to both machines); they are NOT a region carved out of "the 5MPM module" by page
  allocation, and there is no fixed 0x040000 constant in the sources. CONFIRMED with
  refinement.

### C3 - Which CPU runs the swapper?

- Claim A (ND500-SWAPPER-LOADING-MECHANISM.md): 5SWAP is an ND-100 RT-program.
- Claim B (ND500-SWAPPER-ANALYSIS.md): the 5SWAP process runs on the ND-500.
- **VERDICT: Claim A.** Deciding evidence: NPL:RP-P2-N500.NPL:16-58 (2.7) - 5SWRT uses
  *2BANK and MON 131 (ABSLI), both ND-100 constructs; it RESERVES ND-500 process #0
  and serves it from the ND-100 side. CONFIRMED.

### C4 - CONTROL register bit 4

- Claim A (IF-USAGE, COMPREHENSIVE, OS/IOX-REGISTER-COMPLETE-REFERENCE): bit4 = PCLY
  "ND-500 programmed clear".
- Claim B (ND500-IF-LOCKING.md): bit4 = ClearInterrupt.
- Claim C (old/ND500-BOOT-DETECTION-MECHANISM.md): bit4 = INTEN interrupt enable.
- **VERDICT: Claim A.** Deciding evidence: TMP section 3.1 "4 = ND-500 programmed
  clear" (4.3); TST25 "Bit 4=1 clears bit 6" (DMA error) - it IS a clear, of the
  ND-500 + DMA-error latch, not of interrupts; interrupt enable is bit 0. CONFIRMED.

### C5 - What value enables the level-12 interrupt?

- Claim A (consensus docs): LCON5 bit0 (value 1) = interrupt enable; octal 10 = test
  mode.
- Claim B (old/ND500-BOOT-DETECTION-MECHANISM.md CONFIG3022): writing octal 10 to
  LCON5 "enables interrupts on level 12".
- **VERDICT: Claim A.** Deciding evidence: TMP section 3.1 (bit0 = enable interrupt,
  bit3 = test mode) plus the four-mode table (4.4) showing why SINTRAN writes 10
  first: test mode legalizes the LSTA5 status write; the NPL comment "Enable for
  interrupt" (MP:3089) labels the whole sequence (4.3). CONFIRMED.

### C6 - TAG codes: "high-level TAG-IN codes 8/9/16" vs hardware strobes

- Claim A (IF-USAGE + NEC-01/TMP): TAG-IN codes 0-15 are register-clock/enable
  strobes; TAG-OUT codes 0-7 are 3022 register operations driven by the ND-500;
  monitor calls are signalled via messages.
- Claim B (ND500-IF-LOCKING.md, Emulator/DETAILED-TAG-MECHANISM-EXPLANATION.md,
  Emulator/ND500-QUICK-REFERENCE.md, C# NDBusND500IF.cs): "high-level" TAG-IN codes
  8 = MonitorCallRequest, 9 = PageFaultRequest, 16 = OperationComplete trigger the
  level-12 interrupt; TAG-OUT 1 = DMARead, 2 = DMAWrite, 3 = ClearInterrupt.
- **VERDICT: Claim A. Claim B is fabricated.** Deciding evidence: TMP sections
  3.12/3.13 full code tables (4.5) - code 8 is DIEN, code 9 is DUEN, a TAG-IN code is
  4 bits so "16" cannot exist; TAG-OUT 1 = write MAR, 2 = read STATUS, 3 = write
  STATUS. Additionally SINTRAN never touches RTAG5/LTAG5 at all (2.2), and monitor
  calls flow through message MICFU/STOPR (2.5.7). CONFIRMED fabrication - the C#
  emulator's TAG protocol has no basis in hardware or SINTRAN.

### C7 - Offset notation and the SLOC5/CLKD5/UNLC5 labels

- Claim A (symbol tables + TMP): offsets are octal; SLOC5=14 set-lock (write),
  CLKD5=15 clock-DATA, UNLC5=16 unlock (write), RETG5=17 return gate/tag.
- Claim B (old/ND-500-INTERFACE.md and copies): +14 "ReadLockedMaybe", +15 "unclear",
  +16 "ReadLocked - plausible" (as reads). ND500-IF-LOCKING.md mixes octal and
  decimal offsets and spells MCLE5.
- **VERDICT: Claim A.** Deciding evidence: SYM:M06 lines in 2.1; TMP section 3.14
  (SLOC 074 "Set locked", CLKD 075 "Clock DATA", UNLC 076 "Release locked", RETG 077
  "Return tag"); RETG bit semantics from TST02/TST04 (4.5). CONFIRMED.

### C8 - 5CPUTYPE encoding (where the CPU-type field lives)

- Claim A (ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md): type in CPUAVAILABLE bits
  15-14 (mask 140000), OLD500=01 (0x4000), SAMSON=10 (0x8000).
- Claim B (ND5000-SAMSON-ARCHITECTURE.md): 5CPUTYPE mask = 000007 (low bits),
  OLD500=1, SAMSON=3, citing SYMBOL-1-LIST.SYMB.TXT.
- **VERDICT: Claim A is wrong about the field position; Claim B's model (low-bit
  mask) is right but its citation is false and its values are unverified.** Deciding
  evidence: `CPUAVAILABLE/\140000\/OLD500` PRESERVES bits 15-14 and ORs the type into
  the low bits (2.5.5); `CPUAVAILABLE/\5CPUTYPE><SAMSON` / `B/\5CPUTYPE = SAMSON`
  use 5CPUTYPE as a low-bit mask (MP:265, PH-RESTART:112). The numeric values of
  5CPUTYPE/OLD500/SAMSON/5ALIVE/5NOTPRESENT appear in NO symbol table or listing in
  this repository (grep 2026-07-08) - Claim B's "SYMBOL-1-LIST" citation does not
  check out. Values remain UNVERIFIED.

### C9 - Segment-capability bit layout

- Claim A (ND500-IF-USAGE section 9.3): bit13=S, bit12=P, bit11=W, bits0-10 physical
  segment (11 bits).
- Claim B (Emulator/ND500-QUICK-REFERENCE.md): W=bit15, P=bit14, S=bit13, physical
  segment bits 0-11 (12 bits).
- **VERDICT: UNRESOLVED by this pass.** The mined manuals' interface sections do not
  cover segment capabilities (they live in the ND-500 MMS documentation,
  ND-05.009.4 memory-management chapters). Out of scope for the bus-interface
  reference; carried as an open item. Both docs agree only on S=bit13.

### C10 - The interface device number ("HDEV")

- Claims: "1560B" (old/ND-500-INTERFACE.md, IF-LOCKING), "typically 100-120B"
  (retired boot doc, init guide), "typically 500B or 600B"
  (OS/IOX-REGISTER-COMPLETE-REFERENCE.md).
- **VERDICT: all three "typical" values are unsupported.** Deciding evidence:
  ND-06.015.02 section D.13.1 (4.1): the five valid thumbwheel bases are 60, 1060,
  660, 760, 560 (octal) with idents 16, 116, 36, 114, 76. In SINTRAN, HDEV is
  per-CPU datafield configuration (2.8). CONFIRMED (docs need correcting to the
  thumbwheel table).

### C11 - S-bit (cache bypass) wording

- Claim A (OS/MPM5-KEY-FINDINGS.md): S-bit is a CPU-level cache-bypass flag; MPM
  hardware knows nothing of it.
- Claim B (IF-USAGE section 9.3 wording): S-bit makes accesses "bypass the ND-500
  cache and go directly to 5MPM" (reads as if an MPM feature).
- **VERDICT: Claim A's framing is correct; B is compatible but misleading.**
  ND-10.004.01 (4.10) documents no per-access cache semantics on the MPM side -
  ports, windows, interleave only. Wording alignment task for Phase 4; no register
  facts in dispute.

### C12 (new) - STATUS bits 10-14 stop reason

Raised by this pass: an earlier NPL-only conclusion ("stop reasons are NOT in STATUS
bits 10-14") conflicted with older docs claiming a STOPREASON field there.
**VERDICT: both locations are real (2.3 as revised): hardware defines STATUS bits
10-14 = "ND-500 stop reason" (TMP section 3.2); the SINTRAN driver dispatches on the
message STOPR field (offset 11), not on those bits.** The older docs' claimed VALUES
(MOCALL=1, TRAPCODE=2, 5FMOCALL=3, TPSTRA=65) are not confirmed by NPL or the mined
manual text and remain UNVERIFIED as numbers.

---

## 6. Corrections to earlier exploration/analysis claims

Recorded so future readers do not resurrect these errors:

1. **"No symbol at offset 6 / MCLR5 unverified" - WRONG.** MCLR5=000006 exists in all
   three symbol-table versions (SYM:M06:7164, L07:7065, K03:4472) and in
   SYMBOL-1-LIST (M06:5255, L07:5105). What remains true: SINTRAN never ISSUES MCLR5
   (2.2).
2. **HDEV citation.** `PH-P2-OPPSTART.NPL:274 X.SWHDEV=:HDEV` is disk-driver setup,
   not ND-500 interface setup (2.8).
3. **CHN5STATUS on MSGN500/WAITING.** Earlier summary said "still waiting for the
   ND-500 CPU" (no action). The code calls XTER500 (terminate) in that branch
   (MP:753-755). The waiting SEMANTICS belong to XACT500/XACTRDY queue scanning
   (2.5.6).
4. **3RPREG/3RMICV locations.** They are written in RP-P2-N500.NPL (XMSINIT region,
   RP:803/811/822 and RP:282/384), not MP-P2-N500.NPL:803-822 (those lines are
   DECOMESS).
5. **CPUAVAILABLE type-field position.** `/\140000\/OLD500` PRESERVES bits 15-14 and
   ORs the type into low bits; the comprehensive guide's "CPU type in bits 15-14,
   OLD500=0x4000" reading is wrong (2.5.5, C8).
6. **ND500 folder README** previously said the ND-500 is "byte-oriented (NOT 32-bit
   word CPU!)" etc. - out of scope for this dossier; architecture claims are handled
   by the master reference where relevant to the bus interface only.

---

**Document owner:** ND-500 bus interface overhaul, Phase 2 (complete).
**Next:** the master reference
[ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md) is written from
this dossier; the emulator gap list lives in
[ND500-EMULATOR-DISCREPANCY-AUDIT.md](ND500-EMULATOR-DISCREPANCY-AUDIT.md).
