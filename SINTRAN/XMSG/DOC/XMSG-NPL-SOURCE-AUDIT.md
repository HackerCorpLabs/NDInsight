# XMSG / TAD / FA Implementation Audit Against the SINTRAN III Version J NPL Source

> **APPLIED 2026-08-18.** Every finding below was re-checked against the NPL source it cites before
> anything was changed, and the confirmed ones are now in the code and in `DOC/protocols/tad-wire.json`.
> Three notes for the next reader:
>
> - **One claim in this document is wrong.** Section 5.1 says `TadOpcodes.cs` "already lists all of
>   these names and values". It did not - `EDRS` (0x29) was absent, and so was `ESRS` (0x20). Both
>   are there now.
> - **Section C1's recommendation was applied in a weaker form, on purpose.** The version J driver
>   accepts only BDAT/TMOD/TTYP/DESC/DUMM at normal priority and rejects everything else. Copying
>   that list verbatim would make us reject `OPSV`, which every real client we have captured sends to
>   Release L machines that plainly accept it. We reject on "an opcode `TadOp` cannot name" instead -
>   see `SRC/Xmsg.Node/Tad/TadRejectPolicy.cs`, which records J's own list beside it.
> - **Section C5 is not actionable and was documented rather than implemented.** How our HDLC-carried
>   frames map onto an XMSG buffer's allocated `FBSIZ` is not established, so there is nothing to
>   check against.
>
> What could not be settled is now in `DOC/WHAT-WE-DO-NOT-KNOW.md` and in the `open_questions` block
> of `tad-wire.json`.

**Date:** 2026-08-18
**Auditor task:** compare everything under `SINTRAN/XMSG` (registry, C# code, docs, tests) against the recovered SINTRAN III **version J** source listing in `SINTRAN/NPL-SOURCE-2` (SI-GEN build of 8 October 1985, CPU N102.2921), and record every confirmation, contradiction and new fact.

**Evidence hierarchy used** (per the audit brief):
1. **Wire capture from a version L machine** — authoritative for L behavior.
2. **Version J NPL source** — authoritative for what J does; a J fact is applied to L only where L evidence agrees or the structure is provably unchanged.
3. **Existing registry/doc interpretation** — a hypothesis until checked.
4. **Inference** — labeled INFERRED.

Confidence labels: **PROVEN** (executable NPL logic or unambiguous wire evidence), **STRONG**, **INFERRED**, **UNKNOWN**.

Source files referenced constantly below, abbreviated:

| Abbrev | File |
|---|---|
| RES | `SINTRAN/NPL-SOURCE-2/NPL-CLEAN/06-COS-TAD-RES-CODE.NPL` (766 lines) |
| POF | `SINTRAN/NPL-SOURCE-2/NPL-CLEAN/20-COS-TAD-POF-CODE.NPL` (2216 lines) |
| SYSTABS | `SINTRAN/NPL-SOURCE-2/NPL-CLEAN/10-XMSG-SYSTABS.NPL` (56 lines) |
| SINA | `SINTRAN/NPL-SOURCE-2/NPL-CLEAN/12-EX-MRES-SINA.NPL` |
| DEBUG24 | `SINTRAN/NPL-SOURCE-2/NPL-CLEAN/24-SYMBOLIC-DEBUGGER.NPL` |
| SYM-J | `SINTRAN/NPL-SOURCE/SYMBOLS/J/SYMBOL-1-LIST.SYMB.TXT` |
| SYM-K03 | `SINTRAN/NPL-SOURCE/SYMBOLS/K03/SYMBOL-1-LIST.SYMB.TXT` |
| BOUT | `SINTRAN/NPL-SOURCE-2/BOUT-6.SYMB` (per-build symbol dump, from line 2079) |
| L03SYM | `SINTRAN/XMSG/DOC/COSMOS-RE/xmsg-kernel-l03-symbols-decoded.txt` |

Absolute addresses quoted as `BOUT: NAME = octal` are from this J build's own symbol dump and are given for cross-reference only — they are NOT the addresses of the L kernels we drive.

---

## 1. Executive summary

| Category | Count | Headline |
|---|---|---|
| Wire-derived facts **confirmed** by NPL | 15 | word alignment, pad-skip rule, 255-byte message cap, partner filtering, reset-confirm ladder, retransmit-same-bytes, TMOD-as-flags, TTYP big-endian, CESC 0/1, error-code base 41000B, more |
| **Definite errors** in registry/doc meaning | 5 | `Bmmx`, `Sycn`, `Reco`, `Cesc` meanings in `tad-wire.json`; the "20-byte" echo/break table (it is 16 bytes — an octal misread) |
| **Solved unknowns** | 3 | the "intrinsic 0x00 prefix" before ECKM/BMMX (it is CRHEOD's deliberate odd start), the identity of the 0xFD notify (`7POLL`, INFERRED), and the meaning of the 0x06/0x07 port-assign opcodes (`7CORQ`/`7CORS`, INFERRED) |
| Probable interop gaps (P1) | 3 | no REJE (0xFE) is ever sent or handled; ISRQ/ISRS (ISIZE over TAD) unimplemented; escape-disabled should answer EDRS not ESRS |
| Missing registry entries | 12 TAD ops | USCN, ISRQ, ISRS, NOWT, TNOW, NWRE, RLOC, EDRS, TREP, CPCO, ERRS, REJE — all with source-proven roles |
| New protocol facts | 14 | see section 15 |
| J → L version-delta candidates | 2 proven, 1 corroborated | XT-block layout change; UMOD/MOD8 absent from J; op values stable J=K03=L-wire |
| Circular tests identified | 1 family | `TadConnectAcceptTests` (real inputs, self-built expected outputs) |
| Unresolved | 6 | accept trailer (B3), session-port rule (T1), FBSI payload, SYCN payload encoding, LUN allocation rule, POLL usage |

The single most important structural finding: **this source contains the complete TAD kernel driver (the datafield side), but NOT TADADM** — the RT program that builds the connect accept and port assignment lives on its own segment (`SG36`, `SINTRAN/NPL-SOURCE-2/NPL-CLEAN/04-SIN4-GEN.NPL:460`) and is not in the variable-parts listing. The two open questions that block issue #34 (accept trailer B3, session-port rule T1) therefore **cannot be answered from this source** and remain open.

---

## 2. Critical findings (most serious first)

### C1. We never send and never handle the REJECT message (7REJE = 0xFE) — P1
**PROVEN (J source):** the TAD driver rejects any control message it does not understand, and any illegal high-priority message, by sending `[0xFE][0x01][rejected-type]` — REJECT carrying the offending type byte:

- POF:1596-1597 (`REJECT`): `A:=7REJE; CALL STORBYT; A:=1; CALL STORBYT` then `DFOPP.CURMES/\377; CALL STORBYT`. BOUT: `REJEC = 143105`.
- POF:1412 (`ESCDIS`): an unknown high-priority head is converted to a reject: `A:=X SHZ -10=:CURMES; GO FAR SRJE`.
- POF:1317 (`BDRINP` dispatch): unknown control message → `CALL REJECT`.
- SNDREJ (POF:834-875, BOUT: `SNDRE = 140162`) additionally appends an RFI when the rejected message was data.
- On the receiving side, a rejected input surfaces as error `TER01` (see section 8).

**Our implementation:** nothing constructs or parses 0xFE anywhere (`SRC/Xmsg.Node/Tad/`, `SRC/Xmsg.Servers/Tad/TadServer.cs:427-429, 477-478` return empty frames on anything unrecognised). `TadOpcodes.cs:75` names REJE but no code path touches it.

**Consequences:** (a) a real SINTRAN client that sends us an op we ignore expects either handling or a REJE — silence leaves its call hanging until timeout; (b) when a real host REJEs *us* we will not recognise it and will misread the session state. Whether L still sends REJE has not been captured — but the constant exists in every symbol table J/K03/L07/M06, so the mechanism is almost certainly still there. **STRONG** for L.

### C2. Registry meanings wrong for four TAD ops — the machine-checked file is the wrong one — P1/P2
`DOC/protocols/tad-wire.json` is the authority the conformance tests enforce, and four of its `meaning` fields are refuted by the executable source. In two cases the C# XML doc is already correct, so **code and registry currently disagree and the test cannot see it** (it checks names/values, not meaning text). Details in section 4.

### C3. ISIZE over TAD (7ISRQ 0x22 / 7ISRS 0x23) is a real request/response we do not implement — P1
**PROVEN (J source):** when a program on the TAD-owning side calls ISIZE (MON 66) or IBRSIZ (MON 313) and the local input buffer is empty, SINTRAN sends `7ISRQ` (I-field 0 bytes) to the partner and suspends the caller until a `7ISRS` (2 data bytes, big-endian count) arrives: RES:644-708 (`BISIZ`/`OISIZ`, the `TAGAI:` send at RES:678), reply data read as `byte6<<8 + byte7` at POF:1251-1256. The response's op word is remembered with **bit 15 set** to distinguish the two callers (RES:693 `7ISRS BONE 17`).

**Our implementation:** neither op exists in `TadOp`; an incoming ISRQ is silently dropped (C1 applies), so any remote program calling ISIZE against our TAD hangs. Not yet observed on the wire — it needs a program, not a login, to trigger. **PROVEN** for J, **STRONG** for L.

### C4. Escape while escape is disabled must answer EDRS (0x29), not ESRS (0x20) — P2
**PROVEN (J source):** POF:1385-1407 (`ESCDIS`): escape enabled → process the escape, respond `ESRS`; escape **disabled** (DFLAG bit 5IESC) → respond `7EDRS` ("ESCAPE RESPONSE ESCAPE DISABLED", header constant at POF:1161). Our responder sends ESRS unconditionally (`TadServer.cs:774`, `TadTerminalResponder.cs:601`). Low practical impact while we never disable escape, but it is a distinct wire message the parser should at least name.

### C5. Wrong-size incoming buffers are silently discarded by a real TAD — interop trap for our sender — P2
**PROVEN (J source):** `CHSIZE` (POF:1557-1570, BOUT: `CHSIZ = 143052`) checks every received buffer against the datafield's `FBSIZ`; a mismatch **releases the buffer with no reply**. Likewise `CHPART` (POF:1535-1546) silently releases buffers from any port other than the recorded partner. If our frames ever arrive as XMSG buffers whose allocated size differs from what the responder's TAD expects, the symptom would be pure silence — worth remembering when a TAD exchange dies with no error. (How buffer size maps onto our HDLC-carried frames is not established; see the FBSI hypothesis in section 17.)

---

## 3. Wire-derived facts confirmed by NPL

Each row: our fact → the J source that proves the mechanism.

| # | Our fact (where) | NPL evidence | Confidence |
|---|---|---|---|
| 1 | TAD message = `[opcode][count][data]`, count is one byte, 255-byte data cap (`TadMessageBuilder.cs:337-368`) | header written type-high/count-low (POF:77 `A SHZ 10; CALL WRMHEAD`, WRMHEAD POF:32-51, BOUT `WRMHE=135500`); count updated by `UPDMBC` which fails at `>= 400B` = 256 (POF:191-206) | PROVEN |
| 2 | messages start word-aligned; lone 0x00 is a pad the parser skips (`TadChain.cs:126-131`) | CREMES pads odd→even before the header (POF:75, and WRMHEAD POF:42-46 "ODD START, CREATE PAD BYTE"); GETMES skips a zero type byte as pad (POF:168-176) | PROVEN |
| 3 | "count 0xFF sentinel = more follows" is a fiction (`TadServer.cs:173` comment) | there is no sentinel: BYTPUT returns error 3 "MESSAGE FULL" at 255 and the caller simply opens the next BDAT in the same buffer (POF:236-252, BM8OUT POF:453-458) | PROVEN |
| 4 | a session accepts frames only from its partner port (`fa-transfer-must-filter-by-session-port`, `FaReadDriver.cs:359-399`) | `CHPART` receives-and-releases anything not from `DFOPP.RPORT`, POF:1535-1546 | PROVEN (TAD); the FA server binary is not in this source, but the sibling mechanism is |
| 5 | Rese is confirmed by Reco; the responder waits for it (`TadServer.cs` bring-up ladder) | after sending 7RESE the driver stores `7RECO=:DFOPP.RSPNUM` and sleeps until the RESET-CONF arrives (RES:435-444; POF:1345-1348) | PROVEN |
| 6 | a retransmission is answered with the same bytes, not a new event (five prior defects; `FaServer.cs:873-891`) | the driver model never advances state on a repeat: response-wait is keyed on RSPNUM, and `BDRINP` re-serves the same buffer until consumed (POF:1231-1233, 1295) | STRONG (mechanism-level; the Flags1 rule itself is an XMSG-kernel matter, not in this listing) |
| 7 | TMOD payload is a bit-flags byte, not an enum (`TadMessageBuilder.Tmod(byte flags)`) | `BDTMOD` POF:1054-1080 and `CTMOD` RES:569-589 test/set individual bits — see section 5 for the four bit meanings | PROVEN |
| 8 | TTYP payload is one big-endian 16-bit word (`TadServer.cs:2349`) | `BDTTYP` POF:1088-1096 assembles high byte first on odd starts; `BSTTY` sends `DFOPP.CTTYP` via WORDPUT (RES:527-540) | PROVEN |
| 9 | CESC payload: 0 = escape disabled, 1 = enabled (`CescState.cs`) | `BCESC` RES:600-601: `IF DFOPP.DFLAG BIT 5IESC THEN A:=0 ELSE A:=1` | PROVEN |
| 10 | DESC payload = the escape character itself (`TadMessageBuilder.Desc`) | `BSDAE` RES:620-621 sends `CESCP/\377`; `BDDESC` POF:1104-1110 stores it into CESCP | PROVEN |
| 11 | DUMM messages of any length must be skipped by their count (`TadChain` walk) | `CLIDAT`/`CLODAT` (POF:700-758) **overwrite data messages in place with 7DUMM**, keeping their counts — a nonzero-length DUMM is a blanked-out data message, and `BDDUMM` skips it by REMBYT (POF:1118-1122) | PROVEN |
| 12 | ESCA answered by ESRS (`tad-wire.json` Esrs entry) | ESCDIS POF:1385-1400, response head `ERESP:=(7ESRS\0,...)` POF:1160 | PROVEN |
| 13 | ESCA/DCON/notify ride as separate high-priority control frames (`BareTadControl`) | the driver serves message type `XMTHI` (=3, matches `xmsg-constants.json` message_types) immediately, comparing whole 6-byte heads: EBUFF/CESCR/RESCF/ISZRS/ERRSP/NWREM/TREPS/BDESC/RLOCA/BDDIS (POF:1241-1292) | PROVEN |
| 14 | XMSG errors convert to SINTRAN error numbers on base 41000B (`XKXXX = 16896` in `xmsg-constants.json`) | `CNVERR` POF:625-627: `A-\/XKXXX` — negate the (negative) XMSG code and OR with 41000B. XKXXX = 041000 in SYM-J and SYM-K03 | PROVEN |
| 15 | XMSG hands transmit work to the HDLC driver by DCB chaining (`XMSG-PROTOCOL.md` §12) | `ZXS12`/`ZXS13` chain the DCB into the HDLC datafield and kick level 12/13 under interrupt-off: SINA:7-17. BOUT: `ZXS12 = 072711`; the L03 kernel has the same pair at 132760/132761 (L03SYM:1830-1831) | PROVEN (J), STRONG (L, symbols present) |

---

## 4. Contradictions

### 4.1 `Bmmx` (0x04) — registry meaning refuted

```text
Current registry:      tad-wire.json operations "Bmmx": "the largest block the far end
                       will accept in one go" (INFERRED)
Current code doc:      TadOp.cs:39 "BMMX - break strategy / max break (0x04)"  <- correct
NPL implementation:    BDBREA "ROUTINE TO SEND BREAK-MESSAGE" builds message type 7BMMX:
                       [break-strategy byte][BRKMAX word]["strategy 7" -> 16-byte break table]
                       POF:933-960; header created with CRHEOD (odd start).
                       BOUT: BDBRE = 140523. 7BMMX = 000004 (SYM-J, SYM-K03).
Wire evidence:         our captures only ever carry 00 04 03 s hi lo (strategy + max word),
                       consistent with the source layout, never with "largest block".
Assessment:            the registry text is wrong; the code doc is right. BMMX = "break
                       max/strategy" - it configures WHICH characters break input and how
                       many characters may accumulate before a break (BRKMAX).
Recommended action:    fix tad-wire.json meaning; cite POF:933-960. Update evidence from
                       "name from ND sources" to the source lines.
Confidence:            PROVEN.
```

### 4.2 `Sycn` (0x13) — registry meaning refuted; code doc half-right

```text
Current registry:      "get back in step - used when the two ends may have lost track of
                       each other" (INFERRED)
Current code doc:      TadOp.cs:81 "session sync / login-state word", values 0x0002/3/6/A
                       VERIFIED from conn-to-d102 frames 62/64/68/70.
NPL implementation:    7SYCN is the SYSTEM CONTROL message. CTOBAD (RES:460-505) sends it
                       when the output-ioset control code is 23: one word of payload
                       (WORDPUT), flushed immediately only for parameter values 1, 13B, 17B.
                       Its twin is 7USCN (0x14) "USER CONTROLL" (control code 24), which
                       waits for a 7ERRS response. 7SYCN = 000023, 7USCN = 000024 (SYM-J,
                       SYM-K03).
Wire evidence:         the observed values 0x0002/0x0003/0x0006/0x000A stepping through a
                       login are real and stay MEASURED for L.
Assessment:            the op is a general system-control channel; the login-state stepping
                       we observed is one USE of it (whatever writes those codes runs above
                       the driver - not in this listing). "Get back in step" is wrong.
                       "Login-state word" is an over-narrow fit; keep the observed values,
                       relabel the op.
Recommended action:    registry meaning -> "system control word (7SYCN); one-word payload;
                       user-control twin is USCN 0x14". Keep SycnState values as observed-
                       on-L data, not as the op's definition.
Confidence:            PROVEN (role), UNKNOWN (encoding of the payload values).
```

### 4.3 `Reco` (0x17) — registry meaning refuted

```text
Current registry:      "pick up a session again after it was interrupted" (INFERRED)
Current code doc:      TadOp.cs:91 "RECO - reset confirm (0x17)"  <- correct
NPL implementation:    the driver names its prebuilt head "RESCF ... % RESET-CONF MESSAGE"
                       (POF:1152) and treats an incoming RECO purely as the awaited
                       confirmation of a sent RESE (POF:1345-1348; RES:444).
Assessment:            RECO = reset-confirm. Registry text wrong, code right.
Recommended action:    fix tad-wire.json; cite POF:1152 and POF:1345-1348.
Confidence:            PROVEN.
```

### 4.4 `Cesc` (0x0E) — registry meaning refuted

```text
Current registry:      "choose which key means escape for this session" (INFERRED)
Current code doc:      TadOp.cs:65 "1-byte state that steps 0x00 (auth-prompt) to 0x01
                       (auth-complete)" - a wire-fitted story.
NPL implementation:    CESC enables/disables the escape function: payload 1 = enabled,
                       0 = disabled (BCESC RES:596-610; the responder that has escape
                       inhibited answers ESCA with EDRS, POF:1401-1407). Choosing WHICH
                       character is escape is DESC (0x0F), not CESC.
Wire evidence:         CescState = {EscapeDisabled=0, EscapeEnabled=1} in CescState.cs is
                       exactly right; the observed 0->1 step during login is the host
                       disabling escape while credentials are typed, then enabling it.
Assessment:            registry text describes DESC; CESC is enable/disable. The CescState
                       enum survives unchanged; the TadOp.cs remark should be rewritten.
Recommended action:    fix both meaning texts; cite RES:596-610.
Confidence:            PROVEN.
```

### 4.5 Echo/break custom table is 16 bytes, not 20

```text
Current doc:           EchoStrategy.cs:9-11 "strategies 1-7 with an optional 20-byte custom
                       table" (from TAD-Message-Formats.md).
NPL implementation:    CBRECTA copies exactly 8 words = 16 bytes (FOR X:=0 TO 7, then
                       TDBTPT+20=:TDBTPT - that 20 is OCTAL = 16 decimal), POF:885-896.
                       Message sizes confirm: ECKM strategy 7 reserves 21B = 17 bytes
                       (1 + 16), POF/BDECHO:908; BMMX strategy 7 reserves 23B = 19 bytes
                       (1 + 2 + 16), POF/BDBREA:938.
Assessment:            "20-byte" is an octal value read as decimal. The table is a 128-bit
                       per-character bitmap (16 bytes covers ASCII 0-127).
Recommended action:    fix EchoStrategy.cs remark and TAD-Message-Formats.md.
Confidence:            PROVEN (size); INFERRED (bitmap interpretation).
```

### 4.6 The "intrinsic 0x00 prefix" before ECKM/BMMX — mechanism identified

```text
Current code:          TadMessageBuilder.cs:351-358: ECKM, BMMX (and port-assign 0x07/0x0B)
                       are "ALWAYS preceded by an extra 0x00 on the wire (a 16-bit opcode
                       or a flag byte - UNKNOWN which)".
NPL implementation:    it is NEITHER. ECKM and BMMX headers are built with CRHEOD -
                       "MESSAGE HEADER IS CREATED ON ODD BYTE" (POF:116-146; BDECHO uses it
                       at POF:917, BDBREA at POF:947). From an even boundary CRHEOD first
                       writes one 0x00 pad (POF:140), putting the 2-byte header at an odd
                       offset so that the word-sized payload that follows (the BRKMAX word,
                       the 8-word table, written with whole-word STATX stores) lands
                       word-aligned. CREMES does the opposite (even start) for byte-stream
                       messages.
Assessment:            our byte output is already correct; the model ("intrinsic prefix")
                       can now be stated as what it is: odd-start alignment for messages
                       with word-aligned payloads. This also explains why the port-assign
                       opcodes 0x07/0x0B carry the same prefix - their payloads are words
                       (system number, port number).
Recommended action:    rewrite the comment; no byte change.
Confidence:            PROVEN for ECKM/BMMX; STRONG for 0x07/0x0B (same shape, TADADM code
                       not in this source).
```

### 4.7 My own earlier claim, corrected: the "20 RFA sessions" ceiling

```text
Earlier claim (this audit's precursor conversation): "a COSMOS file server has a hard
ceiling of 20 concurrent file-access sessions".
NPL implementation:    the pool is on the CLIENT (asking) machine, not the server. PT3FUSER
                       (DEBUG24:1354-1374) assigns one RFA data segment per BACKGROUND
                       program touching a remote file; RT programs bypass the pool through
                       the single RTRFA program (segment "5RRUS", serialized by semaphore
                       1730, DEBUG24:1378-1391). The count is the generation variable
                       NRFSG (DEBUG24:1334; BOUT: NRFSG = 055571) - it is 20 in THIS build
                       because SI-GEN answered "20 file-access segments" (marks 8RFAC 8SG20,
                       BOUT-6 line 149 area), not a protocol constant. Exhaustion returns
                       ER188 to the caller after trying to steal a segment from a passive
                       program (DEBUG24:1362-1372).
Assessment:            no conflict with FaServer.MaxSessions = 10 (our own choice) or the
                       30 seats *FA-SERVER declares (XmsgKnownServers.cs:155) - three
                       different numbers limiting three different things.
Confidence:            PROVEN (mechanism and this build's count).
```

### 4.8 Registry `Fbsi`/`Lun` — no contradiction, but TADADM absent

The port-assign trailer fields (0x07 block, LUN 0x0B, FBSI 0x15, terminator 0xFF 00) are built by TADADM, which is **not in this source** (segment SG36). Nothing here contradicts our copied bytes. One naming gain: the raw opcodes our client sends in session setup, `0x06`, and the server's `0x07` block, match `7CORQ = 000006` / `7CORS = 000007` ("connect request / connect response") in SYM-J — with `7CONF = 000005` beside them. **INFERRED only** (value match; small values collide across symbol families — e.g. `7IRQI` is also 4).

---

## 5. Incorrect / incomplete enums, flags and constants

### 5.1 `TadOp` / `tad-wire.json` — 12 missing members, all source-proven

Values identical in SYM-J and SYM-K03 (and consistent with every value L-wire has confirmed for the ops we do carry), so **stable across J..K at least; STRONG for L**:

| ND name | Octal | Hex | Role (from executable J source) | Source cite |
|---|---|---|---|---|
| 7USCN | 000024 | 0x14 | user-control word; sender then awaits 7ERRS | RES:482-492 |
| 7ISRQ | 000042 | 0x22 | ISIZE request (empty I-field) | RES:678 |
| 7ISRS | 000043 | 0x23 | ISIZE response, 2 data bytes big-endian | POF:1154, 1249-1256 |
| 7NOWT | 000044 | 0x24 | nowait status (1 status byte) | NWSTA POF:968-988 |
| 7TNOW | 000045 | 0x25 | nowait status, variant selected when entry A≠0 | NWSTA POF:973 |
| 7NWRE | 000046 | 0x26 | nowait restart (high-priority; receiver bounces it back and restarts the user) | POF:1153, 1273-1277 |
| 7RLOC | 000047 | 0x27 | "REMOTE LOCAL (RUBOUT NORD-NET)" — local-character/rubout signal, handled like ESCA | POF:1149, 1385-1394 |
| 7EDRS | 000051 | 0x29 | escape response when escape is disabled | POF:1161, 1401-1407 |
| 7TREP | 000052 | 0x2A | terminal report status; 2 data bytes; bit 2 = buffer overrun, bit 3 = parity error, bit 4 = framing error | POF:1156, 1278-1290 |
| 7CPCO | 000372 | 0xFA | completion code, 4 data bytes (two words) | SNDCP POF:998-1011 |
| 7ERRS | 000373 | 0xFB | error response, 2 data bytes; answer to USCN | POF:1155, 1357-1364 |
| 7REJE | 000376 | 0xFE | reject; 1 data byte = the rejected message type | POF:1596-1597 |

`TadOpcodes.cs:47-75` already lists all of these names and values (they were never wrong) — the enum and the registry are what lag.

### 5.2 Ops previously raw-cast, now nameable (all INFERRED — value match only)

| Byte | Symbol | Value | Note |
|---|---|---|---|
| 0xFD | `7POLL` | 000375 | the "SessionNotify" byte our teardown sends; name from SYM-J. Not used in the COS-TAD driver source itself — TADADM-side |
| 0x06 | `7CORQ` | 000006 | client session-setup chain (`TadConnectClient.cs:154-159`) |
| 0x07 | `7CORS` | 000007 | the port-assign "0x07 05 …" block |
| 0x05 | `7CONF` | 000005 | unobserved; sits between them |
| 0x1B | `7KEYI` | 000033 | client setup chain; meaning unknown |
| 0x1C | `7BADT` | 000034 | client setup chain; meaning unknown |
| 0x10 / 0x11 | `7USID` / `7PASS` | 000020 / 000021 | user-id / password messages, presumably TADADM login forwarding; never on our wire |
| 0x19 / 0x1A | `7STRQ` / `7STRS` | 000031 / 000032 | status request/response pair; never observed |

Do **not** enter these as MEASURED; the 7-prefix symbol space carries several unrelated families (7ZMEM/7ZSCR, 7DBRE/7DECO, …) with overlapping small values.

### 5.3 TMOD flag bits — new, source-proven decode

`BDTMOD` (POF:1059-1077) and `CTMOD` (RES:574-581), identical in both directions:

```text
Bit   Mask   NPL meaning                       Current meaning     Result
---   ----   -------------------------------   -----------------   ------
0     0x01   capital letters only (5CAPITAL)   undecoded raw byte  NEW
1     0x02   CR delay (5CRDLY)                 undecoded           NEW
2     0x04   stop on full page (SCREEN)        undecoded           NEW
3     0x08   logout on missing carrier (5LBLOG) undecoded          NEW
4-7   —      not read by BDTMOD                —                   absent in J
```

The single observed client value `0x08` (`TadNegotiatedParameters.cs:38`) therefore means **"log me out if carrier drops"** — a sensible thing for a remote terminal to request. PROVEN for J; STRONG for L (the same four features exist in L configuration).

### 5.4 Error-model constants (new)

| Name | Octal | Decimal | Meaning | Source |
|---|---|---|---|---|
| TER00 | 000314 | 204 | input done while delayed escape action pending | IEDCHK RES:350 |
| TER01 | 000315 | 205 | message rejected (sent with/after SNDREJ) | RES:667, POF:420 |
| TER02 | 000316 | 206 | TAD not connected (PORTNO=0) | POF:404, 497, 561 |
| XKXXX | 041000 | 16896 | base OR-ed onto negated XMSG error codes by CNVERR | POF:625-627 |

TAD-internal (never on the wire) builder/reader statuses, PROVEN: CREMES/BYTPUT/WORDPUT return 1 = no output buffer, 2 = buffer full, 3 = message full (255 cap); GETMES returns 1 = no input buffer, 2 = buffer empty, 3 = message bigger than buffer (triggers SNDREJ); BYTGET returns 0 = message empty. Our A=1/A=2 registry note ("no output buffer present / output-buffer full") in `tad-wire.json` matches — those are exactly CREMES's codes (they appear in the accept-era docs); no change needed there.

### 5.5 FA enums/constants — nothing refuted

The FA server/protocol (`*FA-SERVER`, QFORM) is a COSMOS product **not present** in this kernel listing; no FA constant could be checked against J source. `fa-qform.json` stands as-is. The only FA-adjacent kernel facts found are the RFA client-side pool (4.7) and the DF datafield for RFA in `01-SIN1-GEN.NPL:609`.

---

## 6. Packet and structure layout audit

### 6.1 TAD message framing (body level)

```text
Offset  Size  NPL source (GETMES/WRMHEAD/CRHEOD)     Current definition          Finding
------  ----  -------------------------------------  --------------------------  -------
+0      1     message type byte (0 = pad, skipped)   opcode; 0x00 pad skipped    MATCH
+1      1     byte count (max 255; 400B refused)     count byte; >255 throws     MATCH
+2      n     I-field                                data                        MATCH
align   —     CREMES: even start; CRHEOD: odd start  even pad + "intrinsic 0x00  MATCH bytes,
              for word-payload messages (ECKM/BMMX)  prefix" for ECKM/BMMX       model corrected (4.6)
```

The J buffer also carries a 4-byte preamble the wire does not show: `SETSIZE` (POF:1520-1528) writes word 0 = 0 and word 1 = used-byte-count into the buffer head, and `BUDIS = 000004` (SYM-J) is the fixed data displacement. This is XMSG-buffer-internal, PROVEN for J; whether those 4 bytes correspond to anything in our captured frames is **UNKNOWN** — do not map it onto the wire without a capture-level check.

### 6.2 High-priority (bare control) messages

PROVEN for J: single-op messages travel as message type XMTHI (=3) and the receiver compares the whole 6-byte XFRHD head against prebuilt constants (POF:1145-1161, 1245-1249). The prebuilt heads are `(op\count, 0, 2)` triples; data-bearing responses (ISRS/ERRS/TREP) carry their 16-bit value at head bytes 6-7, read big-endian (POF:1251-1256). Our `BareTadControl` frames (2-byte bodies like `18 00`) match the visible part; the relationship between the 6-byte XFWHD head and our 14-byte XMSG sub-header is an XMSG-kernel matter this listing does not decide.

### 6.3 XT-block (XMSG task descriptor)

J source layout (SYSTABS:25-51) vs the L03 kernel symbols (L03SYM). Offsets octal:

```text
Off   J field  J comment                                   L03 field   Delta
---   -------  ------------------------------------------  ----------  -----
0     XTCHN    chain for free/waiting for resources        XTCHN       same
1     XTSTA    status word                                 XTSTA       same
2     XTRTA    RT-address or 0 if driver                   XTRTA       same
3     XTPRT    port chain header (first in queue)          XTPRT       same
4     XTMEM    number of bytes of memory in use            XTMEM       same
5     XTMMX    max amount of memory allowed                XTRSE       CHANGED
6     XTCMS    task current message                        XTCMS       same
7     XTSTS    bit 17: paging status; 0-4: interrupt lvl   XTSTS       same
10-17 XTPRG..XTBRG register save area                      same        same
20    XTAPR    saved ACTPRI while on level 1 (YSTL1)       XTAPR       same
21    XTTAP    ACTPRI for transfer windows                 XTTAP/XTTDF same
22    XTUBF    user buffer address                         XTUBF       same
23    XTSBK    system buffer bank; BIT 15 => user->sys     XTSBK/XTSAD same
24    XTSBF    system buffer address                       XTSBF       same
25    XTCNT    transfer count in bytes                     XTCNT       same
26    XTTCN    dynamic counter used by XTRAN               XTTCN       same
27    XTHOM    return address from XTRAN (also XDHAN)      XTHOM       same
30    XTASG    actual segments 1 and 2                     XTASG/XTDAR same
31    XTRSG    re-entrant segment                          XTUSG       RENAMED
32-35 XTBM0..3 bit map                                     XTACH/XTAMS/XTUAC/XTUMS  CHANGED
—     —        —                                           XTMMH/XTMMX/XTMML @36-37 MOVED (memory allowance triple)
—     —        —                                           XTFUN/XTCON/XTDLR/XTDSR/XTDBR/XTFRE/XTUSE/XTMAX/XTLIM  NEW in L
```

**J → L version delta, PROVEN:** the block grew and the memory-allowance field moved from offset 5 to a triple at 36-37. **Consequence:** never apply J XT offsets 5, 31, 32-35 to an L kernel; offsets 0-4, 6-30 (octal) are stable and the J comments can safely annotate them. The direction bit (XTSBK bit 15 = user-to-system transfer, SYSTABS:40) is at an unchanged offset — **STRONG** for L.

### 6.4 SINTRAN envelope / FA layouts

Nothing in this kernel listing describes the 14-byte SINTRAN header, the XMSG sub-header, or any FA/QFORM byte — those live in the XMSG kernel and COSMOS products, not in the OS variable part. No check possible; no contradiction found.

---

## 7. Naming problems

| Current name | Proposed | Evidence | Certainty |
|---|---|---|---|
| `tad-wire.json` Bmmx meaning "largest block…" | "break parameters: strategy byte, max-break word, optional 16-byte break table" | POF:933-960 | CERTAIN |
| `tad-wire.json` Sycn meaning "get back in step" | "system-control word (7SYCN); observed login values remain L-wire data" | RES:478-492 | CERTAIN (role) |
| `tad-wire.json` Reco meaning "pick up a session again" | "reset confirm (RESCF)" | POF:1152 | CERTAIN |
| `tad-wire.json` Cesc meaning "choose which key means escape" | "enable (1) / disable (0) the escape function" | RES:596-610 | CERTAIN |
| `TadOp.cs` Cesc remark "auth-prompt/auth-complete state" | as above; the login 0→1 step is escape being re-enabled | RES:596-610 | CERTAIN |
| `XmcsmService.SessionNotify` / raw 0xFD | `Poll` (7POLL) | SYM-J `7POLL = 000375` | INFERRED |
| `EchoStrategy.cs` "20-byte custom table" | "16-byte (8-word) table" | POF:885-896 | CERTAIN |
| `TadMessageBuilder` "intrinsic 0x00 prefix… UNKNOWN which" | "odd-start alignment pad (CRHEOD) so word payloads align" | POF:116-146 | CERTAIN |
| raw `(TadOp)0x06` / `0x07` in setup/port-assign | `Corq` / `Cors` | SYM-J values | INFERRED — keep raw until captured semantics confirm |

No renames proposed for FA — nothing was checkable.

---

## 8. Error model (consolidated)

New entries from this audit in **bold**; the XE*/XR* tables in `xmsg-constants.json` and the registry are unchanged and were not contradicted.

| Code | Value | Layer | Meaning | Origin |
|---|---|---|---|---|
| **TER00** | 204 (314B) | TAD → caller | input during delayed escape action | RES:350 |
| **TER01** | 205 (315B) | TAD → caller | message rejected (inconsistent/unknown) | RES:667, POF:420 |
| **TER02** | 206 (316B) | TAD → caller | TAD not connected | POF:404 |
| **CNVERR rule** | — | TAD | SINTRAN code = 41000B OR (−XMSG code) — so e.g. XENSE (−34 = −42B) surfaces as 41042B | POF:625-627 |
| **CREMES 1/2/3** | — | TAD internal | no buffer / buffer full / message full | POF:69-81, 236-252 |
| **GETMES 1/2/3** | — | TAD internal | no buffer / empty / inconsistent (→ SNDREJ) | POF:159-181 |
| **7REJE payload** | — | TAD wire | the rejected message's type byte | POF:1596-1597 |
| **7TREP bits 2/3/4** | — | TAD wire | buffer overrun / parity / framing, mapped to TINFO bits 5BFUL/5PAER/5FRER | POF:1286-1289 |
| **ER188** | (SINTRAN error) | RFA client | all RFA data segments reserved | DEBUG24:1371 |
| XE\* table | −1..−63 | XMSG | unchanged, confirmed in use (XENOT tested at INIBDR POF:1186) | `xmsg-constants.json:320-500` |
| FA statuses 0/46/48/97/129/197/211 | — | FA | unchanged; not checkable against this source | `fa-qform.json:360-425` |

---

## 9. State-machine findings (established from source only)

TAD driver (`BDRINP` loop, POF:1216-1513), per direction pair of datafields:

1. **Connect phase** (driven by TADADM, outside this source): TADADM calls `INIBDR` — open XMSG port (XFOPN), reserve NOBUFF buffers of FBSIZ (XFGET + XDINF), build the free pool. Then `INISND` — send a 6-byte DUMM head to `PARTNER`, register `BDRINP` as the wake-up function (XFWDF), clear line-dead bit. POF:1183-1206. This matches the wire fact that a bare DUMM appears early in setup.
2. **Receive loop**: read port status (`XFPST` with the XFWAK bit, = wait-flag bit 14 → mask 0x4000, consistent with the oracle-confirmed memory); high-priority (XMTHI) messages served immediately by whole-head compare; normal messages taken only when the current input buffer is consumed (POF:1231-1243, 1295).
3. **Response waits**: a datafield stores at most one awaited response op in RSPNUM (7CERS, 7RECO, 7ISRS±bit15, 7ERRS); the matching high-priority arrival clears it and restarts the suspended program; a data message does NOT restart a program that is awaiting a response (POF:1330-1372). **Bit 15 of RSPNUM is a local variant flag, not a wire bit.**
4. **Escape**: ESCA/RLOC → if enabled: process, hold the received buffer, answer ESRS from `BERESP` after escape handling completes (edit-in-place of the received buffer — the same pattern as the Ethernet firmware's reply builder); if disabled: answer EDRS immediately. POF:1385-1407, 1421-1427.
5. **Disconnect**: DCON → `STOTAD` → logout or restart per 5TLREP, then `BDDSCN`: release current buffer (XFREL), clear datafields, XFDCT. POF:1409-1410, 1438-1484. Confirms the wire disconnect ladder's finality.
6. **Send path**: user-level output accumulates messages in the current buffer; `SNDBUF` sends only a non-empty buffer (REMSIZ ≠ FBSIZ−4), via driver function STDEV → `BDROUT` → XFSCM + XFSND, then immediately re-arms with a fresh pool buffer. POF:765-773, 1492-1502.

Not invented: nothing here describes the accept, the parameter trailer, the session-port choice, or Flags1 — all TADADM/XMSG-kernel territory.

---

## 10. Buffer and memory semantics

- **Pool model, PROVEN (J):** each TAD direction pair owns NOBUFF pre-reserved XMSG buffers of exactly FBSIZ bytes, linked through the buffer bodies themselves (POOLLI chain, PUTPOOL/GETPOOL POF:641-676). Input buffers can be re-purposed as output (`MOVITO` POF:684-690) when the pool is dry — which is why a reject or RFI can still go out under memory pressure.
- **Data displacement:** BUDIS = 4 bytes reserved at the front of every buffer; `SETSIZE` writes `[0][used-byte-count]` there before send (POF:1520-1528).
- **Exact-size rule:** received buffers must be exactly FBSIZ or they are discarded (C5).
- **XT-block quantities (J comments, SYSTABS:26-44):** XTMEM = bytes of memory in use, XTMMX = max allowed (J position; L moves it), XTUBF = user buffer address, XTSBK/XTSBF = system buffer bank/address with **bit 15 of XTSBK = transfer direction user→system**, XTCNT = transfer count in bytes, XTTCN = dynamic counter used by XTRAN. These are the authoritative comments for the fields our L03 symbol decode names without explanation.
- **Our `XmsgKernel`** models quota (XFDMM ceiling → XETMM) and buffer ownership at the API level; nothing in the J source contradicts it. The J kernel-side XT fields are *internals* our API model deliberately does not mirror — consistent with the existing decision in `XMSG-ENUM-STRUCT-AUDIT-2026-08-10.md` ("do NOT build them speculatively"). That decision stands; what changed is that the SYSTABS comments are no longer OCR-damaged guesswork — the J listing is clean.

---

## 11. TAD findings — reconstruction summary

Message construction, from source (all PROVEN for J):

| Message | Builder | Layout after `[op][count]` | Notes |
|---|---|---|---|
| Bdat 0x01 | BDPUT/BM8OUT | stream of bytes, ≤255 per message | new message opened transparently at 255 |
| Rfi 0x02 | SNDRFI | empty | sent when input buffer empty; at most one outstanding (5RQI bit); driver retries via 5WRQI when out of buffers |
| Eckm 0x03 | BDECHO (CRHEOD, odd start) | [strategy]; strategy 7 → +16-byte table | table = 8 words copied by CBRECTA |
| Bmmx 0x04 | BDBREA (CRHEOD, odd start) | [strategy][BRKMAX word]; strategy 7 → +16-byte table; strategy clamped to ≤7; table source differs for strategy 11B | **break message** |
| Sycn 0x13 / Uscn 0x14 | CTOBAD | one word | system/user control; USCN awaits ERRS |
| Tmod 0x0C | BTMOD/CTMOD | one flags byte (bits in 5.3) | |
| Ttyp 0x0D | BSTTY | one word (CTTYP) big-endian | |
| Cesc 0x0E | BCESC | one byte 0/1 | |
| Desc 0x0F | BSDAE | one byte = escape char | |
| Isrq 0x22 / Isrs 0x23 | BISIZ/OISIZ | empty / 2-byte count | |
| Nowt 0x24 / Tnow 0x25 | NWSTA (CRHEEV) | one status byte | |
| Cpco 0xFA | SNDCP (CRHEEV) | two words | completion code |
| Reje 0xFE | SNDREJ/REJECT | one byte = rejected type | + RFI when data was rejected |
| high-priority singles | prebuilt heads | (see 6.2) | DUMM/ESCA/RLOC/DCON/CERS/RECO/NWRE + data-bearing ISRS/ERRS/TREP |

Parsing: GETMES walks `[type][count]` word-oriented, skipping 0x00 pads, erroring (→ reject) when count exceeds the remaining buffer — our `TadChain.Parse` walk is behaviourally identical including the pad rule; its tolerance of a truncated final message (clamping `available`) is more lenient than GETMES (which rejects), acceptable for an observer, wrong for a strict server. `TadChainWriter`'s no-pad single-message form is safe only because callers use it for even-boundary cases — the class remark already says so.

---

## 12. RFA / FA findings

- FA (QFORM, `*FA-SERVER`) — **not in this source**; zero checks possible. Everything in `fa-qform.json` keeps its current status.
- RFA client-side pool: section 4.7. New facts: table `RFSTB` (BOUT: 055572), count `NRFSG` (BOUT: 055571; 20 in this build), error ER188, RT path via RTRFA + semaphore 1730, segment-steal from passive programs, per-user datafield `DRFSG` = `"DSSNM" + index`.
- `04-SIN4-GEN.NPL:3138-3160` (region "DATA SEGMENTS FOR REMOTE FILE ACCESS"): each RFA data segment is 4 pages (entry `0;0;2072;GBSEG;162003`), one per 8SGnn mark.
- Nothing here bears on the FA wire protocol, conversation numbers, ShortAck, or QFORM.

---

## 13. XMSG / HDLC integration

- **PROVEN (J):** `ZXS12`/`ZXS13` (SINA:7-17) are the complete kernel-resident handoff: chain the caller's DCB into the HDLC datafield (`ICHAIN`) and activate level 12 or 13 (`ACT12`/`ACT13`), under `PIOF`. Which level is input vs output is **not stated** in this chapter — do not assert it.
- The L03 XMSG kernel exports the same two names (L03SYM:1830-1831), so the mechanism survives into L. **STRONG.**
- `5XLEV = 5` (SYSTABS:19): the XMSG driver monitor runs on interrupt level 5 in J — consistent with L's crash code XXMON "Inconsistency in level 5 monitor queues" (`xmsg-constants.json`). **STRONG for L.**
- This does not advance the DCB field layout in `XMSG-PROTOCOL.md` §12 (that came from `MP-P2-HDLC-DRIV.NPL`, a different, later source) — no conflict either.

---

## 14. Version J versus version L candidates

| Behavior | Version J source | Version L evidence | Assessment |
|---|---|---|---|
| XT-block layout | XTMMX at offset 5; bitmap words at 32-35; block ends at 035 area (SYSTABS:25-51) | L03SYM: XTRSE at 5, memory triple at 36-37, many appended fields | **Proven delta.** Use L03SYM for L work; J comments annotate the stable offsets only |
| TAD op values | full table, SYM-J | identical in SYM-K03; every L-wire-observed value matches | stable; treat as valid for L |
| UMOD 0x2B / MOD8 0x2C | **absent from SYM-J** (and from the COS-TAD driver) | present in L07/M06 symbol tables, absent K03 (registry note) | corroborates "arrived after K"; a J/K peer will never send them |
| XMSG function codes / option bits | XF\*/XD\* values in SYM-J match `xmsg-constants.json` (generated from the M-era include) for every code the driver uses (XFOPN 12, XFSCM 8, XFSND 12(B)=10?, see note) | — | **Note:** SYM-J values are octal (`XFSND = 000014` = 12 decimal) and agree with the decimal constants file throughout the codes checked; no delta found |
| REJE/ISRQ/EDRS mechanisms | present and active | constants exist in L07/M06 tables; never captured | STRONG that L behaves the same; a capture would settle it |

---

## 15. New facts not currently represented in our model

1. **REJE reject message** with its 1-byte payload = rejected type, and the reject-plus-RFI rule for rejected data (C1).
2. **ISRQ/ISRS** — remote ISIZE (C3).
3. **TMOD bit decode** — capital / CR-delay / page-stop / carrier-logout (5.3).
4. **BMMX = break parameters**; BRKMAX word; 16-byte break table; strategy clamp at 7 (4.1).
5. **ECKM table = 16 bytes**, strategy 7 selects it (4.5).
6. **EDRS** — the escape-disabled response (C4).
7. **TREP** terminal-report bits: overrun/parity/framing (5.1).
8. **NOWT/TNOW/NWRE** — the nowait status/restart family; NWRE is bounced back to sender before restarting the user (POF:1273-1277).
9. **RLOC** — the NORD-NET remote-local/rubout signal, escape-class (5.1).
10. **CPCO** — completion-code message, two words (5.1).
11. **CNVERR / XKXXX conversion rule** and TER00-02 error codes (8).
12. **CLIDAT/CLODAT blanking**: cleared data messages become in-place DUMMs; control messages survive a buffer clear (row 11 of section 3) — explains stray DUMMs with nonzero counts.
13. **Exact-FBSIZ acceptance rule** and partner-port silent discard (C5).
14. **RFA client segment pool** — RFSTB/NRFSG/ER188/RTRFA (12).

Plus two model clarifications that change words, not bytes: the ECKM/BMMX "prefix" is CRHEOD odd-start alignment (4.6), and the 0xFD notify is (probably) 7POLL (5.2).

---

## 16. Recommended changes (not applied — this is an audit)

**P0 — none.** Nothing found that makes our current, working exchanges emit wrong bytes on the paths we exercise. The wrong meanings have not yet produced wrong traffic because the affected ops are either copied verbatim or unused.

**P1 — semantically wrong / likely interop problem**
1. Add REJE handling: recognise incoming 0xFE (log + surface the rejected type), and send REJE for unknown control messages in `TadServer`/`TadTerminalResponder`. Files: `SRC/Xmsg.Node/Tad/TadOp.cs`, `SRC/Xmsg.Servers/Tad/TadServer.cs`, `DOC/protocols/tad-wire.json`.
2. Implement ISRQ→ISRS answering (respond with buffered-byte count, 2 bytes big-endian). Files: `TadServer.cs`, `TadOp.cs`, registry.
3. Fix the four wrong registry meanings (Bmmx, Sycn, Reco, Cesc) with the citations in section 4 — same commit as any code-comment fixes, per the registry rule.

**P2 — terminology / model**
4. Add the 12 missing ops to `TadOp` and `tad-wire.json` (values PROVEN from SYM-J/SYM-K03; mark evidence "J+K03 symbol tables + COS-TAD J source; L assumed stable").
5. EDRS on escape-disabled (only matters if we ever set escape-disabled); at minimum add the enum member.
6. Rewrite the "intrinsic prefix" comment (`TadMessageBuilder.cs:351-358`) and the "20-byte table" remark (`EchoStrategy.cs:9-11`).
7. Record the TMOD bit decode in the registry (a proper bitfield block — bits belong in JSON, not prose).
8. Consider naming 0xFD `Poll` (INFERRED tag) in `XmcsmService`/docs.

**P3 — documentation / tests**
9. Un-circle `TadConnectAcceptTests`: the accept/port-assign/DUMM expected byte strings are our own builder's output (agent-verified: `TadConnectAcceptTests.cs:44-47, 104-108, 138-140`). Either pin them to real capture bytes where a real accept exists, or mark them explicitly as regression-of-our-own-output, not protocol truth. The rule-shaped tests in `TwoNodeTerminalTests` are the better pattern.
10. Annotate `xmsg-kernel-l03-symbols-decoded.txt`'s XT rows with the J comments for the stable offsets (6.3), noting the J→L delta.
11. Add WHAT-WE-DO-NOT-KNOW rows for: FBSI payload meaning, SYCN payload encoding, 7POLL usage, whether L still sends REJE (a capture would settle it), and the CORQ/CORS naming.
12. `X4FSO` units disagreement between `XMSG-GENERATION-VARIABLES-2026-08-07.md` ("WORDS") and `XMSG-PROTOCOL.md:1064` ("bytes") — pre-existing, unresolved by this source; pick it up separately.

---

## 17. Unresolved questions

1. **B3 (accept parameter trailer)** and **T1 (session-port rule)** — TADADM is on segment SG36 and not in this listing. Still open; still need a machine experiment or the TADADM binary.
2. **FBSI (0x15) payload `01 08`** — the driver's buffers are `NOBUFF × FBSIZ`; the trailer bytes could be that pair, or FBSIZ=0x0108=264, or neither. **Speculation only**; a capture varying the value, or the TADADM code, would settle it.
3. **SYCN payload values** (0x0002/3/6/A on L wire) — the J driver shows the channel, not the codes; whatever writes them (UEADM?) is not in this source.
4. **LUN index allocation rule** — TADADM-side; unchanged.
5. **7POLL** — name INFERRED from value only; its sender/receiver logic is not in the COS-TAD driver.
6. **Does L still send REJE / EDRS / TREP?** Constants exist in L symbol tables; behavior uncaptured.

---

*Every NPL citation above was read in the listing during this audit; line numbers refer to the `NPL-CLEAN` split files (address-stripped). The same content with octal load addresses is in `SINTRAN/NPL-SOURCE-2/NPL/`, and each routine's absolute address in this J build is in `BOUT-6.SYMB` (dump starts line 2079). Nothing under `SINTRAN/NPL-SOURCE-2` was modified.*
