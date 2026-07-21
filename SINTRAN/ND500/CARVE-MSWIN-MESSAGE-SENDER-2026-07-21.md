# CARVE: Who posts the MSWIN (reason-5) swapper message with an empty body?

## CORRECTION 2026-07-21 (SUPERSEDED on the sender identity) - see `CARVE-S3SM5-MSWIN-STAMP-AND-FILL-2026-07-21.md`

**This doc's BOTTOM LINE ("no ND-100 routine fills the body; the sender is ND-500-side / 030-S3SM5
monitor or ND-5800 microcode") is DISPROVEN.** A reliable decode of `030-S3SM5` (see the new doc)
shows:
- **`030-S3SM5` is ND-100 code, NOT byte-addressed ND-500 code** (nd100-dis on the byte-swapped LE
  image decodes cleanly at word base 040000B; nd500-dis gives garbage; matches the project memory's
  "S3SM5 is ND-100 (compiled 5STDRIV)" note). It is the ND-100 System Monitor that manages the ND-500
  swapper and emits the "> Loading Swapper" string.
- **S3SM5 ITSELF (ND-100 code) stamps `MICFU(off6):=3SWMESS`(literal 5) and writes `SWFUN(off7)` + the
  ~15-word body** - the MSWIN builder is at runtime octal 140771..141001 and the full body builder at
  162155..162207. So the fill is ND-100 code and does NOT require ND-5800 microcode.
- **Why this doc got it wrong [V]:** its "proven negative" grep covered ONLY the resident nucleus
  (`s3vs-4.symb` + the NPL tree). **S3SM5's source is NOT in the repo** (it is a paged segment,
  carved as bytes only), so its stores were invisible to that grep. The negative was scope-limited,
  not a true absence.

The relay/offset facts below (5ACTSWAPPER writes SWPST+HSWPI only; SWMESS fall-through; MICFU vs SWFUN
both =5 as different fields; the LNEWSWAP-EMPTY genuine-no-work path) remain [V] and useful. Read the
rest of this doc through the correction above.

---

Date: 2026-07-21
Scope: static SINTRAN III L (L-VSX-500) carve, ND-5800 image, D4 flow
Method: direct reading of the carved NPL source + full build symb + carved ND-500
segment map. Every claim graded:
- **[V]** byte/line-verified by direct reading of a cited file:line
- **[I]** inference from that reading (reasoned, not directly stated by the bytes)
- **[OPEN]** not established by the bytes; needs a further carve or live trace

Files read (full absolute paths):
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-N500.NPL`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\CC-P2-N500.NPL`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\XC-P2-N500.NPL`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb` (full Pass-2 build listing)
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-handlers.md`
- `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\030-S3SM5-routine-map.md`

---

## THE QUESTION

In the ND-500 D4 flow (`PLACE-DOMAIN (FLOPPY-USER)LINKAGE-LOAD-H02` then `RUN`
under SINTRAN L on the ND-5800 image), the ND-500 swapper (SWAPPER-K01) is handed
a reason-5 (MSWIN) work message and crashes because the message BODY is empty.
WHO posts that message, and WHY is its body empty? Name the sender routine and the
exact reason the requester MESSBUFF body is never filled.

---

## BOTTOM LINE (graded verdict)

1. **The ND-100 routine that hands the swapper reason 5 + the requester pointer is
   `5ACTSWAPPER` (144762B, MP-P2-N500.NPL:2857), reached for SWFUN=MSWIN=5 through
   `SWMESS`'s generic fall-through (MP-P2-N500.NPL:428/510).** `SWMESS` special-cases
   ONLY `MSWSTART`(7) and `MSWSWAIT`(20); every other SWFUN value, including
   `MSWIN`=5, drops to `CALL 5ACTSWAPPER; GO TRACO`. **[V]**

2. **`5ACTSWAPPER` / `SWMESS` are NOT the sender of the message content. They are the
   ND-100 *relay*.** They copy the requester message's *own* `SWFUN` field into
   `SWMSG.SWPST` (the reason the swapper reads) and the requester message's *address*
   into `SWMSG.HSWPI` - they never fill the requester's 15-word body. **[V]**

3. **The actual SENDER - the code that fills `MESSBUFF.MICFU := 3SWMESS`, `MESSBUFF.SWFUN
   := MSWIN=5` and the body the swapper derefs - is on the ND-500 side and is OUTSIDE
   the carved ND-100 NPL tree. It is the ND-500 System Monitor `030-S3SM5` (and/or the
   ND-5800 microcode) running on the ND-500.** This is proven negatively: a full-tree
   grep of every ND-100 NPL file AND the complete `s3vs-4.symb` build shows `SWFUN` is
   only ever LOADED (`LDATX`), never STORED, and `MICFU := 3SWMESS` is never written by
   any ND-100 routine. Both fields therefore arrive already set from the ND-500. **[V]**
   Attribution to S3SM5 specifically is **[I]** (S3SM5 is the ND-500 system monitor that
   packages the non-500B MON calls; the exact routine inside it is not disassembled).

4. **WHY the body is empty in the D4 flow: `5ACTSWAPPER` only converts the requester's
   ADDRESS into `HSWPI` (`CNVWADR`); the body content must already have been written into
   that requester MESSBUFF by the ND-500-side sender BEFORE the swapper message was
   raised. On the ND-5800 emulation the real ND-500 placement/monitor path that would
   fill it never executes (the swapper is faked / functional), so `HSWPI` points at a
   reused, zeroed process-1 MON-200B buffer and the swapper fn-5 handler derefs zeros.**
   The fill step is not skippable by any ND-100 code - it simply belongs to a sender that
   did not run. **[I]**, resting on the **[V]** fact that no ND-100 code fills the body.

One-line answer: *the poster of the reason-5 message content is the ND-500-side System
Monitor S3SM5 / ND-5800 microcode (uncarved at instruction level); the ND-100 relay
`5ACTSWAPPER` copies only the SWFUN reason and the buffer POINTER, never the body, so
when the ND-500 sender does not run on the emulated ND-5800 the body stays zero.*

---

## EVIDENCE CHAIN (file:line)

### A. The reason value and the field offsets (symbol table)
- `SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT:4595` `3SWME=000005` -> MICFU dispatch value 5.
- `...:4986` `SWFUN=000007` -> SWFUN is field offset 0o7 in the message.
- `...:5680` `MSWIN=000005` -> the swap-function value MSWIN = octal 5.
- `SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT:3540 SWPST=000103`, `3574 SWPST=000103` -> SWMSG.SWPST at word offset 0o103.
  **[V]** reason 5 = MSWIN; note the two "5"s are different fields: MICFU=3SWMESS=5
  (selects SWMESS) and the SWFUN payload = MSWIN=5 (selects the generic path).

### B. Message arrival and dispatch on MICFU (ND-100 driver, level 14)
- `MP-P2-N500.NPL:360` `N500C: X:=X.MESSBUFF=:5MMESSAGE` - the arriving message IS the
  process's MESSBUFF. **[V]**
- `MP-P2-N500.NPL:388-390` `X:=5MMESSAGE; *MICFU@3 LDATX ... GO N5XXC` - dispatch key is
  the message's MICFU. **[V]**
- `MP-P2-N500.NPL:393-395` `N5XXC: A GOSW ... FAR MILLFU, SWMESS, EXAMD, DEPMD % 04-07`
  - GOSW index 05 = `SWMESS`. So a message with MICFU=3SWMESS(=5) routes to `SWMESS`. **[V]**

### C. SWMESS decodes SWFUN; MSWIN=5 falls through to 5ACTSWAPPER
- `MP-P2-N500.NPL:428-430` `SWMESS: ... *SWFUN@3 LDATX % A:=X.SWFUNC`. **[V]**
- `:431-463` `IF A=MSWSTART THEN ... FI` (start-swapper branch, SWFUN=7). **[V]**
- `:464-508` `IF A=MSWSWAIT THEN ... FI` (restart-and-wait branch, SWFUN=20). **[V]**
- `:509-511` `*IOF ; CALL 5ACTSWAPPER ; GO TRACO` - the generic fall-through taken for
  ANY other SWFUN, including MSWIN=5. **[V]** (No MSWIN-specific branch exists in SWMESS.)

### D. 5ACTSWAPPER = the relay: copies reason + pointer, NOT the body
- `MP-P2-N500.NPL:2857-2861` `5ACTSWAPPER: ... X=:D=:MSGTOSW; A:=5MBBANK; CNVWADR; AD=:CMSGTOSW`
  - takes the requester message address and converts it to a word/MPM address. **[V]**
- `:2862-2864` `SWPWAIT; CALL WN5STATUS ... IF A=PSWWAIT THEN` - only proceeds if the
  swapper is free. **[V]**
- `:2866` `AD:=CMSGTOSW; *AAX HSWPI; STDTX` -> `SWMSG.HSWPI := address of requester msg`. **[V]**
- `:2867` `SWACTIVE; *AAX SWPFU-HSWPI; STATX`. **[V]**
- `:2875-2877` `*MICFU@3 LDATX ... IF 3SWMESS=D THEN *SWFUN@3 LDATX` - if the requester
  message's MICFU=3SWMESS, take its SWFUN as the reason (else use the trap number). **[V]**
- `:2883` `X:=SWMSG; *AAX SWPST; STATX % Save reason for activating swapper` -> `SWMSG.SWPST := SWFUN(=MSWIN=5)`. **[V]**
- `:2884-2887` writes only `NUMPA:=6`, `FUNCV:=0`, `KFLIP:=0`, `MICFU:=3MONCO`; then
  `CALL MCCO` restarts the swapper. **No copy of the requester's 15-word body into SWMSG
  occurs anywhere in this routine.** **[V]**

  => The swapper is told *reason 5* and *"the body is over there at HSWPI"*. It then
  RIOMs the body from the requester buffer itself. If that buffer is zero, the swapper
  reads zero. **[I]**

### E. The swapper fn-5 handler dereferences that body
- `swapper-k01-handlers.md` section 5, idx 5 (entry 0x0800836C, worker 1000077534):
  the fn-5 stub loads message-buffer word `[0x240BC]` and calls the working-set
  (re)builder. `[0x240BC]` is the RIOM'd copy of the requester body. Empty body ->
  zero id/params -> the table-build worker operates on garbage. **[V]** for the deref;
  the crash mechanism is consistent with an all-zero body **[I]**.

### F. No ND-100 code ever fills SWFUN or marks MICFU=3SWMESS (the negative proof)
- Full-tree grep for `SWFUN` writes across every `*.NPL` and across `s3vs-4.symb`:
  every hit is `*SWFUN@3 LDATX` (a LOAD). Zero stores. Sites:
  MP-P2-N500.NPL:430, 973, 2877; s3vs-4.symb:58736, 59279, 61183/87532. **[V]**
- Full-tree grep for `MICFU@3 STATX` (writes of MICFU): every write stores `3MONCO`,
  `3START`, `3TRACO`, `3RMED`, `3RMICV`, `3WMONCO`, `3RPREG` - i.e. ND-100-internal
  micro-function reclassifications. **`3SWMESS` is NEVER stored into MICFU** by any
  ND-100 routine; `3SWMESS` appears only in three `IF ... =3SWMESS` TESTS
  (MP-P2-N500.NPL:381, 972, 2876; RP-P2-N500.NPL:369). **[V]**
- Conclusion: a message can only ARRIVE with MICFU=3SWMESS and SWFUN=MSWIN already set.
  The setter is on the ND-500 side. **[V]**

### G. MON 510B (CallSwapper / SWMC) is NOT the MSWIN sender
- `MP-P2-N500.NPL:2047-2051` `SWMC: MSM510 SHZ 10=:D ... *AAX TRAPN; ... STATX; CALL
  5ACTSWAPPER; GO NXTMSG`. SWMC sets `TRAPN`, not SWFUN, and does not set MICFU=3SWMESS.
  Its message therefore takes the *trap* leg of 5ACTSWAPPER (line 2879), not the
  `3SWMESS -> SWFUN` leg. So the operator-visible "CallSwapper" MON is a distinct,
  trap-numbered path and is not the origin of a reason-5 message. **[V]**

### H. The genuine "no work" path proves reason-5-with-empty-body is illegitimate
- `MP-P2-N500.NPL:1060-1065` `EMPTY: A:=0=:D ... X:=SWMSG; *AAX HSWPI; STZTX; *AAX
  SWPIN-HSWPI; STZTX; ... GO NXTMSG`. When the swap-fifo is empty SINTRAN ZEROES HSWPI
  and does NOT restart the swapper (leaves it at PSWWAIT). So legitimately there is
  never a "reason 5 + live HSWPI + empty body". The D4 state is an upstream anomaly:
  a swapper message really did arrive (HSWPI got set), but its body content was never
  produced by the ND-500 sender. **[V]** for the EMPTY path; **[I]** for "anomaly".

### I. The sender lives in the ND-500 System Monitor S3SM5 (uncarved at instruction level)
- `030-S3SM5-routine-map.md` sec 4-5: S3SM5 is the ND-500 System Monitor that PACKAGES
  the non-500B MON calls and the ND-500 command set; the 500B-523B calls are serviced
  ND-100-side. The swapper working-set bring-up during placement is ND-500-monitor work,
  not a 500B GOSW call. **[V]** that S3SM5 is the ND-500 monitor.
- Same doc sec 6: S3SM5's instruction bodies are NOT reliably disassembled (nd500-dis
  frames it inconsistently; only its dispatch tables are solid). So the exact S3SM5
  routine that builds the MICFU=3SWMESS / SWFUN=MSWIN message body is not identified. **[V]**

---

## EVIDENCE TABLE

| Claim | Grade | Evidence |
|-------|-------|----------|
| reason 5 = MSWIN; MICFU=3SWMESS=5; SWFUN=offset 0o7 | [V] | N500-SYMBOLS.SYMB.TXT:4595/4986/5680 |
| Arriving message = process MESSBUFF, dispatched on MICFU | [V] | MP-P2-N500.NPL:360, 388-393 |
| MICFU=3SWMESS -> SWMESS (GOSW idx 05) | [V] | MP-P2-N500.NPL:393-395 |
| SWMESS special-cases only MSWSTART/MSWSWAIT; MSWIN falls to 5ACTSWAPPER | [V] | MP-P2-N500.NPL:431, 464, 509-511 |
| 5ACTSWAPPER writes SWMSG.SWPST:=SWFUN(=5), SWMSG.HSWPI:=addr(requester) | [V] | MP-P2-N500.NPL:2866, 2876-2883 |
| 5ACTSWAPPER never copies the requester body into SWMSG | [V] | MP-P2-N500.NPL:2857-2887 (whole routine) |
| Swapper fn-5 handler derefs the RIOM'd body word [0x240BC] | [V] | swapper-k01-handlers.md idx 5 |
| SWFUN is only ever LOADED, never STORED, in the entire ND-100 build | [V] | grep NPL + s3vs-4.symb (all hits LDATX) |
| MICFU:=3SWMESS is never written by any ND-100 routine (only tested) | [V] | grep `MICFU@3 STATX` vs `=3SWMESS` |
| MON 510B/SWMC is a trap-numbered path, not the MSWIN sender | [V] | MP-P2-N500.NPL:2047-2051 |
| EMPTY path zeroes HSWPI + does not restart swapper (so reason-5+empty is illegitimate) | [V] | MP-P2-N500.NPL:1060-1065 |
| Sender fields set by ND-500 side (not ND-100) | [V] | negative proof (rows above) |
| Sender = S3SM5 ND-500 System Monitor specifically | [I] | 030-S3SM5-routine-map.md sec 4-5 |
| Exact S3SM5 routine that builds the body | [OPEN] | S3SM5 not instruction-decoded (sec 6) |
| Body empty because ND-500 placement/swapper faked on ND-5800 | [I] | MEMORY D4-RUN-BLOCKER finding + EMPTY path |

---

## [OPEN] - what still needs a carve or live trace

1. **[OPEN] The exact S3SM5 (or ND-5800 microcode) routine that builds the swapper
   message** - which routine sets `MESSBUFF.MICFU:=3SWMESS`, `MESSBUFF.SWFUN:=MSWIN`
   and fills the 15-word body. `030-S3SM5.bin` is carved but not reliably disassembled
   (`030-S3SM5-routine-map.md` sec 6). Needs a working ND-500 disassembler / SLEIGH spec
   or a live single-step of the ND-500 monitor during PLACE-DOMAIN.

2. **[OPEN] Whether MICFU=3SWMESS is stamped by the ND-500 microcode or by S3SM5 code.**
   The ND-100 side only reads it; the split between microcode-set vs monitor-set fields
   for a swapper message is not established.

3. **[OPEN] The intended body layout for an MSWIN=5 request** - the swapper fn-5 worker
   (1000077534) reads `[0x240BC]`; what segment-id / descriptor fields a *correct* MSWIN
   body must contain is not decoded. Needed to confirm the emulated buffer is genuinely
   "empty" vs "wrong shape".

4. **[OPEN] Confirm on a genuine classic-500 image that the body IS filled.** The
   verdict that the body is empty *because the ND-5800 swapper is faked* rests on the
   D4-RUN-BLOCKER finding (live) plus the EMPTY-path bytes; a side-by-side trace on a
   real classic-500 placement (where the ND-500 monitor actually runs) would promote the
   [I] to [V].

5. **[OPEN] Identity of the "reused process-1 MON-200B buffer" at HSWPI.** The live
   anchor observed HSWPI = word 0x210718 (byte 0x420E30) reading all zeros; confirming
   statically that this address is a recycled MON-200B/XMSG buffer (vs a freshly
   allocated placement buffer) would nail the "never initialized" mechanism.

---

## NOTE ON NAMESPACES (to avoid a known trap)

`MICFU=3SWMESS=5` (the level-14 GOSW selector) and `SWFUN=MSWIN=5` (the swap
sub-function inside the message) are **two different fields that both happen to hold 5**.
MICFU=3SWMESS selects the `SWMESS` handler; SWFUN=MSWIN selects, inside SWMESS, the
generic `5ACTSWAPPER` path. Do not collapse them into one "5".
