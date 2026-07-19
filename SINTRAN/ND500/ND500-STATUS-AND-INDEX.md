# ND-500 <-> ND-100 — Status, Evidence Register, and Master Index

**Full path:** `SINTRAN/ND500/ND500-STATUS-AND-INDEX.md`

**THIS IS THE ND-500 STATUS DOCUMENT OF RECORD.** It is the single place that answers
"what do we actually know about ND-500 <-> ND-100 communication, and how well do we know it?"

**Goal driving this work:** recreate the ND-100 <-> ND-500 communication in the RetroCore emulator.
**Version under analysis:** L-VSX-500 (L07).
**Last status refresh:** 2026-07-15.

**Parent status doc:** [`..\CARVING-HANDOFF.md`](../CARVING-HANDOFF.md) — the overall carving/MON effort.
This file is the ND-500-specific view; the parent owns ND-100 dispatch and segment status.

**Maintenance rule:** see section 8. Update this file whenever an ND-500 MON call, segment, symbol,
or NPL routine is analysed. **All output goes under `E:\Dev\Ronny\NDInsight\` — never the D: drive.**

---

## 0d. Q7 ANSWERED - ND-500 completion detection is INTERRUPT-DRIVEN (level 12), not RSTA5 poll (added 2026-07-18)

Answer doc: [`CARVE-ANSWER-Q7-COMPLETION-POLL-VS-INTERRUPT.md`](CARVE-ANSWER-Q7-COMPLETION-POLL-VS-INTERRUPT.md).

**Verdict [V-NPL + V symbols]: INTERRUPT-DRIVEN.** Walked `5STDRIV` in
`../NPL-SOURCE/NPL/MP-P2-N500.NPL:659-697`. When level 12 fires (LV12B=000140 octal =
level 12 [V]; re-armed by `WT12` at :693, no busy-poll), `5STDRIV` reads `RSTA5` via
`CLE5STATUS` ONLY to branch on the ERROR group (`IF A/\720><0`, bits 4/6/7/8 =
5PAGF/5DMAER/5PFAIL/5POWOF), then UNCONDITIONALLY drains the `MAILINK` exec-queue and
calls `CHN5STATUS` per message. `CHN5STATUS` (:730) reads the MPM message field `N5STA`
(offset 2 [V]) to discriminate answer / 5ERANSWER / restart - a shared-memory read, not a
hardware poll. **`RSTA5` (offset 2 from HDEV [V]) has NO completion bit** - its full bit
map (`XC-P2-N500.NPL:41-45`) is all error/power/clock/lock, so there is nothing to poll for
"done". The activate path `XACT500` (:3057) reads `RSTA5` once and ends by the source's own
`% Enable for interrupt` block (`LCON5:=10`, then `LSTA5`/`LCON5`/`SLOC5`) and returns - no
wait loop. The only `RSTA5` spin in the file is `XTER500`'s STOP handshake (`WHILE A BIT
5ILOCK`, :2938), not normal completion. No IOX IDENT read inside `5STDRIV` - the level-12
LINE is the detector; the "ident=16" in the request is neither confirmed nor needed.

Decision tests: (1) branch on a finished bit? NO. (2) assume entered=>finished, status only
discriminates type/error? YES. (3) poll loop outside 5STDRIV that level 12 nudges? NO.
(4) status(error) before draining queue; payload from MPM `N5STA` not `RSTA5`; master-clear
only on timeout; ident not read.

**GAP:** byte-level disassembly of L07 `5STDR` (symbol `5STDR = 0xBA08` = octal 135010) not
done - verdict rests on [V-NPL] logic + [V] symbols; NPL is a different revision (listing
address octal 134610 vs L07 135010, ~0x80-word drift). Byte cross-check is the follow-up.

## 0c. ND LINKER B01 "(-677:52)" - V10: jumpg fork is a CHARACTER dispatch, 0x42 = 'B' (correct, not mis-derived); fault re-pinned at B0040C3C (2026-07-18)

**V10 (current):** [`CARVE-ANSWER-LINKER-LOAD-ERROR52-V10.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V10.md).
Answers the "jumpg $0xB004A588+ ... b.0x30=0x42 -> error 52" relay.
- **`b.0x30` holds a CHARACTER, not an arg-type code** [V]: range-checked as a char
  (`comp2 b.0x30,$0x3C '<'` B003C8C7; `,$0xC7` B003C8CD) and the error-52 arm compares it to
  **`0x41 'A'`** (B003D09F). So `0x42` is literally **`'B'`**, the first name char of `B:NRF` -
  **the dispatch selector is CORRECT, not mis-derived.**
- **Fork map** [V]: `B003C8E2 div4 b.0x30,$0x100 -> b.0x48`; `B003C8EB by2 comp $0xB004A584`
  (bound byte = 0x0D); `B003C8F1 if >> go $0xE20` -> out-of-range arm `B003D711`;
  `B003C8F4 jumpg $0xB004A588+`; fall-through calls `B003E0F5`. The error-52 arm is `B003D08F`
  (`call B003DCE2` then `B003D0E2 call B0040C3C`).
- **`B004A570` region is a DATA constant pool** (referenced as data by `B003C850 by scopa
  $0xB004A570`, `B003C876 bmove $0xB004A578`), NOT the "counter code" the linear disassembler shows.
- **[OPEN]** jumpg entry stride/format + div4 result placement - unverifiable from static bytes,
  NOT guessed. Now MOOT for the fix: since 0x42 is correct, the fault is `B0040C3C`'s grammar gate.
- **Action unchanged from V9:** run `LOAD "B:NRF"` (exit A) / `LOAD B.NRF` (exit B). Decoding the
  table is unnecessary.

**V9:** [`CARVE-ANSWER-LINKER-LOAD-ERROR52-V9.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V9.md).
H2 fully disproven by nd500x runtime: `B0040C3C` IS LOAD's intended arg-resolver (reached
`B003C8F4->B003D08F->B003D0E2`), not a misroute; the object-open routines are simply downstream of a
check that aborts first.
- **Under-stated before, now corrected:** `B0040C3C` has **TWO accept exits** - `B0040D3F` (arg
  contains a `"` 0x22, found by the quote-scan `B0040D33-D40`) OR `B0040D84` (`b.0x49==4`, i.e. a `.`
  0x2E). A bare colon `B:NRF` has neither -> error 52. Arg contract = "quoted, or dot-bearing name".
- The `{0xB0048FEC,5,9}`="B:NRF" descriptor is the RAW command-line slice set by `B003C8F4`; the
  recursive parser `B003CFDA` (B0040C3C sites B003D064 & B003D0E2; dispatcher B004D4F4 sel 0x28/0x23)
  never inserts a `"` or `.`.
- **Decisive live test handed back:** type `LOAD "B:NRF"` (exit A) or `LOAD B.NRF` (exit B); whichever
  passes names the contract and tells us what the input path must produce. Recommend pausing static
  drilling until that one-line test picks the exit.

**V8 (LOAD is a real command; data-driven/CPS dispatch):** [`CARVE-ANSWER-LINKER-LOAD-ERROR52-V8.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V8.md).
nd500x proved live that LOAD never enters the object-open subtree (grep of the LOAD-round trace: 0
hits on B004E874/B004CABC/B004AEBC) and dies in the B003Cxxx symbol/spec resolver at B0040C3C.
- **LOAD is a real, first-class command** (DSEG command-name tables at file 0x88a54 full set / 0x88cf4
  restricted set = exactly the session's commands; LOAD at file 0x88b52). So NOT "unrecognized ->
  symbol"; LOAD dispatches as a command and its ARGUMENT `B:NRF` is sent to the wrong parser.
- Dispatch is **data-driven + indirect (jumpg/CPS)**: shared prologue B0031504-152B builds context
  0xB003DFE4-DFF8, jumpg 0xB003DFE8; continuation B0031531 hands a per-command DESCRIPTOR 0xB003EE90
  to executor **B002EF7F**. The file-vs-symbol arg routing lives in that descriptor/executor layer.
- **POISONED PRIORS (mine, deleted):** the `0xB00530BC` keyword-table + `B004ACD9` dispatch model is
  wrong (0xB00530BC is a structured field table; B004ACD9 never reached, live break count 0). Static
  caller-tracing cannot reconstruct this dispatch (all indirect jumpg).
- **Next (split by who's fastest):** nd500x dumps LOAD's vs OPEN-DOMAIN's command descriptor
  (via B002EF7F / the 0xB003EE90-style pointer); carver then decodes B002EF7F + descriptor format to
  find the arg-type selector that routes to the symbol resolver.

**V7 (object-OPEN subtree disjoint) - V6's dot-form model RETRACTED:**
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V7.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V7.md).
- **POISONED PRIOR (delete V6's NET claim):** "DEABF's dot-form B.NRF becomes B0040C3C's input" is
  FALSE - nd500x set DEABF OUT='B.NRF' live and error 52 was byte-unchanged. Bytes agree: B0040C3C
  parses its window at `B0040C4F` BEFORE its internal DEABF at `B0040D5C`; and the `B003DDCD` resolve
  subroutine is never reached (no-dot branch `B003DD82 -> B003DE13` skips it). The dot-form idea is dead.
- **Verified call graph:** the USER-OBJECT `MON 50B OPEN` is `B004E874` (dynamic `'`-quoted name) in
  routine `B004E80F`, reached `B004AEBC`(post command-dispatch)/`B004DC33` -> `B004E80F`. The other
  OPEN `B004CABC` opens a FIXED-name (`$0xB0054314`, mode 3) work/scratch file, NOT the object.
- The object-open subtree does **not** pass through `B0040C3C`/`B003CFDA`. The error-52 path is a
  SEPARATE mutually-recursive name subsystem (`B003CFDA`/`B003D19E`/`B003D337`/`B003D442` -> `B0040C3C`).
  So nd500x's hypothesis 2 (LOAD mis-routed into the wrong resolver) is the live lead. `B0040C3C`'s
  `;)`/`.` stage grammar looks like a symbol/spec parser, not a file-name parser.
- **OPEN (not yet decoded):** the command keyword table at `0xB00530BC` (matcher `B004ACD9` after the
  MON 511B read at `B004ACBF`) - decoding it names LOAD's intended handler. Offered to carve next.

**V6 (RETRACTED dot-form model) / canonicalizer B003DCE2 contract:**
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V6.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V6.md). nd500x confirmed
V5 live (break at B0040D85 fires at the B0040C3C gate, I2=0xAE=DEABF; parse window {0xB0048FEC,5,9}=
"B:NRF" ends at stage 1). New byte findings on `B003DCE2` (the name canonicalizer feeding B0040C3C):
- **B003DCE2 SPLITS its input on `.`(0x2E) (B003DD26-DD35); it never ADDS a `.`.** No-dot input is
  carried through unchanged. So a colon `B:NRF` stays dot-less -> stage 1 -> error 52.
- `b.0xBC` is a **scratch descriptor** assembled from input ptr `b.0x14` (B003DD3F/DD52), NOT a
  stored default-type source - corrects the relay's assumption.
- **B003DCE2 is NOT MON-free** (corrects relay point 5): it calls the SAME dispatcher `B004D4F4`
  (DEABF's) at `B003DDCD` (selector 0x28) and `B003DDEE` (0x23). Those are the file-system
  resolution calls the emulator must check. [selector->MON decode is [I], confirm live.]
- **Net root-cause model:** DEABF resolves `B:NRF` -> `B.NRF` (dot-form; nd500x's own observation).
  The real-HW `.` is that RESOLVED name; B0040C3C must parse `B.NRF`. On the emulator the resolution
  op's dot-form is not propagating, so B003DCE2 carries the raw colon window and B0040C3C sees no
  `.`. Fix is in what the B003DCE2 dispatcher call returns - NOT a default-append, NOT b.0x49.
- The prompt buffer `0xB0035CEC` windowed [0,8] = "File name" (a PROMPT pass, not the arg pass);
  multiple B003DCE2 passes exist - the erroring one windows the raw line to `B:NRF`.

**V5 (the parse-STAGE gate itself):**
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V5.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V5.md). After the V4
line-length issue was moved past (DEABF now RESOLVES B:NRF -> found), the linker still errors 52 at a
NEW gate. Byte-verified in `linker-b01.dom.asm`:
- Success gate `B0040D7D` = `comp2 b.0x49,$0x4` and runs on **both** K paths; K only gates the
  `B0040C44` save-link helper (which RETURNS). So preserving DEABF's K is necessary-but-NOT-sufficient.
- `b.0x49` = parse-stage counter, init 1 (`B0040C59`); `;`->2, `)`->3, `.`(0x2E)->4 (`B0040CC1`);
  **`:`(0x3A) sets flag b.0x4C but does NOT advance the stage**. So `B:NRF` ends at stage 1 -> error 52.
- Real fix is upstream: what `B0040C3C` parses is the OUTPUT of `B003CFDA`/`B003DCE2` (caller
  `B003D0E2`, callee `b.0x18`=caller `b.0x20`), NOT the raw "LOAD B:NRF" line. Probe: dump that
  descriptor; it must reach a `.` (stage 4).
- "error 52" is linker-internal: `B0040D85` jumps into the variadic message FORMATTER at `B004DADC`
  (NOT an error-code table) -> blank message slot. The real object `MON 50B OPEN` is downstream at
  `B004CABC`/`B004E874`, never reached because this gate errors first.
- CAVEAT to reconcile: "(-677:52)" also names V4's scanner error (0x9016). The `(-677:52)` render is
  the generic DIV/MOD-64 formatter; confirm whether the two error sites share one status word or the
  team is now genuinely at the later B0040C3C gate.

**V4 (line-length scanner, prior gate):**
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-V4.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V4.md).
History: [`CARVE-ANSWER-LINKER-LOAD-ERROR52-V3.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V3.md)
(raise site + 0x90xx family correct; "71 delimiter-free chars" content-explanation retracted),
[`CARVE-ANSWER-LINKER-LOAD-ERROR52-REFINED.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-REFINED.md)
(display chain correct; event attribution retracted) and
[`CARVE-ANSWER-LINKER-LOAD-ERROR52.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52.md) (retracted).

**V4 mechanism [V]:** for the file-name parameter kind (0x42, bitmap 0xB0048F20), scanner
B0036620's token loop consumes chars <= 0x20 (NUL/space/CR) silently - only %, &, comma, =
terminate; otherwise the token ends at the COUNT limit r.0x94 - 1 (read live from
0xB00491D4 = context 0xB0049140+0x94 at B0036725). Token then copied into B003472C's local
b.0x98 by B00401FC, whose returned desc {ptr, 0, len-1} becomes b.0x44/0x48/0x4C. So the
fixed [0,70] = r.0x94 = 71 at scan time = the LINE LENGTH set at B003519B from line reader
**B003F876**'s returned descriptor (B003F876 = editor/history layer, 0xB004D8xx state,
162-byte history records). **Emulator-side bug: the input path delivers/claims 71-byte
lines.** Decisive probe: write-watch 0xB00491D4, expect 0x47; then read B003F876's return
desc at B0035158. Not stale state - rebuilt every call from the live count.

**ADDRESSING CORRECTION: DSEG file offset = VA - 0xB0000000 + 0x57800, NOT +0x58000**
(proof: command-table record ptr 0xB00314F4 -> "CLOSE" at file 0x88CF4). All prior STATIC
DSEG content reads made with +0x58000 are poisoned; instruction-operand facts survive.

Verified mechanism [V]: "(SSI:NN)" = ONE error word printed as DIV 64 and MOD 64 (octal),
at B003604F/B00360B2 inside reporter **B0035C88** (arg r.0x30). Report hub = **B0015B3F**
(code in H1; 999 = already-reported sentinel); display = code - 0x443 + mem[0xB002C5DC],
and nd500x's trace (H1=0x9016 -> printed 0x906A) proves **mem[0xB002C5DC] = 0x497** at
runtime, so display = code + 0x54. "(-677:52)" = word 0x906A = raw code **0x9016 + 0x54**;
the "-677" pseudo-SSI is a rebase artifact, not a subsystem id.

**The real bug: 0x9016 = "parameter too long", parked at B0035291 inside B003472C - the
universal read-one-parameter routine** (~100 call sites; LOAD calls it at B001648A/B0016689
with prompt "File name", default ":NRF", dest = LOAD's 65-byte name slot {b.0x18,0,0x40}
built at B0016471-B0016481). Token comes from scanner B0036620; delimiter bitmap at DSEG
0xB0048F40 = {NUL, %, &, comma} ONLY (space is NOT a delimiter); line buffer = 0xB0048FEC
(256 bytes). nd500x's run answered a "File name" prompt with a 71-char delimiter-free line:
72 > 65 -> 0x9016 -> deferred retk at B003530B -> LOAD aborts (rethrow B0016446). LOAD's
collection loop ends normally only on an EMPTY answer (parked 0x9011 tolerated at B00164B0).
0xB0048CFC = deferred-error cell; family: 0x9011 no-parameter, 0x9014 copy phase, 0x9015
line-input layer (B0033996), 0x9016 too-long (B0035291, B003541F).
**Not domain state, not segments, not DDBTABLES, not the script logic - the input FEED to
the second File-name prompt is wrong on the emulator side.** Decisive dump: 256 bytes at
VA 0xB0048FEC at the break = the offending line verbatim.

Poisoned priors retracted: (1) 0x106A-as-the-52 (startup DDBTABLES check B004AFBE/B004AFC3);
(2) "-677 is garbage"; (3) **"0x906A = internal 0x46D = error 42 segment-used bit" - WRONG**
(assumed base 0xFFFF9040; real base 0x497). The 0x46D raise sites B001735E/B00185D2 and
their segment-ATT test (desc stride 0x1C, ATT byte +0xA, mask 0x20) remain correctly carved
facts about THOSE routines but never fired in this event.

## 0b. ND LINKER B01 config-prompt carve ANSWERED (added 2026-07-17)

The nd500x session's four questions on the linker's config-prompt "state machine" are
answered in [`CARVE-ANSWER-LINKER-B01-CONFIG-PROMPTS.md`](CARVE-ANSWER-LINKER-B01-CONFIG-PROMPTS.md)
(binary at `D:\ND\500\nd-linker\`, read-only). Three request premises disproven from bytes:
0xB0030DE8 IS written (5 sites, from the item record's +0x3C answer field); B0047162-73 is a
bounded copy loop, not a wait; B004D4F4 is an I/O SERVICE GATEWAY (function code -> jumpg
case ladder; function 1 = read char, masked 0x7F, LF/VT/CR terminators), not the command
loop. The "prompt state machine" = a LINEAR per-parameter prompt sequence (B0014341-B0014626);
B0035319/B003472C = token fetchers on the B0048Cxx tokenizer datafield (B0048CE0 delimiter
byte; refill via B0038FDC when B0048DAC set); B0006FDC announces the two booleans (writers
B0007796/B000719E). Yes/No is a token match via B003C66E, not a byte compare. The 0xB0002000
region = the PLANC CALL STACK (init at B0013B41: base 0x1ABC, limit 0x10000) - nothing to
fill. Escape = feed real answer tokens interactively, not a mode-word poke.

## 0a. ND-5000 activation work flag + head cell ANSWERED (added 2026-07-17)

O1's blocker ("which cell does activation link the message address into; who sets the halfword
work flag the microcode IDLE poll spins on") is answered at NPL grade in
[`../ND5000/CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md`](../ND5000/CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md):
on ND-5000 there is **no MAR/head-cell write** (`XACT500` -> `GO XACTRDY`, NNJ14); the message
address goes only into the ex-queue (`ITO500XQ`) and the **X5FIF ring** (`ITOFIFOQ`; X500DF
header: X5SEM=0, X5HEN=3, X5FYL=4, X5MXF=5, X5FIF=6-7 = 32-bit ring base). The work flag =
**X5ACT, word 5 of the per-CPU MAILINK extension block** (stride 5EXTD=200B): init -1
(XMSINIT), set **0** by `XACTRDY`/ACT51 (145500B) = "work pending"; octobus kick (N100KICK)
is only the preempt path. The flat symbol table collides here (X5FYL=4/X5MXF=5 are X500DF-
relative; X5CPU=4/X5ACT=5/X5PRO=6 are MAILINK-relative) - do not mix bases. Also recorded:
CMSYSPAR=016B, N100IDENT=1, FN5DEST=070B, LN5DEST=073B, SYSPAR=111B (16-word block, MON60
fn 103/104). OPEN there: FIFOB identity.

**Follow-up ANSWERED 2026-07-17** in
[`../ND5000/CARVE-ANSWER-SYSPAR-LSYSPAR-DISAMBIGUATION.md`](../ND5000/CARVE-ANSWER-SYSPAR-LSYSPAR-DISAMBIGUATION.md):
the 3 micro-command-1 words are **NOT** the N500DF+111B block - they are the CON5IDENT
CMSYSPAR payload `S5=5OMDNO<<8, S6=0, S7=0` (5OMDNO = runtime OMD from CONOMD; live 10B
reproduces O1's observed GIVEINT word 100401B exactly). N500DF+SYSPAR(111B) = the ND-500
Monitor SET-SYSTEM-PARAMETERS tunables (ND-60.136 sec 8.10.11; 10 named params, word order
[I]), touched only by MON60 fn 103/104. Name collision, two different structures.

**Follow-up ANSWERED 2026-07-19** in
[`../ND5000/CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION-2026-07-19.md`](../ND5000/CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION-2026-07-19.md):
the octobus-emulation blocker "the 3RMICV that never reaches us / Wrong microprogram". Confirms
the CS-load-stage activation is the **`X5ACT:=0`** write (`ACT51`, MP-P2-N500.NPL:3027 `145500`),
NOT a kick (kick = preempt-only `ACT52`); **`3RMICV` (MICFU=1) is the ND-500 WATCHDOG** carried
on the same ex-queue + `ACTRDY` (RP-P2-N500.NPL:384-390 `130023`), and its miss triggers
`N5TIMOUT/RSTARTALL` (not literally "Wrong microprogram" - that string is [OPEN]). The mailbox is
at a **boot-allocated MPM page** `5MBBANK = 5FPMAILBOX<<10`, header at `+X500DF`, per-CPU block at
`+n*200B` - NEVER at MPM offset 0, which is why `ConfigureMailbox(0x420000)` misses. Emulator fix:
discover the base from SINTRAN's own `X5ACT` `0xFFFF->0x0000` write (address - 0x0A = ext block),
trigger the servicer on that write (keep the OCB kick as the preempt trigger), and answer
`N5STA:=3` (+ version `027232B`@HW7, CPUPAR `001741B`@HW `0o10` for the RMVER path; watchdog reads
neither). OPEN: exact "Wrong microprogram" string source; window-offset attribution; pre-CS-load
vs post-start ordering of the first 3RMICV.

---

## 0. Caller-side carve + swapper deep analysis (added 2026-07-15)

Two folders in this subsystem now hold the ND-100 **caller** side and the ND-500 swapper,
complementing the SINTRAN **worker** carve under
[`tools/sintran-segment-carver/versions/L-VSX-500/re/`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/):

- [`nd-500-mon/`](nd-500-mon/README.md) — the ND-500/5000 MONITOR J04 (`MON-DEBUG:PROG`) caller
  analysis: the single `MON 60` gateway at `146256B`, the 123-thunk table, `mon60-callers/`
  (INDEX + SUBFUNCTION-TABLE + **101 per-subfunction folders**), the RetroCore control-store /
  TAG-OUT-DMA crash handoff, and the bring-up / bus-interface feedback. Front door =
  `MON 60`/N500M -> **FPT2ENTRY ("ENTER ND-500 SYSTEM MONITOR")** -> 5MPM message.
- [`swapper/`](swapper/README.md) — refreshed swapper analysis; start at
  [`swapper/swapper-k01-deep-analysis.md`](swapper/swapper-k01-deep-analysis.md). The swapper is an
  ND-500-side paging/swap worker DOMAIN and a **CLIENT** of SINTRAN (RIOM DMA intake, 29-way
  private dispatch, `MON 377B` = segment-31 monitor call 255 = N5SWAP trapped **outward**).
  The three older swapper docs are retired to [`old/`](old/README.md).

Evidence grade: caller carve = BYTES (resolved from the disassembly); descriptor/purpose naming
cross-checked against the worker source `5P-P2-MON60.NPL` (NPL grade). See each folder's README.

---

## 1. Evidence grades used here

| Grade | Means |
|---|---|
| **BYTES** | Read out of a carved `.bin` by me. Ground truth. |
| **SYMBOL** | Read out of a real symbol-table artifact. Strong, but a value, not a layout. |
| **NPL** | From NPL source. **NPL is a DIFFERENT REVISION than the carved bytes** — logic only, never authority. |
| **MANUAL** | From an ND manual. Weak; often idealised. |
| **INFERRED** | Reasoned. Not evidence. |
| **REPORTED** | A byte-claim from another session/agent that **I have not re-checked**. Treat as a strong lead, NOT as fact. Promote to BYTES only after reading the bytes yourself. |
| **FABRICATED** | Made up by a previous pass. Poison. Delete on sight. |

**Rule: NPL never promotes anything to VERIFIED.** A claim is VERIFIED only when someone read the bytes.

---

## 2. THE HEADLINE — what is and is not known

### 2.1 What is solid

**The 5xx handler BODIES are byte-verified and symbol-pinned** (L07):

| MON | Handler | Address | Folder |
|---|---|---|---|
| 500B | `STAPR` (=`SWITP`) | `140356B` | [500B-StartProcess](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/500B-StartProcess/) |
| 501B | `NSTOP` | `140511B` | [501B-StopProcess](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/501B-StopProcess/) |
| 503B | `NINST` / `XNINS` | `141272B` / `141277B` | [503B-InputString](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/503B-InputString/) |
| 504B | `OSTRS` | `141205B` | [504B-OutputString](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/504B-OutputString/) |
| 505B | `GERRC` | `141633B` | [505B-GetTrapReason](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/505B-GetTrapReason/) |
| 506B | `5SIBM` | `141716B` | (no folder) |
| 510B | `SWMC` | `142153B` | [510B-CallSwapper](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/510B-CallSwapper/) |
| 511B | `DVIO` | `141027B` | [511B-DVIO](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/511B-DVIO/) |
| 512B | `A5XMS` | `142253B` | [512B-XMSGCallA](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/512B-XMSGCallA/) |
| 513B | `B5XMS` | `142253B` — **same body as 512B** | [513B-XMSGCallB](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/513B-XMSGCallB/) |
| 514B | `M5TMO` | `140563B` (in `026-S3IMPIT`, load `32000B`) | [514B-ND500TimeOut](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/514B-ND500TimeOut/) |
| 515B | `5MTRA` | `143445B` | [515B-MultipleDataTransfer](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/515B-MultipleDataTransfer/) |

Other symbol-verified constants: `MOCAL=1`, `TRAPC=2`, `5FMOC=3`; MICFU `3STAR=23B`, `3MONC=24B`,
`3TRAC=25B`, `3WMON=26B`, `3MONO=34B`, `33MON=46B`; STOPREASON = bits 10-14 of `RSTA5`, mask `037000B`.

### 2.2 The gaps that block the emulator

**GAP 0 — MON 60B (`N500M`), the ND-100 -> ND-500 direction. LARGELY CLOSED 2026-07-15.** Essentially
every command from `ND-500-MON` reaches the ND-500 through MON 60B and the 3022 bus interface. The
worker `MCTAB[60B] -> N500M=030416B` is byte-verified; the worker body IS now disassembled (in
`050-S3I5PIT`, 5PIT context) and **all 47 `5IFUNC` subfunction folders are carved**, and the
downstream ND-500 SYSTEM MONITOR (`030-S3SM5`) it hands off to via `5NOPAR`/`FPT2ENTRY` is carved end
to end (FUNCS table + 3022 IOX driver + control-store gate + 5MPM + level-12 return). The prior "lives
outside the carve" conclusion was the wrong-overlay signature and is withdrawn. **See section 3a for
the full resolution and the carve deliverables.** Only remaining sub-item: the bank-2 5IFUNC table for
a per-handler `.ASM`.

**GAP 1 — CLOSED 2026-07-15: the level-12 GOSW dispatch is BYTE-LOCATED.** The compiled `GOSW`
lives in **BOTH `017-S3SMPIT` and `026-S3IMPIT` (identical bytes, load `32000B`)**:

- **Pointer table at `137625B`-`137650B`** (20 slots, 500B-523B; slot 0 file offset `0x1172A`).
- **Range constants at `137623B`/`137624B` = `000500`/`000523`** (L12MIN/L12MAX, read P-relative by
  the range check at `137532B`-`137537B` and the `SUB 60` at `137543B`).
- **Dispatcher = `N5MPA` (SYMBOL-2-LIST `N5MPA=137525`)**: saves MCNO to `137665B`, subtracts 500B,
  `RADD SA DP` computed-goto into a 20-entry `JMP I` ladder at `137545B`-`137570B`, each slot
  indirecting through the pointer table. All offsets reproduced by raw byte reads in both segments.

**Byte-proven slot map** (grade BYTES; names SYMBOL from SYMBOL-2-LIST, which cross-validates —
every named address lands exactly on a byte-verified handler entry):

| MON | Slot value | Symbol | Note |
|---|---|---|---|
| 500B | `140356` | STAPR/SWITP | |
| 501B | `140511` | NSTOP | |
| 502B | `140356` | SWITP | same body as 500B (`STAPR=SWITP`) |
| 503B | `141272` | NINST | (`XNINS=141277` is the inner entry) |
| 504B | `141027` | **NOUTS** | **NPL-only "504B=OSTRS 141205" claim DISPROVEN — bytes say NOUTS, which SHARES its entry with DVIO (both `=141027` in SYMBOL-2-LIST); `OSTRS=141205` is an inner routine** |
| 505B | `141633` | GERRC | |
| 506B | `141716` | 5SIBM | |
| 507B | `142033` | **SPRIO** | previously unknown handler |
| 510B | `142153` | SWMC | |
| 511B | `141027` | DVIO/NOUTS | shared body with 504B |
| 512B/513B | `142253` | A5XMS/B5XMS | |
| 514B | `140563` | M5TMO | |
| 515B | `143445` | 5MTRA | |
| 516B-523B | `137651`-`137663` | M516-M523 | six 2-word stubs, ALL `JMP I` via `[137726B]=137167B` = **NORMM** (common unimplemented/error handler; same target as the range-check failure via `[137620B]`) |

Special case: the dispatcher prologue compares MCNO against `347B` (`137621B`) and routes it via
`[137622B]=050211B` = **`5SERV`** (SYMBOL-1-LIST). The prologue also calls through `[137610B]=023030B`
= **`MONIC` (MONICO)** — the return path is wired right here.

**The four "never located" routines are now SYMBOL-pinned in the same overlay** (bodies still to
be disassembled): `5STDR(IV)=135010`, `CHN5S(TATUS)=135205`, `DECOM(ESS)=135361`, `MCHAN(DEL)=137206`;
plus `NORMM=137167`, `N5MPA=137525`, `N5FUD=137727`, `MONIC=023030`.

### 2.3 Corrected 2026-07-15 — the message fields and status codes are NOT unknown

Previous docs said the 5MPM field offsets and status-code values were "not found in symbol files".
**That was wrong** — they are present in
[`swapper\N500-SYMBOLS.SYMB`](swapper/N500-SYMBOLS.SYMB). Read directly from that artifact:

**Message-block field offsets (grade: SYMBOL for the value; INFERRED that it is a message offset):**

| Symbol | Value (octal) | Cross-check |
|---|---|---|
| `N5STA` | `000002` | |
| `SENDE` | `000003` | |
| `X5CPU` | `000004` | |
| `X5ACT` | `000005` | |
| `MICFU` | `000006` | |
| `STOPR` | `000011` | **matches the independently-known `STOPR` @ `000011B`** |
| `NUMPA` | `000012` | |
| `MCNO` | `000013` | |
| `MSWMC` | `000014` | |
| `TRAPN` | `000016` | **matches the independently-known `TRAPN` @ `000016B`** |
| `SMCNO` | `000037` | |
| `FUNCV` | `000013` | see collision note |
| `KFLIP` | `000011` | see collision note |

**Status-code values (grade: SYMBOL) — the manual hint is now CONFIRMED:**

| Symbol (5-char truncated) | Value | Manual hint said |
|---|---|---|
| `MSGN5` (= `MSGN500`) | `000001` | 1 = to-ND500 |
| `WAITI` (= `WAITING`) | `000002` | 2 = in-process |
| `ANSWE` (= `ANSWER`) | `000003` | 3 = answer |
| `5ERAN` (= `5ERANSWER`) | `000004` | 4 = error |

All four line up with the manual's `0=free, 1=to-ND500, 2=in-process, 3=answer, 4=error` ordering.
Two independent sources agreeing on a 4-value sequence is strong. `0 = free` remains INFERRED
(no symbol for it).

**Two honest caveats — do not skip these:**

1. **`N500-SYMBOLS.SYMB` is a FLAT, alphabetically-sorted, 5-CHAR-TRUNCATED global symbol table.
   It is NOT a struct layout.** It proves *a symbol has a value*. It does NOT prove that value is an
   offset into the 5MPM message block. That step is INFERRED from the NPL usage pattern
   `AAX <sym>; LDATX` (add to X, load indexed), which is a strong but indirect argument.
2. **Value collisions are expected and are NOT contradictions:** `FUNCV=13` = `MCNO=13`,
   `KFLIP=11` = `STOPR=11`, `N5STA=2` = `WAITI=2`. A flat table holding both field offsets AND
   status constants will collide. Do not "resolve" these — they are different namespaces.
   `WAITI=2` is a status VALUE; `N5STA=2` is a field OFFSET.

**Why this table is trustworthy at all** (gotcha 6 — validate before use): `STOPR=000011` and
`TRAPN=000016` both match values known independently. Two known slots match => the table is the
right one.

**Still absent from every source:** the *name* expansions are truncated, so `WAITI` -> `WAITING` is
INFERRED from context; and `0 = free` has no symbol.

### 2.4 POISONED PRIORS — active, must be deleted

1. **The "TAG code" protocol is FABRICATED.** Codes 8/9/16 = MonitorCall / PageFault /
   OperationComplete. It exists **neither in ND hardware nor in SINTRAN**. It lives in:
   - `..\Emulator\ND500-QUICK-REFERENCE.md`
   - `..\Emulator\DETAILED-TAG-MECHANISM-EXPLANATION.md`
   - ~~RetroCore **`NDBusND500IF.cs`** (shipping code!)~~ **PURGED 2026-07-16**: the fabricated
     enums/tests were deleted from `NDBusND500IF.cs` and the test suite, and replaced with the
     carve-verified TAG-IN/TAG-OUT model (Phase 1 of ND500-BUS-INTERFACE-DESIGN.md, live-validated).
   Recorded in [`ND500-L-RELEASE-RE-TASK-HANDOFF.md`](ND500-L-RELEASE-RE-TASK-HANDOFF.md).
   Notably, none of the four real ND-500 MON docs mention TAG codes at all.
   The two Emulator/ docs above are still poisoned - do not cite them.
2. **`..\Emulator\ND500-MESSAGE-STRUCTURE-VERIFIED.md` is MISNAMED.** Its filename says VERIFIED; its
   content explicitly disclaims offsets, struct size, layout, function codes and status codes, and
   contains a section headed *"What I MADE UP (Apologies!)"* admitting the GRAPHICS/DATABASE/COMPUTE
   values 10/20/30 have **no source**. Its "VERIFIED" column means only *"this field name appears in
   NPL"* — name-existence, not layout. The file is internally honest; **only the filename lies.**
   Do not cite it for offsets. Cite `swapper\N500-SYMBOLS.SYMB` (section 2.3) instead.
3. **`ND500-MON-ACTIVATION-AND-MAPPING.md`'s offsets are second-hand** — it cites another doc, not a
   primary source. Same numbers as 2.3, but get them from the symbol file.
4. **Naming disagreement, unresolved:** the routing map calls it `MCHANDEL`; the activation doc calls
   it `MCHANDLE` a.k.a. `5MONICO` (the a.k.a. has **no citation**). Do not treat `5MONICO` as a known alias.

---

## 3. The communication model (current best understanding)

**Grade: NPL-derived. Not byte-proven. This is the thing Phase 1 exists to verify.**

```
ND-500 executes MON
  -> microcode stops the CPU, sets STOPR, writes MCNO + params into the 5MPM message,
     sets status ANSWER (=3, SYMBOL), clears 5ILOCK
  -> hardware raises ND-100 level-12 interrupt
  -> 5STDRIV                (NPL MP-P2-N500.NPL:659)      [NOT located in bytes]
  -> power-fail / comm-error checks
  -> scan exec queue from MAILINK
  -> CHN5STATUS             (NPL MP:730-759)              [NOT located in bytes]
  -> on ANSWER: DECOMESS    (NPL MP:803-818)              [NOT located in bytes]
  -> on STOPR = MOCALL(=1) / 5FMOCALL(=3): MCHANDEL (NPL MP:1251-1406) [NOT located in bytes]
  -> MCHANDEL reads MCNO (offset 013, SYMBOL), saves to SMCNO (offset 037, SYMBOL)
  -> IF A >= L12MIN(500B) AND A <= L12MAX(523B):
         5CMNO-L12MIN GOSW STAPROC, NSTOPROC, SWITPROC, NINSTR, NOUTSTR, GERRC,
                           5SIBMO, SPRIO, SWMC, DVIO, A5XMSG, B5XMSG, M5TMOUT,
                           5MTRANS, M516..M523          (NPL MP:1385-1390)  [THE KEY GAP]
     ELSE -> NORMMC (MP:1277) -> 5RRTWT -> level-1 / background monitor
  -> return path MONICO (NPL CC-P2-N500.NPL:363-372):
         write FUNCV, KFLIP, MICFU=3MONCO(=24B, SYMBOL), status MSGN500(=1, SYMBOL),
         PSTAT=5ACTIVE, then XACTRDY -> LCON5=5
```

Index rule (NPL): **the GOSW index is `MCNO - 500B` (octal).**
Constants (NPL MP:1269-1273): `L12MIN=500`, `L12MAX=523`, `CERN=376`, `N5SWAP=377`.

**MON 60B (`N500M`) is NOT part of this path — it is the OTHER DIRECTION.** See section 3a.

---

## 3a. MON 60B (`N500M`) — THE OTHER DIRECTION. TOP PRIORITY.

**Why it matters:** section 3 is the **ND-500 -> ND-100** path (ND-500 executes MON, ND-100 services
it). **MON 60B is the reverse: ND-100 -> ND-500.** The `ND-500-MONITOR` program (`:PROG`, running on
the ND-100) issues **MON 60B** to make SINTRAN drive the ND-500 across the **3022 bus interface** —
start, stop, load, examine, deposit, trap handling, register access.

**Essentially every command from `ND-500-MON` reaches the ND-500 through MON 60B.** Without it, the
emulator can service ND-500-originated calls but cannot *control* the ND-500 at all. It is the
missing half of the interface, and it is the concrete form of open question **Q3** ("the user side of
the monitor-to-driver interface is not documented").

### What is known

| Fact | Grade |
|---|---|
| `MCTAB[60B] = 005700B -> N500M = 030416B` | **BYTES** — a real, populated worker slot |
| `GOTAB[60B] = 000000` (level-14 fall-through -> `MFELL` -> `CALLP` -> `MCTAB`) | **BYTES** — normal, expected for a non-fast-path call |
| ~67 subfunctions | **MANUAL only** (Loader/Monitor manual + yaml). No byte evidence |
| "servicing lives outside this carve, in the `ND-500-MONITOR` / `MP-P2-N500` back-end" | **INFERRED — and probably WRONG. See below.** |

### The caller side — `ND-500-MON:PROG` (REPORTED 2026-07-15, external decode)

From a parallel disassembly of the `ND-500-MON` program itself. **Grade: REPORTED — not re-checked
against the bytes by this session. Strong lead; do not cite as VERIFIED until confirmed.**
All addresses below are in **`ND-500-MON:PROG`'s own address space**, NOT SINTRAN's.

| Finding | Consequence |
|---|---|
| **Exactly ONE `MON 60` call site** in the whole program: address `146256` (line 52478) | All ~67 subfunctions funnel through **one centralised `N500M` wrapper**. The emulator has a single choke point to implement, not 67 |
| **ZERO `IOXT` instructions in the program** | **ANSWERS Q6.** `ND-500-MON:PROG` never touches the 3022 bus registers directly. **All register access is confined to resident SINTRAN** — exactly as the spec predicted. So the entire ND-100 -> ND-500 control path runs through `MON 60B -> N500M` and nowhere else |
| Wrapper contract: `A` = param-list pointer (`AAA -173`); `JMP 2` = skip return (**success**); `JPL I 23` = direct return (**error**) | Matches the documented MON 60B contract exactly |
| Retry loop: `JMP -21` back to `146254` | The wrapper retries the MON |
| Constant pool at the wrapper's P-relative operands: `146304 = 002032` = **`ECSLOAD` = 2032B** = "CONTROL STORE MUST BE LOADED" | **Exact match to the documented constant** — a strong independent cross-check that this really is the N500M wrapper |
| `146305 = 004017` = `0x080F` = 2063 decimal | Second retry constant; meaning not yet established |

**Why this matters for section 6:** the two directions meet here. `MON 60B` is the *only* ND-100 ->
ND-500 door, and SINTRAN owns the bus registers on both sides. Byte-closing `N500M` therefore closes
the whole control direction.

**To promote from REPORTED to BYTES:** re-derive the `MON 60` site and the constant pool from the
`ND-500-MON:PROG` binary independently, and confirm `002032` = `ECSLOAD` against the L07 symbol table.

### MEMORY-CONFIGURATION — live shared-memory map (OBSERVED 2026-07-15, J04)

`MEMORY-CONFIGURATION` runs safely **without** the control store loaded (pure ND-100-side query).
Live output on Ronny's machine, and the arithmetic closes on every row:

```
   PART       WIDTH        N100   N500P  N500D
     0B      0B-  7777B      Y      Y      Y     (part 0 = pages 0..7777B, all three ports)

                             PAGE          WORD(ND-100)   BYTE(ND-500)
                        ND-100  ND-500
ND-500 address zero:    004100  000000    00010200000    00000000000
ND-500 register block:  004212  000112    00010424000    00000450000
Physical segment table: 004252  000152    00010524000    00000650000
WIP/PGU table:          004211  000111    00010422000    00000444000
```

**Verified facts (arithmetic confirmed from the table):**
- **Base:** ND-500 physical address 0 = **ND-100 page `4100B`** = ND-100 word `010200000B`. This is
  exactly the `DEFINE-MEMORY-CONFIGURATION` parameter ("ND-100 page number for ND-500 phys addr 0").
- **Mapping rule:** **ND-100 page = `4100B` + ND-500 physical page.** (reg block `112B`->`4212B`;
  seg table `152B`->`4252B`; WIP/PGU `111B`->`4211B`.)
- **Unit convention** (matches the memory model): ND-100 addresses are **word** addresses; ND-500
  addresses are **byte** addresses; 1 page = 1024 words = 2048 bytes (`2000B` words = `4000B` bytes).
- **The ND-500 register block is MEMORY-MAPPED into the ND-100 at page `4212B` (word `010424000B`),
  NOT reached by IOX.** This is the mechanism behind the zero-`IOXT` finding: SINTRAN accesses ND-500
  registers through this shared-memory window. **For the emulator: ND-500 register access = ND-100
  word `010424000B`.**
- Physical segment table @ ND-100 word `010524000B` (page `4252B`, ND-500 page `152B`).
- WIP/PGU table @ ND-100 word `010422000B` (page `4211B`, ND-500 page `111B`).

**Grade:** OBSERVED (live J04). The page-base relationship and unit convention are arithmetically
verified. These are J04 values on THIS install; confirm before applying to L07 — the base page
`4100B` is an install/config parameter and may differ.

### LIVE-MACHINE HAZARD — control store must be loaded first (OBSERVED 2026-07-15)

Running `ND-500-MON` (Version **J04**, 88.6.16) on Ronny's machine:
- `LOAD-SWAPPER` -> `> Loading Control Store` -> `Error when loading Control Store` -> `NO SUCH FILE NAME`.
- `VERSION` **looped** on the same control-store error and **required a reboot**.

**Interpretation (OBSERVED behaviour + REPORTED `ECSLOAD` constant):** with the ND-500 control store
(microcode / WCS) NOT loaded, `ND-500-MON` funnels almost every command through a control-store load
attempt, which fails because the microcode image file is absent, and some commands (`VERSION`) loop
until reboot. This is the live counterpart of `ECSLOAD=2032B` "CONTROL STORE MUST BE LOADED" and of
open **Q7** (microcode image NOT FOUND). **No microcode/control-store image exists in the NDInsight
repo** (only the manuals) — so this install genuinely lacks it.

**RULE for the live machine:** until the control store is confirmed loaded, do **NOT** run ND-500-MON
commands — even "read-only" ones like `VERSION` are unsafe (they wedge the machine). First resolve
the microcode image. Unknown, do not guess: the exact default microcode filename this J04 monitor
expects, and whether this is real hardware or the emulator.

### CONTROL STORE / microcode — why the emulator wedges, and the way past it (2026-07-15)

**Context:** Ronny's setup is the **RetroCore emulator** with enough of the ND-100 bus interface
working to lure the real `ND-500-MON-J:PROG` (J04) into starting. There is **no ND-500 CPU / no
microcode** behind it.

**Verified from `Reference-Manuals\500\ND-05.012.01 ND-500 Micro Program Guide.md` section 16.4:**
- The ND-500 uses a **WRITABLE control store (WCS)** — microcode is loaded, not in PROM. An ND-500
  with nothing loaded has no working CPU.
- Standard system microcode file = **`(SYSTEM)CONTROL-STORE:DATA`** (line 2852).
- Normal ND-500 bring-up (lines 2861-2872): `MASTER-CLEAR` -> `MICRO-START 0` ->
  `LOAD-SWAPPER SWAPPER` -> `START-SWAPPER` -> `GIVE <pages>`. `LOAD-CONTROL-STORE` is NOT in this
  sequence — normally the control store is already loaded at boot; `LOAD-CONTROL-STORE
  CONTROL-STORE,16010,200` is only for *user* microcode.
- Swapper is loaded as `LOAD-SWAPPER SWAPPER` — a domain/file named **`SWAPPER`**.

**Verified live (LIST-FILES on `PACK-ONE:SYSTEM`):** ND-500 files present are ONLY
`ND-500-MON-J:PROG`, `SWAPPER-K:PSEG`, `SWAPPER-K:DSEG`, `N500-SYMBOLS:SYMB`. **There is NO
`CONTROL-STORE:DATA` and no microcode image.** => Q7 confirmed on the live install. Also: Ronny's
earlier `LOAD-SWAPPER swapper-k01` used the wrong name — the file is `SWAPPER-K` (`:PSEG`/`:DSEG`).

**Why it wedges:** with WCS empty, the monitor's `ECSLOAD` "CONTROL STORE MUST BE LOADED" gate fires;
`LOAD-SWAPPER` (and even `VERSION`) implicitly attempt a control-store load, which fails because
`CONTROL-STORE:DATA` is absent — and `VERSION` loops until reboot.

**What `CONTROL-STORE:DATA` actually is + where it comes from (VERIFIED from manual + package doc):**
- It is the **assembled ND-500 microcode image**: an 8-Kiloword microprogram of **144-bit words**
  (Micro Program Guide line 1596). `(SYSTEM)CONTROL-STORE:DATA` is that image; `LOAD-CONTROL-STORE`
  writes it into the WCS, `LOOK-AT-CONTROL-STORE` saves edits back to it (line 2852).
- Produced by ND's **micro-code assembler** `@N500-8K-ASSEM` (section 14.8) from micro-source `:SYMB`
  files + mnemonic tables (`N500-MNE-SYMBOLS:SYMB`, `N500-MNE-VALUES:DATA`); output is a `:DATA`
  object "input to control store" (lines 1557, 1562), default `N500-MICRO-OBJEC:DATA`. Normally
  shipped pre-built by ND, not site-assembled.
- **Shipped on a SEPARATE microcode diskette, NOT in the ND-211305 L package**
  (`ND500-L-PACKAGE-CONTENTS.md`: "No MIC-5xxx microcode file... ND-5000 microcode as a separate
  diskette"). This is exactly why it is absent = Q7. Getting the genuine image is a separate artifact
  hunt, only needed if you ever want to actually EXECUTE ND-500 code.
- Corroborating: on **real** L hardware `@ND-500` prints `No ND-500(0) CPU found` and stops early
  (`ND500-L-PACKAGE-CONTENTS.md` line 69). The emulator answers the bus enough to march past that to
  "Loading Control Store" — so its further progress is emulator responsiveness, not a new fault.

**WHERE the classic ND-500 CONTROL-STORE:DATA comes from — exact ND product numbers (VERIFIED 2026-07-16):**
The microcode is a SEPARATE per-CPU-model floppy diskette product, not part of SINTRAN or the
ND-500/5000 System Package (ND-211305). Source: L-release info section 1.2 "MICROPROGRAM VERSIONS
FOR ND-500/5000" (`SINTRAN\Release-Documentation\ND-860230-6-EN Sintran III - Release Information - L-Version.md`
lines 353-368):

| ND prod.no. | System type    | Microprogram version | Class       |
|-------------|----------------|----------------------|-------------|
| ND-210786 D | ND-550/560/570 | 15211                | classic 500 |
| ND-210787 D | ND-530         | 15311                | classic 500 |
| ND-210701 F | ND-580         | 15111                | classic 500 |
| ND-211272 C | ND-5200        | 11529                | ND-5000     |
| ND-211273 C | ND-5400        | 11629                | ND-5000     |
| ND-211274 C | ND-5500        | 11729                | ND-5000     |
| ND-211275 C | ND-5700        | 11829                | ND-5000     |
| ND-211276 C | ND-5800        | 11929                | ND-5000     |

("or later versions: ...30, etc." - the M/N release docs list the same products at newer revisions,
e.g. 210701 H = version 15113.)

Microprogram version-number families (first-generation table from
`Reference-Manuals\500\ND-05.012.01 ND-500 Micro Program Guide.md` section 16, lines 2271-2276):

| Version family | Meaning                                      | Generation        |
|----------------|----------------------------------------------|-------------------|
| 103xx          | ND-500 CX micro program                      | ND-500/1 classic  |
| 104xx          | ND-500 AX option                             | ND-500/1 classic  |
| 105xx          | ND-500 standard micro program                | ND-500/1 classic  |
| 106xx          | ND-500 CX, AX option                         | ND-500/1 classic  |
| 15xxx          | ND-500/2 (ND-530/550/560/570/580)            | ND-500/2 classic  |
| 11xxx          | ND-5200..5800                                | ND-5000           |

So e.g. version 10509 = first-gen ND-500 standard microcode rev 09; 10609 = first-gen ND-500 CX
with AX option rev 09 - both are CLASSIC ND-500 (144-bit), NOT ND-5000. The AX option adds the
array-processing instruction set (the guide's section 16 text: some user-instruction codes "are
used in the micro program version containing the ND-500 Area Processing Instruction set" - OCR;
the APF library docs describe it as "a special microprogram ... extension of the standard
microprogram"). The xx suffix is the revision; user-microcode space differs per family
(105xx/103xx: 13000B-17777B free; 104xx/106xx: only 16000B-17777B, rest used by AX).

- On-floppy filename, classic 500: the System Supervisor manual
  (`Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md` lines 3546-3642) shows
  floppy ND-210701 ("ND-500/2 CX MULTI CPU MICRO-PROGRAM") contains the file `CONT-STORE-151xx`
  (xx = version/revision), installed with `@COPY-FILE CONTROL-STORE:DATA,CONT-STORE-151:DATA`.
  So classic-500 microcode files on disk are named `CONT-STORE-15xxx:DATA`.
- On-floppy filename, ND-5000: `MIC-5xxx-2-500:DATA`, installed with
  `@COPY-FILE CONTROL-STORE:DATA (211:)MIC-5xxx-2-500:DATA` (K/L/M/N release docs; the M-version
  variant also shows `(21):CONT-STORE:DATA` for classic 500).
- Multi-CPU default filenames (K-release info section 17.7.13): from K onward the monitor picks the
  default per CPU: `CONTROL-1-STORE:DATA` / `CONTROL-2-STORE:DATA` on multi-CPU systems;
  `CONTROL-STORE:DATA` on single-CPU (unless `CONTROL-1-STORE:DATA` exists).
- Hunt targets for a genuine classic-500 image: a diskette image of ND-210786 / ND-210787 /
  ND-210701, OR a system-disk image from a classic-500 site containing `(SYSTEM)CONTROL-STORE:DATA`
  or `CONT-STORE-15xxx:DATA`.
- **HUNT STATUS 2026-07-16: a source is FOUND.** Bo Goran (friend of Ronny) has a floppy carrying
  microcode versions 10509 (ND-500/1 standard) and 10609 (ND-500/1 CX+AX option); a digital copy is
  awaited. When it arrives, sanity-check: 144-bit = 18-byte microwords, full 8KW image =
  8192 x 18 = 147456 bytes per version (file/floppy headers aside). Classic word size 144 bits = 18 bytes/word (SINTRAN Ref Manual
  LOAD-CONTROL-STORE note: "Every micro-program word (144 bits, 18 bytes)"), 8KW control store,
  default LOAD size 20000B (octal) = 8192 words -> a full image is 8192 x 18 = 147456 bytes
  (vs the ND-5800 `CONTROL-STORE.DATA` we already have: 128-bit x 16384 = a different format;
  it will NOT run a classic 500).

**CONTROL-STORE GATE SOLVED 2026-07-15 (byte-verified) — the exact emulator fix:**
`tools\...\re\ND500-SYSTEM-MONITOR\ND500-CONTROL-STORE-GATE.md`. The driver reads interface STATUS
(`RSTA5`, IOX `dev+2`) and tests **bit 9 `5CLOST` (`001000`) = "micro clock stopped"**; if set ->
control store not loaded -> `ECSLOAD` (`2032B`). **Emulator fix: return STATUS with bit 9 (`5CLOST`)
CLEAR** (plus bit 5 `5ILOCK` clear for idle-ready; error bits clear) - i.e. STATUS `0` for a ready,
idle, control-store-loaded ND-500. That alone clears the gate and lets `VERSION`/monitor proceed
without a real microcode image. (Unblocks command/message traffic, not real ND-500 execution.)

**The way past it (for the interface-RE goal — do NOT emulate the microcode engine):**
**Satisfy the "control store loaded" gate** in the emulated bus interface so the monitor proceeds to
the real MON-call / 5MPM message traffic. **Strong hypothesis (Ronny, 2026-07-15, to confirm by
capture): ND-500-MON reads the ND-100 bus interface to decide whether the control store needs
loading; the emulator must answer "already loaded / ready."** There is a precedent chain of interface
status reads: (1) "CPU present?" — the emulator already answers yes (real HW prints
`No ND-500(0) CPU found` here); (2) "control store loaded / running?" — currently answered no.

**Capture, do NOT guess (it is the emulator — one run is authoritative):**
1. **Instrument the emulator's ND-500 bus interface to log every read (address + returned value +
   caller PC).** Issue `VERSION` or `LOAD-SWAPPER SWAPPER` and find the **last interface read before
   `> Loading Control Store` prints.** That read IS the gate. Expected location: the memory-mapped
   register block at ND-100 page `4212B` (word `010424000B`), or a dedicated interface/status word;
   candidate HW regs `CSCNT`/`CSWA` (Micro Program Guide register list, `A,XD,CSCNT`).
2. Make the emulated interface return the **"loaded & ready" pattern** for that read.
3. **Confirm against the binary before trusting it:** in `ND-500-MON-J:PROG`, the branch that decides
   to load control store is the `ECSLOAD` path (decoded in the MON 60B wrapper). Verify its compare
   matches the register/value you captured — so the emulator returns the RIGHT status, not just any
   value that dodges the branch. The interface status word likely packs CPU-present + running/stopped
   + stop-reason + control-store-loaded together; a blind value could clear one bit and wedge on
   another. `LOOK-AT-HARDWARE INTERFACE` reads this same status word.
4. Then `VERSION` / `START-MONCALL-LOG All` should work -> live capture of the ND-100<->ND-500
   traffic (the whole point).

Unknown / do not guess: whether the ND-5000 (SAMSON) control-store handling differs — needs
`ND-05.020`, which we do not have. The WCS is definitely an ND-500 feature per this manual, so this
is not "5000-only".

### MON 60B / N500M worker — RESOLVED 2026-07-15 via the L release-doc PIT layout

**CONFIRMED: N500M worker = `030416B` in `050-S3I5PIT` (the 5PIT overlay).** Three independent
sources agree:
1. **Bytes:** `MCTAB[60B] = 030416B = N500M` (044-S3IDPIT; anchors `5B`/`144B` match).
2. **Authoritative doc:** the L release doc `ND-860230-6-EN ... L-Version.md` section 8.2 "Page Index
   Table Layout" shows **PIT 5 (5PIT) maps `MON 60` at page 13** = virtual `26000B` (page 13 x
   `2000B`), spanning to page 20 = `40000B` ("ND-500 system monitor"). `N500M=030416B` is inside that
   `26000B-37777B` MON-60 region. Segment 050-S3I5PIT ("Image of 5PIT segment") loads at exactly
   `26000B` (release doc segment table, line 2539).
3. **Code:** `050-S3I5PIT@030416B` is a coherent subfunction dispatcher (range-check `177B`, MPY/ADD
   jump table).

**So the monitor runs MON 60 in the 5PIT (PIT 5) context, and `030416B` in that context IS N500M.**
The reason `N500M` is absent from `N500-SYMBOLS` (050's carver-assigned symbol file): that assignment
is "medium confidence" and **wrong** — the 5PIT MON-60 code is resident code named by
**SYMBOL-2-LIST** (which has `N500M=030416B`). The commoncode overlay's *data* block at `030416B` is a
different PIT context (same virtual address, different content — the normal overlay mechanism).

**Prior retraction (that `050-S3I5PIT` was NOT N500M) is itself withdrawn.** The lesson stands: a
carver `symbol_file` guess is not proof — cross-check against the release-doc PIT layout, which is the
authoritative overlay map. **Cleared to extract N500M's subfunctions from `050-S3I5PIT`.**

**Subfunction structure RESOLVED + ALL SUBFUNCTIONS CARVED 2026-07-15.** The dispatch table is
**`5IFUNC`** (128 entries), sourced from `SINTRAN\NPL-SOURCE\NPL\5P-P2-MON60.NPL` (the MON 60 worker's
own NPL). Full map + 3-way cross-verification: `tools\...\60B-N500M\60B-5IFUNC-dispatch-table.md`.
**All 47 subfunction folders are carved** (README + .pseudo.c + verbatim .npl each) under
`tools\...\re\mon-analysis\60B-N500M\` - every distinct `5IFUNC` handler body, the `5NOPAR` common
path, and the error handlers. Index: `60B-N500M\CARVE-PROGRESS.md`.

**Cross-analysis with the caller side done 2026-07-15.** The other session's `nd-500-mon:prog`
(caller) carve (101 folders, `D:\...\mon60-callers\`) was cross-checked against this worker carve:
`tools\...\60B-N500M\60B-CROSS-ANALYSIS-caller-vs-worker.md`. Both derive the `5IFUNC` map from the
same NPL and **agree exactly** (mutual validation: 117B/122B shared body, 024B/157B duplicate,
MSTOP/MSTCL STOP sequence). The worker carve **closes the caller's open item** (the >142B handler
identities); the caller carve **confirms the control-store gate** (gateway `146244` auto-loads the
control store on `ECSLOAD` error - the byte-level cause of the emulator's `VERSION` wedge) and enriches
the `5NOPAR` codes with verbatim purposes. Both converge on the same next target: the ND-500 SYSTEM
MONITOR (`FPT2ENTRY`).

**Follow-up 2 (the ND-500 SYSTEM MONITOR carve) - DONE 2026-07-15. Follow-up 1 remains.**

**ND-500 SYSTEM MONITOR CARVED 2026-07-15 (byte-verified).** The `5NOPAR` hand-off target that builds
the 5MPM message and actually drives the ND-500 - the concrete "more than MON 60" code (SCOPE NOTE) and
the bridge to Phase 1 (5MPM handshake) and Phase 3 (bus interface / bring-up) - is now carved.
Deliverables under
`tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/`
([`README.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/README.md)):
- **Entry chain (byte-verified):** `FPT2ENTRY = FP2ENT = FPT2E = 040003B` (N500-SYMBOLS + SYMBOL-2-LIST),
  in segment **`030-S3SM5` ("ND-500 System Monitor", load base `40000B`)** - the same `40000B` the
  release-doc PIT layout maps at 5PIT page 20. The bytes at `040003B` are `125001 JMP I 1` - a
  **trampoline** to `MEM[040004] = 142231B` = `5FP2E` (the system-monitor entry body), consistent with
  the documented "enter page-table-2 (FPT2)" context switch.
- **FUNCS operation table:** `FUNCS = 142031B` (128 entries) - the server-side twin of the worker's
  `5IFUNC`: `5IFUNC[N]` marshals params, `FUNCS[N]` performs the ND-500 operation. Every entry lands on
  a named N500-SYMBOLS routine (`REGRE`, `PMWRI`, `CSLOA`, `RSTAT`, ...). Maps the whole MON 60B path
  end to end.
  [`FUNCS-dispatch-table.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/FUNCS-dispatch-table.md).
- **3022 IOX bus interface:** the `WADR`/`WRDAT`/`RDATL`/`REDAT`/`WRTAG`/`RSTAT` driver at
  `051023B`+ (via `IOXT`), register-offset map byte-validated both ways (driver bytes AND the hardware
  manual) - matches `ND500-BUS-INTERFACE-REFERENCE.md` section 3.2. Confirms Q6 from bytes (IOX is in
  resident SINTRAN, not the caller).
  [`ND500-3022-IOX-INTERFACE.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/ND500-3022-IOX-INTERFACE.md).
- **5MPM message + `ACT50` activation** and the **level-12 return path** (ISR chain
  `5STDR=135010 -> CHN5S=135205 -> DECOM=135361 -> MCHAN=137206` in the RESIDENT `026-S3IMPIT`,
  dispatch on the MICFU code) are also carved.
  [`ND500-5MPM-MESSAGE-AND-ACTIVATION.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/ND500-5MPM-MESSAGE-AND-ACTIVATION.md),
  [`ND500-LEVEL12-RETURN-PATH.ASM`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/ND500-LEVEL12-RETURN-PATH.ASM).

This clears the prior "cannot enumerate with confidence" (~53% opcode-0) scare: that was whole-segment
linear disassembly; carving from validated symbol entry points (`5FP2E` + the N500-SYMBOLS addresses)
gives coherent code. **What remains is mechanical body-listing of each `FUNCS` routine, not new
mechanism** - the emulator now has the FUNCS/5IFUNC dispatch, the IOX register map, the control-store
gate fix, the 5MPM message + activation, and the level-12 return path.

**Follow-up 1 (still open):** **locate the bank-2 5IFUNC table** in L07 to attach a byte-verified
`.ASM` per handler (the per-subfunction folders are README + `.pseudo.c` + verbatim `.npl` until then).

---
_(historical, superseded by the block above)_ MON 60B / N500M — status (one claim made then retracted)

**Byte-verified and solid:**
- `MCTAB[60B] = 030416B = N500M` (read from `044-S3IDPIT.bin`; MCTAB anchors `5B`/`144B` match).
- Full dispatch chain to that point: `MON 60B -> ENT14 -> GOTAB[60B]=MFELL(072114B) -> CALLP ->
  MCTAB[60B]=N500M=030416B`. Real GOTAB (026-S3IMPIT) verified, not the discredited commoncode one.
- `N500M=030416B` appears in **exactly one** symbol file: **SYMBOL-2-LIST**. In the overlay
  SYMBOL-2-LIST describes, `030416B` is the head of an ND-500 disk/paging **DATA block**
  (`N500M, CMD, NSECT, MNBLK, NOOK, DSKAD, 0PCOU, MPAGS, PGCOP, STOP, PAGTA, SECTA`).

**RETRACTED (was wrong for ~1 iteration):** I briefly claimed the executable N500M body is coherent
code at `030416B` in `050-S3I5PIT`. **That is NOT supported.** `050-S3I5PIT`'s own symbol file is
**N500-SYMBOLS**, which contains **no `N500M` and nothing at `030416B`** — so the coherent-looking
code there is a *different* routine of the I5PIT overlay, not N500M. This was the classic
wrong-overlay trap (gotcha 3): coherent bytes in an overlay whose symbol table does not place the
target there. Do not treat `050-S3I5PIT@030416B` as N500M.

**Honest open question:** where is N500M's *executable* body? Two live possibilities, neither proven:
1. **N500M is genuinely a DATA/descriptor block** (SYMBOL-2 names a data structure), and MON 60B's
   service code is a different symbol reached via `CALLP`'s handling of `MCTAB[N]` — which is NOT a
   plain `JMP ,X` (CALLP @ `032201B` is an elaborate level-switch dispatcher, only partly traced).
2. The monitor-level overlay actually mapped at `030416B` for MON 60B is some segment not yet
   correctly identified (for MON 317B the worker overlay was `003-S3CP`; the per-call worker overlay
   varies).

**Next (rigorous):** fully trace `CALLP` (`032201B`) to learn exactly how it consumes `MCTAB[N]` and
which overlay/level the worker runs on. Only then can N500M's real body be located. Do NOT extract
subfunctions until the worker overlay is proven. Supersedes any "050-S3I5PIT byte-verified" wording.

### The suspect conclusion — a live lead, not a fact

The `60B-N500M` folder (README-only, no body) concluded the worker is **outside the carve** because
the symbol `N500M=030416B` "lands in a data/ASCII region".

**A symbol landing in ASCII is the signature of gotcha 3 (wrong overlay), not of absent code.**
`MCTAB[60B]` is populated with a real worker address; MCTAB slots land on named L07 symbols. The far
more likely reading: **the wrong segment was mapped when that address was decoded.** All the big
segments span to `177777B`, so `030416B` decodes in many of them.

**This is a HYPOTHESIS, not a finding.** It must be tested by the coherence method (gotcha 3): take
sibling symbols near `N500M` and find the segment where they ALL land on parallel entries. Do not
record it as fact until the bytes agree.

### Plan for 60B (Phase 1, item 0 — do this FIRST)

1. Pull the symbols neighbouring `N500M=030416B` from the L07 tables to get a sibling set.
2. Sweep **every** carved L07 segment, not the 4 already disassembled, decoding `030416B` in each.
   Score by sibling coherence. Prime suspects: `030-S3SM5`, `062-S3SSM5`, `046-S3S5PIT`,
   `050-S3I5PIT`, `021-S3NMS5`, `025-S3IRPIT`, `026-S3IMPIT`, `003-S3CP`, `044-S3IDPIT`.
3. If a segment wins, disassemble `N500M` and **byte-recover the subfunction dispatch table**.
   Validate it against known manual subfunction numbers before trusting it (gotcha 6).
4. Trace `N500M` down to the **3022 bus interface** IOX/register writes — this is what the emulator
   must reproduce. Cross-check against `ND500-BUS-INTERFACE-REFERENCE.md` and
   `ND500-BUS-OCTOBUS-HW-INTERFACE.md`.
5. **Carve every dependency `N500M` reaches** (each `JPL`/`JMP` target), recursively, until the path
   from MON 60B to the bus registers is closed and byte-verified.
6. Answer **Q6** on the way: does `ND-500-MON:PROG` ever IOX the 3022 directly, or is all register
   access confined to resident SINTRAN? Tracing `N500M` settles it.
7. Only if the sweep genuinely fails everywhere: revisit "outside the carve", and say which segments
   were checked.

**Deliverable:** a real `60B-N500M/` folder with `.ASM` + `.bin` + `.pseudo.c` on the corrected
model, plus a byte-verified subfunction table. It is currently README-only.

---

## 4. Per-call ND-500 status

**Warning:** the ND-500 table in the full MON index asserts in prose that ND-500 calls were "never
affected by the GOTAB bug ... always byte-verified". **That claim is unsupported** — that table has no
worker column, no segment column, no per-row status. Do not rely on it. This section supersedes it.

### 4.1 Bodies byte-verified, dispatch BYTE-PROVEN (level-12 GOSW located 2026-07-15)
`500B` `501B` `503B` `504B` `505B` `510B` `511B` `512B` `513B` `514B` `515B` — see the table in 2.1
and the byte-proven slot map in section 2.2 (GAP 1, now closed): GOSW pointer table at
`137625B`-`137650B` in `017-S3SMPIT`/`026-S3IMPIT` (load `32000B`), dispatcher `N5MPA=137525B`,
L12MIN/L12MAX constants `500B`/`523B` at `137623B`/`137624B`.
**Corrections from bytes:** `504B` -> `NOUTS=141027B` (shares body with `511B DVIO`; the NPL-only
"OSTRS 141205B" claim is disproven — OSTRS is an inner routine); `502B` -> `SWITP=140356B` (=STAPR);
`507B` -> `SPRIO=142033B` (new); `516B`-`523B` -> stubs `M516`-`M523` all routed to `NORMM=137167B`
(unimplemented). `503B` has an extra `ADDENDUM.md`.
**Folder Dispatch sections still describe the old guess — rewrite pending (Phase 2).**

### 4.2 S3SM5 `0x60` vector routing byte-PROVEN, body decode only PARTIAL

| MON | S3SM5 slot | Vector value | Note |
|---|---|---|---|
| 264B ND500ReadFile | `0x01C8` | `0xA7AD` | not a proven instruction boundary |
| 265B ND500WriteFile | `0x01CA` | `0xA825` | partly coherent decode |
| 266B ND500MagTape | `0x01CC` | `0xA89D` | region only **17 bytes** |
| 416B SaveND500Segment | (ND-500 vector) | body @ `0xbd70` | per-page write-back semantics MANUAL-inferred |
| 420B GetUserRegisters | (vector) | `0xBE0F` | 448-byte window does NOT decode as one clean subroutine; the unrolled descending-immediate pattern is *structurally consistent* with copying 39 registers — INFERRED, flagged |

### 4.3 Byte-PROVEN ABSENT (S3SM5 vector slot = `0000`)

| MON | Slot | Note |
|---|---|---|
| 425B SetProcessName | `0x28a` | |
| 426B GetProcessNo | `0x28c` | |
| 427B GetOwnProcessInfo | `0x28e` | expected ND-100 companion `GPRNA` NOT located |
| 436B SetND500Param | `0x29c` | `GOTAB[436]` hit is a **coincidence** into device table `DT85W` — not a handler. Servicing point NOT LOCATED |
| 437B GetND500Param | `0x29e` | same; coincidental hit on `DT86R`. NOT LOCATED |

### 4.4 ND-100-side, worker byte-verified (from `MCTAB`), dispatch section stale

| MON | Worker | Segment |
|---|---|---|
| 60B N500M | `N500M=030416` | resident / 025-S3IRPIT (but see section 3 — data region) |
| 157B SegmentToPageTable | `ENTSG=067764` | resident / 025-S3IRPIT |
| 262B GetSystemInfo | `CPUST=063022` | 006-S3FS |
| 264B | `500RF=026375` | 006-S3FS |
| 265B | `500WF=026401` | 006-S3FS |
| 266B | `500MT=026351` | 006-S3FS |
| 322B GetSegmentNo | `GSGNO=041424` | resident / 003-S3CP |

### 4.5 No per-row evidence in any index
`410B` `411B` `412B` `413B` `417B` `422B` `423B` — the delivery-mirror snapshot claimed
`410B`/`411B`/`416B`/`417B` map to S3SM5 vectors `0x270`/`0x272`/`0x27c`/`0x27e` as "byte-verified",
but that mirror is a stale snapshot (see parent doc section 1a). **Re-verify from bytes before use.**

---

## 5. Segments to attack (Phase 1)

Carved and **never disassembled** — the GOSW table and the driver chain are almost certainly in here.
**"Uncarved" has almost always meant "nobody looked."**

`tools/sintran-segment-carver/versions/L-VSX-500/segments/`

| Segment | Why it matters |
|---|---|
| `030-S3SM5` | ND-500 monitor. HAS an `.asm` already — verify it. Note: a prior pass found **53% of it decodes as `??? opcode 0x0000`**, and concluded an S3SM5-internal MON dispatch table "cannot be enumerated with confidence". Re-examine that conclusion — it smells like a wrong load base or wrong overlay. |
| `062-S3SSM5` | ND-500 monitor, second copy |
| `046-S3S5PIT` | ND-500 PIT |
| `050-S3I5PIT` | ND-500 PIT |
| `020-S3SDT5` | ND-500 device tables |
| `021-S3NMS5` | ND-500 message system — **prime suspect for the 5MPM message handling** |
| `025-S3IRPIT` / `026-S3IMPIT` | resident PIT; `M5TMO` already located in `026` |

---

## 6. PLAN — Phase 1: close the interface

**The interface has TWO directions. Both must close.**
- **ND-100 -> ND-500** = **MON 60B** across the 3022 bus. **Item 0. Highest priority.** Section 3a.
- **ND-500 -> ND-100** = the level-12 / 5MPM path. Items 1-6. Section 3.

**0. MON 60B (`N500M`) + every dependency, down to the 3022 bus registers — LARGELY DONE 2026-07-15.**
   Essentially all `ND-500-MON` commands reach the ND-500 through this call. The wrong-overlay
   hypothesis on `N500M=030416B` was confirmed (5PIT context), the worker + all 47 subfunctions are
   carved, and the downstream ND-500 SYSTEM MONITOR path to the 3022 bus registers is carved end to
   end. **Deliverables:**
   `tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/60B-N500M/`
   ([README](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/60B-N500M/README.md))
   and
   `tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/`
   ([README](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/README.md)).
   Remaining: bank-2 5IFUNC table for per-handler `.ASM`. See section 3a.

1. Disassemble the section-5 segments; promote each to
   `..\..\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\<seg>\` with `.asm` +
   `.symbols.txt` + `.meta.md`.
2. **DONE 2026-07-15 — level-12 GOSW table BYTE-LOCATED** at `137625B` in `017-S3SMPIT` +
   `026-S3IMPIT` (identical), validated against the required slots (0=STAPR, 1=NSTOP,
   11B=DVIO all land) plus ten more. Full slot map + dispatcher (`N5MPA=137525B`) in section 2.2.
3. Byte-locate `5STDRIV`, `CHN5STATUS`, `DECOMESS`, `MCHANDEL` — **addresses now SYMBOL-pinned in
   the same MPIT overlay** (`135010`/`135205`/`135361`/`137206`, plus `MONIC=023030`); remaining
   work = disassemble the bodies at those addresses (grade BYTES).
4. **Confirm from BYTES** that the section-2.3 symbol values are used as message-block offsets
   (look for the `AAX <off>; LDATX` pattern against the message base).
5. Confirm the status codes from the compare instructions in `CHN5STATUS`. Symbol + manual now agree
   (section 2.3) — close it with bytes and it is done.
6. Byte-locate the return path `MONICO`.
7. **Fix the emulator:** delete the fabricated TAG protocol from `NDBusND500IF.cs`, implement the
   byte-verified handshake, and write the interface spec as the deliverable.
8. Rename or delete `..\Emulator\ND500-MESSAGE-STRUCTURE-VERIFIED.md` — the filename is a lie and
   will re-poison the next reader.

### SCOPE NOTE - complete 500 integration needs MORE than MON 60B (2026-07-15)

MON 60B / N500M is only the **front door**: it validates the subfunction code, copies user params to
the MON60 buffer (`5IFUNC` handler), then `GO FAR 5NOPAR` into the **common system-monitor path**.
The actual ND-500 control (5MPM message build, level-12 driver, register/memory transfer, the reply)
happens AFTER that hand-off, in code MON 60B does not contain. To do complete 500 integration we must
also carve:
- the **`5NOPAR` common path** and the **system-monitor worker** it calls (what builds the 5MPM
  message and issues the operation);
- the **resident ND-500 driver** + level-12 handshake (`5STDRIV`, `CHN5STATUS`, `DECOMESS`,
  `MCHANDEL`, the GOSW table - Phase 1 gaps);
- likely more of `050-S3I5PIT`/`062-S3SSM5`/`021-S3NMS5` and the resident overlay.
Keep an eye out while extracting MON 60B: each handler's `GO FAR 5NOPAR` and any `CALL`/`GOSW` that
leaves the 5IFUNC region is a pointer to code that also needs carving. **Pending external input:** the
other session's decode of the **swapper** and **nd-500-mon:prog** will feed this (caller side + first
real ND-500-side code); integrate when it lands.

### Phase 3 - ND-500 bus interface deep validation + CPU bring-up (AFTER extraction; user-defined 2026-07-15)

Once MON 60B + the other ND-500 MON calls are extracted, do a deep validation pass on the ND-500 bus
interface docs and connect the ND-500 CPU. **The 2026-07-15 ND-500 SYSTEM MONITOR carve already
supplies the byte-grounded inputs for items 2, 3 and 6** (the 3022 IOX register map, the memory-mapped
register window, the control-store gate, the 5MPM message layout + `ACT50` activation, and the
level-12 return path) - see
`tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/`
([README](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/README.md)) and
its [`ND500-3022-IOX-INTERFACE.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/ND500-3022-IOX-INTERFACE.md),
[`ND500-CONTROL-STORE-GATE.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/ND500-CONTROL-STORE-GATE.md),
[`ND500-5MPM-MESSAGE-AND-ACTIVATION.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/ND500-5MPM-MESSAGE-AND-ACTIVATION.md).
User's explicit questions to answer:

1. **Validate the bus-interface documentation we have** (`ND500-BUS-INTERFACE-REFERENCE.md`,
   `ND500-BUS-OCTOBUS-HW-INTERFACE.md`, `ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md`,
   `WHERE-IS-5MPM-LOCATED.md`): analyse deep, validate against bytes, extend where incomplete. Purge
   any remaining fabricated content (the TAG protocol - section 2.4).
2. **Do we actually understand the IOX messages, queues, slots?** Map every IOX command/register on
   the 3022, the 5MPM message-block layout (offsets in section 2.3), the message status/handshake
   state machine (section 3), and the queues (exec queue / MAILINK / time queue). Confirm from bytes,
   not NPL.
3. **Map the bus interface correctly so the emulator can connect the ND-500 CPU.** The ND-100 side is
   memory-mapped register block @ ND-100 word `010424000B` (section MEMORY-CONFIGURATION); ND-500
   register access is confined to resident SINTRAN (zero-IOXT finding). Build the interface spec.
4. **How does the ND-500 CPU logic map to the board on the 500 side of the cabinet?** How the
   hardware interface works physically. (Needs `ND-05.020` ND-5000 Hardware Description, which we may
   not have - flag if missing.)
5. **How does the code running on the ND-500 work - is it the swapper?** The swapper (`SWAPPER-K`) is
   the first real ND-500-side code (Q4; `old\SWAPPER-K01-ANALYSIS.md`). Clarify swapper vs domain
   vs user program on the 500.
6. **Options for loading and setting things in the ND-500 CPU.** Enumerate: `LOAD-CONTROL-STORE`
   (microcode/WCS - see MON 60B `037B ICSLOAD`), `LOAD-SWAPPER` (`007B IPLSWAPPER`), register
   deposit/examine (MON 60B `026B-033B`), memory read/write (`004B/005B/033B`), `MICRO-START`.
7. **How do we get the ND-500 bootstrapped?** The bring-up sequence (Micro Program Guide 16.4):
   `MASTER-CLEAR -> MICRO-START 0 -> LOAD-SWAPPER -> START-SWAPPER -> GIVE <pages>`, plus the
   control-store gate (`ECSLOAD`) the emulator must satisfy. Ties to the missing `CONTROL-STORE:DATA`
   microcode (Q7) and the control-store-loaded check (section 6 item 7). **This is where the whole
   effort converges: MON 60B `037B ICSLOAD` extraction (current work) is the first concrete piece.**

Deliverable: a validated, byte-grounded ND-500 bus-interface + bring-up spec the emulator can
implement to actually run/connect the ND-500 CPU.

### Phase 2 (ND-500-touching cleanup only)
- Rewrite the ND-500 folders' Dispatch sections onto the real GOSW.
- Finish partial bodies: `264B` `265B` `266B` `416B` `420B`.
- Locate the `436B`/`437B` servicing point.
- Reconcile `ND500-MONITOR-CALL-MECHANISM.md` vs `ND500-L-RELEASE-RE-TASK-HANDOFF.md` — they
  disagree on whether the MICFU values are known (the mechanism doc lists `3MONC=24B` from symbols;
  the handoff calls them unverified). Section 2.3 suggests the mechanism doc is right.
- Give the full index's ND-500 table a real worker/segment/status column; delete the unsupported
  "always byte-verified" prose.

---

## 7. Open questions (from ND500-L-RELEASE-RE-TASK-HANDOFF.md)

| Q | Question | Status |
|---|---|---|
| **Q2** | message status code values | **LARGELY CLOSED 2026-07-15** — see 2.3. Confirm with bytes. |
| **Q3** | the USER side of the monitor-to-driver interface: which MON calls the background monitor issues, with which parameter blocks. The `:PROG` side is undocumented | **PARTLY ANSWERED 2026-07-15 (REPORTED)** — it issues exactly ONE: `MON 60B`, from a single wrapper at `146256`, `A` = param-list pointer. The param-block *contents* per subfunction are still open. See 3a |
| **Q6** | does `ND-500-MON:PROG` ever IOX the 3022 directly, or is register access confined to resident SINTRAN? (spec predicts the latter) | **ANSWERED 2026-07-15 (REPORTED)** — **ZERO `IOXT` in the program. Register access IS confined to resident SINTRAN.** The spec's prediction holds. Re-verify from bytes to promote to VERIFIED. See 3a |
| Q1 | 211305 floppy contents | OPEN |
| Q4 | `SWAPPER-K:PSEG/DSEG` — first real ND-500-side code | partially analysed, see `swapper\` |
| Q5 | segment capability word; contradiction C9: 11-bit vs 12-bit segment field | OPEN |
| Q7 | microcode image | **artifact NOT FOUND on `F:\ND`** |

---

## 8. MAINTENANCE RULE

**Update this file IN THE SAME change as the analysis work.** It is the ND-500 status of record.

| Trigger | Update |
|---|---|
| An ND-500 **MON call** analysed / corrected | Section 4 (right subsection), and 2.1 if a body is pinned |
| An ND-500 **segment** disassembled / promoted | Section 5, tick section 6 |
| **NPL** code analysed | Cite it, grade it **NPL**. NPL never promotes to VERIFIED |
| A **symbol** artifact read | Section 2.3; validate against 2 known values first (gotcha 6) |
| A table/address/offset **byte-proven** | Upgrade its grade and say which bytes. Move it out of "the gap" (2.2) |
| A claim **disproven** | Delete it, record it in 2.4 as a poisoned prior. Do not leave it hedged |
| A **Phase-1 item lands** | Tick in section 6, update 2.2 |

- **All paths E: only.** `E:\Dev\Ronny\NDInsight\`. The D: drive was a one-off delivery mirror and is
  NOT a destination — never write there.
- **Never upgrade a grade without reading the bytes yourself.**
- Do not let a "VERIFIED" in a filename or a prose blanket-claim substitute for a per-row status.
  Both have already burned this effort (2.4).

---

## 9. FULL DOCUMENT INDEX (ND-500)

### Status / entry points
| Doc | What |
|---|---|
| **this file** | ND-500 status of record |
| [`..\CARVING-HANDOFF.md`](../CARVING-HANDOFF.md) | parent: overall carving + MON status |
| [`README.md`](README.md) | ND-500 folder readme |

### Interface / mechanism
| Doc | Grade / caution |
|---|---|
| [`ND500-MAILBOX-MESSAGE-CATALOG.md`](ND500-MAILBOX-MESSAGE-CATALOG.md) | **The complete 5MPM message spec for the emulator**: all fields + direction overlays (11B/13B), every MICFU, STOPR codes, ISR dispatch tables, MONICO write-back, watchdog/HIMESS/DUMMESS, swapper MSW*/MON 377B family, 11 explicit UNKNOWNS. NOTE: kills the "40B-47B result block" assumption (no symbol evidence) and flags MP-P2-N500.md sec 7.6 as a conflicting NON-mailbox table |
| [`ND500-WHO-ANSWERS-THE-MAILBOX.md`](ND500-WHO-ANSWERS-THE-MAILBOX.md) | **The mailbox servicer is THE MICROCODE** (hence MICFU) - not the 5015, not the swapper; three-layer model, MICFU handling map, emulator role mapping. MANUAL/DERIVED + trace-consistent |
| [`ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md`](ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md) | **OBSERVED (live traces of real SINTRAN + nd-500-mon J04).** The complete CS-load protocol incl. the previously UNDOCUMENTED verify pass (words 0-7 read-back; mismatch aborts BEFORE micro-start); SLOC5/UNLC5/MCLR5 strobe on IOX READ; RETG5 bit1-clear = clock restart; bare LCON5 activate = lock only. Where it disagrees with the bus reference, the trace record wins |
| [`CARVE-ANSWER-Q7-COMPLETION-POLL-VS-INTERRUPT.md`](CARVE-ANSWER-Q7-COMPLETION-POLL-VS-INTERRUPT.md) | **Q7: completion detection is INTERRUPT-DRIVEN (level 12), not RSTA5 poll.** [V-NPL] walk of `5STDRIV`/`XACT500`/`CLE5STATUS`/`CHN5STATUS` + [V] L07 symbols. RSTA5 has no finished bit; payload read from MPM `N5STA`. See section 0d. GAP: L07 5STDR byte disassembly |
| [`ND500-MONITOR-CALL-MECHANISM.md`](ND500-MONITOR-CALL-MECHANISM.md) | v1.1. **NPL + symbol only, no carved bytes.** Origin of the section-3 model |
| [`ND500-MONITOR-CALL-PARAMETER-PASSING.md`](ND500-MONITOR-CALL-PARAMETER-PASSING.md) | cited second-hand by the activation doc |
| [`ND500-BUS-INTERFACE-REFERENCE.md`](ND500-BUS-INTERFACE-REFERENCE.md) | section 7.4 = MICFU function codes |
| [`ND500-BUS-OCTOBUS-HW-INTERFACE.md`](ND500-BUS-OCTOBUS-HW-INTERFACE.md) | HW interface |
| [`ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md`](ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md) | |
| [`ND500-IF-LOCKING.md`](ND500-IF-LOCKING.md) · [`ND500-IF-USAGE-DEEP-ANALYSIS.md`](ND500-IF-USAGE-DEEP-ANALYSIS.md) | locking / usage |
| [`WHERE-IS-5MPM-LOCATED.md`](WHERE-IS-5MPM-LOCATED.md) | 5MPM location |
| [`ND5000-SAMSON-ARCHITECTURE.md`](ND5000-SAMSON-ARCHITECTURE.md) | |

### MON calls
| Doc | Note |
|---|---|
| [`MON\ND500-MON-CALL-ROUTING-MAP.md`](MON/ND500-MON-CALL-ROUTING-MAP.md) | GOSW at NPL MP:1385; concludes S3SM5 table "cannot be enumerated with confidence" (53% undecodable) — **re-examine** |
| [`MON\ND500-MON-ACTIVATION-AND-MAPPING.md`](MON/ND500-MON-ACTIVATION-AND-MAPPING.md) | offsets here are **second-hand** — use `swapper\N500-SYMBOLS.SYMB` |
| [`MON\README.md`](MON/README.md) | |
| [`ND500-MON-RE-FINDINGS.md`](ND500-MON-RE-FINDINGS.md) | |
| [`mon-oracle-for-NC\`](mon-oracle-for-NC/) | NC deliverables: `262B-CPUST`, `312B-MOINF_317B-UECOM`, tier2, tier3. **NB: the shipped 312B/317B GOTAB values were WRONG and corrected** — see parent doc 3a |

### Swapper — contains the primary symbol artifact
| Doc | Note |
|---|---|
| [`swapper\N500-SYMBOLS.SYMB`](swapper/N500-SYMBOLS.SYMB) | **PRIMARY ARTIFACT** — the offsets + status codes in 2.3. Flat, 5-char-truncated, alphabetical |
| [`old\SWAPPER-MON-DISPATCH.md`](old/SWAPPER-MON-DISPATCH.md) | the only doc that correctly cites the symbol file |
| [`old\SWAPPER-K01-ANALYSIS.md`](old/SWAPPER-K01-ANALYSIS.md) · [`SWAPPER-K01.PSEG.asm`](old/SWAPPER-K01.PSEG.asm) | first real ND-500-side code (Q4) |
| [`ND500-SWAPPER-ANALYSIS.md`](ND500-SWAPPER-ANALYSIS.md) · [`ND500-SWAPPER-LOADING-MECHANISM.md`](ND500-SWAPPER-LOADING-MECHANISM.md) | |

### Execution / scheduling / setup
[`ND500-INITIALIZATION-AND-EXECUTION-GUIDE.md`](ND500-INITIALIZATION-AND-EXECUTION-GUIDE.md) ·
[`ND500-SCHEDULING-ANALYSIS.md`](ND500-SCHEDULING-ANALYSIS.md) ·
[`SINTRAN-DOMAIN-SETUP-DEEP-DIVE.md`](SINTRAN-DOMAIN-SETUP-DEEP-DIVE.md) ·
[`ND500-PLACE-LIBRARY-C9-FINDINGS.md`](ND500-PLACE-LIBRARY-C9-FINDINGS.md)

### Audit / contradictions — READ BEFORE TRUSTING ANYTHING
[`ND500-EVIDENCE-AND-CONTRADICTIONS.md`](ND500-EVIDENCE-AND-CONTRADICTIONS.md) ·
[`ND500-EMULATOR-DISCREPANCY-AUDIT.md`](ND500-EMULATOR-DISCREPANCY-AUDIT.md) ·
[`ND500-L-RELEASE-RE-TASK-HANDOFF.md`](ND500-L-RELEASE-RE-TASK-HANDOFF.md) (**records the TAG fabrication**) ·
[`ND500-L-PACKAGE-CONTENTS.md`](ND500-L-PACKAGE-CONTENTS.md)

### Session handoffs (historical)
[`ND500-RE-SESSION-HANDOFF-2026-07-08.md`](ND500-RE-SESSION-HANDOFF-2026-07-08.md) ·
[`ND500-RE-SESSION-2-HANDOFF.md`](ND500-RE-SESSION-2-HANDOFF.md)

### NPL source docs (grade: NPL — logic only)
[`MP-P2-N500.md`](MP-P2-N500.md) (driver; GOSW @ :1385, MCHANDEL @ :1251-1406) ·
[`CC-P2-N500.md`](CC-P2-N500.md) (`MONICO` @ :363-372) ·
[`RP-P2-N500.md`](RP-P2-N500.md) · [`XC-P2-N500.md`](XC-P2-N500.md)

### `old\` — superseded, do not cite without re-verifying
[`old\MP-P2-N500_API_Documentation.md`](old/MP-P2-N500_API_Documentation.md) ·
[`old\ND-500-INTERFACE.md`](old/ND-500-INTERFACE.md) ·
[`old\ND500-BOOT-DETECTION-MECHANISM.md`](old/ND500-BOOT-DETECTION-MECHANISM.md)

### Emulator side — CONTAINS POISON (section 2.4)
| File | Status |
|---|---|
| `..\Emulator\ND500-QUICK-REFERENCE.md` | **FABRICATED TAG codes** |
| `..\Emulator\DETAILED-TAG-MECHANISM-EXPLANATION.md` | **FABRICATED TAG codes** |
| `..\Emulator\ND500-MESSAGE-STRUCTURE-VERIFIED.md` | **MISNAMED** — content disclaims everything the name implies |
| `..\Emulator\ND500-EMULATION-COMPLETE.cs` · `Interface3022-5015.cs` · `ND100-Interrupt-Level-12-Handler.cs` · `ND500-SEGMENT31-TRAP-HANDLER.cs` | emulator code — audit against Phase 1 output |
| `..\Emulator\ND500-MESSAGE-STRUCTURE-VERIFIED.md`, `ND500-INTEGRATION-GUIDE.md`, `ND500-ENTB-IMPLEMENTATION-GUIDE.md`, `INTEGRATION-WITH-EXISTING-3022.md`, `INTEGRATION-GUIDE-SEGMENT31.md`, `ND100Bridge.md`, `KERNEL-ACCESS-EMULATOR.md`, `MONITOR-CALL-DEBUGGING-GUIDE.md` | integration docs — re-verify |
| RetroCore `NDBusND500IF.cs` | fabricated TAG protocol **PURGED 2026-07-16** (Phase 1 de-fabrication); now carries the carve-verified register/TAG/CS/mailbox model, live-validated |

### Carved data
| Path | What |
|---|---|
| `..\..\tools\sintran-segment-carver\versions\L-VSX-500\segments\` | all carved segments (section 5) |
| `..\..\tools\sintran-segment-carver\versions\L-VSX-500\re\mon-analysis\` | 156 call folders |
| [`..\..\tools\...\re\ND500-SYSTEM-MONITOR\`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/ND500-SYSTEM-MONITOR/README.md) | **ND-500 SYSTEM MONITOR carve (2026-07-15)** - `030-S3SM5`: FUNCS table, 3022 IOX driver + register map, control-store gate, 5MPM + `ACT50`, level-12 return path. See sections 3a, 6 |
| [`..\..\tools\...\re\mon-analysis\60B-N500M\`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/60B-N500M/README.md) | **MON 60B / N500M worker carve (2026-07-15)** - worker `030416B` (5PIT), all 47 `5IFUNC` subfunction folders, `5NOPAR` common path, caller-vs-worker cross-analysis |
| `..\..\tools\sintran-segment-carver\versions\L-VSX-500\re\MON-CALL-INDEX.md` | **authoritative** full MON index |
| `..\NPL-SOURCE\SYMBOLS\{K03,L07,M06}\` | per-version symbol tables |
