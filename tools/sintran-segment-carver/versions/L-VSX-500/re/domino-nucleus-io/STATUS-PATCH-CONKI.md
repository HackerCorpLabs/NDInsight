# STATUS PATCH - CONKI / KICKENT carve (S0-1) - 2026-07-20

Paste block for the status docs (CARVING-HANDOFF.md; cross-link from
ND500-STATUS-AND-INDEX.md if the ND-5000 octobus section references kick reception).
Do not edit those docs from a subagent; main session folds this in.

## New [V] facts

- **CONKI @ 040765 (SYMBOL-1-LIST, overlay 017-S3SMPIT = 026-S3IMPIT, base 032000B)
  fully carved.** CONKI(A, T, X, B) registers KICKENT[T] := (DLEVE=A, DFADD=B) in the
  ring-X octobus INPUT controller's kick table (pointer table at input-df[-13], bank
  at df[-14], via STDTX). Arg meanings PROVEN: T = kick number (validated 1..17B),
  A = DLEVE dispatch code (= octal PIL level: 12B->lvl10, 13B->lvl11, 14B->lvl12),
  X = ring, B = datafield (DFADD). Errors: 101402 bad ring / 101403 ring not
  configured / 101407 bad kick number (error return L+1, success L+2).
  Doc: re/domino-nucleus-io/CONKI-KICKENT-CARVE.md (+ a-conki-040765.txt).
- **HEADLINE / gate for DOMINO->ND-100 BDIO completion delivery: incoming octobus
  KICK 1 (NUCKI) dispatches to DKICK @ 044747.** NKINI calls CONKI with T=1, A=14B,
  X=0, B=125144 [V bytes 042132-042136 + pointer cells 042164/042165]. Receive path
  [V]: input frame decoder 035555 (C bit 15, K bit 6) -> kick dispatch 036047:
  entry := KICKENT[frame & 17B]; DLEVE & 17B computed-jump 036116; code 14B -> arm
  036233 = fire PIL level 12 with B := DFADD (125144) and P := mem[DFADD-1]
  = [125143] = 044747 = DKICK (dd-verified in 044-S3IDPIT). Matches the send side
  (NKICK -> SKICK kick 1) end to end.
- **KICKENT entry layout [V offsets, NPL-V names]:** (OLINK, DLEVE, DFADD) triplet;
  the df[-13] pointer table stores entry-base+1 (points at DLEVE); OLINK at
  pointer-1 is the busy-chain link (append routine 036022, terminator -1), used when
  the target level is already pending (mask cell mem[007347] [I]).
- **Kick-number aliasing [V]:** the receiver masks the frame with 17B (cell 036137)
  before indexing KICKENT - only 16 receive slots although SKICK accepts kick
  numbers up to 37B; 20B-37B alias 0-17B.
- **Kick dispatch table [V]:** DLEVE codes 0-2 -> datafield driver activation via
  resident 013552 (same routine as SETEV mode 2); 5 -> fire level 5; 12B/13B/14B ->
  activate PIL level 10/11/12 (B := DFADD, P := mem[DFADD-1]); all other codes ->
  error via 000215.
- **ECONID @ 040467 [V structure]:** separate body (ident-entry connect, per-source
  station lists at df[-7], different page-zero ring table at 32B); does NOT share a
  registration helper with CONKI. Disconnect-style twin at 040676 (STZTX pair).
- **NKINI only CONKI caller in this overlay [V]:** exactly one pointer cell
  (042165) holds 040765 in 026-S3IMPIT.

## Closed [OPEN] items / killed priors

- NUCLEUS-PRIMITIVES-CARVE.md section 5.3 [OPEN] "exact octobus kick-1 -> datafield
  wiring: the octobus agent's receive path ends at the ident/RFT dispatch" is now
  CLOSED: the full chain kick frame -> KICKENT[1] -> level-12 fire -> DKICK is byte-
  verified (CONKI-KICKENT-CARVE.md sections 4-5).
- NUCLEUS-PRIMITIVES-CARVE.md section 9 follow-up "CONKI full decode (kick-entry
  registration list at cells 007341-007344)" is DONE - with a correction: cells
  007341/007342 are the CBPOOL free-list head/count (octobus carve 3.3), NOT CONKI's
  registration cells; CONKI writes the KICKENT table in octobus physical memory
  (bank at input-df[-14]), and NKINI's resident cells are 007307/007310 (counters,
  [I] semantics). No prior claim published the wrong cells as [V], so nothing to
  poison - but the 007341-007344 pointer in the follow-up list was a wrong lead.
- The guess in NUCLEUS-PRIMITIVES-CARVE.md section 2 that A=14B "could be the
  datafield, an index, or a level" is resolved: it is the DLEVE dispatch code
  (octal PIL level number 14B = level 12). Any doc text calling 14B a datafield or
  kick count would be wrong - none found published.

## Remaining [OPEN]

- Identity/semantics of resident cell 007347 (level-pending mask tested by the
  level arms) and where the OLINK busy chain is drained (WT10/WT12/WT13 edge).
- Which subsystems register DLEVE codes 0-2 and 5 (no other CONKI callers in this
  overlay; other segments not swept for CONKI pointer words).
