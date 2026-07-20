# Ready-to-paste status patches - NUCLEUS carve (2026-07-19)

Do NOT apply automatically; paste into the two status docs on integration.

---

## Block A - for E:\Dev\Ronny\NDInsight\SINTRAN\CARVING-HANDOFF.md

### NUCLEUS kernel (DOMINO message passing) - CARVED (2026-07-19)

The SINTRAN NUCLEUS primitives and kernel data structures are byte-carved:
`tools/sintran-segment-carver/versions/L-VSX-500/re/domino-nucleus-io/NUCLEUS-PRIMITIVES-CARVE.md`
(+ NUCLEUS-SEGMENTS-RECON.md, + annotated listings a-*.txt in the same folder).

- Overlay: NK* primitives live in **017-S3SMPIT = 026-S3IMPIT** (identical, base
  032000B), NOT in 003-S3CP and NOT in the NKSE segments. Proven by call-target
  density (21 distinct family targets / 64 pointer hits) + uniform prologue coherence.
- Byte-proven kernel structures (offsets octal words): master block (+2 descr-table ID,
  +7 count, +20 kick-table ID, +25 version, +34/40/42/104 trace, +74/76 health flags);
  descriptor records 40B words each (LOCK+0, TYPE+1, OWNER+2..3); port record
  (+10 MESS HEAD, +12 MESS TAIL, +14 KICKLINK, +16 KICK HEAD, +20 KICK DEST=octobus
  station, +21 INQUEUE, +22 KICK PROC/EVENTS, +30 OWNID) - order matches ND-820026
  fig 25 exactly; message record (+10 LINK, +12 BUFFERPOINTER, +14 HOMEPORT,
  +21 OWNINDEX); message buffer (+23 SIZE, +25 LENGTH, data at +26B); kick table =
  14B-word entries per octobus station (KHEAD+0, KTAIL+2, KLOCK+4).
- Kick path [V]: NKSEND -> port+20 == own station ? SETEV local : NKICK -> (first
  entry in queue) SKICK(A=1=NUCKI, X=0 ring, T=station). Receive: DKICK 044747 drains
  own kick entry; DKICK pointer words sit in resident common data at VA 125142/125143
  (carved in 044-S3IDPIT/053-S3SDPIT).
- **MON 347B = NUCLEUS.** MCTAB[347B]=047072 [V] = `SERVE` (the MON-CALL-INDEX name
  "MGDAE" is a flat-symbol-table collision; index row should be renamed and its
  overlay corrected from "003-S3CP" to MPIT). ND-500 side: level-12 `IF A=347 GO
  5SERVER` -> ENUCL 050123 -> N5FU0/1/2 -> NKGET/NKSEN/NKREC.
- **Poisoned prior / discrepancy:** this handoff's MCTAB validation example
  "MON 200B -> 007516B" does NOT reproduce: MCTAB[200B] = 000000 in 044-S3IDPIT [V]
  (coherent: 200B XMSG is a GOTAB level-14 fast call). Keep 005B/144B (+317B/347B) as
  the validation slots.
- Locking: NUCLEUS TSET on physical memory = opcode 140516 (nd100-dis "USER1"), lock
  value 070000B, retry 020 with master+74/76 health-flag abort.
- NUCST (PH-P2-OPPSTART 063570) allocation confirmed as mailbox page + second mailbox
  page + abort table; mailbox phys addr cells = resident 007300/007301 and
  007276/007277 (used by NCALL 050407 / RNMSG 045432).
- [OPEN]: ENKIC=047526 (N500-SYMBOLS, octobus/ACCP family) resolves in NO carved
  overlay tried (MPIT/commoncode/MACM/all covering segments) - belongs to the octobus
  ND-5000 servicer module; NKUSE=144162 runtime-only; server-side structures (hash
  array, buffer freelist, NCALL mailbox map) need segment-105 carve.

---

## Block B - for E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-STATUS-AND-INDEX.md

### NUCLEUS <-> ND-500 bridge (MON 347) - carved 2026-07-19

See `tools/sintran-segment-carver/versions/L-VSX-500/re/domino-nucleus-io/NUCLEUS-PRIMITIVES-CARVE.md`
section 6.

- MP-P2-N500 L1381 `IF A = 347 GO 5SERVER` [NPL-V] lands in **ENUCL = 050123** (MPIT):
  function code from 5MPM message word +102; fn 0..6 -> N5FU0 (NKGET), N5FU1 (NKSEN
  under IOF), N5FU2 (NKREC), N5FU3/5/6; fn 7 -> driver code 137167. Answers via
  NURET 047315 (writes message words +110/+0/+2; chains resident 023044 + driver
  cells 145466/135067). ND-500 owner ids = CLUST (041574) + process number.
- N5FU5 tail (050100) emits SKICK(A=1,X=0,T=station) directly - an ND-500-initiated
  octobus NUCLEUS kick.
- 5NFUN..5NMBU (047541-047555) are the ND-500 NUCLEUS parameter cells in MPIT (zero
  on disk, runtime-populated).
- NUCLEUS delayed abort NKREL -> 5NUREL (MP-P2-N500 L563 area) noted [NPL-V], body not
  carved in this pass.
- [OPEN] handed to octobus/ND-5000 work: ENKIC=047526 overlay (N500-SYMBOLS ACCP
  family: NSPIT/GMESS/ACCPE/VPARP/OCTOS/NMPIT); callers exist in 007-S3DMAC, 130-CFT,
  135-XFTRAD, 134-SNA3270.
