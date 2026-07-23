# domino-nucleus-io - DOMINO / NUCLEUS / octobus-driver carve (2026-07-19)

Byte-verified carve of the SINTRAN L07 host-side I/O stack toward octobus
device controllers (DOMINO SCSI, MF controllers, ND-5000 CPUs). Produced
2026-07-19 in one session by three parallel carve passes; this README is
the handoff/index for the folder.

Evidence: [V] = byte-verified here (dd-reproduced anchors), [NPL-V] = NPL
logic (different revision), [I] = inference, [OPEN] = explicitly open.
Overlay for EVERYTHING in this folder: **017-S3SMPIT = 026-S3IMPIT**
(byte-identical, cmp = 0 diffs), load base 032000B.

## Files

| File | What |
|---|---|
| `BDIO-DOMINO-DRIVER-CARVE.md` | The in-kernel DOMINO block-I/O driver: STRBDIO/REBDIO/MBUILD/DCNVA annotated asm + pseudo-C. BDIO message in DOMDF=041064 (body at DSVER=041104B), fn 166B read (size 74B) / 167B write (70B) / 213B compare (70B), sent via NKWRI 043411 / NKSEN 042171, completion NKREC 043076 / NKREA 043375, HSTAT error ladder (-2 SINEC 1661, -4 SINEC 1662 + BDTMU retry). DCNVA: DOMINO byte addr = ((word_addr - (ADRZERO<<10dec)) << 1) \| bit31, self-modifying bias cache |
| `OCTOBUS-DRIVER-ROUTINES-CARVE.md` | SKICK/SIDEN (037254/037256), MBSEND (037425), OMBREAD (037660), XKICK500/LV12KICK (146526/146555), XRS5CPU/RS5CPU, 5OMBREAD (146756), CON5OMD, MFPREPARE (147300), CON5IDENT, 5MTRANS (143445), 5MRDTRANS - annotated asm + pseudo-C. MBSEND has NO IOXT (queues + fires level 13 P:=SOCTW); OMBREAD record = [0]=src station [1]=src OMD [2]=broadcast [3]=count +4 payload; MFPREPARE wire body 0E 01 <5OMDNO>; L07 SAMSON stations 70-77B |
| `NUCLEUS-PRIMITIVES-CARVE.md` | NKSEND/NKGETINFO/NCALL/NKWRI + NUCST: kernel structure byte layout (master block; 40B-word descriptors; port +20 = KICK DEST octobus station; kick table 14B-word entries KHEAD/KTAIL/KLOCK), NKSEND -> SKICK(kick 1 NUCKI) path, DKICK 044747 receive drain, MON 347B = SERVE 047072 (MCTAB), TSET opcode 140516 lock 070000B |
| `NUCLEUS-SEGMENTS-RECON.md` | Recon of segments 104/105-NKSE (NUCLEUS server "C02 Sep 26 1988") + 106/107-NKNA (PLANC name server) + follow-up carve target list |
| `CONKI-KICKENT-CARVE.md` | S0-1 (2026-07-20): CONKI @040765 fully carved. HEADLINE: incoming octobus KICK 1 dispatches to DKICK @044747 (NKINI -> CONKI(T=1, A=14B=PIL level 12, X=0, B=125144); receive chain 035555 -> 036047 KICKENT[frame & 17B] -> 036233 level-12 fire, P := mem[125143]). Kick args proven, receiver masks with 17B, dispatch codes 0-2/5/12B-14B mapped |
| `DOMDF-INITIALIZER-CARVE.md` | S0-2 (2026-07-20): DOMDF initializer = FILSYS 006-S3FS pool/port module (QUINI @134206 lazy -> DLPRT/DSVER/DMSIDs via MON 347 fn 1/6; PDF.DRPRT via DOPPR @136352 = fn 3 open-port-by-NAME). DSVER+32..67 "static header" DISPROVEN (don't-care tail). Unit binding = named pool port + DXPOO/OPAIN. BDTMU/BDTMV are RPIT not MPIT (poisoned prior) |
| `NKSE-SERVER-INTERIOR-CARVE.md` | S0-3 (2026-07-20): segment 105-S3INKSE interior. PLANC-compiled server (runtime lib 112xxx); doNuc dispatcher @037033 fns 1..14B (table dd-verified); fn 10B @047432 = descriptor create/provision (port +20 KICKDEST, +30 OWNID - kernel-layout coherence PASSED); ACONV/walker/allocators; NCALL wrappers. Backed by 6 a-nkse-105-*.txt listings |
| `PROMAN-AUTORUN-RECON.md` | S0-4 (2026-07-20): PROMAN does NOT run at boot on this image [V] (live RT listing PASSIVE P=0; no PMA-* files on pack; segments 120/121=PROMAN, 124/125=BOPCOM string-proven). DIOC stations 10B-13B get NO boot traffic. NEW [OPEN]: command-processor server-start table (003-S3CP @0xbb60) with unknown consumer |
| `QUDF-ABPA2-PRODUCER-CARVE-2026-07-23.md` | Answers the DSTBL-unit question. `GAPFU 000744B` / `GAPFD 034006B` copy the ABSTrans media address (ABP21) into QUDF.ABPA2 VERBATIM (`LDD I,B 2 / STD ,X 17`, xxd-verified), and the DOMINO `BDMTR` path skips the SMD `TOSECT` geometry conversion. So DSTBL is a LOGICAL 2KB-page/block index handed to the DIOC unchanged; disk byte offset = DSTBL*2048. [V driver path] |
| `BDIO-ADDRESS-MODEL-FINDINGS-2026-07-23.md` | The BDIO record IS an ABSTrans message (field map to ND-820023). Memory address DMYAD is window-relative once bit 31 is stripped (`mpmByte = mpmStart + (DMYAD & 0x7FFFFFFF)`; ADRZERO cancels) - RESOLVED. Media address DSTBL (ABP21) unit still [OPEN]: strongly evidenced as a 2KB-page index but not byte-verified (a sector-addressed SMD sibling exists); close via the QUDF.ABPA2 producer carve or a live STRBDIO trace. Backs the RetroCore `BdioRecord.cs` decoder |
| `a-*.txt` | Full annotated disassembly listings (EAs, resolved indirect pointers, labels) backing the NUCLEUS doc + the S0 carves (a-conki-040765.txt, a-domdf-init-006-s3fs.txt) |
| `STATUS-PATCH-*.md` | The paste blocks that were folded into the status docs (kept for provenance; already APPLIED 2026-07-19) |

## Status-doc integration (done 2026-07-19)

- `SINTRAN\CARVING-HANDOFF.md` section 1.9 (+ MCTAB validation example
  fixed: "200B->007516B" was WRONG, MCTAB[200B]=0; use 005B/144B/317B/347B).
- `SINTRAN\ND500\ND500-STATUS-AND-INDEX.md` section 0e + document index.
- `SINTRAN\ND5000\SINTRAN-OCTOBUS-MESSAGE-CATALOG.md` corrected (MBSEND
  no-IOXT; 9FLER offsets LMREC+2/+3 for L07; LN5DEST=77B in L07).
- Architecture/plan context:
  `SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
  (phase D updated with these results).

## Open items / next session targets

UPDATE 2026-07-20: phase S0 of the SCSI-DIOC plan
(`SINTRAN\ND5000\SCSI-DIOC-OCTOBUS-EMULATION-PLAN-2026-07-20.md`)
executed against this list. S0-1 CONKI: DONE (kick 1 -> DKICK,
CONKI-KICKENT-CARVE.md). S0-2 DOMDF initializer: DONE (FILSYS 006-S3FS,
DOMDF-INITIALIZER-CARVE.md; closes item 3's DOMDF-words question). S0-4
PROMAN: DONE (does not run, PROMAN-AUTORUN-RECON.md; supersedes item 4's
"needs a live trace" for the SINTRAN-facing question - controller
firmware itself still absent). S0-3 = item 1 (NKSE interior): DONE
(NKSE-SERVER-INTERIOR-CARVE.md; recon targets 1 DONE, 2/3 PARTIAL,
5 open). Remaining [OPEN] tail across the folder: DRPRT/DLPRT
sub-offset pin, freelist head master offset (runtime global), full
NCALL + MON 347 request/answer per-word maps and connect-answer
DIPOO/OPAIX/ARESZ fields (live round-trip = SCSI plan S4-2), fn
11B-14B worker bodies, item 2 ENKIC overlay (deferrable).

1. NKSE segment-105 interior carve: hash array, buffer freelists, NCALL
   mailbox map, server dispatch (`doNuc: unknown func=`) - target list in
   NUCLEUS-SEGMENTS-RECON.md.
2. [OPEN] ENKIC=047526 (N500-SYMBOLS, ACCP family) resolves in no carved
   overlay tried; callers in 007-S3DMAC, 130-CFT, 135-XFTRAD, 134-SNA3270.
3. [OPEN] OMBREAD entry[-10..-12] counter semantics; helper 036765
   dispatch; MBSEND error 101426 site; DOMDF static header words;
   WT12/NFUNC wake path internals.
4. Controller-side firmware (PROMAN boot images, DOMINOS, PMA-SCSI-BDIO /
   PMA-ETH3-*) is ABSENT from the repo and F: (manuals only) - needs a
   live trace or media find before the DIOC internals can be emulated
   beyond the host-visible contract carved here.
