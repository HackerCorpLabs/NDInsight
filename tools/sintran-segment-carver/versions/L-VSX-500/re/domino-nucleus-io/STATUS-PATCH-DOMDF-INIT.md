# STATUS PATCH - DOMDF initializer carve (S0-2) - 2026-07-20

Paste-block of new facts for the status docs (CARVING-HANDOFF.md /
SCSI-DIOC-OCTOBUS-EMULATION-PLAN). Do not edit those docs from this file
blindly - integrate.

## New facts (all [V] unless tagged)

- **S0-2 ANSWERED.** DOMDF initializer located and byte-verified:
  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\DOMDF-INITIALIZER-CARVE.md`
  + annotated listing `a-domdf-init-006-s3fs.txt`.
- The initializer is the FILSYS DOMINO pool/port module in **006-S3FS
  (= 012-S3SFS, identical), VA 133203-137000, base 026000B** - 35 L07
  FILSYS symbols land on parallel PROC entries (overlay proven).
- **QUINI @134206** (lazy, guard flag DOMDF+15): creates the local NUCLEUS
  port via MON 347 (DCRPR fn1, T=3 DOMDF-owner) and writes
  **DOMDF.DLPRT(041103) := port; DSVER(041104) := 1; DOMDF+21 := 30B**;
  then creates one NUCLEUS message per disk queue element
  (033341..EFQUE=036350, stride 37B) storing the id at **elem+13 = DMSID**
  (the id STRBDIO later hands to NKWRI/NKSEN). Sets DOMDF+15 := 1.
- **PDF.DRPRT writers: GPOOL @133343 (first connect), RGPOO @133701
  (reconnect, XBDTU path), RCPOO @134516 (mirror pool, XBDTV path)** -
  value = result of **DOPPR @136352 = MON 347 fn 3 open-port-by-NAME
  (T=3, DOMDF owner)**. Same routines write PDF.DIPOO(+17)/OPAIX(+21)/
  ARESZ(+23) doubles (values come from the connect exchange, layout [OPEN]).
- MON 347 wrapper family in 006-S3FS byte-mapped: CRPRT(fn1)/OPPRT(fn3)/
  CRMSG(fn6)/CLPRT(fn7) with T=1; DCRPR/DOPPR/DCRMS/DCLPR same fns with
  T=3 (on behalf of DOMDF); SNMSG(X=1)/RCMSG(X=2)/REMSG+WRMSG(X=3);
  GVACD @136056 allocates a vacant pool DF scanning device numbers
  **2260B-2277B** via LOGPH=010376 (claims by RSFLA:=1); INICO @134620 =
  CRPRT+OPPRT combo; DSHIN @135711 initializes the NKMBU(041152) header
  (+0:=0, +1:=30B, +2..7:=(0,CURPROG), +10/11:=-1, +16/17:=0).
- **DSVER+32..67 "static header" DISPROVEN**: those words are the generated
  zero tail of DOMDF + ADOML lock (041146, self-ptr at +2) + start of the
  NKMBU buffer, swept along because the 70B/76B NKWRI/NKREA windows exceed
  the 32B-word record content (fields end at DNRPG=DOMDF+51). They are
  DON'T CARE for the DIOC. The SCSI unit/LUN binding is carried by
  PDF.DRPRT (per-pool remote port from the name lookup) + DXPOO/OPAIN
  in each message - NOT by DSVER+32..67.
- On-disk (044-S3IDPIT=053-S3SDPIT, base 04000) DOMDF generated values:
  +2..3=(*-2,2), **+6=074246 REBDIO pre-planted at generation**; everything
  else zero incl. DLPRT/DSVER. Matches generation listing s3vs-4.symb
  4276-4289 (DOMDF/ADOML/NKMBU; pool DFs DOM01-20 devnos 2257+n, DRPRT
  generated 0) [NPL-V].
- **Poisoned prior: BDTMU=075326/BDTMV=075356 bodies are in the RPIT
  overlay (016-S3SRPIT=025-S3IRPIT), NOT MPIT** (MPIT bytes there are XMSG
  code). They only queue the pool DF into X1ARR/X2ARR and start RT program
  RECST @075225, which calls FILSYS(RGPOOL/RCPOOL) via XBDTU/XBDTV
  [V bytes + NPL-V listing 075224-075512].
- False-positive sweep: ALL other segment hits for words 041103/041104/
  041064/041136 are ASCII text ("BC"/"BD"/"B4") or unrelated words -
  checked 007-DMAC, 014-ERRP, 022-RFAC, 116/117-ERWD, 124/125-BOPC,
  130-CFT, 133, 135-XFTRAD, 136-FSASG, 137-COSPOOL, 140, 141, commoncode,
  rtloader, and the MPIT 056233 "DSVER+32" (an instruction, not a literal).
- QP100 element truth: queue-element list = BFQUE=033332 (+7 first elem
  033341) .. EFQUE=036350, stride **37B words** (not 100).

## Emulator consequences (feeds S3-1/S4-1)

- DIOC/NUCLEUS emulation must: answer create-port (local DLPRT) and
  create-message (DMSIDs); register one port per pool NAME and answer
  open-by-name (DOPPR) -> that port number becomes PDF.DRPRT; supply
  DIPOO/OPAIX/ARESZ in the connect answer; ignore BDIO request words rel
  +32..67; reply with DSSTS/DSQCN per REBDIO rules.
- Remaining blocking gap moved to **S0-3 (segment 105 server carve)** or
  S4-2 live capture: exact MON 347 request/answer layouts + connect-answer
  fields for DIPOO/OPAIX/ARESZ.
