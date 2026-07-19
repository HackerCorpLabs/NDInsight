; ============================================================================
; MON 200B (octal) XMSGFunction (XMSG) - real SINTRAN L disassembly
; The single entry point to the SINTRAN III XMSG inter-process message system.
; One MON 200 call performs one XMSG function; the T register carries the
; function code (an XF* value 0..47) with option bits OR-ed into its high byte,
; and the parameters ride in A/D/X. Programs on the same machine or across a
; COSMOS network communicate through this call.
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[200B] = 000000  -> FALL-THROUGH (no direct GOTAB handler word);
;   the level-14 handler is reached via the resident MFELL/CALLPROC path,
;   which is NOT present in any carved segment (uncarved bridge). The handler
;   then switches on the XF* function code in T (a GOSW / jump table).
;
;   The XMSG worker body is NOT isolated in the carve set:
;     - 2XMSG resolves to address 200B, but 14 unrelated symbols share that
;       exact address (a symbol-cram / relocation-data artifact, not code).
;     - MXMSG=75202B sits in a banked-overlay data cluster (block-queue
;       descriptors); its bytes differ between the 025 and 026 overlays.
;   So the function-code table and per-function bodies below are DOCUMENTED
;   (XMSG version-M constants + SINTRAN manuals), not byte-proven here.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- Region A: GOTAB[200] dispatch word (SINTRAN-DATA_commoncode, load 0) ----
; GOTAB base = 071233B ; entry 200 -> 071233B + 200B = 071433B (1 word).
; Byte-proven 000000 = a resident fall-through: there is no entry stub to
; disassemble on the dispatch head; dispatch enters MFELL/CALLPROC (uncarved).
071433  000000  	.WORD 000000		; GOTAB[200] = fall-through (VERIFIED)

; ---- Region B: the '2XMSG' symbol at 200B is a 14-way symbol-cram (NOT code) ----
; The symbol table maps 14 unrelated symbols to the single address 200B:
;   55MES 5EXTD AUPIN SEGFS CSPGS ER113 2XMSG REGBS CMHWF MNOBU LUNBL CBSIZ
;   SG20 LOADR   all = 200B
; This is a relocation/data artifact at a low address, not the XMSG routine.
; The word actually stored at 200B in commoncode is data, shown here for proof:
000200  000000  	.WORD 000000		; 2XMSG address = data artifact (MISATTRIBUTED)

; ---- Region C: 'MXMSG' at 75202B is a banked-overlay data cluster (UNVERIFIED) ----
; MXMSG=75202B lands among block-queue descriptors (BQ09I=75201, MXMSG=75202,
; CHDVB=75204, RECST=75225, BQ10O=75231, DRXAC=75243) - a data table, not a
; routine - and the bytes differ per overlay (paging-dependent). Neither overlay
; is the statically-reachable MON-200 worker. Shown as the 026-S3IMPIT view;
; every word here is UNCARVED-as-worker (data cluster), do not model as the body.
075201  000000  	.WORD 000000		; BQ09I (SYMBOL-2-LIST) - overlay data
075202  144045  	.WORD 144045		; MXMSG (SYMBOL-2-LIST) - overlay data (UNVERIFIED)
075203  004402  	.WORD 004402		; overlay data
075204  146145  	.WORD 146145		; CHDVB (SYMBOL-2-LIST) - overlay data
;
; No entry-stub region and no isolated worker exist for MON 200 in this carve.
; The functional model (T GOSW over XF* 0..47) is the DOCUMENTED XMSG dispatch;
; see the README function-code table and 200B-XMSGFunction.pseudo.c. The ND-500
; sibling handlers A5XMS/B5XMS (MON 512B/513B, in 026-S3IMPIT at 142253B) are the
; byte-verified reverse direction: they RE-ISSUE this MON 200 for an ND-500
; process (e.g. the MON 200 at 026-S3IMPIT:075214B is one such re-issue).
</content>
