; ============================================================================
; MON 142B (octal) ToErrorDevice - real SINTRAN L disassembly
; Outputs a user-defined real-time error (numbers 50-69) on the error device
; (normally the console), e.g.:
;   23.10.59 ERROR 59 AT XPROG AT 134562, USER ERROR, SUBERROR 4
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[142] = 000000  -> FALL-THROUGH (no direct GOTAB handler word);
;   the level-14 handler is reached via the resident MFELL/CALLPROC path,
;   which is NOT present in any carved segment (uncarved bridge).
;   The named ND-100 worker is not isolated in any carved segment: the ERMON
;   short name resolves only to the ND-500 side (026-S3IMPIT / 030-S3SM5,
;   114574B, N500-SYMBOLS / 1WQ1O SYMBOL-2-LIST alias), which is not the ND-100
;   body. So there is no ND-100 worker region to disassemble here (see README).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- GOTAB[142] dispatch word (resident SINTRAN-DATA_commoncode, load 0) ----
; 071375B = GOTAB base 071233B + 142. Byte offset 58874 dec. Raw = 00 00.
071375  000000  	; GOTAB[142] = 000000  (FALL-THROUGH; no direct handler word)
;
; No entry-stub region exists: GOTAB[142] = 000000 is a resident fall-through,
; not a 025-S3IRPIT stub, so there is nothing to disassemble on the dispatch head.
; No ND-100 to-error-device worker is isolated in the carved set (ERMON is ND-500
; only), so there is no worker body region to disassemble either.
