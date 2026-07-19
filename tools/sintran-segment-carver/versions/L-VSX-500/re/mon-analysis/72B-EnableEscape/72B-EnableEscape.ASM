; ============================================================================
; MON 72B (octal) EnableEscape - real SINTRAN L disassembly
; Re-enables the terminal ESCAPE (user-break) function after MON 71
; (DisableEscape). The ESCAPE key normally terminates a program (a user break);
; the function is also enabled automatically at log-out.
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[72] = 000000  -> FALL-THROUGH (no direct GOTAB handler word);
;   the level-14 handler is reached via the resident MFELL/CALLPROC path,
;   which is NOT present in any carved segment (uncarved bridge).
;   The named ND-100 worker is not isolated in any carved segment: the EESCF
;   short name resolves only to the ND-500 side (026-S3IMPIT / 030-S3SM5,
;   112123B, N500-SYMBOLS), which is not the ND-100 body. So there is no
;   ND-100 worker region to disassemble here (see README caveats).
;   MON 72 (enable) and MON 71 (disable) are a pair that likely share one
;   terminal-flag body forked on a selector - inferred, not proven.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- GOTAB[72] dispatch word (resident SINTRAN-DATA_commoncode, load 0) ----
; 071325B = GOTAB base 071233B + 072. Byte offset 58794 dec. Raw = 00 00.
071325  000000  	; GOTAB[72] = 000000  (FALL-THROUGH; no direct handler word)
;
; No entry-stub region exists: GOTAB[72] = 000000 is a resident fall-through,
; not a 025-S3IRPIT stub, so there is nothing to disassemble on the dispatch head.
; No ND-100 enable-escape worker is isolated in the carved set (EESCF is ND-500
; only), so there is no worker body region to disassemble either.
