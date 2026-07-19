; ============================================================================
; MON 71B (octal) DisableEscape - real SINTRAN L disassembly
; Disables the terminal ESCAPE (user-break) function so the escape character is
; treated as an ordinary character. Re-enabled by MON 72 (EnableEscape) or by
; logging out.
;
; Two regions (see README Code location):
;   A. GOTAB[71] level-14 dispatch word  @071324B  in resident SINTRAN-DATA_commoncode (load 0)
;      = 121417B (NON-ZERO) -> points at the F1643 entry stub in overlay 025-S3IRPIT.
;   B. F1643 entry stub                   F1643 @121417B in segment 025-S3IRPIT (load 32000B)
;      3 words (121417-121421), bounded above by NOWTS=121422B. In THIS real
;      SINTRAN L image all three words are 000000: the stub is a set of
;      runtime-populated pointer cells, zero at rest, so the transfer onward to
;      the actual disable-escape routine is NOT byte-followable from a static
;      decode (see README caveats). The named ND-100 worker is not isolated in
;      any carved segment; the DESCF symbol resolves only to the ND-500 side.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region A: GOTAB[71] dispatch word (resident SINTRAN-DATA_commoncode) ----
; 071324B = GOTAB base 071233B + 071. Byte offset 58792 dec. Raw = a3 0f -> 121417B.
071324  121417  	; GOTAB[71] = 121417  (-> F1643 stub in 025-S3IRPIT)

; ---- Region B: F1643 entry stub (025-S3IRPIT), 121417B..121421B (3 words) ----
; Bounded below by F1643=121417B (SYMBOL-2-LIST), above by NOWTS=121422B.
; Every word is 000000 in this L image - the 3-word stub holds runtime-populated
; pointer cells (zero at rest), so there is no static instruction body to follow
; onward to the disable-escape worker. Treat each line as NOT-CARVED (absent body).
121417  000000  	.WORD 000000		; F1643 (runtime pointer cell; zero in this L image)
121420  000000  	.WORD 000000		; F1643+1 (zero in this L image)
121421  000000  	.WORD 000000		; F1643+2 (zero in this L image)
; 121422 = NOWTS boundary (next symbol); not part of the F1643 stub.
;
; The onward hop from F1643 to the real disable-escape routine goes through these
; runtime cells and the resident CALLPROC/segment-switch - NOT present in any
; carved segment - so the MON 71 -> worker link cannot be followed here.
