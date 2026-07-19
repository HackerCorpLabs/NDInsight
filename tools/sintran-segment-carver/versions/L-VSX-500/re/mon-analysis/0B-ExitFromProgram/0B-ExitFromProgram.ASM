; ============================================================================
; MON 0B (octal) ExitFromProgram (LEAVE) - real SINTRAN L disassembly
; Terminates the calling program and returns to SINTRAN III. A batch job
; continues with its next command. Background programs close all files that are
; not permanently open; RT programs close no files but release all reserved
; devices.
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[0B] = 000000  -> FALL-THROUGH (no direct GOTAB handler word); the
;   level-14 handler is reached via the resident MFELL/CALLPROC path, which is
;   NOT present in any carved segment (uncarved bridge).
;   Named region for this call: LEAVE @144142B in resident SINTRAN-DATA_commoncode
;   (load 0). In THIS real SINTRAN L image the word at LEAVE is ZERO: LEAVE sits
;   deep inside the large uncarved zero block 103031B..170177B, so the
;   program-termination worker body cannot be read from these bytes. LEAVE is
;   attached by symbol NAME only and its code-vs-data nature is NOT-RECOVERABLE
;   (see README caveats).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- GOTAB[0] dispatch word (SINTRAN-DATA_commoncode, load 0) ----
; GOTAB base = 071233B; GOTAB[0] is the base word itself.
071233  000000  	.WORD 000000		; GOTAB[0] = 000000 -> FALL-THROUGH (no stub)

; ---- LEAVE named region (SINTRAN-DATA_commoncode, load 0) ----
; LEAVE = 144142B (SYMBOL-1-LIST); previous symbol ENTER=144141B, next OUTTE=144143B,
; so the named slot is a single word (144142). It is 000000 in this L image -
; LEAVE lies inside the uncarved zero block 103031B..170177B, so there is no
; executable body to disassemble here. Treat the word as NOT-CARVED (absent).
144142  000000  	.WORD 000000		; NOT CARVED (zero-filled in this L image)
; 144143 = OUTTE boundary (next symbol); not part of this region.
;
; No entry-stub region exists: GOTAB[0] = 000000 is a resident fall-through,
; not a 025-S3IRPIT stub, so there is nothing to disassemble on the dispatch head.
; The ENTER/LEAVE/OUTTE symbols are consecutive single-word slots (144141/144142/
; 144143) - a vector-style region - all zero here because the range was not carved.
