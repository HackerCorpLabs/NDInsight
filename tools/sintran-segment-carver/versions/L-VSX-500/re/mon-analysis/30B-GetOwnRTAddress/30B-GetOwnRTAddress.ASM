; ============================================================================
; MON 30B (octal) GetOwnRTAddress (GETRT) - real SINTRAN L disassembly
; Returns the address of the CALLING program's RT description. Background
; programs get the RT-description address of the RT program controlling the
; terminal. The result is returned in W1 (the A register on the ND-100 side;
; MAC example: MON 30 / STA RTPRO).
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[30B] = 000000  -> FALL-THROUGH (no direct GOTAB handler word); the
;   level-14 handler is reached via the resident MFELL/CALLPROC path, which is
;   NOT present in any carved segment (uncarved bridge).
;
;   There is NO ND-100 code worker, and NO ND-100 named region, for this call in
;   the carved segments. The manual short name GETRT resolves ONLY in L07
;   N500-SYMBOLS (GETRT=106704B) - the ND-500-side companion, not the ND-100
;   body. So there is nothing to disassemble on the ND-100 side except the proof
;   that GOTAB[30] is a zero (fall-through). The real worker is tiny: it reads
;   the current RT-description pointer and returns it in A - but that read lives
;   past the uncarved MFELL/CALLPROC bridge and cannot be read from these bytes.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- No ND-100 region to disassemble ----
; GOTAB[30] = 000000 is a resident fall-through, not a 025-S3IRPIT stub, so there
; is no entry-stub code. No ND-100 symbol names an RT-address worker body here
; (GETRT=106704B is the ND-500 companion in N500-SYMBOLS). The only byte fact is
; the GOTAB zero, shown as the one data word below.
; ---- GOTAB[30] dispatch word (SINTRAN-DATA_commoncode, load 0) ----
071263  000000  	.WORD 000000		; GOTAB[30] = 000000 -> FALL-THROUGH (no direct handler)
;
; The returned value is the caller's RT-description address, taken from the
; running RT program's descriptor by the resident handler (uncarved). See the
; README caveats.
