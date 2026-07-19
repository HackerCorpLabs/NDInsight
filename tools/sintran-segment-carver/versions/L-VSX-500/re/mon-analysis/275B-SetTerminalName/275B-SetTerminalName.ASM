;=============================================================================
; MON 275B (octal) SetTerminalName (STRFI) - real SINTRAN L disassembly
;
; Defines the file name used for terminals (normally 'TERMINAL:'). Background users
; identify their own terminal with this file name; the call may be issued repeatedly.
;
; Dispatch reality (see README Code location):
;   GOTAB[275B] = 000000  -> FALL-THROUGH (no direct GOTAB handler word); the level-14
;   handler is reached via the resident MFELL/CALLPROC path, which is NOT present in any
;   carved segment (uncarved bridge).
;   NO worker region is isolated for this call: the manual short name STRFI has NO
;   matching symbol in any carved segment (commoncode, 006-S3FS, 025-S3IRPIT), and the
;   only near name STERM=041333B is a DATA variable (its words are 000000/000035, not
;   code). So the set-terminal-name worker body cannot be read from these carved bytes.
;   SetTerminalName is a sibling of SetPeripheralName (MON 234B, worker SPEFI in
;   006-S3FS); a shared or adjacent peripheral/terminal-file body is INFERRED but is
;   not present in these bytes (see README Honest caveats).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
;=============================================================================

; ---- No carved worker region ----
; GOTAB[275B] = 000000 is a resident fall-through, so there is no 025-S3IRPIT entry
; stub to disassemble on the dispatch head. The worker (short name STRFI) is not
; located in any carved segment - there is no executable body to list here. The
; documented behaviour is modelled in 275B-SetTerminalName.pseudo.c only.
;
; STERM = 041333B (SYMBOL-1-LIST) - the nearest matching name - is DATA, not code:
041333  000000  	.WORD 000000	; STERM: data variable (not the worker)
041334  000035  	.WORD 000035	; data variable
; 041335 = 1FU4 boundary (next symbol); not part of any worker for this call.
