; ============================================================================
; MON 62B (octal) GetBytesInFile - real SINTRAN L disassembly
; One region (the GOTAB slot is zero, so there is no per-call entry stub):
;   B. RMAX byte-count worker body   RMAX @103767B  in segment 006-S3FS (load 26000B)
;   (dispatch drops into the uncarved resident MFELL/CALLPROC path - not in these bytes)
; RMAX calls the deeper worker RMAXB @72016B (byte-proven via link cell 104033).
; The three indirect-link cells RMAX uses (104024/104030/104033) sit in the shared
; pointer block just past RMAX, between it and the next symbol REABT (104005B); they
; are data (worker addresses), not code.
; Addresses and values are octal.
; ============================================================================

; ---- Region B: RMAX byte-count worker (006-S3FS) ----
103767  021034  	STD I 34			; save caller D (parameter pointer)
103770  146145  	RADD CLD SL DA
103771  146131  	RADD CLD SB DD
103772  170006  	SAB 6			; build local frame B
103773  135031  	JPL I 31		; -> 104024 (link cell = 003752, prologue worker)
103774  011031  	STT I 31			; stash T (file number)
103775  135036  	JPL I 36		; -> 104033 (link cell = 072016, RMAXB worker)
103776  124005  	JMP 5		; -> 104003 (RMAXB no-skip = error path)
103777  020402  	STD ,B 2			; success: store double-word byte count -> caller B+2
104000  040404  	MIN ,B 4			; success: bump caller skip-return flag (B+4)
104001  170772  	SAA -6
104002  125026  	JMP I 26		; -> 104030 (link cell = 003776, resident return)
104003  004402  	STA ,B 2			; error: store error code A -> caller B+2
104004  124375  	JMP -3		; -> 104001

; ---- link cells RMAX references (data, in the shared pointer block; resolved) ----
; 104024  003752   = 003752  (prologue worker; same cell RDISK/RFILE use)
; 104030  003776   = 003776  (resident return cell)
; 104033  072016   = RMAXB   (72016B, the deeper byte-count worker)
