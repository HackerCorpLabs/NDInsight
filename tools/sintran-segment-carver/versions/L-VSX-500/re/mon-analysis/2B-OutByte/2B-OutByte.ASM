; ============================================================================
; MON 2B (octal) OutByte (YFPUT) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version declared the call "misattributed"
; (GOTAB[2B]=000000 fall-through; worker OUTBT=032355B in commoncode). The
; 000000 was read out of SINTRAN-DATA_commoncode's FAKE GOTAB (its slot 0 is
; zero). Byte-proven: GOTAB[2B] = 071635B = M2 (a resident level-14 fast handler
; - MON 2B is one of the 32 GOTAB slots that are NOT MFELL), and MCTAB[2B] =
; 026600B = YFPUT, the filesystem byte-output worker, carved in 006-S3FS.
;
; Dispatch (see README):
;   Fast path:  MON 2B -> ENT14 072167B -> GOTAB[2B]=M2 071635B (level-14 handler)
;   File path:  MCTAB[2B] @005622B = 026600B = YFPUT -> worker (below)
;   The M2 -> YFPUT hand-off is INFERRED, not byte-proven.
;
; Worker home: 006-S3FS (load 26000B), symbol YFPUT=026600B (FILSYS-SYMBOLS).
; YFPUT (put) and YFGET (get, MON 1B) share one body from 026601B.
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[2B] slot (026-S3IMPIT, load 32000B) ----
071235  071635  	; GOTAB[2B] = M2 = 071635B (fast handler)   (dd: byteoff 32058 = 73 9d)

; ---- MCTAB[2B] slot (044-S3IDPIT, load 4000B) ----
005622  026600  	; MCTAB[2B] = YFPUT = 026600B   (dd: byteoff 1828 = 2d 80)

; ---- YFPUT worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (026600-026000)*2 = 768 -> 50 25 = 050045 (LDT 45).
; --- YFGET entry (MON 1B) sits just above at 026576B ---
026576  050046  	LDT 46			; YFGET: T := get selector (46B); JMP 2 -> 026601
026577  124002  	JMP 2			; -> 026601
026600  050045  	LDT 45			; YFPUT entry: T := put selector (45B); falls through
; --- shared put/get body ---
026601  012030  	STT ,X 30		; store selector into open-file control block
026602  135044  	JPL I 44		; -> 026646 (byte-transfer helper)
026603  170401  	SAA 1
026604  004421  	STA ,B 21		; status flag := 1
026605  044411  	LDA ,B 11
026606  000427  	STZ ,B 27		; clear byte counter
026607  051040  	LDT I 40		; reload control-block pointer
026610  142006  	SKP IF DT UEQ 0		; full? -> flush/error path
026611  124004  	JMP 4			; -> 026615
026612  044036  	LDA 36
026613  001036  	STZ I 36
026614  124025  	JMP 25			; -> 026641 (in-buffer fast return)
026615  135035  	JPL I 35		; -> 026652 (flush helper)
026616  131402  	JAF 2			; -> 026620
026617  146115  	RADD CLD SD DA
026620  131403  	JAF 3			; -> 026623
026621  170532  	SAA 132			; error code 132B
026622  124333  	JMP -45			; -> 026555 (shared error tail)
026623  146157  	RADD CLD SA DX
026624  046007  	LDA ,X 7
026625  175305  	BSKP ONE 100 DA
026626  124005  	JMP 5			; -> 026633
026627  046003  	LDA ,X 3
026630  175365  	BSKP ONE 160 DA
026631  124002  	JMP 2			; -> 026633
026632  124021  	JMP 21			; -> 026653
026633  046003  	LDA ,X 3
026634  175165  	BSKP ZRO 160 DA
026635  124003  	JMP 3			; -> 026640
026636  170533  	SAA 133			; error code 133B
026637  124316  	JMP -62			; -> 026555
026640  046004  	LDA ,X 4		; (body continues; see segments-ref) ...
