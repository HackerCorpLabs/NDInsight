; ============================================================================
; MON 1B (octal) InByte (YFGET) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version declared the call "misattributed
; (stub-routed)" (GOTAB[1B]=120303B -> F1607 stub; worker INBT=032471B in
; commoncode). The 120303B was read out of SINTRAN-DATA_commoncode's FAKE GOTAB.
; Byte-proven: GOTAB[1B] = 071633B = M1 (a resident level-14 fast handler - MON
; 1B is one of the 32 GOTAB slots that are NOT MFELL), and MCTAB[1B] = 026576B =
; YFGET, the filesystem byte-input worker, carved in 006-S3FS.
;
; Dispatch (see README):
;   Fast path:  MON 1B -> ENT14 072167B -> GOTAB[1B]=M1 071633B (level-14 handler)
;   File path:  MCTAB[1B] @005621B = 026576B = YFGET -> worker (below)
;   The M1 -> YFGET hand-off is INFERRED, not byte-proven.
;
; Worker home: 006-S3FS (load 26000B), symbol YFGET=026576B (FILSYS-SYMBOLS).
; YFGET (get) and YFPUT (put, MON 2B) share one body from 026601B.
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[1B] slot (026-S3IMPIT, load 32000B) ----
071234  071633  	; GOTAB[1B] = M1 = 071633B (fast handler)   (dd: byteoff 32056 = 73 9b)

; ---- MCTAB[1B] slot (044-S3IDPIT, load 4000B) ----
005621  026576  	; MCTAB[1B] = YFGET = 026576B   (dd: byteoff 1826 = 2d 7e)

; ---- YFGET worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (026576-026000)*2 = 764 -> 50 26 = 050046 (LDT 46).
026576  050046  	LDT 46			; YFGET entry: T := get selector (46B)
026577  124002  	JMP 2			; -> 026601 (shared body)
; --- YFPUT entry (MON 2B) ---
026600  050045  	LDT 45			; YFPUT: T := put selector (45B); falls through
; --- shared get/put body ---
026601  012030  	STT ,X 30		; store selector into open-file control block
026602  135044  	JPL I 44		; -> 026646 (byte-transfer helper)
026603  170401  	SAA 1
026604  004421  	STA ,B 21		; status flag := 1
026605  044411  	LDA ,B 11
026606  000427  	STZ ,B 27		; clear byte counter
026607  051040  	LDT I 40		; reload control-block pointer
026610  142006  	SKP IF DT UEQ 0		; empty? -> refill/error path
026611  124004  	JMP 4			; -> 026615
026612  044036  	LDA 36
026613  001036  	STZ I 36
026614  124025  	JMP 25			; -> 026641 (in-buffer fast return)
026615  135035  	JPL I 35		; -> 026652 (refill helper)
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
