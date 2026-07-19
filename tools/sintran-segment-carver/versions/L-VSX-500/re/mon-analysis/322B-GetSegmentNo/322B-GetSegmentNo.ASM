; ============================================================================
; MON 322B (octal) GSGNO / GetSegmentNo - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Earlier folders modelled MON dispatch through a fictional
; "GOTAB" monitor-call table (DSI7 stub) and read GSGNO from
; SINTRAN-DATA_commoncode. Monitor calls are dispatched through MCTAB @005620B
; (segment 044-S3IDPIT). MCTAB[322B] = 041424B = GSGNO, and that worker IS carved
; in 003-S3CP (load 30000B), with siblings GRTDA / 1FU7.
;
; Dispatch (all hops carved; see README):
;   MON 322B -> ENT14 072167B -> GOTAB[322B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[322B] @006142B = 041424B = GSGNO -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol GSGNO=041424B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[322B] slot (044-S3IDPIT, load 4000B) ----
006142  041424  	; MCTAB[322B] = GSGNO = 041424B   (dd: byteoff 2244 = 43 14)

; ---- GSGNO worker body (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (041424-030000)*2 = 9768 -> ba 16 = 135026.
041424  135026  	JPL I 26		; -> 041452 helper (name lookup)
041425  150402  	ION			; interrupts back on
041426  044600  	LDA ,B -200		; GRTDA: load first argument word
041427  146151  	RADD CLD SA DD		; D := A
041430  044601  	LDA ,B -177
041431  135035  	JPL I 35		; -> 041466 helper
041432  050602  	LDT ,B -176
041433  135034  	JPL I 34		; -> 041467 helper
041434  125742  	JMP I ,B -36		; -> 041376 (error / not-found return)
041435  050601  	LDT ,B -177
041436  150401  	IOF			; interrupts off (critical section)
041437  054036  	LDX 36
041440  046001  	LDA ,X 1
041441  142065  	SKP IF DA UEQ ST
041442  124004  	JMP 4			; -> 041446
041443  054601  	LDX ,B -177
041444  046013  	LDA ,X 13
041445  131431  	JAF 31			; -> 041476
041446  150402  	ION			; interrupts back on
041447  125742  	JMP I ,B -36		; -> 041411 (normal return with seg number)
; 041450+ : local data / continuation of the lookup path.
