; ============================================================================
; MON 143B (octal) RSIO / ExecutionInfo - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Earlier folders modelled MON dispatch through a fictional
; "GOTAB" monitor-call table (F1670 stub) and located the worker in
; SINTRAN-DATA_commoncode. Monitor calls are dispatched through MCTAB @005620B
; (segment 044-S3IDPIT). MCTAB[143B] = 051430B = RSIO, and that worker is carved
; in 003-S3CP (load 30000B) - the same overlay as its siblings GBGSZ / MBECH.
;
; Dispatch (all hops carved; see README):
;   MON 143B -> ENT14 072167B -> GOTAB[143B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[143B] @005763B = 051430B = RSIO -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol RSIO=051430B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[143B] slot (044-S3IDPIT, load 4000B) ----
005763  051430  	; MCTAB[143B] = RSIO = 051430B   (dd: byteoff 2022 = 53 18)

; ---- RSIO worker body (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (051430-030000)*2 = 17968 -> cc 59 = 146131.
051430  146131  	RADD CLD SB DD		; D := B (frame/param base)
051431  044021  	LDA 21
051432  146153  	RADD CLD SA DB		; B := A (switch to a work base)
051433  044675  	LDA ,B -103
051434  131404  	JAF 4			; -> 051440 (branch on flag)
051435  044631  	LDA ,B -147
051436  146156  	RADD CLD SA DT		; T := A (one info source)
051437  124005  	JMP 5			; -> 051444
051440  054632  	LDX ,B -146		; alternate source: chase a table
051441  052026  	LDT ,X 26
051442  056012  	LDX ,X 12
051443  046023  	LDA ,X 23
051444  054675  	LDX ,B -103
051445  146113  	RADD CLD SD DB		; B := D (restore param base)
051446  146151  	RADD CLD SA DD		; D := A (result out)
051447  146175  	RADD CLD SX DA
051450  055357  	LDX I -21
051451  146142  	EXIT			; return with the execution-info result
; 051452 = tail / GBGSZ boundary follows.
