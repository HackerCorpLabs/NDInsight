; ============================================================================
; MON 312B (octal) MOINF / CheckMonCall - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. This is the capability-probe call: "is monitor call N
; implemented, and what is its entry?" It is SPECIAL: unlike the ordinary MON
; workers (which live in 003-S3CP), MOINF is part of the monitor-dispatch
; machinery and lives WITH the dispatcher in 026-S3IMPIT / 017-S3SMPIT
; (load 32000B, identical bytes in both), right after CALLP=032201B. In the
; 003-S3CP overlay the same virtual address 032600 is an unrelated DATA record
; table (X103T..X107E), NOT this code - do not read MOINF there.
;
; MOINF READS MCTAB[N] directly and returns it: VERIFIED FROM BYTES (below).
;
; Dispatch (all hops carved; see README):
;   MON 312B -> ENT14 072167B -> GOTAB[312B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[312B] @006132B = 032600B = MOINF -> worker (below).
;
; Worker home: 026-S3IMPIT (load 32000B), symbol MOINF=032600B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[312B] slot (044-S3IDPIT, load 4000B) ----
006132  032600  	; MCTAB[312B] = MOINF = 032600B   (dd: byteoff 2228 = 35 80)

; ---- MOINF worker body (026-S3IMPIT, load 32000B) ----
; Entry byte dd-verified: byteoff (032600-032000)*2 = 768 -> d7 8d = 153615.
032600  153615  	IRR 10 DA		; A := caller's A register (the MON# argument)
032601  050035  	LDT 35			; T := mem[P+35] = mem[032636] = 000400B (=256, the bound)
032602  143465  	SKP IF DA MLST ST	; skip (in-range) if MON# < 256
032603  124004  	JMP 4			; -> 032607 (out of range)
032604  146157  	RADD CLD SA DX		; X := MON#
032605  047032  	LDA I ,X 32		; A := mem[ mem[P+32] + X ] = mem[ 005620B + MON# ]
;					;   = MCTAB[MON#]   (pointer word at P+32 = 032637 = 005620B)
032606  124002  	JMP 2			; -> 032610
032607  146105  	RADD CLD 0 DA		; out of range -> A := 0 (not implemented)
032610  153415  	IRW 10 DA		; write A (the MCTAB entry, or 0) back to caller's A
032611  131004  	JAZ 4			; -> 032615 if A==0 (NORMAL return: not implemented)
032612  153612  	IRR 10 DP		; A!=0: read caller's P (return address)
032613  172401  	AAA 1			; P := P+1  (SKIP RETURN: call implemented)
032614  153412  	IRW 10 DP		; write the skip-return P back to caller
032615  125006  	JMP I 6			; return to caller

; ---- MOINF local data block (the P-relative constants the code above reads) ----
; dd-verified: 032636 byteoff 828 = 01 00 (=000400B); 032637 byteoff 830 = 0b 90 (=005620B).
032636  000400  	; bound = 400B = 256 (number of MON-call slots); read by LDT 35 (P+35)
032637  005620  	; = MCTAB base (005620B); read by LDA I ,X 32 (pointer at P+32)
