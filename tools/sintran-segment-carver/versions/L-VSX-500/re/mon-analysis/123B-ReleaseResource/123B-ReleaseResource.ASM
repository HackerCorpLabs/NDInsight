; ============================================================================
; MON 123B (octal) RELES / ReleaseResource - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Earlier folders modelled MON dispatch through a fictional
; "GOTAB" monitor-call table and declared the worker not-carved (it read RELES out
; of SINTRAN-DATA_commoncode, where 037156B is zero-filled - the WRONG overlay).
; Monitor calls are dispatched through MCTAB @005620B (segment 044-S3IDPIT).
; MCTAB[123B] = 037156B = RELES, and that worker IS carved - in 003-S3CP
; (load 30000B), immediately after its sibling RESRV (037103B).
;
; Dispatch (all hops carved; see README):
;   MON 123B -> ENT14 072167B -> GOTAB[123B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[123B] @005743B = 037156B = RELES -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol RELES=037156B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[123B] slot (044-S3IDPIT, load 4000B) ----
005743  037156  	; MCTAB[123B] = RELES = 037156B   (dd: byteoff 1990 = 3e 6e)

; ---- RELES worker body (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (037156-030000)*2 = 7388 -> cc 67 = 146147.
037156  146147  	RADD CLD SL DX		; X := L (save the return link)
037157  135010  	JPL I 10		; -> 037167 helper (shared release path)
037160  044640  	LDA ,B -140
037161  004637  	STA ,B -141		; stage the release argument
037162  146172  	RADD CLD SX DP		; P := X - computed return / tail jump
; --- 037163-037205 is a pointer/vector table (decoded as LDF I ,X words) ---
037163  007252  	; (data / pointer)
037164  002200  	; (data)
037165  004177  	; (data)
037166  075771  	; (data)
037167  004116  	; (data)  helper target of JPL I 10
037170  037206  	; ptr
037171  037213  	; ptr
037172  037220  	; ptr
037173  037224  	; ptr
037174  037230  	; ptr
037175  037233  	; ptr
; ... vector continues to 037205; the common release path is entered via JPL I 10.
