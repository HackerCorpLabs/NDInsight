; ============================================================================
;  LOGOUT  ->  MON 60 subfunction LOGOUT = 122B (0x52 = 82 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR,
;  callsite+2 = SUCCESS (skip return).
; ============================================================================
;  Enclosing routine : ENTER-routine 110333 (framesize 000013 = 11 dec).
;  ONE routine handles both ABORT (117B) and LOGOUT (122B): it selects on the
;  input flag at B-172 (JAZ at 110341).  flag == 0 -> LOGOUT.  LOGOUT call site
;  = 110355.  (ABORT path is documented in 117B-ABORT.)
;  Thunk VERIFIED: bank1[110363]=146670 ; 146670 SAA 122 ; 146671 JMP I 1 ;
;  146672 146244 (gateway).

; ---- routine 110333 : combined ABORT/LOGOUT handler ----
110333  146547  	RADD AD1 CLD SL DX	; X := L+1
110334  135024  	JPL I 24		; -> ptr 110360 = 177300  ENTER
110335  000013  	<inline framesize = 000013>
110336  024607  	LDD ,B -171		; D := input arg (32-bit value) at B-171
110337  020611  	STD ,B -167		; local(B-167) := D
110340  044606  	LDA ,B -172		; A := input flag at B-172
110341  131010  	JAZ 10		; -> 110351  ; flag==0 -> LOGOUT branch
; ---- ABORT branch (flag != 0) : see 117B-ABORT ----
110342  146135  	RADD CLD SB DA
110343  172611  	AAA -167
110344  054602  	LDX ,B -176
110345  006006  	STA ,X 6
110346  135013  	JPL I 13		; -> thunk 146657 ABORT (117B)
110347  135013  	JPL I 13		; ABORT err -> 110362 = 177327
110350  124007  	JMP 7		; -> 110357
; ---- LOGOUT branch (flag == 0) ----
110351  146135  	RADD CLD SB DA		; A := B
110352  172611  	AAA -167		; A := &local(B-167)
110353  054602  	LDX ,B -176		; X := stack top (gateway param base)
110354  006006  	STA ,X 6		; MON60 param1 := &local(B-167)
110355  135006  	JPL I 6		; -> ptr 110363 = thunk 146670  LOGOUT (122B)  *** MON 60 ***
110356  135004  	JPL I 4		; callsite+1 ERROR -> ptr 110362 = 177327 LEAVE(value)
110357  135005  	JPL I 5		; callsite+2 SUCCESS -> ptr 110364 = 177335 LEAVE-SKIP
; ---- local pointer pool (data) ----
110360  177300  	<ptr>  -> 177300  ENTER
110361  146657  	<ptr>  -> thunk 146657 (ABORT 117B)
110362  177327  	<ptr>  -> 177327  LEAVE(value)   (error return)
110363  146670  	<ptr>  -> thunk 146670 (LOGOUT 122B)
110364  177335  	<ptr>  -> 177335  LEAVE-SKIP     (success return)
