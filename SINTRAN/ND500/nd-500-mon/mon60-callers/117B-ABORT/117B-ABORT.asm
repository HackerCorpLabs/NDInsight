; ============================================================================
;  ABORT  ->  MON 60 subfunction ABORT = 117B (0x4F = 79 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct
;  return), callsite+2 = SUCCESS (skip return).
; ============================================================================
;  Enclosing routine : ENTER-routine 110333 (framesize 000013 = 11 dec).
;    110333 RADD AD1 CLD SL DX ; 110334 JPL I 24 -> ptr 110360 = 177300 ENTER ;
;    110335 000013 = inline framesize.  (ENTER pointer VERIFIED: bank1[110360]=177300.)
;  This ONE routine handles both ABORT (117B) and LOGOUT (122B): it selects on
;  the input flag at B-172 (JAZ at 110341).  flag != 0 -> ABORT ; flag == 0 ->
;  LOGOUT.  ABORT call site = 110346.
;  Thunk VERIFIED: bank1[110361]=146657 ; 146657 SAA 117 ; 146660 JMP I 1 ;
;  146661 146244 (gateway).

; ---- routine 110333 : combined ABORT/LOGOUT handler ----
110333  146547  	RADD AD1 CLD SL DX	; X := L+1 (save caller return link)
110334  135024  	JPL I 24		; -> ptr 110360 = 177300  ENTER
110335  000013  	<inline framesize = 000013>
110336  024607  	LDD ,B -171		; D := input arg (32-bit value) at B-171
110337  020611  	STD ,B -167		; local(B-167) := D   (copy of the value)
110340  044606  	LDA ,B -172		; A := input flag at B-172
110341  131010  	JAZ 10		; -> 110351  ; if flag==0 -> LOGOUT branch
; ---- ABORT branch (flag != 0) ----
110342  146135  	RADD CLD SB DA		; A := B
110343  172611  	AAA -167		; A := &local(B-167)
110344  054602  	LDX ,B -176		; X := stack top (gateway param base)
110345  006006  	STA ,X 6		; MON60 param1 := &local(B-167)
110346  135013  	JPL I 13		; -> ptr 110361 = thunk 146657  ABORT (117B)  *** MON 60 ***
110347  135013  	JPL I 13		; callsite+1 ERROR -> ptr 110362 = 177327 LEAVE(value)
110350  124007  	JMP 7		; callsite+2 SUCCESS -> 110357
; ---- LOGOUT branch (flag == 0) : see 122B-LOGOUT ----
110351  146135  	RADD CLD SB DA
110352  172611  	AAA -167
110353  054602  	LDX ,B -176
110354  006006  	STA ,X 6		; param1 := &local(B-167)
110355  135006  	JPL I 6		; -> ptr 110363 = thunk 146670  LOGOUT (122B)
110356  135004  	JPL I 4		; LOGOUT callsite+1 ERROR -> 110362 = 177327 LEAVE(value)
110357  135005  	JPL I 5		; SUCCESS (both) -> ptr 110364 = 177335 LEAVE-SKIP
; ---- local pointer pool (data; disassembles as nonsense) ----
110360  177300  	<ptr>  -> 177300  ENTER
110361  146657  	<ptr>  -> thunk 146657 (ABORT 117B)
110362  177327  	<ptr>  -> 177327  LEAVE(value)   (error return of routine 110333)
110363  146670  	<ptr>  -> thunk 146670 (LOGOUT 122B)
110364  177335  	<ptr>  -> 177335  LEAVE-SKIP     (success return of routine 110333)
