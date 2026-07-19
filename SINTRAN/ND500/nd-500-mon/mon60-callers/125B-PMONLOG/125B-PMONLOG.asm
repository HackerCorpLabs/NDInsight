; ============================================================================
;  PMONLOG  ->  MON 60 subfunction PMONLOG = 125B (0x55 = 85 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  READ MONCALL LOG DATA (PRINT MONCALL LOG).  Enclosing routine :
;  ENTER-routine 111217 (framesize 001411 = 777 dec).
;    (111217 RADD AD1 CLD SL DX ; 111220 JPL I 166 -> ptr 111406=177300 ENTER)
;  This routine is also reached from the interpreter case at 007315 (JPL I 70
;  -> ptr 007405 = 111217).  Call site = 111232.
;  Thunk VERIFIED: bank1[111410]=146701 ; 146701 SAA 125 ; 146702 JMP I 1 ;
;  146703 146244 (gateway).

; ---- PMONLOG call sequence (111222..111234) ----
111222  146135  	RADD CLD SB DA		; A := B
111223  172615  	AAA -163		; A := &local(B-163)
111224  004606  	STA ,B -172		; local(B-172) := &local(B-163)  (pointer stash)
111225  044162  	LDA 162			; A := pooled word (P-rel EA = 111407)
111226  144151  	SWAP CLD SA DD		; D := old A (pooled word) ; A := old D
111227  050606  	LDT ,B -172		; T := local(B-172) = &local(B-163)
111230  054602  	LDX ,B -176		; X := gateway param base
111231  032006  	STF ,X 6		; MON60 param1 := F (T,A,D = 3 words)
111232  135156  	JPL I 156		; -> ptr 111410 = thunk 146701  PMONLOG (125B)  *** MON 60 ***
111233  135156  	JPL I 156		; callsite+1 ERROR -> ptr 111411 = 177327 LEAVE(value)
111234  000607  	STZ ,B -171		; callsite+2 SUCCESS (continues in-line)
; ---- relevant pointer pool words (data) ----
111410  146701  	<ptr>  -> thunk 146701 (PMONLOG 125B)
111411  177327  	<ptr>  -> 177327  LEAVE(value)  (error return of routine 111217)
