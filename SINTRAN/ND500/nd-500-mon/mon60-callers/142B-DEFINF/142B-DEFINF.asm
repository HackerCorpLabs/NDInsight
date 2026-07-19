; ============================================================================
;  DEFINF  ->  MON 60 subfunction DEFINF = 142B (0x62 = 98 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Purpose (SUBFUNCTION-TABLE.md row 142, dispatch 5NOPAR): redefine default
;  infant file.  Six call sites; every one marshals exactly ONE parameter
;  (slot 6) = a pooled word loaded P-relative.
;  Thunk VERIFIED (all six point to it):
;    146745 SAA 142 ; 146746 JMP I 1 ; 146747 146244 (gateway).
;  Group A (003072=146745) : inside command interpreter ENTER-routine 002662.
;  Group B (030404=146745) : inside ENTER-routine 030302 (framesize 000004).
;    (030302 RADD AD1 CLD SL DX ; 030303 JPL I 77 -> ptr 030402=177300 ENTER)

; ============================================================================
;  GROUP A - inside command interpreter 002662 (framesize 000331)
;  002715..002724 is a local error/retry handler (target of JPL -73/-100/-111/
;  -120 from below) that itself issues DEFINF.
; ============================================================================
; ---- call site 002723 ----
002715  146147  	RADD CLD SL DX		; local handler entry: X := L+1
002716  014476  	STX ,B 76
002717  004605  	STA ,B -173
002720  044151  	LDA 151			; A := pooled word (P-rel EA = 003071)
002721  054602  	LDX ,B -176
002722  006006  	STA ,X 6		; MON60 param1 := pooled word @003071
002723  135147  	JPL I 147		; -> ptr 003072 = thunk 146745  DEFINF (142B)  *** MON 60 ***
002724  135147  	JPL I 147		; callsite+1 ERROR -> ptr 003073 = 177327 LEAVE(value)
002725  044605  	LDA ,B -173		; callsite+2 SUCCESS (continues)
; ---- call site 003014 ----
003011  044075  	LDA 75			; A := pooled word (P-rel EA = 003106)
003012  054602  	LDX ,B -176
003013  006006  	STA ,X 6		; MON60 param1 := pooled word @003106
003014  135056  	JPL I 56		; -> ptr 003072 = thunk 146745  DEFINF (142B)  *** MON 60 ***
003015  134300  	JPL -100		; callsite+1 ERROR -> 002715 (local handler)
003016  054602  	LDX ,B -176		; callsite+2 SUCCESS (continues)
; ---- call site 003034 ----
003031  044040  	LDA 40			; A := pooled word (P-rel EA = 003071)
003032  054602  	LDX ,B -176
003033  006006  	STA ,X 6		; MON60 param1 := pooled word @003071
003034  135036  	JPL I 36		; -> ptr 003072 = thunk 146745  DEFINF (142B)  *** MON 60 ***
003035  134260  	JPL -120		; callsite+1 ERROR -> 002715 (local handler)
003036  170401  	SAA 1			; callsite+2 SUCCESS (continues)
; ---- Group A pointer pool words (data) ----
003072  146745  	<ptr>  -> thunk 146745 (DEFINF 142B)
003073  177327  	<ptr>  -> 177327  LEAVE(value)

; ============================================================================
;  GROUP B - inside ENTER-routine 030302 (framesize 000004)
;  030306..030310 is a local error/retry handler (target of JPL -56/-63/-65/-72).
; ============================================================================
; ---- call site 030314 ----
030311  044072  	LDA 72			; A := pooled word (P-rel EA = 030403)
030312  054602  	LDX ,B -176
030313  006006  	STA ,X 6		; MON60 param1 := pooled word @030403
030314  135070  	JPL I 70		; -> ptr 030404 = thunk 146745  DEFINF (142B)  *** MON 60 ***
030315  135070  	JPL I 70		; callsite+1 ERROR -> ptr 030405 = 177327 LEAVE(value)
030316  034606  	LDF ,B -172		; callsite+2 SUCCESS (continues)
; ---- call site 030370 ----
030365  044032  	LDA 32			; A := pooled word (P-rel EA = 030417)
030366  054602  	LDX ,B -176
030367  006006  	STA ,X 6		; MON60 param1 := pooled word @030417
030370  135014  	JPL I 14		; -> ptr 030404 = thunk 146745  DEFINF (142B)  *** MON 60 ***
030371  134315  	JPL -63		; callsite+1 ERROR -> 030306 (local handler)
030372  135026  	JPL I 26		; callsite+2 SUCCESS -> ptr 030420
; ---- call site 030377 ----
030374  044007  	LDA 7			; A := pooled word (P-rel EA = 030403)
030375  054602  	LDX ,B -176
030376  006006  	STA ,X 6		; MON60 param1 := pooled word @030403
030377  135005  	JPL I 5		; -> ptr 030404 = thunk 146745  DEFINF (142B)  *** MON 60 ***
030400  134306  	JPL -72		; callsite+1 ERROR -> 030306 (local handler)
030401  135020  	JPL I 20		; callsite+2 SUCCESS -> ptr 030421
; ---- Group B pointer pool words (data) ----
030404  146745  	<ptr>  -> thunk 146745 (DEFINF 142B)
030405  177327  	<ptr>  -> 177327  LEAVE(value)
