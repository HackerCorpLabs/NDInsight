; ============================================================================
;  MICST  ->  MON 60 subfunction 025B  (MICRO START)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 025B = 0x15 = 21 dec.  Thunk 146415 (SAA 25; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <micro program start address>.
;  FOUR call sites resolve to thunk 146415 (verified via each pointer word).
; ============================================================================

; ---------------------------------------------------------------------------
;  Site A : 006307  -- CASE inside the command interpreter ENTER 002662
;           (framesize 000331=217).  Case body 006277-006311.
;           Shared error 002673 (via pool 006130), loop 010613 (via pool 006343).
; ---------------------------------------------------------------------------
006277  170400  	SAA 0
006300  135234  	JPL I -144		; -> ptr 006134 = 002003  eval one numeric operand
006301  135227  	JPL I -151		; helper error -> ptr 006130 = 002673
006302  020505  	STD ,B 105		; store operand -> local B+105
006303  146135  	RADD CLD SB DA		; A := B
006304  172505  	AAA 105			; A := B+105
006305  054602  	LDX ,B -176		; X := stack top
006306  006006  	STA ,X 6		; param1 := &(B+105) = <micro program start address>
006307  135041  	JPL I 41		; -> ptr 006350 = thunk 146415  MON60 MICST
006310  135220  	JPL I -160		; callsite+1 ERROR   -> ptr 006130 = 002673
006311  125032  	JMP I 32		; callsite+2 SUCCESS -> ptr 006343 = 010613 (command loop)
   ; pool: 006130=002673, 006343=010613, 006350=146415 (thunk MICST)

; ---------------------------------------------------------------------------
;  Site B : 130130  in standalone ENTER-routine 127551 (framesize 000010=8).
;           Error path is a frame dispatch (JMP I ,B -141); success = fall-through.
; ---------------------------------------------------------------------------
130125  044043  	LDA 43			; A := micro start address (P-relative constant)
130126  054602  	LDX ,B -176
130127  006006  	STA ,X 6		; param1 := <micro program start address> (by value)
130130  135042  	JPL I 42		; -> ptr 130172 = thunk 146415  MON60 MICST
130131  125637  	JMP I ,B -141		; callsite+1 ERROR   (frame dispatch)
130132  146135  	RADD CLD SB DA		; callsite+2 SUCCESS (fall-through, next op)

; ---------------------------------------------------------------------------
;  Site C : 130361  in the same ENTER-routine 127551 (framesize 000010=8).
; ---------------------------------------------------------------------------
130356  044102  	LDA 102			; A := micro start address (P-relative constant)
130357  054602  	LDX ,B -176
130360  006006  	STA ,X 6		; param1 := <micro program start address> (by value)
130361  135101  	JPL I 101		; -> ptr 130462 = thunk 146415  MON60 MICST
130362  125651  	JMP I ,B -127		; callsite+1 ERROR   (frame dispatch)
130363  034656  	LDF ,B -122		; callsite+2 SUCCESS (fall-through, next op)

; ---------------------------------------------------------------------------
;  Site D : 131140  in standalone ENTER-routine 130475 (framesize 000207=135).
;           Here the start address is passed by POINTER (&(B-66)).
; ---------------------------------------------------------------------------
131132  024171  	LDD 171			; D := constant (P-relative)
131133  020712  	STD ,B -66		; -> local B-66
131134  146135  	RADD CLD SB DA
131135  172712  	AAA -66			; A := B-66
131136  054602  	LDX ,B -176
131137  006006  	STA ,X 6		; param1 := &(B-66) = <micro program start address>
131140  135165  	JPL I 165		; -> ptr 131325 = thunk 146415  MON60 MICST
131141  135346  	JPL I -32		; callsite+1 ERROR   -> 131107
131142  024164  	LDD 164			; callsite+2 SUCCESS (fall-through, next op)
