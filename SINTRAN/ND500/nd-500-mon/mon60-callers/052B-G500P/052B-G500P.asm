; ============================================================================
;  052B-G500P  ->  MON 60 subfunction G500P = 52B (0x2A = 42 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146517  170452  SAA 52      (subfunction 52B)
;    146520  125001  JMP I 1     (-> gateway pointer)
;    146521  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): (give ND-500 pages)
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- GIVE-N500-PAGES, MON 60 G500P (52B) at 010232 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 010222 SAA 0 ; 010223 JPL I 54 -> routine 002003 (numeric-arg evaluator) -> D
; 010224 JPL I -161 -> 007500 if the evaluator fails
; 010225 STD ,B 105 ; 010226 RADD SB DA ; 010227 AAA 105 ; 010231 STA ,X 6 = &(B+105)
; 010232 JPL I 55 -> ptr 010307 = thunk 146517 = SAA 52
010222  170400  SAA 0
010223  135054  JPL I 54		; -> 010277
010224  135217  JPL I -161		; -> 010043
010225  020505  STD ,B 105
010226  146135  RADD CLD SB DA
010227  172505  AAA 105
010230  054602  LDX ,B -176
010231  006006  STA ,X 6
010232  135055  JPL I 55		; -> 010307
010233  135210  JPL I -170		; -> 010043
010234  125215  JMP I -163		; -> 010051
