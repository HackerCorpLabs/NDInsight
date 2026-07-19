; ============================================================================
;  051B-RIFRG  ->  MON 60 subfunction RIFRG = 51B (0x29 = 41 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146514  170451  SAA 51      (subfunction 51B)
;    146515  125001  JMP I 1     (-> gateway pointer)
;    146516  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): READ INTERFACE (COMMUNICATION), IODATUT REGISTER
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- RIFRG call site at 130136 ----
; enclosing ENTER routine 127551 (framesize 000010) - a standalone ENTER routine
; 130132 RADD SB DA ; 130133 AAA -167 ; 130134 LDX ,B -176 ; 130135 STA ,X 6 = &(B-167)
; 130136 JPL I 35 -> ptr 130173 = thunk 146514 = SAA 51
; 130137 callsite+1 = JMP I ,B -141 (dynamic error exit)
130132  146135  RADD CLD SB DA
130133  172611  AAA -167
130134  054602  LDX ,B -176
130135  006006  STA ,X 6
130136  135035  JPL I 35		; -> 130173
130137  125637  JMP I ,B -141		; -> 127776
