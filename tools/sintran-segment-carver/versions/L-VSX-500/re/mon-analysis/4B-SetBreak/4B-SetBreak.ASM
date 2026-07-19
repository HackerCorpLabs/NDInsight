; ============================================================================
; MON 4B (octal) SetBreak (BRKM) - real SINTRAN L disassembly
; Sets the break characters for a terminal. A program normally waits for input;
; when a break character is typed the program restarts. Break strategy selects a
; predefined or user-defined 128-bit break table, with an optional max-character
; count. For RT programs a logical device number selects the terminal.
; Two regions (dispatch is 3-layer; see README Code location):
;   A. GOTAB[4] level-14 dispatch word  = 000000B  (FALL-THROUGH; no entry stub in 025-S3IRPIT)
;   B. BRKM named worker                BRKM @044425B in resident SINTRAN-DATA_commoncode (load 0)
;   (GOTAB[4]=0 means the call is dispatched via the resident MFELL / CALLPROC path - NOT
;    present in these bytes. Region B IS real executable code; the dispatch->worker link is
;    by symbol NAME - see README caveats.)
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region A: GOTAB[4] dispatch word (resident SINTRAN-DATA_commoncode, load 0) ----
; GOTAB[4] = 000000B at 071237B. A zero entry = FALL-THROUGH: there is no direct handler
; stub for MON 4 in overlay 025-S3IRPIT. Level-14 dispatch falls through the resident
; MFELL / CALLPROC machinery (uncarved) to reach the worker. No stub bytes to list.

; ---- Region B: BRKM named worker (SINTRAN-DATA_commoncode, load 0) ----
; BRKM = 044425B (SYMBOL-1-LIST). This region IS real executable code: I/O setup, indirect
; indexed loads, compare/skip tests, in-body loops, shared-helper calls and a
; return-through-frame. It is the SetBreak entry into the shared break/echo terminal-message
; module. CTRMA/PL007/CTR2M/PL008/REECE/REECT/TMMT1/SSSLE/SSLEA are internal labels inside
; this flow. It also branches/calls into shared cells past the carved window
; (JMP 150 -> 044613, JPL I 112 -> 044612, JPL I 73 -> 044606), so this window is the named
; SetBreak entry region, not a fully self-contained subroutine (see README caveats).
; Identity as the MON 4 worker rests on the symbol NAME (BRKM); the dispatch->worker link
; crosses the uncarved MFELL / CALLPROC.
044425  000200  	STZ -200
044426  161000  	IOT 1000
044427  146107  	RADD CLD 0 DX
044430  047741  	LDA I ,B ,X -37		; indirect indexed load
044431  171377  	SAT -1
044432  142065  	SKP IF DA UEQ ST
044433  124010  	JMP 10		; -> 044443  (DIRECT, in-body)
044434  173401  	AAX 1
044435  047741  	LDA I ,B ,X -37		; indirect indexed load
044436  135006  	JPL I 6		; -> deref [044444]  (shared helper call)
044437  135006  	JPL I 6		; -> deref [044445]  (shared helper call)
044440  135006  	JPL I 6		; -> deref [044446]  (shared helper call)  (CTRMA)
044441  173401  	AAX 1
044442  124366  	JMP -12		; -> 044430  (DIRECT, in-body loop)
044443  124150  	JMP 150		; -> 044613  (DIRECT, out of window -> shared PL009)  (PL007/CTR2M)
044444  060742  	ADD ,B -36		; pointer/constant cell (read by JPL I 6 at 044436)
044445  000215  	STZ -163		; pointer/constant cell (read by JPL I 6 at 044437)
044446  060665  	ADD ,B -113		; pointer/constant cell (read by JPL I 6 at 044440)
044447  131003  	JAZ 3		; -> 044452  (PL008)
044450  120734  	MPY ,B -44
044451  060564  	ADD ,B 164
044452  004736  	STA ,B -42
044453  146145  	RADD CLD SL DA
044454  004750  	STA ,B -30
044455  170772  	SAA -6
044456  004036  	STA 36		; (REECE)
044457  045125  	LDA I 125		; (REECT)
044460  146056  	RADD SA DT
044461  010735  	STT ,B -43
044462  146175  	RADD CLD SX DA
044463  120734  	MPY ,B -44
044464  004737  	STA ,B -41
044465  024732  	LDD ,B -46
044466  054730  	LDX ,B -50
044467  022025  	STD ,X 25		; (TMMT1)
044470  022015  	STD ,X 15
044471  044736  	LDA ,B -42
044472  146151  	RADD CLD SA DD
044473  146105  	RADD CLD 0 DA
044474  050735  	LDT ,B -43
044475  146174  	RADD CLD SX DL
044476  054737  	LDX ,B -41
044477  146143  	RADD CLD SL DB
044500  135112  	JPL I 112		; -> deref [044612]  (shared helper call)
044501  124006  	JMP 6		; -> 044507  (DIRECT, in-body)
044502  124376  	JMP -2		; -> 044500  (DIRECT, in-body wait loop)
044503  044102  	LDA 102
044504  146153  	RADD CLD SA DB
044505  150110  	TRR CCL
044506  125750  	JMP I ,B -30		; -> 044456  (return through frame pointer)
044507  044076  	LDA 76
044510  146153  	RADD CLD SA DB		; (SSSLE)
044511  040003  	MIN 3
044512  124353  	JMP -25		; -> 044465  (DIRECT, in-body loop)  (SSLEA)
044513  135073  	JPL I 73		; -> deref [044606]  (shared helper call)
044514  000000  	STZ 0
044515  120734  	MPY ,B -44
044516  060564  	ADD ,B 164
044517  004735  	STA ,B -43
044520  146165  	RADD CLD ST DA
044521  120734  	MPY ,B -44
044522  060564  	ADD ,B 164
044523  004736  	STA ,B -42
044524  146175  	RADD CLD SX DA
044525  120734  	MPY ,B -44
044526  004737  	STA ,B -41
044527  146145  	RADD CLD SL DA
044530  004750  	STA ,B -30
044531  170772  	SAA -6
044532  004362  	STA -16
044533  024732  	LDD ,B -46
044534  054730  	LDX ,B -50
044535  022015  	STD ,X 15
044536  022025  	STD ,X 25
044537  054737  	LDX ,B -41
; 044540 = ECHOM boundary (next peer worker symbol); not part of this entry region.
