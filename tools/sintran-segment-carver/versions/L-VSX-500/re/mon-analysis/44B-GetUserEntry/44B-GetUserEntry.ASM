;=============================================================================
; MON 44B (octal) GetUserEntry (GUSEN) - real SINTRAN L disassembly
;
; Gets the user entry of a user (name, default file accesses, pages in use,
; password, table of friends, and more). Only user RT and user SYSTEM may read
; the user entries of other users. Available on the ND-100 and ND-500 to all
; programs.
;
; MON 44B is a FALL-THROUGH call: GOTAB[44] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   GUSEN get-user-entry worker body   GUSEN @055111B in segment 006-S3FS (load 26000B)
;      Window 055111B..055205B = 61 words, bounded strictly by the next routine
;      prologue at 055206B (STD I 47 ...). Words 055111B..055173B are code;
;      055174B is a one-word ROP NOOP pad; 055175B..055205B are a pointer table
;      (link cells, DATA) that the JPL I / JMP I indirections dereference.
;   (the fall-through -> GUSEN bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; GUSEN has a two-entry idiom: GUSEN=055111B clears the SSK skip flag, the sibling
; NGUSN=055113B sets it; both join the common body at 055114B and the flag selects
; the returned status word (055121B BSKP ONE SSK). GUSEN is a FILSYS-SYMBOLS symbol
; in the file-system segment 006-S3FS.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- GUSEN get-user-entry worker (006-S3FS, load 26000B) ----
055111  174020  	BSET ZRO SSK	; GUSEN entry: clear skip flag (SSK = 0)
055112  124002  	JMP 2		; -> 055114
055113  174220  	BSET ONE SSK	; NGUSN alt entry: set skip flag (SSK = 1)
055114  021060  	STD I 60	; store A/D pair via [link cell] - save incoming
055115  146145  	RADD CLD SL DA	; A = L  (copy return link)
055116  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
055117  170007  	SAB 7		; B = 7
055120  135055  	JPL I 55	; -> [055175]  call resident prologue worker
055121  175220  	BSKP ONE SSK	; skip next if SSK set (which entry called)
055122  124004  	JMP 4		; -> 055126
055123  170401  	SAA 1		; A = 1
055124  004406  	STA ,B 6	; mem[B+6] = 1  (NGUSN-form status flag)
055125  124002  	JMP 2		; -> 055127
055126  000406  	STZ ,B 6	; mem[B+6] = 0  (GUSEN-form status flag)
055127  054047  	LDX 47		; X = mem[P+47]
055130  135047  	JPL I 47	; -> [055177]  call resident worker
055131  124041  	JMP 41		; -> 055172  (error tail)
055132  175175  	BSKP ZRO 170 DA	; skip if bit 15 of A clear
055133  124003  	JMP 3		; -> 055136
055134  170426  	SAA 26		; A = 26   (error code)
055135  124035  	JMP 35		; -> 055172  (error tail)
055136  054406  	LDX ,B 6	; X = mem[B+6]
055137  140007  	SKP IF DX EQL 0	; skip if X == 0
055140  124005  	JMP 5		; -> 055145
055141  175335  	BSKP ONE 130 DA	; skip if selected bit of A set
055142  124003  	JMP 3		; -> 055145
055143  170547  	SAA 147		; A = 147  (error code)
055144  124026  	JMP 26		; -> 055172  (error tail)
055145  050401  	LDT ,B 1	; T = mem[B+1]
055146  044402  	LDA ,B 2	; A = mem[B+2]
055147  135031  	JPL I 31	; -> [055200]  call resident worker (find user block)
055150  124022  	JMP 22		; -> 055172  (error tail)
055151  046025  	LDA ,X 25	; A = mem[X+25]
055152  175375  	BSKP ONE 170 DA	; skip if bit 15 of A set
055153  124010  	JMP 10		; -> 055163
055154  044025  	LDA 25		; A = mem[P+25]
055155  146075  	RADD SX DA	; A = A + X
055156  146157  	RADD CLD SA DX	; X = A  (copy)
055157  044400  	LDA ,B 0	; A = mem[B+0]
055160  171020  	SAT 20		; T = 20  (transfer count / access flag)
055161  135021  	JPL I 21	; -> [055202]  call resident worker (copy entry out)
055162  124004  	JMP 4		; -> 055166
055163  044020  	LDA 20		; A = mem[P+20]
055164  054400  	LDX ,B 0	; X = mem[B+0]
055165  006000  	STA ,X 0	; mem[X+0] = A
055166  135016  	JPL I 16	; -> [055204]  call resident worker
055167  040404  	MIN ,B 4	; mem[B+4]++ ; skip next if it wraps to 0 (success advance)
055170  170771  	SAA -7		; A = -7   (error code)
055171  125014  	JMP I 14	; -> [055205]  indirect return
055172  004402  	STA ,B 2	; mem[B+2] = A  (error tail: store error code)
055173  124375  	JMP -3		; -> 055170
055174  147417  	ROP NOOP	; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 055175..055205 ----
055175  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 55 at 055120)
055176  000000  	STZ 0		; constant/pointer cell
055177  050124  	LDT 124		; pointer word (read by JPL I 47 at 055130)
055200  053246  	LDT I ,X -132	; pointer word (read by JPL I 31 at 055147)
055201  000026  	STZ 26		; constant/pointer cell
055202  030104  	STF 104		; pointer word (read by JPL I 21 at 055161)
055203  037447  	LDF I ,B ,X 47	; constant/pointer cell
055204  053721  	LDT I ,B ,X -57	; pointer word (read by JPL I 16 at 055166)
055205  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 14 at 055171)
; 055206 = next routine prologue (STD I 47 ...); not part of this worker.
