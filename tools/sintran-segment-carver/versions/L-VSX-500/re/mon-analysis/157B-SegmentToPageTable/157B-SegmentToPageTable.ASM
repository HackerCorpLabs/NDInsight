; ============================================================================
; MON 157B (octal) SegmentToPageTable (ENTSG) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version of this folder used the WRONG
; dispatch model: it read a "GOTAB" out of SINTRAN-DATA_commoncode, got
; GOTAB[157]=122506B -> an F1676 "entry stub", and declared the ENTSG worker
; "misattributed" (reached only across an uncarved CALLPROC bridge). That whole
; GOTAB-in-commoncode table is fiction: its slots do not match the known GOTAB
; entries (slot 0 must be MFELL 072114B; commoncode has 000000 there).
;
; MON calls are NOT dispatched by that GOTAB. The real monitor-call table is
; MCTAB @ 005620B (segment 044-S3IDPIT, load 4000B). MCTAB[N] gives the true
; worker, indexed directly by N. MCTAB[157B] = 067764B = ENTSG - the SAME
; worker the old folder found by name, now proven directly from the table slot.
;
; Dispatch (all hops carved; see README):
;   MON 157B -> ENT14 072167B -> GOTAB[157B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[157B] @005777B = 067764B = ENTSG -> worker (below).
;
; Worker home: 025-S3IRPIT (load 32000B), symbol ENTSG=067764B (SYMBOL-2-LIST).
; The identical symbol ENTSG=067764B in resident commoncode is ZERO-filled - the
; old analysis read that zero overlay and wrongly concluded "absent"; the real
; ENTSG code lives here in 025-S3IRPIT. Region B below is an EXCERPT of a longer
; routine (it continues past the shown window before returning). Addresses and
; values are octal.
; ============================================================================

; ---- MCTAB[157B] slot (044-S3IDPIT, load 4000B) ----
; MCTAB base 005620B + 157 = 005777B. dd byteoff (05777-04000)*2 = 2046 -> 6f f4 = 067764B.
005777  067764  	; MCTAB[157B] = ENTSG = 067764B

; ---- ENTSG worker body EXCERPT (025-S3IRPIT, load 32000B) --------------------
; ENTSG @067764B. Entry byte dd-verified: byteoff (067764-032000)*2 = 30696 -> ba 54 = 135124B.
; Reads the param block (LDA ,B 20/21/22/23 = SegmentNo/PageTable/InterruptLevel/
; StartAddress), range-checks, indexes the segment table (MPY 120), then walks
; page-table entries with physical transfers (LDDTX/LDXTX/LDATX = 24-bit PHYSICAL,
; bypassing the MMU). Continues past 070107.
067764  135124  	JPL I 124		; -> 070110  (in-overlay subroutine)
067765  045124  	LDA I 124		; A = mem[ mem[070111B] ]
067766  171030  	SAT 30			; T := 30B
067767  143065  	SKP IF DA LST ST	; skip if A < T (range check)
067770  125122  	JMP I 122		; -> 070112  (error/exit vector)
067771  044420  	LDA ,B 20		; A = SegmentNo (param block +20)
067772  051121  	LDT I 121		; T = mem[ mem[070113B] ]
067773  141456  	SKP IF DT MGRE SA	; skip if T >= A (unsigned)
067774  125120  	JMP I 120		; -> 070114  (error/exit vector)
067775  120120  	MPY 120			; A *= mem[067775+120]  (segment-table entry stride)
067776  061120  	ADD I 120		; A += mem[ mem[070116B] ]  (segment-table base)
067777  004424  	STA ,B 24		; save segment-table pointer to +24
070000  044421  	LDA ,B 21		; A = PageTable (param block +21)
070001  171006  	SAT 6			; T := 6
070002  142065  	SKP IF DA UEQ ST	; skip if A != 6
070003  124007  	JMP 7			; -> 070012
070004  171007  	SAT 7			; T := 7
070005  143056  	SKP IF DT LST SA	; skip if T < A
070006  124003  	JMP 3			; -> 070011
070007  171017  	SAT 17			; T := 17B  (VSX K page-table range clamp)
070010  141456  	SKP IF DT MGRE SA	; skip if T >= A
070011  125106  	JMP I 106		; -> 070117  (error/exit vector)
070012  051106  	LDT I 106		; T = mem[ mem[070120B] ]
070013  054424  	LDX ,B 24		; X = segment-table pointer
070014  143360  	LDATX 6			; A = phys[EL]  (EL = high8=(T&0377), low16=(X+disp); 24-bit PHYSICAL, MMU bypassed)
070015  175215  	BSKP ONE 10 DA		; skip if bit set
070016  125103  	JMP I 103		; -> 070121  (error/exit vector)
070017  044422  	LDA ,B 22		; A = InterruptLevel (param block +22)
070020  171005  	SAT 5			; T := 5
070021  141065  	SKP IF DA GRE ST	; skip if A >= T
070022  124003  	JMP 3			; -> 070025
070023  171011  	SAT 11			; T := 11B
070024  141056  	SKP IF DT GRE SA	; skip if T >= A  (level in 6,7,10B,11B)
070025  125075  	JMP I 75		; -> 070122  (error/exit vector)
070026  054075  	LDX 75			; X = mem[070123B]
070027  146037  	RADD SB DX		; X = X + B
070030  044420  	LDA ,B 20		; A = SegmentNo
070031  006000  	STA ,X 0		; mem[X] = SegmentNo
070032  044421  	LDA ,B 21		; A = PageTable
070033  154410  	SHA 10			; A <<= 10B
070034  060422  	ADD ,B 22		; A += InterruptLevel
070035  006001  	STA ,X 1		; mem[X+1] = (PageTable<<8 | level)
070036  044423  	LDA ,B 23		; A = StartAddress (param block +23)
070037  006002  	STA ,X 2		; mem[X+2] = StartAddress
070040  051064  	LDT I 64		; T = mem[ mem[070124B] ]  (page-table bank/base)
070041  045064  	LDA I 64		; A = mem[ mem[070125B] ]
070042  146154  	RADD CLD SA DL		; L = A
070043  055063  	LDX I 63		; X = mem[ mem[070126B] ]
070044  143302  	LDDTX			; A = phys[EL]; D = phys[EL+1]  (PHYSICAL page-table pair)
070045  064420  	SUB ,B 20		; A -= SegmentNo
070046  131407  	JAF 7			; -> 070055 (if A != 0)
070047  156370  	SHD ZIN SHR 10		; D >>= 10B (zero fill)
070050  146115  	RADD CLD SD DA		; A = D
070051  064421  	SUB ,B 21		; A -= PageTable
070052  131403  	JAF 3			; -> 070055
070053  024426  	LDD ,B 26		; A = mem[B+26]; D = mem[B+27]
070054  125053  	JMP I 53		; -> 070127  (exit vector; already-entered)
070055  173403  	AAX 3			; X += 3  (next segment-table slot)
070056  143447  	SKP IF DX MLST SL	; skip if X < L (unsigned) - more slots
070057  124002  	JMP 2			; -> 070061
070060  124364  	JMP -14			; -> 070044  (scan loop)
070061  044421  	LDA ,B 21		; A = PageTable
070062  154407  	SHA 7			; A <<= 7
070063  060045  	ADD 45			; A += mem[070130B]  (page-table base for that PT)
070064  146154  	RADD CLD SA DL		; L = A  (page-table walk limit/base)
070065  051033  	LDT I 33		; T = mem[ mem[070131B] ]  (page bank)
070066  054424  	LDX ,B 24		; X = segment-table pointer
070067  143371  	LDXTX 7			; X = phys[EL]  (PHYSICAL)
070070  051041  	LDT I 41		; T = mem[ mem[070132B] ]
070071  133016  	JXZ 16			; -> 070107 (if X == 0)
070072  143330  	LDATX 3			; A = phys[EL]  (PHYSICAL page-table entry)
070073  146171  	RADD CLD SX DD		; D = X
070074  070036  	AND 36			; A &= mem[070133B]  (page-frame mask)
070075  154401  	SHA 1			; A <<= 1
070076  145445  	RORA SL DA		; A = A | L  (OR in page-table select)
070077  146157  	RADD CLD SA DX		; X = A
070100  146106  	RADD CLD 0 DT		; T := 0
070101  143300  	LDATX			; A = phys[EL]  (read PT entry)
070102  131471  	JAF 71			; -> 070173 (if A != 0)
070103  146117  	RADD CLD SD DX		; X = D
070104  051025  	LDT I 25		; T = mem[ mem[070131B] area ]
070105  143301  	LDXTX			; X = phys[EL]  (PHYSICAL)
070106  124363  	JMP -15			; -> 070071  (page-walk loop)
070107  124024  	JMP 24			; -> 070133  (continues; excerpt ends here)
; ---- ENTSG continues past 070107B (page-table entry commit + EXIT) ----------
; ---- end of excerpt ----
