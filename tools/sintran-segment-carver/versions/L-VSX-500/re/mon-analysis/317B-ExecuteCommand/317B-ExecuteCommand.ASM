; ============================================================================
; MON 317B (octal) UECOM / ExecuteCommand - real SINTRAN L07 disassembly
;
; The dispatch is 4 layers and EVERY layer is byte-carved (see README):
;   A. ENT14  level-14 monitor entry      @072167B  026-S3IMPIT  (load 32000B)
;   B. GOTAB[317B] slot                   @071552B  026-S3IMPIT  -> MFELL
;   C. MFELL  fall-through + level switch @072114B  026-S3IMPIT  -> CALLP
;   D. MCTAB[317B] slot                   @006137B  044-S3IDPIT  (load  4000B) -> UECOM
;   E. UECOM  worker body                 @050701B  003-S3CP     (load 30000B)
;
; Addresses and values are octal.
; ============================================================================


; ---- Region A: ENT14, the level-14 monitor-call dispatcher (026-S3IMPIT) ----
; Entered on level 14 with T = the MON number (masked to 8 bits by 000377 @072266).
072167  044074  	LDA 74
072170  146153  	RADD CLD SA DB
072171  124005  	JMP 5			; -> 072176
072172  050072  	LDT 72
072173  146161  	RADD CLD ST DD
072174  151000  	WAIT
072175  146155  	RADD CLD SA DA
072176  150005  	TRA IIC
072177  142016  	SKP IF DT UEQ SD
072200  124077  	JMP 77			; -> 072277
072201  150003  	TRA PGS
072202  150001  	TRA STS
072203  135062  	JPL I 62		; -> 072265
072204  054062  	LDX 62
072205  144476  	RAND SX DT
072206  010607  	STT ,B -171		; save the MON number in the frame
072207  146151  	RADD CLD SA DD
072210  150004  	TRA PVL
072211  070056  	AND 56
072212  156575  	SHA ZIN SHR 3
072213  171006  	SAT 6
072214  141065  	SKP IF DA GRE ST
072215  124010  	JMP 10			; -> 072225
072216  171012  	SAT 12
072217  143065  	SKP IF DA LST ST
072220  124005  	JMP 5			; -> 072225
072221  004601  	STA ,B -177
072222  170401  	SAA 1
072223  004602  	STA ,B -176
072224  125044  	JMP I 44		; -> 072270
072225  146115  	RADD CLD SD DA
072226  050607  	LDT ,B -171
072227  055042  	LDX I 42
072230  133024  	JXZ 24			; -> 072254
072231  055041  	LDX I 41
072232  171377  	SAT -1
072233  142067  	SKP IF DX UEQ ST
072234  124004  	JMP 4			; -> 072240
072235  051036  	LDT I 36
072236  140067  	SKP IF DX EQL ST
072237  124014  	JMP 14			; -> 072253
072240  146154  	RADD CLD SA DL
072241  044607  	LDA ,B -171
072242  154401  	SHA 1
072243  061031  	ADD I 31
072244  146157  	RADD CLD SA DX
072245  051030  	LDT I 30
072246  143302  	LDDTX
072247  146401  	RADD AD1 0 DD
072250  147155  	RADD ADC CLD SA DA
072251  143306  	STDTX
072252  146145  	RADD CLD SL DA
;
; --- THE DISPATCH ---
072253  050607  	LDT ,B -171		; T := the MON number
072254  174000  	BSET ZRO SSPTM		; select the page-table for the table fetch
072255  146167  	RADD CLD ST DX		; X := MON number
072256  057020  	LDX I ,X 20		; X := MEM[ @072276 + X ] = MEM[ MGOTA + N ] = GOTAB[N]
072257  174200  	BSET ONE SSPTM		; restore the page-table
072260  126000  	JMP ,X 0		; JUMP straight to GOTAB[N].  No call, no CALLPROC.
;
; --- literal pool for region A (data words, not instructions) ---
072261  012210  	; (data)
072262  072126  	; (data)
072263  005055  	; (data)
072264  001000  	; (data)
072265  127743  	; (data)
072266  000377  	; MON-number mask (8 bits -> GOTAB is 256 words)
072267  000170  	; (data)
072276  071233  	; = MGOTA, the GOTAB base   <-- fetched by 072256


; ---- Region B: GOTAB[317B] (026-S3IMPIT) ----
; GOTAB base MGOTA = 071233B, one word per MON number, indexed directly by N.
; 071233B + 317B = 071552B.
071552  072114  	; GOTAB[317B] = MFELL  -> MON 317B is NOT a resident fast call


; ---- Region C: MFELL, the fall-through handler + level switch (026-S3IMPIT) ----
; 224 of the 256 GOTAB slots point here.  MFELL is NOT "illegal monitor call":
; it is the hand-off to the monitor level, where the full MON set is implemented.
072114  146165  	RADD CLD ST DA		; A := T = the MON number
072115  153427  	IRW 20 DX		; pass the MON number to the target level (its X)
072116  044043  	LDA 43			; A := @072161B = 032201B = CALLP
072117  153422  	IRW 20 DP		; set that level's P register := CALLP
072120  170404  	SAA 4
072121  150306  	MST PID			; activate the level
072122  150307  	MST PIE
072123  125014  	JMP I 14		; -> 072137
;
; --- literal pool for region C (data words) ---
072161  032201  	; = CALLP  <-- loaded by 072116.  This is the real "CALLPROC bridge":
;                       ; a program-LEVEL switch, not a subroutine call.
072162  032655  	; (second CALLP-family entry, used by the 072126 path)


; ---- Region D: MCTAB[317B] (044-S3IDPIT, load 4000B) ----
; MCTAB = 005620B (alias 9MCTA), the monitor-level Monitor-Call TABle:
; one word per MON number, indexed directly by N.  005620B + 317B = 006137B.
; This is the table CALLP dispatches through, and it is what MON 312B (MOINF) reads.
006132  032600  	; MCTAB[312B] = MOINF  (the capability-probe call itself)
006137  050701  	; MCTAB[317B] = UECOM  <-- MON 317B's worker address
006140  050726  	; MCTAB[320B] = UELOG


; ---- Region E: UECOM worker body (003-S3CP, load 30000B) ----
; Three sibling monitor calls share ONE command-execution body and are told apart
; only by a mode code written to frame slot ,B -175:
;   COMSB (MON 070B) mode 1    UECOM (MON 317B) mode 2 or 4    UELOG (MON 320B) mode 3
; This is why the manual says MON 70B COMND terminates the caller on error while
; MON 317B UECOM does not: same code, different mode.

; --- COMSB = 050673B (MON 070B) - sibling, shown for contrast ---
050673  146141  	RADD CLD SL DD		; D := L (save the return link)
050674  135741  	JPL I ,B -37		; common prologue
050675  004601  	STA ,B -177		; A (the caller's string pointer) -> frame
050676  170401  	SAA 1			; mode = 1  (COMSB)
050677  004603  	STA ,B -175
050700  124040  	JMP 40			; -> 050740, the shared body

; --- UECOM = 050701B (MON 317B) -- THE CALL ---
050701  146141  	RADD CLD SL DD		; D := L (save the return link)
050702  135741  	JPL I ,B -37		; common prologue
050703  031076  	STF I 76		; save F
050704  015076  	STX I 76		; save X
050705  004601  	STA ,B -177		; A = address of the command string -> frame
050706  010600  	STT ,B -200		; T -> frame
050707  146165  	RADD CLD ST DA		; A := T
050710  050073  	LDT 73			; T := @051003B (range limit)
050711  141065  	SKP IF DA GRE ST
050712  124006  	JMP 6			; -> 050720
050713  050071  	LDT 71			; T := @051004B (range limit)
050714  141056  	SKP IF DT GRE SA
050715  124003  	JMP 3			; -> 050720
050716  170404  	SAA 4			; mode = 4  (UECOM, in-range variant)
050717  124002  	JMP 2			; -> 050721
050720  170402  	SAA 2			; mode = 2  (UECOM, out-of-range variant)
050721  004603  	STA ,B -175		; store the mode code
050722  045063  	LDA I 63
050723  174265  	BSET ONE 60 DA
050724  005061  	STA I 61
050725  124014  	JMP 14			; -> 050741, the shared body

; --- UELOG = 050726B (MON 320B) - sibling, shown for contrast ---
050726  146141  	RADD CLD SL DD
050727  135741  	JPL I ,B -37
050730  031051  	STF I 51
050731  015051  	STX I 51
050732  004601  	STA ,B -177
050733  170403  	SAA 3			; mode = 3  (UELOG)
050734  004603  	STA ,B -175
050735  045050  	LDA I 50
050736  174265  	BSET ONE 60 DA
050737  005046  	STA I 46

; --- shared command-execution body (entered at 050740B / 050741B) ---
050740  010600  	STT ,B -200
050741  054045  	LDX 45
050742  046017  	LDA ,X 17
050743  146151  	RADD CLD SA DD
050744  050600  	LDT ,B -200
050745  054601  	LDX ,B -177		; X := the caller's command-string pointer
050746  135041  	JPL I 41		; -> the command decoder
050747  146107  	RADD CLD 0 DX
; character loop: upcase the command text before it reaches the decoder
050750  171146  	SAT 146
050751  141076  	SKP IF DT GRE SX
050752  124021  	JMP 21			; -> 050773
050753  050035  	LDT 35
050754  142200  	LBYT			; load one byte of the command string
050755  171141  	SAT 141			; 141B = 'a'
050756  141065  	SKP IF DA GRE ST
050757  124007  	JMP 7			; -> 050766
050760  171175  	SAT 175
050761  141056  	SKP IF DT GRE SA
050762  124004  	JMP 4			; -> 050766
050763  172740  	AAA -40			; 'a'..'z' -> upper case (subtract 40B)
050764  050024  	LDT 24
050765  142600  	SBYT			; store the upcased byte back
050766  171047  	SAT 47
050767  142065  	SKP IF DA UEQ ST
050770  124003  	JMP 3			; -> 050773
050771  173401  	AAX 1
