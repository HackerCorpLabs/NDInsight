; ============================================================================
; MON 273B (octal) GetFileName - real SINTRAN L disassembly
;
; MON 273B is a FALL-THROUGH call: GOTAB[273] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. MGFIL get-file-name worker body   MGFIL @111013B  in segment 006-S3FS (load 26000B)
;      MGFIL enters at 111013B (SSK=1). The sibling entry DEABF (@111015B, two
;      words later) enters the SAME joined body at 111016B with SSK=0; MGFIL
;      jumps over DEABF (111014 JMP 2 -> 111016) - the two-entry / SSK-fork
;      pattern. Joined body window 111013B..111207B = 125 words (up to but not
;      including FOBJN=111210B).
;      The body resolves a file name from directory/user/object indices: it
;      JPL-calls GFILN (@60600B) - the file-system Get-FILe-Name primitive -
;      which is the byte-level reason MGFIL is the GetFileName worker.
;   (the fall-through -> MGFIL bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; Words 111166B..111207B are a POINTER TABLE (link cells), not code; nd100-dis
; renders them as bogus ROP/STZ/STF/FAD instructions. Their contents are the
; real worker addresses (resolved in the trailing comments and README walkthrough).
; Addresses and values are octal.
; ============================================================================

; ---- Region B: MGFIL / DEABF get-file-name worker (006-S3FS, load 26000B) ----
111013  174220  	BSET ONE SSK		; MGFIL entry: set read/write selector SSK = 1
111014  124002  	JMP 2			; -> 111016 (jump over DEABF entry into joined body)
111015  174020  	BSET ZRO SSK		; DEABF entry (sibling): SSK = 0
111016  021150  	STD I 150		; joined body: stash caller D (params)
111017  146145  	RADD CLD SL DA
111020  146131  	RADD CLD SB DD
111021  170165  	SAB 165			; establish local frame (B), 165 words
111022  135145  	JPL I 145		; -> 111167 (link cell = 003752, resident prologue worker)
111023  175220  	BSKP ONE SSK		; fork on SSK (MGFIL vs DEABF)
111024  124004  	JMP 4			; -> 111030
111025  170401  	SAA 1
111026  004406  	STA ,B 6
111027  124023  	JMP 23			; -> 111052
111030  000406  	STZ ,B 6
111031  044137  	LDA 137
111032  146035  	RADD SB DA
111033  050136  	LDT 136
111034  135136  	JPL I 136		; -> 111172 (link cell = 031075, USCPS)
111035  124127  	JMP 127			; -> 111164 (store-status exit)
111036  146151  	RADD CLD SA DD
111037  146157  	RADD CLD SA DX
111040  044133  	LDA 133
111041  135133  	JPL I 133		; -> 111174 (link cell = 046231, FLPAR parameter parse)
111042  124122  	JMP 122			; -> 111164 (store-status exit)
111043  054401  	LDX ,B 1
111044  044131  	LDA 131
111045  146035  	RADD SB DA
111046  171004  	SAT 4
111047  135123  	JPL I 123		; -> 111172 (link cell = 031075, USCPS)
111050  124114  	JMP 114			; -> 111164 (store-status exit)
111051  135125  	JPL I 125		; -> 111176 (link cell = 030205, CPTYP file-type helper)
111052  044406  	LDA ,B 6
111053  131415  	JAF 15			; -> 111070
111054  054114  	LDX 114
111055  146037  	RADD SB DX
111056  044121  	LDA 121
111057  146035  	RADD SB DA
111060  146101  	RADD CLD 0 DD
111061  174201  	BSET ONE 0 DD
111062  050113  	LDT 113
111063  146036  	RADD SB DT
111064  135114  	JPL I 114		; -> 111200 (link cell = 061044, MDEAB)
111065  124065  	JMP 65			; -> 111152
111066  044402  	LDA ,B 2
111067  124053  	JMP 53			; -> 111142
111070  050401  	LDT ,B 1
111071  175376  	BSKP ONE 170 DT
111072  124040  	JMP 40			; -> 111132
111073  044075  	LDA 75
111074  146035  	RADD SB DA
111075  171105  	SAT 105
111076  054403  	LDX ,B 3
111077  135073  	JPL I 73		; -> 111172 (link cell = 031075, USCPS)
111100  124064  	JMP 64			; -> 111164 (store-status exit)
111101  146151  	RADD CLD SA DD
111102  146157  	RADD CLD SA DX
111103  044070  	LDA 70
111104  135070  	JPL I 70		; -> 111174 (link cell = 046231, FLPAR parameter parse)
111105  124057  	JMP 57			; -> 111164 (store-status exit)
111106  044401  	LDA ,B 1
111107  174175  	BSET ZRO 170 DA
111110  004407  	STA ,B 7
111111  044402  	LDA ,B 2
111112  004410  	STA ,B 10
111113  044066  	LDA 66
111114  146035  	RADD SB DA
111115  055065  	LDX I 65
111116  006024  	STA ,X 24
111117  044051  	LDA 51
111120  146035  	RADD SB DA
111121  006025  	STA ,X 25
111122  044055  	LDA 55
111123  146035  	RADD SB DA
111124  006026  	STA ,X 26
111125  171430  	SAX 30
111126  135055  	JPL I 55		; -> 111203 (link cell = 020274, resident helper)
111127  124035  	JMP 35			; -> 111164 (store-status exit)
111130  044400  	LDA ,B 0
111131  124011  	JMP 11			; -> 111142
111132  054402  	LDX ,B 2
111133  135051  	JPL I 51		; -> 111204 (link cell = 101303, CHDUO)
111134  124030  	JMP 30			; -> 111164 (store-status exit)
111135  054042  	LDX 42
111136  146037  	RADD SB DX
111137  135046  	JPL I 46		; -> 111205 (link cell = 060600, GFILN Get-FILe-Name primitive)
111140  124024  	JMP 24			; -> 111164 (store-status exit)
111141  044400  	LDA ,B 0
111142  171114  	SAT 114
111143  054034  	LDX 34
111144  146037  	RADD SB DX
111145  135041  	JPL I 41		; -> 111206 (link cell = 031072, SUCPS)
111146  124016  	JMP 16			; -> 111164 (store-status exit)
111147  040404  	MIN ,B 4
111150  170613  	SAA -165		; return leader
111151  125036  	JMP I 36		; -> 111207 (link cell = 003776, resident return)
111152  171111  	SAT 111
111153  140065  	SKP IF DA EQL ST
111154  124004  	JMP 4			; -> 111160
111155  146101  	RADD CLD 0 DD
111156  174211  	BSET ONE 10 DD
111157  124303  	JMP -75			; -> 111062 (loop)
111160  171106  	SAT 106
111161  140065  	SKP IF DA EQL ST
111162  124002  	JMP 2			; -> 111164
111163  170513  	SAA 113			; error 113
111164  004402  	STA ,B 2		; store status word -> caller slot B+2
111165  124363  	JMP -15			; -> 111150 (fall to resident return)
; ---- link cells (DATA, not code) 111166..111207 ----
111166  147417  	ROP NOOP		; (data) padding
111167  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
111170  000013  	STZ 13			; (data)
111171  000203  	STZ -175		; (data)
111172  031075  	STF I 75		; (data) link -> 031075 (USCPS)
111173  043465  	MIN I ,B ,X 65		; (data)
111174  046231  	LDA ,X -147		; (data) link -> 046231 (FLPAR parameter parse)
111175  000011  	STZ 11			; (data)
111176  030205  	STF -173		; (data) link -> 030205 (CPTYP file-type helper)
111177  000115  	STZ 115			; (data)
111200  061044  	ADD I 44		; (data) link -> 061044 (MDEAB)
111201  000007  	STZ 7			; (data)
111202  147421  	ROP NOOP		; (data)
111203  020274  	STD -104		; (data) link -> 020274 (resident helper, below segment base)
111204  101303  	FAD I -75		; (data) link -> 101303 (CHDUO)
111205  060600  	ADD ,B -200		; (data) link -> 060600 (GFILN Get-FILe-Name primitive)
111206  031072  	STF I 72		; (data) link -> 031072 (SUCPS)
111207  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
