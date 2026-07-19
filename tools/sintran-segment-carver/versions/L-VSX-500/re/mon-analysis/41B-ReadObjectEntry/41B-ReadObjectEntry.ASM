; ============================================================================
; MON 41B (octal) ReadObjectEntry - real SINTRAN L disassembly
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[41] level-14 entry  F1627 @121023B  in segment 025-S3IRPIT (load 32000B)
;   B. ROBJE read-object-entry worker  ROBJE @55566B  in segment 006-S3FS (load 26000B)
;   (the A->B bridge is the uncarved resident CALLPROC - not in these bytes)
;
; Region A is bounded strictly to the next symbol BISIZ=121033B (8 words). The
; eight words are a VECTOR / LINK-CELL block, not executable code - nd100-dis
; renders them as bogus ADD/FDV/STZ/STA instructions. The identical words appear
; as explicitly-labelled link cells in the F1656 stub of MON 117B, confirming the
; pattern. The GOTAB vector points here; the transfer to the ROBJE worker is the
; uncarved resident CALLPROC hop (see README Honest caveats).
;
; Region B: words 55726B..55747B are a POINTER TABLE (link cells), not code;
; nd100-dis renders them as bogus ROP/STZ/LDX/... instructions. Their contents are
; the real worker addresses (resolved in trailing comments). ROBJE walks the file
; object-entry structure via FOBJB / GDDRT / RINDX / RBLOC / RELBU.
; Addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[41] vector block  F1627 (025-S3IRPIT, load 32000B) ----
;      8 words of vector / link-cell DATA (renders as bogus instructions)
121023  061160  	ADD I 160		; (data) vector / link cell
121024  116234  	FDV ,X -144		; (data) vector / link cell
121025  061157  	ADD I 157		; (data) vector / link cell
121026  000377  	STZ -1			; (data) vector / link cell
121027  116610  	FDV ,X ,B -170		; (data) vector / link cell
121030  116036  	FDV ,X 36		; (data) vector / link cell
121031  004007  	STA 7			; (data) vector / link cell
121032  117107  	FDV I ,X 107		; (data) vector / link cell (BISIZ=121033 follows)

; ---- Region B: ROBJE read-object-entry worker (006-S3FS, load 26000B) ----
055566  021140  	STD I 140		; ROBJE entry: stash caller D (file number / buffer ptr)
055567  146145  	RADD CLD SL DA
055570  146131  	RADD CLD SB DD
055571  170010  	SAB 10			; establish local frame (B), 10B words
055572  135135  	JPL I 135		; -> 055727 (link cell = 003752, resident prologue worker)
055573  135135  	JPL I 135		; -> 055730 (link cell = 055563, FOBJB find-object-block)
055574  014407  	STX ,B 7
055575  046000  	LDA ,X 0
055576  135133  	JPL I 133		; -> 055731 (link cell = 010500, resident worker)
055577  046001  	LDA ,X 1
055600  050401  	LDT ,B 1
055601  142065  	SKP IF DA UEQ ST
055602  124011  	JMP 11		; -> 055613
055603  170777  	SAA -1
055604  006001  	STA ,X 1
055605  044125  	LDA 125
055606  146057  	RADD SA DX
055607  024402  	LDD ,B 2
055610  156770  	SAD ZIN SHR 10
055611  156603  	SAD ZIN 3
055612  124026  	JMP 26		; -> 055640
055613  046002  	LDA ,X 2
055614  050402  	LDT ,B 2
055615  140065  	SKP IF DA EQL ST
055616  124004  	JMP 4		; -> 055622
055617  046003  	LDA ,X 3
055620  142015  	SKP IF DA UEQ SD
055621  124064  	JMP 64		; -> 055705
055622  171377  	SAT -1
055623  012001  	STT ,X 1
055624  044106  	LDA 106
055625  060407  	ADD ,B 7
055626  146157  	RADD CLD SA DX
055627  024402  	LDD ,B 2
055630  156770  	SAD ZIN SHR 10
055631  156603  	SAD ZIN 3
055632  052000  	LDT ,X 0
055633  140065  	SKP IF DA EQL ST
055634  124004  	JMP 4		; -> 055640
055635  052001  	LDT ,X 1
055636  142061  	SKP IF DD UEQ ST
055637  124014  	JMP 14		; -> 055653
055640  022000  	STD ,X 0
055641  050401  	LDT ,B 1
055642  054071  	LDX 71
055643  135071  	JPL I 71		; -> 055734 (link cell = 050121, GDDRT get-directory)
055644  124055  	JMP 55		; -> 055721
055645  054407  	LDX ,B 7
055646  050064  	LDT 64
055647  146067  	RADD ST DX
055650  050401  	LDT ,B 1
055651  135064  	JPL I 64		; -> 055735 (link cell = 051453, RINDX read-index)
055652  124047  	JMP 47		; -> 055721
055653  044403  	LDA ,B 3
055654  156574  	SHA ZIN SHR 4
055655  070061  	AND 61
055656  060061  	ADD 61
055657  060407  	ADD ,B 7
055660  146157  	RADD CLD SA DX
055661  026000  	LDD ,X 0
055662  131403  	JAF 3		; -> 055665
055663  142001  	SKP IF DD UEQ 0
055664  124032  	JMP 32		; -> 055716
055665  050401  	LDT ,B 1
055666  070052  	AND 52
055667  135052  	JPL I 52		; -> 055741 (link cell = 035531, RBLOC read-block)
055670  124031  	JMP 31		; -> 055721
055671  014406  	STX ,B 6
055672  146157  	RADD CLD SA DX
055673  044403  	LDA ,B 3
055674  070046  	AND 46
055675  156405  	SHA ZIN 5
055676  146057  	RADD SA DX
055677  044407  	LDA ,B 7
055700  060043  	ADD 43
055701  171040  	SAT 40
055702  135042  	JPL I 42		; -> 055744 (link cell = 001224, resident worker)
055703  054406  	LDX ,B 6
055704  135041  	JPL I 41		; -> 055745 (link cell = 035476, RELBU release-buffer)
055705  040404  	MIN ,B 4
055706  054407  	LDX ,B 7
055707  014400  	STX ,B 0
055710  044401  	LDA ,B 1
055711  006001  	STA ,X 1
055712  024402  	LDD ,B 2
055713  022002  	STD ,X 2
055714  170770  	SAA -10			; error / status leader
055715  125031  	JMP I 31		; -> 055746 (link cell = 003776, resident return)
055716  054407  	LDX ,B 7
055717  002026  	STZ ,X 26
055720  124365  	JMP -13		; -> 055705
055721  004402  	STA ,B 2			; store status word -> caller slot B+2
055722  054407  	LDX ,B 7
055723  046000  	LDA ,X 0
055724  135023  	JPL I 23		; -> 055747 (link cell = 010506, resident worker)
055725  124367  	JMP -11		; -> 055714 (funnel to resident return)
; ---- link cells (DATA, not code) 55726..55747 ----
055726  147417  	ROP NOOP		; (data) padding
055727  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
055730  055563  	LDX I ,B 163		; (data) link -> 055563 (FOBJB, in-segment)
055731  010500  	STT ,B 100		; (data) link -> 010500 (resident worker)
055732  000004  	STZ 4			; (data)
055733  000015  	STZ 15			; (data)
055734  050121  	LDT 121			; (data) link -> 050121 (GDDRT)
055735  051453  	LDT I ,B 53		; (data) link -> 051453 (RINDX)
055736  000016  	STZ 16			; (data)
055737  000006  	STZ 6			; (data)
055740  000377  	STZ -1			; (data)
055741  035531  	LDF I ,B 131		; (data) link -> 035531 (RBLOC)
055742  000037  	STZ 37			; (data)
055743  000026  	STZ 26			; (data)
055744  001224  	STZ I -154		; (data) link -> 001224 (resident worker)
055745  035476  	LDF I ,B 76		; (data) link -> 035476 (RELBU)
055746  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
055747  010506  	STT ,B 106		; (data) link -> 010506 (resident worker)
