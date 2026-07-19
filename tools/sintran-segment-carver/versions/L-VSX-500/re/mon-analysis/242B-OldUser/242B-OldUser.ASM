;=============================================================================
; MON 242B (octal) OldUser (RUSCN) - real SINTRAN L disassembly
;
; Switches back to the user name you were logged in under before NewUser (MON 241B).
; Your program continues under the old user. OldUser always resets the FIRST user
; name; it has no function if NewUser was not executed. All users; background programs.
;
; Dispatch is 3-layer (see README Code location). Two labelled regions:
;   A. GOTAB[242] level-14 entry stub  F1727 @066212B in segment 025-S3IRPIT (load 32000B)
;      Window 066212B..066215B = 4 words, bounded strictly to the next symbol
;      F2130=066216B. A tiny compiler entry stub sitting in a shared stub block; the
;      real transfer to the RUSCN worker is the resident CALLPROC (uncarved).
;   B. RUSCN switch-back worker body   RUSCN @106562B in segment 006-S3FS (load 26000B)
;      Window 106562B..106731B = 104 words, bounded strictly to the next symbol
;      FDFDI=106732B. Words 106562B..106701B are code; 106702B is a one-word ROP
;      NOOP pad; 106703B..106731B are a pointer table (link cells, DATA) that the
;      JPL I / JMP I indirections dereference.
;   (the A->B bridge is the uncarved resident CALLPROC - NOT present in these bytes;
;    region B IS real executable code; the A->B link is by symbol NAME.)
;
; RUSCN (Reset USer, Change Name) is a FILSYS-SYMBOLS symbol in the file-system
; segment 006-S3FS, directly after its twin SUSCN (NewUser, MON 241B).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- Region A: GOTAB[242] entry stub F1727 (025-S3IRPIT, load 32000B) ----
; F1727 = 066212B (SYMBOL-2-LIST); next symbol F2130 = 066216B, so the stub is 4 words.
; GOTAB[242] = 066212B (byte-proven). The 4 words land inside a shared stub block
; (neighbours F2130/F2131/MTOUS); they are not a self-contained named handler and the
; real transfer to the worker is the resident CALLPROC. A static decode cannot follow it.
066212  146075  	RADD SX DA	; A = A + X
066213  172777  	AAA -1		; A -= 1
066214  146151  	RADD CLD SA DD	; D = A  (copy)
066215  044051  	LDA 51		; A = mem[P+51]
; 066216 = F2130 boundary (next symbol); not part of this stub.

; ---- Region B: RUSCN switch-back (OldUser) worker (006-S3FS, load 26000B) ----
106562  021120  	STD I 120	; store A/D pair via [link cell] - save incoming
106563  146145  	RADD CLD SL DA	; A = L  (copy return link)
106564  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
106565  170012  	SAB 12		; B = 12
106566  135115  	JPL I 115	; -> [106703]  call resident prologue worker
106567  045115  	LDA I 115	; A = mem[[106704]]  (saved-name slot)
106570  171377  	SAT -1		; T = -1
106571  142065  	SKP IF DA UEQ ST	; skip if A != T  (unsigned)
106572  124076  	JMP 76		; -> 106670  (no NewUser done: error/no-op tail)
106573  045112  	LDA I 112	; A = mem[[106705]]
106574  142065  	SKP IF DA UEQ ST	; skip if A != T  (unsigned)
106575  124073  	JMP 73		; -> 106670
106576  135110  	JPL I 110	; -> [106706]  call resident worker
106577  135110  	JPL I 110	; -> [106707]  call resident worker
106600  124073  	JMP 73		; -> 106673  (error tail)
106601  045104  	LDA I 104	; A = mem[[106710]]
106602  156570  	SHA ZIN SHR 10	; A >>= 8  (logical right shift, zero fill)
106603  005105  	STA I 105	; mem[[106710]] = A
106604  045101  	LDA I 101	; A = mem[[106705]]
106605  070104  	AND 104		; A &= mem[P+104]
106606  005104  	STA I 104	; mem[[106710]] = A
106607  045104  	LDA I 104	; A = mem[[106713]]
106610  005074  	STA I 74	; mem[[106704]] = A  (restore first user name)
106611  005103  	STA I 103	; mem[[106714]] = A
106612  170777  	SAA -1		; A = -1
106613  005072  	STA I 72	; mem[[106705]] = A  (clear saved-name flag)
106614  005077  	STA I 77	; mem[[106713]] = A
106615  045100  	LDA I 100	; A = mem[[106715]]
106616  005100  	STA I 100	; mem[[106716]] = A
106617  005100  	STA I 100	; mem[[106717]] = A
106620  135100  	JPL I 100	; -> [106720]  call resident worker (re-enter old user)
106621  045063  	LDA I 63	; A = mem[[106704]]
106622  156570  	SHA ZIN SHR 10	; A >>= 8  (logical right shift)
106623  146156  	RADD CLD SA DT	; T = A  (copy)
106624  004406  	STA ,B 6	; mem[B+6] = A
106625  135074  	JPL I 74	; -> [106721]  call resident worker
106626  014411  	STX ,B 11	; mem[B+11] = X
106627  046003  	LDA ,X 3	; A = mem[X+3]
106630  135072  	JPL I 72	; -> [106722]  call resident worker
106631  045053  	LDA I 53	; A = mem[[106704]]
106632  070057  	AND 57		; A &= mem[P+57]
106633  004407  	STA ,B 7	; mem[B+7] = A
106634  135067  	JPL I 67	; -> [106723]  call resident worker
106635  124040  	JMP 40		; -> 106675  (error tail)
106636  046025  	LDA ,X 25	; A = mem[X+25]
106637  172401  	AAA 1		; A += 1
106640  070051  	AND 51		; A &= mem[P+51]
106641  146151  	RADD CLD SA DD	; D = A  (copy)
106642  046025  	LDA ,X 25	; A = mem[X+25]
106643  070061  	AND 61		; A &= mem[P+61]
106644  146015  	RADD SD DA	; A = A + D
106645  006025  	STA ,X 25	; mem[X+25] = A
106646  044057  	LDA 57		; A = mem[P+57]
106647  146075  	RADD SX DA	; A = A + X
106650  004410  	STA ,B 10	; mem[B+10] = A
106651  146107  	RADD CLD 0 DX	; X = 0  (clear)
106652  171010  	SAT 10		; T = 10  (loop count = 8 friend words)
106653  141076  	SKP IF DT GRE SX	; skip if (signed) T > X
106654  124005  	JMP 5		; -> 106661  (loop done)
106655  047410  	LDA I ,B ,X 10	; A = mem[[B+X+10]]  (copy friend table word)
106656  007050  	STA I ,X 50	; mem[[X+50]] = A
106657  173401  	AAX 1		; X += 1
106660  124372  	JMP -6		; -> 106652  (loop back)
106661  050406  	LDT ,B 6	; T = mem[B+6]
106662  044407  	LDA ,B 7	; A = mem[B+7]
106663  135044  	JPL I 44	; -> [106727]  call resident worker
106664  124011  	JMP 11		; -> 106675  (error tail)
106665  054411  	LDX ,B 11	; X = mem[B+11]
106666  046003  	LDA ,X 3	; A = mem[X+3]
106667  135041  	JPL I 41	; -> [106730]  call resident worker
106670  040404  	MIN ,B 4	; mem[B+4]++ ; skip next if it wraps to 0 (success advance)
106671  170766  	SAA -12		; A = -12  (error code)
106672  125037  	JMP I 37	; -> [106731]  indirect return
106673  004402  	STA ,B 2	; mem[B+2] = A  (error tail entry)
106674  124375  	JMP -3		; -> 106671
106675  004402  	STA ,B 2	; mem[B+2] = A  (error tail entry)
106676  054411  	LDX ,B 11	; X = mem[B+11]
106677  046003  	LDA ,X 3	; A = mem[X+3]
106700  135030  	JPL I 30	; -> [106730]  call resident worker (release)
106701  124370  	JMP -10		; -> 106671
106702  147417  	ROP NOOP	; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 106703..106731 ----
106703  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 115 at 106566)
106704  146274  	RADD CM1 SX DL	; constant/pointer cell
106705  146300  	RADD CM1 CLD 0 0	; constant/pointer cell
106706  101127  	FAD I 127	; pointer word (read by JPL I 110 at 106576)
106707  101201  	FAD I -177	; pointer word (read by JPL I 110 at 106577)
106710  146275  	RADD CM1 SX DA	; constant/pointer cell
106711  000377  	STZ -1		; constant/pointer cell
106712  146276  	RADD CM1 SX DT	; constant/pointer cell
106713  146277  	RADD CM1 SX DX	; constant/pointer cell
106714  144111  	SWAP CLD SD DD	; constant/pointer cell
106715  144134  	SWAP CLD SB DL	; constant/pointer cell
106716  144037  	SWAP SB DX	; constant/pointer cell
106717  144040  	SWAP SL 0	; constant/pointer cell
106720  101120  	FAD I 120	; pointer word (read by JPL I 100 at 106620)
106721  030225  	STF -153	; pointer word (read by JPL I 74 at 106625)
106722  010500  	STT ,B 100	; pointer word (read by JPL I 72 at 106630)
106723  053246  	LDT I ,X -132	; pointer word (read by JPL I 67 at 106634)
106724  177400  	BORC SSPTM	; constant/pointer cell
106725  000026  	STZ 26		; constant/pointer cell
106726  146314  	RADD CM1 CLD SD DL	; constant/pointer cell
106727  053410  	LDT I ,B ,X 10	; pointer word (read by JPL I 44 at 106663)
106730  010506  	STT ,B 106	; pointer word (read by JPL I 41/30 at 106667/106700)
106731  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 37 at 106672)
; 106732 = FDFDI boundary (GetDefaultDir worker, next symbol); not part of this worker.
