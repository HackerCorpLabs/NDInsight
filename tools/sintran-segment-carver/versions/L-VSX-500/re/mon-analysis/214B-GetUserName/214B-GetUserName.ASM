;=============================================================================
; MON 214B (octal) GetUserName (GUSNA) - real SINTRAN L disassembly
;
; Returns the name of a user (16 chars) given a directory index and a user
; index. With the COSMOS network installed the user may be on a remote computer,
; in which case the remote system name is also returned. RT programs return the
; name of user RT.
;
; MON 214B is a FALL-THROUGH call: GOTAB[214] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   GUSNA get-user-name worker body   GUSNA @105301B in segment 006-S3FS (load 26000B)
;      Window 105301B..105431B = 89 words, bounded strictly to the next symbol
;      GUIOI=105432B. Words 105301B..105407B are code; 105410B is a one-word ROP
;      NOOP pad; 105411B..105431B are a pointer table (link cells, DATA) that the
;      JPL I / JMP I indirections dereference.
;   (the fall-through -> GUSNA bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; GUSNA is a FILSYS-SYMBOLS symbol in the file-system segment 006-S3FS.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- GUSNA get-user-name worker (006-S3FS, load 26000B) ----
105301  021107  	STD I 107		; store A/D pair via [105410's neighbour] - save incoming
105302  146145  	RADD CLD SL DA	; A = L  (copy return link)
105303  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
105304  170063  	SAB 63		; B = 63
105305  135104  	JPL I 104		; -> [105411]  call resident prologue worker
105306  044400  	LDA ,B 0		; A = mem[B+0]
105307  130036  	JAP 36		; -> 105345  if A>=0 take remote/alt path
; ---- local-user path (105310-105344) ----
105310  044102  	LDA 102		; A = mem[105412]
105311  146035  	RADD SB DA		; A = A + B
105312  171105  	SAT 105		; T = 105
105313  054401  	LDX ,B 1		; X = mem[B+1]
105314  135077  	JPL I 77		; -> [105413]  call resident worker
105315  124071  	JMP 71		; -> 105406  (error/exit tail)
105316  146151  	RADD CLD SA DD	; D = A  (copy)
105317  146157  	RADD CLD SA DX	; X = A  (copy)
105320  044074  	LDA 74		; A = mem[105414]
105321  135074  	JPL I 74		; -> [105415]  call resident worker
105322  124064  	JMP 64		; -> 105406  (error/exit tail)
105323  044400  	LDA ,B 0		; A = mem[B+0]
105324  174175  	BSET ZRO 170 DA	; A &= ~(1<<15)  clear bit 15
105325  004406  	STA ,B 6		; mem[B+6] = A
105326  044064  	LDA 64		; A = mem[105412]
105327  146035  	RADD SB DA		; A = A + B
105330  055066  	LDX I 66		; X = mem[[105416]]
105331  006024  	STA ,X 24		; mem[X+24] = A
105332  044065  	LDA 65		; A = mem[105413]
105333  146035  	RADD SB DA		; A = A + B
105334  006025  	STA ,X 25		; mem[X+25] = A
105335  044063  	LDA 63		; A = mem[105412]
105336  146035  	RADD SB DA		; A = A + B
105337  006026  	STA ,X 26		; mem[X+26] = A
105340  004462  	STA ,B 62		; mem[B+62] = A
105341  171415  	SAX 15		; X = 15
105342  135057  	JPL I 57		; -> [105421]  call resident worker
105343  124043  	JMP 43		; -> 105406  (error/exit tail)
105344  124032  	JMP 32		; -> 105376  (join common tail)
; ---- remote / alt path (105345-105375) ----
105345  050400  	LDT ,B 0		; T = mem[B+0]
105346  156170  	SHT ZIN SHR 10	; T >>= 10 (logical, zero fill)
105347  135053  	JPL I 53		; -> [105422]  call resident worker
105350  044053  	LDA 53		; A = mem[105423]
105351  141457  	SKP IF DX MGRE SA	; skip if (unsigned) X >= A
105352  124003  	JMP 3		; -> 105355
105353  044051  	LDA 51		; A = mem[105424]
105354  124032  	JMP 32		; -> 105406  (error/exit tail)
105355  044400  	LDA ,B 0		; A = mem[B+0]
105356  070047  	AND 47		; A &= mem[105425]
105357  054041  	LDX 41		; X = mem[105420]
105360  146037  	RADD SB DX		; X = X + B
105361  135045  	JPL I 45		; -> [105426]  call resident worker
105362  124024  	JMP 24		; -> 105406  (error/exit tail)
105363  044035  	LDA 35		; A = mem[105420]
105364  146035  	RADD SB DA		; A = A + B
105365  146156  	RADD CLD SA DT	; T = A  (copy)
105366  004462  	STA ,B 62		; mem[B+62] = A
105367  171400  	SAX 0		; X = 0
105370  142200  	LBYT		; fetch one character (name byte)
105371  171077  	SAT 77		; T = 77
105372  140065  	SKP IF DA EQL ST	; skip if char == 77B
105373  124003  	JMP 3		; -> 105376
105374  044033  	LDA 33		; A = mem[105427]
105375  124011  	JMP 11		; -> 105406  (error/exit tail)
; ---- common tail (105376-105410) ----
105376  044402  	LDA ,B 2		; A = mem[B+2]
105377  054462  	LDX ,B 62		; X = mem[B+62]
105400  171020  	SAT 20		; T = 20
105401  135027  	JPL I 27		; -> [105430]  call resident worker
105402  124004  	JMP 4		; -> 105406  (error/exit tail)
105403  040404  	MIN ,B 4		; mem[B+4]++ ; skip next if it wraps to 0
105404  170715  	SAA -63		; A = -63  (error code)
105405  125024  	JMP I 24		; -> [105431]  indirect return
105406  004402  	STA ,B 2		; mem[B+2] = A  (error/exit tail entry)
105407  124375  	JMP -3		; -> 105404
105410  147417  	ROP NOOP		; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 105411..105431 ----
105411  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 104 at 105305)
105412  000007  	STZ 7			; constant/pointer cell
105413  031075  	STF I 75		; pointer word (read by JPL I 77 at 105314)
105414  043465  	MIN I ,B ,X 65	; constant/pointer cell
105415  046231  	LDA ,X -147		; pointer word (read by JPL I 74 at 105321)
105416  147421  	ROP NOOP		; pointer word (read by LDX I 66 at 105330)
105417  000006  	STZ 6			; constant/pointer cell
105420  000052  	STZ 52			; constant/pointer cell
105421  020274  	STD -104		; pointer word (read by JPL I 57 at 105342)
105422  030225  	STF -153		; pointer word (read by JPL I 53 at 105347)
105423  132263  	JPC -115		; constant/pointer cell (read by LDA 53 at 105350)
105424  000304  	STZ -74			; constant/pointer cell
105425  000377  	STZ -1			; constant/pointer cell
105426  055111  	LDX I 111		; pointer word (read by JPL I 45 at 105361)
105427  000250  	STZ -130		; constant/pointer cell
105430  031072  	STF I 72		; pointer word (read by JPL I 27 at 105401)
105431  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 24 at 105405)
; 105432 = GUIOI boundary (next symbol); not part of this worker.
