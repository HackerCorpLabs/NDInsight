; ============================================================================
; MON 76B (octal) SetBlockSize - real SINTRAN L disassembly
; Two regions (the dispatch is a fall-through; see README Code location):
;   A. GOTAB[76] level-14 dispatch word  @071331B  in resident SINTRAN-DATA_commoncode (load 0)
;      = 000000 -> FALL-THROUGH: NO F16xx code stub; the level-14 default
;        path (resident MFELL / CALLPROC) selects the worker. That bridge is
;        uncarved, so there is no stub to list here - only the data word.
;   B. SBSIZ file-system worker body      SBSIZ @103752B in segment 006-S3FS (load 26000B)
;      (the documented MON short name is SETBS; the internal FILSYS symbol is SBSIZ)
;      (the A->B link is the uncarved resident CALLPROC - not in these bytes)
; SBSIZ is one member of a family of file-position setters that share ONE
; skeleton verbatim (SETBY 74B / SETBC / SBSIZ 76B / RMAX 62B / REABT 75B):
; each differs only in the pointer-table displacement it selects. See 074B-SetStartByte.
; Addresses and values are octal. Disassembled from the canonical 006-S3FS binary.
; ============================================================================

; ---- Region A: GOTAB[76] dispatch word (resident SINTRAN-DATA_commoncode) ----
; 071331B = GOTAB base 071233B + 076. Byte offset 58802 dec. Raw = 00 00.
071331  000000  	; GOTAB[76] = 000000  (FALL-THROUGH; no direct handler word)

; ---- Region B: SBSIZ file-system worker (006-S3FS), 103752B..103766B (13 words) ----
; Bounded below by SBSIZ=103752B (FILSYS-SYMBOLS), above by the next symbol
; RMAX=103767B. All backward/forward direct branches stay inside the block; the
; JPL I / JMP I transfers go through the shared pointer table 104023B..104034B
; (DATA) into resident FS routines that lie OUTSIDE this carve.
103752  021051  	STD I 51		; save 32-bit block-size arg (A:D) via ptr@104023 = 147417
103753  146145  	RADD CLD SL DA		; A := L
103754  146131  	RADD CLD SB DD		; D := B
103755  170006  	SAB 6			; B := 6  (argument-block index)
103756  135046  	JPL I 46		; -> 104024  (ptr = 003752 -> resident worker 3752B, UNCARVED)
103757  011046  	STT I 46		; store returned T via ptr@104025 = 147505
103760  135052  	JPL I 52		; -> 104032  (ptr = 072351 -> resident worker 72351B, UNCARVED)
103761  124004  	JMP 4		; -> 103765  (normal path)
103762  040404  	MIN ,B 4		; (error/return tail) bump arg-block+4
103763  170772  	SAA -6			; A := -6  (error/skip code, exact meaning UNVERIFIED)
103764  125044  	JMP I 44		; -> 104030  (ptr = 003776 -> resident common exit 3776B, UNCARVED)
103765  004402  	STA ,B 2		; store result A into arg block +2
103766  124375  	JMP -3		; -> 103763  (tail loop back to SAA -6 / exit)
; 103767 = RMAX boundary (next symbol, the 62B GetBytesInFile worker); not part of SBSIZ.
;
; ---- Shared pointer table 104023B..104034B (DATA, not code) ----
; This table is shared by the whole file-position-setter family; read as raw words:
;   104023 = 147417   104024 = 003752   104025 = 147505   104030 = 003776
;   104032 = 072351
; The 3752B / 3776B / 72351B targets are resident FS routines OUTSIDE this carve,
; so the SBSIZ -> resident-worker edges cannot be followed from these bytes.
