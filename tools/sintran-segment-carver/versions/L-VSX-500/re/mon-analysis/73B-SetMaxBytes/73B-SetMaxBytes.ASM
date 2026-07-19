; ============================================================================
; MON 73B (octal) SetMaxBytes - real SINTRAN L disassembly
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[73] level-14 entry stub  F1644 @121444B  in segment 025-S3IRPIT (load 32000B)
;   B. SMAX set-max-bytes worker body  SMAX @103706B   in segment 006-S3FS   (load 26000B)
;   (the A->B bridge is the uncarved resident CALLPROC - not in these bytes)
; SMAX sets the maximum byte pointer of an opened file; the byte-level identity
; is its JPL-call to SMAXB (@72620B) - the set-max-bytes primitive.
; The worker at 103706B shares one link-cell table (104024..104032) and one
; error/return tail (103731..103734) with the sibling calls SETBY (MON 74B,
; @103720B) and SETBC (@103735B); the sibling entry bodies are NOT part of MON
; 73B and are omitted here. Addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[73] entry stub  F1644 (025-S3IRPIT, load 32000B) ----
; Bounded by the next SYMBOL-2 symbol F1645=121471B. The forward JMPs to
; 121473B and the JPL/JMP I link cells transfer on through the resident
; CALLPROC bridge (uncarved) - they are not followed here.
121444  124006  	JMP 6		; -> 121452
121445  044353  	LDA -25
121446  135035  	JPL I 35		; -> 121503 (link cell)
121447  135035  	JPL I 35		; -> 121504 (link cell)
121450  144400  	RAND 0 0
121451  124022  	JMP 22		; -> 121473 (onward, into F1645 continuation)
121452  171001  	SAT 1
121453  140065  	SKP IF DA EQL ST
121454  124010  	JMP 10		; -> 121464
121455  045030  	LDA I 30
121456  146153  	RADD CLD SA DB
121457  045027  	LDA I 27
121460  146157  	RADD CLD SA DX
121461  044336  	LDA -42
121462  146151  	RADD CLD SA DD
121463  125024  	JMP I 24		; -> 121507 (link cell; onward via CALLPROC)
121464  171002  	SAT 2
121465  140065  	SKP IF DA EQL ST
121466  124005  	JMP 5		; -> 121473 (onward, into F1645 continuation)
121467  135015  	JPL I 15		; -> 121504 (link cell)
121470  124003  	JMP 3		; -> 121473 (onward, into F1645 continuation)

; ---- Region B: SMAX set-max-bytes worker (006-S3FS, load 26000B) ----
103706  021115  	STD I 115		; SMAX entry: stash caller D (params) via link cell 104025
103707  146145  	RADD CLD SL DA
103710  146131  	RADD CLD SB DD
103711  170006  	SAB 6			; establish local frame (B), 6 words
103712  135112  	JPL I 112		; -> 104024 (link cell = 003752, resident prologue worker)
103713  011112  	STT I 112		; store caller file-number (T) via link cell 104025
103714  135112  	JPL I 112		; -> 104026 (link cell = 072620, SMAXB set-max-bytes primitive)
103715  124016  	JMP 16		; -> 103733 (store-status exit)
103716  040404  	MIN ,B 4		; (alt path) bump B+4
103717  124012  	JMP 12		; -> 103731 (unwind + resident return)

; ---- shared error/return tail (103731..103734) - reached by SMAX and the
;      siblings SETBY/SETBC; SMAX branches here from 103715/103717 ----
103731  170772  	SAA -6			; unwind 6-word frame (matches SAB 6)
103732  125076  	JMP I 76		; -> 104030 (link cell = 003776, resident return cell)
103733  004402  	STA ,B 2		; store status word -> caller slot B+2
103734  124375  	JMP -3		; -> 103731 (fall to unwind + resident return)

; ---- link cells (DATA, not code) 104024..104032 - shared table ----
; nd100-dis renders these pointer words as bogus instructions; their contents
; are the real worker addresses (resolved below).
104024  003752  	STZ I ,B ,X -26		; = 003752  (resident prologue worker; used by SMAX 103712)
104025  147505  	ROP NOOP		; (data) indirect store slot (used by SMAX 103706/103713)
104026  072620  	AND ,X ,B -160		; = 072620  (SMAXB set-max-bytes primitive; used by SMAX 103714)
104027  072622  	AND ,X ,B -156		; = 072622  (SBYTE - SETBY sibling primitive; not MON 73B)
104030  003776  	STZ I ,B ,X -2		; = 003776  (resident return cell; used by SMAX 103732)
104031  072072  	AND ,X 72		; = 072072  (SBLOP - sibling primitive; not MON 73B)
104032  072351  	AND ,X -27		; = 072351  (SBLSZ - sibling primitive; not MON 73B)
