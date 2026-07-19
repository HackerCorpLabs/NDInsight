;=============================================================================
; MON 32B (octal) OutMessage - real SINTRAN L disassembly
;
; MON 32B is a FALL-THROUGH call: GOTAB[32] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   MSG output-message worker body   MSG @102453B  in segment 025-S3IRPIT (load 32000B)
;      Window 102453B..102524B = 42 words, bounded strictly to the next symbol
;      GERDV=102525B. Words 102453B..102514B are code; 102515B..102524B are a
;      pointer table (link cells, DATA) that the JPL I / JMP I indirections use.
;   (the fall-through -> MSG bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; The parallel overlay 026-S3IMPIT holds unrelated float/data words at 102453B,
; so 025-S3IRPIT (the SYMBOL-2-LIST code overlay) is the segment with the real
; MSG worker. Addresses and values are octal.
;=============================================================================

; ---- MSG output-message worker (025-S3IRPIT, load 32000B) ----
102453  135043  	JPL I 43		; -> 102516   MSG entry: JPL via link @102516 (resident worker)
102454  050410  	LDT ,B 10
102455  044417  	LDA ,B 17
102456  146151  	RADD CLD SA DD
102457  135040  	JPL I 40		; -> 102517
102460  146161  	RADD CLD ST DD
102461  170404  	SAA 4
102462  150307  	MST PIE		; MST PIE: enable interrupt-enable bits (A=4)
102463  146107  	RADD CLD 0 DX		; clear X (loop index)
102464  050034  	LDT 34		; LDT 34: load message-length / char count for the scan
102465  141076  	SKP IF DT GRE SX		; loop guard: skip while T > X (chars remaining)
102466  124017  	JMP 17		; -> 102505
102467  146116  	RADD CLD SD DT		; D = current byte position
102470  142200  	LBYT		; LBYT: fetch one character from the message string
102471  171047  	SAT 47		; SAT 47: compare char against 47B
102472  142065  	SKP IF DA UEQ ST		; skip if char != 47B
102473  124012  	JMP 12		; -> 102505
102474  171044  	SAT 44		; SAT 44: compare char against 44B
102475  140065  	SKP IF DA EQL ST		; skip if char == 44B
102476  124004  	JMP 4		; -> 102502
102477  170415  	SAA 15
102500  134347  	JPL -31		; -> 102447   JPL -31 -> 102447: call preceding emit routine (in-segment, returns)
102501  170412  	SAA 12
102502  134345  	JPL -33		; -> 102447   JPL -33 -> 102447: call preceding emit routine (in-segment, returns)
102503  173401  	AAX 1		; advance X to next character
102504  124360  	JMP -20		; -> 102464   JMP -20 -> 102464: loop for next character
102505  135014  	JPL I 14		; -> 102521   JPL via link @102521 (resident worker)
102506  146175  	RADD CLD SX DA
102507  156577  	SHA ZIN SHR 1		; SHA ZIN SHR 1: logical right shift by 1
102510  172401  	AAA 1
102511  060410  	ADD ,B 10
102512  004410  	STA ,B 10
102513  135007  	JPL I 7		; -> 102522   JPL via link @102522 (resident worker)
102514  125007  	JMP I 7		; -> 102523   JMP I via link @102523 (resident tail/return)
102515  015436  	STX I ,B 36		; ---- link cells (DATA, not code) 102515..102524 ----
102516  000374  	STZ -4
102517  004177  	STA 177
102520  001000  	STZ I 0
102521  004116  	STA 116
102522  001135  	STZ I 135
102523  010341  	STT -37
102524  144031  	SWAP SB DD
