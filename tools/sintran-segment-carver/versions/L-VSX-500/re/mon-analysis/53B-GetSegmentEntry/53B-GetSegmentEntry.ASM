; ============================================================================
; MON 53B (octal) GetSegmentEntry (RSEGM) - real SINTRAN L disassembly
; Returns the 5-word segment-table entry for an ND-100 segment number.
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[53B] = 121174B  -> F1634, the level-14 entry thunk in segment
;   025-S3IRPIT (load 32000B). This value is byte-proven (GOTAB word 071306B =
;   121174B). F1634 is one of the F163x family of level-14 stubs
;   (F1630=121050 .. F1637=121273, stride 21 words); GOTAB[53] lands on F1634.
;   The stub's first instruction is an indirect JMP through a link cell into a
;   RESIDENT routine (010341B) that is NOT present in any carved segment
;   (uncarved bridge). So the actual segment-entry worker is reached past the
;   uncarved resident hop.
;
;   The manual short name RSEGM resolves (L07 SYMBOL-1-LIST) to RSEGM=000021B -
;   a tiny value, NOT a code address (no ND-100 segment loads below 32000B).
;   It is a data constant / table index, NOT a carveable worker body. So there
;   is no separate RSEGM code region to disassemble; the byte-recoverable part
;   of MON 53 is the F1634 entry thunk shown below.
;
; Region bound: F1634=121174B to next symbol BOSIZ=121215B (17 words). Words
; 121201B..121214B are a POINTER/LINK-CELL table (data): nd100-dis renders them
; as bogus FDV/MPY/STZ/ADD instructions - their contents are routine addresses,
; not opcodes.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- F1634 GOTAB[53] entry thunk (025-S3IRPIT, load 32000B) ----
121174  125015  	JMP I 15		; -> link cell 121211 (= 010341, RESIDENT routine, uncarved)
121175  050421  	LDT ,B 21		; T = mem[B+21]
121176  146163  	RADD CLD ST DB		; B = T   (CLD clears dest, source = T)
121177  004412  	STA ,B 12		; mem[B+12] = A
121200  125011  	JMP I 11		; -> link cell 121211 (= 010341, RESIDENT routine, uncarved)
; ---- link cells (DATA, not code) 121201..121214 ----
121201  116276  	FDV ,X -102		; (data) link cell -> 116276
121202  121640  	MPY I ,B -140		; (data) link cell -> 121640
121203  000315  	STZ -63			; (data) link cell -> 000315
121204  061160  	ADD I 160		; (data) link cell -> 061160
121205  061157  	ADD I 157		; (data) link cell -> 061157
121206  115720  	FDV I ,B -60		; (data) link cell -> 115720
121207  116234  	FDV ,X -144		; (data) link cell -> 116234
121210  116026  	FDV ,X 26		; (data) link cell -> 116026
121211  010341  	STT -37			; (data) link cell -> 010341 (RESIDENT routine target of the two JMP I above)
121212  116036  	FDV ,X 36		; (data) link cell -> 116036
121213  004007  	STA 7			; (data) link cell -> 004007
121214  117107  	FDV I ,X 107		; (data) link cell -> 117107
; 121215 = BOSIZ boundary (next symbol); not part of this region.
;
; The F1634 head is a one-instruction dispatch thunk: MON 53 lands at 121174B,
; which JMP-indirects through cell 121211B (= 010341B) into the RESIDENT monitor,
; where the real RSEGM segment-entry read is performed. That resident routine is
; NOT carved, so the MON 53 -> segment-entry-worker link cannot be followed from
; these bytes (see README Honest caveats). The 121175-121200 words are shared
; F163x-family level-14 handling reached by sibling stubs, not the entry path.
