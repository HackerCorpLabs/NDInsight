;=============================================================================
; MON 152B (octal) GetRTName (GRTNA) - real SINTRAN L disassembly
;
; Gets the name of an RT program (7 bytes) given the RT-description address (0 = the
; calling program). Background programs (SINTRAN III VSX). The name is returned with a
; terminating apostrophe if shorter than 7 characters.
;
; MON 152B is a FALL-THROUGH call: GOTAB[152] = 000000 (byte-proven), so there is NO
; per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled point:
;   GRTNA @041745B in resident SINTRAN-DATA_commoncode (load 0), a SYMBOL-1-LIST
;   symbol. GRTNA is NOT a routine entry: it is an INTERIOR label (it aliases the
;   SYMBOL-1 cell WMSBA at the same address) inside a larger resident routine that
;   copies a two-word program name out of the parameter block [B-2] into the caller
;   frame at B+162/B+163. The bytes at and around GRTNA are real executable code; the
;   excerpt below is a bounded window of that enclosing routine (it is not a
;   self-contained procedure - there is no prologue or return in the window).
;   (the fall-through -> GRTNA bridge is the uncarved resident CALLPROC - not in these
;    bytes; see README Honest caveats)
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
;=============================================================================

; ---- Name-copy region enclosing GRTNA (SINTRAN-DATA_commoncode, load 0) ----
; Window 041730B..041751B. GRTNA = 041745B (SYMBOL-1-LIST); the neighbouring symbols
; DOM11=041730, WDIBA/MINSE=041731 ... SMWSE=041746 are interior labels of this dense
; region. This is a bounded excerpt, not a whole routine.
041730  143001  	SKP IF DD LST 0	; skip next if (signed) D < 0 (name-order selector)
041731  124005  	JMP 5		; -> 041736
041732  054776  	LDX ,B -2	; X = mem[B-2]  (parameter block address)
041733  046001  	LDA ,X 1	; A = mem[X+1]
041734  052000  	LDT ,X 0	; T = mem[X+0]
041735  124004  	JMP 4		; -> 041741
041736  054776  	LDX ,B -2	; X = mem[B-2]  (parameter block address)
041737  046000  	LDA ,X 0	; A = mem[X+0]
041740  052001  	LDT ,X 1	; T = mem[X+1]
041741  010562  	STT ,B 162	; mem[B+162] = T  (store first name word)
041742  124007  	JMP 7		; -> 041751
041743  054776  	LDX ,B -2	; X = mem[B-2]
041744  046000  	LDA ,X 0	; A = mem[X+0]
041745  004562  	STA ,B 162	; GRTNA: mem[B+162] = A  (store name word)
041746  046001  	LDA ,X 1	; A = mem[X+1]
041747  004563  	STA ,B 163	; mem[B+163] = A  (store second name word)
041750  146105  	RADD CLD 0 DA	; A = 0
041751  004745  	STA ,B -33	; mem[B-33] = 0
; The enclosing routine continues past this window; this is an excerpt, not a closure.
