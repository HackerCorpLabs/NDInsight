; ============================================================================
; MON 432B (octal) SIBASFunction (SIBFU) - real SINTRAN L disassembly
;
; MON 432B has a byte-proven GOTAB vector: GOTAB[432] = 056524B. That value is an
; address inside 025-S3IRPIT (load base 32000B, the overlay mapped for level-14
; dispatch), where it resolves to the symbol DT83W = 56524B. Disassembled at the
; SAME address in resident commoncode (load 0) the bytes are `124025 JMP 25`, a
; lone jump; the 025-S3IRPIT overlay is the segment active during MON dispatch
; and there it is a structured level-14 dispatcher - so 025-S3IRPIT is the
; real-code region for this vector.
;
; One labelled region:
;   A. DT83W dispatcher   DT83W @56524B in 025-S3IRPIT (load 32000B)
;      Window 56524B..56536B (code, to DT84R=56537B) + shared tail / link cells
;      56537B..56563B. It loads the function selector into X, calls sub-routines
;      indirectly through a pointer table (JPL I / JMP I via link cells in the
;      56550B..56562B tail), and checks results against -1 (error).
;
;   (whether DT83W is the true semantic "various SIBAS functions" worker, or a
;    dispatch-table slot the real SIBAS worker is reached from across the uncarved
;    CALLPROC, is UNVERIFIED - the symbol NAME does not confirm SIBAS; see README
;    Honest caveats)
;
; Words 56544B..56563B are POINTER/CONSTANT cells (link cells) shared by the
; DT83W/DT84R/DT84W/SCGLB/SCGLN sibling entries; nd100-dis renders them as bogus
; instructions. Their contents are the real sub-worker addresses.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region A: DT83W dispatcher (025-S3IRPIT, load 32000B) ----
; DT83W = 56524B (SYMBOL-2-LIST); next symbol DT84R = 56537B bounds the entry.
; Real executable code: a compact function dispatcher.
056524  146157  	RADD CLD SA DX		; DT83W entry: X := A (function selector)
056525  135030  	JPL I 30		; call sub-routine -> link cell 56555 (=050710)
056526  135033  	JPL I 33		; call sub-routine -> link cell 56561 (=063743)
056527  171377  	SAT -1			; T := -1 (error sentinel)
056530  142065  	SKP IF DA UEQ ST	; skip if A != T  (result != -1)
056531  124006  	JMP 6		; -> 56537 (result == -1: error path into DT84R)
056532  135016  	JPL I 16		; call sub-routine -> link cell 56550 (=063661)
056533  171377  	SAT -1
056534  142065  	SKP IF DA UEQ ST	; skip if A != T  (result != -1)
056535  125025  	JMP I 25		; -> link cell 56562 (=057177): dispatch onward
056536  124003  	JMP 3		; -> 56541 (shared tail)
; ---- sibling entries + shared tail (DT84R/DT84W/SCGLB/SCGLN) ----
; DT84R = 56537B is a sibling entry in the same dispatch-table block; DT83W's
; error path (56531 JMP 6) and its 56536 JMP 3 fall into this shared tail.
056537  000425  	STZ ,B 25		; DT84R entry
056540  000426  	STZ ,B 26
056541  135017  	JPL I 17		; -> link cell 56560 (=050661)
056542  135010  	JPL I 10		; -> link cell 56552 (=063775)
056543  125020  	JMP I 20		; -> link cell 56563 (=057161)
; ---- link cells (DATA, not code) 56544..56563 ----
; sub-worker address pointers read by the JPL I / JMP I calls above.
056544  064023  	SUB 23			; (data)
056545  063772  	ADD I ,B ,X -6		; (data)
056546  177764  	BORA 160 DL		; (data)
056547  004475  	STA ,B 75		; (data)
056550  063661  	ADD I ,B ,X -117		; (data) link cell (read by JPL I at 56532)
056551  057170  	LDX I ,X 170		; (data)
056552  063775  	ADD I ,B ,X -3		; (data) DT84W entry / link cell (read at 56542)
056553  050633  	LDT ,B -145		; (data) SCGLB
056554  000004  	STZ 4			; (data)
056555  050710  	LDT ,B -70		; (data) link cell (read by JPL I at 56525)
056556  063741  	ADD I ,B ,X -37		; (data) SCGLN
056557  057174  	LDX I ,X 174		; (data)
056560  050661  	LDT ,B -117		; (data) link cell (read by JPL I at 56541)
056561  063743  	ADD I ,B ,X -35		; (data) link cell (read by JPL I at 56526)
056562  057177  	LDX I ,X 177		; (data) link cell (read by JMP I at 56535)
056563  057161  	LDX I ,X 161		; (data) link cell (read by JMP I at 56543)
; 56565 = DT85R (next dispatch-table entry); not part of DT83W.
