; ============================================================================
; MON 64B (octal) WarningMessage (ERMSG) - real SINTRAN L disassembly
;
; MON 64B is a FALL-THROUGH call: GOTAB[64] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. ERMSG error-message worker   ERMSG @16714B in resident SINTRAN-DATA_commoncode (load 0)
;      Window 16714B..17037B (up to but not including SEGTX=17040B).
;      The body outputs a file-system error message for the input error code and
;      returns (the program continues). It has TWO entry points:
;        ERMSG @16714B (T:=0) and QERMS @16716B (T:=1); both merge at 16717B.
;   (the fall-through -> ERMSG bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; Words 17021B..17037B are POINTER/CONSTANT cells (link cells), not code;
; nd100-dis renders them as bogus instructions. Their contents are the real
; worker addresses/constants the body indexes through.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region B: ERMSG error-message worker (SINTRAN-DATA_commoncode, load 0) ----
; ERMSG = 16714B (SYMBOL-1-LIST); secondary entry QERMS = 16716B; next symbol
; SEGTX = 17040B bounds the region. Real executable code: it sets a flag (T=0 for
; ERMSG, T=1 for QERMS), stores the caller parameters through pointer cells, then
; calls the message-format/output workers and returns via EXIT. Identity as the
; MON 64 worker rests on the symbol NAME (ERMSG) + the dual ERMSG/QERMS entry
; shape; the fall-through->worker link crosses the uncarved CALLPROC (README caveats).
16714  146106  	RADD CLD 0 DT		; ERMSG entry: T := 0 (warning/continue flavour)
16715  124002  	JMP 2		; -> 16717 (skip QERMS entry)
16716  171001  	SAT 1			; QERMS entry: T := 1 (alternate flavour)
16717  011102  	STT I 102		; store flag through pointer cell [.+102]
16720  050102  	LDT 102
16721  144036  	SWAP SB DT
16722  011101  	STT I 101		; store caller param through pointer cell [.+101]
16723  005101  	STA I 101		; store error code (A) through pointer cell [.+101]
16724  146145  	RADD CLD SL DA
16725  005100  	STA I 100
16726  045100  	LDA I 100
16727  131404  	JAF 4		; -> 16733
16730  171005  	SAT 5
16731  135076  	JPL I 76		; -> 17027 (link cell = 004762)
16732  124015  	JMP 15		; -> 16747
16733  045066  	LDA I 66
16734  131013  	JAZ 13		; -> 16747
16735  045067  	LDA I 67
16736  005072  	STA I 72
16737  135072  	JPL I 72		; -> 17031 (link cell = 001135)
16740  045063  	LDA I 63
16741  146153  	RADD CLD SA DB
16742  135070  	JPL I 70		; -> 17032 (link cell = 012315)
16743  144165  	SWAP CLD ST DA
16744  000002  	STZ 2
16745  044055  	LDA 55
16746  146153  	RADD CLD SA DB
16747  171014  	SAT 14
16750  135063  	JPL I 63		; -> 17033 (link cell = 004735)
16751  011063  	STT I 63
16752  045052  	LDA I 52
16753  135062  	JPL I 62		; -> 17035 (link cell = 060033)
16754  045052  	LDA I 52
16755  131007  	JAZ 7		; -> 16764
16756  045043  	LDA I 43
16757  131005  	JAZ 5		; -> 16764
16760  055043  	LDX I 43
16761  046032  	LDA ,X 32
16762  055054  	LDX I 54
16763  006021  	STA ,X 21
16764  051050  	LDT I 50
16765  135046  	JPL I 46		; -> 17033 (link cell = 004735)
16766  045033  	LDA I 33
16767  131023  	JAZ 23		; -> 17012
16770  045036  	LDA I 36
16771  131003  	JAZ 3		; -> 16774
16772  135045  	JPL I 45		; -> 17037 (link cell = 016262)
16773  042300  	MIN ,X -100
16774  055027  	LDX I 27
16775  046001  	LDA ,X 1
16776  052031  	LDT ,X 31
16777  140065  	SKP IF DA EQL ST
17000  124012  	JMP 12		; -> 17012
17001  146167  	RADD CLD ST DX
17002  046017  	LDA ,X 17
17003  070035  	AND 35
17004  060035  	ADD 35
17005  006017  	STA ,X 17
17006  044034  	LDA 34
17007  056025  	LDX ,X 25
17010  146106  	RADD CLD 0 DT
17011  143304  	STATX
17012  051011  	LDT I 11		; exit-path merge point
17013  146163  	RADD CLD ST DB
17014  034411  	LDF ,B 11
17015  055010  	LDX I 10
17016  146174  	RADD CLD SX DL
17017  054410  	LDX ,B 10
17020  146142  	EXIT			; return to caller (program continues)
; ---- link cells (DATA, not code) 17021..17037 ----
17021  144253  	SWAP CM1 SA DB		; (data) constant/pointer cell
17022  144200  	SWAP CM1 0 0		; (data)
17023  144231  	SWAP CM1 SB DD		; (data)
17024  144234  	SWAP CM1 SB DL		; (data)
17025  144241  	SWAP CM1 SL DD		; (data)
17026  004004  	STA 4			; (data)
17027  004762  	STA ,B -16		; (data) link cell (read by JPL I at 16731)
17030  144166  	SWAP CLD ST DT		; (data)
17031  001135  	STZ I 135		; (data) link cell (read by JPL I at 16737)
17032  012315  	STT ,X -63		; (data) link cell (read by JPL I at 16742)
17033  004735  	STA ,B -43		; (data) link cell (read by JPL I at 16750/16765)
17034  144254  	SWAP CM1 SA DL		; (data)
17035  060033  	ADD 33			; (data) link cell (read by JPL I at 16753)
17036  004007  	STA 7			; (data)
17037  016262  	STX ,X -116		; (data) link cell (read by JPL I at 16772)
; 17040 = SEGTX boundary (next symbol); not part of this worker.
