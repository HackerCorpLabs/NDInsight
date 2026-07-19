; ============================================================================
; MON 65B (octal) ErrorMessage (QERMS) - real SINTRAN L disassembly
;
; MON 65B has a byte-proven GOTAB vector: GOTAB[65] = 121345B = F1641, an F16xx
; level-14 dispatch stub in 025-S3IRPIT (load 32000B). This is the SIBLING call
; of MON 64B WarningMessage: both are served by ONE shared worker body in
; resident commoncode with two entry points -
;   ERMSG @16714B (T:=0) -> WarningMessage (MON 64B, program CONTINUES)
;   QERMS @16716B (T:=1) -> ErrorMessage   (MON 65B, program TERMINATES)
; both entries merge at 16717B and run the same body.
;
; Two labelled regions:
;   A. F1641 entry stub         F1641 @121345B in 025-S3IRPIT (load 32000B)
;      Window 121345B..121366B (up to but not including P36R2=121367B).
;   B. QERMS/ERMSG worker body  QERMS @16716B / ERMSG @16714B in commoncode (load 0)
;      Window 16714B..17037B (up to but not including SEGTX=17040B).
;
;   (the stub -> worker transfer is the uncarved resident CALLPROC / segment
;    switch - not present in these bytes; see README Honest caveats)
;
; Words 17021B..17037B are POINTER/CONSTANT cells (link cells), not code;
; nd100-dis renders them as bogus instructions. Their contents are the real
; worker addresses/constants the body indexes through.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region A: F1641 entry stub (025-S3IRPIT, load 32000B) ----
; F1641 = 121345B (SYMBOL-2-LIST); next symbol P36R2 = 121367B bounds the region.
; The entry proper is 121345B..121346B: it calls a shared F16xx routine
; (JPL I 47 -> 121414) then jumps into shared family tail code (JMP 34 -> 121402);
; both targets stay inside 025-S3IRPIT. Words 121347B..121366B are shared
; continuation code reached through those family routines, not from the F1641
; entry directly. The stub does NOT itself reach the QERMS worker - that transfer
; is the uncarved CALLPROC hop (README caveats).
121345  135047  	JPL I 47		; F1641 entry: call shared F16xx routine -> 121414
121346  124034  	JMP 34		; -> 121402 (shared F16xx family tail)
121347  044417  	LDA ,B 17
121350  156577  	SHA ZIN SHR 1
121351  050415  	LDT ,B 15
121352  054416  	LDX ,B 16
121353  146057  	RADD SA DX		; X := X + A
121354  044350  	LDA -30
121355  143304  	STATX			; phys[EL] := A  (EL=((T&377)<<16)|(X+0))
121356  173401  	AAX 1			; X := X + 1
121357  044346  	LDA -32
121360  143304  	STATX			; phys[EL] := A
121361  044417  	LDA ,B 17
121362  172404  	AAA 4			; A := A + 4
121363  004417  	STA ,B 17
121364  044446  	LDA ,B 46
121365  172774  	AAA -4			; A := A - 4
121366  004446  	STA ,B 46
; 121367 = P36R2 boundary (next symbol); not part of this stub.

; ---- Region B: QERMS/ERMSG error-message worker (commoncode, load 0) ----
; QERMS = 16716B / ERMSG = 16714B (SYMBOL-1-LIST); next symbol SEGTX = 17040B
; bounds the region. Real executable code shared with MON 64B. For MON 65B the
; entry is QERMS (T:=1); ERMSG (T:=0) is MON 64B's entry. Both merge at 16717B,
; store the caller parameters through pointer cells, call the message-format /
; output workers, and return via EXIT. Identity as the MON 65B worker rests on
; the symbol NAME (QERMS = 65B short name in the manual) + the dual ERMSG/QERMS
; entry shape; the stub->worker link crosses the uncarved CALLPROC (README caveats).
16714  146106  	RADD CLD 0 DT		; ERMSG entry (MON 64B): T := 0 (warning/continue)
16715  124002  	JMP 2		; -> 16717 (skip QERMS entry)
16716  171001  	SAT 1			; QERMS entry (MON 65B): T := 1 (error/terminate)
16717  011102  	STT I 102		; merge point: store flag through pointer cell [.+102]
16720  050102  	LDT 102
16721  144036  	SWAP SB DT
16722  011101  	STT I 101		; store caller param through pointer cell [.+101]
16723  005101  	STA I 101		; store error number (A) through pointer cell [.+101]
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
17020  146142  	EXIT			; return through the level-14 frame
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
