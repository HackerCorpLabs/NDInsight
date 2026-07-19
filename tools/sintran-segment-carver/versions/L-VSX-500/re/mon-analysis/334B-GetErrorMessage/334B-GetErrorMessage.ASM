; ============================================================================
; MON 334B (octal) GetErrorMessage (GETXM) - real SINTRAN L disassembly
;
; MON 334B is a FALL-THROUGH call: GOTAB[334] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below.
;
; One labelled region:
;   B. GETXM error-message-text worker   GETXM @107273B in 025-S3IRPIT (load 32000B)
;      Window 107273B..107464B (code) + 107465B..107525B (message/link data).
;      The body takes an error number, runs a chain of range tests, computes a
;      message index, and copies the message text with MOVEW; all in-range paths
;      converge on the common exit 107464B (JMP -> 107526, shared copy/return,
;      outside this window).
;
;   (the fall-through -> GETXM bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; SYMBOL NOTE: the flat symbol map also places X21RD/X21DC/DMLP2/X21CH/X21BR/
; DILP2/X21C labels inside 107344B..107457B, but the BYTES there decode as the
; continuous GETXM range-classification chain (identical LDT/SKP/SUB/RADD/JPL I/
; LDA I ,X/JMP pattern) - those labels do NOT match this instruction stream (a
; symbol/overlay artefact; bytes are ground truth). Words 107465B..107525B are a
; message-offset / pointer table (DATA); nd100-dis renders them as bogus.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region B: GETXM error-message-text worker (025-S3IRPIT, load 32000B) ----
; GETXM = 107273B (SYMBOL-2-LIST). Real executable code: classify the error
; number into ranges, index a message-offset table, copy the message text.
; Identity as the MON 334 worker rests on the symbol NAME (GETXM = the 334B
; manual short name) + the error-number-classify/copy behaviour; the fall-through
; -> worker link crosses the uncarved CALLPROC (README caveats).
107273  135172  	JPL I 172		; GETXM entry -> 107465 (link cell)
107274  055172  	LDX I 172
107275  044417  	LDA ,B 17		; fetch caller error number
107276  146151  	RADD CLD SA DD		; D := A
107277  044410  	LDA ,B 10
107300  171100  	SAT 100
107301  135166  	JPL I 166		; -> 107467 (link cell)
107302  125166  	JMP I 166		; -> 107470 (link cell)
107303  055163  	LDX I 163
107304  046014  	LDA ,X 14
107305  004420  	STA ,B 20
107306  046021  	LDA ,X 21
107307  004421  	STA ,B 21
107310  170404  	SAA 4			; MLEV
107311  150307  	MST PIE			; *MST PIE
107312  171014  	SAT 14
107313  135156  	JPL I 156		; -> 107471 (link cell)
107314  044412  	LDA ,B 12
107315  050155  	LDT 155
107316  146161  	RADD CLD ST DD		; D := T
107317  135154  	JPL I 154		; -> 107473 (link cell)
107320  050154  	LDT 154
107321  146161  	RADD CLD ST DD
107322  171020  	SAT 20
107323  143465  	SKP IF DA MLST ST	; range test: if A < T (unsigned) skip
107324  124004  	JMP 4		; -> 107330
107325  146157  	RADD CLD SA DX		; X := A (message index)
107326  047147  	LDA I ,X 147		; A := message via index
107327  124135  	JMP 135		; -> 107464 (common exit)
107330  050146  	LDT 146
107331  143465  	SKP IF DA MLST ST
107332  124005  	JMP 5		; -> 107337
107333  172760  	AAA -20
107334  146157  	RADD CLD SA DX
107335  047142  	LDA I ,X 142
107336  124126  	JMP 126		; -> 107464 (common exit)
107337  050141  	LDT 141
107340  141465  	SKP IF DA MGRE ST	; range test: if A >= T (unsigned) skip
107341  124010  	JMP 10		; -> 107351
107342  050137  	LDT 137
107343  143465  	SKP IF DA MLST ST
107344  124005  	JMP 5		; -> 107351   (label X21RD here is a symbol artefact)
107345  064133  	SUB 133
107346  146157  	RADD CLD SA DX		; (label X21DC artefact)
107347  047133  	LDA I ,X 133
107350  124114  	JMP 114		; -> 107464 (common exit)
107351  050132  	LDT 132
107352  141465  	SKP IF DA MGRE ST
107353  124011  	JMP 11		; -> 107364
107354  050130  	LDT 130			; (label DMLP2 artefact)
107355  143465  	SKP IF DA MLST ST
107356  124006  	JMP 6		; -> 107364
107357  064124  	SUB 124
107360  146157  	RADD CLD SA DX
107361  135124  	JPL I 124		; -> 107505 (message-table pointer)
107362  047124  	LDA I ,X 124
107363  124101  	JMP 101		; -> 107464 (common exit)
107364  050123  	LDT 123
107365  141465  	SKP IF DA MGRE ST
107366  124011  	JMP 11		; -> 107377
107367  050121  	LDT 121
107370  143465  	SKP IF DA MLST ST
107371  124006  	JMP 6		; -> 107377
107372  064115  	SUB 115
107373  146157  	RADD CLD SA DX
107374  135111  	JPL I 111		; -> 107505 (message-table pointer)
107375  047114  	LDA I ,X 114
107376  124066  	JMP 66		; -> 107464 (common exit; label X21CH artefact)
107377  050113  	LDT 113
107400  141465  	SKP IF DA MGRE ST
107401  124017  	JMP 17		; -> 107420
107402  050111  	LDT 111
107403  143465  	SKP IF DA MLST ST
107404  124014  	JMP 14		; -> 107420
107405  064105  	SUB 105
107406  146157  	RADD CLD SA DX
107407  146116  	RADD CLD SD DT		; T := D
107410  044104  	LDA 104
107411  146151  	RADD CLD SA DD
107412  170411  	SAA 11			; length (9. words)
107413  146154  	RADD CLD SA DL		; L := word count for MOVEW
107414  143103  	MOVEW			; block move: copy message text
107415  146161  	RADD CLD ST DD
107416  047077  	LDA I ,X 77
107417  124045  	JMP 45		; -> 107464 (common exit)
107420  050076  	LDT 76
107421  141465  	SKP IF DA MGRE ST
107422  124017  	JMP 17		; -> 107441
107423  050074  	LDT 74
107424  143465  	SKP IF DA MLST ST
107425  124014  	JMP 14		; -> 107441
107426  064070  	SUB 70
107427  146157  	RADD CLD SA DX
107430  146116  	RADD CLD SD DT
107431  044067  	LDA 67
107432  146151  	RADD CLD SA DD
107433  170411  	SAA 11
107434  146154  	RADD CLD SA DL
107435  143103  	MOVEW			; block move: copy message text
107436  146161  	RADD CLD ST DD
107437  047062  	LDA I ,X 62
107440  124024  	JMP 24		; -> 107464 (common exit)
107441  050061  	LDT 61
107442  141465  	SKP IF DA MGRE ST
107443  124017  	JMP 17		; -> 107462
107444  050057  	LDT 57
107445  143465  	SKP IF DA MLST ST	; (label X21BR artefact)
107446  124014  	JMP 14		; -> 107462
107447  064053  	SUB 53
107450  146157  	RADD CLD SA DX		; (label DILP2 artefact)
107451  146116  	RADD CLD SD DT
107452  044052  	LDA 52
107453  146151  	RADD CLD SA DD
107454  170411  	SAA 11
107455  146154  	RADD CLD SA DL
107456  143103  	MOVEW			; block move: copy message text
107457  146161  	RADD CLD ST DD		; (label X21C artefact)
107460  047045  	LDA I ,X 45
107461  124003  	JMP 3		; -> 107464 (common exit)
107462  171400  	SAX 0			; default: X := 0
107463  047014  	LDA I ,X 14		; default message
107464  124042  	JMP 42		; common exit -> 107526 (shared copy/return, outside window)
; ---- message-offset / pointer table (DATA, not code) 107465..107525 ----
; read by the JPL I calls above (107465 107467 107470 107471 107473 107505 ...).
107465  000374  	STZ -4			; (data) link cell (read by JPL I at 107273)
107466  004007  	STA 7			; (data)
107467  011544  	STT I ,B 144		; (data) link cell (read by JPL I at 107301)
107470  107601  	FSB I ,B ,X -177		; (data) link cell (read by JMP I at 107302)
107471  004735  	STA ,B -43		; (data) link cell (read by JPL I at 107313)
107472  002200  	STZ ,X -200		; (data)
107473  004177  	STA 177			; (data) link cell (read by JPL I at 107317)
107474  107613  	FSB I ,B ,X -165		; (data)
107475  034252  	LDF -126			; (data)
107476  000400  	STZ ,B 0			; (data)
107477  034273  	LDF -105			; (data)
107500  003200  	STZ I ,X -200		; (data)
107501  003231  	STZ I ,X -147		; (data)
107502  034653  	LDF ,B -125		; (data)
107503  001000  	STZ I 0			; (data)
107504  001100  	STZ I 100			; (data)
107505  107713  	FSB I ,B ,X -65		; (data) link cell (read by JPL I at 107361/107374)
; 107506..107525 continue the pointer/message table (DATA); the code path
; converges at 107464 -> 107526 (shared message copy/return), outside this window.
