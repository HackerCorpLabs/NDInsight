; ============================================================================
; MON 252B (octal) BackupClose - real SINTRAN L disassembly
;
; Two labelled regions:
;   A. F1733 GOTAB stub fragment     F1733 @066252B  in segment 025-S3IRPIT (load 32000B)
;      GOTAB[252] = 066252B (byte-proven, non-zero) points here. F1733 is part of
;      the F17xx resident GOTAB-stub cluster (siblings F1725=236B, F1734=254B). It
;      is real bytes but a FRAGMENT: control flow continues past the next symbol
;      F1735=066256B into uncarved resident code, and it holds no static pointer to
;      BCLOS - so the stub -> worker hop crosses the uncarved resident CALLPROC.
;   B. BCLOS backup-close worker      BCLOS @103350B  in segment 006-S3FS (load 26000B)
;      BCLOS is one of three 1-2 word MODE-SELECT entry points in a shared close
;      dispatcher:
;         BCLOS @103350B  BSET ZRO SSM / BSET ONE SSK   (BackupClose,   252B)
;         SPERM @103353B  BSET ONE SSM                  (permanent-close variant)
;         CLOFI @103355B  BSET ZRO SSK / BSET ZRO SSM   (plain close)
;      All three merge at the shared body @103357B. SSM = STS bit7 (M), SSK = STS
;      bit2 (K); nd100-dis prints them symbolically. The body tests the two flags
;      (103365 BSKP ONE SSM / 103373 BSKP ONE SSK) and calls the file-close
;      primitive FCLOS (@067612B, link cell 103415) - the same primitive CLOSF
;      (MON 43B CloseFile) calls - which is the byte-level reason BCLOS is the
;      BackupClose worker. Window 103350B..103416B (self-contained; closes at the
;      003776 resident-return link cell at 103416B, next symbol SETUP=103417B).
;   (the stub -> BCLOS bridge is the uncarved resident CALLPROC - not in these
;    bytes; see README Honest caveats)
;
; Words 103410B..103416B are POINTER TABLES (link cells), not code. Octal.
; ============================================================================

; ---- Region A: F1733 GOTAB stub fragment (025-S3IRPIT, load 32000B) ----
066252  006376  	STA ,X -2
066253  044422  	LDA ,B 22
066254  006377  	STA ,X -1
066255  046374  	LDA ,X -4

; ---- Region B: BCLOS backup-close worker (006-S3FS, load 26000B) ----
103350  174070  	BSET ZRO SSM
103351  174220  	BSET ONE SSK
103352  124005  	JMP 5		; -> 103357
103353  174270  	BSET ONE SSM
103354  124003  	JMP 3		; -> 103357
103355  174020  	BSET ZRO SSK
103356  174070  	BSET ZRO SSM
103357  021031  	STD I 31
103360  146145  	RADD CLD SL DA
103361  146131  	RADD CLD SB DD
103362  170006  	SAB 6
103363  135026  	JPL I 26		; -> 103411
103364  011026  	STT I 26
103365  175270  	BSKP ONE SSM
103366  124005  	JMP 5		; -> 103373
103367  146165  	RADD CLD ST DA
103370  135023  	JPL I 23		; -> 103413
103371  124015  	JMP 15		; -> 103406
103372  124011  	JMP 11		; -> 103403
103373  175220  	BSKP ONE SSK
103374  124005  	JMP 5		; -> 103401
103375  044402  	LDA ,B 2
103376  135016  	JPL I 16		; -> 103414
103377  124007  	JMP 7		; -> 103406
103400  124003  	JMP 3		; -> 103403
103401  135014  	JPL I 14		; -> 103415
103402  124004  	JMP 4		; -> 103406
103403  040404  	MIN ,B 4
103404  170772  	SAA -6
103405  125011  	JMP I 11		; -> 103416
103406  004402  	STA ,B 2
103407  124375  	JMP -3		; -> 103404
103410  147417  	ROP NOOP
103411  003752  	STZ I ,B ,X -26
103412  147505  	ROP NOOP
103413  072465  	AND ,X ,B 65
103414  067602  	SUB I ,B ,X -176
103415  067612  	SUB I ,B ,X -166
103416  003776  	STZ I ,B ,X -2
