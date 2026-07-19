; ============================================================================
; MON 13B (octal) ClearInBuffer - real SINTRAN L disassembly
; Two labelled regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[13] dispatch target  120454B  in segment 025-S3IRPIT (load 32000B)
;      -> decodes as DATA (FDV/STA/STZ), NOT a jump stub. The stub->worker hop
;         for MON 13B is NOT statically followable (see README Honest caveats).
;   B. CIBUF ring-buffer clear worker body  CIBUF @044120B (window 044113B..044156B)
;      in the resident image SINTRAN-DATA_commoncode (load 0)
;      -> real SINTRAN L bytes; linked to MON 13B by symbol name + behaviour ONLY.
; Addresses and values are octal. Regenerate: see README ".ASM generation recipe".
; ============================================================================

; ---- Region A: GOTAB[13] dispatch target (025-S3IRPIT, load 32000B) ----
; GOTAB[13] = 120454B (byte-proven, prove-mon.py 013). What sits AT 120454B is
; DATA, not a followable stub. In commoncode (load 0) this address reads 000000
; (a runtime-populated zero run 103031B..170177B); the bytes below come from the
; 025-S3IRPIT overlay, whose SYMBOL-2-LIST label F1614 is cross-revision.
120454  116036  	FDV ,X 36
120455  004007  	STA 7
120456  117107  	FDV I ,X 107
120457  000000  	STZ 0
120460  000000  	STZ 0
120461  000000  	STZ 0

; ---- Region B: CIBUF ring-buffer clear worker (SINTRAN-DATA_commoncode, load 0) ----
; Shared clear-buffer worker. CIBUF (044120B) = input ring entry (MON 13B);
; COBUF (044125B) = output ring entry (MON 14B ClearOutBuffer). Prologue at
; 044113B stages the datafield; the two entries select input vs output side.
044113  054600  	LDX ,B -200		; X <- (B-200): base ptr into device/buffer table
044114  150404  	POF			; paging/interrupts off -> atomic datafield read
044115  026000  	LDD ,X 0		; load double word (A,D) from table entry X+0
044116  150410  	PON			; restore
044117  171377  	SAT -1			; T <- -1  (scan sentinel / mask seed)
044120  142065  	SKP IF DA UEQ ST	; CIBUF entry: skip next if (D unequal T)
044121  124031  	JMP 31		; -> 044152 (equal -> fall out to tail)
044122  175365  	BSKP ONE 160 DA		; test a status bit in D
044123  124014  	JMP 14		; -> 044137 (bit clear -> second arm)
044124  146115  	RADD CLD SD DA
044125  070073  	AND 73			; COBUF entry (MON 14B): mask
044126  050073  	LDT 73
044127  143465  	SKP IF DA MLST ST	; skip if D magnitude-less-than T
044130  124006  	JMP 6		; -> 044136
044131  060724  	ADD ,B -54		; update counter/pointer at (B-54)
044132  004724  	STA ,B -54
044133  175260  	BSKP ONE SSC		; test single-shift-carry condition
044134  124002  	JMP 2		; -> 044136
044135  135065  	JPL I 65		; -> 044222 (CALL shared reset routine, indirect)
044136  124012  	JMP 12		; -> 044150 (rejoin loop advance)
044137  175355  	BSKP ONE 150 DA
044140  124010  	JMP 10		; -> 044150
044141  146115  	RADD CLD SD DA
044142  070056  	AND 56
044143  060762  	ADD ,B -16
044144  004762  	STA ,B -16
044145  175260  	BSKP ONE SSC
044146  124002  	JMP 2		; -> 044150
044147  135053  	JPL I 53		; -> 044222 (CALL same shared reset routine, indirect)
044150  173402  	AAX 2			; advance table cursor X by 2 words (one entry)
044151  124343  	JMP -35		; -> 044114 (back to top: next atomic LDD ,X)
044152  044051  	LDA 51			; tail: load result/status words (exit path)
044153  054051  	LDX 51
044154  006014  	STA ,X 14
044155  044044  	LDA 44
044156  052015  	LDT ,X 15
