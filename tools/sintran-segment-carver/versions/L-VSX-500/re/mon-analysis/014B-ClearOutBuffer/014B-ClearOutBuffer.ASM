; ============================================================================
; MON 14B (octal) ClearOutBuffer - real SINTRAN L disassembly
; ONE region (the dispatch is fall-through; see README Code location):
;   COBUF clear-output-buffer worker  COBUF @044125B  in resident image
;   SINTRAN-DATA_commoncode (load 0B, big-endian).
;   COBUF (MON 14B) and CIBUF (MON 13B) are two labelled entries INSIDE this
;   one shared clear-buffer routine; the loop top is 044114B.
;   (No level-14 stub: GOTAB[14B]=000000; A->this body bridge is the uncarved
;    resident CALLPROC - not in these bytes.)
; Addresses and values are octal.
; ============================================================================

; ---- Region: shared CIBUF/COBUF clear-buffer worker (SINTRAN-DATA_commoncode) ----
044113  054600  	LDX ,B -200		; X <- datafield base (B-relative)
044114  150404  	POF			; <== loop re-entry; interrupts/paging off
044115  026000  	LDD ,X 0		; load 2-word buffer descriptor (A,D)
044116  150410  	PON
044117  171377  	SAT -1			; T <- -1 (sentinel)
044120  142065  	SKP IF DA UEQ ST	; <== CIBUF entry (MON 13B)
044121  124031  	JMP 31		; -> 044152 (finalize/exit)
044122  175365  	BSKP ONE 160 DA
044123  124014  	JMP 14		; -> 044137 (input-buffer clear block)
044124  146115  	RADD CLD SD DA
044125  070073  	AND 73			; <== COBUF entry (MON 14B): mask control word
044126  050073  	LDT 73
044127  143465  	SKP IF DA MLST ST
044130  124006  	JMP 6		; -> 044136 (skip clear, join tail)
044131  060724  	ADD ,B -54		; output-buffer pointer cell
044132  004724  	STA ,B -54
044133  175260  	BSKP ONE SSC
044134  124002  	JMP 2		; -> 044136
044135  135065  	JPL I 65		; -> 044222 (indirect shared drain worker)
044136  124012  	JMP 12		; -> 044150 (loop tail)
044137  175355  	BSKP ONE 150 DA
044140  124010  	JMP 10		; -> 044150
044141  146115  	RADD CLD SD DA
044142  070056  	AND 56
044143  060762  	ADD ,B -16		; input-buffer pointer cell
044144  004762  	STA ,B -16
044145  175260  	BSKP ONE SSC
044146  124002  	JMP 2		; -> 044150
044147  135053  	JPL I 53		; -> 044222 (indirect shared drain worker)
044150  173402  	AAX 2			; next buffer descriptor
044151  124343  	JMP -35		; -> 044114 (reload, continue scan)
044152  044051  	LDA 51			; finalize / exit path
044153  054051  	LDX 51
044154  006014  	STA ,X 14
044155  044044  	LDA 44
044156  052015  	LDT ,X 15		; carved window ends here
