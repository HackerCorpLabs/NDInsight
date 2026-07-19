; ============================================================================
; MON 51B (octal) DMACBreakpoint - real SINTRAN L disassembly
; Two regions (dispatch is level-14 GOTAB -> stub -> worker; see README):
;   A. GOTAB[51] level-14 entry stub  F1633 @121147B  in segment 025-S3IRPIT (load 32000B)
;   B. debugger worker body           117107B (REDOM/PRIMH) in segment 025-S3IRPIT (load 32000B)
;      (the worker's tail JMP I 2 escapes to resident 010331B - not in any carved segment)
; Disassembly: nd100-dis -a -o -b 13312 from the canonical 025-S3IRPIT.bin.
; Addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[51] entry stub F1633 (025-S3IRPIT), 121147B..121173B ----
; Main path: JMP 4 skips 3 alt-entry/pointer words, body loads A/B/X/D, then
; JMP I 33 leaves through pointer word 121214B = 117107B (worker, Region B).
; Words 121150..121152 and 121162..121200 are alternate-entry / fall paths for
; OTHER F16xx debugger-family callers - NOT executed on the MON 51B path.
121147  124004  	JMP 4		; -> 121153   HEAD: skip 3 words, enter body
121150  135042  	JPL I 42		; -> 121212   alt-entry/ptr word (SKIPPED on 51B path)
121151  124024  	JMP 24		; -> 121175   alt-entry (SKIPPED)
121152  124350  	JMP -30		; -> 121122   alt-entry (SKIPPED)
121153  045032  	LDA I 32		; body: A <- mem[ptr@121205 = 061157B]  (in-seg data)
121154  146153  	RADD CLD SA DB	; A -> B
121155  045036  	LDA I 36		; A <- mem[ptr@121213 = 004007B]  (resident data, out-of-window)
121156  146157  	RADD CLD SA DX	; A -> X
121157  044421  	LDA ,B 21		; A <- caller frame  B+21  (input param)
121160  146151  	RADD CLD SA DD	; A -> D
121161  125033  	JMP I 33		; -> 121214   MAIN TRANSFER: JMP I ptr@121214 = 117107B (worker)
121162  146137  	RADD CLD SB DX	; --- alt/fall path below, not on 51B main path ---
121163  050421  	LDT ,B 21
121164  146163  	RADD CLD ST DB
121165  056020  	LDX ,X 20
121166  004412  	STA ,B 12
121167  140007  	SKP IF DX EQL 0
121170  124002  	JMP 2		; -> 121172
121171  004410  	STA ,B 10
121172  040407  	MIN ,B 7
121173  144400  	RAND 0 0		; pad to fixed 25B-word stub slot
121174  125015  	JMP I 15		; -> 121211   (F1634 = next stub begins here)

; ---- Region A pointer/constant pool (data words the stub dereferences) ----
; Disassembler renders these as bogus FDV/ADD/STZ - they are DATA, not code.
121205  061157  	; ptr for 121153 LDA I 32 -> value 061157B (in-segment 025 data)
121211  010341  	; ptr for 121174 JMP I 15 -> value 010341B (resident, out-of-window)
121212  116036  	; ptr for 121150 JPL I 42 -> value 116036B (in-segment 025 worker, alt path)
121213  004007  	; ptr for 121155 LDA I 36 -> value 004007B (resident data, out-of-window)
121214  117107  	; ptr for 121161 JMP I 33 -> value 117107B (worker, Region B) [VERIFIED]

; ---- Region B: debugger worker body (025-S3IRPIT), 117107B..117137B ----
; Reached by 121161 JMP I 33. Symbol at 117107B: REDOM / PRIMH (SYMBOL-2-LIST,
; conflicting). Tail JMP I 2 at 117137B escapes to resident 010331B (uncarved).
117107  144031  	SWAP SB DD
117110  044407  	LDA ,B 7		; caller-frame counter
117111  172777  	AAA -1		; decrement
117112  004407  	STA ,B 7
117113  144031  	SWAP SB DD
117114  054400  	LDX ,B 0		; chase descriptor via frame B+0
117115  046001  	LDA ,X 1
117116  065022  	SUB I 22
117117  131417  	JAF 17		; -> 117136
117120  055020  	LDX I 20
117121  046001  	LDA ,X 1
117122  175165  	BSKP ZRO 160 DA
117123  124013  	JMP 13		; -> 117136
117124  054400  	LDX ,B 0
117125  046004  	LDA ,X 4
117126  131410  	JAF 10		; -> 117136
117127  055011  	LDX I 11
117130  046001  	LDA ,X 1
117131  174375  	BSET ONE 170 DA
117132  006001  	STA ,X 1
117133  170402  	SAA 2
117134  054400  	LDX ,B 0
117135  006004  	STA ,X 4
117136  146113  	RADD CLD SD DB
117137  125002  	JMP I 2		; -> 117141   TAIL: JMP I ptr@117141 = 010331B (resident, UNVERIFIED)
