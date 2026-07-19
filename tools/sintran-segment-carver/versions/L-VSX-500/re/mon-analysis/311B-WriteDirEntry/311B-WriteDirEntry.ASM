; ============================================================================
; MON 311B (octal) WriteDirEntry (WDIEN) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version declared the call "misattributed
; (stub-routed)" (GOTAB[311B]=112030B -> DIA2 stub). That was the wrong dispatch
; model. MON calls take their worker from MCTAB@005620B (segment 044-S3IDPIT),
; NOT from that GOTAB (MFELL for 224 of 256 calls). MCTAB[311B] = 107106B =
; WDIEN, carved in 006-S3FS - a mode-select entry (SSM=1) into a body shared
; with GDIEN (107111B) and GNAEN (107114B).
;
; Dispatch (all hops carved; see README):
;   MON 311B -> ENT14 072167B -> GOTAB[311B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[311B] @006131B = 107106B = WDIEN -> worker (below).
;
; Worker home: 006-S3FS (load 26000B), symbol WDIEN=107106B (FILSYS-SYMBOLS).
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[311B] slot (026-S3IMPIT, load 32000B) ----
071544  072114  	; GOTAB[311B] = MFELL = 072114B   (dd: byteoff 32456 = 74 4c)

; ---- MCTAB[311B] slot (044-S3IDPIT, load 4000B) ----
006131  107106  	; MCTAB[311B] = WDIEN = 107106B   (dd: byteoff 2226 = 8e 46)

; ---- WDIEN worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (107106-026000)*2 = 50316 -> f8 b8 = 174270 (BSET ONE SSM).
107106  174270  	BSET ONE SSM		; WDIEN entry: SSM=1 (write mode)
107107  174020  	BSET ZRO SSK		;              SSK=0 (index lookup)
107110  124006  	JMP 6			; -> 107116 (shared body)
; --- sibling GDIEN entry (MON 244B) ---
107111  174020  	BSET ZRO SSK		; GDIEN: both bits 0 (read by index)
107112  174070  	BSET ZRO SSM
107113  124003  	JMP 3			; -> 107116
; --- sibling GNAEN entry (MON 245B) ---
107114  174220  	BSET ONE SSK		; GNAEN: SSK=1 (name lookup)
107115  174070  	BSET ZRO SSM
; --- shared body ---
107116  021112  	STD I 112		; prologue: save regs / set frame
107117  146145  	RADD CLD SL DA		; A := L (return link)
107120  146131  	RADD CLD SB DD		; D := B
107121  170106  	SAB 106			; new frame base offset
107122  135107  	JPL I 107		; -> 107231 (shared directory helper)
107123  175270  	BSKP ONE SSM		; test write mode
107124  124004  	JMP 4			; -> 107130 (not write)
107125  170401  	SAA 1
107126  004504  	STA ,B 104		; write flag := 1
107127  124002  	JMP 2			; -> 107131
107130  000504  	STZ ,B 104		; write flag := 0
107131  175220  	BSKP ONE SSK		; test name-lookup mode
107132  124004  	JMP 4			; -> 107136
107133  170401  	SAA 1
107134  004505  	STA ,B 105		; name-lookup flag := 1
107135  124002  	JMP 2			; -> 107137
107136  000505  	STZ ,B 105		; name-lookup flag := 0
107137  044505  	LDA ,B 105		; (body continues; see segments-ref) ...
