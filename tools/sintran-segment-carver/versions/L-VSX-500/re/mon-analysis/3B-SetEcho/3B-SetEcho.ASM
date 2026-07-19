; ============================================================================
; MON 3B (octal) ECHOM - real SINTRAN L07 disassembly
;
; OVERLAY RESOLVED 2026-07-13. An intermediate pass could not read a coherent
; body: at 044540B the 003-S3CP overlay holds ASCII command-name text (not code).
; That was the WRONG overlay. The real ECHOM worker is coherent code in
; 026-S3IMPIT (the monitor-PIT overlay) - confirmed by its use of MOVEW block
; copies bracketed by BSET ONE/ZRO SSPTM (the cross-page-table switch), which is
; monitor-level machinery and belongs in the monitor-PIT view.
;
; Dispatch (all hops carved; see README):
;   MON 3B -> ENT14 072167B -> GOTAB[3B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[3B] @005623B = 044540B = ECHOM -> worker (below).
;
; Worker home: 026-S3IMPIT (load 32000B), symbol ECHOM=044540B (SYMBOL-1-LIST).
; Same bytes in 017-S3SMPIT (swap twin). Addresses/values octal.
;
; NOTE on the name: the old folder glossed MON 3B as "SetEcho". The carved body
; is a page-table-crossing block-copy routine, which does not obviously match a
; "set terminal echo" primitive - the exact function is left INFERRED (see README).
; ============================================================================

; ---- MCTAB[3B] slot (044-S3IDPIT, load 4000B) ----
005623  044540  	; MCTAB[3B] = ECHOM = 044540B   (dd: byteoff 1830 = 49 60)

; ---- ECHOM worker body (026-S3IMPIT, load 32000B) ----
; Entry byte dd-verified: byteoff (044540-032000)*2 = 10944 -> 59 2a = 054452.
044540  054452  	LDX ,B 52		; X := frame[52] (source descriptor)
044541  147007  	RADD ADC 0 DX
044542  170400  	SAA 0
044543  146151  	RADD CLD SA DD		; (SMFIL: internal fall-in label)
044544  146031  	RADD SB DD
044545  143102  	MOVEW			; block move #1
044546  170401  	SAA 1
044547  004432  	STA ,B 32
044550  170401  	SAA 1
044551  146154  	RADD CLD SA DL
044552  050453  	LDT ,B 53
044553  173021  	AAT 21
044554  054452  	LDX ,B 52
044555  147007  	RADD ADC 0 DX
044556  170432  	SAA 32
044557  146151  	RADD CLD SA DD
044560  146031  	RADD SB DD
044561  143102  	MOVEW			; block move #2
044562  024462  	LDD ,B 62
044563  135030  	JPL I 30		; -> 044613 (helper)
044564  044464  	LDA ,B 64
044565  131021  	JAZ 21			; -> 044606
044566  170401  	SAA 1
044567  146154  	RADD CLD SA DL
044570  024452  	LDD ,B 52
044571  171020  	SAT 20
044572  146061  	RADD ST DD
044573  147005  	RADD ADC 0 DA
044574  171032  	SAT 32
044575  146036  	RADD SB DT
044576  143106  	MOVEW			; block move #3
044577  146107  	RADD CLD 0 DX
044600  170401  	SAA 1
044601  050432  	LDT ,B 32
044602  174200  	BSET ONE SSPTM		; switch page-table (cross-PIT access)
044603  135011  	JPL I 11		; -> 044614 (helper, under alt page-table)
044604  124000  	JMP 0			; -> 044604 (wait/spin placeholder)
044605  174000  	BSET ZRO SSPTM		; restore page-table
044606  040263  	MIN -115		; --- return path ---
044607  144400  	RAND 0 0
044610  125261  	JMP I -117		; -> 044471 (return)
