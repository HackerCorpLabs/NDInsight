; ============================================================================
;  ABSMR  ->  MON 60 subfunction 032B  (PHYSICAL DATA MEMORY READ / abs mem read)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 032B = 0x1A = 26 dec.  Thunk 146426 (SAA 32; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <no. of bytes> <ND-500 addr.> <data area> <bytes returned>.
;  FOUR call sites, all resolving to thunk 146426 (verified per pointer word).
; ============================================================================

; ---------------------------------------------------------------------------
;  Site A : 012721  in standalone ENTER-routine 012700 (framesize 000012 = 10 dec).
;           Here <data area> (,X 10) is a single word; <bytes ret.> at ,X 11.
; ---------------------------------------------------------------------------
012703  024606  	LDD ,B -172
012704  154613  	SAD 13
012705  020613  	STD ,B -165
012706  044054  	LDA 54			; A := <no. of bytes> (P-relative)
012707  054602  	LDX ,B -176
012710  006006  	STA ,X 6		; param1 := <no. of bytes> (by value)
012711  146135  	RADD CLD SB DA
012712  172613  	AAA -165		; A := B-165
012713  006007  	STA ,X 7		; param2 := &(B-165) = <ND-500 addr.>
012714  044610  	LDA ,B -170
012715  006010  	STA ,X 10		; param3 := (B-170) = <data area> (by value)
012716  146135  	RADD CLD SB DA
012717  172615  	AAA -163		; A := B-163
012720  006011  	STA ,X 11		; param4 := &(B-163) = <bytes returned>
012721  135042  	JPL I 42		; -> ptr 012763 = thunk 146426  MON60 ABSMR
012722  135042  	JPL I 42		; callsite+1 ERROR   -> ptr 012764 = 177327 LEAVE(value)
012723  024615  	LDD ,B -163		; callsite+2 SUCCESS (continues)
   ; pool: 012763=146426 (thunk ABSMR), 012764=177327

; ---------------------------------------------------------------------------
;  Site B : 022616  in standalone ENTER-routine 022310 (framesize 000014 = 12 dec).
;           <data area> is a 3-word F descriptor at ,X 10; <bytes ret.> at ,X 13.
; ---------------------------------------------------------------------------
022577  024030  	LDD 30			; D := constant (P-relative)
022600  020620  	STD ,B -160
022601  146135  	RADD CLD SB DA
022602  172620  	AAA -160		; A := B-160
022603  054602  	LDX ,B -176
022604  006006  	STA ,X 6		; param1 := &(B-160) = <no. of bytes>
022605  044311  	LDA -67			; A := <ND-500 addr.> (P-relative)
022606  006007  	STA ,X 7		; param2 := <ND-500 addr.> (by value)
022607  044320  	LDA -60
022610  144151  	SWAP CLD SA DD
022611  050317  	LDT -61			; build <data area> descriptor
022612  032010  	STF ,X 10		; param3 := F = <data area> (3 words)
022613  146135  	RADD CLD SB DA
022614  172610  	AAA -170		; A := B-170
022615  006013  	STA ,X 13		; param4 := &(B-170) = <bytes returned>
022616  135013  	JPL I 13		; -> ptr 022631 = thunk 146426  MON60 ABSMR
022617  135004  	JPL I 4			; callsite+1 ERROR   -> ptr 022623
022620  124001  	JMP 1			; callsite+2 SUCCESS -> 022621 (JPL I -107 -> 022512)
   ; pool: 022631=146426 (thunk ABSMR)

; ---------------------------------------------------------------------------
;  Site C : 056364  in standalone ENTER-routine 056042 (framesize 000050 = 40 dec).
;           <data area> is a 3-word F at ,X 10; <bytes ret.> at ,X 13.
; ---------------------------------------------------------------------------
056344  044301  	LDA -77			; A := <no. of bytes> (P-relative)
056345  054602  	LDX ,B -176
056346  006006  	STA ,X 6		; param1 := <no. of bytes> (by value)
056347  146135  	RADD CLD SB DA
056350  172615  	AAA -163		; A := B-163
056351  006007  	STA ,X 7		; param2 := &(B-163) = <ND-500 addr.>
056352  170400  	SAA 0
056353  146151  	RADD CLD SA DD
056354  044610  	LDA ,B -170
056355  050102  	LDT 102			; build <data area> descriptor
056356  030635  	STF ,B -143
056357  034635  	LDF ,B -143
056360  032010  	STF ,X 10		; param3 := F = <data area> (3 words)
056361  146135  	RADD CLD SB DA
056362  172613  	AAA -165		; A := B-165
056363  006013  	STA ,X 13		; param4 := &(B-165) = <bytes returned>
056364  135075  	JPL I 75		; -> ptr 056461 = thunk 146426  MON60 ABSMR
056365  135253  	JPL I -125		; callsite+1 ERROR   -> ptr 056240
056366  125064  	JMP I 64		; callsite+2 SUCCESS -> ptr 056452
   ; pool: 056461=146426 (thunk ABSMR)

; ---------------------------------------------------------------------------
;  Site D : 131163  in standalone ENTER-routine 130475 (framesize 000207 = 135 dec).
;           <data area> is a 3-word F at ,X 10; <bytes ret.> at ,X 13.
; ---------------------------------------------------------------------------
131142  024164  	LDD 164
131143  020670  	STD ,B -110		; B-110 <- constant (later = <bytes returned>)
131144  024164  	LDD 164
131145  020712  	STD ,B -66		; B-66  <- <no. of bytes> constant
131146  146135  	RADD CLD SB DA
131147  172712  	AAA -66			; A := B-66
131150  054602  	LDX ,B -176
131151  006006  	STA ,X 6		; param1 := &(B-66) = <no. of bytes>
131152  044341  	LDA -37			; A := <ND-500 addr.> (P-relative)
131153  006007  	STA ,X 7		; param2 := <ND-500 addr.> (by value)
131154  044156  	LDA 156
131155  144151  	SWAP CLD SA DD
131156  050155  	LDT 155			; build <data area> descriptor
131157  032010  	STF ,X 10		; param3 := F = <data area> (3 words)
131160  146135  	RADD CLD SB DA
131161  172670  	AAA -110		; A := B-110
131162  006013  	STA ,X 13		; param4 := &(B-110) = <bytes returned>
131163  135151  	JPL I 151		; -> ptr 131334 = thunk 146426  MON60 ABSMR
131164  135323  	JPL I -55		; callsite+1 ERROR   -> 131107
131165  044672  	LDA ,B -106		; callsite+2 SUCCESS (continues)
   ; pool: 131334=146426 (thunk ABSMR)
