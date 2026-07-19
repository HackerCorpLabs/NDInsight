; ============================================================================
;  005B-WDATA  ->  MON 60 subfunction WDATA = 5B (0x05 = 5)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146327 SAA 5 ; 146330 JMP I 1 ; 146331 = 146244 (gateway).
;  Four call sites: 002326, 002515 (rtn 002222); 055140 (rtn 055113); 056023 (rtn 055255).
; ============================================================================

; ---- Call site 002326 : ENTER-routine 002222 (framesize 000004) ------------
002306  024162  	LDD 162			; D := 32-bit const (logical DM addr, INFERRED)
002307  020674  	STD ,B -104
002310  146135  	RADD CLD SB DA
002311  172674  	AAA -104
002312  054602  	LDX ,B -176
002313  006006  	STA ,X 6		; param1 := &(B-104)
002314  024611  	LDD ,B -167
002315  154633  	SAD 33
002316  020676  	STD ,B -102
002317  146135  	RADD CLD SB DA
002320  172676  	AAA -102
002321  006007  	STA ,X 7		; param2 := &(B-102)
002322  170403  	SAA 3
002323  144151  	SWAP CLD SA DD
002324  050146  	LDT 146
002325  032010  	STF ,X 10		; param3 := F register (3-word)
002326  135145  	JPL I 145		; -> 002473  MON60 WDATA (5B) thunk=146327
002327  125615  	JMP I ,B -163		; -> 002144  callsite+1 = ERROR (frame dispatch)
; (002330 = callsite+2 = SUCCESS)
002473  146327  	<thunk WDATA (SAA 5)>	; bank1[002473] = 146327

; ---- Call site 002515 : ENTER-routine 002222 (reached via JMP 27 from 002466)
002450  024043  	LDD 43			; D := 32-bit const
002451  020674  	STD ,B -104
002452  146135  	RADD CLD SB DA
002453  172674  	AAA -104
002454  054602  	LDX ,B -176
002455  006006  	STA ,X 6		; param1 := &(B-104)
002456  146135  	RADD CLD SB DA
002457  172670  	AAA -110
002460  006007  	STA ,X 7		; param2 := &(B-110)
002461  170400  	SAA 0
002462  144151  	SWAP CLD SA DD
002463  146136  	RADD CLD SB DT
002464  173216  	AAT -162
002465  032010  	STF ,X 10		; param3 := F register (3-word)
002466  124027  	JMP 27			; -> 002515
002515  135356  	JPL I -22		; -> 002473  MON60 WDATA (5B) thunk=146327
002516  125615  	JMP I ,B -163		; -> 002333  callsite+1 = ERROR (frame dispatch)
; (002517 = callsite+2 = SUCCESS)

; ---- Call site 055140 : ENTER-routine 055113 (framesize 000007) ------------
055123  154760  	SAD SHR 20
055124  020613  	STD ,B -165
055125  170401  	SAA 1
055126  005020  	STA I 20
055127  146135  	RADD CLD SB DA
055130  172613  	AAA -165
055131  054602  	LDX ,B -176
055132  006006  	STA ,X 6		; param1 := &(B-165)
055133  146135  	RADD CLD SB DA
055134  172606  	AAA -172
055135  006007  	STA ,X 7		; param2 := &(B-172)
055136  034610  	LDF ,B -170
055137  032010  	STF ,X 10		; param3 := F register (3-word)
055140  135007  	JPL I 7			; -> 055147  MON60 WDATA (5B) thunk=146327
055141  135004  	JPL I 4			; -> 055145  callsite+1 = ERROR (ptr=177327)
055142  135006  	JPL I 6			; -> 055150  callsite+2 = SUCCESS (ptr=177335)
055147  146327  	<thunk WDATA (SAA 5)>	; bank1[055147] = 146327

; ---- Call site 056023 : ENTER-routine 055255 (framesize 000302) ------------
056012  146135  	RADD CLD SB DA
056013  172610  	AAA -170
056014  054602  	LDX ,B -176
056015  006006  	STA ,X 6		; param1 := &(B-170)
056016  146135  	RADD CLD SB DA
056017  172612  	AAA -166
056020  006007  	STA ,X 7		; param2 := &(B-166)
056021  034623  	LDF ,B -155
056022  032010  	STF ,X 10		; param3 := F register (3-word)
056023  135015  	JPL I 15		; -> 056040  MON60 WDATA (5B) thunk=146327
056024  135003  	JPL I 3			; -> 056027  callsite+1 = ERROR (ptr=177327)
056025  135014  	JPL I 14		; -> 056041  callsite+2 = SUCCESS
056040  146327  	<thunk WDATA (SAA 5)>	; bank1[056040] = 146327
