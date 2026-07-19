; GENERATED single-region slice of ../../segments-ref/026-S3IMPIT/ (canonical
;   bin: ../../../segments/026-S3IMPIT.bin, load base 32000B) at
;   byte-offset 72970 length 104 - regenerate from canonical, do not hand-edit.
;   Slice sha256 == 504B-OutputString.bin (329b2220...c9fd2aec).
;
; MON 504B (octal) - OutputString (DVOUTS / NPL NOUTSTR) - SINTRAN III VSX/500 L
;   - real SINTRAN L bytes.  Source: 026-S3IMPIT.bin, load 32000B.
; OSTRS = the level-12 output-string restart worker, entry 141205B.
;   Executable body closes at EXIT 141266B; internal continuation label PT5RS at
;   141256B (per L07 SYMBOL-2-LIST). 141267B..141270B are the trailing next-region
;   entry + a pointer word (@141270B = 141205B, points back to OSTRS), carved in
;   only to keep the slice ONE contiguous 52-word block.
; Addresses and values are octal.
;
141205  146145  	RADD CLD SL DA		; OSTRS entry
141206  004376  	STA -2
141207  135023  	JPL I 23		; -> 141232 (indirect via pool)
141210  135023  	JPL I 23		; -> 141233 (indirect via pool)
141211  146153  	RADD CLD SA DB
141212  051022  	LDT I 22
141213  173437  	AAX 37
141214  143300  	LDATX
141215  173741  	AAX -37
141216  050017  	LDT 17
141217  140065  	SKP IF DA EQL ST
141220  124007  	JMP 7		; -> 141227 (in-window)
141221  051013  	LDT I 13
141222  173534  	AAX 134
141223  143302  	LDDTX
141224  173644  	AAX -134
141225  015011  	STX I 11
141226  135011  	JPL I 11		; -> 141237 (indirect)
141227  135011  	JPL I 11		; -> 141240 (indirect)
141230  135011  	JPL I 11		; -> 141241 (indirect)
141231  125353  	JMP I -25		; -> 141204 (indirect)
141232  023624  	STD I ,B ,X -154	; pointer/literal pool word (data)
141233  000215  	STZ -163		; pool word (data)
141234  004654  	STA ,B -124		; pool word (data)
141235  000511  	STZ ,B 111		; pool word (data)
141236  011260  	STT I -120		; pool word (data)
141237  141277  	RMPY SX DX
141240  023025  	STD I ,X 25
141241  145751  	RORA CM1 CLD SA DD
141242  150401  	IOF			; guard device I/O sequence
141243  146175  	RADD CLD SX DA
141244  153523  	IRW 120 DB
141245  044006  	LDA 6
141246  153522  	IRW 120 DP
141247  044005  	LDA 5
141250  150306  	MST PID
141251  150402  	ION
141252  125003  	JMP I 3		; -> 141255 (indirect)
141253  077230  	ORA I ,X -150
141254  002000  	STZ ,X 0
141255  032000  	STF ,X 0
141256  146175  	RADD CLD SX DA		; PT5RS (SYMBOL-2-LIST): continuation
141257  153547  	IRW 140 DX
141260  044007  	LDA 7
141261  153544  	IRW 140 DL
141262  044006  	LDA 6
141263  153542  	IRW 140 DP
141264  044005  	LDA 5
141265  150306  	MST PID
141266  146142  	EXIT			; OSTRS worker returns here
141267  135075  	JPL I 75		; -> 141364 (trailing: next-region entry)
141270  141205  	RMPY 0 DA		; trailing pointer word = 141205B (-> OSTRS)
