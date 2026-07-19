; ============================================================================
; MON 255B (octal) PIOCFunction - real SINTRAN L disassembly
;
; MON 255B is an ND-100 call (255B < 400B) with a ZERO GOTAB slot, i.e. a
; level-14 FALL-THROUGH.  There is therefore NO per-call F17xx entry stub to
; list (the usual "region A"); the only code that exists on disk for this call
; is the worker body:
;
;   Worker body   PIOCM @114120B  in segment 025-S3IRPIT  (load base 32000B)
;   region span   114120B..114241B  (control-flow closure, 82 words)
;                   - code 114120B..114215B (PIOCM entry + body + PIRET epilogue)
;                   - link-cell / data table 114216B..114241B
;   next symbol    EXEL @114242B  (bounds the carve)
;
; PIOCM is the coherent-code overlay.  The SAME symbol PIOCM=114120B also lands
; inside 026-S3IMPIT, but there those words are a repeating 8-word data/float
; record (000000 000000 177777 ...), NOT executable code - the coherent-vs-garbage
; contrast is the segment tie-breaker (see README "Honest caveats").
;
; The GOTAB[255] -> worker bridge is the uncarved resident CALLPROC second-level
; dispatch; it is NOT present in any carved segment (see README Honest caveats).
;
; Extracted verbatim from the byte-identity-checked whole-segment disassembly
; ../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm .  Addresses and values octal.
; >>> NAME lines are in-range SYMBOL-2-LIST labels (OPD65/66/67/77 are internal
;     labels inside the one PIOCM routine, not separate entry points).
; ============================================================================

; ---- Worker body: PIOCM parallel-I/O-controller function dispatcher ----
; ---- (025-S3IRPIT, load 32000B) ----
>>> PIOCM(SYMBOL-2-LIST) = 114120B
; entry prologue: save caller X and (copied) B through resident save cells
114120  015076  	STX I 76		; mem[@114216 = 007253] := X   (save X)
114121  146135  	RADD CLD SB DA		; A := B  (COPY B -> A)
114122  005075  	STA I 75		; mem[@114217 = 007254] := A   (save B)
>>> OPD65(SYMBOL-2-LIST) = 114123B
114123  046011  	LDA ,X 11		; A := mem[X+11]  (function/slot word from caller block)
114124  171377  	SAT -1			; T := -1
114125  140065  	SKP IF DA EQL ST	; skip if A == -1
114126  124002  	JMP 2		; -> 114130
114127  125071  	JMP I 71		; ptr @114220 = 114242 -> EXEL (func == -1)
114130  046010  	LDA ,X 10		; A := mem[X+10]  (second parameter word)
114131  135070  	JPL I 70		; ptr @114221 = 010376 -> resident worker (returns)
114132  131402  	JAF 2		; -> 114134  (A != 0)
114133  146115  	RADD CLD SD DA		; A := D
114134  131403  	JAF 3		; -> 114137  (A != 0)
>>> OPD66(SYMBOL-2-LIST) = 114135B
114135  170746  	SAA -32			; status -32B = Illegal LDN
114136  125064  	JMP I 64		; ptr @114222 = 114207 -> PIRET (error exit)
114137  146153  	RADD CLD SA DB		; B := A
114140  004357  	STA -21			; mem[P-21] := A
114141  044412  	LDA ,B 12		; A := mem[B+12]
114142  171173  	SAT 173			; T := 173B
114143  142065  	SKP IF DA UEQ ST	; skip if A != T
114144  124003  	JMP 3		; -> 114147
114145  170746  	SAA -32			; status -32B = Illegal LDN
114146  125055  	JMP I 55		; ptr @114223 = 114207 -> PIRET (error exit)
>>> OPD67(SYMBOL-2-LIST) = 114147B
114147  046011  	LDA ,X 11		; A := mem[X+11]  (function code)
114150  171020  	SAT 20			; T := 20B
114151  141065  	SKP IF DA GRE ST	; skip if A >= 20B (signed)
114152  124002  	JMP 2		; -> 114154
114153  124022  	JMP 22		; -> 114175  (func >= 20B: alternate range path)
114154  135050  	JPL I 50		; ptr @114224 = 000374 -> resident worker (returns)
114155  170404  	SAA 4			; A := 4
114156  150307  	MST PIE			; PIE |= A  (enable priority-interrupt bits)
114157  044340  	LDA -40			; A := mem[P-40]
114160  146153  	RADD CLD SA DB		; B := A
114161  055035  	LDX I 35		; X := mem[@... ] (indirect load of a block ptr)
114162  046011  	LDA ,X 11		; A := mem[X+11]  (function code, via new block base)
114163  171010  	SAT 10			; T := 10B
114164  143456  	SKP IF DT MLST SA	; skip if T <u A  (unsigned 10B < func)
114165  124003  	JMP 3		; -> 114170
114166  170754  	SAA -24			; status -24B = Illegal function code
>>> OPD77(SYMBOL-2-LIST) = 114167B
114167  125036  	JMP I 36		; ptr @114225 = 114207 -> PIRET (error exit)
; ---- cross-bank jump-table dispatch (func in range) ----
114170  174000  	BSET ZRO SSPTM		; select base bank (*1BANK: clear page-table bit)
114171  146157  	RADD CLD SA DX		; X := A  (scaled function index)
114172  047034  	LDA I ,X 34		; A := mem[ X + mem[@114226 = 114106] ]  (table read)
114173  174200  	BSET ONE SSPTM		; restore alternate bank (*2BANK: set page-table bit)
114174  146152  	RADD CLD SA DP		; P := A  (computed jump to dispatch target)
114175  172760  	AAA -20			; A := A - 20B   (entered from 114153, func >= 20B)
114176  171002  	SAT 2			; T := 2
114177  143056  	SKP IF DT LST SA	; skip if T < A (signed)
114200  124003  	JMP 3		; -> 114203
114201  170754  	SAA -24			; status -24B = Illegal function code
114202  125025  	JMP I 25		; ptr @114227 = 114207 -> PIRET (error exit)
114203  146052  	RADD SA DP		; P := P + A  (computed relative jump)
114204  125024  	JMP I 24		; ptr @114230 = 115532 -> resident worker
114205  125024  	JMP I 24		; ptr @114231 = 115527 -> resident worker
114206  125024  	JMP I 24		; ptr @114232 = 115642 -> resident worker
; ---- PIRET: common return / error-exit epilogue ----
>>> PIRET(SYMBOL-2-LIST) = 114207B
114207  055007  	LDX I 7			; X := mem[@... ] (restore/return block base)
114210  146173  	RADD CLD SX DB		; B := X
114211  054401  	LDX ,B 1		; X := mem[B+1]
114212  004411  	STA ,B 11		; mem[B+11] := A  (status word -> caller Status slot)
114213  150401  	IOF			; interrupts off (serialise driver hand-off)
114214  135017  	JPL I 17		; ptr @114233 = 010610 -> resident finaliser
114215  135017  	JPL I 17		; ptr @114234 = 010341 -> resident finaliser
; ---- 114216..114241: link-cell / data table (decodes as bogus instructions) ----
; each word below is a POINTER or save cell used by the code above, NOT code.
114216  007253  	; save cell for X   (target of STX I 76 @114120)
114217  007254  	; save cell for A   (target of STA I 75 @114122)
114220  114242  	; ptr -> EXEL          (JMP I 71  @114127)
114221  010376  	; ptr -> resident     (JPL I 70  @114131)
114222  114207  	; ptr -> PIRET        (JMP I 64  @114136)
114223  114207  	; ptr -> PIRET        (JMP I 55  @114146)
114224  000374  	; ptr -> resident     (JPL I 50  @114154)
114225  114207  	; ptr -> PIRET        (JMP I 36  @114167)
114226  114106  	; jump-table base     (LDA I ,X 34 @114172)
114227  114207  	; ptr -> PIRET        (JMP I 25  @114202)
114230  115532  	; ptr -> resident     (JMP I 24  @114204)
114231  115527  	; ptr -> resident     (JMP I 24  @114205)
114232  115642  	; ptr -> resident     (JMP I 24  @114206)
114233  010610  	; ptr -> resident     (JPL I 17  @114214)
114234  010341  	; ptr -> resident     (JPL I 17  @114215)
114235  144037  	; trailing data word
114236  000000  	; padding
114237  000000  	; padding
114240  000000  	; padding
114241  000000  	; padding
; ---- 114242B (EXEL) onward is the next routine, not a live MON 255B path ----
