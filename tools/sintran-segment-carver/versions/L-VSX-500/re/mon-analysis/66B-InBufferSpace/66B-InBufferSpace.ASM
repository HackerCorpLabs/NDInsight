; ============================================================================
; MON 66B (octal) InBufferSpace (ISIZE) - real SINTRAN L disassembly
; Returns the number of bytes currently held in a device's INPUT buffer.
; This is the input-buffer twin of MON 67B OutBufferSpace (OSIZE): ISIZE and
; OSIZE share the same per-device scan loop body in resident commoncode.
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[66B] = 000000  -> FALL-THROUGH (no direct GOTAB handler word);
;   the level-14 handler is reached via the resident MFELL/CALLPROC path,
;   which is NOT present in any carved segment (uncarved bridge).
;   Candidate worker body: ISIZE @044203B in resident SINTRAN-DATA_commoncode
;   (load 0). Real SINTRAN L bytes, but NOT statically linked from GOTAB
;   (the MON 66 -> ISIZE hop crosses the uncarved bridge - see README caveats).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- ISIZE worker (SINTRAN-DATA_commoncode, load 0) ----
; ISIZE = 044203B (SYMBOL-1-LIST). Its own 4-word prologue (044203-044214)
; ends with JMP 20 -> 044234, jumping INTO the shared device-scan loop that
; OSIZE (044231) also runs. Words 044215-044230 are data/pointer cells
; (skipped by ISIZE flow; they carry the small data symbols T1P01=044225,
; DFRRT=044226); 044231-044233 is OSIZE's own separate 3-word entry prologue.
; The scan loop 044234-044342 is SHARED by both calls.

; -- ISIZE entry prologue (044203-044214) --
044203  047024  	LDA I ,X 24			; ISIZE entry
044204  064024  	SUB 24
044205  135024  	JPL I 24		; -> deref [044231]  (INDIRECT, runtime pointer)
044206  060641  	ADD ,B -137
044207  004642  	STA ,B -136
044210  044022  	LDA 22
044211  060022  	ADD 22
044212  156566  	SHA ZIN SHR 12
044213  004400  	STA ,B 0
044214  124020  	JMP 20		; -> 044234  (DIRECT, into the SHARED scan loop)

; -- data / pointer cells 044215-044230 (skipped by ISIZE flow) --
044215  007777  	STA I ,B ,X -1		; DATA
044216  000276  	STZ -102		; DATA
044217  042150  	MIN ,X 150		; DATA
044220  077777  	ORA I ,B ,X -1		; DATA
044221  002000  	STZ ,X 0			; DATA
044222  000215  	STZ -163		; DATA
044223  172000  	AAB 0			; DATA
044224  042423  	MIN ,X ,B 23		; DATA
044225  000033  	STZ 33			; T1P01 (data cell)
044226  026000  	LDD ,X 0			; DFRRT (data cell)
044227  000044  	STZ 44			; DATA
044230  004000  	STA 0			; DATA

; -- OSIZE's own entry prologue (044231-044233); ISIZE does NOT run these --
044231  060730  	ADD ,B -50			; OSIZE entry (MON 67B); shown for context
044232  061205  	ADD I -173
044233  001777  	STZ I ,B -1

; -- SHARED per-device scan loop (044234-044342) - run by ISIZE and OSIZE --
044234  000776  	STZ ,B -2			; ISIZE enters here (from 044214 JMP 20)
044235  044107  	LDA 107
044236  150105  	TRR IIE
044237  150005  	TRA IIC
044240  054776  	LDX ,B -2			; loop head: X = index
044241  027104  	LDD I ,X 104		; load double-word device-table entry
044242  171377  	SAT -1
044243  142065  	SKP IF DA UEQ ST		; end-of-table sentinel test
044244  124077  	JMP 77		; -> 044343  (exit loop -> data/return block)
044245  131072  	JAZ 72		; -> 044337  (skip empty slot -> loop advance)
044246  020745  	STD ,B -33			; save device-table pointer
044247  146157  	RADD CLD SA DX
044250  052375  	LDT ,X -3
044251  173002  	AAT 2
044252  150415  	IOXT			; device I/O: read buffer status
044253  146151  	RADD CLD SA DD
044254  150005  	TRA IIC
044255  131027  	JAZ 27		; -> 044304  (branch on device class)
044256  044746  	LDA ,B -32
044257  070067  	AND 67
044260  146156  	RADD CLD SA DT
044261  044746  	LDA ,B -32
044262  156572  	SHA ZIN SHR 6
044263  146157  	RADD CLD SA DX
044264  056417  	LDX ,X ,B 17
044265  146067  	RADD ST DX
044266  146467  	RADD AD1 ST DX
044267  150404  	POF			; interrupts off
044270  026000  	LDD ,X 0			; read buffer descriptor (double word)
044271  150410  	PON			; interrupts on
044272  050745  	LDT ,B -33
044273  140065  	SKP IF DA EQL ST
044274  124007  	JMP 7		; -> 044303
044275  140061  	SKP IF DD EQL ST
044276  124005  	JMP 5		; -> 044303
044277  150404  	POF
044300  002000  	STZ ,X 0			; buffer empty: zero the pair
044301  002001  	STZ ,X 1
044302  150410  	PON
044303  124034  	JMP 34		; -> 044337
044304  175261  	BSKP ONE 60 DD
044305  124011  	JMP 11		; -> 044316
044306  146115  	RADD CLD SD DA
044307  156570  	SHA ZIN SHR 10
044310  156406  	SHA ZIN 6
044311  146151  	RADD CLD SA DD
044312  170477  	SAA 77
044313  054745  	LDX ,B -33
044314  006027  	STA ,X 27
044315  124014  	JMP 14		; -> 044331
044316  175301  	BSKP ONE 100 DD
044317  124002  	JMP 2		; -> 044321
044320  135027  	JPL I 27		; -> deref [044347]  (INDIRECT, pointer block)
044321  146115  	RADD CLD SD DA
044322  156570  	SHA ZIN SHR 10
044323  070025  	AND 25
044324  156406  	SHA ZIN 6
044325  146151  	RADD CLD SA DD
044326  044023  	LDA 23
044327  054745  	LDX ,B -33
044330  006027  	STA ,X 27
044331  146015  	RADD SD DA			; assemble the byte count into A
044332  144015  	SWAP SD DA
044333  054776  	LDX ,B -2
044334  174000  	BSET ZRO SSPTM
044335  023015  	STD I ,X 15		; store result
044336  174200  	BSET ONE SSPTM
044337  044776  	LDA ,B -2			; loop advance: index += 2
044340  172402  	AAA 2
044341  004776  	STA ,B -2
044342  124276  	JMP -102		; -> 044240  (back to loop head)

; -- data / pointer block (044343-044352); JMP 10 at 044343 jumps over it --
044343  124010  	JMP 10		; -> 044353  (jump over data block)
044344  000200  	STZ -200		; DATA
044345  171115  	SAT 115		; DATA
044346  000077  	STZ 77			; DATA
044347  000215  	STZ -163		; DATA: pointer word = 000215B (deref by JPL I 044320)
044350  000375  	STZ -3			; DATA
044351  000377  	STZ -1			; DATA
044352  036127  	LDF ,X 127		; DATA
; (044353.. is the shared tail scan documented under 067B-OutBufferSpace;
;  role for the input path is UNVERIFIED and not repeated here.)
