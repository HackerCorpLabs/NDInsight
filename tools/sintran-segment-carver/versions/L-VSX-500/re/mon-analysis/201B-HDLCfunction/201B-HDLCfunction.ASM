; ============================================================================
; MON 201B (octal) HDLCfunction (HDLC) - real SINTRAN L disassembly
;
; MON 201B is a FALL-THROUGH call: GOTAB[201] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. HDLC line-driver function dispatcher   HDLC @103112B in segment
;      025-S3IRPIT (load 32000B), the SYMBOL-2-LIST code overlay.
;      HDLC is a labelled entry inside the PIO / X.21 line-driver module
;      (neighbours PIO02, ENSTA, PIO03=103122B, PIO04=103240B). The body reads
;      the caller Func code, validates it (returning HDLC status codes on error),
;      and either SENDS a Driver Control Block (DCB) to the driver with STATX
;      physical stores, or RECEIVES a DCB with LDATX physical loads - which is
;      the byte-level reason HDLC is the HDLCfunction worker (send/receive DCB).
;   (the fall-through -> HDLC bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; The window below (103112B..103237B) is the HDLC entry, function validation,
; the SEND-DCB path (returns via JMP I ,B 57) and the head of the RECEIVE-DCB
; path; the receive tail continues into the adjacent PIO04=103240B driver label
; (one shared line-driver module). Addresses and values are octal. Bytes are
; from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region B: HDLC line-driver function dispatcher (025-S3IRPIT, load 32000B) ----
; HDLC = 103112B (SYMBOL-2-LIST); PIO03=103122B and PIO04=103240B are internal
; labels of the same driver module. Real executable code.
103112  146145  	RADD CLD SL DA		; HDLC entry: A := L (save return link)
103113  004457  	STA ,B 57		; stash return link in frame [B+57]
103114  044440  	LDA ,B 40		; A := frame[B+40] (state / DevNo cell)
103115  171377  	SAT -1			; T := -1
103116  142065  	SKP IF DA UEQ ST	; skip if A != -1
103117  124020  	JMP 20		; -> 103137 (state already set: main path)
103120  044441  	LDA ,B 41		; (A == -1) first-use init path
103121  004447  	STA ,B 47
103122  044415  	LDA ,B 15		; PIO03 label (internal)
103123  172777  	AAA -1
103124  156577  	SHA ZIN SHR 1
103125  060414  	ADD ,B 14
103126  146151  	RADD CLD SA DD
103127  044414  	LDA ,B 14
103130  146137  	RADD CLD SB DX
103131  050441  	LDT ,B 41
103132  173442  	AAX 42
103133  135150  	JPL I 150		; -> deref [103303] (driver init worker)
103134  135150  	JPL I 150		; -> deref [103304]
103135  170777  	SAA -1
103136  004440  	STA ,B 40		; mark state initialised
103137  044433  	LDA ,B 33		; main path: A := function selector cell
103140  171000  	SAT 0			; T := 0
103141  140065  	SKP IF DA EQL ST	; skip if A == 0 (Func 1 = RECEIVE?)
103142  124062  	JMP 62		; -> 103224 (RECEIVE-DCB path)
103143  044436  	LDA ,B 36		; SEND-DCB path: A := used-size field
103144  130003  	JAP 3		; -> 103147 (size ok)
103145  171373  	SAT -5			; T := status 5 (illegal DCB size)
103146  125457  	JMP I ,B 57		; -> return via saved link (error 5)
103147  050437  	LDT ,B 37		; T := max-size field
103150  143056  	SKP IF DT LST SA	; skip if T < A  (max < used ?)
103151  124003  	JMP 3		; -> 103154
103152  171371  	SAT -7			; T := status 7 (max size < used size)
103153  125457  	JMP I ,B 57		; -> return via saved link (error 7)
103154  054131  	LDX 131			; obtain a driver buffer
103155  146037  	RADD SB DX
103156  170412  	SAA 12
103157  060437  	ADD ,B 37
103160  135126  	JPL I 126		; -> deref [103306] (buffer-alloc worker)
103161  135126  	JPL I 126		; -> deref [103307]
103162  131403  	JAF 3		; -> 103165 (buffer obtained)
103163  171374  	SAT -4			; T := status 4 (no buffer)
103164  125457  	JMP I ,B 57		; -> return via saved link (error 4)
103165  004460  	STA ,B 60		; buffer address saved
103166  146157  	RADD CLD SA DX		; X := A (buffer address)
103167  044322  	LDA -56
103170  172401  	AAA 1
103171  130003  	JAP 3		; -> 103174
103172  170401  	SAA 1
103173  004316  	STA -62
103174  004315  	STA -63
103175  004456  	STA ,B 56
103176  050441  	LDT ,B 41		; T := DevNo/bank selector for physical transfer
103177  143314  	STATX			; phys[EL] := A  (write DCB word to buffer)
103200  044437  	LDA ,B 37
103201  143334  	STATX			; phys[EL] := A  (DCB word)
103202  044436  	LDA ,B 36
103203  143324  	STATX			; phys[EL] := A  (DCB word)
103204  044104  	LDA 104
103205  143304  	STATX			; phys[EL] := A  (DCB word)
103206  044435  	LDA ,B 35
103207  004446  	STA ,B 46
103210  050436  	LDT ,B 36
103211  146161  	RADD CLD ST DD
103212  173405  	AAX 5
103213  014450  	STX ,B 50
103214  135075  	JPL I 75		; -> deref [103311] (queue DCB to driver)
103215  150401  	IOF			; I/O interrupts off (guard driver queue)
103216  054460  	LDX ,B 60
103217  135073  	JPL I 73		; -> deref [103312] (link DCB / start driver)
103220  150402  	ION			; I/O interrupts on
103221  135410  	JPL I ,B 10		; -> deref [103231] (post-send worker)
103222  146106  	RADD CLD 0 DT		; T := 0 (status OK)
103223  125457  	JMP I ,B 57		; -> return via saved link (success)
103224  150401  	IOF			; RECEIVE-DCB path: I/O interrupts off
103225  135066  	JPL I 66		; -> deref [103313] (dequeue DCB from driver)
103226  124072  	JMP 72		; -> 103320 (no DCB in queue -> status)
103227  150402  	ION			; I/O interrupts on
103230  014460  	STX ,B 60
103231  050441  	LDT ,B 41		; T := DevNo/bank selector
103232  143310  	LDATX			; A := phys[EL]  (read DCB word from buffer)
103233  004456  	STA ,B 56
103234  143320  	LDATX			; A := phys[EL]  (read DCB word)
103235  146156  	RADD CLD SA DT
103236  044433  	LDA ,B 33
103237  172776  	AAA -2			; (receive tail continues into PIO04=103240B)
; 103240 = PIO04 boundary (next symbol); the RECEIVE-DCB tail runs on inside the
; same shared line-driver module and is not part of this excerpt.
