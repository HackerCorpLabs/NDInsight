; ============================================================================
; SINTRAN III L07 (L-VSX-500) - SCSI disk-layer FUNCTION 42 (READ FORMAT)
; Return-code logic + 5SCIN state machine + the caller-side branch it feeds.
;
; Segment : 065-S3SIPIT  (load base 32000B ; byte-identical twin 066-S3IIPIT)
; Source  : ../../segments/065-S3SIPIT.bin  (52224 words)
; Disasm  : nd100-dis -a -o -b 13312   over the byte-swapped (little-endian) image
;
; KEY REVISION FACT (byte-verified, previously mis-stated):
;   The SCSI DISK LAYER (IP-P2-SCSI-DISK: SCSDI/SCSID/INQUI/function 42) sits at
;   L07 virtual = NPL-source-label + 376B.  Proof: L07 symbol SCSID=062217,
;   NPL label SCSID=061621, 062217-061621 = 376B.  The SCSI *driver*
;   (IP-P2-SCSI-DRIV: SCLLD=067160 ...) is a SEPARATE unit and DOES align with
;   its NPL labels.  Earlier notes that disassembled the disk layer AT the NPL
;   labels (057074, 062214, 062503 ...) hit garbage (IOF/ION/FAD) - those were
;   the WRONG addresses.  Every address below is the corrected L07 address and
;   decodes coherently.
;
; All addresses/values octal.  Every opcode word below was confirmed to equal
; the segment bytes (spot dd's noted).  VERIFIED = read from bytes.
; ============================================================================

; L07 symbols (SYMBOL-2-LIST.SYMB.TXT):
;   SCSDI=057215  SCSID=062217  SCLLD=067160  SCINT=067247  SELEC=070165  INITO=070261
; Datafield offsets (FILSYS-SYMBOLS / SYMBOL-1-LIST L07):
;   SUTYP=23   5SCIN=7 (bit in SUTYP)   SCDFA=177772   SCWAQ=177721
; Disk-layer internal return codes carried in T/D (values proven at ERREX map, below):
;   0=OK   1=TYPER   4=ILAOP   5=BADPA   6=RQSER   11=NOCRC

; ----------------------------------------------------------------------------
; (A) FUNCTION-42 ENTRY  -- "IF A=42 THEN ... clear 5SCIN"
;     NPL 057074-057112  ->  L07 057472-057514
;     dd: word@057472 = 171042 (off 22132) ; word@057506 = 174075 (off 22156)  VERIFIED
; ----------------------------------------------------------------------------
057472  171042   SAT 42                 ; T := 42
057473  140065   SKP IF DA EQL ST       ; IF A = 42 ...
057474  124014   JMP 14 -> 057510       ;   ... else skip the func-42 preamble
057475  026015   LDD ,X 15              ; \ X.MEMAD =: MEMAD  (DMA target, double @15)
057476  020415   STD ,B 15              ; /
057477  046014   LDA ,X 14              ; \ X.ABFUN =: ABFUN
057500  004414   STA ,B 14              ; /
057501  000421   STZ ,B 21              ; 0   =: ABP31
057502  170500   SAA 100                ; \ 100 =: ABP32   (100B blocks probe span)
057503  004422   STA ,B 22              ; /
057504  054772   LDX ,B -6              ; X := SCDFA (unit datafield)
057505  046023   LDA ,X 23              ; A := X.SUTYP
057506  174075   BSET ZRO 70 DA         ; *** CLEAR bit 5SCIN in A   (force re-inquiry) ***
057507  006023   STA ,X 23              ; X.SUTYP := A          == SCDFA.SUTYP BZERO 5SCIN
057510  044762   LDA ,B -16             ; (fall-through) TACNS =: TACOU
057511  004763   STA ,B -15
057512  054772   LDX ,B -6              ; X := SCDFA =: B

; ----------------------------------------------------------------------------
; (B) RETRY loop -- automatic INQUIRY when 5SCIN clear, then TYPER guard
;     NPL 057116-057161  ->  L07 057514-057557
; ----------------------------------------------------------------------------
057514  044423   LDA ,B 23              ; A := SUTYP
057515  175075   BSKP ZRO 70 DA         ; IF SUTYP NBIT 5SCIN ...    (skip if 5SCIN set)
057516  124041   JMP 41 -> 057557       ;   already initialised -> skip inquiry
057517  046365   LDA ,X -13             ; A := X.SCOCW
057520  175165   BSKP ZRO 160 DA        ;   AND X.SCOCW NBIT 3SNTR (16) ...
057521  124036   JMP 36 -> 057557
057522  175305   BSKP ONE 100 DA        ; IF A BIT 3SF42 ...
057523  124004   JMP 4 -> 057527
057524  170442   SAA 42                 ;   42 =: X.ABFUN   (inquiry variant for func 42)
057525  006014   STA ,X 14
057526  124003   JMP 3 -> 057531
057527  170436   SAA 36                 ;   36 =: X.ABFUN   (inquiry variant for func 36)
057530  006014   STA ,X 14
057531  146176   RADD CLD SX DT         ; T := X
057532  135077   JPL I 77 -> 057631     ; CALL SCSID  (INQUIRY + READ CAPACITY -> INQUI)
057533  125077   JMP  I 77 -> 057632     ;   GO FAR ERREX  (SCSID error return)
057534  135077   JPL I 77 -> 057633     ;   CALL ERRFATAL
057535  146151   RADD CLD SA DD         ; \ WHILE D:=A BZERO 17 = 6 OR = 13
057536  174171   BSET ZRO 170 DD        ; |   (unit-attention / aborted -> retry inquiry)
057537  171006   SAT 6                  ; |
057540  142061   SKP IF DD UEQ ST       ; |
057541  124004   JMP 4 -> 057545        ; |
057542  171013   SAT 13                 ; |
057543  140065   SKP IF DA EQL ST       ; |
057544  124003   JMP 3 -> 057547        ; |
057545  042363   MIN ,X -15             ; |   MIN X.TACOU
057546  124363   JMP -15 -> 057531      ; /   loop
057547  171001   SAT 1                  ; WHILE D<=1
057550  141016   SKP IF DT GRE SD
057551  124054   JMP 54 -> 057625
057552  044423   LDA ,B 23              ; IF SUTYP SHZ -10 >< 0 THEN     (device-type byte)
057553  156570   SHA ZIN SHR 10
057554  131003   JAZ 3 -> 057557
057555  171001   SAT 1                  ;   *** T := TYPER (=1) ; GO ERREX  (illegal type) ***
057556  125054   JMP I 54 -> 057632

; ----------------------------------------------------------------------------
; (C) FUNCTION-42 CONTROL-RECORD READ  -- "IF X.SCOCW BIT 3SF42"
;     NPL 057161-057227  ->  L07 057557-057625   (the single READ(6) on the wire)
; ----------------------------------------------------------------------------
057557  046365   LDA ,X -13             ; A := X.SCOCW
057560  175305   BSKP ONE 100 DA        ; IF X.SCOCW BIT 3SF42 ...  (this op is READ FORMAT)
057561  124024   JMP 24 -> 057605       ;   else ordinary transfer parameter
057562  170700   SAA -100               ; \ 177700 /\ X.ABFUN =: X.ABFUN   (INDICATE READ)
057563  072014   AND ,X 14              ; |
057564  006014   STA ,X 14              ; /
057565  146174   RADD CLD SX DL         ; L := X
057566  052015   LDT ,X 15              ; \ address of CONTROL RECORD from X.MEMA1/MEMA2
057567  056016   LDX ,X 16              ; |   (*LDDTX 10)
057570  143312   LDDTX 1                  ; /
057571  146147   RADD CLD SL DX
057572  022017   STD ,X 17              ; AD =: L.ABPA2   (LBA of control record = last block)
057573  002021   STZ ,X 21              ; 0 =: X.ABP31
057574  170401   SAA 1                  ; \ 1 =: X.ABP32   (ONE block)
057575  006022   STA ,X 22              ; /
057576  026015   LDD ,X 15              ; \ X.MEMAD + 1000 =: X.MEMAD  (advance DMA)
057577  050035   LDT 35                 ; |
057600  146061   RADD ST DD             ; |
057601  147155   RADD ADC CLD SA DA     ; |
057602  022015   STD ,X 15             ; /
057603  146176   RADD CLD SX DT
057604  124002   JMP 2 -> 057606
057605  052004   LDT ,X 4               ; (ordinary op) T := X.SLINK
057606  044027   LDA 27                 ; \ 377 /\ X.SCOCW =: control word
057607  072365   AND ,X -13             ; /
057610  135021   JPL I 21 -> 057631     ; CALL SCSID  -> the READ(6) of the control record
057611  125021   JMP  I 21 -> 057632     ;   GO FAR ERREX
057612  135021   JPL I 21 -> 057633     ;   CALL ERRFATAL
057613  146151   RADD CLD SA DD         ; \ WHILE D:=A BZERO 17 = 6 OR = 13 (retry)
057614  174171   BSET ZRO 170 DD        ; |
057615  171006   SAT 6                  ; |
057616  142061   SKP IF DD UEQ ST       ; |
057617  124004   JMP 4 -> 057623        ; |
057620  171013   SAT 13                 ; |
057621  140065   SKP IF DA EQL ST       ; |
057622  124003   JMP 3 -> 057625        ; |
057623  042363   MIN ,X -15             ; |   MIN X.TACOU
057624  124270   JMP -110 -> 057514     ; /   GO RETRY (re-check 5SCIN)
057625  124030   JMP 30 -> 057655       ; GO FINEX

; ----------------------------------------------------------------------------
; (D) FINEX  -- func-42 result validation + THE RETURN (UHLIM, status 36, T:=0)
;     NPL 057257-057351  ->  L07 057655-057747
; ----------------------------------------------------------------------------
057655  146173   RADD CLD SX DB         ; B := SLINK  (caller's request element)
057656  054404   LDX ,B 4
057657  052012   LDT ,X 12              ; T := X.DQOPC
057660  175306   BSKP ONE 100 DT        ; IF X.DQOPC BIT 3SF42 ...      (was it READ FORMAT?)
057661  124066   JMP 66 -> 057747       ;   no -> just T:=0 and return
057662  006010   STA ,X 10              ; \ IF A:=X.HSTAT BZERO 17 <= 1 (status OK) ...
057663  174175   BSET ZRO 170 DA        ; |
057664  171001   SAT 1                  ; |
057665  141056   SKP IF DT GRE SA       ; |
057666  124061   JMP 61 -> 057747       ; /   bad status -> T:=0 return (no geometry)
057667  051117   LDT I 117              ; \ SCCLR: clear controller cache
057670  140660   EXR ST                 ; /
057671  050415   LDT ,B 15              ; \ read the control record into D
057672  054416   LDX ,B 16              ; |
057673  143300   LDATX                  ; |   A := header word ; A>>8 =: NPART
057674  156570   SHA ZIN SHR 10         ; |
057675  004775   STA ,B -3              ; /   NPART =: local -3
057676  146154   RADD CLD SA DL         ; L := NPART
057677  044110   LDA 110                ; \ -1000 =: LOOPC  (checksum word count)
057700  004354   STA -24                ; /
057701  146101   RADD CLD 0 DD          ; D := 0   (running XOR)
057702  143300   LDATX                  ; \ FOR LOOPC: *LDATX ; D XOR A ; X+1
057703  145051   REXO SA DD             ; |   *** XOR CHECKSUM over control record ***
057704  173401   AAX 1                  ; |
057705  040347   MIN -31                ; |
057706  124374   JMP -4 -> 057702       ; /
057707  140001   SKP IF DD EQL 0        ; IF D >< 0 ...          (checksum non-zero)
057710  124007   JMP 7 -> 057717        ;   -> NOCRC
057711  171002   SAT 2                  ; IF L <= 2 ...          (too few partitions)
057712  143046   SKP IF DT LST SL
057713  124004   JMP 4 -> 057717        ;   -> NOCRC
057714  171012   SAT 12                 ; IF L > NCOPA (=12/10) ...(too many partitions)
057715  143046   SKP IF DT LST SL
057716  124003   JMP 3 -> 057721        ;   ok -> success path
057717  171011   SAT 11                 ; *** T := NOCRC (=11) ; GO FAR RETEX  (no ctrl record) ***
057720  125070   JMP I 70 -> 060010
                                        ; ---- SUCCESS PATH (control record valid) ----
057721  044775   LDA ,B -3              ; NPART
057722  120067   MPY 67                 ; NPART * 6
057723  146154   RADD CLD SA DL
057724  024415   LDD ,B 15
057725  171002   SAT 2                  ; MEMAD + 2
057726  146061   RADD ST DD
057727  147155   RADD ADC CLD SA DA
057730  054425   LDX ,B 25             ; \ *MOVPP : copy partition table to CMAD buffer
057731  050426   LDT ,B 26             ; |
057732  143110   MOVEW                  ; /
057733  050425   LDT ,B 25             ; \ *LDDTX 40 : fetch DATA AREA SIZE
057734  054426   LDX ,B 26             ; |
057735  143342   LDDTX 4                  ; /
057736  054404   LDX ,B 4
057737  052015   LDT ,X 15             ; \ *STDTX 10 : store data-area size into
057740  056016   LDX ,X 16             ; |            caller's SLINK.MEMA1/MEMA2 buffer
057741  143316   STDTX 1                  ; /
057742  020776   STD ,B -2             ; \ AD =: UHLIM
057743  170436   SAA 36                ; |  36 =: status word  (*STATX 00)
057744  143304   STATX                  ; /  -> caller's buffer gets {geometry, 36}
057745  054404   LDX ,B 4
057746  046010   LDA ,X 10
057747  146106   RADD CLD 0 DT          ; *** T := 0   (OK RETURN, our run lands here) ***
                                        ;     falls into RETEX (057750): RETOP terminate op

; ----------------------------------------------------------------------------
; (E) INQUI completion -- SET 5SCIN, then func-42 -> RCAFI (return block/record size)
;     NPL 062503-062506  ->  L07 063101-063110
;     dd: word@063101 = 044423 (off 25730)  VERIFIED
; ----------------------------------------------------------------------------
063101  044423   LDA ,B 23              ; A := SUTYP
063102  174275   BSET ONE 70 DA         ; *** SET bit 5SCIN   (INITIALIZATION FINISHED) ***
063103  004423   STA ,B 23              ; SUTYP := A     == SUTYP BONE 5SCIN
063104  170477   SAA 77                 ; \ IF 77 /\ ABFUN = 42 GO RCAFI
063105  070414   AND ,B 14              ; |
063106  171042   SAT 42                 ; |
063107  142065   SKP IF DA UEQ ST       ; |
063110  124014   JMP 14 -> 063124       ; /   (else A=36 -> FINEX / REPEAT)

; ----------------------------------------------------------------------------
; (F) NEXT-OPERATION 5SCIN gate  -- what READS 5SCIN after function 42 returns
;     NPL 063354-063356  ->  L07 063752-063754
;     This is the SCSID/queue-entry inquiry gate a later block-0 (fn 0) read hits.
; ----------------------------------------------------------------------------
063752  044423   LDA ,B 23              ; A := SUTYP
063753  175275   BSKP ONE 70 DA         ; IF SUTYP NBIT 5SCIN ...   (skip if 5SCIN SET)
063754  125163   JMP I 163 -> 064137    ;   5SCIN clear -> GO FAR INQUI (do inquiry first)
                                        ;   5SCIN SET (our case) -> DO NOT branch;
                                        ;   fall through to the normal transfer (CACOB/EXCOM/SCLLD)
063755  000403   STZ ,B 3
063756  000404   STZ ,B 4

; ----------------------------------------------------------------------------
; (G) ERREX -> user-error mapping (proves the internal return-code VALUES)
;     NPL 061526.. -> L07 062124..   "IF D=TYPER / =ILAOP / =BADPA ..."
; ----------------------------------------------------------------------------
062124  171001   SAT 1                  ; IF D = TYPER (=1) ...
062125  140061   SKP IF DD EQL ST
062126  124003   JMP 3 -> 062131
062127  054061   LDX 61                 ;   -> user error index 61
062131  171004   SAT 4                  ; IF D = ILAOP (=4) ...
062132  140061   SKP IF DD EQL ST
062134  054055   LDX 55                 ;   -> user error index 55
062136  171005   SAT 5                  ; IF D = BADPA (=5) ...
062137  140061   SKP IF DD EQL ST
062141  171574   SAX 174                ;   -> user error 174B (illegal parameter)
062143  171006   SAT 6                  ; IF D = RQSER (=6) ...

; ============================================================================
; SUMMARY OF THE DECISION SURFACE (byte-verified)
;   - Function 42 CLEARS 5SCIN at entry (057506), the RETRY loop re-INQUIRES,
;     INQUI SETS 5SCIN on success (063102).  After func 42 returns -> 5SCIN SET.
;   - The next fn-0 (block-0) read reads 5SCIN at 063752-063754 and, seeing it
;     SET, does NOT divert to INQUI: it proceeds to the transfer.  5SCIN cannot
;     cause a block-0 skip.
;   - Function-42 RETURN on our run (ctrl record valid, XOR=0, NPART=8, UHLIM
;     set): T := 0 at 057747 (SUCCESS) - identical success sense to SMD.
;   - There is NO branch inside the disk layer that chooses "issue block-0 vs
;     not".  That decision lives in the (uncarved) device-agnostic caller that
;     consumes UHLIM/status-36/partition-table.  See README section 5.
; ============================================================================
