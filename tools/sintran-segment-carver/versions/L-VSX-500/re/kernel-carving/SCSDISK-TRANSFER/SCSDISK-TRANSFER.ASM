; ============================================================================
; SCSDISK-TRANSFER.ASM
; ----------------------------------------------------------------------------
; Byte-verified disassembly of the SINTRAN III L07 (L-VSX-500) SCSI disk-layer
; transfer primitive SCSDISK (= SCSDI, the datafield ",X 14" target) and the
; SCSI driver-command dispatch SCSID, traced along the FUNCTION-0 (READ) path
; for block 0 all the way to the driver enqueue SCLLD, or to the pre-transfer
; reject (BADPA).
;
; Segment : 065-S3SIPIT   (load base 32000B = 13312 decimal ; byte-identical
;                          twin 066-S3IIPIT)
; Source  : ../../segments/065-S3SIPIT.bin  (big-endian words)
; Disasm  : nd100-dis -a -o -b 13312  over the byte-swapped image.
; dd off  : byte_offset = (addr - 32000B) * 2   (decimal).
;
; REVISION-OFFSET FACT (verified): the whole IP-P2-SCSI-DISK compilation unit
; (SCSDISK, SCSID, CACOB, INQUI, the fn-42 handling) sits in the L07 image at
; NPL-source-label + 376B. e.g. SCSID (SYMBOL-2-LIST 062217) = NPL 061621 + 376.
; The SCSI driver (IP-P2-SCSI-DRIV: SCLLD=067160, INITO=070261, SELEC=070165,
; SCINT=067247) is a separate unit aligned directly to its NPL labels (no +376).
;
; Every octal word below equals the raw carve word (dd-confirmed for the anchor
; instructions; see the dd table in README.md). All numbers are OCTAL.
; Mnemonics are nd100-dis output; where nd100-dis mis-decodes a data word as an
; instruction it is annotated. NPL logic is a DIFFERENT revision - used only to
; name the operation, never as the authoritative bytes.
; ============================================================================


; ============================================================================
; SCSDISK  (SCSDI = 057215B)   -- the per-device transfer primitive
; ----------------------------------------------------------------------------
; This is what the device-agnostic filesystem reaches through the SCSI unit
; datafield field ",X 14" (RCBLO's JPL I ,B 10 device transfer dispatch).
; ENTRY:  B = disk datafield, X = DAQ (abstract parameter list / request),
;         L = return address.
; ============================================================================
SCSDI:
057215  146145   RADD CLD SL DA      ; A := L                (save return addr)
057216  006006   STA  ,X 6           ; A =: X.NFUNC          (NPL: A:=L=:X.NFUNC)
057217  135051   JPL I 51 -> 057270  ; CALL DIALO            (MON PERFO disk util)
057220  144400   RAND 0 0            ; (0/\0 filler after the *MPRF call)
057221  046014   LDA  ,X 14          ; A := X.ABFUN          (function+unit word)
057222  014372   STX  -6             ; ... =: SAVEX region
057223  171477   SAX  77             ; X := 77               (mask 6 bits)
057224  144457   RAND SA DX          ; X := 77 /\ A          (fn code, 0..77)
                                     ; -> OPTYP(X) lookup follows (control word)
; ... OPTYP[fn] -> DQOPC operation-type control word ; ILAOP if OPTYP=0 ...
; ... 3DPA2/3DPA3 promotion of ABPA2/ABPA3 to double disk address/amount ...
; ... PUNDF(unit) generation; NOLUN if no unit datafield ...

; ----------------------------------------------------------------------------
; NON-PARTITION geometry bounds check  (NPL: T:="UHLIM"+B; CALL COMPD;
;                                            IF A>0 GO ERR2)
; ENTRY here after the "IF L NBIT 3SPES / IF L BIT 3PART ... ELSE" ELSE arm.
; This is the ONLY pre-transfer reject in the disk layer for a normal read.
; ----------------------------------------------------------------------------
057352  026017   LDD  ,X 17          ; AD := X.ABPA2         (disk block address)
057353  052022   LDT  ,X 22          ; T  := X.ABP32         (transfer amount)
057354  146061   RADD ST DD          ; D  := D + T
057355  147155   RADD ADC CLD SA DA  ; A  := A + carry       (addr + amount, double)
057356  050032   LDT  32             ; T  := UHLIM           (NPL: T:="UHLIM"+B)
057357  146036   RADD SB DT          ; T  := UHLIM + B       (datafield-relative)
057360  135027   JPL I 27 -> 057407  ; CALL COMPD            (compare double vs UHLIM)
057361  141050   SKP IF 0 GRE SA     ; skip if 0 >= A        (i.e. within bounds)
057362  124015   JMP 15 -> 057377    ; A>0 (past end) -> GO ERR2  *** REJECT ***
                                     ; block 0: addr=0 -> A<=0 -> skip -> NO reject
; fall-through: address within UHLIM -> proceed to DISK SORTING / NEWOP.
057363  124004   JMP 4 -> 057367     ; (join from the partition arm)

; ----------------------------------------------------------------------------
; DISK SORTING then start-new-operation
;   MIN SUNOP ; IF SLINK><0 -> CALL DSORT ; GO SWT11   ELSE  GO NEWOP
; ----------------------------------------------------------------------------
057367  040410   MIN  ,B 10          ; MIN SUNOP             (count operations)
057370  144400   RAND 0 0            ; (0/\0 filler)
057371  044404   LDA  ,B 4           ; A := SLINK
057372  131004   JAZ  4 -> 057376    ; if SLINK=0 -> NEWOP branch
057373  144037   SWAP SB DX          ; X := B  (prep DSORT)
057374  135015   JPL I 15 -> 057411  ; CALL DSORT            (sort into queue)
057375  125015   JMP I 15 -> 057412  ; GO SWT11
057376  124016   JMP 16 -> 057414    ; GO NEWOP  (057415 region)

; ----------------------------------------------------------------------------
; ERR2 / ERR1  -- pre-transfer reject terminators
; ----------------------------------------------------------------------------
ERR2:
057377  044400   LDA  ,B 0           ; A := ULINK            (NPL: ULINK=:B)
057400  146153   RADD CLD SA DB      ; B := ULINK
057401  171005   SAT  5              ; T := 5 = BADPA        *** T=5 illegal param ***
ERR1:
057402  135011   JPL I 11 -> 057413  ; CALL FAR RETOP        (terminate operation)
057403  125007   JMP I 7 -> 057412   ; GO SWT11
; --- literal pointer words used by the CALLs above (data, not code) ---
057407  055607   =word 055607        ; pointer -> COMPD
057411  055460   =word 055460        ; pointer -> DSORT
057412  056516   =word 056516        ; pointer -> SWT11   (SYMBOL-2 SWT11=056516)
057413  056766   =word 056766        ; pointer -> RETOP


; ----------------------------------------------------------------------------
; 5SCIN-init DO-loop (NPL RETRY head): if SUTYP NBIT 5SCIN issue fn 42/36 first.
; For OUR block-0 read 5SCIN is already SET (left set by function 42), so this
; whole loop is skipped and control drops to the real-transfer CALL SCSID below.
; ----------------------------------------------------------------------------
057524  170442   SAA  42             ; A := 42               (fn 42 INQUIRY+CAP)
057525  006014   STA  ,X 14          ; X.ABFUN := 42
057526  124003   JMP  3 -> 057531
057527  170436   SAA  36             ; A := 36               (fn 36 read-format)
057530  006014   STA  ,X 14          ; X.ABFUN := 36
057531  146176   RADD CLD SX DT      ; T := X                (param pointer to SCSID)
057532  135077   JPL I 77 -> 057631  ; CALL SCSID            (driver-command dispatch)
057533  125077   JMP I 77 -> 057632  ; GO FAR ERREX
057534  135077   JPL I 77 -> 057633  ; CALL ERRFATAL

; ----------------------------------------------------------------------------
; REAL TRANSFER driver call (NPL: 377/\X.SCOCW; CALL SCSID; GO ERREX; ERRFATAL)
; This is the fn-0 READ enqueue call for block 0.
; ----------------------------------------------------------------------------
057605  052004   LDT  ,X 4           ; T := X.SLINK          (parameter pointer)
057606  044027   LDA  27             ; A := ... X.SCOCW
057607  072365   AND  ,X -13         ; A := 377 /\ X.SCOCW   (driver function code)
057610  135021   JPL I 21 -> 057631  ; CALL SCSID            *** enqueue path ***
057611  125021   JMP I 21 -> 057632  ; GO FAR ERREX
057612  135021   JPL I 21 -> 057633  ; CALL ERRFATAL
; --- literal pointer words (data) ---
057631  062217   =word 062217        ; pointer -> SCSID   (dd off 22322 = 062217)
057632  057636   =word 057636        ; pointer -> ERREX
057633  000215   =word 000215        ; pointer -> ERRFATAL (rel)


; ============================================================================
; SCSID  (062217B)   -- SCSI driver-command dispatch (builds+issues a command)
; ----------------------------------------------------------------------------
; NPL 061621 + 376B = 062217. Called by SCSDISK (above), by SCSTREAM, SCDTR,
; CTRSCSI. Dispatches by ABFUN and, for a normal read, builds the SCSI CDB
; (CACOB) and hands it to the driver (EXCOM -> GO SCLLD). It performs NO
; UHLIM/geometry bound check of its own.
; ENTRY: T = ptr to abstract parameter area, X = device datafield,
;        B = LUN datafield, A = driver function code, L = return.
; ============================================================================
SCSID:
062217  006010   STA  ,X 10          ; A =: X.HSTAT          (dd off 24862 = 006010)
062220  146145   RADD CLD SL DA      ; A := L
062221  006370   STA  ,X -10         ; L =: X."FINISH"
062222  146135   RADD CLD SB DA      ; A := B
062223  006371   STA  ,X -7          ; B =: X.SCUDF
062224  012364   STT  ,X -14         ; T =: X.SCTRG
062225  144076   SWAP SX DT          ; T <-> X
062226  170477   SAA  77             ; A := 77
062227  072014   AND  ,X 14          ; A := 77 /\ X.ABFUN    (function code 0..77)
062230  146167   RADD CLD ST DX      ; X := T
062231  171037   SAT  37             ; T := 37
062232  140065   SKP IF DA EQL ST    ; IF A = 37
062233  124010   JMP  10 -> 062243   ;   not 37 -> queue/NEWOP
062234  135152   JPL I 152 -> 062406 ;   fn 37 -> CALL DOEXS (read extended status)
; ...
062243  044431   LDA  ,B 31          ; A := SUTHS            (driver busy?)
062244  131016   JAZ  16 -> 062262   ; SUTHS=0 -> NEWOP
                                     ; SUTHS><0 -> link request into queue,
                                     ; GO SCWTI (return to interrupt handler)

; ----------------------------------------------------------------------------
; REPEAT: dispatch by function (NPL 061702 + 376 = 062300)
;   fn 31 -> BDRST ; fn 74 -> GUSCB ; fn 42/36 -> INQUI ; fn 23/25 -> MODES ;
;   everything else (fn 0 READ, fn 1 WRITE, ...) -> CALL CACOB ; CALL EXCOM
; ----------------------------------------------------------------------------
REPEAT:
062300  170477   SAA  77
062301  070414   AND  ,B 14          ; A := 77 /\ ABFUN
062302  171031   SAT  31
062303  142065   SKP IF DA UEQ ST    ; IF fn = 31
062304  125104   JMP I 104 -> 062410 ;   -> BDRST (bus device reset)
062305  171074   SAT  74
062306  140065   SKP IF DA EQL ST    ; IF fn = 74
062307  124003   JMP  3 -> 062312
062310  135101   JPL I 101 -> 062411 ;   -> CALL GUSCB (user SCSI command block)
062311  124016   JMP  16 -> 062327
062312  171042   SAT  42
062313  142065   SKP IF DA UEQ ST    ; IF fn = 42
062314  124003   JMP  3 -> 062317
062315  171036   SAT  36
062316  142065   SKP IF DA UEQ ST    ; IF fn = 36
062317  125073   JMP I 73 -> 062412  ;   fn 42 or 36 -> INQUI (auto inquiry+capacity)
062320  171023   SAT  23
062321  142065   SKP IF DA UEQ ST    ; IF fn = 23
062322  124003   JMP  3 -> 062325
062323  171025   SAT  25
062324  142065   SKP IF DA UEQ ST    ; IF fn = 25
062325  125066   JMP I 66 -> 062413  ;   fn 23 or 25 -> MODES (mode sense/select)
; ---- FALL THROUGH for fn 0 (READ), fn 1 (WRITE), fn 2/3, fn 60/61 ... ----
062326  135066   JPL I 66 -> 062414  ; CALL CACOB   (build SCSI command block)
062327  135066   JPL I 66 -> 062415  ; CALL EXCOM   (execute command -> GO SCLLD)
062330  135066   JPL I 66 -> 062416  ; CALL SCEIO
062331  125066   JMP I 66 -> 062417  ; GO FAR IMRET
; pointer words (data): 062412->INQUI, 062413->MODES, 062414->CACOB,
;                       062415->EXCOM, 062416->SCEIO, 062417->IMRET


; ============================================================================
; CACOB  (063352 NPL + 376 = 063750)   -- build the SCSI command block
; ----------------------------------------------------------------------------
; THE 5SCIN RE-ENTRY GATE the FUNCTION-42-RETURN carve located at 063752B.
; NPL: CACOB: A:=L=:"HOME"; IF SUTYP NBIT 5SCIN GO FAR INQUI; ... build CDB ...
; ============================================================================
CACOB:
063750  146145   RADD CLD SL DA      ; A := L
063751  004374   STA  -4             ; L =: HOME  (return address)
063752  044423   LDA  ,B 23          ; A := SUTYP    (dd off 26580 = 044423)
063753  175275   BSKP ONE 70 DA      ; skip next IF bit 7 (5SCIN) = ONE
063754  125163   JMP I 163 -> 064137 ; 5SCIN CLEAR -> GO FAR INQUI (start inquiry)
                                     ; 5SCIN SET  -> skip the JMP, build the CDB:
063755  000403   STZ  ,B 3           ; 0 =: SUIB1  (zero bytecount)
063756  000404   STZ  ,B 4           ; 0 =: SUIB2
063757  050423   LDT  ,B 23          ; T := SUTYP
063760  176666   BLDA 60 DT          ; device type (5SCDA) to K
063761  170477   SAA  77
063762  070414   AND  ,B 14          ; A := 77 /\ ABFUN  (command index)
                                     ; -> SCSF1(X) / SCSF2(X) opcode lookup
                                     ;    SCSF1[0] = 010\340 = READ(6) op 0x08.
; ... builds the 6/10-byte SCSI READ CDB into SMBP1/SMBP2, returns via HOME ...


; ============================================================================
; EXCOM / FELLS  (063005 NPL + 376 = 063403)  -- issue command to the driver
; ----------------------------------------------------------------------------
; NPL FELLS tail: X:=SUDDF:=:B; SAVA; GO SCLLD.  This is the DRIVER ENQUEUE.
; ============================================================================
; ... EXCOM sets SUCM1/SUCM2, LUN into command byte 0 ...
063430  125023   JMP I 23 -> 063453  ; GO SCLLD          *** driver enqueue ***
; pointer word (data):
063453  067160   =word 067160        ; pointer -> SCLLD  (dd off 26198 = 067160)
                                     ; SCLLD 067160B = IP-P2-SCSI-DRIV enqueue
                                     ; (SCLLD -> INITO -> SCWAQ, carved in
                                     ;  ../SCSI-DRIVER/).

; ============================================================================
; RESULT (see README.md section "THE ANSWER"):
; A fn-0 (READ) request for block 0, with 5SCIN SET, flows
;   ",X 14" -> SCSDI 057215
;          -> UHLIM check 057356..057362  (block 0 = addr 0: WITHIN bounds,
;                                           does NOT branch to ERR2/BADPA)
;          -> DSORT/NEWOP -> CALL SCSID  (057610 -> 062217)
;          -> REPEAT 062300: fn 0 falls through -> CALL CACOB 062326
;          -> CACOB 5SCIN gate 063752: 5SCIN SET -> builds READ(6) CDB
;          -> CALL EXCOM 062327 -> GO SCLLD 063430 -> 067160.
; It REACHES SCLLD. The only pre-transfer reject (BADPA at 057401) requires
; address+amount > UHLIM, which block 0 cannot trigger. Case 2 (pre-transfer
; reject of block 0) is therefore REFUTED by the bytes; case 1 (the caller
; never issues the block-0 request) is the remaining explanation.
; ============================================================================
