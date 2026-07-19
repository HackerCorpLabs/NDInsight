; ============================================================================
;  SINTRAN III  L07  (running system L-VSX-500)  -  SCSI DISK DRIVER + DISK LAYER
;  Byte-verified static carve.
;
;  SEGMENT   : 065-S3SIPIT  (load base 32000B)   [identical twin: 066-S3IIPIT]
;  BINARY    : versions/L-VSX-500/segments/065-S3SIPIT.bin  (big-endian)
;  DISASM    : nd100-dis -a -o -b 13312 (=32000B)  on the byte-swapped image
;
;  This file annotates the load-bearing routines only.  The COMPLETE raw
;  disassembly of every word is in the companion files in this folder:
;      _driver.dis     067160B..072011B  (SCSI protocol driver core)
;      _disklayer.dis  056120B..057406B  (SCSI disk layer dispatch/FINEX)
;      _inqui.dis      062214B..062577B  (INQUI / READ CAPACITY / RCAFI)
;
;  Every octal offset below was reproduced with dd before publishing.
;  Ground truth for the sequences is the carved L07 bytes; the NPL
;  (IP-P2-SCSI-DRIV.NPL / IP-P2-SCSI-DISK.NPL) is a DIFFERENT revision and is
;  used only to name the logic (its addresses are ~+376B off from L07).
;
;  Field offsets used (verified anchors, from the task):
;    ABFUN=14  MEMA1=15  MEMA2=16  SUTYP=23  SUCON=30  SLINK=4  5SCIN=7(bit)
;    SCWAQ=177721(cell)  SCDFA=177772(cell)
;  Register-offset access "STA ,X 30" etc. is X.<field> where X = unit datafield.
; ============================================================================


; ============================================================================
;  SECTION 1 - SCSI PROTOCOL DRIVER CORE   (IP-P2-SCSI-DRIV.NPL)
; ============================================================================
;
;  Call sequence into the driver (the ONLY way work enters it):
;       A = function type,  X = unit datafield
;       JPL I (SCLLD ; JMP BUSY ; JMP ERROR ; JMP FIN
;  SCLLD enqueues onto SCWAQ (via INITO) and, if the bus is free, kicks off
;  arbitration (SELEC).  The driver NEVER initiates a transfer on its own.
;
; ----------------------------------------------------------------------------
;  SCLLD = 067160B   driver entry - classify function, enqueue, arbitrate
;          dd off 29920 -> cc 69 dc f8 f2 03 c6 31 a8 0e   (all 5 words verified)
; ----------------------------------------------------------------------------
067160  146151   RADD CLD SA DD      ; D := A                 } D := A SHZ -10
067161  156370   SHD ZIN SHR 10      ; D >>= 8 (octal -10)    }   (function class)
067162  171003   SAT  3              ; T := 3
067163  143061   SKP IF DD LST ST    ; skip next if D < 3   (data-transfer funcs)
067164  124016   JMP  16 -> 067202   ; else -> control-function dispatch (D>=3)
;  --- D < 3 : normal data transfer (READ / WRITE / ...) ---------------------
067165  006030   STA  ,X 30          ; A =: X.SUCON           (control word)
067166  146145   RADD CLD SL DA      ; A := L
067167  006032   STA  ,X 32          ; L =: X.SULRG           (return address)
067170  002033   STZ  ,X 33          ; 0 =: X.SUTRG
067171  044731   LDA  ,B -47         ; A := NCROK
067172  130003   JAP  3  -> 067175   ; if NCROK >= 0 skip error path
067173  171061   SAT  61             ; T := NCRST  (interface error)
067174  125044   JMP I 44 -> ...     ; GO FAR EXDRI   (@067240 = 067746 ptr)
067175  135044   JPL I 44 -> 067241  ; CALL INITO      ** @067241 = 070261 = INITO **
067176  044743   LDA  ,B -35         ; A := BUSFL
067177  131402   JAF  2  -> 067201   ; if BUSFL <> 0 skip
067200  135042   JPL I 42 -> 067242  ; CALL SELEC      ** @067242 = 070165 = SELEC **
067201  124036   JMP  36 -> 067237   ; GO SCWTI (busy return)
;  --- D = 3 : start-with-timer control function -----------------------------
067202  171003   SAT  3
067203  140061   SKP IF DD EQL ST    ; if D = 3 ...
067204  124012   JMP  12 -> 067216
067205  006030   STA  ,X 30          ; A =: X.SUCON
067206  146145   RADD CLD SL DA
067207  006032   STA  ,X 32          ; L =: X.SULRG
067210  002033   STZ  ,X 33          ; 0 =: X.SUTRG
067211  170400   SAA  0
067212  174375   BSET ONE 170 DA     ; A := 1 << 6SFUN         (initial status)
067213  006031   STA  ,X 31          ; =: X.SUTHS
067214  135027   JPL I 27 -> 067243  ; CALL ENTIM  (@067243 = 070421 = ENTIM)
067215  124022   JMP  22 -> 067237   ; GO SCWTI
;  --- D = 4 : bus reset control function -------------------------------------
067216  171004   SAT  4
067217  140061   SKP IF DD EQL ST    ; if D = 4 ...
067220  124016   JMP  16 -> 067236   ; else illegal function -> T:=ILDCO
067221  050731   LDT  ,B -47         ; T := NCROK
067222  175006   BSKP ZRO 0 DT       ; test 8SRST
067223  124007   JMP  7  -> 067232   ; already in progress -> intermediate return
067224  014734   STX  ,B -44         ; X =: SCRXR
067225  146147   RADD CLD SL DX
067226  014733   STX  ,B -45         ; L =: SCRLR   (save registers)
067227  004732   STA  ,B -46         ; A =: SCRCO
067230  125014   JMP I 14 -> 067244  ; GO FAR SCRST (@067244 = 070635 = SCRST)
067236  171010   SAT  10             ; T := ILDCO   (illegal function)
067237  125727   JMP I ,B -51 -> ..  ; GO SCWTI     (busy/return dispatcher)
;  --- embedded pointer words (data, not code) --------------------------------
067240  067746   .word 067746        ; EXDRI  (GO FAR target)
067241  070261   .word 070261        ; INITO
067242  070165   .word 070165        ; SELEC
067243  070421   .word 070421        ; ENTIM
067244  070635   .word 070635        ; SCRST
067246  071121   .word 071121        ; (MSGIN/SCISR family ptr)

; ----------------------------------------------------------------------------
;  SCINT = 067247B   interrupt handler - read device status, dispatch phase
; ----------------------------------------------------------------------------
067247  050775   LDT  ,B -3          ; T := HDEV
067250  173004   AAT  4              ; T := HDEV + RSTAU   (04 = read status)
067251  150415   IOXT                ; *IOXT  -> A := device status
067252  171464   SAX  64
067253  144457   RAND SA DX          ; X := 64 /\ A
067254  133014   JXZ  14 -> 067270   ; if bits 0 -> skip
067255  175025   BSKP ZRO 20 DA      ; test bit 2  (controller busy)
067256  125727   JMP I ,B -51 -> ..  ; GO SCWTI  (controller busy, leave)
;  ... (SCSI bus reset / IDE / NCR-interrupt handling; see _driver.dis)
;  067310..067315: A/\37 = SCEIM class -> CALL SCISR (expected interrupt)
;  067324 LDX ,B -60 (SCCSU); JXZ; JPL DCTHR  -> disconnect logical thread
;    (this is the A=4 disconnect leg the failing trace takes)

; ----------------------------------------------------------------------------
;  SELEC = 070165B   arbitration/selection - if SCWAQ empty, declare bus free
; ----------------------------------------------------------------------------
070165  030374   STF  -4             ; TAD =: SVTAD          (save)
070166  014376   STX  -2             ; X   =: SAVXR
070167  170777   SAA  -1
070170  004701   STA  ,B -77         ; -1 =: SCEIM           (disable interrupt)
070171  054721   LDX  ,B -57         ; X := SCWAQ            ** arbitration queue **
070172  133046   JXZ  46 -> 070240   ; IF SCWAQ = 0  -> bus-free leg (070240)
070173  170400   SAA  0
070174  050775   LDT  ,B -3          ; T := HDEV
070175  173005   AAT  5              ; T := HDEV + WCONT
070176  150415   IOXT                ; clear to memory
070177  174255   BSET ONE 50 DA      ; A := 1 << 6SARB       (indicate arbitration)
070200  004743   STA  ,B -35         ; =: BUSFL
;  ... program NCR: WDESI, WTCM/WTC2/WTCL (waiting time), WNCOM select cmd ...
070217  056030   LDX  ,X 30          ; X := X.SUCON
070220  175247   BSKP ONE 40 DX      ; test 4SINA
070240  000743   STZ  ,B -35         ; 0 =: BUSFL            ** SCWAQ EMPTY -> BUS FREE **
070241  170405   SAA  5
070242  050775   LDT  ,B -3          ; T := HDEV
070243  173005   AAT  5              ; T := HDEV + WCONT      (re-enable interrupt) ...
;  On the FAILING run SELEC is entered with SCWAQ = 0, takes the JXZ at 070172
;  to 070240, sets BUSFL := 0 (bus free) and returns normally.  This is the
;  verified "SELEC finds SCWAQ EMPTY -> normal/success return" of the trace.

; ----------------------------------------------------------------------------
;  INITO = 070261B   initialize operation - build status, INSERT INTO SCWAQ
;  This is THE enqueue.  Nothing reaches the bus that was not linked here.
; ----------------------------------------------------------------------------
070261  046030   LDA  ,X 30          ; A := X.SUCON
070262  146151   RADD CLD SA DD      ; D := A
070263  156570   SHA ZIN SHR 10      ; A >>= 8   (X.SUCON SHZ -10 : op class)
070264  131406   JAF  6  -> 070272   ; if != 0 -> abort/bus-device-reset variant
070265  044371   LDA  -7             ; A := INOPR pattern     (normal operation)
070266  175261   BSKP ONE 60 DD      ; test 4SRCA in D
070267  124002   JMP  2  -> 070271
070270  174315   BSET ONE 110 DA     ; set 6SRFD (return on command accepted)
070271  124006   JMP  6  -> 070277
070272  172777   AAA  -1             ; A-1 : =0 abort, else bus device reset
;  ... build initial thread status ...
070300  170777   SAA  -1
070301  006013   STA  ,X 13
070302  026001   LDD  ,X 1
070303  022007   STD  ,X 7           ; X.SUIDP =: X.SUSDP   (initial data ptr)
070304  026003   LDD  ,X 3
070305  022011   STD  ,X 11          ; X.SUIBC =: X.SUSBC   (initial byte count)
070306  170776   SAA  -2
070307  006035   STA  ,X 35          ; -2 =: X.SUTMR
070310  044060   LDA  60             ; A := (SCWAQ - SULINK) base constant
070311  146035   RADD SB DA          ; A := "SCWAQ-SULINK" + B  (B = unit df)
070312  146176   RADD CLD SX DT      ; T := X
070313  146157   RADD CLD SA DX      ; loop: X := A
070314  046027   LDA  ,X 27          ; A := X.SULINK
070315  131002   JAZ  2  -> 067317   ; while SULINK <> 0 keep walking
070316  124375   JMP  -3 -> 070313
070317  012027   STT  ,X 27          ; T =: X.SULINK        ** LINKED INTO SCWAQ **
070320  146142   EXIT                ; return to SCLLD
;  After INITO returns, SCLLD does:  IF BUSFL=0 THEN CALL SELEC FI
;  SELEC then finds SCWAQ NON-empty and starts arbitration on this element.


; ============================================================================
;  SECTION 2 - SCSI DISK LAYER   (IP-P2-SCSI-DISK.NPL, "SCSTREAM/SCSID")
;  The level-11 routine the filesystem hands off to.  It builds the SCSI
;  command and calls the driver (CALL SCSID -> ... -> SCLLD).
; ============================================================================
;
;  Dispatch / NEWOP / RETRY / FINEX live at 056120B..057406B (see _disklayer.dis).
;  Key function-42 handling (NPL IP-P2-SCSI-DISK.NPL lines 337-405):
;
;    NEWOP, function 42 (READ FORMAT / read the CONTROL RECORD):
;       057074  IF A=42 THEN
;       057077     X.MEMAD =: MEMAD ; X.ABFUN =: ABFUN
;       057103     0 =: ABP31 ; 100 =: ABP32
;       057106     SCDFA.SUTYP BZERO 5SCIN =: X.SUTYP   ; FORCE RE-INQUIRY
;
;    RETRY loop:
;       057116  IF SUTYP NBIT 5SCIN ... THEN            ; not yet inited
;       057124     set ABFUN := 42 or 36
;       057133     DO  CALL SCSID  ...  OD              ; -> AUTO INQUIRY + READ CAPACITY
;                                                        ; (INQUI, sets 5SCIN)
;       057162  IF X.SCOCW BIT 3SF42 THEN               ; this is a read-format op
;       057164     177700/\X.ABFUN =: X.ABFUN           ; indicate READ
;       057170     T:=X.MEMA1 ; X:=X.MEMA2 ; *LDDTX 10  ; ADDRESS OF CONTROL RECORD
;       057173     AD =: L.ABPA2                        ; = last-LBA block address
;                  0 =: X.ABP31 ; 1 =: X.ABP32          ; transfer 1 block
;       057200     bump X.MEMAD by 1000
;       057210  377/\X.SCOCW ; CALL SCSID               ; ** THE control-record READ(6) **
;       057227  GO FINEX
;
;    FINEX (057257) post-processing of the control record:
;       057261  IF X.DQOPC BIT 3SF42 THEN
;       057264     IF A=:X.HSTAT BZERO 17 <= 1 THEN     ; good status
;       057271        clear cache; read control record; checksum over -1000 words
;       057311        IF bad-sum OR L<=2 OR L>NCOPA THEN T:=NOCRC ; GO RETEX  (no CR)
;       057323        NPART*6 ...; MOVE partition table to buffer
;       057335        *LDDTX 40  return DATA AREA SIZE
;       057344        AD =: UHLIM ; 36 =: status word    ; ** RETURN GEOMETRY **
;       057351     FI FI
;       057351  T:=0
;       057352  RETEX: ... CALL FAR RETOP               ; TERMINATE OPERATION, return
;
;  ==> Function 42 is SELF-CONTAINED: INQUIRY + READ CAPACITY + one READ(6) of
;      the control record, validate, return UHLIM + partition table, DONE.
;      It NEVER sets up a block-0/page-0 data read and NEVER calls SCLLD for one.
;      This matches the live trace exactly (INQUIRY, READ CAPACITY, one READ(6)
;      of LBA 129311, then silence).
;
;  INQUI / READ CAPACITY / RCAFI  (062215B..062577B, see _inqui.dis):
;    062215 INQUI: zero old status
;    062223 build INQUIRY command (11000), alloc len, CALL EXINT (=driver)
;    062260 device type: direct-access -> emit READ CAPACITY (22400)
;    062341 CALL EXINT  (READ CAPACITY)  -> SURSZ (record size), SUSI1/2/3 shifts
;    062503 SUTYP BONE 5SCIN =: SUTYP    ; INITIALIZATION FINISHED
;    062506 IF 77/\ABFUN=42 GO RCAFI     ; function-42 -> finish, return record size
;    062513 IF A-36=0 GO FINEX
;    062516 GO REPEAT                    ; a NORMAL op would loop back to do the read
;
; ============================================================================
;  SECTION 3 - SCSDI datafield anchor = 057215B  (verified coherent in 065)
;    057215 RADD CLD SL DA ; STA ,X 6 ; JPL I 51 ; ... ; LDA ,X 14 (=ABFUN)
;    Confirms the disk-layer datafield code lives in this same segment/overlay.
; ============================================================================
