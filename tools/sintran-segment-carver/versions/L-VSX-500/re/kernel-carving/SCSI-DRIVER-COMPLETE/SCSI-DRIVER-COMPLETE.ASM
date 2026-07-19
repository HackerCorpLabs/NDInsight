; ============================================================================
;  SINTRAN III  L07  (running system L-VSX-500)
;  SCSI PROTOCOL DRIVER  (IP-P2-SCSI-DRIV)  -  COMPLETE level-11/interrupt carve
;
;  SEGMENT : 065-S3SIPIT  (load base 32000B)   [byte-identical twin: 066-S3IIPIT]
;  BINARY  : versions/L-VSX-500/segments/065-S3SIPIT.bin  (big-endian, as carved)
;  DISASM  : nd100-dis -a -o -b 13312 (=32000B)  on the byte-swapped image
;  sha256  : 9ccd8f3df7666eb3ac3d89e3edf68a0b1b7688f2c98a4a3b1fb761d58562b144
;
;  Byte offset of octal address A:  (A - 32000B) * 2   (both octal, result decimal)
;
;  GROUND TRUTH  = the carved L07 bytes.  The NPL (IP-P2-SCSI-DRIV.NPL) is a
;  DIFFERENT revision and is used ONLY to name the logic; its column-1 addresses
;  do NOT equal the L07 addresses (the per-routine offset drifts, it is NOT a
;  fixed +376B).  Every carved address below was taken from bytes or from an
;  embedded pointer word, and every entry + load-bearing word was reproduced
;  with dd (see README section "dd verification").
;
;  This file supersedes the DRIVER half of ../SCSI-DRIVER/ by carving EVERY
;  path (SCINT phase dispatch, DCTHR/TEROP/BUSFP, SCRST, the message handlers).
;  It does NOT re-carve the SCSI DISK LAYER (IP-P2-SCSI-DISK) - that is
;  ../SCSDISK-TRANSFER/ + ../FUNCTION-42-RETURN/ + the SCSI-DISKLAYER-COMPLETE
;  carve.  Only the disk-layer -> driver hand-off (EXCOM -> GO SCLLD) is shown.
; ============================================================================


; ----------------------------------------------------------------------------
;  VERIFIED ROUTINE ADDRESS MAP  (all octal; source of each address in [])
; ----------------------------------------------------------------------------
;    SCLLD 067160  driver entry / enqueue          [dd entry word 146151]
;    SCINT 067247  interrupt / completion handler   [dd entry word 050775]
;    SCTIO 067471  software timeout handler         [NPL SCTIO logic @067471]
;    SCDIS 067724  fatal error -> reset SCSI bus     [struct @067724 + ptr->SCRST]
;    BUSFP 067745  bus-free phase -> CALL SELEC       [JPL I 20 -> ptr@067765=070165]
;    EXDRI 067746  exit driver (compute return P)    [struct matches NPL EXDRI]
;    SELEC 070165  arbitration / selection           [dd entry word 030374; ptr@067242]
;    RFWAQ 070250  remove head of SCWAQ              [struct @070250]
;    INITO 070261  init op + splice onto SCWAQ tail   [dd entry word 046030; ptr@067241]
;    CNTHR 070321  connect physical path             [struct @070321]
;    DCTHR 070341  disconnect logical thread         [ptr@067350=070341; calls TEROP]
;    RSTMR 070375  restart LUN timer                 [struct @070375]
;    ENTIM 070421  enable LUN timer                  [struct @070421; ptr@067243]
;    TEROP 070500  terminate operation               [dd entry word 046033; ptr@070373]
;    DITIM ~070561 disable LUN timer                 [NPL DITIM follows TEROP]
;    SCRST 070635  start reset sequence on SCSI bus   [struct @070635; ptr@067244]
;    STFIN ~071xxx terminate reset / selftest done   [NPL, reached GO FAR STFIN]
;    NEWPH ~071xxx new-phase demux (DAOPH..MSIPH)     [NPL NEWPH GOSW]
;    RINFO ~071325 read one info byte from SCSI bus    [NPL RINFO; GO FAR SCINT]
;    MSGIN/MSGI/EMxx  message-in state machine        [NPL]
;    SCMRJ/SCMPE/SCIDE/SCABO/EMSGO  message-out setters[NPL]
;
;  DISK-LAYER -> DRIVER hand-off (boundary, in the disk layer's overlay code):
;    ptr @063453 = 067160         [dd] = the "JPL I (SCLLD" pointer used by EXCOM
;
; ----------------------------------------------------------------------------
;  DRIVER DATAFIELD (B-relative).  B holds the driver datafield base; the cells
;  live at the very top of the 64K space, so a displacement d resolves to the
;  absolute cell  0200000+d.  Names taken from SYMBOL-1-LIST at those addresses.
; ----------------------------------------------------------------------------
;    ,B -3   = HDEV   (177775)  device IOX base
;    ,B -35  = BUSFL  (177743)  bus/phase status word (the live state machine)
;    ,B -36  = SCTST  (177742)  interface-test / timer-owner flag
;    ,B -44  = SCRXR  (177734)  saved X across bus reset
;    ,B -45  = SCRLR  (177733)  saved L across bus reset
;    ,B -46  = SCRCO  (177732)  saved A/CO across bus reset
;    ,B -47  = NCROK  (177731)  controller-ok / recovery flags (bit8SRST etc.)
;    ,B -50  = SCRSL  (177730)  direct-timeout scratch
;    ,B -51  = SCWTI  (177727)  ->pointer: common "busy return" dispatcher
;    ,B -56  = SCTQP  (177722)  head of TIMER queue (SCTQP-SULINK is queue anchor)
;    ,B -57  = SCWAQ  (177721)  head of WAIT-FOR-ARBITRATION queue  ** the queue **
;    ,B -60  = SCCSU  (177720)  currently-selected unit datafield (0 = none)
;    ,B -61  = CMSGI  (177717)  current message-in byte
;    ,B -62  = CMSGO  (177716)  current message-out byte
;    ,B -75  = WATFS  (177703)  arbitration/select waiting-time constant
;    ,B -76  = SCISR  (177702)  ->pointer: expected-interrupt service routine
;    ,B -77  = SCEIM  (177701)  expected-interrupt mask / SCNIH next-int handler
;    ,B -100 = SCCCW  (177700)  control-word shadow written to WCONT
;    ,B -101 = SCNIS  (177677)  new interrupt status (from RITRG)
;    ,B -102 = SCSSR  (177676)  saved SCSI status register
;
;  UNIT DATAFIELD (X-relative) fields used by the driver:
;    X.0   SUDLU   device/LUN select id (>>14 = SCSI ident)
;    X.1   SUIDP   initial data pointer (double)     X.7  SUSDP saved data ptr
;    X.3   SUIBC   initial byte count (double)       X.11 SUSBC saved byte count
;    X.13  SUSTA   status                            X.27 SULINK queue link
;    X.30  SUCON   control word (function class = SUCON SHZ -10)
;    X.31  SUTHS   thread status (BUSFL snapshot for this op)
;    X.32  SULRG   caller return address (L)
;    X.33  SUTRG   caller-supplied target status / error code
;    X.35  SUTMR   timer delta for this op
; ============================================================================


; ============================================================================
;  SCLLD = 067160B    DRIVER ENTRY.  A = function type, X = unit datafield.
;    Classify A>>8; enqueue data transfers; kick arbitration when bus idle.
;    dd off 29920 : cc69 dcf8 f203 c631 a80e  (SCLLD..+4, all verified)
; ============================================================================
067160  146151  RADD CLD SA DD      ; D := A
067161  156370  SHD ZIN SHR 10      ; D := A >> 8         (function CLASS)
067162  171003  SAT  3              ; T := 3
067163  143061  SKP IF DD LST ST    ; if D < 3 do transfer, else dispatch
067164  124016  JMP 16 -> 067202
;  --- CLASS 0..2 : DATA TRANSFER (READ / WRITE / ...) ------------------------
067165  006030  STA  ,X 30          ; A =: X.SUCON            (control word)
067166  146145  RADD CLD SL DA      ; A := L
067167  006032  STA  ,X 32          ; L =: X.SULRG            (return address)
067170  002033  STZ  ,X 33          ; 0 =: X.SUTRG
067171  044731  LDA  ,B -47         ; A := NCROK
067172  130003  JAP  3 -> 067175    ; if NCROK >= 0 skip     [dd 130003]
067173  171061  SAT  61             ; T := NCRST (interface error)
067174  125044  JMP I 44 -> 067240  ; GO FAR EXDRI (ptr@067240=067746)
067175  135044  JPL I 44 -> 067241  ; CALL INITO   (ptr@067241=070261)  ** ENQUEUE **
067176  044743  LDA  ,B -35         ; A := BUSFL
067177  131402  JAF  2 -> 067201    ; if BUSFL <> 0 (bus busy) skip SELEC
067200  135042  JPL I 42 -> 067242  ; CALL SELEC   (ptr@067242=070165)  ** ARBITRATE **
067201  124036  JMP 36 -> 067237    ; GO SCWTI  (busy/return dispatcher)
;  --- CLASS 3 : start-with-timer control function ---------------------------
067202  171003  SAT  3
067203  140061  SKP IF DD EQL ST    ; if D = 3
067204  124012  JMP 12 -> 067216
067205  006030  STA  ,X 30          ; A =: X.SUCON
067206  146145  RADD CLD SL DA
067207  006032  STA  ,X 32          ; L =: X.SULRG
067210  002033  STZ  ,X 33          ; 0 =: X.SUTRG
067211  170400  SAA  0
067212  174375  BSET ONE 170 DA     ; A := 1 << 6SFUN   (initial status)
067213  006031  STA  ,X 31          ; =: X.SUTHS
067214  135027  JPL I 27 -> 067243  ; CALL ENTIM   (ptr@067243=070421)
067215  124022  JMP 22 -> 067237    ; GO SCWTI
;  --- CLASS 4 : bus-reset control function -----------------------------------
067216  171004  SAT  4
067217  140061  SKP IF DD EQL ST    ; if D = 4
067220  124016  JMP 16 -> 067236    ; else illegal
067221  050731  LDT  ,B -47         ; T := NCROK
067222  175006  BSKP ZRO 0 DT       ; test 8SRST (reset already running?)
067223  124007  JMP  7 -> 067232    ; already in progress -> intermediate return
067224  014734  STX  ,B -44         ; X =: SCRXR
067225  146147  RADD CLD SL DX
067226  014733  STX  ,B -45         ; L =: SCRLR    (save registers)
067227  004732  STA  ,B -46         ; A =: SCRCO
067230  125014  JMP I 14 -> 067244  ; GO FAR SCRST (ptr@067244=070635)
067231  124004  JMP  4 -> 067235
067232  146106  RADD CLD 0 DT       ; T := 0
067233  146404  RADD AD1 0 DL       ; L := L+1     (intermediate return address)
067234  146542  RADD AD1 CLD SL DP  ; P := L+1     (EXITA)
067235  124002  JMP  2 -> 067237
067236  171010  SAT  10             ; T := ILDCO  (illegal function)
067237  125727  JMP I ,B -51        ; GO SCWTI  (indirect via SCWTI pointer cell)
;  --- embedded pointer words (DATA, not code) --------------------------------
067240  067746  .word 067746        ; -> EXDRI
067241  070261  .word 070261        ; -> INITO
067242  070165  .word 070165        ; -> SELEC
067243  070421  .word 070421        ; -> ENTIM
067244  070635  .word 070635        ; -> SCRST
067246  071121  .word 071121        ; -> (MSGOUT phase / SCISR family)


; ============================================================================
;  SCINT = 067247B   INTERRUPT / COMPLETION HANDLER.
;    Read device status, split hardware-error vs NCR interrupt, then either
;    hand to the expected-interrupt service (SCISR) or run the phase-decode
;    ladder (A=4 disconnect, A=1 arbitration won, A=20 reconnect, parity, ...).
;    dd off 30030 : entry word 050775.
; ============================================================================
067247  050775  LDT  ,B -3          ; T := HDEV
067250  173004  AAT  4              ; T := HDEV + RSTAU   (read status)
067251  150415  IOXT                ; A := device status
067252  171464  SAX  64
067253  144457  RAND SA DX          ; X := 64 /\ A
067254  133014  JXZ 14 -> 067270    ; no hw-error bits -> straight to NCR check
067255  175025  BSKP ZRO 20 DA      ; test bit 2 (controller busy)
067256  125727  JMP I ,B -51        ; GO SCWTI (controller busy, leave)
067257  004676  STA  ,B -102        ; A =: SCSSR
067260  175255  BSKP ONE 50 DA      ; test bit 5 (SCSI BUS RESET received)
067261  124003  JMP  3 -> 067264
067262  171043  SAT  43             ; T := SBRST
067263  125062  JMP I 62 -> 067345  ; GO FAR SCDIS (ptr@067345=067724)
067264  175245  BSKP ONE 40 DA      ; test bit 4 (initiator detected error)
067265  124003  JMP  3 -> 067270
067266  135060  JPL I 60 -> 067346  ; CALL SCIDE (ptr@067346=071754)
067267  044676  LDA  ,B -102        ; A := SCSSR
067270  004676  STA  ,B -102        ; A =: SCSSR
067271  175315  BSKP ONE 110 DA     ; test bit 11 (INTERRUPT FROM NCR)
067272  124161  JMP 161 -> 067453    ; not from NCR -> tail (re-enable + GO SCWTI)
067273  170400  SAA  0
067274  050775  LDT  ,B -3          ; T := HDEV
067275  173005  AAT  5              ; T := HDEV + WCONT
067276  150415  IOXT                ; "0" -> WCONT  (clear to memory)
067277  173043  AAT  43             ; T += RAUXS-WCONT
067300  150415  IOXT                ; read AUXILIARY STATUS
067301  156770  SAD ZIN SHR 10
067302  173004  AAT  4              ; T += RITRG-RAUXS
067303  150415  IOXT                ; read INTERRUPT REGISTER
067304  156770  SAD ZIN SHR 10
067305  146115  RADD CLD SD DA      ; A := D  (new interrupt status)
067306  004677  STA  ,B -101        ; A =: SCNIS
067307  000700  STZ  ,B -100        ; 0 =: SCCCW
067310  070037  AND 37              ; A := SCNIS /\ 177500  (masked below)
067311  050701  LDT  ,B -77         ; T := SCEIM (expected interrupt mask)
067312  140065  SKP IF DA EQL ST    ; if (A/\177500) = SCEIM  -> EXPECTED
067313  124003  JMP  3 -> 067316
067314  135702  JPL I ,B -76        ; CALL SCISR (via SCISR pointer cell)   ** expected int **
067315  124136  JMP 136 -> 067453   ; -> tail
;  --- UNEXPECTED interrupt: decode phase from A = SCNIS >> 8 -----------------
067316  156570  SHA ZIN SHR 10      ; A := SCNIS >> 8   (phase code)
067317  054743  LDX  ,B -35         ; X := BUSFL
067320  171004  SAT  4
067321  140065  SKP IF DA EQL ST    ; if A = 4 (DISCONNECT) and BUSFL<>0 ...
067322  124034  JMP 34 -> 067356
067323  133033  JXZ 33 -> 067356    ; BUSFL = 0 -> not our disconnect
067324  054720  LDX  ,B -60         ; X := SCCSU  (currently selected unit)
067325  133003  JXZ 3 -> 067330
067326  135022  JPL I 22 -> 067350  ; CALL DCTHR (ptr@067350=070341)   ** DISCONNECT THREAD **
067327  124015  JMP 15 -> 067344
067330  044743  LDA  ,B -35         ; A := BUSFL       (SCCSU = 0 path)
067331  175255  BSKP ONE 50 DA      ; test 6SARB (arbitration in progress?)
067332  124012  JMP 12 -> 067344
067333  054721  LDX  ,B -57         ; X := SCWAQ
067334  042035  MIN  ,X 35          ; count retries: MIN SCWAQ.SUTMR
067335  146107  RADD CLD 0 DX       ; X := 0
067336  133006  JXZ  6 -> 067344
067337  000742  STZ  ,B -36         ; 0 =: SCTST
067340  135011  JPL I 11 -> 067351  ; CALL RSTMR (ptr@067351=070375)
067341  135011  JPL I 11 -> 067352  ; CALL RFWAQ (ptr@067352=070250)
067342  171040  SAT  40             ; T := NESER  (not able to select device)
067343  135010  JPL I 10 -> 067353  ; CALL TEROP (ptr@067353=070500)
067344  125010  JMP I 10 -> 067354  ; GO FAR BUSFP (ptr@067354=067745)
;  --- embedded pointer island (DATA) ----------------------------------------
067345  067724  .word 067724        ; -> SCDIS
067346  071754  .word 071754        ; -> SCIDE
067350  070341  .word 070341        ; -> DCTHR
067351  070375  .word 070375        ; -> RSTMR
067352  070250  .word 070250        ; -> RFWAQ
067353  070500  .word 070500        ; -> TEROP
067354  067745  .word 067745        ; -> BUSFP
;  --- A = 1 : arbitration WON ------------------------------------------------
067356  171001  SAT  1
067357  140065  SKP IF DA EQL ST    ; if A = 1
067360  124016  JMP 16 -> 067376
067361  175257  BSKP ONE 50 DX      ; test 6SARB in BUSFL(X)
067362  124014  JMP 14 -> 067376
067363  135076  JPL I 76 -> 067461  ; CALL RFWAQ (ptr@067461=070250)  (won: leave queue)
067364  044076  LDA 76
067365  072031  AND ,X 31           ; CNCLR/\X.SUTHS
067366  174235  BSET ONE 30 DA      ; BONE 6SCRP  (connect/reconnect phase)
067367  004743  STA  ,B -35         ; A =: BUSFL
067370  135073  JPL I 73 -> 067463  ; CALL CNTHR (ptr@067463=070321)   ** CONNECT PATH **
067371  000742  STZ  ,B -36         ; 0 =: SCTST
067372  135072  JPL I 72 -> 067464  ; CALL ENTIM (ptr@067464=070421)   ** START TIMER **
067373  024252  LDD -126
067374  020701  STD  ,B -77         ; RBSIR =: SCNIH   (allow bus service ints)
067375  124056  JMP 56 -> 067453    ; -> tail
;  --- A = 20 : RECONNECT -----------------------------------------------------
067376  171020  SAT  20
067377  140065  SKP IF DA EQL ST    ; if A = 20
067400  124012  JMP 12 -> 067412
067401  174057  BSET ZRO 50 DX      ; BZERO 6SARB
067402  174157  BSET ZRO 150 DX     ; BZERO 6SRST
067403  140007  SKP IF DX EQL 0     ; ... = 0 ?
067404  124006  JMP  6 -> 067412
067405  174237  BSET ONE 30 DX      ; X BONE 6SCRP
067406  014743  STX  ,B -35         ; =: BUSFL   (indicate reconnect)
067407  024236  LDD -142
067410  020701  STD  ,B -77         ; RBSIR =: SCNIH
067411  124042  JMP 42 -> 067453    ; -> tail
;  --- A = 1 & NCROK.8SDIA : SELFTEST finished --------------------------------
067412  171001  SAT  1
067413  140065  SKP IF DA EQL ST
067414  124006  JMP  6 -> 067422
067415  050731  LDT  ,B -47         ; T := NCROK
067416  175226  BSKP ONE 20 DT      ; test 8SDIA
067417  124003  JMP  3 -> 067422
067420  125045  JMP I 45 -> 067465  ; GO FAR STFIN (ptr@067465=071031)
067421  124032  JMP 32 -> 067453
;  --- A = SCEIM>>8 & parity : message/initiator parity error -----------------
067422  050701  LDT  ,B -77         ; T := SCEIM
067423  156170  SHT ZIN SHR 10
067424  140065  SKP IF DA EQL ST    ; A = SCEIM>>8 ?
067425  124024  JMP 24 -> 067451
067426  175261  BSKP ONE 60 DD      ; test PARIT in D
067427  124022  JMP 22 -> 067451
067430  170411  SAA 11
067431  070743  AND ,B -35          ; 11/\BUSFL
067432  171001  SAT  1
067433  140065  SKP IF DA EQL ST    ; = 1 ?
067434  124015  JMP 15 -> 067451
067435  040745  MIN  ,B -33         ; MIN SCNPE (count parity errors)
067436  144400  RAND 0 0
067437  170417  SAA 17
067440  070743  AND ,B -35          ; 17/\BUSFL
067441  171007  SAT  7
067442  140065  SKP IF DA EQL ST    ; = 7 ?  (message phase)
067443  124003  JMP  3 -> 067446
067444  135022  JPL I 22 -> 067466  ; CALL SCMPE (ptr@067466=071746)  message parity err
067445  124002  JMP  2 -> 067447
067446  135021  JPL I 21 -> 067467  ; CALL SCIDE (ptr@067467=071754)  initiator det err
067447  135702  JPL I ,B -76        ; CALL SCISR
067450  124003  JMP  3 -> 067453
067451  171060  SAT  60             ; T := NCRER
067452  125016  JMP I 16 -> 067470  ; GO FAR SCDIS (ptr@067470=067724) illegal interrupt
;  --- SCINT COMMON TAIL : activate + enable interrupt, then busy return ------
067453  170405  SAA  5
067454  074700  ORA  ,B -100        ; A := 5 \/ SCCCW
067455  050775  LDT  ,B -3          ; T := HDEV
067456  173005  AAT  5              ; T := HDEV + WCONT
067457  150415  IOXT                ; WCONT := 5\/SCCCW  (ACTIVATE + ENABLE INT)
067460  125727  JMP I ,B -51        ; GO SCWTI
;  --- embedded pointer island (DATA) ----------------------------------------
067461  070250  .word 070250        ; -> RFWAQ
067463  070321  .word 070321        ; -> CNTHR
067464  070421  .word 070421        ; -> ENTIM
067465  071031  .word 071031        ; -> STFIN
067466  071746  .word 071746        ; -> SCMPE
067467  071754  .word 071754        ; -> SCIDE
067470  067724  .word 067724        ; -> SCDIS


; ============================================================================
;  SCDIS = 067724B   FATAL ERROR -> RESET SCSI BUS.
;    Set X.SUTRG (if selected + still 0), snapshot BUSFL/SCSSR/SCNIS/CMSGI/CMSGO
;    into SEST1..SEST5, then GO FAR SCRST.
; ============================================================================
067724  054720  LDX  ,B -60         ; X := SCCSU
067725  133004  JXZ  4 -> 067731
067726  046033  LDA  ,X 33          ; A := X.SUTRG
067727  131402  JAF  2 -> 067731    ; if SUTRG <> 0 skip
067730  012033  STT  ,X 33          ; T =: X.SUTRG   (record status)
067731  044743  LDA  ,B -35         ; A := BUSFL
067732  004746  STA  ,B -32         ;      =: SEST1
067733  044676  LDA  ,B -102        ; SCSSR
067734  004747  STA  ,B -31         ;      =: SEST2
067735  044677  LDA  ,B -101        ; SCNIS
067736  004750  STA  ,B -30         ;      =: SEST3
067737  044717  LDA  ,B -61         ; CMSGI
067740  004751  STA  ,B -27         ;      =: SEST4
067741  044716  LDA  ,B -62         ; CMSGO
067742  004752  STA  ,B -26         ;      =: SEST5
067743  010744  STT  ,B -34         ; T =: SCLRE
067744  125020  JMP I 20 -> 067764  ; GO FAR SCRST (ptr@067764=070635)

; ============================================================================
;  BUSFP = 067745B   BUS FREE PHASE.  Just re-check the arbitration queue.
; ============================================================================
067745  135020  JPL I 20 -> 067765  ; CALL SELEC (ptr@067765=070165)   ** re-arbitrate **
;   (falls straight into EXDRI)

; ============================================================================
;  EXDRI = 067746B   EXIT DRIVER.  X = unit datafield or 0, T = status.
;    Compute the caller return: SULRG+2 (ok), +1 (intermediate, T<0),
;    or SULRG (error, T>0, MIN ERCNT).
; ============================================================================
067746  142007  SKP IF DX UEQ 0
067747  125727  JMP I ,B -51        ; X = 0 -> GO SCWTI  (no caller to return to)
067750  046032  LDA  ,X 32          ; A := X.SULRG
067751  140006  SKP IF DT EQL 0     ; if T = 0 (OK)
067752  124003  JMP  3 -> 067755
067753  172402  AAA  2              ; A := SULRG + 2   (normal return)
067754  146152  RADD CLD SA DP      ; P := A
067755  143006  SKP IF DT LST 0     ; if T < 0 (intermediate)
067756  124003  JMP  3 -> 067761
067757  172401  AAA  1              ; A := SULRG + 1   (intermediate return)
067760  124003  JMP  3 -> 067763
067761  040756  MIN  ,B -22         ; MIN ERCNT         (error return)
067762  144400  RAND 0 0
067763  146152  RADD CLD SA DP      ; P := A            (error: SULRG+0)
067764  070635  .word 070635        ; -> SCRST  (used by SCDIS above)
067765  070165  .word 070165        ; -> SELEC  (used by BUSFP above)
067770  071121  .word 071121        ; -> MSGOUT/EMSGO family


; ============================================================================
;  SELEC = 070165B   ARBITRATION / SELECTION.
;    Save regs, disable interrupt (SCEIM:=-1), load SCWAQ.
;    EMPTY  -> BUSFL := 0 (declare bus free), re-enable, return.
;    NON-EMPTY -> program NCR select (WDESI/WTCM/WTC2/WTCL/WNCOM), arm the
;                 select timeout (SCTST:=1, TMR:=-5), re-enable, return.
;    dd off 30954 : entry word 030374.
; ============================================================================
070165  030374  STF  -4             ; TAD =: SVTAD      (save)
070166  014376  STX  -2             ; X   =: SAVXR
070167  170777  SAA  -1
070170  004701  STA  ,B -77         ; -1 =: SCEIM       (DISABLE expected interrupt)
070171  054721  LDX  ,B -57         ; X := SCWAQ        ** the arbitration queue **
070172  133046  JXZ 46 -> 070240    ; IF SCWAQ = 0 -> BUS-FREE leg   [dd 133046]
;  --- NON-EMPTY : start a select on the head element ------------------------
070173  170400  SAA  0
070174  050775  LDT  ,B -3          ; T := HDEV
070175  173005  AAT  5              ; T += WCONT
070176  150415  IOXT                ; "0" -> WCONT  (clear to memory)
070177  174255  BSET ONE 50 DA      ; A := 1 << 6SARB
070200  004743  STA  ,B -35         ; =: BUSFL          (INDICATE ARBITRATION)
070201  046000  LDA  ,X 0           ; A := X.SUDLU
070202  156564  SHA ZIN SHR 14      ; SCSI ident (>>12)
070203  173042  AAT  42             ; T += WDESI-WCONT
070204  150415  IOXT                ; WDESI := ident
070205  024703  LDD  ,B -75         ; D := WATFS  (waiting time)
070206  173022  AAT  22             ; T += WTCM-WDESI
070207  150415  IOXT                ; WTCM := waiting-time MSB
070210  156610  SAD ZIN 10
070211  173002  AAT  2              ; WTC2
070212  150415  IOXT
070213  156610  SAD ZIN 10
070214  173002  AAT  2              ; WTCL
070215  150415  IOXT
070216  170410  SAA  10             ; select command = 10
070217  056030  LDX  ,X 30          ; X := X.SUCON
070220  175247  BSKP ONE 40 DX      ; test 4SINA (initiate-without-atn?)
070221  124002  JMP  2 -> 070223
070222  170411  SAA  11             ;   ...select-with-ATN command = 11
070223  173346  AAT -32             ; T += WNCOM-WTCL
070224  150415  IOXT                ; WNCOM := select command   ** SELECT ON THE WIRE **
070225  054742  LDX  ,B -36         ; X := SCTST
070226  171002  SAT  2
070227  143476  SKP IF DT MLST SX   ; if SCTST >> 2 (a test/timer already owns TMR)
070230  124003  JMP  3 -> 070233
070231  044773  LDA  ,B -5          ; A := TMR
070232  006035  STA  ,X 35          ;      =: X.SUTMR    (save current timer)
070233  170401  SAA  1
070234  004742  STA  ,B -36         ; 1 =: SCTST
070235  170773  SAA  -5
070236  004773  STA  ,B -5          ; -5 =: TMR          (ENABLE SELECT TIMEOUT)
070237  124002  JMP  2 -> 070241
;  --- EMPTY : SCWAQ = 0 -> declare BUS FREE -----------------------------------
070240  000743  STZ  ,B -35         ; 0 =: BUSFL         ** SCWAQ EMPTY -> BUS FREE **
;  --- common tail : enable interrupt, restore, return -----------------------
070241  170405  SAA  5
070242  050775  LDT  ,B -3          ; T := HDEV
070243  173005  AAT  5              ; T += WCONT
070244  150415  IOXT                ; WCONT := 5  (ENABLE INTERRUPT)
070245  054317  LDX  -61            ; X := SAVXR
070246  034313  LDF  -65            ; TAD := SVTAD
070247  146142  EXIT

; ============================================================================
;  RFWAQ = 070250B   REMOVE HEAD FROM WAIT-FOR-ARBITRATION QUEUE.
; ============================================================================
070250  054721  LDX  ,B -57         ; X := SCWAQ
070251  133004  JXZ  4 -> 070255    ; empty -> nothing to do
070252  046027  LDA  ,X 27          ; A := X.SULINK
070253  004721  STA  ,B -57         ;      =: SCWAQ    (unlink head)
070254  002027  STZ  ,X 27          ; 0 =: X.SULINK
070255  146142  EXIT


; ============================================================================
;  INITO = 070261B   INITIALIZE OPERATION + SPLICE ONTO SCWAQ TAIL.
;    Build INOPR/INABO/INBDR thread status from SUCON class, seed the saved
;    data pointer/byte count, then walk SULINK to the tail and link X in.
;    THIS is the ONLY writer that appends to SCWAQ.  dd off 31074 : word 046030.
; ============================================================================
070261  046030  LDA  ,X 30          ; A := X.SUCON
070262  146151  RADD CLD SA DD      ; D := A
070263  156570  SHA ZIN SHR 10      ; A := SUCON >> 8   (op class)
070264  131406  JAF  6 -> 070272    ; class <> 0 -> abort / bus-device-reset
070265  044371  LDA  -7             ; A := INOPR        (normal operation)
070266  175261  BSKP ONE 60 DD      ; test 4SRCA in D
070267  124002  JMP  2 -> 070271
070270  174315  BSET ONE 110 DA     ; set 6SRFD (return on command accepted)
070271  124006  JMP  6 -> 070277
070272  172777  AAA  -1             ; A-1 : =0 -> abort, else bus device reset
070273  131403  JAF  3 -> 070276    ; A-1 <> 0 -> INBDR
070274  044363  LDA  -15            ; A := INABO  (abort thread status)
070275  124002  JMP  2 -> 070277
070276  044362  LDA  -16            ; A := INBDR  (bus-device-reset thread status)
070277  006031  STA  ,X 31          ; A =: X.SUTHS  (thread status word)
070300  170777  SAA  -1
070301  006013  STA  ,X 13          ; -1 =: X.SUSTA     (initial status)
070302  026001  LDD  ,X 1           ; D := X.SUIDP
070303  022007  STD  ,X 7           ;      =: X.SUSDP   (initial data ptr)
070304  026003  LDD  ,X 3           ; D := X.SUIBC
070305  022011  STD  ,X 11          ;      =: X.SUSBC   (initial byte count)
070306  170776  SAA  -2
070307  006035  STA  ,X 35          ; -2 =: X.SUTMR
070310  044060  LDA  60             ; A := (SCWAQ-SULINK) constant
070311  146035  RADD SB DA          ; A := "SCWAQ-SULINK" + B   (queue anchor)
070312  146176  RADD CLD SX DT      ; T := X            (element to link)
070313  146157  RADD CLD SA DX      ; loop: X := A
070314  046027  LDA  ,X 27          ; A := X.SULINK
070315  131002  JAZ  2 -> 070317    ; while SULINK <> 0 keep walking
070316  124375  JMP -3 -> 070313
070317  012027  STT  ,X 27          ; T =: X.SULINK     ** LINKED ONTO SCWAQ TAIL **
070320  146142  EXIT

; ============================================================================
;  CNTHR = 070321B   CONNECT PHYSICAL PATH (arbitration won / reconnect).
;    Mark SCCSU := X, (re)send pending message-out if 6SMSO, restore data ptrs.
; ============================================================================
070321  014720  STX  ,B -60         ; X =: SCCSU        (path connected)
070322  046031  LDA  ,X 31          ; A := X.SUTHS
070323  175325  BSKP ONE 120 DA     ; test 6SMSO (message-out pending)
070324  124007  JMP  7 -> 070333
070325  070044  AND 44              ; A/\377 (message)
070326  004716  STA  ,B -62         ;      =: CMSGO
070327  170403  SAA  3
070330  050775  LDT  ,B -3          ; T := HDEV
070331  173043  AAT  43             ; T += WNCOM
070332  150415  IOXT                ; WNCOM := 3  ("Set ATN" to NCR)
070333  026007  LDD  ,X 7           ; D := X.SUSDP
070334  020723  STD  ,B -55         ;      =: SCCDP
070335  026011  LDD  ,X 11          ; D := X.SUSBC
070336  020725  STD  ,B -53         ;      =: SCCBC
070337  146142  EXIT

; ============================================================================
;  DCTHR = 070341B   DISCONNECT LOGICAL THREAD.
;    If X.SUTRG=0 and BUSFL.6SDIS: save thread status (DCCLR\/CMSGO) and return
;    (intermediate if 6SRFD, else "no return"); otherwise CALL TEROP.  Always
;    clears SCCSU and returns via HOME4.   ** the A=4 disconnect leg **
; ============================================================================
070341  146145  RADD CLD SL DA      ; A := L
070342  004376  STA  -2             ;      =: HOME4     (return address)
070343  052033  LDT  ,X 33          ; T := X.SUTRG
070344  140006  SKP IF DT EQL 0     ; if SUTRG = 0
070345  124020  JMP 20 -> 070365    ;   else -> CALL TEROP
070346  044743  LDA  ,B -35         ; A := BUSFL
070347  175305  BSKP ONE 100 DA     ; test 6SDIS (disconnect message seen)
070350  124015  JMP 15 -> 070365    ;   not 6SDIS -> CALL TEROP
070351  175315  BSKP ONE 110 DA     ; test 6SRFD (return on command accepted)
070352  124006  JMP  6 -> 070360
070353  070017  AND 17              ; A/\DCCLR ...
070354  074716  ORA  ,B -62         ; ... \/ CMSGO
070355  006031  STA  ,X 31          ;      =: X.SUTHS   (save thread status)
070356  171377  SAT  -1             ; T := -1          (INTERMEDIATE return)
070357  124005  JMP  5 -> 070364
070360  070012  AND 12              ; A/\DCCLR (no 6SRFD)
070361  074716  ORA  ,B -62         ; \/ CMSGO
070362  006031  STA  ,X 31          ;      =: X.SUTHS
070363  146107  RADD CLD 0 DX       ; X := 0           (NO return)
070364  124002  JMP  2 -> 070366
070365  135006  JPL I 6 -> 070373   ; CALL TEROP (ptr@070373=070500)   ** terminate op **
070366  000720  STZ  ,B -60         ; 0 =: SCCSU       (path disconnected)
070367  125351  JMP I -27 -> HOME4  ; GO HOME4  (return)
070373  070500  .word 070500        ; -> TEROP

; ============================================================================
;  RSTMR = 070375B   RESTART LUN TIMER (reload TMR from head of timer queue).
; ============================================================================
070375  044742  LDA  ,B -36         ; A := SCTST
070376  131421  JAF 21 -> 070417    ; if SCTST <> 0 -> just EXIT (someone owns TMR)
070377  014375  STX  -3             ; X =: SAVRX
070400  054722  LDX  ,B -56         ; X := SCTQP       (timer queue head)
070401  133014  JXZ 14 -> 070415    ; empty -> disable timer
070402  046035  LDA  ,X 35          ; A := X.SUTMR
070403  004773  STA  ,B -5          ;      =: TMR       (new time value)
070404  131004  JAZ  4 -> 070410
070405  002035  STZ  ,X 35          ; 0 =: X.SUTMR
070406  014742  STX  ,B -36         ; X =: SCTST        (timer running)
070407  124005  JMP  5 -> 070414
070410  045060  LDA I 60            ;  (SCTST scratch reload)
070411  004730  STA  ,B -50
070412  146135  RADD CLD SB DA
070413  005055  STA I 55
070414  124002  JMP  2 -> 070416
070415  000773  STZ  ,B -5          ; 0 =: TMR          (disable timer)
070416  054356  LDX  -22            ; X := SAVRX
070417  146142  EXIT

; ============================================================================
;  ENTIM = 070421B   ENABLE LUN TIMER (insert this element into the timer queue).
; ============================================================================
070421  030053  STF  53             ; TAD =: TITAD
070422  144047  SWAP SL DX
070423  014050  STX  50             ; L,X =: HOME5/SAVX
070424  054742  LDX  ,B -36         ; X := SCTST
070425  171002  SAT  2
070426  143476  SKP IF DT MLST SX   ; SCTST >> 2 ?
070427  124004  JMP  4 -> 070433
070430  044773  LDA  ,B -5          ; A := TMR
070431  006035  STA  ,X 35          ;      =: X.SUTMR   (save current timer)
070432  000742  STZ  ,B -36         ; 0 =: SCTST
;   (070433..070471: compute max time via SUCON, walk SCTQP-SULINK, splice,
;    then T=:X.SUTMR ; CALL RSTMR ; GO HOME5 - see NPL ENTIM)
070465  135005  JPL I 5 -> 070472   ; CALL RSTMR (ptr@070472=070375)
070472  070375  .word 070375        ; -> RSTMR

; ============================================================================
;  TEROP = 070500B   TERMINATE OPERATION.  T = status in.
;    If X.SUTRG=0 and SCCSU=X: save data pointers, derive final status
;    (0 = command complete, NOSST/UNDIS/TRANE/MNIBT variants); else A=:T.
;    Clears X.SUTHS (mark free) then falls into DITIM (disable LUN timer).
;    dd off 31360 : entry word 046033.
; ============================================================================
070500  046033  LDA  ,X 33          ; A := X.SUTRG
070501  131460  JAF 60 -> 070561    ; SUTRG <> 0 -> "error in operation" (A=:T; -> DITIM)
070502  044720  LDA  ,B -60         ; A := SCCSU
070503  140075  SKP IF DA EQL SX    ; if SCCSU = X
070504  124044  JMP 44 -> 070550    ;   else -> timer-op branch
070505  024723  LDD  ,B -55         ; D := SCCDP
070506  022007  STD  ,X 7           ;      =: X.SUSDP  (save data pointer)
070507  024725  LDD  ,B -53         ; D := SCCBC
070510  022011  STD  ,X 11          ;      =: X.SUSBC
;   (070511..070557: derive final T; 070560 clear X.SUTHS; -> DITIM @070561)
;   ** DITIM ~070561 unlinks this element from the timer queue, CALL RSTMR **

; ============================================================================
;  SCRST = 070635B   START RESET SEQUENCE ON SCSI BUS  (GO FAR from SCLLD/SCDIS).
;    Mark all active ops for recovery (8SCLR), lock the interface (SCEIM:=-1,
;    SCTST:=2), disconnect+clear the NCR, drive RST for >=250us or handle an
;    incoming RST, launch selftest/diagnostic, then GO SCTIO.
; ============================================================================
070635  044731  LDA  ,B -47         ; A := NCROK
070636  131463  JAF 63 -> 070721    ; NCROK <> 0 -> else-path (NCROK/\CLMSK)
070637  054742  LDX  ,B -36         ; X := SCTST
070640  171002  SAT  2
070641  143476  SKP IF DT MLST SX   ; SCTST >> 2 ? (save current timer)
070642  124003  JMP  3 -> 070645
070643  044773  LDA  ,B -5          ; A := TMR
070644  006035  STA  ,X 35          ;      =: X.SUTMR
070645  044676  LDA  ,B -102        ; A := SCSSR
070646  175255  BSKP ONE 50 DA      ; test bit 5 (are WE receiving a RST?)
070647  124006  JMP  6 -> 070655
070650  044731  LDA  ,B -47         ; A := NCROK
070651  174215  BSET ONE 10 DA      ; BONE 8SRIN (receiving reset)
070652  004731  STA  ,B -47         ;      =: NCROK
070653  171043  SAT  43             ; T := SBRST
070654  124002  JMP  2 -> 070656
070655  171051  SAT  51             ; T := LIRST (locally initiated reset)
070656  054720  LDX  ,B -60         ; X := SCCSU
070657  133013  JXZ 13 -> 070672
070660  046033  LDA  ,X 33          ; A := X.SUTRG
070661  131402  JAF  2 -> 070663
070662  012033  STT  ,X 33          ; T =: X.SUTRG   (set status)
;   (070663..: BUSFL BONE 6SRST ; NCROK BONE 8SCLR ; walk SCTQP marking active
;    ops for recovery ; NCROK BONE 8SRST ; SCEIM:=-1 ; SCTST:=2 ; drive RST ;
;    selftest/diagnostic ; GO SCTIO - see NPL SCRST/STFIN)


; ============================================================================
;  NEWPH / phase handlers / RINFO / message handlers  (071xxx)
;  Reached from SCISR (the "expected interrupt" service) when a bus-service
;  interrupt reports a phase change.  NEWPH computes bytes-transferred and the
;  new MAR, sets BUSFL to the new phase, then:
;     GOSW DAOPH,DAIPH,COMPH,STAPH,ILOPH,ILIPH,MSOPH,MSIPH
;  Each phase programs the NCR transfer counter (WTCM/WTC2/WTCL) and command
;  (WNCOM 224 = "DMA mode + transfer info", 124 = "single byte") to move the
;  DATA / COMMAND(CDB) / STATUS / MESSAGE bytes.  RINFO (071325) polls AUXS for
;  DARGF and reads one byte from RNDAT, or GO FAR SCINT on a pending interrupt.
;  Full logic is authoritative in the NPL (lines 655-918); the carved bytes for
;  this region live in ../SCSI-DRIVER/_driver.dis.  The COMMAND phase (COMPH)
;  is where the CDB built by the disk layer is DMA'd to the target - see README
;  "How a queued block-0 transfer reaches the wire".
; ============================================================================


; ============================================================================
;  DISK-LAYER -> DRIVER HAND-OFF  (boundary; code lives in the disk-layer path)
;    The disk layer's EXCOM builds the SCSI CDB and executes it by calling the
;    driver through the pointer cell at 063453 (= 067160 = SCLLD).  Verified:
;      word @063453 = 067160   [dd off 26198 : bytes 6e70 = 067160]
;    So "JPL I (SCLLD" / "GO SCLLD" from the disk layer enters SCLLD with
;    A = function, X = unit datafield exactly as documented above.
;    (The CACOB build-CDB / EXCOM execute / 5SCIN re-inquiry gate internals are
;     owned by the SCSI-DISKLAYER-COMPLETE carve; only this pointer is in scope.)
; ============================================================================
063453  067160  .word 067160        ; -> SCLLD  (disk-layer call vector)
