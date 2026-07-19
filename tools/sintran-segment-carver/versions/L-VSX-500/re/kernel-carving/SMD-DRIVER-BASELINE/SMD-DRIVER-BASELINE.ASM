; ============================================================================
; SMD / Winchester / ST506 MASS-STORAGE DISK DRIVER  -  L07 baseline carve
; ============================================================================
; SINTRAN III  L-VSX-500  (running system L-VSX-500, symbols L07).
;
; Segment : 065-S3SIPIT   (Save of IPIT)   load base = 032000B  (13312 dec)
;           == 066-S3IIPIT (Image of IPIT) - byte-identical (0 diffs, cmp).
; Symbols : SYMBOL-2-LIST.SYMB.TXT  (L07) - the resident device-driver overlay.
;           The SMD/ST506 driver AND the SCSI driver both live in this one
;           SYMBOL-2-LIST overlay, at non-overlapping addresses:
;               SMD / ST506  ~ 054000B .. 057200B   (this file)
;               SCSI         ~ 057215B .. 070xxxB   (see ../SCSI-DRIVER/)
;
; Ground truth = carved bytes of 065-S3SIPIT.bin. NPL logic (different
; revision, IP-P2-DISK-START.NPL) used for naming only. Every word below was
; dd/byte-verified against 065-S3SIPIT.bin; VERIFIED tags carry the byte offset.
;
; Disassembly reproduced with:
;   python3 -c "d=bytearray(open('065-S3SIPIT.bin','rb').read()); \
;               d[0::2],d[1::2]=d[1::2],d[0::2]; open('/tmp/s.le','wb').write(d)"
;   nd100-dis -a -o -b 13312 /tmp/s.le
; (13312 dec = 032000B = segment load base; -o octal, -a addresses).
;
; Datafield displacement symbols (device datafield, X- or B-relative):
;   TRNSF = 177766B (-12 in field)  device-type tag word ("BDISK" for SMD/cartridge)
;   CTRLR = 000040B                 saved controller link (WSEEK writes L here)
;   DODMA = 000013B                 SMD level-11 DMA transfer-primitive pointer
;                                    (CALL DODMA = JPL I ,B 13)
;   (SLINK = 4, TYPCO = 11 : per-unit sort-link / type-and-condition fields)
; ============================================================================


; ---------------------------------------------------------------------------
; BSEEK  056011B   - initiate parallel seek for sorting abstraction (level 11)
;   NPL: BSEEK: T:=X; A:=X.SLINK.TYPCO; X:=T
;              IF A NBIT SSEEK THEN EXIT
;              IF "TRNSF" >< "BDISK" THEN EXIT     % not parallel seek
; VERIFIED (dd bytes @ byte 20498: cc 7e  5c 04  4c 09  cc 77)
; ---------------------------------------------------------------------------
056011  146176   RADD CLD SX DT        ; T := X                 (save unit ptr)
056012  056004   LDX  ,X 4             ; X := X.SLINK           (sort-link)
056013  046011   LDA  ,X 11            ; A := X.TYPCO           (type/cond word)
056014  146167   RADD CLD ST DX        ; X := T                 (restore unit ptr)
056015  175175   BSKP ZRO 170 DA       ; test bit SSEEK in A
056016  124002   JMP  056020
056017  146142   EXIT                  ;   IF A NBIT SSEEK -> EXIT
056020  044766   LDA  ,B -12           ; A := "TRNSF"           (device-type tag)
056021  050046   LDT  46               ; T := "BDISK" constant
056022  142065   SKP  IF DA UEQ ST     ; IF TRNSF = BDISK skip EXIT
056023  124002   JMP  056025
056024  146142   EXIT                  ;   IF TRNSF >< BDISK -> EXIT (no par.seek)
056025  046011   LDA  ,X 11            ; ... (continue: A \/ 020004; initiate seek)
                                        ;     IOX seek to controller


; ---------------------------------------------------------------------------
; WSEEK  056072B   - WAIT-ON-SEEK routine for the SMD controller
;   NPL: WSEEK: A:=L=:"CTRLR"          % save return link into datafield CTRLR
;              ... MIN LOOP; ... IOXT
;              A \/ 030005; *IOXT      % enable int, seek-complete search
;              CALL ID11               % wait on seek interrupt
;              T:=HDEV+RSC; *IOXT      % read seek condition
; VERIFIED (dd bytes @ byte 20596: cc 65  09 20)
; ---------------------------------------------------------------------------
056072  146145   RADD CLD SL DA        ; A := L                 (return link)
056073  004440   STA  ,B 40            ; =: CTRLR               (datafield word 40)
056074  170677   SAA  -101
056075  004374   STA  -4
056076  040373   MIN  -5               ; LOOP: count down
056077  124002   JMP  056101
056100  125440   JMP  I ,B 40          ;   -> error exit via CTRLR
056101  050775   LDT  ,B -3
056102  173004   AAT  4                ; T := HDEV + 4  (read status reg)
056103  150415   IOXT                  ; *IOXT  read controller status
056104  175375   BSKP ONE 170 DA
056105  124005   JMP  056112
056106  173001   AAT  1
056107  044414   LDA  ,B 14
056110  150415   IOXT                  ; *IOXT
056111  124365   JMP  056076           ;   spin
056112  175045   BSKP ZRO 40 DA
056113  125440   JMP  I ,B 40          ;   error -> CTRLR
056114  175235   BSKP ONE 30 DA
056115  124361   JMP  056076
056116  044774   LDA  ,B -4
056117  004773   STA  ,B -5
056120  173001   AAT  1
056121  044414   LDA  ,B 14
056122  074045   ORA  45               ; A \/ 030005 (enable int + seek-compl srch)
056123  150415   IOXT                  ; *IOXT
056124  135044   JPL  I 44             ; CALL ID11 (wait on seek interrupt)-> 056170


; ---------------------------------------------------------------------------
; DEVICE-TYPE + FUNCTION-42 GATE   (inside CTRDISK function-decode, ~054463B)
;   NPL (IP-P2-DISK-START.NPL ~054066):
;     IF A=42 AND "TRNSF" >< "BDISK" GO FAR FIN   % READ FORMAT NOT LEGAL IN DRIVER
;   A = filesystem function code (CTRG /\ 77);  42 is OCTAL.
; VERIFIED (dd: 054464=f2 22 =171042; 054472=aa 24 =125044)
; ---------------------------------------------------------------------------
054463  055216   LDX  I -162           ; X := &CTRG (control/function record)
054464  171042   SAT  42               ; T := 42B  (READ FORMAT function)
054465  140065   SKP  IF DA EQL ST     ; IF function = 42 -> fall through to TRNSF test
054466  124005   JMP  054473           ;   function <> 42 -> normal transfer decode
054467  044766   LDA  ,B -12           ; A := "TRNSF"  (device-type tag)
054470  050045   LDT  45               ; T := "BDISK" constant
054471  140065   SKP  IF DA EQL ST     ; IF TRNSF = BDISK skip the FIN jump
054472  125044   JMP  I 44             ;   TRNSF <> BDISK -> GO FAR FIN (054536)
                                        ;   == "READ FORMAT NOT LEGAL IN DRIVER"
054473  044773   LDA  ,B -5            ; (real disk / normal path continues)


; ---------------------------------------------------------------------------
; FIN  054536B    - driver termination / return-format cleanup (GO FAR FIN)
; INFERRED (NPL): for TRNSF=BDISK, function 42 returns a FORMAT NUMBER from a
;   table into the FIRST location of the DMA buffer, WITH NO DISK I/O, then
;   terminates. The store-format-number-to-buffer[0] instruction is not pinned
;   to a single verified word here (FIN cleanup shows STF ,X 2 @054537).
; ---------------------------------------------------------------------------
054536  055216   LDX  I -162           ; X := &CTRG / DMA-buffer descriptor
054537  032002   STF  ,X 2             ; store result/status word
054540  044276   LDA  -102
054541  000007   STZ  7
054542  000077   STZ  77               ; ... terminate (no seek, no DODMA)


; ---------------------------------------------------------------------------
; SSTDI  056504B  - level-11 "start disk transfer" : CALL DODMA
;   NPL: SSTDI: CALL "DODMA"  => STRDISK ; ... ; GO WT11
; VERIFIED (dd bytes @ byte 21128: bb 0b  = 135413 = JPL I ,B 13 = CALL DODMA)
; ---------------------------------------------------------------------------
056504  135413   JPL  I ,B 13          ; CALL DODMA  (datafield word 13 = DMA prim.)
056505  146176   RADD CLD SX DT        ; T := X
056506  146137   RADD CLD SB DX        ; X := B
056507  173441   AAX  41
056510  135003   JPL  I 3              ;   -> STRDISK / STRETRANS dispatch
                                        ;   (block number taken from datafield;
                                        ;    block 0 is NOT special-cased)


; ---------------------------------------------------------------------------
; STRDI  056266B  - STRDISK: build + start the DMA read/write of a phys. block
;   Extracts the function field from datafield word ,X 14 (ABFUN) and issues
;   the transfer. Block 0 is just another physical block here.
; VERIFIED (055xx-056xxx coherent; excerpt)
; ---------------------------------------------------------------------------
056266  146135   RADD CLD SB DA        ; A := B
056271  146145   RADD CLD SL DA        ; A := L (link)
056272  006006   STA  ,X 6
056275  046014   LDA  ,X 14            ; A := ABFUN  (filesystem function word)
056276  070170   AND  170
056277  156572   SHA  ZIN SHR 6        ; extract function field
056300  004414   STA  ,B 14            ; ... then issue DMA (DODMA) for the block
; ============================================================================
; END
; ============================================================================
