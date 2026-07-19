; ============================================================================
;  RCBLO - reserve/read a disk-cache block  (SINTRAN III VSX/500 L07)
; ============================================================================
;  Segment  : 006-S3FS  (File System)   load base 26000B   sha256 b4a563d0...
;  Entry    : RCBLO = 035766B            (FILSYS-SYMBOLS, L07)
;  Siblings : WBLOC 036221B, WCBLO 036357B, WTAPE 036511B  (parallel cache
;             write routines - the sibling-coherence proof that RCBLO is a real
;             routine resident IN 006-S3FS, not a foreign overlay body).
;  Callers  : RXDIR 037643B  (037652: JPL I 23 -> [037675]=035766)
;  Prologue : 003752B (resident reserve/link save) + GSIZE 037101B
;  Purpose  : the disk-buffer-cache block manager. Given a 32-bit block number
;             (D) and an operation code, it (a) searches the cache for the
;             block, and (b) on a miss, dispatches the physical transfer to the
;             device driver through the unit datafield transfer pointer
;             (datafield word ,X 14 -> local 10 -> JPL I ,B 10).
;
;  ALL bytes below were re-read from 006-S3FS.bin with dd/python and equal the
;  values shown (byte offset = (addr - 26000B)*2 decimal). Addresses/values are
;  OCTAL. Comments graded VERIFIED (from bytes) or INFERRED (reasoned).
;
;  *** IMPORTANT disassembler note ***
;  The whole-segment listing annotates every `JPL I <disp>` / `JMP I <disp>`
;  with a P-relative "-> NNN" target. That is CORRECT for P-relative literal
;  pointers (035772 JPL I 63 -> [036055]) but MISLEADING for the four transfer
;  dispatches `135410 JPL I ,B 10`: those are indirect through FRAME word B+10
;  (a runtime pointer), NOT P-relative. The "-> 036145" style comments on the
;  135410 lines are bogus; the real target is MEM[B+10] = the datafield
;  transfer primitive loaded at 036100-036102. This file marks them (RUNTIME).
; ----------------------------------------------------------------------------

; === RCBLO entry + prologue ==================================================
035766  021066   STD I 66        ; save return link  (VERIFIED entry byte 021066)
035767  146145   RADD CLD SL DA  ; A := L  (caller arg shuffle)
035770  146131   RADD CLD SB DD  ; D := B
035771  170017   SAB 17          ; build stack frame (17B words)
035772  135063   JPL I 63        ; CALL [036055]=003752  resident reserve/prologue
035773  135063   JPL I 63        ; CALL [036056]=037101 = GSIZE  (get disk size)
                                  ;   VERIFIED: [036056]=037101 (=GSIZE). GSIZE
                                  ;   reads the IN-CORE disk-parameter tables
                                  ;   (helpers 050124/050220/050223B, bounds->174B);
                                  ;   it issues NO device transfer. INFERRED: a
                                  ;   GSIZE failure or geometry mismatch here can
                                  ;   drive the error path (see 035774 / 036177).
035774  125063   JMP I 63        ; JMP  [036057]=036205  (GSIZE fail-return stub)

; === cache-block search ======================================================
035775  020406   STD ,B 6        ; local 6 := D  = requested block number (32-bit)
035776  054062   LDX 62          ; X := [036061]=050124 area ptr (cache-head getter)
035777  135062   JPL I 62        ; CALL [036061]=050124  get cache/device descriptor
036000  125057   JMP I 57        ; fail -> [036057]=036205
036001  004412   STA ,B 12       ; local 12 := A  = the OPERATION code (masked below)
036002  070060   AND 60          ; isolate op-class bits
036003  146156   RADD CLD SA DT  ; T := A
036004  135057   JPL I 57        ; CALL [036063]=030235  (classify / table lookup)
036005  014411   STX ,B 11       ; local 11 := X = unit/device descriptor pointer
036006  050401   LDT ,B 1
036007  024402   LDD ,B 2
036010  020414   STD ,B 14
036011  135053   JPL I 53        ; CALL [036064]=036616  cache-hash / search helper
036012  125045   JMP I 45        ; fail -> [036057]=036205
036013  004402   STA ,B 2
036014  010413   STT ,B 13
036015  050401   LDT ,B 1
036016  041047   MIN I 47
036017  144400   RAND 0 0
036020  135046   JPL I 46        ; CALL [036066]=034371
036021  046004   LDA ,X 4        ; A := buffer flag word
036022  174175   BSET ZRO 170 DA ; clear bit 15 of A
036023  146151   RADD CLD SA DD
036024  050401   LDT ,B 1
036025  012004   STT ,X 4
036026  044413   LDA ,B 13
036027  006012   STA ,X 12       ; buffer ,X 12 := device id
036030  014416   STX ,B 16       ; local 16 := candidate cache buffer
; --- compare requested block (D / ,B2 / ,B3) against buffer's stored block ---
036031  140061   SKP IF DD EQL ST
036032  124040   JMP 40          ; -> 036072  (miss: go allocate/transfer)
036033  044402   LDA ,B 2
036034  052005   LDT ,X 5        ; buffer block-hi
036035  140065   SKP IF DA EQL ST
036036  124034   JMP 34          ; -> 036072
036037  044403   LDA ,B 3
036040  052006   LDT ,X 6        ; buffer block-lo
036041  140065   SKP IF DA EQL ST
036042  124030   JMP 30          ; -> 036072
; --- cache HIT path (block already resident) ---------------------------------
036043  024414   LDD ,B 14
036044  131403   JAF 3           ; -> 036047
036045  142001   SKP IF DD UEQ 0
036046  124024   JMP 24          ; -> 036072
036047  045020   LDA I 20
036050  135020   JPL I 20        ; CALL [036070]=010506
036051  041020   MIN I 20
036052  144400   RAND 0 0
036053  124071   JMP 71          ; -> 036144  (HIT -> success, NO device read)
036054  147417   ROP NOOP        ; --- literal pointer pool starts here ---
036055  003752   ; ptr -> 003752B  resident reserve/prologue        (VERIFIED)
036056  037101   ; ptr -> 037101B  GSIZE                            (VERIFIED)
036057  036205   ; ptr -> 036205B  local fail-return stub
036060  000002   ; literal 2
036061  050124   ; ptr -> 050124B
036062  000377   ; literal
036063  030235   ; ptr -> 030235B
036064  036616   ; ptr -> 036616B  (in-segment cache helper)
036065  010633   ; literal
036066  034371   ; ptr -> 034371B
036067  120031   ; literal
036070  010506   ; ptr
036071  010634   ; literal

; === cache MISS: build device function + dispatch the transfer ===============
036072  024402   LDD ,B 2        ; reload requested block number
036073  022005   STD ,X 5        ; stamp buffer block-hi/lo := requested block
036074  044413   LDA ,B 13
036075  006012   STA ,X 12       ; stamp buffer device id
036076  045111   LDA I 111
036077  135111   JPL I 111       ; CALL [036210]=010506 (reserve buffer / wait)
; --- load the datafield transfer function pointer into local 10 --------------
036100  054411   LDX ,B 11       ; X := unit/device descriptor            (VERIFIED)
036101  046014   LDA ,X 14       ; A := datafield word 14 = TRANSFER FN PTR (VERIFIED)
036102  004410   STA ,B 10       ; local 10 := that pointer  (the driver entry)
; --- geometry/address arithmetic for the transfer ---------------------------
036103  054416   LDX ,B 16
036104  046005   LDA ,X 5
036105  070104   AND 104
036106  146154   RADD CLD SA DL
036107  052006   LDT ,X 6
036110  024406   LDD ,B 6        ; D := requested block number
036111  146661   RSUB ST DD
036112  147155   RADD ADC CLD SA DA
036113  172777   AAA -1
036114  146645   RSUB SL DA
036115  130003   JAP 3           ; -> 036120
036116  170500   SAA 100         ; A := 100B
036117  124060   JMP 60          ; -> 036177  error/return
036120  050401   LDT ,B 1
036121  054071   LDX 71
036122  135071   JPL I 71        ; CALL [036213]=050124
036123  124054   JMP 54          ; -> 036177 fail
; --- dispatch #1 (primary transfer, function built from op-code ,B 12) -------
036124  070065   AND 65
036125  146151   RADD CLD SA DD
036126  044412   LDA ,B 12       ; A := operation code
036127  070065   AND 65          ; mask op bits
036130  172466   AAA 66          ; + 66B  -> device function code
036131  146116   RADD CLD SD DT
036132  054411   LDX ,B 11
036133  146171   RADD CLD SX DD
036134  054416   LDX ,B 16
036135  135410   JPL I ,B 10     ; *** DEVICE TRANSFER *** via MEM[B+10] (RUNTIME)
                                  ;   VERIFIED byte 135410. Target = datafield
                                  ;   word ,X 14 = the resident SCSI/mass-storage
                                  ;   transfer primitive (-> SCSDISK -> SCLLD).
                                  ;   THIS is the instruction that enqueues the
                                  ;   page-0 read. If it never executes, SCWAQ
                                  ;   stays empty and no read is issued.
036136  124041   JMP 41          ; driver ERROR skip-return -> 036177
036137  044412   LDA ,B 12       ; (normal return continues here)
036140  070054   AND 54
036141  172463   AAA 63
036142  135410   JPL I ,B 10     ; *** DEVICE TRANSFER *** #2 (RUNTIME) alt fn
036143  124016   JMP 16          ; skip-return -> 036161
036144  040404   MIN ,B 4        ; <== SUCCESS marker (ok return convergence)
036145  055050   LDX I 50
036146  046017   LDA ,X 17
036147  146151   RADD CLD SA DD
036150  054416   LDX ,B 16
036151  052010   LDT ,X 10
036152  056011   LDX ,X 11
036153  135043   JPL I 43        ; CALL [036216]=011236
036154  014402   STX ,B 2
036155  044416   LDA ,B 16
036156  004400   STA ,B 0
036157  170761   SAA -17
036160  125037   JMP I 37        ; return -> [036217]=003776 resident epilogue
; --- dispatch #3 ------------------------------------------------------------
036161  044412   LDA ,B 12
036162  070032   AND 32
036163  172466   AAA 66
036164  054411   LDX ,B 11
036165  146171   RADD CLD SX DD
036166  054416   LDX ,B 16
036167  135410   JPL I ,B 10     ; *** DEVICE TRANSFER *** #3 (RUNTIME)
036170  124007   JMP 7           ; ERROR skip-return -> 036177
036171  044412   LDA ,B 12
036172  070022   AND 22
036173  172463   AAA 63
036174  135410   JPL I ,B 10     ; *** DEVICE TRANSFER *** #4 (RUNTIME)
036175  124002   JMP 2           ; ERROR skip-return -> 036177
036176  124346   JMP -32         ; normal -> 036144 (success)
; === error/return epilogue ==================================================
036177  004402   STA ,B 2        ; error path: store status
036200  054416   LDX ,B 16
036201  026005   LDD ,X 5
036202  052004   LDT ,X 4
036203  135015   JPL I 15        ; CALL [036220]=035240 = CL1DB (release buffer)
                                  ;   VERIFIED: [036220]=035240. On the error
                                  ;   return RCBLO releases the cache buffer via
                                  ;   CL1DB and propagates the error to RXDIR.
036204  124353   JMP -25         ; -> 036157 (return)
036205  004402   STA ,B 2        ; GSIZE/helper fail-return stub
036206  124351   JMP -27         ; -> 036157 (return with error)
; --- remaining words 036207-036220 are the literal pointer pool -------------
036207  120031   ; literal
036210  010506   ; ptr (reserve/wait)
036211  007777   ; literal
036212  000001   ; literal
036213  050124   ; ptr
036214  007000   ; literal
036215  004007   ; literal
036216  011236   ; ptr
036217  003776   ; ptr -> 003776B resident epilogue/return
036220  035240   ; ptr -> 035240B = CL1DB                          (VERIFIED)

; ============================================================================
;  GSIZE = 037101B  - get configured disk size (in-core, NO device I/O)
;  Called by RCBLO prologue (035773) and by CHDSI (040023: JPL I 121 ->
;  [040144]=037101). Reserves via 003752, calls three in-core parameter
;  getters (050124 / 050223 / 050220), then computes size via RMPY (multiply).
;  Proof it does no disk transfer: no JPL I ,B (no datafield dispatch); helpers
;  are table getters with a bounds->174B check (see scsi-mount-geometry.md).
; ============================================================================
037101  021042   STD I 42        ; save link                        (VERIFIED entry)
037102  146145   RADD CLD SL DA
037103  146131   RADD CLD SB DD
037104  170010   SAB 10          ; frame
037105  135037   JPL I 37        ; CALL [037144]=003752 resident reserve/prologue
037106  054037   LDX 37
037107  135037   JPL I 37        ; CALL [037146]=050124  param getter
037110  124031   JMP 31          ; fail -> 037141
037111  004407   STA ,B 7
037112  070035   AND 35
037113  146156   RADD CLD SA DT
037114  054034   LDX 34
037115  135034   JPL I 34        ; CALL [037151]=050223  param getter
037116  124023   JMP 23          ; fail -> 037141
037117  004406   STA ,B 6
037120  054032   LDX 32
037121  135032   JPL I 32        ; CALL [037153]=050220  param getter
037122  124017   JMP 17          ; fail -> 037141
037123  050406   LDT ,B 6
037124  175306   BSKP ONE 100 DT ; test a geometry bit
037125  124010   JMP 10          ; -> 037135
037126  131002   JAZ 2           ; -> 037130
037127  135025   JPL I 25        ; CALL [037154]=000215 (low resident helper)
037130  044407   LDA ,B 7
037131  070024   AND 24
037132  156570   SHA ZIN SHR 10  ; shift
037133  146156   RADD CLD SA DT
037134  141216   RMPY SD DT      ; D := size = (params) * (shift)  <- COMPUTE SIZE
037135  020402   STD ,B 2        ; return size (32-bit) in local 2
037136  040404   MIN ,B 4        ; ok marker
037137  170770   SAA -10
037140  125016   JMP I 16        ; return -> [037156]=003776 resident epilogue
037141  004402   STA ,B 2        ; error path
037142  124375   JMP -3          ; -> 037137 (return with error)
037143  147417   ROP NOOP
037144  003752   ; ptr -> 003752B resident reserve/prologue         (VERIFIED)
037145  000002   ; literal
037146  050124   ; ptr -> 050124B                                   (VERIFIED)
037147  000377   ; literal
037150  000013   ; literal
037151  050223   ; ptr -> 050223B
037152  000010   ; literal
037153  050220   ; ptr -> 050220B
037154  000215   ; ptr -> 000215B (low resident)
037155  007400   ; literal
037156  003776   ; ptr -> 003776B resident epilogue

; ============================================================================
;  DISPATCH TARGET (the OPEN boundary) - datafield word ,X 14
;  For a SCSI disk unit, ,X 14 points at SCSDISK ("LEVEL 11 ROUTINE TO PERFORM
;  TRANSFERS ON SCSI DISKS", IP-P2-SCSI-DISK.NPL). SCSDISK enqueues work via
;  SCLLD (INITO -> SCWAQ). The pointer VALUE is a runtime binding set at device
;  configuration time and is therefore NOT recoverable from static 006-S3FS
;  bytes. SCSDISK/SCLLD live in a resident driver segment (not carved here).
;  Function codes (SCSDISK header): 0=READ 1=WRITE 4=SEEK 42=READ FORMAT
;  (read control record at the last block). The four JPL I ,B 10 dispatches
;  above build their function code from op-code ,B 12 via AND/AAA masks.
; ============================================================================
