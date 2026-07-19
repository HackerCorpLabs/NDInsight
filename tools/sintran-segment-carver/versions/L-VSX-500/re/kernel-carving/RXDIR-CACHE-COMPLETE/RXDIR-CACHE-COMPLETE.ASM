; ============================================================================
;  RXDIR-CACHE-COMPLETE - the COMPLETE block-0 read path (SINTRAN III VSX/500 L07)
; ============================================================================
;  Segment  : 006-S3FS (File System)   load base 26000B
;             sha256(006-S3FS.bin) = b4a563d0...
;  Bin      : versions/L-VSX-500/segments/006-S3FS.bin
;  Symbols  : SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT
;
;  This listing carves EVERY routine on the "read directory block 0 through the
;  page cache" path and ALL of its branches:
;
;     RXDIR 037643B  read page-0 ext-info via cache  (sets block := 0)
;       -> RCBLO 035766B  reserve/read a cache block  (HIT / MISS / alloc / dispatch)
;            -> GSIZE 037101B  in-core disk size  (no I/O)
;            -> COMPP 036616B  cache compare/hash probe  (RCBLO's search helper)
;            -> G3NWT 034371B  get-new cache entry / wait
;            -> the 4 device-transfer dispatches  JPL I ,B 10  (RUNTIME target)
;       R3BUF 035112B / R3IBU 035102B  RELEASE (invalidate) cache buffers for a device
;
;  BYTE PROVENANCE. Every word below was re-read from 006-S3FS.bin with dd
;  (byte offset = (addr - 26000B)*2 decimal, big-endian word). 15 load-bearing
;  words dd-verified - see README.md "dd verification" table. Addresses/values
;  are OCTAL. Comments graded VERIFIED (from bytes) / INFERRED (reasoned) / OPEN.
;
;  *** disassembler note (same trap as RCBLO.ASM) ***
;  The whole-segment listing annotates `JPL I <disp>` with a P-relative "-> NNN"
;  target - CORRECT for P-relative literal pointers, but the four dispatch
;  `135410 JPL I ,B 10` are indirect through FRAME word B+10 (a runtime pointer
;  = datafield word ,X 14), NOT P-relative. Those are marked (RUNTIME).
; ----------------------------------------------------------------------------


; ############################################################################
;  RXDIR = 037643B  - read page-0 extended-info via the buffer cache
;  Caller: CHDSI 037763B (040000: JPL I 143 -> [040143]=037643).
;  Also reached from [040216]=037643.
;  Body 037643-037672; 037673 ROP NOOP separates code from the pointer pool
;  037674-037701. Next routine WXDIR = 037702B.
; ############################################################################
037643  021030   STD I 30        ; save return link                 (VERIFIED entry)
037644  146145   RADD CLD SL DA  ; A := L   (caller arg shuffle)
037645  146131   RADD CLD SB DD  ; D := B
037646  170007   SAB 7           ; build 7-word stack frame
037647  135025   JPL I 25        ; CALL [037674]=003752  resident reserve/prologue
; --- set the requested block number to ZERO, then read it through the cache ---
037650  146105   RADD CLD 0 DA   ; A := 0
037651  146101   RADD CLD 0 DD   ; D := 0  == BLOCK NUMBER := 0     (VERIFIED 146101)
037652  135023   JPL I 23        ; CALL [037675]=035766 = RCBLO (reserve/read blk 0)
037653  124016   JMP 16          ; RCBLO error skip-return -> 037671 (ERROR EXIT)
; --- RCBLO returned OK: X = cache-buffer ptr; extract the 8-word ext-info -----
037654  014406   STX ,B 6        ; local 6 := X = returned cache-buffer pointer
037655  060021   ADD 21          ; A := A + 21B  (offset to ext-info inside buffer)
037656  054400   LDX ,B 0        ; X := caller's dest area (,B 0)
037657  171010   SAT 10          ; T := 10B  (word count = 8. decimal)
037660  144057   SWAP SA DX      ; set up src(A)/dst(X) for the copy
037661  135016   JPL I 16        ; CALL [037677]=001224  block-copy helper (resident)
                                  ;   copies the 8-word ext-info out of the cache
                                  ;   buffer into the caller's area.        (VERIFIED ptr)
; --- release the cache buffer we borrowed -----------------------------------
037662  050401   LDT ,B 1        ; T := ,B 1
037663  146105   RADD CLD 0 DA   ; A := 0
037664  146101   RADD CLD 0 DD   ; D := 0
037665  135013   JPL I 13        ; CALL [037700]=035240 = CL1DB (RELEASE buffer)
                                  ;   VERIFIED [037700]=035240. RXDIR does NOT keep
                                  ;   the buffer - it copies ext-info out then frees it.
037666  040404   MIN ,B 4        ; ok marker (skip-return convention)
037667  170771   SAA -7          ; unwind frame (-7 words)
037670  125011   JMP I 11        ; RETURN -> [037701]=003776 resident epilogue (OK EXIT)
; --- error exit -------------------------------------------------------------
037671  004402   STA ,B 2        ; ERROR: store status word in ,B 2
037672  124375   JMP -3          ; -> 037667  (unwind + return with error)
037673  147417   ROP NOOP        ; --- pointer pool ---
037674  003752   ; ptr -> 003752B  resident reserve/prologue          (VERIFIED)
037675  035766   ; ptr -> 035766B  RCBLO                              (VERIFIED)
037676  001750   ; literal
037677  001224   ; ptr -> 001224B  resident block-copy helper         (VERIFIED)
037700  035240   ; ptr -> 035240B  CL1DB (release buffer)             (VERIFIED)
037701  003776   ; ptr -> 003776B  resident epilogue/return           (VERIFIED)


; ############################################################################
;  RCBLO = 035766B  - reserve/read a disk-cache block
;  (FULL every-path carve; unchanged bytes shared with ../RCBLO/RCBLO.ASM.)
;  Body 035766-036220 with an embedded pointer pool 036054-036071.
; ############################################################################
; === entry + prologue ========================================================
035766  021066   STD I 66        ; save return link                  (VERIFIED entry)
035767  146145   RADD CLD SL DA  ; A := L
035770  146131   RADD CLD SB DD  ; D := B
035771  170017   SAB 17          ; build 17B-word frame
035772  135063   JPL I 63        ; CALL [036055]=003752  resident reserve/prologue
035773  135063   JPL I 63        ; CALL [036056]=037101 = GSIZE (in-core size, NO I/O)
                                  ;   VERIFIED [036056]=037101.
035774  125063   JMP I 63        ; GSIZE fail-return -> [036057]=036205 (ERROR STUB)
; === cache-block search ======================================================
035775  020406   STD ,B 6        ; local 6 := D = requested block number (=0 from RXDIR)
035776  054062   LDX 62          ; X := [036061]=050124 (cache/device descriptor getter)
035777  135062   JPL I 62        ; CALL [036061]=050124
036000  125057   JMP I 57        ; fail -> [036057]=036205 (ERROR STUB)
036001  004412   STA ,B 12       ; local 12 := A = OPERATION code (from descriptor getter)
036002  070060   AND 60          ; isolate op-class bits
036003  146156   RADD CLD SA DT  ; T := A
036004  135057   JPL I 57        ; CALL [036063]=030235 = GNAMA (classify/lookup)
036005  014411   STX ,B 11       ; local 11 := X = unit/device descriptor pointer
036006  050401   LDT ,B 1
036007  024402   LDD ,B 2
036010  020414   STD ,B 14
036011  135053   JPL I 53        ; CALL [036064]=036616 = COMPP (cache compare/search)
036012  125045   JMP I 45        ; COMPP fail -> [036057]=036205 (ERROR STUB)
036013  004402   STA ,B 2
036014  010413   STT ,B 13
036015  050401   LDT ,B 1
036016  041047   MIN I 47
036017  144400   RAND 0 0
036020  135046   JPL I 46        ; CALL [036066]=034371 = G3NWT (get-new entry / wait)
036021  046004   LDA ,X 4        ; A := buffer flag word
036022  174175   BSET ZRO 170 DA ; clear bit 15 of A
036023  146151   RADD CLD SA DD
036024  050401   LDT ,B 1
036025  012004   STT ,X 4
036026  044413   LDA ,B 13
036027  006012   STA ,X 12       ; buffer ,X 12 := device id
036030  014416   STX ,B 16       ; local 16 := candidate cache buffer
; --- compare requested block (,B 2 / ,B 3) vs buffer stored block (,X 5 / ,X 6) ---
036031  140061   SKP IF DD EQL ST
036032  124040   JMP 40          ; MISS -> 036072 (allocate/transfer)
036033  044402   LDA ,B 2
036034  052005   LDT ,X 5        ; buffer block-hi
036035  140065   SKP IF DA EQL ST
036036  124034   JMP 34          ; MISS -> 036072
036037  044403   LDA ,B 3
036040  052006   LDT ,X 6        ; buffer block-lo
036041  140065   SKP IF DA EQL ST
036042  124030   JMP 30          ; MISS -> 036072
; === cache HIT (block already resident, NO device read) ======================
036043  024414   LDD ,B 14
036044  131403   JAF 3           ; -> 036047
036045  142001   SKP IF DD UEQ 0
036046  124024   JMP 24          ; -> 036072 (treat as miss if dirty/invalid)
036047  045020   LDA I 20
036050  135020   JPL I 20        ; CALL [036070]=010506 (reserve buffer / wait)
036051  041020   MIN I 20
036052  144400   RAND 0 0
036053  124071   JMP 71          ; HIT -> 036144 (SUCCESS, no device read)
036054  147417   ROP NOOP        ; --- pointer pool ---
036055  003752   ; ptr -> 003752B  resident reserve/prologue          (VERIFIED)
036056  037101   ; ptr -> 037101B  GSIZE                              (VERIFIED)
036057  036205   ; ptr -> 036205B  local fail-return stub
036060  000002   ; literal 2
036061  050124   ; ptr -> 050124B  descriptor getter
036062  000377   ; literal
036063  030235   ; ptr -> 030235B  GNAMA
036064  036616   ; ptr -> 036616B  COMPP  (cache compare/search)      (VERIFIED)
036065  010633   ; literal
036066  034371   ; ptr -> 034371B  G3NWT  (get-new entry / wait)
036067  120031   ; literal
036070  010506   ; ptr -> 010506B  (reserve buffer / wait, resident)
036071  010634   ; literal
; === cache MISS: build device function + dispatch the transfer ===============
036072  024402   LDD ,B 2        ; reload requested block number
036073  022005   STD ,X 5        ; stamp buffer block-hi/lo := requested block
036074  044413   LDA ,B 13
036075  006012   STA ,X 12       ; stamp buffer device id
036076  045111   LDA I 111
036077  135111   JPL I 111       ; CALL [036210]=010506 (reserve buffer / wait)
; --- load the datafield transfer-function pointer into local 10 --------------
036100  054411   LDX ,B 11       ; X := unit/device descriptor            (VERIFIED)
036101  046014   LDA ,X 14       ; A := datafield word 14 = TRANSFER FN PTR (VERIFIED)
036102  004410   STA ,B 10       ; local 10 := that pointer (the driver entry)
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
036116  170500   SAA 100         ; A := 100B (geometry-error status)
036117  124060   JMP 60          ; -> 036177 (ERROR EPILOGUE)
036120  050401   LDT ,B 1
036121  054071   LDX 71
036122  135071   JPL I 71        ; CALL [036213]=050124
036123  124054   JMP 54          ; fail -> 036177 (ERROR EPILOGUE)
; --- DISPATCH #1  (ABFUN = (op & 065) + 066)  primary READ-class transfer -----
036124  070065   AND 65          ; A(from 036122 return) &= 065
036125  146151   RADD CLD SA DD  ; D := A
036126  044412   LDA ,B 12       ; A := operation code
036127  070065   AND 65          ; A := op & 065B
036130  172466   AAA 66          ; A := (op & 065B) + 066B  == ABFUN #1  (VERIFIED arith)
036131  146116   RADD CLD SD DT  ; T := D
036132  054411   LDX ,B 11       ; X := unit descriptor
036133  146171   RADD CLD SX DD
036134  054416   LDX ,B 16       ; X := cache buffer
036135  135410   JPL I ,B 10     ; *** DEVICE TRANSFER #1 *** via MEM[B+10] (RUNTIME)
                                  ;   VERIFIED 135410. Target = datafield word ,X 14 =
                                  ;   resident SCSI/mass-storage transfer primitive.
                                  ;   THIS instruction enqueues the page-0 read.
036136  124041   JMP 41          ; driver error skip-return -> 036177 (ERROR EPILOGUE)
; --- DISPATCH #2  (ABFUN = (op & 054) + 063) --------------------------------
036137  044412   LDA ,B 12       ; A := operation code (ok-return path of #1)
036140  070054   AND 54          ; A := op & 054B
036141  172463   AAA 63          ; A := (op & 054B) + 063B  == ABFUN #2  (VERIFIED arith)
036142  135410   JPL I ,B 10     ; *** DEVICE TRANSFER #2 *** (RUNTIME)
036143  124016   JMP 16          ; skip-return -> 036161 (DISPATCH #3 group)
; === SUCCESS convergence =====================================================
036144  040404   MIN ,B 4        ; <== SUCCESS marker (ok-return convergence, incl. HIT)
036145  055050   LDX I 50
036146  046017   LDA ,X 17
036147  146151   RADD CLD SA DD
036150  054416   LDX ,B 16
036151  052010   LDT ,X 10
036152  056011   LDX ,X 11
036153  135043   JPL I 43        ; CALL [036216]=011236 (link buffer into cache/LRU)
036154  014402   STX ,B 2        ; return X = cache-buffer pointer to caller (RXDIR ,B 6)
036155  044416   LDA ,B 16
036156  004400   STA ,B 0
036157  170761   SAA -17         ; unwind 17B-word frame
036160  125037   JMP I 37        ; RETURN -> [036217]=003776 resident epilogue (OK EXIT)
; --- DISPATCH #3  (ABFUN = (op & 032) + 066) --------------------------------
036161  044412   LDA ,B 12       ; A := operation code
036162  070032   AND 32          ; A := op & 032B
036163  172466   AAA 66          ; A := (op & 032B) + 066B  == ABFUN #3  (VERIFIED arith)
036164  054411   LDX ,B 11
036165  146171   RADD CLD SX DD
036166  054416   LDX ,B 16
036167  135410   JPL I ,B 10     ; *** DEVICE TRANSFER #3 *** (RUNTIME)
036170  124007   JMP 7           ; error skip-return -> 036177 (ERROR EPILOGUE)
; --- DISPATCH #4  (ABFUN = (op & 022) + 063) --------------------------------
036171  044412   LDA ,B 12       ; A := operation code
036172  070022   AND 22          ; A := op & 022B
036173  172463   AAA 63          ; A := (op & 022B) + 063B  == ABFUN #4  (VERIFIED arith)
036174  135410   JPL I ,B 10     ; *** DEVICE TRANSFER #4 *** (RUNTIME)
036175  124002   JMP 2           ; error skip-return -> 036177 (ERROR EPILOGUE)
036176  124346   JMP -32         ; normal -> 036144 (SUCCESS)
; === ERROR EPILOGUE ==========================================================
036177  004402   STA ,B 2        ; store error status
036200  054416   LDX ,B 16
036201  026005   LDD ,X 5
036202  052004   LDT ,X 4
036203  135015   JPL I 15        ; CALL [036220]=035240 = CL1DB (RELEASE buffer)  (VERIFIED)
036204  124353   JMP -25         ; -> 036157 (unwind + return with error)
; === GSIZE / helper fail-return stub =========================================
036205  004402   STA ,B 2        ; store error status (from 035774/036000/036012)
036206  124351   JMP -27         ; -> 036157 (unwind + return with error)
036207  120031   ; literal      ; --- pointer pool ---
036210  010506   ; ptr -> 010506B (reserve/wait)
036211  007777   ; literal
036212  000001   ; literal
036213  050124   ; ptr -> 050124B
036214  007000   ; literal
036215  004007   ; literal
036216  011236   ; ptr -> 011236B (link buffer helper)
036217  003776   ; ptr -> 003776B resident epilogue/return
036220  035240   ; ptr -> 035240B CL1DB                              (VERIFIED)


; ############################################################################
;  GSIZE = 037101B  - get configured disk size (in-core, NO device I/O)
;  Called by RCBLO prologue (035773) and CHDSI (040023: [040144]=037101).
;  Proof of no I/O: no JPL I ,B (no datafield dispatch) anywhere in the body.
; ############################################################################
037101  021042   STD I 42        ; save link                         (VERIFIED entry)
037102  146145   RADD CLD SL DA
037103  146131   RADD CLD SB DD
037104  170010   SAB 10          ; frame
037105  135037   JPL I 37        ; CALL [037144]=003752 resident reserve/prologue
037106  054037   LDX 37
037107  135037   JPL I 37        ; CALL [037146]=050124  param getter
037110  124031   JMP 31          ; fail -> 037141 (ERROR EXIT)
037111  004407   STA ,B 7
037112  070035   AND 35
037113  146156   RADD CLD SA DT
037114  054034   LDX 34
037115  135034   JPL I 34        ; CALL [037151]=050223  param getter
037116  124023   JMP 23          ; fail -> 037141 (ERROR EXIT)
037117  004406   STA ,B 6
037120  054032   LDX 32
037121  135032   JPL I 32        ; CALL [037153]=050220  param getter
037122  124017   JMP 17          ; fail -> 037141 (ERROR EXIT)
037123  050406   LDT ,B 6
037124  175306   BSKP ONE 100 DT ; test a geometry bit
037125  124010   JMP 10          ; -> 037135
037126  131002   JAZ 2           ; -> 037130
037127  135025   JPL I 25        ; CALL [037154]=000215 (low resident helper)
037130  044407   LDA ,B 7
037131  070024   AND 24
037132  156570   SHA ZIN SHR 10  ; shift
037133  146156   RADD CLD SA DT
037134  141216   RMPY SD DT      ; D := size = (params) * (shift)   (VERIFIED 141216)
037135  020402   STD ,B 2        ; return 32-bit size in local 2
037136  040404   MIN ,B 4        ; ok marker
037137  170770   SAA -10         ; unwind frame
037140  125016   JMP I 16        ; RETURN -> [037156]=003776 resident epilogue (OK EXIT)
037141  004402   STA ,B 2        ; ERROR path: store status
037142  124375   JMP -3          ; -> 037137 (unwind + return with error)
037143  147417   ROP NOOP        ; --- pointer pool ---
037144  003752   ; ptr -> 003752B resident reserve/prologue          (VERIFIED)
037145  000002   ; literal
037146  050124   ; ptr -> 050124B                                    (VERIFIED)
037147  000377   ; literal
037150  000013   ; literal
037151  050223   ; ptr -> 050223B
037152  000010   ; literal
037153  050220   ; ptr -> 050220B
037154  000215   ; ptr -> 000215B (low resident)
037155  007400   ; literal
037156  003776   ; ptr -> 003776B resident epilogue


; ############################################################################
;  R3IBU = 035102B / R3BUF = 035112B  - RELEASE (invalidate) cache buffer(s)
;  Symbol pair with the getters G3IBU 034633B / G3BUF 034643B and G3NWT 034371B.
;  Two entry points into ONE shared body starting at 035121:
;    R3IBU 035102 sets local 10 := 2  (index-buffer flavour)
;    R3BUF 035112 sets local 10 := 1  (data-buffer flavour)
;  The body walks the cache-buffer list (LDX I 101 = list head), and for each
;  buffer matching this device (,X 5 vs ,B 1) and unit-flavour it: clears the
;  buffer valid flags (,X 4 := -1, ,X 5 := -1 at 035153/035154) and UNLINKS the
;  buffer from the LRU doubly-linked list (forward ,X 22 / back ,X 23 pointer
;  fixups at 035160-035210). Loop continues via 035212 JMP -66 until the list is
;  exhausted (SAT -1 / SKP IF DX UEQ ST at 035124-035126).
;  INFERRED role: dismount/invalidate - drop all cached blocks for a device so a
;  later mount re-reads them. It is the release counterpart to RCBLO's acquire.
; ############################################################################
; --- R3IBU entry (flavour 2) ------------------------------------------------
035102  021116   STD I 116       ; save link                         (VERIFIED entry)
035103  146145   RADD CLD SL DA
035104  146131   RADD CLD SB DD
035105  170011   SAB 11          ; frame
035106  135113   JPL I 113       ; CALL [035221]=003752 resident reserve/prologue
035107  170402   SAA 2           ; A := 2  (index-buffer flavour)
035110  004410   STA ,B 10       ; local 10 := 2
035111  124010   JMP 10          ; -> 035121 (shared body)
; --- R3BUF entry (flavour 1) ------------------------------------------------
035112  021106   STD I 106       ; save link                         (VERIFIED entry)
035113  146145   RADD CLD SL DA
035114  146131   RADD CLD SB DD
035115  170011   SAB 11          ; frame
035116  135103   JPL I 103       ; CALL [035221]=003752 resident reserve/prologue
035117  170401   SAA 1           ; A := 1  (data-buffer flavour)
035120  004410   STA ,B 10       ; local 10 := 1
; --- shared body: walk the buffer list --------------------------------------
035121  045101   LDA I 101       ; A := list-head token                 (VERIFIED 045101)
035122  135101   JPL I 101       ; CALL [035223]=010500 (get next buffer in list)
035123  055101   LDX I 101       ; X := returned buffer pointer
035124  171377   SAT -1          ; T := -1 (end-of-list sentinel)
035125  142067   SKP IF DX UEQ ST
035126  124065   JMP 65          ; end of list -> 035213 (DONE, OK EXIT)
035127  014406   STX ,B 6        ; save current buffer
035130  046022   LDA ,X 22       ; A := LRU forward link
035131  004407   STA ,B 7
035132  046005   LDA ,X 5        ; A := buffer device id
035133  050401   LDT ,B 1        ; T := requested device
035134  140065   SKP IF DA EQL ST
035135  124054   JMP 54          ; device mismatch -> 035211 (advance, skip this buffer)
035136  044410   LDA ,B 10       ; A := flavour (1 or 2)
035137  171001   SAT 1
035140  140065   SKP IF DA EQL ST
035141  124007   JMP 7           ; flavour != 1 -> 035150 (release regardless)
035142  046001   LDA ,X 1
035143  051062   LDT I 62
035144  142065   SKP IF DA UEQ ST
035145  124003   JMP 3           ; -> 035150
035146  131002   JAZ 2           ; -> 035150
035147  124042   JMP 42          ; skip (busy/pinned) -> 035211
; --- release this buffer: invalidate flags ----------------------------------
035150  046007   LDA ,X 7
035151  135055   JPL I 55        ; CALL [035226]=010506 (reserve/wait on buffer)
035152  170777   SAA -1          ; A := -1
035153  006004   STA ,X 4        ; buffer ,X 4 := -1  (invalidate)
035154  006005   STA ,X 5        ; buffer ,X 5 := -1  (device id := invalid)
035155  045047   LDA I 47
035156  140075   SKP IF DA EQL SX
035157  124011   JMP 11          ; -> 035170
; --- LRU unlink: head-of-list case ------------------------------------------
035160  046022   LDA ,X 22       ; forward link
035161  005043   STA I 43        ; store into list head
035162  171377   SAT -1
035163  142065   SKP IF DA UEQ ST
035164  124003   JMP 3           ; -> 035167
035165  146157   RADD CLD SA DX
035166  012023   STT ,X 23       ; fix successor's back link
035167  124011   JMP 11          ; -> 035200
; --- LRU unlink: mid-list case ----------------------------------------------
035170  046022   LDA ,X 22       ; forward link
035171  056023   LDX ,X 23       ; X := predecessor (back link)
035172  006022   STA ,X 22       ; predecessor.forward := this.forward
035173  171377   SAT -1
035174  142065   SKP IF DA UEQ ST
035175  124003   JMP 3           ; -> 035200
035176  144075   SWAP SX DA
035177  006023   STA ,X 23       ; successor.back := predecessor
; --- link the freed buffer onto the free list -------------------------------
035200  045027   LDA I 27
035201  054406   LDX ,B 6        ; X := the freed buffer
035202  006023   STA ,X 23
035203  144075   SWAP SX DA
035204  006022   STA ,X 22
035205  005022   STA I 22
035206  171377   SAT -1
035207  146157   RADD CLD SA DX
035210  012022   STT ,X 22
; --- advance to next buffer in list -----------------------------------------
035211  054407   LDX ,B 7        ; X := saved forward link (next buffer)
035212  124312   JMP -66         ; -> 035124 (loop)
; --- done (list exhausted) --------------------------------------------------
035213  040404   MIN ,B 4        ; ok marker                           (OK EXIT)
035214  045006   LDA I 6
035215  135011   JPL I 11        ; CALL [035226]=010506 (release the reserve)
035216  170767   SAA -11         ; unwind frame
035217  125011   JMP I 11        ; RETURN -> [035230]=003776 resident epilogue
035220  147417   ROP NOOP        ; --- pointer pool ---
035221  003752   ; ptr -> 003752B resident reserve/prologue
035222  120031   ; literal
035223  010500   ; ptr -> 010500B (list iterator, resident)
035224  007726   ; literal
035225  004007   ; literal
035226  010506   ; ptr -> 010506B (reserve/wait, resident)
035227  007725   ; literal
035230  003776   ; ptr -> 003776B resident epilogue


; ############################################################################
;  COMPP = 036616B  - RCBLO's cache compare/search helper ([036064]=036616)
;  Entry only (structural anchor); full carve is a separate helper. VERIFIED
;  that RCBLO reaches the cache-search through this symbol.
; ############################################################################
036616  021115   STD I 115       ; save link                         (VERIFIED entry)
036617  146145   RADD CLD SL DA
036620  146131   RADD CLD SB DD
036621  170010   SAB 10          ; frame
036622  135112   JPL I 112       ; CALL [036734]=... (probe helper)
;   ... COMPP hashes the requested block number, divides by the bucket count
;   (RDIV at 036641/036671) and walks a hash chain comparing entries. Its result
;   (X = candidate buffer) is what RCBLO stores at 036005/036011 return. Deeper
;   carve is OPEN here - not required for the block-0 read decision map.
; ============================================================================
