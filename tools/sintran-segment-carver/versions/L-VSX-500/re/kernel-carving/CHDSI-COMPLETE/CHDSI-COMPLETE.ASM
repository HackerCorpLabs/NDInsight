; ============================================================================
;  CHDSI-COMPLETE.ASM
;  SINTRAN III  L07 / L-VSX-500  --  segment 006-S3FS  (load base 26000B)
;
;  The complete directory master-block (extended-info) processing:
;    WXDIR  37702B  - recompute additive checksum + write block 0 back
;    CHDSI  37763B  - read block 0, checksum, capacity, owner interlock,
;                     rebuild (self-heal), owner stamp, write-back
;    REENB  40162B  - release directory (clear entered bit + write back)
;
;  Every word below is byte-identical to 006-S3FS.bin (sha256
;  b4a563d0715a6304a35167ec5ff0090c92b0b99dd743aef159ac6b182e080585).
;  Reproduced from the whole-segment listing
;    ../../segments-ref/006-S3FS/006-S3FS.asm  lines 5192-5401
;  and spot-checked with dd against ../../../segments/006-S3FS.bin (see README).
;
;  ADDRESSING NOTE (ND-100): when the X/B/I mode bits of a memory-reference
;  word are all 0 the operand is P-relative: effective = P + sign_extend(disp).
;  All "JPL I n / LDA n / LDX n" targets below are P-relative and are shown
;  resolved to their pool word and that word's contents.
;
;  RETURN CONVENTION (PLANC internal routine, resident epilogue 003776B):
;    local ,B 4  = success marker. MIN ,B 4 (increment) on EVERY success path
;                  makes the epilogue take the SKIP (caller L+1) return.
;                  Error paths never touch ,B 4  -> caller L (error) return.
;    local ,B 2  = returned status word (error code / propagated callee A).
;    A on return = -(frame size)  (SAA -20 CHDSI, SAA -7 WXDIR, SAA -16 REENB);
;                  this is the epilogue's frame-deallocate argument, NOT a code.
; ============================================================================


; ------------------------------------------------------------------ WXDIR ---
;  WXDIR 37702B  - recompute checksum over words 1..7, store into word 0,
;  reserve cache buffer for block 0, copy the 8-word block in, WRITE it back
;  via WCBLO, release. Error 35B on any transfer failure.
;
037702  021050  STD I 50            ; save return link
037703  146145  RADD CLD SL DA
037704  146131  RADD CLD SB DD
037705  170007  SAB 7               ; 7-word stack frame
037706  135045  JPL I 45            ; -> [037753]=003752  resident prologue (OPEN)
037707  170401  SAA 1
037710  146151  RADD CLD SA DD      ; D := 1   (checksum index starts at word 1)
037711  173401  AAX 1               ; X -> word 1
037712  146105  RADD CLD 0 DA       ; A := 0   (accumulator)
037713  171010  SAT 10              ; T := 8   (loop limit)
;                                    -- checksum recompute loop (words 1..7) --
037714  143061  SKP IF DD LST ST    ; while D < 8
037715  124005  JMP 5               ; -> 037722  (done)
037716  062000  ADD ,X 0            ; A += MEM[X]         plain 16-bit ADD
037717  173401  AAX 1               ; X++
037720  146401  RADD AD1 0 DD       ; D++
037721  124373  JMP -5              ; -> 037714
037722  054400  LDX ,B 0            ; X -> block base
037723  006000  STA ,X 0            ; word0 := computed checksum   <== STORE SUM
037724  050401  LDT ,B 1
037725  146105  RADD CLD 0 DA       ; A := 0
037726  146101  RADD CLD 0 DD       ; D := 0   (block number = 0)
037727  135025  JPL I 25            ; -> [037754]=035240  CL1DB (release/flush)  (OPEN edge)
037730  135025  JPL I 25            ; -> [037755]=035766  RCBLO (reserve buf, blk 0)
037731  124017  JMP 17              ; -> 037750  RCBLO error
037732  014406  STX ,B 6            ; local 6 := cache-buffer ptr
037733  060023  ADD 23              ; A += 23  (dest offset inside page buffer)
037734  054400  LDX ,B 0
037735  171010  SAT 10              ; T := 8   (copy 8 words)
037736  135021  JPL I 21            ; -> [037757]=001224  resident block copy IN (OPEN)
037737  050401  LDT ,B 1
037740  054406  LDX ,B 6            ; X := cache-buffer ptr
037741  135017  JPL I 17            ; -> [037760]=036357  WCBLO : WRITE block 0  <== WRITE-BACK
037742  124004  JMP 4               ; -> 037746  WCBLO error
037743  040404  MIN ,B 4            ; success marker (skip-return)
037744  170771  SAA -7              ; frame-pop arg
037745  125014  JMP I 14            ; -> [037761]=003776  resident epilogue (OK return)
037746  135014  JPL I 14            ; -> [037762]=047365  resident error handler (OPEN)
037747  170435  SAA 35              ; ERROR CODE 35B "master block transfer error"
037750  004402  STA ,B 2            ; local 2 := 35B
037751  124375  JMP -5              ; -> 037744  (SAA -7 ; epilogue error return)
037752  147417  ROP NOOP            ; separator
;                                    -- WXDIR literal / pointer pool --
037753  003752  ; 003752   resident prologue
037754  035240  ; CL1DB
037755  035766  ; RCBLO
037756  001750  ; resident helper (OPEN)
037757  001224  ; resident block-copy IN
037760  036357  ; WCBLO
037761  003776  ; resident epilogue
037762  047365  ; resident error handler (OPEN)


; ------------------------------------------------------------------ CHDSI ---
;  CHDSI 37763B  - check / enter directory.  T (from ENDIR local 26) = entering
;  system number.  Reads block 0 via RXDIR, validates checksum + capacity,
;  self-heals a bad block, applies the cross-system owner interlock, stamps the
;  owner + entered bit, and writes back via WXDIR.
;
037763  021154  STD I 154           ; save return link
037764  146145  RADD CLD SL DA
037765  146131  RADD CLD SB DD
037766  170020  SAB 20              ; 16-word stack frame
037767  135151  JPL I 151           ; -> [040140]=003752  resident prologue (OPEN)
037770  135151  JPL I 151           ; -> [040141]=050323  resident param/datafield helper (OPEN)
037771  124144  JMP 144             ; -> 040135  helper error exit (propagate A)
037772  004416  STA ,B 16           ; local 16 := helper result word (option/flag bits)
037773  175105  BSKP ZRO 100 DA     ; test bit6 of local-16 word (resource-reserve bit)
037774  124135  JMP 135             ; -> 040131  bit6 SET -> EARLY SUCCESS (no page-0 I/O)
037775  050401  LDT ,B 1
037776  054144  LDX 144             ; -> [040142]=000006  (X := 6, datafield offset)
037777  146037  RADD SB DX          ; X := block/datafield base + 6
040000  135143  JPL I 143           ; -> [040143]=037643  RXDIR : read block 0  <== READ
040001  124134  JMP 134             ; -> 040135  RXDIR error (propagate A)
;                                    -- checksum recompute + compare (words 1..7) --
040002  170401  SAA 1
040003  146151  RADD CLD SA DD      ; D := 1
040004  173401  AAX 1               ; X -> word 1
040005  146105  RADD CLD 0 DA       ; A := 0   (accumulator)
040006  171010  SAT 10              ; T := 8
040007  143061  SKP IF DD LST ST    ; while D < 8
040010  124005  JMP 5               ; -> 040015 (done)
040011  062000  ADD ,X 0            ; A += MEM[X]  (16-bit additive sum)
040012  173401  AAX 1
040013  146401  RADD AD1 0 DD       ; D++
040014  124373  JMP -5              ; -> 040007
040015  173770  AAX -10             ; X -= 8  (back to block base)
040016  052000  LDT ,X 0            ; T := stored checksum (word 0)
040017  140065  SKP IF DA EQL ST    ; computed == stored ?
040020  124043  JMP 43              ; -> 040063  MISMATCH -> REBUILD
040021  131042  JAZ 42              ; -> 040063  sum == 0 -> REBUILD
;                                    -- GOOD checksum: capacity compare/adjust --
040022  050401  LDT ,B 1
040023  135121  JPL I 121           ; -> [040144]=037101  get geometry capacity (in-seg helper)
040024  124111  JMP 111             ; -> 040135  helper error (propagate A)
040025  146156  RADD CLD SA DT      ; T := A (hi geometry word)
040026  146114  RADD CLD SD DL      ; L := D (lo geometry word)
040027  026006  LDD ,X 6            ; DD := stored capacity (words 6-7, 32-bit)
040030  140065  SKP IF DA EQL ST    ; stored-hi == geometry-hi ?
040031  124003  JMP 3               ; -> 040034  hi differs -> adjust
040032  142041  SKP IF DD UEQ SL    ; stored-lo == geometry-lo ?
040033  124027  JMP 27              ; -> 040062  capacity MATCHES -> skip to interlock
;                                    -- capacity mismatch: resident adjust sequence --
040034  050401  LDT ,B 1
040035  054110  LDX 110             ; -> [040146]=000002  (X := 2)
040036  135110  JPL I 110           ; -> [040146]=050124  resident helper (OPEN)
040037  124076  JMP 76              ; -> 040135  error (propagate A)
040040  070107  AND 107             ; -> [040150]=000013 mask? (P-rel data 040150=000013)
040041  004417  STA ,B 17           ; local 17 := masked value
040042  146156  RADD CLD SA DT
040043  054105  LDX 105             ; -> [040150]=000013  (X := 11)
040044  135105  JPL I 105           ; -> [040151]=050223  resident helper (OPEN)
040045  124070  JMP 70              ; -> 040135  error (propagate A)
040046  175245  BSKP ONE 40 DA      ; test bit4 of A
040047  124065  JMP 65              ; -> 040134  -> generic error (loads 003204)
040050  135102  JPL I 102           ; -> [040152]=037565  in-seg helper
040051  124064  JMP 64              ; -> 040135  error (propagate A)
040052  124062  JMP 62              ; -> 040134  -> generic error
040053  054067  LDX 67              ; -> [040142]=000006  (X := 6)
040054  146037  RADD SB DX
040055  026006  LDD ,X 6            ; DD := capacity again
040056  050417  LDT ,B 17
040057  054074  LDX 74              ; -> [040153]=000010  (X := 8)
040060  135074  JPL I 74            ; -> [040154]=050226  resident helper (OPEN)
040061  124054  JMP 54              ; -> 040135  error (propagate A)
040062  124016  JMP 16              ; -> 040100  join owner interlock
;                                    -- BAD checksum: REBUILD (self-heal) --
040063  146107  RADD CLD 0 DX       ; X := 0
040064  171010  SAT 10              ; T := 8
040065  143067  SKP IF DX LST ST    ; while X < 8
040066  124004  JMP 4               ; -> 040072 (done)
040067  002406  STZ ,X ,B 6         ; zero block word [X]
040070  173401  AAX 1
040071  124374  JMP -4              ; -> 040065
040072  054050  LDX 50              ; -> [040142]=000006  (X := 6)
040073  146037  RADD SB DX
040074  050401  LDT ,B 1
040075  135047  JPL I 47            ; -> [040144]=037101  get geometry capacity
040076  124037  JMP 37              ; -> 040135  error (propagate A)
040077  022006  STD ,X 6            ; words 6-7 := geometry-derived capacity
;                                    -- OWNER INTERLOCK (join point 040100) --
040100  054042  LDX 42              ; -> [040142]=000006  (X := 6)
040101  146037  RADD SB DX          ; X -> block base
040102  044416  LDA ,B 16           ; A := local-16 option word
040103  175035  BSKP ZRO 30 DA      ; test bit3 of A (force/override bit)
040104  124015  JMP 15              ; -> 040121  bit3 SET -> bypass interlock, STAMP
040105  051050  LDT I 50            ; T := entering system number (indirect via local 50)
040106  142006  SKP IF DT UEQ 0     ; entering system != 0 ?
040107  124012  JMP 12              ; -> 040121  system 0 -> STAMP (no interlock)
040110  046004  LDA ,X 4            ; A := flag word
040111  175375  BSKP ONE 170 DA     ; bit15 set (already entered) ?
040112  124007  JMP 7               ; -> 040121  not entered -> STAMP
040113  046005  LDA ,X 5            ; A := stored owner system number
040114  131005  JAZ 5               ; -> 040121  owner 0 (unowned) -> STAMP
040115  142065  SKP IF DA UEQ ST    ; stored owner == entering system ?
040116  124003  JMP 3               ; -> 040121  same owner (re-enter) -> STAMP
040117  044037  LDA 37              ; -> [040156]=003203  REJECT sentinel (cross-system)
040120  124015  JMP 15              ; -> 040135  reject exit
;                                    -- STAMP owner + entered bit --
040121  045034  LDA I 34            ; A := entering system number
040122  006005  STA ,X 5            ; word5 := owner := entering system   <== STAMP OWNER
040123  046004  LDA ,X 4            ; A := flag word
040124  174375  BSET ONE 170 DA     ; set bit15 "entered"                 <== STAMP FLAG
040125  006004  STA ,X 4            ; word4 := flag word
040126  050401  LDT ,B 1
040127  135030  JPL I 30            ; -> [040157]=037702  WXDIR : checksum + write-back  <== WRITE
040130  124005  JMP 5               ; -> 040135  WXDIR error (propagate A = 35B)
;                                    -- early-success tail (from 037774 bit6 path) --
040131  040404  MIN ,B 4            ; success marker
040132  170760  SAA -20             ; frame-pop arg
040133  125025  JMP I 25            ; -> [040160]=003776  resident epilogue (OK return)
040134  044025  LDA 25              ; -> [040161]=003204  generic error status
040135  004402  STA ,B 2            ; local 2 := status (loaded value OR propagated callee A)
040136  124374  JMP -4              ; -> 040132  (SAA -20 ; epilogue error return)
040137  147417  ROP NOOP            ; separator
;                                    -- CHDSI literal / pointer pool --
040140  003752  ; 003752   resident prologue
040141  050323  ; resident param/datafield helper (OPEN)
040142  000006  ; const 6  (datafield offset, reused 8x)
040143  037643  ; RXDIR
040144  037101  ; geometry-capacity helper (in 006-S3FS)
040145  000002  ; const 2
040146  050124  ; resident helper (OPEN)
040147  000377  ; const 377
040150  000013  ; const 11
040151  050223  ; resident helper (OPEN)
040152  037565  ; in-seg helper
040153  000010  ; const 8
040154  050226  ; resident helper (OPEN)
040155  004051  ; (data)
040156  003203  ; REJECT sentinel loaded by 040117 (owner reject) - NOT a clean errno (OPEN)
040157  037702  ; WXDIR
040160  003776  ; resident epilogue
040161  003204  ; generic error status loaded by 040134 - NOT a clean errno (OPEN)


; ------------------------------------------------------------------ REENB ---
;  REENB 40162B  - release directory: re-read block 0, CLEAR entered bit15,
;  write back via WXDIR.  The mirror of CHDSI's stamp.  Error code -20 (-16).
;
040162  021030  STD I 30            ; save link
040163  146145  RADD CLD SL DA
040164  146131  RADD CLD SB DD
040165  170016  SAB 16              ; 14-word frame
040166  135025  JPL I 25            ; -> [040213]=003752  resident prologue
040167  135025  JPL I 25            ; -> [040214]=050323  param helper
040170  124020  JMP 20              ; -> 040210  error
040171  175105  BSKP ZRO 100 DA     ; resource-reserve bit6 test
040172  124013  JMP 13              ; -> 040205  bit6 SET -> early success
040173  050401  LDT ,B 1
040174  054021  LDX 21              ; -> [040215]=000006  (X := 6)
040175  146037  RADD SB DX
040176  135020  JPL I 20            ; -> [040216]=037643  RXDIR : read block 0
040177  124011  JMP 11              ; -> 040210  error
040200  046004  LDA ,X 4            ; A := flag word
040201  174175  BSET ZRO 170 DA     ; CLEAR bit15 "entered"                <== RELEASE
040202  006004  STA ,X 4
040203  135014  JPL I 14            ; -> [040217]=037702  WXDIR : write back
040204  124004  JMP 4               ; -> 040210  error
040205  040404  MIN ,B 4            ; success marker
040206  170762  SAA -16             ; frame-pop arg
040207  125011  JMP I 11            ; -> [040220]=003776  epilogue (OK)
040210  004402  STA ,B 2            ; local 2 := status
040211  124375  JMP -3              ; -> 040206
040212  147417  ROP NOOP
040213  003752  ; resident prologue
040214  050323  ; param helper
040215  000006  ; const 6
040216  037643  ; RXDIR
040217  037702  ; WXDIR
040220  003776  ; resident epilogue
; ============================================================================
