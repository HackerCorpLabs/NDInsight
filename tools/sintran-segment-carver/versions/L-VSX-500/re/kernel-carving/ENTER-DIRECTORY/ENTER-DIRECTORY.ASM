; ============================================================================
;  ENTER-DIRECTORY (directory mount) - full carved call chain
;  SINTRAN III VSX/500  L07     segment 006-S3FS    load base 26000B
; ============================================================================
;  Command:  @ENTER-DIRECTORY <dir>,<device>,<unit>[,<F|R>][,<subunit>]
;  Worker :  ENDIR = 140176B  (FILSYS-SYMBOLS, L07)
;
;  This listing is the DEVICE-AGNOSTIC filesystem side of the mount, carved
;  byte-for-byte from 006-S3FS.bin, down to the per-device transfer hand-off
;  (the JPL I ,B 10 through datafield word ,X 14). The per-device driver
;  (SCSDISK/SCLLD for SCSI) is a runtime-bound pointer into a foreign resident
;  segment and is carved separately by the SCSI team.
;
;  EVERY word below was re-read from 006-S3FS.bin with dd/python and equals the
;  value shown. byte offset = (addr - 26000B) * 2 (decimal). Addresses/values
;  are OCTAL. Comments graded VERIFIED (from bytes) / INFERRED (reasoned).
;
;  Routines in this file (all resident IN 006-S3FS, proven by sibling entries):
;     GDIRA 030225B   get directory datafield address (leaf)
;     ENDIR 140176B   the enter-directory worker (top level)
;     CHDSI 037763B   check/enter directory (read+validate+stamp page 0)
;     RXDIR 037643B   read page-0 extended-info via the buffer cache
;     WXDIR 037702B   recompute checksum + write the ext-info block back
;  Carved in the sibling folder (referenced, not duplicated):
;     RCBLO 035766B   reserve/read cache block  -> the JPL I ,B 10 dispatch
;     GSIZE 037101B   get configured disk size (in-core, no I/O)
;  See ../RCBLO/RCBLO.ASM for RCBLO+GSIZE.
;
;  *** OVERLAY WARNING (trap #4) ***
;  The MON-124 worker PRSRV=037076B, RESRV=037103B, PRLS=037147B, RELES=037156B
;  (FILSYS-SYMBOLS) are RESIDENT-MONITOR virtual addresses. In the 006-S3FS
;  image those SAME addresses decode as GSIZE's body (037101-037156). They are a
;  DIFFERENT overlay reached through MCTAB, NOT the 006-S3FS bytes. Do not carve
;  PRSRV/RESRV from 006-S3FS - that would disassemble GSIZE. See README section
;  "MON calls".
; ----------------------------------------------------------------------------


; ============================================================================
;  GDIRA = 030225B  - get directory datafield base address (LEAF routine)
;  Called by ENDIR at 140244 (JPL I 141 -> [140405]=030225).
;  Input : T = directory index ; Output: X = datafield base address.
;  No STD I prologue - it is a leaf reached by JPL and returns via EXIT
;  (restoring from the L link register JPL loaded). Descriptor stride = 4 words.
; ----------------------------------------------------------------------------
030225  146157  RADD CLD SA DX  ; X := A                                (VERIFIED 146157)
030226  146165  RADD CLD ST DA  ; A := T   (A := directory index)
030227  120004  MPY 4           ; A := index * 4   (4-word descriptor stride) (VERIFIED 120004)
030230  060004  ADD 4           ; A += base  (P-relative table base word)
030231  144075  SWAP SX DA      ; X <-> A  => X = datafield base address
030232  146142  EXIT            ; return (via L link)                    (leaf return)


; ============================================================================
;  ENDIR = 140176B  - the ENTER-DIRECTORY worker (top level)
;  PLANC frame idiom: SAB 27 (23-word frame), STD I 30 saves link.
;  Locals used: ,B 20 = subunit/part arg ; ,B 24 = flag scratch ;
;               ,B 25 = directory datafield ptr (from GDIRA) ;
;               ,B 26 = entering SYSTEM number (owner arg to CHDSI) ;
;               ,B 6  = scratch datafield ptr.
; ----------------------------------------------------------------------------
140176  021030  STD I 30        ; save return link                       (VERIFIED entry 021030)
140177  146145  RADD CLD SL DA  ; A := L    (PLANC prologue arg shuffle)
140200  146131  RADD CLD SB DD  ; D := B
140201  170027  SAB 27          ; build 23-word stack frame
140202  135025  JPL I 25        ; CALL [140227]=003752  resident reserve/prologue
140203  135025  JPL I 25        ; CALL [140230]=053174  (arg-fetch helper)   (VERIFIED 135025)
140204  125025  JMP I 25        ; err -> [140231]=140770
140205  044025  LDA 25
140206  054025  LDX 25
140207  146037  RADD SB DX
140210  171020  SAT 20
140211  135023  JPL I 23        ; CALL [140234]=044777  (parse helper)      (VERIFIED 135023)
140212  125023  JMP I 23        ; err -> [140235]
140213  135023  JPL I 23        ; CALL [140236]
140214  125021  JMP I 21        ; err -> [140235]
140215  146151  RADD CLD SA DD
140216  045021  LDA I 21
140217  135021  JPL I 21        ; CALL [140240]
140220  146115  RADD CLD SD DA
140221  010420  STT ,B 20       ; local 20 := subunit / part selector
140222  135017  JPL I 17        ; CALL [140241]
140223  125017  JMP I 17        ; err -> [140242]
140224  124017  JMP 17          ; -> 140243
140225  125015  JMP I 15        ; err -> [140242]
140226  147417  ROP NOOP
; --- ENDIR literal pointer pool (140227-140243) -----------------------------
140227  003752  ; -> 003752B  resident reserve/prologue
140230  053174  ; -> 053174B
140231  140770  ; -> 140770B
140232  043221  ; literal
140233  000007  ; literal
140234  044777  ; -> 044777B
140235  141016  ; -> 141016B (err handler)
140236  132072  ; literal
140237  120027  ; literal
140240  010500  ; -> 010500B
140241  131732  ; literal
140242  141020  ; -> 141020B (err handler)
140243  010426  ; literal
; --- reserve unit + directory datafield -------------------------------------
140244  135141  JPL I 141       ; CALL [140405]=030225 = GDIRA  (get dir datafield) (VERIFIED 135141; ptr 030225)
140245  014425  STX ,B 25       ; local 25 := directory datafield pointer (X)
140246  046003  LDA ,X 3        ; A := datafield word 3
140247  135137  JPL I 137       ; CALL [140406]=010500  (helper)             (VERIFIED 135137)
140250  046004  LDA ,X 4        ; A := datafield word 4 (unit/reserve descriptor)
140251  131005  JAZ 5           ; if 0 -> skip reserve (-> 140256)
140252  153124  MON 124         ; *** ForceReserve (PRSRV) the mass-storage unit *** (VERIFIED 153124)
                                ;   MON 124B -> MCTAB[124]=037076 = PRSRV. Reserves the
                                ;   physical unit before ANY transfer. Fail -> 147B.
140253  130003  JAP 3           ; if A>=0 (reserved OK) -> 140256
140254  170547  SAA 147         ; else error 147B "device unit reserved for special use" (VERIFIED SAA)
140255  125132  JMP I 132       ; -> error exit [140407]
140256  046000  LDA ,X 0        ; A := datafield word 0 (device flags/type)
140257  175355  BSKP ONE 150 DA ; test device-type bit (150 octal)
140260  124003  JMP 3           ; bit clear -> 140263
140261  170545  SAA 145         ; else error 145B "illegal on tape device"   (VERIFIED SAA)
140262  125126  JMP I 126       ; -> error exit [140410]
140263  135126  JPL I 126       ; CALL [140411]
140264  125124  JMP I 124       ; err -> [140410]
140265  004424  STA ,B 24       ; local 24 := flag scratch
140266  175105  BSKP ZRO 100 DA ; test bit
140267  124003  JMP 3           ; -> 140272
140270  135122  JPL I 122       ; CALL [140412]
140271  125117  JMP I 117       ; err -> [140410]
140272  050426  LDT ,B 26       ; T := entering system number
140273  135120  JPL I 120       ; CALL [140413]
140274  125114  JMP I 114       ; err -> [140410]
140275  050420  LDT ,B 20       ; T := subunit / part
140276  054116  LDX 116
140277  135116  JPL I 116       ; CALL [140415]=050223 ISETP  (set part flag)
140300  125116  JMP I 116       ; err -> [140416]=050231 ICLEP
140301  175235  BSKP ONE 30 DA
140302  124004  JMP 4           ; -> 140306
140303  004424  STA ,B 24
140304  135113  JPL I 113       ; CALL [140417]
140305  125111  JMP I 111       ; err -> [140416]
140306  044424  LDA ,B 24
140307  175325  BSKP ONE 120 DA
140310  124010  JMP 10          ; -> 140320
140311  146105  RADD CLD 0 DA   ; A := 0
140312  146101  RADD CLD 0 DD   ; D := 0
140313  050426  LDT ,B 26
140314  135104  JPL I 104       ; CALL [140420]
140315  170442  SAA 42          ; error 42B "main directory not last one released" (VERIFIED SAA)
140316  135103  JPL I 103       ; CALL [140421]
140317  125103  JMP I 103       ; err -> [140422]
140320  050426  LDT ,B 26
140321  135102  JPL I 102       ; CALL [140423]
140322  146105  RADD CLD 0 DA
140323  146101  RADD CLD 0 DD
140324  135100  JPL I 100       ; CALL [140424]
140325  125063  JMP I 63        ; err -> [140410]
; --- name / abbreviation match against on-unit directory name ---------------
140326  014406  STX ,B 6
140327  060076  ADD 76
140330  172760  AAA -20
140331  146156  RADD CLD SA DT
140332  146151  RADD CLD SA DD
140333  146107  RADD CLD 0 DX
140334  142200  LBYT            ; byte-wise name compare setup
140335  171101  SAT 101
140336  141065  SKP IF DA GRE ST
140337  124004  JMP 4           ; -> 140343
140340  171132  SAT 132
140341  143056  SKP IF DT LST SA
140342  124012  JMP 12          ; -> 140354
140343  171060  SAT 60
140344  141065  SKP IF DA GRE ST
140345  124004  JMP 4           ; -> 140351
140346  171071  SAT 71
140347  143056  SKP IF DT LST SA
140350  124004  JMP 4           ; -> 140354
140351  171055  SAT 55
140352  140065  SKP IF DA EQL ST
140353  125053  JMP I 53        ; -> [140426]
140354  054053  LDX 53
140355  146037  RADD SB DX
140356  146115  RADD CLD SD DA
140357  171020  SAT 20
140360  135050  JPL I 50        ; CALL [140430]
140361  125045  JMP I 45        ; err -> [140426]
140362  146157  RADD CLD SA DX
140363  135046  JPL I 46        ; CALL [140431]
140364  124006  JMP 6           ; -> 140372
140365  131005  JAZ 5           ; -> 140372
140366  054406  LDX ,B 6
140367  135043  JPL I 43        ; CALL [140432]
140370  170432  SAA 32          ; error 32B "directory entered" (already-entered guard) (VERIFIED SAA)
140371  125017  JMP I 17        ; -> error exit [140410]
140372  044425  LDA ,B 25       ; A := directory datafield ptr
140373  060040  ADD 40
140374  146117  RADD CLD SD DX
140375  171020  SAT 20
140376  135036  JPL I 36        ; CALL [140434]=001224  (resident block copy)
140377  054406  LDX ,B 6
140400  135032  JPL I 32        ; CALL [140432]
; --- hand entering-system number to CHDSI, do the mount ---------------------
140401  050426  LDT ,B 26       ; T := entering system number (owner arg)
140402  135033  JPL I 33        ; CALL [140435]=037763 = CHDSI  *** THE MOUNT *** (VERIFIED 135033; ptr 037763)
140403  125005  JMP I 5         ; CHDSI error return -> [140410] error exit
140404  124032  JMP 32          ; CHDSI ok return    -> 140436  (post-mount bookkeeping)
; --- ENDIR literal pointer pool (140405-140435) -----------------------------
140405  030225  ; -> 030225B = GDIRA                                   (VERIFIED)
140406  010500  ; -> 010500B
140407  141024  ; -> 141024B  error exit
140410  141000  ; -> 141000B  error exit (common)
140411  050323  ; -> 050323B
140412  053047  ; literal
140413  037377  ; literal
140414  000013  ; literal
140415  050223  ; -> 050223B  ISETP
140416  050231  ; -> 050231B  ICLEP
140417  050231  ; -> (dup)
140420  034557  ; -> 034557B
140421  036511  ; -> 036511B
140422  141004  ; -> 141004B  error exit
140423  035231  ; -> 035231B
140424  035531  ; -> 035531B
140425  002000  ; literal
140426  140773  ; -> 140773B
140427  000007  ; literal
140430  041552  ; literal
140431  047402  ; literal
140432  035476  ; -> 035476B
140433  000005  ; literal
140434  001224  ; -> 001224B  resident block copy
140435  037763  ; -> 037763B = CHDSI                                   (VERIFIED)
; --- 140436+ : post-mount in-core directory-table bookkeeping (OK return) ---
;     partially carved; the on-disk "entered" state is set by CHDSI/WXDIR.
;     Routine RGLOB/WGLOB start at 140500+ (separate routines).


; ============================================================================
;  CHDSI = 037763B  - check / enter directory (read + validate + stamp page 0)
;  Called by ENDIR at 140402 (T = entering system number).
;  Reads page-0 ext-info via RXDIR, verifies additive checksum, self-heals a
;  bad/zero checksum, enforces the cross-system owner interlock, then stamps
;  owner+flag and writes back via WXDIR.
; ----------------------------------------------------------------------------
037763  021154  STD I 154       ; save link                              (VERIFIED entry 021154)
037764  146145  RADD CLD SL DA
037765  146131  RADD CLD SB DD
037766  170020  SAB 20          ; frame
037767  135151  JPL I 151       ; CALL [040140]  (setup helper)
037770  135151  JPL I 151       ; CALL [040141]
037771  124144  JMP 144         ; err -> 040135
037772  004416  STA ,B 16
037773  175105  BSKP ZRO 100 DA
037774  124135  JMP 135         ; err -> 040131
037775  050401  LDT ,B 1
037776  054144  LDX 144
037777  146037  RADD SB DX      ; X = datafield-derived buffer descriptor
040000  135143  JPL I 143       ; CALL [040143]=037643 = RXDIR  *** READ PAGE 0 *** (VERIFIED 135143; ptr 037643)
040001  124134  JMP 134         ; RXDIR error -> 040135
; --- additive checksum of ext-info words 1..7 -------------------------------
040002  170401  SAA 1
040003  146151  RADD CLD SA DD  ; counter := 1
040004  173401  AAX 1
040005  146105  RADD CLD 0 DA   ; A := 0 (accumulator)
040006  171010  SAT 10          ; T := 8 (loop bound)
040007  143061  SKP IF DD LST ST; while counter < 8
040010  124005  JMP 5           ; -> 040015 (done)
040011  062000  ADD ,X 0        ; A += word   (plain 16-bit ADD, not XOR)
040012  173401  AAX 1
040013  146401  RADD AD1 0 DD   ; counter++
040014  124373  JMP -5          ; -> 040007
040015  173770  AAX -10         ; X -= 8 (back to block base)
040016  052000  LDT ,X 0        ; T := stored checksum (word 0)
040017  140065  SKP IF DA EQL ST; computed == stored ?
040020  124043  JMP 43          ; mismatch -> rebuild 040063
040021  131042  JAZ 42          ; sum == 0 -> also rebuild 040063
; --- good checksum: compare capacity vs in-core geometry (GSIZE) ------------
040022  ...     (LDA/GSIZE size compare - see below)
040023  135121  JPL I 121       ; CALL [040144]=037101 = GSIZE (in-core size, NO I/O) (VERIFIED 135121; ptr 037101)
;   ... capacity compare, owner interlock (040110), stamp (040121), WXDIR call
;   (040127 -> [040157]=037702). Full body 040022-040161 is documented with
;   verified bytes in enter-directory.md sections 5.1-5.5 (identical listing).
; --- CHDSI selected pointer pool (VERIFIED) ---------------------------------
040143  037643  ; -> 037643B = RXDIR                                   (VERIFIED)
040144  037101  ; -> 037101B = GSIZE                                   (VERIFIED)
040157  037702  ; -> 037702B = WXDIR                                   (VERIFIED)


; ============================================================================
;  RXDIR = 037643B  - read page-0 extended-info via the buffer cache
;  Sets block number = 0 and calls RCBLO to fetch page 0 through the cache;
;  the cache miss drives the device transfer (RCBLO's JPL I ,B 10). Then copies
;  the 8-word ext-info sub-block out of the page buffer and releases it.
; ----------------------------------------------------------------------------
037643  021030  STD I 30        ; save link                              (VERIFIED entry 021030)
037644  146145  RADD CLD SL DA
037645  146131  RADD CLD SB DD
037646  170007  SAB 7           ; frame
037647  135025  JPL I 25        ; CALL [037674]=003752  resident reserve/prologue
037650  146105  RADD CLD 0 DA   ; A := 0
037651  146101  RADD CLD 0 DD   ; D := 0   *** block number = 0 (page 0) ***  (VERIFIED 146101)
037652  135023  JPL I 23        ; CALL [037675]=035766 = RCBLO  (reserve/read cache block 0) (VERIFIED 135023; ptr 035766)
037653  124016  JMP 16          ; RCBLO error -> 037671
037654  014406  STX ,B 6        ; local 6 := X (returned cache-buffer ptr)
037655  060021  ADD 21          ; source offset into the page buffer (ext-info at word 21)
037656  054400  LDX ,B 0
037657  171010  SAT 10          ; T := 8  (copy 8 words = ext-info block)
037660  144057  SWAP SA DX
037661  135016  JPL I 16        ; CALL [037677]=001224  resident block copy (8 words)
037662  050401  LDT ,B 1
037663  146105  RADD CLD 0 DA
037664  146101  RADD CLD 0 DD
037665  135013  JPL I 13        ; CALL [037700]=035240 = CL1DB  (release cache buffer) (ptr 035240)
037666  040404  MIN ,B 4        ; ok-return marker
037667  170771  SAA -7
037670  125011  JMP I 11        ; return -> [037701]=003776 resident epilogue
037671  004402  STA ,B 2        ; error path: store status
037672  124375  JMP -3          ; join epilogue at 037667
037673  147417  ROP NOOP
; --- RXDIR literal pointer pool (VERIFIED) ----------------------------------
037674  003752  ; -> 003752B  resident reserve/prologue
037675  035766  ; -> 035766B = RCBLO                                   (VERIFIED)
037676  001750  ; literal (ext-info source offset base)
037677  001224  ; -> 001224B  resident block copy
037700  035240  ; -> 035240B = CL1DB                                   (VERIFIED)
037701  003776  ; -> 003776B  resident epilogue


; ============================================================================
;  WXDIR = 037702B  - recompute checksum + write ext-info block back to page 0
;  Called by CHDSI at 040127 (-> [040157]=037702) after stamping owner+flag.
;  Same additive checksum loop as CHDSI, then writes the 8-word block back
;  through the page cache -> datafield transfer path (WBLOC/WCBLO writers).
; ----------------------------------------------------------------------------
037702  021050  STD I 50        ; save link                              (VERIFIED entry 021050)
037703  146145  RADD CLD SL DA
037704  146131  RADD CLD SB DD
037705  170007  SAB 7
037706  135045  JPL I 45        ; CALL [037753]=003752 resident reserve/prologue
037707  170401  SAA 1
037710  146151  RADD CLD SA DD  ; counter := 1
037711  173401  AAX 1
037712  146105  RADD CLD 0 DA   ; A := 0
037713  171010  SAT 10          ; T := 8
037714  143061  SKP IF DD LST ST; while counter < 8
037715  124005  JMP 5           ; -> 037722
037716  062000  ADD ,X 0        ; A += word (additive checksum)
037717  173401  AAX 1
037720  146401  RADD AD1 0 DD   ; counter++
037721  124373  JMP -5          ; -> 037714
037722  054400  LDX ,B 0
037723  006000  STA ,X 0        ; store recomputed checksum into word 0
037724  050401  LDT ,B 1
037725  146105  RADD CLD 0 DA
037726  146101  RADD CLD 0 DD   ; block number = 0 (write page 0 back)
037727  135025  JPL I 25        ; CALL [037754]=035240 = CL1DB / [037755]=035766 RCBLO writer path
037730  135025  JPL I 25        ; CALL [037755]
037731  124017  JMP 17          ; err -> 037750
037732  014406  STX ,B 6
037733  060023  ADD 23
037734  054400  LDX ,B 0
037735  171010  SAT 10          ; 8 words
037736  135021  JPL I 21        ; CALL [037757]=001224 resident block copy (into buffer)
037737  050401  LDT ,B 1
037740  054406  LDX ,B 6
037741  135017  JPL I 17        ; CALL [037760]=036357 = WCBLO  (write cache block back)
037742  124004  JMP 4           ; -> 037746
037743  040404  MIN ,B 4        ; ok marker
037744  170771  SAA -7
037745  125014  JMP I 14        ; return -> [037761]=003776 resident epilogue
037746  135014  JPL I 14        ; CALL [037762]  (error helper)
037747  170435  SAA 35          ; error 35B "master block transfer error"    (VERIFIED SAA)
037750  004402  STA ,B 2
037751  124373  JMP -5          ; -> 037744
037752  147417  ROP NOOP
; --- WXDIR literal pointer pool (VERIFIED) ----------------------------------
037753  003752  ; -> 003752B  resident reserve/prologue
037754  035240  ; -> 035240B = CL1DB
037755  035766  ; -> 035766B = RCBLO
037756  001750  ; literal
037757  001224  ; -> 001224B  resident block copy
037760  036357  ; -> 036357B = WCBLO  (cache-block writer)
037761  003776  ; -> 003776B  resident epilogue
037762  047365  ; literal


; ============================================================================
;  DEVICE HAND-OFF (the seam) - carved in ../RCBLO/RCBLO.ASM
; ----------------------------------------------------------------------------
;  RCBLO 035766B, on a cache MISS, loads the per-device transfer primitive out
;  of the unit datafield and dispatches to it:
;
;     036100  054411  LDX ,B 11    ; X := unit/device descriptor        (VERIFIED)
;     036101  046014  LDA ,X 14    ; A := datafield word 14 = TRANSFER FN PTR (VERIFIED)
;     036102  004410  STA ,B 10    ; local 10 := that pointer
;     ...
;     036135  135410  JPL I ,B 10  ; *** DEVICE TRANSFER *** (RUNTIME target = ,X 14)
;     036142/036167/036174  135410 JPL I ,B 10  ; alternate function sites
;
;  The function code passed to the driver is built by masking op-code ,B 12:
;     036135: fn = (op & 065) + 066   (primary transfer / READ)
;  For a SCSI unit, ,X 14 = SCSDISK -> SCLLD (INITO -> SCWAQ). That pointer
;  VALUE is a device-configuration runtime binding, not in the static bytes.
;  See ../RCBLO/README.md for the full RCBLO + GSIZE carve and the "why the
;  SCSI page-0 read is never enqueued" analysis.
; ============================================================================
