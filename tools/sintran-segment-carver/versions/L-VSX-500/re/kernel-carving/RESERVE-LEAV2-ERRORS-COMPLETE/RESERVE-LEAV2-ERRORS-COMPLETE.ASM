; ============================================================================
;  RESERVE / LEAV2 / ENTER-DIRECTORY error paths  - byte-verified carve
;  SINTRAN III VSX/500  L07   (running system L-VSX-500, symbols L07)
; ============================================================================
;  Closes the last two gaps in the ENTER-DIRECTORY carves:
;    PART 1  - the reserve/release family + PRSRV's uncarved jump target LEAV2.
;    PART 2  - the ENTER-DIRECTORY error exits + how a code reaches the console.
;
;  Byte offset in a .bin = (addr_oct - loadbase) * 2  (decimal).
;  Every >>> VERIFIED word was re-read with dd from the .bin named on the line.
;  Grades: VERIFIED (from bytes) / INFERRED (reasoned) / OPEN (runtime/foreign).
;  All addresses and values are OCTAL.
; ============================================================================


; ############################################################################
; # PART 1 - THE RESERVE / RELEASE FAMILY + LEAV2                             #
; ############################################################################
;
;  Dispatch (from PRSRV-124B, re-confirmed by dd on 044-S3IDPIT.bin):
;    MON N -> ENT14 -> GOTAB[N]=MFELL -> CALLP -> MCTAB[N] -> worker
;    MCTAB @ 005620B  (044-S3IDPIT, load base 4000B):
;      MCTAB[122B] byteoff 7302? no - MCTAB byteoff:  122B->037103 RESRV
;      MCTAB[123B] -> 037156 RELES
;      MCTAB[124B] -> 037076 PRSRV
;      MCTAB[125B] -> 037147 PRLS
;      MCTAB[070B] -> 050673 COMSB   (@-command interpreter; used by EXECC)
;
;  Reserve family bodies are RESIDENT MONITOR code, byte-identical in
;    071-S3SM / 070-S3SSM / 003-S3CP  (all load base 30000B).
;  dd re-reads (071-S3SM.bin):
;    [037076] byteoff 7292 = 48 d2 = 044322  PRSRV first word (LDA -56)   >>> VERIFIED
;    [037020] byteoff 7200 = 00 03 = 000003  the constant PRSRV loads (=3) >>> VERIFIED
;    [037107] byteoff 7310 = 2f 0f = 027417  PRSRV JMP I pointer (=LEAV2)  >>> VERIFIED
;    [037103] byteoff 7302 = 3d fe = 036776  RESRV first word            >>> VERIFIED
;    [037110] byteoff 7312 = 4a 2b = 045053  EXECC first word            >>> VERIFIED


; ----------------------------------------------------------------------------
;  PRSRV = 037076B  (MON 124B ForceReserve)  - 2-word trampoline (recap)
; ----------------------------------------------------------------------------
;  Fully carved in ../PRSRV-124B/. Reproduced here only as the entry to LEAV2.
037076  044322   LDA -56       ; A := MEM[037020] = 000003  (constant 3)        >>> VERIFIED
037077  125010   JMP I 10      ; PC := MEM[037107] = 027417B = LEAV2            >>> VERIFIED
;  *** PRSRV enters LEAV2 with A=3 and does nothing else. ***


; ----------------------------------------------------------------------------
;  LEAV2 = 027417B  - PRSRV's jump target, NOW CARVED
; ----------------------------------------------------------------------------
;  OVERLAY: 027417B is BELOW the System Monitor's 30000B load base, so it is
;  NOT in 071-S3SM. Using the skill's sibling-coherence discriminator on the
;  monitor-leave family
;     3ENTE=027346  L3EAV=027371  LEAVX=027404  LEAV2=027417  3LEAV=027430
;  the ONLY overlays where ALL FIVE land on parallel entries are
;     013-S3SCP  and  041-S3IMED   (byte-identical, load base 26000B).
;  In 006-S3FS / 012-S3SFS the same addresses land mid-instruction-stream
;  (027417 there = 004007 "STA 7", not an entry) -> NOT coherent, rejected.
;  In 022-S3RFAC they land mid-loop -> rejected.
;
;  dd re-read (013-S3SCP.bin, byteoff = (027417-026000)*2 = 1566):
;    [027417] byteoff 1566 = 31 95 49 b2 f5 f5 09 b2
;             = 030625 044662 172765 004662                              >>> VERIFIED
;    041-S3IMED.bin byteoff 1566 = 31 95 49 b2  (identical)              >>> VERIFIED
;
;  LEAV2 is one member of a parallel "leave / pop context frame" family.
;  Each entry (L3EAV/LEAVX/LEAV2/3LEAV) has the SAME shape: save F, descend the
;  frame-stack pointer at ,B -116 by one 11-word frame (AAA -13 = -11 dec),
;  bounds-check (SUB <limit>; JAN underflow -> 027452 refill), bump a frame
;  counter, then join the common restore tail at 027437. The entries differ
;  only in the SUB <limit> immediate (L3EAV 63, LEAVX 50, LEAV2 36, 3LEAV 25),
;  i.e. they select WHICH frame level to leave to.
;
;  --- LEAV2 body (013-S3SCP) ---
027417  030625   STF ,B -153   ; save caller F into frame temp (A=3 still live) >>> VERIFIED
027420  044662   LDA ,B -116   ; A := frame-stack pointer  (A=3 CLOBBERED HERE)  >>> VERIFIED
027421  172765   AAA -13       ; A -= 11  (descend one 11-word frame)            >>> VERIFIED
027422  004662   STA ,B -116   ; store back the decremented frame pointer        >>> VERIFIED
027423  146157   RADD CLD SA DX; X := A   (new frame base index)
027424  064036   SUB 36        ; A -= MEM[..36]  (underflow bounds compare)
027425  130425   JAN 25        ; if A<0 -> 027452  (stack underflow / refill)
027426  042001   MIN ,X 1      ; increment counter word[X+1] in the frame
027427  124010   JMP 10        ; -> 027437  common restore tail
;  --- common restore tail (shared by the whole leave family) ---
027437  036002   LDF ,X 2      ; F := frame word[X+2]  (restore saved fields)
027440  030600   STF ,B -200
027441  036005   LDF ,X 5      ; frame word[X+5]
027442  030603   STF ,B -175
027443  036010   LDF ,X 10     ; frame word[X+8]
027444  030606   STF ,B -172
027445  026000   LDD ,X 0      ; D := frame word[X+0]  (saved return context)
027446  146157   RADD CLD SA DX
027447  146114   RADD CLD SD DL; L := D   ** return link := popped frame's word 0 **
027450  034625   LDF ,B -153   ; restore F saved at entry
027451  146142   EXIT          ; return to the popped context (via new L)
;  --- underflow / refill handler ---
027452  044010   LDA 10        ; A := 10  (base)
027453  004662   STA ,B -116   ;   reset frame-stack pointer to base
027454  044007   LDA 7
027455  171003   SAT 3
027456  135006   JPL I 6       ; -> [027464] refill helper
027457  135743   JPL I ,B -35
027460  125005   JMP I 5       ; -> [027465]
;
;  *** A=3 (loaded by PRSRV at 037076) is OVERWRITTEN at 027420 before any use
;      and is NOT the value in A at the 027451 EXIT. So under THIS overlay
;      LEAV2 does not consume A=3: PRSRV performs a monitor leave/return and
;      no explicit reservation. See README PART 1 for the honest overlay caveat
;      (whether 013-S3SCP is the live overlay at monitor time is a runtime fact
;      and is OPEN). ***


; ----------------------------------------------------------------------------
;  RESRV = 037103B (MON 122B), PRLS = 037147B (MON 125B),
;  RELES = 037156B (MON 123B) and the shared executor EXECC = 037110B
;  [071-S3SM, load base 30000B]  - the REAL reserve/release primitives
; ----------------------------------------------------------------------------
;  Unlike PRSRV, these three do NOT trampoline away; they funnel into EXECC,
;  which BUILDS a command line and runs it through the @-command interpreter
;  (MON 70B -> COMSB). i.e. reserve/release are implemented by synthesising and
;  executing a "RESERVE .." / "RELEASE .." command internally.
;
037103  036776   LDF ,X ,B -2  ; RESRV entry - marshalling into EXECC            >>> VERIFIED
037104  033456   STF I ,B ,X 56
037105  037007   LDF I ,X 7
037106  035351   LDF I -27
037107  027417   (word 027417) ; dual-use: also PRSRV's JMP I pointer above
;
037110  045053   LDA I 53      ; EXECC: A := gate/flag word                       >>> VERIFIED
037111  131402   JAF 2         ; if A=0 skip -> 037113
037112  146142   EXIT          ;   early EXIT when gate clear
037113  146146   RADD CLD SL DT; T := L (save link)
037114  170402   SAA 2
037115  004637   STA ,B -141
037116  044046   LDA 46        ; command-string source
037117  146151   RADD CLD SA DD
037120  135045   JPL I 45      ; -> [037165]=004177B string builder (low res, OPEN)
037121  146164   RADD CLD ST DL; L := T
037122  044044   LDA 44
037123  146151   RADD CLD SA DD
037124  153070   MON 70        ; *** COMMND: run the built command line ***       >>> VERIFIED
;                                MON 70B -> MCTAB[070]=050673=COMSB (@-cmd interp)
037125  146107   RADD CLD 0 DX ; byte-scan of the command line ...
;   (scan loop 037125-037146; second MON 70 in the PRLS arm below)
;
037147  142200   LBYT          ; PRLS entry - scan next field                     >>> VERIFIED
037150  171047   SAT 47
037151  142065   SKP IF DA UEQ ST
037152  124004   JMP 4         ; -> 037156
037153  146115   RADD CLD SD DA
037154  153070   MON 70        ; *** COMMND: run RELEASE command line -> COMSB *** >>> VERIFIED
037155  124350   JMP -30       ; -> 037125 (back into the scan loop)
;
037156  146147   RADD CLD SL DX; RELES entry: X := L                              >>> VERIFIED
037157  135010   JPL I 10      ; -> [037167]=004116B helper (low res, OPEN)
;   037165=004177 (JPL I 45 target), 037167=004116 (JPL I 10 target) = data ptrs


; ############################################################################
; # PART 2 - THE ENTER-DIRECTORY ERROR PATHS + CONSOLE EMISSION              #
; ############################################################################
;  [006-S3FS, load base 26000B]. ENDIR=140176B. Full mount body in
;  ../ENTER-DIRECTORY/. Here: the error EXITS and the error-return convention.
;
;  The FILSYS error-return convention (skip-return), VERIFIED from the bytes:
;    * SUCCESS: the worker executes  MIN ,B 4  (set the OK marker) and returns
;      through the resident epilogue SPOP=003776, which takes the L+1 (skip)
;      return -> caller's instruction AFTER the "JMP I <err>" error return.
;    * ERROR: the worker does  SAA <code> ; JMP I <ptr>  to a per-site error
;      exit. The exit does  STA ,B 2  (park the code in frame slot ,B 2), runs
;      site cleanup (release LOCK via UNLOC=010506, and MON 125 ForceRelease if
;      the unit was reserved), then joins the common tail (140723/140726) which
;      returns through SPOP WITHOUT the ,B 4 marker -> the L+0 (non-skip) error
;      return, so the CALLER falls into its "JMP I <err>" and propagates upward.
;    SPUSH=003752 / SPOP=003776 = the resident frame push / pop (skip) epilogue.
;    LOCK/XLOCK=010500, UNLOC/XUNLO=010506 = directory lock / unlock primitives.

; --- the five ENDIR error raise sites (SAA immediates, all dd-verified) ------
140254  170547   SAA 147       ; 147B device unit reserved for special use        >>> VERIFIED (f167)
140255  125132   JMP I 132     ;   -> [140407]=141024 error exit
140261  170545   SAA 145       ; 145B illegal on tape device                       >>> VERIFIED (f165)
140262  125126   JMP I 126     ;   -> [140410]=141000 error exit (common)
140315  170442   SAA 42        ; 42B  main directory not last one released         >>> VERIFIED (f122)
140316  135103   JPL I 103     ;   (cleanup) ; 140317 JMP I 103 -> [140422]=141004
140370  170432   SAA 32        ; 32B  directory entered (already-entered guard)     >>> VERIFIED (f11a)
140371  125017   JMP I 17      ;   -> [140410]=141000 error exit (common)
037747  170435   SAA 35        ; 35B  master block transfer error (WXDIR write)     >>> VERIFIED (../ENTER-DIRECTORY)

; --- the per-site error exits (funnel) ---------------------------------------
141000  004402   STA ,B 2      ; park error code in frame slot ,B 2               >>> VERIFIED (0902)
141001  045035   LDA I 35      ; A := lock descriptor (via [141036])
141002  135035   JPL I 35      ;   -> [141037]=010506 = UNLOC  (release dir lock)
141003  124002   JMP 2         ;   -> 141005
141004  004402   STA ,B 2      ; (alternate entry) park error code
141005  050426   LDT ,B 26     ; T := entering-system number
141006  054032   LDX 32        ; X := MEM[141040]=000004
141007  135032   JPL I 32      ;   -> [141041] cleanup helper
141010  124313   JMP -65       ;   -> 140723  common tail
141011  054425   LDX ,B 25     ; X := directory datafield ptr
141012  046004   LDA ,X 4      ; A := df word 4 (unit reserve descriptor)
141013  131002   JAZ 2         ; if 0 (was not reserved) -> 141015
141014  153125   MON 125       ; *** ForceRelease (PRLS): un-reserve the unit ***  >>> VERIFIED (d655)
141015  124306   JMP -72       ;   -> 140723  common tail
141016  004402   STA ,B 2      ; error exit variant: park code
141017  124307   JMP -71       ;   -> 140726
141020  004402   STA ,B 2      ; error exit variant: park code
141021  045015   LDA I 15      ; A := lock descriptor
141022  135015   JPL I 15      ;   -> [141037]=010506 UNLOC
141023  124303   JMP -75       ;   -> 140726
141024  004402   STA ,B 2      ; error exit variant (147B site): park code
141025  045011   LDA I 11      ; A := lock descriptor
141026  135011   JPL I 11      ;   -> [141037]=010506 UNLOC
141027  124274   JMP -104      ;   -> 140723

; --- the common error/return tail --------------------------------------------
140723  054425   LDX ,B 25     ; X := directory datafield ptr
140724  046003   LDA ,X 3
140725  135014   JPL I 14      ;   -> [140741]=010506 = UNLOC  (release dir lock)
140726  170751   SAA -27       ; A := error/return marker (negative)
140727  125015   JMP I 15      ;   -> [140744]=003776 = SPOP (return via epilogue)
;   [140741]=010506 UNLOC   [140744]=003776 SPOP                                 >>> VERIFIED (symbols)
;
;  From SPOP the code in A/,B 2 is delivered up the FILSYS call chain to the
;  @-command executor. The executor's error return calls the resident error-
;  message printer ERMSG=016714 (ERMON=114574), which maps the numeric code to
;  its text and writes it to the console as "<name> (<code>B)". ERMSG's body is
;  in a lower resident overlay (016714 < 26000B) and is OPEN here; its ROLE is
;  named/known. The message TEXTs live in the SINTRAN error-message table
;  (segments 005-S3ERRS / 011-S3ERRL / 014-S3ERRP; not carved here).
;
;  RED HERRING: "APPROACHING END OF ACCOUNTING FILE" = error 243B, set ONLY in
;  RP-P2-ACCRT (the RT-accounting collector run at LOGOUT). It is NOT on the
;  ENTER-DIRECTORY path - no call edge from the mount to RP-P2-ACCRT. Documented
;  in ../PRSRV-124B/ sec (e); repeated here as a known non-mount code.
; ============================================================================
