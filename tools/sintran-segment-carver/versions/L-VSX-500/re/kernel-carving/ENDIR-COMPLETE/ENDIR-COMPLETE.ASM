; ============================================================================
;  ENDIR-COMPLETE - the ENTIRE ENTER-DIRECTORY worker, every branch and exit
;  SINTRAN III VSX/500  L07     segment 006-S3FS    load base 26000B
; ============================================================================
;  Worker : ENDIR = 140176B  (FILSYS-SYMBOLS, L07)
;  Command: @ENTER-DIRECTORY <dir>,<device>,<unit>[,<F|R>][,<subunit>]
;
;  This listing is EXHAUSTIVE: it carves the ENDIR body (140176-140404), the
;  literal pointer pools, the name-mismatch tail, EVERY SAA error immediate, and
;  EVERY error-exit handler (140770-141041) they jump to. It supersedes the
;  happy-path-only ../ENTER-DIRECTORY/ENTER-DIRECTORY.ASM for the ENDIR level.
;
;  Companion: ../ENTER-DIRECTORY/ENTER-DIRECTORY.ASM carves the DOWNSTREAM chain
;  (GDIRA / CHDSI / RXDIR / WXDIR); ../RCBLO/RCBLO.ASM carves the device seam.
;  This file does NOT re-carve those - it resolves the ENDIR-internal control
;  flow the earlier files left as "post-mount bookkeeping / OPEN".
;
;  All addresses and values OCTAL. byte offset = (addr - 26000B) * 2 (decimal).
;  Every word below re-read from 006-S3FS.bin with dd; the load-bearing ones are
;  called out with (dd off=NNN word=VVV). Comments graded VERIFIED / INFERRED.
;
;  *** CORRECTION to SINTRAN/Filesystem/code-logic/enter-directory.md sec 7 ***
;  That doc cites "140140B SAA 40" for error 40B. Byte-verified: the SAA 40 that
;  ENDIR actually REACHES is 140777B (dd off=76798 word=170440). 140140B is a
;  separate SAA 40 in the PRECEDING routine's error tail (before ENDIR entry
;  140176B) and is NOT on any ENDIR path. See README "Correction".
; ----------------------------------------------------------------------------


; ============================================================================
;  ENDIR = 140176B  - prologue + argument fetch/parse
;  PLANC frame idiom: SAB 27 (23-word frame), STD I 30 saves the return link.
;  Locals: ,B 20 = subunit/part selector ; ,B 24 = flag scratch ;
;          ,B 25 = directory datafield ptr (from GDIRA) ;
;          ,B 26 = entering SYSTEM number (owner arg to CHDSI) ;
;          ,B 6  = scratch datafield ptr ; ,B 2 = returned error code.
; ----------------------------------------------------------------------------
140176  021030  STD I 30        ; save return link      (dd off=76028 word=021030) VERIFIED entry
140177  146145  RADD CLD SL DA  ; A := L   (PLANC arg shuffle)
140200  146131  RADD CLD SB DD  ; D := B
140201  170027  SAB 27          ; build 23-word stack frame
140202  135025  JPL I 25        ; CALL [140227]=003752  resident reserve/prologue
140203  135025  JPL I 25        ; CALL [140230]=053174  (arg-fetch helper)
140204  125025  JMP I 25        ; ERR-> [140231]=140770  (parse-error tail A)
140205  044025  LDA 25
140206  054025  LDX 25
140207  146037  RADD SB DX
140210  171020  SAT 20
140211  135023  JPL I 23        ; CALL [140234]=044777  (parse helper)
140212  125023  JMP I 23        ; ERR-> [140235]=141016  (parse-error exit B)
140213  135023  JPL I 23        ; CALL [140236]=132072  (parse helper)
140214  125021  JMP I 21        ; ERR-> [140235]=141016  (parse-error exit B)
140215  146151  RADD CLD SA DD
140216  045021  LDA I 21
140217  135021  JPL I 21        ; CALL [140240]=010500  (helper)
140220  146115  RADD CLD SD DA
140221  010420  STT ,B 20       ; local 20 := subunit / part selector
140222  135017  JPL I 17        ; CALL [140241]=131732 = GDIRE  (get-directory-entry helper)
140223  125017  JMP I 17        ; ERR-> [140242]=141020  (parse-error exit C)
140224  124017  JMP 17          ; ok -> 140243  (into next block)
140225  125015  JMP I 15        ; ERR-> [140242]=141020  (parse-error exit C)
140226  147417  ROP NOOP
; --- ENDIR literal pointer pool #1 (140227-140243) --------------------------
140227  003752  ; -> 003752B  resident reserve/prologue
140230  053174  ; -> 053174B  arg-fetch helper
140231  140770  ; -> 140770B  parse-error tail A
140232  043221  ; literal
140233  000007  ; literal (7)
140234  044777  ; -> 044777B  parse helper
140235  141016  ; -> 141016B  ERROR EXIT B
140236  132072  ; -> 132072B  parse helper
140237  120027  ; literal
140240  010500  ; -> 010500B  helper
140241  131732  ; -> 131732B = GDIRE
140242  141020  ; -> 141020B  ERROR EXIT C
140243  010426  ; literal

; ============================================================================
;  STAGE 2 - reserve the unit + get directory datafield + device-type guards
; ----------------------------------------------------------------------------
140244  135141  JPL I 141       ; CALL [140405]=030225 = GDIRA (dir datafield) (dd off=76298 word=030225)
140245  014425  STX ,B 25       ; local 25 := directory datafield pointer
140246  046003  LDA ,X 3        ; A := datafield word 3
140247  135137  JPL I 137       ; CALL [140406]=010500  (helper)
140250  046004  LDA ,X 4        ; A := datafield word 4 (unit / reserve descriptor)
140251  131005  JAZ 5           ; if 0 -> skip reserve (-> 140256)
;   ***  ERROR LADDER RUNG 1 : 147B  ***
140252  153124  MON 124         ; ForceReserve (PRSRV) the unit (dd off=76116 word=153124) VERIFIED
                                ;   MON 124B -> MCTAB[124]=037076 PRSRV. Fail => A<0.
140253  130003  JAP 3           ; if A>=0 (reserved OK) -> 140256
140254  170547  SAA 147         ; ERR 147B "device unit reserved for special use" (dd off=76120 word=170547)
140255  125132  JMP I 132       ; -> [140407]=141024  ERROR EXIT D (releases nothing new)
;   ***  ERROR LADDER RUNG 2 : 145B  ***
140256  046000  LDA ,X 0        ; A := datafield word 0 (device flags / type)
140257  175355  BSKP ONE 150 DA ; skip next if device-type bit (150 octal) SET
140260  124003  JMP 3           ; bit CLEAR (not tape) -> 140263
140261  170545  SAA 145         ; ERR 145B "illegal on tape device" (dd off=76130 word=170545)
140262  125126  JMP I 126       ; -> [140410]=141000  COMMON ERROR EXIT (release path)
; --- more setup: build flag scratch, set part flag ISETP/ICLEP --------------
140263  135126  JPL I 126       ; CALL [140411]=050323
140264  125124  JMP I 124       ; ERR-> [140410]=141000  COMMON ERROR EXIT
140265  004424  STA ,B 24       ; local 24 := flag scratch
140266  175105  BSKP ZRO 100 DA ; skip if bit (100 octal) ZERO
140267  124003  JMP 3           ; -> 140272
140270  135122  JPL I 122       ; CALL [140412]=053047
140271  125117  JMP I 117       ; ERR-> [140410]=141000  COMMON ERROR EXIT
140272  050426  LDT ,B 26       ; T := entering system number
140273  135120  JPL I 120       ; CALL [140413]=037377
140274  125114  JMP I 114       ; ERR-> [140410]=141000  COMMON ERROR EXIT
140275  050420  LDT ,B 20       ; T := subunit / part
140276  054116  LDX 116
140277  135116  JPL I 116       ; CALL [140415]=050223 = ISETP  (set part flag)
140300  125116  JMP I 116       ; ERR-> [140416]=141020  ERROR EXIT C
140301  175235  BSKP ONE 30 DA
140302  124004  JMP 4           ; -> 140306
140303  004424  STA ,B 24
140304  135113  JPL I 113       ; CALL [140417]=050231 = ICLEP  (clear part flag)
140305  125111  JMP I 111       ; ERR-> [140416]=141020  ERROR EXIT C
; ============================================================================
;  STAGE 3 - main-directory ordering guard  ***  ERROR LADDER RUNG 3 : 42B  ***
; ----------------------------------------------------------------------------
140306  044424  LDA ,B 24       ; A := flag scratch
140307  175325  BSKP ONE 120 DA ; skip if bit (120 octal) SET
140310  124010  JMP 10          ; bit SET -> 140320  (skip the 42B block)
140311  146105  RADD CLD 0 DA   ; A := 0
140312  146101  RADD CLD 0 DD   ; D := 0
140313  050426  LDT ,B 26       ; T := entering system number
140314  135104  JPL I 104       ; CALL [140420]=034557
140315  170442  SAA 42          ; ERR 42B "main directory not last one released" (dd off=76186 word=170442)
140316  135103  JPL I 103       ; CALL [140421]=036511  (error-report/log with A=42 - INFERRED preserves A)
140317  125103  JMP I 103       ; -> [140422]=141004  ERROR EXIT E (release path)
; --- normal continuation --------------------------------------------------
140320  050426  LDT ,B 26       ; T := entering system number
140321  135102  JPL I 102       ; CALL [140423]=035231
140322  146105  RADD CLD 0 DA
140323  146101  RADD CLD 0 DD
140324  135100  JPL I 100       ; CALL [140424]=035531
140325  125063  JMP I 63        ; ERR-> [140410]=141000  COMMON ERROR EXIT
; ============================================================================
;  STAGE 4 - name / abbreviation match against on-unit directory name
;            (byte-wise compare of the requested name vs the label on the unit)
;    ***  ERROR LADDER RUNG 4 : 40B (name mismatch) via 140773  ***
;    ***  ERROR LADDER RUNG 5 : 32B (already entered) at 140370  ***
; ----------------------------------------------------------------------------
140326  014406  STX ,B 6        ; local 6 := scratch datafield ptr
140327  060076  ADD 76
140330  172760  AAA -20
140331  146156  RADD CLD SA DT
140332  146151  RADD CLD SA DD
140333  146107  RADD CLD 0 DX
140334  142200  LBYT            ; byte-wise name-compare setup
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
140352  140065  SKP IF DA EQL ST ; requested name == label name ? (dd off=76244 word=140065)
140353  125053  JMP I 53        ; NOT equal -> [140426]=140773  NAME-MISMATCH tail -> SAA 40
140354  054053  LDX 53
140355  146037  RADD SB DX
140356  146115  RADD CLD SD DA
140357  171020  SAT 20
140360  135050  JPL I 50        ; CALL [140430]=041552
140361  125045  JMP I 45        ; ERR-> [140426]=140773  NAME-MISMATCH tail -> SAA 40
140362  146157  RADD CLD SA DX
140363  135046  JPL I 46        ; CALL [140431]=047402  (already-entered probe)
140364  124006  JMP 6           ; -> 140372  (not entered: proceed to mount)
140365  131005  JAZ 5           ; A==0 -> 140372  (not entered: proceed to mount)
140366  054406  LDX ,B 6
140367  135043  JPL I 43        ; CALL [140432]=035476
140370  170432  SAA 32          ; ERR 32B "directory entered" (already entered) (dd off=76272 word=170432)
140371  125017  JMP I 17        ; -> [140410]=141000  COMMON ERROR EXIT
; ============================================================================
;  STAGE 5 - hand entering-system number to CHDSI, do the mount
; ----------------------------------------------------------------------------
140372  044425  LDA ,B 25       ; A := directory datafield ptr
140373  060040  ADD 40
140374  146117  RADD CLD SD DX
140375  171020  SAT 20
140376  135036  JPL I 36        ; CALL [140434]=001224  resident block copy
140377  054406  LDX ,B 6
140400  135032  JPL I 32        ; CALL [140432]=035476
140401  050426  LDT ,B 26       ; T := entering system number (owner arg)
140402  135033  JPL I 33        ; CALL [140435]=037763 = CHDSI *** THE MOUNT *** (dd off=76292 word=135033)
140403  125005  JMP I 5         ; CHDSI ERROR return -> [140410]=141000 COMMON ERROR EXIT
140404  124032  JMP 32          ; CHDSI OK return    -> 140436  (post-mount bookkeeping)
; --- ENDIR literal pointer pool #2 (140405-140435) --------------------------
140405  030225  ; -> 030225B = GDIRA                 (dd off=76298 word=030225) VERIFIED
140406  010500  ; -> 010500B  helper
140407  141024  ; -> 141024B  ERROR EXIT D           (dd off=76302 word=141024) VERIFIED
140410  141000  ; -> 141000B  COMMON ERROR EXIT      (dd off=76304 word=141000) VERIFIED
140411  050323  ; -> 050323B
140412  053047  ; -> 053047B
140413  037377  ; -> 037377B
140414  000013  ; literal (11 dec)
140415  050223  ; -> 050223B = ISETP
140416  050231  ; -> 050231B = ICLEP  (ERROR EXIT C is [140242]/[140416]=141020; 140416 itself = ICLEP ptr)
140417  050231  ; -> 050231B = ICLEP
140420  034557  ; -> 034557B
140421  036511  ; -> 036511B
140422  141004  ; -> 141004B  ERROR EXIT E           (dd off=76324 word=141004) VERIFIED
140423  035231  ; -> 035231B
140424  035531  ; -> 035531B
140425  002000  ; literal
140426  140773  ; -> 140773B  NAME-MISMATCH tail (SAA 40)
140427  000007  ; literal
140430  041552  ; -> 041552B
140431  047402  ; -> 047402B
140432  035476  ; -> 035476B
140433  000005  ; literal
140434  001224  ; -> 001224B  resident block copy
140435  037763  ; -> 037763B = CHDSI                 (dd off=76346 word=037763) VERIFIED

; ============================================================================
;  POST-MOUNT OK PATH (140436+) - in-core directory-table bookkeeping
;  Reached ONLY on CHDSI ok-return (140404 -> 140436). Sets the in-core
;  directory-table entry live. Shown here for completeness; two of its internal
;  guards fall into the SAME 141004 release-exit (140747, 140760) - see below.
;  Full field semantics past here interleave WGLOB/RGLOB/CNREK/LBIT (140500+)
;  and are only partially settled (OPEN, tracked in ../ENTER-DIRECTORY/README).
; ----------------------------------------------------------------------------
140436  135165  JPL I 165       ; CALL [140623]  (mark entry present)
140437  046000  LDA ,X 0
;   ... (140440-140717 in-core table update; not error-bearing for ENDIR)
; --- shared unwind / return tail (converged from the exits) -----------------
140723  054425  LDX ,B 25       ; X := directory datafield ptr (unwind: reload)
140724  046003  LDA ,X 3
140725  135014  JPL I 14        ; CALL [140741]
140726  170751  SAA -27         ; A := -27  (skip/return marker, NOT an error code)
140727  125015  JMP I 15        ; -> [140744]  common return tail
;   ... 140730-140746 return/cleanup ...
140745  171043  SAT 43
140746  140065  SKP IF DA EQL ST
140747  124035  JMP 35          ; -> 141004  ERROR EXIT E  (post-mount guard -> release)
140750  044424  LDA ,B 24
140751  175305  BSKP ONE 100 DA
140752  124007  JMP 7           ; -> 140761
140753  045055  LDA I 55
140754  171377  SAT -1
140755  142065  SKP IF DA UEQ ST
140756  124003  JMP 3           ; -> 140761
140757  044052  LDA 52          ; A := (post-mount error code from literal 52)
140760  124024  JMP 24          ; -> 141004  ERROR EXIT E  (post-mount guard -> release)
140761  054425  LDX ,B 25
140762  046000  LDA ,X 0
;   ... 140763-140767 flag fixups, loop back 140767 JMP -51 -> 140716 ...
; --- parse-error tail A (from 140204) ---------------------------------------
140770  135043  JPL I 43        ; CALL [141033]
140771  125043  JMP I 43        ; ERR-> [141034]
140772  124024  JMP 24          ; -> 141016  ERROR EXIT B
; --- NAME-MISMATCH tail (from 140353 / 140361)  ERROR LADDER RUNG 4 : 40B ---
140773  050426  LDT ,B 26       ; T := entering system number
140774  146105  RADD CLD 0 DA
140775  146101  RADD CLD 0 DD
140776  135037  JPL I 37        ; CALL [141035]  (probe; A reloaded by SAA 40 next)
140777  170440  SAA 40          ; ERR 40B "directory not on specified unit" (dd off=76798 word=170440)
                                ;   falls straight through into 141000 STA ,B 2 (common exit)

; ============================================================================
;  ERROR-EXIT HANDLERS (141000-141041)
;  Each begins STA ,B 2 = store the A-register error code into local 2, which
;  is the value ENDIR returns to its caller (the command interpreter). They
;  differ only in which error-report pointer they load (LDA I nn) and whether
;  they run the reserve-release unwind (RLDIR + MON 125 ForceRelease).
; ----------------------------------------------------------------------------
; --- COMMON ERROR EXIT (141000): 145B, 32B, 40B, CHDSI-err, 140264/271/274/325
141000  004402  STA ,B 2        ; return code := A         (dd off=76800 word=004402)
141001  045035  LDA I 35        ; A := [141036] error-report arg
141002  135035  JPL I 35        ; CALL [141037]  (error-report / trace)
141003  124002  JMP 2           ; -> 141005  (fall into release unwind)
; --- ERROR EXIT E (141004): 42B, and post-mount guards 140747/140760 --------
141004  004402  STA ,B 2        ; return code := A         (dd off=76808 word=004402)
141005  050426  LDT ,B 26       ; T := entering system number
141006  054032  LDX 32
141007  135032  JPL I 32        ; CALL [141041]=RLDIR  (release in-core directory)
141010  124313  JMP -65         ; -> 140723  (shared unwind/return)
; --- reserve-release cleanup (fall-through target of the unwind) ------------
141011  054425  LDX ,B 25       ; X := directory datafield ptr
141012  046004  LDA ,X 4        ; A := datafield word 4 (reserve descriptor)
141013  131002  JAZ 2           ; if 0 (was never reserved) -> 141015 (skip release)
141014  153125  MON 125         ; ForceRelease (PRLRS) the unit (dd off=76824 word=153125) VERIFIED
                                ;   MON 125B -> undo the MON 124 reserve done at 140252
141015  124306  JMP -72         ; -> 140723  (shared unwind/return)
; --- ERROR EXIT B (141016): parse errors 140212/140214, tail A 140772 -------
141016  004402  STA ,B 2        ; return code := A
141017  124307  JMP -71         ; -> 140726  (return tail; no unit-release, none held yet)
; --- ERROR EXIT C (141020): parse errors 140223/140225, ISETP/ICLEP 140300/305
141020  004402  STA ,B 2        ; return code := A
141021  045015  LDA I 15        ; A := [141036] error-report arg
141022  135015  JPL I 15        ; CALL [141037]  (error-report / trace)
141023  124303  JMP -75         ; -> 140726  (return tail)
; --- ERROR EXIT D (141024): 147B reserve-fail (from 140255) -----------------
141024  004402  STA ,B 2        ; return code := A (=147)
141025  045011  LDA I 11        ; A := [141036] error-report arg
141026  135011  JPL I 11        ; CALL [141037]  (error-report / trace)
141027  124274  JMP -104        ; -> 140723  (shared unwind/return; MON 124 had failed,
                                ;   so ,X 4 path in 141011 finds nothing extra to free)
; --- error-report pointer pool (141030-141041) ------------------------------
141030  004156  ; literal
141031  000240  ; literal
141032  176000  ; literal
141033  047653  ; -> 047653B  (tail-A helper)
141034  140205  ; -> 140205B  (tail-A err)
141035  035240  ; -> 035240B = CL1DB  (name-mismatch tail helper)
141036  120027  ; literal (error-report arg word)
141037  010506  ; -> 010506B  error-report / trace routine
141040  000004  ; literal
141041  050124  ; -> 050124B = RLDIR path base
; ============================================================================
;  END ENDIR-COMPLETE
; ============================================================================
