; ============================================================================
; MON 124B  PRSRV  (ForceReserve)  - byte-verified carve, SINTRAN III L07
; ============================================================================
; Running system L-VSX-500, symbols L07.
;
; Dispatch (all VERIFIED by dd, see README section (a)/(b)):
;   MON 124B -> ENT14 -> GOTAB[124]=MFELL -> CALLP -> MCTAB[124B]=037076B=PRSRV
;   MCTAB @ 005620B in 044-S3IDPIT (load base 4000B):
;     dd 044-S3IDPIT.bin byteoff 1992 -> 3e 3e = 037076B  (PRSRV)   VERIFIED
;   Sibling reserve/release family in the SAME table:
;     MCTAB[122B] byteoff 1988 -> 3e 43 = 037103B  RESRV
;     MCTAB[123B] byteoff 1990 -> 3e 6e = 037156B  RELES
;     MCTAB[125B] byteoff 1994 -> 3e 67 = 037147B  PRLS
;
; Overlay: the worker bodies are RESIDENT MONITOR code. They live in
;   071-S3SM  (System Monitor,        load base 30000B)  <-- authoritative
;   070-S3SSM (System Monitor save,   load base 30000B)  identical bytes
;   003-S3CP  (Command segment,       load base 30000B)  identical bytes
; All three carry byte-identical code at 037076..037170 (VERIFIED by cross-seg
; scan). Disassembly below is from 071-S3SM.bin via nd100-dis (little-endian).
;
; Byte offset in the .bin = (addr_oct - 30000B) * 2  (decimal).
; Every word below re-read with dd; see the >>> VERIFIED markers.
; ============================================================================


; ----------------------------------------------------------------------------
; PRSRV = 037076B  (MON 124B ForceReserve)   -- a 2-word TRAMPOLINE
; ----------------------------------------------------------------------------
; VERIFIED bytes (dd 071-S3SM.bin):
;   [037076] byteoff 7292 = 48 d2 = 044322   LDA -56
;   [037020] byteoff 7200 = 00 03 = 000003    (the constant PRSRV loads = 3)
;   [037107] byteoff 7310 = 2f 0f = 027417    (the JMP I 10 pointer word)
;
037076  044322   LDA -56       ; A := MEM[037076-56] = MEM[037020] = 000003
                                ; -> load selector/function constant 3 into A
037077  125010   JMP I 10      ; PC := MEM[037077+10] = MEM[037107] = 027417B
                                ; *** indirect jump to the reserve body at
                                ;     027417B, which is BELOW this segment's
                                ;     30000B load base -> NOT in this carve.
                                ;     The reservation primitive itself is OPEN
                                ;     (low resident, another segment). ***
;
; So PRSRV proper is only these two words: set selector=3, jump to 027417B.
; The block that follows (037100..037170) is the co-located reserve/release
; command-executor family (EXECC) shared by RESRV/PRLS/RELES; PRSRV branches
; AWAY to 027417B before reaching it.


; ----------------------------------------------------------------------------
; Neighbour words 037100..037102 (tail of preceding routine / pointer words)
; ----------------------------------------------------------------------------
037100  125742   JMP I ,B -36  ; -> pointer 037042
037101  033443   STF I ,B ,X 43
037102  035720   LDF I ,B -60


; ----------------------------------------------------------------------------
; RESRV = 037103B  (MON 122B ReserveResource)  -- sibling entry
; ----------------------------------------------------------------------------
; VERIFIED [037103] byteoff 7302 = 3d fe = 036776.
; RESRV's first words do not form a clean LDA/JMP trampoline under a linear
; sweep; the family funnels into the shared executor at 037110B (EXECC).
; Exact RESRV marshalling = INFERRED (see README caveats).
037103  036776   LDF ,X ,B -2
037104  033456   STF I ,B ,X 56
037105  037007   LDF I ,X 7
037106  035351   LDF I -27
037107  027417   LDD I ,B ,X 17   ; NOTE: this same word is PRSRV's JMP pointer
                                   ; (=027417). Dual use (pointer vs opcode)
                                   ; depending on the live path.


; ----------------------------------------------------------------------------
; EXECC = 037110B  (== OTRAN 037110B)  the shared command executor
; ----------------------------------------------------------------------------
; VERIFIED [037110]=4a 2b=045053 ; [037124]=d6 38=153070 (MON 70) ;
;          [037154]=d6 38=153070 (MON 70).
037110  045053   LDA I 53      ; A := MEM[MEM[037110+53]]  (load a gate/flag)
037111  131402   JAF 2         ; if A = 0 skip 2  -> 037113
037112  146142   EXIT          ;   early EXIT when the gate word is clear
037113  146146   RADD CLD SL DT ; T := L   (save return link)
037114  170402   SAA 2         ; A := 2
037115  004637   STA ,B -141   ; store parameter
037116  044046   LDA 46        ; A := MEM[037116+46]   (command-string source)
037117  146151   RADD CLD SA DD ; D := A
037120  135045   JPL I 45      ; call helper via [037165]=004177B (string build)
                                ;   target 004177B = low resident (OPEN)
037121  146164   RADD CLD ST DL ; L := T   (restore link)
037122  044044   LDA 44        ; A := MEM[037122+44]
037123  146151   RADD CLD SA DD ; D := A
037124  153070   MON 70        ; *** COMMND (CallCommand): run the built command
                                ;     line through the @-command interpreter.
                                ;     MON 70B -> MCTAB[070]=050673B=COMSB. ***
037125  146107   RADD CLD 0 DX  ; X := 0     (byte-scan loop init)
037126  171177   SAT 177        ; T := 177
037127  141076   SKP IF DT GRE SX
037130  124010   JMP 10         ; -> 037140
037131  146116   RADD CLD SD DT
037132  142200   LBYT           ; load byte D[X]
037133  171047   SAT 47         ; T := 47   (field delimiter, octal 47 = "'")
037134  142065   SKP IF DA UEQ ST
037135  124003   JMP 3          ; -> 037140
037136  173401   AAX 1          ; X := X+1
037137  124367   JMP -11        ; -> 037126   (loop over the string)
037140  146175   RADD CLD SX DA
037141  172402   AAA 2
037142  156577   SHA ZIN SHR 1
037143  146015   RADD SD DA
037144  146151   RADD CLD SA DD
037145  146156   RADD CLD SA DT
037146  146107   RADD CLD 0 DX


; ----------------------------------------------------------------------------
; PRLS = 037147B  (MON 125B ForceRelease)  -- sibling entry into the executor
; ----------------------------------------------------------------------------
; VERIFIED [037147]=c4 80=142200 ; [037154]=d6 38=153070 (MON 70).
037147  142200   LBYT           ; scan next field
037150  171047   SAT 47         ; delimiter 47
037151  142065   SKP IF DA UEQ ST
037152  124004   JMP 4          ; -> 037156
037153  146115   RADD CLD SD DA
037154  153070   MON 70         ; *** COMMND: run RELEASE command line -> COMSB
037155  124350   JMP -30        ; -> 037125   (back into the scan loop)


; ----------------------------------------------------------------------------
; RELES = 037156B  (MON 123B ReleaseResource)  -- sibling entry
; ----------------------------------------------------------------------------
; VERIFIED [037156]=cc 67=146147.
037156  146147   RADD CLD SL DX ; X := L
037157  135010   JPL I 10       ; call helper via [037167]=004116B  (OPEN, low res)
037160  044640   LDA ,B -140
037161  004637   STA ,B -141
037162  146172   RADD CLD SX DP
037163  007252   STA I ,X -126
037164  002200   STZ ,X -200
037165  004177   (data 004177)  ; = pointer 004177B used by JPL I 45 @037120
037166  075771   (data 075771)
037167  004116   (data 004116)  ; = pointer 004116B used by JPL I 10 @037157
;   037170.. onward: an ascending pointer table (037206,037213,037220,...) -
;   data, not code.


; ============================================================================
; DEPENDENCY: MON 70B  COMMND / CallCommand  (COMSB)
; ============================================================================
; VERIFIED: MCTAB[070B] @ 044-S3IDPIT byteoff 1936 = 51 bb = 050673B = COMSB.
;   COMSB 050673B is the @-command interpreter worker (siblings UECOM 050701B,
;   UELOG 050726B; the family that decodes/executes a SINTRAN command line).
;   Carve: 003-S3CP (see re/mon-analysis/317B-ExecuteCommand + the command
;   interpreter family). COMSB body itself is not re-listed here.
;
; So the RESERVE/RELEASE monitor calls are realised by BUILDING a command line
; and running it through the command interpreter (MON 70B), except PRSRV which
; trampolines to the dedicated reserve primitive at 027417B (OPEN).


; ============================================================================
; THE BLOCK-0 READ-MODIFY-WRITE  "directory reserve"  (SEPARATE from MON 124)
; ============================================================================
; This is the DIRECTORY-level ownership stamp done by @ENTER-DIRECTORY's CHDSI,
; NOT the device-unit reservation done by MON 124 PRSRV. Bodies are in
; 006-S3FS (load base 26000B); full carve in ../ENTER-DIRECTORY/ and ../RCBLO/.
; Re-verified excerpts (WXDIR write-back path):
;
; WXDIR = 037702B  (recompute checksum of ext-info words 1..7, write block 0)
037702  021050   STD I 50      ; link-save prologue
037714  143061   SKP IF DD LST ST   ; checksum loop guard
037716  062000   ADD ,X 0      ;   sum += word[X]
037717  173401   AAX 1
037720  146401   RADD AD1 0 DD
037721  124373   JMP -5        ; -> 037714  (loop words 1..7)
037722  054400   LDX ,B 0
037723  006000   STA ,X 0      ; store fresh checksum into the block-0 image
037727  135025   JPL I 25      ; -> [037754] WCBLO : WRITE block 0 back (fn 1)
037730  135025   JPL I 25      ; -> [037755]        (retry / second dispatch)
037747  170435   SAA 35        ; error 35B "Master block transfer error" on
                               ;   write failure  (the page-0 WRITE fault code)
;
; Read side: RXDIR 037643B sets block:=0 (037651 RADD CLD 0 DD) then
;   JPL I 23 -> [037675]=035766B RCBLO, whose JPL I ,B 10 (036135/.../036174)
;   is the per-device transfer that ISSUES the block-0 read. On SCSI that
;   dispatch never runs for block 0 (SCWAQ empty) -> the read-modify-write
;   never completes. See ../RCBLO/README.md and README section (d).
; ============================================================================
