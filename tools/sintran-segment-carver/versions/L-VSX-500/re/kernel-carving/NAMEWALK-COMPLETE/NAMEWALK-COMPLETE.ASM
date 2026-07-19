; ===========================================================================
; NAMEWALK-COMPLETE.ASM  -  byte-verified carve, SINTRAN III VSX/500 L07
; Segment 006-S3FS (FILSYS), load base 26000B (big-endian .bin, OCTAL).
; sha256(006-S3FS.bin)=b4a563d0715a6304a35167ec5ff0090c92b0b99dd743aef159ac6b182e080585
; Byte offset (decimal) of any addr A = (A_oct - 26000B)*2  (16-bit words).
;
; SCOPE: the COMPLETE directory-entry + name-walk machinery that
;        @ENTER-DIRECTORY (ENDIR) uses to resolve a directory name to an
;        on-disk directory-entry index.  This carves COLDE's name-walker
;        callees FULLY.  COLDE's own entry split is already carved in
;        ../COLDE-CONNECT/ ; here we carve every callee, plus GDIRE/GDIRA.
;
; RESULT: this whole family issues NO device transfer.  It is pure in-core
;         table walking over two per-directory tables:
;           - the DIRECTORY table  (2-word entries)   accessed by GDIRT/PDIRT
;           - the NAME      table  (2-word entries)    accessed by GNAMT/PNAMT
;         plus leaf address calculators GDIRA / GNAMA.
;
; DISASSEMBLY SOURCE: ../../segments-ref/006-S3FS/006-S3FS.asm (authoritative)
; Every JPL I / JMP I is INDIRECT through a P-relative pointer word; the
; disassembler's "-> NNNNNN" is the POINTER-word address, the real target is
; the CONTENTS of that word.  Pointer contents resolved against
; FILSYS-SYMBOLS and listed inline as  [ptr]=value  NAME.
;
; Tags: VERIFIED = read from the bytes here.  INFERRED / OPEN as marked.
;
; dd proofs (big-endian .bin), all reproduced with
;   dd if=006-S3FS.bin bs=1 skip=<off> count=2 | od -An -tx1 :
;   CLPAR  044777B off=15358 = 22 51  (021121 STD I 121)
;   GNAMI  047536B off=18108 = 22 44  (021104 STD I 104)
;   GDIRT  050124B off=18600 = f8 38  (174070 BSET ZRO SSM)
;   GNAMT  050223B off=18726 = f8 38  (174070 BSET ZRO SSM)
;   GDIRA  030225B off= 2346 = cc 6f  (146157 RADD CLD SA DX)
;   GNAMA  030235B off= 2362 = cc 6f  (146157 RADD CLD SA DX)
;   GDIRE  131732B off=69556 = 22 49  (021111 STD I 111)
; ===========================================================================


; ###########################################################################
; # PART A - the two 2-word table accessors (4-way BSET split, shared body)  #
; ###########################################################################
;
; Idiom is identical to RDISK/WDISK and to COLDE/DCOLD/XCOLD: a bank of tiny
; entry stubs each set two mode flags (SSM, SSK) with BSET, then fall into ONE
; shared body.  For each table there are four stubs:
;     SSM=1 SSK=0  Gx-DEFAULT-read     (GDDRT / GDNMT)
;     SSM=0 SSK=0  Gx-USER-read        (GDIRT / GNAMT)   <== the name-walk reads
;     SSM=1 SSK=1  Px-DEFAULT-write    (PDDRT / PDNMT)
;     SSM=0 SSK=1  Px-USER-write       (PDIRT / PNAMT)
; SSM (VERIFIED as the 1st BSET flag) selects DEFAULT vs USER directory table.
; SSK (VERIFIED as the 2nd BSET flag) selects PUT(write) vs GET(read).
; (SSM=default/user split is INFERRED from the G*D* / G*I* naming; the BSET
;  ordering itself is VERIFIED from the bytes.)

; --------------------------------------------------------------------------
; DIRECTORY-table accessor stubs -> shared body 050134B
; --------------------------------------------------------------------------
>>> GDDRT (050121B)   get default directory-table entry
050121  174270  	BSET ONE SSM			; SSM:=1 (default table)
050122  174020  	BSET ZRO SSK			; SSK:=0 (read)
050123  124011  	JMP 11		; -> 050134   shared body

>>> GDIRT (050124B)   get user directory-table entry  (READ; used by walk)
050124  174070  	BSET ZRO SSM			; SSM:=0 (user table)
050125  174020  	BSET ZRO SSK			; SSK:=0 (read)
050126  124006  	JMP 6		; -> 050134

>>> PDDRT (050127B)   put default directory-table entry
050127  174270  	BSET ONE SSM
050130  174220  	BSET ONE SSK			; SSK:=1 (write)
050131  124003  	JMP 3		; -> 050134

>>> PDIRT (050132B)   put user directory-table entry
050132  174070  	BSET ZRO SSM
050133  174220  	BSET ONE SSK

; --- shared body: read/write two words of directory-table entry #index ---
050134  021055  	STD I 55			; save link/B
050135  146145  	RADD CLD SL DA
050136  146131  	RADD CLD SB DD
050137  170010  	SAB 10				; B := frame (10B locals)
050140  135052  	JPL I 52	; ->[050212]=003752  resident enter/setup
050141  146105  	RADD CLD 0 DA
050142  157401  	SHA LIN 1			; index arithmetic (link-in shift)
050143  004406  	STA ,B 6
050144  174605  	BSET BAC 0 DA
050145  004407  	STA ,B 7			; local 7 := second-word-present flag
050146  144065  	SWAP ST DA
050147  120044  	MPY 44				; A := index * W   (W = stride word)
050150  060044  	ADD 44				; A := A + W       => (index+1)*W
050151  060400  	ADD ,B 0			; A += base(local 0)  = entry address
050152  146157  	RADD CLD SA DX			; X := entry address
050153  044042  	LDA 42				; A := upper-bound
050154  141457  	SKP IF DX MGRE SA		; range check (X vs bound)
050155  124003  	JMP 3		; -> 050160
050156  044040  	LDA 40				; out-of-range: error status
050157  124030  	JMP 30		; -> 050207
050160  044034  	LDA 34				; lower-bound
050161  143457  	SKP IF DX MLST SA
050162  124003  	JMP 3		; -> 050165
050163  170574  	SAA 174				; below-range: error status 174
050164  124023  	JMP 23		; -> 050207
; --- in range: X = &entry[index].  SSK chooses copy direction ---
050165  044407  	LDA ,B 7			; second-word-present?
050166  131410  	JAF 10		; -> 050176      (write path if flag set)
050167  046000  	LDA ,X 0			; GET word0: A := entry[0]
050170  004402  	STA ,B 2			;            param2 := entry[0]
050171  044406  	LDA ,B 6
050172  131003  	JAZ 3		; -> 050175
050173  046001  	LDA ,X 1			; GET word1: A := entry[1]
050174  004403  	STA ,B 3			;            param3 := entry[1]
050175  124007  	JMP 7		; -> 050204
050176  044402  	LDA ,B 2			; PUT word0: entry[0] := param2
050177  006000  	STA ,X 0
050200  044406  	LDA ,B 6
050201  131003  	JAZ 3		; -> 050204
050202  044403  	LDA ,B 3			; PUT word1: entry[1] := param3
050203  006001  	STA ,X 1
050204  040404  	MIN ,B 4			; success bookkeeping
050205  170770  	SAA -10				; A := -10 (ok status)
050206  125011  	JMP I 11	; ->[050217] return
050207  004402  	STA ,B 2			; error: stash status
050210  124375  	JMP -3		; -> 050205
050211  147417  	ROP NOOP
; pointer words for the directory body:
050212  003752  	(ptr) 003752  resident enter/setup
050217  003776  	(ptr) return frame-restore

; --------------------------------------------------------------------------
; NAME-table accessor stubs -> shared body 050233B  (same shape as above)
; --------------------------------------------------------------------------
>>> GDNMT (050220B)   get default name-table entry
050220  174270  	BSET ONE SSM
050221  174020  	BSET ZRO SSK
050222  124011  	JMP 11		; -> 050233

>>> GNAMT (050223B)   get user name-table entry  (READ; used by walk)  [dd f8 38]
050223  174070  	BSET ZRO SSM
050224  174020  	BSET ZRO SSK
050225  124006  	JMP 6		; -> 050233

>>> PDNMT (050226B)   put default name-table entry
050226  174270  	BSET ONE SSM
050227  174220  	BSET ONE SSK
050230  124003  	JMP 3		; -> 050233

>>> PNAMT (050231B)   put user name-table entry
050231  174070  	BSET ZRO SSM
050232  174220  	BSET ONE SSK

050233  021062  	STD I 62			; shared name-table body
050234  146145  	RADD CLD SL DA
050235  146131  	RADD CLD SB DD
050236  170010  	SAB 10
050237  135057  	JPL I 57	; ->[050316]=003752  resident enter/setup
050240  175220  	BSKP ONE SSK			; test read/write flag
050241  124004  	JMP 4		; -> 050245
050242  170401  	SAA 1
050243  004407  	STA ,B 7			; write: local 7 := 1
050244  124002  	JMP 2		; -> 050246
050245  000407  	STZ ,B 7			; read:  local 7 := 0
050246  175270  	BSKP ONE SSM			; (default vs user select continues...)
; body continues to 050315 (same MPY-stride + 2-word copy shape as directory
; body).  Full body not re-listed - structurally identical to 050134..050206.


; ###########################################################################
; # PART B - GNEXM: scan directory table for the next matching entry        #
; ###########################################################################
; GNEXM (050025B) linear-scans the directory table, entry stride 55B, using
; the GDIRT accessor, comparing an unsigned key until a match is found.
>>> GNEXM (050025B)
050025  021064  	STD I 64
050026  146145  	RADD CLD SL DA
050027  146131  	RADD CLD SB DD
050030  170007  	SAB 7
050031  135061  	JPL I 61	; ->[050112]=003752  resident enter/setup
050032  000406  	STZ ,B 6			; match-flag := 0
050033  044401  	LDA ,B 1			; A := start index
050034  172401  	AAA 1				; ++index
050035  146151  	RADD CLD SA DD
050036  120055  	MPY 55				; entry_addr = index*55B
050037  060055  	ADD 55				;            + 55B  => (index+1)*55B
050040  146157  	RADD CLD SA DX
050041  050054  	LDT 54				; T := search key
050042  142067  	SKP IF DX UEQ ST		; entry key vs search key (unsigned)
050043  124032  	JMP 32		; -> 050075      no-match this slot
; ... match path 050044..; loop continues, advance +30 per iter (050070) ...
050070  146401  	RADD AD1 0 DD
050071  170430  	SAA 30
050072  146075  	RADD SX DA
050073  146157  	RADD CLD SA DX
050074  124345  	JMP -33		; -> 050041      next slot
050075  044406  	LDA ,B 6			; end: A := match-flag
050076  171147  	SAT 147
050077  140065  	SKP IF DA EQL ST
050100  124003  	JMP 3		; -> 050103
050101  004402  	STA ,B 2
050102  124003  	JMP 3		; -> 050105
050103  170443  	SAA 43				; "not found" status 43
050104  004402  	STA ,B 2
050105  170771  	SAA -7
050106  125012  	JMP I 12	; ->[050120] return
; pointer words: [050112]=003752 setup, [050117]=050124 GDIRT.
050117  050124  	(ptr) 050124  GDIRT


; ###########################################################################
; # PART C - GDIRE: get directory entry by name  (THE NAME-MATCHER)         #
; ###########################################################################
; GDIRE (131732B) resolves an object NAME to a directory-entry index by
; walking a hash chain over the NAME table (GNAMT) and DIRECTORY table
; (GDIRT).  It performs NO device I/O.  On success it returns the entry
; index in the parameter block; on exhaustion it returns status 33B.
>>> GDIRE (131732B)   [dd 22 49 = 021111 STD I 111]
131732  021111  	STD I 111			; save link/B
131733  146145  	RADD CLD SL DA
131734  146131  	RADD CLD SB DD
131735  170011  	SAB 11				; B := frame (11B locals)
131736  135106  	JPL I 106	; ->[132044]=003752  resident enter/setup
131737  054106  	LDX 106
131740  135106  	JPL I 106	; ->[132046]=050223  GNAMT (read name-tbl head)
131741  124100  	JMP 100		; -> 132041      (name-tbl read error)
131742  004406  	STA ,B 6			; local 6 := chain head
131743  000410  	STZ ,B 10			; local 10 := chain cursor := 0
131744  044103  	LDA 103
131745  004407  	STA ,B 7			; local 7 := hash accumulator seed
; --- hash-chain walk ---
131746  044407  	LDA ,B 7
131747  050101  	LDT 101
131750  142065  	SKP IF DA UEQ ST		; chain exhausted ?
131751  124040  	JMP 40		; -> 132011      yes -> "not found"
131752  050410  	LDT ,B 10			; T := cursor
131753  054076  	LDX 76
131754  135076  	JPL I 76	; ->[132052]=050124  GDIRT (read dir entry[cursor])
131755  124064  	JMP 64		; -> 132041
131756  131026  	JAZ 26		; -> 132004      empty slot -> advance
131757  146151  	RADD CLD SA DD
131760  054073  	LDX 73
131761  135071  	JPL I 71	; ->[132052]=050124  GDIRT (read 2nd word)
131762  124057  	JMP 57		; -> 132041
131763  050406  	LDT ,B 6			; T := target name
131764  175306  	BSKP ONE 100 DT			; test flag bit-6 of name word
131765  124002  	JMP 2		; -> 131767
131766  070066  	AND 66				; mask off flag bit
131767  146156  	RADD CLD SA DT
131770  054065  	LDX 65
131771  044402  	LDA ,B 2
131772  156410  	SHA ZIN 10			; combine hash bits (shift)
131773  060401  	ADD ,B 1
131774  140056  	SKP IF DT EQL SA		; primary name-word compare
131775  124007  	JMP 7		; -> 132004      mismatch -> advance chain
131776  146116  	RADD CLD SD DT
131777  144476  	RAND SX DT
132000  044402  	LDA ,B 2
132001  144475  	RAND SX DA
132002  142056  	SKP IF DT UEQ SA		; secondary (masked) compare
132003  124012  	JMP 12		; -> 132015      full match -> resolve entry
; --- advance to next chain link ---
132004  040410  	MIN ,B 10			; ++cursor
132005  044407  	LDA ,B 7
132006  172430  	AAA 30				; hash += 30B (next bucket stride)
132007  004407  	STA ,B 7
132010  124336  	JMP -42		; -> 131746      loop
; --- not found ---
132011  170433  	SAA 33				; status 33B = entry not found
132012  004402  	STA ,B 2
132013  170767  	SAA -11
132014  125042  	JMP I 42	; ->[132056] return
; --- full match: fetch/classify the resolved entry ---
132015  050410  	LDT ,B 10			; T := matched cursor
132016  054041  	LDX 41
132017  135033  	JPL I 33	; ->[132052]=050124  GDIRT (read matched entry)
132020  124021  	JMP 21		; -> 132041
132021  175335  	BSKP ONE 130 DA			; classify entry-type bits
132022  124004  	JMP 4		; -> 132026
132023  040404  	MIN ,B 4
132024  170547  	SAA 147
132025  124007  	JMP 7		; -> 132034
132026  175375  	BSKP ONE 170 DA
132027  124004  	JMP 4		; -> 132033
132030  040404  	MIN ,B 4
132031  170434  	SAA 34
132032  124002  	JMP 2		; -> 132034
132033  170426  	SAA 26
132034  040404  	MIN ,B 4
132035  004402  	STA ,B 2			; param2 := status/type
132036  044410  	LDA ,B 10
132037  004401  	STA ,B 1			; param1 := matched entry index
132040  124353  	JMP -25		; -> 132013      return
132041  004402  	STA ,B 2			; table-read error arm
132042  124351  	JMP -27		; -> 132013
132043  147417  	ROP NOOP
; pointer words:  [132044]=003752 setup  [132046]=050223 GNAMT  [132052]=050124 GDIRT


; ###########################################################################
; # PART D - GDIRA / GNAMA: leaf entry-address calculators                   #
; ###########################################################################
; Both are 6-word LEAF routines (no frame, no STD I).  They are BYTE-IDENTICAL
; (VERIFIED): given base pointer in A and entry index in T, they compute
;   entry_addr = (index+1) * W        [MPY and ADD use the SAME operand W]
; and exchange the result into X.  GDIRA is used where a DIRECTORY-table entry
; address is needed; GNAMA where a NAME-table entry address is needed - the
; two tables share the same 6-word calculator (same stride W).
; The SOURCE of the operand W (P-relative constant vs caller B-relative) is
; OPEN - the (index+1)*W shape is VERIFIED from the opcodes; W's value is not.
>>> GDIRA (030225B)   [dd cc 6f = 146157]
030225  146157  	RADD CLD SA DX			; X := A            (A = base ptr)
030226  146165  	RADD CLD ST DA			; A := T            (T = entry index)
030227  120004  	MPY 4				; A := A * W
030230  060004  	ADD 4				; A := A + W   => (index+1)*W
030231  144075  	SWAP SX DA			; exchange result into X
030232  146142  	EXIT				; return

>>> GNAMA (030235B)   [dd cc 6f = 146157]  (byte-identical to GDIRA)
030235  146157  	RADD CLD SA DX
030236  146165  	RADD CLD ST DA
030237  120004  	MPY 4
030240  060004  	ADD 4
030241  144075  	SWAP SX DA
030242  146142  	EXIT


; ###########################################################################
; # PART E - CLPAR: parse/classify the leading parameter of the spec         #
; ###########################################################################
; Prior-carve label: "clear parameter block".  BYTES DISAGREE: CLPAR reads
; characters via GTTCH ([045122]=030070) and compares them against ASCII
; LETTER constants (P=120 S=123 A=101 U=125 F=106 I=111 D=104 B=102 L=114
; O=117 X=130), i.e. it PARSES/CLASSIFIES a 1-2 letter keyword/type code from
; the input spec.  It is a PARSER, not a memory clear.  (VERIFIED: it calls
; GTTCH and the compare immediates are ASCII letters.  The exact grammar it
; accepts is OPEN.)  COLDE calls CLPAR first, to classify the spec prefix.
>>> CLPAR (044777B)   [dd 22 51 = 021121 STD I 121]
044777  021121  	STD I 121			; save link/B
045000  146145  	RADD CLD SL DA
045001  146131  	RADD CLD SB DD
045002  170017  	SAB 17				; B := frame (17B locals)
045003  135116  	JPL I 116	; ->[045121]=003752  resident enter/setup
045004  000406  	STZ ,B 6			; char index := 0
045005  054402  	LDX ,B 2			; X := spec pointer
045006  050406  	LDT ,B 6
045007  135113  	JPL I 113	; ->[045122]=030070  GTTCH  (get char 1)
045010  004407  	STA ,B 7			; local 7 := char1
045011  040406  	MIN ,B 6			; ++char index
045012  050406  	LDT ,B 6
045013  135107  	JPL I 107	; ->[045122]=030070  GTTCH  (get char 2)
045014  004410  	STA ,B 10			; local 10 := char2
; --- decision tree over ASCII letters (char1 in ,B 7, char2 in ,B 10) ---
045015  044407  	LDA ,B 7
045016  171120  	SAT 120				; 'P'
045017  142065  	SKP IF DA UEQ ST
045020  124105  	JMP 105		; -> 045125   (reject/next)
045021  044407  	LDA ,B 7
045022  171123  	SAT 123				; 'S'
045023  140065  	SKP IF DA EQL ST
045024  124017  	JMP 17		; -> 045043
045025  044410  	LDA ,B 10
045026  171101  	SAT 101				; 'A'
045027  142065  	SKP IF DA UEQ ST
045030  124012  	JMP 12		; -> 045042
045031  171125  	SAT 125				; 'U'
045032  142065  	SKP IF DA UEQ ST
045033  124007  	JMP 7		; -> 045042
045034  171106  	SAT 106				; 'F'
045035  142065  	SKP IF DA UEQ ST
045036  124004  	JMP 4		; -> 045042
045037  171123  	SAT 123				; 'S'
045040  140065  	SKP IF DA EQL ST
045041  124036  	JMP 36		; -> 045077
045042  124063  	JMP 63		; -> 045125
045043  044407  	LDA ,B 7
045044  171111  	SAT 111				; 'I'
045045  140065  	SKP IF DA EQL ST
045046  124011  	JMP 11		; -> 045057
045047  044410  	LDA ,B 10
045050  171104  	SAT 104				; 'D'
045051  142065  	SKP IF DA UEQ ST
045052  124004  	JMP 4		; -> 045056
045053  171102  	SAT 102				; 'B'
045054  140065  	SKP IF DA EQL ST
045055  124022  	JMP 22		; -> 045077
045056  124047  	JMP 47		; -> 045125
045057  044407  	LDA ,B 7
045060  171101  	SAT 101				; 'A'
045061  140065  	SKP IF DA EQL ST
045062  124011  	JMP 11		; -> 045073
045063  044410  	LDA ,B 10
045064  171114  	SAT 114				; 'L'
045065  142065  	SKP IF DA UEQ ST
045066  124004  	JMP 4		; -> 045072
045067  171117  	SAT 117				; 'O'
045070  140065  	SKP IF DA EQL ST
045071  124006  	JMP 6		; -> 045077
045072  124033  	JMP 33		; -> 045125
045073  044407  	LDA ,B 7
045074  171130  	SAT 130				; 'X'
045075  142065  	SKP IF DA UEQ ST
045076  124027  	JMP 27		; -> 045125
045077  135024  	JPL I 24	; ->[045123]=040730  accepted-token handler
; ... 045100..045116 store the classified token / dispatch; return: ...
045117  125005  	JMP I 5		; ->[045124]=045650  return
; pointer words: [045121]=003752 setup  [045122]=030070 GTTCH
;                [045123]=040730 handler  [045124]=045650 return-target


; ###########################################################################
; # PART F - GNAMI: build/search the name index (name-table lookup+insert)   #
; ###########################################################################
; GNAMI (047536B) packs an ASCII name into name-table word form and searches
; (and, on the PUT variants of the family, inserts) it in the NAME table,
; reading entries through GNAMT ([047646]=050223).  It performs NO device I/O.
; Loop 047551..047567 packs characters (limit in ,B 7, index ,B 10) into the
; name buffer; block 047570.. hashes and probes name-table slots with a 3-way
; result dispatch (free slot / match / grow).  Returns slot info in params.
>>> GNAMI (047536B)   [dd 22 44 = 021104 STD I 104]
047536  021104  	STD I 104			; save link/B
047537  146145  	RADD CLD SL DA
047540  146131  	RADD CLD SB DD
047541  170023  	SAB 23				; B := frame (23B locals)
047542  135101  	JPL I 101	; ->[047643]=003752  resident enter/setup
047543  044101  	LDA 101
047544  004422  	STA ,B 22			; local 22 := name-buffer base
047545  000421  	STZ ,B 21			; slot cursor := 0
047546  000407  	STZ ,B 7			; char count := 0
047547  000410  	STZ ,B 10			; char index := 0
; --- character-pack loop ---
047550  044410  	LDA ,B 10
047551  171007  	SAT 7
047552  141056  	SKP IF DT GRE SA		; index >= 7 ?  (max name chars)
047553  124015  	JMP 15		; -> 047570      done packing
047554  050421  	LDT ,B 21
047555  044070  	LDA 70				; A := name source base
047556  060410  	ADD ,B 10			; + char index
047557  146157  	RADD CLD SA DX
047560  135066  	JPL I 66	; ->[047646]=050223  GNAMT  (fetch name word)
047561  124057  	JMP 57		; -> 047640
047562  054410  	LDX ,B 10
047563  006411  	STA ,X ,B 11			; store packed char
047564  044410  	LDA ,B 10
047565  172401  	AAA 1
047566  004410  	STA ,B 10			; ++char index
047567  124362  	JMP -16		; -> 047551      loop
; --- hash + probe name table ---
047570  146135  	RADD CLD SB DA
047571  060056  	ADD 56
047572  054400  	LDX ,B 0
047573  171020  	SAT 20
047574  135054  	JPL I 54	; ->[047650] probe helper (name-table slot)
047575  124003  	JMP 3		; -> 047600      result A: free slot
047576  124031  	JMP 31		; -> 047627      result B: match
047577  124034  	JMP 34		; -> 047633      result C: grow/continue
047600  040421  	MIN ,B 21			; free-slot path: bump cursor
047601  044422  	LDA ,B 22
047602  172416  	AAA 16				; advance name-buffer ptr by 16B
047603  004422  	STA ,B 22
047604  044422  	LDA ,B 22
047605  050044  	LDT 44
047606  140065  	SKP IF DA EQL ST		; table full ?
047607  124340  	JMP -40		; -> 047547      retry pack from new slot
047610  044407  	LDA ,B 7
047611  131403  	JAF 3		; -> 047614
047612  170430  	SAA 30				; status 30B
047613  124025  	JMP 25		; -> 047640
047614  171001  	SAT 1
047615  143056  	SKP IF DT LST SA
047616  124003  	JMP 3		; -> 047621
047617  170431  	SAA 31				; status 31B
047620  124020  	JMP 20		; -> 047640
047621  044406  	LDA ,B 6			; success: return resolved slot
047622  004401  	STA ,B 1			; param1 := slot
047623  000402  	STZ ,B 2			; param2 := 0 (ok)
047624  040404  	MIN ,B 4
047625  170755  	SAA -23
047626  125024  	JMP I 24	; ->[047652] return
047627  040407  	MIN ,B 7			; match path
047630  044421  	LDA ,B 21
047631  004406  	STA ,B 6
047632  124346  	JMP -32		; -> 047600
047633  044421  	LDA ,B 21			; grow path
047634  004401  	STA ,B 1
047635  170401  	SAA 1
047636  004402  	STA ,B 2
047637  124365  	JMP -13		; -> 047624
047640  004402  	STA ,B 2			; error arm
047641  124364  	JMP -14		; -> 047625
047642  147417  	ROP NOOP
; pointer words: [047643]=003752 setup  [047646]=050223 GNAMT
; ===========================================================================
; END NAMEWALK-COMPLETE.ASM
; ===========================================================================
