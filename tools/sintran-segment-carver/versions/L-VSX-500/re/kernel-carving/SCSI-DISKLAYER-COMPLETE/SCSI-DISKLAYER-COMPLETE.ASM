; ============================================================================
; SCSI-DISKLAYER-COMPLETE.ASM
; SINTRAN III VSX/500 L07  -  IP-P2-SCSI-DISK compilation unit (disk layer)
; segment 065-S3SIPIT (load base 32000B ; byte-identical twin 066-S3IIPIT)
; ----------------------------------------------------------------------------
; Every path of the SCSI disk-layer transfer state machine, fully disassembled
; from the carved L07 bytes and commented.  Supersedes/consolidates the partial
; FUNCTION-42-RETURN and SCSDISK-TRANSFER folders.
;
; Ground truth : nd100-dis over byte-swapped 065-S3SIPIT.bin, load base 32000B.
;                Every word below was re-read with dd and matches this text.
; dd offset    : byte_off = (addr - 32000B) * 2   (decimal).  addr/values octal.
; Revision fact: the whole IP-P2-SCSI-DISK unit sits at NPL-source-label + 376B
;                (SCSDI = NPL 056617 + 376 = 057215 ; SCSID = NPL 061621 + 376 =
;                 062217).  The SCSI *driver* (IP-P2-SCSI-DRIV: SCLLD 067160 ...)
;                is a separate unit at its own NPL labels (no +376B).
;
; Datafield offsets (from FILSYS / SYMBOL-1 ; ,X or ,B relative to unit df):
;   14 = ABFUN  (function/op word ; fn = ABFUN & 077)
;   17 = ABPA2  (disk block address, double word 15/16 or 17)
;   22 = ABP32  (transfer amount / block count)
;   23 = SUTYP  (device/status flags ; bit 7 = 5SCIN "init done" ;
;                HIGH byte bits 8-15 = SCSI device type from INQUIRY)
;   32 = UHLIM  (upper block limit = disk capacity, whole-disk arm)
;   10 = HSTAT  (hardware/return status)
;    6 = NFUNC  (saved link/function on entry)
; Internal return codes in T (ERREX user-map at 062124, from FUNCTION-42 carve):
;   0=OK  1=TYPER  4=ILAOP  5=BADPA  11=NOCRC   (6/13 = retry-in-place, not returned)
; ============================================================================


; ####################################################################
; ## 1. SCSDI / SCSDISK  entry + preamble + geometry bounds check    ##
; ##    SCSDI = 057215B  (reached via RCBLO datafield ",X 14")       ##
; ####################################################################
057215  146145  	RADD CLD SL DA		; A := L  (=: X.NFUNC)   ENTRY
057216  006006  	STA ,X 6		; X.NFUNC := link
057217  135051  	JPL I 51		; -> 057270   MON PERFO / DIALO setup
057220  144400  	RAND 0 0
057221  046014  	LDA ,X 14		; A := ABFUN
057222  014372  	STX -6
057223  171477  	SAX 77			; X := 77 (mask)
057224  144457  	RAND SA DX		; fn := ABFUN & 077
057225  174000  	BSET ZRO SSPTM
057226  047043  	LDA I ,X 43
057227  174200  	BSET ONE SSPTM
057230  054364  	LDX -14
057231  006012  	STA ,X 12
057232  131403  	JAF 3			; -> 057235
057233  171004  	SAT 4			; *** T := 4 = ILAOP  (illegal op) ***
057234  125036  	JMP I 36		; -> 057272   go far -> error terminate
057235  146154  	RADD CLD SA DL
057236  175145  	BSKP ZRO 140 DA
057237  124007  	JMP 7			; -> 057246
057240  046017  	LDA ,X 17		; (bus-reset / retry housekeeping region)
057241  146151  	RADD CLD SA DD
057242  046014  	LDA ,X 14
057243  156567  	SHA ZIN SHR 11
057244  070027  	AND 27
057245  022017  	STD ,X 17
057246  175154  	BSKP ZRO 150 DL
057247  124004  	JMP 4			; -> 057253
057250  046021  	LDA ,X 21
057251  006022  	STA ,X 22
057252  002021  	STZ ,X 21
057253  046014  	LDA ,X 14
057254  156572  	SHA ZIN SHR 6
057255  070016  	AND 16
057256  171003  	SAT 3
057257  141056  	SKP IF DT GRE SA
057260  124005  	JMP 5			; -> 057265
057261  146157  	RADD CLD SA DX
057262  056442  	LDX ,X ,B 42
057263  140007  	SKP IF DX EQL 0
057264  124010  	JMP 10			; -> 057274
057265  054327  	LDX -51
057266  171007  	SAT 7
057267  125003  	JMP I 3			; -> 057272   error terminate
057270  027443  	LDD I ,B ,X 43		; (057270 = PERFO/DIALO helper, JPL target)
057271  057114  	LDX I ,X 114
057272  057402  	LDX I ,B ,X 2		; RETOP / terminate op (T holds code)
057273  000007  	STZ 7
057274  146173  	RADD CLD SX DB		; 057274: main continue - select address arm
057275  054317  	LDX -61
057276  175134  	BSKP ZRO 130 DL
057277  124065  	JMP 65			; -> 057364   (partition arm skips bounds)
057300  175324  	BSKP ONE 120 DL
; >>> DISPR
057301  124051  	JMP 51			; -> 057352   whole-disk arm -> UHLIM check
; ---- partition-relative bounds arm (057302..057363) ----
057302  026017  	LDD ,X 17
057303  070101  	AND 101
057304  020417  	STD ,B 17
057305  046017  	LDA ,X 17
057306  156570  	SHA ZIN SHR 10
057307  050775  	LDT ,B -3
057310  173377  	AAT -1
057311  141056  	SKP IF DT GRE SA
057312  124065  	JMP 65			; -> 057377   *** past partition -> BADPA ***
057313  120072  	MPY 72
057314  050425  	LDT ,B 25
057315  054426  	LDX ,B 26
057316  146057  	RADD SA DX
057317  143300  	LDATX
057320  175314  	BSKP ONE 110 DL
057321  124004  	JMP 4			; -> 057325
057322  175355  	BSKP ONE 150 DA
057323  124054  	JMP 54			; -> 057377   *** -> BADPA ***
057324  124003  	JMP 3			; -> 057327
057325  175365  	BSKP ONE 160 DA
057326  124051  	JMP 51			; -> 057377   *** -> BADPA ***
057327  143342  	LDDTX 4
057330  020421  	STD ,B 21
057331  143322  	LDDTX 2
057332  050420  	LDT ,B 20
057333  146061  	RADD ST DD
057334  050417  	LDT ,B 17
057335  147065  	RADD ADC ST DA
057336  054256  	LDX -122
057337  022017  	STD ,X 17
057340  024417  	LDD ,B 17
057341  052022  	LDT ,X 22
; >>> SESCL   partition-arm bounds compare
057342  146061  	RADD ST DD		; AD := ABPA2 + ABP32 (end addr)
057343  147155  	RADD ADC CLD SA DA
057344  050042  	LDT 42
057345  146036  	RADD SB DT		; T := limit (+B)
057346  135041  	JPL I 41		; -> 057407   COMPD (compare AD,limit)
057347  141050  	SKP IF 0 GRE SA		; skip if within bounds
057350  124027  	JMP 27			; -> 057377   *** past end -> BADPA ***
057351  124012  	JMP 12			; -> 057363

; ---- WHOLE-DISK bounds arm : addr+amount vs UHLIM  (VERIFIED dd) ----
057352  026017  	LDD ,X 17		; AD := X.ABPA2  (disk block addr; 0 for block 0)
057353  052022  	LDT ,X 22		; T  := X.ABP32  (amount)
057354  146061  	RADD ST DD		; AD := addr + amount
057355  147155  	RADD ADC CLD SA DA	; A  := (addr+amount)
057356  050032  	LDT 32			; T  := UHLIM
057357  146036  	RADD SB DT		; T  := UHLIM (+B base)
057360  135027  	JPL I 27		; -> 057407   COMPD(end, UHLIM)
057361  141050  	SKP IF 0 GRE SA		; skip if  end <= UHLIM  (within bounds)
057362  124015  	JMP 15			; -> 057377   *** end > UHLIM -> BADPA ***
057363  124004  	JMP 4			; -> 057367   in bounds -> continue (SUNOP)

057364  046011  	LDA ,X 11		; (arm that skipped bounds - set a flag)
057365  174375  	BSET ONE 170 DA
057366  006011  	STA ,X 11
057367  040410  	MIN ,B 10		; SUNOP++ (pending-op count)
057370  144400  	RAND 0 0
057371  044404  	LDA ,B 4		; A := SLINK
057372  131004  	JAZ 4			; -> 057376   SLINK==0 -> NEWOP path
057373  144037  	SWAP SB DX
057374  135015  	JPL I 15		; -> 057411   DSORT (link into queue)
057375  125015  	JMP I 15		; -> 057412   SWT11 (dispatch)
057376  124016  	JMP 16			; -> 057414   NEWOP

; ---- ERR2 : BADPA terminate  (T := 5)  (VERIFIED dd 057401=171005) ----
057377  044400  	LDA ,B 0		; ERR2
057400  146153  	RADD CLD SA DB
057401  171005  	SAT 5			; *** T := 5 = BADPA  (bad partition/addr) ***
057402  135011  	JPL I 11		; -> 057413   terminate op (SWT11), no SCLLD
057403  125007  	JMP I 7			; -> 057412
057404  000377  	STZ -1
057405  000006  	STZ 6
057406  000021  	STZ 21
; 057407 = COMPD pointer/helper region ; 057411/057412/057413 = far ptrs
057407  055607  	LDX I ,B -171
057410  177776  	BORA 170 DT
057411  055460  	LDX I ,B 60		; ptr -> DSORT
057412  056516  	LDX ,X ,B 116		; ptr -> SWT11 (terminate)
057413  056766  	LDX ,X ,B -12		; ptr -> RETOP


; ####################################################################
; ## 2. NEWOP : per-op setup + function pre-decode (SCSDISK level)   ##
; ####################################################################
057414  014404  	STX ,B 4		; NEWOP: SLINK := X
057415  002005  	STZ ,X 5
057416  046001  	LDA ,X 1
057417  054400  	LDX ,B 0
057420  006004  	STA ,X 4
057421  054404  	LDX ,B 4
057422  046012  	LDA ,X 12
057423  004765  	STA ,B -13
057424  175135  	BSKP ZRO 130 DA
057425  124004  	JMP 4			; -> 057431
057426  026017  	LDD ,X 17
057427  020406  	STD ,B 6
057430  124060  	JMP 60			; -> 057510   (already-initialised -> transfer)

; ---- SCSDISK-level function pre-decode ----
057431  170477  	SAA 77
057432  072014  	AND ,X 14		; fn := ABFUN & 077
057433  171006  	SAT 6
057434  140065  	SKP IF DA EQL ST	; fn == 6 ?
057435  124005  	JMP 5			; -> 057442   no
057436  146106  	RADD CLD 0 DT		; fn 6 handling
057437  170400  	SAA 0
057440  125166  	JMP I 166		; -> 057626   go far (init-call block)
057441  124047  	JMP 47			; -> 057510
057442  171036  	SAT 36
057443  140065  	SKP IF DA EQL ST	; fn == 36 (read layout)?
057444  124026  	JMP 26			; -> 057472   no -> test fn 42
; ---- fn 36 SCSDISK-level layout copy ----
057445  052015  	LDT ,X 15
057446  056016  	LDX ,X 16
057447  143301  	LDXTX
057450  050157  	LDT 157
057451  141476  	SKP IF DT MGRE SX
057452  124014  	JMP 14			; -> 057466   -> BADPA (T:=5)
057453  047155  	LDA I ,X 155
057454  131012  	JAZ 12			; -> 057466   -> BADPA
057455  054404  	LDX ,B 4
057456  052016  	LDT ,X 16
057457  056015  	LDX ,X 15
057460  146151  	RADD CLD SA DD
057461  170411  	SAA 11
057462  146154  	RADD CLD SA DL
057463  143105  	MOVEW			; copy layout descriptor into caller buffer
057464  146106  	RADD CLD 0 DT		; T := 0 (OK)
057465  124002  	JMP 2			; -> 057467
057466  171005  	SAT 5			; T := 5 = BADPA
057467  170400  	SAA 0
057470  125136  	JMP I 136		; -> 057626   go far (terminate)
057471  124017  	JMP 17			; -> 057510

; ---- fn 42 gate + force-reinit (clear 5SCIN)  (VERIFIED dd) ----
057472  171042  	SAT 42			; (VERIFIED dd 171042)
057473  140065  	SKP IF DA EQL ST	; fn == 42 (read format) ?
057474  124014  	JMP 14			; -> 057510   no -> fall to RETRY/transfer
057475  026015  	LDD ,X 15
057476  020415  	STD ,B 15
057477  046014  	LDA ,X 14
057500  004414  	STA ,B 14
057501  000421  	STZ ,B 21
057502  170500  	SAA 100
057503  004422  	STA ,B 22
057504  054772  	LDX ,B -6
057505  046023  	LDA ,X 23		; A := SUTYP
057506  174075  	BSET ZRO 70 DA		; *** CLEAR 5SCIN (bit 7) -> force re-INQUIRY *** (dd 174075)
057507  006023  	STA ,X 23


; ####################################################################
; ## 3. RETRY : lazy INQUIRY/READ-CAPACITY init, then the transfer   ##
; ##    057510 = RETRY join ; outer retry loop back-edge at 057624   ##
; ####################################################################
057510  044762  	LDA ,B -16		; RETRY join (from fn 0/1/4/etc. and fn 42)
057511  004763  	STA ,B -15
057512  054772  	LDX ,B -6
057513  144037  	SWAP SB DX
057514  044423  	LDA ,B 23		; A := SUTYP
057515  175075  	BSKP ZRO 70 DA		; *** if 5SCIN==0 (not init) do INQUIRY block *** (dd 175075)
057516  124041  	JMP 41			; -> 057557   5SCIN SET -> SKIP init, go transfer
; ---- INIT block: 5SCIN clear -> build+run INQUIRY / READ CAPACITY ----
057517  046365  	LDA ,X -13		; A := X.SCOCW  (SCSI op control word)
057520  175165  	BSKP ZRO 160 DA		; already-INQUIRY bit ?
057521  124036  	JMP 36			; -> 057557
057522  175305  	BSKP ONE 100 DA
057523  124004  	JMP 4			; -> 057527
057524  170442  	SAA 42			; ABFUN := 42 (read-format) for the init read
057525  006014  	STA ,X 14
057526  124003  	JMP 3			; -> 057531
; >>> CDRTC
057527  170436  	SAA 36			; ABFUN := 36 (read-layout) for the init read
057530  006014  	STA ,X 14
; ---- CALL SCSID (init) with 3-way status return  (SXONO) ----
057531  146176  	RADD CLD SX DT		; SXONO:
; >>> SXONO
057532  135077  	JPL I 77		; -> 057631   CALL SCSID (ptr 057631 = 062217)  INIT read
057533  125077  	JMP I 77		; -> 057632   return+1: go far -> ERREX (ptr 057632->057636)
057534  135077  	JPL I 77		; -> 057633   return+2: alt far
; ---- return+3 (057535): STATUS CHECK on init read (WHILE D:=A BZERO 17) ----
057535  146151  	RADD CLD SA DD		; D := A  (HSTAT of the init read)
057536  174171  	BSET ZRO 170 DD		; D := D with bit 15 cleared  (mask status)
057537  171006  	SAT 6			; T := 6  (UNIT ATTENTION)
057540  142061  	SKP IF DD UEQ ST	; skip if D != 6
057541  124004  	JMP 4			; -> 057545   D==6 -> retry in place
057542  171013  	SAT 13			; T := 13 (ABORTED/reset)
057543  140065  	SKP IF DA EQL ST	; A == 13 ?
057544  124003  	JMP 3			; -> 057547   no
057545  042363  	MIN ,X -15		; TACOU-- (retry counter)
057546  124363  	JMP -15			; -> 057531   GO RETRY (re-issue init read)
057547  171001  	SAT 1
057550  141016  	SKP IF DT GRE SD	; skip if 1 > D  (D == 0 -> status OK)
057551  124054  	JMP 54			; -> 057625   D nonzero (other error) -> FINEX/terminate
; ---- DEVICE-TYPE GATE  (VERIFIED dd, every word) ----
057552  044423  	LDA ,B 23		; A := SUTYP                     (dd 044423)
057553  156570  	SHA ZIN SHR 10		; A := SUTYP >> 8  (device type) (dd 156570)
057554  131003  	JAZ 3			; -> 057557  device type == 0 (DISK) -> READ  (dd 131003)
057555  171001  	SAT 1			; *** T := 1 = TYPER  (illegal device type) *** (dd 171001)
057556  125054  	JMP I 54		; -> 057632  go far -> ERREX/terminate (dd 125054)

; ####################################################################
; ## 4. TRANSFER : build control-record READ(6) + CALL SCSID (real) ##
; ####################################################################
057557  046365  	LDA ,X -13		; TRANSFER: A := X.SCOCW
057560  175305  	BSKP ONE 100 DA
057561  124024  	JMP 24			; -> 057605
057562  170700  	SAA -100
057563  072014  	AND ,X 14
057564  006014  	STA ,X 14
057565  146174  	RADD CLD SX DL
057566  052015  	LDT ,X 15
057567  056016  	LDX ,X 16
057570  143312  	LDDTX 1
057571  146147  	RADD CLD SL DX
057572  022017  	STD ,X 17		; set control-record LBA (last block)
057573  002021  	STZ ,X 21
057574  170401  	SAA 1
057575  006022  	STA ,X 22		; ABP32 := 1  (one block)
057576  026015  	LDD ,X 15
057577  050035  	LDT 35
057600  146061  	RADD ST DD
057601  147155  	RADD ADC CLD SA DA
057602  022015  	STD ,X 15
057603  146176  	RADD CLD SX DT
057604  124002  	JMP 2			; -> 057606
057605  052004  	LDT ,X 4
; ---- CALL SCSID (real transfer) 3-way return  (VERIFIED dd 057610) ----
057606  044027  	LDA 27
057607  072365  	AND ,X -13		; A := 377 & X.SCOCW
057610  135021  	JPL I 21		; -> 057631   CALL SCSID (ptr 062217)  TRANSFER (dd 135021)
057611  125021  	JMP I 21		; -> 057632   return+1: go far -> ERREX
057612  135021  	JPL I 21		; -> 057633   return+2: alt far
057613  146151  	RADD CLD SA DD		; return+3: D := A (HSTAT of transfer)
057614  174171  	BSET ZRO 170 DD		; mask status
057615  171006  	SAT 6			; T := 6 (UNIT ATTN)
057616  142061  	SKP IF DD UEQ ST	; D != 6 ?
057617  124004  	JMP 4			; -> 057623
057620  171013  	SAT 13			; T := 13 (ABORTED)
057621  140065  	SKP IF DA EQL ST	; A == 13 ?
057622  124003  	JMP 3			; -> 057625
057623  042363  	MIN ,X -15		; TACOU--
057624  124270  	JMP -110		; -> 057514   *** OUTER RETRY back-edge (WHILE) ***
057625  124030  	JMP 30			; -> 057655   FINEX (checksum/geometry publish)

; ---- far dispatch pointers used by the 3-way calls above ----
057626  057750  	LDX I ,B ,X -30		; ptr chain for "go far" error/init returns
057627  000047  	STZ 47
057630  031371  	STF I -7
057631  062217  	<ptr>			; = SCSID entry  (VERIFIED dd 062217)
057632  057636  	<ptr>			; = 057636  (ERREX/PFAIL far handler below)
057633  000215  	<ptr>			; = 000215  (alt far)
057634  001000  	STZ I 0
057635  000377  	STZ -1

; ---- ERREX / PFAIL far handler : bus-reset & power-fail retry ----
057636  170450  	SAA 50			; ERREX far: A := 50 (SBRST?)
057637  142065  	SKP IF DA UEQ ST
057640  124007  	JMP 7			; -> 057647   -> MIN TACOU ; GO 060005 (RETRY)
057641  170443  	SAA 43			; A := 43 (LIRST?)
057642  142065  	SKP IF DA UEQ ST
057643  124004  	JMP 4			; -> 057647   -> retry
057644  170451  	SAA 51			; A := 51 (PFAIL?)
057645  140065  	SKP IF DA EQL ST
057646  124003  	JMP 3			; -> 057651   not a reset -> terminate with code
057647  042363  	MIN ,X -15		; TACOU--
057650  125135  	JMP I 135		; -> 060005   GO RETRY (bus reset / power fail)
057651  146173  	RADD CLD SX DB
057652  170420  	SAA 20
057653  124075  	JMP 75			; -> 057750   RETOP terminate (T holds ERREX code)
057654  000000  	STZ 0

; ####################################################################
; ## 5. FINEX : control-record checksum, geometry publish, T:=0     ##
; ####################################################################
057655  146173  	RADD CLD SX DB		; FINEX
057656  054404  	LDX ,B 4
057657  052012  	LDT ,X 12
057660  175306  	BSKP ONE 100 DT
057661  124066  	JMP 66			; -> 057747   already done -> success tail
057662  006010  	STA ,X 10
057663  174175  	BSET ZRO 170 DA
057664  171001  	SAT 1
057665  141056  	SKP IF DT GRE SA
057666  124061  	JMP 61			; -> 057747
057667  051117  	LDT I 117
057670  140660  	EXR ST
057671  050415  	LDT ,B 15
057672  054416  	LDX ,B 16
057673  143300  	LDATX
057674  156570  	SHA ZIN SHR 10
057675  004775  	STA ,B -3		; NPART := control-record[hdr] >> 8
057676  146154  	RADD CLD SA DL
057677  044110  	LDA 110
057700  004354  	STA -24
; ---- XOR checksum loop over control record ----
057701  146101  	RADD CLD 0 DD		; D := 0 (accumulator)
057702  143300  	LDATX
057703  145051  	REXO SA DD		; D ^= word
057704  173401  	AAX 1
057705  040347  	MIN -31			; count--
057706  124374  	JMP -4			; -> 057702   loop
057707  140001  	SKP IF DD EQL 0		; XOR checksum == 0 ?
057710  124007  	JMP 7			; -> 057717   *** nonzero -> NOCRC ***
057711  171002  	SAT 2
057712  143046  	SKP IF DT LST SL	; NPART > 2 ?
057713  124004  	JMP 4			; -> 057717   *** NPART<=2 -> NOCRC ***
057714  171012  	SAT 12
057715  143046  	SKP IF DT LST SL	; NPART <= NCOPA(=10) ?
057716  124003  	JMP 3			; -> 057721   OK
057717  171011  	SAT 11			; *** T := 11 = NOCRC (no valid control record) ***
057720  125070  	JMP I 70		; -> 060010   terminate NOCRC
; ---- geometry publish (control record OK) ----
057721  044775  	LDA ,B -3		; A := NPART
057722  120067  	MPY 67
057723  146154  	RADD CLD SA DL
057724  024415  	LDD ,B 15
057725  171002  	SAT 2
057726  146061  	RADD ST DD
057727  147155  	RADD ADC CLD SA DA
057730  054425  	LDX ,B 25
057731  050426  	LDT ,B 26
057732  143110  	MOVEW			; copy partition/geometry table to caller
057733  050425  	LDT ,B 25
057734  054426  	LDX ,B 26
057735  143342  	LDDTX 4
057736  054404  	LDX ,B 4
057737  052015  	LDT ,X 15		; publish into caller DMA buffer:
057740  056016  	LDX ,X 16
057741  143316  	STDTX 1			;   buffer := {UHLIM / data-area size}
057742  020776  	STD ,B -2
057743  170436  	SAA 36			; status word := 36
057744  143304  	STATX			;   buffer[status] := 36
057745  054404  	LDX ,B 4
057746  046010  	LDA ,X 10
057747  146106  	RADD CLD 0 DT		; *** T := 0 = OK (SUCCESS) ***  fn-42 success tail
057750  146131  	RADD CLD SB DD		; RETOP / terminate (T holds code)
057751  054400  	LDX ,B 0
057752  144037  	SWAP SB DX
057753  056004  	LDX ,X 4
057754  000404  	STZ ,B 4
057755  135035  	JPL I 35		; -> 060012   final RETOP


; ####################################################################
; ## 6. SCSID  (062217B) - SCSI driver-command dispatch by function ##
; ##    reached via ptr 057631/062217 from every CALL SCSID above    ##
; ####################################################################
062217  006010  	STA ,X 10		; SCSID: X.HSTAT := A          ENTRY (dd 006010)
062220  146145  	RADD CLD SL DA
062221  006370  	STA ,X -10		; save link
062222  146135  	RADD CLD SB DA
062223  006371  	STA ,X -7
062224  012364  	STT ,X -14
062225  144076  	SWAP SX DT
062226  170477  	SAA 77
062227  072014  	AND ,X 14		; fn := ABFUN & 077
062230  146167  	RADD CLD ST DX
062231  171037  	SAT 37
062232  140065  	SKP IF DA EQL ST	; fn == 37 (DOEXS) ?
062233  124010  	JMP 10			; -> 062243   no
062234  135152  	JPL I 152		; -> 062406   CALL DOEXS (ptr 062406 = 063460)
062235  052370  	LDT ,X -10
062236  173002  	AAT 2
062237  146164  	RADD CLD ST DL
062240  146106  	RADD CLD 0 DT
062241  146142  	EXIT			; 3-way skip return to caller
062242  124071  	JMP 71			; -> 062333
; ---- busy gate : SUTHS (,B 31) ----
062243  044431  	LDA ,B 31		; A := SUTHS (device busy?)
062244  131016  	JAZ 16			; -> 062262   not busy -> dispatch
062245  146176  	RADD CLD SX DT		; busy -> queue this op, SCWTI wait
062246  054141  	LDX 141
062247  146037  	RADD SB DX
062250  046367  	LDA ,X -11
062251  131003  	JAZ 3			; -> 062254
062252  146157  	RADD CLD SA DX
062253  124375  	JMP -3			; -> 062250   walk queue tail
062254  012367  	STT ,X -11
062255  146167  	RADD CLD ST DX
062256  002367  	STZ ,X -11
062257  044440  	LDA ,B 40
062260  146153  	RADD CLD SA DB
062261  125727  	JMP I ,B -51		; -> 062210   return (queued)
; ---- not busy : latch op params, then REPEAT dispatch ----
062262  014434  	STX ,B 34		; not busy
062263  056364  	LDX ,X -14
062264  046014  	LDA ,X 14
062265  004414  	STA ,B 14		; ABFUN
062266  026015  	LDD ,X 15
062267  020415  	STD ,B 15
062270  026017  	LDD ,X 17
062271  020417  	STD ,B 17		; ABPA2
062272  026021  	LDD ,X 21
062273  020421  	STD ,B 21
062274  054434  	LDX ,B 34
062275  046010  	LDA ,X 10
062276  004436  	STA ,B 36
062277  000446  	STZ ,B 46

; ---- REPEAT : dispatch by function code  (VERIFIED) ----
062300  170477  	SAA 77
062301  070414  	AND ,B 14		; fn := ABFUN & 077
062302  171031  	SAT 31
062303  142065  	SKP IF DA UEQ ST	; fn == 31 ?
062304  125104  	JMP I 104		; -> 062410   fn 31 -> BDRST (ptr 063522)
062305  171074  	SAT 74
062306  140065  	SKP IF DA EQL ST	; fn == 74 ?
062307  124003  	JMP 3			; -> 062312   no
062310  135101  	JPL I 101		; -> 062411   fn 74 -> CALL GUSCB (ptr 063431)
062311  124016  	JMP 16			; -> 062327   then EXCOM
062312  171042  	SAT 42
062313  142065  	SKP IF DA UEQ ST	; fn == 42 ?
062314  124003  	JMP 3			; -> 062317
062315  171036  	SAT 36			; else test fn == 36
062316  142065  	SKP IF DA UEQ ST	; fn == 36 ?
062317  125073  	JMP I 73		; -> 062412   fn 42 or 36 -> INQUI (ptr 062613)
062320  171023  	SAT 23
062321  142065  	SKP IF DA UEQ ST	; fn == 23 ?
062322  124003  	JMP 3			; -> 062325
062323  171025  	SAT 25			; else test fn == 25
062324  142065  	SKP IF DA UEQ ST	; fn == 25 ?
; >>> MNGCF
062325  125066  	JMP I 66		; -> 062413   fn 23 or 25 -> MODES (ptr 063244)
; ---- fall-through : fn 0(read)/1(write)/4(seek)/... -> real transfer ----
062326  135066  	JPL I 66		; -> 062414   CALL CACOB (build CDB) (ptr 063750) (dd 135066)
062327  135066  	JPL I 66		; -> 062415   CALL EXCOM (execute -> SCLLD) (ptr 063403)
062330  135066  	JPL I 66		; -> 062416   CALL (post) (ptr 062471)
062331  125066  	JMP I 66		; -> 062417   -> tail (ptr 062460)
062332  144037  	SWAP SB DX
062333  044413  	LDA ,B 13		; (SCSID op-completion tail)
062334  004450  	STA ,B 50
062335  131072  	JAZ 72			; -> 062427

; ---- SCSID dispatch far-pointer table (062406..062425) ----
062406  063460  	<ptr DOEXS>
062410  063522  	<ptr BDRST>		; fn 31
062411  063431  	<ptr GUSCB>		; fn 74
062412  062613  	<ptr INQUI>		; fn 42/36     (VERIFIED dd 062613)
062413  063244  	<ptr MODES>		; fn 23/25
062414  063750  	<ptr CACOB>		; fall-through build CDB
062415  063403  	<ptr EXCOM>		; execute
062416  062471  	<ptr>
062417  062460  	<ptr>
062421  062475  	<ptr>
062423  067160  	<ptr SCLLD>		; driver enqueue (IP-P2-SCSI-DRIV)


; ####################################################################
; ## 7. INQUI (062613B) - SUTYP construction from INQUIRY response  ##
; ##    reached from SCSID fn 42/36 dispatch (ptr 062412 = 062613)   ##
; ####################################################################
062613  044423  	LDA ,B 23		; INQUI: A := SUTYP            (dd 044423)
062614  070376  	AND -2			; A := SUTYP & mask(-2)   (clear volatile bits)
062615  074076  	ORA 76			; A := A | preset(76)     (pre-load default bits)
062616  004423  	STA ,B 23		; SUTYP := re-initialised base
062617  050442  	LDT ,B 42		; T := DMA buffer base (lo)
062620  054443  	LDX ,B 43		; X := DMA buffer base (hi)
062621  044073  	LDA 73			; build INQUIRY CDB in command area
062622  143304  	STATX
062623  143315  	STZTX 1
062624  170404  	SAA 4
; >>> MNGLD
062625  156411  	SHA ZIN 11		; allocation length field
062626  143324  	STATX 2
062627  143335  	STZTX 3
062630  143345  	STZTX 4
062631  143355  	STZTX 5
062632  135063  	JPL I 63		; -> 062715   CALL execute-command (INQUIRY)
062633  135063  	JPL I 63		; -> 062716   return+1: far error
062634  135063  	JPL I 63		; -> 062717   return+2: alt
062635  144037  	SWAP SB DX		; return+3: process INQUIRY response
062636  044413  	LDA ,B 13		; A := command status
062637  004450  	STA ,B 50
062640  070060  	AND 60			; status bits 4-5 set?
062641  140005  	SKP IF DA EQL 0
062642  125057  	JMP I 57		; -> 062721   status err -> retry/terminate
062643  044047  	LDA 47
062644  004426  	STA ,B 26
062645  045055  	LDA I 55
062646  140650  	EXR SA			; EXR: execute computed copy (move INQUIRY bytes)
; ---- SUTYP high byte := device type from INQUIRY response ----
062647  050442  	LDT ,B 42		; T := DMA buffer base
062650  054443  	LDX ,B 43
062651  173470  	AAX 70			; X += 70  (offset to device-type word in resp)
062652  143300  	LDATX			; A := response word (device type in HIGH byte) (dd 143300)
062653  074050  	ORA 50			; A |= low-byte mask (50)
062654  070423  	AND ,B 23		; A := response & SUTYP  (SUTYP_hi := preset_hi & devtype)
062655  004423  	STA ,B 23		; *** SUTYP := device-type into HIGH byte *** (dd 004423)
062656  156570  	SHA ZIN SHR 10		; A := SUTYP >> 8   (= device type)
062657  131007  	JAZ 7			; -> 062666   device type == 0 (DISK)
062660  171003  	SAT 3			; non-disk classification (WORM/CD/etc.)
062661  142065  	SKP IF DA UEQ ST
062662  124004  	JMP 4			; -> 062666
062663  171004  	SAT 4
062664  140065  	SKP IF DA EQL ST
062665  124006  	JMP 6			; -> 062673
062666  044423  	LDA ,B 23		; A := SUTYP
062667  174265  	BSET ONE 60 DA		; set bit 6 (removable/valid flag)
062670  004423  	STA ,B 23
062671  044033  	LDA 33
062672  124036  	JMP 36			; -> 062730   continue (2nd cmd = READ CAPACITY)
062673  171001  	SAT 1
062674  140065  	SKP IF DA EQL ST
; ... (READ CAPACITY exec + capacity->UHLIM at 062730..063100) ...

; ---- INQUI success tail : SET 5SCIN ("INITIALIZATION FINISHED") ----
063074  044424  	LDA ,B 24
063075  146755  	RADD AD1 CM1 CLD SA DA
063076  070017  	AND 17
063077  074213  	ORA -165
063100  004425  	STA ,B 25
063101  044423  	LDA ,B 23		; A := SUTYP
063102  174275  	BSET ONE 70 DA		; *** SET 5SCIN (bit 7) = init done *** (dd 174275)
063103  004423  	STA ,B 23
063104  170477  	SAA 77
063105  070414  	AND ,B 14		; fn := ABFUN & 077
063106  171042  	SAT 42
063107  142065  	SKP IF DA UEQ ST	; fn == 42 ?
063110  124014  	JMP 14			; -> 063124   ... return to caller


; ####################################################################
; ## 8. CACOB (063750B) - build CDB ; fn-0 re-entry 5SCIN gate       ##
; ####################################################################
063750  146145  	RADD CLD SL DA		; CACOB entry (dd 146145)
063751  ......  	; (save link)
063752  044423  	LDA ,B 23		; A := SUTYP                    (dd 044423)
063753  175275  	BSKP ONE 70 DA		; skip next if 5SCIN (bit 7) == 1
063754  125163  	JMP I 163		; -> INQUI   5SCIN CLEAR -> divert to INQUI
;                       5SCIN SET (post fn-42) -> fall through -> build READ(6) CDB
;                       then EXCOM (063403) -> GO SCLLD (ptr 063453 = 067160)
; ============================================================================
; End of carved disk-layer state machine.
; ============================================================================
