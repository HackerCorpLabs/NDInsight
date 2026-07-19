; ===========================================================================
; COLDE-CONNECT.ASM  -  byte-verified carve, SINTRAN III VSX/500 L07
; Segment 006-S3FS, load base 26000B (big-endian .bin, addresses/values OCTAL)
; sha256(006-S3FS.bin)=b4a563d0715a6304a35167ec5ff0090c92b0b99dd743aef159ac6b182e080585
; Byte offset (decimal) of any addr A = (A_oct - 26000B)*2  (16-bit words).
;
; PURPOSE: carve the cold-enter/device-connect path of @ENTER-DIRECTORY and
;          locate where (if anywhere in FILSYS) the block-0 read is gated.
;
; TOP-LINE RESULT (see README): COLDE issues NO device transfer. The block-0
; read is issued by CHDSI -> RXDIR -> RCBLO -> per-device driver vector, as a
; single transfer with no FILSYS retry. The read-vs-fn42-init decision is NOT
; in this segment; it is in the SCSI disk driver (065-S3SIPIT).
;
; Every JPL I / JMP I below is INDIRECT through a P-relative pointer word.
; The disassembler's "-> NNNNNN" is the pointer-word address; the real target
; is the CONTENTS of that word. Contents resolved against FILSYS-SYMBOLS:
;   [132236]=003752 resident-setup     [132241]=044777 CLPAR
;   [132242]=132364 local              [132245]=047536 GNAMI
;   [132251]=050124 GDIRT              [132256]=050223 GNAMT
;   [132540]=004735 resident-helper    [132541]=047536 GNAMI
;   [132542]=030235 GNAMA
;   [140236]=132072 COLDE              [140241]=131732 GDIRE
;   [140435]=037763 CHDSI              [140234]=044777 CLPAR
;   [040143]=037643 RXDIR              [037675]=035766 RCBLO
; ===========================================================================


; --------------------------------------------------------------------------
; ENTER-DIRECTORY top: ENDIR = 140176B  (calls COLDE, then GDIRE, then CHDSI)
; --------------------------------------------------------------------------
>>> ENDIR (140176B)
140176  021030  	STD I 30			; save link/B  (prologue)
140177  146145  	RADD CLD SL DA
140200  146131  	RADD CLD SB DD
140201  170027  	SAB 27				; B := frame (27B locals)
140202  135025  	JPL I 25	; ->[140227]=003752  resident enter/setup
140205  044025  	LDA 25
140206  054025  	LDX 25
140207  146037  	RADD SB DX
140210  171020  	SAT 20
140211  135023  	JPL I 23	; ->[140234]=044777  CLPAR   (clear params)
140213  135023  	JPL I 23	; ->[140236]=132072  COLDE   <== cold-enter call
140214  125021  	JMP I 21	; ->[140235]  COLDE error-return arm
140215  146151  	RADD CLD SA DD			; COLDE success continues here
140216  045021  	LDA I 21
140217  135021  	JPL I 21	; ->[140240]  resident helper (010500B)
140220  146115  	RADD CLD SD DA
140221  010420  	STT ,B 20
140222  135017  	JPL I 17	; ->[140241]=131732  GDIRE  (get dir entry)
140223  125017  	JMP I 17	; ->[140242]  GDIRE error arm
; ... directory-object bookkeeping ...
140244  135141  	JPL I 141	; ->[140405]
140252  153124  	MON 124				; PRSRV (124B)  <- the ONE MON call
140253  130003  	JAP 3		; -> 140256      (JAP proves PRSRV not the abort)
; ... more processing ...
140402  135033  	JPL I 33	; ->[140435]=037763  CHDSI  <== block-0 read path
140403  125005  	JMP I 5		; ->[140410]     CHDSI error arm


; --------------------------------------------------------------------------
; XCOLD / DCOLD / COLDE  (the read/write-variant split, same idiom as RDISK/WDISK)
;   XCOLD=132060B  DCOLD=132070B  COLDE=132072B
;   DCOLD: BSET ONE SSK  (write/exclusive)   COLDE: BSET ZRO SSK  (read)
;   both fall into the shared body at 132073.
; --------------------------------------------------------------------------
>>> XCOLD (132060B)
132060  021155  	STD I 155
132061  146145  	RADD CLD SL DA
132062  146131  	RADD CLD SB DD
132063  170035  	SAB 35				; B := frame (35B locals)
132064  135152  	JPL I 152	; ->[132236]=003752  resident enter/setup
132065  171001  	SAT 1
132066  010417  	STT ,B 17			; local 17 := 1
132067  124020  	JMP 20		; -> 132107

>>> DCOLD (132070B)
132070  174220  	BSET ONE SSK			; SSK := 1  (write variant)
132071  124002  	JMP 2		; -> 132073

>>> COLDE (132072B)   [dd off=69748: F8 10]   [132073 off=69750: 22 62]
132072  174020  	BSET ZRO SSK			; SSK := 0  (READ variant)
132073  021142  	STD I 142			; save link/B
132074  146145  	RADD CLD SL DA
132075  146131  	RADD CLD SB DD
132076  170035  	SAB 35				; B := frame (35B locals)
132077  135137  	JPL I 137	; ->[132236]=003752  resident enter/setup
132100  175220  	BSKP ONE SSK			; test read/write flag
132101  124004  	JMP 4		; -> 132105
132102  170401  	SAA 1
132103  004417  	STA ,B 17			; write: local 17 := 1
132104  124002  	JMP 2		; -> 132106
132105  000417  	STZ ,B 17			; read:  local 17 := 0
132106  044131  	LDA 131
132107  054131  	LDX 131				; (XCOLD join point)
132110  146037  	RADD SB DX
132111  171020  	SAT 20
132112  135127  	JPL I 127	; ->[132241]=044777  CLPAR
132113  125127  	JMP I 127	; ->[132242]=132364  local
; --- directory-tree / name-match walk (NO device transfer anywhere below) ---
132125  010433  	STT ,B 33
132133  044421  	LDA ,B 21			; loop over directory slots
132141  135110  	JPL I 110	; ->[132251]=050124  GDIRT  (get dir-entry)
132151  135100  	JPL I 100	; ->[132251]=050124  GDIRT
132162  124351  	JMP -27		; -> 132133      loop back
132171  135065  	JPL I 65	; ->[132256]=050223  GNAMT  (name table)
132200  135041  	JPL I 41	; ->[132241]=044777  CLPAR
132214  135025  	JPL I 25	; ->[132241]=044777  CLPAR
; ... continues; local sub-blocks at 132364, 132401 ...
132402  135136  	JPL I 136	; ->[132540]=004735  resident helper
132403  135136  	JPL I 136	; ->[132541]=047536  GNAMI  (name init)
132406  135134  	JPL I 134	; ->[132542]=030235  GNAMA  (get name)
132445  135103  	JPL I 103	; ->[132550]=044777  CLPAR
; COLDE call-set = {003752, 004735, CLPAR, GDIRT, GNAMI, GNAMA, GNAMT}
; -> pure directory/name processing.  NO RCBLO / RXDIR / driver dispatch.


; ===========================================================================
; THE ACTUAL BLOCK-0 READ PATH:  CHDSI -> RXDIR -> RCBLO -> driver
; ===========================================================================

; --------------------------------------------------------------------------
; CHDSI = 037763B  "check disk info" -- reads directory block 0 via RXDIR.
; --------------------------------------------------------------------------
>>> CHDSI (037763B)
037763  021154  	STD I 154
037764  146145  	RADD CLD SL DA
037765  146131  	RADD CLD SB DD
037766  170020  	SAB 20				; B := frame (20B locals)
037767  135151  	JPL I 151	; ->[040140]=003752  resident enter/setup
037772  004416  	STA ,B 16			; local 16 := setup result
037773  175105  	BSKP ZRO 100 DA			; test bit-6 of setup result
037774  124135  	JMP 135		; -> 040131      (bit set -> error exit)
040000  135143  	JPL I 143	; ->[040143]=037643  RXDIR  <== READ BLOCK 0
040001  124134  	JMP 134		; -> 040135      RXDIR error arm
; --- after the read: validate directory magic in the loaded block ---
040015  173770  	AAX -10
040016  052000  	LDT ,X 0
040017  140065  	SKP IF DA EQL ST		; compare directory header word
040020  124043  	JMP 43		; -> 040063      mismatch -> init/no-retry arm
040021  131042  	JAZ 42		; -> 040063      zero -> init/no-retry arm
; NB: 040063 arm re-initialises the descriptor; it does NOT re-issue RXDIR.

; --------------------------------------------------------------------------
; RXDIR = 037643B  "read directory block 0"  (block := 0, then RCBLO)
; --------------------------------------------------------------------------
>>> RXDIR (037643B)
037643  021030  	STD I 30
037644  146145  	RADD CLD SL DA
037645  146131  	RADD CLD SB DD
037646  170007  	SAB 7				; B := frame (7B locals)
037647  135025  	JPL I 25	; ->[037674]=003752  resident enter/setup
037650  146105  	RADD CLD 0 DA			; A := 0  \  block number
037651  146101  	RADD CLD 0 DD			; D := 0  /  := 0  (BLOCK 0)
037652  135023  	JPL I 23	; ->[037675]=035766  RCBLO  <== issue transfer
037653  124016  	JMP 16		; -> 037671      RCBLO error arm
037666  040404  	MIN ,B 4			; success bookkeeping
037670  125011  	JMP I 11	; ->[037701]     return

; --------------------------------------------------------------------------
; RCBLO = 035766B  "read (cache) block" -- builds transfer, dispatches driver.
; The device driver is called INDIRECTLY via  JPL I ,B 10, where local 10
; was loaded from word 14 of the directory/disk-info descriptor (returned by
; GDIRT).  Exactly one dispatch per call; NO retry loop on success.
; --------------------------------------------------------------------------
>>> RCBLO (035766B)
035766  021066  	STD I 66
035767  146145  	RADD CLD SL DA
035770  146131  	RADD CLD SB DD
035771  170017  	SAB 17				; B := frame (17B locals)
035772  135063  	JPL I 63	; ->[036055]=003752  resident enter/setup
035773  135063  	JPL I 63	; ->[036056]=037101  resident (page/buffer)
035774  125063  	JMP I 63	; ->[036057]=036205  error arm
035777  135062  	JPL I 62	; ->[036061]=050124  GDIRT  (get descriptor -> X)
036005  014411  	STX ,B 11			; local 11 := descriptor pointer
; ...
036100  054411  	LDX ,B 11			; X := descriptor
036101  046014  	LDA ,X 14			; A := descriptor[14]  (driver vector / ABFUN)
036102  004410  	STA ,B 10			; local 10 := driver dispatch vector
; ... build DMA/seek params, select read/write/seek branch ...
036135  135410  	JPL I ,B 10	; -> driver   [dd off=8378: BB 08]  <== DEVICE XFER
036136  124041  	JMP 41		; -> 036177      (return on success; NO re-issue)
036142  135410  	JPL I ,B 10	; -> driver   (alternate variant branch)
036167  135410  	JPL I ,B 10	; -> driver   (alternate variant branch)
036174  135410  	JPL I ,B 10	; -> driver   (alternate variant branch)
; The four JPL I ,B 10 sites are mutually-exclusive branch variants
; (read / write / seek); on any given call exactly ONE fires, once.
; ===========================================================================
