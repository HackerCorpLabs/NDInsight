# ND-500 level-12 MON handlers: which carved segment overlays them

**Question answered:** the L07 symbols `DVIO`, `GERRC`, `SWMC`, `A5XMSG/B5XMSG`,
`5MTRANS`, `5SIBMO`, `STAPROC`, `NSTOPROC` (all in virtual `140000-143500`, octal)
read as zero in `resident/SINTRAN-DATA_commoncode.bin` because they sit in the
`104000-170000` overlaid BSS gap. This document resolves them to the carved segment
that supplies the PIT overlay for that window and gives the real L disassembly.

## Verdict

**Winning segment: `S3MPIT`** (the memory-resident "MPIT" segment, `madr==0`, so not
carved directly), recovered byte-for-byte from its carved **Image / Save** copies:

| Role | Carved file | segle | Load addr (octal) | nd100-dis `-b` (dec) | Ghidra base (hex) |
|------|-------------|-------|-------------------|----------------------|-------------------|
| Image of MPIT | `../segments/026-S3IMPIT.bin` | 51 | `32000` | `13312` | `0x3400` |
| Save of MPIT  | `../segments/017-S3SMPIT.bin` | 51 | `32000` | `13312` | `0x3400` |

The Image and Save copies are **byte-identical** across the whole driver region
(`136000-144000` verified equal), exactly the pattern used for the other
`madr==0` segments (Extended Common in EXTRACTING-RESIDENT-CODE.md section 7.2,
S3IMAGE/S3SAVE in section 4). This is the PIT-overlay resolution called for by
section 7.3 step 2: `S3MPIT` covers `32000 .. 32000+51*2000 = 154000` and is the
segment whose live PIT maps into the `140000-143500` gap of the common image.

### Offset math

```
file_word_offset = symbol_addr(octal) - 32000(octal)      # = load address
file_byte_offset = file_word_offset * 2                    # .bin is big-endian words
```
Example DVIO: `141027 - 32000 = 107027(oct)` words = `36887(dec)` -> byte `73774`.

### Disassembly command

```bash
# byte-swap the big-endian carve to little-endian for nd100-dis
python3 -c "import sys;d=bytearray(open(sys.argv[1],'rb').read());d[0::2],d[1::2]=d[1::2],d[0::2];open(sys.argv[2],'wb').write(d)" \
  ../segments/026-S3IMPIT.bin /tmp/impit.le.bin
# base 32000 octal = 13312 decimal so printed addresses are the true virtual addresses
nd100-dis -a -o -S -b 13312 /tmp/impit.le.bin
```

## Why S3MPIT and not the other candidates

The load-address-aware "non-zero content" scan returns many candidates because the
`104000-170000` window is where several swappable segments map in. The discriminator
below is **semantic**, not "has bytes here".

### 1. Embedded call-pointer density (decisive)

The level-12 handlers reach shared routines through P-relative **indirect** pointer
words (`JPL I n`, `JMP I n`, `LDT I n`) whose target words hold fixed resident
addresses. Counting how many of those exact target addresses appear as data words in
`136000-144000` of each covering segment:

| Segment (load, octal) | pointer-word hits | breakdown |
|-----------------------|-------------------|-----------|
| **S3IMPIT / S3SMPIT (32000)** | **140** | 5MBBANK x29, NXTMSG x24, XACTRDY x17, WN5STATUS x17, GCPUDF x14, EMONICO x9, OKMONICO x8, SUNLOCK x8, SLOCK x6, MCCO x3, RTACT x3, 5ACTSWAP x2 |
| S3SM5 / S3SSM5 (40000) | 8 | NXTMSG x3, 5MBBANK x3, GCPUDF x2 |
| S3FS / S3SFS (26000)   | 5 | NXTMSG x5 |
| S3DMAC (64000)         | 0 | - |
| S3XMK, S3SPRMA, ...    | <=1 | noise |

Resolved target addresses used above (L07 `SYMBOL-2-LIST` / `SYMBOL-1-LIST`, octal):
`5MBBANK=4654`, `NXTMSG=135067`, `EMONICO=23021`, `OKMONICO=23025`, `XACTRDY=145466`,
`WN5STATUS=23670`, `MCCO=23044`, `SLOCK=23706`, `SUNLOCK=24041`, `5ACTSWAPPER=145162`,
`RTACT=13552`, `GCPUDF=23624`. Only S3MPIT carries the whole ND-500 driver linkage
table; every other segment is at noise level.

### 2. Instruction-level structural match to `NPL/MP-P2-N500.NPL`

nd100-dis decodes the ND-500 message micro-ops (`LDATX`, `LDDTX`, `LDXTX`, `STATX`,
`STDTX`) as mnemonics, so the NPL one-liners map directly onto the L bytes (the NPL is
a different revision - structure matches, exact bytes do not; L07 symbols sit a
uniform **+200 octal** above the NPL's own addresses):

- **GERRC** NPL `T:=5MBBANK; *5RECE@3 LDATX` -> L `141633 LDT I 50` then `141634 LDATX`.
- **SWMC** NPL `MSM510 SHZ 10=:D; T:=5MBBANK; *AAX TRAPN; LDATX` ->
  L `SAA 27 / SHA ZIN 10 / RADD..DD / LDT I 11 / AAX 16 / LDATX`.
- **STAPROC** NPL `T:=5MBBANK; *AAX NPROC; LDDTX` -> L `LDT I 156 / AAX 100 / LDDTX`.
- **A5XMSG** NPL `X=:B; T:=5MBBANK; *AAX N5XFU; LDATX; A/\X5MASK=:D` ->
  L `RADD..SX DB / LDT I 127 / AAX 101 / LDATX / AND 125 / RADD..DD`.
- **5MTRANS** NPL `X=:CMSGA; 0=:CUREL; A:=B=:XC5CPUDF; T:=5MBBANK; *AAX 5MNWA; LDDTX` ->
  L `STX I 142 / STZ I 142 / RADD SB DA / STA I 141 / LDT I 141 / AAX 100 / LDDTX`.

### 3. Elimination of the confident-looking decoys

- **S3DMAC (64000)** and **S3FS/S3SFS (26000)** both decode as *valid* ND-100 code in
  this window (low invalid-opcode density), which is why a "non-zero + coherent-ish"
  test flags them - but the code is **not** the ND-500 driver: 0 and 5 linkage hits
  respectively, and no `LDATX/LDDTX` message-decode shape. S3FS is file-system code.
- **S3SM5 / S3SSM5 (40000)** is the ND-500 32-bit System Monitor: 73 invalid ND-100
  opcodes + 24 `IOX` in the window - it is 32-bit ND-500 code / data, not ND-100
  level-12 handlers (confirmed the task's suspicion).
- **S3RFAC (26000)** does not even cover the full driver (`hi = 146000 < 147715`).

## Per-handler L disassembly (from `026-S3IMPIT.bin`, base `-b 13312`)

Addresses are the true virtual (octal) addresses. `I n` = P-relative indirect through
a pointer word; those pointer words hold the resident routine addresses listed above.

### STAPROC = 140356  (Start / switch process)
NPL: `T:=5MBBANK; *AAX NPROC; LDDTX; IF A<=5SWPROC OR A>>MX5PROCS GO ILPROC ...`
```
140356  051156  LDT I 156        ; T := 5MBBANK
140357  173500  AAX 100          ; AAX NPROC
140360  143302  LDDTX            ; A=proc.no, D=magno  (read message)
140361  051154  LDT I 154
140362  143056  SKP IF DT LST SA ; A <= 5SWPROC ?
140363  124003  JMP 3            ; -> 140366
140364  051152  LDT I 152
140365  141456  SKP IF DT MGRE SA; A >> MX5PROCS ?
140366  124017  JMP 17           ; -> 140405 (GO ILPROC)
140367  065146  SUB I 146
140370  120147  MPY 147
140371  060147  ADD 147          ; A-5SWPROC*5PRDSIZE+"S500S"
140372  146157  RADD CLD SA DX
140373  056007  LDX ,X 7         ; X := A.MESSBUFF
140374  051140  LDT I 140        ; T := 5MBBANK
140375  173775  AAX -3
140376  143300  LDATX            ; A := magno
140377  173547  AAX 147
140400  143301  LDXTX            ; X := process descriptor
140401  140015  SKP IF DA EQL SD
```

### NSTOPROC = 140511  (Stop process)
NPL: `CALL SLOCK; 0/\0; T:=5MBBANK; *AAX 5MSFL; LDATX; IF A BIT 55REP ...`
```
140511  135043  JPL I 43         ; CALL SLOCK   (ptr -> 023706)
140512  144400  RAND 0 0         ; 0/\0  (interlock marker)
140513  051021  LDT I 21         ; T := 5MBBANK
140514  173777  AAX -1
140515  143300  LDATX            ; A := 5MSFL (message flag)
140516  175375  BSKP ONE 170 DA  ; IF A BIT 55REP
140517  124010  JMP 10           ; -> 140527
140520  174175  BSET ZRO 170 DA  ; A BZERO 55REP (reset REP bit)
140521  143304  STATX            ; write flag back
140522  173401  AAX 1
140523  135034  JPL I 34         ; CALL SUNLOCK
140524  135026  JPL I 26         ; CALL OKMONICO (restart ND-500 proc)
140525  135017  JPL I 17         ; CALL XACTRDY
140526  124005  JMP 5            ; GO NXTMSG path
140527  135030  JPL I 30         ; CALL SUNLOCK
140530  170413  SAA 13           ; STOPPED
140531  055011  LDX I 11         ; X := N5MESSAGE
140532  135030  JPL I 30         ; CALL WN5STATUS
140533  125012  JMP I 12         ; GO NXTMSG   (-> 135067)
140534  004654  <data 004654>    ; 5MBBANK pointer constant
```

### DVIO = 141027  (Device I/O / NOUTSTR)
NPL: `CALL 5GTDF; GO NORMMC; A:=D; X:=N5MESSAGE; T:=5MBBANK; *AAX TODF; STATX ...`
```
141027  135063  JPL I 63         ; CALL 5GTDF  (get datafield if terminal)
141030  125063  JMP I 63         ; GO NORMMC
141031  146115  RADD CLD SD DA   ; A := D
141032  055062  LDX I 62         ; X := N5MESSAGE
141033  051062  LDT I 62         ; T := 5MBBANK
141034  173542  AAX 142          ; AAX TODF
141035  143304  STATX            ; store output-datafield ptr
141036  173740  AAX -40
141037  143302  LDDTX            ; read DNOBY (byte count)
141040  173676  AAX -102
141041  131404  JAF 4            ; -> 141045
141042  050054  LDT 54
141043  143416  SKP IF DT MLST SD; D >> 4000 ?
141044  124006  JMP 6            ; -> 141052
141045  170574  SAA 174          ; A := EC174 (error code)
141046  135051  JPL I 51         ; CALL EMONICO
141047  135051  JPL I 51         ; CALL XACTRDY
141050  125051  JMP I 51         ; GO NXTMSG
141051  124005  JMP 5            ; -> 141056
```

### GERRC = 141633  (Get error register from ND-500 trap context)
NPL: `T:=5MBBANK; *5RECE@3 LDATX; A-5SWPROC+1*REGBSZ+"ERREG"=:T ...`
```
141633  051050  LDT I 50         ; T := 5MBBANK
141634  143340  LDATX            ; A := 5RECE (received-trap register)
141635  065047  SUB I 47         ; A - 5SWPROC ...
141636  172401  AAA 1            ;   ... + 1
141637  120046  MPY 46           ;   * REGBSZ
141640  060046  ADD 46           ;   + "ERREG"
141641  146156  RADD CLD SA DT   ; =: T
141642  054045  LDX 45           ; "N500DF".CNTXPAGE + X.ADRZERO
141643  046057  LDA ,X 57
141644  062060  ADD ,X 60
141645  146151  RADD CLD SA DD   ; =: D
141646  146105  RADD CLD 0 DA
141647  156612  SAD ZIN 12       ; AD SHZ 12
141650  146061  RADD ST DD       ; D + T
```

### 5SIBMO = 141716  (Special MON call from SIBAS server in ND-500)
NPL: `T:=5MBBANK; *AAX SIBNO; LDDTX; IF A><0 OR D>>MXSIBAS OR T=0 THEN ...error`
```
141716  051101  LDT I 101        ; T := 5MBBANK
141717  173500  AAX 100          ; AAX SIBNO
141720  143302  LDDTX            ; D := SIBAS number
141721  131406  JAF 6            ; IF A><0  -> 141727 (error)
141722  051076  LDT I 76
141723  141416  SKP IF DT MGRE SD; D >> MXSIBAS ?
141724  124003  JMP 3            ; -> 141727 (error)
141725  140006  SKP IF DT EQL 0  ; T = 0 ?
141726  124006  JMP 6            ; -> 141734 (ok, legal SIBAS)
141727  055072  LDX I 72         ; X := N5MESSAGE
141730  170574  SAA 174          ; A := EC174
141731  135071  JPL I 71         ; CALL EMONICO (illegal SIBAS number)
141732  135071  JPL I 71         ; CALL XACTRDY
141733  125071  JMP I 71         ; GO NXTMSG
141734  146117  RADD CLD SD DX   ; X := D (=CSIBNO)
141735  014357  STX -21          ; =: CSIBNO
141736  057067  LDX I ,X 67      ; SIBBDEVS(X)
141737  146171  RADD CLD SX DD
141740  014355  STX -23
141741  046001  LDA ,X 1
```

### SWMC = 142153  (SWap Monitor Call -> activate swapper)
NPL: `MSM510 SHZ 10=:D; T:=5MBBANK; *AAX TRAPN; LDATX; ... CALL 5ACTSWAPPER`
```
142153  170427  SAA 27           ; MSM510 (mask constant)
142154  156410  SHA ZIN 10       ; SHZ 10
142155  146151  RADD CLD SA DD   ; =: D
142156  051011  LDT I 11         ; T := 5MBBANK
142157  173416  AAX 16           ; AAX TRAPN
142160  143300  LDATX            ; A := trap number
142161  070007  AND 7
142162  146015  RADD SD DA
142163  143304  STATX            ; write trap word back to message
142164  173762  AAX -16
142165  135004  JPL I 4          ; CALL 5ACTSWAPPER (ptr at 142171 = 145162)
142166  125004  JMP I 4          ; GO NXTMSG        (ptr at 142172 = 135067)
142167  004654  <data 004654>    ; 5MBBANK pointer
142170  000377  <data 000377>
142171  145162  <data 145162>    ; 5ACTSWAPPER
142172  135067  <data 135067>    ; NXTMSG
```
This is the cleanest single confirmation: SWMC is exactly `read TRAPN via 5MBBANK ->
CALL 5ACTSWAPPER -> GO NXTMSG`, with the resident addresses `145162` and `135067`
sitting inline as the indirect pointer words the two branches use.

### A5XMSG / B5XMSG = 142253  (ND-500 XMSG bridge)
Both labels share one entry. The `XMRETMASK` writeback-mask array occupies the words
just below the entry. Its L bytes at `142173..142252` decode as
`16, 0, 4, 0, 34, 0, 20, 20, 0, 14, 2, 0, 0, 74, 34, 4, ...` - a **byte-exact** match
to the NPL `INTEGER ARRAY XMRETMASK:=(16,0,4,0,34,0,20,20,0,14,2,0,0,74,34,4,...)`,
independent proof this is the ND-500 driver and that the segment is aligned correctly.
NPL: `X=:B; T:=5MBBANK; *AAX N5XFU; LDATX; A/\X5MASK=:D; IF A-X5MAXF>0 GO X5EILF ...`
```
142253  146173  RADD CLD SX DB   ; X =: B
142254  051127  LDT I 127        ; T := 5MBBANK
142255  173501  AAX 101          ; AAX N5XFU
142256  143300  LDATX            ; A := xmsg function word
142257  070125  AND 125          ; A /\ X5MASK
142260  146151  RADD CLD SA DD   ; =: D  (save xmsg function)
142261  172721  AAA -57          ; A - X5MAXF
142262  141050  SKP IF 0 GRE SA
142263  124135  JMP 135          ; GO X5EILF (illegal function) -> 142420
142264  173647  AAX -131
142265  143300  LDATX            ; XTBLK  (xtblock allocated?)
142266  131426  JAF 26           ; -> 142314
142267  170401  SAA 1
142270  142015  SKP IF DA UEQ SD
142271  125114  JMP I 114        ; GO FAR X5LEAVE (-> 142405)
142272  146137  RADD CLD SB DX   ; X := B
142273  173540  AAX 140          ; AAX HBUFA
142274  143300  LDATX
142275  171043  SAT 43
142276  146104  RADD CLD 0 DL
```

### 5MTRANS = 143445  (ND-500 memory / disk transfer request)
NPL: `X=:CMSGA; 0=:CUREL; A:=B=:XC5CPUDF; T:=5MBBANK; *AAX 5MNWA; LDDTX; AD=:NWFUNC ...`
```
143445  015142  STX I 142        ; X =: CMSGA (current message ptr)
143446  001142  STZ I 142        ; 0 =: CUREL
143447  146135  RADD CLD SB DA   ; A := B
143450  005141  STA I 141        ; =: XC5CPUDF
143451  051141  LDT I 141        ; T := 5MBBANK
143452  173500  AAX 100          ; AAX 5MNWA
143453  143302  LDDTX            ; read 5MNWA (function/word)
143454  173700  AAX -100
143455  021136  STD I 136        ; AD =: NWFUNC
143456  045136  LDA I 136
143457  175205  BSKP ONE 0 DA    ; IF 5MFNC NBIT 5DTRANS
143460  125135  JMP I 135        ; GO FAR CHEVENT (-> 143615)
143461  173510  AAX 110          ; AAX 5MLGN
143462  143300  LDATX            ; A := logical device number
143463  135133  JPL I 133        ; CALL LOGPH  (-> 143616)
143464  146157  RADD CLD SA DX   ; A =: X
143465  050132  LDT 132
143466  141467  SKP IF DX MGRE ST; X >>= "9BBHD" (hard-disk range check)
143467  124024  JMP 24           ; -> 143513
143470  050130  LDT 130
```

## One-line verdict per handler (all in S3MPIT via `026-S3IMPIT.bin`, base 13312)

| Handler | Addr (octal) | Verdict |
|---------|--------------|---------|
| STAPROC  | 140356 | CONFIRMED - `LDT 5MBBANK / AAX NPROC / LDDTX`, legal-proc checks, matches NPL start/switch process |
| NSTOPROC | 140511 | CONFIRMED - `CALL SLOCK / 0/\0 / LDT 5MBBANK / AAX 5MSFL / LDATX`, REP-bit test + WN5STATUS(STOPPED) |
| DVIO     | 141027 | CONFIRMED - `CALL 5GTDF / GO NORMMC`, TODF/DNOBY message reads, EC174 error path |
| GERRC    | 141633 | CONFIRMED - `LDT 5MBBANK / LDATX 5RECE`, reads trap ERREG from ND-500 context |
| 5SIBMO   | 141716 | CONFIRMED - `LDT 5MBBANK / AAX SIBNO / LDDTX`, SIBAS-number validation then EMONICO |
| SWMC     | 142153 | CONFIRMED - `MSM510 SHZ 10 =:D / LDT 5MBBANK / AAX TRAPN / LDATX` into swap-activate (5ACTSWAPPER) |
| A5XMSG/B5XMSG | 142253 | CONFIRMED - shared entry, `X=:B / LDT 5MBBANK / AAX N5XFU / LDATX / AND X5MASK`, XMRETMASK array below |
| 5MTRANS  | 143445 | CONFIRMED - `X=:CMSGA / A:=B / LDT 5MBBANK / AAX 5MNWA / LDDTX`, disk/mem transfer dispatch |

**Confidence: HIGH.** Not a forced positive: the linkage-pointer density (140 vs <=8),
the `LDATX/LDDTX` message-decode shape, and the per-handler NPL structural match all
point to the same single segment. Save and Image copies are byte-identical, so both
`026-S3IMPIT.bin` and `017-S3SMPIT.bin` are valid sources.
