# BDIO / DOMINO nucleus block-I/O driver - L07 byte-verified carve

**Date:** 2026-07-19
**Version:** SINTRAN III L-VSX-500 (running system), symbols L07.
**Ground truth:** carved bytes of `../../segments/017-S3SMPIT.bin`
(byte-identical to `026-S3IMPIT.bin`, `cmp` = 0 diffs), load base `032000B`.
**NPL logic reference (DIFFERENT revision, never authoritative bytes):**
`SINTRAN/NPL-SOURCE/NPL/MP-P2-DISK-START.NPL` (BDMTR/BDMFU/MBUILD/DCNVA/
SSBDI/BDTRANS/STRBDIO/REBDIO, NPL-listing addresses 073246-074243) and
`DP-P2-VARIABLES.NPL` (5DSKC, MTNKDF).

**Evidence convention (per claim):**
- `[V]`   = byte-verified HERE (carved word reproduced, dd where listed)
- `[NPL-V]` = matches NPL logic, bytes consistent, but the claim itself
  (a name/meaning) comes from the NPL revision
- `[I]`   = inferred, not proven from bytes
- `[OPEN]` = unresolved; what was tried is recorded

**Symbol sources (5-char truncation, L07):**
- `SYMBOL-2-LIST.SYMB.TXT`: routine addresses `BDMTR=073454 BDMFU=073565
  CCM14=073630 MBUIL=073700 DCNVA=073750 SSBDI=074000 BDTRA=074012
  BD12T=074024 STRBD=074072 REBDI=074246 MPUTP=074467 (next symbol)
  BDTMU=075326 BDTMV=075356 GETFR=073121 PUTFR=073150 GETSW=073111
  ST12L=027027 MONEN=032000 STUPR=032034 RWAIT=032043 WT=032571
  GETOU=026656 PUTIN=026670 TO12Q=026725 DOMDF=041064 CLUST=041574
  BBASE=041576 N500D=051767 QP100=033315 5DSKC=011171 GAPFD=034006`
- `SYMBOL-1-LIST.SYMB.TXT`: `WT12=033616 NKSEN=042171 NKREC=043076
  NKREA=043375 NKWRI=043411 9FLEX=012074 RTACT=013552 BRESE=010563
  BRELE=010610 WDATA=003527 RDATA=003535 ERRFA=000215 GAPFU=000744
  RTREF=004007 CURPR=004010 SSECN=007271 PDVNO=007267 SINEC=007266
  STUPR/...` plus all datafield offsets used below (`HSTAT=10 NFUNC=6
  TRLRE=7 TYPCO=11 DMSID=13 ABFUN=14 MEMAD=15 ABPA2=17 ABP31=21 ARESZ=23
  DIBRE=25 RTRES=1 SSSTA=10 TYPRI=3 RTDLG=25 DAREG=3 ALEVB=10 LV12B=140
  DODMA=13 OWN=5 ADRZE=60 RSFLA=14 PLDNO=15 DRPRT=16 DLPRT=17 DIPO1=17
  DIPO2=20 DIPOO=17 OPAIX=21 OPAIN=40 5NOAB=7 STDRI=177776 TMSUB=177772
  TMR=177773 TTMR=177774 DSVER=20 DSOW1=22 DSOW2=23 DSCA1=26 DSTSD=26
  DSSTS=30 DSCMD=32 DXPOO=34 DSQC1=36 DSQCN=36 DSQC2=37 DSTBL=42
  DMYAD=44 DNRPG=50 DMSID=13`).

---

## 1. Overlay resolution proof  [V]

All big resident overlays span the same virtual window, so `074072B`
"disassembling to something" proves nothing (carving trap 4). Discriminator =
sibling coherence across the whole SUBR family, plus every P-relative literal
resolving to an L07 symbol value.

Candidates tested at base `032000B`: `017-S3SMPIT` and `065-S3SIPIT`
(the SYMBOL-2 driver overlay of the SMD/SCSI carve). Result:

- In **017-S3SMPIT** ALL of `BDMTR 073454 / BDMFU 073565 / MBUIL 073700 /
  DCNVA 073750 / SSBDI 074000 / BDTRA 074012 / BD12T 074024 / STRBD 074072 /
  REBDI 074246` land on routine entries whose bodies reproduce the NPL logic
  instruction for instruction (sections 3-8), and **every** literal-pool word
  resolves to a known L07 symbol: `NKWRI 043411` at 074241, `NKSEN 042171`
  at 074243, `NKREC 043076` at 074445, `NKREA 043375` at 074450,
  `WT12 033616` at 074245/074465/074011, `9FLEX 012074` at 074233/074455,
  `BDTMU 075326` at 074227/074462, `BDTMV 075356` at 074451,
  `DOMDF 041064` at 073743/074242/074244(-2)/074463, `DSVER+DOMDF 041104`
  at 074240, `CLUSTER 041574` at 074236/074443, `BBASE 041576` at
  074237/074444, `N500D 051767` at 073776, `SSECN 007271` at 074226/074446,
  `PDVNO 007267` at 074230/074452, `SINEC 007266` at 074232/074454,
  `GETFR 073121` at 073547, `PUTFR 073150` at 073672, `ERRFA 000215` at
  073550/073674, `BRESE 010563` at 073551, `GAPFU 000744` at 073552,
  `GAPFD 034006` at 073553, `WDATA 003527` at 073555, `RWAIT 032043` at
  073556, `MONEN 032000` at 073557, `WT 032571` at 073560, `RTREF 004007`
  at 073545, `QP100 033315` at 073546/073671, `GETOU 026656` at 073665,
  `RDATA 003535` at 073666, `BRELE 010610` at 073667, `CURPR 004010` at
  073673, `STUPR 032034` at 073675, `PUTIN 026670` at 074007,
  `RTACT 013552` at 074010, `ST12L 027027` at 074032, `BD12T 074024` at
  074031, `REBDI 074246` at 074244, `TO12Q 026725` at 074464.
  Zero mismatches over 40+ independent symbol hits.
- In **065-S3SIPIT** the same addresses decode to unrelated bytes (074072 =
  `156403 SHA ZIN 3`, then stray `IOX 505` etc.) - wrong overlay.
- Bonus coherence: the four nucleus gates called from here live in the SAME
  overlay window and share an identical prologue (`174000 BSET ZRO SSPTM;
  STA I ..; SWAP SA DB; STA ,B 2; A:=L; STA ,B 3`) at `NKSEN 042171`,
  `NKREC 043076`, `NKREA 043375`, `NKWRI 043411`.  [V]

**Conclusion: the BDIO/DOMINO driver lives in `017-S3SMPIT`
(= `026-S3IMPIT`), load base `032000B`, symbols from SYMBOL-2-LIST (routines)
+ SYMBOL-1-LIST (offsets/nucleus).**  [V]

### dd byte proof (reproduced before publishing)

`017-S3SMPIT.bin` is big-endian; byte offset = `(addr_dec - 13312) * 2`.

| addr (oct) | byte off (dec) | word (oct) | bytes | what |
|---|---|---|---|---|
| 073454 | 34392 | 004373 | `08 fb` | BDMTR entry `STA -5` |
| 073750 | 34768 | 020376 | `20 fe` | DCNVA entry `STD -2` |
| 073777 | 34814 | 124012 | `a8 0a` | the `A:=124012` self-mod constant |
| 074000 | 34816 | 135413 | `bb 0b` | SSBDI `JPL I ,B 13` = CALL DODMA |
| 074024 | 34856 | 171005 | `f2 05` | BD12T `SAT 5` |
| 074072 | 34932 | 146135 | `cc 5d` | STRBD entry `A:=B` |
| 074152 | 35028 | 170567 | `f1 77` | `SAA 167` = BDIO WRITE fn 167B |
| 074153 | 35030 | 171070 | `f2 38` | `SAT 70` = message size 70B |
| 074235 | 35130 | 000213 | `00 8b` | literal 213B = COMPARE fn |
| 074241 | 35138 | 043411 | `47 09` | literal = NKWRI |
| 074245 | 35146 | 033616 | `37 8e` | literal = WT12 |
| 074246 | 35148 | 146173 | `cc 7b` | REBDI entry `X=:B` |
| 074456 | 35420 | 104031 104651 104622 | `88 19 89 a9 89 92` | the 3 benign statuses |

Reproduce e.g.:
```
dd if=017-S3SMPIT.bin bs=1 skip=34932 count=2 | od -An -tx1   # -> cc 5d
```
Disassembly pipeline (WSL): byte-swap then
`nd100-dis -a -o -b 13312 <swapped>`.

---

## 2. Datafields and the BDIO message record  [V]

Four datafields are in play (offsets collide across them - do NOT mix):

- **Queue element DF** (`B` on monitor level, `X` in STRBDIO):
  `RTRES=1, TYPRING=3, NFUNC=6, TRLREG=7, SSSTAT/HSTAT=10, TYPCO=11,
  DMSID=13, ABFUN=14, MEMAD=15-16 (double), ABPA2=17-20 (double),
  ABP31=21`.  `HSTAT` (+10) receives the final driver status.  [V]
- **Pool DF** (`PDF`; the DOMINO device/pool datafield):
  `RSFLA=14, PLDNO=15, DRPRT=16, DIPO1=17, DIPO2=20 (double DIPOO),
  OPAIX=21-22 (double), ARESZ=23-24 (double), DSTSD=26 (last nucleus/BDIO
  error), TMSUB=-6, TMR=-5, TTMR=-4` (timer-retry cells).  [V]
- **Controller DF**: `STDRIV=-2` (level-12 driver entry), `DIBRE=25`
  (input-queue head), `DODMA=13` (the DODMA vector slot).  [V]
- **DOMDF = 041064** [SYMBOL-2] - the global DOMINO message datafield.
  `NFUNC=+6` (level-12 continuation), `DLPRT=+17` (local nucleus port).
  The **BDIO message record** starts at `DSVER = DOMDF+20 = 041104B`
  (this exact sum is the literal at 074240).  [V]

### BDIO message record (word-by-word, as initialized by the carve)

Offsets octal. "msg rel" = offset from the message start (`DSVER`), which is
what NKWRI/NKREA transfer.

| DOMDF+ | msg rel | field | set by (carved addr) | value |
|---|---|---|---|---|
| 20 | 0 | DSVER | (not written here) | version word, pre-set at init [OPEN] |
| 21 | 1 | - | (not written here) | [OPEN] |
| 22 | 2 | DSOW1 | MBUIL 073734-073735 | `1` = "abstrans message" marker [V] |
| 23 | 3 | DSOW2 | MBUIL 073732-073733 | queue-element DF address (owner) [V] |
| 26 | 6 | DSCA1 | MBUIL 073723-073724 | pool DF address (single word) [V] |
| 30-31 | 10-11 | DSSTS | MBUIL 073716-073720 | `-1,-1`; completion: `0,0`=OK else status in low word [V] |
| 32 | 12 | DSCMD | STRBD 074165 | `166B`=READ / `167B`=WRITE / `213B`=COMPARE [V] |
| 34-35 | 14-15 | DXPOO | MBUIL 073725-073727 (`LDD ,B 17` pool) | DIPOO double (pool index DIPO1,DIPO2) [V] |
| 36 | 16 | DSQC1 (=DSQCN hi) | MBUIL 073721 | 0; `-1,-1` on completion = moved to mirror pool / rejected marker [V] |
| 37 | 17 | DSQC2 | MBUIL 073722 | 0 [V] |
| 40-41 | 20-21 | OPAIN | MBUIL 073730-073731 (`LDD ,B 21` pool) | OPAIX double (area index) [V] |
| 42-43 | 22-23 | DSTBL | MBUIL 073710-073711 | ABPA2 double = media (disk block) address [V] |
| 44-45 | 24-25 | DMYAD | MBUIL 073706-073707 | DOMINO memory byte address, bit 31 set (from DCNVA) [V] |
| 50-51 | 30-31 | DNRPG | MBUIL 073712-073715 | `0 : ABP31` = number of pages [V] |
| 52..67 | 32..47 | - | not written by MBUIL/STRBD | [OPEN] carried as-is from DOMDF |

Sizes handed to the nucleus (STRBD 074146-074164, all [V]):

| ABFUN | BDIO function (DSCMD) | NKWRI size (words) |
|---|---|---|
| 61 (write) | `167B` | `70B` |
| 63 (compare) | `213B` (literal at 074235) | `70B` |
| 60 / 66 (read) | `166B` | `74B` |

Completion read-back (REBDI 074264-074272): `NKREA` with max size `76B`
words into the same buffer `DSVER+DOMDF`.  [V]

### Nucleus call conventions as used here (register truth from bytes)

- `NKWRI 043411` [SYMBOL-1]: `A` = buffer address (`DSVER+DOMDF`),
  `T` = message id (`DMSID`, queue-DF word 13), `X` = 0,
  `D` = word count (70B/74B). Returns `A` = status (0 = OK).  [V]
- `NKSEN 042171`: `A` = destination port (`DOMDF.DLPRT`),
  `D` = `PDF.DRPRT` (remote port), `X` = message id (`DMSID`).
  Returns `A` = status.  [V]
- `NKREC 043076`: `T` = local port (`DOMDF.DLPRT`). Returns `X` = message id
  (0 = none), `A` = nucleus status.  [V]
- `NKREA 043375`: `T` = message id, `X` = 0, `D` = max words (76B),
  `A` = buffer address. Returns `A` = status (0 = OK).  [V]
- Before every nucleus buffer operation the owner is stamped:
  `1BANK (BSET ZRO SSPTM); A:=M[CLUSTER]+14B; D:=DOMDF;
  AD =: BBASE.OWN (STD ,X 5); 2BANK (BSET ONE SSPTM)`.
  `CLUSTER=041574`, `BBASE=041576`, `OWN=+5`.  [V]

---

## 3. STRBDIO 074072B - annotated disassembly  [V]

Level-12 DODMA routine for BDIO. Entry (NPL header): `B` = pool DF,
`X` = queue DF, `T` = type. `PDF` local = 074033, `QUDF` local = 074034.

```
; ---- local error/return block (shared, lives BEFORE the entry) ----
074035  005171  STA I 171        ; ERET: SSECN := A        (ptr 074226 -> 007271 = SSECN)
074036  054375  LDX -3           ;   X := PDF              (074033)
074037  006026  STA ,X 26        ;   PDF.DSTSD := A        (save nucleus error)
074040  046014  LDA ,X 14        ;   A := PDF.RSFLA
074041  171001  SAT 1
074042  140065  SKP IF DA EQL ST ;   reserved (RSFLA=1) ?
074043  124007  JMP 7            ;     no -> 074052
074044  046373  LDA ,X -5        ;   A := PDF.TMR
074045  131405  JAF 5            ;   already in timer queue -> 074052
074046  046374  LDA ,X -4        ;   A := PDF.TTMR
074047  006373  STA ,X -5        ;   PDF.TMR := TTMR       (arm timer)
074050  044157  LDA 157          ;   A := BDTMU            (ptr 074227 = 075326)
074051  006372  STA ,X -6        ;   PDF.TMSUB := BDTMU    (timer retry hook)
074052  046015  LDA ,X 15        ;   A := PDF.PLDNO
074053  005155  STA I 155        ;   PDVNO := A            (074230 -> 007267)
074054  044155  LDA 155          ;   A := 1661             (074231 = 001661)
074055  005155  STA I 155        ;   SINEC := 1661         (074232 -> 007266)
074056  054401  LDX ,B 1         ;   X := QUDF.RTRES
074057  135154  JPL I 154        ;   CALL 9FLEX(SINEC,4)   (074233 = 012074)
074060  007266  (arg) SINEC ptr  ;     inline arg 1 = 007266
074061  000004  (arg) 4          ;     inline arg 2 = 4
074062  170776  SAA -2           ;   A := -2  (nucleus error)
074063  054351  LDX -27          ; RETU: X := QUDF         (074034)
074064  006010  STA ,X 10        ;   QUDF.HSTAT := A
074065  044346  LDA -32          ;   A := PDF
074066  146153  RADD CLD SA DB   ;   B := PDF
074067  046006  LDA ,X 6         ;   A := QUDF.NFUNC
074070  146154  RADD CLD SA DL   ;   L := NFUNC
074071  146142  EXIT             ;   immediate return

; ---- entry ----
074072  146135  RADD CLD SB DA   ; STRBDIO: A := B
074073  004340  STA -40          ;   PDF := B  (pool DF)   (074033)
074074  014340  STX -40          ;   QUDF := X (queue DF)  (074034)
074075  146145  RADD CLD SL DA   ;   A := L
074076  006006  STA ,X 6         ;   QUDF.NFUNC := L       (restart address)
074077  012011  STT ,X 11        ;   QUDF.TYPCO := T
074100  144037  SWAP SB DX       ;   X :=: B  (B=queue DF, X=pool DF)
074101  044414  LDA ,B 14        ;   A := ABFUN
074102  171024  SAT 24
074103  140065  SKP IF DA EQL ST ;   ABFUN = 24 (return status)?
074104  124003  JMP 3
074105  046026  LDA ,X 26        ;     A := PDF.DSTSD
074106  124355  JMP -23          ;     GO RETU
074107  171042  SAT 42
074110  140065  SKP IF DA EQL ST ;   ABFUN = 42 (read capacity/format)?
074111  124015  JMP 15           ;     no -> 074126
074112  024415  LDD ,B 15        ;     AD := MEMAD (caller buffer, phys)
074113  146156  RADD CLD SA DT   ;     T := A (bank)
074114  146117  RADD CLD SD DX   ;     X := D (offset)
074115  170442  SAA 42
074116  143304  STATX            ;     buffer[0] := 42     (phys store)
074117  146174  RADD CLD SX DL   ;     L := X
074120  054313  LDX -65          ;     X := PDF
074121  026023  LDD ,X 23        ;     AD := PDF.ARESZ     (area size, double)
074122  146147  RADD CLD SL DX   ;     X := L
074123  143316  STDTX            ;     buffer[+..] := ARESZ (phys, see drift #2)
074124  146105  RADD CLD 0 DA    ;     A := 0
074125  124336  JMP -42          ;     GO RETU (OK)
074126  171060  SAT 60           ;   read?
074127  142065  SKP IF DA UEQ ST
074130  124012  JMP 12           ;     yes -> 074142
074131  171061  SAT 61           ;   write?
074132  142065  SKP IF DA UEQ ST
074133  124007  JMP 7
074134  171063  SAT 63           ;   compare?
074135  142065  SKP IF DA UEQ ST
074136  124004  JMP 4
074137  171066  SAT 66           ;   read to cache-inhibited?
074140  140065  SKP IF DA EQL ST
074141  124026  JMP 26           ;     none of 60/61/63/66 -> 074167
074142  150401  IOF              ;   (global DOMDF in use from here)
074143  054270  LDX -110         ;   X := PDF
074144  135070  JPL I 70         ;   CALL MBUILD           (074234 = 073700)
074145  124316  JMP -62          ;     error return -> RETU (A=-3)
074146  044414  LDA ,B 14        ;   A := ABFUN
074147  171061  SAT 61
074150  140065  SKP IF DA EQL ST ;   write?
074151  124004  JMP 4
074152  170567  SAA 167          ;     A := 167B  BDIO WRITE function
074153  171070  SAT 70           ;     T := 70B   message size
074154  124011  JMP 11           ;     -> 074165
074155  171063  SAT 63
074156  140065  SKP IF DA EQL ST ;   compare?
074157  124004  JMP 4
074160  044055  LDA 55           ;     A := 213B  BDIO COMPARE (074235 = 000213)
074161  171070  SAT 70           ;     T := 70B
074162  124003  JMP 3
074163  170566  SAA 166          ;     A := 166B  BDIO READ
074164  171074  SAT 74           ;     T := 74B   message size
074165  006032  STA ,X 32        ;   DOMDF.DSCMD := function   (X=DOMDF after MBUILD)
074166  124003  JMP 3            ;   -> 074171
074167  170777  SAA -1           ;   illegal function
074170  124273  JMP -105         ;   GO RETU (A=-1)
074171  146171  RADD CLD SX DD   ;   D := X (=DOMDF)
074172  054044  LDX 44           ;   X := CLUSTER          (074236 = 041574)
074173  174000  BSET ZRO SSPTM   ;   1BANK
074174  046000  LDA ,X 0         ;   A := M[CLUSTER]
074175  172414  AAA 14           ;   A += 14B
074176  054041  LDX 41           ;   X := BBASE            (074237 = 041576)
074177  022005  STD ,X 5         ;   BBASE.OWN := (A,D)    set owner
074200  174200  BSET ONE SSPTM   ;   2BANK
074201  146161  RADD CLD ST DD   ;   D := T (message size 70B/74B)
074202  050413  LDT ,B 13        ;   T := QUDF.DMSID (message id)
074203  044035  LDA 35           ;   A := DSVER+DOMDF = 041104 (074240)
074204  146107  RADD CLD 0 DX    ;   X := 0
074205  135034  JPL I 34         ;   CALL NKWRI            (074241 = 043411)
074206  131627  JAF -151         ;   IF A<>0 GO ERET
074207  054224  LDX -154         ;   X := PDF
074210  046016  LDA ,X 16        ;   A := PDF.DRPRT
074211  146151  RADD CLD SA DD   ;   D := DRPRT
074212  054030  LDX 30           ;   X := DOMDF            (074242 = 041064)
074213  046017  LDA ,X 17        ;   A := DOMDF.DLPRT
074214  054413  LDX ,B 13        ;   X := DMSID
074215  135026  JPL I 26         ;   CALL NKSEN            (074243 = 042171)
074216  150402  ION
074217  131616  JAF -162         ;   IF A<>0 GO ERET
074220  044022  LDA 22           ;   A := DOMDF            (074242)
074221  146153  RADD CLD SA DB   ;   B := DOMDF
074222  044022  LDA 22           ;   A := REBDIO           (074244 = 074246)
074223  004406  STA ,B 6         ;   DOMDF.NFUNC := REBDIO
074224  135021  JPL I 21         ;   CALL WT12             (074245 = 033616)
074225  125017  JMP I 17         ;   GO REBDIO             (074244)
; ---- literal pool 074226-074245 (all resolved above) ----
```

## 4. REBDIO 074246B - completion, annotated  [V]

Entered from WT12 with `X` = DOMDF when nucleus message(s) arrived.

```
074246  146173  RADD CLD SX DB   ; REBDIO: B := X (=DOMDF)
074247  150401  IOF              ; DO:
074250  054173  LDX 173          ;   X := CLUSTER          (074443 = 041574)
074251  174000  BSET ZRO SSPTM   ;   1BANK
074252  046000  LDA ,X 0         ;   A := M[CLUSTER]
074253  172414  AAA 14           ;   A += 14B
074254  146131  RADD CLD SB DD   ;   D := B (=DOMDF)
074255  054167  LDX 167          ;   X := BBASE            (074444 = 041576)
074256  022005  STD ,X 5         ;   BBASE.OWN := (A,D)
074257  174200  BSET ONE SSPTM   ;   2BANK
074260  050417  LDT ,B 17        ;   T := DOMDF.DLPRT
074261  135164  JPL I 164        ;   CALL NKREC            (074445 = 043076)
074262  133160  JXZ 160          ;   WHILE X<>0; else -> 074442 (GO WT12)
074263  005163  STA I 163        ;   SSECN := A (nucleus status) (074446 -> 007271)
074264  146176  RADD CLD SX DT   ;   T := X (message id)
074265  146107  RADD CLD 0 DX    ;   X := 0
074266  170476  SAA 76
074267  146151  RADD CLD SA DD   ;   D := 76B (max words)
074270  044157  LDA 157          ;   A := DSVER (=20)      (074447 = 000020)
074271  146035  RADD SB DA       ;   A += B  (= DSVER+DOMDF buffer)
074272  135156  JPL I 156        ;   CALL NKREA            (074450 = 043375)
074273  150402  ION
074274  131545  JAF 145          ;   IF A<>0 -> next iteration (074441)
074275  054423  LDX ,B 23        ;   X := DSOW2 (waiting queue DF)
074276  024430  LDD ,B 30        ;   AD := DSSTS
074277  131440  JAF 40           ;   A<>0 -> error path 074337
074300  140001  SKP IF DD EQL 0
074301  124036  JMP 36           ;   D<>0 -> error path 074337
; -------- transfer OK --------
074302  024436  LDD ,B 36        ;   AD := DSQCN
074303  171377  SAT -1
074304  054426  LDX ,B 26        ;   X := DSCA1 (pool DF)
074305  140065  SKP IF DA EQL ST
074306  124024  JMP 24           ;   } if DSQCN = (-1,-1):
074307  140061  SKP IF DD EQL ST ;   }   OK but changed to mirror pool
074310  124022  JMP 22
074311  024434  LDD ,B 34        ;     AD := DXPOO
074312  052017  LDT ,X 17        ;     = PDF.DIPO1 ?
074313  140065  SKP IF DA EQL ST
074314  124016  JMP 16
074315  052020  LDT ,X 20        ;     = PDF.DIPO2 ?
074316  140061  SKP IF DD EQL ST
074317  124013  JMP 13
074320  046014  LDA ,X 14        ;     PDF.RSFLA = 1 ?
074321  171001  SAT 1
074322  140065  SKP IF DA EQL ST
074323  124007  JMP 7
074324  046373  LDA ,X -5        ;     PDF.TMR = 0 ?
074325  131405  JAF 5
074326  046374  LDA ,X -4        ;     PDF.TMR := PDF.TTMR
074327  006373  STA ,X -5
074330  044121  LDA 121          ;     A := BDTMV          (074451 = 075356)
074331  006372  STA ,X -6        ;     PDF.TMSUB := BDTMV  (timer reconnect)
074332  146105  RADD CLD 0 DA    ;   A := 0
074333  002026  STZ ,X 26        ;   PDF.DSTSD := 0
074334  054423  LDX ,B 23        ;   X := DSOW2
074335  006010  STA ,X 10        ;   QUDF.HSTAT := 0  (everything OK)
074336  124071  JMP 71           ;   -> deliver 074427
; -------- transfer failed --------
074337  171377  SAT -1
074340  054426  LDX ,B 26        ;   X := DSCA1 (pool DF)
074341  140065  SKP IF DA EQL ST ;   } if DSSTS = (-1,-1)
074342  124021  JMP 21           ;   }   (message never processed)
074343  140061  SKP IF DD EQL ST
074344  124017  JMP 17
074345  045101  LDA I 101        ;     A := SSECN          (074446 -> 007271)
074346  131015  JAZ 15           ;     and SSECN<>0 => REJECTED
074347  006026  STA ,X 26        ;       PDF.DSTSD := SSECN
074350  046015  LDA ,X 15        ;       A := PDF.PLDNO
074351  005101  STA I 101        ;       PDVNO := A        (074452 -> 007267)
074352  044101  LDA 101          ;       A := 1661         (074453 = 001661)
074353  005101  STA I 101        ;       SINEC := 1661     (074454 -> 007266)
074354  054423  LDX ,B 23        ;       X := DSOW2
074355  056001  LDX ,X 1         ;       X := X.RTRES
074356  135077  JPL I 77         ;       CALL 9FLEX(SINEC,4) (074455 = 012074)
074357  007266  (arg) SINEC ptr
074360  000004  (arg) 4
074361  170776  SAA -2           ;       A := -2 (nucleus error)
074362  124043  JMP 43           ;       -> HSTAT store 074425
074363  146115  RADD CLD SD DA   ;     else: A := D (BDIO status word)
074364  006026  STA ,X 26        ;       PDF.DSTSD := status
074365  005061  STA I 61         ;       SSECN := status   (074446)
074366  050070  LDT 70           ;       T := 104031       (074456)
074367  142065  SKP IF DA UEQ ST
074370  124034  JMP 34           ;       status = 104031 -> -5
074371  050066  LDT 66           ;       T := 104651       (074457)
074372  142065  SKP IF DA UEQ ST
074373  124031  JMP 31
074374  050064  LDT 64           ;       T := 104622       (074460)
074375  142065  SKP IF DA UEQ ST
074376  124026  JMP 26
074377  046015  LDA ,X 15        ;       real device error:
074400  005052  STA I 52         ;         PDVNO := PDF.PLDNO (074452)
074401  044060  LDA 60           ;         A := 1662       (074461 = 001662)
074402  005052  STA I 52         ;         SINEC := 1662   (074454)
074403  046014  LDA ,X 14        ;         RSFLA=1 and TMR=0 ?
074404  171001  SAT 1
074405  140065  SKP IF DA EQL ST
074406  124007  JMP 7
074407  046373  LDA ,X -5
074410  131405  JAF 5
074411  046374  LDA ,X -4        ;         PDF.TMR := TTMR
074412  006373  STA ,X -5
074413  044047  LDA 47           ;         A := BDTMU      (074462 = 075326)
074414  006372  STA ,X -6        ;         PDF.TMSUB := BDTMU
074415  054423  LDX ,B 23        ;         X := DSOW2
074416  056001  LDX ,X 1         ;         X := X.RTRES
074417  135036  JPL I 36         ;         CALL 9FLEX(SINEC,4) (074455)
074420  007266  (arg) SINEC ptr
074421  000004  (arg) 4
074422  170774  SAA -4           ;         A := -4 (device error)
074423  124002  JMP 2
074424  170773  SAA -5           ;       A := -5 (blank check / read only)
074425  054423  LDX ,B 23        ;   X := DSOW2
074426  006010  STA ,X 10        ;   QUDF.HSTAT := A  (BDIO error)
; -------- deliver to waiting process --------
074427  044034  LDA 34           ;   A := DOMDF            (074463 = 041064)
074430  144075  SWAP SX DA       ;   X := DOMDF-addr, A := DSOW2-queue-DF
074431  056026  LDX ,X 26        ;   X := DOMDF.DSCA1 (pool DF)
074432  146173  RADD CLD SX DB   ;   B := pool DF
074433  150401  IOF
074434  146157  RADD CLD SA DX   ;   X := DSOW2 queue DF
074435  135027  JPL I 27         ;   CALL TO12Q            (074464 = 026725)
074436  150402  ION              ;     (queue DF onto level-12 queue)
074437  044024  LDA 24           ;   A := DOMDF            (074463)
074440  146153  RADD CLD SA DB   ;   B := DOMDF
074441  124206  JMP -172         ;   OD (loop -> 074247)
074442  125023  JMP I 23         ;   GO WT12               (074465 = 033616)
; ---- literal pool 074443-074465 (all resolved above) ----
```

## 5. MBUILD 073700B and DCNVA 073750B - message build + address conversion  [V]

```
073700  146145  RADD CLD SL DA   ; MBUILD: A := L
073701  004376  STA -2           ;   SAVL := L             (073677)
073702  014374  STX -4           ;   SAVX := X (pool DF)   (073676)
073703  024415  LDD ,B 15        ;   AD := QUDF.MEMAD (ND-100 phys addr)
073704  135036  JPL I 36         ;   CALL DCNVA            (073742 = 073750)
073705  124033  JMP 33           ;     error -> ERET (073740: SAA -3; JMP I SAVL)
073706  054035  LDX 35           ;   X := DOMDF            (073743 = 041064)
073707  022044  STD ,X 44        ;   DOMDF.DMYAD := AD  (DOMINO addr, bit31 set)
073710  024417  LDD ,B 17        ;   AD := QUDF.ABPA2 (media address)
073711  022042  STD ,X 42        ;   DOMDF.DSTBL := AD
073712  044421  LDA ,B 21        ;   A := QUDF.ABP31 (number of pages)
073713  146151  RADD CLD SA DD   ;   D := A
073714  146105  RADD CLD 0 DA    ;   A := 0
073715  022050  STD ,X 50        ;   DOMDF.DNRPG := (0, pages)
073716  170777  SAA -1
073717  146151  RADD CLD SA DD   ;   D := -1
073720  022030  STD ,X 30        ;   DOMDF.DSSTS := (-1,-1)
073721  002036  STZ ,X 36        ;   DOMDF.DSQC1 := 0
073722  002037  STZ ,X 37        ;   DOMDF.DSQC2 := 0
073723  050353  LDT -25          ;   T := SAVX (pool DF addr)
073724  012026  STT ,X 26        ;   DOMDF.DSCA1 := pool DF
073725  144063  SWAP ST DB       ;   B :=: T  (B=pool DF, T=queue DF)
073726  024417  LDD ,B 17        ;   AD := PDF.DIPOO (DIPO1,DIPO2)
073727  022034  STD ,X 34        ;   DOMDF.DXPOO := AD  (pool index)
073730  024421  LDD ,B 21        ;   AD := PDF.OPAIX (double)
073731  022040  STD ,X 40        ;   DOMDF.OPAIN := AD  (area index)
073732  146163  RADD CLD ST DB   ;   B := queue DF again
073733  012023  STT ,X 23        ;   DOMDF.DSOW2 := queue DF (owner)
073734  170401  SAA 1
073735  006022  STA ,X 22        ;   DOMDF.DSOW1 := 1 (abstrans message)
073736  040341  MIN -37          ;   MIN SAVL (skip return)
073737  125340  JMP I -40        ;   GO SAVL
073740  170775  SAA -3           ; ERET: A := -3 (illegal memory address)
073741  125336  JMP I -42        ;   GO SAVL
073742  073750  (ptr) DCNVA
073743  041064  (ptr) DOMDF
073744  177777  5BIA1 := -1      ; ND-100 addr of ND-500 first page (patched)
073745  177777  5BIA2 := -1
073746  000000  1ADDR (double)
073747  000000
; ---- DCNVA: ND-100 phys word address -> DOMINO byte address, bit 31 ----
073750  020376  STD -2           ; DCNVA: 1ADDR := AD   << SELF-MODIFIED to
;                                   124012 (JMP 12 -> 073762) after 1st call
073751  054025  LDX 25           ;   X := N500D            (073776 = 051767)
073752  046060  LDA ,X 60        ;   A := N500D.ADRZERO (ND-500 base page no)
073753  146151  RADD CLD SA DD   ;   D := A
073754  146105  RADD CLD 0 DA    ;   A := 0
073755  156612  SAD ZIN 12       ;   AD := AD SHZ 12 (page no -> word addr)
073756  020366  STD -12          ;   5BIAS := AD           (073744-073745)
073757  044020  LDA 20           ;   A := 124012           (073777)
073760  004370  STA -10          ;   M[DCNVA] := 124012 (arm the JMP shortcut)
073761  024365  LDD -13          ;   AD := 1ADDR (reload)
073762  144015  SWAP SD DA       ;   A :=: D
073763  064362  SUB -16          ;   A -= 5BIA2 (low)      (073745)
073764  144015  SWAP SD DA       ;   A :=: D
073765  147155  RADD ADC CLD SA DA ; A := A + carry ...    } A:=A+C-1-5BIA1
073766  172777  AAA -1           ;   A -= 1                }
073767  064355  SUB -23          ;   A -= 5BIA1 (high)     (073744)
073770  175375  BSKP ONE 170 DA  ;   IF bit 17 (sign) set:
073771  124002  JMP 2
073772  146142  EXIT             ;     outside multiport memory -> error return
073773  156601  SAD ZIN 1        ;   AD := AD SHZ 1 (word -> BYTE address)
073774  174375  BSET ONE 170 DA  ;   set bit 17 of A = bit 31 of AD
073775  146542  RADD AD1 CLD SL DP ; EXITA (skip return)
073776  051767  (ptr) N500D
073777  124012  (const) the self-mod JMP word
```

DOMINO address formula [V]:
`DMYAD = ((nd100_word_addr - (ADRZERO << 10dec)) << 1) | 0x80000000`,
error (no skip return) if the subtraction goes negative (outside multiport).
`N500D=051767` [SYMBOL-2], `ADRZERO = N500D+60` [SYMBOL-1 ADRZE=60].
5BIAS is computed once and cached; the entry word is self-modified to
`124012 (JMP 12)` so later calls skip the bias computation.  [V]

## 6. Level-12 stubs: SSBDI 074000B, BDTRANS 074012B / BD12T 074024B  [V]

```
074000  135413  JPL I ,B 13      ; SSBDI: CALL DODMA (controller DF word 13)
074001  146176  RADD CLD SX DT   ;   T := X
074002  146137  RADD CLD SB DX   ;   X := B
074003  173425  AAX 25           ;   X += DIBRE
074004  135003  JPL I 3          ;   CALL PUTIN            (074007 = 026670)
074005  135003  JPL I 3          ;   CALL RTACT            (074010 = 013552)
074006  125003  JMP I 3          ;   GO WT12               (074011 = 033616)

074012  146145  RADD CLD SL DA   ; BDTRANS: A := L
074013  006007  STA ,X 7         ;   QUDF.TRLREG := L
074014  044015  LDA 15           ;   A := BD12T            (074031 = 074024)
074015  006006  STA ,X 6         ;   QUDF.NFUNC := BD12T
074016  150401  IOF
074017  135013  JPL I 13         ;   CALL ST12L            (074032 = 027027)
074020  150402  ION
074021  046007  LDA ,X 7         ;   A := TRLREG
074022  146154  RADD CLD SA DL   ;   L := TRLREG
074023  146142  EXIT             ;   BUSY return
074024  171005  SAT 5            ; BD12T (on lev 12, B=disk DF, X=que DF):
074025  135413  JPL I ,B 13      ;   T := 5; CALL DODMA
074026  046007  LDA ,X 7         ;   A := TRLREG
074027  146154  RADD CLD SA DL   ;   L := TRLREG
074030  146542  RADD AD1 CLD SL DP ; EXITA - FINISH return (SSSTAT valid)
```

For a DOMINO device the datafield `DODMA` slot (word 13) points at STRBDIO;
that binding is runtime configuration data, not carved code.  [I]

## 7. BDMTR 073454B - MTRANS entry for ABSTRANS to DOMINO  [V]

Monitor level. Entry: `A` = parameter list, `B` = pool DF, `T` = PIT no
(0 => use RTREF), `L` = return address or 0. Locals: TREG=073446,
AREG=073447, LREG=073450, BREG=073451, QDF=073452, PROW=073453.

```
073454  004373  STA -5           ; BDMTR: AREG := A
073455  146135  RADD CLD SB DA   ;   A := B
073456  004373  STA -5           ;   BREG := B
073457  010367  STT -11          ;   TREG := T
073460  146146  RADD CLD SL DT   ;   T := L
073461  010367  STT -11          ;   LREG := L
073462  150401  IOF
073463  140006  SKP IF DT EQL 0  ;   IF T<>0 (swapping):
073464  124010  JMP 10
073465  055060  LDX I 60         ;   X := M[RTREF]         (073545 = 004007)
073466  014365  STX -13          ;   PROW := X
073467  044057  LDA 57           ;   A := QP100            (073546 = 033315)
073470  146153  RADD CLD SA DB   ;   B := QP100
073471  135056  JPL I 56         ;   CALL GETFREE          (073547 = 073121)
073472  124051  JMP 51           ;     pool empty -> FULL (073543: ION; GO WT)
073473  124002  JMP 2            ;   -> 073475
073474  135054  JPL I 54         ;   CALL ERRFATAL (swapping illegal) (073550 = 000215)
073475  150402  ION
073476  146163  RADD CLD ST DB   ;   B := T (que element from GETFREE)
073477  010353  STT -25          ;   QDF := T
073500  054353  LDX -25          ;   X := PROW  (073453)
073501  135050  JPL I 50         ;   CALL BRESERVE         (073551 = 010563)
073502  130002  JAP 2            ;   IF A<0:
073503  135045  JPL I 45         ;     CALL ERRFATAL       (073550)
073504  050344  LDT -34          ;   T := LREG
073505  010407  STT ,B 7         ;   QDF.TRLREG := LREG  (return addr on mon lev)
073506  000411  STZ ,B 11        ;   QDF.TYPCO := 0
073507  044340  LDA -40          ;   A := AREG (parameter list)
073510  140006  SKP IF DT EQL 0  ;   IF T=0:
073511  124003  JMP 3
073512  135040  JPL I 40         ;     CALL GAPFU          (073552 = 000744)
073513  124002  JMP 2
073514  135037  JPL I 37         ;   ELSE CALL GAPFD       (073553 = 034006)
073515  150401  IOF              ; ---- start level 12 ----
073516  044333  LDA -45          ;   A := BREG (controller/pool DF)
073517  146153  RADD CLD SA DB
073520  153543  IRW 140 DB       ;   LV12:B := controller DF   (LV12B=140)
073521  044331  LDA -47          ;   A := QDF
073522  153547  IRW 140 DX       ;   LV12:X := parameters (que DF)
073523  044776  LDA ,B -2        ;   A := STDRIV (controller DF -2)
073524  153542  IRW 140 DP       ;   LV12:P := STDRIV
073525  044027  LDA 27           ;   A := 010000 (level-12 bit) (073554)
073526  150306  MST PID          ;   kick level 12
073527  044323  LDA -55          ;   A := QDF
073530  146153  RADD CLD SA DB   ;   B := QDF
073531  054401  LDX ,B 1         ;   X := QDF.RTRES
073532  135023  JPL I 23         ;   CALL WDATA            (073555 = 003527)
073533  046001  LDA ,X 1         ;   A := X.STATUS
073534  174275  BSET ONE 70 DA   ;   set bit 7 = 5NOABORT (delayed abort)
073535  006001  STA ,X 1         ;   X.STATUS := A
073536  150402  ION
073537  051006  LDT I 6          ;   T := M[RTREF]         (073545)
073540  142067  SKP IF DX UEQ ST ;   IF X = RTREF:
073541  125015  JMP I 15         ;     GO RWAIT            (073556 = 032043)
073542  125015  JMP I 15         ;   ELSE GO MONEN         (073557 = 032000)
073543  150402  ION              ; FULL:
073544  125014  JMP I 14         ;   GO WT                 (073560 = 032571)
; literal pool 073545-073560 (resolved above)
```

## 8. BDMFU 073565B - monitor-level completion  [V]

Activated after the driver finished; `X` = controller DF. Locals:
PROW=073561, CDF=073562, SSTA=073563, TRLR=073564.

```
073565  146173  RADD CLD SX DB   ; BDMFU: B := X
073566  014374  STX -4           ;   CDF := X
073567  146137  RADD CLD SB DX   ; DO: X := B
073570  173425  AAX 25           ;   X += DIBRE
073571  046000  LDA ,X 0         ;   A := S0 (queue head)
073572  171377  SAT -1
073573  142065  SKP IF DA UEQ ST ;   WHILE S0 <> -1
073574  124070  JMP 70           ;     empty -> 073664 (GO STUPR via 073675)
073575  150401  IOF
073576  135067  JPL I 67         ;   CALL GETOUT           (073665 = 026656)
073577  146163  RADD CLD ST DB   ;   B := T (que element)
073600  150402  ION
073601  054401  LDX ,B 1         ;   X := RTRES
073602  014357  STX -21          ;   PROW := X
073603  133004  JXZ 4            ;   IF PROW<>0:
073604  000403  STZ ,B 3         ;     TYPRING := 0 (double use, BREGQ)
073605  135061  JPL I 61         ;     CALL RDATA          (073666 = 003535)
073606  135061  JPL I 61         ;     CALL BRELEASE       (073667 = 010610)
073607  044407  LDA ,B 7         ;   TRLR := TRLREG
073610  004354  STA -24
073611  044410  LDA ,B 10        ;   SSTA := SSSTAT
073612  004351  STA -27
073613  044414  LDA ,B 14        ;   A := ABFUN
073614  070054  AND 54           ;   A &= 77               (073670 = 000077)
073615  171001  SAT 1            ;   IF fn not 1, 61, 66:
073616  142065  SKP IF DA UEQ ST
073617  124011  JMP 11
073620  171061  SAT 61
073621  142065  SKP IF DA UEQ ST
073622  124006  JMP 6
073623  171066  SAT 66
073624  142065  SKP IF DA UEQ ST
073625  124003  JMP 3
073626  150110  TRR CCL          ;     clear cache
073627  124002  JMP 2
073630  150110  TRR CCL          ; CCM14: (patch site; symbol CCM14=073630)
073631  146136  RADD CLD SB DT   ;   T := B (que elem)
073632  044037  LDA 37           ;   A := QP100            (073671 = 033315)
073633  146153  RADD CLD SA DB   ;   B := QP100
073634  150401  IOF
073635  135035  JPL I 35         ;   CALL PUTFREE          (073672 = 073150)
073636  150402  ION
073637  146167  RADD CLD ST DX   ;   X := T (que elem)
073640  044322  LDA -56          ;   B := CDF              (073562)
073641  146153  RADD CLD SA DB
073642  044322  LDA -56          ;   A := TRLR             (073564)
073643  131417  JAF 17           ;   IF TRLR<>0 (from CALL MTRANS):
073644  044315  LDA -63          ;     (at 073662) CALL ERRFATAL (073674 = 000215)
073645  131014  JAZ 14           ;   IF PROW<>0 (prog aborted?):
073646  044315  LDA -63          ;     A := SSTA           (073563)
073647  054312  LDX -66          ;     X := PROW           (073561)
073650  051023  LDT I 23         ;     T := M[CURPROG]     (073673 = 004010)
073651  140067  SKP IF DX EQL ST ;     IF PROW = CURPROG:
073652  124003  JMP 3
073653  153415  IRW 10 DA        ;       ALEVB:A := SSTA   (background A-reg)
073654  124005  JMP 5
073655  056025  LDX ,X 25        ;     ELSE X := X.RTDLGADDR
073656  146106  RADD CLD 0 DT    ;       T := 0
073657  173403  AAX 3            ;       X += DAREG
073660  143304  STATX            ;       phys store SSTA -> saved A-reg
073661  124002  JMP 2
073662  135012  JPL I 12         ;   (ERRFATAL, see 073643)
073663  124304  JMP -74          ; OD (-> 073567)
073664  125011  JMP I 11         ; GO STUPR                (073675 = 032034)
; literal pool 073665-073675 (resolved above)
```

---

## 9. Pseudo-C of the whole flow (for emulator implementers)

ASCII only. Word = 16 bits. Octal constants written with a leading `0o`
comment where ambiguous. All addresses cited are L07 virtual addresses in
overlay 017-S3SMPIT (base 032000B).

```c
/* ---- global cells (resident data, runtime-only values) ---- */
/* DOMDF   = 041064  global DOMINO message datafield            */
/* CLUSTER = 041574, BBASE = 041576 (OWN at +5, double)          */
/* N500D   = 051767  ND-500/DOMINO CPU datafield, ADRZERO at +60 */
/* SSECN=007271 PDVNO=007267 SINEC=007266 RTREF=004007           */
/* CURPROG=004010 QP100=033315 (pool of 100-word que elements)   */

/* ---- DCNVA @073750: ND-100 phys word addr -> DOMINO byte addr ---- */
/* returns skip (ok) with AD = DOMINO address, or no-skip = error     */
bool dcnva(uint32_t nd100_word_addr, uint32_t *domino_addr)
{
    static uint32_t bias5 = 0xFFFFFFFF;      /* 5BIA1:5BIA2 @073744-45 */
    static bool armed = false;               /* entry word self-mod @073760 */
    if (!armed) {                            /* first call only */
        uint16_t adrzero = mem[N500D + 060]; /* ND-500 first page @073752 */
        bias5 = (uint32_t)adrzero << 10;     /* pages -> words, SHZ 12B */
        armed = true;                        /* M[DCNVA] := 124012 (JMP) */
    }
    uint32_t a = nd100_word_addr - bias5;    /* @073762-073767 */
    if (a & 0x80000000u)                     /* bit 17 of high word set */
        return false;                        /* outside multiport @073772 */
    *domino_addr = (a << 1) | 0x80000000u;   /* byte addr + bit 31 @073773-74 */
    return true;                             /* EXITA @073775 */
}

/* ---- MBUILD @073700: build the read/write-area message in DOMDF ----
 * entry: B = queue DF, X = pool DF; interrupts OFF.
 * return: A = -3 (illegal memory address) | skip-return, X = DOMDF.   */
int mbuild(df_t *qdf /*B*/, df_t *pdf /*X*/)
{
    uint32_t dom;
    if (!dcnva(qdf->d[MEMAD], &dom))         /* MEMAD=15-16 @073703-04 */
        return -3;                           /* @073740 */
    word *m = &mem[DOMDF];
    m[044] = dom;                            /* DMYAD: DOMINO memory addr */
    m[042] = qdf->d[ABPA2];                  /* DSTBL: media (block) addr */
    m[050] = (uint32_t)qdf->w[ABP31];        /* DNRPG: (0, number of pages) */
    m[030] = 0xFFFFFFFF;                     /* DSSTS := -1,-1 */
    m[036] = 0; m[037] = 0;                  /* DSQC1/DSQC2 := 0 */
    m[026] = (word)pdf;                      /* DSCA1 := pool DF addr */
    m[034] = pdf->d[DIPOO];                  /* DXPOO := DIPO1,DIPO2 */
    m[040] = pdf->d[OPAIX];                  /* OPAIN := area index (double) */
    m[023] = (word)qdf;                      /* DSOW2 := queue DF (owner) */
    m[022] = 1;                              /* DSOW1 := 1 (abstrans mark) */
    return OK_SKIP;                          /* MIN SAVL @073736 */
}

/* ---- STRBDIO @074072: level-12 DODMA routine for BDIO ----
 * entry: B = pool DF, X = queue DF, T = type, L = restart NFUNC.
 * exit: JMP via QUDF.NFUNC with A -> QUDF.HSTAT (RETU @074063),
 *       or waits on WT12 and continues in REBDIO.                    */
void strbdio(df_t *pdf /*B*/, df_t *qdf /*X*/, word type /*T*/, word lnk)
{
    qdf->w[NFUNC]  = lnk;                    /* @074076 */
    qdf->w[TYPCO]  = type;                   /* @074077 */
    word fn = qdf->w[ABFUN];                 /* @074101 */

    if (fn == 024) {                         /* return last status @074102 */
        return retu(pdf->w[DSTSD]);          /* pool DF +26 */
    }
    if (fn == 042) {                         /* read capacity @074107 */
        phys_write(qdf->d[MEMAD] + 0, 042);  /* buffer[0] := 42 @074116 */
        phys_write_dbl(qdf->d[MEMAD] + .., pdf->d[ARESZ]); /* @074123, drift #2 */
        return retu(0);                      /* OK @074124 */
    }
    if (fn != 060 && fn != 061 && fn != 063 && fn != 066)
        return retu(-1);                     /* illegal function @074167 */

    iof();                                   /* global DOMDF in use @074142 */
    if (mbuild(qdf, pdf) == -3)              /* @074144 */
        return retu(-3);
    word cmd, size;
    if      (fn == 061) { cmd = 0167; size = 070; }  /* WRITE   @074152 */
    else if (fn == 063) { cmd = 0213; size = 070; }  /* COMPARE @074160 */
    else                { cmd = 0166; size = 074; }  /* READ    @074163 */
    mem[DOMDF + 032] = cmd;                  /* DSCMD @074165 */

    set_owner();                             /* BBASE.OWN := (M[CLUSTER]+14,
                                                DOMDF) in 1BANK @074172-074200 */
    /* NKWRI(A=DSVER+DOMDF, T=DMSID, X=0, D=size) @074205 */
    if (nkwri(DOMDF + 020, qdf->w[DMSID], 0, size) != 0)
        return eret();                       /* @074206 */
    /* NKSEN(A=DOMDF.DLPRT, D=PDF.DRPRT, X=DMSID) @074215 */
    if (nksen(mem[DOMDF + 017], pdf->w[DRPRT], qdf->w[DMSID]) != 0)
        return eret();                       /* @074217 */
    mem[DOMDF + 06] = REBDIO;                /* DOMDF.NFUNC @074223 */
    wt12();                                  /* wait; nucleus msg resumes us */
    rebdio();                                /* @074225 */
}

/* ERET @074035: nucleus error on send path */
void eret(word nucleus_status /*A*/)
{
    SSECN = nucleus_status;
    pdf->w[DSTSD] = nucleus_status;
    if (pdf->w[RSFLA] == 1 && pdf->w[TMR] == 0) {  /* not in timer queue */
        pdf->w[TMR]   = pdf->w[TTMR];
        pdf->w[TMSUB] = BDTMU;               /* let TIMRT try reconnect */
    }
    PDVNO = pdf->w[PLDNO];
    SINEC = 01661;
    flex9(SINEC, 4, qdf->w[RTRES]);          /* 9FLEX error log @074057 */
    retu(-2);                                /* nucleus error */
}

/* RETU @074063: store status, immediate return through NFUNC */
void retu(word status)
{
    qdf->w[HSTAT] = status;                  /* queue DF +10 */
    B = pdf;
    goto_via(qdf->w[NFUNC]);
}

/* ---- REBDIO @074246: completion, entered from WT12 with X=DOMDF ---- */
void rebdio(void)
{
    for (;;) {
        iof();
        set_owner();                         /* @074250-074257 (1BANK/2BANK) */
        word msgid = nkrec(mem[DOMDF + 017]);/* T=DLPRT @074261; X=msgid */
        if (msgid == 0) { wt12(); return; }  /* @074262 -> 074442 */
        SSECN = A;                           /* nucleus status @074263 */
        /* NKREA(T=msgid, X=0, D=76B, A=DSVER+DOMDF) @074272 */
        if (nkrea(msgid, 0, 076, DOMDF + 020) != 0)
            { ion(); continue; }             /* @074274 */
        df_t *own = (df_t *)mem[DOMDF + 023];      /* DSOW2 queue DF */
        uint32_t ssts = mem_dbl[DOMDF + 030];      /* DSSTS */
        df_t *pl = (df_t *)mem[DOMDF + 026];       /* DSCA1 pool DF */
        word hstat;
        if (ssts == 0) {                     /* transfer OK @074277 */
            if (mem_dbl[DOMDF + 036] == 0xFFFFFFFF /* DSQCN = -1,-1 */
             && mem_dbl[DOMDF + 034] == pl->d[DIPOO]) { /* still same pool */
                /* OK but changed to mirror pool: arm timer reconnect */
                if (pl->w[RSFLA] == 1 && pl->w[TMR] == 0) {
                    pl->w[TMR]   = pl->w[TTMR];
                    pl->w[TMSUB] = BDTMV;    /* @074330 */
                }
            }
            pl->w[DSTSD] = 0;                /* @074333 */
            hstat = 0;                       /* everything OK @074335 */
        } else if (ssts == 0xFFFFFFFF && SSECN != 0) {
            /* message rejected by nucleus @074337-074361 */
            pl->w[DSTSD] = SSECN;
            PDVNO = pl->w[PLDNO]; SINEC = 01661;
            flex9(SINEC, 4, own->w[RTRES]);
            hstat = -2;
        } else {
            word st = ssts & 0xFFFF;         /* low word = BDIO status @074363 */
            pl->w[DSTSD] = st; SSECN = st;
            if (st != 0104031 && st != 0104651 && st != 0104622) {
                /* real device error @074377 */
                PDVNO = pl->w[PLDNO]; SINEC = 01662;
                if (pl->w[RSFLA] == 1 && pl->w[TMR] == 0) {
                    pl->w[TMR]   = pl->w[TTMR];
                    pl->w[TMSUB] = BDTMU;    /* @074413 */
                }
                flex9(SINEC, 4, own->w[RTRES]);
                hstat = -4;                  /* device error */
            } else {
                hstat = -5;                  /* blank check / read only
                                                (NPL naming) @074424 */
            }
        }
        own->w[HSTAT] = hstat;               /* @074426 (or @074335) */
        to12q(own, pl);                      /* queue owner's que DF onto
                                                level-12 queue @074435 */
        ion();
    }
}

/* ---- BDTRANS @074012 / BD12T @074024: monitor-side start helper ---- */
/* BDTRANS (called with B=disk DF, X=que DF): saves L in TRLREG,
 * plants BD12T in NFUNC, CALL ST12L to schedule level 12, returns BUSY.
 * BD12T (on level 12): T:=5; CALL DODMA (disk DF word 13 -> STRBDIO for
 * DOMINO units [I]); EXITA = FINISH, SSSTAT in que DF valid.           */

/* ---- SSBDI @074000: multi-thread DMA lev-12 start ---- */
/* CALL DODMA; PUTIN(que DF -> controller DIBRE queue); RTACT; GO WT12. */

/* ---- BDMTR @073454: ABSTRANS -> DOMINO on monitor level ----
 * T!=0 (swapping) => ERRFATAL. Get a 100-word queue element from QP100
 * (GETFREE; pool empty => GO WT), BRESERVE it for RTREF, TRLREG := L,
 * TYPCO := 0, GAPFU/GAPFD build the parameter block, then:
 *   IRW LV12:B := controller DF, IRW LV12:X := que DF,
 *   IRW LV12:P := controller.STDRIV(-2), MST PID(level 12)   @073515-073526
 * WDATA; STATUS |= 5NOABORT(bit 7); IF caller is RTREF GO RWAIT else MONEN.
 *
 * ---- BDMFU @073565: monitor-level completion after driver finished ----
 * drain controller DIBRE queue: GETOUT; RDATA+BRELEASE if owner alive;
 * cache clear unless fn&77 is 1/61/66 (patch site CCM14 @073630);
 * PUTFREE que elem back to QP100; TRLR!=0 => ERRFATAL (MTRANS path);
 * else deliver SSSTAT to aborted program's A-reg (current: IRW ALEVB,
 * other: phys store via RTDLGADDR+DAREG); loop; GO STUPR.              */
```

---

## 10. Differences vs the NPL revision (drift)

1. **Addresses shifted.** NPL listing places STRBDIO at `073633B`; L07 has
   `STRBD=074072B` (whole file shifted ~ +237B). All logic matched 1:1 at
   the new addresses.  [V]
2. **Fn-42 second store:** NPL writes `*STDTX 10` (displacement 10B); the
   carved word at `074123` is `143316`, which `nd100-dis` decodes as plain
   `STDTX`. Whether `143316` carries the 10B displacement inside the
   opcode or the revision dropped it is not settled here - the octal word
   is the ground truth.  [OPEN, minor]
3. **MBUILD `OPAIN`:** carved stores a DOUBLE (`LDD ,B 21 / STD ,X 40`,
   pool-DF OPAIX -> DOMDF.OPAIN 40-41); NPL prose reads as scalar. NPL's
   `DIPOO/OPAIX` may simply be doubles in that revision too.  [V carve]
4. Everything else (function codes 166B/167B/213B, sizes 70B/74B, error
   codes -1..-5, 1661/1662, statuses 104031/104651/104622, BDTMU/BDTMV
   timer hooks, the whole call ladder) matches the NPL logic exactly. [V]

## 11. Open questions

- **[OPEN]** Message words `DSVER+32B .. +67B` (write) / `+73B` (read):
  not initialized by MBUILD/STRBDIO; NKWRI still transfers them. CLOSED
  2026-07-20 (S0-2, DOMDF-INITIALIZER-CARVE.md): the "static header"
  presumption is DISPROVEN - rel +32..67 is the generated zero tail of
  DOMDF + the ADOML lock + the NKMBU buffer start, swept along because
  the 70B/76B transfer windows exceed the 32B-word record content.
  DON'T CARE for the DIOC.
- `DSVER` (+20) and +21: CLOSED 2026-07-20 (S0-2) - QUINI @134206
  (FILSYS 006-S3FS, lazy init at first pool access) writes DSVER := 1
  and DOMDF+21 := 30B after creating the local port (MON 347 fn 1 ->
  DLPRT). See DOMDF-INITIALIZER-CARVE.md.
- **[OPEN]** Who fires `DOMDF.NFUNC` (= REBDIO) when a nucleus message
  arrives - the WT12/level-12 nucleus dispatcher was not carved here
  (WT12=033616 entry exists in this overlay; body not analysed).
- **[OPEN]** NKWRI/NKSEN/NKREC/NKREA internals (042171/043076/043375/
  043411): only the common prologue was verified; register conventions
  above are proven from the caller side only.
- **[OPEN]** Meaning of benign statuses `104031B/104651B/104622B`: the
  "-5 = blank check or read only" reading is NPL naming only.  [NPL-V]
- **[OPEN]** The `STDTX` displacement question (drift #2).
- **[I]** DODMA slot (device DF word 13) -> STRBDIO binding for DOMINO
  units is configuration data (runtime), consistent with `5DSKC=011171`
  [SYMBOL-2] selecting SMD/SCSI-100=0 vs DOMINO=1, but not byte-proven.
```
(end)
