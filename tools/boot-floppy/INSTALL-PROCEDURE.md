# Installing / generating SINTRAN III onto mass storage — evidence from the distribution media

**Scope of this document.** Everything below is derived **only from the ND
distribution diskettes**, opened read-only. No manual was used. Where the media
do not answer a question the answer is written as **NOT FOUND** rather than
reconstructed.

Every statement is tagged:

* **[VERIFIED]** — the exact file and the exact text read out of it is quoted.
* **[INFERRED]** — the reasoning is shown.
* **NOT FOUND** — the media do not contain it.

Floppy *boot-loader* analysis is deliberately out of scope.

## 0. Sources actually read

Extracted read-only with
`ndtool.exe -x -p -o <outdir> <image>`
(`E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build-win\ndtool.exe`).

| image | volume label | files |
|---|---|---|
| `D:\ND\S\VSXK1.img` | `N-220046K03--01D` | `MACM-1718L:BPUN`, `SINTRAN:DATA` |
| `D:\ND\S\VSXK2.img` | `N-220046K03--02D` | `NEW-SYSTEM:PROG`, `DMAC-1915F:BPUN`, symbol lists |
| `D:\ND\S\N-250306K05--01D.img` | `N-250306K05--01D` | `MACM-1718L:BPUN`, `SINTRAN:DATA` |
| `D:\ND\S\N-250306K05--02D.img` | `N-250306K05--02D` | `NEW-SYSTEM:PROG`, `DMAC-1915F:BPUN`, symbol lists |
| `D:\ND\S\N-250306K05-patch.img` | `ND-PATCH-SIN-K` | `PATCHES-10200:PATC`, `START-PATCH-FILE:MODE`, `PATCH-FILE:MODE`, `SYMBOLS-10200:FADM`, `REFERENCE-10200:FADM`, `NEW-SYSTEM:PROG` |
| `D:\ND\S\VSXL1.IMG` | `250305L07-XX-01D` | `MACM-1718L:BPUN`, `SINTRAN-L-1:DATA` |
| `D:\ND\S\VSXL2.IMG` | `250305L07-XX-02D` | `SINTRAN-L-2:DATA` |
| `D:\ND\S\VSXL3.IMG` | `250305L07-XX-03D` | `NEW-SYSTEM:PROG`, `DMAC-1915G:BPUN`, symbol lists, XMSG |
| `D:\ND\S\250306M06-XX-01D.image` | `250306M06-XX-01D` | `MACM-1718L:BPUN`, `SINTRAN-M-1:DATA` |
| `D:\ND\S\250306M06-XX-02D.image` | `250306M06-XX-02D` | `SINTRAN-M-2:DATA` |
| `D:\ND\S\250306M06-XX-03D.image` | `250306M06-XX-03D` | `NEW-SYSTEM:PROG`, `DMAC-1915G:BPUN`, `ND500-MONITOR:BPUN`, symbol lists |

[VERIFIED] — `ndtool -t` listings of all eleven images.

**There is no `*:MODE`, `README`, `INFO` or install-script file on any of the
SINTRAN K / L / M system diskettes.** The only `:MODE` files on the whole media
set are `XMSG-STARTEX:MODE` (VSXL3, 250306M06-03D — XMSG startup, unrelated) and
`START-PATCH-FILE:MODE` / `PATCH-FILE:MODE` on the **K patch** floppy. [VERIFIED
— full `ndtool -t` listing of all eleven images, reproduced above.]

Consequence: the operator-facing installation procedure exists on the media only
as (a) banner text embedded in the `SINTRAN*:DATA` MACM command stream and
(b) prompt text compiled into `MACM-1718L:BPUN`.

Reusable extractor:
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_media_install_evidence.py`

---

## 1. Large-disc install (288 Mb-class SMD and everything else that is not SCSI)

### 1.1 What the media show the operator typing

All quotes are from the printable header / trailer of the `SINTRAN*:DATA` MACM
command stream, and from literal strings inside `MACM-1718L:BPUN`.

**Step A — MACM asks for the disc type.** `MACM-1718L:BPUN` contains, as
parity-stripped ASCII (byte offsets in `VSXL1.IMG` copy):

```
14543  PLEASE DEFINE THE DISC TYPE (MSTYP) !
14584  MSTYP  SINTRAN DEVICE NAME
14617  REMOVABLE OR FIXED (R/F): '
14647  ENTER MSTYP: '
13987  INITIALIZED FOR: 'REMOVABLE
14016  'FIXED
```

and, separately:

```
11949  GIVE DISK TYPE AS ONE OF THE FOLLOWING OCTAL NUMBERS:
12620  DISK TYPE: '
```

[VERIFIED — `strings` over `(SYSTEM)MACM-1718L:BPUN` from `VSXL1.IMG`, bit 7
masked. Identical strings, at different offsets, in the `VSXK1.img`,
`N-250306K05--01D.img` and `250306M06-XX-01D.image` copies of MACM.]

**Step B — MACM lists its own commands:**

```
14669  REMEMBER THE MACM COMMANDS:
14700  )REDEF => REDEFINE DISC TYPE
       )HENT  => GET SINTRAN FROM SAVE-AREA
       22!    => START SINTRAN
14793  10,0$  => LOAD SINTRAN FROM DISKETTE
       TYPE ANY MACM COMMAND:
```

[VERIFIED — same file.]

**Step C — the stream banner tells the operator to start loading.** From
`(SYSTEM)SINTRAN-L-1:DATA` (`VSXL1.IMG`), printable header, lines 300–312:

```
10,1$
%%====================================================================%%
%%  SINTRAN-III/VSX  VERSION L   LOAD  SINTRAN-DISKETTE 1             %%
%%====================================================================%%
10,0$

)MCDEF VOLNA
 000072/MSTYQ  %%
]
"DEBUG;10,1$;"
POPST
10,0,10$
)9READ
```

[VERIFIED.] The K stream says `LOAD SINTRAN-DISKETTE-I` and its `VOLNA` macro
deposits at `004655` instead of `000072`
(`N-250306K05--01D.img`/`VSXK1.img`, `SINTRAN:DATA`, header line 36 and 221–225).
[VERIFIED.]

**Step D — diskette change.** End of `SINTRAN-L-1:DATA`:

```
"DEBUG;10,1$;"
POPST
VOLNA
PCCST
)HENT

10,1$
%%====================================================================%%
%% INSERT SINTRAN-L DISKETTE 2  IN FLOPPY-DISK UNIT 0                 %%
%% AND TYPE: 10,0$  TO CONTINUE LOADING.                              %%
%%====================================================================%%
)9SBLO 2
```

[VERIFIED — `SINTRAN-L-1:DATA` text lines 16150–16161. The M stream is identical
with `INSERT SINTRAN-M DISKETTE 2` (`SINTRAN-M-1:DATA` lines 17427–17434). The K
distribution is a single system diskette and has no such banner.]

**Step E — optional patch macros, then start.** End of `SINTRAN-L-2:DATA`
(and of the single K `SINTRAN:DATA`):

```
PCCST
10,1$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% THE  SINTRAN  III  SYSTEM  MAY  NOW  BE  STARTED  BY  TYPING:  22! %%
%%                                                                    %%
%% LATER  COLD  STARTS  MAY  BE  DONE  EITHER  BY  LOADING  MACM FROM %%
%% SINTRAN DISKETTE I,  TYPE:  )HENT (CR), WAIT  FOR  LINE  FEED  AND %%
%% TYPE: 22!,  OR  BY  PERFORMING  THE  SINTRAN  COMMAND:  COLD-START %%
%%                                                                    %%
%% DISKETTE III CONTAINS  A PROGRAM CALLED NEW-SYSTEM. THIS PROGRAM   %%
%% SHOULD  BE STARTED AS SOON AS THE MAIN DIRECTORY HAS BEEN ENTERED. %%
%% THE PROGRAM WILL GUIDE YOU THROUGH SOME OF THE PROCEDURES          %%
%% THAT HAVE TO BE PERFORMED AFTER LOADING A NEW SYSTEM.              %%
%%                                                                    %%
%% DISKETTE  III  CONTAINS THE SYMBOL-LISTS FOR YOUR SYSTEM.          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
)LINE
```

[VERIFIED — `SINTRAN-L-2:DATA` text lines 5050–5067. The K stream carries the
same banner with `DISKETTE II CONTAINS A PROGRAM CALLED NEW-SYSTEM`
(`SINTRAN:DATA` text lines 13927–13941).]

The patch-macro list the operator may run before `22!` is printed at the very top
of every stream, e.g. L:

```
%% BEFORE 22! IS TYPED, PATCHES TO THE SYSTEM CAN BE DONE BY CALLING
%% A  SET OF MACROES FOR THIS PURPOSE.
...
%%     - PEND                RESET CORE-IMAGE BEFORE STARTING!
%%     I M P O R T A N T !
%%     THE MACRO / PCCST/ MUST ALWAYS BE USED BEFORE STARTING
%%     IF ANY OF THE OTHER MACROES HAVE BEEN USED.
```

[VERIFIED — `SINTRAN-L-1:DATA` header lines 5–35. In K the same block ends
`THE MACRO / PEND / MUST ALWAYS BE USED BEFORE STARTING`.]

### 1.2 Where the disc type is actually chosen

Choice is made **once, at the MACM prompt**, not in the stream. The stream only
*reads* the result, through MAC library-mark conditionals:

```
A=175777     % CRMAX
FA=177677
UA=077777    %CRMAX FOR U-CODE
F=MSTYP      % MASS STORAGE TYPE
D=0          % DUMMY PARAMETER

% DEVICE NUMBER
"BD288+BDFIX -MADEF
G=1540
"W8INC+REMOV+FIXED -MADEF
G=500
"SCASI -MADEF
G=144300

% BIT 17 IS SET IF FIXED DISK
"BD288+W8INC+REMOV+SCASI -MADEF
FR=0
"BDFIX+FIXED
FR=100000


"BD288+BDFIX+W8INC+SCASI -MADEF
H=1@1 FR
POP=PPOP@1 FR
EP=PEP@1 FR
...
```

[VERIFIED — `SINTRAN-L-1:DATA` header lines 125–179; byte-identical structure in
`SINTRAN-M-1:DATA` lines 125–179 and, without the `-MADEF` term, in K
`SINTRAN:DATA` lines 90–145.]

* `F=MSTYP` — the first `)9BYTT` parameter is the symbol `MSTYP`. Nothing in any
  stream assigns `MSTYP` a value. [VERIFIED — grep of all five streams.]
* `MSTYP` and `MSTYQ` are **defined by MACM**, from the `ENTER MSTYP:` answer.
  [INFERRED] — reasoning: MACM contains the prompt `ENTER MSTYP:` and the table
  header `MSTYP  SINTRAN DEVICE NAME`; the stream consumes `MSTYP` and `MSTYQ`
  without defining them; no other file on the media defines them (grep over every
  extracted file: `MSTYP`/`MSTYQ` occur only in the MACM binaries and the `:DATA`
  streams).
* Likewise, exactly which of `BD288 / BDFIX / W8INC / SCASI / REMOV / FIXED` is
  a *defined* symbol (making the guard false) versus an undefined library mark
  (making it true) is set by MACM from the MSTYP + R/F answers. [INFERRED] —
  same reasoning; these six names appear **nowhere** on the media except in the
  five stream headers. In particular they are *not* in `LIBRARY-MARKS:SYMB`,
  which holds SINTRAN's own device marks (`8BD1`, `8WDIS`, `8MD1`, `8F1U0`, …).
  [VERIFIED — grep over all extracted files.]
* The exact MSTYP → mark mapping (which MSTYP numbers select `BD288` rather than
  `W8INC`, etc.) is **NOT FOUND** in the media as text. It is encoded in MACM's
  machine code.

The K stream writes the guards bare (`"BD288+BDFIX`); L and M append `-MADEF`
and add `0/MADEF^` after the conditional block closes. [VERIFIED — K header
lines 96–145 vs L header lines 132–146 and line 298.]

**L and M dropped removable/cartridge support from the address arithmetic.** K
has a second address block:

```
"REMOV+FIXED
H=1@4 FR
EP=PEP@4 FR
...
"
```

L and M have only the `@1` block and then close with `"`. [VERIFIED — K
`SINTRAN:DATA` header lines 128–145; L `SINTRAN-L-1:DATA` header lines 146–180;
M `SINTRAN-M-1:DATA` lines 146–180.] `REMOV`/`FIXED` still appear in the L/M
DEVNO block (`G=500`) and in the `FR` block.

### 1.3 The ten `)9BYTT` parameters

```
%%  )9BYTT PARAMETRES ARE AS FOLLOWS:
%%  NO  1:  MSTYP  - SYMBOL USED:  F
%%  NO  2:  DEVNO  - SYMBOL USED:  G
%%  NO  3:  CORAD  - SYMBOL USED:  D  (DUMMY)
%%  NO  4:  LONG   - SYMBOL USED:  D  (DUMMY)
%%  NO  5:  CLM    - SYMBOL USED:  D  (DUMMY)
%%  NO  6:  BLST   - SYMBOL USED:  D  (DUMMY)
%%  NO  7:  DRES   - SEE BELOW
%%  NO  8:  CRMAX  - SYMBOL USED:  A
%%  NO  9:  MACAD  - SYMBOL USED:  D  (DUMMY)
%%  NO 10:  DASA   - SYMBOL USED:  H
```

[VERIFIED — identical in all five streams.] `CORAD`, `LONG`, `CLM`, `BLST` and
`MACAD` are passed as the dummy value `0` (`D=0`) in every stream — i.e. **no
disc geometry is passed through `)9BYTT`.** [VERIFIED.]

`DRES` (parameter 7) is a block address, and the header states explicitly:

```
%%  NOTE THAT THE ABOVE SYMBOLS STATES BLOCK ADDRESSES FOR THE
%%  RESPECTIVE AREAS AND MUST BE CALCULATED FROM PAGE NUMBER
%%  DEPENDING ON DISK TYPE.
```

[VERIFIED.] The page→block conversion is the `@n` shift: `@1` for
`BD288+BDFIX+W8INC+SCASI`, `@4` for `REMOV+FIXED` (K only). [VERIFIED, quoted
above.]

`FR=100000` (bit 17) marks a **fixed** disc; `FR=0` a removable one. The comment
in the stream is literally `% BIT 17 IS SET IF FIXED DISK`. [VERIFIED.]

### 1.4 ALD / thumbwheel / device-address settings

**NOT FOUND.** No ALD value, no thumbwheel setting and no controller device
address other than the three DEVNO constants (`1540`, `500`, `144300`) appears
anywhere on the K, L or M media. Those would have lived in the operator manual
or on the boot floppy, neither of which is in scope here.

### 1.5 After the system is on disc

`NEW-SYSTEM:PROG` (diskette 2 for K, diskette 3 for L/M) is the guided
post-install program. Readable strings inside it:

```
MultiFunction Program
SINTRAN III/VSX L-version installation L02
WRITE-MESSAGE CHECK-WORKMODE WRITE-SYSINFO UPDATE COPY-FILES CPU-UPDATE
FILE-TEST LIST-IMPLEMENTED-PATCHES RUN-PATCHFILE HELP
@CC ==== D E F I N E  S E G F I L 0 ====
@SINTRAN-SERVICE-PROGRAM
@DEFINE-SEGMENT-FILE Y Y 0 SEGFIL0:DATA
@CC ========= L O A D  D M A C =========
@RT-LOADER
YES
READ-BINARY DMAC 7
YES
Exit
ENTER-DIRECTORY,2--,FLOPPY-DISC-1,0
RELEASE-DIRECTORY 2--
$Insert SINTRAN diskette (no. B1 IF more than one floppy).
$Copying files from SINTRAN floppy(ies) to user SYSTEM...
> Give CPU number (in Decimal):
> Give CPU type (in Decimal):
```

[VERIFIED — `(SYSTEM)NEW-SYSTEM:PROG` from `VSXL3.IMG`, parity-stripped strings.
`250306M06-XX-03D.image` carries the same strings.]

The K patch floppy shows the same shape as an actual command script
(`(SYSTEM)START-PATCH-FILE:MODE`, `N-250306K05-patch.img`) [VERIFIED, full text
read]:

```
@RTENTER
@SIN-SER-PROG
@DEF-SEG-FILE YES YES 0 SEGFIL0
@EXIT
@RT-LOADER
Y
READ-BINARY DMAC-1915F 7
Y
EXIT
@DMAC
)9EXIT
@MODE (ND-PATCH-SIN-:SYST)PATCH-FILE:MODE,,,
...
---Patching OF Sintran-III successfully  completed ---
$Please do a cold start (@COLD-START) of your SYSTEM to get
the patches into effect.Please remember to run the
program S3-CONFIG to check your configuration $
```

---

## 2. SCSI install

Everything in section 1 applies unchanged; SCSI differs only in what MACM is
told and in the three constants the stream then selects.

### 2.1 The SCSI-specific values in the generation stream

| item | value | evidence |
|---|---|---|
| conditional guard | `"SCASI` | present **only** in K, L, M streams; absent from H and J [VERIFIED] |
| DEVNO (`)9BYTT` param 2) | `144300` octal | `"SCASI -MADEF` / `G=144300` [VERIFIED] |
| fixed-disc bit | `FR=0` — SCSI is grouped with the *non*-fixed set `"BD288+W8INC+REMOV+SCASI` [VERIFIED] |
| page→block shift | `@1` (`"BD288+BDFIX+W8INC+SCASI` block) [VERIFIED] |
| `MSTYP` value | `23` octal — MACM's table line reads exactly `23 SCSI` [VERIFIED] |
| MACM "DISK TYPE" value | `24` octal — table line `24: SCSI` [VERIFIED] |

`"SCASI` occurs exactly three times in each stream, all three inside the header
parameter block (DEVNO, FR, address block). There is **no other SCSI-conditional
region** in any generation stream. [VERIFIED — grep of the complete
`SINTRAN-L-1:DATA`, `SINTRAN-L-2:DATA`, `SINTRAN-M-1:DATA`, `SINTRAN-M-2:DATA`
and K `SINTRAN:DATA`.]

### 2.2 What `144300` means

`144300` is the value handed to `)9BYTT` parameter 2, whose documented name in
the stream header is `DEVNO`. [VERIFIED.] Beyond "it is the device number MACM
and SINTRAN use for the SCSI disc controller", the media contain no further
decoding of it. Any bit-field interpretation would be a guess — **NOT FOUND**.

For comparison, from the same block: `1540` for `BD288`/`BDFIX` (the big-disc /
SMD path) and `500` for `W8INC`/`REMOV`/`FIXED`. [VERIFIED.]

### 2.3 SCSI unit / sub-unit selection

The SINTRAN image contains the mass-storage device-name table with explicit SCSI
names, e.g. from `SINTRAN-L-1:DATA` at byte offset 901832 onward:

```
DISC-SCSI-1'   DISC-2-SCSI-1'  DISC-3-SCSI-1'  DISC-4-SCSI-1'
DISC-5-SCSI-1' DISC-6-SCSI-1'  DISC-7-SCSI-1'  DISC-8-SCSI-1'
DISC-SCSI-2'   ... DISC-8-SCSI-2'   ...  up to DISC-8-SCSI-14'
```

[VERIFIED — parity-stripped strings from `(SYSTEM)SINTRAN-L-1:DATA`.]

Enumerated per distribution:

| media set | leading numbers present | trailing unit numbers | distinct SCSI names |
|---|---|---|---|
| `VSXK1.img` (`N-220046K03`) | 2 3 4 5 | 1–14 | 70 |
| `N-250306K05--01D.img` | 2 3 4 5 6 8 | 1–14 | 98 |
| `VSXL1.IMG` (`250305L07`) | 2 3 4 5 6 7 8 | 1–14 | 112 |
| `250306M06-XX-01D.image` | 2 3 4 5 6 7 8 | 1–14 | 112 |

[VERIFIED — enumeration of every `DISC-…SCSI-…` string in each image.]
Note `N-220046K03` stops at 5 and `250306K05` is missing `7`; L and M carry the
full 2–8 set.

Meaning of the two numbers: **[INFERRED].** By analogy with the SMD names in the
same table — `DISC-70MB-1`, `DISC-2-70MB-1-F`, `DISC-4-70MB-1-R`,
`DISC-6-70MB-1-N`, where the leading number is the count of spindles combined
into one logical mass-storage unit and the trailing number is the unit number —
`DISC-`*n*`-SCSI-`*u* is *n* SCSI drives forming SINTRAN mass-storage unit *u*.
The media do not state this; nothing on the media contradicts it.

The SCSI *target ID* / *LUN* is **NOT FOUND** as a MACM or `)9BYTT` parameter.
The running system does prompt for unit and sub-unit — from the same image:

```
DEVICE NAME: 'DEVICE UNIT: 'DEVICE SUB-UNIT: 'FIXED(F) OR REMOVABLE(R): '
```

(`SINTRAN-L-1:DATA` @549514; the M image reads `DEVICE OR POOL NAME:` @555823)
plus the commands `DEFINE-MASS-STORAGE-UNIT`, `DELETE-MASS-STORAGE-UNIT`,
`LIST-MASS-STORAGE-UNITS`. [VERIFIED.] That is a SINTRAN-runtime configuration
step, not part of the MACM generation.

The L and M images also carry a full SCSI driver with its own error text
(`SCSI domino driver`, `SCSI domino device level`, `SCSI bus parity error in the
Data In phase. Retry in progress`, `Error in reading configuration from
"(SCSI-DOMINO)DEVICES:CNFG"`). [VERIFIED — `SINTRAN-L-2:DATA` @268599, @339813,
@350526.] The reference to a `DEVICES:CNFG` file implies SCSI device
configuration is read from a file at run time, not baked in at generation.
[INFERRED from the error-message text.]

### 2.4 SCSI size limits

**NOT FOUND.** No maximum page count, block count, bit-file size or directory
size limit for SCSI (or for any other disc type) appears anywhere on the media.
The `)9BYTT` size-ish parameters (`LONG`, `CLM`, `BLST`, `CORAD`, `MACAD`) are
all passed as the dummy `0`. `CRMAX` (`A=175777`, `FA=177677`, `UA=077777`) is
the *core-image* maximum used by the patch macros, not a disc size. [VERIFIED.]

---

## 3. Supported disc units table

### 3.1 Is disc size a free parameter?

**No — it must match a fixed table.** The operator does not type a capacity,
cylinder count or page count anywhere. He types **one octal number from a table
that is compiled into MACM**, in answer to `ENTER MSTYP:`, plus `R` or `F` in
answer to `REMOVABLE OR FIXED (R/F):`. [VERIFIED — the prompt strings, and the
absence of any size parameter in the `)9BYTT` block, where all five candidate
parameters are the dummy `0`.]

### 3.2 Table 1 — `MSTYP` → SINTRAN device name

Printed by MACM under the heading `MSTYP  SINTRAN DEVICE NAME`. Verbatim from
`MACM-1718L:BPUN` on `VSXL1.IMG` (offsets 13238–13975); the `250306M06-XX-01D`
copy is identical.

```
 2 DISC-10MB-1
 3 DISC-33MB-1,    DISC-66MB-1
 4 DISC-38MB-1,    DISC-70MB-1,      DISC-75MB-1
 5 DISC-288MB-1-R, DISC-3-75MB-1,    DISC-4-70MB-1-R, DISC-225MB-1-R
 6 DISC-30MB-1,    DISC-60MB-1,      DISC-90MB-1
 7 DISC-2-75MB-1
10 DISC-21MB-1,    DISC-14MB-1
11 DISC-45MB-1
12 DISC-23MB-1     DISC-16MB-1
13 DISC-74MB-1     BUTTERFLY: DISC-36MB-C, DISC-49MB, DISC-77MB
14 DISC-28MB-1
15 DISC-140MB-1-F, DISC-2-70MB-1-F
16 DISC-288MB-1-F, DISC-225MB-1-F,   DISC-4-70MB-1-F
17 DISC-288MB-1-E, DISC-225MB-1-E,   DISC-4-70MB-1-E
20 DISC-450MB-1-F, DISC-2-225MB-1-F, DISC-6-70MB-1-F
21 DISC-450MB-1-N, DISC-2-225MB-1-N, DISC-6-70MB-1-N
22 DISC-288MB-1-N, DISC-225MB-1-N,   DISC-4-70MB-1-N
23 SCSI
```

[VERIFIED.] Numbers are octal (the sequence runs 7 → 10). There is no MSTYP 0
or 1 in the table. That this is the `MSTYP` table (rather than the other one) is
[INFERRED] from the adjacent column header `MSTYP  SINTRAN DEVICE NAME` and from
the fact that its entries are SINTRAN device *names*.

The **K** copies differ slightly. `N-250306K05--01D.img` matches L/M except
`13 DISC-74MB-1     BUTTERFLY: DISC-36MB-C, DISC-49MB, DISC-77MB` — same.
`VSXK1.img` (`N-220046K03`) is older and narrower:

```
13 DISC-74MB-1     DISC-36-C (BUTTERFLY)
16 DISC-288MB-1-F, DISC-4-70MB-1-F
17 DISC-288MB-1-E, DISC-4-70MB-1-E
22 DISC-288MB-1-N, DISC-4-70MB-1-N
```

i.e. no `DISC-225MB-*` aliases at 16/17/22. [VERIFIED.]

### 3.3 Table 2 — MACM's own "DISK TYPE" enumeration

A **second, differently numbered** table, printed after
`GIVE DISK TYPE AS ONE OF THE FOLLOWING OCTAL NUMBERS:` and answered at the
prompt `DISK TYPE: `. Verbatim from `MACM-1718L:BPUN` on `VSXL1.IMG`
(offsets 12006–12608):

```
 0: DISC-14MB
 1: DISC-21MB
 2: DISC-23MB
 3: DISC-28MB
 4: DISC-30MB    (DISC-60MB/DISC-90MB)
 5: DISC-33MB
 6: DISC-38MB
 7: DISC-45MB
10: DISC-66MB
11: DISC-70MB
12: DISC-74MB    (BUTTERFLY: DISC-36MB-C, DISC-49MB, DISC-77MB)
13: DISC-75MB
14: DISC-140MB   (DISC-2-70MB)
15: DISC-2-75MB
16: DISC-288MB-R (DISC-225MB-R/DISC-3-75MB/DISC-4-70MB-R)
17: DISC-288MB-F (DISC-225MB-F/DISC-4-70MB-F)
20: DISC-450MB-F (DISC-2-225MB-F/DISC-6-70MB-F)
21: DISC-288MB-E (DISC-225MB-E/DISC-4-70MB-E)
22: DISC-450MB-N (DISC-2-225MB-N/DISC-6-70MB-N)
23: DISC-288MB-N (DISC-225MB-N/DISC-4-70MB-N)
24: SCSI
```

[VERIFIED.] `VSXK1.img`'s copy again lacks the `225MB` aliases at 17/21/23 and
reads `12: DISC-74MB    (DISC-36MB-C (BUTTERFLY))`. [VERIFIED.]

Which MACM command consumes this second table, and how the two numberings relate
to each other, is **NOT FOUND** on the media.

### 3.4 Cylinders / heads / sectors / bytes-per-sector / page counts

**NOT FOUND.** No geometry table — no cylinder counts, head counts, sectors per
track, bytes per sector, or per-unit SINTRAN page counts — exists as readable
text on any of the eleven images. The stream passes only dummy zeros for the
geometry-shaped `)9BYTT` parameters. Any such table must live in MACM's or
SINTRAN's machine code, which was not decoded for this document.

The one capacity-ish enumeration that *is* on the media is in the SINTRAN M
image, in a configuration display screen:

```
Available WINCHESTER sizes: 14 16 21 23 28 45 74
```

alongside the column headers `2-75 3-75 2-225 F/N`, `UNIT 0 1 2 3`,
`M A G  T A P E S : FLOPPY: BIG :`, `PERTEC / STC`.
[VERIFIED — `SINTRAN-M-2:DATA` @581160 and repeated per screen instance.] These
are Winchester capacities in MB, matching MSTYP 10/12/10/12/14/11/13.

### 3.5 The SINTRAN device-name table itself

The full mass-storage device-name table is present in every system image and is
the authoritative list of names SINTRAN will accept. Non-SCSI portion, from
`SINTRAN-L-1:DATA` @897753 onward (unit-1 entries only; the table repeats the
same family for units 2, 3, 4):

```
DISC-38MB-1  DISC-75MB-1  DISC-288MB-1-R  DISC-225MB-1-R  DISC-90MB-1
DISC-60MB-1  DISC-30MB-1  DISC-3-75MB-1   DISC-2-75MB-1   DISC-4-70MB-1-R
DISC-70MB-1  DISC-2-70MB-1-F  DISC-140MB-1-F  DISC-4-70MB-1-F
DISC-288MB-1-F  DISC-6-70MB-1-F  DISC-2-225MB-1-F  DISC-450MB-1-F
DISC-14MB-1  DISC-21MB-1  DISC-45MB-1  DISC-74MB-1  DISC-28MB-1
DISC-23MB-1  DISC-16MB-1
FLOPPY-DISC-1  FLOPPY-DISC-2
DISC-225MB-1-F  DISC-225MB-1-E  DISC-288MB-1-E  DISC-4-70MB-1-E
DISC-6-70MB-1-N  DISC-2-225MB-1-N  DISC-450MB-1-N  DISC-288MB-1-N
DISC-225MB-1-N  DISC-4-70MB-1-N
DISC-SCSI-1 … DISC-8-SCSI-1
```

[VERIFIED.] `DISC-10MB-1` (MSTYP 2) does **not** appear in the L device-name
table; it is only in MACM's MSTYP list. [VERIFIED.]

---

## 4. Anything the media declare INVALID or unsupported

* Any MSTYP outside the printed table: the media give no error text for it, so
  the failure mode is **NOT FOUND**. The tables are however finite and MACM
  re-prompts via `)REDEF => REDEFINE DISC TYPE`. [VERIFIED that `)REDEF` exists.]
* **Removable / cartridge discs are unsupported from version L onwards** in the
  address arithmetic: the `"REMOV+FIXED` block with the `@4` page→block shift
  exists in K and is gone from L and M. [VERIFIED — see §1.2.] The `REMOV`/`FIXED`
  marks still appear in the L/M DEVNO and `FR` blocks, so the media do not say
  the marks were removed outright, only that no block addresses are computed for
  them. This means selecting a REMOV/FIXED disc under L/M would leave every area
  address undefined. [INFERRED from the missing block.]
* **SCSI is unsupported before K.** `"SCASI` is absent from the H and J
  generation streams and present in K, L and M. [VERIFIED — the H stream
  (`H-10-203-I`, `SINTRAN-I:DATA`) contains only `"BD288 "W8INC "BDFIX "REMOV
  "FIXED` at header lines 49/65/81/93/105 and the J stream (`J-900-188-I`,
  `SINTRAN-I:DATA`) only the same five at lines 82/96/110/119/128, with no
  `"SCASI`. Note: these two H/J text extracts were already present in the working
  directory and were not re-extracted from their source images in this pass;
  the K/L/M side of the comparison was re-extracted here.]
* SCSI leading-number coverage grew: `N-220046K03` 2–5, `250306K05` 2–6 and 8
  (no 7), `250305L07` and `250306M06` 2–8. Anything outside that range has no
  device name and therefore cannot be defined. [VERIFIED by enumeration.]
* Addressing / bit-file / directory size limits: **NOT FOUND** on the media.

---

## 5. Open questions (what the media could not answer)

1. **Disc geometry per unit** — cylinders, heads, sectors, bytes/sector, page
   count. Not present as text. Would require disassembling MACM's disc-type
   table (`MACM-1718L:BPUN`) or SINTRAN's mass-storage descriptor table.
2. **The MSTYP → `BD288`/`BDFIX`/`W8INC`/`SCASI`/`REMOV`/`FIXED` mapping.** Only
   MACM's code knows it. This is the single most valuable missing piece: it is
   what turns "MSTYP 16 = DISC-288MB-1-F" into "`BDFIX`, DEVNO 1540, FR=100000".
3. **Meaning of the second table** (`GIVE DISK TYPE … 0–24`) and which MACM
   command reads it, and how its numbering relates to MSTYP.
4. **Decoding of DEVNO `144300`**, and where the SCSI target/LUN is set. The
   `"(SCSI-DOMINO)DEVICES:CNFG"` error string suggests a run-time config file
   that is not on these diskettes.
5. **ALD / thumbwheel / load-descriptor values.** Nothing on the media. These
   would be in the operator documentation or on the boot floppy.
6. **Maximum addressable disc size.** No limit stated anywhere on the media.

---

## 6. How to reproduce every quote above

```powershell
# 1. extract (read-only) — repeat per image
E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build-win\ndtool.exe -x -p -o <outdir> D:\ND\S\VSXL1.IMG

# 2. pull out headers, disc-type tables and device-name tables
python E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_media_install_evidence.py <media-root> --out <evidence-dir>
```

`extract_media_install_evidence.py` masks the even-parity bit 7 (ND text
convention) and writes, per stream, a `.header.txt` (MACM command text up to the
first NUL) and a `.devicenames.txt`, plus a `.disctypes.txt` per MACM binary.
