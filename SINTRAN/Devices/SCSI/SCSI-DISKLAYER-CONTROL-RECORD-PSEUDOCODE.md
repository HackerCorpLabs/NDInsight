# SCSI Disk-Layer: Control-Record Connect + Validation (Pseudo-C)

**SINTRAN III VSX/500 L07, IP-P2-SCSI-DISK compilation unit (disk layer), segment 065-S3SIPIT, load base 32000B.**

This document translates the carved, byte-verified ND-100 assembly of the SCSI
disk-layer "function 42" path (ENTER-DIRECTORY / mount) into readable pseudo-C.
The path: dispatch the op, lazily run INQUIRY/READ-CAPACITY init, read the
geometry block, use the geometry's **last-block LBA as the control-record
address** (by design: SINTRAN stores the directory control record in the last
block of the disk), read that one block, then validate it in FINEX with an XOR
checksum and an NPART range check, and publish the partition/geometry table on
success.

Sources of truth:

- Carved ASM (byte-verified against the L07 image):
  [SCSI-DISKLAYER-COMPLETE.ASM](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/SCSI-DISKLAYER-COMPLETE/SCSI-DISKLAYER-COMPLETE.ASM)
- NPL source (a DIFFERENT revision - names and logic hints only, every claim
  taken from it is marked `NPL:`):
  [IP-P2-SCSI-DISK.NPL](../../NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL)

Marking convention used below:

- **VERIFIED** = provable from the carved words quoted (octal address + word)
  or from the live-run ground truth listed in "Live-verified runtime facts".
- **NPL:** = taken from the NPL source (different revision; treated as a hint).
- **UNVERIFIED:** = not provable from either; explicitly flagged, never guessed.

Live-verified runtime facts (ground truth for this analysis):

- The mechanism loops re-issuing SC_WRITE_6 to LBA 0xEFFE (61438 decimal),
  which IS the geometry last-block - BY DESIGN the control-record location.
- The chip and driver complete each write cleanly.
- The disk-layer 6/13 retry described in section 6 does NOT fire at runtime
  (the status the disk layer sees reads 0).

Note on an apparent tension (stated, not resolved): the carved transfer build
(section 5) clears the low 6 function bits of ABFUN (`ABFUN &= 177700`), which
the NPL comments call "INDICATE READ" (fn 0 = read per the SCSID fall-through
comment "fn 0(read)/1(write)"). The live trace nevertheless shows SC_WRITE_6
being issued. This document records both facts; how a WRITE(6) CDB results is
NOT explained here. UNVERIFIED: the ABFUN-to-CDB-opcode mapping inside CACOB
(063750) is not fully carved.

---

## 1. Datafield offsets used

Offsets are octal, relative to the op/unit datafield (`,X` or `,B` as noted).
Names from the ASM header (FILSYS / SYMBOL-1) and NPL.

| Offset | Name  | Meaning | Status |
|--------|-------|---------|--------|
| 6      | NFUNC | saved link/function on entry | VERIFIED (header + `057216 006006 STA ,X 6`) |
| 10     | HSTAT | hardware/return status | VERIFIED (header; `062217 006010 STA ,X 10`) |
| 12     | DQOPC | saved op word (bit 100B = fn-42 flag, NPL: 3SF42) | carved use VERIFIED; name NPL: |
| 14     | ABFUN | function/op word; fn = ABFUN & 077 | VERIFIED |
| 15/16  | MEMAD (MEMA1/MEMA2) | 32-bit physical buffer pointer (T:X form, high:low) | name NPL:; carved use VERIFIED |
| 17     | ABPA2 | disk block address (LBA), double word | VERIFIED |
| 21     | ABP31 | transfer amount high / aux | NPL: name; carved use VERIFIED |
| 22     | ABP32 | transfer amount / block count | VERIFIED |
| 23     | SUTYP | device/status flags; bit 7 = 5SCIN "init done"; high byte = INQUIRY device type | VERIFIED |
| X-13   | SCOCW | SCSI op control word (bit 100B = fn-42 in progress, NPL: 3SF42) | name NPL:; carved use VERIFIED |
| X-15   | TACOU | retry counter (negative count, MIN counts toward 0) | name NPL: (057112 `TACNS=:TACOU`); carved use VERIFIED |
| B-3    | NPART | partition count from control record | name NPL:; carved cell VERIFIED (`057675 004775 STA ,B -3`) |
| B-2    | UHLIM | upper block limit (disk capacity) | carved cell VERIFIED (constant 177776 at 057410 gives address B-2; FINEX stores it at `057742 020776 STD ,B -2`). NOTE: the ASM header lists UHLIM at df offset 32; the relation between that offset and the B-2 frame is UNVERIFIED. |
| B-15/16 | (unit copy of MEMAD) | control-record buffer pointer saved for fn 42 | VERIFIED (`057475/057476 LDD ,X 15 / STD ,B 15`) |
| B-25/26 | CMAD1/CMAD2 | command/geometry area pointer (MOVEW destination) | name NPL:; carved use VERIFIED |
| 057654 | (loop counter cell) | XOR-loop counter, P-relative cell | VERIFIED (STA -24 at 057700 and MIN -31 at 057705 both resolve to 057654) |

Internal return codes in T (from the ASM header / FUNCTION-42 ERREX map at 062124):

| T (octal) | Name  | Meaning |
|-----------|-------|---------|
| 0         | OK    | success |
| 1         | TYPER | illegal device type |
| 4         | ILAOP | illegal operation |
| 5         | BADPA | bad partition/address |
| 11        | NOCRC | no valid control record |
| 6 / 13    | -     | retry-in-place (UNIT ATTENTION / ABORTED), never returned |

---

## 2. Entry: SCSDI (057215 - 057273) - VERIFIED

```c
/* 057215-057217: entry, save link, PERFO/DIALO setup */
void SCSDI(void)                       /* 057215 146145 RADD CLD SL DA */
{
    X.NFUNC = link;                    /* 057216 006006 STA ,X 6 */
    call_perfo_dialo_helper();         /* 057217 135051 JPL I 51 -> 057270 */

    /* 057220-057234: function extraction + validity gate */
    fn = X.ABFUN & 077;                /* 057221 046014 LDA ,X 14 ; 057224 144457 RAND */
    a = table_lookup(fn);              /* 057226 047043 LDA I ,X 43 (indirect via fn) */
    X.12 = a;                          /* 057231 006012 STA ,X 12   (save op word) */
    if (a == 0) {                      /* 057232 131403 JAF 3 (skip if A has bits) */
        T = ILAOP;                     /* 057233 171004 SAT 4  *** T := 4 = ILAOP *** */
        goto terminate;                /* 057234 125036 JMP I 36 -> 057272 RETOP */
    }
    /* 057235-057267: op-word bit housekeeping (address form, amount move
       X.21 -> X.22, sub-op range check -> T := 7 terminate on bad range).
       UNVERIFIED: the exact meaning of the individual op-word bits tested
       at 057236/057246 (bits 140B/150B of L) is not named in any source. */
}
```

## 3. Bounds check + queue (057274 - 057413) - VERIFIED

```c
/* 057274: select address arm by op-word bits */
if (opword_bit_130(L))                 /* 057276 175134 BSKP ZRO 130 DL */
    goto skip_bounds;                  /* 057277 -> 057364 (sets flag bit, no check) */

if (opword_bit_120(L)) {               /* 057300 175324 BSKP ONE 120 DL */
    /* WHOLE-DISK arm (057352-057362), VERIFIED dd: end = addr + amount,
       compare against UHLIM */
    AD = X.ABPA2;                      /* 057352 026017 LDD ,X 17 */
    T  = X.ABP32;                      /* 057353 052022 LDT ,X 22 */
    end = AD + T;                      /* 057354 146061 RADD ST DD ;
                                          057355 147155 ADC carry into A (32-bit add) */
    limit = *(B - 2);                  /* 057356 050032 LDT 32 -> P-rel cell
                                          057410 = 177776 = -2 ; 057357 146036
                                          RADD SB DT => T := B - 2 = &UHLIM */
    COMPD(end, limit);                 /* 057360 135027 JPL I 27 -> 057407 */
    if (end > UHLIM) {                 /* 057361 141050 SKP IF 0 GRE SA */
        T = BADPA;                     /* 057377-057401: ERR2, 057401 171005 SAT 5 */
        goto terminate_no_transfer;    /* 057402 -> SWT11, no SCLLD */
    }
} else {
    /* PARTITION arm (057302-057351): partition index = ABPA2 >> 8 checked
       against partition count (057311 141056 SKP IF DT GRE SA, else BADPA at
       057377); per-partition base+size fetched via LDDTX, end compared with
       COMPD as above. NPL: 056707 "IF X.ABP21 SHZ -10 > NPART-1 GO ERR2". */
}

SUNOP++;                               /* 057367 040410 MIN ,B 10 (pending-op count) */
if (B.SLINK != 0) {                    /* 057371/057372: LDA ,B 4 ; JAZ 4 */
    DSORT();                           /* 057374 -> queue the op */
    SWT11();                           /* 057375 -> dispatch */
} else {
    goto NEWOP;                        /* 057376 -> 057414 */
}
```

## 4. NEWOP: pre-decode + fn-42 setup (057414 - 057507) - VERIFIED

```c
NEWOP:                                 /* 057414 014404 STX ,B 4 : SLINK := X */
    /* 057424: if "already initialised" op bit set, copy ABPA2 and go
       straight to the transfer join */
    fn = A & 077;  A &= X.ABFUN;       /* 057431/057432 */

    if (fn == 6)  { ... }              /* 057433-057441: fn 6 far init-call block */

    if (fn == 36) {                    /* 057442-057471: read-layout */
        /* bounds-check descriptor, then MOVEW copy of the layout
           descriptor into the caller buffer; T := 0 OK or T := 5 BADPA
           (057466 171005 SAT 5) */
    }

    if (fn == 42) {                    /* 057472 171042 SAT 42 (VERIFIED dd) */
        /* fn 42 = "read format" = the ENTER-DIRECTORY control-record connect */
        B.MEMAD  = X.MEMAD;            /* 057475/057476: LDD ,X 15 ; STD ,B 15
                                          save caller 32-bit buffer pointer */
        B.ABFUN  = X.ABFUN;            /* 057477/057500 */
        B.ABP31  = 0;                  /* 057501 000421 STZ ,B 21 */
        B.ABP32  = 0100;               /* 057502/057503: SAA 100 ; STA ,B 22
                                          initial read amount = 100B (64 dec) */
        SUTYP &= ~5SCIN;               /* 057505/057506/057507: LDA ,X 23 ;
                                          BSET ZRO 70 DA (dd 174075) ; STA ,X 23
                                          *** clear bit 7 -> force re-INQUIRY *** */
    }
    /* fall through to RETRY join 057510 */
```

## 5. RETRY join: lazy init, then the control-record transfer build (057510 - 057610)

### 5a. Init block (057510 - 057556) - VERIFIED

```c
RETRY:                                 /* 057510; outer back-edge re-enters at 057514 */
    A = B.SUTYP;                       /* 057514 044423 LDA ,B 23 */
    if ((SUTYP & 5SCIN) == 0) {        /* 057515 175075 BSKP ZRO 70 DA (dd 175075) */
        /* not initialised: run the init read (INQUIRY/READ CAPACITY path) */
        if (X.SCOCW & bit100)          /* 057522 175305 BSKP ONE 100 DA */
            X.ABFUN = 042;             /* 057524/057525: init read as fn 42 */
        else
            X.ABFUN = 036;             /* 057527/057530: init read as fn 36 */

        do {                                       /* inner init-retry loop */
            status = SCSID(T = X);                 /* 057532 135077 JPL I 77
                                                      -> ptr 057631 = 062217 SCSID */
            /* return+1 -> ERREX (057533), return+2 -> alt far (057534) */
            D = status & ~0100000;                 /* 057535/057536: D := A,
                                                      clear bit 15 */
        } while ((D == 6 || A == 013)              /* 057537-057544: SAT 6 / SAT 13
                                                      compares (UNIT ATTN / ABORTED) */
                 && ++TACOU != 0);                 /* 057545 042363 MIN ,X -15 ;
                                                      057546 JMP -15 -> 057531 */

        if (D >= 1)                                /* 057547/057550: SAT 1 ;
                                                      SKP IF DT GRE SD (skip if 1 > D) */
            goto FINEX;                            /* 057551 JMP 54 -> 057625 -> FINEX */
            /* NPL: the source revision loops "WHILE D<=1" here; the carved
               revision branches to FINEX on D >= 1. Carved behavior is
               authoritative for L07. */

        /* DEVICE-TYPE GATE - every word VERIFIED dd */
        devType = B.SUTYP >> 8;                    /* 057552 044423 ; 057553 156570 */
        if (devType != 0) {                        /* 057554 131003 JAZ 3 */
            T = TYPER;                             /* 057555 171001 SAT 1 *** T:=1 *** */
            goto ERREX_terminate;                  /* 057556 125054 JMP I 54 */
        }
    }
    /* fall through to TRANSFER (057557) */
```

### 5b. TRANSFER: build the control-record I/O (057557 - 057610) - VERIFIED

This is the heart of the connect: take the last-block LBA out of the geometry
data just read, make it the transfer LBA, transfer exactly 1 block, and bump
the 32-bit buffer pointer by one block (512 words) so the control record lands
after the geometry block.

```c
TRANSFER:
    A = X.SCOCW;                       /* 057557 046365 LDA ,X -13 */
    if (A & bit100) {                  /* 057560 175305 BSKP ONE 100 DA
                                          (NPL: SCOCW BIT 3SF42 - fn-42 op) */

        X.ABFUN &= 0177700;            /* 057562-057564: SAA -100 (=177700) ;
                                          AND ,X 14 ; STA ,X 14
                                          clear fn bits -> fn 0.
                                          NPL: comment "INDICATE READ".
                                          (Live fact: SC_WRITE_6 is what appears
                                          on the wire - see intro note; the
                                          fn->CDB mapping is in CACOB, not
                                          fully carved.) */

        /* --- load geometry last-block as the control-record LBA --- */
        L = X;                         /* 057565 146174 RADD CLD SX DL */
        T = X.MEMA1; X = X.MEMA2;      /* 057566/057567: LDT ,X 15 ; LDX ,X 16
                                          T:X = 32-bit physical buffer pointer */
        AD = phys[bufferPtr32 + 010];  /* 057570 143312 LDDTX disp 10
                                          (base 143302 + 10; matches NPL
                                          "*LDDTX 10 % ADDRESS OF CONTROLL
                                          RECORD"): read the double word at
                                          word offset 10B (8 dec) of the
                                          geometry block = last-block LBA.
                                          Live-VERIFIED value: 0xEFFE (61438),
                                          the geometry last block. */
        X = L;                         /* 057571 146147 RADD CLD SL DX */
        X.ABPA2 = AD;                  /* 057572 022017 STD ,X 17
                                          controlRecordLBA := last block */
        X.ABP31 = 0;                   /* 057573 002021 STZ ,X 21 */
        X.ABP32 = 1;                   /* 057574/057575: SAA 1 ; STA ,X 22
                                          exactly ONE block */

        /* --- advance the 32-bit buffer pointer by one block --- */
        AD = X.MEMAD;                  /* 057576 026015 LDD ,X 15 */
        T  = 01000;                    /* 057577 050035 LDT 35 -> P-rel cell
                                          057634 = 001000 (VERIFIED)
                                          = 512 words = 1KB = one disk block */
        D += T; A += carry;            /* 057600 146061 RADD ST DD ;
                                          057601 147155 RADD ADC CLD SA DA
                                          32-bit add with carry propagation */
        X.MEMAD = AD;                  /* 057602 022015 STD ,X 15
                                          control record will be DMAed one
                                          block past the geometry data */
        T = X;                         /* 057603 146176 RADD CLD SX DT */
    } else {
        T = X.SLINK;                   /* 057605 052004 LDT ,X 4
                                          NPL: "PARAMETER POINTER" (non-42 ops) */
    }

    /* --- CALL SCSID (real transfer) --- */
    A = 0377 & X.SCOCW;                /* 057606 044027 LDA 27 -> P-rel cell
                                          057635 = 000377 (VERIFIED) ;
                                          057607 072365 AND ,X -13 */
    SCSID();                           /* 057610 135021 JPL I 21 -> ptr
                                          057631 = 062217 (VERIFIED dd) */
```

## 6. Transfer status / retry check (057611 - 057625) - VERIFIED

SCSID uses a 3-way skip return: return+0 = hard error, return+1 = alternate
error, return+2 = normal (status in A).

```c
    /* 057611 125021 JMP I 21 -> 057632 : return+0 -> ERREX far handler */
    /* 057612 135021 JPL I 21 -> 057633 : return+1 -> alt far
       (NPL: CALL ERRFATAL) */

    /* return+2: status check */
    D = A & ~0100000;                  /* 057613 146151 RADD CLD SA DD ;
                                          057614 174171 BSET ZRO 170 DD
                                          (clear bit 15 of the copy) */
    if (D == 6                         /* 057615 171006 SAT 6 ;
                                          057616 142061 SKP IF DD UEQ ST
                                          6 = UNIT ATTENTION */
        || A == 013) {                 /* 057620 171013 SAT 13 ;
                                          057621 140065 SKP IF DA EQL ST
                                          13B = ABORTED/reset */
        if (++TACOU != 0)              /* 057623 042363 MIN ,X -15
                                          (negative counter toward 0;
                                          MIN skips next when it hits 0) */
            goto RETRY;                /* 057624 124270 JMP -110 -> 057514
                                          *** OUTER RETRY back-edge *** */
        /* retries exhausted: falls through to FINEX below */
    }
    goto FINEX;                        /* 057625 124030 JMP 30 -> 057655 */
```

Live-VERIFIED: at runtime the status here reads 0, so this 6/13 retry does NOT
fire; execution goes straight to FINEX. The observed re-issue loop of
SC_WRITE_6 is therefore NOT this loop.

### ERREX far handler (057636 - 057653) - carved words VERIFIED, symbol names partly NPL

```c
ERREX:  /* reached via ptr 057632 = 057636 */
    if (T == 050 ||                    /* 057636 170450 SAA 50 ; 057637 SKP UEQ */
        T == 043 ||                    /* 057641 170443 SAA 43 */
        T == 051) {                    /* 057644 170451 SAA 51
                                          NPL: PFAIL / SBRST / LIRST (bus reset,
                                          line reset, power fail). UNVERIFIED:
                                          which constant maps to which symbol. */
        ++TACOU;                       /* 057647 042363 MIN ,X -15 */
        goto RETRY;                    /* 057650 125135 JMP I 135 -> 060005 */
    }
    /* else: terminate with the ERREX code (A := 20) */
    /* 057651-057653 -> 057750 RETOP */
```

## 7. FINEX: control-record checksum + geometry publish (057655 - 057755)

### 7a. Guards (057655 - 057666) - VERIFIED

```c
FINEX:
    B = X;                             /* 057655 146173 RADD CLD SX DB */
    X = B.SLINK;                       /* 057656 054404 LDX ,B 4 */
    T = X.DQOPC;                       /* 057657 052012 LDT ,X 12 */
    if ((T & bit100) == 0)             /* 057660 175306 BSKP ONE 100 DT
                                          NPL: "IF T:=X.DQOPC BIT 3SF42" -
                                          only fn-42 ops get validated */
        goto success_tail;             /* 057661 124066 JMP 66 -> 057747 T:=0 */

    X.HSTAT = A;                       /* 057662 006010 STA ,X 10 */
    A &= ~0100000;                     /* 057663 174175 BSET ZRO 170 DA */
    if (A >= 1)                        /* 057664/057665: SAT 1 ;
                                          SKP IF DT GRE SA (skip if 1 > A) */
        goto success_tail;             /* 057666 124061 JMP 61 -> 057747
                                          carved: only HSTAT == 0 (masked)
                                          proceeds to validation.
                                          NPL: source revision says "<= 1"
                                          (057264 IF A=:X.HSTAT BZERO 17<=1);
                                          carved L07 behavior differs. */
```

### 7b. XOR checksum over the control record (057667 - 057720) - VERIFIED

```c
    exec(*ptr117);                     /* 057667 051117 LDT I 117 ;
                                          057670 140660 EXR ST
                                          NPL: "T:=SCCLR; *EXR ST % CLEAR CACHE" */

    /* point T:X at the control record (the buffer pointer that was advanced
       by one block in section 5b, saved in the unit df at B.15/16) */
    T = B.MEMA1; X = B.MEMA2;          /* 057671 050415 LDT ,B 15 ;
                                          057672 054416 LDX ,B 16 */

    NPART = phys[ctrlRec + 0] >> 8;    /* 057673 143300 LDATX disp 0 ;
                                          057674 156570 SHA ZIN SHR 10 ;
                                          057675 004775 STA ,B -3
                                          first word of the control record,
                                          high byte = partition count */
    L = NPART;                         /* 057676 146154 RADD CLD SA DL */

    counter = *(cell_060007);          /* 057677 044110 LDA 110 -> P-rel cell
                                          060007. UNVERIFIED: cell value not in
                                          the carve listing. NPL: -1000B
                                          (= -512 dec) => loop over 512 words
                                          = the whole 1KB control-record block */
    *(cell_057654) = counter;          /* 057700 004354 STA -24 -> counter
                                          cell 057654 (VERIFIED: 057654=000000
                                          in the carve) */

    xorChecksum = 0;                   /* 057701 146101 RADD CLD 0 DD */
    do {
        A = phys[T:X];                 /* 057702 143300 LDATX */
        xorChecksum ^= A;              /* 057703 145051 REXO SA DD */
        X++;                           /* 057704 173401 AAX 1
                                          (low half only; the 512-word scan
                                          stays within one 64KW bank) */
    } while (++*(cell_057654) != 0);   /* 057705 040347 MIN -31 (cell 057654) ;
                                          057706 124374 JMP -4 -> 057702 */

    /* verdict on the record */
    if (xorChecksum != 0               /* 057707 140001 SKP IF DD EQL 0 ;
                                          057710 JMP 7 -> NOCRC */
        || NPART <= 2                  /* 057711 171002 SAT 2 ;
                                          057712 143046 SKP IF DT LST SL
                                          (skip if 2 < NPART) */
        || NPART > NCOPA) {            /* 057714 171012 SAT 12 ;
                                          057715 143046 SKP IF DT LST SL
                                          NCOPA = 12B = 10 dec (VERIFIED as the
                                          immediate; the NAME NCOPA is NPL:) */
        T = NOCRC;                     /* 057717 171011 SAT 11
                                          *** T := 11B = NOCRC:
                                          no valid control record *** */
        goto terminate;                /* 057720 125070 JMP I 70 -> 060010 RETEX */
    }
```

So a control record is VALID iff:
1. the XOR of all 512 words of the block is 0 (the record carries its own
   XOR-closing checksum word), AND
2. 2 < NPART <= 10 (partition count in a sane range).

### 7c. Geometry publish (057721 - 057747) - VERIFIED (multiplier value NPL:)

```c
    /* copy the partition table out of the control record */
    A = NPART;                         /* 057721 044775 LDA ,B -3 */
    L = NPART * (*cell_060011);        /* 057722 120067 MPY 67 -> P-rel cell
                                          060011 ; 057723 RADD CLD SA DL.
                                          UNVERIFIED: multiplier cell value not
                                          in the carve listing. NPL: "NPART*6"
                                          - 6 words per partition entry. */
    AD = B.MEMAD + 2;                  /* 057724 024415 LDD ,B 15 ;
                                          057725 171002 SAT 2 ;
                                          057726/057727 32-bit add w/ carry
                                          source = controlRecord + 2 words
                                          (skip header + checksum area;
                                          exact 2-word header layout
                                          UNVERIFIED) */
    X = B.CMAD1; T = B.CMAD2;          /* 057730 054425 LDX ,B 25 ;
                                          057731 050426 LDT ,B 26 */
    MOVEW(src=AD, dst=T:X, count=L);   /* 057732 143110 MOVEW
                                          NPL: "*MOVPP % MOVE TO BUFFER AREA".
                                          UNVERIFIED: exact register-operand
                                          convention of this MOVEW variant. */

    /* publish data-area size + status into the caller's buffer */
    T = B.CMAD1; X = B.CMAD2;          /* 057733/057734 */
    AD = phys[CMAD + 040];             /* 057735 143342 LDDTX disp 40
                                          NPL: "*LDDTX 40 % RETURN DATA AREA
                                          SIZE" */
    X = B.SLINK;                       /* 057736 054404 LDX ,B 4 */
    T = X.MEMA1; X = X.MEMA2;          /* 057737/057740: caller buffer ptr */
    phys[callerBuf + 010] = AD;        /* 057741 143316 STDTX disp 10
                                          data-area size written at word
                                          offset 10B of the caller buffer
                                          (same offset the last-block LBA was
                                          read from in 5b) */
    UHLIM = AD;                        /* 057742 020776 STD ,B -2
                                          NPL: "AD=:UHLIM" - arms the
                                          whole-disk bounds check (section 3)
                                          for all subsequent transfers */
    phys[callerBuf + 0] = 036;         /* 057743 170436 SAA 36 ;
                                          057744 143304 STATX disp 0
                                          NPL: "36; *STATX 00" - status word
                                          36B written into the buffer */
    A = X.HSTAT;                       /* 057745/057746: LDX ,B 4 ; LDA ,X 10 */

success_tail:
    T = 0;                             /* 057747 146106 RADD CLD 0 DT
                                          *** T := 0 = OK (SUCCESS) *** */

    /* RETOP terminate (057750-057755): unlink op, SUNOP--, final return
       with the verdict in T */
```

## 8. SCSID dispatch (062217) - VERIFIED (context)

The disk layer's `CALL SCSID` lands at 062217 (`062217 006010 STA ,X 10`,
VERIFIED dd), which after a busy gate dispatches by `fn = ABFUN & 077`:

```c
SCSID(fn):
    if (busy) queue_and_wait();        /* 062243-062261 */
    switch (fn) {
        case 037: DOEXS();             /* 062232 -> ptr 063460 */
        case 031: BDRST();             /* 062303 -> ptr 063522 */
        case 074: GUSCB();             /* 062306 -> ptr 063431 */
        case 042:
        case 036: INQUI();             /* 062312/062315 -> ptr 062613 (VERIFIED dd)
                                          INQUIRY + READ CAPACITY; sets SUTYP
                                          device type (062655) and finally
                                          SETS 5SCIN "init done" (063102
                                          174275 BSET ONE 70 DA, VERIFIED dd) */
        case 023:
        case 025: MODES();             /* -> ptr 063244 */
        default:  /* fn 0/1/4/...: real transfer */
                  CACOB();             /* 062326 -> ptr 063750 build CDB;
                                          re-checks 5SCIN (063753) and diverts
                                          to INQUI if clear */
                  EXCOM();             /* 062327 -> ptr 063403 execute */
                  /* -> SCLLD 067160 (driver enqueue, IP-P2-SCSI-DRIV) */
    }
```

## 9. What the algorithm computes (summary)

For a function-42 (ENTER-DIRECTORY / mount) request the disk layer:

1. bounds-checks the request and queues it (T := 4 ILAOP / 5 BADPA on failure);
2. forces device re-init by clearing 5SCIN, reads the geometry (fn 42 init
   read, 100B blocks into the caller buffer), and gates on INQUIRY device
   type == 0 disk (T := 1 TYPER otherwise);
3. takes the 32-bit double word at word offset 10B of the geometry data as the
   **control-record LBA** (live-verified: 0xEFFE = the last block, by design),
   programs a 1-block transfer at that LBA, and advances the 32-bit buffer
   pointer by 512 words (001000B, VERIFIED at cell 057634) so the control
   record lands after the geometry block;
4. calls SCSID and retries the whole transfer while the masked status is
   6 (UNIT ATTENTION) or 13B (ABORTED), bounded by the TACOU counter
   (live-verified: this retry does NOT fire in the failing mount - status
   reads 0);
5. in FINEX, XORs all 512 words of the control-record block and requires
   xorChecksum == 0 AND 2 < NPART <= 10 (NCOPA); failure returns
   T := 11B NOCRC ("no valid control record", carved word 057717 171011);
6. on success copies NPART * 6 (NPL:) words of partition table from
   controlRecord+2 to the command area, writes the data-area size into the
   caller buffer at offset 10B, arms UHLIM with it, writes marker 36B into
   buffer word 0, and returns T := 0 OK (carved word 057747 146106).
