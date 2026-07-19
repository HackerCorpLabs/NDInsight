/* ==========================================================================
 * CHDSI-COMPLETE.pseudo.c
 * SINTRAN III L07 / L-VSX-500 - segment 006-S3FS (load base 26000B)
 *
 * Readable reconstruction of the directory master-block (extended-info)
 * processing. Behaviour is transcribed 1:1 from the carved bytes in
 * CHDSI-COMPLETE.ASM; C control flow mirrors the ND-100 skip/branch logic.
 *
 * The "extended-info" / master block is 8 words at the front of directory
 * page 0 (disk words 1750B..1757B):
 *     word 0   additive checksum of words 1..7
 *     word 1..3  (directory identity / dates - not touched here)
 *     word 4   flag word;  bit15 (170 octal field) = "entered"
 *     word 5   owner system number
 *     word 6..7  capacity = pages available (32-bit, big-endian)
 *
 * Return convention: OK  -> caller L+1 (skip),  ERROR -> caller L, with the
 * status word left in frame local 2. In C we model that as a status_t.
 * ========================================================================== */

typedef short          word;   /* 16-bit */
typedef unsigned short u16;
typedef struct { word w[8]; } extinfo;   /* the 8-word master block */

/* status_t: 0 == OK (skip return). Non-zero == the value stored in local 2. */
typedef int status_t;
#define OK  0

/* ---- resident / cross-segment routines (OPEN boundary - not in 006-S3FS) --
 * 003752  prologue        003776  epilogue
 * 050323  get param/datafield + resource descriptor (returns option word in A)
 * 001224  block copy       047365  error handler       037101 geometry cap
 * 050124 / 050223 / 050226 / 037565  capacity-adjust helpers                */
extern word     resident_get_param(word *ok);        /* 050323 */
extern void     geometry_capacity(u16 *hi, u16 *lo);  /* 037101 */
extern status_t cap_adjust1(word x);                  /* 050124 */
extern status_t cap_adjust2(word x);                  /* 050223 */
extern status_t cap_adjust3(u16 cap, word l17);       /* 050226 */
extern status_t inseg_helper(void);                   /* 037565 */

/* ---- disk-cache primitives (RCBLO / WCBLO / CL1DB carved in 006-S3FS) ---- */
extern status_t RCBLO(long block, word **buf);   /* 35766B reserve+read buf  */
extern status_t WCBLO(long block, word *buf);    /* 36357B WRITE cache block */
extern void     CL1DB(void);                     /* 35240B release buffer    */
extern status_t RXDIR(word *df, extinfo *out);   /* 37643B read blk0 -> 8w   */

#define ENTERED_BIT   0100000    /* bit15, "170 DA" field                    */
#define ERR_XFER_35B  035        /* master block transfer error (WXDIR)      */

/* --------------------------------------------------------------------------
 * WXDIR 37702B - recompute checksum, store, WRITE block 0 back.
 * -------------------------------------------------------------------------- */
status_t WXDIR(extinfo *blk)
{
    /* checksum recompute loop 037714-037721: plain 16-bit additive sum */
    u16 sum = 0;
    for (int i = 1; i < 8; i++)          /* words 1..7 */
        sum += (u16)blk->w[i];
    blk->w[0] = (word)sum;               /* 037723 STA ,X 0 : store checksum */

    word *buf;
    CL1DB();                             /* 037727 release/flush (OPEN edge) */
    if (RCBLO(0, &buf) != OK)            /* 037730 reserve cache buf, block 0 */
        goto xfer_error;                 /* 037731 */

    copy_words(buf + 23, (word *)blk, 8);/* 037733-037736 block copy IN      */
    if (WCBLO(0, buf) != OK)             /* 037741 WRITE block 0 back        */
        goto xfer_error;                 /* 037742 */

    return OK;                           /* 037743 MIN ,B 4 (success)        */

xfer_error:
    resident_error(047365);              /* 037746 */
    return ERR_XFER_35B;                 /* 037747 SAA 35 ; 037750 STA ,B 2  */
}

/* --------------------------------------------------------------------------
 * CHDSI 37763B - check / enter directory.
 *   df       = directory datafield (from GDIRA, via ENDIR)
 *   enter_sys = entering system number (ENDIR passes it in T; here local 34/50)
 * -------------------------------------------------------------------------- */
status_t CHDSI(word *df, word enter_sys)
{
    /* 037767 prologue (003752). 037770 param helper (050323): returns an
     * option/resource word; a failure takes the error exit.                */
    word ok;
    word opt = resident_get_param(&ok);  /* 037770 JPL I 151 -> 050323      */
    if (!ok)                             /* 037771 JMP 144 -> 040135        */
        return opt;                      /* propagate helper A              */
    /* local 16 := opt (037772). */

    /* 037773 BSKP ZRO 100 DA : bit6 of opt = resource-reserve bit.
     * If SET -> early SUCCESS return, WITHOUT touching page 0.             */
    if (opt & 0100)                      /* bit6 set */
        return OK;                       /* 037774 JMP 135 -> 040131 (MIN ,B4) */

    /* 040000: read directory block 0 into the 8-word master block. */
    extinfo blk;
    if (RXDIR(df, &blk) != OK)           /* 040000 JPL I 143 -> 037643 RXDIR */
        return /*callee A*/ RXDIR_status;/* 040001 JMP 134 -> 040135         */

    /* --- checksum recompute + compare (040002-040021) --- */
    u16 sum = 0;
    for (int i = 1; i < 8; i++)          /* words 1..7 */
        sum += (u16)blk.w[i];
    if (sum != (u16)blk.w[0] || sum == 0)/* mismatch OR zero */
        goto rebuild;                    /* 040020 / 040021 -> 040063        */

    /* --- GOOD checksum: capacity compare vs device geometry (040022-) --- */
    u16 ghi, glo;
    if (geometry_capacity_or_err(&ghi, &glo) != OK)  /* 040023 -> 037101   */
        return callee_A;                 /* 040024 -> 040135                 */
    u16 shi = (u16)blk.w[6], slo = (u16)blk.w[7];     /* stored capacity     */
    if (shi == ghi && slo == glo)        /* 040030-040033 */
        goto interlock;                  /* capacity matches -> owner check  */

    /* capacity mismatch -> resident adjust sequence (040034-040061).
     * Any sub-step failure -> error exit (propagate A or generic 003204).  */
    if (cap_adjust1(2)      != OK) return callee_A;           /* 040036 */
    word l17 = /*040040 AND*/ (last_A & 0000013);            /* 040040-041 */
    if (cap_adjust2(11)     != OK) return callee_A;           /* 040044 */
    if (last_A & 0000020)  return 03204 /*generic*/;          /* 040046-047 */
    if (inseg_helper()      != OK) return callee_A;           /* 040050 */
    if (cap_adjust3(cap, l17) != OK) return callee_A;         /* 040060 */
    goto interlock;                                            /* 040062 */

rebuild:  /* --- BAD / zero checksum: SELF-HEAL, do NOT reject (040063-) --- */
    for (int i = 0; i < 8; i++)          /* 040063-040071 */
        blk.w[i] = 0;                    /* zero the whole 8-word block      */
    {
        u16 ghi2, glo2;
        if (geometry_capacity_or_err(&ghi2, &glo2) != OK)  /* 040075 -> 037101 */
            return callee_A;             /* 040076 -> 040135                 */
        blk.w[6] = (word)ghi2;           /* 040077 STD ,X 6 : write capacity */
        blk.w[7] = (word)glo2;
    }
    /* fall through to interlock */

interlock: /* --- OWNER INTERLOCK (join 040100) --- */
    /* Two overrides first, then the cross-system check. */
    if (opt & 0000010)                   /* 040102-040104 bit3 = force/override */
        goto stamp;
    if (enter_sys == 0)                  /* 040105-040107 system 0 = no interlock */
        goto stamp;
    if (!(blk.w[4] & ENTERED_BIT))       /* 040110-040112 not yet entered */
        goto stamp;
    if (blk.w[5] == 0)                   /* 040113-040114 unowned */
        goto stamp;
    if ((u16)blk.w[5] == (u16)enter_sys) /* 040115-040116 same owner re-enters */
        goto stamp;
    /* REJECT: entered AND owned AND owner != entering system. */
    return 03203;                        /* 040117 LDA 37 (=003203 sentinel) */
                                         /* 040120 -> 040135 error exit      */

stamp: /* --- STAMP owner + entered bit, then write back (040121-) --- */
    blk.w[5] = enter_sys;                /* 040122 owner := entering system  */
    blk.w[4] |= ENTERED_BIT;             /* 040124 set bit15 "entered"       */
    {
        status_t s = WXDIR(&blk);        /* 040127 recompute cksum + write   */
        if (s != OK)                     /* 040130 -> 040135 (s == 35B)      */
            return s;
    }
    return OK;                           /* 040131 MIN ,B 4 (success)        */
}

/* --------------------------------------------------------------------------
 * REENB 40162B - release directory (mirror of the stamp).
 * -------------------------------------------------------------------------- */
status_t REENB(word *df, word enter_sys)
{
    word ok;
    word opt = resident_get_param(&ok);  /* 040167 -> 050323 */
    if (!ok) return opt;                 /* 040170 */
    if (opt & 0100) return OK;           /* 040171-040172 resource bit -> early OK */

    extinfo blk;
    if (RXDIR(df, &blk) != OK)           /* 040176 -> 037643 read block 0 */
        return callee_A;                 /* 040177 */

    blk.w[4] &= ~ENTERED_BIT;            /* 040201 BSET ZRO 170 : CLEAR bit15 */

    status_t s = WXDIR(&blk);            /* 040203 write back */
    if (s != OK) return s;               /* 040204 */
    return OK;                           /* 040205 MIN ,B 4 */
}
