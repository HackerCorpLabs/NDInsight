/* ===========================================================================
 *  MICST  ->  MON 60 subfunction 025B (0x15 = 21 dec) MICRO START
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  FOUR call sites, all resolving to thunk 146415 (SAA 25).
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <micro program start address>.
 * ===========================================================================
 */

extern int MON60_MICST(void);
extern word *p;

/* Site A @ 006307 -- a CASE inside the command interpreter ENTER 002662 */
void micst_case_interpreter(void)      /* case body 006277..006311 */
{
    dword start = get_num_param();     /* 006300 JPL I -144 -> helper 002003 */
    startlocal = start;                /* 006302 STD ,B 105 */
    p[6] = &startlocal;                /* 006306 STA ,X 6 = &(B+105) (by pointer) */
    if (MON60_MICST() == ERROR)        /* 006307 MON60 025B */
        goto err_002673;               /* 006310 -> 002673 */
    goto loop_010613;                  /* 006311 -> 010613 (command loop) */
}

/* Site B @ 130130 (routine 127551) -- start address by VALUE */
void micst_B(void)
{
    p[6] = MICRO_START_CONST_B;        /* 130125 LDA 43 (P-rel) ; 130127 STA ,X 6 */
    if (MON60_MICST() == ERROR)        /* 130130 MON60 025B */
        goto frame_dispatch;           /* 130131 JMP I ,B -141 */
    /* 130132 SUCCESS fall-through */
}

/* Site C @ 130361 (routine 127551) -- start address by VALUE */
void micst_C(void)
{
    p[6] = MICRO_START_CONST_C;        /* 130356 LDA 102 (P-rel) ; 130360 STA ,X 6 */
    if (MON60_MICST() == ERROR)        /* 130361 MON60 025B */
        goto frame_dispatch;           /* 130362 JMP I ,B -127 */
    /* 130363 SUCCESS fall-through */
}

/* Site D @ 131140 (routine 130475) -- start address by POINTER */
void micst_D(void)
{
    startlocal = MICRO_START_CONST_D;  /* 131132 LDD 171 ; 131133 STD ,B -66 */
    p[6] = &startlocal;                /* 131137 STA ,X 6 = &(B-66) */
    if (MON60_MICST() == ERROR)        /* 131140 MON60 025B */
        goto handler_131107;           /* 131141 JPL I -32 -> 131107 */
    /* 131142 SUCCESS fall-through */
}

/* NOTE (PROVEN, differs between sites): site A and site D pass a POINTER to the
 * start address (&(B+105), &(B-66)); sites B and C pass the address BY VALUE in
 * ,X 6.  The yaml documents one operand "<micro program start address>"; the
 * pointer-vs-value distinction is read from the bytes, not assumed. */
