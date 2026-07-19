/* ===========================================================================
 *  RPROC   ->  MON 60 subfunction 077B = 0x3F = 63 dec
 * ---------------------------------------------------------------------------
 *  Purpose : READ MESSAGE   (server handler IRMESS; RPROC is the yaml client name)
 *  Six call sites, each with TWO parameters (a P-relative constant selector and a
 *  frame buffer pointer, slot order varies):
 *    001332, 001510, 001545, 001625 (routine 001264, framesize 000153),
 *    007531 (main interpreter 002662), 104456 (routine 103722).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_RPROC(void);   /* thunk 146613 (SAA 77) at each site */
extern word *p;                   /* gateway frame top; p[6],p[7] = params */

/* Site 001332 shown (const in slot 1, buffer in slot 2): */
int read_message_001332(word *rxbuf /* @B-170 */)
{
    p[6] = const_selector;         /* 001324 LDA 142 ; 001326 STA ,X 6  (selector, role INFERRED) */
    p[7] = (word)rxbuf;            /* 001327 RADD SB DA;001330 AAA -170;001331 STA ,X 7 */

    if (MON60_RPROC() == ERROR)    /* 001332 JPL I 135 -> thunk 146613 (RPROC 77B) */
        goto err_001333;           /* 001333 callsite+1 = ERROR */
    /* 001334 callsite+2 = SUCCESS: LDA ,B -152 (uses result) */
    return OK;
}

/* Site 104456 reverses the order: p[6] = &buffer(B-125), p[7] = const (LDA 106). */
