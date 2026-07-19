/* ===========================================================================
 *  WPHSG  ->  MON 60 subfunction 110B (0x48 = 72 dec) WRITE INTO PHYSICAL SEGMENT
 * ---------------------------------------------------------------------------
 *  Handler : standalone ENTER-routine @ 055255 (framesize 000302).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <segm no.> <ND-500 address> <no. of bytes> <data area>.
 * ===========================================================================
 */

extern int MON60_WPHSG(void);          /* 055736 JPL I 74 -> thunk 146423 (SAA 110) */
extern word *p;

int write_physical_segment(void)       /* within routine @ 055255 */
{
    p[6]  = seglocal + 5;              /* 055725 STA ,X 6  = (B-172)+5 = <segm no.> */
    p[7]  = &nd500_addr;               /* 055730 STA ,X 7  = &(B-166)  = <ND-500 address> */
    p[10] = &nbytes;                   /* 055733 STA ,X 10 = &(B-170)  = <no. of bytes> */
    p[11] = data_area_desc;            /* 055735 STF ,X 11 = F(B-155)  = <data area> (3 words) */

    if (MON60_WPHSG() == ERROR)        /* 055736 MON60 110B */
        return LEAVE_value();          /* 055737 -> 056027 = 177327 */
    /* 055740 JMP 65 -> 056025 -> 056041 = 177335 */
    return LEAVE_skip();               /* SUCCESS */
}

/* 4 params: slots 6/7/10/11.  param3 is a single word (&nbytes), so the 3-word
 * data-area descriptor lands at 11.  All stores PROVEN. */
