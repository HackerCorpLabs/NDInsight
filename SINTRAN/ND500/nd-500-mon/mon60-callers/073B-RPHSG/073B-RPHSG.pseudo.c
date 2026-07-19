/* ===========================================================================
 *  RPHSG  ->  MON 60 subfunction 073B (0x3B = 59 dec) READ FROM PHYSICAL SEGMENT
 * ---------------------------------------------------------------------------
 *  Handler : standalone ENTER-routine @ 056042 (framesize 000050).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <phys.segment no.> <address> <no. of bytes> <array>.
 * ===========================================================================
 */

extern int MON60_RPHSG(void);          /* 056407 JPL I 53 -> thunk 146420 (SAA 73) */
extern word *p;

int read_physical_segment(void)        /* @ 056042 */
{
    p[6]  = seglocal + 5;              /* 056372 STA ,X 6  = (B-172)+5 = <phys.segment no.> */
    p[7]  = &address;                  /* 056375 STA ,X 7  = &(B-163)  = <address> */
    p[10] = nbytes;                    /* 056377 STA ,X 10 = value     = <no. of bytes> */
    p[11] = array_desc;                /* 056406 STF ,X 11 = F(B-170)  = <array> (3 words) */

    if (MON60_RPHSG() == ERROR)        /* 056407 MON60 073B */
        goto err_056240;               /* 056410 -> ptr 056240 */
    goto ok_056452;                    /* 056411 -> ptr 056452 */
}

/* 4 params: slots 6/7/10/11 (param3 is a single word, so the 3-word array
 * descriptor lands at 11).  All stores PROVEN. */
