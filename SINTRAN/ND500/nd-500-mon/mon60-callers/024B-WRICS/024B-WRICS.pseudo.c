/* ===========================================================================
 *  WRICS  ->  MON 60 subfunction 024B (0x14 = 20 dec) WRITE CONTROL STORE
 * ---------------------------------------------------------------------------
 *  Handler : standalone ENTER-routine @ 123343 (framesize 000035).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <CS addr.> <no of 16 bit words> <data-area>.
 * ===========================================================================
 */

extern int MON60_WRICS(void);          /* 123420 JPL I 64 -> thunk 146412 (SAA 24) */
extern word *p;

int write_control_store(void)          /* @ 123343 */
{
    p[6]  = &cs_addr;                  /* 123412 STA ,X 6  = &(B-162) */
    p[7]  = &word_count;               /* 123415 STA ,X 7  = &(B-164) */
    p[10] = data_area_desc;            /* 123417 STF ,X 10 = F(B-171) */

    if (MON60_WRICS() == ERROR)        /* 123420 MON60 024B */
        return LEAVE_value();          /* 123421 callsite+1 -> 177327 */

    /* 123422 ... SUCCESS continues */
}

/* 3 params in slots 6/7/10, matching yaml.  All stores PROVEN.  Handler IWCNT
 * on the server side (same as func 157). */
