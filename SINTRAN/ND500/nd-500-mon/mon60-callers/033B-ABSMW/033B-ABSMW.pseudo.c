/* ===========================================================================
 *  ABSMW  ->  MON 60 subfunction 033B (0x1B = 27 dec) PHYSICAL DATA MEMORY WRITE
 * ---------------------------------------------------------------------------
 *  Handler : standalone ENTER-routine @ 055255 (framesize 000302).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <no. of bytes> <ND-500 addr.> <data area>.
 * ===========================================================================
 */

extern int MON60_ABSMW(void);          /* 055717 JPL I 112 -> thunk 146431 (SAA 33) */
extern word *p;

int absolute_memory_write(void)        /* within routine @ 055255 */
{
    p[6]  = &nbytes;                   /* 055711 STA ,X 6  = &(B-170) = <no. of bytes> */
    p[7]  = &nd500_addr;               /* 055714 STA ,X 7  = &(B-166) = <ND-500 addr.> */
    p[10] = data_area_desc;            /* 055716 STF ,X 10 = F(B-155) = <data area> (3 words) */

    if (MON60_ABSMW() == ERROR)        /* 055717 MON60 033B */
        return LEAVE_value();          /* 055720 -> 056027 = 177327 */
    /* 055721 JMP 104 -> 056025 -> 056041 = 177335 */
    return LEAVE_skip();               /* SUCCESS */
}

/* 3 params in slots 6/7/10, matching yaml.  All stores PROVEN. */
