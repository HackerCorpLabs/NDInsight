/* ===========================================================================
 *  004B-WPROG   ->  MON 60 subfunction WPROG = 4B (0x04 = 4 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : standalone ENTER-routine 055255 (framesize 000302).
 *  Purpose (NPL, authoritative): LOGICAL PROGRAM MEMORY WRITE.  Handler: IPMWRITE.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_WPROG(void);   /* 055702 JPL I 124 -> ptr 056026 = thunk 146321;
                                 /*   SAA 4 -> gateway 146244 -> MON 60 146256   */
extern word *p;                 /* p[6],p[7],p[10] = MON 60 params 1..3          */

void write_prog_mem(void)               /* routine @055255 */
{
    p[6]  = (word)&local_B170;   /* 055656..055661 - logical PM address (INFERRED) */
    p[7]  = (word)&local_B166;   /* 055662..055664 - count (INFERRED)            */
    p[10] = f_register_B155;     /* 055665/055701 STF ,X 10 (3-word source data) */

    if (MON60_WPROG() == ERROR)  /* 055702 JPL I 124 -> thunk 146321 (WPROG 4B)  */
        return LEAVE_value();    /* 055703 callsite+1 = ERROR -> ptr 056027 = 177327 */
    /* 055704/055705 callsite+2 = SUCCESS -> JMP 120 -> 056025                   */
}
