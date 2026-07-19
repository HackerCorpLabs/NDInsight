/* ===========================================================================
 *  013B-CNCFI   ->  MON 60 subfunction CNCFI = 13B (0x0B = 11 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : standalone ENTER-routine 036374 (framesize 000022).
 *  Purpose (NPL, authoritative): CONNECT FILE.  Server handler: ICONNFI.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_CNCFI(void);   /* 036440 JPL I 34 -> ptr 036474 = thunk 146351;
                                 /*   SAA 13 -> gateway 146244 -> MON 60 146256  */
extern word *p;                 /* p[6..12] = MON 60 params 1..5                 */

void connect_file(void)                 /* routine @036374 */
{
    p[6]  = local_B162;          /* 036422..036424 - param1 (value)              */
    p[7]  = (word)&local_B152;   /* 036425..036427 - param2                      */
    p[10] = local_B157;          /* 036430..036431 - param3 (value)              */
    p[11] = (word)&local_B167;   /* 036432..036434 - param4                      */
    p[12] = (word)&local_B154;   /* 036435..036437 - param5                      */

    if (MON60_CNCFI() == ERROR)  /* 036440 JPL I 34 -> thunk 146351 (CNCFI 13B)  */
        return LEAVE_value();    /* 036441 callsite+1 = ERROR -> ptr 036470 = 177327 */
    /* 036442 callsite+2 = SUCCESS: LDD ,B -167 ...                              */
}
