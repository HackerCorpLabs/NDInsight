/* ===========================================================================
 *  GSGTE   ->  MON 60 subfunction 072B = 0x3A = 58 dec
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose : READ A PHYS.SEGMENT TABLE ENTRY FROM SYS.MON   (server 5NOPAR)
 *  Call site 110447, inside routine 110365 (framesize 002250).
 *  Every fact is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_GSGTE(void);   /* JPL I 122 @110447 -> ptr 110571 = thunk 146574 */
                                /*   SAA 72 -> gateway 146244 -> MON 60 146256    */
extern word *p;                 /* gateway frame top (X := B-176); p[6],p[7] = params */

void gsgte_call_110447(void)    /* fragment of routine 110365 */
{
    /* two parameter pointers into this routine's frame; role INFERRED */
    p[6] = (word)&local_at_B_plus_34;   /* 110440 RADD SB DA;110441 AAA 34 ;110443 STA ,X 6 */
    p[7] = (word)&local_at_B_minus_42;  /* 110444 RADD SB DA;110445 AAA -42;110446 STA ,X 7 */

    if (MON60_GSGTE() == ERROR)  /* 110447 JPL I 122 -> thunk 146574 (GSGTE 72B) */
        goto err_110450;         /* 110450 callsite+1 = ERROR  (JPL -16, relative) */
    /* 110451 callsite+2 = SUCCESS: SAA 1 ; routine continues */
}
