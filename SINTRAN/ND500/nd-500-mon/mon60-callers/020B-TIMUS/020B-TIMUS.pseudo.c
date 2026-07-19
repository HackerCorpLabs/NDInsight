/* ===========================================================================
 *  020B-TIMUS   ->  MON 60 subfunction TIMUS = 20B (0x10 = 16 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : CASE 005162..005164 inside the command interpreter ENTER-routine
 *            that begins at 002662 (framesize 000331).
 *  Purpose (NPL, authoritative): (time used).  Server handler: 5NOPAR.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  No parameter is stored into the gateway block for this call.
 * ===========================================================================
 */

extern int  MON60_TIMUS(void);       /* 005162 JPL I 60 -> thunk 146370          */
extern void cmd_error_002673(void);  /* shared error reporter (ptr 005017)       */
extern void cmd_loop_010613(void);   /* command loop (ptr 005232)                */

void cmd_time_used(void)                /* case @005162 */
{
    if (MON60_TIMUS() == ERROR)  /* 005162 JPL I 60 -> thunk 146370 (TIMUS 20B)  */
        cmd_error_002673();      /* 005163 callsite+1 = ERROR -> ptr 005017=002673 */
    cmd_loop_010613();           /* 005164 callsite+2 = SUCCESS -> ptr 005232=010613 */
}
