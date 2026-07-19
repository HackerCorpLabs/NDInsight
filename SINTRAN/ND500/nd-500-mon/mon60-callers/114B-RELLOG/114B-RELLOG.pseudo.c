/* ===========================================================================
 *  RELLOG  ->  MON 60 subfunction 114B = 0x4C = 76 dec
 * ---------------------------------------------------------------------------
 *  Purpose : STOP LOGGING AND RELEASE LOGGING FACILITY   (server handler IRELLOG)
 *  Call site 006676, inside the main command interpreter routine 002662
 *  (framesize 000331). No input parameters.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_RELLOG(void);  /* JPL I 71 @006676 -> ptr 006767 = thunk 146651 (SAA 114) */
extern void  cmd_error_002673(void);
extern void  cmd_loop_010613(void);

void cmd_release_log(void)
{
    if (MON60_RELLOG() == ERROR)   /* 006676 JPL I 71 -> thunk 146651 (RELLOG 114B) */
        cmd_error_002673();        /* 006677 callsite+1 = ERROR  -> 002673 */
    cmd_loop_010613();             /* 006700 callsite+2 = SUCCESS -> 010613 */
}
