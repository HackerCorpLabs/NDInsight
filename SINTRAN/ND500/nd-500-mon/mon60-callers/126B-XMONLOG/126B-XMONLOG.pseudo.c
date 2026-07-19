/* ===========================================================================
 *  XMONLOG   ->  MON 60 subfunction XMONLOG = 126B (0x56 = 86 dec)
 *  Purpose: STOP AND RELEASE MONCALL LOG.  Server handler ISTOMLOG.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE 007320..007322, INSIDE the command interpreter
 *            ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_XMONLOG(void);  /* 007320 JPL I ->007406; thunk 146704 SAA 126 */
extern void cmd_error_002673(void);/* interpreter error reporter (PROVEN)         */
extern void cmd_loop_010613(void); /* command loop (PROVEN)                        */

void cmd_stop_release_moncall_log(void)
{
    /* PROVEN: no MON60 parameter slots are written before 007320. */
    if (MON60_XMONLOG() == ERROR)    /* 007320 -> thunk 146704 (XMONLOG 126B)      */
        cmd_error_002673();          /* 007321 callsite+1 -> ptr 007171 = 002673    */
    cmd_loop_010613();               /* 007322 callsite+2 -> ptr 007376 = 010613    */
}

/* PROVEN: XMONLOG takes no marshalled parameters (consistent with a stop/release
 *   action).  Server handler ISTOMLOG stops and releases the moncall log. */
