/* ===========================================================================
 *  GPSGE   ->  MON 60 subfunction GPSGE = 102B (0x42 = 66 dec)
 *  Authoritative purpose: STOP ND-500 SYSTEM (ABORT ALL ACTIVE PROCS, RELEASE
 *  MON60 BUFFERS).  Server handler IFORGET.  (NOT a status call.)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE around 007507..007516, INSIDE the command
 *            interpreter ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_GPSGE(void);    /* 007514 JPL I ->007630; thunk 146673 SAA 102 */
extern void cmd_error_007500(void);/* local error handler (PROVEN target)         */
extern void cmd_loop_010613(void); /* command loop (PROVEN target)                */

void cmd_stop_nd500_system(void)
{
    *flag_115 = 1;                   /* 007510 SAA 1 ; 007511 STA I 115  (INFERRED) */
    *flag_114 = 1;                   /* 007512 SAA 1 ; 007513 STA I 114  (INFERRED) */

    if (MON60_GPSGE() == ERROR)      /* 007514 -> thunk 146673 (GPSGE 102B)         */
        cmd_error_007500();          /* 007515 callsite+1 -> 007500                  */
    cmd_loop_010613();               /* 007516 callsite+2 -> ptr 007376 = 010613     */

    /* PROVEN: NO MON60 parameter slots (,X 6/7/10) are written before 007514.
     *   GPSGE is issued with no input parameters - consistent with a global
     *   "stop the whole ND-500 system" action.
     * INFERRED: the two "STA I" stores at 007511/007513 set local flags (e.g.
     *   an abort/stop-requested indicator) before the call; not verified. */
}
