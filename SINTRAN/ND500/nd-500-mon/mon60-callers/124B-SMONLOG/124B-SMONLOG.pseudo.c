/* ===========================================================================
 *  SMONLOG   ->  MON 60 subfunction SMONLOG = 124B (0x54 = 84 dec)
 *  Purpose: START MONITOR CALL LOG.  Server handler ISTAMLOG.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE 007277..007314, INSIDE the command interpreter
 *            ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern dword resolve_002222(fdesc name); /* helper @002222; INFERRED = filename resolve */
extern int   MON60_SMONLOG(void);        /* 007312 JPL I ->007404; thunk 146676 SAA 124 */
extern void  cmd_error_002673(void);     /* interpreter error reporter (PROVEN)  */
extern void  cmd_loop_010613(void);      /* command loop (PROVEN)                */
extern word *p;                          /* gateway slots, p[6]=param1 */

void cmd_start_moncall_log(void)
{
    dword v;                             /* frame local @B-127 */

    /* 007277..007302 : call helper 002222 with the filename descriptor (F@B-113) */
    if (resolve_002222(fname_B113) == ERROR) /* 007302 -> routine 002222 ; 007303 err */
        cmd_error_002673();              /* 007303 callsite+1 -> ptr 007171 = 002673 */
    v = get_D() >> 16;                   /* 007304 SAD SHR 20 ; 007305 STD ,B -127   */

    p[6] = (word)&v;                     /* 007306..007311 : param1 := &local(B-127)  */
    if (MON60_SMONLOG() == ERROR)        /* 007312 -> thunk 146676 (SMONLOG 124B)     */
        cmd_error_002673();              /* 007313 callsite+1 -> ptr 007171 = 002673  */
    cmd_loop_010613();                   /* 007314 callsite+2 -> ptr 007376 = 010613  */
}

/* PROVEN: one MON60 parameter (slot 6) = &local(B-127); the value comes from
 *   helper 002222 (fed the filename descriptor F@B-113) masked by SAD SHR 20.
 * INFERRED: routine 002222 resolves the log file/segment; local(B-127) is the
 *   resulting log-buffer descriptor/size passed to SMONLOG.  Exact roles not
 *   traced.  Server handler ISTAMLOG starts the monitor-call log. */
