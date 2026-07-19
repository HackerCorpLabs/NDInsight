/* ===========================================================================
 *  MSTOP  ->  MON 60 subfunction 034B (0x1C = 28 dec) MICRO STOP
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  TWO call sites; MSTOP takes no parameters.
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_MSTOP(void);          /* thunk 146453 (SAA 34) */
extern int MON60_MSTCL(void);          /* thunk 146456 (SAA 35) */

/* Site A @ 006312 -- CASE inside command interpreter ENTER 002662 (006312..006314) */
void mstop_case_interpreter(void)
{
    if (MON60_MSTOP() == ERROR)        /* 006312 MON60 034B (no params) */
        goto err_002673;               /* 006313 -> 002673 */
    goto loop_010613;                  /* 006314 -> 010613 (command loop) */
}

/* Site B @ 122512 -- standalone routine 122507 (framesize 0):
 *   micro-stop then master-clear.  MSTOP success FALLS THROUGH into the MSTCL call. */
int micro_stop_then_master_clear(void) /* @ 122507 */
{
    if (MON60_MSTOP() == ERROR)        /* 122512 MON60 034B (no params) */
        return LEAVE_value();          /* 122513 -> 122521 = 177327 */

    /* 122514 SUCCESS: continue directly into MON60 MSTCL (035B) -- carved in 035B-MSTCL */
    if (MON60_MSTCL() == ERROR)        /* 122514 MON60 035B (no params) */
        return LEAVE_value();          /* 122515 -> 122521 = 177327 */
    return LEAVE_skip();               /* 122516 -> 122523 = 177335 SUCCESS */
}
