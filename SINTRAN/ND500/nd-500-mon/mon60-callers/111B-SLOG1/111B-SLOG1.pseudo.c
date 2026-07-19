/* ===========================================================================
 *  SLOG1   ->  MON 60 subfunction 111B = 0x49 = 73 dec
 * ---------------------------------------------------------------------------
 *  Purpose : START PROCESS LOG ONE   (server handler ISTAPRLOG)
 *  Call site 110161, inside the process-logging service routine 110055
 *  (framesize 001137).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_SLOG1(void);   /* JPL I 105 @110161 -> ptr 110266 = thunk 146643 (SAA 111) */
extern word *p;

int start_process_log_one(void)   /* fragment of routine 110055 */
{
    /* proc : process identifier at frame offset B-171 (role INFERRED) */
    p[6] = (word)&proc;            /* 110155 RADD SB DA;110156 AAA -171;110160 STA ,X 6 */

    if (MON60_SLOG1() == ERROR)    /* 110161 JPL I 105 -> thunk 146643 (SLOG1 111B) */
        goto err_110162;           /* 110162 callsite+1 = ERROR (JPL -72, relative) */
    /* 110163 callsite+2 = SUCCESS: SAA 15 ; routine continues */
    return OK;
}
