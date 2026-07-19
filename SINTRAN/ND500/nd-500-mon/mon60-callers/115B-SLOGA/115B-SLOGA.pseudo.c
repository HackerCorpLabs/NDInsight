/* ===========================================================================
 *  SLOGA   ->  MON 60 subfunction 115B = 0x4D = 77 dec
 * ---------------------------------------------------------------------------
 *  Purpose : START PROCESS-LOG-ALL   (server handler ISTLAPR)
 *  Call site 110143, inside the process-logging service routine 110055
 *  (framesize 001137). No input parameters.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_SLOGA(void);   /* JPL I 120 @110143 -> ptr 110263 = thunk 146654 (SAA 115) */

int start_process_log_all(void)  /* fragment of routine 110055 */
{
    if (MON60_SLOGA() == ERROR)  /* 110143 JPL I 120 -> thunk 146654 (SLOGA 115B) */
        goto err_110144;         /* 110144 callsite+1 = ERROR (JPL -54, relative) */
    /* 110145 callsite+2 = SUCCESS: SAA 24 ; routine continues */
    return OK;
}
