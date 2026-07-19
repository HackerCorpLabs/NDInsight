/* ===========================================================================
 *  RLOG    ->  MON 60 subfunction 113B = 0x4B = 75 dec
 * ---------------------------------------------------------------------------
 *  Purpose : READ LOG DATA (PRINT LOG INFO)   (server handler IPRILOG)
 *  Three call sites in the process-logging service routine 110055
 *  (framesize 001137): 110116, 110243, 110310 - each with TWO parameters
 *  (a selector constant + the F register 3-word descriptor).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_RLOG(void);    /* thunk 146646 (SAA 113) at each site */
extern word *p;                   /* p[6]=selector; p[7..11]=F (3 words) */

/* Site 110116 shown; 110243 and 110310 differ only in the selector constant: */
int read_log_data_110116(void)
{
    p[6] = log_selector;           /* 110107 LDA 144 ; 110111 STA ,X 6  (role INFERRED) */
    /* fdescr : F register = 3-word file/buffer descriptor */
    p[7] = fdescr;                 /* 110115 STF ,X 7  (stores F into slots 7/10/11) */

    if (MON60_RLOG() == ERROR)     /* 110116 JPL I 137 -> thunk 146646 (RLOG 113B) */
        goto err_110117;           /* 110117 callsite+1 = ERROR (JPL -27, relative) */
    /* 110120 callsite+2 = SUCCESS: STZ ,B -134 */
    return OK;
}
