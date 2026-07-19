/* ===========================================================================
 *  016B-RELIS   ->  MON 60 subfunction RELIS = 16B (0x0E = 14 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): RELEASE ND-500 PROCESS.  Handler: 5NOPAR.
 *  Two call sites: 001126 (routine 001072) and 010322 (interpreter case).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  Neither site stores a parameter into the gateway block - RELIS is issued
 *  with no explicit MON 60 parameters (5NOPAR computes the process itself).
 * ===========================================================================
 */

extern int MON60_RELIS(void);   /* thunk 146362 (SAA 16) -> gateway 146244       */

/* --- 001126 (routine 001072) --------------------------------------------- */
void release_process_A(void)
{
    /* reached as success-continuation of the 001124 call; no block params set */
    if (MON60_RELIS() == ERROR)  /* 001126 JPL I 17 -> thunk 146362              */
        goto loop_001076;        /* 001127 callsite+1 = ERROR -> loop head 001076 */
    /* 001130 callsite+2 = SUCCESS: SAA 1 ...                                   */
}

/* --- 010322 (case inside interpreter 002662) ----------------------------- */
void cmd_release_process(void)
{
    flag_010273 = 1;             /* 010320 SAA 1 ; 010321 STA I -26 (NOT the block) */
    if (MON60_RELIS() == ERROR)  /* 010322 JPL I 172 -> thunk 146362             */
        error_007500();          /* 010323 callsite+1 = ERROR -> ptr 010256=007500 */
    /* 010324 callsite+2 = SUCCESS (next case)                                  */
}
