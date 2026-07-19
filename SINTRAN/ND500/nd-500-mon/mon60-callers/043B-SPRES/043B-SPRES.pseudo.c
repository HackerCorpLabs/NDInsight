/* ===========================================================================
 *  SPRES  ->  MON 60 subfunction 043B (0x23 = 35 dec)
 *             RESERVE ND-500 CPU/SYSTEM FOR SPECIAL USE
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Handler: CASE inside command interpreter ENTER 002662 (case 007450..007476).
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_SPRES(void);          /* 007474 JPL I 130 -> thunk 146467 (SAA 43) */
extern word *p;

void reserve_special_use(void)         /* case body 007450..007476 */
{
    /* a word is computed into local B-127 (007467 LDD 133 / 007470 STD ,B -127
       on the JAF-false path) and handed to the ND-500 in slot ,X 6 */
    p[6] = local_B127;                 /* 007471 LDD ,B -127 ; 007473 STD ,X 6 */

    if (MON60_SPRES() == ERROR)        /* 007474 MON60 043B */
        goto err_007434;               /* 007475 JPL -41 -> local error handler */
    goto loop_010613;                  /* 007476 JMP I -100 -> ptr 007376 = 010613 */
}

/* NOTE (discrepancy, recorded as-is): the yaml `60B_N500M.yaml` lists SPRES params
 * as "(none)".  This L-revision binary DOES store one word (local B-127) into the
 * gateway slot ,X 6 immediately before the call (PROVEN, 007473 STD ,X 6).  The
 * meaning of that word is UNKNOWN; do not assume it is ignored server-side. */
