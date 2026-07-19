/* ===========================================================================
 *  SPREL  ->  MON 60 subfunction 044B (0x24 = 36 dec)
 *             RELEASE ND-500 CPU/SYSTEM FROM SPECIAL USE
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Handler: CASE inside command interpreter ENTER 002662 (case 007477..007507).
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: (none).
 * ===========================================================================
 */

extern int MON60_SPREL(void);          /* 007505 JPL I 120 -> thunk 146472 (SAA 44) */

void release_special_use(void)         /* case body 007477..007507 */
{
    /* 007477 JMP 6 jumps straight to the call - NO parameters are set */
    if (MON60_SPREL() == ERROR)        /* 007505 MON60 044B (no params) */
        goto err_007500;               /* 007506 JPL -6 -> local error handler */
    goto loop_010613;                  /* 007507 JMP I -100 -> ptr 007376 = 010613 */
}

/* SPREL takes no parameters (PROVEN: 007477 JMP 6 lands directly on the call at
 * 007505 with no ,X-slot stores in between). */
