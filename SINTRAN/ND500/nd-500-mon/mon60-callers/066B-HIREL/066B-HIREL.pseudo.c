/* 066B-HIREL -> MON 60 HIREL = 66B (0x36 = 54 dec). Purpose: STOP AND RELEASE HISTOGRAM (IRELHIST).
 * Two call sites: 010443 (interpreter 002662) and 110130 (routine 110055). OCTAL, BANK 1, base 0. */
extern int  MON60_HIREL(void);        /* JPL I -> thunk 146560; SAA 66 */
extern void err_007500(void), err_110070(void);
extern void cmd_loop_010613(void);
void cmd_release_histogram(void)      /* site 1: 010443..010445 */
{
    if (MON60_HIREL() == ERROR)       /* 010443 JPL I 62 -> thunk 146560 (66B); no params */
        err_007500();                 /* 010444 callsite+1 = ERROR */
    cmd_loop_010613();                /* 010445 callsite+2 = SUCCESS */
}
int release_histogram_site2(void)     /* site 2: 110125..110132 */
{
    global_via_ptr131 = A_from_B171;  /* 110125..110127 STA I 131 (indirect store to a global) */
    if (MON60_HIREL() == ERROR)       /* 110130 JPL I 131 -> thunk 146560 (66B); no gateway params */
        err_110070();                 /* 110131 callsite+1 -> 110070 (local error handler) */
    /* 110132 callsite+2 = SUCCESS */
}
