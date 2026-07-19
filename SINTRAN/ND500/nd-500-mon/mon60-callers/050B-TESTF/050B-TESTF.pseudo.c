/* 050B-TESTF -> MON 60 TESTF = 50B (0x28 = 40 dec). Purpose: (test function), handler 5NOPAR.
 * Two call sites in command interpreter ENTER-routine 002662. OCTAL, BANK 1, base 0. */
extern int  MON60_TESTF(void);        /* JPL I -> thunk 146511; SAA 50 */
extern void err_007500(void);         /* internal error/abort helper (PROVEN target, role INFERRED) */
extern void cmd_loop_010613(void);
extern word *p;
void cmd_testf_site1(void)            /* 007725..007742 */
{
    p[6] = param1_a;                  /* 007734 STA ,X 6 */
    p[7] = &frame_B127;               /* 007737 STA ,X 7 = &(B-127) */
    if (MON60_TESTF() == ERROR)       /* 007740 JPL I 106 -> thunk 146511 */
        err_007500();                 /* 007741 callsite+1 = ERROR */
    cmd_loop_010613();                /* 007742 callsite+2 = SUCCESS */
}
void cmd_testf_site2(void)            /* 007751..007764 */
{
    p[6] = &frame_B127;               /* 007756 STA ,X 6 = &(B-127) */
    p[7] = &frame_B127;               /* 007761 STA ,X 7 = &(B-127) */
    if (MON60_TESTF() == ERROR)       /* 007762 JPL I 64 -> thunk 146511 */
        err_007500();                 /* 007763 callsite+1 = ERROR */
    cmd_loop_010613();                /* 007764 callsite+2 = SUCCESS */
}
