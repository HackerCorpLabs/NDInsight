/* 063B-HISTA -> MON 60 HISTA = 63B (0x33 = 51 dec). Purpose: START HISTOGRAM (ISTAHIST).
 * Call site 010440 inside command interpreter 002662, no parameters. OCTAL, BANK 1, base 0. */
extern int  MON60_HISTA(void);        /* JPL I ->010524; thunk 146547; SAA 63 */
extern void err_007500(void);
extern void cmd_loop_010613(void);
void cmd_start_histogram(void)
{
    if (MON60_HISTA() == ERROR)       /* 010440 JPL I 64 -> thunk 146547 (63B); no params */
        err_007500();                 /* 010441 callsite+1 = ERROR */
    cmd_loop_010613();                /* 010442 callsite+2 = SUCCESS */
}
