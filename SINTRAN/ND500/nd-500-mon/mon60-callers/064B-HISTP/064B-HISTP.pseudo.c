/* 064B-HISTP -> MON 60 HISTP = 64B (0x34 = 52 dec). Purpose: STOP HISTOGRAM (ISTOHIAT).
 * Call site 010435 inside command interpreter 002662, no parameters. OCTAL, BANK 1, base 0. */
extern int  MON60_HISTP(void);        /* JPL I ->010523; thunk 146552; SAA 64 */
extern void err_007500(void);
extern void cmd_loop_010613(void);
void cmd_stop_histogram(void)
{
    if (MON60_HISTP() == ERROR)       /* 010435 JPL I 66 -> thunk 146552 (64B); no params */
        err_007500();                 /* 010436 callsite+1 = ERROR */
    cmd_loop_010613();                /* 010437 callsite+2 = SUCCESS */
}
