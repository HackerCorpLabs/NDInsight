/* 065B-HISTN -> MON 60 HISTN = 65B (0x35 = 53 dec). Purpose: READ HISTOGRAM (IREAHIST).
 * Call site 040437 inside routine 040422 (framesize 000243). OCTAL, BANK 1, base 0. */
extern int  MON60_HISTN(void);        /* JPL I ->040622; thunk 146555; SAA 65 */
extern word *p;
int read_histogram(void)
{
    p[6] = fdesc;                     /* 040435 LDX ,B -176; 040436 STF ,X 6 (F = 3 words) */
    if (MON60_HISTN() == ERROR)       /* 040437 JPL I 163 -> thunk 146555 (65B) */
        return LEAVE_error(A);        /* 040440 callsite+1 -> 177327 LEAVE-with-value */
    /* 040441 callsite+2 = SUCCESS: consume returned histogram data */
}
