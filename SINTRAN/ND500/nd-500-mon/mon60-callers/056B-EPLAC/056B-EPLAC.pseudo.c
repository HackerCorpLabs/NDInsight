/* 056B-EPLAC -> MON 60 EPLAC = 56B (0x2E = 46 dec). Purpose: END-PLACE (IEPLACE).
 * Two call sites in routines 043011 and 062257, closing the SPLAC/EPLAC bracket.
 * OCTAL, BANK 1, base 0. */
extern int  MON60_EPLAC(void);        /* JPL I -> thunk 146533; SAA 56 */
extern void err_043321(void), err_062446(void);
extern word *p;
void end_place_site1(void)            /* 044055..044064 */
{
    p[6] = fdesc;                     /* 044060 LDX ,B -176; 044061 STF ,X 6 (F = 3 words) */
    if (MON60_EPLAC() == ERROR)       /* 044062 JPL I 35 -> thunk 146533 (56B) */
        err_043321();                 /* 044063 callsite+1 = ERROR */
    /* 044064 callsite+2 = SUCCESS -> 044074 */
}
void end_place_site2(void)            /* 063335..063344 */
{
    p[6] = fdesc;                     /* 063340 LDX ,B -176; 063341 STF ,X 6 (F = 3 words) */
    if (MON60_EPLAC() == ERROR)       /* 063342 JPL I 23 -> thunk 146533 (56B) */
        err_062446();                 /* 063343 callsite+1 = ERROR */
    /* 063344 callsite+2 = SUCCESS */
}
