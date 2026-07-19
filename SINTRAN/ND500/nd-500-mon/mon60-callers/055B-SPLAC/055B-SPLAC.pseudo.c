/* 055B-SPLAC -> MON 60 SPLAC = 55B (0x2D = 45 dec). Purpose: START-PLACE (ISPLACE).
 * Two call sites, each the ELSE arm of a SPLAC/SRESPL(140B) branch, in routines
 * 043011 and 062257. OCTAL, BANK 1, base 0. */
extern int  MON60_SPLAC(void);        /* JPL I -> thunk 146530; SAA 55 */
extern int  MON60_SRESPL(void);       /* JPL I -> thunk 146737; SAA 40 (140B) */
extern void err_043321(void), err_062446(void);
void place_site1(void)                /* 043545..043554 in routine 043011 */
{
    if (flag_B163 != 0) {             /* 043545 LDA ,B -163; 043546 JAZ 4 */
        if (MON60_SRESPL() == ERROR)  /* 043547 SRESPL (140B) */
            err_043321();             /* 043550 */
    } else {
        if (MON60_SPLAC() == ERROR)   /* 043552 JPL I 124 -> thunk 146530 (55B) */
            err_043321();             /* 043553 callsite+1 = ERROR */
    }
    /* 043554 callsite+2 = SUCCESS: continue placement */
}
void place_site2(void)                /* 063060..063067 in routine 062257 */
{
    if (flag_B165 != 0) {             /* 063060 LDA ,B -165; 063061 JAZ 4 */
        if (MON60_SRESPL() == ERROR)  /* 063062 SRESPL (140B) */
            err_062446();
    } else {
        if (MON60_SPLAC() == ERROR)   /* 063065 JPL I 65 -> thunk 146530 (55B) */
            err_062446();             /* 063066 callsite+1 = ERROR */
    }
    /* 063067 callsite+2 = SUCCESS */
}
