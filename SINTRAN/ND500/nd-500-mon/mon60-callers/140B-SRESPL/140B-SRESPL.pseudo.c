/* ===========================================================================
 *  SRESPL   ->  MON 60 subfunction SRESPL = 140B (0x60 = 96 dec)
 *  NOTE: 140B is UNDOCUMENTED in the NPL source (dispatch 5NOPAR, no FUNCTION=
 *        comment).  'SRESPL' is only the yaml/thunk client label.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Two call sites, each the "flag set" arm of an if/else whose "flag clear" arm
 *  issues SPLAC (055B, START-PLACE).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_SRESPL(void);    /* thunk 146737 SAA 140 (both sites)           */
extern int MON60_SPLAC(void);     /* thunk 146530 SAA 055 (the else arm)         */

/* ---- call site 1 @043547 (routine 043011, framesize 000717) --------------- */
void srespl_site1(void)           /* flag = local(B-163) */
{
    if (flag_B163 != 0) {            /* 043545 LDA ,B -163 ; 043546 JAZ -> 043552 */
        if (MON60_SRESPL() == ERROR) /* 043547 -> thunk 146737 (SRESPL 140B)      */
            err_043321();            /* 043550 -> ptr 043675 = routine 043321      */
        /* 043551 success -> 043554 */
    } else {
        if (MON60_SPLAC() == ERROR)  /* 043552 -> thunk 146530 (SPLAC 055B)       */
            err_043321();            /* 043553 -> ptr 043675 = routine 043321      */
    }
}

/* ---- call site 2 @063062 (routine 062257, framesize 000544) --------------- */
void srespl_site2(void)           /* flag = local(B-165) */
{
    if (flag_B165 != 0) {            /* 063060 LDA ,B -165 ; 063061 JAZ -> 063065 */
        if (MON60_SRESPL() == ERROR) /* 063062 -> thunk 146737 (SRESPL 140B)      */
            err_062446();            /* 063063 -> ptr 063140 = routine 062446      */
        /* 063064 success -> 063067 */
    } else {
        if (MON60_SPLAC() == ERROR)  /* 063065 -> thunk 146530 (SPLAC 055B)       */
            err_062446();            /* 063066 -> ptr 063140 = routine 062446      */
    }
}

/* PROVEN: both SRESPL sites are issued with NO marshalled parameters (no
 *   'LDX ,B -176 / STA ,X' precedes either call).  Handler is 5NOPAR (generic
 *   forward path), consistent with a parameterless system-side action.
 * UNKNOWN: the meaning of 140B / SRESPL.  It is undocumented in the NPL source;
 *   no name expansion is asserted.  It pairs with SPLAC (START-PLACE) as the
 *   two arms of a flag test, suggesting a PLACE-related reserve/reset, but this
 *   is NOT proven. */
