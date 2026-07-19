/* ===========================================================================
 *  MSTCL  ->  MON 60 subfunction 035B (0x1D = 29 dec) MASTER CLEAR
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  THREE call sites; MSTCL takes no parameters.
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_MSTCL(void);          /* thunk 146456 (SAA 35) */

/* Site A @ 005736 -- CASE inside interpreter ENTER 002662 (005736..005740) */
void mstcl_case_A(void)
{
    if (MON60_MSTCL() == ERROR)        /* 005736 MON60 035B (no params) */
        goto err_002673;               /* 005737 -> 002673 */
    goto loop_010613;                  /* 005740 -> 010613 */
}

/* Site B @ 005744 -- CASE inside the same interpreter ENTER 002662 (005744..005746) */
void mstcl_case_B(void)
{
    if (MON60_MSTCL() == ERROR)        /* 005744 MON60 035B (no params) */
        goto err_002673;               /* 005745 -> 002673 */
    goto loop_010613;                  /* 005746 -> 010613 */
}

/* Site C @ 122514 -- standalone routine 122507; reached as MSTOP's fall-through */
void mstcl_C(void)
{
    if (MON60_MSTCL() == ERROR)        /* 122514 MON60 035B (no params) */
        LEAVE_value();                 /* 122515 -> 122521 = 177327 */
    else
        LEAVE_skip();                  /* 122516 -> 122523 = 177335 SUCCESS */
}
