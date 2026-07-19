/* ===========================================================================
 *  015B-RESRV   ->  MON 60 subfunction RESRV = 15B (0x0D = 13 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): RESERVE ND-500 PROCESS.  Handler: 5NOPAR.
 *  Two call sites: 010333 (interpreter case) and 011142 (routine 011043).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_RESRV(void);   /* thunk 146357 (SAA 15) -> gateway 146244       */
extern word *p;                 /* p[6]=param1, p[7]=param2 (3-word STF)          */

/* --- 010333 (case inside interpreter 002662) ----------------------------- */
void cmd_reserve_process(void)
{
    p[6] = const_010515;         /* 010324..010326 - mode/type value (INFERRED)  */
    p[7] = F(local_B114, 012);   /* 010327..010332 - F reg: T=B-114, D=012       */
    if (MON60_RESRV() == ERROR)  /* 010333 JPL I 163 -> thunk 146357             */
        error_007500();          /* 010334 callsite+1 = ERROR -> ptr 010256=007500 */
    /* 010335 callsite+2 = SUCCESS */
}

/* --- 011142 (standalone routine 011043) ---------------------------------- */
void reserve_process(void)
{
    p[6] = const_011254;         /* 011133..011135 */
    p[7] = F(local_B172, 012);   /* 011136..011141 - F reg: T=B-172, D=012       */
    if (MON60_RESRV() == ERROR)  /* 011142 JPL I 113 -> thunk 146357             */
        goto err_011114;         /* 011143 callsite+1 = ERROR (local block)      */
    /* 011144 callsite+2 = SUCCESS */
}
