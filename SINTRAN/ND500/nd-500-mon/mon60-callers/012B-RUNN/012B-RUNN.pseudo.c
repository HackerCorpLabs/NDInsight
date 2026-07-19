/* ===========================================================================
 *  012B-RUNN   ->  MON 60 subfunction RUNN = 12B (0x0A = 10 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : standalone ENTER-routine 030515 (framesize 000011).
 *  Purpose (NPL, authoritative): START ND-500 PROGRAM.  Handler: 5NOPAR.
 *  Signature (yaml): <stop reason> <returned trap info> <clear time used>.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  (This is the section-5.5 end-to-end verification case in prog.md.)
 * ===========================================================================
 */

extern int MON60_RUNN(void);    /* thunk 146346 (SAA 12) -> gateway 146244       */
extern word *p;                 /* p[6],p[7],p[10] = MON 60 params 1..3          */

/* --- 030635 ---------------------------------------------------------------- */
void start_nd500_program(void)          /* routine @030515 */
{
    word stop_reason;   /* B-167 */
    word trap_info;     /* B-165 */

    p[6]  = (word)&stop_reason;  /* 030624..030627 - &<stop reason>              */
    p[7]  = local_B171;          /* 030630..030631 - <clear time used> (INFERRED) */
    p[10] = (word)&trap_info;    /* 030632..030634 - &<returned trap info>       */

    if (MON60_RUNN() == ERROR)   /* 030635 JPL I 110 -> thunk 146346             */
        goto err_030521;         /* 030636 callsite+1 = ERROR                    */
    read(stop_reason);           /* 030637 callsite+2 = SUCCESS: LDD ,B -167     */
}

/* --- 030737 (second RUNN in same routine; params at 030673, JMP 33 -> here) - */
void start_nd500_program_retry(void)
{
    /* 030673..030703: identical fill of p[6]=&B-167, p[7]=B-171, p[10]=&B-165 */
    if (MON60_RUNN() == ERROR)   /* 030737 JPL I -3 -> thunk 146346              */
        goto err_030746;         /* 030740 callsite+1 = ERROR (-> 030521)        */
    read(stop_reason);           /* 030741 callsite+2 = SUCCESS: LDD ,B -167     */
}
