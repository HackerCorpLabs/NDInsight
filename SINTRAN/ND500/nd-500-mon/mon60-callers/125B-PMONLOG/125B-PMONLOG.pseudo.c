/* ===========================================================================
 *  PMONLOG   ->  MON 60 subfunction PMONLOG = 125B (0x55 = 85 dec)
 *  Purpose: READ MONCALL LOG DATA (PRINT MONCALL LOG).  Server handler IPRIMLOG.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : ENTER-routine 111217 (framesize 001411).  Call site 111232.
 *            Also reached from the interpreter case at 007315.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_PMONLOG(void);  /* 111232 JPL I ->111410; thunk 146701 SAA 125 */
extern word *p;                   /* gateway slots, p[6]=param1 (3 words) */

void read_moncall_log(void)
{
    word local_B163;                 /* frame local @B-163 (log data sink; INFERRED) */

    /* Build the 3-word F descriptor for param1:
     *   T := &local(B-163)          111222..111227
     *   D := pooled word @111407    111225..111226
     *   A := prior D                111226
     */
    Freg F;
    F.T = (word)&local_B163;         /* 111227 LDT ,B -172 (= &local(B-163))       */
    F.D = pooled_111407;             /* 111225 LDA 162 ; 111226 SWAP -> D           */
    p[6] = F;                        /* 111230..111231 : STF ,X 6 (3 words)         */

    if (MON60_PMONLOG() == ERROR)    /* 111232 -> thunk 146701 (PMONLOG 125B)      */
        return LEAVE_value();        /* 111233 callsite+1 -> ptr 111411 = 177327    */
    /* 111234 callsite+2 SUCCESS: continues in-line */
}

/* PROVEN: one MON60 parameter (slot 6) = a 3-word F descriptor assembled at
 *   111222..111231 from &local(B-163) (in T) and a pooled word @111407 (in D).
 * INFERRED: the descriptor is {buffer pointer, length/count} for the moncall-log
 *   read; exact field roles not traced.  Server handler IPRIMLOG reads/prints
 *   the monitor-call log on the SINTRAN side. */
