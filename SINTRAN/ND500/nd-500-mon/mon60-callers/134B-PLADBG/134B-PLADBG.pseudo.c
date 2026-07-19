/* ===========================================================================
 *  PLADBG   ->  MON 60 subfunction PLADBG = 134B (0x5C = 92 dec)
 *  Purpose: PLACE DEBUGGER.  Server handler IPLDEB.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : ENTER-routine 002527 (framesize 000075).  Call site 002560.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_PLADBG(void);   /* 002560 JPL I ->002624; thunk 146726 SAA 134 */
extern word *p;                   /* gateway slots, p[6]=param1, p[7]=param2 */

void place_debugger(fdesc dbg_name /*B-172*/)
{
    dword local;                     /* frame local @B-167 */

    /* ... main path (002552..) ... */
    p[6] = (word)&local;             /* 002552..002555 : param1 := &local(B-167)  */
    p[7] = dbg_name;                 /* 002556 LDF ,B -172 ; 002557 STF ,X 7 (3w) */
    if (MON60_PLADBG() == ERROR)     /* 002560 -> thunk 146726 (PLADBG 134B)      */
        return LEAVE_value();        /* 002561 callsite+1 -> ptr 002622 = 177327   */
    /* 002562 callsite+2 SUCCESS: continues in-line */
}

/* PROVEN: two MON60 parameters on the main path - slot 6 = &local(B-167),
 *   slot 7 = the 3-word F descriptor from input B-172.
 * INFERRED: B-172 is the debugger domain/file name; local(B-167) is an output
 *   or a mode/status word.  Exact roles not traced.  A separate branch
 *   (002536..002546, taken when local(X+2) < 0) sets slots differently and
 *   calls routine 043011 (NOT a MON 60 thunk).  Server handler IPLDEB places
 *   the debugger on the SINTRAN side. */
