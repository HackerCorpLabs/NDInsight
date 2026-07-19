/* ===========================================================================
 *  LOGOUT   ->  MON 60 subfunction LOGOUT = 122B (0x52 = 82 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : ENTER-routine 110333 (framesize 000013).  ONE routine dispatches
 *            both ABORT (117B) and LOGOUT (122B) on the input flag at B-172.
 *            LOGOUT is the flag==0 branch (110351..110357).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_LOGOUT(void);   /* 110355 JPL I ->110363; thunk 146670 SAA 122 */
extern word *p;                   /* gateway outgoing slots, p[6]=param1 */

int abort_or_logout(dword arg_value /*B-171*/, word arg_flag /*B-172*/)
{
    dword local = arg_value;         /* 110336 LDD ,B -171 ; 110337 STD ,B -167 */

    if (arg_flag != 0) {             /* 110340..110341 : !=0 -> ABORT (see 117B) */
        /* ... ABORT (117B) branch, documented in 117B-ABORT ... */
    } else {
        /* ---- LOGOUT (122B) ----------------------------------------------- */
        p[6] = (word)&local;         /* 110351..110354 : param1 := &local(B-167) */
        if (MON60_LOGOUT() == ERROR) /* 110355 -> thunk 146670 (LOGOUT 122B)     */
            return LEAVE_value();     /* 110356 callsite+1 -> 110362 = 177327     */
        return LEAVE_skip();         /* 110357 callsite+2 -> 110364 = 177335      */
    }
}

/* PROVEN: exactly one MON60 parameter (slot 6) = &local(B-167), the 32-bit
 *   value copied from input B-171.  No slots 7/10 are stored.
 * INFERRED: B-171 carries the ND-500 process identifier to log off; B-172 is
 *   the ABORT-vs-LOGOUT selector.  Server handler = ILOGOFF (SINTRAN side). */
