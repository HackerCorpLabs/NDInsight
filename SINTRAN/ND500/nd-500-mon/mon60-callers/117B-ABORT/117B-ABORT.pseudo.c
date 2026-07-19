/* ===========================================================================
 *  ABORT   ->  MON 60 subfunction ABORT = 117B (0x4F = 79 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : ENTER-routine 110333 (framesize 000013).  ONE routine dispatches
 *            both ABORT (117B) and LOGOUT (122B) on the input flag at B-172.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_ABORT(void);    /* 110346 JPL I ->110361; thunk 146657 SAA 117 */
extern int  MON60_LOGOUT(void);   /* 110355 JPL I ->110363; thunk 146670 SAA 122 */
extern word *p;                   /* gateway outgoing slots, p[6]=param1 */

/* Inputs to routine 110333 (PROVEN as read/store offsets; roles INFERRED):
 *   arg_value @ B-171 : a 32-bit value (LDD ,B -171) copied to local(B-167)
 *   arg_flag  @ B-172 : selector; !=0 -> ABORT, ==0 -> LOGOUT
 */
int abort_or_logout(dword arg_value /*B-171*/, word arg_flag /*B-172*/)
{
    dword local = arg_value;         /* 110336 LDD ,B -171 ; 110337 STD ,B -167 */

    if (arg_flag != 0) {             /* 110340 LDA ,B -172 ; 110341 JAZ -> LOGOUT */
        /* ---- ABORT (117B) ------------------------------------------------ */
        p[6] = (word)&local;         /* 110342..110345 : param1 := &local(B-167) */
        if (MON60_ABORT() == ERROR)  /* 110346 -> thunk 146657 (ABORT 117B)      */
            return LEAVE_value();     /* 110347 callsite+1 -> 110362 = 177327     */
        return LEAVE_skip();         /* 110350 -> 110357 -> 110364 = 177335       */
    } else {
        /* ---- LOGOUT (122B) : documented in 122B-LOGOUT ------------------- */
        p[6] = (word)&local;         /* 110351..110354                          */
        if (MON60_LOGOUT() == ERROR) /* 110355 -> thunk 146670 (LOGOUT 122B)     */
            return LEAVE_value();     /* 110356 callsite+1 -> 110362 = 177327     */
        return LEAVE_skip();         /* 110357 -> 110364 = 177335                 */
    }
}

/* PROVEN: exactly one MON60 parameter is marshalled (slot 6) = &local(B-167),
 *   the 32-bit value copied from input B-171.  No slots 7/10 are stored.
 * INFERRED: B-172 is an ABORT-vs-LOGOUT selector; B-171 is the process/PCB
 *   identifier the ND-500 side needs to abort.  The exact semantics of the
 *   value were not traced. */
