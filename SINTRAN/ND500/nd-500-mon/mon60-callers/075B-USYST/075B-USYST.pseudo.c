/* ===========================================================================
 *  USYST   ->  MON 60 subfunction 075B = 0x3D = 61 dec
 * ---------------------------------------------------------------------------
 *  Purpose : CHECK IF CURRENT USER IS USER SYSTEM   (server handler ITSTUSER)
 *  Privilege gate: NO input parameters; used for its skip/direct return only.
 *  Four call sites: 006325, 006411, 006447 (in main interpreter 002662),
 *  and 073461 (in routine 073412, framesize 000223).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): success (skip) = user IS SYSTEM; error (direct) = not.
 * ===========================================================================
 */

extern int  MON60_USYST(void);   /* thunk 146605 (SAA 75) at each of the 4 sites */
extern void cmd_error_002673(void);

/* Pattern common to all four sites (no parameter block is built): */
int is_user_system_gate(void)    /* e.g. call site 006325 */
{
    if (MON60_USYST() == ERROR)  /* 006325 JPL I 26 -> thunk 146605 (USYST 75B) */
        cmd_error_002673();      /* 006326 callsite+1 = ERROR  -> 002673 (not SYSTEM) */
    /* 006327 callsite+2 = SUCCESS: user IS SYSTEM, caller continues */
    return SYSTEM;
}

/* 073461 variant: on error, LEAVE-with-value (177327) returns an error from the
 * enclosing routine 073412 instead of jumping to the interpreter reporter. */
