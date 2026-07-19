/* ===========================================================================
 *  164B UNDOC  ->  MON 60 subfunction 164B (0x74 = 116 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147014 = SAA 164.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_164(void);   /* JPL I -> thunk 147014 (SAA 164) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 141310, in ENTER-routine 141305 (framesize 000000) --------------- */
void site_141310(void)
{
    /* NONE. Parameterless wrapper; no 'STA ,X n' between ENTER and the call. PROVEN. */
    if (MON60_164() == ERROR)   /* 141310 */
        goto error;            /* 141311 -> ptr 141315 = 177327 (LEAVE error) */
    /* success: 141312 -> ptr 141316 = 177335 (LEAVE-SKIP) */
    return;
error:
    handle_error();
}

