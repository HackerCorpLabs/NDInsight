/* ===========================================================================
 *  155B UNDOC  ->  MON 60 subfunction 155B (0x6D = 109 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147030 = SAA 155.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_155(void);   /* JPL I -> thunk 147030 (SAA 155) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 005741, in ENTER-routine 002662 (framesize 000331) --------------- */
void site_005741(void)
{
    /* NONE. 005740 (the preceding word) is a JMP; no parameter store precedes the call. PROVEN. */
    if (MON60_155() == ERROR)   /* 005741 */
        goto error;            /* 005742 -> ptr 005702 = 002673 (error reporter) */
    /* success: 005743 -> ptr 005731 = 010613 (command loop) */
    return;
error:
    handle_error();
}

