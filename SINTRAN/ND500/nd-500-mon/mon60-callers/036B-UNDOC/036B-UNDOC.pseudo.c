/* ===========================================================================
 *  036B UNDOC  ->  MON 60 subfunction 036B (0x1E = 30 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 146775 = SAA 036.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_036(void);   /* JPL I -> thunk 146775 (SAA 036) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 005733, in ENTER-routine 002662 (framesize 000331) --------------- */
void site_005733(void)
{
    /* NONE. No 'STA ,X n' appears in 005727-005732; 005731/005732 are pool/mis-decoded data (prog.md sec 9.1). Caller marshals no parameters. PROVEN. */
    if (MON60_036() == ERROR)   /* 005733 */
        goto error;            /* 005734 -> ptr 005702 = 002673 (interpreter error reporter) */
    /* success: 005735 -> 005740 (005740 JMP I -7 -> ptr 005731=010613 command loop) */
    return;
error:
    handle_error();
}

