/* ===========================================================================
 *  171B UNDOC  ->  MON 60 subfunction 171B (0x79 = 121 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147036 = SAA 171.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_171(void);   /* JPL I -> thunk 147036 (SAA 171) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 123671, in ENTER-routine 123577 (framesize 000064) --------------- */
void site_123671(void)
{
    /* param1 (,X 6) = &(B-156) (123662 RADD / 123663 AAA -156 / 123665 STA ,X 6). */
    /* param2 (,X 7) = &(B-160) (123666 RADD / 123667 AAA -160 / 123670 STA ,X 7). */
    if (MON60_171() == ERROR)   /* 123671 */
        goto error;            /* 123672 -> ptr 124021 = 177327 (LEAVE error) */
    /* success: 123673 (JMP 124 -> 124017) continues */
    return;
error:
    handle_error();
}

