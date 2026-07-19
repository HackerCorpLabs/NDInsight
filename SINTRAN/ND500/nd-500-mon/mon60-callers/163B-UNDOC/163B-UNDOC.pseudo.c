/* ===========================================================================
 *  163B UNDOC  ->  MON 60 subfunction 163B (0x73 = 115 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147011 = SAA 163.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_163(void);   /* JPL I -> thunk 147011 (SAA 163) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 141002, in ENTER-routine 140763 (framesize 000032) --------------- */
void site_141002(void)
{
    /* NONE. Guarded by a byte test (140775 LDT ,B-157 / 140776 LBYT / 140777 SAT 131 / 141000 SKP IF DA EQL ST); no parameter store precedes the call. PROVEN. */
    if (MON60_163() == ERROR)   /* 141002 */
        goto error;            /* 141003 -> 140767 (inner error/leaf handler) */
    /* success: 141004 (SAA 1 ...) continues */
    return;
error:
    handle_error();
}

/* --- call site 143045, in ENTER-routine 143042 (framesize 000000) --------------- */
void site_143045(void)
{
    /* NONE. Parameterless wrapper. PROVEN. */
    if (MON60_163() == ERROR)   /* 143045 */
        goto error;            /* 143046 -> ptr 143052 = 177327 */
    /* success: 143047 -> ptr 143053 = 177335 */
    return;
error:
    handle_error();
}

