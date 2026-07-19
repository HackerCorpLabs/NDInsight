/* ===========================================================================
 *  167B UNDOC  ->  MON 60 subfunction 167B (0x77 = 119 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147025 = SAA 167.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_167(void);   /* JPL I -> thunk 147025 (SAA 167) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 140621, in ENTER-routine 140575 (framesize 000014) --------------- */
void site_140621(void)
{
    /* NONE freshly marshalled: no 'STA ,X n' between the preceding 165B call (140617) and this call. Generic dispatch. PROVEN. */
    if (MON60_167() == ERROR)   /* 140621 */
        goto error;            /* 140622 -> 140601 (inner error handler) */
    /* success: 140623 (SAA 7 ...) continues the sequence */
    return;
error:
    handle_error();
}

/* --- call site 143057, in ENTER-routine 143054 (framesize 000000) --------------- */
void site_143057(void)
{
    /* NONE. Parameterless wrapper. PROVEN. */
    if (MON60_167() == ERROR)   /* 143057 */
        goto error;            /* 143060 -> ptr 143064 = 177327 */
    /* success: 143061 -> ptr 143065 = 177335 */
    return;
error:
    handle_error();
}

