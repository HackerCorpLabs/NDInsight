/* ===========================================================================
 *  165B UNDOC  ->  MON 60 subfunction 165B (0x75 = 117 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147017 = SAA 165.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_165(void);   /* JPL I -> thunk 147017 (SAA 165) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 140617, in ENTER-routine 140575 (framesize 000014) --------------- */
void site_140617(void)
{
    /* Nearest preceding store: 140612 LDX ,B-176 / 140613 STA ,X 6 (param1 := A; A from 140610 SAA 100 / 140611 STA I 127). In a chained sequence over shared slots; parameter attribution is not separable. INFERRED. */
    if (MON60_165() == ERROR)   /* 140617 */
        goto error;            /* 140620 -> 140601 (inner error handler; 140605 JMP I ,B-164 -> 140421) */
    /* success: 140621 = the next call (167B) - chained sequence */
    return;
error:
    handle_error();
}

/* --- call site 141322, in ENTER-routine 141317 (framesize 000000) --------------- */
void site_141322(void)
{
    /* NONE. Parameterless wrapper. PROVEN. */
    if (MON60_165() == ERROR)   /* 141322 */
        goto error;            /* 141323 -> ptr 141327 = 177327 */
    /* success: 141324 -> ptr 141330 = 177335 */
    return;
error:
    handle_error();
}

