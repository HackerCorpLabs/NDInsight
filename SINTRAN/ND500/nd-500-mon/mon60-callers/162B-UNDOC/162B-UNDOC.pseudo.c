/* ===========================================================================
 *  162B UNDOC  ->  MON 60 subfunction 162B (0x72 = 114 dec)
 *  Undocumented: no FUNCTION= comment; generic 5NOPAR dispatch. Purpose UNKNOWN.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147006 = SAA 162.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_162(void);   /* JPL I -> thunk 147006 (SAA 162) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 141266, in ENTER-routine 140763 (framesize 000032) --------------- */
void site_141266(void)
{
    /* FOUR pointer params: 141254 STA ,X 6 = &(B-154); 141257 STA ,X 7 = &(B-152); 141262 STA ,X 10 = &(B-150); 141265 STA ,X 11 = &(B-146). PROVEN. */
    if (MON60_162() == ERROR)   /* 141266 */
        goto error;            /* 141267 -> ptr 141302 = 140767 (inner error handler) */
    /* success: 141270 -> ptr 141304 = 177335 (LEAVE-SKIP) */
    return;
error:
    handle_error();
}

