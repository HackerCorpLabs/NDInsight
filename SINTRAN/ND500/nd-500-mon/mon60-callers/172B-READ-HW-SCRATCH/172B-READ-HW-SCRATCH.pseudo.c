/* ===========================================================================
 *  172B READ HW SCRATCH REGISTER FILE  ->  MON 60 subfunction 172B (0x7A = 122 dec)
 *  Purpose: READ HW SCRATCH REGISTER FILE ; server handler 5NOPAR (generic).
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147041 = SAA 172.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_172(void);   /* JPL I -> thunk 147041 (SAA 172) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 056700, in ENTER-routine 056042 (framesize 000050) --------------- */
void site_056700(void)
{
    /* param1 (,X 6) = &(B-132) (056622/056623 AAA -132 / 056625 STA ,X 6). */
    /* param2 (,X 7) = &(B-130) (056643/056644 AAA -130 / 056646 STA ,X 7). */
    /* param3 (,X 10) = 3-word float built from B-143 (056653 STF ,B-143 / 056654 LDF ,B-143 / 056655 STF ,X 10). */
    if (MON60_172() == ERROR)   /* 056700 */
        goto error;            /* 056701 JPL -74 -> 056605 */
    /* success: 056702 JMP 23 -> 056725 */
    return;
error:
    handle_error();
}

