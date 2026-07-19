/* ===========================================================================
 *  170B READ CPU-TYPE AND MIC.VERSION  ->  MON 60 subfunction 170B (0x78 = 120 dec)
 *  Purpose: READ ND-500 CPU-TYPE AND MIC.VERSION ; server handler 5NOPAR (generic).
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147033 = SAA 170.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_170(void);   /* JPL I -> thunk 147033 (SAA 170) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 143134, in ENTER-routine 143104 (framesize 000004) --------------- */
void site_143134(void)
{
    /* param1 (,X 6) = value at 17 (143126 LDA 17 / 143127 LDX ,B-176 / 143130 STA ,X 6). */
    /* param2 (,X 7) = a buffer address (143131 LDA 14 / 143132 AAA 2 / 143133 STA ,X 7). */
    if (MON60_170() == ERROR)   /* 143134 */
        goto error;            /* 143135 JPL -25 -> 143110 (inner error handler) */
    /* success: 143136 (SAA 1 ...) continues */
    return;
error:
    handle_error();
}

