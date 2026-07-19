/* ===========================================================================
 *  173B SET CPU STATUS  ->  MON 60 subfunction 173B (0x7B = 123 dec)
 *  Purpose: SET CPU STATUS (ICPUSTAT) ; server handler ICPUSTAT.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147044 = SAA 173.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_173(void);   /* JPL I -> thunk 147044 (SAA 173) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 032605, in ENTER-routine 032442 (framesize 000046) --------------- */
void site_032605(void)
{
    /* param1 (,X 6) = word at (B-172)+3 (032567 LDX ,B-172 / 032570 AAX 3 / 032571 LDA ,X 0 / 032573 STA ,X 6). */
    /* param2 (,X 7) = &(B-137) (032575 AAA -137 / 032576 STA ,X 7). */
    /* param3 (,X 10) = &(B-141) (032600 AAA -141 / 032601 STA ,X 10). */
    /* param4 (,X 11) = &(B-135) (032603 AAA -135 / 032604 STA ,X 11). */
    if (MON60_173() == ERROR)   /* 032605 */
        goto error;            /* 032606 -> ptr 032612 = 177327 (LEAVE error) */
    /* success: 032607 -> ptr 032616 = 177335 (LEAVE-SKIP) */
    return;
error:
    handle_error();
}

