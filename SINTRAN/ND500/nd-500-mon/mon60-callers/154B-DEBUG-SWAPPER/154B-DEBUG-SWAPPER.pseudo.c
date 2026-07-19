/* ===========================================================================
 *  154B DEBUG SWAPPER  ->  MON 60 subfunction 154B (0x6C = 108 dec)
 *  Purpose: DEBUG SWAPPER <ON/OFF> ; server handler 5NOPAR (generic).
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 146767 = SAA 154.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_154(void);   /* JPL I -> thunk 146767 (SAA 154) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 010133, in ENTER-routine 002662 (framesize 000331) --------------- */
void site_010133(void)
{
    /* param1 (,X 6) = &(B-127). 010117 JAZ selects one of two constants into B-127: 010120 LDD 145 (path A) or 010123 JPL I 144 -> ptr 010267=035034 helper then 010125 LDD 143 (path B). Store at 010132. */
    if (MON60_154() == ERROR)   /* 010133 */
        goto error;            /* 010134 -> ptr 010043 = 007500 (leaf error handler, role INFERRED) */
    /* success: 010135 falls through (STZ I 136, no jump) */
    return;
error:
    handle_error();
}

