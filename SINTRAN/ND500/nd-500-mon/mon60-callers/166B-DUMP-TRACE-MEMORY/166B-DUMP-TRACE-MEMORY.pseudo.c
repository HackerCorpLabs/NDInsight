/* ===========================================================================
 *  166B DUMP TRACE-MEMORY  ->  MON 60 subfunction 166B (0x76 = 118 dec)
 *  Purpose: DUMP-TRACE-MEMORY ; server handler 5NOPAR (generic).
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147022 = SAA 166.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_166(void);   /* JPL I -> thunk 147022 (SAA 166) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 140654, in ENTER-routine 140575 (framesize 000014) --------------- */
void site_140654(void)
{
    /* param1 (,X 6) = &(B-162); 140642 LDD 105 / 140643 STD ,B-162 / 140645 AAA -162 / 140646 STA ,X 6. */
    /* param2 (,X 7) = 3-word float; 140650 LDA 101 / 140651 SWAP CLD SA DD / 140652 LDT 100 / 140653 STF ,X 7. */
    if (MON60_166() == ERROR)   /* 140654 */
        goto error;            /* 140655 -> 140601 (inner error handler) */
    /* success: 140656 (LDA I 62 ...) continues the sequence */
    return;
error:
    handle_error();
}

