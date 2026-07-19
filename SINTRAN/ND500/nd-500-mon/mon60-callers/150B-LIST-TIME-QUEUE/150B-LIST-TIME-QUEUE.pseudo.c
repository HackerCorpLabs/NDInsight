/* ===========================================================================
 *  150B LIST TIME-QUEUE  ->  MON 60 subfunction 150B (0x68 = 104 dec)
 *  Purpose: LIST ND-500 TIME-QUEUE (ILI5TQU) ; server handler ILI5TQU.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 146753 = SAA 150.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_150(void);   /* JPL I -> thunk 146753 (SAA 150) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 111614, in ENTER-routine 111604 (framesize 000004) --------------- */
void site_111614(void)
{
    /* param1 (,X 6): 111607 LDA ,B-172 (routine incoming local); 111610 STA I 112 / 111611 LDA 112 (indirection through pointer word at 112); 111613 STA ,X 6. */
    if (MON60_150() == ERROR)   /* 111614 */
        goto error;            /* 111615 -> ptr 111725 = 177327 (LEAVE with error) */
    /* success: 111616 (SAA 41 ...) falls through, continues in routine */
    return;
error:
    handle_error();
}

