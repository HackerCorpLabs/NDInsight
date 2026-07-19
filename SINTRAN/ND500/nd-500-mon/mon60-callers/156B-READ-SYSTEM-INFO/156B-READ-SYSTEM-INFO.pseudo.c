/* ===========================================================================
 *  156B READ SYSTEM INFO  ->  MON 60 subfunction 156B (0x6E = 110 dec)
 *  Purpose: READ SYSTEM INFO ; server handler 5NOPAR (generic).
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 146772 = SAA 156.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_156(void);   /* JPL I -> thunk 146772 (SAA 156) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 143074, in ENTER-routine 143066 (framesize 000000) --------------- */
void site_143074(void)
{
    /* param1 (,X 6): 143071 LDA 7 loads one word; 143073 STA ,X 6. Source of 'LDA 7' (word 044007) not resolved to a named object. */
    if (MON60_156() == ERROR)   /* 143074 */
        goto error;            /* 143075 -> ptr 143102 = 177327 (LEAVE error) */
    /* success: 143076 -> ptr 143103 = 177335 (LEAVE-SKIP) */
    return;
error:
    handle_error();
}

