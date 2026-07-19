/* ===========================================================================
 *  161B DEFINE DOMAIN (NEW FORMAT)  ->  MON 60 subfunction 161B (0x71 = 113 dec)
 *  Purpose: DEFINE STANDARD DOMAIN (NEW DOMAIN FORMAT) (INDFSYDOM) ; server handler INDFSYDOM.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147003 = SAA 161.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_161(void);   /* JPL I -> thunk 147003 (SAA 161) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 071025, in ENTER-routine 070160 (framesize 002172) --------------- */
void site_071025(void)
{
    /* param1 (,X 6) = value at B-153 (071000 LDA ,B-153 / 071001 LDX ,B-176 / 071002 STA ,X 6). */
    /* param2 (,X 7) = 3-word float from B-172 (071003 LDF ,B-172 / 071004 STF ,X 7). */
    if (MON60_161() == ERROR)   /* 071025 */
        goto error;            /* 071026 -> ptr 071041 = 070175 (leaf error handler) */
    /* success: 071027 (LDA ,B-152 ...) continues */
    return;
error:
    handle_error();
}

