/* ===========================================================================
 *  160B PLACE SEGMENT (NEW FORMAT)  ->  MON 60 subfunction 160B (0x70 = 112 dec)
 *  Purpose: LOAD (PLACE) ONE SEGMENT (NEW DOMAIN FORMAT) (IN5SEGLOAD) ; server handler IN5SEGLOAD.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 147000 = SAA 160.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_160(void);   /* JPL I -> thunk 147000 (SAA 160) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 060562, in ENTER-routine 060374 (framesize 000004) --------------- */
void site_060562(void)
{
    /* param1 (,X 6) = A from 060546 LDA 64; store 060550. */
    /* param2 (,X 7) = &(B-150); 060551/060552/060553. */
    /* param3 (,X 10) = A from 060554 LDA 57; store 060555. */
    /* param4 (,X 11) = 3-word float (060556 SAA 36 / 060560 LDT 54 / 060561 STF ,X 11). */
    if (MON60_160() == ERROR)   /* 060562 */
        goto error;            /* 060563 JMP I ,B-155 -> ptr 060406 = 032006 (frame-relative error exit) */
    /* success: 060564 continues */
    return;
error:
    handle_error();
}

/* --- call site 061135, in ENTER-routine 060374 (framesize 000004) --------------- */
void site_061135(void)
{
    /* param1 (,X 6) = A from 061121 LDA 142; store 061123. */
    /* param2 (,X 7) = &(B-150); store 061126. */
    /* param3 (,X 10) = A from 061127 LDA 135; store 061130. */
    /* param4 (,X 11) = 3-word float; store 061134. */
    if (MON60_160() == ERROR)   /* 061135 */
        goto error;            /* 061136 JMP I ,B-155 -> ptr 060761 = 047060 */
    /* success: 061137 continues */
    return;
error:
    handle_error();
}

