/* ============================================================================
 * MON 11B - TIME / GetBasicTime - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 11B-GetBasicTime.ASM.
 * The dispatch chain and the entry stub are VERIFIED from bytes; the shared
 * timer-body internals (TMOUT @040713B, reached by JPL I ,B -37) are INFERRED.
 *
 * Dispatch: MON 11B -> ENT14 072167B -> GOTAB[11B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[11B] @005631B = 040747B = TIME (worker below).
 * All constants octal.  Returns the internal clock in basic time units (double word).
 * ============================================================================
 */

#define F_MODE (-0135)   /* ,B -135 : clock-family mode/selector counter */

/* TIME is one entry of a 5-entry family (TIME/CLOCK/XPERC/PERCE/DPERC); each
 * entry sets a mode then falls into the shared prologue SM2DE @040751B, which
 * saves the return link and calls the common timer body @040713B (TMOUT). */
void TIME(mon_regs *r)                       /* entry 040747B; result out = A:D */
{
    frame[F_MODE]++;                         /* 040747B: MIN ,B -135 (select this function) */
    word dw_lo_hi = mem_dd(r->X - 1);        /* 040750B: LDD ,X ,B -1 */

    /* 040751B SM2DE: save link, call the shared timer body (not carved here). */
    saved_D = r->L;                          /* 040751B: RADD CLD SL DD */
    timer_body(r);                           /* 040752B: JPL I ,B -37 -> 040713B (TMOUT) */

    /* 040753B-040755B: stage the result + a unit code, then return. */
    r->AD = basic_time_units;                /* INFERRED: internal clock, basic time units */
}

/* Caveats:
 *  - Dispatch chain + entry bytes are BYTE-VERIFIED (see 11B-GetBasicTime.ASM).
 *  - timer_body()/TMOUT @040713B is reached by JPL I ,B -37 but is not carved in
 *    this folder; its internals and the exact result packing are INFERRED.
 */
