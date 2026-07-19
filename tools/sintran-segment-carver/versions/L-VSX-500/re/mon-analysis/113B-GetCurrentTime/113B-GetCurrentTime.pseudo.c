/* ============================================================================
 * MON 113B - CLOCK / GetCurrentTime - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 113B-GetCurrentTime.ASM.
 * The dispatch chain and the divide/convert entry are VERIFIED from bytes; the
 * calendar-field packing and caller-buffer layout are INFERRED from the manual.
 *
 * Dispatch: MON 113B -> ENT14 072167B -> GOTAB[113B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[113B] @005733B = 040756B = CLOCK (worker below).
 * All constants octal.  Returns the current system time and date.
 * ============================================================================
 */

/* CLOCK converts the raw internal clock into calendar/time-of-day fields using
 * successive divide steps and the decimal-breakdown helper XPERC @040766B (which
 * divides by the powers-of-ten table). */
void CLOCK(mon_regs *r)                       /* entry 040756B */
{
    dword q = rdiv(r->AD, r->T);              /* 040756B: RDIV ST (split raw clock) */
    r->X = q;                                 /* 040757B: RADD CLD SD DX */
    word field0 = xperc_decimal(q + 060);     /* 040760B-040761B: AAA 60 / JPL I 5 -> 040766B */

    word next = r->X + 060;                    /* 040762B: SAA 60 */
    r->A = r->X;                               /* 040763B: RADD SX DA */
    word field1 = xperc_decimal(next);         /* 040764B: JPL I 2 -> 040766B */

    /* 040765B CONKI: return via the shared body @040727B. */
    store_time_and_date_fields(r, field0, field1 /* ... */);  /* INFERRED layout */
}

/* Caveats:
 *  - Dispatch chain + entry bytes are BYTE-VERIFIED (see 113B-GetCurrentTime.ASM).
 *  - xperc_decimal() is the shared XPERC/PERCE/DPERC decimal-breakdown code at
 *    040766B (divide by powers of ten); its full internals and the exact set and
 *    order of returned calendar fields are INFERRED from the manual, not proven.
 */
