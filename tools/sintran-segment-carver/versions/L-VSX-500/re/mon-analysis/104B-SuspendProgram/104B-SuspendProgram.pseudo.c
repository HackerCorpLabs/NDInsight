/* ============================================================================
 * MON 104B - HOLD / SuspendProgram - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Worker home: 026-S3IMPIT (monitor-PIT overlay),
 * symbol HOLD=040645B.  The priority-ladder and the ION/EXIT tail are VERIFIED
 * from bytes; the caller-argument -> priority-entry mapping is INFERRED.
 *
 * Dispatch: MON 104B -> ENT14 072167B -> GOTAB[104B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[104B] @005724B = 040645B = HOLD (below).
 * Constants octal.
 * ============================================================================
 */

/* Five entry points, one per suspend priority (040645B, 040647B, 040651B,
 * 040653B, 040655B).  Which one the caller reaches is set up by the level
 * switch / caller wrapper (INFERRED - not visible in this body). */
void HOLD(int entry_priority /* one of 021,020,017,016,015 */)
{
    int A = entry_priority;      /* 040645B..040655B: LDA <prio> */

    /* common tail @040656B */
    int T = frame[-0115];        /* LDT -115 : saved base/link value */
    B = T;                       /* 040657B: RADD CLD ST DB */
    T = frame[-0115];            /* 040660B: LDT -115 */
    L = T;                       /* 040661B: RADD CLD ST DL  (set return link) */
    interrupts_on();             /* 040662B: ION */
    EXIT();                      /* 040663B: return to the scheduler at priority A */
}
