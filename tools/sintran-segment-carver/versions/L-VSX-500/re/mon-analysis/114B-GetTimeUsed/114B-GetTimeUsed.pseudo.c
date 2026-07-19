/* ============================================================================
 * MON 114B - TUSED / GetTimeUsed - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 114B-GetTimeUsed.ASM.
 * The dispatch chain, the arg check, the selector decode and the interrupt-guarded
 * accounting read are VERIFIED from bytes; field offsets and sub-function semantics
 * are INFERRED.
 *
 * Dispatch: MON 114B -> ENT14 072167B -> GOTAB[114B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[114B] @005734B = 041303B = TUSED (worker below).
 * All constants octal.  Returns CPU time used since login (batch: since job start).
 * ============================================================================
 */

#define F_FIELD (-0176)   /* ,B -176 : staged accounting field */
#define ERR_CODE 0147     /* error selector on arg mismatch */

void TUSED(mon_regs *r)                       /* entry 041303B */
{
    if (r->A == r->T) {                        /* 041303B: SKP IF DA UEQ ST (mismatch -> error) */
        r->A = ERR_CODE;                        /* 041305B: SAA 147 */
        return_error();                         /* 041306B: JMP I 146 -> 041454B */
    }

    r->X = r->A;                                /* 041307B: RADD CLD SA DX (selector) */
    frame[F_FIELD] = mem_at(r->X + 012);        /* 041310B-041311B: LDT ,X 12 / STT ,B -176 */
    helper(0143);                               /* 041312B-041313B: LDX 143 / JPL I 143 -> 041456B */

    /* Multi-way selector (1NOTO/XSBPR/1FU2/1FU3/STERM/1FU4/1FU5): each arm loads a
     * table pointer and calls the helper at 041456B, returning via JMP I ,B -36. */

    interrupts_off();                           /* 041345B: IOF */
    word acct = mem_at(r->X + 3);               /* 041341B: LDA ,X 3 (per-process CPU-time field) */
    interrupts_on();                            /* 041361B: ION */

    r->result = acct;                           /* INFERRED: CPU time used; packing not proven */
}

/* Caveats:
 *  - Dispatch chain + entry + arg check + IOF/ION-guarded read are BYTE-VERIFIED
 *    (see 114B-GetTimeUsed.ASM).
 *  - The accounting-field offsets, the sub-function set (1FU2..1FU5) and the result
 *    packing are INFERRED from structure and the manual, not proven.
 */
