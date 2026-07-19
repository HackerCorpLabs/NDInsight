/* ============================================================================
 * MON 033B - MALTN / AltPageTable - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 33B-AltPageTable.ASM.
 * The dispatch chain, the entry, the sibling family and the index/loop arithmetic
 * are VERIFIED from bytes; the alternative-page-table field layout is INFERRED.
 *
 * Dispatch: MON 033B -> ENT14 072167B -> GOTAB[033B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[033B] @005653B = 037246B = MALTN (worker below).
 * All constants octal.
 * ============================================================================
 */

#define F_LINK  (-0200)   /* ,B -200 : saved return link */
#define F_OP1   (-0175)   /* ,B -175 : staged operand */
#define F_OP2   (-0177)   /* ,B -177 : staged operand */

/* MALTN heads a family (MALTN/SKICK/SIDEN) that share a page-table body. */
void MALTN(mon_regs *r)                       /* entry 037246B */
{
    r->T = mem(0124);                         /* 037246B: LDT 124 (page-table selector/bank) */
    frame_at(r->X + 0115)++;                  /* 037247B: MIN ,X ,B 115 */
    mem_indirect(0105)++;                     /* 037250B: MIN I 105 */
    dd_store(r->X + 0, r->D);                 /* 037251B-037252B: LDT I 40 / STD I ,B ,X 0 */
    word status = dd_load(r->X + 0103);       /* 037253B: LDA I ,B ,X 103 */

    /* 037272B shared body: save link + staged operands, normalize/count loop,
     * MPY 74 (index * entry size), JPL I 75 -> 037404B helper to index/update
     * the alternative page-table entry. */
    frame[F_LINK] = r->L;                     /* 037272B-037273B */
    page_table_update(status /* ... */);      /* INFERRED: APT entry layout not byte-proven */
}

/* Caveats:
 *  - Dispatch chain + entry + loop/index arithmetic are BYTE-VERIFIED
 *    (see 33B-AltPageTable.ASM).
 *  - The exact alternative-page-table entry layout, the staged-field meanings and
 *    the 037404B helper internals are INFERRED from structure, not proven.
 */
