/* ============================================================================
 * MON 432B  SIBASFunction (SIBFU)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  "Various SIBAS functions" (manual section 2.14, short
 * name SIBFU).  A system-program monitor call.  The manual lists it name-only -
 * no parameter block, return values, or caller convention are documented.
 *
 * Dispatch reality:
 *   GOTAB[432B] = 056524B (BYTE-VERIFIED).  That value is an address in
 *   025-S3IRPIT (load 32000B, the overlay mapped for level-14 dispatch), where it
 *   resolves to DT83W = 56524B - a compact function dispatcher (real code).  At
 *   the same address in resident commoncode (load 0) the bytes are a lone
 *   `124025 JMP 25`; the 025-S3IRPIT overlay is active during dispatch, so it is
 *   the real-code region for this vector.
 *
 * The DT83W dispatcher body is BYTE-VERIFIED (see 432B-SIBASFunction.ASM); the
 * SIBAS-function MEANING is manual-only (INFERRED), and whether DT83W is the true
 * semantic SIBAS worker or a dispatch-table slot is UNVERIFIED - the symbol NAME
 * (DT83W) does not confirm SIBAS.  Addresses in comments are octal.
 * ============================================================================ */

/* (A) DT83W dispatcher @56524B (025-S3IRPIT).  REAL executable code: loads the
 * function selector into X, calls sub-routines through a pointer table, and
 * checks each result against -1 (error sentinel).  Control flow is BYTE-VERIFIED;
 * the SIBAS semantics of each sub-routine are NOT (see caveats). */
int mon_432B_SIBASFunction(mon_regs *r)      /* in: A = function selector (inferred) */
{
    /* 56524 RADD CLD SA DX : X := A  (the function selector). */
    r->X = r->A;

    /* 56525 JPL I 30 : call sub-routine *(56555) (=050710).
     * 56526 JPL I 33 : call sub-routine *(56561) (=063743). */
    call_indirect(0x050710 /*056555 link cell*/, r);
    r->A = call_indirect(0x063743 /*056561 link cell*/, r);

    /* 56527 SAT -1 ; 56530 SKP IF DA UEQ ST : T := -1; skip next if A != T.
     * SKP IF DA UEQ ST = "skip if A != T" (UEQ, per SKP class), so:            */
    if (r->A == (word)-1) {
        /* 56531 JMP 6 -> 56537 : result == -1 -> error path (DT84R shared tail). */
        goto shared_tail_DT84R;
    }

    /* 56532 JPL I 16 : call sub-routine *(56550) (=063661). */
    r->A = call_indirect(0x063661 /*056550 link cell*/, r);

    /* 56533 SAT -1 ; 56534 SKP IF DA UEQ ST : skip if A != -1. */
    if (r->A == (word)-1) {
        /* 56535 JMP I 25 -> *(56562) (=057177): dispatch onward. */
        return call_indirect(0x057177 /*056562 link cell*/, r);
    }
    /* 56536 JMP 3 -> 56541 : fall into the shared tail. */

shared_tail_DT84R:
    /* 56537-56543 (DT84R sibling entry, shared): STZ ,B 25 / STZ ,B 26 clear two
     * caller status words, then JPL I -> *(56560)(=050661), JPL I -> *(56552)
     * (=063775), JMP I -> *(56563)(=057161) finish/dispatch.                   */
    clear_status_words(r);                       /* STZ ,B 25 / STZ ,B 26 */
    call_indirect(0x050661 /*056560*/, r);
    call_indirect(0x063775 /*056552*/, r);
    return call_indirect(0x057161 /*056563*/, r);
}

/* Caveats for the emulator author:
 *   - GOTAB[432B]=056524B is BYTE-VERIFIED and resolves to DT83W in 025-S3IRPIT
 *     (the active dispatch overlay); DT83W is real dispatcher code.
 *   - The symbol NAME "DT83W" does NOT confirm the SIBAS-function meaning; that
 *     is manual-only (432B_SIBASFunction.yaml lists the call name-only).  The
 *     dispatcher SHAPE (selector in X + sub-routine pointer table + -1 error
 *     checks) is consistent with "various SIBAS functions" but is not name-proven.
 *   - Whether DT83W is the true semantic SIBAS worker, or a dispatch-table slot
 *     from which the real SIBAS worker is reached across the uncarved resident
 *     CALLPROC, is UNVERIFIED.  The sub-worker pointers (050710, 063743, 063661,
 *     057177, 050661, 063775, 057161) are link-cell DATA; their bodies are not
 *     resolved here.
 *   - A live PC trace (issue a real MON 432, capture the first PC after the trap)
 *     is needed to confirm P lands on DT83W=56524 and to identify the sub-workers.
 */
