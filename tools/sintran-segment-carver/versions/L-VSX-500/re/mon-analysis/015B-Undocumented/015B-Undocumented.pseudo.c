/* ============================================================================
 * MON 15B (octal)  Undocumented  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Level-14 monitor call served by the Resident PIT
 * (S3RPIT).  NOT listed in the SINTRAN III Monitor Calls manual.
 *
 * Derived from the real disassembly (see 015B-Undocumented.ASM). The CONTROL
 * FLOW below is BYTE-VERIFIED. The SEMANTIC labels (what each shared worker
 * does, what the arguments mean) are UNVERIFIED - the manual does not document
 * this call and the shared workers are not individually reversed. Treat this as
 * a control-flow model, not a functional spec. Addresses in comments are octal.
 * ============================================================================ */

/* Entry: GOTAB[15B] = 120501B = F1615 stub. Thin per-MON stub that vectors into
 * shared S3RPIT worker routines, passing a selector in T. */
int mon_15B_undocumented(mon_regs *r)
{
    int rc;

    /* 120501 JPL I 35 -> worker_B @116234B. Worker uses skip / no-skip return. */
    if (worker_B_116234(r)) {
        /* SKIP return (fall-through at 120503): rebuild regs, second worker */
        r->x = mem[ptr_061157];            /* 120503 LDX I 34  (data @061157B)  */
        r->a = mem[r->x - 017];            /* 120504 LDA ,X -17                  */
        worker_C_116665(r);                /* 120505 JPL I 33 -> 116665B         */
        r->x = mem[ptr_061157];            /* 120507 LDX I 30  (data @061157B)   */
        r->a = mem[/*120457*/ 0];          /* 120510 LDA -31   (word = 000000B)  */
        /* 120511 RADD CLD SA DB : B := A                                        */
        return jmp_indirect(mem[/*120461*/ 0]); /* 120512 JMP I -31: pointer word
                                             * @120461B = 000000B, RUNTIME-POPULATED.
                                             * Static image cannot resolve this target. */
    }

    /* NORMAL return (120502 JMP 11 -> 120513): subfunction-style dispatch.
     * SKP IF DA EQL ST compares the single registers A (dst) and T (src) - "DA"
     * = destination A, "ST" = source T (ref sec 6.2); it is NOT a D:A / S:T
     * double-word compare. Skip-on-condition = skip the NEXT word when A == T. */
    r->t = 2;                              /* 120513 SAT 2                       */
    if (r->a != r->t) {                    /* 120514 SKP IF DA EQL ST skips 120515 when A==T */
        /* A != T (120515 JMP 4 -> 120521 taken): try selector 1 below         */
    } else {
        worker_D_116036(r);                /* 120516 JPL I 23 -> 116036B         */
        /* 120517 RAND 0 0 ; 120520 JMP -21 -> 120477 (shared tail)            */
        goto tail_120477;
    }

    r->t = 1;                              /* 120521 SAT 1                       */
    if (r->a != r->t)                      /* 120522 SKP IF DA EQL ST skips 120523 when A==T */
        goto tail_120534;                  /* 120523 JMP 11 -> 120534 (taken when A!=T) */
    if (!test_K_one_bit())                 /* 120524 BSKP ONE K (STS bit2) skips 120525 when K==1 */
        goto tail_120534;                  /* 120525 JMP 7  -> 120534 (taken when K==0) */
    /* K == 1: fall through into F1616 region (next stub) ...                   */

tail_120477:
tail_120534:
    rc = r->a;                             /* status left in accumulator (inferred) */
    return rc;
}

/* ----------------------------------------------------------------------------
 * Shared S3RPIT workers (called via inline JPL I pointer words). NOT reversed
 * individually here; signatures are placeholders. worker_B returns skip/no-skip.
 * ---------------------------------------------------------------------------- */
extern int  worker_B_116234(mon_regs *r);  /* @116234B, skip-return convention  */
extern void worker_C_116665(mon_regs *r);  /* @116665B                          */
extern void worker_D_116036(mon_regs *r);  /* @116036B                          */

/* Open questions for an emulator author:
 *   1. Purpose of MON 15B is undocumented (live call or dormant TSS carryover?).
 *   2. The JMP I -31 exit (@120461B) reads 000000B in the static image; it is
 *      almost certainly patched at runtime. A live trace is required to resolve
 *      where the skip-path actually returns.
 *   3. Semantics of workers 116234B / 116665B / 116036B are not established.
 */
