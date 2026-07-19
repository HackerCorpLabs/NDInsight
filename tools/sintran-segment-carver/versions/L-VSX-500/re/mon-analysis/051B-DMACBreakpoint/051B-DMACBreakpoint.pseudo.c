/* ============================================================================
 * MON 51B  DMACBreakpoint  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Debugger-family call (same block as MON 45B
 * DefineBreakpoint). "Assembly breakpoints": patch MAC instructions of the OS
 * or a subsystem (DMAC subsystem / @LOOK-AT).
 *
 * Derived from the real disassembly (see 051B-DMACBreakpoint.ASM). The DISPATCH
 * spine (GOTAB[51]=121147B -> JMP 4 -> body -> JMP I 33 -> worker 117107B) is
 * BYTE-VERIFIED. The register/frame SEMANTICS are INFERRED - treat as a model,
 * not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Level-14 entry: GOTAB[51B] = 121147B (F1633 stub, in 025-S3IRPIT overlay).   */
int mon_dmac_breakpoint(mon_regs *r)
{
    /* 121147 JMP 4: skip 3 alt-entry words, enter body at 121153.              */

    /* 121153-121160: marshal working registers for the shared worker.          */
    r->b = load_word(0o061157);            /* 121153 LDA I 32 -> ptr@121205 (in-seg data) */
    r->x = load_word(0o004007);            /* 121155 LDA I 36 -> ptr@121213 (resident, UNVERIFIED src) */
    r->d = frame_read(r->b, 021);          /* 121157 LDA ,B 21 -> input param (patch addr/word? inferred) */

    /* 121161 JMP I 33 through pointer word 121214B = 117107B: tail-transfer     */
    /* into the shared debugger worker (Region B). This is the real work.        */
    return dmac_worker(r);                  /* worker @117107B (REDOM/PRIMH)      */
}

/* Worker @117107B (025-S3IRPIT). Byte-verified control flow; semantics inferred.*/
int dmac_worker(mon_regs *r)
{
    /* 117107 SWAP SB DD exchanges B and D (ref sec 3.3: plain SWAP is a FULL
     * register exchange, not a copy), so 117110-117112 (LDA ,B 7; AAA -1;
     * STA ,B 7) decrement the counter at mem[D+7] - D being the input-param
     * pointer set by the stub (121160). 117113 SWAP SB DD swaps B and D back. */
    frame_dec(r->d, 07);                    /* 117107-117113: --counter at mem[D+7]  */

    void *desc = frame_read(r->b, 0);       /* 117114 LDX ,B 0: descriptor pointer (B restored) */
    /* 117115-117117 SUB I 22 / JAF: classify the request; three table walks     */
    /*   (LDX I 20 / LDX I 11) set/test flag bits via BSET/BSKP, then join 117136.*/
    classify_and_flag(desc);

    /* 117137 JMP I 2 through pointer word 117141B = 010331B: escape to a         */
    /* RESIDENT low-memory routine (RETST?) that is NOT in any carved segment.    */
    return resident_escape(0o010331);       /* UNVERIFIED - target body not carved   */
}

/* Notes:
 *   - The exact per-register DMAC contract (which reg holds the patch address vs
 *     the instruction word, error codes) is NOT recoverable from these bytes;
 *     it lives in the resident 010331B tail. Capture it with a live trace.
 *   - MON 45B (DefineBreakpoint) enters an adjacent F16xx stub of the same shape.
 */
