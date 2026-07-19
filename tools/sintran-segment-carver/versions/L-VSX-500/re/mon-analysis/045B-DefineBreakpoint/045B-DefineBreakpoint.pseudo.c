/* ============================================================================
 * MON 45B  DefineBreakpoint (DBRK)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Byte-verified GOTAB[45] target is the F1631 stub.
 *
 * Derived from the real disassembly (see 045B-DefineBreakpoint.ASM). The stub
 * CONTROL FLOW (the A==3 fork, the slot-clear sequence, the two JPL I worker
 * hand-offs) is BYTE-VERIFIED. The SEMANTIC labels below (breakpoint slot,
 * worker meaning) are INFERRED - the F1631 workers resolve to a message/queue
 * send family (SNDRE/PUTPO per SYMBOL-2-LIST) whose exact role in
 * DefineBreakpoint is UNVERIFIED. Treat this as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* GOTAB[45] = 121075B = F1631. Entry runs at level 14 with the caller arg in A. */
int mon_define_breakpoint(mon_regs *r)
{
    int a = r->A;                          /* 121075 JAN 100 tests sign of A       */
    if (a < 0)                             /* 121075: A negative -> alternate exit  */
        return alt_exit(r);                /*   -> 121175 (outside this stub)       */

    if (a == 3)                            /* 121076 SAT 3 / 121077 SKP EQL         */
        return worker_SNDRE(r);            /* 121101 JPL I 101 -> 121640 (UNVERIFIED)*/

    /* --- general slot path (A != 3), 121105-121121 --- */
    slot *s = &bp_table[/* index from ptr@121205 = 061157 */];  /* 121107 LDX I 76 */

    if (s->f14 == 0)                       /* 121110 LDA ,X 14 / 121111 JAZ         */
        return next_or_done(r);            /*   empty slot -> 121122 (F1632)        */

    s->f14 = 0;                            /* 121112 STZ ,X 14  release/re-arm slot */
    s->f46 = 0;                            /* 121113 STZ ,X 46                      */
    s->f4  = 0;                            /* 121114 STZ ,X 4                       */
    s->f2  = 0;                            /* 121115 STZ ,X 2                       */
    dword d = s->f15;                      /* 121117 LDD ,X 15  slot descriptor     */

    return worker_PUTPO(r, d);             /* 121121 JPL I 65 -> 115720 (UNVERIFIED)*/
}

/* General-path worker at 115720B (region B in the .ASM). Edits an X-indexed
 * table inside an IOF/ION critical section. Semantics INFERRED. */
int worker_PUTPO(mon_regs *r, dword d)
{
    IO_off();                              /* 115720 IOF - critical section         */
    if (r->X != 0) {                       /* 115721 JXZ -> skip edit if X == 0      */
        store_double_at_X(d);              /* 115726 STDTX                          */
        store_A_at_X();                    /* 115731 STATX                          */
        /* pointer housekeeping: AAX 2 / AAX -2, STD ,B 5 (115727-115735)          */
    }
    IO_on();                               /* 115736 ION                            */
    return /* EXIT 115737 */ OK;
}

/* Caveat: the F1631->worker hand-offs are real JPL I pointer words (121640,
 * 115720), byte-verified. The worker at 121211 -> 010341 lands in LOW resident
 * memory BELOW the carved overlay (load 32000B) and is runtime-populated:
 * NOT modelled here. */
