/* ============================================================================
 * MON 505B  GetTrapReason (GERRC)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 level-12 (stop-MOCALL) monitor call.
 *
 * Derived from the real disassembly (see 505B-GetTrapReason.ASM). The carved
 * 51-word window is BYTE-EXACT and its symbol boundary (GERRC=141633B ..
 * 5SIBM=141716B) is confirmed in L07. Control flow inside the window is
 * byte-read; the SEMANTIC labels below (which table, which field, which
 * register is cleared) are INFERRED from instruction shape and the folder's
 * "read then clear trap error code" description - treat as a model, not gospel.
 * Addresses in comments are octal.
 *
 * Dispatch note: this handler is reached via the ND-500 level-12 GOSW
 * (5CMNO/L12MIN, index 5), NOT via the ND-100 GOTAB. GOTAB[505] holds an
 * unrelated ND-100 stub (T105R) - see README "Honest caveats". The
 * GOSW[5] -> GERRC link is asserted by the folder, UNVERIFIED by any tool run.
 * ============================================================================ */

void mon_get_trap_reason(nd500_msg *m)
{
    /* 141633-141641: compute a per-process slot address.
     * LDT I 50 / LDATX / SUB I 47 / AAA 1 / MPY 46 / ADD 46 / RADD.
     * Shape = base + process_index * record_size.  INFERRED which table.   */
    slot = proc_table_base + proc_index * REC_SIZE;   /* INFERRED */

    /* 141642-141653: read two fields off an X base at disp 57, 60, then a
     * RADD register-shuffle chain.  Plausibly the per-proc trap/error record. */
    x    = slot;                                        /* 141642 LDX 45      */
    a    = mem[x + 057];                                /* 141643 LDA ,X 57   */
    a   += mem[x + 060];                                /* 141644 ADD ,X 60   */
    /* ... RADD moves between A/D/T/X (141645-141653) ...  INFERRED */

    /* 141654-141657: LDDTX / STZTX / AAX 1 / STZTX = double-word ZERO store
     * to the indexed location.  This is the "clear the error register after
     * reading" step.  VERIFIED as a zeroing store; INFERRED as to the target. */
    err = mem_dword[x];                                 /* 141654 LDDTX       */
    mem_dword[x] = 0;                                   /* 141655-141657 STZ  */

    /* 141660-141677: LDX I 30 / LDT I 22 / AAX .. / STDTX / STATX / STZTX -
     * stage words into an indexed output block.  Plausibly builds the reply
     * message (M505E error field + NUMPA=1).  INFERRED field mapping.       */
    build_reply(m, /*M505E=*/ err, /*NUMPA=*/ 1);       /* INFERRED */

    /* 141700-141702: escape via indirect pointer words (data at 141711-13):
     *   141700 JPL I 11 -> @141711 = 023044B   (worker)
     *   141701 JPL I 11 -> @141712 = 145466B   (worker)
     *   141702 JMP I 11 -> @141713 = 135067B   (tail jump)
     * Words 141711/12/13 are indirect-pointer DATA, so their in-line decode
     * is a data/code overlap artefact.  Exact flow past 141702 is NOT
     * byte-provable.  UNVERIFIED targets.                                    */
    call_worker(0023044);                               /* 141700 JPL I 11    */
    call_worker(0145466);                               /* 141701 JPL I 11    */
    goto_worker(0135067);                               /* 141702 JMP I 11    */
    /* 141714-141715: 000000 000000 - alignment/padding, not code. */
}

/* Caller: ND-500 process issues MON 505B; level-12 driver routes GOSW[5] here.
 * Out: reply message field M505E = trap error code, NUMPA = 1 (per README).   */
