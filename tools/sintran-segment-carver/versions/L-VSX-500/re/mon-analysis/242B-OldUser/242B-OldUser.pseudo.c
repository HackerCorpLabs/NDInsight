/* ============================================================================
 * MON 242B  OldUser (RUSCN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Switches back to the user name in effect before NewUser (MON 241B). Always resets
 * the FIRST user name; has no function if NewUser was not executed. All users.
 *
 * Derived from the real disassembly (see 242B-OldUser.ASM), the RUSCN worker at
 * 106562B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). Control flow (the
 * "was NewUser done?" guard, the saved-name restore, the friend-table copy loop and
 * the error tails) is BYTE-VERIFIED. The register/field meanings are INFERRED from
 * the code shape and the NewUser twin - treat as a model. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[242B] = 066212B -> a 4-word entry stub F1727 in 025-S3IRPIT (byte-proven
 *   value). The stub sits in a shared stub block and does not itself reach RUSCN;
 *   the real transfer is the resident CALLPROC (uncarved). So the MON 242 -> RUSCN
 *   link is NOT byte-followable statically; identity rests on the symbol NAME (RUSCN,
 *   the OldUser twin of SUSCN) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   RADD CLD Ss Dd = register copy;  RADD SB DA / SX DA / SD DA = adds;  SHA ZIN SHR
 *   10 = logical right shift 8;  SAB/SAT/SAX n = set arg;  SKP IF DA UEQ ST = skip if
 *   A!=T (unsigned);  SKP IF DT GRE SX = skip if (signed) T>X;  AAA n = A += n;
 *   MIN ,B n = increment mem, skip on wrap;  JPL I / JMP I = indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 242 OldUser - MAC:  MON 242 / JMP ERROR
 *   No input parameters; error number returned in A. */

int mon_242B_OldUser(mon_regs *r)
{
    /* 106562-106566: save the incoming A/D pair, stage L/B, call the resident
     * prologue worker (JPL I 115 -> [106703]).                                   */
    save_ad_pair(r->A, r->D);                  /* 106562: STD I 120                */
    r->A = r->L;                               /* 106563: RADD CLD SL DA (copy)     */
    r->D = r->B;                               /* 106564: RADD CLD SB DD (copy)     */
    r->B = 012;                                /* 106565: SAB 12                   */
    resident_prologue_worker();                /* 106566: JPL I 115 -> [106703]     */

    /* 106567-106575: guard - if the saved-name slots are still -1 (no NewUser done)
     * take the no-op / error tail.                                                 */
    if (mem_ind(106704) == (word)(-1))         /* 106567-106572                    */
        goto noop_tail;
    if (mem_ind(106705) == (word)(-1))         /* 106573-106575                    */
        goto noop_tail;

    resident_worker_106706();                  /* 106576: JPL I 110                */
    if (resident_worker_106707_failed())       /* 106577 JPL I 110 / 106600 JMP -> tail */
        goto error_tail;

    /* 106601-106617: rebuild the old coded name and clear the saved-name flag.     */
    mem_ind(106710) = (mem_ind(106710) >> 8) & mem[pc_rel(106605,0104)]; /* SHA ZIN SHR / AND */
    mem_ind(106704) = mem_ind(106713);         /* 106607-106610: restore first name */
    mem_ind(106705) = (word)(-1);              /* 106612-106613: clear saved flag   */
    resident_worker_106720();                  /* 106620: JPL I 100 (re-enter user) */

    r->T = mem_ind(106704) >> 8;               /* 106621-106623: high byte to T     */
    r->B[6] = r->T;                            /* 106624: STA ,B 6                 */
    resident_worker_106721();                  /* 106625: JPL I 74                 */
    r->B[011] = r->X;                          /* 106626: STX ,B 11                */
    r->A = ((word *)r->X)[3];                  /* 106627: LDA ,X 3                 */
    resident_worker_106722();                  /* 106630: JPL I 72                 */
    r->B[7] = mem_ind(106704) & mem[pc_rel(106632,057)]; /* 106631-106633           */
    if (resident_worker_106723_failed())       /* 106634 JPL I 67 / 106635 JMP -> tail(675) */
        goto error_tail2;

    /* 106636-106645: merge access bits back into the object/user access word.      */
    r->D = (((word *)r->X)[025] + 1) & mem[pc_rel(106640,051)]; /* 106636-106641     */
    ((word *)r->X)[025] = (((word *)r->X)[025] & mem[pc_rel(106643,061)]) + r->D; /* 106642-106645 */

    /* 106646-106660: copy the 8-word friend table into the user datafield.         */
    r->B[010] = mem[pc_rel(106646,057)] + r->X; /* 106646-106650                    */
    r->X = 0;                                  /* 106651: RADD CLD 0 DX             */
    r->T = 010;                                /* 106652: SAT 10 (8 words)          */
    for (; (short)r->T > (short)r->X; r->X++) { /* 106653: SKP IF DT GRE SX          */
        mem_ind(r->B[010] + r->X) = mem_ind(r->B[010] + r->X); /* 106655-106657 copy */
    }                                          /* 106660: JMP -6 -> 106652          */

    r->T = r->B[6];                            /* 106661: LDT ,B 6                 */
    r->A = r->B[7];                            /* 106662: LDA ,B 7                 */
    if (resident_worker_106727_failed())       /* 106663 JPL I 44 / 106664 JMP -> tail(675) */
        goto error_tail2;
    r->X = r->B[011];                          /* 106665: LDX ,B 11                */
    r->A = ((word *)r->X)[3];                  /* 106666: LDA ,X 3                 */
    resident_worker_106730();                  /* 106667: JPL I 41                 */

noop_tail:
    if (++mem_at_B4() == 0) {                   /* 106670: MIN ,B 4 (skip on wrap)   */
        r->A = (word)(-012);                   /* 106671: SAA -12 (error code)      */
        return indirect_return_106731(r);      /* 106672: JMP I 37 -> [106731]      */
    }
error_tail:
    r->B[2] = r->A;                            /* 106673: STA ,B 2                 */
    r->A = (word)(-012);                       /* 106674 JMP -3 -> 106671           */
    return indirect_return_106731(r);

error_tail2:                                   /* 106675-106701: release then return */
    r->B[2] = r->A;                            /* 106675: STA ,B 2                 */
    r->X = r->B[011];                          /* 106676: LDX ,B 11                */
    r->A = ((word *)r->X)[3];                  /* 106677: LDA ,X 3                 */
    resident_worker_106730();                  /* 106700: JPL I 30 (release)        */
    r->A = (word)(-012); return indirect_return_106731(r); /* 106701 -> 106671       */
}

/* Byte-verified anchors:
 *   RUSCN entry 106562 (006-S3FS), the "no NewUser done" guard (106567-106575), the
 *   saved-name restore (106607-106613), the friend-table copy loop (106652-106660,
 *   SKP IF DT GRE SX), the install/release resident-worker calls and the error tails
 *   converging on the SAA -12 return (106671, JMP I 37 -> [106731]).
 * NOT proven: the GOTAB[242]=066212 stub -> RUSCN bridge (uncarved CALLPROC); the
 *   semantic label of each B-frame word (INFERRED); the JPL I / JMP I link cells
 *   (106703..106731) are a pointer table (DATA) whose runtime targets are not
 *   resolved here. */
