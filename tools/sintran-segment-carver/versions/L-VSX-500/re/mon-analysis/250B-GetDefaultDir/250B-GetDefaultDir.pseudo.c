/* ============================================================================
 * MON 250B  GetDefaultDir (FDFDI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Gets the user's default directory: the directory index and the user index are
 * returned for the named user (the name may identify a remote user). All users.
 *
 * Derived from the real disassembly (see 250B-GetDefaultDir.ASM), the FDFDI worker
 * at 106732B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). Control flow (the SSK
 * two-entry idiom, the name-parse, the named-vs-current-user branch and the return
 * of the directory/user indexes) is BYTE-VERIFIED; the register/field meanings are
 * INFERRED from the manual MAC example and the code shape. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[250B] = 066242B -> a 4-word entry stub F1732 in 025-S3IRPIT (byte-proven
 *   value). The stub sits in a shared stub block and does not itself reach FDFDI; the
 *   real transfer is the resident CALLPROC (uncarved). So the MON 250 -> FDFDI link
 *   is NOT byte-followable statically; identity rests on the symbol NAME (FDFDI =
 *   Find DeFault DIrectory) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ZRO/ONE SSK = clear/set skip flag;  RADD CLD Ss Dd = register copy;
 *   RADD SB DA/DX = add B;  SAB/SAT/SAX/SAA n = set arg;  JAF d = jump if flag false;
 *   SKP IF DA EQL SX = skip if A==X;  MIN ,B n = increment mem, skip on wrap;
 *   STT/STX ,B n = store T/X to frame;  JPL I / JMP I = indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 250 GetDefaultDir - MAC:
 *     LDX (USER / MON 250 / JMP ERROR / STT DIRIX / STX USRIX
 *   X = address of a string holding the user name
 *   On return T = directory index, X = user index. */

int mon_250B_GetDefaultDir(mon_regs *r, int ssk_entry) /* ssk_entry: 1 = FDFDI, 0 = FDINA */
{
    int SSK = ssk_entry;                       /* 106732 BSET ONE / 106734 BSET ZRO */
    save_ad_pair(r->A, r->D);                  /* 106735: STD I 123                */
    r->A = r->L;                               /* 106736: RADD CLD SL DA (copy)     */
    r->D = r->B;                               /* 106737: RADD CLD SB DD (copy)     */
    r->B = 075;                                /* 106740: SAB 75                   */
    resident_prologue_worker();                /* 106741: JPL I 120 -> [107061]     */

    r->B[074] = SSK ? 1 : 0;                    /* 106742-106747: decode-name flag   */
    r->B[071] = mem[pc_rel(106750,0112)] + r->B; /* 106750-106752: name buffer ptr  */
    r->X = r->B[0];                            /* 106753: LDX ,B 0                 */
    r->T = 0146;                               /* 106754: SAT 146                  */
    if (resident_worker_107063_failed())       /* 106755 JPL I 106 / 106756 JMP -> tail */
        goto error_tail;                       /*   (parse user name)               */
    r->D = r->A; r->X = r->A;                   /* 106757-106760: copies             */
    r->A = mem[pc_rel(106761,0103)];           /* 106761: LDA 103                  */
    if (resident_worker_107065_failed())       /* 106762 JPL I 103 / 106763 JMP -> tail */
        goto error_tail;

    if (flag_false(r->B[074])) {                /* 106764-106765: LDA ,B 74 / JAF     */
        /* ---- no name given: use the current user (106766 path skipped) ----      */
        goto use_current_user;                 /* 106765: JAF 21 -> 107006          */
    }

    /* ---- named user (106766-107005) ---- */
    r->D = mem_ind(107066);                     /* 106766-106767                    */
    if (resident_worker_107067_failed_or_next()) /* 106770: JPL I 77 (find named user) */
        { /* fallthrough into 106771 on success */ }
    r->X = mem[pc_rel(106771,071)] + r->B;     /* 106771-106772                    */
    if (resident_worker_107070_failed())       /* 106773 JPL I 75 / 106774 JMP -> tail(052) */
        goto error_tail2;
    r->A = r->D;                               /* 106775: RADD CLD SD DA            */
    resident_worker_107071();                  /* 106776: JPL I 73                 */
    r->B[1] = r->T;                            /* 106777: STT ,B 1                 */
    r->X = mem[pc_rel(107000,072)];            /* 107000: LDX 72                   */
    if (resident_worker_107073_failed())       /* 107001 JPL I 72 / 107002 JMP -> tail */
        goto error_tail;
    r->B[2] = r->A & mem[pc_rel(107003,071)];  /* 107003-107004                    */
    goto success_tail;                         /* 107005: JMP 31 -> 107036          */

use_current_user:
    r->A = mem[pc_rel(107006,067)];            /* 107006: LDA 67                   */
    r->X = r->B[071];                          /* 107007: LDX ,B 71                */
    if (resident_worker_107076_failed())       /* 107010 JPL I 66 / 107011 JMP -> 107026 */
        goto alt_user;
    r->T = r->X;                               /* 107012: RADD CLD SX DT            */
    { word *e = (word *)mem_ind(107077);       /* 107013: LDX I 64                 */
      e[024] = r->T;                           /* 107014: STT ,X 24                */
      e[025] = mem[pc_rel(107015,063)] + r->B; /* 107015-107017                    */
    }
    r->X = 034;                                /* 107020: SAX 34                   */
    if (resident_worker_107101_failed())       /* 107021 JPL I 60 / 107022 JMP -> tail */
        goto error_tail;
    r->T = r->B[072];                          /* 107023: LDT ,B 72 (directory idx) */
    r->A = r->B[073];                          /* 107024: LDA ,B 73 (user idx)      */
    goto do_return;                            /* 107025: JMP 7 -> 107034           */

alt_user:
    r->X = mem[pc_rel(107026,034)] + r->B;     /* 107026-107027                    */
    if (resident_worker_107102_failed())       /* 107030 JPL I 52 / 107031 JMP -> tail */
        goto error_tail;
    if (resident_worker_107103_next())         /* 107032 JPL I 51 / 107033 JMP 6 -> 107041 */
        goto check_user;

do_return:
    r->B[1] = r->T;                            /* 107034: STT ,B 1 (directory index) */
    r->B[2] = r->A;                            /* 107035: STA ,B 2 (user index)      */
success_tail:
    if (++mem_at_B4() == 0) {                   /* 107036: MIN ,B 4 (skip on wrap)   */
        r->A = (word)(-075);                   /* 107037: SAA -75 (error code)      */
        return indirect_return_107104(r);      /* 107040: JMP I 44 -> [107104]      */
    }
    /* falls into error_tail on non-wrap */

check_user:
    r->X = 073;                                /* 107041: SAX 73                   */
    if (r->A != r->X) {                         /* 107042: SKP IF DA EQL SX          */
        goto error_tail2;                      /* 107043: JMP 7 -> 107052           */
    }
    r->A = r->T;                               /* 107044: RADD CLD ST DA            */
    resident_worker_107105();                  /* 107045: JPL I 40                 */
    /* 107046-107051: store X and rejoin the success advance                        */
    r->B[2] = r->X;                            /* 107050: STX ,B 2                 */
    goto success_tail;                         /* 107051: JMP -12 -> 107037         */

error_tail2:                                   /* 107052-107055: error via resident */
    r->B[2] = r->A;                            /* 107052: STA ,B 2                 */
    r->A = r->D;                               /* 107053: RADD CLD SD DA            */
    resident_worker_107071();                  /* 107054: JPL I 15                 */
    r->A = (word)(-075); return indirect_return_107104(r); /* 107055 -> 107037       */
error_tail:
    r->B[2] = r->A;                            /* 107056: STA ,B 2                 */
    r->A = (word)(-075);                       /* 107057 JMP -20 -> 107037          */
    return indirect_return_107104(r);
}

/* Byte-verified anchors:
 *   FDFDI entry 106732 / FDINA 106734 (006-S3FS), the parse-name calls (JPL I ->
 *   [107063/107065]), the named-vs-current-user branch (JAF at 106765), the index
 *   returns (STT ,B 1 / STA/STX ,B 2 at 107034-107035), and the SAA -75 return
 *   (107037, JMP I 44 -> [107104]).
 * NOT proven: the GOTAB[250]=066242 stub -> FDFDI bridge (uncarved CALLPROC); the
 *   semantic label of each B-frame word (INFERRED); some resident-worker return
 *   conventions are modelled loosely; the JPL I / JMP I link cells (107061..107105)
 *   are a pointer table (DATA) whose runtime targets are not resolved here. */
