/* ============================================================================
 * MON 246B  ReserveDir (RESDI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Reserves a directory for special use; other users cannot open files on it. The
 * directory must be entered and all its files closed. Release with ReleaseDir
 * (MON 247B). User RT and user SYSTEM.
 *
 * Derived from the real disassembly (see 246B-ReserveDir.ASM), the RESDI worker at
 * 107401B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). This is a SHARED body: the
 * SSK skip flag selects reserve (RESDI, SSK=0, this call) vs release (RELDI, SSK=1,
 * MON 247B). Control flow is BYTE-VERIFIED; the directory-index register meaning is
 * INFERRED from the manual MAC example. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[246B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into the
 *   resident MFELL/CALLPROC second-level path (uncarved) which reaches RESDI. So the
 *   MON 246 -> RESDI link is NOT byte-followable statically; identity rests on the
 *   symbol NAME (RESDI = REServe DIrectory) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ZRO/ONE SSK = clear/set skip flag;  RADD CLD Ss Dd = register copy;
 *   SAB n = set B;  LDX ,B n / LDX ,X n = indexed loads;  BSKP ONE SSK = skip if
 *   skip-flag set;  MIN ,B n = increment mem, skip on wrap;  JPL I / JMP I =
 *   indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 246 ReserveDir - MAC:  LDT DIRIX / MON 246 / JMP ERROR
 *   T = directory index (from @LIST-DIRECTORIES); error number returned in A. */

int mon_246B_ReserveDir(mon_regs *r, int ssk_entry) /* ssk_entry: 0 = RESDI (reserve), 1 = RELDI (release) */
{
    int SSK = ssk_entry;                       /* 107401 BSET ZRO / 107403 BSET ONE */
    save_ad_pair(r->A, r->D);                  /* 107404: STD I 23                 */
    r->A = r->L;                               /* 107405: RADD CLD SL DA (copy)     */
    r->D = r->B;                               /* 107406: RADD CLD SB DD (copy)     */
    r->B = 06;                                 /* 107407: SAB 6                    */
    resident_prologue_worker();                /* 107410: JPL I 20 (find directory) */

    r->X = r->B[5];                            /* 107411: LDX ,B 5 (dir datafield)  */
    r->X = ((word *)r->X)[031];                /* 107412: LDX ,X 31                */

    if (SSK) {                                  /* 107413: BSKP ONE SSK              */
        if (resident_worker_107431_failed())   /* 107415 JPL I 14 / 107416 JMP -> tail */
            goto error_tail;                   /*   (release directory)             */
    } else {
        if (resident_worker_107432_failed())   /* 107420 JPL I 12 / 107421 JMP -> tail */
            goto error_tail;                   /*   (reserve directory)             */
    }
    if (++mem_at_B4() == 0) {                   /* 107422: MIN ,B 4 (skip on wrap)   */
        r->A = (word)(-06);                    /* 107423: SAA -6 (error code)       */
        return indirect_return_107433(r);      /* 107424: JMP I 7 -> [107433]       */
    }
error_tail:
    r->B[2] = r->A;                            /* 107425: STA ,B 2                 */
    r->A = (word)(-06);                        /* 107426 JMP -3 -> 107423           */
    return indirect_return_107433(r);          /* 107424: JMP I 7 -> [107433]       */
}

/* Byte-verified anchors:
 *   RESDI/RELDI shared entry 107401/107403 with the SSK discriminator, the
 *   find-directory prologue (JPL I 20 -> [107430]), the directory-datafield walk
 *   (107411-107412), the reserve/release branch (BSKP ONE SSK at 107413 selecting
 *   JPL I 14 -> [107431] release vs JPL I 12 -> [107432] reserve) and the SAA -6
 *   return (107424, JMP I 7 -> [107433]).
 * NOT proven: the fall-through MON 246 -> RESDI bridge (uncarved MFELL/CALLPROC);
 *   the directory-datafield offsets 5/31 (INFERRED); the JPL I / JMP I link cells
 *   (107430..107433) are a pointer table (DATA) whose runtime targets are not
 *   resolved here. */
