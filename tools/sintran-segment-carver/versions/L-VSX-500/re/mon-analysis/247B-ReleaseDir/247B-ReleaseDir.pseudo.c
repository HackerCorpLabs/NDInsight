/* ============================================================================
 * MON 247B  ReleaseDir (RELDI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Releases a directory that was reserved with ReserveDir (MON 246B).
 *
 * Derived from the real disassembly (see 247B-ReleaseDir.ASM), the RELDI worker at
 * 107403B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). This is a SHARED body: the
 * SSK skip flag selects reserve (RESDI, SSK=0, MON 246B) vs release (RELDI, SSK=1,
 * this call). Control flow is BYTE-VERIFIED; the directory-index register meaning is
 * INFERRED from the manual MAC example. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[247B] = 066226B -> a 4-word entry stub F1731 in 025-S3IRPIT (byte-proven
 *   value). The stub sits in a shared stub block and does not itself reach RELDI; the
 *   real transfer is the resident CALLPROC (uncarved). So the MON 247 -> RELDI link
 *   is NOT byte-followable statically; identity rests on the symbol NAME (RELDI =
 *   RELease DIrectory, the release entry of the shared RESDI/RELDI body) - see README.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ZRO/ONE SSK = clear/set skip flag;  RADD CLD Ss Dd = register copy;
 *   SAB n = set B;  LDX ,B n / LDX ,X n = indexed loads;  BSKP ONE SSK = skip if
 *   skip-flag set;  MIN ,B n = increment mem, skip on wrap;  JPL I / JMP I =
 *   indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 247 ReleaseDir - MAC:  LDT DIRIX / MON 247 / JMP ERROR
 *   T = directory index (from @LIST-DIRECTORIES); error number returned in A. */

int mon_247B_ReleaseDir(mon_regs *r, int ssk_entry) /* ssk_entry: 0 = RESDI (reserve), 1 = RELDI (release, this call) */
{
    int SSK = ssk_entry;                       /* 107401 BSET ZRO / 107403 BSET ONE */
    save_ad_pair(r->A, r->D);                  /* 107404: STD I 23                 */
    r->A = r->L;                               /* 107405: RADD CLD SL DA (copy)     */
    r->D = r->B;                               /* 107406: RADD CLD SB DD (copy)     */
    r->B = 06;                                 /* 107407: SAB 6                    */
    resident_prologue_worker();                /* 107410: JPL I 20 (find directory) */

    r->X = r->B[5];                            /* 107411: LDX ,B 5 (dir datafield)  */
    r->X = ((word *)r->X)[031];                /* 107412: LDX ,X 31                */

    if (SSK) {                                  /* 107413: BSKP ONE SSK (release path) */
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
 *   (107411-107412), the release branch (BSKP ONE SSK at 107413 selecting JPL I 14
 *   -> [107431] release) and the SAA -6 return (107424, JMP I 7 -> [107433]).
 * NOT proven: the GOTAB[247]=066226 stub -> RELDI bridge (uncarved CALLPROC); the
 *   directory-datafield offsets 5/31 (INFERRED); the JPL I / JMP I link cells
 *   (107430..107433) are a pointer table (DATA) whose runtime targets are not
 *   resolved here. */
