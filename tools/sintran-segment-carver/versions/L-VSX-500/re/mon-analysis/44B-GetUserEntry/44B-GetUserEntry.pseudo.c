/* ============================================================================
 * MON 44B  GetUserEntry (GUSEN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Gets the user entry of a user (name, default file accesses, pages in use,
 * password, table of friends, and more). Only user RT and user SYSTEM may read
 * other users' entries.
 *
 * Derived from the real disassembly (see 44B-GetUserEntry.ASM), the GUSEN worker
 * at 055111B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). Control flow (the SSK
 * two-entry idiom, the resident-worker calls, the bit tests, the entry copy-out
 * and the error tail) is BYTE-VERIFIED. The register/field meanings (which frame
 * word is the buffer address, the user-name pointer) are INFERRED from the SINTRAN
 * III Monitor Calls manual MAC example and the code shape - treat as a model.
 * Addresses in comments are octal.
 *
 * Dispatch reality:
 *   GOTAB[44B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into the
 *   resident MFELL/CALLPROC second-level path (uncarved) which reaches GUSEN. So the
 *   MON 44 -> GUSEN link is NOT byte-followable statically; identity rests on the
 *   symbol NAME (GUSEN = Get USer ENtry) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ZRO/ONE SSK = clear/set skip flag;  RADD CLD Ss Dd = register copy;
 *   RADD SB DA = A = A + B;  RADD SX DA = A = A + X;  SAB/SAT/SAX/SAA n = set arg;
 *   BSKP ONE SSK = skip if skip-flag set;  BSKP ZRO/ONE nn DA = bit test on A;
 *   SKP IF DX EQL 0 = skip if X==0;  MIN ,B n = increment mem, skip on wrap;
 *   JPL I / JMP I = indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 44 GetUserEntry - MAC:
 *     LDA (BUFF  / LDX (USER / MON 44 / JMP ERROR
 *   A = address of a 64-byte buffer to receive the user entry (Buff)
 *   X = address of a string holding the user name (UserName; may include
 *       directory name, e.g. PACK-ONE:P-HANSEN)
 *   Error number returned in A. */

int mon_44B_GetUserEntry(mon_regs *r, int ssk_entry)   /* ssk_entry: 0 = GUSEN, 1 = NGUSN */
{
    /* 055111-055120: two-entry idiom, save the incoming A/D pair, stage L/B, call
     * the resident prologue worker (JPL I 55 -> [055175]).                        */
    int SSK = ssk_entry;                       /* 055111 BSET ZRO / 055113 BSET ONE */
    save_ad_pair(r->A, r->D);                  /* 055114: STD I 60                 */
    r->A = r->L;                               /* 055115: RADD CLD SL DA (copy)     */
    r->D = r->B;                               /* 055116: RADD CLD SB DD (copy)     */
    r->B = 07;                                 /* 055117: SAB 7                    */
    resident_prologue_worker();                /* 055120: JPL I 55 -> [055175]      */

    r->B[6] = SSK ? 1 : 0;                      /* 055121-055126: status word by entry */

    r->X = mem[pc_rel(055127, 047)];           /* 055127: LDX 47                   */
    if (resident_worker_055177_failed())       /* 055130 JPL I 47 / 055131 JMP -> tail */
        goto error_tail;
    if ((r->A & (1 << 15)) == 0) {             /* 055132: BSKP ZRO 170 DA           */
        r->A = 026; goto error_tail;           /* 055134 SAA 26 / 055135 JMP -> tail */
    }
    r->X = r->B[6];                            /* 055136: LDX ,B 6                 */
    if (r->X != 0) {                           /* 055137: SKP IF DX EQL 0           */
        if ((r->A & SELECT_BIT_130) == 0) {    /* 055141: BSKP ONE 130 DA           */
            r->A = 0147; goto error_tail;      /* 055143 SAA 147 / 055144 JMP -> tail */
        }
    }
    r->T = r->B[1];                            /* 055145: LDT ,B 1                 */
    r->A = r->B[2];                            /* 055146: LDA ,B 2                 */
    if (resident_worker_055200_failed())       /* 055147 JPL I 31 / 055150 JMP -> tail */
        goto error_tail;                       /*   (find user block)               */

    r->A = ((word *)r->X)[025];                /* 055151: LDA ,X 25                */
    if (r->A & (1 << 15)) {                     /* 055152: BSKP ONE 170 DA           */
        r->A = mem[pc_rel(055154, 025)] + r->X; /* 055154 LDA 25 / 055155 RADD SX DA */
        r->X = r->A;                           /* 055156: RADD CLD SA DX (copy)     */
        r->A = r->B[0];                        /* 055157: LDA ,B 0                 */
        r->T = 020;                            /* 055160: SAT 20 (transfer/access)  */
        resident_worker_055202();              /* 055161: JPL I 21 (copy entry out) */
    } else {
        r->A = mem[pc_rel(055163, 020)];       /* 055163: LDA 20                   */
        r->X = r->B[0];                        /* 055164: LDX ,B 0                 */
        ((word *)r->X)[0] = r->A;              /* 055165: STA ,X 0                 */
    }
    resident_worker_055204();                  /* 055166: JPL I 16                 */
    if (++mem_at_B4() == 0) {                   /* 055167: MIN ,B 4 (skip on wrap)   */
        r->A = (word)(-07);                    /* 055170: SAA -7 (error code)       */
        return indirect_return_055205(r);      /* 055171: JMP I 14 -> [055205]      */
    }
error_tail:
    r->B[2] = r->A;                            /* 055172: STA ,B 2                 */
    r->A = (word)(-07);                        /* 055173 JMP -3 -> 055170 (SAA -7)  */
    return indirect_return_055205(r);          /* 055171: JMP I 14 -> [055205]      */
}

/* Byte-verified anchors:
 *   GUSEN entry 055111 / NGUSN 055113 (006-S3FS), the SSK-selected status store
 *   (055121-055126), the resident-worker calls (JPL I -> [055175/055177/055200/
 *   055202/055204]), the bit tests (BSKP), the entry copy-out (055161) and the
 *   error-code tail (SAA -7 at 055170 / 055173).
 * NOT proven: the fall-through MON 44 -> GUSEN bridge (uncarved MFELL/CALLPROC);
 *   the semantic label of each B-frame word (INFERRED from the manual); the
 *   JPL I / JMP I link cells (055175..055205) are a pointer table (DATA) whose
 *   runtime targets are not resolved here. */
