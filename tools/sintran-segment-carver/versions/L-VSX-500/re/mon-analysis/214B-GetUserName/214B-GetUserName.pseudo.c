/* ============================================================================
 * MON 214B  GetUserName (GUSNA)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Returns the name of a user (16 chars) given a directory index and a user
 * index. With the COSMOS network installed the user may live on a remote
 * computer, in which case the remote system name is also returned. RT programs
 * return the name of user RT.
 *
 * Derived from the real disassembly (see 214B-GetUserName.ASM), the GUSNA worker
 * at 105301B in segment 006-S3FS. GUSNA is a FILSYS-SYMBOLS symbol in the file-
 * system segment. Control flow (the local/remote branch on LDA ,B 0 + JAP, the
 * resident-worker calls, the field stores, the LBYT name-byte fetch and the
 * error tail) is BYTE-VERIFIED. The register/field meanings (which frame word is
 * the name buffer address, the packed index, the remote flag) are INFERRED from
 * the SINTRAN III Monitor Calls manual MAC example and the code shape - treat as
 * a model, not gospel. Addresses in comments are octal.
 *
 * Dispatch reality:
 *   GOTAB[214B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into
 *   the resident MFELL/CALLPROC second-level path (uncarved) which reaches GUSNA.
 *   So the MON 214 -> GUSNA link is NOT byte-followable statically; identity
 *   rests on the symbol NAME (GUSNA = Get USer NAme) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction
 * semantics reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   RADD CLD Ss Dd = register copy;  RADD SB DA = A = A + B;  SAB/SAT/SAX n = set arg;
 *   JAP d = jump if A>=0;  BSET ZRO 15 DA = clear bit 15;  SHT ZIN SHR = logical
 *   right shift;  SKP IF DX MGRE SA = unsigned X>=A skip;  LBYT = load byte;
 *   MIN ,B n = increment mem, skip on wrap;  JPL I / JMP I = indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 214 GetUserName - MAC:
 *     LDA (USER  / LDX INDEX / LDT (REMID / MON 214 / JMP ERROR
 *   A = address of a 16-char string buffer to receive the user name (UserName)
 *   X = packed index: left byte = directory index, right byte = user index;
 *       bit 15 set = remote user (DirectoryIndex, UserIndex, RemoteFlag)
 *   T = address of the remote-system identification string (RemoteSystem),
 *       used only when bit 15 of X is set.
 *   Error number returned in A. */

int mon_214B_GetUserName(mon_regs *r)          /* in: UserName, DirIndex, UserIndex, REMID */
{
    /* 105301-105305: save the incoming A/D pair, stage L/B, call the resident
     * prologue worker (JPL I 104 -> [105411]).                                  */
    save_ad_pair(r->A, r->D);                  /* 105301: STD I 107               */
    r->A = r->L;                               /* 105302: RADD CLD SL DA (copy)   */
    r->D = r->B;                               /* 105303: RADD CLD SB DD (copy)   */
    r->B = 063;                                /* 105304: SAB 63                  */
    resident_prologue_worker();                /* 105305: JPL I 104 -> [105411]   */

    r->A = r->B[0];                            /* 105306: LDA ,B 0                 */
    if ((short)r->A >= 0)                      /* 105307: JAP 36 -> 105345         */
        goto remote_path;

    /* ---- local-user path (105310-105344) ---- */
    r->A = mem[0105412] + r->B;                /* 105310 LDA 102 / 105311 RADD SB DA */
    r->T = 0105;                               /* 105312: SAT 105                 */
    r->X = r->B[1];                            /* 105313: LDX ,B 1                */
    if (resident_worker_105413_failed())       /* 105314 JPL I 77 / 105315 JMP -> tail */
        goto error_tail;
    r->D = r->A;                               /* 105316: RADD CLD SA DD (copy)   */
    r->X = r->A;                               /* 105317: RADD CLD SA DX (copy)   */
    r->A = mem[0105414];                       /* 105320: LDA 74                  */
    if (resident_worker_105415_failed())       /* 105321 JPL I 74 / 105322 JMP -> tail */
        goto error_tail;
    r->A = r->B[0] & ~(1 << 15);               /* 105323 LDA ,B 0 / 105324 BSET ZRO 15 DA */
    r->B[6] = r->A;                            /* 105325: STA ,B 6                */
    {
        word *e = (word *)mem[ind(0105416)];   /* 105330: LDX I 66 -> entry ptr   */
        e[024] = mem[0105412] + r->B;          /* 105326-105331 store name words  */
        e[025] = mem[0105413] + r->B;          /* 105332-105334                   */
        e[026] = mem[0105412] + r->B;          /* 105335-105337                   */
        r->B[062] = e[026];                    /* 105340: STA ,B 62               */
    }
    r->X = 015;                                /* 105341: SAX 15                  */
    if (resident_worker_105421_failed())       /* 105342 JPL I 57 / 105343 JMP -> tail */
        goto error_tail;
    goto common_tail;                          /* 105344: JMP 32 -> 105376        */

    /* ---- remote / COSMOS path (105345-105375) ---- */
remote_path:
    r->T = r->B[0] >> 8;                        /* 105345 LDT ,B 0 / 105346 SHT ZIN SHR 10 */
    resident_worker_105422();                  /* 105347: JPL I 53 -> [105422]    */
    if (!((unsigned)r->X >= (unsigned)mem[0105423])) { /* 105350-105352 SKP IF DX MGRE SA */
        r->A = mem[0105424];                   /* 105353: LDA 51 (error code)     */
        goto error_tail;                       /* 105354: JMP 32 -> 105406        */
    }
    r->A = r->B[0] & mem[0105425];             /* 105355 LDA ,B 0 / 105356 AND 47 */
    r->X = mem[0105420] + r->B;                /* 105357 LDX 41 / 105360 RADD SB DX */
    if (resident_worker_105426_failed())       /* 105361 JPL I 45 / 105362 JMP -> tail */
        goto error_tail;
    r->A = mem[0105420] + r->B;                /* 105363 LDA 35 / 105364 RADD SB DA */
    r->T = r->A;                               /* 105365: RADD CLD SA DT (copy)   */
    r->B[062] = r->A;                          /* 105366: STA ,B 62               */
    r->X = 0;                                  /* 105367: SAX 0                   */
    {
        int ch = load_byte(r->T, r->X);        /* 105370: LBYT - fetch a name byte */
        if (ch != 077) {                       /* 105371 SAT 77 / 105372 skip-if-== */
            r->A = mem[0105427];               /* 105374: LDA 33 (error code)     */
            goto error_tail;                   /* 105375: JMP 11 -> 105406        */
        }
    }

    /* ---- common tail (105376-105410) ---- */
common_tail:
    r->A = r->B[2];                            /* 105376: LDA ,B 2                */
    r->X = r->B[062];                          /* 105377: LDX ,B 62               */
    r->T = 020;                                /* 105400: SAT 20                  */
    if (resident_worker_105430_failed())       /* 105401 JPL I 27 / 105402 JMP -> tail */
        goto error_tail;
    if (++mem_at_B4() == 0) {                  /* 105403: MIN ,B 4 (skip on wrap) */
        r->A = (word)(-063);                   /* 105404: SAA -63 (error code)    */
        return indirect_return_105431(r);      /* 105405: JMP I 24 -> [105431]    */
    }
error_tail:
    r->B[2] = r->A;                            /* 105406: STA ,B 2                */
    r->A = (word)(-063);                       /* 105407 JMP -3 -> 105404 (SAA -63) */
    return indirect_return_105431(r);          /* 105405: JMP I 24 -> [105431]    */
}

/* Byte-verified anchors:
 *   GUSNA entry 105301 (006-S3FS), the local/remote branch JAP 36 (105307), the
 *   resident-worker calls (JPL I -> [105411/105413/105415/105421/105422/105426/
 *   105430]), the BSET ZRO 15 DA bit-15 clear (105324), the LBYT name-byte fetch
 *   (105370), and the error-code tail (SAA -63 at 105404 / 105407).
 * NOT proven: the fall-through MON 214 -> GUSNA bridge (uncarved MFELL/CALLPROC);
 *   the semantic label of each B-frame word and the exact meaning of the packed X
 *   index / remote bit (INFERRED from the manual MAC example); the JPL I / JMP I
 *   link cells (105411..105431) are a pointer table (DATA) whose runtime targets
 *   are not resolved here. */
