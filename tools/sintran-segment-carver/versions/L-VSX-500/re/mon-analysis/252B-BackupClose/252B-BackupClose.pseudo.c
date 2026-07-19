/* ============================================================================
 * MON 252B  BackupClose (BCLOS)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Transcription of the real SINTRAN L bytes in 252B-BackupClose.ASM.  BCLOS is
 * one of three MODE-select entries of a shared close dispatcher; the body @103357B
 * is common to BCLOS (BackupClose, 252B), SPERM (permanent-close) and CLOFI
 * (plain close).  Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - "BSET ONE/ZRO b Dr" with Dr=STS sets/clears STS bit b:
 *        SSM = STS bit7 (M), SSK = STS bit2 (K)
 *   - "BSKP ONE b Dr" skips the NEXT instruction when STS bit b == 1
 *   - "STD I 31" = mem[mem[P+31]] = A ; mem[..+1] = D  (indirect double store)
 *   - "STT I 26" = mem[mem[P+26]] = T  (indirect store)
 *   - "RADD CLD ST DA" = A = T ; bare "LDA ,B n" = mem[B+n]
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 * Register model: A,D,T,X,B,L are 16-bit CPU registers; mem[] is word memory;
 * STS.M / STS.K are status flags.  Addresses in comments are octal.
 * ============================================================================ */

void mon_252B_BCLOS(void)   /* BCLOS @103350B (006-S3FS), shared body 103357..103416 */
{
    /* ---- MODE-select entry: BCLOS = SSM 0, SSK 1 ------------------------- */
    STS.M = 0;                              /* 103350 BSET ZRO SSM               */
    STS.K = 1;                              /* 103351 BSET ONE SSK               */
    /* 103352 JMP 5 -> 103357 (merge into the shared close body)               *
     * (SPERM 103353: SSM 1 = permanent-close; CLOFI 103355: SSK 0 / SSM 0 =   *
     *  plain close)                                                            */

    mem[ind(P + 031)] = A;                  /* 103357 STD I 31 : stash caller A.. */
    mem[ind(P + 031) + 1] = D;              /*        ..and D (file# + flag)      */
    A = L;                                  /* 103360 RADD CLD SL DA : A = L      */
    D = B;                                  /* 103361 RADD CLD SB DD : D = B      */
    B = 6;                                  /* 103362 SAB 6 : local frame 6 words */
    jpl(ind(P + 026));                      /* 103363 JPL I 26 -> 003752 prologue */
    mem[ind(P + 026)] = T;                  /* 103364 STT I 26 : file number      */

    /* ---- MODE dispatch on the STS flags ---------------------------------- */
    /* 103365 BSKP ONE SSM : skip next when STS.M == 1 (SPERM permanent path) */
    if (STS.M) {
        A = T;                              /* 103367 RADD CLD ST DA : A = T      */
        if (jpl(ind(P + 023)))              /* 103370 JPL I 23 -> 072465 = SETPO  */
            goto success;                   /* 103372 JMP 11 -> 103403 (skip-ret) */
        goto store_status;                  /* 103371 JMP 15 -> 103406            */
    }
    /* 103373 BSKP ONE SSK : skip next when STS.K == 1 (BCLOS backup path) */
    if (STS.K) {
        A = mem[B + 2];                     /* 103375 LDA ,B 2 : modified flag    */
        if (jpl(ind(P + 016)))              /* 103376 JPL I 16 -> 067602 close-w/-flag */
            goto success;                   /* 103400 JMP 3 -> 103403 (skip-ret)  */
        goto store_status;                  /* 103377 JMP 7 -> 103406             */
    }
    /* ---- CLOFI plain close (neither flag set) ---------------------------- */
    if (jpl(ind(P + 014)))                  /* 103401 JPL I 14 -> 067612 = FCLOS  */
        goto success;                       /* (skip-return)                      */
    goto store_status;                      /* 103402 JMP 4 -> 103406             */

success:                                    /* 103403                              */
    { int t = mem[B+4]+1; mem[B+4] = t; }   /* 103403 MIN ,B 4 : success bump      */
    A = -6;                                 /* 103404 SAA -6 : frame teardown      */
    jpl_ret(mem[P + 011]);                  /* 103405 JMP I 11 -> 103416 = 003776  */
    return;

store_status:                               /* 103406                              */
    mem[B + 2] = A;                         /* 103406 STA ,B 2 : status -> caller  */
    /* 103407 JMP -3 -> 103404 : teardown without MIN ,B 4 => error (non-skip)  */
    A = -6;                                 /* 103404 SAA -6                       */
    jpl_ret(mem[P + 011]);                  /* 103405 JMP I 11 -> 003776           */
}

/* Byte-verified anchors:
 *   BCLOS entry 103350 (BSET ZRO SSM / BSET ONE SSK), SPERM 103353 (BSET ONE SSM),
 *   CLOFI 103355 (BSET ZRO SSK/SSM), shared body STD I 31 @103357, frame SAB 6,
 *   prologue JPL I 26 -> 003752, MODE dispatch 103365/103373 (BSKP ONE SSM/SSK),
 *   FCLOS call JPL I 14 -> 067612 (the same file-close primitive CLOSF/MON 43B
 *   calls), backup-with-flag close JPL I 16 -> 067602, permanent path JPL I 23 ->
 *   072465 (= SETPO, MON 236B worker), success MIN ,B 4 @103403, status store
 *   STA ,B 2 @103406, resident return JMP I 11 -> 003776 @103416.
 * INFERRED: that B+2 holds the caller's modified flag, and the skip-return
 *   polarity of the primitive calls. */
