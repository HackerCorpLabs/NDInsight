/* ============================================================================
 * MON 217B  GetAllFileIndexes (GUIOI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * This model is a line-by-line transcription of the real SINTRAN L bytes in
 * 217B-GetAllFileIndexes.ASM.  Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD Sx Dy" = Dy = Sx  (register copy)
 *   - "BSKP ZRO/ONE b Dr" skips NEXT instruction when bit b of Dr is 0 / 1
 *     (the printed number is b<<3)
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 * Register model: A,D,T,X,B,L are the 16-bit CPU registers; mem[] is word
 * memory; ind(a)=mem[a].  Field roles (which index word is dir / user / object)
 * remain INFERRED from the manual.  Addresses in comments are octal.
 *
 * Skip-return convention (INFERRED, consistent across the worker): each
 * "JPL I <primitive>" is followed by a JMP to the store-status exit; the
 * primitive returns to that JMP on FAILURE (no skip) and one word past it on
 * SUCCESS (skip).  So "call; JMP err; <success>" reads as
 * "if (!call()) goto err; <success>".
 * ============================================================================ */

void mon_217B_GUIOI(void)     /* GUIOI @105432B (006-S3FS), bounded by SFACC=105552 */
{
    mem[ind(P + 0104)] = A;                /* 105432 STD I 104 : stash caller A.. */
    mem[ind(P + 0104) + 1] = D;            /*        ..and D (parameter block)    */
    A = L;                                 /* 105433 RADD CLD SL DA : A = L       */
    D = B;                                 /* 105434 RADD CLD SB DD : D = B       */
    B = 037;                               /* 105435 SAB 37 : local frame = 037 W */
    jpl(ind(P + 0101));                    /* 105436 JPL I 101 -> 003752 prologue */
    A = mem[B + 2];                        /* 105437 LDA ,B 2                     */
    mem[ mem[P + 0100] ] = A;              /* 105440 STA I 100 (indirect store)   */
    jpl(ind(P + 0100));                    /* 105441 JPL I 100 -> 010376 worker   */
    /* 105442 JAF 2 : if A != 0 skip the next copy */
    if (A == 0)
        A = D;                             /* 105443 RADD CLD SD DA : A = D       */
    X = A;                                 /* 105444 RADD CLD SA DX : X = A       */
    /* 105445 JAF 3 : if A != 0 jump past the error store */
    if (A == 0) {
        A = 0132;                          /* 105446 SAA 132 : error 132 bad file# */
        goto store_status;                 /* 105447 JMP 65 -> 105534             */
    }
    A = mem[X + 3];                        /* 105450 LDA ,X 3                     */
    /* 105451 BSKP ZRO 160 DA : bit 14 of A; skip the JMP when bit14 == 0 */
    if (((A >> 14) & 1) == 0) {
        /* skip-return: FOPTB fails -> error 133, succeeds -> fall through */
        if (!jpl(ind(P + 067)))            /* 105453 JPL I 67 -> FOPTB 101043     */
            goto err133;                   /* 105454 JMP 57 -> 105533             */
    }
    /* 105452 JMP 3 -> 105455 taken when bit14 == 1 (FOPTB path skipped)         */
    A = mem[X + 7];                        /* 105455 LDA ,X 7                     */
    /* 105456 BSKP ONE 100 DA : bit 8 of A; skip the JMP when bit8 == 1 */
    if (((A >> 8) & 1) == 0)
        goto build_triple;                 /* 105457 JMP 33 -> 105512             */
    A = mem[X + 3];                        /* 105460 LDA ,X 3                     */
    /* 105461 BSKP ONE 160 DA : bit 14 of A; skip the JMP when bit14 == 1 */
    if (((A >> 14) & 1) == 0)
        goto build_triple;                 /* 105462 JMP 30 -> 105512             */

    /* ---- remote-id path (105463-105511): X+3 bit14 set and X+7 bit8 set ----- */
    T = X;                                 /* 105463 RADD CLD SX DT : T = X       */
    X = mem[ mem[P + 057] ];               /* 105464 LDX I 57 (indirect)          */
    mem[X + 024] = T;                      /* 105465 STT ,X 24                    */
    A = mem[P + 056];                      /* 105466 LDA 56 (P-relative)          */
    A = do_add(A, B, 0);                   /* 105467 RADD SB DA : A = A + B       */
    mem[X + 025] = A;                      /* 105470 STA ,X 25                    */
    mem[B + 036] = A;                      /* 105471 STA ,B 36                    */
    A = mem[P + 053];                      /* 105472 LDA 53 (P-relative)          */
    A = do_add(A, B, 0);                   /* 105473 RADD SB DA : A = A + B       */
    mem[X + 026] = A;                      /* 105474 STA ,X 26                    */
    X = 016;                               /* 105475 SAX 16                       */
    if (!jpl(ind(P + 050)))                /* 105476 JPL I 50 -> 020274 worker    */
        goto store_status;                 /* 105477 JMP 35 -> 105534 (no skip)   */
    A = mem[B + 3];                        /* 105500 LDA ,B 3   (skip-return)     */
    X = mem[B + 036];                      /* 105501 LDX ,B 36                    */
    T = 053;                               /* 105502 SAT 53                       */
    if (!jpl(ind(P + 044)))                /* 105503 JPL I 44 -> SUCPS 031072     */
        goto store_status;                 /* 105504 JMP 30 -> 105534 (no skip)   */
    A = mem[B + 6];                        /* 105505 LDA ,B 6   (skip-return)     */
    mem[B + 1] = A;                        /* 105506 STA ,B 1                     */
    A = mem[B + 7];                        /* 105507 LDA ,B 7                     */
    mem[B + 0] = A;                        /* 105510 STA ,B 0                     */
    goto success;                          /* 105511 JMP 17 -> 105530             */

build_triple:                              /* 105512: assemble the index triple    */
    T = mem[X + 015];                      /* 105512 LDT ,X 15                    */
    T = T << 8;                            /* 105513 SHT ZIN 10 : logical left 8  */
    A = mem[X + 016];                      /* 105514 LDA ,X 16                    */
    A = A >> 8;                            /* 105515 SHA ZIN SHR 10 : logical rt 8*/
    T = do_add(T, A, 0);                   /* 105516 RADD SA DT : T = T + A       */
    mem[B + 1] = T;                        /* 105517 STT ,B 1 : dir/user index    */
    A = mem[X + 6];                        /* 105520 LDA ,X 6                     */
    A = A >> 12;                           /* 105521 SHA ZIN SHR 14 : logical rt12*/
    A = A << 8;                            /* 105522 SHA ZIN 10 : logical left 8  */
    T = A;                                 /* 105523 RADD CLD SA DT : T = A       */
    A = mem[X + 016];                      /* 105524 LDA ,X 16                    */
    A = A & mem[P + 023];                  /* 105525 AND 23 (P-relative mask)     */
    A = do_add(A, T, 0);                   /* 105526 RADD ST DA : A = A + T       */
    mem[B + 0] = A;                        /* 105527 STA ,B 0 : object index      */

success:                                   /* 105530                              */
    { int t = mem[B + 4] + 1; mem[B + 4] = t; /* 105530 MIN ,B 4 : return-link bump = skip-return (success) */ }
    A = -037;                              /* 105531 SAA -37 : = -(frame size), the resident-return teardown token */
    jpl_ret(mem[P + 017]);                 /* 105532 JMP I 17 -> 003776 resident return */
    return;

err133:
    A = 0133;                              /* 105533 SAA 133 : error 133 not open */
store_status:
    mem[B + 2] = A;                        /* 105534 STA ,B 2 : status -> caller  */
    goto success_return_no_bump;           /* 105535 JMP -4 -> 105531             */
success_return_no_bump:
    /* falls onto 105531 (SAA -37 / JMP I 17); no MIN ,B 4 bump, so the caller
     * takes the NON-skip (error) return with the status word in B+2 */
    A = -037;                              /* 105531 SAA -37                      */
    jpl_ret(mem[P + 017]);                 /* 105532 JMP I 17 -> 003776           */
}

/* Byte-verified anchors:
 *   GUIOI entry 105432 (STD I 104), frame SAB 37, prologue JPL I 101 -> 003752,
 *   FOPTB call JPL I 67 -> 101043, SUCPS call JPL I 44 -> 031072,
 *   MIN ,B 4 success bump at 105530, SAA -37 teardown token, status store
 *   STA ,B 2 at 105534, resident return JMP I 17 -> 003776.
 * INFERRED: index-field roles, error numbers 0132/0133, and the skip-return
 *   polarity of the primitive calls (see header). */
