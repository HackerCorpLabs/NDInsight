/* ============================================================================
 * MON 253B  NewFileVersion (CRALN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Transcription of the real SINTRAN L bytes in 253B-NewFileVersion.ASM.  CRALN is
 * the MODE-1 entry of a shared create/allocate/access/expand dispatcher; the body
 * @105564B is common to SFACC (237B), EXPFI (231B), CRALN (253B) and CRALF.
 * Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - "BSET ONE/ZRO b Dr" with Dr=STS sets/clears STS bit b:
 *        SSM = STS bit7 (M, multi-shift link), SSK = STS bit2 (K, one-bit accum)
 *   - "SHA LIN n" shifts A left n, filling vacated bit0 from the STS M bit
 *     (LIN fill is EMULATOR-AUTHORITATIVE)
 *   - "BSET BAC 0 DA" sets A bit0 = STS.K (EMULATOR-AUTHORITATIVE over carry)
 *   - bare "LDA disp" = mem[P+disp]; "LDA I disp" = mem[mem[P+disp]] (indirect)
 *   - "RADD CLD Sx Dy" = Dy = Sx; "RADD Sx Dy" = Dy = Dy + Sx
 *   - "SKP IF DA LST ST" skips NEXT instruction when A < T (signed less-than)
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 * Register model: A,D,T,X,B,L are 16-bit CPU registers; mem[] is word memory;
 * STS.M / STS.K are status flags.  Addresses in comments are octal.
 * ============================================================================ */

void mon_253B_CRALN(void)   /* CRALN @105560B (006-S3FS), shared body 105564..106042 */
{
    /* ---- MODE-select entry: CRALN = SSK 1, SSM 0  => mode 1 --------------- */
    STS.K = 1;                              /* 105560 BSET ONE SSK               */
    /* 105561 JMP 2 -> 105563 */
    STS.M = 0;                              /* 105563 BSET ZRO SSM (merge tail)  */
    /* fall into the shared body at 105564                                      *
     * (SFACC 105552: SSM 1 / SSK 1 = mode 3; EXPFI 105555: SSM 1 / SSK 0 =     *
     *  mode 2; CRALF 105562: SSK 0 / SSM 0 = mode 0)                           */

    mem[ind(P + 077)] = A;                  /* 105564 STD I 77 : stash caller A.. */
    mem[ind(P + 077) + 1] = D;              /*        ..and D (parameter block)   */
    A = L;                                  /* 105565 RADD CLD SL DA : A = L      */
    D = B;                                  /* 105566 RADD CLD SB DD : D = B      */
    B = 0145;                               /* 105567 SAB 145 : local frame 145W  */
    jpl(ind(P + 074));                      /* 105570 JPL I 74 -> 003752 prologue */

    /* ---- rebuild the 2-bit MODE word from the STS M / K flags ------------- */
    A = 0;                                  /* 105571 RADD CLD 0 DA : A = 0       */
    A = (A << 2) | (STS.M ? 1 : 0);         /* 105572 SHA LIN 2 : left 2, M fill  */
    A = (A & ~1) | (STS.K ? 1 : 0);         /* 105573 BSET BAC 0 DA : bit0 = K    */
    mem[B + 0123] = A;                      /* 105574 STA ,B 123 : MODE = (M<<1)|K */

    /* ---- marshal caller parameter pointers into the local frame ---------- *
     * For NewFileVersion the caller passes the file name (with version number),
     * the FirstPage double-word, and the NoOfPages double-word.               */
    A = mem[ind(P + 070)]; mem[B + 0140] = A; /* 105575-105576 LDA I 70 / STA ,B 140 */
    A = mem[ind(P + 067)]; mem[B + 0141] = A; /* 105577-105600 LDA I 67 / STA ,B 141 */
    A = mem[ind(P + 066)]; mem[B + 0142] = A; /* 105601-105602 LDA I 66 / STA ,B 142 */
    A = mem[P + 065]; A = do_add(A, B, 0); mem[B + 0143] = A; /* 105603-105605 name buf */
    A = mem[P + 063]; A = do_add(A, B, 0); mem[B + 0144] = A; /* 105606-105610       */
    A = mem[P + 061]; A = do_add(A, B, 0); mem[B + 0135] = A; /* 105611-105613       */
    A = mem[P + 057]; A = do_add(A, B, 0); mem[B + 0136] = A; /* 105614-105616       */
    A = mem[P + 055]; A = do_add(A, B, 0); mem[B + 0137] = A; /* 105617-105621       */

    /* ---- resolve the file name (105622-105626) --------------------------- */
    X = mem[B + 0];                         /* 105622 LDX ,B 0                    */
    A = mem[B + 0143];                      /* 105623 LDA ,B 143 : name buffer    */
    T = mem[P + 051];                       /* 105624 LDT 51                      */
    if (!jpl(ind(P + 051)))                 /* 105625 JPL I 51 -> 031075 (parse)  */
        goto store_status;                  /* 105626 JMP 162 -> 106010           */

    /* ---- MODE dispatch (105627-105662) ----------------------------------- *
     * CRALN is MODE 1: it falls to the MODE < 3 (create / allocate) arm.      */
    A = mem[B + 0123];                      /* 105627 LDA ,B 123 : MODE           */
    T = 3;                                  /* 105630 SAT 3                       */
    /* 105631 SKP IF DA LST ST : skip next when MODE < 3 (TRUE for CRALN=1) */
    if (A >= 3) {
        /* MODE 3 (SetFileAccess) arm - not taken for CRALN */
        X = mem[B + 1];                     /* 105633 LDX ,B 1                    */
        A = mem[B + 0144];                  /* 105634 LDA ,B 144                  */
        T = 2;                              /* 105635 SAT 2                       */
        if (!jpl(ind(P + 041)))             /* 105636 JPL I 41 -> 031067          */
            goto store_status;              /* 105637 JMP 151 -> 106010           */
        goto epilogue;                      /* 105640 JMP 20 -> 105660            */
    }
    /* ---- MODE < 3: create / allocate new version (105641-105657) ---------- */
    X = mem[B + 2];                         /* 105641 LDX ,B 2 : FirstPage        */
    A = mem[B + 0135];                      /* 105642 LDA ,B 135                  */
    T = 6;                                  /* 105643 SAT 6                       */
    if (!jpl(ind(P + 032)))                 /* 105644 JPL I 32 -> 031075          */
        goto store_status;                  /* 105645 JMP 143 -> 106010           */
    X = mem[B + 3];                         /* 105646 LDX ,B 3 : NoOfPages        */
    A = mem[B + 0136];                      /* 105647 LDA ,B 136                  */
    T = 6;                                  /* 105650 SAT 6                       */
    if (!jpl(ind(P + 025)))                 /* 105651 JPL I 25 -> 031075          */
        goto store_status;                  /* 105652 JMP 136 -> 106010           */
    X = mem[B + 1];                         /* 105653 LDX ,B 1                    */
    A = mem[B + 0137];                      /* 105654 LDA ,B 137                  */
    T = 6;                                  /* 105655 SAT 6                       */
    if (!jpl(ind(P + 020)))                 /* 105656 JPL I 20 -> 031075          */
        goto store_status;                  /* 105657 JMP 131 -> 106010           */

epilogue:                                   /* 105660: success epilogue             */
    { int t = mem[B+4]+1; mem[B+4] = t; }   /* 105777 MIN ,B 4 (via 105660 -> 105721 -> 105777) */
    A = mem[B + 0140]; mem[ind(P + 036)] = A; /* 106000-106001 restore caller words */
    A = mem[B + 0141]; mem[ind(P + 035)] = A; /* 106002-106003                     */
    A = mem[B + 0142]; mem[ind(P + 034)] = A; /* 106004-106005                     */
    A = -0145;                              /* 106006 SAA -145 : frame teardown   */
    jpl_ret(mem[P + 033]);                  /* 106007 JMP I 33 -> 106042 = 003776  */
    return;

store_status:                               /* 106010                              */
    mem[B + 2] = A;                         /* 106010 STA ,B 2 : status -> caller  */
    /* falls onto 106000 frame restore without MIN ,B 4 => error (non-skip)     */
    A = mem[B + 0140]; mem[ind(P + 036)] = A;
    A = mem[B + 0141]; mem[ind(P + 035)] = A;
    A = mem[B + 0142]; mem[ind(P + 034)] = A;
    A = -0145;
    jpl_ret(mem[P + 033]);                  /* -> 003776 resident return           */
}

/* Byte-verified anchors:
 *   CRALN entry 105560 (BSET ONE SSK, then 105563 BSET ZRO SSM => mode 1),
 *   shared body STD I 77 @105564, frame SAB 145, prologue JPL I 74 -> 003752,
 *   MODE build 105571-105574 (SHA LIN 2 + BSET BAC 0 DA => (M<<1)|K), MODE
 *   dispatch 105627-105631 (SAT 3 / SKP IF DA LST ST), success MIN ,B 4 @105777,
 *   status store STA ,B 2 @106010, resident return JMP I 33 -> 003776 @106042.
 * INFERRED: mapping of FirstPage / NoOfPages to the B+2 / B+3 / B+135.. slots and
 *   the MODE-1 arm assignment.
 * UNVERIFIED: the identity of each JPL I link cell (031067, 031075) - low-address
 *   resident helpers matching no FILSYS symbol; and the exact success route
 *   through 105660 -> 105721 -> 105777 (the mid-body sub-blocks 105700-105776
 *   handle the create/allocate variants and are modelled only at the spine). */
