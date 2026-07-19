/* ============================================================================
 * MON 41B  ReadObjectEntry (ROBJE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * This model is a line-by-line transcription of the real SINTRAN L bytes in
 * 41B-ReadObjectEntry.ASM.  Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD Sx Dy" = Dy = Sx  (register copy); "RADD Sx Dy" = Dy = Dy + Sx
 *   - "SKP IF Dx <cond> Sy" skips the NEXT instruction; UEQ skips when x != y,
 *     EQL skips when x == y
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 * Register model: A,D,T,X,B,L are the 16-bit CPU registers; mem[] is word
 * memory; ind(a)=mem[a].  Region A (F1627 @121023B) is 8 words of vector /
 * link-cell DATA, not code, so it is not modelled here.  Field roles remain
 * INFERRED from the FILSYS symbol names + manual.  Addresses in comments octal.
 *
 * Skip-return convention (INFERRED, consistent across the worker): each
 * "JPL I <primitive>" that is followed by a JMP to the store-status exit uses a
 * skip-return - the primitive returns to that JMP on FAILURE (no skip) and one
 * word past it on SUCCESS (skip).  So "call; JMP err; <ok>" reads as
 * "if (!call()) goto err; <ok>".
 * ============================================================================ */

void mon_41B_ROBJE(void)      /* ROBJE @055566B (006-S3FS), bounded by next FILSYS symbol */
{
    mem[ind(P + 0140)] = A;                /* 055566 STD I 140 : stash caller A.. */
    mem[ind(P + 0140) + 1] = D;            /*        ..and D                      */
    A = L;                                 /* 055567 RADD CLD SL DA : A = L       */
    D = B;                                 /* 055570 RADD CLD SB DD : D = B       */
    B = 010;                               /* 055571 SAB 10 : local frame = 010 W */
    jpl(ind(P + 0135));                    /* 055572 JPL I 135 -> 003752 prologue */
    jpl(ind(P + 0135));                    /* 055573 JPL I 135 -> FOBJB 055563    */
    mem[B + 7] = X;                        /* 055574 STX ,B 7                     */
    A = mem[X + 0];                        /* 055575 LDA ,X 0                     */
    jpl(ind(P + 0133));                    /* 055576 JPL I 133 -> 010500 worker   */
    A = mem[X + 1];                        /* 055577 LDA ,X 1                     */
    T = mem[B + 1];                        /* 055600 LDT ,B 1                     */
    /* 055601 SKP IF DA UEQ ST : skips the JMP when A != T */
    if (A == T)
        goto cmp_entry;                    /* 055602 JMP 11 -> 055613             */

    /* ---- A != mem[B+1]: seed a fresh entry pointer (055603-055612) ---------- */
    A = -1;                                /* 055603 SAA -1                       */
    mem[X + 1] = A;                        /* 055604 STA ,X 1                     */
    A = mem[P + 0125];                     /* 055605 LDA 125 (P-relative)         */
    X = do_add(X, A, 0);                   /* 055606 RADD SA DX : X = X + A       */
    A = mem[B + 2]; D = mem[B + 3];        /* 055607 LDD ,B 2                     */
    ad_shift_right_logical(&A,&D, 8);      /* 055610 SAD ZIN SHR 10 : (A:D) >>= 8 */
    ad_shift_left_logical (&A,&D, 3);      /* 055611 SAD ZIN 3     : (A:D) <<= 3  */
    goto directory_read;                   /* 055612 JMP 26 -> 055640             */

cmp_entry:                                 /* 055613: A == mem[B+1]                */
    A = mem[X + 2];                        /* 055613 LDA ,X 2                     */
    T = mem[B + 2];                        /* 055614 LDT ,B 2                     */
    /* 055615 SKP IF DA EQL ST : skips the JMP when A == T */
    if (A != T)
        goto reindex;                      /* 055616 JMP 4 -> 055622              */
    A = mem[X + 3];                        /* 055617 LDA ,X 3                     */
    /* 055620 SKP IF DA UEQ SD : skips the JMP when A != D */
    if (A == D)
        goto copy_out;                     /* 055621 JMP 64 -> 055705             */

reindex:                                   /* 055622                              */
    T = -1;                                /* 055622 SAT -1                       */
    mem[X + 1] = T;                        /* 055623 STT ,X 1                     */
    A = mem[P + 0106];                     /* 055624 LDA 106 (P-relative)         */
    A = do_add(A, mem[B + 7], 0);          /* 055625 ADD ,B 7                     */
    X = A;                                 /* 055626 RADD CLD SA DX : X = A       */
    A = mem[B + 2]; D = mem[B + 3];        /* 055627 LDD ,B 2                     */
    ad_shift_right_logical(&A,&D, 8);      /* 055630 SAD ZIN SHR 10               */
    ad_shift_left_logical (&A,&D, 3);      /* 055631 SAD ZIN 3                    */
    T = mem[X + 0];                        /* 055632 LDT ,X 0                     */
    /* 055633 SKP IF DA EQL ST : skips the JMP when A == T */
    if (A != T)
        goto directory_read;               /* 055634 JMP 4 -> 055640              */
    T = mem[X + 1];                        /* 055635 LDT ,X 1                     */
    /* 055636 SKP IF DD UEQ ST : skips the JMP when D != T */
    if (D == T)
        goto block_lookup;                 /* 055637 JMP 14 -> 055653             */

directory_read:                            /* 055640                              */
    mem[X + 0] = A; mem[X + 1] = D;        /* 055640 STD ,X 0                     */
    T = mem[B + 1];                        /* 055641 LDT ,B 1                     */
    X = mem[P + 071];                      /* 055642 LDX 71 (P-relative)          */
    if (!jpl(ind(P + 071)))                /* 055643 JPL I 71 -> GDDRT 050121     */
        goto store_status;                 /* 055644 JMP 55 -> 055721 (no skip)   */
    X = mem[B + 7];                        /* 055645 LDX ,B 7   (skip-return)     */
    T = mem[P + 064];                      /* 055646 LDT 64 (P-relative)          */
    X = do_add(X, T, 0);                   /* 055647 RADD ST DX : X = X + T       */
    T = mem[B + 1];                        /* 055650 LDT ,B 1                     */
    if (!jpl(ind(P + 064)))                /* 055651 JPL I 64 -> RINDX 051453     */
        goto store_status;                 /* 055652 JMP 47 -> 055721 (no skip)   */

block_lookup:                              /* 055653                              */
    A = mem[B + 3];                        /* 055653 LDA ,B 3                     */
    A = A >> 4;                            /* 055654 SHA ZIN SHR 4 : logical rt 4 */
    A = A & mem[P + 061];                  /* 055655 AND 61 (P-relative)          */
    A = do_add(A, mem[P + 061], 0);        /* 055656 ADD 61 (P-relative)          */
    A = do_add(A, mem[B + 7], 0);          /* 055657 ADD ,B 7                     */
    X = A;                                 /* 055660 RADD CLD SA DX : X = A       */
    A = mem[X + 0]; D = mem[X + 1];        /* 055661 LDD ,X 0                     */
    /* 055662 JAF 3 : if A != 0 jump past the D test */
    if (A == 0) {
        /* 055663 SKP IF DD UEQ 0 : skips the JMP when D != 0 */
        if (D == 0)
            goto clear_slot;               /* 055664 JMP 32 -> 055716             */
    }
    T = mem[B + 1];                        /* 055665 LDT ,B 1                     */
    A = A & mem[P + 052];                  /* 055666 AND 52 (P-relative)          */
    if (!jpl(ind(P + 052)))                /* 055667 JPL I 52 -> RBLOC 035531     */
        goto store_status;                 /* 055670 JMP 31 -> 055721 (no skip)   */

    /* ---- resolved block present: read + release the buffer (055671-055704) -- */
    mem[B + 6] = X;                        /* 055671 STX ,B 6   (skip-return)     */
    X = A;                                 /* 055672 RADD CLD SA DX : X = A       */
    A = mem[B + 3];                        /* 055673 LDA ,B 3                     */
    A = A & mem[P + 046];                  /* 055674 AND 46 (P-relative)          */
    A = A << 5;                            /* 055675 SHA ZIN 5 : logical left 5   */
    X = do_add(X, A, 0);                   /* 055676 RADD SA DX : X = X + A       */
    A = mem[B + 7];                        /* 055677 LDA ,B 7                     */
    A = do_add(A, mem[P + 043], 0);        /* 055700 ADD 43 (P-relative)          */
    T = 040;                               /* 055701 SAT 40                       */
    jpl(ind(P + 042));                     /* 055702 JPL I 42 -> 001224 worker (plain call) */
    X = mem[B + 6];                        /* 055703 LDX ,B 6                     */
    jpl(ind(P + 041));                     /* 055704 JPL I 41 -> RELBU 035476 (plain call) */

copy_out:                                  /* 055705: copy resolved entry to caller */
    { int t = mem[B + 4] + 1; mem[B + 4] = t; /* 055705 MIN ,B 4 : return-link bump = skip-return (success) */ }
    X = mem[B + 7];                        /* 055706 LDX ,B 7                     */
    mem[B + 0] = X;                        /* 055707 STX ,B 0                     */
    A = mem[B + 1];                        /* 055710 LDA ,B 1                     */
    mem[X + 1] = A;                        /* 055711 STA ,X 1                     */
    A = mem[B + 2]; D = mem[B + 3];        /* 055712 LDD ,B 2                     */
    mem[X + 2] = A; mem[X + 3] = D;        /* 055713 STD ,X 2                     */
resident_return:
    A = -010;                              /* 055714 SAA -10 : = -(frame size), the resident-return teardown token */
    jpl_ret(mem[P + 031]);                 /* 055715 JMP I 31 -> 003776 resident return */
    return;

clear_slot:                                /* 055716                              */
    X = mem[B + 7];                        /* 055716 LDX ,B 7                     */
    mem[X + 026] = 0;                      /* 055717 STZ ,X 26                    */
    goto copy_out;                         /* 055720 JMP -13 -> 055705            */

store_status:                              /* 055721: error path (primitive no-skip) */
    mem[B + 2] = A;                        /* 055721 STA ,B 2 : status -> caller  */
    X = mem[B + 7];                        /* 055722 LDX ,B 7                     */
    A = mem[X + 0];                        /* 055723 LDA ,X 0                     */
    jpl(ind(P + 023));                     /* 055724 JPL I 23 -> 010506 worker    */
    goto resident_return;                  /* 055725 JMP -11 -> 055714            */
}

/* Byte-verified anchors:
 *   ROBJE entry 055566 (STD I 140), frame SAB 10, prologue JPL I 135 -> 003752,
 *   FOBJB 055563, GDDRT 050121, RINDX 051453, RBLOC 035531, RELBU 035476,
 *   MIN ,B 4 success bump at 055705, SAA -10 teardown token, status store
 *   STA ,B 2 at 055721, resident return JMP I 31 -> 003776.
 * INFERRED: object-entry field roles, the meaning of the (B+1,B+2,B+3) vs
 *   (X+1,X+2,X+3) compares, and the skip-return polarity of the primitive
 *   calls (see header). */
