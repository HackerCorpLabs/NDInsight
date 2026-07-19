/* ============================================================================
 * MON 274B  GetFileIndexes (FOBJN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Line-by-line transcription of the real SINTRAN L bytes in
 * 274B-GetFileIndexes.ASM.  Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD Sx Dy" = Dy = Sx  (register copy); "RADD Sx Dy" = Dy = Dy + Sx
 *   - "BSET ONE/ZRO b Dr" with Dr=STS sets/clears STS bit b; SSK = STS bit2 (K)
 *   - "BSKP ONE/ZRO b Dr" skips NEXT instruction on bit b == 1 / 0
 *   - "SHA ZIN n" = A <<= n (zero fill); "SHA ZIN SHR n" = A >>= n (logical)
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 * Register model: A,D,T,X,B,L are 16-bit CPU registers; mem[] is word memory;
 * ind(a)=mem[a]; STS.K is the one-bit accumulator flag.  Field roles (which
 * word is dir / user / object / next-object index) are INFERRED from the manual
 * and from the identical build sequence in GUIOI (MON 217B).  Addresses octal.
 *
 * Skip-return convention (INFERRED, consistent across the worker): each
 * "JPL I <primitive>" is followed by a JMP to an error/exit; the primitive
 * returns to that JMP on FAILURE (no skip) and one word past it on SUCCESS.
 * ============================================================================ */

void mon_274B_FOBJN(void)   /* FOBJN @111210B (006-S3FS), bounded 111210..111500 */
{
    /* ---- mode-select entry: FOBJN sets STS.K, FOPFN clears it ------------- */
    STS.K = 1;                              /* 111210 BSET ONE SSK (FOBJN entry) */
    goto merge;                             /* 111211 JMP 2 -> 111213            */
    /* FOPFN @111212 would do STS.K = 0 here (the sibling open-by-name entry)   */
merge:
    mem[ind(P + 066)] = A;                  /* 111213 STD I 66 : stash caller A.. */
    mem[ind(P + 066) + 1] = D;              /*        ..and D (parameter block)   */
    A = L;                                  /* 111214 RADD CLD SL DA : A = L      */
    D = B;                                  /* 111215 RADD CLD SB DD : D = B      */
    B = 0122;                               /* 111216 SAB 122 : local frame 122W  */
    jpl(ind(P + 063));                      /* 111217 JPL I 63 -> 003752 prologue */
    /* 111220 BSKP ONE SSK : skip next when STS.K == 1 (FOBJN path) */
    if (STS.K == 0) {
        A = 1;                              /* 111222 SAA 1                       */
        mem[B + 0121] = A;                  /* 111223 STA ,B 121 : open-flag = 1  */
    } else {
        mem[B + 0121] = 0;                  /* 111225 STZ ,B 121 : open-flag = 0  */
    }
    A = mem[P + 055];                       /* 111226 LDA 55 (P-relative)         */
    A = do_add(A, B, 0);                    /* 111227 RADD SB DA : A = A + B       */
    mem[B + 0113] = A;                      /* 111230 STA ,B 113 : name-buf ptr    */
    T = mem[P + 053];                       /* 111231 LDT 53 (P-relative)          */
    if (!jpl(ind(P + 053)))                 /* 111232 JPL I 53 -> 031075 (parse)   */
        goto err;                           /* 111233 JMP I 53 -> 111306 (error)   */

    /* ---- resolve default directory / user (111234-111254) ---------------- */
    X = mem[B + 0113];                      /* 111234 LDX ,B 113                   */
    D = X;                                  /* 111235 RADD CLD SX DD : D = X       */
    A = mem[P + 051];                       /* 111236 LDA 51                       */
    if (!jpl(ind(P + 051)))                 /* 111237 JPL I 51 -> 031310 worker    */
        goto store_dir;                     /* 111240 JMP 143 -> 111403            */
    A = mem[P + 050];                       /* 111241 LDA 50                       */
    A = do_add(A, B, 0);                    /* 111242 RADD SB DA                    */
    T = 4;                                  /* 111243 SAT 4                        */
    X = mem[B + 2];                         /* 111244 LDX ,B 2                     */
    if (!jpl(ind(P + 040)))                 /* 111245 JPL I 40 -> 031075           */
        goto err;                           /* 111246 JMP I 40 -> 111306           */
    jpl(ind(P + 043));                      /* 111247 JPL I 43 -> 031312 worker    */
    A = mem[B + 0121];                      /* 111250 LDA ,B 121 : open-flag       */
    /* 111251 JAZ 46 : if A == 0 jump to build_from_dir (111317) */
    if (A == 0) goto build_from_dir;
    X = mem[B + 0113];                      /* 111252 LDX ,B 113                   */
    if (!jpl(ind(P + 040)))                 /* 111253 JPL I 40 -> 031313 (open)    */
        goto build_from_dir;                /* 111254 JMP 43 -> 111317             */

    /* ---- (open-by-name path 111255-111277: stash indexes into remote slot) - */
    A = X;                                  /* 111255 RADD CLD SX DA               */
    X = mem[ind(P + 036)];                  /* 111256 LDX I 36 (indirect)          */
    mem[X + 024] = A;                       /* 111257 STA ,X 24                    */
    A = mem[P + 031];                       /* 111260 LDA 31                       */
    A = do_add(A, B, 0);                    /* 111261 RADD SB DA                   */
    mem[X + 025] = A;                       /* 111262 STA ,X 25                    */
    A = mem[P + 032];                       /* 111263 LDA 32                       */
    A = do_add(A, B, 0);                    /* 111264 RADD SB DA                   */
    mem[X + 026] = A;                       /* 111265 STA ,X 26                    */
    X = 037;                                /* 111266 SAX 37                       */
    if (!jpl(ind(P + 027)))                 /* 111267 JPL I 27 -> 031316 worker    */
        goto err;                           /* 111270 JMP I 16 -> 111306           */
    A = mem[B + 0114];                      /* 111271 LDA ,B 114 : dir index       */
    mem[B + 1] = A;                         /* 111272 STA ,B 1  : -> DirIndex out  */
    A = mem[B + 0115];                      /* 111273 LDA ,B 115 : user index      */
    mem[B + 2] = A;                         /* 111274 STA ,B 2  : -> UserIndex out */
    A = mem[B + 0116];                      /* 111275 LDA ,B 116 : object index    */
    mem[B + 3] = A;                         /* 111276 STA ,B 3  : -> ObjectIndex   */
    { int t = mem[B+4]+1; mem[B+4] = t; }   /* 111277 MIN ,B 4 : success bump      */
    goto done;                              /* 111300 JMP 137 -> 111437            */

build_from_dir:                            /* 111317: walk directory for the name   */
    X = mem[B + 0113];                      /* 111317 LDX ,B 113                   */
    A = mem[P + 0145];                      /* 111320 LDA 145                      */
    A = do_add(A, B, 0);                    /* 111321 RADD SB DA                   */
    T = -1;                                 /* 111322 SAT -1                       */
    if (!jpl(ind(P + 0143)))                /* 111323 JPL I 143 -> object-scan     */
        goto store_dir;                     /* 111324 JMP 137 -> 111463            */
    mem[B + 0114] = T;                      /* 111325 STT ,B 114 : dir index       */
    X = A;                                  /* 111326 RADD CLD SA DX               */
    D = mem[X + 0];                         /* 111327 LDD ,X 0                     */
    mem[B + 0117] = A; mem[B+0117+1] = D;   /* 111330 STD ,B 117 : object entry    */
    X = mem[B + 0121];                      /* 111331 LDX ,B 121 : open-flag       */
    /* 111332 SKP IF DX EQL 0 : skip when X == 0 */
    if (X != 0) {
        if (!jpl(ind(P + 0133)))            /* 111334 JPL I 133 -> open primitive  */
            goto store_dir;                 /* 111335 JMP 46 -> 111403             */
    } else {
        goto pack_open;                     /* 111333 JMP 52 -> 111405             */
    }
    /* ---- classify access bits into ObjectIndex slot (111336-111347) ------- */
    A = mem[X + 044];                       /* 111336 LDA ,X 44                    */
    /* 111337 BSKP ZRO 10 DA : bit1; 111341 BSKP ONE 0 DA : bit0 */
    if (((A >> 1) & 1) != 0 && ((A >> 0) & 1) == 0) {
        A = mem[X + 045];                   /* 111343 LDA ,X 45                    */
        mem[B + 3] = A;                     /* 111344 STA ,B 3                     */
    } else {
        mem[B + 3] = 0;                     /* 111346 STZ ,B 3                     */
    }
    jpl(ind(P + 0121));                     /* 111347 JPL I 121 -> 031470 worker   */

    /* ---- directory table walk matching the parsed name (111350-111401) ---- */
    X = mem[P + 0121];                      /* 111350 LDX 121                      */
    T = mem[P + 0121];                      /* 111351 LDT 121                      */
scan:
    /* 111352 SKP IF DX UEQ ST : skip when X != T (end of table) */
    if (X == T) goto not_found;             /* 111353 JMP 27 -> 111402             */
    D = mem[X + 0];                         /* 111354 LDD ,X 0                     */
    /* 111355 JAF 2 : if A != 0 skip next */
    if (A == 0) A = D;                      /* 111356 RADD CLD SD DA               */
    /* 111357 JAZ 21 : if A == 0 -> 111400 (advance) */
    if (A == 0) goto advance;
    D = X;                                  /* 111360 RADD CLD SX DD               */
    X = A;                                  /* 111361 RADD CLD SA DX               */
    A = mem[X + 015];                       /* 111362 LDA ,X 15                    */
    T = mem[B + 0114];                      /* 111363 LDT ,B 114                   */
    /* 111364 SKP IF DA EQL ST : match dir index? */
    if (A == T) {
        A = mem[X + 016];                   /* 111366 LDA ,X 16                    */
        T = mem[B + 0120];                  /* 111367 LDT ,B 120                   */
        /* 111370 SKP IF DA EQL ST : match user index? */
        if (A == T) {
            A = mem[X + 6];                 /* 111372 LDA ,X 6                     */
            A = A >> 12;                     /* 111373 SHA ZIN SHR 14 : logical rt12*/
            T = mem[B + 0117];              /* 111374 LDT ,B 117                   */
            /* 111375 SKP IF DA UEQ ST : object slot differs? */
            if (A != T) goto pack_version;  /* 111376 JMP 43 -> 111441             */
        }
    }
    X = D;                                  /* 111377 RADD CLD SD DX               */
advance:
    X += 2;                                 /* 111400 AAX 2                        */
    goto scan;                              /* 111401 JMP -30 -> 111351            */
not_found:
    A = mem[P + 071];                       /* 111402 LDA 71 : error number        */
store_dir:
    mem[B + 2] = A;                         /* 111403 STA ,B 2 : status -> caller  */
    goto done;                              /* 111404 JMP 26 -> 111432             */

pack_open:                                  /* 111405: pack indexes for an open file */
    T = mem[B + 0114];                      /* 111405 LDT ,B 114                   */
    D = mem[B + 0117]; /* +low */           /* 111406 LDD ,B 117                   */
    if (!jpl(ind(P + 065)))                 /* 111407 JPL I 65 -> 031474 worker    */
        goto store_dir;                     /* 111410 JMP -5 -> 111403             */
    /* ---- build the returned index quadruple (111411-111431) --------------- *
     * Identical packing shape to GUIOI (MON 217B): the dir index sits in the
     * left byte and the user index in the right byte of one word (INDEX), the
     * object index and the next-object index are separate words.               */
    D = D << 8;                             /* 111411 SHD ZIN 10                   */
    D = D >> 8;                             /* 111412 SAD ZIN SHR 10 (logical)     */
    A = D;                                  /* 111413 RADD CLD SD DA               */
    mem[B + 3] = A;                         /* 111414 STA ,B 3 : ObjectIndex       */
    A = mem[B + 0114];                      /* 111415 LDA ,B 114 : dir index       */
    A = A << 8;                             /* 111416 SHA ZIN 10 : dir -> left byte*/
    T = A;                                  /* 111417 RADD CLD SA DT               */
    A = mem[B + 0120];                      /* 111420 LDA ,B 120 : user index      */
    A = A >> 8;                             /* 111421 SHA ZIN SHR 10               */
    A = do_add(A, T, 0);                    /* 111422 RADD ST DA : combine bytes   */
    mem[B + 1] = A;                         /* 111423 STA ,B 1 : INDEX (dir|user)  */
    D = mem[B + 0117];                      /* 111424 LDD ,B 117                   */
    D = D << 8;                             /* 111425 SHD ZIN 10                   */
    D = D >> 8;                             /* 111426 SAD ZIN SHR 10               */
    A = D;                                  /* 111427 RADD CLD SD DA               */
    mem[B + 2] = A;                         /* 111430 STA ,B 2 : next-object index */
    { int t = mem[B+4]+1; mem[B+4] = t; }   /* 111431 MIN ,B 4 : success bump      */
done:                                       /* 111432                              */
    T = mem[B + 0114];                      /* 111432 LDT ,B 114                   */
    X = mem[P + 042];                       /* 111433 LDX 42                       */
    if (!jpl(ind(P + 042)))                 /* 111434 JPL I 42 -> 031476 epilogue  */
        goto store_dir2;                    /* 111435 JMP 26 -> 111463             */
    jpl(ind(P + 041));                      /* 111436 JPL I 41 -> 031477 epilogue  */
    A = -0122;                              /* 111437 SAA -122 : frame teardown    */
    jpl_ret(mem[P + 040]);                  /* 111440 JMP I 40 -> 111500 = 003776  */
    return;

pack_version:                               /* 111441: emit next-version object idx  */
    A = D;                                  /* 111441 RADD CLD SD DA               */
    A = A - mem[P + 027];                   /* 111442 SUB 27                       */
    A = A >> 1;                             /* 111443 SHA ZIN SHR 1                */
    A += 0100;                              /* 111444 AAA 100                      */
    mem[B + 1] = A;                         /* 111445 STA ,B 1                     */
    { int tmp = D; D = X; X = tmp; }        /* 111446 SWAP SD DX : exchange D,X    */
    D = mem[X + 0];                         /* 111447 LDD ,X 0                     */
    /* 111450 JAF 3 : if A != 0 -> 111453 */
    if (A == 0) {
        T = 1;                              /* 111451 SAT 1                        */
        goto emit_ver;                      /* 111452 JMP 6 -> 111460              */
    }
    /* 111453 SKP IF DD EQL 0 */
    if (D == 0) { T = A; }                  /* 111455 RADD CLD 0 DT (T from A path)*/
    else        { T = 2; }                  /* 111457 SAT 2                        */
emit_ver:
    mem[B + 2] = T;                         /* 111460 STT ,B 2 : NextObjectIndex   */
    { int t = mem[B+4]+1; mem[B+4] = t; }   /* 111461 MIN ,B 4 : success bump      */
    goto done;                              /* 111462 JMP -30 -> 111432            */

err:
store_dir2:
    mem[B + 2] = A;                         /* 111463 STA ,B 2 : status word       */
    goto done_ret;                          /* 111464 JMP -25 -> 111437            */
done_ret:
    A = -0122;                              /* 111437 SAA -122                     */
    jpl_ret(mem[P + 040]);                  /* 111440 JMP I 40 -> 003776           */
}

/* Byte-verified anchors:
 *   FOBJN entry 111210 (BSET ONE SSK), FOPFN sibling 111212 (BSET ZRO SSK),
 *   shared body STD I 66 @111213, frame SAB 122, prologue JPL I 63 -> 003752,
 *   index-quadruple build 111411-111431 (SHA ZIN dir/user byte pack, identical
 *   to GUIOI/217B), success MIN ,B 4 @111277/111431/111461, resident return
 *   JMP I 40 -> 003776 @111500.
 * INFERRED: index-field roles, the error number at 111402, the exact primitive
 *   behind each JPL I link cell, and the skip-return polarity (see header).
 * UNVERIFIED: any link-cell target that matches no FILSYS symbol (031075,
 *   031310, 031312, 031313, 031316, 031470, 031474, 031476, 031477) - these are
 *   low-address resident helpers outside the file-system segment. */
