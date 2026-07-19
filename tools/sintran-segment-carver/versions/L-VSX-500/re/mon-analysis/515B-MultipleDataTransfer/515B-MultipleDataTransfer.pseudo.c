/* ============================================================================
 * MON 515B  MultipleDataTransfer (5MTRA)  -  pseudo-C model for an emulator.
 * SINTRAN III VSX/500 L.  ND-500 level-12 GOSW call (5MTRANS; GOSW index 13).
 *
 * Source: real SINTRAN L bytes, 5MTRA @143445B in 026-S3IMPIT.bin, load 32000B.
 * Every instruction below is translated per
 *   ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md
 * CONTROL FLOW is byte-verified against 515B-MultipleDataTransfer.ASM (every DIRECT
 * branch resolves inside 143445B..143643B). Field/status SEMANTICS (which field is
 * a function code, what each literal means) are INFERRED from instruction shape and
 * marked inline. Addresses in comments are octal. No unicode below.
 *
 * This is a 5MTRANS memory-transfer call, so the T/X-indexed transfers are the core
 * of the handler. LDDTX / LDATX / STDTX form a 24-bit PHYSICAL address that BYPASSES
 * the MMU / page tables - grounded in the reference, section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * disp3 = (opcode >> 3) & 7 = 0 in every transfer here (the code adjusts X with AAX).
 * T is the BANK (high byte), X the word offset; T is loaded from mem[004654B] - the
 * ND-500 message-buffer bank - so these read/write the message buffer at physical
 * bank:offset. phys[] is that 24-bit physical space.
 *
 * Entry convention (VERIFIED by 143447 RADD CLD SB DA / 143450 STA I 141):
 *   B = base of the ND-500 process message / control block. X = a caller index.
 * ============================================================================ */

#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */

#define BANK  0004654   /* msg-buffer physical bank; the I-mode ptrs at +0141/+62/+54 point here */

/* Pool words 143607B..143642B are DATA reached only as INDIRECT pointers (never
 * executed): resident field addresses (011xxx), the BANK word (004654B), resident
 * worker routine addresses, and NXTMSG. nd100-dis renders them as instructions -
 * disregard those mnemonics. The exits below jump/call THROUGH these words.
 * 143643B (LDA I 127) is the ONE word in this range that is code: a direct JMP
 * target that runs on into the uncarved continuation past the window. */
#define P_W144021 0144021   /* mem[143615B]: JMP I 135 exit target (worker; role INFERRED) */
#define P_W010376 0010376   /* mem[143616B]: JPL I 133 worker                              */
#define P_WEXIT   0144561   /* mem[143625B]: JMP I 111 / JMP I 17 status-post exit worker  */
#define P_W023706 0023706   /* mem[143630B]: JPL I 106 worker                              */
#define P_W145372 0145372   /* mem[143631B]: JPL I 105 worker                              */
#define P_W022704 0022704   /* mem[143632B]: JPL I 104 worker                              */
#define P_W024041 0024041   /* mem[143633B]: JPL I 104 worker                              */
#define P_W026656 0026656   /* mem[143640B]: JPL I 67  worker                              */
#define P_NXTMS   0135067   /* mem[143637B]: -> NXTMSG (next-message dispatch loop)        */

int mon_515B_MultipleDataTransfer(void)
{
    int A, D, T, X, B;          /* ND-100 CPU registers (B = msg-block base on entry) */

    /* 143445B..143451B: save context, take a copy of the base, set T := BANK. */
    mem[ind(0143445 + 0142)] = X;   /* 143445 STX I 142 : save X into field mem[143607B]->011160B */
    mem[ind(0143446 + 0142)] = 0;   /* 143446 STZ I 142 : zero field mem[143610B]->011165B         */
    A = B;                          /* 143447 RADD CLD SB DA (COPY SB DA) : A = B                  */
    mem[ind(0143450 + 0141)] = A;   /* 143450 STA I 141 : save base A into field mem[143611B]->011157B */
    T = mem[ BANK ];                /* 143451 LDT I 141 : T = mem[ mem[143612B] ] = mem[004654B] = BANK */

    /* 143452B..143456B: read the PHYSICAL double-word descriptor at bank:(X+100B),
     * stash it, reload its high word into A. */
    X = X + 0100;                   /* 143452 AAX 100                                              */
    A = phys[ ELADDR(T, X) ];       /* 143453 LDDTX : A = phys[EL]                                 */
    D = phys[ ELADDR(T, X) + 1 ];   /* 143453 LDDTX : D = phys[EL+1]                               */
    X = X - 0100;                   /* 143454 AAX -100                                             */
    mem[ind(0143455 + 0136)]   = A; /* 143455 STD I 136 : field mem[143613B]->011162B = A          */
    mem[ind(0143455 + 0136)+1] = D; /* 143455 STD I 136 : ...+1 = D (011163B)                      */
    A = mem[ind(0143456 + 0136)];   /* 143456 LDA I 136 : A = mem[143614B]->011163B (= the D word) */

    /* 143457B..143460B: validity gate on bit0 of A. */
    if ( !(A & 1) )                 /* 143457 BSKP ONE 0 DA (skip if A bit0 set)                   */
        goto exit_W144021;          /* 143460 JMP I 135 -> mem[143615B]=144021 (exit)             */

    /* 143461B..143464B: read another PHYSICAL word at bank:(X+110B), call a worker,
     * move the loaded word into X as the classification value (call it DX). */
    X = X + 0110;                   /* 143461 AAX 110                                              */
    A = phys[ ELADDR(T, X) ];       /* 143462 LDATX : A = phys[EL]                                 */
    call((( P_W010376 )));          /* 143463 JPL I 133 -> mem[143616B]=010376 (worker)           */
    X = A;                          /* 143464 RADD CLD SA DX (COPY SA DX) : X = A (classification) */

    /* 143465B..143512B: bounds ladder - compare X (DX, unsigned) against constants
     * loaded P-relative from the data pool, and build a selector in A (1 or 0).
     * MGRE = skip if desti(X) >= source(T); MLST = skip if X < T (both unsigned).  */
    T = mem[0143617];               /* 143465 LDT 132 : bound = mem[143617B]                       */
    if ( !((unsigned)X >= (unsigned)T) ) goto L143513;  /* 143466 SKP DX MGRE ST / 143467 JMP 24  */
    T = mem[0143620];               /* 143470 LDT 130 : bound = mem[143620B]                       */
    if ( (unsigned)X < (unsigned)T ) { }                /* 143471 SKP DX MLST ST : skip JMP if X<T */
    else goto L143513;              /* 143472 JMP 21 -> 143513 (X >= bound)                        */
    T = mem[0143621];               /* 143473 LDT 126 : bound = mem[143621B]                       */
    if ( !((unsigned)X >= (unsigned)T) ) goto L143500;  /* 143474 SKP DX MGRE ST / 143475 JMP 3   */
    A = 1;                          /* 143476 SAA 1 : in-window selector                           */
    goto L143501;                   /* 143477 JMP 2 -> 143501                                      */
L143500:
    A = 0;                          /* 143500 RADD CLD 0 DA (RCLR DA) : A = 0 (out-window selector) */
L143501:
    mem[ind(0143501 + 0121)] = A;   /* 143501 STA I 121 : store selector A into field              */
    T = mem[0143623];               /* 143502 LDT 121 : bound = mem[143623B]                       */
    if ( !((unsigned)X >= (unsigned)T) ) goto L143512;  /* 143503 SKP DX MGRE ST / 143504 JMP 6   */
    T = mem[0143624];               /* 143505 LDT 117 : bound = mem[143624B]                       */
    if ( !((unsigned)X < (unsigned)T) ) goto L143512;   /* 143506 SKP DX MLST ST / 143507 JMP 3   */
    A = mem[X + 060];               /* 143510 LDA ,X 60                                            */
    if ( A == 0 ) goto L143513;     /* 143511 JAZ 2 -> 143513 (also sets C = (A==0))               */
L143512:
    goto L143515;                   /* 143512 JMP 3 -> 143515                                      */

L143513:                            /* out-of-range / rejected */
    A = 6;                          /* 143513 SAA 6 : status/err 6                                 */
    goto exit_WEXIT;                /* 143514 JMP I 111 -> mem[143625B]=144561                     */

    /* 143515B..143520B: save the classification index, load a resident table base,
     * read a per-slot word, branch on its busy/free flag. */
L143515:
    mem[ind(0143515 + 0111)] = X;   /* 143515 STX I 111 : save X (index) into field mem[143626B]->011161B */
    X = mem[0143627];               /* 143516 LDX 111 : X = mem[143627B]=033315 (resident table base) */
    A = mem[X + 013];               /* 143517 LDA ,X 13 : per-slot word                           */
    if ( A != 0 ) goto L143547;     /* 143520 JAF 27 -> 143547 (slot active)                       */

    /* 143521B..143546B: FREE path - reload X from a field, issue indirect worker
     * calls (interleaved with RAND 0 0 = nop pads), build a transfer descriptor via
     * PHYSICAL transfers, do sector arithmetic, store it, exit to NXTMSG. */
    X = mem[ mem[0143607] ];        /* 143521 LDX I 66 : X = mem[ mem[143607B] ] = mem[011160B]    */
    call((( P_W023706 )));          /* 143522 JPL I 106 -> mem[143630B]=023706                     */
    /* 143523 RAND 0 0 : nop pad */
    call((( P_W145372 )));          /* 143524 JPL I 105 -> mem[143631B]=145372                     */
    /* 143525 RAND 0 0 : nop pad */
    call((( P_W022704 )));          /* 143526 JPL I 104 -> mem[143632B]=022704                     */
    call((( P_W024041 )));          /* 143527 JPL I 104 -> mem[143633B]=024041                     */

    T = mem[ BANK ];                /* 143530 LDT I 62 : T = mem[143612B] = mem[004654B] = BANK    */
    X = X + 0147;                   /* 143531 AAX 147                                              */
    A = phys[ ELADDR(T, X) ];       /* 143532 LDATX : A = phys[EL]                                 */
    X = X - 0147;                   /* 143533 AAX -147                                             */
    mem[ind(0143534 + 0100)] = A;   /* 143534 STA I 100 : field mem[143634B]->011260B = A          */
    A = mem[ind(0143535 + 0100)];   /* 143535 LDD I 100 : A = mem[143635B]->011167B                */
    D = mem[ind(0143535 + 0100)+1]; /* 143535 LDD I 100 : D = ...+1 (011170B)                      */
    T = mem[ BANK ];                /* 143536 LDT I 54 : T = mem[143612B] = BANK                   */
    phys[ ELADDR(T, X) ]     = A;   /* 143537 STDTX : phys[EL]   = A                               */
    phys[ ELADDR(T, X) + 1 ] = D;   /* 143537 STDTX : phys[EL+1] = D                               */
    A = T;                          /* 143540 RADD CLD ST DA (COPY ST DA) : A = T                  */
    D = X;                          /* 143541 RADD CLD SX DD (COPY SX DD) : D = X                  */
    { long AD = ((long)A << 16) | (D & 0177777);  /* SAD operates on the A:D 32-bit pair */
      AD <<= 6;                                    /* 143542 SAD 6 : left shift 6 (zero fill)     */
      A = (AD >> 16) & 0177777; D = AD & 0177777; }
    A = A - mem[ind(0143543 + 073)];/* 143543 SUB I 73 : A -= mem[ field ]                         */
    { unsigned long AD = ((unsigned long)(A & 0177777) << 16) | (D & 0177777);
      AD >>= 5;                                    /* 143544 SAD ZIN SHR 5 : logical right 5      */
      A = (AD >> 16) & 0177777; D = AD & 0177777; }
    mem[ind(0143545 + 070)]   = A;  /* 143545 STD I 70 : field mem[143635B]->011167B = A           */
    mem[ind(0143545 + 070)+1] = D;  /* 143545 STD I 70 : ...+1 = D                                 */
    goto exit_NXTMSG;               /* 143546 JMP I 71 -> mem[143637B]=135067 = NXTMSG             */

    /* 143547B..143606B: SLOT-ACTIVE path - decrement an outstanding-count field,
     * read a PHYSICAL device-status word, mask it, map it to a status code. */
L143547:
    B = X;                          /* 143547 RADD CLD SX DB (COPY SX DB) : B = X                  */
    X = X + 5;                      /* 143550 AAX 5                                                */
    call((( P_W026656 )));          /* 143551 JPL I 67 -> mem[143640B]=026656                      */
    A = mem[B + 013];               /* 143552 LDA ,B 13                                            */
    A = A - 1;                      /* 143553 AAA -1                                               */
    mem[B + 013] = A;               /* 143554 STA ,B 13 : decrement the outstanding-count field    */
    mem[ind(0143555 + 033)] = T;    /* 143555 STT I 33 : field mem[143610B]->011165B = T           */
    B = T;                          /* 143556 RADD CLD ST DB (COPY ST DB) : B = T                  */
    X = mem[ mem[0143607] ];        /* 143557 LDX I 30 : X = mem[ mem[143607B] ] = mem[011160B]    */
    T = mem[ BANK ];                /* 143560 LDT I 32 : T = mem[143612B] = BANK                   */
    X = X + 0111;                   /* 143561 AAX 111                                              */
    A = phys[ ELADDR(T, X) ];       /* 143562 LDATX : A = phys[EL] (device-status word)            */
    mem[ind(0143563 + 056)] = A;    /* 143563 STA I 56 : field mem[143641B]->011164B = A           */
    A = A & mem[0143642];           /* 143564 AND 56 : P-relative; mem[143642B]=077 => A &= 077     */

    /* 143565B..143604B: map the masked status A to a code in T via compares.
     * Match values loaded by SAT: 1, 7, 6. Output codes (also in T): 60, 61, 66. */
    if ( A != 0 ) goto L143570;     /* 143565 JAF 3 -> 143570                                      */
    T = 060;                        /* 143566 SAT 60 : status code 060                             */
    goto L143643;                   /* 143567 JMP 54 -> 143643                                     */
L143570:
    T = 1;                          /* 143570 SAT 1                                                */
    if ( A == T ) goto L143576;     /* 143571 SKP DA UEQ ST (skip if A!=T) / 143572 JMP 4 -> 143576 (taken if A==1) */
    T = 7;                          /* 143573 SAT 7                                                */
    if ( A != T ) goto L143600;     /* 143574 SKP DA EQL ST (skip if A==T) / 143575 JMP 3 -> 143600 */
L143576:
    T = 061;                        /* 143576 SAT 61 : status code 061                             */
    goto L143643;                   /* 143577 JMP 44 -> 143643                                     */
L143600:
    T = 6;                          /* 143600 SAT 6                                                */
    if ( A == T ) goto L143603;     /* 143601 SKP DA EQL ST (skip if A==T) / 143602 JMP 3 -> 143605 */
    goto L143605;
L143603:
    T = 066;                        /* 143603 SAT 66 : status code 066                             */
    goto L143643;                   /* 143604 JMP 37 -> 143643                                     */
L143605:
    A = 7;                          /* 143605 SAA 7 : status 7                                     */
    goto exit_WEXIT;                /* 143606 JMP I 17 -> mem[143625B]=144561                      */

L143643:
    /* 143643B LDA I 127: A = mem[ mem[143643B+127B] ]. This is the ONE executable
     * word in the pool range; it runs on into the UNCARVED continuation past the
     * window (143644B+), which posts the status and returns to MCHANDEL. */
    A = mem[ ind(0143643 + 0127) ]; /* 143643 LDA I 127 -> continues outside carved window        */
    /* UNVERIFIED: continuation body is not in these bytes. */
    return A;

exit_W144021:
    goto_addr((( P_W144021 )));     /* -> 144021B (resident, role INFERRED) */
    return A;
exit_WEXIT:
    goto_addr((( P_WEXIT )));       /* -> 144561B (status-post exit worker) */
    return A;
exit_NXTMSG:
    goto_addr((( P_NXTMS )));       /* -> NXTMSG = 135067B */
    return A;
}

/* Notes for the emulator:
 *  - The T/X transfers (LDDTX 143453, LDATX 143462/143532/143562, STDTX 143537) are
 *    PHYSICAL, MMU-bypassing; T = mem[004654B] is the message-buffer bank, X the word
 *    offset. This is the heart of the 5MTRANS transfer - model phys[] accordingly.
 *  - Return-status literals in this window (octal): match values 1, 6, 7 (SAT, used
 *    as SKP compare operands) and output codes 60, 61, 66 (SAT), plus 6 and 7 (SAA).
 *    The older "status 1/2/4 or errors 6/7" note is only partly consistent: 2 and 4
 *    do not appear as literals here. Exact field/status meanings are INFERRED.
 *  - The bounds constants (loaded P-relative by LDT 132/130/126/121/117) and the
 *    field-address / worker pointers all live in the data pool 143607B..143642B; the
 *    worker addresses (144021, 010376, 144561, 023706, 145372, 022704, 024041, 026656)
 *    are resident routines NOT carved here - their bodies are UNVERIFIED.
 *  - 143637 JPL I 67 -> 143726 as printed by nd100-dis is DATA (the word 135067B =
 *    NXTMSG used by 143546 JMP I 71), not an executed instruction - disregard it.
 */
