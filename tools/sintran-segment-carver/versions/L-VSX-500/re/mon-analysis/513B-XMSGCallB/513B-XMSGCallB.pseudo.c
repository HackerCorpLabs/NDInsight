/* ============================================================================
 * MON 513B  XMSGCallB (B5XMS)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 -> ND-100 XMSG gateway (level-12 call).
 * MON 513B and MON 512B share ONE body BYTE-FOR-BYTE: L07 symbols
 * B5XMS = A5XMS = 142253B.  The two MON numbers enter the identical code and
 * differentiate INSIDE via the message function field, not by a separate entry.
 * This file models that shared body.
 *
 * Grounded in the real ND-100 instruction semantics documented in
 *   ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md
 * Every T/X transfer below (LDATX/LDXTX/LDDTX/STATX/STZTX/STDTX) is a 24-bit
 * PHYSICAL, MMU-BYPASSING access into the ND-500 message buffer, NOT a
 * T-relative or plain X-indexed memory reference (reference S5):
 *
 *     EL   = ((T & 0xFF) << 16) | ((X + disp3) & 0xFFFF)
 *            disp3 = (opcode >> 3) & 7   (0 for every transfer here except
 *            the one STATX at 142533 whose opcode 143364 carries disp3 = 6)
 *     LDATX: A = phys[EL]
 *     LDXTX: X = phys[EL]
 *     LDDTX: A = phys[EL];  D = phys[EL+1]
 *     STATX: phys[EL] = A
 *     STDTX: phys[EL] = A;  phys[EL+1] = D
 *     STZTX: phys[EL] = 0
 *
 * T holds the message-buffer BANK (5MBBANK / datafield bank), (re)loaded by each
 * "LDT I n" as ind(n); X is a running physical word cursor advanced by the
 * "AAX n" between transfers; B saves the entry cursor. ROP words follow
 * reference S3: "RADD CLD Ssr Ddr" == "dr = sr" (dest cleared, source added to
 * 0), i.e. a register COPY; "RADD SA DP" (no CLD) == "P = P + A" (computed jump).
 * Register letters: STS0 D1 P2 B3 L4 A5 T6 X7.
 *
 * Control flow closes inside 142253B..142611B (223 words); every DIRECT branch
 * resolves in-file. INDIRECT branches go through two data pointer pools
 * (142403..142417 and 142433..142437) plus pointer words at 142631+ that live
 * PAST the carve edge; those targets are UNVERIFIED. Field offsets (+101, +130,
 * ...) and worker identities are INFERRED - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Physical message-buffer word at the current [T-bank : X-offset].            */
#define MB    phys[ ((T & 0xFF) << 16) | ( X       & 0xFFFF) ]  /* disp3 = 0    */
#define MB1   phys[ ((T & 0xFF) << 16) | ((X + 1)  & 0xFFFF) ]  /* phys[EL+1]   */
#define MB6   phys[ ((T & 0xFF) << 16) | ((X + 6)  & 0xFFFF) ]  /* 142533 disp3=6 */

/* Indirect / P-relative memory helpers (reference S1/S2):
 *   ind(n)     = mem[ mem[P + n] ]   (one indirect fetch: "LDx I n", "SUB I n")
 *   memP(n)    = mem[ P + n ]        (P-relative direct: "LDA n", "AND n")      */

/* --- data pointer pools (disassembler mis-renders these as code) ------------
 * 142403..142417 (JMP I / JPL I targets for the entry + jump-table region):
 *   142405 = 142643   142406 = 143414   142407 = 147574     (past carve)
 *   142410 = 142611   142411 = 142611   142412 = 142611     (common exit)
 *   142413 = 142555   142414 = 142555   142415 = 142567
 *   142416 = 142574   142417 = 142611
 *   142403 = 004654   142404 = 000077                        (unused pool slots)
 * 142433..142437 (JPL I / JMP I worker targets for the 142420 generic tail):
 *   142433 = 023030   142434 = 145466   142435 = 023624
 *   142436 = 000215   142437 = 135067                        (resident workers,
 *   all outside the carve -> UNVERIFIED targets)
 * 142631, 142633, 142634, 142640, 142641, 142642 : return / worker pointers
 *   PAST the carve edge -> UNVERIFIED. -------------------------------------- */

/* Entry 142253B = B5XMS = A5XMS.  On entry X = ND-500 message cursor,
 * T = message-buffer bank (set up by the caller / level-12 GOSW).             */
int mon_xmsg_call_b(mon_regs *r)
{
    unsigned T;                 /* message-buffer BANK (T & 0xFF used)         */
    int X;                      /* running physical word cursor within bank T  */
    int A, D, B, L;             /* ND-100 working registers                    */

    B = X;                      /* 142253 RADD CLD SX DB : B = X (save cursor) */
    T = ind(0127);              /* 142254 LDT I 127 : message-buffer bank      */
    X += 0101;                  /* 142255 AAX 101                              */
    A = MB;                     /* 142256 LDATX : A = phys[bank:X] (fn word)   */
    A &= memP(0125);            /* 142257 AND 125 : mask function bits         */
    D = A;                      /* 142260 RADD CLD SA DD : D = A (keep masked) */
    A += -057;                  /* 142261 AAA -57 : A = A - 57 (range bias)    */
    /* 142262 SKP IF 0 GRE SA : skip 142263 if (int16)0 >= (int16)A (A <= 0)   */
    if (!((int16_t)0 >= (int16_t)A))
        goto L142420_common;    /* 142263 JMP 135 -> 142420 (fn out of range)  */

    X += -0131;                 /* 142264 AAX -131                             */
    A = MB;                     /* 142265 LDATX : A = phys[bank:X] (flag word) */
    if (A != 0)                 /* 142266 JAF 26 -> 142314                     */
        goto L142314_decode;
    A = 1;                      /* 142267 SAA 1                                */
    /* 142270 SKP IF DA UEQ SD : skip 142271 if A != D                         */
    if (!(A != D))              /* 142271 JMP I 114 -> mem[142405] when A==D    */
        goto L_ptr_142405;      /*   (D == 1: leave via pointer, past carve)    */

    /* --- first XMSG re-issue (142272-142300) ------------------------------- */
    X = B;                      /* 142272 RADD CLD SB DX : X = B               */
    X += 0140;                  /* 142273 AAX 140                              */
    A = MB;                     /* 142274 LDATX : A = phys[bank:X] (XMSG arg)  */
    T = 043;                    /* 142275 SAT 43 : MON 200 sub-argument        */
    L = 0;                      /* 142276 RADD CLD 0 DL : L = 0 (RCLR L)       */
    /* 142277 MON 200 : XMSG monitor call (200B); A/T/L staged above.
     * MON 200B uses a SKIP return (UNVERIFIED convention):
     *   no-skip return -> 142300 (JMP I 105 -> mem[142405], leave via pointer)
     *   skip   return  -> 142301 (T-status test below).                       */
    if (mon200(r) == NO_SKIP)   /* UNVERIFIED: skip-return convention          */
        goto L_ptr_142405;      /* 142300 JMP I 105 -> mem[142405] (past carve)*/
    /* 142301 SKP IF DT GRE 0 : skip 142302 if (int16)T >= 0                   */
    if (!((int16_t)T >= 0))     /* 142302 JMP I 104 -> mem[142406] when T < 0   */
        goto L_ptr_142406;      /*   (error, past carve)                       */
    A = L;                      /* 142303 RADD CLD SL DA : A = L               */
    X = B;                      /* 142304 RADD CLD SB DX : X = B               */
    T = ind(076);               /* 142305 LDT I 76 : bank                      */
    X += -030;                  /* 142306 AAX -30                              */
    MB = A;                     /* 142307 STATX : phys[bank:X] = A (result)    */
    T = 042;                    /* 142310 SAT 42                               */
    A = memP(076);              /* 142311 LDA 76                               */
    mon200(r);                  /* 142312 MON 200 : second XMSG call           */
    goto L_ptr_142405;          /* 142313 JMP I 72 -> mem[142405] (past carve) */

L142314_decode:
    /* --- re-decode the sub-function and dispatch through the jump table ----- */
    X = B;                      /* 142314 RADD CLD SB DX : X = B               */
    T = ind(066);               /* 142315 LDT I 66 : bank                      */
    X += 0101;                  /* 142316 AAX 101                              */
    A = MB;                     /* 142317 LDATX : A = phys[bank:X] (fn word)   */
    X += -0101;                 /* 142320 AAX -101                             */
    A &= memP(063);             /* 142321 AND 63 : form jump-table index in A  */
    /* 142322 RADD SA DP : P = P + A -> computed jump into the table at 142323.
     * Slot (142322 + A) is a JMP/JMP I; targets by index A (octal) below.
     * (A must be >= 1; A == 0 would re-execute 142322.)                       */
    switch (A) {                /* jump table 142323..142402                   */
        case 001: goto L_ptr_142410;  /* 142323 JMP I 65 -> mem[142410]=142611 */
        case 002: goto L_ptr_142411;  /* 142324 JMP I 65 -> mem[142411]=142611 */
        case 013: goto L_ptr_142412;  /* 142335 JMP I 55 -> mem[142412]=142611 */
        case 060: goto L_ptr_142417;  /* 142402 JMP I 15 -> mem[142417]=142611 */
        case 003: case 004: case 005: case 006:
        case 011: case 012: case 014: case 016: case 017: case 020:
        case 030: case 040: case 041: case 046: case 053:
                  goto L142445;       /* 142325.. JMP -> 142445                 */
        case 007: case 010: case 054:
                  goto L142454;       /* 142331/142332/142376 JMP -> 142454     */
        case 015: goto L_ptr_142413;  /* 142337 JMP I 54 -> mem[142413]=142555  */
        case 027: goto L_ptr_142414;  /* 142351 JMP I 43 -> mem[142414]=142555  */
        case 021: case 022: case 023: case 024: case 025: case 026:
        case 031: case 032: case 033: case 034: case 035: case 036:
        case 042: case 043: case 044: case 045: case 055: case 056: case 057:
                  goto L142420_common;/* 142343.. JMP -> 142420                 */
        case 037: goto L142440;       /* 142361 JMP 57 -> 142440                */
        case 047: case 051:
                  goto L142562;       /* 142371/142373 JMP -> 142562            */
        case 050: goto L_ptr_142415;  /* 142372 JMP I 23 -> mem[142415]=142567  */
        case 052: goto L_ptr_142416;  /* 142374 JMP I 22 -> mem[142416]=142574  */
    }

L142420_common:
    /* --- generic tail: call resident XMSG workers via the 142433 pointer pool */
    A = -022;                   /* 142420 SAA -22                              */
    /* 142421 SAD SHR 20 : arithmetic right shift of the 32-bit A:D by 020(16). */
    { int32_t ad = ((int32_t)(int16_t)A << 16) | (uint16_t)D;
      ad >>= 020;  A = (int)((ad >> 16) & 0xFFFF);  D = (int)(ad & 0xFFFF); }
    T = 1;                      /* 142422 SAT 1                                */
    X = B;                      /* 142423 RADD CLD SB DX : X = B               */
    resident_worker(mem[0142433]);  /* 142424 JPL I 7 -> mem[142433] (past carve) */
    resident_worker(mem[0142434]);  /* 142425 JPL I 7 -> mem[142434] (past carve) */
    X = B;                      /* 142426 RADD CLD SB DX : X = B               */
    resident_worker(mem[0142435]);  /* 142427 JPL I 6 -> mem[142435] (past carve) */
    resident_worker(mem[0142436]);  /* 142430 JPL I 6 -> mem[142436] (past carve) */
    B = A;                      /* 142431 RADD CLD SA DB : B = A               */
    goto_worker(mem[0142437]);  /* 142432 JMP I 5 -> mem[142437] (tail, past carve) */

L142440:
    X += -1;                    /* 142440 AAX -1                               */
    A = MB;                     /* 142441 LDATX : A = phys[bank:X]             */
    X += 1;                     /* 142442 AAX 1                                */
    /* 142443 BSKP ONE 160 DA : skip 142444 if A bit14 == 1  (160 oct / 8 = 14) */
    if (!(A & (1 << 14)))       /* 142444 JMP -24 -> 142420 when bit14 == 0     */
        goto L142420_common;
    /* fall through to 142445 */

L142445:
    X += 0104;                  /* 142445 AAX 104                              */
    A = MB;  D = MB1;           /* 142446 LDDTX : A,D <- phys pair             */
    X += -1;                    /* 142447 AAX -1                               */
    A = MB;                     /* 142450 LDATX                                */
    X += 4;                     /* 142451 AAX 4                                */
    X = MB;                     /* 142452 LDXTX : X = phys[bank:X]             */
    goto L142611_exit;          /* 142453 JMP 136 -> 142611                    */

L142454:
    X += 0102;                  /* 142454 AAX 102                              */
    A = MB;  D = MB1;           /* 142455 LDDTX                                */
    if (A != 0)                 /* 142456 JAF 4 -> 142462                      */
        goto L142462_err;
    A = memP(0151);             /* 142457 LDA 151                             */
    /* 142460 SKP IF DA MLST SD : skip 142461 if (uint16)A < (uint16)D         */
    if (!((uint16_t)A < (uint16_t)D))
        goto L142464;           /* 142461 JMP 3 -> 142464 when A >= D unsigned  */
L142462_err:
    A = -036;                   /* 142462 SAA -36 : error code                 */
    goto L_out_142631;          /* 142463 JMP I 146 -> mem[142631] (past carve)*/
L142464:
    X += -1;                    /* 142464 AAX -1                               */
    A = MB;                     /* 142465 LDATX                                */
    X += -0101;                 /* 142466 AAX -101                            */
    A &= memP(0143);            /* 142467 AND 143                             */
    T = 7;                      /* 142470 SAT 7                                */
    /* 142471 SKP IF DA UEQ ST : skip 142472 if A != T (7)                     */
    if (!(A != (int16_t)T))
        goto L142476;           /* 142472 JMP 4 -> 142476 when A == 7           */
    T = 053;                    /* 142473 SAT 53                               */
    /* 142474 SKP IF DA EQL ST : skip 142475 if A == T (53)                    */
    if (!(A == (int16_t)T))
        goto L142545;           /* 142475 JMP 50 -> 142545 when A != 53         */
    /* fall through to 142476 (A == 53) */
L142476:
    resident_worker(mem[0142633]);  /* 142476 JPL I 135 -> mem[142633] (past carve) */
    resident_worker(mem[0142634]);  /* 142477 JPL I 135 -> mem[142634] (past carve) */
    B = A;                      /* 142500 RADD CLD SA DB : B = A               */
    A = mem[B - 010];           /* 142501 LDA ,B -10 : B-relative flag word    */
    /* 142502 BSKP ZRO 0 DA : skip 142503 if A bit0 == 0                       */
    if (!((A & 1) == 0))
        goto L142544;           /* 142503 JMP 41 -> 142544 when bit0 == 1       */
    /* --- build/copy the return descriptor (142504-142537) ------------------ */
    A = D;                      /* 142504 RADD CLD SD DA : A = D               */
    T = ind(0130);              /* 142505 LDT I 130 : bank                     */
    X += 013;                   /* 142506 AAX 13                              */
    MB = A;                     /* 142507 STATX                                */
    X += 1;                     /* 142510 AAX 1                                */
    MB = 0;                     /* 142511 STZTX                                */
    X += 030;                   /* 142512 AAX 30                              */
    A = MB;  D = MB1;           /* 142513 LDDTX                                */
    X += -035;                  /* 142514 AAX -35                             */
    MB = A;  MB1 = D;           /* 142515 STDTX                                */
    X += 0131;                  /* 142516 AAX 131                            */
    A = MB;  D = MB1;           /* 142517 LDDTX                                */
    /* 142520 SAD 6 : left shift the 32-bit A:D by 6, zero fill.               */
    { uint32_t ad = ((uint32_t)(uint16_t)A << 16) | (uint16_t)D;
      ad <<= 6;  A = (int)((ad >> 16) & 0xFFFF);  D = (int)(ad & 0xFFFF); }
    A -= ind(0115);             /* 142521 SUB I 115 : A = A - mem[ind(115)]    */
    /* 142522 SAD ZIN SHR 5 : logical right shift the 32-bit A:D by 5, zero fill*/
    { uint32_t ad = ((uint32_t)(uint16_t)A << 16) | (uint16_t)D;
      ad >>= 5;   A = (int)((ad >> 16) & 0xFFFF);  D = (int)(ad & 0xFFFF); }
    T = ind(0112);              /* 142523 LDT I 112 : bank                     */
    X += -0127;                 /* 142524 AAX -127                           */
    MB = A;  MB1 = D;           /* 142525 STDTX                                */
    A = memP(0111);             /* 142526 LDA 111                             */
    X += 0132;                  /* 142527 AAX 132                            */
    MB = A;                     /* 142530 STATX                                */
    X += -0143;                 /* 142531 AAX -143                           */
    A = 010;                    /* 142532 SAA 10                               */
    MB6 = A;                    /* 142533 STATX (143364, disp3=6): phys[bank:X+6]=A */
    A = 1;                      /* 142534 SAA 1                                */
    resident_worker(mem[0142640]);  /* 142535 JPL I 103 -> mem[142640] (past carve) */
    resident_worker(mem[0142641]);  /* 142536 JPL I 103 -> mem[142641] (past carve) */
    goto_worker(mem[0142642]);  /* 142537 JMP I 103 -> mem[142642] (tail, past carve) */

L142540:
    /* Not reached by any DIRECT in-carve branch; entered only via a resident
     * worker return.  Modeled sequentially for completeness.                  */
    T = ind(075);               /* 142540 LDT I 75 : bank                      */
    X += 0143;                  /* 142541 AAX 143                            */
    MB = 0;                     /* 142542 STZTX                                */
    X += -0143;                 /* 142543 AAX -143                           */
    /* fall through to 142544 */
L142544:
    B = X;                      /* 142544 RADD CLD SX DB : B = X               */
L142545:
    T = ind(070);               /* 142545 LDT I 70 : bank                      */
    X += 0102;                  /* 142546 AAX 102                            */
    A = MB;  D = MB1;           /* 142547 LDDTX                                */
    X += 037;                   /* 142550 AAX 37                              */
    A = MB;                     /* 142551 LDATX                                */
    X += -032;                  /* 142552 AAX -32                            */
    X = MB;                     /* 142553 LDXTX : X = phys[bank:X]             */
    goto L142611_exit;          /* 142554 JMP 35 -> 142611                     */

L142555:
    X += 0102;                  /* 142555 AAX 102                            */
    A = MB;  D = MB1;           /* 142556 LDDTX                                */
    X += 3;                     /* 142557 AAX 3                                */
    X = MB;                     /* 142560 LDXTX                                */
    goto L142611_exit;          /* 142561 JMP 30 -> 142611                     */

L142562:
    X += 0103;                  /* 142562 AAX 103                            */
    A = MB;                     /* 142563 LDATX                                */
    X += 2;                     /* 142564 AAX 2                                */
    X = MB;                     /* 142565 LDXTX                                */
    goto L142611_exit;          /* 142566 JMP 23 -> 142611                     */

L142567:
    X += 0105;                  /* 142567 AAX 105                            */
    A = MB;                     /* 142570 LDATX                                */
    X += -2;                    /* 142571 AAX -2                             */
    X = MB;                     /* 142572 LDXTX                                */
    goto L142611_exit;          /* 142573 JMP 16 -> 142611                     */

L142574:
    X += 0104;                  /* 142574 AAX 104                            */
    A = MB;  D = MB1;           /* 142575 LDDTX                                */
    if (A != 0)                 /* 142576 JAF 4 -> 142602                      */
        goto L142602_err;
    A = memP(031);              /* 142577 LDA 31                             */
    /* 142600 SKP IF DA MLST SD : skip 142601 if (uint16)A < (uint16)D         */
    if (!((uint16_t)A < (uint16_t)D))
        goto L142604;           /* 142601 JMP 3 -> 142604 when A >= D unsigned  */
L142602_err:
    A = -036;                   /* 142602 SAA -36 : error code                 */
    goto L_out_142631;          /* 142603 JMP I 26 -> mem[142631] (past carve) */
L142604:
    X += -1;                    /* 142604 AAX -1                               */
    A = MB;                     /* 142605 LDATX                                */
    X += 036;                   /* 142606 AAX 36                              */
    X = MB;                     /* 142607 LDXTX                                */
    /* 142610 JMP 1 -> 142611 (falls through) */

L142611_exit:
    T = ind(024);               /* 142611 LDT I 24 : restore context, then     */
                                /*   return to the level-12 GOSW dispatcher.   */
    /* Control continues PAST the carved window (142612+); the exact skip/return
     * contract to the level-12 gateway is UNVERIFIED (not in this carve).     */
    return A;

    /* Out-of-carve pointer landings (targets are UNVERIFIED, past 142611):    */
L_ptr_142405:  goto_worker(mem[0142405]);  /* -> 142643 */
L_ptr_142406:  goto_worker(mem[0142406]);  /* -> 143414 */
L_out_142631:  goto_worker(mem[0142631]);  /* XMSG error return */
    /* In-carve indirect landings resolved through the 142410-142417 pool:     */
L_ptr_142410:  goto L142611_exit;          /* mem[142410] = 142611 */
L_ptr_142411:  goto L142611_exit;          /* mem[142411] = 142611 */
L_ptr_142412:  goto L142611_exit;          /* mem[142412] = 142611 */
L_ptr_142413:  goto L142555;               /* mem[142413] = 142555 */
L_ptr_142414:  goto L142555;               /* mem[142414] = 142555 */
L_ptr_142415:  goto L142567;               /* mem[142415] = 142567 */
L_ptr_142416:  goto L142574;               /* mem[142416] = 142574 */
L_ptr_142417:  goto L142611_exit;          /* mem[142417] = 142611 */
}

/* Callers (both enter 142253B; behaviour splits on the message function field):
 *   MON 512B XMSGCallA (A5XMS): mon_xmsg_call_b(r);
 *   MON 513B XMSGCallB (B5XMS): mon_xmsg_call_b(r);   (same body)
 */
