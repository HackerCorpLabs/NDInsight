/* ============================================================================
 * MON 506B  AnswerSIBAS (5SIBMO)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 level-12 driver call (NOT an ND-100 GOTAB MON).
 *
 * Derived from the real SINTRAN L bytes (see 506B-AnswerSIBAS.ASM). Blocks A-C
 * (SIBAS-number guard + reservation check) are BYTE-VERIFIED against the carved
 * 026-S3IMPIT bytes; block D (mark-running / restart waiter / arm timer /
 * reactivate) is INFERRED from the NPL 5SIBMO tail, not annotated line by line
 * in this pass. Field names (SIBBDEVS, RTRES, ...) come from NPL and are naming
 * only. All addresses in comments are octal.
 *
 * T/X PHYSICAL transfers and the message buffer.
 * The handler reaches the ND-500 MESSAGE BUFFER with the T/X physical transfer
 * instructions (LDDTX/LDATX/LDXTX/STATX). These form a 24-bit PHYSICAL address
 * that BYPASSES the MMU - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * T is the BANK (high byte; here T := 5MBBANK, the ND-500 message-buffer bank,
 * loaded via the LDT I pointer words), X the word offset; disp3 (bits 3-5) is 0
 * in every transfer here - the code adjusts X with AAX. phys[] is that 24-bit
 * physical space. The SIBAS-number pair and the status words below are therefore
 * READ/WRITTEN at phys[ELADDR(T,X)], not in ND-100 A/T/X user registers.
 * ============================================================================ */

#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */
#define EC174   0174    /* illegal SIBAS number error code (SAA 174, VERIFIED byte)  */

/* Dispatch: ND-500 SIBAS server issues MON 506B -> level-12 MCHANDEL ->
 * 5CMNO-L12MIN GOSW index 6 (L12MIN=500, 506-500=6) -> 5SIBMO @141716B.
 * Entry contract (NPL 5SIBMO header): X = current message (N5MESSAGE),
 * B = ND-500 CPU datafield.  There is no ND-100 skip-return: every path ends by
 * calling a worker triple and GO NXTMSG (return to the level-12 message loop).
 */

int mon_answer_sibas(void)   /* X = N5MESSAGE, B = ND-500 datafield */
{
    int sibno_hi, sibno_lo;

    /* --- Block A: validate the SIBAS number (141716-141726) --------------- */
    /* 141716 LDT I 101 -> T := 5MBBANK; 141717 AAX 100 -> X += SIBNO field;   *
     * 141720 LDDTX -> A = phys[EL] (high word), D = phys[EL+1] (SIBAS number). */
    sibno_hi = phys[ELADDR(T, X)];             /* 141720 LDDTX: A = phys[EL]        */
    sibno_lo = phys[ELADDR(T, X) + 1];         /*             D = phys[EL+1]        */
    if (sibno_hi != 0 ||                       /* 141721 JAF 6  -> illegal          */
        /* T:=MXSIBAS (141722); 141723 SKP IF DT MGRE SD -> skip if MXSIBAS>=lo */
        sibno_lo > MXSIBAS ||                  /* 141722-141724 vs MXSIBAS          */
        /* 141725 SKP IF DT EQL 0 -> skip if the guard word == 0 (INFERRED)     */
        guard_word == 0)                       /* 141725-141726                     */
    {
        /* --- Block B: illegal-number return (141727-141733) --------------- */
        /* 141727 LDX I 72 -> X := N5MESSAGE; 141730 SAA 174 -> A := EC174     */
        emonico(EC174);                        /* 141731 JPL I 71 -> restart w/ err */
        xactrdy();                             /* 141732 JPL I 71                   */
        goto NXTMSG;                           /* 141733 JMP I 71                   */
    }

    /* --- Block C: resolve datafield, check reservation (141734-141752) ----- */
    /* 141734 RADD CLD SD DX (X := D = SIBAS number); 141735 STX -21;          *
     * 141736 LDX I ,X 67 -> datafield address; 141741 LDA ,X 1 owner;         *
     * 141743 LDT ,X 1 / 141744 SKP IF DA UEQ ST -> skip if owner != current.  */
    if (df->RTRES != PROCAD->RTRES)            /* 141741-141744 compare owners       */
    {
        /* 141746 LDX I 53 -> X := N5MESSAGE; 141747 SAA 5 -> A := 5           */
        emonico(5);                            /* 141750 JPL I 52 (reserved-by-other)*/
        xactrdy();                             /* 141751 JPL I 52                   */
        goto NXTMSG;                           /* 141752 JMP I 52                   */
    }

    /* --- Block D: mark running / restart waiter / arm timer / reactivate ---- */
    /* INFERRED from NPL tail (141753 onward); not annotated line by line.
     * The tail continues to touch the message buffer via T/X physical transfers:
     *   142001 LDDTX, 142035 LDATX, 142043 LDDTX, 142053 LDXTX, 142063 STATX,
     *   142075 STATX, 142077 LDXTX, 142114 LDATX  = phys[ELADDR(T,X)] reads/writes.
     * BSET ONE 120/130 DA (142055/142056) and BSET ZRO 130 DA (142101) set/clear
     * status bits in a word fetched by LDATX, written back by STATX.               */
    df->SIB500 = 1;                            /* 141753 SAA 1 ...  mark running     */
    ap = SIBAPDEVS[/*csibno*/0];
    ap->SRTCSTAT = 0;                          /* clear survey-timer status          */
    if (ap->RTRES != 0 && ap->STATUS_bit5WAIT) /* a process is waiting               */
        rtact(ap);                             /* restart it                         */
    df->TTMR = msg_TMR;                        /* arm survey timer                   */
    if (msg_SIBST == STOP)                     /* re-read requested state            */
        goto NSTOPROC;                         /* stop the server                    */
    okmonico();                                /* reactivate SIBAS server (status OK) */
    xactrdy();

NXTMSG:
    return_to_message_loop();                  /* GO NXTMSG                          */
    return 0;
}

/* Status word delivered to the ND-500 by EMONICO/OKMONICO:
 *   0     = OK        (reactivated, block D)
 *   5     = SIBAS datafield reserved by another process (block C)   VERIFIED
 *   EC174 = illegal SIBAS number                                    VERIFIED
 * The exact OK status value is written inside OKMONICO, which is outside this
 * 157-word slice -> INFERRED.
 *
 * Notes for the emulator:
 *  - Every message-buffer operand is read/written through phys[ELADDR(T,X)]
 *    (LDDTX/LDATX/LDXTX/STATX), the 24-bit PHYSICAL space with the MMU bypassed
 *    (reference ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md sec 5).
 *  - RADD CLD S<r> D<r> (141734/141737/141754/141760/141772/142050/142104) is a
 *    register COPY: dest cleared, source added to 0.
 */
