/* ============================================================================
 * MON 511B  DVIO  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 level-12 monitor call (NOT an ND-100 GOTAB call).
 *
 * DVIO is the fused "output a prompt, then read a line" terminal call: the
 * output phase runs first, and on completion the same ND-500 message re-enters
 * the input phase (XNINSTR) which reports the byte count back.
 *
 * Grounded in the real ND-100 instruction semantics documented in
 *   ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md
 * Every T/X transfer below (LDATX/LDDTX/STATX/STDTX/STZTX) is a 24-bit
 * PHYSICAL, MMU-BYPASSING access into the ND-500 message buffer, NOT a
 * T-relative or plain X-indexed memory reference (reference S5):
 *
 *     EL   = ((T & 0xFF) << 16) | ((X + disp) & 0xFFFF)   (disp = 0 here)
 *     LDATX: A = phys[EL]
 *     LDDTX: A = phys[EL];  D = phys[EL+1]
 *     STATX: phys[EL] = A
 *     STDTX: phys[EL] = A;  phys[EL+1] = D
 *     STZTX: phys[EL] = 0
 *
 * T holds the message-buffer BANK (5MBBANK / datafield bank), reloaded by each
 * "LDT I n"; X is a running word cursor advanced by the "AAX n" between
 * transfers. ROP words follow reference S3: "RADD CLD Ssr Ddr" == "dr = sr"
 * (dest cleared, source added to 0), i.e. a register COPY.
 *
 * Control flow and the seven indirect worker pointers (141112..141124) are
 * BYTE-VERIFIED against the L07 symbol table; the semantic labels are INFERRED
 * from the call structure and the NPL DVIO body (a different revision) - treat
 * as a model, not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Physical message-buffer word at the current [T-bank : X-offset]. */
#define MB    phys[ ((T & 0xFF) << 16) | ( X       & 0xFFFF) ]   /* phys[EL]   */
#define MB1   phys[ ((T & 0xFF) << 16) | ((X + 1)  & 0xFFFF) ]   /* phys[EL+1] */

/* Entry 141027 = DVIO (shared entry NOUTS). X = current ND-500 message. */
void mon_dvio(mon_regs *r)
{
    unsigned T;            /* message-buffer / datafield BANK (T & 0xFF used)  */
    int X;                 /* running physical word cursor within bank T       */
    int A, D, B, L;        /* ND-100 working registers                         */

    /* 141027 JPL I 63 -> 5GTDF: is the target a real terminal? get its output
     * datafield. 141030 JMP I 63 -> NORMMC is the not-a-terminal skip-return.  */
    if (!get_terminal_datafield(r))          /* 141027 JPL I 63 -> 5GTDF       */
        return normal_mon_path(r);           /* 141030 JMP I 63 -> NORMMC      */

    /* 141031-141040: seed the output-datafield pointer, then read DNOBY.       */
    A = D;                                   /* 141031 RADD CLD SD DA : A = D   */
    X = ind(r, 062);                         /* 141032 LDX I 62 : msg offset    */
    T = ind(r, 062);                         /* 141033 LDT I 62 : msg/df bank   */
    X += 0142;                               /* 141034 AAX 142                  */
    MB = A;                                  /* 141035 STATX : phys[bank:X] = A */
    X += -040;                               /* 141036 AAX -40                  */
    A = MB;  D = MB1;                        /* 141037 LDDTX : A,D <- phys pair */
    X += -0102;                              /* 141040 AAX -102                 */

    /* 141041-141050: range-check the output byte count (now in D). D != 0 skips
     * ahead; otherwise DNOBY is validated as an unsigned magnitude against the
     * 4000B ceiling. Out of range -> error EC174, restart the ND-500 process.  */
    if (A != 0)                              /* 141041 JAF 4 -> 141045          */
        goto ec174;
    T = mem_ref(r, 054);                     /* 141042 LDT 54 : max byte count  */
    if ((unsigned)T < (unsigned)D)           /* 141043 SKP IF DT MLST SD        */
        goto count_ok;                       /* 141044 JMP 6 -> 141052          */
ec174:
    A = 0174;                                /* 141045 SAA 174 : error EC174    */
    restart_nd500_proc(r);                   /* 141046 JPL I 51 -> EMONICO      */
    reactivate_exec_queue(r);                /* 141047 JPL I 51 -> XACTRDY      */
    return process_next_nd500_message();     /* 141050 JMP I 51 -> NXTMSG       */
    /* 141051 JMP 5 -> 141056 (only reached if 141041 fell through, i.e. A==0
     * and count valid; folds into the buffer-residency test at 141056)         */

count_ok:
    /* 141052-141055: a zero-length prompt (D==0) skips output and enters the
     * output-string restart, which chains into the input phase for SMCNO=511.  */
    if (D == 0) {                            /* 141052 SKP IF DD EQL 0          */
        output_string_restart(r);            /* 141054 JPL I 46 -> OSTRS        */
        return process_next_nd500_message(); /* 141055 JMP I 44 -> NXTMSG       */
    }
    /* 141053 JMP 3 -> 141056 (D != 0 path) */

    /* 141056-141060: MIFLAG bit WSMC - is the data buffer already resident in
     * the COM-buffer? If so, skip the read-data-memory micro-function setup.    */
    A = mem[B - 010];                        /* 141056 LDA ,B -10 : MIFLAG      */
    if ((A & 1) == 0)                        /* 141057 BSKP ZRO 0 DA            */
        goto sttdriv;                        /* 141060 JMP 31 -> 141111         */

    /* 141061-141105: build the ND-500 read-data-memory micro-function in the
     * message buffer via physical bank:offset stores. Each AAX walks X to the
     * next field; T (bank) is reloaded once at 141061.                          */
    T = ind(r, 034);                         /* 141061 LDT I 34 : XMICF bank    */
    A = 010;                                 /* 141062 SAA 10  : func = read-DM */
    MB = A;                                  /* 141063 STATX                    */
    A = D;                                   /* 141064 RADD CLD SD DA : A = D    */
    X += 013;                                /* 141065 AAX 13                   */
    MB = A;                                  /* 141066 STATX : NRBYT            */
    X += 1;                                  /* 141067 AAX 1                    */
    MB = 0;                                  /* 141070 STZTX                    */
    X += 030;                                /* 141071 AAX 30                   */
    A = MB;  D = MB1;                        /* 141072 LDDTX : 5DITN pair       */
    X += -035;                               /* 141073 AAX -35                  */
    MB = A;  MB1 = D;                        /* 141074 STDTX : N500A            */
    X += 0131;                               /* 141075 AAX 131                  */
    A = MB;  D = MB1;                        /* 141076 LDDTX                    */
    X += -0127;                              /* 141077 AAX -127                 */
    MB = A;  MB1 = D;                        /* 141100 STDTX : N100A phys addr  */
    A = mem_ref(r, 022);                     /* 141101 LDA 22                   */
    X += 0132;                               /* 141102 AAX 132                  */
    MB = A;                                  /* 141103 STATX : SPFLA restart    */
    X += -0143;                              /* 141104 AAX -143                 */
    A = 1;                                   /* 141105 SAA 1                    */
    write_nd500_status(r);                   /* 141106 JPL I 16 -> WN5STATUS    */
    reactivate_exec_queue(r);                /* 141107 JPL I 11 -> XACTRDY      */
    return process_next_nd500_message();     /* 141110 JMP I 11 -> NXTMSG       */
    /* 141111 JMP 15 -> 141126 (the buffer-resident shortcut) */

    /* --- pointer/literal pool 141112..141125 is DATA (indirect worker targets),
     *     not code; the disassembler mis-renders it. Resolved via L07:
     *       141112 5GTDF   141113 NORMMC  141117 EMONICO 141120 XACTRDY
     *       141121 NXTMSG  141122 OSTRS   141124 WN5STATUS                     */

sttdriv:
    /* 141126-141166: STTDRIV - start the terminal output driver. */
    save_message(X);                         /* 141126 STX -1 : mem[141125]=X   */
    T = ind(r, 042);                         /* 141127 LDT I 42 : TODF bank     */
    X += 0142;                               /* 141130 AAX 142                  */
    A = MB;                                  /* 141131 LDATX                    */
    B = A;                                   /* 141132 RADD CLD SA DB : B = A    */
    X += -1;                                 /* 141133 AAX -1                   */
    A = MB;                                  /* 141134 LDATX                    */
    X += 1;                                  /* 141135 AAX 1                    */
    MB = A;                                  /* 141136 STATX                    */
    X += 1;                                  /* 141137 AAX 1                    */
    MB = 0;                                  /* 141140 STZTX                    */
    X += -0143;                              /* 141141 AAX -143                 */
    A = X;                                   /* 141142 RADD CLD SX DA : A = X    */
    { int t = X; X = B; B = t; }             /* 141143 SWAP SB DX : full exchange*/
    T = mem_ref(r, 026);                     /* 141144 LDT 26                   */
    setup_output_driver_frame(r);            /* 141145 JPL I 26 -> XSTDFADDR    */

    int mfunc;
    A = mem[X + 3];                          /* 141146 LDA ,X 3 : TYPRING word  */
    if (A & (1 << 4)) {                      /* 141147 BSKP ONE 40 DA (bit4=5BAD)*/
        A = mem_ref(r, 023);                 /* 141151 LDA 23 : L12STDV (TAD)   */
        mfunc = A;                           /* 141152 JMP 5 -> 141157          */
    } else {
        T = mem_ref(r, 022);                 /* 141153 LDT 22                   */
        A = 0;                               /* 141154 RADD CLD 0 DA : A = 0    */
        set_xnochar_zero(r);                 /* 141155 JPL I 16 -> 141173       */
        A = mem_ref(r, 020);                 /* 141156 LDA 20 : L3STDV (normal) */
        mfunc = A;
    }
    { int t = X; X = B; B = t; }             /* 141157 SWAP SB DX : restore     */
    mem[B + 6] = mfunc;                      /* 141160 STA ,B 6 : MFUNC         */
    A = 014;                                 /* 141161 SAA 14 : N5IOWAIT        */
    write_nd500_status(r);                   /* 141162 JPL I 15 -> WN5STATUS    */
    check_cpu_datafield(r);                  /* 141163 JPL I 15 -> GCPUDF       */
    start_terminal_driver(r);                /* 141164 JPL I 15 -> RTACT        */
    A = X;                                   /* 141165 RADD CLD SA DX (X = A)... */
    return process_next_nd500_message();     /* 141166 JMP I 14 -> NXTMSG       */

    /* The driver runs asynchronously. On output completion the terminal driver
     * re-enters level-12 via OSTRS, which for SMCNO=511 runs the input phase
     * (XNINSTR/NECHO) and writes 11NOCHRET (bytes read) + NUMPAR mask 100000B
     * back into the message buffer. That input code is adjacent and NOT in this
     * carve - see the README honest caveats. */
}
