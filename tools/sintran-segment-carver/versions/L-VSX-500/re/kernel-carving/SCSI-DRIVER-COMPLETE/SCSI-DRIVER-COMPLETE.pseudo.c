/* ==========================================================================
 *  SINTRAN III L07 (L-VSX-500)  SCSI PROTOCOL DRIVER  (IP-P2-SCSI-DRIV)
 *  Complete driver state machine in readable pseudo-C.
 *
 *  Segment 065-S3SIPIT, load base 32000B.  Addresses in the comments are the
 *  VERIFIED carved octal addresses (see SCSI-DRIVER-COMPLETE.ASM).  Logic names
 *  are from the NPL (a different revision - names only, never bytes).
 *
 *  Two actors:
 *    - LEVEL 11 (request side):  SCLLD enqueues onto SCWAQ, kicks SELEC.
 *    - INTERRUPT side:           SCINT + SCISR drive the NCR 5386 phase machine
 *                                to completion, then TEROP/EXDRI return to the
 *                                caller and BUSFP re-arbitrates for the next op.
 * ========================================================================== */

/* ---- driver datafield (B-relative cells; see ASM legend) ---------------- */
static int   BUSFL;      /* -35  live bus/phase status word (the state)       */
static int   SCTST;      /* -36  interface-test / timer-owner flag            */
static int   NCROK;      /* -47  controller-ok / recovery flags               */
static void *SCWAQ;      /* -57  head of Wait-for-ARBitration Queue           */
static void *SCTQP;      /* -56  head of timer queue                          */
static void *SCCSU;      /* -60  currently-selected unit datafield (0 = none) */
static int   SCEIM;      /* -77  expected-interrupt mask (-1 = disabled)      */
static int   SCNIH;      /* -77  next-interrupt handler vector (alias)        */
static int   SCCCW;      /*-100  control-word shadow                          */
static int   SCNIS;      /*-101  new interrupt status                         */
static int   SCSSR;      /*-102  saved SCSI status register                   */
static int   CMSGO, CMSGI;                       /* current msg out / in      */
static int   SCCDP, SCCBC;                        /* current data ptr / count  */
static int   HDEV;       /* -3   device IOX base                              */
static int   TMR;        /* -5   countdown timer                             */

/* NCR register displacements from HDEV (from IP-P2-SCSI-DRIV.NPL SYMBOLs) */
enum { RSTAU=4, WCONT=5, WDESI=47, WTCM=71, WTC2=73, WTCL=75,
       WNCOM=43, WNDAT=41, RAUXS=50, RITRG=54, RNDAT=40 };

/* BUSFL / SUTHS phase-flag bits */
enum { b6SARB=5, b6SMSI=6, b6SCCO=7, b6SDIS=10, b6SRFD=11, b6SMSO=12,
       b6SRST=15, b6SIDE=16, b6SFUN=17 };

/* unit datafield */
typedef struct unit {
    int   SUDLU;   /* 0   device/LUN select id                */
    long  SUIDP;   /* 1   initial data pointer                */
    long  SUIBC;   /* 3   initial byte count                  */
    int   SUSTA;   /* 13  status                              */
    struct unit *SULINK; /* 27 queue link                     */
    int   SUCON;   /* 30  control word; class = SUCON >> 8    */
    int   SUTHS;   /* 31  thread status (this op's BUSFL)     */
    int   SULRG;   /* 32  caller return address (L)           */
    int   SUTRG;   /* 33  caller target / error status        */
    int   SUTMR;   /* 35  timer delta                         */
    long  SUSDP;   /* 7   saved data pointer                  */
    long  SUSBC;   /* 11  saved byte count                    */
} unit;

static void iox(int reg, int val);   /* *IOXT to HDEV+reg                    */
static int  iox_read(int reg);

/* forward decls of the internal routines */
static void INITO(unit *x);
static void SELEC(void);
static void RFWAQ(void);
static void CNTHR(unit *x);
static void DCTHR(unit *x);
static void TEROP(unit *x, int T);
static void ENTIM(unit *x);
static void RSTMR(void);
static void SCRST(int T);
static int  EXDRI(unit *x, int T);


/* ==========================================================================
 *  SCLLD @067160  - DRIVER ENTRY.  The ONE door into the driver.
 *    A = function type, X = unit.  L = caller return address.
 * ========================================================================== */
int SCLLD(int A, unit *x, int L)
{
    int cls = A >> 8;                       /* 067160-067163  D := A >> 8       */

    if (cls < 3) {                          /* --- DATA TRANSFER (READ/WRITE) --*/
        x->SUCON = A;                       /* 067165                          */
        x->SULRG = L;                       /* 067167                          */
        x->SUTRG = 0;                       /* 067170                          */
        if (NCROK < 0)                      /* 067171-067172  interface dead?  */
            return EXDRI(x, /*NCRST*/061);  /* 067173-067174  GO FAR EXDRI     */

        INITO(x);                           /* 067175  ** ENQUEUE onto SCWAQ ** */
        if (BUSFL == 0)                     /* 067176-067177  bus idle?        */
            SELEC();                        /* 067200  ** START ARBITRATION **  */
        goto scwti;                         /* 067201  busy return             */
    }
    else if (cls == 3) {                    /* --- start-with-timer control --- */
        x->SUCON = A; x->SULRG = L; x->SUTRG = 0;    /* 067205-067210          */
        x->SUTHS = (1 << b6SFUN);           /* 067211-067213  initial status   */
        ENTIM(x);                           /* 067214  enable timer            */
        goto scwti;                         /* 067215                          */
    }
    else if (cls == 4) {                    /* --- bus reset ------------------ */
        if (NCROK & (1 << /*8SRST*/0)) {    /* 067221-067223  already resetting?*/
            return L + 1;                   /* 067232-067234  intermediate ret  */
        }
        /* save X/L/A into SCRXR/SCRLR/SCRCO, then reset the bus */
        SCRST(/*SCLRE from SCRCO*/A);       /* 067224-067230  GO FAR SCRST     */
    }
    else {
        /* T := ILDCO (illegal function)  067236 */
    }
scwti:
    return /*GO SCWTI busy/return dispatcher*/ 0;   /* 067237 JMP I SCWTI      */
}


/* ==========================================================================
 *  INITO @070261  - INITIALIZE OPERATION + SPLICE ONTO SCWAQ TAIL.
 *    The only routine that appends to SCWAQ.  Nothing reaches the wire that was
 *    not linked here.
 * ========================================================================== */
static void INITO(unit *x)
{
    int cls = x->SUCON >> 8;                /* 070261-070263                   */
    int ths;
    if (cls == 0) {                         /* 070264  normal operation        */
        ths = /*INOPR*/ 0;
        if (x->SUCON & (1 << /*4SRCA*/6))   /* 070266  return-on-cmd-accepted? */
            ths |= (1 << b6SRFD);           /* 070270                          */
    } else if (cls == 1) {
        ths = /*INABO*/ (1 << b6SMSO) | 6;  /* 070272  ABORT                   */
    } else {
        ths = /*INBDR*/ (1 << b6SMSO) | 014;/*        BUS DEVICE RESET         */
    }
    x->SUTHS = ths;                         /* 070300-070301 (SUSTA:=-1)       */
    x->SUSDP = x->SUIDP;                    /* 070302-070303  initial ptrs     */
    x->SUSBC = x->SUIBC;                    /* 070304-070305                   */
    x->SUTMR = -2;                          /* 070306-070307                   */

    /* walk SULINK from the SCWAQ anchor to the tail, link x in */
    unit *p = (unit *)&SCWAQ;               /* 070310-070311  "SCWAQ-SULINK"+B */
    while (p->SULINK != 0)                  /* 070313-070316                   */
        p = p->SULINK;
    p->SULINK = x;                          /* 070317  ** LINKED ONTO TAIL **  */
}


/* ==========================================================================
 *  SELEC @070165  - ARBITRATION / SELECTION.
 *    SCWAQ empty  -> BUSFL := 0 (bus free), return (this is the FAILING-trace
 *                    leg: nothing queued, so nothing happens).
 *    SCWAQ head   -> program the NCR select command onto the wire, arm the
 *                    select timeout.
 * ========================================================================== */
static void SELEC(void)
{
    SCEIM = -1;                             /* 070167-070170  disable exp int   */
    unit *x = SCWAQ;                        /* 070171                          */

    if (x == 0) {                           /* 070172  ** SCWAQ EMPTY **        */
        BUSFL = 0;                          /* 070240  declare BUS FREE        */
    } else {                                /* ** SCWAQ NON-EMPTY: select **    */
        iox(WCONT, 0);                      /* 070173-070176  clear to memory  */
        BUSFL = (1 << b6SARB);              /* 070177-070200  ARBITRATION       */
        iox(WDESI, x->SUDLU >> 12);         /* 070201-070204  SCSI ident       */
        iox(WTCM, /*WATFS*/0);              /* 070205-070215  waiting time ->   */
        iox(WTC2, 0);                       /*                transfer counter   */
        iox(WTCL, 0);
        int cmd = (x->SUCON & (1 << /*4SINA*/4)) ? 011 : 010;  /* 070216-070222 */
        iox(WNCOM, cmd);                    /* 070224  ** SELECT ON THE WIRE ** */
        if (SCTST >> 2) x->SUTMR = TMR;     /* 070225-070232  save current tmr  */
        SCTST = 1;                          /* 070233-070234                   */
        TMR   = -5;                         /* 070235-070236  arm select TO     */
    }
    iox(WCONT, 5);                          /* 070241-070244  enable interrupt  */
}


/* ==========================================================================
 *  SCINT @067247  - INTERRUPT / COMPLETION HANDLER.
 *    Read status; hardware error bits first, then NCR interrupt.  If the
 *    interrupt matches SCEIM it is EXPECTED -> SCISR runs the phase machine.
 *    Otherwise decode the phase code and take the correct arm.
 * ========================================================================== */
void SCINT(void)
{
    int st = iox_read(RSTAU);               /* 067247-067251                   */
    if (st & 64) {                          /* 067252-067254  hw error bits    */
        if (st & 4) goto scwti;             /* 067255-067256  controller busy  */
        SCSSR = st;
        if (st & (1 << 5)) { SCRST_via_SCDIS(/*SBRST*/043); return; } /* 067260-067263 */
        if (st & (1 << 4)) SCIDE();         /* 067264-067266  initiator error  */
    }
    SCSSR = st;
    if (!(st & (1 << 11)))                  /* 067271-067272  from NCR?        */
        goto tail;

    iox(WCONT, 0);                          /* 067273-067276  clear to memory   */
    int aux = iox_read(RAUXS);              /* 067277-067301                   */
    int ireg = iox_read(RITRG);             /* 067302-067305  interrupt reg    */
    SCNIS = ireg; SCCCW = 0;                /* 067306-067307                   */

    if ((SCNIS & 0177500) == SCEIM) {       /* 067310-067313  ** EXPECTED **    */
        SCISR();                            /* 067314  run the phase machine    */
        goto tail;
    }

    int A = SCNIS >> 8;                     /* 067316  phase code              */
    int X = BUSFL;                          /* 067317                          */

    if (A == 4 && X != 0) {                 /* 067320-067323  ** DISCONNECT **  */
        if (SCCSU != 0) {                   /* 067324-067325                   */
            DCTHR(SCCSU);                   /* 067326  disconnect logical thread*/
        } else if (BUSFL & (1 << b6SARB)) { /* 067330-067332  arbitration TO   */
            ((unit *)SCWAQ)->SUTMR++;       /* 067333-067334  count retries    */
            if (/*retries exhausted*/1) {   /* 067336                          */
                SCTST = 0;                  /* 067337                          */
                RSTMR();                    /* 067340                          */
                RFWAQ();                    /* 067341  drop the element        */
                TEROP(0, /*NESER*/040);     /* 067342-067343  cannot select    */
            }
        }
        BUSFP();                            /* 067344  GO FAR BUSFP (re-arb)   */
    }
    else if (A == 1 && (X & (1 << b6SARB))) {   /* 067356-067362  ARBITRATION WON */
        RFWAQ();                            /* 067363  leave the wait queue    */
        BUSFL = (BUSFL & /*CNCLR*/0177000) | (1 << /*6SCRP*/3);  /* 067364-067367 */
        CNTHR(SCCSU);                       /* 067370  ** CONNECT PHYS PATH ** */
        SCTST = 0; ENTIM(SCCSU);            /* 067371-067372  start timer      */
        SCNIH = /*RBSIR*/0;                 /* 067373-067374  allow BSI ints   */
    }
    else if (A == 020 && !(X & ((1<<b6SARB)|(1<<b6SRST)))) {  /* 067376-067406 RECONNECT */
        BUSFL = X | (1 << /*6SCRP*/3);      /* indicate reconnect              */
        SCNIH = /*RBSIR*/0;                 /* 067407-067410                   */
    }
    else if (A == 1 && (NCROK & (1 << /*8SDIA*/20))) {  /* 067412-067417  SELFTEST done */
        STFIN();                            /* 067420  GO FAR STFIN            */
    }
    else if (A == (SCEIM >> 8) && (SCNIS & (1 << /*PARIT*/6))) { /* 067422-067427 parity */
        /* count parity errors, then message-parity vs initiator-detected err */
        if ((017 & BUSFL) == 7) SCMPE();    /* 067435-067444  message phase    */
        else                    SCIDE();    /* 067446                          */
        SCISR();                            /* 067447                          */
    }
    else {
        SCRST_via_SCDIS(/*NCRER*/060);      /* 067451-067452  illegal interrupt */
    }
tail:
    iox(WCONT, 5 | SCCCW);                  /* 067453-067457  activate+enable int*/
scwti:
    return;                                 /* 067460  GO SCWTI                */
}


/* ==========================================================================
 *  DCTHR @070341  - DISCONNECT LOGICAL THREAD  (the A=4 disconnect leg).
 * ========================================================================== */
static void DCTHR(unit *x)
{
    if (x->SUTRG == 0 && (BUSFL & (1 << b6SDIS))) {   /* 070343-070350          */
        if (BUSFL & (1 << b6SRFD)) {        /* 070351  first-disconnect return  */
            x->SUTHS = (x & /*DCCLR*/0) | CMSGO;      /* 070353-070355          */
            /* T := -1  -> intermediate return later                            */
        } else {
            x->SUTHS = (x & /*DCCLR*/0) | CMSGO;      /* 070360-070362          */
            /* X := 0  -> no return                                             */
        }
    } else {
        TEROP(x, /*T from caller*/0);        /* 070365  ** TERMINATE OP **      */
    }
    SCCSU = 0;                              /* 070366  path disconnected        */
    /* GO HOME4 (return to SCINT)  070367                                       */
}


/* ==========================================================================
 *  TEROP @070500  - TERMINATE OPERATION, derive final status, free the unit.
 *    Falls into DITIM (disable this LUN's timer) then returns.
 * ========================================================================== */
static void TEROP(unit *x, int T)
{
    if (x->SUTRG != 0) {                    /* 070500-070501  error in op       */
        T = x->SUTRG;
    } else if (SCCSU == x) {                /* 070502-070504                   */
        x->SUSDP = SCCDP;                   /* 070505-070510  save data ptrs    */
        x->SUSBC = SCCBC;
        /* NBIT 6SMSO: 6SCCO -> NOSST/0 ; else if disconnect+T==0 -> UNDIS      */
        /* BIT  6SMSO: T==0 -> TRANE (parity) or MNIBT (msg not implemented)    */
    } else {
        /* timer op: SUCON class==3 && CMTMO==T -> T:=0 (timer finished)        */
    }
    x->SUTHS = 0;                           /* 070560  mark unit free           */
    /* DITIM @~070561: unlink x from the timer queue, then RSTMR()              */
}


/* ==========================================================================
 *  BUSFP @067745  - BUS FREE PHASE.  Just re-check the arbitration queue.
 *    After a completed/aborted op this pulls the next SCWAQ head or, if empty,
 *    SELEC sets BUSFL:=0 and the bus goes idle.
 * ========================================================================== */
void BUSFP(void)
{
    SELEC();                                /* 067745  CALL SELEC              */
    /* falls into EXDRI @067746                                                */
}


/* ==========================================================================
 *  EXDRI @067746  - EXIT DRIVER.  Compute caller return from SULRG + T.
 * ========================================================================== */
static int EXDRI(unit *x, int T)
{
    if (x == 0) return 0;                   /* 067746-067747  no caller -> SCWTI*/
    int p = x->SULRG;                       /* 067750                          */
    if (T == 0) return p + 2;               /* 067751-067754  OK               */
    if (T < 0)  return p + 1;               /* 067755-067757  intermediate      */
    /* MIN ERCNT */                         /* 067761  error                    */
    return p;                               /* 067763                          */
}


/* ==========================================================================
 *  HOW A QUEUED BLOCK-0 TRANSFER ACTUALLY REACHES THE WIRE
 *  --------------------------------------------------------
 *   1. Disk layer builds the READ(6) CDB for block 0 and executes it via the
 *      vector at 063453 (= 067160 = SCLLD).                [VERIFIED ptr]
 *   2. SCLLD(A=class<3, x): x->SUCON=A; INITO(x)  -> x linked onto SCWAQ tail.
 *   3. If BUSFL==0, SCLLD calls SELEC:
 *         SCWAQ head = x  -> iox(WNCOM, select) drives ARBITRATION+SELECT.
 *   4. Target responds; SCINT sees A==1 & 6SARB -> RFWAQ (leave queue),
 *      CNTHR (SCCSU:=x, connect), ENTIM (arm op timer), SCNIH:=RBSIR.
 *   5. Each subsequent bus-service interrupt is EXPECTED (SCEIM match) so SCISR
 *      -> NEWPH decodes the phase and:
 *         COMMAND phase (COMPH)  -> DMA the 6-byte CDB out (WNCOM 224).
 *         DATA-IN phase (DAIPH)  -> DMA the 512-byte block into ND-100 memory.
 *         STATUS phase (STAPH)   -> read the status byte into x->SUSTA.
 *         MESSAGE-IN (MSIPH)     -> COMMAND COMPLETE / DISCONNECT messages.
 *   6. On COMMAND COMPLETE the message machine sets 6SCCO; TEROP derives T=0,
 *      frees the unit, EXDRI returns SULRG+2 to the caller, and BUSFP -> SELEC
 *      pulls the next SCWAQ element (or idles the bus).
 *
 *  => A block-0 READ only reaches the wire if SCLLD is called for it.  On the
 *     failing ENTER-DIRECTORY trace the second (block-0) SCLLD call never
 *     arrives: after the function-42 control-record read the driver runs only
 *     the A==4 disconnect leg -> DCTHR -> TEROP -> BUSFP -> SELEC finds SCWAQ
 *     EMPTY -> BUSFL:=0, and the bus goes idle with nothing more queued.  The
 *     missing enqueue is UPSTREAM of this driver (device-agnostic mount path).
 * ========================================================================== */
