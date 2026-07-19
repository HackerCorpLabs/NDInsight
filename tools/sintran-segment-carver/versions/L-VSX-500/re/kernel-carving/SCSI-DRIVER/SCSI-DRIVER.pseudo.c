/* ===========================================================================
 * SINTRAN III L07  -  SCSI DISK DRIVER + DISK LAYER  (readable pseudo-C)
 *
 * Segment 065-S3SIPIT (base 32000B), byte-verified.  See SCSI-DRIVER.ASM.
 *
 * Scope split:
 *   - This driver + disk layer is the SCSI-specific side.
 *   - The device-agnostic ENTER-DIRECTORY / mount path is a SEPARATE module
 *     (companion carve).  It is the CALLER of scsi_disk_layer().
 *
 * Naming:
 *   X   = unit datafield (per-logical-unit control block); X.<field> = offset.
 *   SCWAQ = "waiting for arbitration" queue head (cell 177721B).
 *   BUSFL = current bus/phase flags.  NCROK = interface health.
 *   5SCIN = "initialisation finished" flag bit in X.SUTYP.
 * =========================================================================== */


/* ---------------------------------------------------------------------------
 * DRIVER CORE   (IP-P2-SCSI-DRIV.NPL)
 *
 * The driver is passive: it only ever acts on an element that was linked into
 * SCWAQ by INITO, and INITO is only reached from SCLLD.  There is NO code path
 * by which the driver invents a transfer for itself.
 * ------------------------------------------------------------------------- */

/* SCLLD @067160B  -  the one and only entry through which work enters. */
void SCLLD(int A /*function type*/, unit *X)
{
    int fclass = A >> 8;                 /* D := A SHZ -10  (067160-067161) */

    if (fclass < 3) {                    /* 067163: data transfer READ/WRITE */
        X->SUCON = A;                    /* 067165 */
        X->SULRG = L;                    /* 067167  return address           */
        X->SUTRG = 0;                    /* 067170 */
        if (NCROK < 0) {                 /* 067171-067172 interface error    */
            T = NCRST; goto EXDRI;
        }
        INITO(X);                        /* 067175  ** ENQUEUE onto SCWAQ **  */
        if (BUSFL == 0)                  /* 067176-067177                    */
            SELEC();                     /* 067200  start arbitration        */
    }
    else if (fclass == 3) {              /* 067202  start-with-timer control */
        X->SUCON = A; X->SULRG = L; X->SUTRG = 0;
        X->SUTHS = BIT(6SFUN);           /* initial status                   */
        ENTIM(X);                        /* 067214  enable per-unit timer    */
    }
    else if (fclass == 4) {              /* 067216  SCSI bus reset           */
        if (NCROK & BIT(8SRST)) { T = 0; return_intermediate(); }
        else { SCRXR=X; SCRLR=L; SCRCO=A; goto SCRST; }
    }
    else {
        T = ILDCO;                       /* 067236  illegal function         */
    }
    goto SCWTI;                          /* 067237  busy/return dispatcher   */
}

/* INITO @070261B  -  THE ENQUEUE.  Build initial thread status, then splice
 * this unit datafield onto the tail of the SCWAQ arbitration queue. */
void INITO(unit *X)
{
    int opclass = X->SUCON >> 8;         /* 070261-070263 */
    if (opclass == 0)      X->SUTHS = INOPR | (X->SUCON & BIT(4SRCA) ? BIT(6SRFD):0);
    else if (opclass == 1) X->SUTHS = INABO;          /* abort            */
    else                   X->SUTHS = INBDR;          /* bus device reset */

    X->SUSTA = -1;                       /* initial status                  */
    X->SUSDP = X->SUIDP;                 /* 070302-070303 initial data ptr  */
    X->SUSBC = X->SUIBC;                 /* 070304-070305 initial byte cnt  */
    X->SUTMR = -2;                       /* 070306-070307                   */

    /* walk the SCWAQ chain to its end and link X in (070310-070317) */
    unit *p = SCWAQ_HEAD;                /* "SCWAQ-SULINK" + B              */
    while (p->SULINK != 0) p = p->SULINK;
    p->SULINK = X;                       /* ** X is now in SCWAQ **         */
}

/* SELEC @070165B  -  arbitration.  If SCWAQ is EMPTY there is nothing to
 * arbitrate for, so the bus is declared free and we return normally. */
void SELEC(void)
{
    save(SVTAD, SAVXR);
    SCEIM = -1;                          /* disable interrupt               */
    unit *X = SCWAQ_HEAD;                /* 070171  X := SCWAQ              */
    if (X != 0) {                        /* 070172 */
        BUSFL = BIT(6SARB);              /* indicate arbitration            */
        program_ncr_select(X);           /* WDESI, transfer-counter, WNCOM  */
        SCTST = 1; TMR = -5;             /* arm select timeout              */
    } else {
        BUSFL = 0;                       /* 070240  ** SCWAQ EMPTY: BUS FREE ** */
    }
    enable_interrupt();                  /* 070241-070243 WCONT             */
}

/* SCINT @067247B  -  interrupt handler (phase dispatch).  On the failing run
 * it takes the A=4 disconnect leg: DCTHR -> TEROP(T=0 success) -> GO BUSFP ->
 * SELEC, which finds SCWAQ empty and returns success.  Exactly one WCONT=5. */
void SCINT(void)
{
    A = iox_read(HDEV + RSTAU);          /* 067247-067251 device status     */
    if ((A & 64) != 0) {
        if (A & BIT(2)) goto SCWTI;      /* controller busy                 */
        /* ... bus-reset / initiator-detected-error handling ...            */
    }
    if (A & BIT(11)) {                   /* interrupt from NCR              */
        int newstatus = read_ncr_interrupt_regs();
        if ((newstatus & 0177500) == SCEIM) SCISR();        /* expected     */
        else dispatch_unexpected(newstatus);/* A=4 disconnect -> DCTHR ...  */
    }
    activate_and_enable();               /* 5\/SCCCW; WCONT                 */
    goto SCWTI;
}


/* ---------------------------------------------------------------------------
 * DISK LAYER   (IP-P2-SCSI-DISK.NPL)  -  what the filesystem hands off to.
 *
 * scsi_disk_layer() is the level-11 transfer routine.  It (a) lazily runs
 * INQUIRY + READ CAPACITY once (INQUI, guarded by 5SCIN) and (b) performs the
 * requested function.  For function 42 it reads the CONTROL RECORD and returns
 * the disk geometry.  It builds a SCSI command and calls the driver via SCSID
 * (which ultimately calls SCLLD).
 * ------------------------------------------------------------------------- */

/* Runs INQUIRY then READ CAPACITY, computes the record-size shift table,
 * sets 5SCIN.  For a function-42 op it finishes right here (RCAFI). */
void INQUI(unit *X)
{
    send_scsi(INQUIRY);                          /* 062223 */
    int devtype = inquiry_device_type();
    if (is_direct_access(devtype))
        send_scsi(READ_CAPACITY);                /* 062273  -> SURSZ, block size */
    compute_shift_table(&X->SUSI1, &X->SUSI2, &X->SUSI3);
    X->SUTYP |= BIT(5SCIN);                      /* 062503 INITIALISATION FINISHED */
    if ((X->ABFUN & 077) == 42) { RCAFI(X); return; }   /* 062506 */
    if (X->ABFUN == 36) return; /* FINEX */
    goto REPEAT;   /* a NORMAL data op loops back to actually issue the read */
}

/* scsi_disk_layer (NEWOP/RETRY/FINEX, 057016B..057406B). */
int scsi_disk_layer(unit *X, int func)
{
    /* ---- NEWOP: per-function set-up (057016-057112) ---- */
    if (func == 42) {                            /* 057074 READ FORMAT */
        MEMAD  = X->MEMAD;
        ABFUN  = X->ABFUN;
        X->ABP31 = 0; X->ABP32 = 0100;
        X->SUTYP = SCDFA->SUTYP & ~BIT(5SCIN);   /* 057106 FORCE RE-INQUIRY */
    }

    /* ---- RETRY: lazy init, then perform the function (057116-057227) ---- */
    if (!(X->SUTYP & BIT(5SCIN)))                /* 057116 not inited yet */
        INQUI(X);                                /* AUTO INQUIRY + READ CAPACITY */

    if (X->SCOCW & BIT(3SF42)) {                 /* 057162 this is read-format */
        X->ABFUN &= 0177700;                     /* indicate READ */
        /* 057170: control-record LBA comes from the unit's MEMA1/MEMA2 */
        dword cr_lba = ldd(X->MEMA1, X->MEMA2);
        X->ABPA2 = cr_lba;                       /* disk address = control rec */
        X->ABP31 = 0; X->ABP32 = 1;              /* transfer exactly 1 block */
        X->MEMAD += 01000;                       /* bump memory address */
        SCSID(X);                                /* 057210 ** the control-record READ(6) ** */
    } else {
        SCSID(X);                                /* ordinary transfer */
    }
    goto FINEX;

FINEX:                                           /* 057257 */
    if (X->DQOPC & BIT(3SF42)) {                 /* post-process control record */
        if ((X->HSTAT & 017) <= 1) {             /* good status */
            clear_cache();
            /* checksum control record over -1000 words (057304-057307) */
            if (control_record_bad())            /* 057311 */
                { T = NOCRC; goto RETEX; }        /* no control record */
            move_partition_table_to_buffer();    /* 057323-057335 */
            UHLIM = data_area_size();            /* 057344 ** RETURN GEOMETRY ** */
            /* status word 36 stored for the caller */
        }
    }
    T = 0;

RETEX:                                            /* 057352 */
    RETOP(X);                                     /* TERMINATE OPERATION, return */
    return T;
    /* NOTE: control flow returns to the FILESYSTEM here.  No block-0 read has
     * been (or will be) issued by this routine for a function-42 request. */
}


/* ===========================================================================
 * THE ENQUEUE DECISION   -  clearly marked, per task.
 *
 * "Does the SCSI disk layer enqueue a page-0 / block-0 data read after
 *  function-42 success?"   -> NO.
 *
 * The only calls that reach SCLLD in a function-42 flow are:
 *   1. INQUIRY  (via INQUI/EXINT)
 *   2. READ CAPACITY (via INQUI/EXINT)
 *   3. exactly ONE READ(6) of the CONTROL RECORD at the last LBA (057210)
 * After that, FINEX returns UHLIM + the partition table to the caller and the
 * operation terminates (RETEX -> RETOP).  There is no 4th SCSID/SCLLD call for
 * block 0 inside this module - by design.  Function 42 is "learn the layout",
 * not "read the directory".
 *
 * The block-0 / page-0 directory read is a SEPARATE, function-0 request that
 * the device-agnostic ENTER-DIRECTORY/mount path must issue AFTER consuming
 * the function-42 result.  That request would re-enter scsi_disk_layer() with
 * func==0, fall through to the "ordinary transfer" SCSID(X) call, and THEN
 * reach SCLLD -> INITO -> SCWAQ for block 0.
 * =========================================================================== */
int does_scsi_layer_enqueue_block0_after_func42(void)
{
    return 0;   /* VERIFIED: it does not, and is not designed to. */
}
