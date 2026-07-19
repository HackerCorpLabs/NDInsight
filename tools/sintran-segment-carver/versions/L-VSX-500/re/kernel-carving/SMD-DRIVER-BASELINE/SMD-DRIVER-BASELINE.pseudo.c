/* ==========================================================================
 * SMD / Winchester / ST506 disk driver  -  block-0 read + function-42 path
 * SINTRAN III  L-VSX-500 / L07,  segment 065-S3SIPIT  base 032000B.
 *
 * Readable pseudo-C of the WORKING baseline that @ENTER-DIRECTORY uses.
 * VERIFIED tags = derived from carved bytes; INFERRED = from NPL logic only.
 * All numeric function/format codes are OCTAL (as in the NPL/carve).
 * ========================================================================== */

/* Device datafield (per unit / per controller), field displacements: */
struct DiskDatafield {
    /* -12 */ word  TRNSF;   /* device-type tag; == "BDISK" for SMD/ST506/cartridge */
    /*   4 */ word  SLINK;   /* sort chain link                                     */
    /*  11 */ word  TYPCO;   /* type + condition bits (SSEEK = seek-in-progress)    */
    /*  13 */ word  DODMA;   /* pointer to the level-11 DMA transfer primitive      */
    /*  14 */ word  ABFUN;   /* filesystem function word (the ,X 14 dispatch cell)  */
    /*  40 */ word  CTRLR;   /* saved controller return-link (WSEEK)                */
    /* ... hardware device number HDEV, buffer descriptor, block number, etc.  */
};

/* --------------------------------------------------------------------------
 * FUNCTION 42 (READ FORMAT) - the near-no-op.        VERIFIED gate @054463
 *
 * The device-agnostic filesystem calls the driver with function 42 to learn
 * the disk FORMAT. For the SMD/ST506/cartridge driver this touches NO disk:
 *   - if this is a real block disk (TRNSF == BDISK) it returns a format
 *     number from an internal table into DMA-buffer word 0 and terminates;
 *   - otherwise READ FORMAT is "not legal in driver" and it jumps to FIN.
 * ------------------------------------------------------------------------ */
void CTRDISK_function_decode(struct DiskDatafield *X, int function /*A*/)
{
    /* 054463..054472 - VERIFIED */
    if (function == 042 && X->TRNSF != BDISK) {
        goto FIN;                 /* GO FAR FIN: READ FORMAT NOT LEGAL IN DRIVER */
    }
    if (function == 042 /* && X->TRNSF == BDISK */) {
        /* INFERRED (NPL): pure table lookup, no controller I/O, no seek,
         * no DODMA. Return the format code in the first DMA-buffer word. */
        dma_buffer[0] = format_number_table[ X->unit_format_index ];
        goto FIN;                 /* terminate - NOTHING is read from the disk */
    }
    /* function != 42 -> normal transfer decode (read/write a block) */
    STRDISK(X, function);
    return;

FIN: /* 054536 - driver termination; STF ,X 2 stores status; no I/O issued */
    driver_terminate(X);
}

/* --------------------------------------------------------------------------
 * FUNCTION 0 (READ) of BLOCK 0 - the WORKING @ENTER-DIRECTORY path.
 *
 * ENDIR -> CHDSI -> RXDIR(block:=0) -> RCBLO loads the device datafield
 * transfer pointer (,X 14) and does JPL I ,B 10 into this driver.
 * There is NO capacity probe and NO special-casing of block 0: the block
 * number (0) is already in the datafield, and the driver just seeks + DMAs
 * that one physical block.                          VERIFIED transfer chain
 * ------------------------------------------------------------------------ */
void STRDISK(struct DiskDatafield *X, int function)          /* STRDI 056266 */
{
    int fn = (X->ABFUN >> 6) & 07;   /* 056275: LDA ,X 14; AND 170; SHR 6  */
    /* set up sector/track/buffer for the requested block (block 0 here) ... */

    if (X->TYPCO & SSEEK)  BSEEK(X);       /* 056011: start parallel seek    */
    WSEEK(X);                              /* 056072: wait seek complete      */
    SSTDI(X);                              /* 056504: CALL DODMA (start DMA)  */
    /* -> GO WT11 : wait for DMA-done interrupt, then complete the request.  */
}

void BSEEK(struct DiskDatafield *X)                          /* 056011 VERIFIED */
{
    word A = X->SLINK->TYPCO;              /* T:=X; X:=X.SLINK; A:=X.TYPCO; X:=T */
    if (!(A & SSEEK))          return;     /* IF A NBIT SSEEK THEN EXIT          */
    if (X->TRNSF != BDISK)     return;     /* IF TRNSF >< BDISK THEN EXIT        */
    /* A \/ 020004 ; IOX -> initiate seek on the controller                     */
}

void WSEEK(struct DiskDatafield *X)                          /* 056072 VERIFIED */
{
    X->CTRLR = current_link;               /* A:=L=:CTRLR                        */
    for (;;) {                             /* spin reading controller status     */
        word st = IOX(HDEV + 4);           /* *IOXT read status                  */
        if (seek_error(st)) goto ctrlr_err;
        if (seek_complete(st)) break;
        IOX(HDEV + 1);                     /* nudge                              */
    }
    IOX_or(HDEV + 1, 030005);              /* A \/ 030005; *IOXT enable int      */
    ID11();                                /* CALL ID11: wait on seek interrupt  */
    /* T:=HDEV+RSC; *IOXT read seek condition ...                                */
    return;
ctrlr_err:
    return_via(X->CTRLR);
}

void SSTDI(struct DiskDatafield *X)                          /* 056504 VERIFIED */
{
    ( *(void(**)()) &X->DODMA )();         /* CALL DODMA = JPL I ,B 13           */
    /* DODMA programs the DMA controller and starts the physical block read.    */
}

/* ==========================================================================
 * WHY SMD WORKS AND SCSI FAILS FOR "read block 0"
 * (SCSI side: ../SCSI-DRIVER/ ; SCLLD 067160B, SCSDI 057215B - same segment.)
 *
 *   SMD  function 42 : table lookup -> buffer[0], NO disk I/O, terminate.
 *   SCSI function 42 : INQUIRY + READ CAPACITY + one control-record READ(6),
 *                      returns UHLIM; issues NO block-0 read and chains none.
 *
 *   SMD  block 0     : block number already 0 in datafield; seek + DMA the
 *                      single physical block. No capacity/geometry needed.
 *   SCSI block 0     : must go SCLLD -> INITO -> SCWAQ, LBA derived from the
 *                      READ CAPACITY result; if capacity/geometry is mis-scaled
 *                      or SCLLD is never called, the page-0 read is never issued
 *                      -> @ENTER-DIRECTORY sees SCWAQ empty and fails.
 * ========================================================================== */
