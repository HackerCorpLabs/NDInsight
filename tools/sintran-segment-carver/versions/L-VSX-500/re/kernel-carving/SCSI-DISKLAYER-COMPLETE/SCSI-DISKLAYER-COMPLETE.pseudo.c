/* ============================================================================
 * SCSI-DISKLAYER-COMPLETE.pseudo.c
 * SINTRAN III VSX/500 L07 - IP-P2-SCSI-DISK disk layer (segment 065-S3SIPIT)
 *
 * Reconstructed C for the COMPLETE SCSI disk-layer transfer state machine:
 * SCSDISK entry -> geometry bounds -> RETRY/lazy-INQUIRY init -> device-type
 * gate -> control-record READ -> checksum/geometry publish -> success/error.
 * Plus SCSID function dispatch and INQUI SUTYP construction.
 *
 * Every branch and every status/error exit is a clearly-marked block below.
 * Addresses in comments are L07 octal (dd offset = (addr-32000B)*2 decimal).
 * VERIFIED = read from L07 bytes ; INFERRED = NPL logic/architecture ;
 * OPEN = uncarved/runtime.  This supersedes FUNCTION-42-RETURN + SCSDISK-TRANSFER.
 * ============================================================================ */

/* ---- unit datafield (,X / ,B relative offsets) ---- */
typedef struct {
    word NFUNC;   /* 6  saved link/function on entry            */
    word SUNOP;   /* 10 pending-op count ; also HSTAT in SCSID  */
    word ABFUN;   /* 14 function/op word ; fn = ABFUN & 077     */
    dword ABPA2;  /* 17 disk block address (0 for block 0)      */
    word ABP32;   /* 22 transfer amount / block count           */
    word SUTYP;   /* 23 flags: bit7=5SCIN ; bits8-15 = devtype  */
    word UHLIM;   /* 32 disk capacity (whole-disk upper limit)  */
    word SCOCW;   /* -13 SCSI op control word                   */
    word SUTHS;   /* 31 device-busy handshake                   */
    word TACOU;   /* -15 retry counter                          */
} SCSIUNIT;

/* internal disk-layer return codes carried in T (ERREX map @062124) */
enum { OK=0, TYPER=1, ILAOP=4, BADPA=5, NOCRC=11,
       UNIT_ATTN=6, ABORTED=13 };   /* 6/13 are retried, never returned */


/* ============================================================================
 * SCSDISK  (SCSDI = 057215B)  -  disk-layer transfer primitive entry
 * Reached from the device-agnostic filesystem via RCBLO datafield ",X 14".
 * ============================================================================ */
int SCSDISK(SCSIUNIT *X)
{
    X->NFUNC = link;                       /* 057215-16 */
    perfo_dialo_setup();                   /* 057217 JPL DIALO (MON PERFO)     */
    int fn = X->ABFUN & 077;               /* 057221-24                        */

    /* 057232: illegal-operation guard (e.g. non-direct-access request) */
    if (illegal_op_flag)                   /* 057232 JAF */
        { T = ILAOP; goto terminate; }     /* 057233 SAT 4 -> 057272           */

    /* ---- geometry / address bounds check (which arm chosen at 057276-057301) */
    if (whole_disk_request) {              /* 057301 -> 057352                  */
        /* WHOLE-DISK ARM  (VERIFIED dd 057361/057401) */
        dword end = X->ABPA2 + X->ABP32;   /* 057352-355                        */
        if (end > X->UHLIM)                /* 057356-361 SKP IF 0 GRE SA        */
            goto ERR2_BADPA;               /* 057362 -> 057377                  */
        /* block 0: ABPA2==0 => end<=UHLIM => NOT taken (VERIFIED semantics)    */
    } else {
        /* PARTITION-RELATIVE ARM (057302-057351): several BADPA exits */
        if (blk >= npart_size || partition_flag_bad)   /* 057312/057323/057326 */
            goto ERR2_BADPA;
        dword end = part_base + X->ABP32;               /* 057342-346          */
        if (end > part_limit)                            /* 057347-350          */
            goto ERR2_BADPA;                             /* -> 057377            */
    }

    /* ---- in bounds : queue / dispatch this op ---- */
    X->SUNOP++;                            /* 057367 MIN ,B 10                  */
    if (X->SLINK != 0) { dsort_link(); swt11_dispatch(); }  /* 057373-375       */
    else return NEWOP(X);                  /* 057376 -> 057414                  */

ERR2_BADPA:                                /* 057377 */
    T = BADPA;                             /* 057401 SAT 5 (VERIFIED dd 171005) */
    return RETOP(X, T);                    /* 057402 -> SWT11/RETOP ; no SCLLD  */

terminate:
    return RETOP(X, T);                    /* 057272 */
}


/* ============================================================================
 * NEWOP  (057414B)  -  per-op setup + SCSDISK-level function pre-decode
 * ============================================================================ */
int NEWOP(SCSIUNIT *X)
{
    X->SLINK = X; setup_op();              /* 057414-424 */
    if (already_initialised)               /* 057424 BSKP ZRO 130 DA */
        goto RETRY;                        /* 057430 -> 057510 */

    int fn = X->ABFUN & 077;               /* 057431-432 */

    if (fn == 6)                           /* 057433-434 */
        goto init_call;                    /* 057440 -> 057626 */

    if (fn == 36) {                        /* 057442-443  read-layout at disk level */
        if (layout_out_of_range || layout_absent)  /* 057451/057454 */
            { T = BADPA; goto go_far; }    /* 057466 SAT 5 */
        movew_layout_to_caller();          /* 057463 MOVEW */
        T = OK;                            /* 057464 */
        goto go_far;                       /* 057470 -> 057626 */
    }

    if (fn == 42) {                        /* 057472-473 (VERIFIED dd 171042)   */
        /* fn 42 = READ FORMAT : force a fresh INQUIRY+READ CAPACITY */
        setup_fmt_request();               /* 057475-503 */
        X->SUTYP &= ~BIT7_5SCIN;           /* 057505-507 CLEAR 5SCIN (dd 174075)*/
    }
    /* fn 0/1/4/... : fall through with 5SCIN unchanged */

    goto RETRY;                            /* 057510 */

go_far:
    return terminate_far(X, T);            /* 057626 */
}


/* ============================================================================
 * RETRY  (057510B)  -  lazy INQUIRY init, device-type gate, real transfer.
 * This is THE decision region.  Outer WHILE back-edge is 057624 -> 057514.
 * ============================================================================ */
int RETRY(SCSIUNIT *X)
{
RETRY:                                     /* 057510-514 : reload SUTYP */
    /* ---- lazy init gate : run INQUIRY only if not yet initialised ---- */
    if (X->SUTYP & BIT7_5SCIN)             /* 057514-515 (VERIFIED dd 175075)   */
        goto TRANSFER;                     /* 057516 5SCIN SET -> skip init     */

    /* ================= INIT block (5SCIN CLEAR) ===================== */
    if (X->SCOCW & already_inquiry_bit)    /* 057517-520 */
        goto TRANSFER;                     /* 057521 */
    X->ABFUN = (X->SCOCW & bit100) ? 42 : 36;   /* 057522-530 pick init read fn */

init_read:                                 /* 057531 SXONO */
    /* CALL SCSID (init read) - 3-way skip return                        */
    int r = SCSID(X);                      /* 057532 JPL SCSID (ptr 062217)     */
    if (r == RET_FAR_ERR)  goto ERREX;     /* 057533 return+1 -> 057636         */
    if (r == RET_FAR_ALT)  goto far_alt;   /* 057534 return+2                   */

    /* return+3 (057535): status check on the init read */
    word D = X->HSTAT & ~BIT15;            /* 057535-536  WHILE D := A BZERO 17 */
    if (D == UNIT_ATTN /*6*/ ||            /* 057537-541 */
        X->HSTAT == ABORTED /*13*/) {      /* 057542-544 */
        X->TACOU--;                        /* 057545 MIN ,X -15                 */
        goto init_read;                    /* 057546 GO RETRY (re-issue init)   */
    }
    if (D != 0)                            /* 057547-550 SKP IF 1 GRE D         */
        goto FINEX_or_term;                /* 057551 -> 057625 -> 057655        */

    /* ---------- DEVICE-TYPE GATE (VERIFIED dd, every word) ---------- */
    word devtype = X->SUTYP >> 8;          /* 057552-553 (dd 044423 / 156570)   */
    if (devtype == 0)                      /* 057554 JAZ  (dd 131003)           */
        goto TRANSFER;                     /* 0 == DISK -> proceed to READ      */
    T = TYPER;                             /* 057555 SAT 1 (dd 171001)          */
    goto ERREX;                            /* 057556 JMP I 54 -> 057632 (dd 125054) */

    /* ================= TRANSFER (real control-record READ) ========= */
TRANSFER:                                  /* 057557 */
    build_read6_cdb_last_lba(X);           /* 057557-604 : ABP32:=1, LBA:=last  */

real_read:                                 /* 057606 */
    int r2 = SCSID(X);                     /* 057610 JPL SCSID (VERIFIED dd 135021) */
    if (r2 == RET_FAR_ERR) goto ERREX;     /* 057611 return+1 -> 057632         */
    if (r2 == RET_FAR_ALT) goto far_alt;   /* 057612 return+2                   */

    /* return+3 (057613): status check on the transfer read */
    word D2 = X->HSTAT & ~BIT15;           /* 057613-614 */
    if (D2 == UNIT_ATTN || X->HSTAT == ABORTED) {  /* 057615-622 */
        X->TACOU--;                        /* 057623 */
        goto RETRY;                        /* 057624 JMP -110 -> 057514 (OUTER WHILE) */
    }
    goto FINEX;                            /* 057625 -> 057655 */

FINEX_or_term:                             /* 057625 */
    goto FINEX;

    /* ---- ERREX far handler : bus reset / power fail retry vs terminate ---- */
ERREX:                                     /* 057636 (via ptr 057632) */
    if (X->HSTAT == 050 /*SBRST*/ ||       /* 057636-640 */
        X->HSTAT == 043 /*LIRST*/ ||       /* 057641-643 */
        X->HSTAT == 051 /*PFAIL*/) {       /* 057644-645 */
        X->TACOU--;                        /* 057647 */
        goto RETRY_hard;                   /* 057650 -> 060005 */
    }
    return RETOP(X, T);                    /* 057651-653 terminate with T code  */
}


/* ============================================================================
 * FINEX  (057655B)  -  control-record checksum + geometry publish + T:=0
 * ============================================================================ */
int FINEX(SCSIUNIT *X)
{
    word npart = ctrl_record_hdr >> 8;     /* 057673-675 */
    word xorsum = 0;
    for (i = 0; i < ctrl_len; i++)         /* 057701-706 */
        xorsum ^= ctrl_record[i];          /* 057703 REXO */

    if (xorsum != 0)   { T = NOCRC; goto term_nocrc; }  /* 057707-710 */
    if (npart <= 2)    { T = NOCRC; goto term_nocrc; }  /* 057711-713 SAT 2 */
    if (npart > 010 /*NCOPA*/) { T = NOCRC; goto term_nocrc; } /* 057714-716 SAT 12 */

    /* control record valid -> publish geometry into caller DMA buffer */
    movew_partition_table_to_caller();     /* 057721-736 */
    caller_buf[0]      = UHLIM_dataarea;   /* 057737-741 STDTX */
    caller_buf[status] = 036;              /* 057743-744 status word = 36       */
    T = OK;                                /* 057747 (VERIFIED dd 146106) */
    return RETOP(X, T);                    /* 057750-755 */

term_nocrc:                                /* 057717 */
    return RETOP(X, T);                    /* 057720 -> 060010 */
}


/* ============================================================================
 * SCSID  (062217B)  -  SCSI driver-command dispatch by function code.
 * Called from every "CALL SCSID" in SCSDISK/RETRY (ptr 057631/062217).
 * ============================================================================ */
int SCSID(SCSIUNIT *X)
{
    X->HSTAT = A;                          /* 062217 (VERIFIED dd 006010) */
    save_link();                           /* 062220-225 */
    int fn = X->ABFUN & 077;               /* 062226-227 */

    if (fn == 037)                         /* 062231-232 */
        return DOEXS(X);                   /* 062234 JPL (ptr 062406=063460)    */

    if (X->SUTHS != 0) {                   /* 062243-244 device busy? */
        queue_op_and_wait_SCWTI(X);        /* 062245-260 */
        return QUEUED;                     /* 062261 */
    }
    latch_op_params(X);                    /* 062262-277 */

    /* ---- REPEAT: dispatch by function (VERIFIED) ---- */
    switch (X->ABFUN & 077) {              /* 062300-301 */
    case 031: return BDRST(X);             /* 062302-304  bus/device reset (ptr 063522) */
    case 074: GUSCB(X);                    /* 062305-310  get SCSI bus (ptr 063431) */
              return EXCOM(X);             /* 062311 -> 062327 */
    case 042:                              /* 062312 */
    case 036: return INQUI(X);             /* 062315-317  read-format/layout (ptr 062613) */
    case 023:                              /* 062320 */
    case 025: return MODES(X);             /* 062323-325  mode select/sense (ptr 063244) */
    default:  /* fn 0 read, 1 write, 4 seek, ... : REAL TRANSFER */
        CACOB(X);                          /* 062326 build CDB (ptr 063750) (dd 135066) */
        return EXCOM(X);                   /* 062327 execute -> SCLLD (ptr 063403) */
    }
}


/* ============================================================================
 * INQUI  (062613B)  -  issue INQUIRY (+READ CAPACITY), build SUTYP, set 5SCIN.
 * THE SUTYP high-byte (device type) is constructed here (VERIFIED bytes).
 * ============================================================================ */
int INQUI(SCSIUNIT *X)
{
    /* 062613-616: re-initialise SUTYP base bits */
    X->SUTYP = (X->SUTYP & mask_m2) | preset_p76;   /* 062613-616 */

    build_inquiry_cdb(X);                  /* 062617-631 */
    int r = exec_command(X);               /* 062632 JPL execute (INQUIRY)      */
    if (r == RET_FAR_ERR) goto far;        /* 062633-634 */

    /* 062636-642: command status gate */
    if ((X->cmd_status & 060) != 0)        /* 062640-641 */
        goto retry_or_term;                /* 062642 -> 062721 */

    copy_inquiry_bytes(X);                  /* 062645-646 EXR computed MOVE     */

    /* ---- SUTYP HIGH byte := INQUIRY device type (byte0) ---- */
    /* response word (device type packed in HIGH byte) at (buf_base + 70).      */
    word resp = X->dma_buf[070];           /* 062647-652 LDATX (VERIFIED dd 143300) */
    resp |= low_mask_p50;                  /* 062653 ORA 50 (low byte only)     */
    X->SUTYP &= resp;                      /* 062654-655 SUTYP_hi := preset_hi & devtype (dd 004423) */

    word devtype = X->SUTYP >> 8;          /* 062656 */
    if (devtype == 0) {                    /* 062657 JAZ -> disk */
        X->SUTYP |= BIT6;                  /* 062666-670 set removable/valid    */
    } else {
        /* 062660-665: classify non-disk (WORM=3 / CD=4 / ...) -> later TYPER   */
    }

    read_capacity_into_UHLIM(X);           /* 062672 -> 062730..063100 : cap->UHLIM */

    /* ---- INQUI success tail : mark init done ---- */
    X->SUTYP |= BIT7_5SCIN;                /* 063101-103 SET 5SCIN (VERIFIED dd 174275) */
    return OK;                             /* 063104-... return to caller       */

far:
retry_or_term:
    return handle_inquiry_error(X);        /* 062716 / 062721 */
}


/* ============================================================================
 * CACOB (063750B)  -  build CDB ; the fn-0 re-entry 5SCIN gate.
 * ============================================================================ */
void CACOB(SCSIUNIT *X)
{
    save_link();                           /* 063750-751 */
    if (!(X->SUTYP & BIT7_5SCIN))          /* 063752-753 (VERIFIED dd 044423)   */
        return (void)INQUI(X);             /* 063754 5SCIN CLEAR -> divert to INQUI */
    build_read6_or_write6_cdb(X);          /* 5SCIN SET -> build transfer CDB   */
    /* then EXCOM (063403) -> GO SCLLD (ptr 063453 = 067160) -> INITO -> SCWAQ  */
}

/* ============================================================================
 * DISPATCH SUMMARY (fn = ABFUN & 077):
 *   0  READ    -> default -> CACOB/EXCOM -> SCLLD   (real DMA transfer)
 *   1  WRITE   -> default -> CACOB/EXCOM -> SCLLD
 *   4  SEEK    -> default -> CACOB/EXCOM -> SCLLD
 *   23 MODE SELECT / 25 MODE SENSE -> MODES  (063244)
 *   31 BUS/DEVICE RESET            -> BDRST  (063522)
 *   36 READ LAYOUT  -> SCSDISK pre-handles (057445); in SCSID -> INQUI (062613)
 *   37 DOEXS (execute-special)     -> DOEXS  (063460)
 *   42 READ FORMAT  -> clears 5SCIN (057506) then INQUI (062613)
 *   74 GET SCSI BUS (GUSCB 063431) -> then EXCOM
 * Function code 75 was NOT found in the SCSID REPEAT dispatch (OPEN).
 * ============================================================================ */
