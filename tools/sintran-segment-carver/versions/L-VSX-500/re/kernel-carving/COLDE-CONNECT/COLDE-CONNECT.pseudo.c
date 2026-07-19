/* =========================================================================
 * COLDE-CONNECT.pseudo.c
 * Readable pseudo-C of the @ENTER-DIRECTORY cold-enter/connect flow.
 * Reconstructed from byte-verified 006-S3FS (L07) disassembly.
 * All addresses OCTAL with trailing B. This is LOGIC, not literal codegen.
 *
 * KEY CONCLUSION: the block-0-read decision is NOT taken in this segment.
 * FILSYS issues the block-0 read exactly once and never retries; whether the
 * SCSI driver services it as a fn-0 read or diverts it to fn-42 init is
 * decided in the SCSI disk driver (065-S3SIPIT), out of this segment.
 * ========================================================================= */

/* --- device / directory descriptor fields referenced (offsets in WORDS) --- */
/*  desc[14] = ABFUN / per-device transfer-routine vector (used by RCBLO)     */
/*  desc[15] = MEMA1, desc[16] = MEMA2  (DMA address)                         */
/*  desc[23] = SUTYP  (bit-7 5SCIN = SCSI init-done)                          */

/* -------------------------------------------------------------------------
 * ENDIR (140176B) -- @ENTER-DIRECTORY top level.
 * Order of the device-relevant calls is fixed by the bytes:
 *   COLDE (140213B)  ->  GDIRE (140222B)  ->  CHDSI (140402B)
 * ------------------------------------------------------------------------- */
int ENDIR(dir_spec spec)
{
    enter_setup();                 /* 140202B  resident 003752                */
    CLPAR();                       /* 140211B  clear parameter block          */

    if (COLDE(spec) != OK)         /* 140213B  cold-enter (name lookup only)  */
        return error;              /* 140214B  error arm                      */

    /* 140215B..: directory-object bookkeeping */
    if (GDIRE(spec) != OK)         /* 140222B  get directory entry            */
        return error;

    PRSRV();                       /* 140252B  MON 124B  (the ONE MON call;   */
                                   /*          JAP at 140253B => not an abort) */

    /* ... more bookkeeping ... */

    if (CHDSI(spec) != OK)         /* 140402B  <== reads directory block 0    */
        return error;              /* 140403B  error arm                      */

    return OK;
}

/* -------------------------------------------------------------------------
 * COLDE / DCOLD / XCOLD (132072B / 132070B / 132060B)
 * cold-enter directory.  Read/write variant select via SSK, exactly the
 * RDISK/WDISK idiom.  Body is a directory-tree / name-match walk.
 * IMPORTANT: this routine issues NO device transfer.  Its entire call set is
 * directory/name helpers: {003752, 004735 (resident), CLPAR, GDIRT, GNAMI,
 * GNAMA, GNAMT}.  There is no RCBLO / RXDIR / driver dispatch here.
 * ------------------------------------------------------------------------- */
int DCOLD(dir_spec s){ SSK = 1; return COLDE_body(s,/*write=*/1); }  /* 132070B */
int COLDE(dir_spec s){ SSK = 0; return COLDE_body(s,/*write=*/0); }  /* 132072B */

int COLDE_body(dir_spec s, int write_flag)
{
    enter_setup();                 /* 132077B  resident 003752                */
    local17 = SSK ? 1 : 0;         /* 132100B  read/write flag latch          */
    CLPAR();                       /* 132112B                                 */

    for (slot = 0; /* dir slots */; slot++) {   /* 132133B loop              */
        e = GDIRT(slot);           /* 132141B / 132151B  get directory entry  */
        /* name compare against requested directory name ... */
        if (name_match) break;     /* 132135B / 132145B                       */
    }
    GNAMT(...);                    /* 132171B  name table                     */
    /* 132401B sub-block: */
    helper_004735();               /* 132402B  resident helper                */
    GNAMI(...);                    /* 132403B  name init                      */
    GNAMA(...);                    /* 132406B  get name                       */
    /* ... returns matched directory descriptor; NO block I/O performed ...   */
    return OK;
}

/* -------------------------------------------------------------------------
 * CHDSI (037763B) -- check disk info: THIS is where block 0 is read.
 * ------------------------------------------------------------------------- */
int CHDSI(dir_spec s)
{
    int r = enter_setup();         /* 037767B  resident 003752                */
    if (r & BIT6)                  /* 037773B  BSKP ZRO 100 DA                */
        return error;              /* 037774B                                 */

    RXDIR();                       /* 040000B  <== READ DIRECTORY BLOCK 0     */

    /* 040015B..: validate the directory header just read into the buffer */
    if (buf[0] != expected_header  /* 040017B  SKP IF DA EQL ST               */
        || buf[0] == 0) {          /* 040021B  JAZ                            */
        goto reinit_descriptor;    /* 040063B  -- NOTE: does NOT re-issue     */
                                   /*             RXDIR.  No read retry.      */
    }
    return OK;

reinit_descriptor:                 /* 040063B */
    /* zero + re-init the descriptor, then fall through to error status.      */
    return error;
}

/* -------------------------------------------------------------------------
 * RXDIR (037643B) -- read directory block 0.  Sets block:=0 then RCBLO.
 * ------------------------------------------------------------------------- */
void RXDIR(void)
{
    enter_setup();                 /* 037647B  resident 003752                */
    unsigned block = 0;            /* 037650B/037651B  RADD CLD 0 DA / DD     */
    RCBLO(block);                  /* 037652B  <== issue the transfer         */
}

/* -------------------------------------------------------------------------
 * RCBLO (035766B) -- build + dispatch the single device transfer.
 * The driver is called indirectly through desc[14].  Exactly one dispatch,
 * NO retry loop.  On SMD this dispatch reads block 0.  On SCSI the driver,
 * if desc.SUTYP.5SCIN is CLEAR, performs fn-42 (INQUIRY/READ-CAPACITY/
 * control-record) instead -- the divert that loses the read -- and that
 * decision lives in 065-S3SIPIT, not here.
 * ------------------------------------------------------------------------- */
int RCBLO(unsigned block)
{
    enter_setup();                 /* 035772B  resident 003752                */
    rdpage();                      /* 035773B  resident 037101 (buffer/page)  */
    desc = GDIRT();                /* 035777B  descriptor -> X                */
    /* ... 036100B: */
    void (*driver)() = desc[14];   /* 036101B/036102B  driver vector / ABFUN  */
    /* build DMA addr (desc[15]/[16]), block, seek; select variant branch ... */

    driver();                      /* 036135B  JPL I ,B 10  <== ONE dispatch  */
                                   /* 036136B  on success: return, no re-issue*/
    /* alternate variant dispatch sites: 036142B / 036167B / 036174B,         */
    /* mutually exclusive (read / write / seek).                              */
    return status;                 /* FILSYS trusts the driver's status.      */
}

/* =========================================================================
 * >>> THE BLOCK-0-ISSUE DECISION (marked function) <<<
 *
 * There is NO such decision in 006-S3FS (FILSYS).  The read is issued
 * unconditionally by CHDSI->RXDIR->RCBLO; the only guard is CHDSI's
 * setup-result bit test (037773B), which is about resource reservation, not
 * device type / SUTYP / 5SCIN.  The read-vs-init fork is inside the driver:
 *
 *   // located in 065-S3SIPIT (SCSI disk layer, NPL-label + 376B), NOT carved here
 *   void scsi_disk_transfer(desc, block, fn) {
 *       if (!(desc.SUTYP & 5SCIN)) {        // <-- THE decisive test
 *           do_function_42_connect(desc);   // INQUIRY/READ-CAPACITY/ctrl-rec
 *           desc.SUTYP |= 5SCIN;            // (FUNCTION-42-RETURN carve: success)
 *           // OPEN: does it FALL THROUGH to the fn-0 read, or RETURN here?
 *           //  - fall through  => block 0 would be read (mount works)
 *           //  - return        => block 0 read is LOST (observed bug)
 *       }
 *       enqueue_read(desc, block, fn);      // SCSDISK-TRANSFER carve: works
 *   }
 * ========================================================================= */
