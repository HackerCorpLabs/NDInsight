/* ==========================================================================
 * ENTER-DIRECTORY (directory mount) - readable pseudo-C for the WHOLE flow
 * SINTRAN III VSX/500  L07   segment 006-S3FS  load base 26000B
 *
 * Reconstructed from the carved bytes (ENTER-DIRECTORY.ASM + ../RCBLO/*).
 * Octal literals shown with a leading 0. This is the DEVICE-AGNOSTIC
 * filesystem side of the mount, down to the per-device transfer hand-off.
 *
 * Command:  @ENTER-DIRECTORY <dir>,<device>,<unit>[,<F|R>][,<subunit>]
 *
 * TOP-LEVEL CALL GRAPH (all VERIFIED edges except the final ,X 14 hop):
 *   ENDIR 0140176
 *     -> GDIRA 030225        get directory datafield (leaf)
 *     -> MON 124 (PRSRV)     reserve the mass-storage unit    [MCTAB[124]=037076]
 *     -> CHDSI 037763        check/enter directory
 *          -> RXDIR 037643   read page-0 ext-info via cache
 *               -> RCBLO 035766  reserve/read cache block 0
 *                    -> (*df->transfer)()   JPL I ,B 10 via datafield ,X 14
 *                       == THE DEVICE HAND-OFF  (SCSDISK for SCSI)
 *          -> GSIZE 037101   in-core disk size (no I/O)
 *          -> WXDIR 037702   recompute checksum + write page 0 back
 * ==========================================================================*/

/* ---- on-disk page-0 extended-info block (8 words at page word 021) -------- */
struct extinfo {
    u16 checksum;      /* word 0  additive 16-bit sum of words 1..7            */
    u16 w1, w2, w3;    /* words 1-3                                            */
    u16 flag;          /* word 4  bit15 = "entered"                           */
    u16 owner;         /* word 5  entering system number                      */
    u32 capacity;      /* words 6-7  pages available (32-bit)                  */
};

/* ---- the per-unit device datafield (built upstream; key fields) ---------- */
struct unit_df {
    u16 word0;         /* ,X 0  device flags/type (bit 150 = tape)            */
    u16 word3;         /* ,X 3                                                */
    u16 reserve_desc;  /* ,X 4  0 => no reserve needed                        */
    /* ...                                                                     */
    fnptr transfer;    /* ,X 14 == the per-device transfer primitive.
                        *          SCSI: SCSDISK ; SMD/Winch/floppy: their own
                        *          driver entry. THIS pointer is where SCSI and
                        *          the working device types diverge.          */
    u16 sutyp;         /* ,X 23 sub-unit type                                 */
    /* ...                                                                     */
};


/* ==========================================================================
 * GDIRA 030225 - directory index -> datafield base address (leaf)
 * ==========================================================================*/
struct unit_df *GDIRA(int dir_index)
{
    return (struct unit_df *)(DIR_TABLE_BASE + dir_index * 4); /* MPY 4 + base */
}


/* ==========================================================================
 * ENDIR 0140176 - the ENTER-DIRECTORY worker (top level)
 *   dir_index      : which directory slot (resolved by the command interpreter)
 *   entering_system: the system number that will own the mount (-> CHDSI arg)
 * ==========================================================================*/
int ENDIR(int dir_index, int part_subunit, int entering_system)
{
    /* --- Stage 2: get the directory datafield and reserve the unit -------- */
    struct unit_df *df = GDIRA(dir_index);          /* 0140244 */

    if (df->reserve_desc != 0) {                    /* 0140250-0140251 */
        /* MON 124 = ForceReserve (PRSRV). Worker MCTAB[124]=037076B.
         * Reserves the physical mass-storage unit BEFORE any transfer.        */
        if (MON_124_ForceReserve(df) < 0)           /* 0140252 */
            return ERR_147;   /* "device unit reserved for special use" 0140254 */
    }
    if (df->word0 & BIT_TAPE /*150*/)               /* 0140256-0140257 */
        return ERR_145;       /* "illegal on tape device" 0140261 */

    /* --- parse subunit/part flags, guard directory-table ordering --------- */
    set_part_flags(df, part_subunit);               /* 0140275-0140324 ISETP/ICLEP */
    if (main_dir_not_last_released(df))             /* 0140314 */
        return ERR_42;        /* "main directory not last one released" 0140315 */

    /* --- match the requested name/abbrev against the on-unit directory ----- */
    if (name_already_entered(df))                   /* 0140326-0140370 */
        return ERR_32;        /* "directory entered" 0140370 */

    /* --- Stage 3-5: read page 0, validate, stamp, write back -------------- */
    int rc = CHDSI(df, entering_system);            /* 0140402 */
    if (rc != OK)                                   /* 0140403 error return */
        return rc;

    /* --- Stage 6: OK return - directory is now ENTERED --------------------
     * On-disk state was set by CHDSI/WXDIR (flag bit15 + owner word).
     * In-core directory-table bookkeeping continues at 0140436+ (partial). */
    mark_directory_live_in_core(df, dir_index);     /* 0140436+ (OPEN, partial) */
    return OK;                                       /* 0140404 -> 0140436 */
}


/* ==========================================================================
 * CHDSI 037763 - check / enter directory (read page 0, validate, stamp)
 * ==========================================================================*/
int CHDSI(struct unit_df *df, int entering_system)
{
    struct extinfo ei;

    /* --- Stage 3: read the page-0 extended-info block --------------------- */
    if (RXDIR(df, &ei) != OK)                       /* 037763 -> 040000 */
        return ERR_MASTER_TRANSFER; /* 035B; RXDIR error -> 040135 */

    /* --- Stage 4a: verify the additive 16-bit checksum -------------------- */
    u16 sum = 0;
    for (int i = 1; i < 8; i++)  sum += ((u16 *)&ei)[i]; /* 040002-040014 */

    if (sum != ei.checksum || sum == 0) {           /* 040017-040021 */
        /* --- Stage 4b: SELF-HEAL, do NOT reject -------------------------- *
         * A bad or zero checksum zero-fills the 8 words and writes the
         * geometry-derived capacity, then falls through to stamp+writeback.
         * Consequence: a garbage page-0 read does NOT raise a checksum
         * mount error - it silently triggers a WRITE-back.                  */
        memzero(&ei, 8);                            /* 040063-040076 */
        ei.capacity = GSIZE(df);                    /* 040077 (in-core size) */
    } else {
        /* good checksum: capacity sanity vs in-core geometry (GSIZE, no I/O)*/
        if (ei.capacity != GSIZE(df))               /* 040023-040032 */
            /* incompatible sizes handled per device layer */ ;
    }

    /* --- Stage 4c: cross-system owner interlock -------------------------- */
    if ((ei.flag & BIT15) && ei.owner != 0 &&       /* 040110-040116 */
        ei.owner != entering_system)
        return ERR_ENTERED_OTHER; /* reject 040117 (INFERRED code 032B/034B) */

    /* --- Stage 5: stamp owner + "entered" flag, write back --------------- */
    ei.owner = entering_system;                     /* 040121-040122 (word 5) */
    ei.flag |= BIT15;                               /* 040123-040125 (word 4) */
    if (WXDIR(df, &ei) != OK)                        /* 040127 */
        return ERR_MASTER_TRANSFER; /* 035B write-back failed */
    return OK;
}


/* ==========================================================================
 * RXDIR 037643 - read page-0 extended-info via the buffer cache
 * ==========================================================================*/
int RXDIR(struct unit_df *df, struct extinfo *out)
{
    /* block number = 0 (RADD CLD 0 DD at 037651): page 0 is the read target. */
    struct cache_buf *b = RCBLO(/*block=*/0, df);   /* 037651-037652 */
    if (b == NULL)                                  /* 037653 error */
        return ERROR;

    copy_words(out, &b->page[021], 8);              /* 037655-037661 (8 words) */
    CL1DB(b);                                        /* 037665 release buffer */
    return OK;
}


/* ==========================================================================
 * WXDIR 037702 - recompute checksum + write the ext-info block back to page 0
 * ==========================================================================*/
int WXDIR(struct unit_df *df, struct extinfo *ei)
{
    u16 sum = 0;
    for (int i = 1; i < 8; i++)  sum += ((u16 *)ei)[i]; /* 037707-037721 */
    ei->checksum = sum;                              /* 037723 */

    struct cache_buf *b = RCBLO_writer(/*block=*/0, df); /* 037727-037730 */
    if (b == NULL) return ERR_35; /* "master block transfer error" 037747 */

    copy_words(&b->page[023], ei, 8);                /* 037736 */
    return WCBLO(b);   /* 037741 write cache block back through datafield path */
}


/* ==========================================================================
 * ============  THE DEVICE HAND-OFF - carved in ../RCBLO/RCBLO.ASM  =========
 * ==========================================================================
 * This is the SEAM the user's bug lives at. RCBLO (035766) turns a
 * "read page 0" request into a device transfer by dispatching THROUGH the
 * unit datafield's transfer pointer. Reproduced here as the key function:
 *
 *   struct cache_buf *RCBLO(u32 block, struct unit_df *df)
 *   {
 *       ... GSIZE prologue (035773), cache search (036005-036042) ...
 *       if (cache_hit) return b;      // NO device read (036043-036053)
 *
 *       // --- MISS: load the per-device transfer primitive and DISPATCH ---
 *       fnptr drv = df->transfer;     // 036100 LDX ,B 11 ; 036101 LDA ,X 14
 *                                     // 036102 STA ,B 10   (VERIFIED)
 *
 *       int fn = (op & 065) + 066;    // 036124-036130 build function code
 *       // *** the ONLY place the page-0 read is issued to the driver ***
 *       rc = (*drv)(fn, b, block);    // 036135  JPL I ,B 10  (VERIFIED 135410)
 *       ...                           // 036142/036167/036174 alternate fn sites
 *   }
 *
 * HAND-OFF CONTRACT (what the driver is asked to do):
 *   - datafield word ,X 14  = drv    = the per-device transfer primitive.
 *                                      SCSI: SCSDISK. SMD/Winch/floppy differ HERE.
 *   - block number          = 0      = page 0 (set by RXDIR, RADD CLD 0 DD).
 *   - function code (ABFUN)          = (op & 065) + 066, built at 036124-036130
 *                                      from the op-code in local ,B 12. For a
 *                                      page-0 READ this selects the driver's
 *                                      READ function (SCSDISK fn 0).
 *   - DMA target (MEMA1/MEMA2)       = the reserved cache buffer; the driver
 *                                      fills ABFUN/MEMA1/MEMA2 in the DEVICE
 *                                      datafield and derives the LBA + word
 *                                      count from READ CAPACITY. RCBLO does NOT
 *                                      compute the SCSI LBA or byte count.
 *
 * WHY SCSI FAILS (from ../RCBLO/README.md): on the SCSI unit the wire shows
 * INQUIRY -> READ CAPACITY -> one last-block READ(6) (a function-42 control-
 * record read, disk init) and then SILENCE. SCWAQ stays empty => RCBLO's
 * `JPL I ,B 10` for block 0 NEVER executed => the page-0 read was never even
 * enqueued. The abort is upstream of 036135 (connect/init overlay or an early
 * RCBLO exit). A live DAP break at CHDSI/RXDIR/RCBLO settles which. On SMD/
 * Winchester/floppy the same JPL I ,B 10 reaches a driver that DOES enqueue the
 * page-0 read, so the mount completes.
 * ==========================================================================*/
