/* ==========================================================================
 * SINTRAN III L07 - SCSI disk-layer FUNCTION 42 (READ FORMAT) return logic
 * vs SMD/Winchester FUNCTION 42, and the caller branch that (elsewhere)
 * decides whether block 0 is read.
 *
 * SCSI side  : byte-verified from 065-S3SIPIT (load base 32000B),
 *              disk layer at L07 = NPL-label + 376B.
 * SMD side   : INFERRED from IP-P2-DISK-START.NPL (a different, uncarved
 *              segment); marked INFERRED below.
 *
 * Internal disk-layer return codes (carried in T; VERIFIED at ERREX map):
 *   OK=0  TYPER=1  ILAOP=4  BADPA=5  RQSER=6  NOCRC=11(octal)
 * ========================================================================== */

/* Unit datafield fields used below (octal offsets, L07 verified):
 *   SUTYP @23   -- bit 5SCIN(=7) = "inquiry/init done"; high byte = device type
 *   SCOCW       -- control word; bit 3SF42 = "this op is READ FORMAT"
 *   MEMA1/MEMA2 -- caller's DMA buffer address (double)                        */

/* --------------------------------------------------------------------------
 * SCSI FUNCTION 42  (L07 057472 .. 057747)   ==> returns T (the return code)
 * -------------------------------------------------------------------------- */
int scsi_func42(unit *X, request *SLINK)
{
    /* (A) ENTRY 057472-057507 : capture params, FORCE re-inquiry */
    MEMAD = X->MEMAD;
    ABFUN = X->ABFUN;
    X->ABP31 = 0;
    X->ABP32 = 0100;                       /* 100B-block probe span */
    SCDFA->SUTYP &= ~BIT(5SCIN);           /* 057506: CLEAR 5SCIN  -> re-inquire */

retry:                                     /* (B) 057514 */
    /* Automatic INQUIRY + READ CAPACITY when not yet initialised */
    if (!(SUTYP & BIT(5SCIN)) && !(X->SCOCW & BIT(3SNTR))) {
        X->ABFUN = (A & BIT(3SF42)) ? 042 : 036;
        do {
            D = scsid(X);                  /* 057532 CALL SCSID -> INQUI       */
            if (SCSID_ERR) goto errex;     /* unit-attention(6)/aborted(13) -> */
        } while ((D & 017) == 6 || (D & 017) == 013);   /* ... loop w/ TACOU-- */
        if ((SUTYP >> 8) != 0)             /* 057552 device type present but   */
            return (T = TYPER);            /* 057555: illegal type -> T:=1     */
        /* INQUI success path SETS 5SCIN and (for fn42) returns block/rec size:
         *   063102: SUTYP |= BIT(5SCIN);   // INITIALIZATION FINISHED
         *   063104: if ((ABFUN & 077)==042) goto RCAFI;                        */
    }

    /* (C) 057557 : the SINGLE control-record READ(6) at the last LBA */
    if (X->SCOCW & BIT(3SF42)) {           /* this op is READ FORMAT */
        X->ABFUN &= 0177700;               /* INDICATE READ */
        X->ABPA2 = dbl(X->MEMA1,X->MEMA2); /* LBA of control record (last block) */
        X->ABP31 = 0; X->ABP32 = 1;        /* exactly ONE block */
        X->MEMAD += 01000;                 /* advance DMA target */
    }
    D = scsid(X);                          /* 057610 CALL SCSID -> READ(6) */
    if (SCSID_ERR) goto errex;
    while ((D & 017) == 6 || (D & 017) == 013) { X->TACOU++; goto retry; }
    /* fall into FINEX */

    /* (D) FINEX 057655 : validate control record, publish geometry, RETURN */
    if (SLINK->DQOPC & BIT(3SF42)) {       /* was READ FORMAT */
        if ((X->HSTAT & 017) <= 1) {       /* status OK */
            scclr();                        /* clear controller cache */
            NPART = ctrlrec[0] >> 8;
            D = 0;
            for (i = 0; i < 01000; i++)     /* 057702: XOR checksum */
                D ^= ctrlrec[i];
            if (D != 0 || NPART <= 2 || NPART > NCOPA)
                return (T = NOCRC);        /* 057717: T:=11  no control record */

            memcpy(CMAD, partition_table, NPART*6);          /* 057730 MOVPP   */
            SLINK->buf.data_area_size = ctrlrec_size;        /* 057737 *STDTX  */
            SLINK->buf.UHLIM          = geometry;            /* 057742 AD=:UHLIM*/
            SLINK->buf.status         = 036;                 /* 057743 *STATX  */
        }
    }
    return (T = 0);                        /* 057747: OK RETURN  <-- OUR RUN   */

errex:
    /* PFAIL/SBRST/LIRST -> MIN TACOU, goto retry; else T:=20 error to RETEX.
     * BADPA(5)/ILAOP(4)/TYPER(1)/NOCRC(11) surface here and are mapped to user
     * error codes at 062124 (TYPER->61, ILAOP->55, BADPA->174B).              */
    return T;
}

/* --------------------------------------------------------------------------
 * SMD / Winchester FUNCTION 42  (IP-P2-DISK-START.NPL ; INFERRED, different seg)
 * -------------------------------------------------------------------------- */
int smd_func42(unit *X, request *PARDF)
{
    /* NPL 054066: for a non-BDISK transfer type it is a silent no-op */
    if (TRNSF != BDISK)
        return 0;                          /* GO FAR FIN, HSTAT stays 0, no I/O */

    /* NPL 054620 FIN: return the *static* format number - NO disk read at all */
    X_unit = (CTRG >> 6) & 7;
    PARDF->buf[0] = HTABL[X_unit].DISPN;   /* 054632 *DEPO format number */
    /* HSTAT stays 0 (bit 4 not set) -> SUCCESS.
     * Geometry (UHLIM etc.) comes later from the in-core DISC-LAYOUT-TABLE
     * indexed by this format number - never from reading the platter.         */
    return 0;
}

/* --------------------------------------------------------------------------
 * THE CALLER (device-agnostic mount / connect-init)  -- NOT carved here.
 * Source is missing (resident filesystem/connect overlay).  What IS proven:
 *
 *  1. Both func42 variants report SUCCESS on our run:
 *        SCSI -> T = 0            SMD -> HSTAT = 0
 *     So there is NO "different error return" for the caller to branch on.
 *
 *  2. The block-0 (page-0 directory) read is a SEPARATE fn-0 request issued by
 *     ENTER-DIRECTORY -> CHDSI -> RXDIR -> RCBLO -> (,X 14) -> SCSDISK fn 0
 *     (see ../ENTER-DIRECTORY/ and ../RCBLO/).  Function 42 never chains it.
 *
 *  3. When that fn-0 read finally re-enters the disk layer it hits the 5SCIN
 *     gate (063752).  Because func42 left 5SCIN SET, the gate does NOT divert
 *     to INQUI and does NOT block the transfer:
 * ------------------------------------------------------------------------- */
void block0_read_reentry(unit *X)          /* L07 063752 */
{
    if (!(X->SUTYP & BIT(5SCIN)))          /* 5SCIN clear? */
        goto INQUI;                        /* would inquire first - NOT our case */
    /* 5SCIN SET (func42 already initialised) -> proceed straight to transfer:
     *   CACOB -> EXCOM -> SCLLD -> INITO -> SCWAQ  (block 0 gets enqueued)     */
    do_transfer(X);
}

/* ==========================================================================
 * ANSWER (see README section 5):
 *   The hypothesis "SCSI func42 returns a DIFFERENT value and the caller
 *   branches on it to skip block 0" is DISPROVED on the return-value axis:
 *   func42 returns T=0 (success) on our run, the same success sense as SMD,
 *   and leaves 5SCIN SET (the correct state), which the block-0 re-entry reads
 *   and correctly does NOT act on to skip.  Neither the return code nor 5SCIN
 *   causes the skip.  The divergence is in the OUTPUT DATA (SCSI reads UHLIM/
 *   partitions off the disk; SMD hands back a static format number) and how the
 *   uncarved caller consumes it - which static disk-layer bytes cannot decide.
 * ========================================================================== */
