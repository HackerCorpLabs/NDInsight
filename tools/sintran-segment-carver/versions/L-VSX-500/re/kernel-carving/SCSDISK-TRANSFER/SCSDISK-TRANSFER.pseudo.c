/* ==========================================================================
 * SCSDISK-TRANSFER.pseudo.c
 * --------------------------------------------------------------------------
 * Readable pseudo-C for the SINTRAN III L07 (L-VSX-500) SCSI disk transfer
 * path: how a function-0 (READ) request for block 0 flows from the
 * device-agnostic filesystem's ",X 14" transfer dispatch, through the disk
 * layer's geometry bounds check, into the driver-command dispatch, and out to
 * the driver enqueue SCLLD -- or is rejected first.
 *
 * This is derived from BYTE-VERIFIED L07 disassembly (see SCSDISK-TRANSFER.ASM)
 * plus NPL logic (IP-P2-SCSI-DISK.NPL, a DIFFERENT revision, used for naming).
 * All addresses in comments are OCTAL, from segment 065-S3SIPIT (base 32000B).
 *
 * Register/struct notes:
 *   X  = DAQ (abstract request): .ABFUN (fn+unit), .ABPA2 (disk block addr,
 *        double), .ABP32 (amount), .MEMAD (DMA buffer), .SCOCW (op control),
 *        .DQOPC (operation-type word from OPTYP[]), .SLINK.
 *   B  = disk/unit datafield: .UHLIM (last valid block+1), .SUTYP (offset 23,
 *        bit 7 = 5SCIN "inquiry/geometry init done"), .SLINK/.SUNOP queue.
 *   T  = return/status code (0 = OK, 5 = BADPA, ...).
 * ========================================================================== */

/* ---- disk-layer function codes (subset) ---- */
enum { FN_READ = 0, FN_WRITE = 1, FN_SEEK = 4, FN_READ_FORMAT = 42,
       FN_READ_LAYOUT = 36 };

/* op-type control-word (DQOPC) flag bits, from OPTYP[] */
#define B_3SF42  (1<<10)   /* function 42                */
#define B_3WRIT  (1<<11)   /* write operation            */
#define B_3PART  (1<<12)   /* partition access           */
#define B_3SPES  (1<<13)   /* special (unsorted) op      */
#define B_3SNTR  (1<<16)   /* neutral operation          */

#define BIT_5SCIN  (1<<7)  /* SUTYP bit 7: inquiry/geometry initialisation done */


/* ==========================================================================
 * SCSDISK  (SCSDI = 057215B)  -- the ",X 14" per-device transfer primitive.
 * Reached from RCBLO's device transfer dispatch (JPL I ,B 10) for a SCSI unit.
 * Returns via the operation-terminate path (RETOP -> SWT11).
 * ========================================================================== */
void SCSDISK(DAQ *X, DiskDF *B, int L /*return addr*/)
{
    X->NFUNC = L;                       /* 057215: save return address        */
    DIALO();                            /* 057217: MON PERFO disk utilisation  */

    int fn   = X->ABFUN & 077;          /* 057221..057224                      */
    int dqopc = OPTYP[fn];              /* operation-type control word         */
    if (dqopc == 0) { X->T = ILAOP; goto ERR1; }   /* illegal operation       */

    /* promote ABPA2/ABPA3 to double disk-address / double amount as needed;   */
    /* generate the per-LUN unit datafield PUNDF(); NOLUN if none.             */

    if (!(dqopc & B_3SPES)) {           /* 057676: normal (sortable) transfer  */
        if (dqopc & B_3PART) {
            /* -------- partition path (NPL 056704+) --------
             * validate partition index, read/write access bit, add partition
             * lower limit, then COMPD(addr+amount, ABPA3-partition-length):
             *   if past end -> goto ERR2 (BADPA).                              */
        } else {
            /* -------- WHOLE-DISK geometry bounds check (057352..057362) ----
             * The ONLY pre-transfer reject for a plain read.                   */
            uint32_t end = X->ABPA2 + X->ABP32;   /* 057352..057355 addr+amt   */
            if (COMPD(end, B->UHLIM) > 0)         /* 057356..057361            */
                goto ERR2;                        /* 057362: past end -> BADPA */
            /* block 0: ABPA2 == 0, ABP32 == 1  ->  end <= UHLIM  ->  NO reject */
        }
    } else {
        X->TYPCO |= SSEEK;              /* mark seekable (special op)          */
    }

    /* -------- disk sorting / start operation (057367..057376) -------- */
    B->SUNOP++;                          /* MIN SUNOP                          */
    if (B->SLINK != 0) { DSORT(X); goto SWT11; }   /* sort into active queue   */
    goto NEWOP;

ERR2:
    B = B->ULINK;                        /* 057377                             */
    X->T = BADPA;                        /* 057401: T := 5   *** REJECT ***    */
ERR1:
    RETOP();                             /* 057402: terminate operation        */
    goto SWT11;

    /* ======================================================================
     * NEWOP / RETRY: the request is now the active operation on this unit.
     * ---------------------------------------------------------------------- */
NEWOP:
    /* ... link X as SLINK, set access owner, special-function sub-handling ...
     * For fn 42 here (NPL 057074 = binary 057472) the code CLEARS 5SCIN so the
     * INQUIRY+READ CAPACITY runs; for fn 0 this block is not taken.           */

    /* -------- 5SCIN lazy-init loop (057515.. / call at 057532) --------
     * If geometry has NOT been learned yet (5SCIN clear) AND the op is not
     * neutral, first issue fn 42 (or 36) to inquire + read capacity.
     * For OUR block-0 read, function 42 already ran and left 5SCIN SET, so
     * this loop is SKIPPED.                                                    */
    if (!(B->SUTYP & BIT_5SCIN) && !(X->SCOCW & B_3SNTR)) {
        X->ABFUN = (dqopc & B_3SF42) ? 42 : 36;
        do { SCSID(X); } while (retry_status());   /* 057532: CALL SCSID       */
        if (B->SUTYP >> 8) { X->T = TYPER; goto ERREX; }
    }

    /* -------- REAL TRANSFER: hand the read to the driver dispatch --------
     * 057605..057612.  For a fn-0 read this is the block-0 enqueue call.       */
    int drvfn = X->SCOCW & 0377;         /* 057606..057607                     */
    SCSID(X /*, drvfn */);               /* 057610: CALL SCSID -> 062217        */
    goto FINEX;                          /* 057226 region -> RETEX -> RETOP     */

SWT11: return;                           /* SWT11 = 056516                     */
}


/* ==========================================================================
 * SCSID  (062217B)  -- SCSI driver-command dispatch.
 * Builds and issues one SCSI command. Performs NO UHLIM / geometry check.
 * ========================================================================== */
void SCSID(DAQ *X /* T=param area, X=devDF, B=lunDF, A=drvfn */)
{
    X->HSTAT  = A_in;                    /* 062217                             */
    X->FINISH = L_in;
    X->SCUDF  = B_in;
    X->SCTRG  = T_in;
    int fn = X->ABFUN & 077;             /* 062226..062227                     */

    if (fn == 37) { DOEXS(); return; }   /* 062231..062234 read ext status     */

    if (X->SUTHS != 0) {                 /* 062243: driver busy                */
        queue_request(X);                /* link into wait queue               */
        goto SCWTI;                      /* return to interrupt handler        */
    }

NEWOP_drv:
    /* copy ABFUN/MEMAD/ABPA2/ABPA3 into the driver work area */

REPEAT:                                  /* 062300: dispatch by function       */
    switch (X->ABFUN & 077) {
        case 31:  goto BDRST;                        /* 062302..062304        */
        case 74:  GUSCB();  break;                   /* 062305..062310        */
        case 42:                                     /* 062312                */
        case 36:  goto INQUI;                        /* 062315..062317        */
        case 23:                                     /* 062320                */
        case 25:  goto MODES;                        /* 062323..062325        */
        default:                                     /* fn 0 READ falls here  */
            CACOB(X);                    /* 062326: build SCSI command block    */
            break;
    }
    EXCOM(X);                            /* 062327: execute -> GO SCLLD         */
    SCEIO();                             /* 062330                             */
    goto IMRET;                          /* 062331                             */
BDRST: /* bus device reset ... */ ;
SCWTI: return;
INQUI: /* auto INQUIRY + READ CAPACITY; on success SETS 5SCIN at 063102 */ ;
MODES: /* mode sense/select ... */ ;
IMRET: return;
}


/* ==========================================================================
 * CACOB  (063750B)  -- build the SCSI command block.
 * Contains the 5SCIN re-entry gate at 063752B.
 * ========================================================================== */
void CACOB(DAQ *X /* B = unit datafield */)
{
    /* 063752..063754: THE 5SCIN GATE.
     * With 5SCIN SET (our block-0 read) this does NOT divert - it builds the
     * READ(6) CDB. With 5SCIN CLEAR it would jump to INQUI first (never a skip).*/
    if (!(B->SUTYP & BIT_5SCIN))         /* 063752 LDA SUTYP / 063753 BSKP ONE  */
        goto INQUI;                      /* 063754 JMP I -> 064137              */

    /* 5SCIN SET: build the command.                                            */
    X->SUIB1 = X->SUIB2 = 0;             /* 063755..063756 zero bytecount       */
    int devtype_da = (B->SUTYP >> 3) & 1;/* 063757..063760 5SCDA -> K           */
    int cmdidx     = X->ABFUN & 077;     /* 063761..063762                      */
    int opword     = devtype_da ? SCSF1[cmdidx] : SCSF2[cmdidx];
    /* SCSF1[FN_READ] = 010\340  ->  SCSI opcode 0x08 = READ(6).                */
    build_cdb_into(SMBP1, SMBP2, opword, X);   /* 6/10-byte READ CDB           */
    return;                              /* GO HOME                            */
INQUI: /* start inquiry (does NOT apply for our SET 5SCIN case) */ ;
}


/* ==========================================================================
 * EXCOM / FELLS  (063403B)  -- issue the built command to the driver.
 * Tail (FELLS): X = SUDDF; GO SCLLD  --> the DRIVER ENQUEUE.
 * ========================================================================== */
void EXCOM(DAQ *X)
{
    /* set SUCM1/SUCM2 = physical command address, poke LUN into command byte 0 */
    B = X->SUDDF;
    goto SCLLD;                          /* 063430 JMP I -> 063453 (= 067160)   */
SCLLD:
    /* SCLLD 067160B (IP-P2-SCSI-DRIV): the driver enqueue.
     * SCLLD -> INITO -> SCWAQ  (carved in ../SCSI-DRIVER/).                     */
    SCLLD_driver_enqueue(X);
}


/* ==========================================================================
 * THE ANSWER (from bytes)
 * --------------------------------------------------------------------------
 * A function-0 READ request for block 0, with 5SCIN SET (the state function 42
 * leaves), REACHES SCLLD in the static code:
 *
 *   ",X 14" -> SCSDISK(057215)
 *     UHLIM check (057356..057362): block 0 addr=0 is WITHIN bounds, so it does
 *       NOT branch to ERR2 -> the T=5 (BADPA) reject at 057401 is NOT taken.
 *     -> CALL SCSID (057610 -> 062217)
 *       REPEAT (062300): fn 0 falls through -> CALL CACOB (062326)
 *         CACOB 5SCIN gate (063752): 5SCIN SET -> builds READ(6) CDB (no divert)
 *       -> CALL EXCOM (062327) -> GO SCLLD (063430 -> 067160)
 *
 * The single pre-transfer reject (BADPA at 057401) fires only when
 * address+amount exceeds UHLIM, which block 0 cannot do. Therefore:
 *   - Case 2 (block-0 request issued but rejected before SCLLD): REFUTED.
 *   - Case 1 (the device-agnostic caller never issues the block-0 request):
 *     remaining explanation. Matches the FUNCTION-42-RETURN / RCBLO /
 *     ENTER-DIRECTORY conclusions and the live trace (SCWAQ empty).
 * ========================================================================== */
