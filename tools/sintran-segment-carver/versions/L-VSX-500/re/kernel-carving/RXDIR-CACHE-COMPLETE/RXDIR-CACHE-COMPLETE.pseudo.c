/* ==========================================================================
 * RXDIR-CACHE-COMPLETE.pseudo.c
 * Readable reconstruction of the SINTRAN III VSX/500 L07 block-0 read path.
 * Segment 006-S3FS, load base 26000B. All addresses octal in the comments.
 *
 * Grades: VERIFIED = matches carved bytes (see .ASM). INFERRED = reasoned.
 * OPEN = crosses into an uncarved resident overlay / runtime binding.
 *
 * This is PSEUDO-code for human reading. It is NOT ND-100 assembly and NOT
 * meant to compile. Skip-return calls (the driver's error/ok convention) are
 * modelled as functions returning a status where the caller branches on it.
 * ========================================================================== */

/* --- shared shapes (INFERRED field layout from the field offsets used) ----- */
typedef struct CacheBuf {
    /*  4 */ int  flags;        /* valid/dirty flags; -1 == invalidated       */
    /*  5 */ int  block_hi;     /* stored block number high / device id slot   */
    /*  6 */ int  block_lo;     /* stored block number low                     */
    /* 12 */ int  device_id;    /* owning device id                            */
    /* 14 */ /* (unit descriptor, not the buffer, carries datafield word 14)   */
    /* 22 */ struct CacheBuf *lru_fwd;   /* LRU forward link                   */
    /* 23 */ struct CacheBuf *lru_back;  /* LRU back link                      */
    int  data[/* full page */];  /* page image; ext-info at word +21B          */
} CacheBuf;

typedef struct UnitDesc {
    /* 14 */ void *transfer_fn;  /* datafield word 14 = driver transfer entry  */
} UnitDesc;                       /* RUNTIME-bound at device config (OPEN)      */


/* ==========================================================================
 * RXDIR = 037643B  - read page-0 extended-info via the buffer cache.
 * Caller: CHDSI (during @ENTER-DIRECTORY mount).
 * ========================================================================== */
int RXDIR(int *dest /* ,B 0 */)
{
    reserve_prologue();                       /* 037647 -> 003752               */

    /* 037650-037651: the requested block number is set to ZERO. VERIFIED.     */
    int block = 0;

    CacheBuf *buf;
    if (RCBLO(block, /*out*/ &buf) != OK)     /* 037652 -> RCBLO 035766         */
        return error_return();                /* 037653 -> 037671 ERROR EXIT    */

    /* 037654-037661: copy the 8-word (10B) extended-info out of the page-0
     * cache buffer (buffer + 21B) into the caller's dest area. VERIFIED.      */
    block_copy(dest, &buf->data[021], /*words=*/010);   /* 001224 helper        */

    /* 037662-037665: we borrowed the buffer only to read it - release it.
     * VERIFIED [037700]=035240 = CL1DB. RXDIR does NOT hold the buffer.        */
    CL1DB(buf);

    return OK;                                /* 037666-037670 OK EXIT          */
}


/* ==========================================================================
 * RCBLO = 035766B  - reserve/read a disk-cache block.
 * op comes from the device-descriptor getter (050124), NOT from the caller,
 * so its concrete value is a RUNTIME input (see ABFUN note below).
 * ========================================================================== */
int RCBLO(int block /* D, ,B 6 */, CacheBuf **out)
{
    reserve_prologue();                       /* 035772 -> 003752               */

    /* 035773: prologue calls GSIZE (in-core disk size, NO device I/O).        */
    if (GSIZE_failed())                       /* VERIFIED [036056]=037101       */
        return error_stub();                  /* 035774 -> 036205 ERROR STUB    */

    /* 035775-036004: fetch the device descriptor, extract the OPERATION code
     * into local ,B 12, and classify it (GNAMA 030235).                       */
    int op;                                   /* ,B 12                          */
    UnitDesc *unit;                           /* ,B 11                          */
    if (get_descriptor(block, &op) != OK)     /* 035777 -> 050124               */
        return error_stub();                  /* 036000 -> 036205               */
    unit = GNAMA(op & 060);                    /* 036004 -> 030235               */

    /* 036011: hash/search the cache for this block. COMPP 036616.             */
    CacheBuf *cand;
    if (COMPP(block, &cand) != OK)            /* 036011 -> 036616               */
        return error_stub();                  /* 036012 -> 036205               */

    G3NWT(cand);                               /* 036020 -> 034371 get-new/wait  */
    cand->flags &= ~(1 << 15);                 /* 036021-036025                  */
    cand->device_id = op_device(op);           /* 036026-036027                  */

    /* 036031-036042: compare requested block vs the candidate's stored block. */
    if (block == cand->block()                 /* hi ,X5 && lo ,X6 && dev match  */
        && cache_hit_clean(cand))              /* 036043-036046                  */
    {
        /* ---- CACHE HIT: block already resident, NO device read. ---------- */
        reserve_wait(cand);                    /* 036050 -> 010506               */
        goto success;                          /* 036053 -> 036144               */
    }

    /* ======================= CACHE MISS ================================== */
    cand->set_block(block);                    /* 036072-036073 stamp block      */
    cand->device_id = op_device(op);           /* 036074-036075                  */
    reserve_wait(cand);                        /* 036077 -> 010506               */

    /* 036100-036102: fetch the driver transfer entry from the unit datafield
     * word 14 and stash it in local 10. VERIFIED. This is the OPEN boundary:
     * transfer_fn is a RUNTIME pointer bound at device-config time; for a
     * SCSI disk it targets SCSDISK -> SCLLD (INITO/SCWAQ), a resident driver
     * segment NOT carved in 006-S3FS.                                         */
    void *transfer_fn = unit->transfer_fn;     /* ,B 10                          */

    /* 036103-036117: geometry/address arithmetic; on out-of-range -> A:=100B  */
    if (geometry_out_of_range(cand, block))    /* 036115 JAP fails               */
        return error_epilogue(/*status=*/0100);/* 036116-036117 -> 036177        */
    if (get_descriptor2() != OK)               /* 036122 -> 050124               */
        return error_epilogue(status);         /* 036123 -> 036177               */

    /* --- the FOUR device-transfer dispatches. Each builds a datafield
     *     function selector "ABFUN" from the operation code and calls the
     *     driver through transfer_fn (JPL I ,B 10). The masks/bases are
     *     VERIFIED; which numeric SCSI op results is INFERRED because op is a
     *     runtime input. See README "ABFUN" and "4 dispatch sites".           */

    /* DISPATCH #1  @036135  ABFUN = (op & 065B) + 066B   (primary READ) */
    if (DEVICE_TRANSFER(transfer_fn, (op & 065) + 066, cand) != OK)
        return error_epilogue(status);         /* 036136 -> 036177               */

    /* DISPATCH #2  @036142  ABFUN = (op & 054B) + 063B */
    if (DEVICE_TRANSFER(transfer_fn, (op & 054) + 063, cand) != OK)
        goto dispatch3;                        /* 036143 -> 036161               */
    goto success;                              /* 036144                         */

dispatch3:
    /* DISPATCH #3  @036167  ABFUN = (op & 032B) + 066B */
    if (DEVICE_TRANSFER(transfer_fn, (op & 032) + 066, cand) != OK)
        return error_epilogue(status);         /* 036170 -> 036177               */

    /* DISPATCH #4  @036174  ABFUN = (op & 022B) + 063B */
    if (DEVICE_TRANSFER(transfer_fn, (op & 022) + 063, cand) != OK)
        return error_epilogue(status);         /* 036175 -> 036177               */
    /* 036176 -> success */

success:                                       /* 036144 convergence (HIT + OK)  */
    link_buffer_into_cache(cand);              /* 036145-036153 -> 011236        */
    *out = cand;                               /* 036154-036156                  */
    return OK;                                 /* 036157-036160 OK EXIT          */

    /* 036177-036204: ERROR EPILOGUE - release the buffer (CL1DB) and return
     * the error up to RXDIR. VERIFIED [036220]=035240 = CL1DB.                */
}


/* ==========================================================================
 * GSIZE = 037101B  - configured disk size, IN-CORE, no device I/O.
 * Proof: no datafield dispatch (no JPL I ,B) anywhere in the body.
 * ========================================================================== */
int GSIZE(int *size_out)
{
    reserve_prologue();                        /* 037105 -> 003752               */
    int p1, p2, p3;
    if (get_param(050124, &p1) != OK) return err();   /* 037107; 037110 err     */
    if (get_param(050223, &p2) != OK) return err();   /* 037115; 037116 err     */
    if (get_param(050220, &p3) != OK) return err();   /* 037121; 037122 err     */

    if (geometry_bit_set(p2))                  /* 037124 BSKP ONE                */
        low_helper();                          /* 037127 -> 000215               */

    int shift = (p1 & 024) >> 010;             /* 037130-037132                  */
    *size_out = p3 * shift;                    /* 037134 RMPY  <- COMPUTE SIZE   */
    return OK;                                 /* 037135-037140 OK EXIT          */
    /* 037141-037142: ERROR path.                                              */
}


/* ==========================================================================
 * R3BUF = 035112B (flavour 1) / R3IBU = 035102B (flavour 2)
 *   RELEASE / INVALIDATE all cache buffers belonging to a device.
 *   INFERRED role: dismount / cache-flush. The acquire counterparts are the
 *   getters G3BUF 034643B / G3IBU 034633B; RCBLO's per-block acquire uses
 *   COMPP + G3NWT + reserve. R3BUF is the bulk release.
 * ========================================================================== */
int R3BUF_release(int device /* ,B 1 */, int flavour /* 1=data, 2=index */)
{
    reserve_prologue();                        /* 035106/035116 -> 003752        */

    CacheBuf *b = cache_list_head();           /* 035121-035123 -> 010500        */
    while (b != END /* T=-1 sentinel */) {     /* 035124-035126 loop guard       */
        CacheBuf *next = b->lru_fwd;           /* 035130 save forward link       */

        if (b->block_hi /*,X5*/ == device      /* 035132-035135 device match     */
            && want_release(b, flavour))       /* 035136-035147 flavour/busy gate*/
        {
            reserve_wait(b);                   /* 035150-035151 -> 010506        */
            b->flags    = -1;                  /* 035153 invalidate              */
            b->block_hi = -1;                  /* 035154 device id := invalid    */
            lru_unlink(b);                     /* 035160-035177 unlink from LRU   */
            free_list_push(b);                 /* 035200-035210 onto free list    */
        }
        b = next;                              /* 035211-035212 advance + loop    */
    }
    return OK;                                 /* 035213-035217 DONE / OK EXIT    */
}
