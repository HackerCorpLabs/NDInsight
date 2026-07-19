/* ==========================================================================
 * RCBLO - reserve/read a disk-cache block   (SINTRAN III VSX/500 L07)
 * Segment 006-S3FS, entry 035766B. Readable pseudo-code reconstruction from
 * the carved bytes (RCBLO.ASM). Octal literals shown with 0 prefix are octal.
 *
 * Contract (from RXDIR 037643B, VERIFIED):
 *   RXDIR sets block = 0 (RADD CLD 0 DD) then CALL RCBLO to fetch page 0.
 *   RCBLO returns X = cache buffer holding the block, or takes the error exit.
 * ==========================================================================*/

/* datafield/buffer field offsets used below (octal ,X n) */
struct cache_buf {
    u16 flags;      /* ,X 4  bit15 = in-use            */
    u16 block_hi;   /* ,X 5                            */
    u16 block_lo;   /* ,X 6                            */
    u16 device;     /* ,X 12                           */
    /* ...page data follows... */
};
struct unit_df {
    u16 word0;
    /* ... */
    fnptr transfer; /* ,X 14  = device transfer primitive (SCSDISK for SCSI) */
};

/* op is the RCBLO operation code (seek/read/write class); block is 32-bit. */
int RCBLO(u32 block, int op /* -> local 12 */)
{
    reserve_link();                 /* 035772 CALL 003752 (resident prologue)  */

    /* 035773 CALL GSIZE: get configured disk size from IN-CORE parameter
     * tables (NO device I/O). INFERRED: a size/geometry failure here can take
     * the error stub at 036205 -> return error without any transfer.          */
    u32 size = GSIZE();             /* 037101B */
    if (GSIZE_failed) goto err_stub;/* 035774 JMP [036057]=036205              */

    struct unit_df  *df  = get_device_descriptor(block); /* 035776-036000     */
    op = classify(op);              /* 036001-036004 local 12 := op            */

    struct cache_buf *b = cache_search(block, df); /* 036005-036030            */

    /* --- cache lookup: does buffer b already hold (block, device)? --------- */
    if (b->block_full == block &&                       /* 036031 */
        b->block_hi  == hi(block) &&                     /* 036033-036035 */
        b->block_lo  == lo(block)) {                     /* 036037-036041 */
        /* HIT: block already resident -> success, NO device read (036043-53) */
        touch_lru(b);               /* 036047 CALL 010506                      */
        goto success;               /* 036053 JMP -> 036144                    */
    }

    /* --- MISS: (re)assign this buffer to (block, device) and TRANSFER ------ */
    b->block = block;               /* 036072-036073 */
    b->device = df->device;         /* 036074-036075 */
    reserve_buffer_wait(b);         /* 036077 CALL 010506 */

    fnptr drv = df->transfer;       /* 036100-036102: local 10 := ,X 14        */
                                    /*   VERIFIED: A := MEM[X+14]; ,B10 := A    */

    if (geometry_check_failed(b, block)) {   /* 036103-036117 */
        return ERR_100;             /* 036116 SAA 100B ; 036117 -> err         */
    }
    prepare_addr(b);                /* 036120-036122 CALL 050124               */

    /* Four dispatch sites build a device function code from `op` (AND/AAA)
     * and call the driver indirectly:  (*drv)(fn, buffer, block).
     * This is the ONLY place the page-0 read is enqueued (-> SCSDISK -> SCLLD
     * -> INITO -> SCWAQ). VERIFIED bytes 135410 = JPL I ,B 10.               */
    int fn1 = (op & 065) + 066;                 /* 036124-036130 */
    if ((*drv)(fn1, b, block) == ERROR)         /* 036135  DISPATCH #1 (READ)  */
        goto err_release;                        /* 036136 -> 036177            */

    int fn2 = (op & 054) + 063;                 /* 036137-036141 */
    if ((*drv)(fn2, b, block) != ERROR)         /* 036142  DISPATCH #2 (alt)   */
        goto more;                               /* 036143 -> 036161            */

success:                                         /* 036144 MIN ,B 4 */
    /* ok convergence: publish buffer, return X = buffer, success code */
    finalize(b);                                 /* 036145-036156 */
    return OK;                                    /* 036160 -> 003776 epilogue  */

more:
    int fn3 = (op & 032) + 066;                 /* 036161-036163 */
    if ((*drv)(fn3, b, block) == ERROR)         /* 036167  DISPATCH #3         */
        goto err_release;                        /* 036170 -> 036177            */
    int fn4 = (op & 022) + 063;                 /* 036171-036173 */
    if ((*drv)(fn4, b, block) == ERROR)         /* 036174  DISPATCH #4         */
        goto err_release;                        /* 036175 -> 036177            */
    goto success;                                /* 036176 JMP -> 036144        */

err_release:                                     /* 036177 */
    CL1DB(b);                                     /* 036203 CALL 035240 release */
    return ERROR;                                /* 036204 -> 036157            */

err_stub:                                        /* 036205 */
    return ERROR;                                /* 036206 -> 036157            */
}

/* ==========================================================================
 * GSIZE 037101B - get configured disk size (IN-CORE, no device transfer)
 * ==========================================================================*/
u32 GSIZE(void)
{
    reserve_link();                 /* 037105 CALL 003752                      */
    u16 p0 = get_param(050124);     /* 037107, fail -> err                     */
    u16 p1 = get_param(050223);     /* 037115, fail -> err                     */
    u16 p2 = get_param(050220);     /* 037121, fail -> err                     */
    if (p1 & bit(0100)) { /* 037124 */ } else if (p2==0) helper_000215();
    u32 size = (u32)(p0 & 024 >> 010) * something;  /* 037132-037134 RMPY      */
    return size;                    /* 037135 ; 037140 -> 003776 epilogue      */
    /* err: return ERROR  (037141-037142)                                     */
}
