/* ============================================================================
 * MON 271B  WriteDiskPage (WDPAG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 270B ReadDiskPage (RDPAG).
 *
 * Derived from the real disassembly (see 271B-WriteDiskPage.ASM). Control flow
 * and the read/write (SSK) fork are BYTE-VERIFIED; semantic labels are INFERRED
 * - treat as a model. Addresses in comments are octal. Instruction behaviour is
 * grounded in ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 *
 * Dispatch reality: GOTAB[271] = 000000 (fall-through). MON 271B has no level-14
 * GOTAB stub; the monitor reaches this body via the uncarved resident
 * CALLPROC / MFELL path. This model begins at the worker entry, AFTER CALLPROC.
 *
 * Writes one or more raw directory pages (2048 bytes each) from a caller buffer
 * to disk. The directory must be reserved (ReserveDir).
 * ============================================================================ */

/* Entry 107447 = READ (ssk=0, MON 270B); Entry 107451 = WRITE (ssk=1, MON 271B).
 * MON 271B WriteDiskPage enters at WDPAG 107451 with ssk=1. One shared body.    */
int mon_disk_page_xfer(mon_regs *r, int ssk /* 0=read; MON 271B -> 1 */)
{
    save_params(r);                        /* 107452: STD I 47 - stash caller A:D pair */
    spush();                               /* 107456: JPL I 44 -> 107522 = 003752 (SPUSH) */

    /* 107460 BSKP ONE SSK -> function code 107462 SAA 61 (write) into ,B 20;
     * 107466 JPL I 35 -> 107523 = 030225 : resolve the directory index.         */
    fn = ssk ? 061 : 060;
    dir = resolve_dir_index(r->T /*DirIndex*/, fn);
    if (!dir_valid(dir)) return err_dir;                 /* 107470-107502 checks  */
    if (!dir_reserved(dir)) return err_not_reserved;     /* 107503-107520 checks  */

    /* 107531-110035: transfer NoOfPages pages. 107536 SHA ZIN 12 scales
     * pages->words (1 page = 1024 words = 2048 bytes); loop advances the 32-bit
     * disk page address by 1 per page (110026-110034) until the count runs out. */
    for (n = 0; n < r->NoOfPages; n++) {
        if (ssk == 1)
            rc = disk_write_page(dir, r->PageAddr + n, r->buf + n*1024);
        else
            rc = disk_read_page (dir, r->PageAddr + n, r->buf + n*1024);
        if (rc != 0) break;
    }

    spop();                                /* 107723/110035: return via 003776 (SPOP) */
    return rc;
}

/* Callers:
 *   MON 270B ReadDiskPage:  mon_disk_page_xfer(r, 0);
 *   MON 271B WriteDiskPage: mon_disk_page_xfer(r, 1);
 *
 * INFERRED / UNVERIFIED:
 *   - Worker pointers live in the tables 107703-107724 and 110036-110047; the
 *     disk-driver internals are past this carve.
 *   - The user-visible MON contract is from 271B_WriteDiskPage.yaml; this post-
 *     CALLPROC body uses ,B frame fields.
 *   - GOTAB[271]=000000: the MON 271 -> WDPAG binding runs through the uncarved
 *     resident CALLPROC/MFELL and is NOT statically proven.
 */
