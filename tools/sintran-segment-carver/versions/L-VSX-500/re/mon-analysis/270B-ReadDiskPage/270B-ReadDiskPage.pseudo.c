/* ============================================================================
 * MON 270B  ReadDiskPage (RDPAG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 271B WriteDiskPage (WDPAG).
 *
 * Derived from the real disassembly (see 270B-ReadDiskPage.ASM). Control flow
 * and the read/write (SSK) fork are BYTE-VERIFIED; the semantic labels are
 * INFERRED - treat as a model. Addresses in comments are octal. Instruction
 * behaviour is grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 *
 * Reads one or more raw directory pages (2048 bytes each) from a disk into a
 * caller buffer. The directory must be reserved (ReserveDir). MAC contract:
 * T=directory index, X=buffer, A=NoOfPages (COPY SA DD), then A=addr of the
 * 32-bit disk page address.
 * ============================================================================ */

/* Entry 107447 = READ  (BSET ZRO SSK -> ssk=0, MON 270B);
 * Entry 107451 = WRITE (BSET ONE SSK -> ssk=1, MON 271B). One shared body.      */
int mon_disk_page_xfer(mon_regs *r, int ssk /* 0=read (270B), 1=write (271B) */)
{
    save_params(r);                        /* 107452: STD I 47 - stash caller A:D pair */
    /* 107453 A=L; 107454 D=B; 107455 SAB 22: B:=22 (frame base).               */
    spush();                               /* 107456: JPL I 44 -> 107522 = 003752 (SPUSH) */

    /* 107457 STZ ,B 21; 107460 BSKP ONE SSK -> function code 107462 SAA 61
     * (write) / 107464 SAA 60 (read) into ,B 20; 107466 JPL I 35 -> 107523 =
     * 030225 : validate/resolve the directory index (RESDI reserved dir).       */
    fn = ssk ? 061 : 060;                  /* device function code (write/read)   */
    dir = resolve_dir_index(r->T /*DirIndex*/, fn);

    /* 107467 STX ,B 7 : caller buffer address. 107470-107520: index validation
     * (LDA ,X flags, LDT I compares); on failure load an error code and take the
     * indirect error exit: 107473 SAA(31) JMP I 31, 107501 SAA(26) JMP I 23,
     * 107517 SAA(10) JMP I 5 -> 107525.                                         */
    if (!dir_valid(dir)) return err_dir;
    if (!dir_reserved(dir)) return err_not_reserved;

    /* 107531-110035: transfer NoOfPages pages. 107535 LDA ,B 3 (count);
     * 107536 SHA ZIN 12 (A <<= 10 octal = page->word scale, 1 page = 2048 bytes
     * = 1024 words); build the disk request (107625-107676 fill an I/O request
     * block, 107635/107676 JPL I to the disk driver worker), advance the disk
     * page address (110026-110034: 32-bit +1) and loop while pages remain
     * (110035 JMP -> 107741).                                                    */
    for (n = 0; n < r->NoOfPages; n++) {
        if (ssk == 0)
            rc = disk_read_page (dir, r->PageAddr + n, r->buf + n*1024);  /* 107636 */
        else
            rc = disk_write_page(dir, r->PageAddr + n, r->buf + n*1024);
        if (rc != 0) break;
    }

    spop();                                /* 107723/110035: return via 003776 (SPOP) */
    return rc;                             /* status in ,B 2 frame field           */
}

/* Callers:
 *   MON 270B ReadDiskPage:  mon_disk_page_xfer(r, 0);
 *   MON 271B WriteDiskPage: mon_disk_page_xfer(r, 1);
 *
 * INFERRED / UNVERIFIED:
 *   - The JPL I worker pointers live in the tables 107703-107724 and 110036-
 *     110047 (SPUSH 003752 / directory helper 030225 among them); the disk-
 *     driver internals are past this carve, so disk_read_page/disk_write_page
 *     are models.
 *   - The user-visible MON contract (T=dirindex, X=buffer, A=count/pageaddr) is
 *     from 270B_ReadDiskPage.yaml; this post-CALLPROC body uses ,B frame fields.
 *   - MON 270 -> this body: GOTAB[270]=066276 (F1742 stub) is byte-proven, but
 *     the stub -> RDPAG hop is the uncarved resident CALLPROC (README caveats).
 */
