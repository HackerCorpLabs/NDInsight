/* ============================================================================
 * MON 10B  WriteBlock (WPAGE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 7B ReadBlock (RPAGE).
 *
 * Derived from the real disassembly (see 10B-WriteBlock.ASM). Control flow and
 * the read/write (SSK) fork are BYTE-VERIFIED; semantic labels are INFERRED -
 * treat as a model. Addresses in comments are octal. Instruction behaviour is
 * grounded in ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 *
 * Dispatch reality: GOTAB[010] = 000000 (fall-through). MON 10B has no level-14
 * GOTAB stub; the monitor reaches this body via the uncarved resident
 * CALLPROC / MFELL path. This model begins at the worker entry, AFTER CALLPROC.
 *
 * Writes one block randomly to a file opened for random write access.
 * ============================================================================ */

/* Entry 101707 = READ (ssk=0, MON 7B); Entry 101711 = WRITE (ssk=1, MON 10B).
 * MON 10B WriteBlock enters at WPAGE 101711 with ssk=1. One shared body.       */
int mon_block_xfer(mon_regs *r, int ssk /* 0=read; MON 10B -> 1 */)
{
    save_params(r);                        /* 101712: STD I 77 - stash caller A:D pair */
    spush();                               /* 101716: JPL I 74 -> 102012 = 003752 (SPUSH) */

    /* 101717-101721: resolve the open-file descriptor from the caller file no.  */
    ofd = resolve_open_file(r->T /*FileNumber*/);
    if (ofd == 0) return err_no_open_file; /* 101727 SKP IF DX EQL 0 -> error codes */

    /* 101742-101756: access checks; write needs random write access.            */
    if (!access_ok(ofd, ssk)) return err_access;

    /* 101757-101771: 32-bit block->disk position (SAD ZIN 1 / RADD ADC negate). */
    pos = block_to_disk_pos(ofd, r->BlockNo);

    if (ssk == 1)                          /* 101775 BSKP ONE SSK (write vs read) */
        rc = fs_write_block(ofd, pos, r->buf);   /* 101777 JPL I 17 -> 102016 = 100130 (FWRT) */
    else
        rc = fs_read_block (ofd, pos, r->buf);   /* 102002 JPL I 15 -> 102017 = 077542 (FREA) */

    spop();                                /* 102006: JMP I 12 -> 102020 = 003776 (SPOP) */
    return rc;                             /* status carried in ,B 2 frame field  */
}

/* Callers:
 *   MON 7B  ReadBlock:   mon_block_xfer(r, 0);
 *   MON 10B WriteBlock:  mon_block_xfer(r, 1);
 *
 * INFERRED / UNVERIFIED:
 *   - Worker pointers 100130 (FWRT), 077542 (FREA), 010376, 003752, 003776 come
 *     from the pointer table 102011-102020; internals are past this carve.
 *   - The user-visible MON contract (T=file, A=block, X=buffer) is from the
 *     manual (10B_WriteBlock.yaml); this post-CALLPROC body uses ,B frame fields.
 *   - GOTAB[010]=000000: the MON 10 -> WPAGE binding runs through the uncarved
 *     resident CALLPROC/MFELL and is NOT statically proven.
 */
