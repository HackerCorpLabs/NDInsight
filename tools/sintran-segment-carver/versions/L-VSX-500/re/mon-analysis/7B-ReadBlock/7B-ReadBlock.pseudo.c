/* ============================================================================
 * MON 7B  ReadBlock (RPAGE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 10B WriteBlock (WPAGE).
 *
 * Derived from the real disassembly (see 7B-ReadBlock.ASM). Control flow and
 * the read/write (SSK) fork are BYTE-VERIFIED; the semantic labels (which file-
 * system worker does what) are INFERRED from the call structure - treat as a
 * model, not gospel. Addresses in comments are octal. Instruction behaviour is
 * grounded in ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 *
 * Reads one block randomly from a file already opened for random read access.
 * The block-transfer core is the same shape as MON 5B/6B RDISK/WDISK, but here
 * the file is selected by a caller-supplied file number (not the fixed scratch
 * file 100B), resolved through the open-file lookup at 101721.
 * ============================================================================ */

/* Entry 101707 = READ  (BSET ZRO SSK -> ssk=0);
 * Entry 101711 = WRITE (BSET ONE SSK -> ssk=1, MON 10B). One shared body.     */
int mon_block_xfer(mon_regs *r, int ssk /* 0=read (MON 7B), 1=write (MON 10B) */)
{
    save_params(r);                        /* 101712: STD I 77 - stash caller A:D pair */
    /* 101713 RADD CLD SL DA: A=L; 101714 RADD CLD SB DD: D=B; 101715 SAB 6: B:=6 */
    spush();                               /* 101716: JPL I 74 -> 102012 = 003752 (SPUSH) */

    /* 101717 RADD CLD ST DA: A=T (the caller file number); 101720 STA I 73;
     * 101721 JPL I 73 -> 102014 = 010376 : resolve the open-file descriptor.   */
    ofd = resolve_open_file(r->T /*FileNumber*/);

    /* 101722 BSKP ONE SSK: pick X from D (write) or A (read).
     * 101727 SKP IF DX EQL 0 -> 101742: if no descriptor, set an error code
     * (101734 SAA 132 / 101736 SAA 126 / 101740 SAA 125) and take the error exit. */
    if (ofd == 0) return err_no_open_file;

    /* 101742-101756: access-attribute checks on the descriptor (LDA ,X 3 flags,
     * masked BSKP tests): not-random / wrong access -> SAA 133 / 125 / 126.     */
    if (!access_ok(ofd, ssk)) return err_access;

    /* 101757-101771: compute the on-disk position from the caller block number
     * as a 32-bit A:D value (101760 LDA ,B 2; 101765 SAD ZIN 1 = A:D << 1 zero-
     * fill; 101766 RADD CM1 0 DD / 101767 RADD ADC CLD SA DA = 32-bit negate/adj;
     * 101771 STD ,X 17). The exact scaling is INFERRED.                         */
    pos = block_to_disk_pos(ofd, r->BlockNo);

    /* 101772 LDA ,B 5 (buffer descriptor) -> D; 101774 LDA ,B 0.               */
    if (ssk == 0)                          /* 101775 BSKP ONE SSK (read vs write) */
        rc = fs_read_block (ofd, pos, r->buf);   /* 102002 JPL I 15 -> 102017 = 077542 (FREA) */
    else
        rc = fs_write_block(ofd, pos, r->buf);   /* 101777 JPL I 17 -> 102016 = 100130 (FWRT) */

    /* error/guard exit funnels through 102007 STA ,B 2 (status into frame)      */
    spop();                                /* 102006: JMP I 12 -> 102020 = 003776 (SPOP) */
    return rc;                             /* 102004 MIN ,B 4 counts a completed block  */
}

/* Callers:
 *   MON 7B  ReadBlock:   mon_block_xfer(r, 0);
 *   MON 10B WriteBlock:  mon_block_xfer(r, 1);
 *
 * INFERRED / UNVERIFIED:
 *   - Pointer words 010376 (open-file lookup), 100130 (FWRT), 077542 (FREA),
 *     003752 (SPUSH), 003776 (SPOP) are read straight from the pointer table
 *     102011-102020; the worker *names* match the RDISK/WDISK family but their
 *     internals are past this carve.
 *   - The user-visible T=file / A=block / X=buffer MON contract is from the
 *     manual (7B_ReadBlock.yaml); this post-CALLPROC body uses ,B frame fields.
 *   - MON 7 -> this body: GOTAB[7]=120402 (F1612 stub) is byte-proven, but the
 *     stub -> RPAGE hop is the uncarved resident CALLPROC (see README caveats).
 */
