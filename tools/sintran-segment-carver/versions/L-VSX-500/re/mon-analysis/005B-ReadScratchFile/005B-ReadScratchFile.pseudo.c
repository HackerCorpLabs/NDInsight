/* ============================================================================
 * MON 5B  ReadScratchFile (RDISK)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 6B WriteScratchFile (WDISK).
 *
 * Derived from the real disassembly (see 005B-ReadScratchFile.ASM). Control flow
 * and the read/write (SSK) fork are BYTE-VERIFIED; the semantic labels (which
 * file-system worker does what) are INFERRED from the call structure - treat as
 * a model, not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Entry 102021 = READ (ssk=0); entry 102023 = WRITE (ssk=1). One shared body. */
int mon_scratch_xfer(mon_regs *r, int ssk /* 0=read, 1=write */)
{
    save_params(r);                        /* 102024: STD I 73 - stash caller A:D pair */

    desc = classify_request(r);            /* 102032-102060: 2-word/entry table   */
                                           /*   scan, masked BSKP attribute tests */

    /* 102060-102064: on-disk position from the caller arg (address/size scaling).
     * 102060 RADD CLD SD DA: A = D; 102061 SUB 40: A -= mem[P+40] (a P-relative
     * constant word, NOT the literal 040); 102062 SHA ZIN SHR 1: logical A >>= 1
     * (zero-fill); 102063 AAA 100: A += 0100; 102064 STA I 37 (store indirect).   */
    pos = ((D - mem[P + 040]) >> 1) + 0100;

    if (ssk == 0)                          /* 102103 BSKP ONE SSK  (read vs write) */
        rc = fs_read_block (desc.dev, pos, r->buf);   /* 102110 JPL I 16 -> 077542 */
    else
        rc = fs_write_block(desc.dev, pos, r->buf);   /* 102105 JPL I 20 -> 100130 */

    return rc;                             /* 102160: success skip-return         */
}

/* Callers:
 *   MON 5B ReadScratchFile:   mon_scratch_xfer(r, 0);
 *   MON 6B WriteScratchFile:  mon_scratch_xfer(r, 1);
 */
