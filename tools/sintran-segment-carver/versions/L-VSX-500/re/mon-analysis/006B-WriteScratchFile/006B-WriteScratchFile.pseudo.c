/* ============================================================================
 * MON 6B  WriteScratchFile (WDISK)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 5B ReadScratchFile (RDISK).
 *
 * Derived from the real disassembly (see 006B-WriteScratchFile.ASM). Control
 * flow and the read/write (SSK) fork are BYTE-VERIFIED; the semantic labels
 * (which file-system worker does what, what the ,B frame fields mean) are
 * INFERRED from the call structure - treat as a model, not gospel. Addresses
 * in comments are octal.
 *
 * Dispatch reality: GOTAB[006] = 000000 (fall-through). MON 6B has no level-14
 * GOTAB stub; the monitor reaches this body via the uncarved resident
 * CALLPROC / MFELL path. This model begins at the worker entry, AFTER CALLPROC.
 * ============================================================================ */

/* Entry 102021 = READ (ssk=0); entry 102023 = WRITE (ssk=1). One shared body.
 * MON 6B WriteScratchFile enters at 102023 with ssk=1. */
int mon_scratch_xfer(mon_regs *r, int ssk /* 0=read, 1=write; MON 6B -> 1 */)
{
    save_params(r);                        /* 102024: STD I 73 - stash caller A:D pair */
    spush();                               /* 102030: JPL I 70 -> SPUSH (003752)  */

    desc = classify_request(r);            /* 102031-102060: 2-word/entry table   */
                                           /*   scan; 102040 BSKP ONE SSK picks   */
                                           /*   write-side (,X 1) vs read-side     */
                                           /*   (,X 0) descriptor field           */

    /* 102060-102064: on-disk position from the caller arg (address/size scaling).
     * 102060 RADD CLD SD DA: A = D; 102061 SUB 40: A -= mem[P+40] (a P-relative
     * constant word, NOT the literal 040); 102062 SHA ZIN SHR 1: logical A >>= 1
     * (zero-fill); 102063 AAA 100: A += 0100; 102064 STA I 37 (store indirect).   */
    pos = ((D - mem[P + 040]) >> 1) + 0100;

    if (ssk == 1)                          /* 102103 BSKP ONE SSK  (write vs read) */
        rc = fs_write_block(desc.dev, pos, r->buf);   /* 102105 JPL I 20 -> 100130 FWRT */
    else
        rc = fs_read_block (desc.dev, pos, r->buf);   /* 102110 JPL I 16 -> 077542 FREA */

    /* error/guard-fail exit funnels through 102115 STA ,B 2 (status into frame) */
    spop();                                /* 102114: JMP I 13 -> SPOP (003776)   */
    return rc;                             /* status carried in ,B 2 frame field  */
}

/* Callers:
 *   MON 5B ReadScratchFile:   mon_scratch_xfer(r, 0);
 *   MON 6B WriteScratchFile:  mon_scratch_xfer(r, 1);
 *
 * INFERRED / UNVERIFIED:
 *   - The user-visible T/X/A MON contract (block/buffer/error) is from the
 *     README/manual, not provable in this post-CALLPROC body (it uses ,B frame
 *     fields, not raw MON registers).
 *   - 033740 (helper at ptr 102124) is ambiguous between B4INW and ATMUL.
 *   - The MON 6 -> this-body binding runs through the uncarved CALLPROC and is
 *     NOT statically proven.
 */
