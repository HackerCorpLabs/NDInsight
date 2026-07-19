/* ============================================================================
 * MON 251B  CopyPage (COPAG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Special call used by the BACKUP-SYSTEM.
 *
 * Derived from the real disassembly (see 251B-CopyPage.ASM). Control flow is
 * BYTE-VERIFIED; the semantic labels (source/dest open, page copy, hole
 * detection) are INFERRED from the call structure and the manual - treat as a
 * model. Addresses in comments are octal. Instruction behaviour is grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 *
 * Copies file pages sequentially between two opened files (one may be magnetic
 * tape/floppy). MAC contract: T=source file, A=dest file, X=addr of 32-bit
 * first-page word, D=addr of short-magtape-record buffer. Double/triple skip
 * return reports missing pages / short records.
 * ============================================================================ */

/* Dispatch reality: GOTAB[251] = 000000 (fall-through). No level-14 stub; the
 * monitor reaches this body via the uncarved resident CALLPROC/MFELL path.     */
int mon_copy_page(mon_regs *r)
{
    save_params(r);                        /* 110050: STD I 136 - stash D (buffer addr) */
    spush();                               /* 110054: JPL I 133 -> 110207 = 003752 (SPUSH) */

    /* 110055-110074: open/validate the SOURCE and DESTINATION open-file
     * descriptors (110057 JPL I 131 -> 110210; error -> 110060 JMP I 131 ->
     * 110211). ,B 12/13 = source/dest descriptor, ,B 10/11 = their file fields. */
    src = open_file_slot(r->T /*SourceFile*/);   /* dest error return here */
    dst = open_file_slot(r->A /*DestFile*/);     /* source error return here */

    /* 110075-110102: fetch the caller 32-bit first-page address (X points at a
     * double word); it is shared by source and destination (per the manual).    */
    page = load_double(r->X /*FirstPage*/);

    /* 110103-110325: main sequential copy loop. For each page:
     *   - 110110/110134/110162 JAF: branch on whether src/dst is a real file;
     *   - read one page from source, write it to destination;
     *   - 110155 JPL I 36 -> 110215 : the per-page transfer worker;
     *   - detect missing pages / holes (110126 JAP, 110145 JAF) and set the
     *     A:D / T:X missing-page skip-return values (110133/110161/110204 STD);
     *   - 110316 LDA I 55 / JAZ tests the loop-continue flag; 110321 MIN ,B 17
     *     bumps the page counter, 110323 MIN ,B 16 the copied count.            */
    for (;;) {
        rc = copy_one_page(src, dst, page); /* JPL I workers via ptr tables      */
        if (page_missing(rc)) { set_missing_skip(r); }   /* double-skip return   */
        if (short_record(rc)) { set_shortrec_skip(r); }  /* triple-skip return   */
        if (rc == EOF || !more_pages(rc)) break;
        page++;
    }

    spop();                                /* 110356/110375: JMP I -> 003776 (SPOP) */
    return rc;                             /* 3 = end-of-file per the manual       */
}

/* INFERRED / UNVERIFIED:
 *   - The per-page worker pointers live in the tables 110206-110216, 110357-
 *     110374 and 110463-110471 (SPUSH 003752, SPOP 003776 among them); their
 *     internals are past this carve, so open_file_slot/copy_one_page are models.
 *   - The exact register->parameter mapping (T/A/X/D) is from 251B_CopyPage.yaml;
 *     this post-CALLPROC body works through ,B frame fields.
 *   - GOTAB[251]=000000: the MON 251 -> COPAG binding runs through the uncarved
 *     resident CALLPROC/MFELL and is NOT statically proven.
 */
