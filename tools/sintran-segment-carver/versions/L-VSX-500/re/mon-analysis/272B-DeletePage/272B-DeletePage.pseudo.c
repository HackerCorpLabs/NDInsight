/* ============================================================================
 * MON 272B  DeletePage (DELPG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 272B-DeletePage.ASM). Control flow is
 * BYTE-VERIFIED; the semantic labels (file lookup, page-range loop, page-table
 * clearing) are INFERRED - treat as a model. Addresses in comments are octal.
 * Instruction behaviour is grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 *
 * Deletes the pages of an opened file between FirstPage and LastPage inclusive
 * (LastPage = -1 means to end of file) and returns the number of pages deleted.
 * MAC contract: T=file number, A=addr of 32-bit first page, X=addr of 32-bit
 * last page; normal return stores the 32-bit deleted count.
 * ============================================================================ */

/* Dispatch reality: GOTAB[272] = 000000 (fall-through). No level-14 stub; the
 * monitor reaches this body via the uncarved resident CALLPROC/MFELL path.     */
int mon_delete_page(mon_regs *r)
{
    save_params(r);                        /* 110472: STD I 116 - stash caller A:D pair */
    spush();                               /* 110476: JPL I 113 -> 110611 = 003752 (SPUSH) */

    /* 110477 LDA ,B 1; 110500 STA I 112; 110501 JPL I 112 -> 110613 = 010376 :
     * resolve the open-file descriptor from the caller file number.            */
    ofd = resolve_open_file(r->T /*FileNo*/);

    /* 110502-110537: validate the descriptor and access. 110504 SKP IF DX EQL 0
     * -> error (110507 SAA 132 / 110511 SAA 125); 110513 LDA ,X 3 attribute
     * tests -> SAA 133 / 125; page-range setup 110524-110537 (JPL I 66/60 ->
     * 110616); all error paths take the indirect exit -> 110614.               */
    if (ofd == 0) return err_no_open_file;
    if (!writable(ofd)) return err_access;

    /* 110540-110606: bound the delete range. 110547 LDA ,X 7 flags; compute
     * (LastPage - FirstPage) as a 32-bit value (110576 RSUB SX DD / 110577 RADD
     * ADC / 110601 RSUB ST DA); 110602 JAP: if the range is negative, error 174. */
    first = r->FirstPage; last = r->LastPage;   /* last = -1 -> end of file      */

    /* 110624-111002: delete loop. For each page in [first..last]:
     *   - 110630-110675: locate the page in the file's page-table entry
     *     (compares against ,X 27 / ,X 51 slot lists) and clear it
     *     (110647/110650 STZ ,X 0 / STZ ,X 1 - free the mapping; 110653/110674
     *     BSET ONE ... DT marks the table word dirty; STT ,X 7 writes it back);
     *   - 110700 JPL I 110 -> 111010 : commit the page-table change;
     *   - 110702-110711: bump the 32-bit deleted-count (,B 15) and current page
     *     (,B 7); 110712-110722 loop while pages remain (110721 JAN -> 110723).  */
    for (p = first; p <= last; p++) {
        free_file_page(ofd, p);            /* clear page-table mapping           */
        ndeleted++;
    }

    /* 110723-110760: store the 32-bit deleted count into the caller frame
     * (110737 STD ,X 23) and return via 110760 JMP I 32 -> 111012 = 003776.     */
    spop();
    return ndeleted;                       /* NoOfPages out (double word)         */
}

/* INFERRED / UNVERIFIED:
 *   - Worker pointers live in the tables 110610-110623 and 111005-111012 (SPUSH
 *     003752, open-file lookup 010376, SPOP 003776 among them); the page-table
 *     and free-space internals are past this carve, so free_file_page is a model.
 *   - The user-visible MON contract (T=file, A=firstpage, X=lastpage) is from
 *     272B_DeletePage.yaml; this post-CALLPROC body uses ,B frame fields.
 *   - GOTAB[272]=000000: the MON 272 -> DELPG binding runs through the uncarved
 *     resident CALLPROC/MFELL and is NOT statically proven.
 */
