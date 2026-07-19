/* ============================================================================
 * MON 257B  OpenFileInfo (FOPEN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 257B-OpenFileInfo.ASM). Control flow
 * and the open-file-table scan + the FCON helper call are BYTE-VERIFIED; the
 * semantic labels (which field is name / type / file-number / access) are
 * INFERRED from the SINTRAN III Monitor Calls manual and the field copies seen
 * - treat as a model, not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   X = address of file-name string
 *   A = address of default file-type string
 *   MON 257 ; JMP ERROR ; STT FILNO ; STA ACODE ; COPY SD DA ; STA DEVNO
 * Returns: T = file number, A = access code, D = peripheral device number.
 * Error return: A = error number, D = peripheral file number. */

int mon_openfileinfo(mon_regs *r)
{
    save_params(r);                        /* 067432: STD I 127 - stash caller D  */
    resident_prologue();                   /* 067436: JPL I 124 -> 003752         */

    /* 067442-067456: scan the open-file table for the named/typed file */
    for (X = table_base; ; X += 2) {       /* 067446 AAX 2 / 067456 JMP -14        */
        entry = mem[X];                    /* 067442: LDA ,X 0                     */
        if (entry == 0) goto matched_or_empty; /* 067445: JAZ 12 -> 067457         */
        if (mem[X] == r->key) goto matched_or_empty;
        if (table_exhausted) { rc = 0122; goto store_status; } /* 067452 SAA 122   */
    }

matched_or_empty:
    /* 067457-067462: fetch caller name/type args, resolve file context */
    if (fcon_helper(r) != OK)              /* 067462: JPL I 103 -> FCON 067002     */
        goto store_status;                 /*   -> 067557 on failure               */

    /* 067464-067551: locate the matching open-file slot, extract file number,
     * access code and (for peripheral files) the logical device number */
    rc = 0120;                             /* 067522/067545: SAA 120 - result code */
    r->status = rc;                        /* 067523/067547: STA ,B 2 -> caller    */
    resident_return();                     /* 067556: JMP I 13 -> 003776           */
    return rc;                             /* T=file number, A=access, D=dev number */

store_status:
    r->status = rc;                        /* 067553/067557: STA ,B 2 -> caller    */
    resident_return();                     /* 067556/067560->067555: JMP I -> 003776 */
    return rc;                             /* A = error number                      */
}

/* Byte-verified anchors:
 *   FOPEN entry 067432, frame SAB 6, prologue JPL I 124 -> 003752,
 *   FCON call JPL I 103 -> 067002, status store STA ,B 2 (067523/067553/067557),
 *   resident return -> 003776. */
