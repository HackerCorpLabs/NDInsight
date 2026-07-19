/* ============================================================================
 * MON 73B  SetMaxBytes (SMAX)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 73B-SetMaxBytes.ASM). Control flow and
 * the call to the SMAXB set-max-bytes primitive are BYTE-VERIFIED; the semantic
 * labels (which field is file-number / max-byte-pointer) are INFERRED from the
 * SINTRAN III Monitor Calls manual and the field copies seen - treat as a model,
 * not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = file number   (returned from an earlier OpenFile)
 *   D = maximum byte pointer  (INTEGER4, a double word: number of bytes - 1)
 * Error return: A = error number. The file must be opened for write; the call
 * is only relevant for sequential access. */

int mon_setmaxbytes(mon_regs *r)
{
    save_params(r);                        /* 103706: STD I 115 - stash caller D  */
    resident_prologue();                   /* 103712: JPL I 112 -> 003752         */
    store_file_number(r->T);               /* 103713: STT I 112 (file number)     */

    rc = smaxb_primitive(r);               /* 103714: JPL I 112 -> SMAXB 072620   */
                                           /*   sets the file's max byte pointer  */

store_status:                              /* 103733 (from 103715)                */
    r->status = rc;                        /* 103733: STA ,B 2 -> caller slot B+2 */
    /* 103734 JMP -3 -> 103731 */
    r->A = -6;                             /* 103731: SAA -6 -> A := -6 (return/skip code, UNVERIFIED) */
    resident_return();                     /* 103732: JMP I 76 -> 003776          */
    return rc;                             /* A = error number on the error path  */
}

/* Byte-verified anchors:
 *   SMAX entry 103706, frame SAB 6, prologue JPL I 112 -> 003752,
 *   SMAXB call JPL I 112 -> 072620, status store STA ,B 2 (103733),
 *   return code A := -6 via SAA -6 (103731) -> resident return 003776 (link cell 104030).
 *
 * The MON 73B -> SMAX link is NOT byte-proven: GOTAB[73] vectors to the F1644
 * stub (025-S3IRPIT), which transfers through the uncarved resident CALLPROC
 * bridge; the value 103706 appears nowhere the stub dereferences. Attribution
 * rests on the SMAX symbol name and its call to the SMAXB primitive. */
