/* ============================================================================
 * MON 50B  OpenFile (OPENF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 50B-OpenFile.ASM). Control flow and
 * the call to the FOPEN file-open primitive are BYTE-VERIFIED; the semantic
 * labels (which field is name / type / access) are INFERRED from the SINTRAN
 * III Monitor Calls manual and the field copies seen - treat as a model, not
 * gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   X = address of file-name string   (if 0, name read from terminal)
 *   A = address of default file-type string ; on return A = open file number
 *   T = access code (0..9: seq/random, read/write/append, contiguous, ...)
 * Error return: A = error number. */

int mon_openfile(mon_regs *r)
{
    save_params(r);                        /* 123525: STD I 73 - stash caller D  */
    resident_prologue();                   /* 123531: JPL I 70 -> 003752         */

    if (parse_name(r) != OK)               /* 123536/123541: JPL I -> CLPAR 044777 */
        goto store_status;                 /*   -> 123612 on parse failure         */

    /* 123545-123554: scan the open-file table for a free / matching slot */
    for (slot = 0; slot < limit; slot++)
        if (table[slot].key == r->key) goto found;
    rc = 0104;                             /* 123555: SAA 104 - no free/not found  */
    goto store_status;                     /*   -> 123612                          */

found:
    resident_worker_010500();              /* 123560: JPL I 51 -> 010500           */
    /* 123561-123564: build FOPEN args (X = B + record offset)                  */
    rc = fopen_primitive(r);               /* 123565: JPL I 46 -> FOPEN 067432     */
    if (fopen_no_skip)                     /* 123566: JMP 26 -> 123614 (no-skip=err)*/
        goto store_status_err;
    /* --- skip return = success (123567-123607) ------------------------------*/
    r->D = r->A;                           /* 123567: RADD CLD SA DD (D = A)       */
    resident_worker_010506();              /* 123571: JPL I 43 -> 010506           */
    r->A = r->D;                           /* 123572: RADD CLD SD DA (A = D)       */
    outrc(); octal(); outrc();             /* 123573/123604/123605: OUTRC/OCTAL echo*/
    r->B[4] += 1;                          /* 123607: MIN ,B 4 - success flag      */
    goto resident_ret;                     /* 123610 SAA -110 / 123611 JMP I ->003776*/

store_status:                              /* parse-fail / no-free path (-> 123612) */
    r->status = rc;                        /* 123612: STA ,B 2 -> caller           */
    goto resident_ret;                     /* 123613: JMP -3 -> 123610             */

store_status_err:                          /* FOPEN error return (123614)           */
    r->status = rc;                        /* 123614: STA ,B 2 -> caller           */
    resident_worker_010506();              /* 123616: JPL I 16 -> 010506           */
resident_ret:
    resident_return();                     /* 123611/123617: JMP I -> 003776       */
    return rc;                             /* A = open file number on success       */
}

/* Byte-verified anchors:
 *   OPENF entry 123525, frame SAB 110, prologue JPL I 70 -> 003752,
 *   FOPEN call  JPL I 46 -> 067432, status store STA ,B 2 (123612/123614),
 *   resident return -> 003776. */
