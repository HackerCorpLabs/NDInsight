/* ============================================================================
 * MON 43B  CloseFile (CLOSF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 43B-CloseFile.ASM). Control flow and
 * the call to the FCLOS file-close primitive are BYTE-VERIFIED; the semantic
 * label (the single file-number argument) is INFERRED from the SINTRAN III
 * Monitor Calls manual - treat as a model, not gospel. Addresses in comments
 * are octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = file number returned when the file was opened
 *        -1 = close all files not permanently open
 *        -2 = close all files including scratch and permanently open files
 * Error return: A = error number. */

int mon_closefile(mon_regs *r)
{
    save_params(r);                        /* 123741: STD I 26 - stash caller D  */
    resident_prologue();                   /* 123745: JPL I 23 -> 003752         */

    if (parse_arg(r) != OK)                /* 123747: JPL I 23 -> CLPAR 044777   */
        goto parse_fail;                   /*   123750 JMP -> 123763 (no-skip)   */

    resident_worker_010500();              /* 123752: JPL I 22 -> 010500         */
    r->T = r->D;                           /* 123753: RADD CLD SD DT (T = D)     */
    rc = fclose_primitive(r);              /* 123754: JPL I 21 -> FCLOS 067612   */
    /* FCLOS is a skip-return primitive: the SKIP return (falls to 123756) is    */
    /* success and bumps the caller's skip flag; the no-skip return (123755 JMP  */
    /* -> 123765) is the error path - same polarity as CLPAR above and MON 62's  */
    /* MIN ,B 4 success bump. (skip convention INFERRED, not byte-proven.)       */
    if (fclose_no_skip)                    /* 123755: JMP 10 -> 123765 (no-skip) */
        goto store_status_err;
    r->B[4] += 1;                          /* 123756: MIN ,B 4 - success flag    */
finish:
    resident_worker_010506();              /* 123757/123760: JPL I 16 -> 010506  */
    resident_return();                     /* 123761/123762: JMP I -> 003776     */
    return rc;

parse_fail:
    r->status = rc;                        /* 123763: STA ,B 2 -> caller         */
    resident_return();                     /* 123764: JMP -3 -> 123761           */
    return rc;

store_status_err:
    r->status = rc;                        /* 123765: STA ,B 2 -> caller         */
    goto finish;                           /* 123766: JMP -7 -> 123757           */
}

/* Byte-verified anchors:
 *   CLOSF entry 123741, frame SAB 6, prologue JPL I 23 -> 003752,
 *   FCLOS call  JPL I 21 -> 067612, status store STA ,B 2 (123763/123765),
 *   resident return -> 003776. */
