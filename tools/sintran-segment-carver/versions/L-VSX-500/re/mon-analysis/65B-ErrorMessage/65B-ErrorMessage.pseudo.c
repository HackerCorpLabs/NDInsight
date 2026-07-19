/* ============================================================================
 * MON 65B  ErrorMessage (QERMS)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Displays a file-system error message for the input
 * error number (see appendix A), then TERMINATES the program.  The message goes
 * to the terminal; RT programs write it to the error device (normally the
 * console).  Error number 0 is illegal.
 *
 * SIBLING of MON 64B WarningMessage: one shared worker body, two entry points -
 *   ERMSG @16714B (T:=0) = MON 64B WarningMessage (program CONTINUES)
 *   QERMS @16716B (T:=1) = MON 65B ErrorMessage   (program TERMINATES)
 * both merge at 16717B.
 *
 * Dispatch reality:
 *   GOTAB[65B] = 121345B = F1641 (BYTE-VERIFIED), an F16xx level-14 stub in
 *   025-S3IRPIT.  The stub's own edges stay inside 025-S3IRPIT (JPL I -> 121414,
 *   JMP -> 121402, shared F16xx family code); the transfer to the commoncode
 *   QERMS worker is the resident CALLPROC / segment switch, which is UNCARVED
 *   and NOT byte-followable statically.  QERMS @16716B IS real executable code
 *   (shared body 16714-17020, closes at EXIT).  Identity rests on the symbol
 *   NAME (QERMS = the 65B short name) + the dual-entry shape - see README caveats.
 *
 * The worker body is BYTE-VERIFIED (see 65B-ErrorMessage.ASM); the T-flag
 * continue-vs-terminate policy and the output routing are INFERRED from the
 * manual (65B_ErrorMessage.yaml).  Addresses in comments are octal.
 * ============================================================================ */

/* (A) F1641 stub @121345B (025-S3IRPIT).  BYTE-VERIFIED entry.  Two-word head
 * calls a shared F16xx routine and jumps into shared family tail code; the
 * transfer to the QERMS worker is the uncarved CALLPROC (not modelled here). */
void dispatch_F1641(mon_regs *r)
{
    /* 121345 JPL I 47 : L := 121346; PC := mem[121345+47] = *(121414) - call
     *                   shared F16xx routine (returns to 121346).
     * 121346 JMP 34   : PC := 121402 - shared F16xx family tail (leaves stub).
     * 121347-121366 : shared continuation reached via those family routines
     *                 (STATX physical stores, ,B 17 / ,B 46 counter updates) -
     *                 NOT reached from the F1641 entry directly. */
    call_shared_F16xx(r);            /* -> 121414, then 121402 (uncarved bridge) */
}

/* (B) QERMS/ERMSG worker @16716B (resident commoncode).  REAL executable code
 * shared with MON 64B.  Sets the flavour flag (T=1 for QERMS/error, T=0 for
 * ERMSG/warning), stashes the caller error number, looks up the message and
 * writes it out.  Control flow is BYTE-VERIFIED; the message-routing and the
 * error-terminate policy are INFERRED from the manual. */
int mon_65B_ErrorMessage(mon_regs *r)        /* in: A = ErrNumber (0 is illegal) */
{
    /* 16714 RADD CLD 0 DT : ERMSG entry (MON 64B), T := 0 (warning/continue).
     * 16716 SAT 1         : QERMS entry (MON 65B), T := 1 (error/terminate).
     * Both merge at 16717.  MON 65B enters at QERMS, so flavour = 1.            */
    int flavour = 1;                         /* QERMS=1 (error) ; ERMSG entry = 0 */

    /* 16717-16726: store the flavour flag and the caller error number through the
     *              worker's pointer cells (STT I / STA I).                       */
    int err = r->A_ErrNumber;
    /* NOTE: error number 0 is illegal (manual); the caller/wrapper is expected to
     * reject it - not re-checked in this carved body.                           */

    /* 16727-16746: JAF/JAZ branches select the message source and format path
     *              (JPL I -> 17027 / 17031 / 17032 format-and-lookup workers).   */
    /* 16747-16765: SAT 14 / JPL I -> 17033 / 17035 : format the message text.    */
    /* 16766-17011: JAZ tests then STATX (17011): phys[EL] = A, with
     *              EL = ((T & 0377) << 16) | ((X + 0) & 0177777) - a physical
     *              store (page tables bypassed) that patches a resident cell.
     *              INFERRED to steer output to the correct device (terminal for
     *              interactive; error device for RT).                            */
    format_and_output_error_message(err, flavour);   /* INFERRED routing */

    /* 17012-17020: LDT I 11 / RADD / LDF ,B 11 / LDX I 10 / RADD / LDX ,B 10 /
     *              EXIT : restore context and return through the level-14 frame.
     * The visible tail returns via EXIT; the ErrorMessage (QERMS) program-
     * TERMINATE behaviour is driven by the T=1 flavour stored at 16717 and taken
     * in one of the indirect JPL I workers (uncarved) - INFERRED from the manual,
     * NOT isolated in this window (which returns for BOTH entries).             */
    return 0;
}

/* Caveats for the emulator author:
 *   - GOTAB[65B]=121345B=F1641 is BYTE-VERIFIED, but the stub->worker transfer is
 *     via the UNCARVED resident CALLPROC; the F1641 -> QERMS hop is NOT
 *     byte-followable here (the stub's own branches stay in 025-S3IRPIT).
 *   - QERMS=16716B IS real executable code, the T=1 entry of a body SHARED with
 *     MON 64B WarningMessage (ERMSG=16714B, T=0).  Its attribution to MON 65B
 *     rests on the symbol NAME (QERMS = the 65B manual short name) + the dual-
 *     entry shape, not a followed pointer - hence status `partial`.
 *   - The continue (warning) vs terminate (error) split is driven by the T flag
 *     (0 vs 1) but the terminate path is NOT byte-isolated in the carved window;
 *     it is manual-derived (65B_ErrorMessage.yaml), as is the output routing.
 *   - A live PC trace (issue a real MON 65, break at 121345, single-step the
 *     segment switch) is needed to confirm P lands on QERMS=16716.
 */
