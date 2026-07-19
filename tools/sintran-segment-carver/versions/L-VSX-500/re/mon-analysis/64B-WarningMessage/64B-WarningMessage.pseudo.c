/* ============================================================================
 * MON 64B  WarningMessage (ERMSG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Outputs a file-system error message for the input
 * error code (see appendix A), then RETURNS - the program continues.  The
 * message goes to the terminal; for batch / mode / RT jobs it goes to the error
 * device (normally the console).  Error code 0 is illegal.
 *
 * Dispatch reality:
 *   GOTAB[64B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED): there is no per-call
 *   level-14 stub; dispatch drops into the resident MFELL/CALLPROC second-level
 *   path, which is UNCARVED and NOT byte-followable statically.
 *   ERMSG @16714B (resident commoncode) is the NAMED worker for this call and IS
 *   real executable code (body 16714-17020, closes at EXIT).  It has two entry
 *   points - ERMSG (T:=0) and QERMS @16716B (T:=1) - that merge at 16717.  The
 *   fall-through -> ERMSG link crosses the uncarved CALLPROC, so identity rests
 *   on the symbol NAME (ERMSG) + the dual-entry shape - see README caveats.
 *
 * The ERMSG worker body is BYTE-VERIFIED (see 64B-WarningMessage.ASM); the
 * output-routing semantics below are INFERRED from the manual
 * (64B_WarningMessage.yaml).  Addresses in comments are octal.
 * ============================================================================ */

/* (B) ERMSG worker @16714B (resident commoncode).  REAL executable code: sets a
 * flavour flag, stashes the caller error code, looks up the message and writes
 * it out, then EXITs (the caller program continues).  Control flow is
 * BYTE-VERIFIED; the message-routing detail is INFERRED from the manual. */
int mon_64B_WarningMessage(mon_regs *r)      /* in: A = ErrCode (0 is illegal) */
{
    /* 16714 RADD CLD 0 DT : ERMSG entry, T := 0 (warning/continue flavour).
     * 16716 SAT 1         : QERMS entry, T := 1 (alternate flavour).
     * Both merge at 16717.                                                     */
    int flavour = 0;                          /* ERMSG=0 ; QERMS entry would set 1 */

    /* 16717-16726: store the flavour flag and the caller error code through the
     *              worker's pointer cells (STT I / STA I).                      */
    int err = r->A_ErrCode;
    /* NOTE: error code 0 is illegal (manual); the caller/wrapper is expected to
     * reject it - not re-checked in this carved body.                          */

    /* 16727-16746: JAF/JAZ branches select the message source and format path
     *              (JPL I -> 17027 / 17031 / 17032 format-and-lookup workers).  */
    /* 16747-16765: SAT 14 / JPL I -> 17033 / 17035 : format the message text.   */
    /* 16766-17011: JAZ tests then STATX (17011): phys[EL] = A, with
     *              EL = ((T & 0377) << 16) | ((X + 0) & 0177777) - a physical
     *              store (page tables bypassed) that patches a resident cell.
     *              INFERRED to steer output to the correct device (terminal for
     *              interactive; error device for batch/mode/RT).                */
    format_and_output_error_message(err, flavour);   /* INFERRED routing */

    /* 17012-17020: LDT I 11 / RADD / LDF ,B 11 / LDX I 10 / RADD / LDX ,B 10 /
     *              EXIT : restore context and return - the program CONTINUES.   */
    return 0;                                 /* control returns to the caller */
}

/* Caveats for the emulator author:
 *   - GOTAB[64B]=0 is a BYTE-VERIFIED fall-through; there is no stub to follow.
 *     The MON 64 -> worker transfer is via the UNCARVED resident MFELL/CALLPROC,
 *     so the fall-through -> ERMSG hop is NOT byte-followable here.
 *   - ERMSG=16714B IS real executable code (body 16714-17020, EXIT), with two
 *     entry points ERMSG (T=0) / QERMS=16716B (T=1).  Its attribution to MON 64
 *     rests on the symbol NAME (ERMSG = ERror MeSsaGe) + the dual-entry shape,
 *     not a followed pointer - hence status `partial` (fall-through).
 *   - The output routing (terminal vs error device) and the appendix-A message
 *     table are manual-derived (64B_WarningMessage.yaml), not byte-isolated here.
 *   - A live PC trace (issue a real MON 64, single-step the level-14 fall-through
 *     into the resident CALLPROC) is needed to confirm P lands on ERMSG=16714.
 */
