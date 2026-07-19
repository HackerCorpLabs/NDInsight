/* ============================================================================
 * MON 4B  SetBreak (BRKM)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Sets the break characters for a terminal.  A program
 * normally waits for input; when a break character is typed the program
 * restarts (e.g. most subsystems break on RETURN).  Break strategy:
 *   <0 = none, 0 = all, 1 = control chars, 2 = MAC, 3..6 = system tables,
 *   7 = user 128-bit break table, 8 = last user table, 9 = max chars only.
 * For RT programs a logical device number selects the terminal.
 *
 * Dispatch reality:
 *   GOTAB[4B] = 000000B (BYTE-VERIFIED zero) -> FALL-THROUGH: there is NO direct
 *   handler stub for MON 4 in overlay 025-S3IRPIT.  Level-14 dispatch falls
 *   through the resident MFELL / CALLPROC machinery (UNCARVED) to the worker -
 *   NOT byte-followable statically.
 *   BRKM @044425B (resident commoncode) is the NAMED worker for this call and IS
 *   real executable code (the SetBreak entry into the shared break/echo
 *   terminal-message module).  The dispatch->BRKM link, however, crosses the
 *   uncarved MFELL / CALLPROC, so identity rests on the symbol NAME (BRKM) -
 *   see README caveats.
 *
 * The BRKM worker body is BYTE-VERIFIED (see 4B-SetBreak.ASM); the parameter
 * mapping (device / strategy / table / max-count) is INFERRED from the manual.
 * Addresses in comments are octal.
 * ============================================================================ */

/* (A) Level-14 dispatch for MON 4.  GOTAB[4]=0 => fall-through: the resident
 * MFELL / CALLPROC path (uncarved) reaches the worker.  Modelled as BRKM below. */
int mon_4B_SetBreak(mon_regs *r)              /* in: DeviceNo, BreakStrategy, Table, NoOfChar */
{
    /* No entry stub exists (GOTAB[4]=0).  The concrete worker that installs the
     * break table lives past the uncarved MFELL/CALLPROC; here it is modelled as
     * BRKM below.                                                              */
    return brkm_set_break(r);                 /* UNVERIFIED bridge -> (B) */
}

/* (B) BRKM worker @044425B (resident commoncode).  REAL executable code: it
 * scans/stages the break request (indirect indexed loads, compare/skip loops),
 * calls shared terminal-message helpers (JPL I 112 -> [044612], JPL I 73 ->
 * [044606]) and spins on in-body wait loops, then returns through the frame
 * pointer (JMP I ,B -30).  Control flow is BYTE-VERIFIED; the field semantics
 * below are INFERRED from the manual. */
int brkm_set_break(mon_regs *r)
{
    /* 044425-044442: IOT/RADD setup then an indirect-indexed scan loop
     *                (LDA I ,B ,X -37 / SKP IF DA UEQ ST / JMP -12) walking the
     *                caller's parameter/table words.                          */
    /* 044443 JMP 150 -> 044613 : branch into the shared break/echo module.     */
    /* 044447-044506: build the break-table words (MPY ,B -44 index math, STA ,B
     *                -42/-43/-41 slots), call the shared helper (JPL I 112 ->
     *                [044612]), wait (JMP -2) and return (JMP I ,B -30).       */
    /* 044507-044537: second staging block (SSSLE/SSLEA) - apply strategy / user
     *                table / max count to the terminal's break control words.  */

    terminal *t = select_terminal(r->DeviceNo);      /* RT: by device; bg: own  */
    t->break_strategy = r->BreakStrategy;             /* <0..9 per manual        */
    if (r->BreakStrategy == 7)
        install_user_break_table(t, r->Table);        /* 128-bit / 8-word table  */
    if (r->BreakStrategy >= 3)
        t->max_chars = r->NoOfChar;                   /* max chars before break  */
    return 0;                                         /* + standard error code    */
}

/* Caveats for the emulator author:
 *   - GOTAB[4B]=0 is BYTE-VERIFIED (a zero word) => MON 4 is a FALL-THROUGH with
 *     no entry stub; the MON 4 -> worker transfer is via the UNCARVED resident
 *     MFELL / CALLPROC, so the dispatch->BRKM hop is NOT byte-followable here.
 *   - BRKM=044425B IS real executable code (the SetBreak entry of the shared
 *     break/echo terminal-message module).  Its attribution to MON 4 rests on the
 *     symbol NAME (BRKM) + position, not a followed pointer - hence the folder
 *     status is `partial` (fall-through dispatch head; worker by name).
 *   - The parameter mapping (device / strategy / table / max-count) is
 *     manual-derived (4B_SetBreak.yaml), not byte-isolated from this carve.
 *   - The worker branches/calls into shared cells (044606/044612/044613) past the
 *     carved window; those are a shared message helper / the adjacent block, not
 *     statically resolvable here.
 *   - A live PC trace (break on a real MON 4, single-step through MFELL/CALLPROC)
 *     is needed to confirm P lands on BRKM=044425.
 */
