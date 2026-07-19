/* ============================================================================
 * MON 72B  EnableEscape (documented EESCF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Re-enables the terminal ESCAPE (user-break) function
 * after MON 71 (DisableEscape).  The ESCAPE key normally terminates a program;
 * the function is also enabled automatically at log-out.
 *
 * Dispatch reality:
 *   GOTAB[72B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED).  There is no direct
 *   GOTAB handler word, so the level-14 handler is reached through the resident
 *   MFELL/CALLPROC path - NOT present in any carved segment (uncarved bridge).
 *   The named ND-100 worker is not isolated in any carved segment: the EESCF
 *   short name resolves only to the ND-500 side (112123B, N500-SYMBOLS), which
 *   is not the ND-100 body.  The model below is therefore of the DOCUMENTED
 *   behaviour only (from the manual), NOT of carved code.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Documented model (NOT carved: no ND-100 enable-escape worker is isolated).
 * EnableEscape and DisableEscape (MON 71) are a pair that likely share one
 * terminal-flag body forked on an enable/disable selector - but that is
 * INFERRED from the call pairing, not proven from these bytes. */
int mon_72B_EnableEscape(mon_regs *r)           /* T = DeviceNumber */
{
    /* Parameters (from 72B_EnableEscape.yaml; INFERRED, not byte-isolated):
     *   DeviceNumber (INTEGER2, in, T): terminal's logical device number.
     *     Ignored for background programs (own terminal is always selected). */

    terminal *tt = select_terminal(r->T);       /* own terminal for background progs */

    tt->escape_enabled = 1;                      /* re-enable user-break: ESC once more */
                                                 /* terminates the running program      */
    return 0;                                     /* + standard error code in A */
}

/* Caveats for the emulator author:
 *   - GOTAB[72B]=000000 (fall-through) is BYTE-VERIFIED; there is no entry stub
 *     to model.  Dispatch enters the resident MFELL/CALLPROC (UNCARVED).
 *   - No ND-100 enable-escape worker is isolated in the carved set (EESCF is
 *     ND-500 only), so NONE of the body above is byte-derived - it is the
 *     manual's behaviour only.
 *   - The enable/disable shared-body hypothesis (MON 72 <-> MON 71) is INFERRED
 *     from the documented pairing, NOT proven.
 *   - A live PC trace (break on a real MON 72, single-step the fall-through and
 *     CALLPROC) is needed to confirm the real worker.
 */
