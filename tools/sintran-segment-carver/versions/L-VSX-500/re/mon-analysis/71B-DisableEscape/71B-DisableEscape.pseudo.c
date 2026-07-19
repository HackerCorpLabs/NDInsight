/* ============================================================================
 * MON 71B  DisableEscape (documented DESCF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Disables the terminal ESCAPE (user-break) function so
 * the escape character is treated as any other character.  Re-enabled by MON 72
 * (EnableEscape) or automatically at log-out.
 *
 * Dispatch reality:
 *   GOTAB[71B] = 121417B (NON-ZERO, BYTE-VERIFIED) -> F1643 entry stub in overlay
 *   025-S3IRPIT.  In this real SINTRAN L image the 3-word F1643 stub
 *   (121417B-121421B, bounded by NOWTS=121422B) is ALL ZERO: it is a set of
 *   runtime-populated pointer cells (zero at rest), so the onward transfer to the
 *   actual disable-escape routine is NOT byte-followable from a static decode.
 *   The named ND-100 worker is not isolated in any carved segment (the DESCF
 *   symbol resolves only to the ND-500 side).  The model below is therefore of
 *   the DOCUMENTED behaviour only (from the manual), NOT of carved code.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Documented model (NOT carved: F1643 is zero/runtime-cells; no worker body is
 * present in these bytes). DisableEscape and EnableEscape (MON 72) are a pair
 * that likely share one terminal-flag body forked on a disable/enable selector -
 * but that is INFERRED from the call pairing, not proven from these bytes. */
int mon_71B_DisableEscape(mon_regs *r)          /* T = DeviceNumber */
{
    /* Parameters (from 71B_DISABLEESCAPE.yaml; INFERRED, not byte-isolated):
     *   DeviceNumber (INTEGER, in, T): terminal's logical device number.
     *     Ignored for background programs (own terminal is always selected). */

    terminal *tt = select_terminal(r->T);       /* own terminal for background progs */

    tt->escape_enabled = 0;                      /* disable user-break: ESC becomes an */
                                                 /* ordinary input character           */
    return 0;                                     /* + standard error code in A */
}

/* Caveats for the emulator author:
 *   - GOTAB[71B]=121417B (non-zero, -> F1643) is BYTE-VERIFIED; but the F1643
 *     stub is 3 ZERO words in this carved L image (runtime pointer cells), so
 *     NONE of the body above is byte-derived - it is the manual's behaviour only.
 *   - The disable/enable shared-body hypothesis (MON 71 <-> MON 72) is INFERRED
 *     from the documented pairing, NOT proven.
 *   - A live PC trace (break on a real MON 71, single-step through F1643 and the
 *     resident CALLPROC) is needed to confirm the real worker.
 */
