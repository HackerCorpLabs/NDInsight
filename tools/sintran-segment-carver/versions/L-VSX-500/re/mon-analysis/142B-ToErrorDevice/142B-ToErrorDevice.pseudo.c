/* ============================================================================
 * MON 142B  ToErrorDevice (documented ERMON)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Outputs a user-defined real-time error (numbers 50-69)
 * with a suberror number on the error device (normally the console), e.g.:
 *   23.10.59 ERROR 59 AT XPROG AT 134562, USER ERROR, SUBERROR 4
 *
 * Dispatch reality:
 *   GOTAB[142B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED).  There is no direct
 *   GOTAB handler word, so the level-14 handler is reached through the resident
 *   MFELL/CALLPROC path - NOT present in any carved segment (uncarved bridge).
 *   The named ND-100 worker is not isolated in any carved segment: the ERMON
 *   short name resolves only to the ND-500 side (114574B, N500-SYMBOLS), which
 *   is not the ND-100 body.  The model below is therefore of the DOCUMENTED
 *   behaviour only (from the manual), NOT of carved code.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Documented model (NOT carved: no ND-100 to-error-device worker is isolated).
 * The error device is the terminal the system uses for error output (see MON 254
 * GetErrorDevice); this call formats and writes the RT error line to it. */
int mon_142B_ToErrorDevice(mon_regs *r)         /* A = ErrorNumber, T = SubErrorNumber */
{
    /* Parameters (from 142B_ToErrorDevice.yaml; INFERRED, not byte-isolated):
     *   ErrorNumber    (INTEGER, in, A): error number, range 50-69, printed after
     *                  "ERROR" (MAC example passes it as two ASCII characters).
     *   SubErrorNumber (INTEGER, in, T): suberror number, printed after "SUBERROR".
     * Available to user RT and user SYSTEM programs (RT programs). */

    int err = r->A;                              /* error number 50-69 */
    int sub = r->T;                              /* suberror number    */

    if (err < 50 || err > 69)
        return ERR_ILLEGAL_ERROR_NUMBER;         /* only 50-69 allowed (inferred) */

    device *ed = error_device();                 /* the current error device (MON 254) */
    format_rt_error(ed, current_program(), err, sub);  /* "... ERROR err AT prog AT ... SUBERROR sub" */
    return 0;                                     /* + standard error code in A */
}

/* Caveats for the emulator author:
 *   - GOTAB[142B]=000000 (fall-through) is BYTE-VERIFIED; there is no entry stub
 *     to model.  Dispatch enters the resident MFELL/CALLPROC (UNCARVED).
 *   - No ND-100 to-error-device worker is isolated in the carved set (ERMON is
 *     ND-500 only), so NONE of the body above is byte-derived - it is the
 *     manual's behaviour only.
 *   - A live PC trace (break on a real MON 142, single-step the fall-through and
 *     CALLPROC) is needed to confirm the real worker.
 */
