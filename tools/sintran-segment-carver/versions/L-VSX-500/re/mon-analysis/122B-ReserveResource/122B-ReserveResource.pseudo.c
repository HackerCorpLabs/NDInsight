/* ============================================================================
 * MON 122B - RESRV / ReserveResource - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07. Derived from the carved bytes in
 * 122B-ReserveResource.ASM. The dispatch chain and the worker's control flow
 * (the two MON 70 CallCommand dispatches and the byte-scan loop) are VERIFIED
 * from bytes; the exact resource-table semantics are INFERRED from the manual.
 *
 * CORRECTED 2026-07-13. The previous version declared RESRV "not carved /
 * zero-filled" - it read 037103B in SINTRAN-DATA_commoncode (zeros there, wrong
 * overlay). The real worker is carved in 003-S3CP.
 *
 * Dispatch: MON 122B -> ENT14 072167B -> GOTAB[122B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[122B] @005742B = 037103B = RESRV (worker below).
 * All constants octal.
 * ============================================================================
 */

/* VERIFIED from bytes: RESRV is a thin wrapper that builds a SINTRAN command
 * line and runs it through the command processor via MON 70B (CallCommand). It
 * does this inside a byte-scan loop (LBYT / compare 47B) that walks the string.
 * The reserve/release pair (122B RESRV / 123B RELES) sit adjacently in 003-S3CP
 * and share this command-issuing infrastructure. */

int RESRV(mon_regs *r)              /* entry 037103B; A = command/param pointer */
{
    if (!execc_flag())              /* 037110B-037111B: LDA I 53 / JAF */
        return;                     /* 037112B: EXIT (early return) */

    word link = r->L;               /* 037113B: T := L (save the return link) */
    word cmd  = mem[046];           /* 037116B: LDA 46 (command-string base) */

    build_command(cmd);             /* 037120B: JPL I 45 (helper builds line) */
    r->L = link;                    /* 037121B: L := T */
    mon_call(070, mem[044]);        /* 037124B: MON 70 - run the RESERVE command */

    /* 037125B-037155B: scan the string byte-by-byte (LBYT), and for each field
     * delimited by 47B re-issue MON 70B. INFERRED: this reserves each named
     * resource part in turn (a device may have separate input/output parts). */
    for (;;) {
        int x = 0;                          /* 037125B: X := 0 */
        while (scan_byte(&x) != 047) { }    /* 037126B-037137B: find delimiter 47B */
        word b = compute_next(x);           /* 037140B-037146B */
        if (scan_byte(&x) == 047)           /* 037147B-037151B */
            break;                          /* 037152B: -> RELES boundary (done) */
        mon_call(070, b);                   /* 037154B: MON 70 again */
    }
    /* returns to caller with the standard error code (INFERRED). */
}

/* Caveats for the emulator author:
 *   - The MON 70B (CallCommand) dispatches and the byte-scan loop are byte-proven.
 *   - The resource-table layout, and the manual's parameter list
 *     (DeviceNo / IOFlag / WaitFlag / Status), are INFERRED - they are not
 *     isolated in these bytes; RESRV issues a text RESERVE command, so the actual
 *     locking is done inside the command processor, not here.
 *   - Faithful emulation must actually run the RESERVE command's side effect if
 *     downstream code depends on the reservation.
 */
