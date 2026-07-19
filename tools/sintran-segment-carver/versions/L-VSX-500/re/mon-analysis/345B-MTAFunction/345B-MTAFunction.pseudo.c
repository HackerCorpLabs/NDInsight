/* ============================================================================
 * MON 345B  MTAFunction (MTAD)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  GOTAB[345] = 000000 (fall-through).
 *
 * MON 345B has a ZERO GOTAB slot, so it is a level-14 FALL-THROUGH: the handler
 * is reached through the resident MFELL/CALLPROC path, which lives in an
 * UNCARVED overlay. The MTA terminal-line worker (manual short name MTAD;
 * connect/disconnect a terminal line to a datafield, manual sections 2.14/2.17)
 * is NOT statically isolated from these bytes. The candidate region MTSTA
 * (025-S3IRPIT @64636B) is real code that drives terminal-line device status
 * via STATX, but the fall-through -> MTSTA link is NOT byte-proven.
 *
 * This model is of the DOCUMENTED behaviour only, NOT carved code. The
 * fall-through bridge is modelled but not proven. Addresses in comments octal.
 * ============================================================================ */

/* Documented MON 345B MTAFunction (manual sections 2.14 / 2.17):
 *   Connect or disconnect a terminal line to/from a datafield. Internal-use
 *   call; the exact register/CALLG mapping is NOT confirmed from these bytes. */
int mon_mta_function(mon_regs *r)
{
    /* GOTAB[345] = 0 -> resident MFELL/CALLPROC fall-through (uncarved).
     * The real worker (yaml: MTSTART, MP-P2-TERM-DRIV) connects/disconnects a
     * terminal line to a datafield. Its exact body is not recoverable here. */
    return mta_connect_or_disconnect(r);   /* UNVERIFIED: worker not carved */
}

/* CANDIDATE terminal-line status routine MTSTA (025-S3IRPIT @64636B) - real
 * bytes, shown for reference; NOT a proven MON 345B worker. It writes two
 * terminal-line status words to the device with STATX (physical device-status
 * transfer) and returns through a caller link word. */
void mtsta_candidate(mta_field *B)
{
    mta_setup(B);                       /* 064636 JPL I 26 -> [064664]           */
    phys_status_out(B->slot21);         /* 064637 LDA ,B 21 ; 064640 STATX        */
    phys_status_out(B->slot23);         /* 064641 LDA ,B 23 ; 064642 STATX        */
    /* 064644-064647: return through caller link word [064707]/[064711]. */
}

/* Caller (INFERRED, manual sections 2.14 / 2.17; NOT byte-proven here):
 *   MON 345B MTAFunction / MTADFunction (short name MTAD): internal-use call to
 *   connect or disconnect a terminal line to a datafield. Because GOTAB[345] is
 *   zero, dispatch enters the resident fall-through handler and the worker is
 *   not reachable from these bytes.
 */
