/* ============================================================================
 * MON 316B - SetRemoteAccess (SLRMO) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 316B-SetRemoteAccess.ASM.
 * The helper call, the zero-test fork and the store of the Mode argument are
 * VERIFIED from bytes; the identity of the resident remote-access flag is INFERRED.
 *
 * Dispatch: MON 316B -> ENT14 072167B -> GOTAB[316B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[316B] @006136B = 027530B = SLRMO (worker below).
 * All constants octal.
 * ============================================================================
 */

void SLRMO(int mode)                    /* entry 027530B; Mode in ,B 12 */
{
    /* 027530B-027532B: call a small helper, then read the state word it returns. */
    word state = remote_access_helper();   /* 027530B: JPL I 6 -> 027536 */

    if (state == 0)                        /* 027532B: JAZ - precondition not met */
        return;                            /* skip the store (027535B: JMP I 4) */

    /* 027533B-027534B: store the caller's Mode through the resident pointer @4.
     * mode == 0 clears COSMOS remote file access; mode == 1 enables it. */
    *remote_access_flag = mode;            /* 027534B: STA I 4  (INFERRED target) */

    /* 027535B: JMP I 4 - return. */
}
