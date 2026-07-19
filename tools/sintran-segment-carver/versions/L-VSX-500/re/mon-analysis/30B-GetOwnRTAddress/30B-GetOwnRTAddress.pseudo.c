/* ============================================================================
 * MON 30B  GetOwnRTAddress (GETRT)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Returns the address of the calling program's RT
 * description.  Background programs get the RT-description address of the RT
 * program controlling the terminal.  The result is returned in W1 (the A
 * register on the ND-100; MAC: MON 30 / STA RTPRO).
 *
 * Dispatch reality:
 *   GOTAB[30B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED).  There is no direct
 *   GOTAB handler word, so the level-14 handler is reached through the resident
 *   MFELL/CALLPROC path - NOT present in any carved segment (uncarved bridge).
 *   There is NO ND-100 code worker and NO ND-100 named region for this call:
 *   GETRT=106704B is the ND-500 companion (N500-SYMBOLS).  The model below is
 *   the DOCUMENTED behaviour only, NOT carved code.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Manual parameter contract (from 30B_GetOwnRTAddress.yaml; INFERRED):
 *   RTDescrAddress : OUT  the RT-description address, returned in W1 (A on ND-100). */

int mon_30B_GetOwnRTAddress(mon_regs *r)
{
    /* Documented model (NOT carved: no ND-100 worker body in these bytes).
     * The worker is tiny/inline: it reads the RT-description pointer of the
     * currently running RT program (for a background program, the RT program
     * controlling the terminal) and returns that address in A/W1. */
    rt_description *rt = current_rt_program();       /* running RT (or terminal owner) */
    r->A = rt_description_address(rt);               /* returned in W1 (A on ND-100) */
    return 0;
}

/* Caveats for the emulator author:
 *   - GOTAB[30B]=000000 (fall-through) is BYTE-VERIFIED; there is no entry stub
 *     to model.  Dispatch enters the resident MFELL/CALLPROC (UNCARVED).
 *   - No ND-100 worker or named region exists in the carved segments; the two
 *     lines above are the manual's behaviour only.  GETRT=106704B is the ND-500
 *     companion.
 *   - The exact "current RT description" pointer read lives past the uncarved
 *     bridge and is not byte-derived.
 *   - A live PC trace (break on a real MON 30, single-step the fall-through) is
 *     needed to confirm the real worker.
 */
