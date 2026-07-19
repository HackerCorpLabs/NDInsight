/* ============================================================================
 * MON 123B - RELES / ReleaseResource - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07. Derived from the carved bytes in
 * 123B-ReleaseResource.ASM. The dispatch chain and the worker prologue are
 * VERIFIED from bytes; the resource-table semantics are INFERRED from the manual.
 *
 * CORRECTED 2026-07-13. The previous version declared RELES "not carved /
 * zero-filled" - it read 037156B in SINTRAN-DATA_commoncode (zeros there, wrong
 * overlay). The real worker is carved in 003-S3CP, right after RESRV (MON 122B).
 *
 * Dispatch: MON 123B -> ENT14 072167B -> GOTAB[123B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[123B] @005743B = 037156B = RELES (worker below).
 * All constants octal.
 * ============================================================================
 */

/* VERIFIED from bytes: RELES is a very short handler - it saves the return link
 * (X := L), calls a shared release helper (JPL I 10), stages a release argument,
 * and returns through a computed jump (P := X). The actual freeing of the
 * reservation happens inside the shared helper / command path. RELES is the
 * release half of the RESRV (122B) / RELES (123B) pair in 003-S3CP. */

int RELES(mon_regs *r)              /* entry 037156B */
{
    word link = r->L;               /* 037156B: X := L (save the return link) */

    release_helper();               /* 037157B: JPL I 10 (shared release path) */

    word arg = mem_B[-0140];        /* 037160B: LDA ,B -140 */
    mem_B[-0141] = arg;             /* 037161B: STA ,B -141 (stage release arg) */

    goto_addr(link);                /* 037162B: P := X (computed return / tail) */
}

/* Documented model of what the shared release path does (INFERRED, not isolated
 * in these bytes):
 *
 *   reservation_slot *slot = resource_slot(DeviceNumber, IOFlag);
 *   if (slot->owner == current_rt_program()) {
 *       slot->owner = 0;            // free the reservation
 *       wake_waiters(slot);         // let a MON 122 waiter proceed
 *   }
 */

/* Caveats for the emulator author:
 *   - The RELES prologue (save link / helper call / computed return) is byte-proven.
 *   - DeviceNumber / IOFlag and the slot-freeing are INFERRED from the manual; the
 *     freeing is done inside the shared helper, not visible in the RELES prologue.
 *   - MON 122B (RESRV) and MON 123B (RELES) are a reserve/release pair sharing
 *     infrastructure in 003-S3CP.
 */
