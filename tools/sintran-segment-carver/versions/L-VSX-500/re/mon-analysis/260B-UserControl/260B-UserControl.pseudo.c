/* ============================================================================
 * MON 260B - USCNT / UserControl - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07. The manual lists this call name-only ("UserControl",
 * short name USCNT), so the parameter/return contract is INFERRED; only the
 * carved control flow is VERIFIED.
 *
 * Derived from the carved bytes in 260B-UserControl.ASM. The worker heads a
 * user/system accounting family (USCNT / SYCNT / NRETM / NCHBU) that updates
 * per-user and per-system counters.
 *
 * CORRECTED 2026-07-13. The previous version treated USCNT as name-only /
 * not-carved under the wrong dispatch model. The real worker is carved in
 * 003-S3CP.
 *
 * Dispatch: MON 260B -> ENT14 072167B -> GOTAB[260B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[260B] @006100B = 047120B = USCNT (worker below).
 * All constants octal.
 * ============================================================================
 */

void USCNT(mon_regs *r)             /* entry 047120B; X = per-user block base */
{
    /* 047120B-047124B: bump a per-user counter and copy a paired word. */
    mem[r->X + 0115]++;             /* 047120B: MIN ,X ,B 115 */
    /* STD 105 ; LDT I 122 ; LDA I ,B ,X 122 ; STD I ,B ,X 0 */

    /* 047125B-047146B: SYCNT/NRETM - update the matching system-side counters
     * and re-stage several accounting words. (INFERRED: this reconciles the
     * per-user figures with the system totals.) */

    if (accounting_terminates())    /* one branch reaches: */
        mon_call(0);                /* 047147B: MON 0 - ExitFromProgram */

    /* 047150B-047166B: NCHBU - charge-back / update the remaining counters and
     * store the results (STD I ,B ,X 0 at 047166B). */
}

/* Caveats for the emulator author:
 *   - The counter-increment structure (MIN on ,X/,B offsets) and the MON 0 branch
 *     are byte-proven; the family USCNT/SYCNT/NRETM/NCHBU is a coherent accounting
 *     cluster in 003-S3CP.
 *   - "Set user control of a device" is the manual's one-line description; the
 *     exact parameters and which counters correspond to which resource are
 *     INFERRED, not isolated in these bytes.
 */
