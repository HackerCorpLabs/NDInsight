/* ============================================================================
 * MON 143B - RSIO / ExecutionInfo - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07. Returns information about how the calling program is
 * executing (execution mode - interactive / batch / RT, command-input device,
 * command-output device, owner's directory+user index).
 *
 * Derived from the carved bytes in 143B-ExecutionInfo.ASM. The dispatch chain and
 * the worker's control flow are VERIFIED from bytes; the exact field meanings are
 * INFERRED from the manual.
 *
 * CORRECTED 2026-07-13. The previous version located the worker via a fictional
 * "GOTAB" F1670 stub and read RSIO from SINTRAN-DATA_commoncode. The real worker
 * is carved in 003-S3CP.
 *
 * Dispatch: MON 143B -> ENT14 072167B -> GOTAB[143B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[143B] @005763B = 051430B = RSIO (worker below).
 * All constants octal.
 * ============================================================================
 */

int RSIO(mon_regs *r)               /* entry 051430B; A = param base in */
{
    word frame = r->B;              /* 051430B: D := B (save frame/param base) */
    r->B = r->A;                    /* 051431B-051432B: B := A (work base) */

    word info;
    if (flag(mem_B[-0103])) {       /* 051433B-051434B: JAF on ,B -103 */
        info = mem_B[-0147];        /* 051435B: LDA ,B -147 (direct source) */
        /* T := info (051436B) */
    } else {
        /* 051440B-051443B: alternate source - chase a per-program table */
        word x = mem_B[-0146];      /* 051440B: LDX ,B -146 */
        /* T := mem[x+26]; X := mem[x+12]; A := mem[x+23] */
        info = mem[mem[x + 012] + 023];
    }

    r->B = frame;                   /* 051445B: B := D (restore param base) */
    return info;                    /* 051446B-051451B: A/D := result, EXIT */
}

/* Caveats for the emulator author:
 *   - The prologue (base swap), the two-way source select and the EXIT are
 *     byte-proven.
 *   - Which execution-info fields (mode / cmd-in dev / cmd-out dev / dir+user
 *     index) map to ,B -103 / -147 / -146 and the chased table offsets (26/12/23)
 *     is INFERRED from the manual, not isolated in these bytes.
 */
