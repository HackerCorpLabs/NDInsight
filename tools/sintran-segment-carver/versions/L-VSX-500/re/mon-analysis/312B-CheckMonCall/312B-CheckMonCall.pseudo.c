/* ============================================================================
 * MON 312B - MOINF / CheckMonCall - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07. Capability probe: "is monitor call N implemented in
 * this system, and if so what is its dispatch entry?" Optional monitor calls are
 * included or omitted when SINTRAN is generated.
 *
 * FULLY VERIFIED FROM BYTES (see 312B-CheckMonCall.ASM): MOINF reads the caller's
 * A register (the MON#), bounds-checks it against 256, indexes MCTAB @005620B by
 * the MON#, and returns the entry with the ND-100 skip-return convention.
 *
 * SPECIAL: MOINF is dispatch machinery and lives in 026-S3IMPIT / 017-S3SMPIT
 * (load 32000B), next to CALLP=032201B - NOT in 003-S3CP with the ordinary
 * workers (where 032600 is an unrelated data table).
 *
 * Dispatch: MON 312B -> ENT14 072167B -> GOTAB[312B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[312B] @006132B = 032600B = MOINF (worker below).
 * All constants octal.
 * ============================================================================
 */

#define MCTAB_BASE   005620         /* 032637B constant, read via LDA I ,X 32 */
#define MON_COUNT    000400         /* 032636B constant (=256), read via LDT 35 */

/* MCTAB[] is the real monitor-call table (segment 044-S3IDPIT). MCTAB[N] holds
 * MON N's worker entry address, or 0 if the call is not implemented. */
extern word MCTAB[MON_COUNT];       /* physical words at 005620B.. */

void MOINF(mon_regs *caller)        /* level-14/monitor handler; operates on the caller's regs */
{
    unsigned n = caller->A;         /* 032600B: IRR 10 DA - read caller's A = MON# */

    word entry;
    if (n < MON_COUNT)              /* 032601B-032603B: LDT 35 (=256) ; SKP IF DA MLST ST */
        entry = MCTAB[n];           /* 032604B-032605B: X:=n ; A := mem[005620B + n] = MCTAB[n] */
    else
        entry = 0;                  /* 032607B: RADD CLD 0 DA - out of range -> 0 */

    caller->A = entry;              /* 032610B: IRW 10 DA - return entry (or 0) in caller's A */

    if (entry != 0) {               /* 032611B: JAZ (skip the skip-return if entry==0) */
        caller->P += 1;             /* 032612B-032614B: IRR/AAA 1/IRW on DP == SKIP RETURN */
    }
    /* 032615B: JMP I 6 - return.
     *   entry==0 -> normal return (call NOT implemented);
     *   entry!=0 -> skip return, A = entry (call implemented). */
}

/* Caveats for the emulator author:
 *   - EVERYTHING above is byte-proven: the bound (256), the MCTAB base (005620B),
 *     the index-by-MON#, and the skip/normal return are all read directly from the
 *     carved instruction and data words.
 *   - The caller-register access uses IRR/IRW (inter-level register read/write):
 *     MOINF runs on the monitor level and reads/writes the CALLER's A and P.
 *   - Minimal faithful emulation: for a MON number you actually implement, take the
 *     skip return with a non-zero A (use the true MCTAB entry if the caller
 *     dereferences it); for an unimplemented number, normal return with A = 0.
 */
