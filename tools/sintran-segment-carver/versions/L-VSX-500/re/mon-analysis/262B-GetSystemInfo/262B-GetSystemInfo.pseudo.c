/* ============================================================================
 * MON 262B - GetSystemInfo (CPUST) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 262B-GetSystemInfo.ASM.
 * The dispatch chain, the parameter range-check and the two MOVEW block copies
 * are VERIFIED from bytes; the exact field layout of the returned block is
 * INFERRED from the official manual (ND-860228-2, section 2.13).
 *
 * Dispatch: MON 262B -> ENT14 072167B -> GOTAB[262B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[262B] @006102B = 063022B = CPUST (worker below).
 * All constants octal.
 * ============================================================================
 */

/* Caller-supplied 12-word (24-byte) buffer.  Field order per the manual (INFERRED): */
struct sysinfo {
    word system_number;     /* system number                     */
    word cpu_type;          /* CPU type                          */
    word version;           /* SINTRAN III version               */
    word instruction_set;   /* instruction set                   */
    word patch_indicator;   /* patch indicator                   */
    word generation_time[]; /* system generation time (rest)     */
};

void CPUST(word number, struct sysinfo *out)   /* entry 063022B */
{
    /* 063022B-063027B: prologue - save registers, call FILSYS entry helper. */
    filsys_entry();                             /* 063026B: JPL I 113 */

    /* 063030B-063045B: validate the Number parameter.  Manual: Number == 0.
     * The bytes require it non-zero-error-free and range-check it against a
     * pair of table bounds (LDT 105 / LDT 103); out of range -> error return. */
    if (number == 0)                            /* 063031B: SKP IF DA UEQ 0 */
        return_error();                         /* 063032B: JMP I 110 */
    if (number < BOUND_LO || number > BOUND_HI) /* 063040B-063044B (INFERRED bounds) */
        return_error();                         /* 063045B: JMP I 77 */

    /* 063046B-063066B: two MOVEW block copies fill the caller's buffer with the
     * resident system-info words.  Word counts 025B and 037B (SAT 25 / SAT 37). */
    movew(out,            src_block_a, 025);     /* 063054B: MOVEW */
    movew(&out[025],      src_block_b, 037);     /* 063063B: MOVEW */

    /* worker closes at 063301B; tail restores regs and returns via saved link. */
    return_ok();
}
