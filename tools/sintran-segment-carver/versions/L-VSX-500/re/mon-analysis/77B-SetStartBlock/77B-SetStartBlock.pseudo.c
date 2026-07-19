/* ============================================================================
 * MON 077B - SetStartBlock (SETBC) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 77B-SetStartBlock.ASM.
 * The dispatch chain, the open-file lookup, the set-position helper call and the
 * error/success fork are VERIFIED from bytes; the helper internals and the exact
 * control-block field are INFERRED from the SETBC/SBSIZ/RMAX family structure.
 *
 * Dispatch: MON 077B -> ENT14 072167B -> GOTAB[077B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[077B] @005717B = 103735B = SETBC (worker below).
 * All constants octal.
 * ============================================================================
 */

void SETBC(int file_number, long block_number)   /* entry 103735B */
{
    /* 103735B-103741B: prologue - save regs, look up the open-file control block. */
    OpenFile *f = filsys_open_lookup(file_number);   /* 103741B: JPL I 63 -> 104024 */
    /* 103742B: STT I 63 - stash the resolved control-block pointer. */

    /* 103743B: set the next-block position in the control block.
     * (Equivalent to SetStartByte with block_number * blocksize.) */
    int err = set_file_position(f, block_number);    /* 103743B: JPL I 66 -> 104031 */

    if (err) {                                       /* 103745B-103747B */
        f->error_count++;                            /* 103745B: MIN ,B 4 */
        return_file_error(-6);                       /* 103747B: JMP I 61 -> 104030 */
    }
    /* 103750B: success - store status and return. */
    return_ok();
}
