/* ============================================================================
 * MON 314B - DefaultRemoteSystem (SDRUS) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in
 * 314B-DefaultRemoteSystem.ASM.  The prologue, the staged operand stores and the
 * record-commit call are VERIFIED from bytes; the exact record layout is INFERRED
 * from the manual and the COSMOS context.
 *
 * Dispatch: MON 314B -> ENT14 072167B -> GOTAB[314B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[314B] @006134B = 111501B = SDRUS (worker below).
 * All constants octal.
 * ============================================================================
 */

void SDRUS(word op1, word op2, word op3)   /* entry 111501B; operands in ,B 1..3 */
{
    /* 111501B-111505B: prologue - save regs, call the FILSYS/COSMOS entry helper. */
    cosmos_entry();                         /* 111505B: JPL I 46 -> 111553 */

    /* 111506B-111531B: stage the caller's operands into the default-remote record.
     * Each iteration: form a frame address, load the operand, call the stage helper
     * at 111555B; on failure branch to the error tail at 111550B. */
    if (!stage_field(op1)) goto err;        /* 111506B-111512B (,B 1) */
    if (!stage_field(op2)) goto err;        /* 111513B-111517B (,B 2) */
    if (!stage_field(op3)) goto err;        /* 111520B-111524B (,B 3) */

    /* 111532B-111543B: assemble the record from resident constants and commit it. */
    default_remote_record = assemble_record(op1, op2, op3);  /* INFERRED layout */
    if (!commit_default_remote(&default_remote_record))      /* 111543B: JPL I 16 */
        goto err;                            /* 111545B: MIN ,B 4 / SAA -47 */

    return_ok();
err:
    return_error();                          /* 111550B error tail */
}
