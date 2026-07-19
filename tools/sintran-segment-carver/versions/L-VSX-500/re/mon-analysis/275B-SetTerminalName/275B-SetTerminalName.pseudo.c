/* ============================================================================
 * MON 275B  SetTerminalName (STRFI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Defines the file name used for terminals (normally 'TERMINAL:'). Background users
 * identify their own terminal with this file name; the call may be issued repeatedly.
 *
 * IMPORTANT: there is NO carved worker for this call. GOTAB[275B] = 000000 is a
 * fall-through, and the manual short name STRFI has no matching symbol in any carved
 * segment (the nearest name STERM=041333B is a DATA variable, not code). So this model
 * is of the DOCUMENTED behaviour only, NOT carved code - every line is inferred from the
 * manual. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[275B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into the
 *   resident MFELL/CALLPROC second-level path (uncarved), which reaches a worker that
 *   is not present in the carved bytes. The MON 275 -> worker link is NOT byte-provable.
 *
 * Instruction semantics (where any real code is referenced) follow the canonical
 * reference: ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 * ============================================================================ */

/* Manual register contract (from the MON 275 description, INFERRED - not byte-proven):
 *   SetTerminalName(TerminalName):
 *     TerminalName : file-name string for terminals (64 chars), normally 'TERMINAL'.
 *   Error number returned in A. */

int mon_275B_SetTerminalName(mon_regs *r) /* documented behaviour; worker not carved */
{
    /* No carved body: the set-terminal-name worker is reached past the uncarved
     * MFELL/CALLPROC bridge and is not present in the carved segments. The documented
     * effect is to record the caller's terminal file-name string (default 'TERMINAL:')
     * so background users resolve their terminal through it; SetTerminalName is a
     * sibling of SetPeripheralName (MON 234B), which connects a file name to a device. */
    record_terminal_file_name(r->X /* address of the TerminalName string */);
    return /* error code in A */ 0;
}

/* Byte-verified anchors: NONE for the worker. Only GOTAB[275B] = 000000 (fall-through)
 * is byte-proven. STERM=041333B is a data variable (words 000000/000035), not the
 * worker.
 * NOT proven: everything about the worker body - it is not present in the carved bytes;
 * the parameter contract is inferred from the manual; the fall-through MON 275 -> worker
 * bridge (uncarved MFELL/CALLPROC) is not followed. */
