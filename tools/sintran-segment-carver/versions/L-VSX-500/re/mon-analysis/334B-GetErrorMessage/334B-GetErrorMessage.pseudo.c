/* ============================================================================
 * MON 334B  GetErrorMessage (GETXM)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Returns the SINTRAN III error-message TEXT for an
 * input error number (see appendix A) into a caller buffer, then RETURNS - the
 * program CONTINUES.  Convenient for advanced terminal use (e.g. show the error
 * text in inverse video).  Error number 0 is illegal.
 *
 * Dispatch reality:
 *   GOTAB[334B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED): there is no per-call
 *   level-14 stub; dispatch drops into the resident MFELL/CALLPROC second-level
 *   path, which is UNCARVED and NOT byte-followable statically.
 *   GETXM @107273B (025-S3IRPIT) is the NAMED worker for this call and IS real
 *   executable code (body 107273-107464, converging on the common exit 107464 ->
 *   107526).  Identity rests on the symbol NAME (GETXM = the 334B short name) +
 *   the error-number-classify/copy behaviour - see README caveats.
 *
 * The GETXM worker body is BYTE-VERIFIED (see 334B-GetErrorMessage.ASM); the
 * appendix-A message table and the caller buffer/register mapping are INFERRED
 * from the manual (334B_GetErrorMessage.yaml).  Addresses in comments are octal.
 * ============================================================================ */

/* (B) GETXM worker @107273B (025-S3IRPIT).  REAL executable code: take the error
 * number, classify it into ranges, index a message-offset table, copy the
 * message text with MOVEW.  Control flow is BYTE-VERIFIED; the message table and
 * the destination buffer layout are INFERRED from the manual. */
int mon_334B_GetErrorMessage(mon_regs *r)  /* in: A = ErrorNo; X = buffer addr */
{
    /* 107273-107277: entry; JPL I -> 107465 (setup via link cell); LDA ,B 17
     *                fetches the caller error number; RADD CLD SA DD : D := A.  */
    int err = r->A_ErrorNo;
    /* NOTE: error number 0 is illegal (manual); not re-checked in this body.   */

    /* 107310-107313: SAA 4 / MST PIE : raise to level 4 / enable PIE, then
     *                JPL I -> 107471 (resident helper). MST PIE per semantics
     *                reference is a masked set of PIE by A.                     */

    /* 107322-107463: the range-classification chain.  Each block does
     *   LDT <threshold>; SKP IF DA MGRE/MLST ST   (compare err vs threshold)
     *   SUB <base>; RADD CLD SA DX                (X := err - base = index)
     *   JPL I -> 107505 / LDA I ,X <n>            (fetch message-table entry)
     *   MOVEW                                     (block-copy the message text)
     *   JMP -> 107464                             (common exit)
     * SKP IF DA MLST ST = skip if (unsigned) A < T; SKP IF DA MGRE ST = skip if
     * (unsigned) A >= T (per ND100-INSTRUCTION-SEMANTICS.md SKP class).  MOVEW is
     * a block word move, count in L (RADD CLD SA DL sets L := 11 = 9. words),
     * source A:D, dest X:T.                                                     */
    int idx = classify_error_number(err);          /* range tests -> table index */
    const word *msg = error_message_table[idx];    /* INFERRED: appendix-A table */

    /* 107462-107463: default path (SAX 0 / LDA I ,X 14) when no range matched.  */

    /* 107464: JMP -> 107526 : common exit = shared copy/return (outside window).
     *         copies the selected message text into the caller buffer and
     *         returns - the program CONTINUES.                                  */
    copy_message_to_buffer(msg, r->X_buffer);      /* INFERRED destination */
    return 0;                                       /* control returns to caller */
}

/* Caveats for the emulator author:
 *   - GOTAB[334B]=0 is a BYTE-VERIFIED fall-through; there is no stub to follow.
 *     The MON 334 -> worker transfer is via the UNCARVED resident MFELL/CALLPROC,
 *     so the fall-through -> GETXM hop is NOT byte-followable here.
 *   - GETXM=107273B IS real executable code (range-classify + MOVEW copy,
 *     converging on the common exit 107464 -> 107526).  Its attribution to
 *     MON 334 rests on the symbol NAME (GETXM = the 334B manual short name) + the
 *     classify/copy behaviour, not a followed pointer - hence status `partial`.
 *   - The alternate-named region GETER=074104B (commoncode, "GET ERror") is
 *     ZERO-FILLED (not carved) in this L image, so it offers no body.
 *   - The X21RD/X21DC/DMLP2/X21CH/X21BR/DILP2/X21C symbol labels inside the body
 *     range do NOT match the decoded instruction stream (symbol/overlay
 *     artefact); the bytes are one continuous GETXM routine.
 *   - The appendix-A message table and the caller buffer/register mapping are
 *     manual-derived (334B_GetErrorMessage.yaml), not byte-isolated here.
 *   - A live PC trace (issue a real MON 334, single-step the level-14 fall-
 *     through into the resident CALLPROC) is needed to confirm P lands on GETXM.
 */
