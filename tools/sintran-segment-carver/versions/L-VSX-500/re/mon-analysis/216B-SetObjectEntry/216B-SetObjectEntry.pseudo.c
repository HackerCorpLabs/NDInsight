/* ============================================================================
 * MON 216B  SetObjectEntry (DWOBJ)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 216B-SetObjectEntry.ASM). Control flow
 * and the calls to the object-entry primitives ROBJE (read) / WOBJE (write) are
 * BYTE-VERIFIED; the semantic labels (directory index / user index / object index
 * / source buffer) are INFERRED from the SINTRAN III Monitor Calls manual - treat
 * as a model, not gospel. Addresses in comments are octal.
 *
 * Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD SL DA" = A = L ; "RADD CLD SB DD" = D = B  (register copy)
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 *   - each "JPL I <primitive>" followed by a "JMP I <store-status exit>" is a
 *     skip-return: fail => goto store_status.
 * Region A (the level-14 stub) does not exist for this call - GOTAB[216] = 0.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   A = address of the 32-word (64-byte) source object entry
 *   T = INDEX: left byte = directory index, right byte = user index
 *   X = object index (bit 15 set = remote file)
 *   D = remote system identification (used only if the remote bit is set)
 * Error return: A = error number. */

int mon_216B_DWOBJ(mon_regs *r)     /* DWOBJ @104410B (006-S3FS), bounded by MRUSE=105010 */
{
    int rc;

    /* 104410-104414: entry prologue */
    save_params(r);                    /* 104410 STD I 102 : stash caller double-word */
    /* 104411 RADD CLD SL DA : A = L ; 104412 RADD CLD SB DD : D = B                  */
    frame_base = 0134;                 /* 104413 SAB 134 : 134B-word local frame B    */
    resident_prologue();               /* 104414 JPL I 77 -> 003752                   */

    /* 104415-104532: validate + resolve the directory / user / object indices.
     * Access-check failure loads error 70 and exits. */
    if (!chduo())         goto store_status;   /* 104417 JPL I 75 -> CHDUO 101303    */
    if (!tusrt())       { rc = 070; goto set_err; } /* 104423 JPL I 73 -> TUSRT 053114*/
    (void)gusen(); (void)gmusi(); (void)gmfkn();/* 104462/104464/104470 user helpers */
    if (!gfiac())       { rc = 070; goto set_err; } /* 104475 JPL I 34 -> GFIAC 057771*/

    /* 104617: read the CURRENT object entry so the caller's changes can be merged. */
    if (!robje(r))        goto store_status;   /* 104617 JPL I 152 -> ROBJE 055566   */

    /* 104621-104755: merge the caller's fields into the entry; the compare loops
     * call GETCH (character/field compare) at 104647/104652/104675/104700. */
    merge_fields(r);                           /* JPL I -> GETCH 030062 (x4)         */

    /* 104756: write the modified 64-byte entry back to disk. */
    if (!wobje(r))        goto store_status;   /* 104756 JPL I 27 -> WOBJE 055750    */

    rc = 0;                                    /* success */
    { int t = frame[4] + 1; frame[4] = t; }    /* 104760 MIN ,B 4 : skip-return ok   */
    goto resident_return;

set_err:
    A = rc;                                    /* 104501/104507 SAA 70 (access error) */
store_status:                                  /* 104764 error path                   */
    r->status = rc = A;                        /* 104764 STA ,B 2 : status -> caller  */
resident_return:
    resident_return_teardown();                /* 104762 JMP I 24 -> 003776           */
    return rc;                                 /* A = error number on error return    */
}

/* Byte-verified anchors:
 *   DWOBJ entry 104410 (STD I 102 / RADD CLD SL DA / RADD CLD SB DD / SAB 134),
 *   prologue JPL I 77 -> 003752,
 *   ROBJE call JPL I 152 -> 055566 (link cell 104771),
 *   WOBJE call JPL I 27 -> 055750 (link cell 105005),
 *   CHDUO 101303, TUSRT 053114, GUSEN 055111, GMUSI 054527, GMFKN 054130,
 *   GFIAC 057771, GETCH 030062, error literal SAA 70,
 *   status store STA ,B 2 (104764), resident return JMP I 24 -> 003776.
 * INFERRED: object-entry field roles, the index-in / buffer-in contract, the
 *   error-70 meaning, and the skip-return polarity of the primitive calls. */
