/* ============================================================================
 * MON 243B  GetDirNameIndex (FDINA)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 243B-GetDirNameIndex.ASM). Control flow
 * and the calls to FLPAR (name parse) / GDIRI (get directory index) / GDIRT
 * (directory table) are BYTE-VERIFIED; the semantic labels (name string in,
 * directory + name index out) are INFERRED from the SINTRAN III Monitor Calls
 * manual - treat as a model, not gospel. Addresses octal.
 *
 * Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD SL DA" = A = L ; "RADD CLD SB DD" = D = B  (register copy)
 *   - "BSET ZRO SSK" clears the SSK skip flag
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 *   - each "JPL I <primitive>" followed by a "JMP <store-status exit>" is a
 *     skip-return: fail => goto store_status.
 * Region A (the level-14 stub) does not exist for this call - GOTAB[243] = 0.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   X = address of the directory name string (1-16 chars)
 * Normal return: T = directory index (STT DIRIX), A = name index (STA NAMIX).
 * Error return:  A = error number. */

int mon_243B_FDINA(mon_regs *r)     /* FDINA @106734B (006-S3FS), bounded by WDIEN=107106 */
{
    int rc, dir_index, name_index;

    /* 106734-106741: entry prologue */
    frame[074] = 0;                    /* 106734 BSET ZRO SSK (latched to B+74)       */
    save_params(r);                    /* 106735 STD I 123 : stash caller double-word */
    frame_base = 075;                  /* 106740 SAB 75 : 75B-word local frame B      */
    resident_prologue();               /* 106741 JPL I 120 -> 003752                  */

    /* 106750-106762: parse + validate the directory name string. */
    (void)uscps();                             /* 106755 JPL I 106 -> USCPS 031075   */
    if (!flpar())         goto store_status;   /* 106762 JPL I 103 -> FLPAR 046231   */

    /* 106764-107032: resolve the directory index and the name/device index. */
    if (!gdiri(&dir_index)) goto store_status; /* 106773 JPL I 75 -> GDIRI 047402    */
    (void)gdirt(&name_index);                  /* 107001 JPL I 72 -> GDIRT 050124    */
    (void)remch();                             /* 107010 JPL I 66 -> REMCH 061451    */
    (void)gmusi(); (void)gdfkn(); (void)gdefd();/* 107030/107032/107045 name helpers */

    /* 107034-107036: return the indices. */
    frame[1] = dir_index;                      /* 107034 STT ,B 1 : directory index  */
    rc = 0;                                    /* success */
    { int t = frame[4] + 1; frame[4] = t; }    /* 107036 MIN ,B 4 : skip-return ok   */
    goto resident_return;

store_status:                                  /* 107035/107052/107056 error path     */
    r->status = rc = A;                        /* STA ,B 2 : status -> caller         */
resident_return:
    resident_return_teardown();                /* 107040 JMP I 44 -> 003776           */
    return rc;                                 /* A = error number on error return    */
}

/* Byte-verified anchors:
 *   FDINA entry 106734 (BSET ZRO SSK / STD I 123 / RADD CLD SL DA / RADD CLD SB DD),
 *   frame SAB 75, prologue JPL I 120 -> 003752,
 *   FLPAR call JPL I 103 -> 046231 (link cell 107065),
 *   GDIRI call JPL I 75 -> 047402 (link cell 107070),
 *   GDIRT 050124, REMCH 061451, GMUSI 054527, GDFKN 054341, GDEFD 055263,
 *   USCPS 031075, dir-index store STT ,B 1 (107034), status store STA ,B 2,
 *   resident return JMP I 44 -> 003776.
 * INFERRED: the name-string-in / dir+name-index-out contract and the skip-return
 *   polarity of the primitive calls (see header). The exact word mapping to the
 *   manual's NAMIX is not isolated beyond the B+1 / B+2 result slots. */
