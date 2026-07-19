/* ============================================================================
 * MON 213B  GetDirUserIndexes (MUIDI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 213B-GetDirUserIndexes.ASM). Control
 * flow and the calls to GDIRI (get directory index) / RUSER (read user entry) /
 * GUSEI (get user entry index) are BYTE-VERIFIED; the semantic labels (name
 * string in, directory + user index out) are INFERRED from the SINTRAN III
 * Monitor Calls manual - treat as a model, not gospel. Addresses octal.
 *
 * Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD SL DA" = A = L ; "RADD CLD SB DD" = D = B  (register copy)
 *   - "SHA ZIN 10" = logical left shift by 8 (ZIN = zero-fill)
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 *   - each "JPL I <primitive>" followed by a "JMP I <store-status exit>" is a
 *     skip-return: fail => goto store_status.
 * Region A (the level-14 stub) does not exist for this call - GOTAB[213] = 0.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   X = address of the directory-and-user name string (up to 16 chars)
 * Normal return: T = directory index, A = user index (packed index word).
 * Error return:  A = error number. */

int mon_213B_MUIDI(mon_regs *r)     /* MUIDI @105012B (006-S3FS), bounded by GUSNA=105301 */
{
    int rc, dir_index, user_index;

    /* 105012-105017: entry prologue */
    frame[0156] = 1;                   /* 105012 BSET ONE SSK (latched to B+156)      */
    save_params(r);                    /* 105013 STD I 51 : stash caller double-word  */
    frame_base = 0157;                 /* 105016 SAB 157 : 157B-word local frame B    */
    resident_prologue();               /* 105017 JPL I 46 -> 003752                   */

    /* 105026-105155: parse the directory-and-user name string. */
    if (!flpar())         goto store_status;   /* 105044 JPL I 27 -> FLPAR 046231    */
    (void)remch();                             /* 105036 JPL I 33 -> REMCH 061451    */
    (void)sepst();                             /* 105122 JPL I 132 -> SEPST 042237   */
    (void)getch();                             /* 105127 JPL I 126 -> GETCH 030062   */

    /* 105145: resolve the directory index. */
    if (!gdiri(&dir_index)) goto store_status; /* 105145 JPL I 113 -> GDIRI 047402   */

    /* 105152-105213: resolve the user and read the user entry. */
    (void)gusei();                             /* 105152 JPL I 107 -> GUSEI 053740   */
    (void)gmusi(); (void)gusen(); (void)gmfkn();/* 105135/105171/105202 user helpers */
    if (!ruser(&user_index)) goto store_status;/* 105213 JPL I 57 -> RUSER 053246    */

    /* 105226-105237: pack the directory + user index for the caller. */
    frame[1] = (dir_index << 8) | user_index;  /* 105234 SHA ZIN 10 / 105236 ADD /   */
                                               /* 105237 STA ,B 1                    */
    rc = 0;                                    /* success */
    { int t = frame[4] + 1; frame[4] = t; }    /* 105240 MIN ,B 4 : skip-return ok   */
    goto resident_return;

store_status:                                  /* 105243 error path                   */
    r->status = rc = A;                        /* 105243 STA ,B 2 : status -> caller  */
resident_return:
    resident_return_teardown();                /* 105242 JMP I 36 -> 003776           */
    return rc;                                 /* A = error number on error return    */
}

/* Byte-verified anchors:
 *   MUIDI entry 105012 (BSET ONE SSK / STD I 51 / RADD CLD SL DA / RADD CLD SB DD),
 *   frame SAB 157, prologue JPL I 46 -> 003752,
 *   GDIRI call JPL I 113 -> 047402 (link cell 105260),
 *   RUSER call JPL I 57 -> 053246 (link cell 105272),
 *   GUSEI call JPL I 107 -> 053740 (link cell 105261),
 *   FLPAR 046231, REMCH 061451, SEPST 042237, GETCH 030062, GMUSI 054527,
 *   index pack STA ,B 1 (105237), status store STA ,B 2 (105243),
 *   resident return JMP I 36 -> 003776.
 * INFERRED: the name-string-in / dir+user-index-out contract and the skip-return
 *   polarity of the primitive calls (see header). */
