/* ============================================================================
 * MON 215B  GetObjectEntry (DROBJ)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 215B-GetObjectEntry.ASM). Control flow
 * and the calls to the object-entry primitives ROBJE / GDIRA are BYTE-VERIFIED;
 * the semantic labels (which field is directory index / user index / object
 * index / entry buffer) are INFERRED from the SINTRAN III Monitor Calls manual -
 * treat as a model, not gospel. Addresses in comments are octal.
 *
 * Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - bare "LDA disp" (no mode) = mem[P+disp]  (P-relative, NOT a literal)
 *   - "RADD CLD SL DA" = A = L ; "RADD CLD SB DD" = D = B  (register copy)
 *   - "BSET ZRO SSK" clears the SSK skip flag (selector = 0 = get object entry)
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 *   - each "JPL I <primitive>" followed by a "JMP I <store-status exit>" is a
 *     skip-return: the primitive returns to the JMP on FAILURE (no skip) and one
 *     word past it on SUCCESS (skip)  => "if (!call()) goto store_status;".
 * Region A (the level-14 stub) does not exist for this call - GOTAB[215] = 0.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   A = address of the 32-word (64-byte) object-entry receive buffer
 *   T = INDEX: left byte = directory index, right byte = user index
 *   X = object index
 *   D = remote system identification (used only if the remote bit is set)
 * Error return: A = error number. */

int mon_215B_DROBJ(mon_regs *r)     /* DROBJ @104037B (006-S3FS), bounded by DWOBJ=104410 */
{
    int rc;

    /* 104037-104044: entry prologue */
    int sel = 0;                       /* 104037 BSET ZRO SSK : selector = get       */
    save_params(r);                    /* 104040 STD I 56 : stash caller double-word */
    /* 104041 RADD CLD SL DA : A = L ; 104042 RADD CLD SB DD : D = B                 */
    frame_base = 0131;                 /* 104043 SAB 131 : 131B-word local frame B   */
    resident_prologue();               /* 104044 JPL I 53 -> 003752                  */

    frame[0127] = sel;                 /* 104045-104054: latch SSK -> B+127          */

    /* 104055-104232: validate + resolve directory / user / object indices.
     * Each helper uses the skip-return convention (fail => goto store_status). */
    if (!uscps())         goto store_status;   /* 104063 JPL I 36 -> USCPS 031075   */
    if (!flpar())         goto store_status;   /* 104070 JPL I 34 -> FLPAR 046231   */
    if (!chduo())         goto store_status;   /* 104132 JPL I 77 -> CHDUO 101303   */
    /* directory/user resolution helpers (byte-verified calls; roles inferred):    */
    (void)foptb();                             /* 104174 JPL I 45 -> FOPTB 101043   */
    (void)stduo();                             /* 104177 JPL I 44 -> STDUO 071413   */

    /* 104202: fetch the object entry with the read-object-entry primitive
     * (the SAME primitive MON 41B ReadObjectEntry drives). */
    if (!robje(r))        goto store_status;   /* 104202 JPL I 42 -> ROBJE 055566   */
    (void)get_directory_address();             /* 104330 JPL I 52 -> GDIRA 030225   */
    (void)gusen(); (void)gmusi(); (void)gmfkn();/* 104300/104302/104306 user helpers */

    rc = 0;                                    /* success */
    { int t = frame[4] + 1; frame[4] = t; }    /* 104356 MIN ,B 4 : skip-return ok  */
    goto resident_return;

store_status:                                  /* 104351/104362 error path          */
    r->status = rc = A;                        /* 104362 STA ,B 2 : status -> caller */
resident_return:
    resident_return_teardown();                /* 104360 JMP I 27 -> 003776         */
    return rc;                                 /* A = error number on error return  */
}

/* Byte-verified anchors:
 *   DROBJ entry 104037 (BSET ZRO SSK / STD I 56 / RADD CLD SL DA / RADD CLD SB DD),
 *   frame SAB 131, prologue JPL I 53 -> 003752,
 *   ROBJE call JPL I 42 -> 055566 (link cell 104244, shared with MON 41B),
 *   GDIRA call JPL I 52 -> 030225 (link cell 104402),
 *   USCPS 031075, FLPAR 046231, CHDUO 101303, GUSEN 055111, GMUSI 054527,
 *   status store STA ,B 2 (104362), resident return JMP I 27 -> 003776.
 * INFERRED: object-entry field roles, the index-in / buffer-out contract, and
 *   the skip-return polarity of the primitive calls (see header). */
