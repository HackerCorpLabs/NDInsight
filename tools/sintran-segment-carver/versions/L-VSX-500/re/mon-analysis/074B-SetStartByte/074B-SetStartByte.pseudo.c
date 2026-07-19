/* ============================================================================
 * MON 074B  SetStartByte (documented SETBT / internal SETBY)  -  pseudo-C model
 * SINTRAN III VSX/500 L.  Sets the next read/write byte pointer in an open file.
 *
 * Derived from the real disassembly (see 074B-SetStartByte.ASM). Control flow is
 * BYTE-VERIFIED for SETBY 103720B..103734B. The semantic labels (what each
 * resident worker does) are INFERRED: the pointer-table targets 3752B, 72622B,
 * 3776B are resident FS routines OUTSIDE the carve, so their behaviour is a
 * model, not gospel. Addresses in comments are octal.
 *
 * DISPATCH CAVEAT: GOTAB[074] = 000000 = fall-through. This body is reached only
 * after the uncarved resident MFELL/CALLPROC second-level dispatch; the
 * MON 074B -> SETBY edge is UNVERIFIED (no static pointer connects them).
 * ============================================================================ */

/* SETBY entry 103720B. Skeleton shared verbatim by SETBC/SBSIZ/RMAX/REABT,
 * which differ only in the pointer-table displacement they select. */
int mon_set_start_byte(mon_regs *r)
{
    save_start_byte(r->D);                 /* 103720: STD I 103 - save 32-bit A:D */
                                           /* 103721-103722: RADD CLD reg setup    */
    r->B = 6;                              /* 103723: SAB 6 - arg-block base index  */

    /* 103724: JPL I 100 -> ptr 003752B : resident FS worker (UNCARVED)            */
    rc = fs_worker_3752(r);                /*   validates file/positions descriptor */
    save_T(r->T);                          /* 103725: STT I 100 - stash returned T  */

    /* 103726: JPL I 101 -> ptr 072622B : second resident FS worker (UNCARVED)     */
    rc = fs_worker_72622(r);               /*   commit the byte pointer (inferred)  */

    if (ok)                                /* 103727: JMP 4 -> 103733 (normal path) */
    {
        arg_block[2] = rc;                 /* 103733: STA ,B 2 - store result       */
        /* 103734 JMP -3 -> 103731 falls into the shared exit tail below */
    }

    /* error / exit tail (103730-103732): */
    /* 103730: MIN ,B 4   ; bump arg-block+4                                        */
    /* 103731: SAA -6     ; A := -6 (error/skip code, exact meaning UNVERIFIED)     */
    /* 103732: JMP I 76 -> ptr 003776B : resident common exit (UNCARVED)            */
    return exit_via_3776(r, /*A=*/-6);
}

/* Inputs (from the documented SETBT contract, consistent with the code):
 *   file number  (INTEGER)          - open-file handle
 *   start byte   (INTEGER4, 32-bit) - carried in D (A:D); saved by STD I 103
 * Output / error codes live in the resident workers (3752B/72622B/3776B) and
 * are therefore INFERRED, not byte-proven. */
