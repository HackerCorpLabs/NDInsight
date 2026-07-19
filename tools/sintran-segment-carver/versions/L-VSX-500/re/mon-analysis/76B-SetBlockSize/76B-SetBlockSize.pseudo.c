/* ============================================================================
 * MON 76B  SetBlockSize (documented SETBS / internal SBSIZ)  -  pseudo-C model
 * SINTRAN III VSX/500 L.  Sets the random-access block size of an open file.
 *
 * Derived from the real disassembly (see 76B-SetBlockSize.ASM). Control flow is
 * BYTE-VERIFIED for SBSIZ 103752B..103766B. The semantic labels (what each
 * resident worker does) are INFERRED: the pointer-table targets 3752B, 72351B,
 * 3776B are resident FS routines OUTSIDE the carve, so their behaviour is a
 * model, not gospel. Addresses in comments are octal.
 *
 * SBSIZ shares its skeleton verbatim with SETBY (74B) / SETBC / RMAX (62B) /
 * REABT (75B); only the pointer-table displacements differ. See 074B-SetStartByte.
 *
 * DISPATCH CAVEAT: GOTAB[76] = 000000 = fall-through. This body is reached only
 * after the uncarved resident MFELL/CALLPROC second-level dispatch; the
 * MON 76B -> SBSIZ edge is UNVERIFIED (no static pointer connects them).
 * ============================================================================ */

/* SBSIZ entry 103752B. */
int mon_set_block_size(mon_regs *r)
{
    save_block_size(r->A, r->D);           /* 103752: STD I 51 - save 32-bit A:D block size */
    r->A = r->L;                           /* 103753: RADD CLD SL DA - A := L                */
    r->D = r->B;                           /* 103754: RADD CLD SB DD - D := B                */
    r->B = 6;                              /* 103755: SAB 6 - arg-block base index           */

    /* 103756: JPL I 46 -> ptr 003752B : resident FS worker (UNCARVED)                       */
    rc = fs_worker_3752(r);                /*   validates file handle / descriptor           */
    save_T(r->T);                          /* 103757: STT I 46 - stash returned T            */

    /* 103760: JPL I 52 -> ptr 072351B : second resident FS worker (UNCARVED)                */
    rc = fs_worker_72351(r);               /*   commit the new block size (inferred)         */

    if (ok)                                /* 103761: JMP 4 -> 103765 (normal path)          */
    {
        arg_block[2] = rc;                 /* 103765: STA ,B 2 - store result                */
        /* 103766 JMP -3 -> 103763 falls into the shared exit tail below */
    }

    /* error / exit tail (103762-103764): */
    /* 103762: MIN ,B 4   ; bump arg-block+4                                                  */
    /* 103763: SAA -6     ; A := -6 (error/skip code, exact meaning UNVERIFIED)               */
    /* 103764: JMP I 44 -> ptr 003776B : resident common exit (UNCARVED)                      */
    return exit_via_3776(r, /*A=*/-6);
}

/* Inputs (from the documented SETBS contract, consistent with the code):
 *   FileNumber (INTEGER, T)          - open-file handle (see OpenFile)
 *   BlockSize  (LONGINT, 32-bit A:D) - block size in bytes; must be even; saved by STD I 51
 * The standard block size is 512 bytes (set at open, reset at close); factors of
 * 2048 bytes are most efficient. Output / error codes live in the resident
 * workers (3752B/72351B/3776B) and are therefore INFERRED, not byte-proven. */
