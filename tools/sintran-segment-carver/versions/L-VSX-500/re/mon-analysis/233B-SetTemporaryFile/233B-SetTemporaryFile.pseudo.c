/* ============================================================================
 * MON 233B  SetTemporaryFile (SETTF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  SETTF is a dedicated entry that presets the operation
 * mode directly (SAA 4) and jumps into the shared directory-entry dispatcher (the
 * same MDLFI body used by MON 54B DeleteFile and MON 232B RenameFile), joining at
 * the mode-store 106107.
 *
 * Derived from the real disassembly (see 233B-SetTemporaryFile.ASM).  Control flow
 * is BYTE-VERIFIED; the semantic labels (which worker does what, error-number
 * meanings) are INFERRED from the FILSYS symbol table and the call structure -
 * treat as a model, not gospel.  Addresses in comments octal.  Every instruction
 * is translated per ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 * ============================================================================ */

/* Entry 106043 = SETTF. */
int mon_set_temporary_file(mon_regs *r)
{
    save_params(r);                        /* 106043 STD I 133 - stash caller D      */
    /* 106044 RADD CLD SL DA => A = L ; 106045 RADD CLD SB DD => D = B (COPY idiom).  */
    frame_setup();                         /* 106046 SAB 125 - build 125B-word B      */
    prologue_worker();                     /* 106047 JPL I 130 -> 003752 (resident)   */

    mode = 4;                              /* 106050 SAA 4 (A = signext8(4) = 4)      */
    /* 106051 JMP -> 106107 joins the shared MDLFI dispatcher at the mode-store,      */
    /* bypassing the SSK/SSM fold (106065-106106) that the family entries use.        */
    r->B[123] = mode;                      /* 106107 STA ,B 123                       */

    /* 106110-106115: locate the file's directory entry via link cell 106202          */
    /* (031075); on failure JMP -> 106174 (store status) and return.                  */
    ent = find_dir_entry(r->filename);     /* 106114 JPL I 66 -> 031075               */

    /* 106116-106170: dispatch by mode.  The ladder tests B+123 against 1, then 0, 1, */
    /* 2, 3 (SAT n / SKP IF DA EQL ST / JAF).  mode 4 matches none of the 0..3 cases, */
    /* so it falls through to the tail worker:                                        */
    /*   106167 JPL I 21 -> 106210 = MSTRM (120233)                                   */
    rc = MSTRM(ent);                       /* set-temporary tail worker               */
    if (rc != 0) goto fail;                /* each JPL I is followed by JMP -> 106174  */

    /* --- success exit (106171-106173) ------------------------------------------- */
    /* 106171 MIN ,B 4 (bump status); 106172 SAA -125; 106173 JMP I 16 -> 106211 =    */
    /* 003776 (resident return).                                                      */
    return 0;

fail:
    r->B[2] = error_no;                    /* 106174 STA ,B 2 (caller status slot)    */
    return error_no;                       /* falls to 106172/106173 resident return  */
}

/* User-register convention (from the manual's MAC example, INFERRED not byte-proven
 * here): X = address of the file-name string.  Normal return skips; error return has
 * the error number in A.  The file can be read once; when closed its contents are
 * deleted (the empty file still exists).
 *
 * Note: the FILSYS symbol table also carries STEFI (106052), a DeleteFile-family
 * sibling (mode 3 -> MSTMP) whose name equally reads "set temporary file".  Because
 * GOTAB[233] = 0, the exact MON 233 -> entry link is not a followed pointer; see the
 * README Honest caveats.
 */
