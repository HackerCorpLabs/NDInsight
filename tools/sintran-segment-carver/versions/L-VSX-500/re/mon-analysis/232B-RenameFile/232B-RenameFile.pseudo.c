/* ============================================================================
 * MON 232B  RenameFile (MRNFI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  RenameFile dispatches through GOTAB[232] = 066172B (the
 * F1723 level-14 stub in 025-S3IRPIT).  The functional worker is MRNFI, a sibling
 * entry of the shared directory-entry dispatcher (the same MDLFI body used by MON
 * 54B DeleteFile); MRNFI presets SSM=0/SSK=1, joins at 106065, folds to operation
 * mode 1, and calls the mode-1 worker MRENF (117352).
 *
 * Derived from the real disassembly (see 232B-RenameFile.ASM).  Control flow and
 * the SSK/SSM -> mode fold are BYTE-VERIFIED; the semantic labels are INFERRED from
 * the FILSYS symbol table and the call structure.  Addresses in comments octal.
 * Every instruction is translated per ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 * ============================================================================ */

/* The F1723 stub (066172-066175 in 025-S3IRPIT) is the level-14 entry vectored from
 * GOTAB[232].  It is an entry into a shared handler (a device/segment table min/max
 * scan that ends in EXIT at 66261); it does NOT itself branch to MRNFI - that
 * transfer is the uncarved resident CALLPROC hop (see README Honest caveats). */

/* Entry 106060 = MRNFI presets ssm=0, ssk=1 -> mode 1. */
int mon_rename_file(mon_regs *r)
{
    int ssk = 1, ssm = 0;                  /* 106060 BSET ZRO SSM ; 106061 BSET ONE SSK */
    /* 106062 JMP -> 106065 joins the shared MDLFI dispatcher.                        */
    save_params(r);                        /* 106065 STD I 111 - stash caller D       */
    /* 106066 RADD CLD SL DA => A = L ; 106067 RADD CLD SB DD => D = B (COPY idiom).   */
    frame_setup();                         /* 106070 SAB 125 - build 125B-word B       */
    prologue_worker();                     /* 106071 JPL I 106 -> 003752 (resident)    */

    /* 106072-106106: fold the mode from SSM then SSK.  106072 BSKP ONE SSM (SSM=0,    */
    /* no skip) -> 106102 BSKP ONE SSK (SSK=1, skip) -> 106104 SAA 1 => mode 1.        */
    mode = fold_mode(ssm, ssk);            /* 106107 STA ,B 123 ; RenameFile => 1      */

    /* 106110-106115: locate the file's directory entry via link cell 106202 (031075);*/
    /* on failure JMP -> 106174 (store status) and return.                            */
    ent = find_dir_entry(r->oldname);      /* 106114 JPL I 66 -> 031075                */

    /* 106116-106170: dispatch by mode.  mode 1 matches at 106137-106147:             */
    /*   106145 JPL I 40 -> 106205 = MRENF (117352)  rename        <- RenameFile       */
    /* (the mode-1 branch at 106116-106127 first re-locates the entry via 106126       */
    /*  JPL I 54 -> 031075 before the rename worker runs.)                             */
    rc = MRENF(ent, r->newname);           /* rename to the new file name              */
    if (rc != 0) goto fail;                /* each JPL I is followed by JMP -> 106174   */

    /* --- success exit (106171-106173) ------------------------------------------- */
    /* 106171 MIN ,B 4 (bump status); 106172 SAA -125; 106173 JMP I 16 -> 106211 =     */
    /* 003776 (resident return).                                                       */
    return 0;

fail:
    r->B[2] = error_no;                    /* 106174 STA ,B 2 (caller status slot)     */
    return error_no;                       /* falls to 106172/106173 resident return   */
}

/* User-register convention (from the manual's MAC example, INFERRED not byte-proven
 * here): X = address of the old file-name string; A = address of the new file-name
 * string (file type only may change, e.g. :SYMB).  Normal return skips; error return
 * has the error number in A.
 */
