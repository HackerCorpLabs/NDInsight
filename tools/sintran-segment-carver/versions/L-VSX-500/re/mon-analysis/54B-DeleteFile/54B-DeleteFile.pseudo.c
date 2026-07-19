/* ============================================================================
 * MON 54B  DeleteFile (MDLFI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  MDLFI shares one dispatcher body with three sibling
 * directory-entry operations (STEFI, SPEFI, MRNFI) that enter through 3-word
 * stubs above it; each presets the SSK/SSM skip-flag pair to select the action.
 *
 * Derived from the real disassembly (see 54B-DeleteFile.ASM). Control flow and
 * the SSK/SSM -> mode split are BYTE-VERIFIED; the semantic labels (which worker
 * does what, error-number meanings) are INFERRED from the FILSYS symbol table and
 * the call structure - treat as a model, not gospel. Addresses in comments octal.
 * ============================================================================ */

/* Entry 106063 = MDLFI (ssk=0,ssm=0 -> mode 0 = delete).  STEFI/SPEFI/MRNFI enter
 * above with other ssk/ssm combos and join at 106065. */
int mon_dir_entry_op(mon_regs *r, int ssk, int ssm)
{
    save_params(r);                        /* 106065 STD I 111 - stash caller D     */
    frame_setup();                         /* 106066-106070 SAB 125, build B frame  */
    prologue_worker();                     /* 106071 JPL I 106 -> 003752 (resident) */

    /* 106072-106106: derive the operation mode (0..3) from SSM then SSK           */
    mode = op_from_flags(ssm, ssk);        /* 106107 STA ,B 123 ; DeleteFile => 0   */

    /* 106110-106115: locate the file's directory entry (common lookup worker via   */
    /* link cell 106202); on failure JMP -> 106174 (store status) and return.       */
    ent = find_dir_entry(r->filename);     /* 106114 JPL I 66 -> 031075             */

    /* 106116-106170: dispatch by mode.  Each branch compares B+123 to a constant   */
    /* (SAT 1/2/3) and, on match, calls the matching worker through a link cell:     */
    /*   mode 0 (DeleteFile):     106134 JPL I 50 -> MODLF (120067)  delete/modify   */
    /*   mode 1:                  106145 JPL I 40 -> MRENF (117352)  rename          */
    /*   mode 2:                  106155 JPL I 31 -> MSPER (120470)  set-permanent    */
    /*   mode 3:                  106164 JPL I 23 -> MSTMP (120236)  set-temporary    */
    /*   default/tail:            106167 JPL I 21 -> MSTRM (120233)                   */
    switch (mode) {
        case 0: rc = MODLF(ent); break;    /* DeleteFile: release the file's pages   */
        case 1: rc = MRENF(ent); break;
        case 2: rc = MSPER(ent); break;
        case 3: rc = MSTMP(ent); break;
        default: rc = MSTRM(ent); break;
    }
    if (rc != 0) goto fail;                /* each JPL I is followed by JMP -> 106174 */

    /* --- success exit -----------------------------------------------------------*/
    /* 106171-106173: MIN ,B 4 (bump status), SAA -125, JMP I 16 -> 106211 (=003776,*/
    /* the resident return cell).                                                    */
    return 0;

fail:
    r->B[2] = error_no;                    /* 106174 STA ,B 2 (caller status slot)  */
    return error_no;                       /* falls to 106172/106173 resident return */
}

/* Callers (all share this dispatcher, flags preset by the entry point):
 *   MON 54B DeleteFile:  mon_dir_entry_op(r, 0, 0);   (entry 106063 MDLFI, mode 0)
 *   siblings STEFI/SPEFI/MRNFI: entries 106052/106055/106060
 *
 * User-register convention (from the manual's MAC example, INFERRED not byte-proven
 * here): X = address of the file-name string; a version number in the name deletes
 * one version, otherwise all versions are deleted.  Normal return skips; error
 * return has the error number in A.
 */
