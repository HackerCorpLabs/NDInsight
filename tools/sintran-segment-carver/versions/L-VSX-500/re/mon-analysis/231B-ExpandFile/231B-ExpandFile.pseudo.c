/* ============================================================================
 * MON 231B  ExpandFile (EXPFI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  EXPFI is one of four sibling entries into a shared
 * directory create/allocate dispatcher (SFACC, EXPFI, CRALN, CRALF); each presets
 * the SSK/SSM skip-flag pair to select the operation, and all join at 105564.
 *
 * Derived from the real disassembly (see 231B-ExpandFile.ASM).  Control flow and
 * the SSK/SSM -> mode fold are BYTE-VERIFIED; the semantic labels (which worker
 * does what, error-number meanings) are INFERRED from the FILSYS symbol table and
 * the call structure - treat as a model, not gospel.  Addresses in comments octal.
 * Every instruction is translated per ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 * ============================================================================ */

/* Entry 105555 = EXPFI presets ssm=1, ssk=0.  Siblings enter above with other
 * combos and join at 105564.  ssm is the high bit, ssk the low bit of the mode. */
int mon_dir_alloc_op(mon_regs *r, int ssk, int ssm)
{
    save_params(r);                        /* 105564 STD I 77   - stash caller D    */
    /* 105565 RADD CLD SL DA => A = L ; 105566 RADD CLD SB DD => D = B  (COPY idiom, */
    /* CLD zeroes dest then adds source, per semantics 3.5).                         */
    frame_setup();                         /* 105567 SAB 145 - build 145B-word B     */
    prologue_worker();                     /* 105570 JPL I 74 -> 003752 (resident)   */

    /* 105571-105574: fold the mode.  105571 RADD CLD 0 DA => A = 0; 105572 SHA LIN 2 */
    /* shifts A left 2 with the M(link) bit filling the vacated low bits (EMULATOR-  */
    /* authoritative LIN fill, semantics 8); 105573 BSET BAC 0 DA sets A bit0 = K.    */
    /* The result is the 0..3 operation mode from (ssm,ssk).  EXPFI => 2.            */
    mode = fold_mode(ssm, ssk);            /* 105574 STA ,B 123                      */

    /* 105575-105621: copy caller parameter words (indirect via link cells 105665..) */
    /* into the frame (B+140/141/142 = returned handles; B+135/136/137/143/144 =     */
    /* page/entry descriptors).                                                       */
    unpack_params();

    /* 105622-105720: set the user/context (USCPS 031075 / USCPB 031067) and read the */
    /* directory object.  Each JPL I is followed by JMP -> 106010 (store status).     */
    if (set_user_context(mode) != 0) goto fail;   /* 105625/105636/... JPL I -> USCPS/USCPB */

    /* 105721-105776: second-phase dispatch by mode.  Each branch compares B+123 to a */
    /* constant (SAT 3/2) and, on match, calls the matching worker through a link cell:*/
    /*   mode 3: 105732 JPL I 77 -> MSFLA (120752)  set-file-allocation               */
    /*   mode 2: 105743 JPL I 67 -> MEXFI (116623)  expand-file        <- ExpandFile   */
    /*   mode 0/1: 105756/105761/105771/105775 -> MCRFI/MCRNW/MALFI/MALNE create/alloc */
    switch (mode) {
        case 3: rc = MSFLA(ent); break;    /* 105732 */
        case 2: rc = MEXFI(ent); break;    /* 105743  ExpandFile: grow the file       */
        case 1: rc = (r->B[2] || r->B[3]) ? MALNE(ent) : MCRNW(ent); break;
        case 0: rc = (r->B[2] || r->B[3]) ? MALFI(ent) : MCRFI(ent); break;
        default: rc = 0; break;
    }
    if (rc != 0) goto fail;                /* each JPL I is followed by JMP -> 106010  */

    /* --- success exit (105777-106007) ------------------------------------------- */
    /* 105777 MIN ,B 4 (bump status); 106000-106005 write the returned handles back   */
    /* through link cells 106037/106040/106041; 106006 SAA -145; 106007 JMP I 33 ->   */
    /* 106042 = 003776 (resident return).                                             */
    return 0;

fail:
    r->B[2] = error_no;                    /* 106010 STA ,B 2 (caller status slot)    */
    return error_no;                       /* funnels to the resident return          */
}

/* Callers (all share this dispatcher, flags preset by the entry point):
 *   MON 231B ExpandFile: mon_dir_alloc_op(r, 0, 1);   (entry 105555 EXPFI, mode 2)
 *   siblings SFACC/CRALN/CRALF: entries 105552/105560/105562 (modes 3/1/0)
 *
 * User-register convention (from the manual's MAC example, INFERRED not byte-proven
 * here): X = address of the file-name string; T = address of a double word giving
 * NoOfPages (additional pages).  Normal return skips; error return has the error
 * number in A.
 */
