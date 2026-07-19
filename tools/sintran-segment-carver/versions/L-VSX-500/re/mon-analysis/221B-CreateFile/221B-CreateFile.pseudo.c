/* ============================================================================
 * MON 221B  CreateFile (CRFIL)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  CRFIL shares one body with three create variants
 * (MCRFI, MCRNW, CRNVE), selected by the SSK/SSM skip-flag pair.
 *
 * Derived from the real disassembly (see 221B-CreateFile.ASM). Control flow and
 * the SSK/SSM entry split are BYTE-VERIFIED; the semantic labels (which worker
 * does what, error-number meanings, directory-entry field meanings) are INFERRED
 * from the call structure and the manual - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Entry 115425 = CRFIL (ssk=0,ssm=0); the other three entries preset ssk/ssm to
 * pick a create variant. One shared body forks on the flags. */
int mon_create_file(mon_regs *r, int ssk, int ssm)
{
    save_params(r);                        /* 115440 STD I 62 - stash caller D      */
    frame_setup();                         /* 115441-115443 SAB 133, build B frame  */
    prologue_worker();                     /* 115444 JPL I 57 -> 003752 (resident)  */

    /* 115445-115461: derive a variant/mode code (0..3) from SSM then SSK          */
    mode = variant_from_flags(ssm, ssk);   /* stored at B+117 (115462 STA ,B 117)   */

    /* 115463-115521: parse the file-name string and locate/allocate the owning    */
    /* directory (indirect JPL I workers through link cells 115527/115530/...).     */
    /* A directory-object record is built at X (LDX I 115 = base) with fields       */
    /* X+24..X+27 (name / type / attributes) set from frame slots 106/110/112.      */
    dir = open_directory(r->filename);     /* 115535-115560                         */

    /* 115561-115566: StartAddress present?  -> contiguous/allocated vs indexed.    */
    is_contiguous = (r->start_addr != 0);  /* B+120 = 1 if start given, else 0      */

    /* 115574-115626: classify request (SAT 3 / SAT 1 tests on the type word) and   */
    /* read the directory object's page/entry descriptors (LDD ,X 0) into the frame */
    /* (B+110 file no, B+112/B+114/B+115 descriptors).                              */

    /* 115627-115656: version handling - if a same-name entry exists (B+121/B+122)  */
    /* compute the new version's page range (B+125/B+126 = start/count).            */
    if (name_collision(dir))
        version = next_version(dir);       /* 115634 JMP I 41 on the no-collision   */

    /* --- allocation loop (115677-116013) -----------------------------------------*/
    /* Walk the directory bitmap / page chain allocating NoOfPages contiguous or     */
    /* building the indexed structure; B+125/B+126/B+127 track running page cursor,  */
    /* B+131/B+132 the limits.  JPL I 116/62/44 -> allocation + write-back workers.  */
    for (;;) {
        entry = alloc_next_page(dir);      /* 115714-115753                          */
        if (entry == r->limit) break;      /* 115727/115762 SKP IF DA EQL ST         */
    }
    write_directory_entry(dir);            /* 115777-116012: store page/start words  */
                                           /*   into X+60/X+64/X+44 and flush         */

    /* --- status + return (116044-116124) -----------------------------------------*/
    /* Each error path loads a code with SAA (e.g. 67) and funnels to 116044         */
    /* STA ,B 2 (caller status slot); the exit link cell 116043 = 003776 returns.   */
    r->B[2] = status;                      /* 116044 STA ,B 2                        */
    return status;                         /* 116043 JMP I 10 -> 003776 (resident)   */
}

/* Callers (all share this body, flags preset by the entry point):
 *   MON 221B CreateFile:        mon_create_file(r, 0, 0);   (entry 115425 CRFIL)
 *   NewFileVersion / variants:  entries 115430/115433/115436 (MCRFI/MCRNW/CRNVE)
 *
 * User-register convention (from the manual's MAC example, INFERRED not byte-proven
 * here): X = address of file-name string; AD (double) = StartAddress (0 = let the
 * system place a contiguous/indexed file); T = address of a double word holding
 * NoOfPages (0 = indexed file).  Normal return skips; error return has the error
 * number in A.
 */
