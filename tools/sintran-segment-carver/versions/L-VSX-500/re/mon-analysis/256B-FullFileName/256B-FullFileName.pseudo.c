/* ============================================================================
 * MON 256B  FullFileName (DEABF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ("de-abbreviate file"; the NC disassembler calls this
 * call "FNAME" - the authoritative name is FullFileName, mnemonic DEABF.)
 *
 * Derived from the real disassembly (see 256B-FullFileName.ASM). Control flow
 * and the calls to the file-system workers (FLPAR, USCPS, SUCPS, GFILN, MDEAB,
 * CHDUO, CPTYP) are BYTE-VERIFIED; the semantic labels and error-number meanings
 * are INFERRED from the FILSYS symbol names and the SINTRAN III Monitor Calls
 * manual - treat as a model, not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   X = address of the abbreviated file-name string (64 chars)
 *   A = address of the buffer to receive the full file name (terminated by ')
 *   T = address of the default file type string (4 chars; ND-100 only)
 * Error return: A = error number. Returns directory, user, name, type, version.
 * The abbreviation must be unambiguous and the caller needs read access. */

int mon_fullfilename(mon_regs *r)
{
    frame_setup();                         /* 111015-111021: SAB 165, build B    */
    resident_prologue();                   /* 111022: JPL I -> 003752            */

    if (default_type_given()) {            /* 111023-111024: BSKP ONE SSK        */
        flag = 1;                          /* 111025-111026: STA ,B 6            */
    } else {
        flag = 0;                          /* 111030: STZ ,B 6                   */
        if (set_user_context(r) != OK)     /* 111034: JPL I -> USCPS 031075      */
            goto store_status;             /* 111035: JMP -> 111164              */
        if (parse_file_name(r) != OK)      /* 111041: JPL I -> FLPAR 046231      */
            goto store_status;             /* 111042: JMP -> 111164              */
        set_user_context_2(r);             /* 111047: JPL I -> USCPS 031075      */
        copy_type(r);                      /* 111051: JPL I -> CPTYP 030205      */
    }

    if (flag == 0) {                       /* 111052-111053: LDA ,B 6 ; JAF 15   */
        /* JAF jumps to 111070 when A(flag) != 0; flag==0 falls through here.    */
        if (match_deabbrev(r) != OK)       /* 111064: JPL I -> MDEAB 061044      */
            goto ambiguity_check;          /* 111065: JMP -> 111152 (no skip)    */
        /* 111066: LDA ,B 2 (skip = success) */
        goto restore_and_return;           /* 111067: JMP -> 111142              */
    }

    /* --- flag != 0 branch (default-type path, 111070-) --------------------- */
    if (name_terminated())                 /* 111071-111072: BSKP ONE 170 DT     */
        goto check_dir_user;               /*   no-skip (T bit15 == 0) -> 111132 */
    if (set_user_context_3(r) != OK)       /* 111077: JPL I -> USCPS 031075      */
        goto store_status;                 /* 111100: JMP -> 111164 (no skip)    */
    if (parse_file_name_2(r) != OK)        /* 111104: JPL I -> FLPAR 046231      */
        goto store_status;                 /* 111105: JMP -> 111164 (no skip)    */
    /* 111106-111125: stage the directory/user/type words into the work record */
    if (aux_worker(r) != OK)               /* 111126: JPL I -> 020274            */
        goto store_status;                 /* 111127: JMP -> 111164 (no skip)    */
    /* 111130: LDA ,B 0 (skip = success) */
    goto restore_and_return;               /* 111131: JMP -> 111142              */

check_dir_user:
    if (check_duo(r) != OK)                /* 111133: JPL I -> CHDUO 101303      */
        goto store_status;                 /* 111134: JMP -> 111164 (no skip)    */
    if (get_file_name(r) != OK)            /* 111137: JPL I -> GFILN 060600      */
        goto store_status;                 /* 111140: JMP -> 111164 (no skip)    */
    /* 111141: LDA ,B 0 -> falls into restore_and_return */

restore_and_return:                        /* common normal-completion tail (111142) */
    if (restore_user_context(r) != OK)     /* 111145: JPL I -> SUCPS 031072      */
        goto store_status;                 /* 111146: JMP -> 111164 (no skip)    */
    /* 111147: MIN ,B 4 - success: bump the caller return addr for the skip     */
    /* return.  111150: SAA -165 (normal-return leader); 111151: JMP I -> 003776 */
    resident_return();
    return 0;                              /* normal (skip) return               */

ambiguity_check:
    /* 111152-111163: A holds MDEAB's code.  If A == 0111 (111153 SKP IF DA EQL  */
    /* ST) the match was ambiguous: set D bit1 and retry MDEAB (111155-111157    */
    /* JMP -> 111062).  If A == 0106 (111161 SKP IF DA EQL ST) remap to error    */
    /* 0113 (111163 SAA 113); otherwise store A unchanged.                       */
    if (unmatched())                       /* 111161: SKP IF DA EQL ST (A==0106) */
        status = 0113;                     /* 111163: SAA 113                    */
store_status:
    r->status = status;                    /* 111164: STA ,B 2 -> caller B+2     */
    /* 111165: JMP -> 111150 (SAA -165 leader, then resident return; no MIN      */
    /* bump = the error / non-skip return).                                      */
    return status;                         /* A = error number on the error path  */
}

/* Byte-verified anchors:
 *   DEABF entry 111015, frame SAB 165, prologue JPL I -> 003752,
 *   USCPS 031075, FLPAR 046231, CPTYP 030205, MDEAB 061044, CHDUO 101303,
 *   GFILN 060600, SUCPS 031072, error 113 (SAA 113), status store STA ,B 2
 *   (111164), resident return -> 003776. */
