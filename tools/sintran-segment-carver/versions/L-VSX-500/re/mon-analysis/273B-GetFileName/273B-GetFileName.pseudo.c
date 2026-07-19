/* ============================================================================
 * MON 273B  GetFileName (MGFIL)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 273B-GetFileName.ASM). Control flow,
 * the SSK two-entry fork (MGFIL vs sibling DEABF) and the GFILN Get-FILe-Name
 * primitive call are BYTE-VERIFIED; the semantic labels (which field is dir /
 * user / object index / name buffer) are INFERRED from the SINTRAN III Monitor
 * Calls manual and the field copies seen - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = INDEX  (left byte = directory index, right byte = user index)
 *   A = object index ; if bit 15 of INDEX set, D = address of remote-system id
 *   X = address of buffer to receive the file name
 *   MON 273 ; JMP ERROR ; ... normal return
 * Error return: A = error number. */

int mon_getfilename(mon_regs *r)
{
    r->ssk = 1;                            /* 111013: BSET ONE SSK - MGFIL entry   */
    /* sibling DEABF enters at 111015 with SSK=0; both join here at 111016 */
    save_params(r);                        /* 111016: STD I 150 - stash caller D   */
    frame_setup();                         /* 111017-111021: SAB 165, build B frame*/
    resident_prologue();                   /* 111022: JPL I 145 -> 003752          */

    if (r->ssk) {                          /* 111023-111024: BSKP ONE SSK - fork   */
        r->b6 = 1;                         /* 111025-111026: SAA 1 ; STA ,B 6      */
        /* 111027: JMP -> 111052 (MGFIL skips the context/parse setup below)      */
    } else {
        r->b6 = 0;                         /* 111030: STZ ,B 6 (DEABF path)        */
        if (uscps(r) != OK)                /* 111034: JPL I -> USCPS 031075        */
            goto store_status;             /* 111035: JMP -> 111164 (no skip)      */
        if (flpar(r) != OK)                /* 111041: JPL I -> FLPAR 046231        */
            goto store_status;             /* 111042: JMP -> 111164 (no skip)      */
        if (uscps(r) != OK)                /* 111047: JPL I -> USCPS 031075        */
            goto store_status;             /* 111050: JMP -> 111164 (no skip)      */
        cptyp(r);                          /* 111051: JPL I -> CPTYP 030205 (uncond)*/
    }

    if (r->b6 == 0) {                      /* 111052-111053: LDA ,B 6 ; JAF 15     */
        /* JAF jumps to 111070 when b6 != 0; b6 == 0 falls through here.          */
        if (mdeab(r) != OK)                /* 111064: JPL I -> MDEAB 061044        */
            goto ambiguity_check;          /* 111065: JMP -> 111152 (no skip)      */
        /* 111066: LDA ,B 2 (skip = success) */
        goto restore_and_return;           /* 111067: JMP -> 111142                */
    }

    /* --- b6 != 0 branch (MGFIL default path, 111070-) --------------------- */
    if (name_terminated())                 /* 111071-111072: BSKP ONE 170 DT       */
        goto check_dir_user;               /*   no-skip (T bit15 == 0) -> 111132   */
    if (uscps(r) != OK)                    /* 111077: JPL I -> USCPS 031075        */
        goto store_status;                 /* 111100: JMP -> 111164 (no skip)      */
    if (flpar(r) != OK)                    /* 111104: JPL I -> FLPAR 046231        */
        goto store_status;                 /* 111105: JMP -> 111164 (no skip)      */
    /* 111106-111125: stage the dir/user/object words into the work record */
    if (resident_helper_020274(r) != OK)   /* 111126: JPL I -> 020274              */
        goto store_status;                 /* 111127: JMP -> 111164 (no skip)      */
    /* 111130: LDA ,B 0 (skip = success) */
    goto restore_and_return;               /* 111131: JMP -> 111142                */

check_dir_user:
    if (chduo(r) != OK)                    /* 111133: JPL I -> CHDUO 101303        */
        goto store_status;                 /* 111134: JMP -> 111164 (no skip)      */
    if (gfiln(r) != OK)                    /* 111137: JPL I -> GFILN 060600        */
        goto store_status;                 /* 111140: JMP -> 111164 (no skip)      */
    /* 111141: LDA ,B 0 -> falls into restore_and_return */

restore_and_return:                        /* common normal-completion tail (111142) */
    if (sucps(r) != OK)                    /* 111145: JPL I -> SUCPS 031072        */
        goto store_status;                 /* 111146: JMP -> 111164 (no skip)      */
    /* 111147: MIN ,B 4 - success: bump the caller return addr for the skip      */
    /* return.  111150: SAA -165 (return leader); 111151: JMP I -> 003776         */
    resident_return();
    return OK;                             /* normal (skip) return; name in buffer */

ambiguity_check:
    /* 111152-111163: A holds MDEAB's code.  If A == 0111 (111153 SKP IF DA EQL   */
    /* ST) ambiguous: set D bit1 and retry MDEAB (111155-111157 JMP -> 111062).   */
    /* If A == 0106 (111161 SKP IF DA EQL ST) remap to error 0113 (111163 SAA);   */
    /* otherwise store A unchanged.                                               */
    if (unmatched())                       /* 111161: SKP IF DA EQL ST (A==0106)   */
        rc = 0113;                         /* 111163: SAA 113                      */
store_status:
    r->status = rc;                        /* 111164: STA ,B 2 -> caller B+2       */
    /* 111165: JMP -> 111150 (SAA -165 leader, then resident return; no MIN bump  */
    /* = the error / non-skip return).                                            */
    return rc;                             /* A = error number on the error path   */
}

/* Byte-verified anchors:
 *   MGFIL entry 111013 (SSK=1), sibling DEABF 111015 (SSK=0), joined body 111016,
 *   frame SAB 165, prologue JPL I 145 -> 003752,
 *   GFILN call JPL I 46 -> 060600 (Get-FILe-Name primitive),
 *   status store STA ,B 2 (111164), resident return -> 003776. */
