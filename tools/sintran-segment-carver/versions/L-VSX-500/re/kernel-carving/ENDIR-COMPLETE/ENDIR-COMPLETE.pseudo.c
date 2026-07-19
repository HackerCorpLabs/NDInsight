/* ==========================================================================
 *  ENDIR-COMPLETE.pseudo.c
 *  ENTER-DIRECTORY worker (ENDIR = 140176B), segment 006-S3FS, L07.
 *
 *  Readable reconstruction of EVERY branch and error exit, from the carved
 *  006-S3FS bytes (see ENDIR-COMPLETE.ASM for the byte-exact disassembly and
 *  the dd offsets). This is pseudo-C, not compilable: it names the control
 *  flow, the SAA error codes, and the two exit styles (release vs no-release).
 *
 *  Grading in comments: VERIFIED (proven from bytes) / INFERRED (reasoned).
 *  Octal literals written 0oNNN for clarity; the machine values are octal.
 *
 *  Return convention: ENDIR returns an error code in local ,B 2. 0 / ok-return
 *  (140404 JMP 32) means the directory is mounted. Any error path stores a
 *  nonzero SINTRAN file-system error code and unwinds.
 * ========================================================================== */

/* datafield word layout used below (per ,X n):
 *   [0]  device flags / type       (bit 0o150 tested for "tape")
 *   [3]  reserve helper arg
 *   [4]  unit / reserve descriptor  (0 => unit needs no reserve)
 *  ...   name label + capacity live further in; see ../ENTER-DIRECTORY
 */

int ENDIR(int dir_index, int entering_system /*,B 26*/, int subunit /*,B 20*/)
{
    df_t *df;              /* ,B 25 directory datafield ptr (from GDIRA)     */
    int   flag_scratch;    /* ,B 24                                          */
    int   rc;              /* returned error code -> ,B 2                    */

    /* ---- prologue + argument fetch/parse (140176-140225) ---------------- */
    reserve_prologue();                       /* 140202 -> 003752B           */
    if (!argfetch_helper())                    /* 140203 -> 053174B          */
        goto parse_tail_A;                     /* 140204 -> 140770 -> EXIT_B  */
    if (!parse_helper1())                       /* 140211 -> 044777B          */
        goto EXIT_B;                           /* 140212 -> 141016            */
    if (!parse_helper2())                       /* 140213 -> 132072B          */
        goto EXIT_B;                           /* 140214 -> 141016            */
    parse_helper3();                            /* 140217 -> 010500B          */
    subunit = T;
    if (!GDIRE())                               /* 140222 -> 131732B  get-dir-entry */
        goto EXIT_C;                           /* 140223/140225 -> 141020     */

    /* ---- STAGE 2: reserve unit + datafield + device-type guards --------- */
    df = GDIRA(dir_index);                      /* 140244 -> 030225B  VERIFIED */
    helper_010500(df->w[3]);                    /* 140247                      */

    if (df->w[4] != 0) {                        /* 140250-140251: unit needs reserve? */
        int a = MON_124_ForceReserve();         /* 140252  MON 124B PRSRV  VERIFIED */
        if (a < 0) {                            /* 140253 JAP: reserve failed  */
            rc = 0o147;                         /* 140254 SAA 147  VERIFIED    */
            goto EXIT_D;                        /* 140255 -> 141024            */
            /* 147B = "device unit reserved for special use"                  */
        }
    }

    if (df->w[0] & BIT_0o150) {                 /* 140256-140257 BSKP ONE 150 DA */
        rc = 0o145;                             /* 140261 SAA 145  VERIFIED    */
        goto EXIT_COMMON;                       /* 140262 -> 141000            */
        /* 145B = "illegal on tape device"; that df[0] bit == tape = INFERRED */
    }

    if (!helper_050323())                       /* 140263                      */
        goto EXIT_COMMON;                       /* 140264 -> 141000            */
    flag_scratch = A;                           /* 140265                      */
    if (A & BIT_0o100) {                        /* 140266                      */
        if (!helper_053047())                   /* 140270                      */
            goto EXIT_COMMON;                   /* 140271 -> 141000            */
    }
    if (!helper_037377(entering_system))        /* 140272-140273               */
        goto EXIT_COMMON;                       /* 140274 -> 141000            */

    if (!ISETP(subunit))                        /* 140277 -> 050223B set-part  */
        goto EXIT_C;                            /* 140300 -> 141020            */
    if (A & BIT_0o30) {                         /* 140301                      */
        flag_scratch = A;                       /* 140303                      */
        if (!ICLEP())                           /* 140304 -> 050231B clear-part */
            goto EXIT_C;                        /* 140305 -> 141020            */
    }

    /* ---- STAGE 3: main-directory ordering guard (42B) ------------------- */
    if (!(flag_scratch & BIT_0o120)) {          /* 140306-140307 BSKP ONE 120 DA */
        /* bit clear => this is a MAIN directory being entered out of order  */
        helper_034557(entering_system);         /* 140314                      */
        rc = 0o42;                              /* 140315 SAA 42  VERIFIED     */
        report_error(rc);                       /* 140316 (INFERRED preserves A) */
        goto EXIT_E;                            /* 140317 -> 141004 (release)  */
        /* 42B = "main directory not last one released"; branch = INFERRED    */
    }

    helper_035231(entering_system);             /* 140320-140321               */
    if (!helper_035531())                       /* 140324                      */
        goto EXIT_COMMON;                       /* 140325 -> 141000            */

    /* ---- STAGE 4: name / abbreviation match vs on-unit label ------------ */
    /* byte-wise compare of requested name against the directory label read
     * from the unit (140326-140352). VERIFIED control flow.                  */
    if (requested_name != label_name) {         /* 140352 SKP IF DA EQL ST     */
        goto name_mismatch;                     /* 140353 -> 140773 -> SAA 40  */
    }
    if (!helper_041552())                       /* 140360                      */
        goto name_mismatch;                     /* 140361 -> 140773 -> SAA 40  */

    int entered = already_entered_probe();      /* 140363 -> 047402B           */
    if (entered != 0 /* and not the skip cases */) {  /* 140364/140365          */
        helper_035476(df6);                     /* 140367                      */
        rc = 0o32;                              /* 140370 SAA 32  VERIFIED     */
        goto EXIT_COMMON;                       /* 140371 -> 141000            */
        /* 32B = "directory entered" (a dir of this name already entered)     */
        /* branch = INFERRED                                                  */
    }

    /* ---- STAGE 5: do the mount via CHDSI -------------------------------- */
    block_copy(df->name_area);                  /* 140376 -> 001224B           */
    helper_035476(df6);                         /* 140400                      */
    if (!CHDSI(entering_system)) {              /* 140402 -> 037763B  VERIFIED */
        /* CHDSI itself can return 035B (WXDIR master-block xfer err) or the
         * cross-system owner reject; those codes are CHDSI's, surfaced here. */
        goto EXIT_COMMON;                       /* 140403 -> 141000            */
    }

    /* CHDSI ok-return (140404 JMP 32 -> 140436): directory is MOUNTED.
     * Post-mount in-core directory-table bookkeeping runs at 140436+.
     * Two of its guards can still fail into EXIT_E (140747, 140760). The
     * detailed field semantics past here are OPEN (partially carved).       */
    post_mount_bookkeeping();                   /* 140436+  (may goto EXIT_E)   */
    return 0;                                    /* success                     */

    /* =====================================================================
     *  ERROR EXITS. Each stores rc into ,B 2 and unwinds. The RELEASE exits
     *  additionally undo the MON 124 reserve (RLDIR + MON 125 ForceRelease).
     * ===================================================================== */

parse_tail_A:                                   /* 140770                      */
    /* JPL/JMP helper pair, then falls to EXIT_B                              */
    goto EXIT_B;                                 /* 140772 -> 141016            */

name_mismatch:                                  /* 140773                      */
    probe_035240();                             /* 140776 -> CL1DB             */
    rc = 0o40;                                  /* 140777 SAA 40  VERIFIED     */
    /* 40B = "directory not on specified unit" (name did not match label)     */
    /* falls straight through into EXIT_COMMON                                */

EXIT_COMMON:                                    /* 141000  (145/32/40/CHDSI/...) */
    B[2] = rc;                                   /* 141000 STA ,B 2             */
    report_error(rc);                           /* 141001-141002 -> 141037     */
    /* fall through 141003 JMP 2 -> 141005 into the release unwind */

EXIT_E:                                         /* 141004  (42B + post-mount)  */
    B[2] = rc;                                   /* 141004 STA ,B 2             */
    RLDIR(entering_system);                      /* 141007 -> 141041 release dir */
    /* unwind at 140723; then reserve-release cleanup: */
    if (df->w[4] != 0)                          /* 141012-141013               */
        MON_125_ForceRelease();                 /* 141014  MON 125B  VERIFIED  */
    return rc;                                   /* via shared return tail      */

EXIT_B:                                         /* 141016  (early parse errors)*/
    B[2] = rc;                                   /* 141016 STA ,B 2             */
    return rc;                                   /* 141017 -> 140726 return tail; no unit held */

EXIT_C:                                         /* 141020  (parse / part flag) */
    B[2] = rc;                                   /* 141020 STA ,B 2             */
    report_error(rc);                            /* 141021-141022 -> 141037     */
    return rc;                                   /* 141023 -> 140726 return tail */

EXIT_D:                                         /* 141024  (147B reserve fail) */
    B[2] = rc;                                   /* 141024 STA ,B 2             */
    report_error(rc);                            /* 141025-141026 -> 141037     */
    return rc;                                   /* 141027 -> 140723; MON 124 had
                                                  * failed so nothing to release */
}
