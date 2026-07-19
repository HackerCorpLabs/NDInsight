/* ==========================================================================
 * RESERVE / LEAV2 / ENTER-DIRECTORY error paths - readable pseudo-C
 * SINTRAN III VSX/500 L07.  Octal in comments; C literals are decimal/hex.
 * Grades: VERIFIED (from carved bytes) / INFERRED / OPEN.
 * ========================================================================== */

/* --------------------------------------------------------------------------
 * PART 1a - MON 124B ForceReserve (PRSRV) : the 2-word trampoline
 * [071-S3SM, base 30000B]. VERIFIED bytes; see ../PRSRV-124B/.
 * ------------------------------------------------------------------------ */
int PRSRV(void)                     /* MON 124B */
{
    A = 3;                          /* 037076 LDA -56 -> MEM[037020]=3   VERIFIED */
    goto LEAV2;                     /* 037077 JMP I 10 -> 027417=LEAV2   VERIFIED */
    /* PRSRV does nothing else. It never touches the reserve executor EXECC. */
}

/* --------------------------------------------------------------------------
 * PART 1b - LEAV2 : PRSRV's jump target, now carved.
 * Overlay chosen by sibling-coherence: 013-S3SCP == 041-S3IMED (base 26000B),
 * where the whole leave family 3ENTE/L3EAV/LEAVX/LEAV2/3LEAV lands on parallel
 * entries. LEAV2 is a "pop one context frame and leave" routine.
 *
 * Frame stack: framePtr = FRAME[-116] (,B -116); frames are 11 words (013 oct).
 * Each family entry differs only in its SUB bound (L3EAV 63, LEAVX 50,
 * LEAV2 36, 3LEAV 25) = which level to leave to.
 * ------------------------------------------------------------------------ */
void LEAV2(void)                    /* entry with A = 3 from PRSRV */
{
    savedF = F;                     /* 027417 STF ,B -153  (A=3 still live) VERIFIED */
    A = FRAME[-116];                /* 027420 LDA ,B -116  ** A=3 CLOBBERED ** VERIFIED */
    A -= 11;                        /* 027421 AAA -13  descend one 11-word frame VERIFIED */
    FRAME[-116] = A;                /* 027422 STA ,B -116 */
    X = A;                          /* 027423 */
    if (A - MEM[36] < 0)            /* 027424 SUB 36 ; 027425 JAN -> refill */
        goto LEAV2_underflow;       /* stack underflow: reset+refill */
    MEM[X + 1] += 1;                /* 027426 MIN ,X 1  frame counter++ */
    /* fall into the common restore tail */

restore_tail:                       /* 027437 - shared by the leave family */
    F_hi   = MEM[X + 2];            /* 027437-027444 restore saved F fields */
    F_mid  = MEM[X + 5];
    F_lo   = MEM[X + 8];
    D      = MEM[X + 0];            /* 027445 saved return context */
    L      = D;                     /* 027447 ** return link := popped frame word 0 ** */
    F      = savedF;                /* 027450 */
    return; /* EXIT to the popped context (027451) */

LEAV2_underflow:                    /* 027452 */
    FRAME[-116] = 10;               /* reset framePtr to base */
    refill();                       /* 027456 JPL I 6 -> [027464] */
    /* ... */
}
/* HONEST RESULT (PART 1b):
 *   Under this overlay LEAV2 overwrites A=3 at 027420 and never reads it, and
 *   A at EXIT is not 3. So PRSRV = "set A=3, then monitor-leave/return"; it
 *   runs NO reserve executor and raises NO 147B. This matches the live console
 *   (no 147B on the failing SCSI mount; MON 124 is passed).
 *   OPEN: whether 013-S3SCP is the overlay actually mapped at 027417 when the
 *   System Monitor runs PRSRV is a runtime fact not decidable from static
 *   bytes; and whether A=3 is a vestigial constant or a live selector in some
 *   other co-resident overlay is therefore OPEN. What IS anchored: MON 124
 *   returns success on every device type, SCSI included. */

/* --------------------------------------------------------------------------
 * PART 1c - RESRV/RELES/PRLS : the REAL reserve/release primitives.
 * Unlike PRSRV, these funnel into the shared executor EXECC, which SYNTHESISES
 * a command line and runs it through the @-command interpreter (MON 70B=COMSB).
 * [071-S3SM, base 30000B]. VERIFIED entries + both MON 70 sites.
 * ------------------------------------------------------------------------ */
void EXECC(void)                    /* 037110B, shared by RESRV/PRLS/RELES */
{
    if (gate_word == 0) return;     /* 037110-037112 early EXIT when gate clear */
    T = L;                          /* save link */
    build_command_string();         /* 037120 JPL I 45 -> 004177 (low res, OPEN) */
    L = T;
    MON(70);                        /* 037124 MON 70B -> COMSB : run "RESERVE .."  VERIFIED */
    for (X = 0; scan_command_line(); )   /* 037125-037146 byte scan of the line */
        ;
    /* PRLS arm: */
    if (field_delim())              /* 037147-037153 */
        MON(70);                    /* 037154 MON 70B -> COMSB : run "RELEASE .."  VERIFIED */
}
/* RESRV=037103 (MON 122B), PRLS=037147 (MON 125B), RELES=037156 (MON 123B)
 * are the entry stubs that marshal args and drop into EXECC. So a plain
 * ReserveResource/ReleaseResource is implemented as "build+execute a
 * RESERVE/RELEASE command". Only these issue a MON call (MON 70B). */

/* --------------------------------------------------------------------------
 * PART 2 - ENTER-DIRECTORY error exits + the FILSYS error-return convention.
 * [006-S3FS, base 26000B]. VERIFIED SAA codes + funnel + common tail.
 * ------------------------------------------------------------------------ */

/* The skip-return convention used by every 006-S3FS worker: */
int filsys_worker(...)              /* returns 0 = ok (skip return), else code */
{
    /* ... on success: */
    FRAME[4] = 1;                   /* MIN ,B 4  set OK marker (e.g. 140722/037666/037743) */
    return_via_SPOP();              /* 003776 SPOP -> L+1 (skip) : caller's normal path */

    /* ... on error: */
    A = ERRCODE;                    /* SAA <code> */
    goto error_exit;                /* JMP I <ptr> to a per-site error exit */
}

/* The five ENDIR raise sites (VERIFIED SAA immediates):
 *   140254 SAA 147  -> 147B device unit reserved for special use
 *   140261 SAA 145  -> 145B illegal on tape device
 *   140315 SAA 42   -> 42B  main directory not last one released
 *   140370 SAA 32   -> 32B  directory entered (already-entered guard)
 *   037747 SAA 35   -> 35B  master block transfer error (WXDIR write-back)   */

void error_exit(int code)           /* 141000 / 141004 / 141016 / 141020 / 141024 */
{
    FRAME[2] = code;                /* STA ,B 2  park the error code            VERIFIED */
    UNLOC(dir_lock);                /* JPL I -> 010506  release the directory lock */
    if (dir_datafield[4] != 0)      /* 141011-141013  only if the unit was reserved */
        MON(125);                   /* 141014 MON 125 ForceRelease : un-reserve  VERIFIED */
    goto common_tail;               /* -> 140723 / 140726 */
}

void common_tail(void)              /* 140723 */
{
    UNLOC(dir_lock);                /* 140725 JPL I 14 -> 010506 UNLOC */
    A = -027;                       /* 140726 SAA -27  negative return marker */
    return_via_SPOP();              /* 140727 JMP I 15 -> 003776 SPOP : L+0 (non-skip) */
    /* No FRAME[4] OK-marker was set -> SPOP takes the ERROR return, so the
     * CALLER falls into its "JMP I <err>" and the code propagates upward with
     * A / FRAME[2] carrying it. */
}

/* How the code reaches the console:
 *   The @-command executor receives the FILSYS error return, and on its error
 *   path calls the resident error-message printer:
 *       ERMSG(code)  @ 016714B    (ERMON @ 114574B)
 *   which looks the numeric code up in the SINTRAN error-message table and
 *   writes the text to the terminal as "<message> (<code>B)". ERMSG's BODY is
 *   in a lower resident overlay (016714 < 26000B) and is OPEN here; its ROLE
 *   is named/known. Message texts live in 005-S3ERRS / 011-S3ERRL / 014-S3ERRP
 *   (error-string segments, not carved here). */

/* RED HERRING (already established, repeated as a known non-mount code):
 *   "APPROACHING END OF ACCOUNTING FILE" = error 243B, set ONLY in
 *   RP-P2-ACCRT (the RT-accounting collector at LOGOUT). It has NO call edge
 *   from the ENTER-DIRECTORY mount path. Not raised by the mount. */
