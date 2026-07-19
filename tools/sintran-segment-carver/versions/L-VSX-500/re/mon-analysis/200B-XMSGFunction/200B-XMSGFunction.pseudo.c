/* ============================================================================
 * MON 200B  XMSGFunction (XMSG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  The single entry point to the XMSG inter-process
 * message system.  One MON 200 call = one XMSG function.  The T register carries
 * the function code (an XF* value 0..47) in its low byte and option bits in its
 * high byte; parameters ride in A/D/X; status returns in T.
 *
 * Dispatch reality:
 *   GOTAB[200B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED).  There is no direct
 *   GOTAB handler word, so the level-14 handler is reached through the resident
 *   MFELL/CALLPROC path - NOT present in any carved segment (uncarved bridge).
 *   The handler then switches on the XF* code in T (a GOSW / jump table).
 *
 *   The XMSG worker body is NOT isolated in the carve set: 2XMSG resolves to
 *   200B inside a 14-symbol cram (relocation/data artifact), and MXMSG=75202B
 *   sits in a banked-overlay data cluster whose bytes differ per overlay.  So
 *   the model below is of the DOCUMENTED behaviour only (XMSG version-M
 *   constants + SINTRAN manuals), NOT of carved code - see README caveats.
 * Addresses in comments are octal.
 * ============================================================================ */

/* XF* function codes (version-M).  DOCUMENTED, not byte-proven from this carve. */
enum {
    XFDUM = 0,  XFDCT = 1,  XFGET = 2,  XFREL = 3,  XFRHD = 4,  XFWHD = 5,
    XFREA = 6,  XFWRI = 7,  XFSCM = 8,  XFMST = 9,  XFOPN = 10, XFCLS = 11,
    XFSND = 12, XFRCV = 13, XFPST = 14, XFGST = 15, XFSIN = 16, XFSRL = 17,
    XFABR = 18, XFABW = 19, XFMLK = 20, XFMUL = 21, XFM2P = 22, XFP2M = 23,
    XFRIN = 24, XFCRD = 25, XFSTD = 26, XFDIB = 27, XFRIB = 28, XFWIB = 29,
    XFPRV = 30, XFRTN = 31, XFRRH = 32, XFDUB = 33, XFWDF = 34, XFDBK = 35,
    XFSMC = 36, XFDMM = 37, XFALM = 38, XFFRM = 39, XFLMP = 40, XFRRE = 41,
    XFCPV = 42, XFWRT = 43, XFMRT = 44, XFSFM = 45, XFCRR = 46, XFGSM = 47
};

/* T-register option bits (high byte), version-M.  DOCUMENTED. */
#define XFSYS (1 << 7)   /* system-mode call                       */
#define XFTCM (1 << 8)   /* send the task-current message          */
#define XFSEC (1 << 9)   /* secure message (return if undelivered) */
#define XFROU (1 << 10)  /* message addressed to XROUT             */
#define XFFWD (1 << 11)  /* forward, keep original sender          */
#define XFBNC (1 << 12)  /* bounce message                         */
#define XFHIP (1 << 13)  /* high-priority (queue at head)          */
#define XFWAK (1 << 14)  /* wake task on status change             */
#define XFWTF (1 << 15)  /* wait if operation not terminated       */

/* XE* completion codes returned NEGATIVE in T (subset; version-M).  DOCUMENTED. */
#define XEILF (-18)      /* illegal function code   */
#define XEIMA (-19)      /* invalid magic number    */
#define XEIPN (-22)      /* illegal port number     */
#define XEPRV (-23)      /* privileged fn w/o privilege */
#define XENRU (-37)      /* XMSG not running        */

/*
 * MON 200B entry.  DOCUMENTED model: the resident handler is a GOSW on the XF*
 * function code in the low byte of T.  Real bytes for the worker are NOT in the
 * carve (only symbol-cram / overlay-cluster artifacts land there), so every
 * per-function body below is a NAMED STUB flagged UNVERIFIED, not carved code.
 */
int mon_200B_XMSGFunction(mon_regs *r)
{
    /* GOTAB[200B]=000000 (BYTE-VERIFIED): dispatch falls through to the resident
     * MFELL/CALLPROC handler (UNCARVED); it decodes T as function + options. */
    int  func    = r->T & 0377;        /* low byte  = XF* function code   */
    int  options = r->T & 0177400;     /* high byte = option bits (XF*)   */

    if (func < XFDUM || func > XFGSM)
        return (r->T = XEILF);          /* illegal function -> negative status */

    /* GOSW on the function code.  All bodies UNVERIFIED (uncarved worker):
     * the register maps are DOCUMENTED (COSMOS Programmer Guide / XMSG-API). */
    switch (func) {
    case XFDUM:  /* UNVERIFIED: dummy / get message-system configuration       */
    case XFDCT:  /* UNVERIFIED: disconnect this task from the message system   */
    case XFGET:  /* UNVERIFIED: A=NBYTES -> A=MESAD ; reserve a message buffer */
    case XFREL:  /* UNVERIFIED: release the current message buffer             */
    case XFRHD:  /* UNVERIFIED: A=MESAD -> AD=B0to3, X=B4to5 ; read 6-byte hdr */
    case XFWHD:  /* UNVERIFIED: AD=B0to3, X=B4to5 ; write 6-byte header        */
    case XFREA:  /* UNVERIFIED: D=NBYTES,A=UADD,X=DISP ; read message -> user  */
    case XFWRI:  /* UNVERIFIED: D=NBYTES,A=UADD,X=DISP ; write user -> message */
    case XFOPN:  /* UNVERIFIED: -> A=PORTNO ; open a local port                */
    case XFCLS:  /* UNVERIFIED: A=PORTNO ; close a port                        */
    case XFSND:  /* UNVERIFIED: AD=MAGNO, X=PORTNO(+opts) ; send message       */
    case XFRCV:  /* UNVERIFIED: A=PORTNO(+XFWTF/XFWAK) -> T=METYP,A,D,X ; recv */
    case XFP2M:  /* UNVERIFIED: A=PORTNO -> AD=MAGNO ; port -> magic (local)   */
    case XFM2P:  /* UNVERIFIED: AD=MAGNO -> A=PORTNO,D=SYSNO,X=INDEX           */
    default:
        /* Selected function body runs against the resident XMSG tables (ports,
         * buffers, XT-blocks, routing) - NONE of which is in this carve. */
        r->T = xmsg_function_body(func, options, r);  /* UNVERIFIED (uncarved) */
        break;
    }

    /* A/D/X are NOT preserved across the call (DOCUMENTED); the caller reloads
     * them.  T holds completion status: >0 success (function-specific), 0 = not
     * terminated (no wait / nothing ready), <0 = an XE* error code. */
    return r->T;
}

/* Caveats for the emulator author:
 *   - GOTAB[200B]=000000 (fall-through) is BYTE-VERIFIED; there is no entry stub
 *     to model.  Dispatch enters the resident MFELL/CALLPROC (UNCARVED).
 *   - The T-GOSW and every per-function body are DOCUMENTED (XMSG version-M
 *     constants + SINTRAN manuals), NOT byte-derived: the worker is not isolated
 *     in the carve (2XMSG=200B is a 14-symbol cram; MXMSG=75202B is an overlay
 *     data cluster).  Treat xmsg_function_body() as UNVERIFIED.
 *   - The ND-500 siblings A5XMS/B5XMS (MON 512B/513B) are byte-verified handlers
 *     that RE-ISSUE this MON 200 on behalf of an ND-500 process.
 *   - A live PC trace (break on a real MON 200, single-step the fall-through and
 *     the T-GOSW) is needed to confirm the real worker and its table.
 */
</content>
