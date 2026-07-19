/* ============================================================================
 * MON 327B  FileSystemFunction  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  File-system information/maintenance call.
 *
 * Derived from the real disassembly (see 327B-FileSystemFunction.ASM). The
 * modelled body is the carved file-system worker GTYPR (113312B, "Get TYP-RING")
 * in segment 006-S3FS. Control flow is BYTE-VERIFIED; the semantic labels
 * (which descriptor field means what, the caller-visible register contract) are
 * INFERRED from the code shape - treat as a model, not gospel.
 *
 * IMPORTANT (see README "Honest caveats"): the byte-verified MON-327 dispatch
 * target is 112503B in 025-S3IRPIT, NOT this worker. GTYPR is NOT statically
 * reachable from 112503B in the carved bytes; the link is the uncarved resident
 * CALLPROC/segment switch. So this models a plausible worker, not a proven path.
 * Addresses in comments are octal.
 * ============================================================================ */

/* B is the routine's local data frame (NPL/PLANC convention). Slots used:
 *   B+57  table-walk counter        B+64  flags accumulator
 *   B+60  saved table entry ptr      B+65  status accumulator
 *   B+61  type value (classified)    B+66  saved descriptor ptr
 *   B+62  function selector (1/2/3)  B+0/2/3/4  return fields (written at exit)
 *   B+63  result value
 * X points to a multi-word file/object descriptor (fields ,X 0/1/3/6/7/11/22/23/44/45).
 */

int mon_327_gtypr(mon_regs *r)          /* GTYPR @113312B */
{
    /* 113317-113331: normalise/range-check the function code in A against limit
     * words held P-relative. 113321 LDT 172: T = mem[P+172]; 113322 SKP IF DA GRE
     * ST: skip if (signed) A >= T. 113324 AAT 100: T += literal 0100. 113325 SKP
     * IF DA LST ST: skip if (signed) A < T. 113327 SUB 165: A -= mem[P+165] - LDT
     * 172 and SUB 165 are memory operands (mem[P+disp]), NOT literals; only AAT is
     * an immediate. Result stashed into B+63 (113330 STA ,B 63).                */
    int fn = range_check(r->A);         /* 113321 LDT 172 .. 113330 STA ,B 63  */

    /* 113332-113510: scan the descriptor at X, walking a 2-word/entry table
     * (LDX ,B 60 / AAX 2 / MIN ,B 57), testing attribute bits with BSKP, until
     * an entry matches. Builds the classified type value into B+61 and a
     * function selector (1/2/3) into B+62. On a null/short descriptor it takes
     * the error path (JMP I -> 114016).                                        */
    desc = scan_descriptor(r->X, &B);   /* fills B+60..B+63 */

    /* 113576-113640: whitelist membership test over B+61. Each step is
     * `SAT n; SKP IF DA UEQ ST; JMP -> 113641`; SKP..UEQ skips the JMP when
     * B[61] != n, so a MATCH takes the JMP to the secondary dispatch at 113641
     * (also entered for B[61] >= 100 via 113601 LST, and B[61] == 0 via 113636
     * JAZ). A value matching NONE of {3,20,21,25,32,33,34,40,41} (octal) falls
     * through to 113637 SAX 1 (X = 1) then 113640 JMP -> 113765 result assembly.
     * NOTE: the earlier draft had this branch inverted.                        */
    if (!is_whitelisted_type(B[61])) {  /* fall-through: 113637 SAX 1; -> 113765 */
        ring = 1;                       /* 113637 SAX 1 (X := 1)                 */
    } else {                            /* match -> 113641 secondary dispatch     */
        /* 113641 LDA ,B 62; 113642 JAF -> 113717: if function selector B[62] != 0
         * run the descending threshold ladder 113717-113764 over B+61, comparing
         * against {100,76,74,72,70,66,64,62,60,56,54,52} to derive a ring/category
         * index (consistent with GTYPR = Get TYP-RING); if B[62] == 0 a secondary
         * descriptor scan (113643-113704) runs instead.                          */
        ring = classify_size(B[61]);
    }

    /* 113765-114004: fold status/flag bits (BSET ONE 10 DA on B+65, etc.).     */
    set_status_bits(&B, ring);

    /* 114005-114013: publish results into the caller's B-frame return slots.   */
    B[3] = B[64];                       /* 114006 flags  */
    B[0] = B[65];                       /* 114010 status */
    B[2] = B[63];                       /* 114012 value  */

    B[4]++;                             /* 114013 MIN ,B 4 - NORMAL return only  */
    return_via_callproc(003776B);       /* 114015 JMP I 20 -> ptr 003776         */

error_return:                           /* 114016: entered from an error JMP I   */
    B[2] = r->A;                        /* 114016 STA ,B 2 - store A as value     */
    /* 114017 JMP -3 -> 114014 : joins the exit but SKIPS the MIN ,B 4, so the
     * caller's skip/return-index field is NOT bumped = error indication.        */
    return_via_callproc(003776B);
}

/* Resident helper pointer words embedded in the body (targets outside the
 * carved window, NOT resolved to names here):
 *   003752, 010376, 020274, 055566, 056307, 071413, 003776
 */
