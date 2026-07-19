/* ============================================================================
 * MON 322B - GSGNO / GetSegmentNo - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07. Given a segment name, return that segment's number
 * (negative / error return = no such segment).
 *
 * Derived from the carved bytes in 322B-GetSegmentNo.ASM. The dispatch chain and
 * the worker's control flow (helper lookup, IOF/ION critical section, two-way
 * return) are VERIFIED from bytes; the name-match detail is INFERRED.
 *
 * CORRECTED 2026-07-13. The previous version located the worker via a fictional
 * "GOTAB" DSI7 stub and read GSGNO from SINTRAN-DATA_commoncode. The real worker
 * is carved in 003-S3CP.
 *
 * Dispatch: MON 322B -> ENT14 072167B -> GOTAB[322B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[322B] @006142B = 041424B = GSGNO (worker below).
 * All constants octal.
 * ============================================================================
 */

int GSGNO(mon_regs *r)              /* entry 041424B; args in ,B -200 / -177 / -176 */
{
    int idx = name_lookup();        /* 041424B: JPL I 26 (find segment by name) */
    ion();                          /* 041425B: ION */

    word a0 = mem_B[-0200];         /* 041426B: LDA ,B -200 (arg word 0) */
    /* D := a0 (041427B) */
    word a1 = mem_B[-0177];         /* 041430B */
    if (!helper35(a1))              /* 041431B: JPL I 35 */
        ;
    word a2 = mem_B[-0176];         /* 041432B */
    if (!helper34(a2))              /* 041433B: JPL I 34 */
        return_error();             /* 041434B: JMP I ,B -36 (not found) */

    ioff();                         /* 041436B: IOF - critical section */
    word x = mem[036];              /* 041437B: LDX 36 */
    if (mem[x + 1] == mem_B[-0177]) {   /* 041440B-041441B: match test */
        /* ... 041443B-041445B: refine via mem[x+13] ... */
    }
    ion();                          /* 041446B: ION */
    return_ok();                    /* 041447B: JMP I ,B -36 -> normal return, A = seg number */
}

/* Caveats for the emulator author:
 *   - The helper-lookup calls, the IOF/ION critical section around the segment
 *     table walk, and the two-way (found / not-found) return are byte-proven.
 *   - The precise segment-name comparison and the table at mem[036] are INFERRED
 *     from structure; the name-match detail is not fully isolated in these bytes.
 */
