/* ============================================================================
 * MON 157B  SegmentToPageTable (ENTSG / documented @ENTSEG)  -  pseudo-C model
 * SINTRAN III VSX/500 L.  Enters a routine as a direct task or device driver and
 * records which segment/page-table it uses in the Page Index Table (PIT), so it
 * can be re-entered at restart after a power failure. The segment must be fixed
 * in memory. Up to 24 segments are remembered.
 *
 * Control flow is BYTE-VERIFIED for the ENTSG worker at 067764B in overlay
 * 025-S3IRPIT; the pseudo-C below models the parameter-processing + physical
 * page-table population core (an EXCERPT - the real routine continues past the
 * shown window before returning). Physical transfers (LDDTX/LDXTX/LDATX) are
 * 24-bit PHYSICAL accesses that BYPASS the MMU, per ND100-INSTRUCTION-SEMANTICS.md
 * section 5. Addresses in comments are octal.
 *
 * DISPATCH (CORRECTED 2026-07-13): MON calls dispatch through MCTAB @ 005620B
 * (segment 044-S3IDPIT, load 4000B), NOT the fictional commoncode "GOTAB". The
 * old model (GOTAB[157]=122506B -> F1676 stub -> uncarved CALLPROC, worker
 * "misattributed") was an artefact of the wrong table. MCTAB[157B] @005777B =
 * 067764B = ENTSG - the same worker, now proven directly from the table slot:
 *   MON 157B -> ENT14 072167B -> GOTAB[157B]=MFELL 072114B -> CALLP 032201B ->
 *   MCTAB[157B]=067764B=ENTSG. (The identical symbol ENTSG in resident commoncode
 *   is zero-filled - the old analysis read that zero overlay; the real code is here.)
 * ============================================================================ */

/* phys[] = physical memory (page tables bypassed). EL is the 24-bit physical
 * address formed by LDATX/LDDTX/LDXTX from T (high 8 bits) and X (low 16 bits). */
int mon_segment_to_page_table(mon_regs *r)
{
    /* --- parameter validation (param block at B: +20..+23) --- */
    int seg   = mem[r->B + 020];           /* 067771: LDA ,B 20 : SegmentNo         */
    int pt    = mem[r->B + 021];           /* 070000: LDA ,B 21 : PageTable         */
    int level = mem[r->B + 022];           /* 070017: LDA ,B 22 : InterruptLevel    */
    int start = mem[r->B + 023];           /* 070036: LDA ,B 23 : StartAddress      */

    if (!(seg < 030))                      /* 067765-067770: range-check SegmentNo   */
        return error_exit(r);
    if (!(pt == 6 || (pt > 6 && pt <= 017)))/* 070000-070011: PageTable range (VSX K) */
        return error_exit(r);
    /* 070017-070025: InterruptLevel must be one of the free levels 6,7,10B,11B */
    if (!((short)level >= 5 && (short)level <= 011))
        return error_exit(r);

    /* --- index the segment table (stride 120B) --- */
    int seg_ptr = seg * 0120 + segment_table_base;  /* 067775 MPY 120 / 067776 ADD I 120 */
    /* 070013-070016: LDATX physical read of the segment-table entry, presence test */

    /* --- fill the PIT slot for this segment --- */
    int slot = mem[0123 /*070123B ptr*/] + r->B;    /* 070026-070027                    */
    mem[slot + 0] = seg;                            /* 070031: STA ,X 0                 */
    mem[slot + 1] = (pt << 010) | level;            /* 070033-070035: PageTable<<8|level */
    mem[slot + 2] = start;                          /* 070037: STA ,X 2 : StartAddress   */

    /* --- scan the segment table for a matching (segment, page-table) pair --- */
    /* 070040-070060: LDDTX physical page-table pair reads; compare vs seg/pt;
     * if already entered, take the already-entered exit (070053-070054). */

    /* --- walk the target page table and enter the pages (physical) --- */
    /* 070061-070106: build the page-table walk base/limit (PageTable<<7 + base),
     * then loop: LDXTX/LDATX physical reads of page-table entries, mask the
     * page-frame field (AND 36B), shift and OR in the page-table select bits
     * (RORA SL DA), and re-read via LDATX until the chain terminates. */

    /* ENTSG continues past this excerpt (070133B: page-table entry commit + EXIT). */
    return unverified_tail(r);             /* UNVERIFIED: routine tail beyond carve window */
}

/* Inputs (documented SegmentToPageTable contract, consistent with the code):
 *   SegmentNo      (INTEGER2) - segment where the routine resides (read at B+20)
 *   PageTable      (INTEGER2) - page table to use, in practice 3 (VSX K: 0-17B) (B+21)
 *   InterruptLevel (INTEGER2) - one of the free levels 6,7,10B,11B (B+22)
 *   StartAddress   (INTEGER2) - entry point of the direct task (B+23)
 * The parameter offsets (+20..+23) and the range checks are byte-proven; the
 * detailed page-table commit and the error-code contract live in the routine
 * tail beyond this excerpt and are therefore INFERRED / UNVERIFIED. */
