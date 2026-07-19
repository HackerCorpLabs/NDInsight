/* ============================================================================
 * MON 416B  SaveND500Segment (WSEGN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (NOT an ND-100 GOTAB call).
 *
 * Behaviour (manual, name only): write all modified (dirty) pages of an ND-500
 * segment back to disk ("save segment").  The step-by-step effect is NOT
 * recoverable from this carve (see MISALIGNMENT below).
 *
 * Derived from the real disassembly (see 416B-SaveND500Segment.ASM), the carved
 * ND-500 System Monitor segment 030-S3SM5.bin, file offset 0xbd70..0xbdf6
 * (134 bytes, one contiguous slice).
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * Lines that model an opcode give the REAL ND-500 operation from that reference
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING (ND-500 call via the S3SM5 0x60 vector
 * table, NOT the ND-100 GOTAB) is byte-verified: slot 0x027c holds 0xbd70, and
 * the entry lands on code (not a text word).  BUT this 134-byte window is a
 * MID-BLOCK entry into a packed fix-family body and does NOT decode as an
 * aligned stream: several '??? opcode' lines (0x00F1 at 0xbd7a/0xbd8e/0xbda2,
 * 0x00F2 at 0xbdce, 0x0001 at 0xbdcf) are operand/prefix bytes, NOT instructions
 * (Sec.9); a 'bp' breakpoint (0xbded) and nonsensical 8-byte immediates and
 * branch targets (e.g. 'go $0xFFFFFFFFCC6949C1' at 0xbdc5) appear mid-body.  The
 * RAW BYTES are ground truth but the mnemonics are UNRELIABLE (Sec.9).  Only the
 * routing and entry are relied upon; the dirty-page write-back action, argument-
 * slot mapping, and status/error contract are UNVERIFIED.  There are NO invented
 * domain calls below (an earlier revision guessed segment_is_fixed/
 * page_is_modified/write_page_to_disk and a page loop; those are removed - they
 * are not provable from these bytes).
 * Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block (CALLG argument list), NOT ND-100
 * A/X/T registers.  Manual parameter list (mapping to frame slots UNVERIFIED):
 *   LogSegmentNo (in) - logical segment number (0 => from the parameter address)
 *   FirstPage    (in) - first logical page in the segment
 *   LastPage     (in) - last logical page in the segment
 * Returns a standard SINTRAN error code (0 = OK).  Constraint (manual): not
 * allowed while the segment is fixed in memory. */
int mon_save_nd500_segment(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xbd70: d move b.0x10,r.0xCC -> mem64[R+0xCC] = mem64[B+0x10] (Sec.5.5).   */
    /*   Entry op; lands on code, so the entry byte is a real handler start, but  */
    /*   the operands are not tied to a named parameter (mapping UNVERIFIED).     */
    /* 0xbd73: by1 := DESC(r3) $0xF -> I1 = descriptor-fetched byte, zero-extended */
    /*   (Sec.5.1 with the DESC(r3) operand prefix, Sec.3.4).                     */
    /* 0xbd76: if >< go $0xEA8 -> if (!Z) goto pc+0xEA8 (not-equal, Sec.7.2).     */
    /*   UNVERIFIED: mid-block displacement is meaningless (Sec.7/Sec.9).         */
    /* 0xbd79: noop (Sec.5.8).                                                    */

    /* 0xbd7a-0xbdf5: UNVERIFIED (possible misalignment).  This region does not   */
    /*   decode as an aligned instruction stream: '??? opcode 0x00F1' (0xbd7a,    */
    /*   0xbd8e, 0xbda2), '??? opcode 0x00F2/0x0001' (0xbdce/0xbdcf) are prefix/  */
    /*   address-code bytes, not instructions (Sec.9); 'w3 mulad $0x2D,$<double>' */
    /*   (0xbd7c) and 'w4 =: $<double>' (0xbd91) carry 8-byte immediates that are */
    /*   implausible as data words; 'go $0xFFFFFFFFCC6949C1' (0xbdc5) is a        */
    /*   nonsense target; 'entsn $0x4,r.0x0' (0xbdd0) is an entry op in the       */
    /*   middle of the body (Sec.8.2 - a handler has ONE ENT*); and 'bp' (0xbded, */
    /*   Sec.5.9) is a breakpoint trap.  The recurring 'by1 := DESC(r3) ... ;     */
    /*   if >< go ... ; noop' groups are the shape of a shared body entered at    */
    /*   several points, but not a decode we can trust here.  The per-page        */
    /*   write-back loop and error path live in the shared tail this slice enters */
    /*   into; they are NOT decoded in this window.  Not modelled.                */

    return OK;                 /* status word / error contract: UNVERIFIED        */
}
