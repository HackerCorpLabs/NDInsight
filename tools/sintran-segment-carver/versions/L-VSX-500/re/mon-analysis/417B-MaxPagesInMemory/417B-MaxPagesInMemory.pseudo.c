/* ============================================================================
 * MON 417B  MaxPagesInMemory (MXPISG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (handled by S3SM5).
 *
 * Behaviour (official manual, name only): set the maximum number of pages an
 * ND-500 logical segment may hold in physical memory at a time.  The step-by-
 * step effect is NOT recoverable from this carve (see MISALIGNMENT below).
 *
 * Derived from the real disassembly (see 417B-MaxPagesInMemory.ASM), the carved
 * ND-500 System Monitor segment 030-S3SM5.bin, file offset 0xbdf6..0xbe0f
 * (25 bytes, one contiguous slice).
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * Lines that model an opcode give the REAL ND-500 operation from that reference
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING (ND-500 call via the S3SM5 0x60 vector
 * table, NOT the ND-100 GOTAB) and the dispatch geometry are byte-verified: slot
 * 0x027e holds 0xbdf6, the window is [0xbdf6..0xbe0f) = 25 bytes, closed by the
 * next vector entry (420B = 0xbe0f).  BUT 0xbdf6 is a MID-BLOCK entry into the
 * packed 400B..421B fix-family body - the reference cites this exact address as
 * its worked example of misalignment (Sec.9).  The VERY FIRST bytes at the entry
 * decode as '??? opcode 0x00F2' / '??? opcode 0x0001' (0xbdf6/0xbdf7), i.e.
 * operand/prefix bytes, NOT instructions (Sec.9); the window also holds an
 * 'entsn' entry op (0xbdf8) and a last op (0xbe09) that reads PAST 0xbe0f into
 * the 420B body.  The RAW BYTES are ground truth but the mnemonics are
 * UNRELIABLE (Sec.9).  Only the routing/geometry is relied upon; the page-
 * ceiling store, argument-slot mapping, and status/error contract are
 * UNVERIFIED.  There are NO invented domain calls below (an earlier revision
 * guessed segment_in_use/set_max_resident_pages; those are removed - they are
 * not provable from these bytes).
 * Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block, NOT ND-100 A/T/X registers.
 * Public contract (official manual, ND-860228.2 EN); mapping onto message-block
 * bytes is UNVERIFIED (not provable from the 25-byte mid-block slice):
 *   SegmentNo (in) - logical segment number (0 => derive from param address)
 *   SegType   (in) - 0 = data segment, 1 = program segment
 *   NoOfPages (in) - new ceiling on resident pages for that segment
 * Returns ErrCode (0 = OK).  Precondition (manual): the segment must be in use. */
int mon_max_pages_in_memory(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xbdf6/0xbdf7 (F2 01): '??? opcode 0x00F2' / '??? opcode 0x0001' - the      */
    /*   vector entry lands mid-instruction; these are operand/prefix bytes, NOT   */
    /*   instructions (Sec.9).  UNVERIFIED (misalignment): the true instruction    */
    /*   boundary is fixed by the caller, not by this entry point.                 */

    /* 0xbdf8: entsn $0x4,r.0x0 -> build stack frame, enter subroutine (Sec.8.2).  */
    /*   Reached mid-block, so even this is UNVERIFIED (possible misalignment).    */

    /* 0xbdfc-0xbe05: decode individually per the reference, but their sequence    */
    /*   is NOT a reliable guide with the entry misaligned:                        */
    /*   0xbdfc: w3 mulad $0x3,b.0xAC -> I3 = I3*3 + mem32[B+0xAC] (Sec.6.2).      */
    /*   0xbdff: w2 lind b.0xB0,r.0x98,b.0xA8 -> I2 = mem32[B+0xB0]; bounds-check  */
    /*           against [mem32[R+0x98], mem32[B+0xA8]] -> K/IX (Sec.6.5).         */
    /*   0xbe03: if <<= go $0x2C -> if (!C || Z) goto pc+0x2C (unsigned <=,        */
    /*           Sec.7.2); displacement meaningless mid-block (Sec.7/Sec.9).       */
    /*   0xbe05: d comp2 r.0xCC,$0x4 -> set flags from mem64[R+0xCC] - 4, store    */
    /*           nothing (Sec.6.3).                                                */
    /*   UNVERIFIED (possible misalignment) - not modelled as the call's logic.    */

    /* 0xbe08: '??? opcode 0x00F2' (Sec.9), then 0xbe09: h1 := @b.0xEA803F1        */
    /*   (Sec.5.1) whose bytes SPILL PAST 0xbe0f into the MON 420B body - further  */
    /*   proof the handler is not self-contained in 25 bytes.  The actual store of */
    /*   the new page ceiling and the return live in the uncarved shared tail      */
    /*   below 0xbe0f.  Not modelled.                                              */

    return OK;                 /* ErrCode / status contract: UNVERIFIED           */
}
