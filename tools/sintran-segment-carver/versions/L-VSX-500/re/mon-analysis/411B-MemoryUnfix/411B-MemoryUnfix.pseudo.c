/* ============================================================================
 * MON 411B  MemoryUnfix (MUNFIX / unfix)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (octal >= 0400).
 *
 * Behaviour (name only): release (UNFIX) an ND-500 segment previously locked
 * in physical memory by MON 410B FIX, so it may again be paged/swapped out.
 * Native back-end name = MUNFIX (inverse of MOFIX).  The step-by-step effect
 * is NOT recoverable from this carve (see MISALIGNMENT below).
 *
 * Derived from the real disassembly (see 411B-MemoryUnfix.ASM), the carved
 * ND-500 System Monitor segment 030-S3SM5.bin, file offset 0xbb38..0xbb73
 * (59 bytes, one contiguous slice).
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * Lines that model an opcode give the REAL ND-500 operation from that reference
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING (ND-500 call via the S3SM5 0x60 vector
 * table, NOT the ND-100 GOTAB) and the exact ENTRY (0xbb38, byte-exact vector
 * value) are byte-verified.  BUT 0xbb38 is a MID-BLOCK entry into a shared
 * fix-family body and the decode is NOT trustworthy as an aligned stream: it
 * holds TWO 'entsn' entry ops (0xbb3e, 0xbb50), a stray '??? opcode 0x00FB' at
 * 0xbb6d (Sec.9: an operand/prefix byte, NOT an instruction), and implausibly
 * large operand fields (e.g. 'd test b.0x62AE46CB' at 0xbb5e).  Per the
 * reference an ENT* is the single first instruction reached by a CALL (Sec.8.2),
 * so two ENT* cannot belong to one correctly-aligned handler.  The RAW BYTES
 * are ground truth but the mnemonics are UNRELIABLE (Sec.9).  Only the routing
 * and entry byte are relied upon; the unfix action, argument-slot mapping, and
 * status/error contract are UNVERIFIED.  There are NO invented domain calls
 * below (an earlier revision guessed load_seg_descriptor/is_fixed/clear_fixed;
 * those are removed - they are not provable from these bytes).
 * Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block (CALLG argument list), NOT ND-100
 * A/X/T registers.  The argument slots (which frame word carries the segment
 * number) are UNVERIFIED - not extractable from this mid-block window. */
int mon_memory_unfix(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xbb38: w3 cind $0xF,r.0xE8,$0x11 -> bounds-check index 0xF against       */
    /*   [mem32[R+0xE8], 0x11], set K/IX, no load (Sec.6.5).  This is the entry  */
    /*   op; its operands are plausible but the window is mid-block, so treat as */
    /*   a boundary hint only.                                                   */
    /* 0xbb3c: by stz $0x11 -> store byte 0 (Sec.5.3).                           */

    /* 0xbb3e: entsn $0x11,b.0x20 -> build stack frame, enter subroutine         */
    /*   (Sec.8.2).                                                              */
    frame_enter();

    /* 0xbb41-0xbb4b: register loads per the reference (Sec.5.1), e.g.           */
    /*   'f2 := r.0xE8' -> A2 = mem32[R+0xE8]; 'w4 := b.0x30' -> I4 = mem32[B+0x30]. */
    /*   With the entry mid-block these loads are individually decodable but      */
    /*   their sequence is not a reliable guide to the real argument staging.     */
    /*   UNVERIFIED (possible misalignment).                                      */

    /* 0xbb4d: if > go $0x2C46 -> if (!S && !Z) goto pc+0x2C46 (signed >,        */
    /*   Sec.7.2).  UNVERIFIED: displacement is meaningless from a mid-block      */
    /*   entry (Sec.7/Sec.9), so this is not modelled as a real guard.           */

    /* 0xbb50: entsn $0xC,b.0x24 -> a SECOND 'entsn' inside the body.  Per        */
    /*   Sec.8.2 a handler has ONE ENT*, so this confirms misalignment; the       */
    /*   intervening loads (0xbb53-0xbb59) are NOT modelled.  UNVERIFIED.         */

    /* 0xbb5b: if -k go $0x6485 -> if (K==0) goto pc+0x6485 (Sec.7.2).           */
    /*   UNVERIFIED (possible misalignment): displacement meaningless mid-block.  */

    /* 0xbb5e-0xbb72: UNVERIFIED (possible misalignment).  'd test b.0x62AE46CB'  */
    /*   (0xbb5e) has an implausible operand, '??? opcode 0x00FB' (0xbb6d) is not */
    /*   an instruction (Sec.9), and 'd move $0xA,$0x21' (0xbb70) is the last     */
    /*   decoded op before the 413B vector target (0xbb73).  The shared worker    */
    /*   body that performs the actual UNFIX effect is reached by falling through */
    /*   past 0xbb73, OUTSIDE this 59-byte window.  Not modelled.                 */

    return OK;                 /* status word / error contract: UNVERIFIED        */
}
