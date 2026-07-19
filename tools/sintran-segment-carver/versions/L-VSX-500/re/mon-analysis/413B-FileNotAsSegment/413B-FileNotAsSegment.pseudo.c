/* ============================================================================
 * MON 413B  FileNotAsSegment (FSCDNT)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (octal >= 0400).
 *
 * Manual behaviour (ND-860228.2 EN p.195): disconnect a file that was connected
 * as a segment by FileAsSegment (MON 412B).  The file is NOT closed.  A file is
 * also disconnected automatically by CloseFile.
 *
 * Derived from the real disassembly (see 413B-FileNotAsSegment.ASM), the carved
 * ND-500 System Monitor segment 030-S3SM5.bin, file offset 0xbb73.
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * Lines that model an opcode give the REAL ND-500 operation from that reference
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING (ND-500 call via the S3SM5 0x60 vector
 * table, not the ND-100 GOTAB) is byte-verified: slot 0x0276 holds 0xbb73.  The
 * decode of the body is NOT trustworthy as an aligned stream: the very first op
 * at the entry is a CONDITIONAL BRANCH ('if <<= go') with no preceding compare
 * to set its flags, and the region contains THREE 'entsn' entry ops (0xbb7c,
 * 0xbb8a, 0xbb9c) plus a mid-body 'init' (0xbb83).  Per the reference an ENT*
 * is the single first instruction reached by a CALL (Sec.8.2) and 'init' runs
 * ONCE at program start (Sec.8.1) - so multiple ENT* and a mid-body init cannot
 * belong to one correctly-aligned handler.  The RAW BYTES are ground truth but
 * the mnemonics are UNRELIABLE (Sec.9).  Only the routing is relied upon; the
 * disconnect action, argument-slot mapping, and status/error contract are
 * UNVERIFIED.  Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block (CALLG argument list), NOT ND-100
 * A/X/T registers.  Manual parameter list (mapping to frame slots UNVERIFIED):
 *   FileNumber        (in) - file number (see OpenFile)
 *   LogSegmentNumber  (in) - segment number (OPTIONAL parameter) */
int mon_file_not_as_segment(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xbb73: if <<= go $0x29 -> if (!C || Z) goto pc+0x29  (unsigned <=,        */
    /*   Sec.7.2).  UNVERIFIED: this is the FIRST op at the entry with no compare */
    /*   before it, so the carry/zero flags it tests are undefined -> the entry   */
    /*   is not a clean instruction boundary (possible misalignment).             */
    /* 0xbb75: f1 neg -> A1 = -A1  (float negate, Sec.5.6).                       */
    /* 0xbb76: if > go $0xD  -> if (!S && !Z) goto  (signed >, Sec.7.2).          */
    /* 0xbb78: w2 - r.0x3C   -> I2 = I2 - mem32[R+0x3C]  (subtract, Sec.6.1).     */
    /* 0xbb7a: if > go $0xD  -> if (!S && !Z) goto  (signed >, Sec.7.2).          */
    /*   These decode as individual ops but, with the entry unaligned, their      */
    /*   order is not a reliable guide to the real validation logic.  UNVERIFIED. */

    /* 0xbb7c: entsn $0x19,... -> build stack frame, enter subroutine (Sec.8.2).  */
    frame_enter();

    /* 0xbb81-0xbb9c: UNVERIFIED (possible misalignment).  A second and third     */
    /*   'entsn' (0xbb8a, 0xbb9c) and an 'init' (0xbb83) appear inside the body;  */
    /*   per Sec.8.1/8.2 that cannot be a single correctly-aligned handler, so    */
    /*   the intervening loads/moves ('d2 := r.0xE8', 'd move $0x10,r.0xE8', ...)  */
    /*   are NOT modelled as the disconnect action.  The trailing entsn at 0xbb9c */
    /*   is a 3-byte op whose last byte lands on the 0xbb9e region boundary.      */

    return OK;                 /* status word / error contract: UNVERIFIED        */
}
