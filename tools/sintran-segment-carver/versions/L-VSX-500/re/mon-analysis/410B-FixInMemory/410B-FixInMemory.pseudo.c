/* ============================================================================
 * MON 410B  FixInMemory (MOFIX / fixseg)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (octal >= 0400).
 *
 * Manual behaviour: lock (FIX) an ND-500 segment resident in physical memory
 * so it cannot be paged/swapped out.  Native back-end name = MOFIX.
 *
 * Derived from the real disassembly (see 410B-FixInMemory.ASM), the carved
 * ND-500 System Monitor segment 030-S3SM5.bin, file offset 0xbae6.
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * Every line below that models an opcode gives the REAL ND-500 operation from
 * that reference (register model, addressing modes, branch conditions;
 * remember C=1 means NO-borrow, i.e. inverted).  Where the decode is not
 * trustworthy it is marked UNVERIFIED - it is NOT modelled as behaviour.
 *
 * HONESTY / MISALIGNMENT: the ROUTING (ND-500 call via S3SM5, not the ND-100
 * GOTAB) is byte-verified.  The ENTRY 0xbae6 is a +5 HEURISTIC correction of an
 * anomalous vector value (0xbae1, which points into an error string), so the
 * carve START is not a proven instruction boundary.  Per the reference (Sec.9)
 * a wrong start byte makes the decoded mnemonics untrustworthy even though the
 * RAW BYTES are ground truth: note that a 'by stz' precedes the 'entsn' (an
 * entry prologue should START with an ENT*), the 'if << go' branch has no
 * preceding compare to set its carry flag, and the tail past 0xbb26
 * ('entf $0xFFFF...', trailing 'cind') decodes incoherently.  Treat the whole
 * body as PROVISIONAL: only the routing and the presence of an 'entsn'/'retd'
 * pair are relied upon.  The argument block and status/error contract are
 * UNVERIFIED.  Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block, NOT ND-100 A/X/T registers.  R is
 * the record base, B the local/frame base (see reference Sec.2).  The register
 * names below (A1..A4 float, I1..I4 integer) are the reference's; the mapping of
 * any frame slot to a named MON argument (seg number, etc.) is NOT byte-proven. */
int mon_fix_in_memory(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xbae6: by stz $0x13   -> byte store-zero: dst($0x13) = 0.  (Sec.5.3)     */
    /*   UNVERIFIED: sits BEFORE the entsn; +5 heuristic entry -> possible       */
    /*   misalignment.                                                           */

    /* 0xbae8: entsn $0x13,$0x2C -> build stack frame, enter subroutine, bounded */
    /*   argc (Sec.8.2).  This is the first ENT* in the stream.                  */
    frame_enter();

    /* 0xbaeb-0xbaf4: literal loads/clears per the reference (Sec.5.1/5.3/5.4).  */
    A1 = mem32[R + 0xE8];      /* 0xbaeb: f1 := r.0xE8  (float load, Sec.5.1)    */
    A3 = mem32[B + 0x20];      /* 0xbaed: f3 := b.0x20                            */
    A3 = mem32[R + 0xE8];      /* 0xbaef: f3 := r.0xE8                            */
    I4 = mem32[B + 0x64];      /* 0xbaf1: w4 := b.0x64  (word load)              */
    I1 = 0;                    /* 0xbaf3: bi1 clr       (clear register, Sec.5.4)*/
    /* 0xbaf4: d stz $0x6 -> doubleword store-zero: dst($0x6) = 0.  (Sec.5.3)    */

    /* 0xbaf6: if << go $0x1BA -> if (!C) goto pc+0x1BA  (unsigned <, Sec.7.2).  */
    /*   UNVERIFIED: no compare/test precedes this, so the carry flag it tests   */
    /*   is undefined in an aligned reading -> misalignment signal.  Not modelled*/
    /*   as a meaningful guard.                                                  */

    /* 0xbaf9-0xbb23: a run of word/float loads, a 'lind' index bounds-check     */
    /*   (0xbb03, Sec.6.5: I2 = idx then K/IX per bounds), a 'comp' (0xbb07,     */
    /*   Sec.6.3), and 'w3 - ...'/'w2 - ...' subtracts (Sec.6.1).  These decode  */
    /*   cleanly as individual ops, but with no proven entry alignment their     */
    /*   sequence is not a reliable guide to the real control flow.  UNVERIFIED. */

    /* 0xbb25: retd -> PC = L  (return direct, no frame unwind; Sec.8.3).        */
    return OK;                 /* status word: UNVERIFIED                        */

    /* 0xbb26-0xbb38: entf $0xFFFFFFFFFF598454, then a 'cind' with out-of-range  */
    /*   operands -> decodes INCOHERENTLY.  UNVERIFIED: possible misalignment;   */
    /*   the slice may include trailing bytes of the previous routine or the     */
    /*   leading bytes of the 411B entry (0xbb38).  NOT modelled.                */
}
