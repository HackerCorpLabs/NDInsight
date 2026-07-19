/* ============================================================================
 * MON 265B  ND500WriteFile (500WF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call.
 *
 * Manual: ND-500 write-file (manual section 2.14, short name 500WF). Only the
 * name/short is documented; the parameter block is UNVERIFIED (the call is not
 * present in the available NPL source tree - bytes are the only source here).
 * This is the sibling of the 264B (read) handler.
 *
 * Carved from the ND-500 System Monitor segment 030-S3SM5.bin, file offset
 * 0xA825 (see 265B-ND500WriteFile.ASM).
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * (register model, addressing modes, branch conditions; remember C=1 means
 * NO-borrow, i.e. inverted). Where the decode is not trustworthy it is marked
 * UNVERIFIED and is NOT modelled as behaviour.
 *
 * WHAT IS BYTE-PROVEN: the ROUTING only. MON 265B is forwarded by MCHANDEL ->
 * NORMMC to the ND-500 System Monitor, which indexes its 0x60 vector table by
 * the octal MON number; slot 0x01CA holds handler file offset 0xA825, sitting
 * inside the contiguous 260B-277B block of real handler offsets. That routing is
 * verified exactly as the 410B-421B block is.
 *
 * WHAT IS NOT PROVEN: the body. The linear decode from the raw vector offset is
 * only PARTLY coherent, so 0xA825 is not a proven instruction boundary (same
 * misalignment symptom documented for 410B; see reference Sec.9). The RAW BYTES
 * are ground truth, but the op sequence is not a reliable guide to the real
 * control flow. No argument/return contract is modelled as verified.
 * Addresses in comments are hex file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block, NOT ND-100 A/X/T registers. R is
 * the record base, B the local/frame base (reference Sec.2). Mapping any frame
 * slot to a named MON argument (file number, byte count, buffer) is UNVERIFIED. */
int mon_nd500_write_file(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xA825..0xA836: literal loads and clears (f4 =:, bi3 clr, d stz, h2 :=)
     * interleaved with unknown opcodes - UNVERIFIED prologue (misalignment). */

    /* 0xA838 ents / 0xA848 ents / 0xA864 entsn: enter-subroutine / frame
     * prologues appear INSIDE the body (reference Sec.8.2). Two 'ents' plus one
     * 'entsn' are consistent with a validate-then-call-shared-worker routine,
     * but being mid-stream the exact boundaries are UNVERIFIED. */
    frame_enter();              /* UNVERIFIED which ents/entsn is the true entry */

    /* 0xA82F ret / 0xA847 rett: at least two return opcodes exist. The status
     * word returned is UNVERIFIED. */
    return OK;                  /* status word: UNVERIFIED */

    /* NOT MODELLED: the file-number / byte-count / buffer argument layout, the
     * error/skip contract, and any transfer to the ND-100 file system. These
     * live in workers this carve does not contain, and the manual gives only the
     * call name. Treat the whole body as PROVISIONAL; only the routing is
     * relied upon. */
}
