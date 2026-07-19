/* ============================================================================
 * MON 264B  ND500ReadFile (500RF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call.
 *
 * Manual: ND-500 read-file (manual section 2.14, short name 500RF). Only the
 * name/short is documented; the parameter block is UNVERIFIED (the call is not
 * present in the available NPL source tree - bytes are the only source here).
 *
 * Carved from the ND-500 System Monitor segment 030-S3SM5.bin, file offset
 * 0xA7AD (see 264B-ND500ReadFile.ASM).
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * (register model, addressing modes, branch conditions; remember C=1 means
 * NO-borrow, i.e. inverted). Where the decode is not trustworthy it is marked
 * UNVERIFIED and is NOT modelled as behaviour.
 *
 * WHAT IS BYTE-PROVEN: the ROUTING only. MON 264B is forwarded by MCHANDEL ->
 * NORMMC to the ND-500 System Monitor, which indexes its 0x60 vector table by
 * the octal MON number; slot 0x01C8 holds handler file offset 0xA7AD, sitting
 * inside the contiguous 260B-277B block of real handler offsets. That routing is
 * verified exactly as the 410B-421B block is.
 *
 * WHAT IS NOT PROVEN: the body. The linear decode from the raw vector offset is
 * only PARTLY coherent - several 'go' targets are out of range and several bytes
 * decode as unknown opcodes - so 0xA7AD is not a proven instruction boundary
 * (same misalignment symptom documented for 410B; see reference Sec.9). The RAW
 * BYTES are ground truth, but the sequence of ops is not a reliable guide to the
 * real control flow. No argument/return contract is modelled as verified.
 * Addresses in comments are hex file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block, NOT ND-100 A/X/T registers. R is
 * the record base, B the local/frame base (reference Sec.2). Mapping any frame
 * slot to a named MON argument (file number, byte count, buffer) is UNVERIFIED. */
int mon_nd500_read_file(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xA7AD..0xA7C7: a run of subtract/compare/conditional-go ops (w3 -,
     * f2 comp, if >= go). Several branch targets are out of range, so this
     * prologue is UNVERIFIED (possible misalignment). Not modelled as guards. */

    /* 0xA7FC / 0xA804 / 0xA822: three 'entsn' frame prologues appear INSIDE the
     * body (reference Sec.8.2 - build stack frame, enter subroutine, bounded
     * argc). Their presence is consistent with the region being real handler
     * code (a routine that compares/validates then calls a shared file worker),
     * but because they are mid-stream rather than at the entry, the exact
     * boundaries are UNVERIFIED. */
    frame_enter();              /* UNVERIFIED which entsn is the true entry */

    /* 0xA7F9 retk / 0xA82F-region ret opcodes: at least one return path exists.
     * The status word returned is UNVERIFIED (no field attributed with
     * confidence). */
    return OK;                  /* status word: UNVERIFIED */

    /* NOT MODELLED: the file-number / byte-count / buffer argument layout, the
     * error/skip contract, and any transfer to the ND-100 file system. These
     * live in workers this carve does not contain, and the manual gives only the
     * call name. Treat the whole body as PROVISIONAL; only the routing is
     * relied upon. */
}
