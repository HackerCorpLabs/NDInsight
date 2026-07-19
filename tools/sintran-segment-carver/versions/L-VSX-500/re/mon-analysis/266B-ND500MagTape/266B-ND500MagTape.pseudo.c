/* ============================================================================
 * MON 266B  ND500MagTape (500MT)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call.
 *
 * Manual: ND-500 magnetic-tape access (manual section 2.14, short name 500MT).
 * Only the name/short is documented; the parameter block is UNVERIFIED (the call
 * is not present in the available NPL source tree - bytes are the only source).
 *
 * Carved from the ND-500 System Monitor segment 030-S3SM5.bin, file offset
 * 0xA89D (see 266B-ND500MagTape.ASM).
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * (register model, addressing modes, branch conditions; remember C=1 means
 * NO-borrow, i.e. inverted). Where the decode is not trustworthy it is marked
 * UNVERIFIED and is NOT modelled as behaviour.
 *
 * WHAT IS BYTE-PROVEN: the ROUTING only. MON 266B is forwarded by MCHANDEL ->
 * NORMMC to the ND-500 System Monitor, which indexes its 0x60 vector table by
 * the octal MON number; slot 0x01CC holds handler file offset 0xA89D, sitting
 * inside the contiguous 260B-277B block of real handler offsets. That routing is
 * verified exactly as the 410B-421B block is.
 *
 * WHAT IS NOT PROVEN: the body. The region is SHORT - only 17 bytes to the next
 * non-zero slot (270B = 0xA8AE) - and the linear decode is only PARTLY coherent,
 * so 0xA89D is not a proven instruction boundary. A short region with a couple
 * of branch/go opcodes is consistent with a small dispatch stub that loads a
 * function code and branches into a shared magtape body, but that is INFERRED,
 * not proven. The RAW BYTES are ground truth.
 * Addresses in comments are hex file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block, NOT ND-100 A/X/T registers. R is
 * the record base, B the local/frame base (reference Sec.2). Mapping any frame
 * slot to a named MON argument is UNVERIFIED. */
int mon_nd500_magtape(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0xA89D f2 comp / 0xA89F noop / 0xA8A0 d4 - : a compare then a subtract,
     * consistent with checking a small function selector. UNVERIFIED alignment. */

    /* 0xA8A5 if << go / 0xA8A8 go: two control transfers in a 17-byte region -
     * the "dispatch stub" shape. Their targets are not resolvable inside this
     * slice (they point past it, into the shared handler bodies), so they are
     * UNVERIFIED and NOT modelled as concrete jumps. */
    dispatch_to_shared_magtape_body();   /* INFERRED from the branch shape */

    /* NOT MODELLED: the tape-function selector values, the argument layout, and
     * the error/skip contract. The manual gives only the call name. Treat the
     * body as PROVISIONAL; only the routing is relied upon. */
    return OK;                  /* status word: UNVERIFIED */
}
