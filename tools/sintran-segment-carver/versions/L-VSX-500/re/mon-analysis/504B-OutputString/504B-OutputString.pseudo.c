/* ==========================================================================
 * MON 504B - OutputString (DVOUTS / NPL NOUTSTR)  pseudo-C model for an
 * emulator author.
 *
 * Source: real SINTRAN L bytes, 026-S3IMPIT.bin (S3MPIT), worker entry
 * OSTRS = 141205B.  ND-500 level-12 call: it arrives via the resident
 * level-12 GOSW (5CMNO-L12MIN, slot 4), NOT the ND-100 GOTAB. The GOSW is in
 * an uncarved overlay, so the MON 504 -> OSTRS runtime link is UNVERIFIED.
 * The worker code below is ND-100 code and is byte-verified.
 *
 * PARAMETERS DO NOT arrive in A/T/X user registers. An ND-500 monitor call
 * passes its arguments in the ND-500 MESSAGE BUFFER (indexed off 5MBBANK +
 * field displacement); the ND-100-side A/T/X here are level-12 driver-internal.
 * Documented DVOUTS(504B) arguments (from the reference manual, provenance =
 * manual, NOT these bytes):
 *   (1) LDN or open file number      [INT]
 *   (2) number of bytes to write     [INT]   (shared DVIO entry errors EC174
 *                                             if > 4000B; see note)
 *   (3) string / array to be written [ARR]
 *
 * Control flow below is byte-verified against 504B-OutputString.ASM.
 * SEMANTIC labels (which field / which worker) are INFERRED - marked inline.
 * All addresses in comments are octal.
 * ========================================================================== */

/* mem[] / phys[] and the T/X PHYSICAL transfers.
 * mem[] is the ND-100 word address space of the resident level-12 driver.
 * The handler reaches the ND-500 message buffer with the T/X physical transfer
 * instructions (LDATX / LDDTX here). These form a 24-bit PHYSICAL address that
 * BYPASSES the MMU - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * T is the BANK (high byte), X the word offset; disp3 (the 3-bit field, bits 3-5)
 * is 0 in every transfer here - the code adjusts X with AAX instead. When T is
 * loaded with the ND-500 message-buffer bank 5MBBANK these are reads of the ND-500
 * message buffer at physical bank:offset. phys[] is that 24-bit physical space.
 * The '(( ptr ))' notation means an INDIRECT jump/call through a pool word. */
#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */

/* Indirect pool words the OSTRS body reaches (141232B..141236B). These are
 * DATA (the disassembler mis-renders them as instructions). Their exact worker
 * targets are NOT resolved from this segment's symbol table - treat as
 * UNVERIFIED pointers. */
/* @141232B = 023624   @141233B = 000215   @141234B = 004654                */
/* @141235B = 000511   @141236B = 011260                                    */

void mon_504B_OSTRS(void)   /* NPL NOUTSTR; L worker OSTRS = 141205B */
{
    /* 141205B..141210B: entry. RADD CLD SL DA copies the return-link register L
     * into A (A = L), STA -2 saves it, then two indirect dispatches through the
     * pool (JPL I 23). The saved link is INFERRED to feed the output datafield /
     * worker selection. */

    /* 141211B..141225B: read the ND-500 message buffer with T/X PHYSICAL
     * transfers. LDT I 22 loads the bank into T, AAX 37 sets the word offset in X,
     * then 141214B LDATX: A = phys[ELADDR(T, X)] - a message-buffer word, MMU
     * bypassed. 141216B LDT 17 reloads T and 141217B SKP IF DA EQL ST compares the
     * fetched word against it (INFERRED: is this a terminal? / state):
     *   141220B  JMP 7  -> 141227B       equal-branch (the only in-window JMP).
     * 141221B..141224B do a second physical read (LDT I 13, AAX 134, then
     *   141223B LDDTX: A = phys[ELADDR(T, X)]; D = phys[ELADDR(T, X) + 1]),
     * STX I 11 stores the index, then dispatch again indirectly (JPL I 11 x3 at
     * 141226B..141230B); 141231B JMP I -25 is the indirect exit of the
     * not-selected path. */

    /* 141237B..141255B: guarded device-output sequence.
     *   141242B  IOF                     disable interrupts (critical section)
     *   IRW 120 DB / IRW 120 DP          write device control/data registers
     *                                    (INFERRED: kick the output driver / DMA)
     *   141250B  MST PID                 set PID / status bits
     *   141251B  ION                     re-enable interrupts
     *   141252B  JMP I 3  -> 141255B     indirect continue. */

    /* 141256B (PT5RS, SYMBOL-2-LIST): continuation / restart point.
     * 141256B..141265B: a second device-register block (IRW 140 DX/DL/DP),
     * loads (LDA 7/6/5), MST PID. INFERRED: program a second device channel or
     * the return-status path. */

    /* 141266B  EXIT : the OSTRS worker returns here. Control then flows back to
     * the level-12 message loop (NXTMSG family) - the exact successor is reached
     * indirectly and is outside this slice. */

    /* 141267B..141270B are NOT part of OSTRS: 141267B (JPL I -> 141364B) is the
     * next region's entry, and 141270B is a data word equal to 141205B (an
     * indirect pointer BACK to OSTRS, e.g. how DVIO's pool reaches it). They are
     * present in this slice only to keep it one contiguous 52-word block. */
}

/* Notes for the emulator:
 *  - No ND-100-style skip-return / A-register error code is set inside this
 *    window; exits are indirect jumps back into the level-12 dispatch loop.
 *  - The output byte-count range check (count > 4000B -> error EC174) lives in
 *    the SHARED DVIO entry region around 141041B (NOUTS = DVIO = 141027B), NOT
 *    in this OSTRS worker. For 504's own OSTRS bytes the range check is
 *    therefore INFERRED / shared-with-DVIO, not observed in this slice.
 *  - The message-buffer field layout (which displacement holds the LDN, the
 *    count, the array pointer) is INFERRED from the code, not independently
 *    proven from these bytes.
 */
