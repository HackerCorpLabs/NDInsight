/* ============================================================================
 * MON 420B  GetUserRegisters (GRBLK)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (octal >= 0400).
 *
 * Manual behaviour (Developer/MON/calls/420B_GetUserRegisters.yaml): return the
 * ND-500 register set that was saved when a program was terminated with the
 * ESCAPE key (the user-break handler armed by SwitchUserBreak, MON 405B).
 *   39 registers are saved.
 *   Buffer (ARRAY, I/O): 154 bytes = 77 words, the registers in number order.
 *
 * Derived from the carved ND-500 System Monitor segment 030-S3SM5.bin,
 * file offset 0xBE0F (see 420B-GetUserRegisters.ASM).
 *
 * INSTRUCTION SEMANTICS reference:
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING is byte-verified - MON 420B is an ND-500
 * call dispatched by the S3SM5 0x60 vector table (slot 0x0280 = 0xBE0F), NOT by
 * the ND-100 GOTAB.  The ENTRY 0xBE0F is the vector value itself, so the entry
 * OFFSET is proven.  BUT the 448-byte window does NOT decode as a single clean
 * subroutine (several undecodable opcodes, no ENT* prologue at entry), so the
 * per-instruction alignment is NOT proven (reference Sec.9).  What IS a real,
 * observable structural fact is a REPEATING unrolled pattern with DESCENDING
 * immediates (... $0x1A,$0x18,$0x16 ... and near the tail $0x35,$0x33 ...),
 * bracketed by 'retd'/'rett' returns - consistent with an unrolled copy over
 * the 39 saved registers.  Below, only the DOCUMENTED buffer-copy contract is
 * modelled; the exact opcodes are NOT modelled as verified behaviour.
 * Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block (CALLG Buffer), NOT ND-100 A/X/T
 * registers.  Buffer is the caller's 154-byte (77-word) array.  The mapping of
 * the source save-area slots to specific frame offsets is NOT byte-proven. */
#define ND500_SAVED_REGS   39            /* manual: 39 registers saved          */
#define GRBLK_BUFFER_BYTES 154           /* manual: 154 bytes (77 words)        */

int mon_get_user_registers(void *Buffer /* out, 154 bytes */)
{
    /* DOCUMENTED behaviour (manual), NOT byte-decoded from this window:         */
    /*   copy the 39 registers saved by the ESCAPE user-break handler (armed by  */
    /*   SwitchUserBreak / 405B) into Buffer, in register-number order.          */
    for (int r = 0; r < ND500_SAVED_REGS; r++) {
        /* ((word*)Buffer)[k..] = saved_regfile[r];  (widths per register kind)  */
        /* UNVERIFIED: the carve shows an UNROLLED equivalent of this loop -      */
        /*   repeating blocks with descending immediates (0x1A,0x18,0x16 ...      */
        /*   0x35,0x33) around 'retd'/'rett' returns - but the exact per-register */
        /*   moves are not byte-decodable at proven alignment.                    */
    }

    /* 0xBE0F..0xBFCE: real SINTRAN L bytes of the GRBLK handler.  Entry offset  */
    /*   is byte-proven (= vector value); instruction alignment is UNVERIFIED.   */
    return OK;   /* status/skip convention: UNVERIFIED (ND-500 MON wrapper) */
}
