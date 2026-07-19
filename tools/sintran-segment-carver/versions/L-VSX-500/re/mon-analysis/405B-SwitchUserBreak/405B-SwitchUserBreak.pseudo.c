/* ============================================================================
 * MON 405B  SwitchUserBreak (USTRK)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (octal >= 0400).
 *
 * Manual behaviour (Developer/MON/calls/405B_SwitchUserBreak.yaml): switch the
 * user-defined ESCAPE handling on or off.  When on, pressing the ESCAPE key on
 * an ND-500 program transfers control to a user routine (and lets the register
 * set be saved for GetUserRegisters, MON 420B).  Two IN parameters:
 *   Func    (INTEGER2)  1 = on, 0 = off
 *   Address (INTEGER2)  program address to start at on ESCAPE
 *
 * Derived from the carved ND-500 System Monitor segment 030-S3SM5.bin,
 * file offset 0xBA31 (see 405B-SwitchUserBreak.ASM).
 *
 * INSTRUCTION SEMANTICS reference:
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING is byte-verified - MON 405B is an ND-500
 * call dispatched by the S3SM5 0x60 vector table (slot 0x026A = 0xBA31), NOT by
 * the ND-100 GOTAB.  The ENTRY 0xBA31 is the vector value itself (no correction
 * needed), so the entry OFFSET is proven.  BUT the byte stream at 0xBA31 does
 * NOT decode as a clean ND-500 subroutine: it opens with 'w1 comp' instead of an
 * ENT* prologue, and 0xBA4C is an undecodable opcode (0x00F2).  Per the
 * reference (Sec.9) that makes the aligned mnemonics untrustworthy even though
 * the RAW BYTES are ground truth.  Therefore NO instruction-level behaviour is
 * modelled as verified below; only the DOCUMENTED contract is expressed, and the
 * body is flagged UNVERIFIED.  Addresses in comments are file byte offsets.
 * ============================================================================ */

/* Transport is the ND-500 MON message block (CALLG Func, Address), NOT ND-100
 * A/X/T registers.  The mapping of the two IN args to specific frame slots is
 * NOT byte-proven (the window does not decode cleanly). */
int mon_switch_user_break(int Func /* 1=on,0=off */, unsigned Address /* ESCAPE entry */)
{
    /* DOCUMENTED behaviour (manual), NOT byte-decoded from this window:         */
    /*   if (Func != 0)  install user ESCAPE handler at Address (enable break);  */
    /*   else            disable user ESCAPE handling.                           */
    /* The handler record set up here is what MON 420B (GetUserRegisters) later  */
    /* reads back (the 39 saved registers on an ESCAPE termination).             */

    /* 0xBA31..0xBA6B: real SINTRAN L bytes of the USTRK handler, but the        */
    /*   instruction alignment inside this 59-byte window is UNVERIFIED (opens   */
    /*   with 'w1 comp', undecodable op at 0xBA4C).  The individual decoded ops  */
    /*   (frame-field compares/loads, an 'entsn' at 0xBA52, forward 'go'         */
    /*   branches) are consistent with a short flag-set / handler-install        */
    /*   routine, but are NOT modelled as verified behaviour. */
    /* UNVERIFIED: exact register/flag effects - not recoverable from this carve. */

    return OK;   /* status/skip convention: UNVERIFIED (ND-500 MON wrapper) */
}
