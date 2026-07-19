/* ============================================================================
 * MON 32B  OutMessage (MSG)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Writes a message string to the calling program's terminal (max 512 chars).
 * Convenient for error messages from background programs.
 *
 * Derived from the real disassembly (see 32B-OutMessage.ASM), the MSG worker at
 * 102453B in segment 025-S3IRPIT. Control flow (the character loop, the byte
 * fetch, the two byte comparisons, the two calls into the preceding emit
 * routine, and the indirect resident tail) is BYTE-VERIFIED. The register/field
 * meanings (which register holds the string address) are INFERRED from the
 * SINTRAN III Monitor Calls manual MAC example and the code shape - treat as a
 * model, not gospel. Addresses in comments are octal.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction
 * semantics reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   RADD CLD Ss Dd = register copy (A = D idiom);  MST PIE = masked-set PIE from A;
 *   LBYT = load byte (A = byte at T + X/2, X odd = low byte);  SAT n = T := signext8(n);
 *   SKP IF DA EQL ST = if (A==T) skip;  SKP IF DA UEQ ST = if (A!=T) skip;
 *   SHA ZIN SHR 1 = logical right shift A by 1;  JPL I = indirect call via link cell.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 32 OutMessage - MAC:  LDX (TEXT / MON 32
 *   X = address of the string to write to the user's terminal.
 *   String max length 512 characters. */

int mon_32B_OutMessage(mon_regs *r)
{
    resident_prologue_worker();            /* 102453: JPL I 43 -> link @102516      */
    /* 102454-102461: set up terminal-output descriptor from B-frame fields         */
    set_interrupt_enable(r->A);            /* 102462: MST PIE - PIE |= (A & mask)    */

    r->X = 0;                              /* 102463: RADD CLD 0 DX  (X = 0, index)  */
    r->T = char_count();                   /* 102464: LDT 34 - remaining char count  */

    /* 102465-102504: scan/emit the message one character at a time */
    while ((short)r->T > (short)r->X) {    /* 102465: SKP IF DT GRE SX / 102466 JMP  */
        r->D = r->X;                       /* 102467: RADD CLD SX DT (byte position) */
        int ch = load_byte(r->T, r->X);    /* 102470: LBYT - fetch one character     */

        if (ch == 047) {                   /* 102471 SAT 47 / 102472 skip-if-!=      */
            if (ch == 044)                 /* 102474 SAT 44 / 102475 skip-if-==      */
                emit_char_variant(047);    /* 102477 SAA 15 / 102500 JPL -31->102447 */
            else
                emit_char_variant(044);    /* 102501 SAA 12 / 102502 JPL -33->102447 */
        }
        r->X += 1;                         /* 102503: AAX 1 - next character          */
    }                                      /* 102504: JMP -20 -> 102464 (loop)        */

    /* 102505-102513: finish - fold the emitted count back into the B-frame */
    resident_worker_at_link_102521();      /* 102505: JPL I 14 -> link @102521        */
    r->A = (r->X >> 1);                    /* 102506 RADD CLD SX DA / 102507 SHA ZIN SHR 1 */
    r->A += 1;                             /* 102510: AAA 1                            */
    r->B[010] = r->A + r->B[010];          /* 102511 ADD ,B 10 / 102512 STA ,B 10      */
    resident_worker_at_link_102522();      /* 102513: JPL I 7 -> link @102522          */
    return resident_return_102523();       /* 102514: JMP I 7 -> link @102523 (tail)   */
}

/* Byte-verified anchors:
 *   MSG entry 102453 (025-S3IRPIT), MST PIE 102462, LBYT char fetch 102470,
 *   byte compares SAT 47 / SAT 44 (102471/102474), emit calls JPL -31/-33 ->
 *   102447 (in-segment, return), SHA ZIN SHR 1 102507, indirect tail JMP I ->
 *   link @102523.
 * NOT proven: the fall-through MON 32 -> MSG bridge (uncarved MFELL/CALLPROC);
 *   the semantic label of each B-frame field; the exact 47B/44B character-class
 *   meaning (INFERRED as message-terminator / control-byte handling). */
