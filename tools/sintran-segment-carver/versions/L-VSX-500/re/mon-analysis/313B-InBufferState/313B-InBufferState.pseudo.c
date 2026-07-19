/* ============================================================================
 * MON 313B  InBufferState (IBRSIZ)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Returns the terminal input-buffer state: how many
 * characters are held, and how many characters precede the next break.
 *
 * Derived from the real disassembly (see 313B-InBufferState.ASM). The worker
 * body's CONTROL FLOW is byte-verified (window 110543B..110770B in 025-S3IRPIT).
 * The SEMANTIC labels (which register holds what, the exact break accounting)
 * are INFERRED from the NPL IBRSIZ structure and the call shape - treat as a
 * model, not gospel. Addresses in comments are octal.
 *
 * DISPATCH CAVEAT: GOTAB[313] byte-forwards to 112247B (a monitor/interrupt-
 * entry routine), NOT to 110543B. Nothing in the carved segment statically
 * links MON 313 to this body; the link is a runtime computed jump. This model
 * assumes the runtime target IS the 110543B IBRSIZ body (UNVERIFIED).
 * ============================================================================ */

/* r->T on entry = logical device number (ZTREG). On return:
 *   r->A = BHOLD  = characters currently held in the input buffer
 *   r->X = BCOUNT = characters before the next break (BRKMAX-limited)
 * On error the NPL sets ZAREG = 240B (UNVERIFIED against these bytes).        */
void mon_in_buffer_state(mon_regs *r)
{
    dev = get0(r);                      /* 110543 JPL I 122 -> 000374 GET0     */
    ztreg = gztreg(dev);               /* 110544 JPL I 122 -> 044276 GZTREG    */

    if (!is_input_device(ztreg))        /* 110546-110547: bail if not eligible  */
        return;                         /*   (TAD / internal-buffer take other  */
                                        /*    paths; this is the terminal path) */

    IOF();                              /* 110555: critical section - the input */
                                        /*   ring must not change while scanned  */

    held = 0; before_break = 0;
    for (p = ring_tail; p != ring_head; p = ring_next(p)) {  /* 110606-110630   */
        c = lbyt(p);                    /* 110614 LBYT: fetch one input byte    */
        held++;
        if (is_break(c) || before_break >= BRKMAX)  /* 110573-110576 / 110645+  */
            break;                      /*   stop counting at first break       */
        before_break++;
    }

    /* end critical section: NO ION appears in the carved 110543B..110770B     */
    /* window; interrupts are re-enabled on a path outside the carve (INFERRED, */
    /* not in these bytes). Only the IOF at 110555 is byte-present.            */

    r->A = held;                        /* 110663 LDA ,B 16 -> BHOLD            */
    r->X = before_break;                /* result staged 110723+ -> BCOUNT      */
    return;                             /* 110762 JMP I ,B 34 -> caller return   */
}

/* Callers:
 *   MON 313B InBufferState:  ztreg = dev; mon_in_buffer_state(r);
 * Notes:
 *   - BRKMAX caps before_break for terminals (NPL IBRSIZ).
 *   - Files are NOT valid here; only terminal/TAD/internal-buffer devices.
 */
