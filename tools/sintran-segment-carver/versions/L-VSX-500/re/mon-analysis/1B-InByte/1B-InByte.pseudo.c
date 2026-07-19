/* ============================================================================
 * MON 1B - InByte (YFGET) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 1B-InByte.ASM.
 * The two-entry get/put selector, the control-block store and the transfer-helper
 * call are VERIFIED from bytes; the buffer-refill logic and the M1 fast-path
 * hand-off are INFERRED.
 *
 * Dispatch (see README):
 *   Fast path: MON 1B -> ENT14 072167B -> GOTAB[1B]=M1 071633B (level-14 handler).
 *   File path: MCTAB[1B] @005621B = 026576B = YFGET (worker below).
 * YFGET (get, MON 1B) and YFPUT (put, MON 2B) share one body; the only difference
 * is the T selector: 46B for get, 45B for put.  All constants octal.
 * ============================================================================
 */

#define SEL_GET 046      /* YFGET: T := 46B */
#define SEL_PUT 045      /* YFPUT: T := 45B */

int YFGET(OpenFile *f)                       /* entry 026576B; MON 1B */
{
    int selector = SEL_GET;                  /* 026576B: LDT 46 */
    return yf_transfer(f, selector);         /* -> shared body at 026601B */
}

/* Shared get/put body (026601B onward): */
static int yf_transfer(OpenFile *f, int selector)
{
    f->ctrl[030] = selector;                 /* 026601B: STT ,X 30 */
    byte_transfer_helper(f);                 /* 026602B: JPL I 44 -> 026646 */

    f->status = 1;                           /* 026603B-026604B: SAA 1 / STA ,B 21 */
    f->byte_count = 0;                       /* 026606B: STZ ,B 27 */

    if (f->buffer_ptr != 0) {                /* 026607B-026610B: LDT I 40 / SKP */
        return next_byte_in_buffer(f);       /* 026614B: JMP 25 -> 026641 (fast return) */
    }

    /* buffer empty -> refill; INFERRED detail. */
    if (!refill_buffer(f))                   /* 026615B: JPL I 35 -> 026652 */
        return ERR_132;                      /* 026621B: SAA 132 */
    /* ... flag tests at 026623B-026637B select ERR_133 vs continue (INFERRED) ... */
    return next_byte_in_buffer(f);
}
