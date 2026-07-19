/* ============================================================================
 * MON 2B - OutByte (YFPUT) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 2B-OutByte.ASM.
 * The two-entry get/put selector, the control-block store and the transfer-helper
 * call are VERIFIED from bytes; the buffer-flush logic and the M2 fast-path
 * hand-off are INFERRED.
 *
 * Dispatch (see README):
 *   Fast path: MON 2B -> ENT14 072167B -> GOTAB[2B]=M2 071635B (level-14 handler).
 *   File path: MCTAB[2B] @005622B = 026600B = YFPUT (worker below).
 * YFPUT (put, MON 2B) and YFGET (get, MON 1B) share one body; the only difference
 * is the T selector: 45B for put, 46B for get.  All constants octal.
 * ============================================================================
 */

#define SEL_GET 046      /* YFGET: T := 46B */
#define SEL_PUT 045      /* YFPUT: T := 45B */

int YFPUT(OpenFile *f, int byte)             /* entry 026600B; MON 2B */
{
    int selector = SEL_PUT;                  /* 026600B: LDT 45 */
    return yf_transfer(f, selector, byte);   /* -> shared body at 026601B */
}

/* Shared put/get body (026601B onward): */
static int yf_transfer(OpenFile *f, int selector, int byte)
{
    f->ctrl[030] = selector;                 /* 026601B: STT ,X 30 */
    byte_transfer_helper(f, byte);           /* 026602B: JPL I 44 -> 026646 */

    f->status = 1;                           /* 026603B-026604B: SAA 1 / STA ,B 21 */
    f->byte_count = 0;                       /* 026606B: STZ ,B 27 */

    if (f->buffer_ptr != 0) {                /* 026607B-026610B: LDT I 40 / SKP */
        return put_byte_in_buffer(f, byte);  /* 026614B: JMP 25 -> 026641 (fast return) */
    }

    /* buffer full -> flush; INFERRED detail. */
    if (!flush_buffer(f))                    /* 026615B: JPL I 35 -> 026652 */
        return ERR_132;                      /* 026621B: SAA 132 */
    /* ... flag tests at 026623B-026637B select ERR_133 vs continue (INFERRED) ... */
    return put_byte_in_buffer(f, byte);
}
