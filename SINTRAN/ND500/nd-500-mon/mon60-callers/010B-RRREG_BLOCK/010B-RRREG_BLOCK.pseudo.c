/* ===========================================================================
 *  RRREG_BLOCK  ->  MON 60 subfunction 010B (0x08 = 8 dec) READ ALL REGISTERS
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : standalone ENTER-routine @ 052522 (framesize 000001).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_RRREG_BLOCK(void);   /* 052563 JPL I 20 -> thunk 146401 (SAA 10) */
extern word *p;                        /* gateway param slots, ,X <off> from stack top */

int read_all_registers_block(void)     /* @ 052522 */
{
    /* 052525..052555: scan/prepare (loop, INFERRED role) */

    /* build the <register block> descriptor and hand it to the ND-500 */
    p[6] = regblock_descriptor;        /* 052556 SAA 77 / 052560 LDT 22 / 052562 STF ,X 6 */

    if (MON60_RRREG_BLOCK() == ERROR)  /* 052563 MON60 010B */
        return LEAVE_value();          /* 052564 callsite+1 -> 177327 (propagate error) */

    /* 052565 STZ I 14 ; 052566 -> 177335 */
    return LEAVE_skip();               /* 052566 callsite+2 = SUCCESS */
}

/* NOTE: yaml signature = "<register block>" (one descriptor).  Byte-level:
 * param slot ,X 6 receives the F register (3-word descriptor) built at
 * 052556-052562.  PROVEN store at 052562 (STF ,X 6). */
