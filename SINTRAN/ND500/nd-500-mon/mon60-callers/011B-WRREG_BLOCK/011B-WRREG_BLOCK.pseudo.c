/* ===========================================================================
 *  WRREG_BLOCK  ->  MON 60 subfunction 011B (0x09 = 9 dec) WRITE REGISTERS
 * ---------------------------------------------------------------------------
 *  Handler : standalone ENTER-routine @ 052703 (framesize 000003).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_WRREG_BLOCK(void);    /* 052711 JPL I 6 -> thunk 146404 (SAA 11) */
extern word *p;                        /* gateway param slots, ,X <off> from stack top */
extern word regblock;                  /* caller local B-172 = <register block>       */

int write_registers_block(void)        /* @ 052703 */
{
    p[6] = regblock;                   /* 052706 LDF ,B -172 ; 052710 STF ,X 6 */

    if (MON60_WRREG_BLOCK() == ERROR)  /* 052711 MON60 011B */
        return LEAVE_value();          /* 052712 callsite+1 -> 177327 */

    /* 052713 SAA 1 ; 052714 STA I 5 ; 052715 -> 177335 */
    return LEAVE_skip();               /* 052715 callsite+2 = SUCCESS */
}

/* NOTE: yaml signature = "<register block>".  Byte-level: ,X 6 = F register loaded
 * from local B-172 (PROVEN, 052710 STF ,X 6). */
