/* ===========================================================================
 *  001B-WRREG   ->  MON 60 subfunction WRREG = 1B (0x01 = 1 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : standalone ENTER-routine 052605 (framesize 000005).
 *  Purpose (NPL, authoritative): (write a register).  Server handler: 5NOPAR.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_WRREG(void);   /* 052642 JPL I 17 -> ptr 052661 = thunk 146313;
                                 /*   SAA 1 -> gateway 146244 -> MON 60 146256   */
extern word *p;                 /* p[6]=param1, p[7]=param2 (,X <offset>)        */

void cmd_write_register(void)           /* routine @052605 */
{
    word regnum;      /* frame local @B-0167 - register number (INFERRED)      */
    word value;       /* frame local @B-0171 - value to write (INFERRED)       */

    regnum = local_B172 >> 020;  /* 052630 LDA ,B -172 ;052631 SAD SHR 20 ;052632 STD ,B -167 */
    p[6]   = (word)&regnum;      /* 052633 RADD SB DA ;052634 AAA -167 ;052636 STA ,X 6 */
    p[7]   = (word)&value;       /* 052637 RADD SB DA ;052640 AAA -171 ;052641 STA ,X 7 */

    if (MON60_WRREG() == ERROR)  /* 052642 JPL I 17 -> thunk 146313 (WRREG 1B)   */
        return LEAVE_value();    /* 052643 callsite+1 = ERROR -> ptr 052657 = 177327 */
    /* 052644 callsite+2 = SUCCESS: LDD ,B -167 ...                              */
}
