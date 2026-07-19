/* ===========================================================================
 *  000B-RRREG   ->  MON 60 subfunction RRREG = 0B (0x00 = 0 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : standalone ENTER-routine 013100 (framesize 000013).
 *  Purpose (NPL, authoritative): READ A REGISTER.  Server handler: 5NOPAR.
 *  Slot convention (PROVEN): caller does  LDX ,B -176  (X := gateway frame
 *  base) then  STx ,X 6/7 = MON 60 parameter 1/2.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_RRREG(void);   /* 013143 JPL I 30 -> ptr 013173 = thunk 146310;
                                 /*   SAA 0 -> gateway 146244 -> MON 60 146256   */
extern word *p;                 /* p[6]=param1, p[7]=param2 (,X <offset>)        */

void cmd_read_register(void)            /* routine @013100 */
{
    word regsel;      /* frame local @B-0163 - register selector (INFERRED)   */
    word result;      /* frame local @B-0161 - returned register value buffer */

    regsel = (word)const_0037;   /* 013132 LDD 37 ; 013133 STD ,B -163          */
    p[6]   = (word)&regsel;      /* 013134 RADD SB DA ;013135 AAA -163 ;013137 STA ,X 6 */
    p[7]   = (word)&result;      /* 013140 RADD SB DA ;013141 AAA -161 ;013142 STA ,X 7 */

    if (MON60_RRREG() == ERROR)  /* 013143 JPL I 30 -> thunk 146310 (RRREG 0B)   */
        goto err_013166;         /* 013144 callsite+1 = ERROR -> ptr 013166      */
    /* 013145 callsite+2 = SUCCESS: LDA ,B -164 ... consume returned value       */
}
