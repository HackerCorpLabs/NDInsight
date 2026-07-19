/* ===========================================================================
 *  SPFLAG  ->  MON 60 subfunction 101B = 0x41 = 65 dec
 * ---------------------------------------------------------------------------
 *  Purpose : WRITE FLAGS INTO ND-500 DATA SEGMENT   (server handler WWFLAG)
 *  Call site 005223, inside the main command interpreter routine 002662
 *  (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern dword get_num_param(int which);   /* helper @005205/005211 (SAA 0 / SAA 1) */
extern int   MON60_SPFLAG(void);          /* JPL I 24 @005223 -> ptr 005247 = thunk 146621 */
extern void  cmd_error_002673(void);
extern void  cmd_loop_010613(void);
extern word *p;

void cmd_write_flags(void)
{
    dword op0 = get_num_param(0);  /* 005204 SAA 0 ;005205 JPL I -164 ;005207 STD ,B -125 */
    dword op1 = get_num_param(1);  /* 005210 SAA 1 ;005211 JPL I -170 ;005213 STD ,B -127 */

    p[6] = (word)&op0;             /* 005214..005217  &(B-125) */
    p[7] = (word)&op1;             /* 005220..005222  &(B-127) */

    if (MON60_SPFLAG() == ERROR)   /* 005223 JPL I 24 -> thunk 146621 (SPFLAG 101B) */
        cmd_error_002673();        /* 005224 callsite+1 = ERROR  -> 002673 */
    cmd_loop_010613();             /* 005225 callsite+2 = SUCCESS -> 010613 */
}
