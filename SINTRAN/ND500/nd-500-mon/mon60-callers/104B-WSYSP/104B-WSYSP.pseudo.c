/* ===========================================================================
 *  WSYSP   ->  MON 60 subfunction 104B = 0x44 = 68 dec
 * ---------------------------------------------------------------------------
 *  Purpose : WRITE SYSTEM PARAMETERS   (server handler IWSYSP)
 *  Call site 073354, inside routine 073115 (framesize 000336) -- the
 *  LIST / SET-SYSTEM-PARAMETERS handler (see ../LIST-SYSTEM-PARAMETERS/).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_WSYSP(void);   /* JPL I 26 @073354 -> ptr 073402 = thunk 146627 (SAA 104) */
extern word *p;

int write_system_parameters(void)   /* fragment of routine 073115 */
{
    /* sysparm_block : system-parameter block at frame offset B-157 */
    p[6] = (word)&sysparm_block;   /* 073347 RADD SB DA;073350 AAA -167;073351 AAA 10;073353 STA ,X 6 */

    if (MON60_WSYSP() == ERROR)    /* 073354 JPL I 26 -> thunk 146627 (WSYSP 104B) */
        return ERROR;              /* 073355 callsite+1 -> 177327 (LEAVE-value) */
    /* 073356 callsite+2 = SUCCESS: routine continues */
    return OK;
}
