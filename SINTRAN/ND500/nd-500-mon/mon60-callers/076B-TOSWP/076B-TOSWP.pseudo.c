/* ===========================================================================
 *  TOSWP   ->  MON 60 subfunction 076B = 0x3E = 62 dec
 * ---------------------------------------------------------------------------
 *  Purpose : MESSAGE TO SWAPPER   (server handler ITOSWP)
 *  Five call sites, each with ONE parameter = pointer to a message block:
 *    073362 (routine 073115, the LIST/SET-SYSTEM-PARAMETERS handler),
 *    073675 (routine 073535), 073741 (routine 073713),
 *    074003 (routine 073752), 107434 (routine 103722).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_TOSWP(void);   /* thunk 146610 (SAA 76) at each site */
extern word *p;                   /* gateway frame top; p[6] = param1 */

/* Common shape (site 073362 shown; other sites differ only in the frame offset
 * of the message block and in how error/success are handled): */
int send_message_to_swapper(word *msgblock)  /* msgblock @ B-167 for 073362 */
{
    p[6] = (word)msgblock;         /* 073356..073361  RADD SB DA;AAA -167;LDX;STA ,X 6 */

    if (MON60_TOSWP() == ERROR)    /* 073362 JPL I 22 -> thunk 146610 (TOSWP 76B) */
        return ERROR;              /* 073363 callsite+1 -> 177327 (LEAVE-value) */
    return OK;                     /* 073364 callsite+2 -> 177335 (LEAVE-SKIP)  */
}
