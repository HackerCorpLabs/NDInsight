/* ===========================================================================
 *  017B-LISOP   ->  MON 60 subfunction LISOP = 17B (0x0F = 15 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : CASE 005157..005161 inside the command interpreter ENTER-routine
 *            that begins at 002662 (framesize 000331).
 *  Purpose (NPL, authoritative): (list open files).  Server handler: 5NOPAR.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  No parameter is stored into the gateway block for this call.
 * ===========================================================================
 */

extern int  MON60_LISOP(void);       /* 005157 JPL I 62 -> thunk 146365          */
extern void cmd_error_002673(void);  /* shared error reporter (ptr 005017)       */
extern void cmd_loop_010613(void);   /* command loop (ptr 005232)                */

void cmd_list_open_files(void)          /* case @005157 */
{
    if (MON60_LISOP() == ERROR)  /* 005157 JPL I 62 -> thunk 146365 (LISOP 17B)  */
        cmd_error_002673();      /* 005160 callsite+1 = ERROR -> ptr 005017=002673 */
    cmd_loop_010613();           /* 005161 callsite+2 = SUCCESS -> ptr 005232=010613 */
}
