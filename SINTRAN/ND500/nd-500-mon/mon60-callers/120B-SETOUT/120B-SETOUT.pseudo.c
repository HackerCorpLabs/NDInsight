/* ===========================================================================
 *  SETOUT   ->  MON 60 subfunction SETOUT = 120B (0x50 = 80 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE 003556..003576, INSIDE the command interpreter
 *            ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_SETOUT(void);   /* 003573 JPL I ->003751; thunk 146662 SAA 120 */
extern void cmd_error_002673(void);/* interpreter error reporter (PROVEN target)  */
extern void cmd_continue_003527(void);/* command-continue point (PROVEN target)   */
extern word *p;                   /* gateway outgoing slots, p[6]=param1 */

void cmd_setout(void)
{
    dword val;                       /* frame local @B-127 */

    val = indirect_load() >> 16;     /* 003564 LDA I -22 ; 003565 SAD SHR 20     */
    /* 003566 STD ,B -127 : local(B-127) := val */
    p[6] = (word)&val;               /* 003567..003572 : param1 := &local(B-127)  */

    if (MON60_SETOUT() == ERROR)     /* 003573 -> thunk 146662 (SETOUT 120B)      */
        cmd_error_002673();          /* 003574 callsite+1 -> ptr 003750 = 002673   */
    cmd_continue_003527();           /* 003575 callsite+2 -> 003527                 */
}

/* PROVEN: one MON60 parameter (slot 6) = &local(B-127), a value produced by the
 *   indirect load at 003564 masked with SAD SHR 20 (>>16).
 * INFERRED: per SUBFUNCTION-TABLE.md, subfunction 120B is "(set output device)"
 *   (handler 5NOPAR).  The passed value is therefore the output-device/unit
 *   selector; its exact encoding was not traced. */
