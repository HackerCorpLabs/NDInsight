/* ===========================================================================
 *  ACTIV   ->  MON 60 subfunction ACTIV = 136B (0x5E = 94 dec)
 *  Purpose: ACTIVATE STOPPED PROCESS.  Server handler IPRACTIVE.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE around 010605..010612, INSIDE the command interpreter
 *            ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_ACTIV(void);    /* 010610 JPL I ->010632; thunk 146731 SAA 136 */
extern void err_010574(void);     /* local error handler (PROVEN target)         */
extern void cmd_loop_010613(void);/* command loop area (PROVEN target)           */
extern word *p;                   /* gateway slots, p[6]=param1 */

void cmd_activate_stopped_process(void)
{
    p[6] = fname_B113;               /* 010605 LDF ,B -113 ; 010607 STF ,X 6 (3w) */
    if (MON60_ACTIV() == ERROR)      /* 010610 -> thunk 146731 (ACTIV 136B)       */
        err_010574();                /* 010611 callsite+1 -> 010574                */
    cmd_loop_010613();               /* 010612 callsite+2 -> 010613                */
}

/* PROVEN: one MON60 parameter (slot 6) = the 3-word F descriptor from B-113 = the
 *   process/domain name to activate.
 * NOTE: the error handler at 010574 itself issues a secondary MON 60 (at 010601,
 *   ptr 010630) as part of cleanup - a different subfunction, not this call.
 * Server handler IPRACTIVE activates the stopped process on the SINTRAN side. */
