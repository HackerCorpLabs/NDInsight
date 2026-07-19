/* ===========================================================================
 *  DELDOM   ->  MON 60 subfunction DELDOM = 131B (0x59 = 89 dec)
 *  Purpose: DELETE STANDARD DOMAIN.  Server handler IDLSYDOM.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE 007344..007351, INSIDE the command interpreter
 *            ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_DELDOM(void);   /* 007347 JPL I ->007411; thunk 146715 SAA 131 */
extern void cmd_error_002673(void);/* interpreter error reporter (PROVEN)         */
extern void cmd_loop_010613(void); /* command loop (PROVEN)                        */
extern word *p;                   /* gateway slots, p[6]=param1 */

void cmd_delete_standard_domain(void)
{
    p[6] = fname_B113;               /* 007344 LDF ,B -113 ; 007346 STF ,X 6 (3 words) */
    if (MON60_DELDOM() == ERROR)     /* 007347 -> thunk 146715 (DELDOM 131B)          */
        cmd_error_002673();          /* 007350 callsite+1 -> ptr 007171 = 002673      */
    cmd_loop_010613();               /* 007351 callsite+2 -> ptr 007376 = 010613       */
}

/* PROVEN: one MON60 parameter (slot 6) = the 3-word F descriptor from B-113 = the
 *   standard-domain name.  No other slots are stored.
 * Server handler IDLSYDOM deletes the standard domain on the SINTRAN side. */
