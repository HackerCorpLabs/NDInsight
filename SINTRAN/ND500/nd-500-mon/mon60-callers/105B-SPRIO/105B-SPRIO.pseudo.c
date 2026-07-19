/* ===========================================================================
 *  SPRIO   ->  MON 60 subfunction 105B = 0x45 = 69 dec
 * ---------------------------------------------------------------------------
 *  Purpose : SET PRIORITY   (server handler 5NOPAR)
 *  Three call sites in the main command interpreter routine 002662
 *  (framesize 000331): 006406, 006444, 006513 - each with THREE parameters.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_SPRIO(void);   /* thunk 146632 (SAA 105) at each site */
extern void  cmd_error_002673(void);
extern void  cmd_loop_010613(void);
extern word *p;

/* Site 006406 shown; 006444 and 006513 are the same shape: */
void cmd_set_priority(void)
{
    dword op1;   /* B+105 */
    dword op2;   /* B+107 */
    dword op3;   /* B+112 */
    /* ... three command operands evaluated into op1/op2/op3 ... */

    p[6]  = (word)&op1;            /* 006331..006334  &(B+105) */
    p[7]  = (word)&op2;            /* 006364..006367  &(B+107) */
    p[10] = (word)&op3;            /* 006402..006405  &(B+112) */

    if (MON60_SPRIO() == ERROR)    /* 006406 JPL I 147 -> thunk 146632 (SPRIO 105B) */
        cmd_error_002673();        /* 006407 callsite+1 = ERROR  -> 002673 */
    cmd_loop_010613();             /* 006410 callsite+2 = SUCCESS -> 010613 */
}
