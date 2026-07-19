/* ===========================================================================
 *  DEFM  ->  MON 60 subfunction 040B (0x20 = 32 dec) DEFINE MEMORY CONFIGURATION
 * ---------------------------------------------------------------------------
 *  Handler : standalone ENTER-routine @ 134731 (framesize 000113).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <start page> <no. of memory parts> <part array>.
 * ===========================================================================
 */

extern int MON60_DEFM(void);           /* 135361 JPL I 3 -> thunk 146450 (SAA 40) */
extern word *p;

int define_memory_config(void)         /* within routine @ 134731 */
{
    p[6]  = &start_page;               /* 135336 STA ,X 6  = &(B-172) = <start page> */
    p[7]  = &nparts;                   /* 135341 STA ,X 7  = &(B-155) = <no. of memory parts> */
    p[10] = part_array_desc;           /* 135345 STF ,X 10 = F(B-153) = <part array> (3 words) */

    if (MON60_DEFM() == ERROR)         /* 135361 MON60 040B */
        goto err_135323;               /* 135362 JPL -37 -> local error handler */
    return LEAVE_skip();               /* 135363 JPL I 2 -> 135365 = 177335 SUCCESS */
}

/* 3 params in slots 6/7/10, matching yaml.  135346 JMP 13 skips the routine's
 * inline pointer pool before reaching the call at 135361.  All stores PROVEN. */
