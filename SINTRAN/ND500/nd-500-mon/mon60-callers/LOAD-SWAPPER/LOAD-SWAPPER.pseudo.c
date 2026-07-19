/* ===========================================================================
 *  LOAD-SWAPPER   ->  MON 60 subfunction SWLOD = 7B (0x07 = 7 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command case 010211..010216, INSIDE command interpreter @002662.
 *  NPL purpose of 7B: PLACE SWAPPER.
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_SWLOD(void);     /* JPL I ->010305; ptr=thunk 146340; SAA 7 -> gateway */
extern void cmd_error_007500(void);/* internal error reporter (PROVEN target, role INFERRED) */
extern void cmd_loop_010613(void); /* return to command loop (PROVEN target) */
extern word *p;                    /* gateway outgoing param slots, ,X <offset> */

void cmd_load_swapper(void)
{
    /* swapper segment name descriptor is in the F-image @B-0113 (3 words) */

    p[6] = swapper_seg_name;       /* 010211 LDF ,B -113 ; 010213 STF ,X 6  = param1 */

    if (MON60_SWLOD() == ERROR)    /* 010214 JPL I 71 -> thunk 146340 (SWLOD 7B)      */
        cmd_error_007500();        /* 010215 callsite+1 = ERROR  -> ptr 010043 = 007500 */
    cmd_loop_010613();             /* 010216 callsite+2 = SUCCESS -> ptr 010051 = 010613 */
}

/* NOTE: the very next instruction, 010217, is a SEPARATE command case
 * (START-SWAPPER = STSWP 54B). SWLOD's success at 010216 jumps to the command
 * loop, so control does NOT fall through into STSWP. They are two distinct
 * operator commands, not one handler. (PROVEN by the 010216 JMP.)            */
