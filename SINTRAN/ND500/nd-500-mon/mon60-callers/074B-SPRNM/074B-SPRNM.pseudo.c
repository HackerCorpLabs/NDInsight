/* ===========================================================================
 *  SPRNM   ->  MON 60 subfunction 074B = 0x3C = 60 dec
 * ---------------------------------------------------------------------------
 *  Purpose : SET NAME ON CURRENT PROCESS   (server handler ISPRNM)
 *  Call site 010112, inside the main command interpreter routine 002662
 *  (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_SPRNM(void);   /* JPL I 151 @010112 -> ptr 010263 = thunk 146602 (SAA 74) */
extern void  err_007500(void);    /* PROVEN target, role INFERRED */
extern void  cmd_loop_010613(void);
extern word *p;                   /* gateway frame top; p[6] = param1 */

void cmd_set_process_name(void)
{
    /* fpname : process-name descriptor in the F-image at B-113 (3 words) */
    p[6] = fpname;                 /* 010107 LDF ,B -113 ; 010111 STF ,X 6 */

    if (MON60_SPRNM() == ERROR)    /* 010112 JPL I 151 -> thunk 146602 (SPRNM 74B) */
        err_007500();              /* 010113 callsite+1 = ERROR  -> 007500 */
    cmd_loop_010613();             /* 010114 callsite+2 = SUCCESS -> 010613 */
}
