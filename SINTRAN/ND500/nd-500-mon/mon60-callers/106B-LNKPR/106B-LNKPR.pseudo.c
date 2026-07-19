/* ===========================================================================
 *  LNKPR   ->  MON 60 subfunction 106B = 0x46 = 70 dec
 * ---------------------------------------------------------------------------
 *  Purpose : (link to process)   (server handler 5NOPAR; purpose INFERRED,
 *            no verbatim NPL FUNCTION= comment for code 106)
 *  Call site 006711, inside the main command interpreter routine 002662
 *  (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern dword get_num_param(int which);   /* helper @006702 */
extern int   MON60_LNKPR(void);           /* JPL I 57 @006711 -> ptr 006770 = thunk 146635 */
extern void  cmd_error_002673(void);
extern word *p;

void cmd_link_to_process(void)
{
    dword target = get_num_param(0);   /* 006701 SAA 0 ;006702 JPL I -127 ;006704 STD ,B -117 */

    p[6] = (word)&target;              /* 006705..006710  &(B-117) */

    if (MON60_LNKPR() == ERROR)        /* 006711 JPL I 57 -> thunk 146635 (LNKPR 106B) */
        cmd_error_002673();            /* 006712 callsite+1 = ERROR  -> 002673 */
    /* 006713 callsite+2 = SUCCESS: STZ I 56 ; routine continues */
}
