/* ===========================================================================
 *  014B-CLSFI   ->  MON 60 subfunction CLSFI = 14B (0x0C = 12 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : CASE 005113..005125 inside the command interpreter ENTER-routine
 *            that begins at 002662 (framesize 000331).
 *  Purpose (NPL, authoritative): (close file).  Server handler: 5NOPAR.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern dword get_num_param(int which);  /* helper @002003 (ptr 005021)          */
extern void  cmd_error_002673(void);     /* shared error reporter (ptr 005017/005230) */
extern void  cmd_loop_010613(void);      /* command loop (ptr 005232)            */
extern int   MON60_CLSFI(void);          /* 005123 JPL I 114 -> thunk 146354      */
extern word *p;                          /* p[6] = MON 60 param 1                 */

void cmd_close_file(void)               /* case @005113 */
{
    word filenum;   /* frame local @B-0117 - file/connect number (INFERRED)      */

    filenum = get_num_param(0);  /* 005113 SAA 0 ; 005114 JPL I -73 ; 005116 STD ,B -117 */
    p[6]    = (word)&filenum;    /* 005117 RADD SB DA ;005120 AAA -117 ;005122 STA ,X 6 */

    if (MON60_CLSFI() == ERROR)  /* 005123 JPL I 114 -> thunk 146354 (CLSFI 14B) */
        cmd_error_002673();      /* 005124 callsite+1 = ERROR -> ptr 005017=002673 */
    cmd_loop_010613();           /* 005125 callsite+2 = SUCCESS -> ptr 005232=010613 */
}
