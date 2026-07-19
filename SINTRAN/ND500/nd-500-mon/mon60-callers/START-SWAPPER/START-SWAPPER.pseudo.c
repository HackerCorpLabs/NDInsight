/* ===========================================================================
 *  START-SWAPPER   ->  MON 60 subfunction STSWP = 54B (0x2C = 44 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command case 010217..010221, INSIDE command interpreter @002662.
 *  NPL/yaml purpose of 54B: START SWAPPER (STSWP). No parameters.
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_STSWP(void);     /* JPL I ->010306; ptr=thunk 146525; SAA 54 */
extern int  MON60_G500P(void);     /* JPL I ->010307; ptr=thunk 146517; SAA 52 */
extern int  MON60_T500P(void);     /* JPL I ->010310; ptr=thunk 146522; SAA 53 */
extern dword get_num_param(int which);   /* helper @002003; ROLE INFERRED */
extern void cmd_error_007500(void);
extern void cmd_error_010256(void);/* T500P uses a different error target (ptr 010256=007500) */
extern void cmd_loop_010613(void);
extern word *p;

/* --- START-SWAPPER : subfunction 54B, no parameters --------------------- */
void cmd_start_swapper(void)
{
    if (MON60_STSWP() == ERROR)    /* 010217 JPL I 67 -> thunk 146525 (STSWP 54B) */
        cmd_error_007500();        /* 010220 callsite+1 = ERROR  -> ptr 010043 = 007500 */
    cmd_loop_010613();             /* 010221 callsite+2 = SUCCESS -> ptr 010051 = 010613 */
}

/* --- adjacent SEPARATE command: GIVE-N500-PAGES (G500P 52B) -------------- */
void cmd_give_n500_pages(void)
{
    dword npages;
    npages = get_num_param(0);     /* 010222 SAA 0 ; 010223 JPL I 54 -> 002003 ; 010225 STD ,B105 */
    p[6]   = (word)&npages;        /* 010226 RADD SB DA ; 010227 AAA 105 ; 010231 STA ,X 6 */
    if (MON60_G500P() == ERROR)    /* 010232 JPL I 55 -> thunk 146517 (G500P 52B) */
        cmd_error_007500();        /* 010233 -> 007500 */
    cmd_loop_010613();             /* 010234 -> 010613 */
}

/* --- adjacent SEPARATE command: TAKE-N500-PAGES (T500P 53B) -------------- */
void cmd_take_n500_pages(void)
{
    dword npages;
    npages = get_num_param(0);     /* 010235 SAA 0 ; 010236 JPL I 41 -> 002003 ; 010240 STD ,B105 */
    p[6]   = (word)&npages;        /* 010241 RADD SB DA ; 010242 AAA 105 ; 010244 STA ,X 6 */
    if (MON60_T500P() == ERROR)    /* 010245 JPL I 43 -> thunk 146522 (T500P 53B) */
        cmd_error_010256();        /* 010246 callsite+1 = ERROR -> ptr 010256 = 007500 */
    cmd_loop_010613();             /* 010247 callsite+2 = SUCCESS -> 010613 */
}

/* FINDING (PROVEN): STSWP (010217), G500P (010222) and T500P (010235) are three
 * distinct command cases in the same dispatch region, each with its own MON 60
 * and its own error/success tails. They are ADJACENT handlers, not one handler. */
