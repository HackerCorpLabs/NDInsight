/* 053B-T500P -> MON 60 T500P = 53B (0x2B = 43 dec). Purpose: (take ND-500 pages), handler 5NOPAR.
 * Call site 010245 inside command interpreter 002662. Also part of START-SWAPPER paging block.
 * OCTAL, BANK 1, base 0. */
extern dword get_num_param(int sel);  /* helper @002003 */
extern int   MON60_T500P(void);       /* JPL I ->010310; thunk 146522; SAA 53 */
extern void  err_007500(void);
extern void  cmd_loop_010613(void);
extern word *p;
void cmd_take_n500_pages(void)
{
    dword npages = get_num_param(0);  /* 010235 SAA 0; 010236 JPL I 41 ->002003; 010240 STD ,B 105 */
    p[6] = (word)&npages;             /* 010241 RADD SB DA; 010242 AAA 105; 010244 STA ,X 6 */
    if (MON60_T500P() == ERROR)       /* 010245 JPL I 43 -> thunk 146522 (53B) */
        err_007500();                 /* 010246 callsite+1 = ERROR */
    cmd_loop_010613();                /* 010247 callsite+2 = SUCCESS */
}
