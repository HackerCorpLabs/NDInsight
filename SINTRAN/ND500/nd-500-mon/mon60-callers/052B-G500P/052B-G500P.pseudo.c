/* 052B-G500P -> MON 60 G500P = 52B (0x2A = 42 dec). Purpose: (give ND-500 pages), handler 5NOPAR.
 * Call site 010232 inside command interpreter 002662. Also part of START-SWAPPER paging block.
 * OCTAL, BANK 1, base 0. */
extern dword get_num_param(int sel);  /* helper @002003 (numeric-arg evaluator) */
extern int   MON60_G500P(void);       /* JPL I ->010307; thunk 146517; SAA 52 */
extern void  err_007500(void);
extern void  cmd_loop_010613(void);
extern word *p;
void cmd_give_n500_pages(void)
{
    dword npages = get_num_param(0);  /* 010222 SAA 0; 010223 JPL I 54 ->002003; 010225 STD ,B 105 */
    p[6] = (word)&npages;             /* 010226 RADD SB DA; 010227 AAA 105; 010231 STA ,X 6 */
    if (MON60_G500P() == ERROR)       /* 010232 JPL I 55 -> thunk 146517 (52B) */
        err_007500();                 /* 010233 callsite+1 = ERROR */
    cmd_loop_010613();                /* 010234 callsite+2 = SUCCESS */
}
