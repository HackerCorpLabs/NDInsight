/* 047B-DELSW -> MON 60 DELSW = 47B (0x27 = 39 dec). Purpose: DELETE SWAP FILE (IDELSWAP).
 * Call site 007430 inside command interpreter ENTER-routine 002662. OCTAL, BANK 1, base 0. */
extern int  MON60_DELSW(void);        /* JPL I ->007610; thunk 146503; SAA 47 */
extern void cmd_error_002673(void);
extern void cmd_loop_010613(void);
extern word *p;
void cmd_delete_swap_file(void)
{
    p[6] = fname;                     /* 007425 LDF ,B -113; 007426 LDX ,B -176; 007427 STF ,X 6 */
    if (MON60_DELSW() == ERROR)       /* 007430 JPL I 160 -> thunk 146503 (47B) */
        cmd_error_002673();           /* 007431 callsite+1 = ERROR */
    cmd_loop_010613();                /* 007432 callsite+2 = SUCCESS */
}
