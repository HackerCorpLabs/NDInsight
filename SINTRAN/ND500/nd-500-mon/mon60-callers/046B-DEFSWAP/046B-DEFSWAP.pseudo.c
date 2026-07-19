/* 046B-DEFSWAP -> MON 60 DEFSWAP = 46B (0x26 = 38 dec). Purpose: DEFINE SWAP FILE (IDEFSWAP).
 * Call site 007422 inside command interpreter ENTER-routine 002662 (framesize 000331).
 * All addresses OCTAL, BANK 1, base 0. */
extern int  MON60_DEFSWAP(void);      /* JPL I ->007607; thunk 146500; SAA 46 */
extern void cmd_error_002673(void);   /* internal error reporter (PROVEN target, role INFERRED) */
extern void cmd_loop_010613(void);    /* command loop (PROVEN target) */
extern word *p;
void cmd_define_swap_file(void)
{
    p[6] = fname;                     /* 007417 LDF ,B -113; 007420 LDX ,B -176; 007421 STF ,X 6 */
    if (MON60_DEFSWAP() == ERROR)     /* 007422 JPL I 165 -> thunk 146500 (46B) */
        cmd_error_002673();           /* 007423 callsite+1 = ERROR */
    cmd_loop_010613();                /* 007424 callsite+2 = SUCCESS */
}
