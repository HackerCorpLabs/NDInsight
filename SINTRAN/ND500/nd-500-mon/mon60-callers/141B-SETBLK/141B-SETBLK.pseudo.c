/* ===========================================================================
 *  SETBLK   ->  MON 60 subfunction SETBLK = 141B (0x61 = 97 dec)
 *  Purpose: set block size of a file (SUBFUNCTION-TABLE.md, dispatch 5NOPAR).
 *  Operator command SET-BLOCK-SIZE.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command CASE 005131..005156, INSIDE the command interpreter
 *            ENTER-routine 002662 (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern dword get_num_operand(int sel); /* helper @005021 (INFERRED numeric-arg eval) */
extern int   MON60_SETBLK(void);       /* 005154 JPL I ->005240; thunk 146742 SAA 141 */
extern void  cmd_error_002673(void);   /* interpreter error reporter (PROVEN)  */
extern void  cmd_loop_010613(void);    /* command loop (PROVEN)                */
extern word *p;                        /* gateway slots, p[6]=param1, p[7]=param2 */

void cmd_set_block_size(void)
{
    dword operand1;                  /* frame local @B+105 (already in D at entry) */
    dword operand2;                  /* frame local @B+107 */

    /* operand1 was produced by the preceding evaluation, in D */
    /* 005131 STD ,B 105 : local(B+105) := operand1 */
    p[6] = (word)&operand1;          /* 005132..005135 : param1 := &local(B+105)   */

    operand2 = get_num_operand(1);   /* 005136..005143 : nested helper 005021       */
    /* 005144 STD ,B 107 : local(B+107) := operand2 */
    p[7] = (word)&operand2;          /* 005150..005153 : param2 := &local(B+107)   */

    if (MON60_SETBLK() == ERROR)     /* 005154 -> thunk 146742 (SETBLK 141B)       */
        cmd_error_002673();          /* 005155 callsite+1 -> ptr 005017 = 002673    */
    cmd_loop_010613();               /* 005156 callsite+2 -> ptr 005232 = 010613     */
}

/* PROVEN: two MON60 parameters - slot 6 = &local(B+105) = operand1, slot 7 =
 *   &local(B+107) = operand2.  The frame top is advanced +7 (005140) and
 *   restored -7 (005146) around the nested helper call.
 * INFERRED: operand1/operand2 map to (file number, block size) in some order,
 *   per the "set block size of a file" purpose; the exact order was not traced.
 *   Helper 005021 is the command-line numeric-argument evaluator (same idiom as
 *   helper 002003 in LOAD-CONTROL-STORE). */
