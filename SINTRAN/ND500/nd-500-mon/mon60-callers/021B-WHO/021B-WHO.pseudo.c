/* ===========================================================================
 *  021B-WHO   ->  MON 60 subfunction WHO = 21B (0x11 = 17 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): (who is on).  Server handler: 5NOPAR.
 *  Two call sites, both inside the command interpreter 002662; neither stores
 *  a parameter into the gateway block.
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_WHO(void);         /* thunk 146373 (SAA 21) -> gateway 146244   */
extern void cmd_error_002673(void);  /* shared error reporter (ptr 005017)        */
extern void cmd_loop_010613(void);   /* command loop (ptr 005232)                 */

/* --- 005165 (simple case 005165-005167) ---------------------------------- */
void cmd_who_is_on(void)
{
    if (MON60_WHO() == ERROR)    /* 005165 JPL I 56 -> thunk 146373              */
        cmd_error_002673();      /* 005166 callsite+1 = ERROR -> 002673          */
    cmd_loop_010613();           /* 005167 callsite+2 = SUCCESS -> 010613        */
}

/* --- 007445 (local sub-block 007434-007447, entered with L as return link) - */
void who_block(void)
{
    save_link_to_B111();         /* 007434..007435 */
    word r = call_007611();      /* 007437 - result of a prior routine           */
    if (r == const_007612) {     /* 007442..007443 */
        if (MON60_WHO() == ERROR)/* 007445 JPL I 146 -> thunk 146373             */
            goto err_007606;     /* 007446 callsite+1 = ERROR                    */
    }
    return via_B111();           /* 007447 callsite+2 = SUCCESS (JMP I ,B 111)   */
}
