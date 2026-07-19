/* ===========================================================================
 *  022B-ERRFL   ->  MON 60 subfunction ERRFL = 22B (0x12 = 18 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): (set error flag).  Server handler: 5NOPAR.
 *  Two call sites, both CASES inside the command interpreter 002662; each
 *  passes ONE constant in param slot ,X 6 (the two differ only in that value).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_ERRFL(void);       /* thunk 146376 (SAA 22) -> gateway 146244   */
extern void cmd_error_002673(void);  /* shared error reporter (ptr 005017)        */
extern void cmd_loop_010613(void);   /* command loop (ptr 005232)                 */
extern word *p;                      /* p[6] = MON 60 param 1                      */

/* --- 005173 (case 005170-005175) ----------------------------------------- */
void cmd_set_error_flag_on(void)
{
    p[6] = 0010636;              /* 005170 LDA 54 ([005244]=010636) ; 005172 STA ,X 6 */
    if (MON60_ERRFL() == ERROR)  /* 005173 JPL I 52 -> thunk 146376              */
        cmd_error_002673();      /* 005174 callsite+1 = ERROR -> 002673          */
    cmd_loop_010613();           /* 005175 callsite+2 = SUCCESS -> 010613        */
}

/* --- 005201 (case 005176-005203) ----------------------------------------- */
void cmd_set_error_flag_off(void)
{
    p[6] = 0010634;              /* 005176 LDA 50 ([005246]=010634) ; 005200 STA ,X 6 */
    if (MON60_ERRFL() == ERROR)  /* 005201 JPL I 44 -> thunk 146376              */
        cmd_error_002673();      /* 005202 callsite+1 = ERROR -> 002673          */
    cmd_loop_010613();           /* 005203 callsite+2 = SUCCESS -> 010613        */
}
