/* ===========================================================================
 *  LIST-STANDARD-DOMAINS   ->  MON 60 subfunction LSTDOM = 132B (0x5A = 90 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command case 007352..007354, INSIDE command interpreter @002662.
 *  NPL purpose of 132B (authoritative): LIST STANDARD DOMAINS.
 *  Shown with the immediately preceding, SEPARATE case DELETE STANDARD DOMAIN
 *  (DELDOM 131B) for context.
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_DELDOM(void);    /* JPL I ->007411; ptr=thunk 146715; SAA 131 DELETE-STD-DOMAIN */
extern int  MON60_LSTDOM(void);    /* JPL I ->007412; ptr=thunk 146720; SAA 132 LIST-STD-DOMAINS  */
extern void cmd_error_002673(void);/* internal error reporter (007171 pool word) */
extern void cmd_loop_010613(void); /* command loop (007376 pool word)            */
extern word *p;

/* --- DELETE STANDARD DOMAIN : subfunction 131B (adjacent, separate) ------- */
void cmd_delete_standard_domain(void)
{
    p[6] = domain_name;            /* 007344 LDF ,B -113 ; 007346 STF ,X 6 = param1 (<name>) */
    if (MON60_DELDOM() == ERROR)   /* 007347 JPL I 42 -> thunk 146715 (DELDOM 131B) */
        cmd_error_002673();        /* 007350 -> ptr 007171 = 002673 */
    cmd_loop_010613();             /* 007351 -> ptr 007376 = 010613 */
}

/* --- LIST STANDARD DOMAINS : subfunction 132B (no parameters) ------------- */
void cmd_list_standard_domains(void)
{
    if (MON60_LSTDOM() == ERROR)   /* 007352 JPL I 40 -> thunk 146720 (LSTDOM 132B) */
        cmd_error_002673();        /* 007353 -> ptr 007171 = 002673 */
    cmd_loop_010613();             /* 007354 -> ptr 007376 = 010613 */
}

/* FINDING (PROVEN): DELDOM (007347) and LSTDOM (007352) are two adjacent but
 * SEPARATE command cases; DELDOM's success at 007351 jumps to the command loop,
 * so control does not fall through into the LSTDOM call. */
