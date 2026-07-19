/* ===========================================================================
 *  003B-RDATA   ->  MON 60 subfunction RDATA = 3B (0x03 = 3 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): LOGICAL DATA MEMORY READ.  Handler: 5NOPAR.
 *  Three call sites: 022462 (rtn 022310), 055211 (rtn 055151), 056723 (rtn 056042).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_RDATA(void);   /* thunk 146324 (SAA 3) -> gateway 146244        */
extern word *p;                 /* p[6],p[7],p[10],p[13] = MON 60 params 1..4    */

/* --- 022462 (routine 022310) --------------------------------------------- */
void read_data_mem_A(void)
{
    dword ldaddr;   /* B-164 := const LDD 67 - logical data address (INFERRED)  */
    p[6]  = (word)&ldaddr;       /* 022444..022450 */
    p[7]  = (word)const_45;      /* 022451..022452 */
    p[10] = f_register;          /* 022453..022456 (3-word)                      */
    p[13] = (word)&local_B170;   /* 022457..022461 */
    if (MON60_RDATA() == ERROR) goto err_022315;  /* 022463 */
    goto ok_022621;                                /* 022464 */
}

/* --- 055211 (routine 055151) --------------------------------------------- */
void read_data_mem_B(void)
{
    p[6]  = (word)&local_B165;   /* 055170..055173 */
    p[7]  = (word)&local_B172;   /* 055174..055176 */
    p[10] = f_register;          /* 055177..055205 (3-word)                      */
    p[13] = (word)&local_B165;   /* 055206..055210 */
    if (MON60_RDATA() == ERROR) return LEAVE_value();  /* 055212 ptr=177327 */
    return LEAVE_skip();                               /* 055213 ptr=177335 */
}

/* --- 056723 (routine 056042) --------------------------------------------- */
void read_data_mem_C(void)
{
    word sel;        /* param1 = value LDA -4 (INFERRED selector)               */
    p[6]  = sel;                 /* 056703..056705 */
    p[7]  = (word)&local_B163;   /* 056706..056710 */
    p[10] = f_register;          /* 056711..056717 (3-word)                      */
    p[13] = (word)&local_B165;   /* 056720..056722 */
    if (MON60_RDATA() == ERROR) goto err_056605;  /* 056724 */
    /* 056725 callsite+2 = SUCCESS: LDX ,B -172 ...                             */
}
