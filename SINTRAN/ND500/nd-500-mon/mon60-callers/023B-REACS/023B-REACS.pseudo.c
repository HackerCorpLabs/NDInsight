/* ===========================================================================
 *  REACS  ->  MON 60 subfunction 023B (0x13 = 19 dec) READ CONTROL STORE
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  TWO call sites: 123556 (routine 123515), 124201 (routine 124023).
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <CS addr.> <no of 16 bit words> <data-area>.
 * ===========================================================================
 */

extern int MON60_REACS(void);          /* thunk 146407 (SAA 23) */
extern word *p;

/* Call site A @ 123556 (routine 123515) */
int read_control_store_A(void)
{
    p[6]  = &cs_addr;                  /* 123550 STA ,X 6 = &(B-164) */
    p[7]  = &word_count;               /* 123553 STA ,X 7 = &(B-166) */
    p[10] = data_area_desc;            /* 123555 STF ,X 10 = F(B-171) */
    if (MON60_REACS() == ERROR)        /* 123556 MON60 023B */
        return LEAVE_value();          /* 123557 -> 177327 */
    /* 123560 ... SUCCESS continues */
}

/* Call site B @ 124201 (routine 124023) */
int read_control_store_B(void)
{
    p[6]  = &cs_addr;                  /* 124167 STA ,X 6 = &(B-153) */
    p[7]  = &word_count;               /* 124172 STA ,X 7 = &(B-151) */
    p[10] = data_area_desc;            /* 124200 STF ,X 10 = F built from B-156/B-154 */
    if (MON60_REACS() == ERROR)        /* 124201 MON60 023B */
        return LEAVE_value();          /* 124202 -> 177327 */
    /* 124203 ... SUCCESS continues (124214 JMP -> 124233 = LEAVE-SKIP) */
}

/* Both sites pass 3 params in slots 6/7/10, matching the yaml signature.  The
 * two data-area descriptors are built differently (B-171 direct vs B-156/B-154),
 * but both land in ,X 10 as the 3-word F descriptor.  All stores PROVEN. */
