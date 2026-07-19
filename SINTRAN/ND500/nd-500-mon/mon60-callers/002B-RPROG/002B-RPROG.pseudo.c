/* ===========================================================================
 *  002B-RPROG   ->  MON 60 subfunction RPROG = 2B (0x02 = 2 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): LOGICAL PROGRAM MEMORY READ.  Handler: 5NOPAR.
 *  Two call sites: 022440 (routine 022310) and 056341 (routine 056042).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_RPROG(void);   /* thunk 146316 (SAA 2) -> gateway 146244        */
extern word *p;                 /* p[6],p[7],p[10],p[13] = MON 60 params 1..4    */

/* --- call site 022440 (routine 022310, framesize 000014) ------------------ */
void read_prog_mem_A(void)
{
    dword lpaddr;   /* B-164, from LDD 37/104 const - logical program address (INFERRED) */
    p[6]  = (word)&lpaddr;       /* 022421..022426 */
    p[7]  = (word)const_67;      /* 022427..022430 */
    p[10] = f_register;          /* 022431..022434 STF ,X 10 (3-word)            */
    p[13] = (word)&local_B170;   /* 022435..022437 */
    if (MON60_RPROG() == ERROR)  /* 022440 JPL I 71 -> thunk 146316              */
        goto err_022315;         /* 022441 callsite+1 = ERROR                    */
    goto ok_022621;              /* 022442 callsite+2 = SUCCESS                  */
}

/* --- call site 056341 (routine 056042, framesize 000050) ------------------ */
void read_prog_mem_B(void)
{
    word sel;       /* param1 = value loaded by LDA -54 (INFERRED selector)     */
    p[6]  = sel;                 /* 056321..056323 */
    p[7]  = (word)&local_B163;   /* 056324..056326 */
    p[10] = f_register;          /* 056327..056335 STF ,X 10 (3-word)            */
    p[13] = (word)&local_B165;   /* 056336..056340 */
    if (MON60_RPROG() == ERROR)  /* 056341 JPL I 117 -> thunk 146316             */
        goto err_056240;         /* 056342 callsite+1 = ERROR                    */
    goto ok_056452;              /* 056343 callsite+2 = SUCCESS                  */
}
